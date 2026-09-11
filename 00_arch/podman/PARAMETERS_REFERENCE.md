# Kafka Trigger-Based Pipeline — Parameter Reference

This is the single master reference for every configuration parameter used by this
project's two scripts. It consolidates `ADDED_CONFIG_PARAMETERS.md`,
`STATUS_MESSAGES_HOW_TO.md`, and `GET_KAFKA_HOW_TO.md` into one document, organized
by topic instead of by file.

Parameters marked **🆕 Added** were introduced in this project on top of the base DSF
streaming framework — everything else is a standard framework parameter that this
project also configures.

---

## Table of Contents

1. [Overview](#1-overview)
2. [Repository Map](#2-repository-map)
3. [How the Two Scripts Connect](#3-how-the-two-scripts-connect)
4. [Parameters — `kafka_trigger_status_messages.py`](#4-parameters--kafka_trigger_status_messagespy)
   - [4.1 Kafka Connection](#41-kafka-connection)
   - [4.2 Avro / Schema Registry](#42-avro--schema-registry)
   - [4.3 Time Window](#43-time-window)
   - [4.4 Output Format](#44-output-format)
   - [4.5 Message Filtering & Instance Validation](#45-message-filtering--instance-validation)
   - [4.6 Retry & Timing](#46-retry--timing)
   - [4.7 Behaviour Flags](#47-behaviour-flags)
5. [Parameters — `00_get_kafka.py`](#5-parameters--00_get_kafkapy)
   - [5.1 Kafka Connection](#51-kafka-connection)
   - [5.2 Avro / Schema Registry](#52-avro--schema-registry)
   - [5.3 Time Window](#53-time-window)
   - [5.4 Output Format](#54-output-format)
   - [5.5 Message Filtering](#55-message-filtering)
   - [5.6 Retry, Timing & Over-Count Handling](#56-retry-timing--over-count-handling)
   - [5.7 Behaviour Flags](#57-behaviour-flags)
6. [Shared Metadata-Bridge Parameters](#6-shared-metadata-bridge-parameters)
7. [Worked End-to-End Example](#7-worked-end-to-end-example)
8. [Quick Reference — "I want to…"](#8-quick-reference--i-want-to)
9. [Local Testing Scripts (not part of the pipeline)](#9-local-testing-scripts-not-part-of-the-pipeline)

---

## 1. Overview

This project consists of **two scripts** that run back-to-back as separate steps in
the same framework job chain:

| Script | Role |
|---|---|
| `kafka_trigger_status_messages.py` | Watches a **status/inflow topic**. Waits until all publisher instances for a business date have reported in, totals up how many business messages *should* exist, and writes that total (plus filter values) to a small **metadata file**. |
| `00_get_kafka.py` | Reads that metadata file, then consumes the **business topic**, counting messages that match the filter values until the actual count reaches the expected count (within tolerance) — then writes the final output file. |

Both scripts share the same shape of configuration (Kafka connection, Avro/schema
registry, time window, retry/timing, output format) but are configured through two
**separate JSON config files**. A handful of parameters — the "metadata bridge" — must
be set identically in both files because they describe the same handoff file. See
[Section 6](#6-shared-metadata-bridge-parameters).

---

## 2. Repository Map

| File | Purpose |
|---|---|
| `kafka_trigger_status_messages.py` | Status-message watcher / metadata writer |
| `00_get_kafka.py` | Business-message consumer / final output writer |
| `status_messages_config.json` | Config for the status-messages script |
| `get_kafka_config.json` | Config for the get-kafka script |
| `STATUS_MESSAGES_HOW_TO.md` | Deep-dive guide + scenarios for the status-messages script |
| `GET_KAFKA_HOW_TO.md` | Deep-dive guide + scenarios for the get-kafka script |
| `GET_KAFKA_DELAY_SCENARIOS.md` | Timing/delay edge-case scenarios for `00_get_kafka.py` |
| `ADDED_CONFIG_PARAMETERS.md` | Focused write-up of only the parameters added in this project |
| `PIPELINE_L2_SUPPORT.md` | L2 support / troubleshooting playbook |
| `RUNBOOK.md` | Full operational runbook |
| `run_local.sh`, `start_kafka.sh`, `stop_kafka.sh`, `reset_kafka.sh`, `run_get_kafka_local.sh` | **Local Podman testing helpers only** — see [Section 9](#9-local-testing-scripts-not-part-of-the-pipeline) |

---

## 3. How the Two Scripts Connect

```
 ┌─────────────────────────────┐        ┌──────────────────────────────┐
 │ kafka_trigger_status_messages│       │        00_get_kafka.py        │
 │            .py               │        │                                │
 │                               │        │                                │
 │ 1. Read status/inflow topic  │        │ 1. Read metadata file          │
 │ 2. Validate all instances    │  ───▶  │    (expected count + filters)  │
 │    have reported in          │  metadata   2. Read business topic      │
 │ 3. Sum message counts        │  file    3. Count matching messages     │
 │ 4. Write metadata file       │        │ 4. Compare vs expected count   │
 │ 5. Write status output file  │        │ 5. Accept / retry / fail       │
 └─────────────────────────────┘        └──────────────────────────────┘
```

The metadata filename is built the same way on both sides:

```
{STATUS_MESSAGES_FEED_NAME}_{ASOF_DT}{METADATA_FILE_SUFFIX}
```

**Example:** `STATUS_MESSAGES_FEED_NAME="CPSB4Q00"`, `ASOF_DT="2026-04-22"`,
`METADATA_FILE_SUFFIX="_metadata.txt"` → `CPSB4Q00_2026-04-22_metadata.txt`

---

## 4. Parameters — `kafka_trigger_status_messages.py`

### 4.1 Kafka Connection

| Parameter | Type | Example | Description |
|---|---|---|---|
| `KAFKA_USER` | object, keyed by mandator | `{"022": "local"}` | Kafka username |
| `STREAMING_KAFKA_BROKER` | object, keyed by mandator | `{"022": "localhost:9092"}` | Broker address |
| `STREAMING_KAFKA_INFLOW_TOPIC` | object, keyed by mandator | `{"022": "inflow-topic"}` | Topic to consume **status** messages from |

All three are mandator-specific; the mandator is injected at runtime via the
`DSF_MANDATOR` environment variable.

### 4.2 Avro / Schema Registry

| Parameter | Type | Default | Description |
|---|---|---|---|
| `AVRO_SCHEMA_REGISTRY` | string | — | Schema registry address, e.g. `localhost:8081` |
| `AVRO_SCHEMA_FILE` (also accepted as `AVRO_SHCEMA_FILE`) | string | `"LATEST"` | Schema version. `"LATEST"` auto-fetches the newest version |
| `AVRO_COLUMNS` | string (comma list) | `""` | Columns to extract. Empty = extract all |
| `INPUT_DATA` | string | `"AVRO"` | Format of messages on the topic |
| `OUTPUT_DATA` | string | `"JSON"` | Format of the output file written to disk |

> **Typo note:** the framework config key is spelled `AVRO_SHCEMA_FILE`. The script
> accepts both spellings; `AVRO_SCHEMA_FILE` is the preferred one going forward.

### 4.3 Time Window

The script only reads status messages whose Kafka timestamp falls inside a configured window.

| Parameter | Type | Example | Description |
|---|---|---|---|
| `START_TS` | `HH:MM:SS` string | `"23:59:00"` | Time-of-day for window start |
| `START_DT_OFFSET` | string integer | `"0"` | Days subtracted from `ASOF_DT` for the start date |
| `STOP_TS` | `HH:MM:SS` string | `"00:00:00"` | Time-of-day for window end |
| `STOP_DT_OFFSET` | string integer | `"-1"` | Days subtracted from `ASOF_DT` for the end date |

**Per-mandator override:**

```json
"LOCATION_TIME_WINDOW": {
    "022": {
        "START_TS": "23:59:00", "START_DT_OFFSET": "0",
        "STOP_TS": "00:00:00",  "STOP_DT_OFFSET": "-1"
    }
}
```

**Formula:**

```
window_start = date(ASOF_DT) + time(START_TS) - timedelta(days=START_DT_OFFSET)
window_end   = date(ASOF_DT) + time(STOP_TS)  - timedelta(days=STOP_DT_OFFSET)
```

**Example** (`ASOF_DT = 2026-04-22`, config above):
`window_start = 2026-04-22 23:59:00`, `window_end = 2026-04-23 00:00:00` — the
one-minute publication window around midnight of the business date.

### 4.4 Output Format

| Parameter | Type | Default | Description |
|---|---|---|---|
| `SEPERATOR` *(spelling intentional — matches framework config key)* | string | `"\|"` | Column delimiter in the output file |
| `DECIMAL_CONV` | string | `"2"` | `"2"` = disabled. `"YES"` = convert to fixed decimal |
| `DECIMAL_SCALE` | string integer | `"2"` | Decimal places when `DECIMAL_CONV="YES"` |

### 4.5 Message Filtering & Instance Validation

| Parameter | Type | Default | Description |
|---|---|---|---|
| `PRODUCER_FILTER` | string | `"CLIENT_STRUCTURES"` | Only process status messages from this producer. Empty = accept all |
| `VALIDATE_TOPIC_MANDATOR` | string | `"NO"` | `"YES"` = verify the topic actually contains data for the expected mandator first |
| `STREAMING_MANDATORY_KEY` | string | `""` | A field that must be present on every message. Empty = no check |
| `VALIDATION_REQUIRED_COLUMNS` | string (comma list) | — | Columns that must be present in the output; validates schema completeness |
| `INSTANCE_VALIDATION_MODE` 🆕 | string, `"SEQUENTIAL"` \| `"UNIQUE_COUNT"` | `"SEQUENTIAL"` | How "all instances reported in" is decided — see below |

**`PRODUCER_FILTER`** — status messages carry a `producer` field. Only messages where
`producer == PRODUCER_FILTER` are counted and written. This lets multiple producers
share a topic without interfering.

```
Message A: producer = "CLIENT_STRUCTURES"  → included
Message B: producer = "RISK_ENGINE"        → excluded
```

**`INSTANCE_VALIDATION_MODE` 🆕 — detail**

Different producers on the same topic can number their `status.instanceIndex` values
differently. This mode controls how "all instances present" is validated for the max
`reconciliationGroupId`:

- **`SEQUENTIAL`** (default) — instances must publish `instanceIndex` `0..totalInstances-1`,
  contiguous and zero-based. Any missing index in that range → retryable failure.
- **`UNIQUE_COUNT`** — for producers with arbitrary/non-sequential instance IDs (e.g.
  hash- or partition-derived). There's no fixed range to check — validation only
  requires the **count of distinct** `instanceIndex` values to equal `totalInstances`.

| Mode | Instances received | Result |
|---|---|---|
| `SEQUENTIAL` (expect 0,1,2) | `0, 1, 2` | ✅ PASS |
| `SEQUENTIAL` (expect 0,1,2) | `0, 2` | ❌ FAIL — index 1 missing, retry |
| `UNIQUE_COUNT` (expect 3) | `17, 42, 5` | ✅ PASS — 3 distinct == 3 |
| `UNIQUE_COUNT` (expect 3) | `17, 42` | ❌ FAIL — 2 distinct != 3, retry |
| `UNIQUE_COUNT` (expect 3) | `17, 42, 5, 9` | ❌ FAIL — 4 distinct != 3 (anomaly, same retry path) |

Duplicate `instanceIndex` values hard-fail immediately in **either** mode.

**Config to switch:**
```json
"INSTANCE_VALIDATION_MODE": "UNIQUE_COUNT"
```
Omit the key (or set `"SEQUENTIAL"`) to keep the original 0-based range-check behaviour.

### 4.6 Retry & Timing

| Parameter | Type | Default | Description |
|---|---|---|---|
| `MAX_LISTEN_DURATION_HOURS` | string float | `"2"` | Total time the script may run before giving up |
| `MIN_LISTENING_DURATION_MINUTES` | string integer | — | Minimum listen time even if all data has already arrived |
| `MAX_RETRY_ATTEMPTS` | string integer | `"0"` | `0` = unlimited (retry until deadline) |
| `RETRY_WAIT_SECONDS` | string integer | `"300"` | Sleep between retry attempts |
| `STREAMING_WAIT_UNTIL_DONE_HOURS` | string integer | `"1"` | Max hours to wait for the publisher's "done" signal |
| `STREAMING_IDLE_TIMEOUT_MINUTES` | string integer | `"1"` | Stop consuming after this many minutes of silence |
| `EXTEND_ON_ITERATE` | string | `"NO"` | `"YES"` = push the scan window's stop time forward on each retry |
| `START_FROM_LAST` | string | `"NO"` | `"YES"` = resume from last committed offset instead of window start |

**`MAX_RETRY_ATTEMPTS` vs `MAX_LISTEN_DURATION_HOURS`:** whichever limit is hit first
stops the script.
- `MAX_RETRY_ATTEMPTS=0`, `MAX_LISTEN_DURATION_HOURS=0.05` → retries for 3 minutes, unlimited attempts.
- `MAX_RETRY_ATTEMPTS=5`, `MAX_LISTEN_DURATION_HOURS=2` → stops after 5 retries even with time left.

**`EXTEND_ON_ITERATE`** — when `"YES"`, each retry pushes the window's stop time
forward so messages published later than the original `STOP_TS` are still picked up.
Use this when publication can run late.

### 4.7 Behaviour Flags

| Parameter | Type | Default | Description |
|---|---|---|---|
| `ALLOW_NO_DATA` | string | `"YES"` | `"YES"` = exit cleanly if nothing arrives. `"NO"` = fail hard |
| `ALLOW_ZERO_MESSAGES_PUBLISHED` 🆕 | string, `"YES"`/`"NO"` | `"NO"` | Accept a status message reporting `total_messages_published=0` — see below |
| `WAIT_FOR_SUBMIT` | string | `"NO"` | `"YES"` = wait for an upstream submit signal before consuming |
| `STREAMING_VERBOSE` | string bool | `"True"` | Detailed vs. minimal logging |
| `STREAMING_STORE_MIDLAYER` | string bool | `"False"` | `"True"` = keep intermediate mid-layer data alongside final output |
| `STREAMING_REPROCESS` | string bool | `"False"` | `"True"` = re-read messages even if offsets were already committed |
| `STREAMING_PAGE_SIZE_LIMIT_BYTES` | string integer | `"8000"` | Max batch size per read page |
| `COMMIT_CNT` | string integer | `"50"` | Commit Kafka offsets every N messages |

**`ALLOW_ZERO_MESSAGES_PUBLISHED` 🆕 — detail**

Some topics legitimately have zero business messages on certain days. Before this flag,
a status message correctly reporting `total_messages_published=0` (the sum of
`numberOfMessagesPublished` across all validated instances) always hard-exited.

| Value | Behaviour |
|---|---|
| `"NO"` (default) | `total = 0` → logs `METADATA ERROR: total_messages_published is 0 …` and hard-exits |
| `"YES"` | `total = 0` → accepted; logs at info level and writes the metadata file normally with `total_messages_published=0` |

```json
"ALLOW_ZERO_MESSAGES_PUBLISHED": "YES"
```

This feeds directly into `00_get_kafka.py` as `EXPECTED_COUNT=0` — see the
[`EXPECTED_COUNT == 0` special case](#expected_count--0-special-case) below, which was
built specifically to pair with this flag so a legitimate "zero published today" day
doesn't get stuck failing (or burn the full retry window) on either side of the pipeline.

---

## 5. Parameters — `00_get_kafka.py`

### 5.1 Kafka Connection

| Parameter | Type | Example | Description |
|---|---|---|---|
| `KAFKA_USER` | object, keyed by mandator | `{"022": "admin"}` | Kafka username |
| `STREAMING_KAFKA_BROKER` | object, keyed by mandator | `{"022": "localhost:9092"}` | Broker address |
| `STREAMING_KAFKA_INFLOW_TOPIC` | object, keyed by mandator | `{"022": "business-topic"}` | Topic to consume **business** messages from |

### 5.2 Avro / Schema Registry

| Parameter | Type | Default | Description |
|---|---|---|---|
| `AVRO_SCHEMA_REGISTRY` | string | — | Schema registry address |
| `AVRO_SCHEMA_FILE` | string | `"LATEST"` | `"LATEST"` fetches the newest schema at startup; a pinned version number reprocesses older data |
| `AVRO_COLUMNS` | string (comma list) | `""` | Columns to extract. Empty = all |
| `INPUT_DATA` | string | `"AVRO"` | Format on the topic |
| `OUTPUT_DATA` | string | `"JSON"` | Output file format (JSON = one object per line) |

### 5.3 Time Window

Same mechanics as the status-messages script (§4.3) — messages outside the window are
never read.

| Parameter | Type | Example | Description |
|---|---|---|---|
| `START_TS` | `HH:MM:SS` | `"16:00:00"` | Window start time-of-day |
| `START_DT_OFFSET` | string integer | `"0"` | Days subtracted from `ASOF_DT` for start date |
| `STOP_TS` | `HH:MM:SS` | `"16:00:00"` | Window end time-of-day |
| `STOP_DT_OFFSET` | string integer | `"-1"` | Days subtracted from `ASOF_DT` for end date |

Per-mandator override via `LOCATION_TIME_WINDOW` (same shape as §4.3).

**Example** (`ASOF_DT=2026-04-22`): `window_start = 2026-04-22 16:00:00`,
`window_end = 2026-04-23 16:00:00` — one full trading day, yesterday's close to today's close.

> **Important:** the window only finds the Kafka *offset* range to scan — it filters by
> the Kafka message timestamp, not by any payload field.

### 5.4 Output Format

| Parameter | Type | Default | Description |
|---|---|---|---|
| `SEPERATOR` | string | `"\|"` | Column delimiter |
| `DECIMAL_CONV` | string | `"2"` | `"2"` = disabled. `"YES"` = convert decimals |
| `DECIMAL_SCALE` | string integer | `"2"` | Decimal places when enabled |

### 5.5 Message Filtering

| Parameter | Type | Default | Description |
|---|---|---|---|
| `PRE_FILTER_VALUES` 🆕 | object `{field: value \| [values]}` | `{}` | Extra filter on every consumed message — see below |
| `VALIDATION_FILTER_VALUES` 🆕 | object, same shape | `{}` | Filters only what *counts*, not what's written — see below |
| `METADATA_FILTER_ALLOW_NULL_FIELDS` 🆕 | list of strings | `[]` | Which metadata filter fields may be null without excluding the message — see [§6](#6-shared-metadata-bridge-parameters) |

**`PRE_FILTER_VALUES` 🆕 — detail**

Applied on top of the metadata-driven filter, to every consumed business message. All
keys AND together; a key's value may be a single string (exact match) or a list
(OR-match). Field names support dot-notation for nested fields.

```json
"PRE_FILTER_VALUES": {
    "timelines": ["EOD", "ITD"],
    "region": "EMEA"
}
```
Passes only if `timelines` is `EOD` or `ITD` **and** `region` is exactly `EMEA`.

- **Case-insensitive on both sides** — config keys are lowercased at load time, and
  values are uppercased before comparison, so `"EOD"` matches `"eod"`, `"Eod"`, etc.
- **`{"timelines": ""}` is a footgun, not an "off switch"** — it means "only match an
  empty string," silently excluding every real value. The script logs a startup
  `WARNING` when it detects this. To disable a filter, delete the key.
- **Config shape is validated at startup** — each value must be a string/number or a
  list of them, never a nested object. An invalid shape exits with code `9` and a clear
  `CONFIG ERROR`, not a raw traceback or a silent zero-match.
- A message that fails `PRE_FILTER_VALUES` is dropped entirely: never written, never counted.

**`VALIDATION_FILTER_VALUES` 🆕 — detail**

A second, independent filter that controls only what counts toward `EXPECTED_COUNT` —
it never affects what's written to the output file. Use it when you must collect
*every* message from the topic but only a subset should count toward the expected total.

```json
"PRE_FILTER_VALUES": {},
"VALIDATION_FILTER_VALUES": {"timelines": ["EOD", "ITD"]}
```
Every message on the topic is written to the output file. Only messages where
`timelines` is `EOD` or `ITD` count toward `EXPECTED_COUNT`.

Internally: every written message increments `filtered_count` (a "rows written" tally,
logging only). A second counter, `validation_count`, increments only when the message
also matches `VALIDATION_FILTER_VALUES` (or unconditionally if it's empty) —
`validation_count` is what the retry/tolerance loop actually compares against
`EXPECTED_COUNT`.

> **Interaction warning:** `PRE_FILTER_VALUES` runs first and drops messages before
> `VALIDATION_FILTER_VALUES` ever sees them. If both are set to *different* criteria,
> the effective validation count is the **intersection** of both filters. If you need
> `VALIDATION_FILTER_VALUES` to apply independently, leave `PRE_FILTER_VALUES` empty (`{}`).

The script logs the divergence in two places whenever `filtered_count` and
`validation_count` can differ: a startup `NOTE`, and an `Output summary` block at
acceptance time showing rows written vs. rows validated vs. expected.

### 5.6 Retry, Timing & Over-Count Handling

| Parameter | Type | Default | Description |
|---|---|---|---|
| `MAX_LISTEN_DURATION_HOURS` | string float | `"2"` | Total time allowed before giving up |
| `RETRY_WAIT_SECONDS` | string integer | `"300"` | Sleep between retry attempts |
| `STABLE_COUNT_REQUIRED_ATTEMPTS` 🆕 | string integer | `"2"` | (`STABILITY` mode) consecutive identical reads required before accepting |
| `OVER_COUNT_BEHAVIOR` 🆕 | string, `"STABILITY"` \| `"WAIT"` | `"STABILITY"` | How to decide when to accept once actual ≥ expected — see below |
| `OVER_COUNT_WAIT_MINUTES` 🆕 | string integer | `"5"` | (`WAIT` mode only) minutes to wait after first crossing expected |

**`STABLE_COUNT_REQUIRED_ATTEMPTS` 🆕 — detail**

In `STABILITY` mode, the number of consecutive retries that must read the **same**
count before accepting — protects against accepting mid-flight while more messages
are still arriving.

```
Attempt 1: count = 1000 → streak 1/2 → retry
Attempt 2: count = 1000 → streak 2/2 → CONFIRMED → accept
```

**`OVER_COUNT_BEHAVIOR` 🆕 — detail**

Controls acceptance once the actual count reaches or exceeds `EXPECTED_COUNT`. An exact
match (`count == expected`) always accepts immediately in either mode, no timer/streak needed.

| Mode | Behaviour |
|---|---|
| `STABILITY` (default) | Wait for `STABLE_COUNT_REQUIRED_ATTEMPTS` consecutive identical reads, then accept |
| `WAIT` | On first crossing expected, start one fixed `OVER_COUNT_WAIT_MINUTES` timer. Accept whatever count is present when it expires — growing or not |

```
STABILITY, expected=1000:
  Attempt 1: 1000 → streak 1/2
  Attempt 2: 1050 → count changed → streak resets to 1/2
  Attempt 3: 1050 → streak 2/2 → accept 1050

WAIT, expected=1000, OVER_COUNT_WAIT_MINUTES=5:
  T+0:00  count=1000 → first crossing → start 5-min timer
  T+2:00  count=1200 → timer not reset/extended
  T+5:00  timer expires → accept 1200
```

**Use `WAIT` instead of `STABILITY`** when a bursty publisher could keep the count
changing indefinitely — `STABILITY` might never see two identical reads in a row.
`WAIT` guarantees a bounded finish time.

```json
"OVER_COUNT_BEHAVIOR": "WAIT",
"OVER_COUNT_WAIT_MINUTES": 5
```

### 5.7 Behaviour Flags

| Parameter | Type | Default | Description |
|---|---|---|---|
| `ALLOW_NO_DATA` | string | `"YES"` | `"YES"` = exit cleanly if nothing found. `"NO"` = fail hard |
| `WAIT_FOR_SUBMIT` | string | `"NO"` | `"YES"` = wait for the upstream status message before starting |
| `VALIDATE_TOPIC_MANDATOR` | string | `"NO"` | `"YES"` = verify topic has data for the expected mandator first |
| `STREAMING_TIMESTAMP_KEY` | string | `"std_enqueueTime"` | Payload field used as the message timestamp |
| `STREAMING_TIMESTAMP_FORMAT` | string | `"%Y-%m-%dT%H:%M:%S%z"` | Format string for parsing that field |
| `STREAMING_WAIT_UNTIL_DONE_HOURS` | string integer | `"1"` | Max hours to wait for upstream completion signal |
| `STREAMING_IDLE_TIMEOUT_MINUTES` | string integer | `"2"` | Stop consuming after this many minutes of silence |
| `STREAMING_VERBOSE` | string bool | `"True"` | Detailed vs. minimal logging |
| `STREAMING_STORE_MIDLAYER` | string bool | `"False"` | Keep intermediate mid-layer data |
| `COMMIT_CNT` | string integer | `"50"` | Commit offsets every N messages |

---

## 6. Shared Metadata-Bridge Parameters

These parameters describe the **handoff file** between the two scripts and must be set
**identically in both config files** — `status_messages_config.json` writes the file
using these settings, and `get_kafka_config.json` reads it back using the same settings.

| Parameter | Type | Default | Appears in |
|---|---|---|---|
| `STATUS_MESSAGES_FEED_NAME` | string | `"CPSB4Q00"` | both |
| `METADATA_FILE_SUFFIX` | string | `"_metadata.txt"` | both |
| `METADATA_OUTPUT_PATH` | string | `""` (empty) | status-messages config only |
| `METADATA_FILTER_COLUMNS` | list of strings | `["mandatorCode", "businessDate", "reconciliationGroupId"]` | both |
| `METADATA_FILTER_FIELD_MAP` | object | see below | both |
| `METADATA_FILTER_ALLOW_NULL_FIELDS` 🆕 | list of strings | `[]` | get-kafka config only |
| `METADATA_COUNT_FIELD` | string | `"total_messages_published"` | both |
| `METADATA_COUNT_TOLERANCE_PCT` | string float | `"10"` | both |
| `LOCATION_TOLERANCE_PCT` | object, keyed by mandator | `{"022": "10"}` | both |

### `STATUS_MESSAGES_FEED_NAME` + `METADATA_FILE_SUFFIX`

Together they build the metadata filename:
`{STATUS_MESSAGES_FEED_NAME}_{ASOF_DT}{METADATA_FILE_SUFFIX}` → e.g.
`CPSB4Q00_2026-04-22_metadata.txt`. Must match on both sides.

### `METADATA_OUTPUT_PATH`

Directory the status-messages script writes the metadata file into. Empty = same
directory as the main output data file — leave empty unless you deliberately need to
separate metadata from data output, since `00_get_kafka.py` looks for the file using
the same default-path convention.

### `METADATA_FILTER_COLUMNS` + `METADATA_FILTER_FIELD_MAP`

`METADATA_FILTER_COLUMNS` lists which status-message fields get carried into the
metadata file as filter values. `METADATA_FILTER_FIELD_MAP` maps each of those
status-message field names to its equivalent field name in the business message
(the two schemas don't necessarily agree on naming):

```json
"METADATA_FILTER_FIELD_MAP": {
    "mandator":                "mandatorCode",
    "business_date":           "businessDate",
    "reconciliation_group_id": "reconciliationGroupId"
}
```

Left side = name in the status message (and metadata file). Right side = name to match
against in the business message.

### `METADATA_FILTER_ALLOW_NULL_FIELDS` 🆕

Lists which `METADATA_FILTER_COLUMNS` fields (business-message names) may be null or
missing on a message **without excluding it**. Every other field still requires an
exact, non-null match — this only relaxes the null check.

**Why it exists:** some producers legitimately publish intraday (`ITD`) messages with
no `reconciliationGroupId` at all. Before this parameter, any null filter field caused
the whole message to be silently dropped from the output file.

```json
"METADATA_FILTER_COLUMNS": ["mandatorCode", "businessDate", "reconciliationGroupId"],
"METADATA_FILTER_ALLOW_NULL_FIELDS": ["reconciliationGroupId"]
```

`mandatorCode` and `businessDate` must still match exactly and non-null. A null/missing
`reconciliationGroupId` no longer excludes the message from the **output file** — but:

> **This only relaxes what gets *written*, never what gets *counted*.** The
> `validation_count` check always re-applies the full, unrelaxed filter — so a message
> let through only because of this setting is written to the file but never counts
> toward `EXPECTED_COUNT`. True regardless of whether `VALIDATION_FILTER_VALUES` or
> `PRE_FILTER_VALUES` are set.

### `METADATA_COUNT_FIELD`

Field name in the metadata file holding the expected count. The status-messages script
sums `numberOfMessagesPublished` across all validated instances and writes the total
under this key; `00_get_kafka.py` reads it back as `EXPECTED_COUNT`.

### `METADATA_COUNT_TOLERANCE_PCT` / `LOCATION_TOLERANCE_PCT`

How far below the expected count is still accepted as complete.
`LOCATION_TOLERANCE_PCT` is a per-mandator override that wins if present.

```
floor = expected_count - expected_count × (tolerance_pct / 100)

expected=1000, tolerance=10% → floor=900
  actual=950 → 950 >= 900 → ACCEPT
  actual=850 → 850 < 900  → FAIL
```

```json
"LOCATION_TOLERANCE_PCT": { "022": "5" }
```
Mandator `022` uses a 5% floor instead of the global 10%.

#### `EXPECTED_COUNT == 0` special case

The tolerance formula divides by `expected_count`, so it can't apply at `0`.
`log_count_comparison()` handles it explicitly instead:

```
expected=0, actual=0  → EXACT MATCH, accept immediately
expected=0, actual>0  → accept immediately too — "no minimum required when expected is 0"
```

Both still pass through the normal `STABLE_COUNT_REQUIRED_ATTEMPTS` stability check
before committing (a few `RETRY_WAIT_SECONDS`), just not the full
`MAX_LISTEN_DURATION_HOURS` window. This exists specifically to pair with
`ALLOW_ZERO_MESSAGES_PUBLISHED` (§4.7) — before this, `expected=0, actual>0` always
burned the entire retry window before an exhaustion-time check let it through anyway;
same eventual outcome, just slow every time.

---

## 7. Worked End-to-End Example

Baseline: `ASOF_DT=2026-04-22`, mandator `022`, `PRODUCER_FILTER="CLIENT_STRUCTURES"`,
2 expected instances.

```
STEP 1 — kafka_trigger_status_messages.py
  Status topic has:
    Instance 0: mandatorCode=022, businessDate=2026-04-22, reconciliationGroupId=1,
                numberOfMessagesPublished=500
    Instance 1: mandatorCode=022, businessDate=2026-04-22, reconciliationGroupId=1,
                numberOfMessagesPublished=600
  → Both instances present (SEQUENTIAL: 0,1) → PASS
  → total_messages_published = 500 + 600 = 1,100
  → Writes CPSB4Q00_2026-04-22_metadata.txt:
        total_messages_published = 1100
        filter values: mandatorCode=022, businessDate=2026-04-22, reconciliationGroupId=1

STEP 2 — 00_get_kafka.py
  → Reads metadata file → EXPECTED_COUNT = 1100, filters mandatorCode/businessDate/reconciliationGroupId
  → Scans business topic within its own time window
  → Applies PRE_FILTER_VALUES (if any), then the metadata filter
  → Attempt 1: count=1100 → streak 1/2
  → Attempt 2: count=1100 (stable) → streak 2/2 → ACCEPT
  → Writes final output file, commits offsets → EXIT 0
```

---

## 8. Quick Reference — "I want to…"

| I want to… | Change this |
|---|---|
| Change which topic to read from | `STREAMING_KAFKA_INFLOW_TOPIC` |
| Adjust the time window | `LOCATION_TIME_WINDOW` (per-mandator) or `START_TS` / `STOP_TS` |
| Filter status messages to a specific producer | `PRODUCER_FILTER` |
| Validate non-sequential / arbitrary instance IDs | `INSTANCE_VALIDATION_MODE = "UNIQUE_COUNT"` |
| Accept status messages from any producer | `PRODUCER_FILTER = ""` |
| Wait longer for late messages | Increase `MAX_LISTEN_DURATION_HOURS` |
| Include business messages after the window end | `EXTEND_ON_ITERATE = "YES"` |
| Limit retries to a fixed number | `MAX_RETRY_ATTEMPTS` |
| Exit cleanly when no data arrives | `ALLOW_NO_DATA = "YES"` |
| Fail when no data arrives | `ALLOW_NO_DATA = "NO"` |
| Accept a status message reporting zero published messages | `ALLOW_ZERO_MESSAGES_PUBLISHED = "YES"` |
| Avoid dropping messages with a null `reconciliationGroupId` | `METADATA_FILTER_ALLOW_NULL_FIELDS = ["reconciliationGroupId"]` |
| Add extra business-message filtering beyond the metadata filter | `PRE_FILTER_VALUES` |
| Count only a subset of messages toward `EXPECTED_COUNT` without dropping the rest | `VALIDATION_FILTER_VALUES` |
| Stop waiting forever on a bursty over-count topic | `OVER_COUNT_BEHAVIOR = "WAIT"` + `OVER_COUNT_WAIT_MINUTES` |
| Require more/fewer stable reads before accepting | `STABLE_COUNT_REQUIRED_ATTEMPTS` |
| Control how often offsets are committed | `COMMIT_CNT` |
| Control where the metadata file is written | `METADATA_OUTPUT_PATH` |
| Change the expected-count tolerance | `METADATA_COUNT_TOLERANCE_PCT` or `LOCATION_TOLERANCE_PCT` |

---

## 9. Local Testing Scripts (not part of the pipeline)

`run_local.sh`, `run_get_kafka_local.sh`, `start_kafka.sh`, `stop_kafka.sh`, and
`reset_kafka.sh` are **local Podman-simulation helpers only** — they exist to run and
reset a local Redpanda instance and drive the two scripts against it for testing. They
are not part of the production DSF framework, are not deployed anywhere, and set no
parameters beyond what's documented above (they just export the framework environment
variables the scripts expect and point them at `localhost`). See `RUNBOOK.md` for how
to use them; they're intentionally excluded from the parameter reference above.
