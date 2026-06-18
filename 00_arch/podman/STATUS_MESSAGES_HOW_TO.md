# kafka_trigger_status_messages.py — Configuration & Behaviour Guide

This guide explains every configurable parameter and shows exactly how the script
behaves in each scenario. Read the **Parameters** section to understand what each
setting does, then read the **Scenarios** section to see worked examples.

---

## How it fits together (30-second overview)

1. Script connects to the **status/inflow Kafka topic** and scans a configured **time window**.
2. It consumes status messages produced by the upstream publisher (e.g. `CLIENT_STRUCTURES`).
3. It validates that all expected instances have reported in and reconciles the total message count.
4. It writes a **metadata file** (expected count + filter values) that `00_get_kafka.py` reads.
5. It writes the consumed status messages to an **output file**.

---

## Parameters Reference

### Kafka Connection

| Parameter | Type | Example | Description |
|---|---|---|---|
| `KAFKA_USER` | object | `{"022": "local"}` | Kafka username, keyed by mandator |
| `STREAMING_KAFKA_BROKER` | object | `{"022": "localhost:9092"}` | Broker address, keyed by mandator |
| `STREAMING_KAFKA_INFLOW_TOPIC` | object | `{"022": "inflow-topic"}` | Topic to consume status messages from, keyed by mandator |

These three are always mandator-specific. The mandator is injected at runtime via
the `DSF_MANDATOR` environment variable.

---

### Avro / Schema Registry

| Parameter | Type | Default | Description |
|---|---|---|---|
| `AVRO_SCHEMA_REGISTRY` | string | — | Schema registry address (e.g. `localhost:8081`) |
| `AVRO_SCHEMA_FILE` / `AVRO_SHCEMA_FILE` | string | `"LATEST"` | Schema version. `"LATEST"` fetches the newest version automatically |
| `AVRO_COLUMNS` | string | `""` | Comma-separated columns to extract. Empty = extract all |
| `INPUT_DATA` | string | `"AVRO"` | Format of messages on the topic |
| `OUTPUT_DATA` | string | `"JSON"` | Format of the output file written to disk |

> Note: the config key is spelled `AVRO_SHCEMA_FILE` (typo in the original framework).
> The script accepts both spellings — `AVRO_SCHEMA_FILE` is preferred.

---

### Time Window

The script only reads messages within a time window. Messages outside the window are ignored.

#### Global defaults

| Parameter | Type | Example | Description |
|---|---|---|---|
| `START_TS` | string `HH:MM:SS` | `"23:59:00"` | Time-of-day for window start |
| `START_DT_OFFSET` | string integer | `"0"` | Days to subtract from ASOF_DT for the start date |
| `STOP_TS` | string `HH:MM:SS` | `"00:00:00"` | Time-of-day for window end |
| `STOP_DT_OFFSET` | string integer | `"-1"` | Days to subtract from ASOF_DT for the end date |

#### Location-specific override

```json
"LOCATION_TIME_WINDOW": {
    "022": {
        "START_TS":        "23:59:00",
        "START_DT_OFFSET": "0",
        "STOP_TS":         "00:00:00",
        "STOP_DT_OFFSET":  "-1"
    }
}
```

If a mandator appears in `LOCATION_TIME_WINDOW`, its values override the global defaults.

**How the window is calculated:**
```
window_start = date(ASOF_DT) + time(START_TS) - timedelta(days=START_DT_OFFSET)
window_end   = date(ASOF_DT) + time(STOP_TS)  - timedelta(days=STOP_DT_OFFSET)
```

**Example** — ASOF_DT = `2026-04-22`, config as above:
```
window_start = 2026-04-22 23:59:00  (ASOF_DT + 0 days)
window_end   = 2026-04-23 00:00:00  (ASOF_DT - (-1) = ASOF_DT + 1 day)
```
Window = the 1-minute publication window around midnight of the business date.

---

### Output Format

| Parameter | Type | Default | Description |
|---|---|---|---|
| `SEPERATOR` | string | `"\|"` | Column delimiter in the output file |
| `DECIMAL_CONV` | string | `"2"` | Decimal conversion. `"2"` = disabled. `"YES"` = convert to fixed decimal |
| `DECIMAL_SCALE` | string integer | `"2"` | Decimal places when `DECIMAL_CONV = "YES"` |

---

### Message Filtering

| Parameter | Type | Default | Description |
|---|---|---|---|
| `PRODUCER_FILTER` | string | `"CLIENT_STRUCTURES"` | Only process messages from this producer. Empty string = accept all producers |
| `VALIDATE_TOPIC_MANDATOR` | string | `"NO"` | `"YES"` = verify topic contains messages for the expected mandator before proceeding |
| `STREAMING_MANDATORY_KEY` | string | `""` | A field that must be present in every message. Empty = no mandatory field check |
| `VALIDATION_REQUIRED_COLUMNS` | string (comma list) | — | Columns that must be present in the output. Used to validate schema completeness |

**`PRODUCER_FILTER` detail:**
Status messages contain a `producer` field. Setting this to `"CLIENT_STRUCTURES"` means
only messages where `producer == "CLIENT_STRUCTURES"` are counted and written to output.
This lets multiple producers publish to the same topic without interfering with each other.

```
Topic contains:
  Message A: producer = "CLIENT_STRUCTURES"  ← included
  Message B: producer = "RISK_ENGINE"        ← excluded (filtered out)
  Message C: producer = "CLIENT_STRUCTURES"  ← included
```

---

### Retry & Timing

| Parameter | Type | Default | Description |
|---|---|---|---|
| `MAX_LISTEN_DURATION_HOURS` | string float | `"2"` | Total time the script is allowed to run before giving up |
| `MIN_LISTENING_DURATION_MINUTES` | string integer | — | Minimum time to keep listening even if all expected data has arrived |
| `MAX_RETRY_ATTEMPTS` | string integer | `"0"` | Maximum number of retry attempts. `0` = unlimited (retry until deadline) |
| `RETRY_WAIT_SECONDS` | string integer | `"300"` | Sleep time between retry attempts |
| `STREAMING_WAIT_UNTIL_DONE_HOURS` | string integer | `"1"` | Maximum hours to wait for the publisher to signal it is done |
| `STREAMING_IDLE_TIMEOUT_MINUTES` | string integer | `"1"` | Stop consuming if no new messages arrive within this many minutes |
| `EXTEND_ON_ITERATE` | string | `"NO"` | `"YES"` = extend the time window forward on each retry iteration |
| `START_FROM_LAST` | string | `"NO"` | `"YES"` = start reading from the last committed offset rather than the window start |

**`EXTEND_ON_ITERATE` detail:**
When `"YES"`, the stop timestamp of the Kafka scan window advances on each retry to
include messages published after the original `STOP_TS`. Use this when messages may
arrive significantly later than the configured window end.

**`MAX_RETRY_ATTEMPTS` vs `MAX_LISTEN_DURATION_HOURS`:**
The script stops when either limit is reached, whichever comes first.
- `MAX_RETRY_ATTEMPTS = 0` with `MAX_LISTEN_DURATION_HOURS = 0.05` → retry for 3 minutes, unlimited attempts
- `MAX_RETRY_ATTEMPTS = 5` with `MAX_LISTEN_DURATION_HOURS = 2` → stop after 5 retries even if time remains

---

### Behaviour Flags

| Parameter | Type | Default | Description |
|---|---|---|---|
| `ALLOW_NO_DATA` | string | `"YES"` | `"YES"` = exit cleanly with no output if no messages found. `"NO"` = fail hard |
| `WAIT_FOR_SUBMIT` | string | `"NO"` | `"YES"` = wait for upstream submit signal before starting to consume |
| `STREAMING_VERBOSE` | string bool | `"True"` | `"True"` = detailed logging. `"False"` = minimal |
| `STREAMING_STORE_MIDLAYER` | string bool | `"False"` | `"True"` = store intermediate mid-layer data alongside final output |
| `STREAMING_REPROCESS` | string bool | `"False"` | `"True"` = re-read messages even if offsets were already committed |
| `STREAMING_PAGE_SIZE_LIMIT_BYTES` | string integer | `"8000"` | Maximum batch size in bytes per read page |
| `COMMIT_CNT` | string integer | `"50"` | Commit Kafka offsets every N messages |

---

### Metadata Output

These parameters control the metadata file written for downstream use by `00_get_kafka.py`.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `STATUS_MESSAGES_FEED_NAME` | string | `"CPSB4Q00"` | Feed name used in the metadata filename |
| `METADATA_FILE_SUFFIX` | string | `"_metadata.txt"` | Suffix appended to the metadata file |
| `METADATA_OUTPUT_PATH` | string | `""` | Directory to write the metadata file. Empty = same directory as the output data |
| `METADATA_FILTER_COLUMNS` | list | `["mandatorCode","businessDate","reconciliationGroupId"]` | Fields from the status message written into the metadata as filter values |
| `METADATA_FILTER_FIELD_MAP` | object | see below | Maps status-message field names to business-message field names |
| `METADATA_COUNT_FIELD` | string | `"total_messages_published"` | Field in the status message containing the expected business message count |
| `METADATA_COUNT_TOLERANCE_PCT` | string float | `"10"` | Allowed shortfall percentage written into the metadata |
| `LOCATION_TOLERANCE_PCT` | object | `{"022": "10"}` | Per-mandator override for `METADATA_COUNT_TOLERANCE_PCT` |

**`METADATA_FILTER_FIELD_MAP` example:**
```json
"METADATA_FILTER_FIELD_MAP": {
    "mandator":                "mandatorCode",
    "business_date":           "businessDate",
    "reconciliation_group_id": "reconciliationGroupId"
}
```
Left side = field name in the **status message**. Right side = field name in the
**downstream business message**. `00_get_kafka.py` reads this map and uses it to
filter business messages that match the status message's mandator, date, and group.

---

## Scenarios

All examples below use these baseline values:
- ASOF_DT = `2026-04-22`
- Mandator = `022`
- `PRODUCER_FILTER` = `"CLIENT_STRUCTURES"`
- Total expected instances = 2 (instanceIndex 0 and 1)
- `MAX_LISTEN_DURATION_HOURS` = `0.05` (3 minutes)
- `RETRY_WAIT_SECONDS` = `5`
- `ALLOW_NO_DATA` = `"YES"`

---

### Scenario 1 — All instances arrive, happy path

**Setup:** Both instances publish before the script starts.

```
Status messages on topic:
  Instance 0: mandatorCode=022, businessDate=2026-04-22, reconciliationGroupId=1,
              numberOfMessagesPublished=500, producer=CLIENT_STRUCTURES
  Instance 1: mandatorCode=022, businessDate=2026-04-22, reconciliationGroupId=1,
              numberOfMessagesPublished=600, producer=CLIENT_STRUCTURES

Script runs:
  → Reads both messages
  → Filters by PRODUCER_FILTER = CLIENT_STRUCTURES → both pass
  → Validates instances: 0 of 2, 1 of 2 → all present ✓
  → Total expected business messages = 500 + 600 = 1,100
  → Writes metadata file: expected=1100, filter values for mandator 022, date 2026-04-22, group 1
  → Writes output file with both status messages
  → Commits offsets → EXIT ✓
```

---

### Scenario 2 — Only one instance arrives (partial data, retry)

**Setup:** Instance 0 arrives. Instance 1 is delayed.

```
Attempt 1:
  → Reads Instance 0 only
  → Validates: instance 1 of 2 missing
  → Under expected → ALLOW_NO_DATA=YES → enter retry mode
  Log: "No initial data found. Entering wait-and-retry mode."

[Instance 1 arrives at T+30s]

Attempt 2 (T+35s):
  → Reads Instance 0 and Instance 1
  → Both instances present ✓
  → Total = 1,100 → write metadata + output → EXIT ✓
```

---

### Scenario 3 — Wrong producer on topic (filtered out)

**Setup:** Two messages on the topic — one from `CLIENT_STRUCTURES`, one from `RISK_ENGINE`.

```
Topic messages:
  Message A: producer=CLIENT_STRUCTURES, numberOfMessagesPublished=1100
  Message B: producer=RISK_ENGINE,       numberOfMessagesPublished=200

Script runs with PRODUCER_FILTER = "CLIENT_STRUCTURES":
  → Message A: passes filter ✓
  → Message B: EXCLUDED (producer does not match) ✗

  → Only Message A counted → total expected = 1,100
  → Output file contains only Message A
  → Metadata written with expected=1100
```

Setting `PRODUCER_FILTER = ""` would include both messages.

---

### Scenario 4 — Wrong mandator (filtered out)

**Setup:** Topic has messages for mandator `022` and mandator `099`.
Script runs for mandator `022`.

```
Topic messages:
  Message A: mandatorCode=022  ← matches DSF_MANDATOR
  Message B: mandatorCode=099  ← does not match

Script:
  → VALIDATE_TOPIC_MANDATOR = "YES": checks topic has data for 022 ✓
  → Filters by mandatorCode = 022
  → Message B excluded
  → Processes only Message A
```

---

### Scenario 5 — No data at all, ALLOW_NO_DATA = "YES"

**Setup:** Topic is empty. No messages arrive within `MAX_LISTEN_DURATION_HOURS`.

```
Attempt 1: no messages found
Attempt 2: no messages found
...
[Deadline reached — 3 minutes]

ALLOW_NO_DATA = "YES" → exit cleanly
No output file written, no metadata file written
Exit code: 0 ✓
Log: "No data consumed."
```

> Downstream `00_get_kafka.py` will also exit cleanly because it checks for the
> metadata file and `ALLOW_NO_DATA` on its side too.

---

### Scenario 6 — No data, ALLOW_NO_DATA = "NO"

```
Same sequence as Scenario 5 but ALLOW_NO_DATA = "NO":
[Deadline reached] → FAIL ✗
Log: "No data consumed from any partition !!!"
Exit code: 1
```

---

### Scenario 7 — Messages arrive after the time window

**Setup:** Status messages have `eventTimestamp` = `2026-04-21 10:00:00` (yesterday).
ASOF_DT = `2026-04-22`. Window = `2026-04-22 23:59:00 → 2026-04-23 00:00:00`.

```
offsets_for_times(window_start = 2026-04-22 23:59:00) → None
  (no messages with Kafka timestamp >= window start)

→ No data found in window
→ Retry until deadline
→ ALLOW_NO_DATA=YES → exit cleanly, or NO → FAIL

Messages are NOT seen because their Kafka timestamp is outside the window.
```

**Fix:** Produce messages with a timestamp that falls within the configured window,
or adjust `START_TS` / `STOP_TS` to cover the actual publication time.

---

### Scenario 8 — Multiple reconciliation groups on the same date

**Setup:** Publisher sends two groups: `reconciliationGroupId=1` and `reconciliationGroupId=2`.
Script is configured to process `reconciliationGroupId=1` only via `METADATA_FILTER_COLUMNS`.

```
Topic messages:
  Group 1, Instance 0: reconciliationGroupId=1, numberOfMessagesPublished=500
  Group 1, Instance 1: reconciliationGroupId=1, numberOfMessagesPublished=600
  Group 2, Instance 0: reconciliationGroupId=2, numberOfMessagesPublished=300

Script selects max reconciliationGroupId found for the mandator/date combination
(or uses the filter value derived from the metadata — depends on config).

  → Group 1 messages → total expected = 1,100
  → Group 2 messages → separate run or separate feed

Metadata written: expected=1100, reconciliationGroupId=1
```

---

### Scenario 9 — EXTEND_ON_ITERATE = "YES" (late messages beyond window end)

**Setup:** Window end = `2026-04-23 00:00:00`. Publisher sends last message at `00:05:00`
(5 minutes after window end). `EXTEND_ON_ITERATE = "YES"`.

```
Attempt 1:
  → Scans up to 00:00:00 → finds instances 0 only (instance 1 not yet published)
  → Extends window end forward → new window end = now + buffer

Attempt 2:
  → Scans up to extended end → finds both instances (00:05:00 < extended end) ✓
  → All instances present → write output + metadata → EXIT ✓
```

Without `EXTEND_ON_ITERATE = "YES"`, the message at `00:05:00` would be outside the
window and never read.

---

### Scenario 10 — STREAMING_IDLE_TIMEOUT_MINUTES kicks in

**Setup:** Script is consuming. Messages stop arriving. After `STREAMING_IDLE_TIMEOUT_MINUTES`
(1 minute) of silence, the script stops consuming and moves to validation.

```
T+0:00  → messages flowing
T+1:00  → no new messages for 1 minute → idle timeout fires
         → stop consuming, move to validation with messages collected so far
         → validate count → accept or retry
```

Useful to prevent the consumer hanging indefinitely waiting for more messages when the
publisher has actually finished.

---

## Quick reference — which setting to change

| I want to... | Change this |
|---|---|
| Change which topic to read from | `STREAMING_KAFKA_INFLOW_TOPIC` |
| Adjust the time window | `LOCATION_TIME_WINDOW` (per-mandator) or `START_TS` / `STOP_TS` |
| Filter to a specific producer | `PRODUCER_FILTER` |
| Accept messages from any producer | Set `PRODUCER_FILTER = ""` |
| Wait longer for late messages | Increase `MAX_LISTEN_DURATION_HOURS` |
| Include messages after the window end | Set `EXTEND_ON_ITERATE = "YES"` |
| Limit retries to a fixed number | Set `MAX_RETRY_ATTEMPTS` to that number |
| Exit cleanly when no data arrives | Set `ALLOW_NO_DATA = "YES"` |
| Fail when no data arrives | Set `ALLOW_NO_DATA = "NO"` |
| Control how often offsets are committed | Adjust `COMMIT_CNT` |
| Control where the metadata file is written | Set `METADATA_OUTPUT_PATH` |
| Change the expected count tolerance | `METADATA_COUNT_TOLERANCE_PCT` or `LOCATION_TOLERANCE_PCT` |
