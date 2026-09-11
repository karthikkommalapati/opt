# 00_get_kafka.py — Configuration & Behaviour Guide

This guide explains every configurable parameter and shows exactly how the script
behaves in each scenario. Read the **Parameters** section to understand what each
setting does, then read the **Scenarios** section to see worked examples.

---

## How it fits together (30-second overview)

1. Script reads a **metadata file** left by `kafka_trigger_status_messages.py`.
   That file says: "I expect N messages with these filter values."
2. Script scans the **business Kafka topic** within a configured **time window**.
3. It filters messages that match the metadata filter values and counts them.
4. It compares the actual count to the expected count and either **accepts**,
   **retries**, or **fails** based on the tolerance and retry settings.

---

## Parameters Reference

### Kafka Connection

| Parameter | Type | Example | Description |
|---|---|---|---|
| `KAFKA_USER` | object | `{"022": "admin"}` | Kafka username, keyed by mandator |
| `STREAMING_KAFKA_BROKER` | object | `{"022": "localhost:9092"}` | Broker address, keyed by mandator |
| `STREAMING_KAFKA_INFLOW_TOPIC` | object | `{"022": "business-topic"}` | Topic to consume business messages from, keyed by mandator |

These three are always mandator-specific. The mandator is injected at runtime via
the `DSF_MANDATOR` environment variable.

---

### Avro / Schema Registry

| Parameter | Type | Default | Description |
|---|---|---|---|
| `AVRO_SCHEMA_REGISTRY` | string | — | Schema registry address (e.g. `localhost:8081`) |
| `AVRO_SCHEMA_FILE` | string | `"LATEST"` | Schema version to use. `"LATEST"` fetches the newest registered schema automatically |
| `AVRO_COLUMNS` | string | `""` | Comma-separated list of Avro columns to extract. Leave empty to extract all columns |
| `INPUT_DATA` | string | `"AVRO"` | Format of messages on the topic. `"AVRO"` is the expected production value |
| `OUTPUT_DATA` | string | `"JSON"` | Format of the output file written to disk. `"JSON"` writes one JSON object per line |

**`AVRO_SCHEMA_FILE` detail:**
- `"LATEST"` — script fetches the current latest schema from the registry at startup.
  If the schema changes mid-run, the script uses the version it fetched at startup.
- A specific version number — pins to that version (useful if you need to reprocess
  data produced under an older schema).

---

### Time Window

The script does not consume all messages on the topic — it scans only within a time
window defined by these parameters. Messages whose Kafka timestamp falls outside the
window are never read.

#### Global defaults (apply when no location-specific window is set)

| Parameter | Type | Example | Description |
|---|---|---|---|
| `START_TS` | string `HH:MM:SS` | `"16:00:00"` | Time-of-day for the window start |
| `START_DT_OFFSET` | string integer | `"0"` | Days to subtract from ASOF_DT for the start date |
| `STOP_TS` | string `HH:MM:SS` | `"16:00:00"` | Time-of-day for the window end |
| `STOP_DT_OFFSET` | string integer | `"-1"` | Days to subtract from ASOF_DT for the end date |

#### Location-specific override

```json
"LOCATION_TIME_WINDOW": {
    "022": {
        "START_TS":        "16:00:00",
        "START_DT_OFFSET": "0",
        "STOP_TS":         "16:00:00",
        "STOP_DT_OFFSET":  "-1"
    }
}
```

If a mandator appears in `LOCATION_TIME_WINDOW`, its values override the global
`START_TS` / `STOP_TS` / `START_DT_OFFSET` / `STOP_DT_OFFSET` settings.

**How the window is calculated:**

```
window_start = date(ASOF_DT) + time(START_TS) - timedelta(days=START_DT_OFFSET)
window_end   = date(ASOF_DT) + time(STOP_TS)  - timedelta(days=STOP_DT_OFFSET)
```

**Example** — ASOF_DT = `2026-04-22`, config as above:
```
window_start = 2026-04-22 16:00:00  (ASOF_DT + 0 days)
window_end   = 2026-04-23 16:00:00  (ASOF_DT - (-1) day = ASOF_DT + 1 day)
```

The window covers one full trading day: yesterday's 16:00 close to today's 16:00 close.

> **Important:** The window is used only to find the Kafka *offset* range to scan.
> It does not filter individual messages by their payload timestamp field — it filters
> by the Kafka message timestamp set when the message was produced.

---

### Output Format

| Parameter | Type | Default | Description |
|---|---|---|---|
| `SEPERATOR` | string | `"\|"` | Column delimiter in the output file |
| `DECIMAL_CONV` | string | `"2"` | Decimal conversion mode. `"2"` = disabled (pass through as-is). `"YES"` = convert decimal fields |
| `DECIMAL_SCALE` | string integer | `"2"` | Number of decimal places when `DECIMAL_CONV` = `"YES"` |

---

### Metadata & Count Validation

The script reads a metadata file written by `kafka_trigger_status_messages.py` to know
how many messages to expect and which field values to filter on.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `STATUS_MESSAGES_FEED_NAME` | string | `"CPSB4Q00"` | Feed name of the upstream status-messages script. Used to locate the metadata file |
| `METADATA_FILE_SUFFIX` | string | `"_metadata.txt"` | Suffix appended to the metadata filename |
| `METADATA_COUNT_FIELD` | string | `"total_messages_published"` | Field in the metadata file that holds the expected message count |
| `METADATA_FILTER_COLUMNS` | list | `["mandatorCode","businessDate","reconciliationGroupId"]` | Fields in the **business messages** used to filter which messages to count |
| `METADATA_FILTER_FIELD_MAP` | object | see below | Maps field names in the **status message** to field names in the **business message** |
| `METADATA_FILTER_ALLOW_NULL_FIELDS` | list | `[]` | `METADATA_FILTER_COLUMNS` fields allowed to be null/missing on a message without excluding it — see below |
| `METADATA_COUNT_TOLERANCE_PCT` | string float | `"10"` | Percentage by which actual count may fall below expected and still be accepted |
| `LOCATION_TOLERANCE_PCT` | object | `{"022": "10"}` | Per-mandator override for `METADATA_COUNT_TOLERANCE_PCT` |

**`METADATA_FILTER_FIELD_MAP` example:**
```json
"METADATA_FILTER_FIELD_MAP": {
    "mandator":                 "mandatorCode",
    "business_date":            "businessDate",
    "reconciliation_group_id":  "reconciliationGroupId"
}
```
Left side = field name in the status message. Right side = field name in the business
message. The script reads filter values from the status message and then applies them
to the business messages.

**`METADATA_FILTER_ALLOW_NULL_FIELDS`** — normally, if a `METADATA_FILTER_COLUMNS` field
is null or missing on a business message, `message_matches_filters` excludes the entire
message (never written to the output file). This is a problem for producers that
legitimately publish intraday (`ITD`) messages with no `reconciliationGroupId` at all —
those messages were being silently dropped rather than "having no group."

```json
"METADATA_FILTER_COLUMNS": ["mandatorCode", "businessDate", "reconciliationGroupId"],
"METADATA_FILTER_ALLOW_NULL_FIELDS": ["reconciliationGroupId"]
```
With this set, a null/missing `reconciliationGroupId` no longer excludes the message —
`mandatorCode` and `businessDate` still must match exactly and non-null. The message is
written to the output file.

**Critically, this only relaxes what gets *written*, not what gets *counted*.** The
`validation_count` logic (see `VALIDATION_FILTER_VALUES` just below) always re-checks the
message against the full, unrelaxed `filter_values` — so a message let through only
because of `METADATA_FILTER_ALLOW_NULL_FIELDS` is written to the file but never
increments `validation_count`, and therefore never affects the `EXPECTED_COUNT`
comparison. This holds whether `VALIDATION_FILTER_VALUES` and `PRE_FILTER_VALUES` are set
or left empty — the strict re-check is independent of both.

**`LOCATION_TOLERANCE_PCT` overrides `METADATA_COUNT_TOLERANCE_PCT`:**
If a mandator appears in `LOCATION_TOLERANCE_PCT`, that value is used instead.
```json
"LOCATION_TOLERANCE_PCT": { "022": "5" }   ← mandator 022 uses 5%, not the global 10%
```

**How the tolerance floor works:**
```
floor = expected_count - expected_count × (tolerance_pct / 100)

Example: expected=1000, tolerance=10%  →  floor = 900
  actual=950 → 950 >= 900 → ACCEPT (within tolerance)
  actual=850 → 850 < 900  → FAIL   (below tolerance)
```

**Special case — `EXPECTED_COUNT == 0`:** the floor formula divides by `expected_count`,
which doesn't work at `0`. `log_count_comparison()` (~line 330-358) handles this
explicitly instead:

```text
expected=0, actual=0  →  EXACT MATCH, accept immediately
expected=0, actual>0  →  accept too — "no minimum required when expected count is 0"
```

Both cases return "at or above" and still go through the normal
`STABLE_COUNT_REQUIRED_ATTEMPTS` stability check before committing — so `expected=0,
actual>0` isn't instant, but it's bounded by the stability window (a few
`RETRY_WAIT_SECONDS`), not the full `MAX_LISTEN_DURATION_HOURS`. See Scenario 14 below
for the full trade-off and when this fast-accept is (and isn't) the right behaviour for
a given feed.

**`PRE_FILTER_VALUES`** — additional field filter applied on top of the metadata-driven
filter above, evaluated per message in `message_matches_filters`. A message that doesn't
match is dropped entirely: not written to the output file, and not counted anywhere.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `PRE_FILTER_VALUES` | object — `{field: value or [values]}` | `{}` | Extra field-level filter on consumed business messages. All keys AND together; a key's value may be a single string (exact match) or a list (OR-match — passes if the message matches any value in the list) |

```json
"PRE_FILTER_VALUES": {"timelines": ["EOD", "ITD"]}
```
Passes messages where `timelines` is `EOD` or `ITD`. Field names support dot-notation
for nested fields (e.g. `"status.timeline"`).

> **Config keys are case-insensitive.** Field-name keys in `PRE_FILTER_VALUES` (and
> `VALIDATION_FILTER_VALUES` below) are lowercased when the config loads, so
> `"TIMELINES"` and `"timelines"` behave identically. This exists because the actual
> message field name comes from the Avro schema and is typically lowercase — you can
> write the config key in whichever case your team's convention prefers.
>
> **Values are also matched case-insensitively.** `message_matches_filters` uppercases
> both the config's expected value and the message's actual value before comparing, so
> `"EOD"` in config matches `"eod"`, `"Eod"`, `"EOD"`, etc. in the message. Pick one case
> for your config (uppercase is the convention used throughout this doc) — the actual
> data can be any case and will still match.

> **Empty-string values do NOT disable a filter.** `{"timelines": ""}` does not mean
> "no filter" — it means "only match messages where `timelines` is itself an empty
> string," which silently excludes every message with a real value. The script logs a
> startup `WARNING` if it detects this (`00_get_kafka.py:733-749`). To disable filtering
> on a field, remove the key entirely (or set the whole filter to `{}`).

> **Config shape is validated at startup.** `PRE_FILTER_VALUES` and
> `VALIDATION_FILTER_VALUES` must each be a JSON object whose values are a single
> string/number or a list of strings/numbers — not a nested object, not a list of
> objects. An invalid shape hard-exits (code 9) with a `CONFIG ERROR` log message and
> format examples, instead of crashing with a raw traceback or silently matching
> nothing. See `load_and_validate_filter_config()` (`00_get_kafka.py:288-322`).

**`VALIDATION_FILTER_VALUES`** — a second, independent filter that controls only what
counts toward the expected-count validation, not what gets written to the output file.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `VALIDATION_FILTER_VALUES` | object — `{field: value or [values]}` | `{}` | Same shape/matching rules as `PRE_FILTER_VALUES`. When empty, every written message counts toward validation (today's behavior). When set, only messages matching it are counted toward `EXPECTED_COUNT` — but every message is still written to the output file |

**Why this exists:** `PRE_FILTER_VALUES` filters what's written to the file — if a
message doesn't match, it's dropped and never seen again. Sometimes you want the
opposite: collect *every* message from the topic into the output file, but only
validate the count against a subset (e.g. "collect everything, but the expected count
of 1000 only applies to `timelines=EOD/ITD`, not other timeline values also on the
topic").

```json
"PRE_FILTER_VALUES": {},
"VALIDATION_FILTER_VALUES": {"timelines": ["EOD", "ITD"]}
```
All messages on the topic are written to the `.par` file. Of those, only ones where
`timelines` is `EOD` or `ITD` count toward matching `EXPECTED_COUNT`.

> **Interaction warning:** `PRE_FILTER_VALUES` runs first in the per-message loop and
> drops non-matching messages before `VALIDATION_FILTER_VALUES` ever sees them. If both
> are set with *different* criteria, `VALIDATION_FILTER_VALUES` only ever evaluates
> messages that already survived `PRE_FILTER_VALUES` — the effective validation count is
> the intersection of both filters, not `VALIDATION_FILTER_VALUES` alone. In practice:
> leave `PRE_FILTER_VALUES` empty on any topic where you're using
> `VALIDATION_FILTER_VALUES` for different criteria.

Internally this is `filtered_count` vs `validation_count`: `filtered_count` (incremented
at the point each message is written) is purely a "how many rows landed in the file"
tally used only in logging. `validation_count` (incremented right after, matched against
`VALIDATION_FILTER_VALUES` or unconditionally if it's empty) is the number actually
compared against `EXPECTED_COUNT` throughout the retry/stability/tolerance logic. They
are equal unless `VALIDATION_FILTER_VALUES` is set.

**Log visibility — two places make the row-count/validation-count split obvious:**

1. **Startup banner** (`00_get_kafka.py:757-764`) — fires only when `VALIDATION_FILTER_VALUES`
   is set and differs from `PRE_FILTER_VALUES`, warning upfront that the output file will
   contain more rows than the validated count:
   ```
   NOTE: VALIDATION_FILTER_VALUES is set and differs from PRE_FILTER_VALUES. All messages
   consumed from the topic will be written to the output file. Only messages matching
   VALIDATION_FILTER_VALUES count toward EXPECTED_COUNT. Expect the output file's row
   count to exceed the validated count — see the 'Output summary' logged at acceptance
   for the exact numbers.
   ```
2. **Output summary at acceptance** (`00_get_kafka.py:1536-1548`) — logged every time a run
   is accepted, with an explicit NOTE line whenever the two counts diverge:
   ```
   Output summary:
     Rows written to output file    : 1300
     Rows counted for validation    : 1000
     Expected count (metadata)      : 1000
     NOTE: file row count (1300) differs from the validated count (1000) because
     VALIDATION_FILTER_VALUES={'timelines': ['EOD', 'ITD']} narrows which messages
     count toward EXPECTED_COUNT — all consumed messages are still written to the file.
   ```

---

### Retry & Timing

These parameters control how long the script waits for messages and how it handles
situations where the count is not yet at the expected level.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `MAX_LISTEN_DURATION_HOURS` | string float | `"2"` | Total time the script is allowed to run before giving up |
| `RETRY_WAIT_SECONDS` | string integer | `"300"` | How long to sleep between retry attempts |
| `STABLE_COUNT_REQUIRED_ATTEMPTS` | string integer | `"2"` | (STABILITY mode) How many consecutive reads with the same count before accepting |
| `OVER_COUNT_BEHAVIOR` | string | `"STABILITY"` | What to do when actual count exceeds expected. `"STABILITY"` or `"WAIT"` |
| `OVER_COUNT_WAIT_MINUTES` | string integer | `"5"` | (WAIT mode only) Minutes to wait after count first exceeds expected before accepting |

---

### Behaviour Flags

| Parameter | Type | Default | Description |
|---|---|---|---|
| `ALLOW_NO_DATA` | string | `"YES"` | `"YES"` = exit cleanly with no output if no messages found. `"NO"` = fail hard if no messages |
| `WAIT_FOR_SUBMIT` | string | `"NO"` | `"YES"` = wait for the upstream status message before starting. `"NO"` = start immediately |
| `VALIDATE_TOPIC_MANDATOR` | string | `"NO"` | `"YES"` = verify the topic contains messages for the expected mandator before proceeding |
| `STREAMING_TIMESTAMP_KEY` | string | `"std_enqueueTime"` | Field name in the message payload used as the message timestamp |
| `STREAMING_TIMESTAMP_FORMAT` | string | `"%Y-%m-%dT%H:%M:%S%z"` | Format string for parsing the timestamp field |
| `STREAMING_WAIT_UNTIL_DONE_HOURS` | string integer | `"1"` | Maximum hours to wait for the upstream process to signal completion |
| `STREAMING_IDLE_TIMEOUT_MINUTES` | string integer | `"2"` | If no new messages arrive for this many minutes, stop consuming |
| `STREAMING_VERBOSE` | string bool | `"True"` | `"True"` = write detailed log entries. `"False"` = minimal logging |
| `STREAMING_STORE_MIDLAYER` | string bool | `"False"` | `"True"` = store intermediate (mid-layer) data. Typically `"False"` for end consumers |
| `COMMIT_CNT` | string integer | `"50"` | Commit Kafka offsets every N messages. Lower = more durable but slower |

---

## Scenarios

All examples below use these baseline values:
- ASOF_DT = `2026-04-22`
- Mandator = `022`
- Expected count from metadata = `1000`
- Tolerance = `10%` → floor = `900`
- `MAX_LISTEN_DURATION_HOURS` = `0.05` (3 minutes, as in local config)
- `RETRY_WAIT_SECONDS` = `5`
- `STABLE_COUNT_REQUIRED_ATTEMPTS` = `2`
- `OVER_COUNT_BEHAVIOR` = `STABILITY` (unless stated otherwise)

---

### Scenario 1 — All messages arrive before the script starts (happy path)

**Setup:** 1000 messages published, script starts, reads them all.

```
Script starts
  → offsets_for_times(window_start) = offset 0  ✓
  → offsets_for_times(window_end)   = offset 999 ✓

Attempt 1: scans offsets 0–999 → filtered_count = 1000
  → 1000 >= 1000 (at or above) → STABILITY: streak = 1

Attempt 2: scans offsets 0–999 → filtered_count = 1000 (same)
  → streak = 2 → streak >= 2 → ACCEPT ✓

Output file written, offsets committed.
```

**Log you will see:**
```
Count stable at 1,000 for 1/2 consecutive attempt(s).
Count stable at 1,000 for 2/2 consecutive attempt(s).
Count stable at 1,000 for 2 consecutive attempt(s) (expected 1,000). Accepting.
```

---

### Scenario 2 — Messages arrive slowly (script starts, then messages trickle in)

**Setup:** 0 messages when script starts. 1000 messages arrive over 30 seconds.

```
Script starts
  → offsets_for_times(window_start) = None  ← topic empty at this moment

Attempt 1:
  → start offset refresh: still None (messages not yet published) → skip → count = 0
  → Under expected → retry

[Messages start arriving]

Attempt 2:
  → start offset refresh: now finds offset 0 ✓ (messages are there)
  → scans → filtered_count = 600  (not all there yet)
  → Under expected → retry

Attempt 3:
  → filtered_count = 1000
  → streak = 1

Attempt 4:
  → filtered_count = 1000 (stable)
  → streak = 2 → ACCEPT ✓
```

> **Why this works:** The start-offset refresh (added fix) re-queries
> `offsets_for_times` on every attempt for any partition that is still `None`.
> Once messages arrive, the offset is found and normal processing resumes.

---

### Scenario 3 — Under count at deadline, within tolerance

**Setup:** Only 950 messages arrive before the 3-minute deadline. Expected 1000, floor = 900.

```
Attempt 1: count = 500  → under → retry
Attempt 2: count = 800  → under → retry
Attempt 3: count = 950  → under → retry
...
[Deadline reached]

Final check: 950 >= 900 (floor) → ACCEPT within tolerance ✓
Log: "Final count 950 is within lower tolerance (900–1000, ±10%). Accepting."
```

---

### Scenario 4 — Under count at deadline, below tolerance

**Setup:** Only 800 messages arrive. Expected 1000, floor = 900.

```
...retries...
[Deadline reached]

Final check: 800 < 900 (floor) → FAIL ✗
Log: "Final count 800 is BELOW lower tolerance threshold 900 (expected 1,000, ±10%). Failing."
Exit code: 1
Offsets NOT committed (safe to rerun).
```

---

### Scenario 5 — Over count, STABILITY mode

**Setup:** 1100 messages arrive (100 extra). `OVER_COUNT_BEHAVIOR = "STABILITY"`.

```
Attempt 1: count = 1100 → over (>= expected)
  → streak = 1  (count changed from None)
  Log: "Count changed: None → 1,100 (>= expected 1,000). Stability streak reset to 1/2."

Attempt 2: count = 1100 → same as last
  → streak = 2 → streak >= 2 → ACCEPT ✓
  Log: "Count stable at 1,100 for 2/2 consecutive attempt(s)."

If more messages keep arriving between attempts:
Attempt 1: count = 1100 → streak = 1
Attempt 2: count = 1150 → streak reset to 1  (count changed)
Attempt 3: count = 1200 → streak reset to 1  (still growing)
Attempt 4: count = 1200 → streak = 2 → ACCEPT ✓  (finally stable)
```

---

### Scenario 6 — Over count, WAIT mode

**Setup:** 1100 messages. `OVER_COUNT_BEHAVIOR = "WAIT"`, `OVER_COUNT_WAIT_MINUTES = 5`.

```
Attempt 1 (T+0s): count = 1100 → over
  → First time over → start 5-min timer → expires at T+5:00
  → Extend retry_deadline to T+5:00 (so loop stays alive)
  Log: "Count EXCEEDS expected: got 1,100, expected 1,000.
        WAIT mode: starting 5-min window. Will accept at T+5:00 regardless."

Attempt 2 (T+5s): count = 1200 → over, timer running
  Log: "Count grew: 1,100 → 1,200. Timer unchanged — 295s remaining."

Attempt 3 (T+10s): count = 1200 → over, timer running, count unchanged
  Log: "Count stable at 1,200. 290s remaining until acceptance."

...retries continue every 5s...

T+5:00 — timer fires
  → ACCEPT ✓
  Log: "Over-count wait window of 5 min expired. Accepting final count 1,200 (expected 1,000)."
```

**To switch between modes, update the config:**
```json
"OVER_COUNT_BEHAVIOR": "WAIT",      ← switch to WAIT
"OVER_COUNT_BEHAVIOR": "STABILITY"  ← switch back
```

---

### Scenario 7 — Count exactly equals expected (WAIT mode)

**Setup:** Exactly 1000 messages. `OVER_COUNT_BEHAVIOR = "WAIT"`.

```
Attempt 1: count = 1000 == expected
  → Exact match → ACCEPT immediately ✓
  Log: "Count exactly matches expected (1,000). Accepting immediately."
  No timer started.
```

---

### Scenario 8 — Messages start low, then cross expected (WAIT mode)

**Setup:** `OVER_COUNT_BEHAVIOR = "WAIT"`. Messages arrive slowly.
Expected = 1000.

```
Attempt 1 (T+0s):   count = 400  → under, retry (no timer)
Attempt 2 (T+5s):   count = 800  → under, retry (no timer)
Attempt 3 (T+10s):  count = 1050 → FIRST TIME over
  → Timer starts now: expires at T+5:10
  → retry_deadline extended to T+5:10
  Log: "Count exceeded expected for first time. 5-min window started. Will accept at T+5:10."

Attempt 4 (T+15s):  count = 1100 → in window, 295s remaining
Attempt 5 (T+20s):  count = 1100 → in window, 290s remaining
...
T+5:10 → timer fires → ACCEPT ✓ with count at that moment
```

> The timer starts from the **crossing point**, not from script startup.
> The full 5-minute window is always available from when count first goes over.

---

### Scenario 9 — No data at all, ALLOW_NO_DATA = "YES"

**Setup:** Topic is empty. No messages ever arrive.

```
All attempts → start offset refresh finds nothing → count = 0
[Deadline reached]
Final tolerance check: 0 < 900 (floor) → would normally fail
But ALLOW_NO_DATA = "YES" → exit cleanly, no output file, no error ✓
Log: "No data consumed."
```

---

### Scenario 10 — No data, ALLOW_NO_DATA = "NO"

```
Same as above but ALLOW_NO_DATA = "NO"
[Deadline reached] → count below tolerance → FAIL ✗
Exit code: 1
```

---

### Scenario 11 — Count hits target near deadline (grace extension, STABILITY mode)

**Setup:** Count reaches expected with less than `RETRY_WAIT_SECONDS` (5s) remaining.

```
T+2:55 (5s before 3-min deadline):
  count = 1000 → streak = 1
  Time remaining = 4s < RETRY_WAIT_SECONDS (5s)
  → Grace extension: deadline += 5s → new deadline = T+3:05
  Log: "Count first reached target with less than 5s remaining — extending deadline by 5s."

T+3:00: next attempt
  count = 1000 → streak = 2 → ACCEPT ✓
  Log: "Count stable at 1,000 for 2/2. Accepting."
```

Without the grace extension the script would hit the deadline at T+3:00 with only
streak=1 and accept with a warning. With it, it gets a clean stable acceptance.

---

### Scenario 12 — Collect everything, validate a subset (`VALIDATION_FILTER_VALUES`)

**Setup:** Topic carries messages with `timelines` in `EOD`, `ITD`, and `RTD`. Expected
count from metadata = 1000, and that 1000 only accounts for `EOD`/`ITD` messages —
`RTD` messages are unrelated volume that still needs to land in the output file.

```json
"PRE_FILTER_VALUES": {},
"VALIDATION_FILTER_VALUES": {"timelines": ["EOD", "ITD"]}
```

```
1300 messages arrive total: 1000 are EOD/ITD, 300 are RTD.

Attempt 1: filtered_count = 1300 (all written)
           validation_count = 1000 (only EOD/ITD counted)
  → 1000 >= 1000 (at or above expected) → STABILITY: streak = 1

Attempt 2: filtered_count = 1300, validation_count = 1000 (stable)
  → streak = 2 → ACCEPT ✓

Output file contains all 1300 messages.
Validation log records actual_count = 1000 (matches expected).
```

**Log you will see:**
```
NOTE: VALIDATION_FILTER_VALUES is set and differs from PRE_FILTER_VALUES. All messages
consumed from the topic will be written to the output file. Only messages matching
VALIDATION_FILTER_VALUES count toward EXPECTED_COUNT. Expect the output file's row
count to exceed the validated count — see the 'Output summary' logged at acceptance
for the exact numbers.
...
Attempt 1 complete: total_messages_seen_in_window=1300, messages_matching_all_filters=1300, validation_matched=1000
Count stable at 1,000 for 1/2 consecutive attempt(s).
Count stable at 1,000 for 2/2 consecutive attempt(s).
Count stable at 1,000 for 2 consecutive attempt(s) (expected 1,000). Accepting.
Output summary:
  Rows written to output file    : 1300
  Rows counted for validation    : 1000
  Expected count (metadata)      : 1000
  NOTE: file row count (1300) differs from the validated count (1000) because
  VALIDATION_FILTER_VALUES={'timelines': ['EOD', 'ITD']} narrows which messages count
  toward EXPECTED_COUNT — all consumed messages are still written to the file.
```

Contrast with leaving `VALIDATION_FILTER_VALUES` empty: `validation_count` would equal
`filtered_count` (1300), which would never match `EXPECTED_COUNT=1000` and the run would
under- or over-count depending on tolerance — this is exactly the case
`VALIDATION_FILTER_VALUES` is meant to avoid.

---

### Scenario 13 — Consume intraday messages with a null `reconciliationGroupId`, uncounted (`METADATA_FILTER_ALLOW_NULL_FIELDS`)

**Setup:** Metadata says `reconciliationGroupId=1`, expected count = 1000 (all 1000
belong to group 1). The same topic also carries `ITD` messages that were published with
no `reconciliationGroupId` at all — those must still land in the output file for the
splitter, but must not be counted toward the 1000.

```json
"METADATA_FILTER_COLUMNS": ["mandatorCode", "businessDate", "reconciliationGroupId"],
"METADATA_FILTER_ALLOW_NULL_FIELDS": ["reconciliationGroupId"],
"PRE_FILTER_VALUES": {"timelines": ["EOD", "ITD"]},
"VALIDATION_FILTER_VALUES": {"timelines": ["EOD", "ITD"]}
```

```
1050 messages arrive total: 1000 have reconciliationGroupId=1, 50 have no
reconciliationGroupId at all (both sets pass PRE_FILTER_VALUES timelines=ITD).

Write-gate (relaxed): all 1050 pass — mandatorCode/businessDate match, and the
null reconciliationGroupId on the 50 is explicitly allowed.
  → filtered_count = 1050

Validation-count (strict re-check, no null allowance): only the 1000 with
reconciliationGroupId=1 satisfy VALIDATION_FILTER_VALUES AND the unrelaxed
filter_values match.
  → validation_count = 1000

Attempt 1: validation_count = 1000 == EXPECTED_COUNT → exact match → ACCEPT immediately.

Output file contains all 1050 messages.
Validation log records actual_count = 1000 (matches expected exactly).
```

**Why `validation_count` still lands on exactly 1000 and not 1050:** the write-gate call
passes `allow_null_fields=METADATA_FILTER_ALLOW_NULL_FIELDS`, so a null
`reconciliationGroupId` doesn't exclude the message from the file. The `validation_count`
block calls `message_matches_filters` a second time on the same `filter_values` *without*
`allow_null_fields` — so those same 50 messages fail that check (null
`reconciliationGroupId` != `1`) and are never added to `validation_count`. This holds
regardless of whether `PRE_FILTER_VALUES`/`VALIDATION_FILTER_VALUES` are populated or
left empty — the strict re-check only depends on `filter_values`, which always comes
from `METADATA_FILTER_COLUMNS`/`METADATA_FILTER_FIELD_MAP`.

**Downstream note:** `filter_recon_group.py` (the plugin step that runs after this
script, before the splitter) has matching behavior — it writes null-`reconciliationGroupId`
records through to its output rather than dropping them, and also keeps them out of its
own `matched_count`/count check. See `00_plugins/PLUGIN_TESTING.md`, Scenario E.

---

### Scenario 14 — `EXPECTED_COUNT = 0`, fast-accept (`ALLOW_ZERO_MESSAGES_PUBLISHED` upstream)

**Setup:** Mandator `022` had nothing to publish today. Upstream,
`kafka_trigger_status_messages.py` ran with `ALLOW_ZERO_MESSAGES_PUBLISHED = "YES"` and
wrote metadata with `total_messages_published=0`, so `EXPECTED_COUNT=0` here.
`STABLE_COUNT_REQUIRED_ATTEMPTS=2`, `RETRY_WAIT_SECONDS=5`.

**Case A — no business messages ever arrive (the common case):**
```
Attempt 1: actual=0 → expected=0, actual=0 → EXACT MATCH → streak=1
Attempt 2: actual=0 → streak=2 → ACCEPT ✓ (~10s total)
```

**Case B — a small number of stray/unrelated messages exist (e.g. a late reprocess,
not more data to come):**
```
Attempt 1: actual=3 → expected=0, actual>0 → accept ("no minimum required"), streak=1
Attempt 2: actual=3 (unchanged) → streak=2 → ACCEPT ✓, commits offsets, writes 3 rows
```
Same ~10s turnaround as Case A — no full-window wait, because there's no minimum count
to wait for when `expected=0`.

**Case C — real messages are still trickling in when this job starts (the risky case):**
```
Attempt 1: actual=1 → accept path, streak=1
Attempt 2: actual=1 (no new message yet) → streak=2 → ACCEPT ✓, commits, finalizes
[10 seconds later] 40 more messages arrive → NEVER SEEN by this run — already exited
```
This is the trade-off documented in `CHANGES.md` (2026-08-14 entry): fast-accept assumes
that once the upstream status-message trigger has fired, anything arriving afterward is
out of scope for this run — it does **not** guarantee "waited long enough to see
everything that was ever going to show up." If a feed can have `EXPECTED_COUNT=0` *and*
a real trickle of messages that matters, fast-accept will under-collect it. That
combination wasn't the case for the feed this was built for (operationally, nothing
after status-message completion is expected to matter) — confirm that holds for any new
feed before relying on `ALLOW_ZERO_MESSAGES_PUBLISHED` + this fast-accept together.

---

## Quick reference — which setting to change

| I want to... | Change this |
|---|---|
| Scan a different time window | `LOCATION_TIME_WINDOW` (per-mandator) or `START_TS` / `STOP_TS` |
| Allow more count shortfall | Increase `METADATA_COUNT_TOLERANCE_PCT` or `LOCATION_TOLERANCE_PCT` |
| Wait longer for messages | Increase `MAX_LISTEN_DURATION_HOURS` |
| Check count stability more times | Increase `STABLE_COUNT_REQUIRED_ATTEMPTS` |
| Use a fixed wait window when over-count | Set `OVER_COUNT_BEHAVIOR=WAIT` and `OVER_COUNT_WAIT_MINUTES` |
| Accept immediately on exact match | Already the default in WAIT mode |
| Handle a status message that legitimately reports 0 expected messages | Upstream: `ALLOW_ZERO_MESSAGES_PUBLISHED="YES"` in `status_messages_config.json`. This script then fast-accepts `EXPECTED_COUNT=0` automatically — nothing to set here |
| Exit cleanly when no data | Set `ALLOW_NO_DATA=YES` |
| Fail hard when no data | Set `ALLOW_NO_DATA=NO` |
| Sleep shorter between retries | Decrease `RETRY_WAIT_SECONDS` |
| Drop non-matching messages from the output file entirely | `PRE_FILTER_VALUES` |
| Collect every message but validate the count against only a subset | `VALIDATION_FILTER_VALUES` (leave `PRE_FILTER_VALUES` empty) |
| Consume messages with a null/missing filter field (e.g. `reconciliationGroupId`) instead of dropping them, without inflating `EXPECTED_COUNT` | `METADATA_FILTER_ALLOW_NULL_FIELDS` |
