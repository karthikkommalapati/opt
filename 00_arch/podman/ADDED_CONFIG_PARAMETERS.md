# New Config Parameters — `00_get_kafka.py` & `kafka_trigger_status_messages.py`

This covers only the parameters that were **added** on top of the base framework config
for these two scripts. For the full parameter reference of either script (including the
standard framework parameters), see `STATUS_MESSAGES_HOW_TO.md` and `GET_KAFKA_HOW_TO.md`.

Three groups:

1. [Metadata Bridge](#1-metadata-bridge-parameters--shared-between-both-scripts) — connects the two scripts together (shared parameters)
2. [`00_get_kafka.py`-only](#2-00_get_kafkapy-only-parameters) — over-count handling
3. [`kafka_trigger_status_messages.py`-only](#3-kafka_trigger_status_messagespy-only-parameters) — instance validation mode

---

## 1. Metadata Bridge parameters (shared between both scripts)

`kafka_trigger_status_messages.py` writes a small metadata file once it has validated
that all expected instances have reported in for a business date. `00_get_kafka.py`
reads that file to know **how many business messages to expect** and **which field
values to filter business messages on**. These parameters configure that hand-off, and
must be set consistently in **both** config files (`status_messages_config.json` and
`get_kafka_config.json`) since they describe the same file and the same field mapping
from both sides.

### `STATUS_MESSAGES_FEED_NAME`

| | |
|---|---|
| **Type** | string |
| **Default** | `"CPSB4Q00"` |
| **Appears in** | both config files |

**What it does:** The feed name used to build the metadata filename. Must match on both
sides — the status-messages script writes `{STATUS_MESSAGES_FEED_NAME}_{ASOF_DT}{METADATA_FILE_SUFFIX}`,
and `00_get_kafka.py` looks for that exact same filename.

**Example:** `STATUS_MESSAGES_FEED_NAME = "CPSB4Q00"`, `ASOF_DT = "2026-04-22"`,
`METADATA_FILE_SUFFIX = "_metadata.txt"` → both scripts agree the file is
`CPSB4Q00_2026-04-22_metadata.txt`.

### `METADATA_FILE_SUFFIX`

| | |
|---|---|
| **Type** | string |
| **Default** | `"_metadata.txt"` |
| **Appears in** | both config files |

**What it does:** Suffix appended after the feed name and date to form the metadata
filename (see example above). Change it if you need multiple metadata files to coexist
per date (e.g. different suffixes per environment).

### `METADATA_OUTPUT_PATH`

| | |
|---|---|
| **Type** | string |
| **Default** | `""` (empty) |
| **Appears in** | `status_messages_config.json` only |

**What it does:** Directory the status-messages script writes the metadata file into.
Empty string = same directory as the main output data file. `00_get_kafka.py` looks for
the metadata file using the same default data-path convention, so leave this empty
unless you have a specific reason to separate metadata from data output.

### `METADATA_FILTER_COLUMNS`

| | |
|---|---|
| **Type** | list of strings |
| **Default** | `["mandatorCode", "businessDate", "reconciliationGroupId"]` |
| **Appears in** | both config files |

**What it does:** Which fields from the **status message** get carried into the metadata
file as filter values. `00_get_kafka.py` then uses those same values to filter which
**business messages** it counts.

### `METADATA_FILTER_FIELD_MAP`

| | |
|---|---|
| **Type** | object |
| **Default** | see example |
| **Appears in** | both config files |

**What it does:** Maps a field name as it appears in the **status message** to the field
name it has in the **business message** — the two message schemas don't necessarily use
the same names for the same concept.

**Example:**

```json
"METADATA_FILTER_FIELD_MAP": {
    "mandator":                "mandatorCode",
    "business_date":           "businessDate",
    "reconciliation_group_id": "reconciliationGroupId"
}
```

Left side = field name in the status message (and in the metadata file). Right side =
field name to match against in the business message. `00_get_kafka.py` reads the filter
values from the metadata file (keyed by the left-side names) and applies them to the
business topic using the right-side field names.

### `METADATA_COUNT_FIELD`

| | |
|---|---|
| **Type** | string |
| **Default** | `"total_messages_published"` |
| **Appears in** | both config files |

**What it does:** The field name in the metadata file that holds the expected business
message count (`EXPECTED_COUNT` in `00_get_kafka.py`). The status-messages script sums
`numberOfMessagesPublished` across all validated instances and writes the total under
this field name.

### `METADATA_COUNT_TOLERANCE_PCT` / `LOCATION_TOLERANCE_PCT`

| | |
|---|---|
| **Type** | `METADATA_COUNT_TOLERANCE_PCT`: string float · `LOCATION_TOLERANCE_PCT`: object |
| **Default** | `"10"` · `{"022": "10"}` |
| **Appears in** | both config files |

**What it does:** How far below the expected count `00_get_kafka.py` will still accept
the data as complete. `LOCATION_TOLERANCE_PCT` is a per-mandator override — if the
running mandator has an entry there, it wins over the global
`METADATA_COUNT_TOLERANCE_PCT`.

**Example:**

```text
floor = expected_count - expected_count × (tolerance_pct / 100)

expected = 1000, tolerance = 10%  →  floor = 900
  actual = 950  →  950 >= 900  →  ACCEPT (within tolerance)
  actual = 850  →  850 < 900   →  FAIL   (below tolerance)
```

```json
"LOCATION_TOLERANCE_PCT": { "022": "5" }
```

Mandator `022` uses a 5% floor instead of the global 10%.

---

## 2. `00_get_kafka.py`-only parameters

These control retry/acceptance behaviour specific to `00_get_kafka.py` when the
business-message count doesn't cleanly match the expected count on the first read.

### `STABLE_COUNT_REQUIRED_ATTEMPTS`

| | |
|---|---|
| **Type** | string integer |
| **Default** | `"2"` |

**What it does:** In `STABILITY` mode (see `OVER_COUNT_BEHAVIOR` below), the number of
consecutive retry attempts that must read the **same** count before the script accepts
it. Protects against accepting a count while messages are still arriving.

**Example** (`STABLE_COUNT_REQUIRED_ATTEMPTS = 2`, expected = 1000):

```text
Attempt 1: count = 1000 → streak 1/2 → not yet confirmed, retry
Attempt 2: count = 1000 → streak 2/2 → CONFIRMED → accept
```

### `OVER_COUNT_BEHAVIOR`

| | |
|---|---|
| **Type** | string |
| **Allowed values** | `"STABILITY"` / `"WAIT"` |
| **Default** | `"STABILITY"` |

**What it does:** Controls how the script decides to accept once the actual count
reaches or exceeds the expected count from the metadata file.

- **`STABILITY`** (existing behaviour) — wait for `STABLE_COUNT_REQUIRED_ATTEMPTS`
  consecutive reads with the identical count, then accept.
- **`WAIT`** — the first time the count crosses expected, start a single fixed timer of
  `OVER_COUNT_WAIT_MINUTES`. Accept whatever count is present when the timer expires,
  whether or not it's still growing. An exact match (`count == expected`) always accepts
  immediately with no timer, in either mode.

**Example — `STABILITY` mode**, expected = 1000:

```text
Attempt 1: count = 1000 → streak 1/2
Attempt 2: count = 1050 → count changed → streak resets to 1/2
Attempt 3: count = 1050 → streak 2/2 → accept 1050
```

**Example — `WAIT` mode**, expected = 1000, `OVER_COUNT_WAIT_MINUTES = 5`:

```text
T+0:00  count = 1000  → first crossing → start 5-min timer (expires T+5:00)
T+2:00  count = 1200  → still within window, timer NOT reset/extended
T+5:00  timer expires → accept 1200 (whatever count is present right now)
```

**When to use `WAIT` instead of `STABILITY`:** if a bursty publisher can keep the count
changing indefinitely, `STABILITY` mode could wait forever for two identical reads in a
row. `WAIT` guarantees the job finishes within a bounded time after the count first
crosses the expected value.

**Config to switch:**

```json
"OVER_COUNT_BEHAVIOR": "WAIT",
"OVER_COUNT_WAIT_MINUTES": 5
```

### `OVER_COUNT_WAIT_MINUTES`

| | |
|---|---|
| **Type** | integer |
| **Default** | `5` |
| **Only used when** | `OVER_COUNT_BEHAVIOR = "WAIT"` |

**What it does:** Minutes to wait, from the moment the count first exceeds expected,
before accepting the data as-is. Ignored entirely in `STABILITY` mode.

### `PRE_FILTER_VALUES`

| | |
|---|---|
| **Type** | object — `{field: expected_value}`, where `expected_value` is a string or a list of strings |
| **Default** | `{}` (no filtering) |

**What it does:** Extra field-level filter applied to every consumed business message,
on top of the metadata-driven filters from `METADATA_FILTER_FIELD_MAP`. Field names
support dot-notation for nested fields (e.g. `"status.timeline"`). All keys in
`PRE_FILTER_VALUES` must match (AND across keys). A key can list multiple acceptable
values — the message passes that key if its value matches *any* one of them (OR within
a key).

**Example — single value:**
```json
"PRE_FILTER_VALUES": {"timelines": "EOD"}
```

**Example — multiple values (OR-match):**
```json
"PRE_FILTER_VALUES": {"timelines": ["EOD", "ITD"]}
```
A message passes the `timelines` filter if its value is `EOD` or `ITD`.

**Example — multiple filters:**
```json
"PRE_FILTER_VALUES": {
    "timelines": ["EOD", "ITD"],
    "region": "EMEA"
}
```
Passes only if `timelines` is `EOD` or `ITD` **and** `region` is exactly `EMEA`.

---

## 3. `kafka_trigger_status_messages.py`-only parameters

### `INSTANCE_VALIDATION_MODE`

| | |
|---|---|
| **Type** | string |
| **Allowed values** | `"SEQUENTIAL"` / `"UNIQUE_COUNT"` |
| **Default** | `"SEQUENTIAL"` |

**What it does:** Controls how the script decides "all instances have reported in" for
the maximum `reconciliationGroupId`, before it will write output. Different producers on
the same topic (selected via `PRODUCER_FILTER`) can number their instances differently.

- **`SEQUENTIAL`** (existing behaviour) — instances are expected to publish
  `status.instanceIndex` values `0` to `totalInstances - 1`, contiguous and zero-based.
  Validation fails (retryable) if any specific index in that range is missing.
- **`UNIQUE_COUNT`** — for producers whose `instanceIndex` values are arbitrary /
  non-sequential (not a clean 0-based range). There's no fixed range to check against —
  validation instead requires the number of **distinct** `instanceIndex` values to equal
  `totalInstances` exactly.

**Example — `SEQUENTIAL` mode** (`totalInstances = 3`):

```text
Messages received: instanceIndex = 0, 1, 2   → all present            → PASS
Messages received: instanceIndex = 0, 2      → index 1 missing        → FAIL, retry
```

**Example — `UNIQUE_COUNT` mode** (`totalInstances = 3`, producer uses arbitrary IDs):

```text
Messages received: instanceIndex = 17, 42, 5    → 3 distinct == 3 expected → PASS
Messages received: instanceIndex = 17, 42       → 2 distinct != 3          → FAIL, retry
Messages received: instanceIndex = 17, 42, 5, 9 → 4 distinct != 3          → FAIL
                                                    (too many — an anomaly, not something
                                                    retrying will fix, but it fails the
                                                    same way as any other validation failure)
```

**Duplicates — same in both modes:** if the same `instanceIndex` value appears twice for
the max `reconciliationGroupId`, validation hard-fails immediately, regardless of mode.

**When to use `UNIQUE_COUNT` instead of `SEQUENTIAL`:** when a producer's instance IDs
aren't a predictable `0..N-1` range — e.g. IDs derived from a hash, a partition key, or
otherwise assigned non-sequentially — so the script has no reliable range to diff
against, only a count to satisfy.

**Config to switch:**

```json
"INSTANCE_VALIDATION_MODE": "UNIQUE_COUNT"
```

Omit the key (or set `"SEQUENTIAL"`) to keep the existing 0-based range-check behaviour.
