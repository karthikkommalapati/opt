# Test Harness Guide — 00_get_kafka.py

No real Kafka broker, schema registry, or upstream metadata dependency required.

---

## Quick Start

```bash
cd trigger_based_new/test_harness

# Run one scenario
/Users/karthikkommalapati/Desktop/dev/personal/ubs_projects/.venv/bin/python3 run_test_get_kafka.py happy_path

# Run all 11 scenarios
/Users/karthikkommalapati/Desktop/dev/personal/ubs_projects/.venv/bin/python3 run_test_get_kafka.py all
```

---

## File Map — What Lives Where

| File | Purpose | When to edit |
|---|---|---|
| `test_data_get_kafka.par` | Base Kafka messages (JSONL, 1 object per line) | Add/change messages for new data scenarios |
| `run_test_get_kafka.py` | All 10 scenario functions + BASE_CONFIG | Add new scenarios, change config defaults, adjust tolerance/retry values |
| `fake_kafka.py` | Fake Kafka consumer replaying `.par` files | Shared — only touch if changing broker/consumer mock behaviour |
| `fake_libs.py` | Fake `dsf_logging` + `assertf` | Shared — only touch if script calls new library methods |
| `../1001_CPSB4QST_config.json` | Production config file read by the script | Update when changing production defaults |
| `../test_output_get_kafka/` | Output from the last test run | Read-only — wiped before each scenario run |

---

## How the Harness Works

```
test_data_get_kafka.par   run_test_get_kafka.py          00_get_kafka.py
───────────────────────   ──────────────────────────     ──────────────────────────
msg0 (line 1)             1. wipes test_output_get_kafka/data/
msg1 (line 2)             2. writes config JSON to  …/config/
msg2 (line 3)             3. writes metadata file to …/data/ root
                          4. injects FakeKafkaConsumer
                          5. patches fastavro.schemaless_reader
                          6. patches requests.Session.get (schema registry)
                          7. patches os._exit → captures exit code
                          8. exec()s 00_get_kafka.py top-to-bottom
                          9. checks .par, .tmp, exit code
```

Each message is encoded as a 4-byte index. `schemaless_reader` mock unpacks that index and returns `test_data_get_kafka.par[index]` as a Python dict — no Avro schema needed.

The metadata file is written to:
```
test_output_get_kafka/data/CPSB4Q00_2026-05-07_metadata.txt
```
This mirrors the production path `{PC_LOD_PROC_PATH}/{STATUS_FEED_NAME}_{ASOF_DT}_metadata.txt`.

---

## Metadata File Format

Pipe-delimited, exactly 2 non-blank lines: header + 1 data row.

```
export_datetime|username|business_date|mandator|producer_name|feed_name|reconciliation_group_id|instances_counted|total_expected_instances|total_messages_published
2026-05-07T17:00:00|testuser|2026-05-07|022|CLIENT_STRUCTURES|CPSB4Q00|5001|3|3|3
```

The `make_metadata_line()` helper in `run_test_get_kafka.py` builds this string. Parameters:

| Parameter | Default | Controls |
|---|---|---|
| `mandator` | `"022"` | Filter value for `mandatorCode` |
| `business_date` | `"2026-05-07"` | Filter value for `businessDate` |
| `recon_id` | `"5001"` | Filter value for `reconciliationGroupId` |
| `total_messages` | `3` | Expected count checked against actual filtered count |
| `username` | `"testuser"` | Username field (informational — not used for filtering) |

---

## Key Config Values (BASE_CONFIG in run_test_get_kafka.py)

| Key | Default in harness | Purpose |
|---|---|---|
| `METADATA_FILTER_COLUMNS` | `["mandatorCode", "businessDate", "reconciliationGroupId"]` | Fields to filter on |
| `METADATA_FILTER_FIELD_MAP` | maps metadata field names → Kafka field names | Must have an entry for every column in FILTER_COLUMNS |
| `METADATA_COUNT_TOLERANCE_PCT` | `"10"` | ±% acceptable deviation from expected count |
| `MAX_LISTEN_DURATION_HOURS` | `"0.05"` | Max retry window + Kafka window extension (≈3 min); Scenario 10 overrides to `"0"` |
| `RETRY_WAIT_SECONDS` | `"30"` | Sleep between retry attempts |

`00_get_kafka.py` reads all three tolerance/retry values via `float()`/`int()` wrappers — string or number both work.

---

## All 11 Scenarios

### Scenario 1 — Happy Path

**Use case:** Normal production run. All messages match the filter, count is exact.

**What it tests:** End-to-end success path — metadata read, filter applied, count validated, `.par` written, offsets committed, temp file cleaned up.

**Data / config:** Standard `test_data_get_kafka.par` (3 messages, all `mandatorCode=022`, `reconciliationGroupId=5001`). `make_metadata_line(total_messages=3)`.

**Expected result:**
- Exit `0`
- `.par` written with 3 lines
- Temp `.par.tmp` cleaned up
- `CPSB4QST_get_kafka_validation.log` appended with `username` field

**To change:** To test with a different message count, add lines to `test_data_get_kafka.par` and update `make_metadata_line(total_messages=N)` in `scenario_happy_path()`.

---

### Scenario 2 — Count Within Tolerance

**Use case:** Kafka published slightly fewer messages than expected (e.g. a late message). Count is within the acceptable ±% band.

**What it tests:** Tolerance calculation. 3 actual vs 4 expected at 50% tolerance → range is [2, 6] → 3 passes.

**Config override:** `METADATA_COUNT_TOLERANCE_PCT: "50"` (wider band). `make_metadata_line(total_messages=4)`.

**Expected result:**
- Exit `0`
- `.par` written with 3 lines

**To change:** Adjust `total_messages` and `METADATA_COUNT_TOLERANCE_PCT` to test different tolerance boundaries.

---

### Scenario 3 — No Filter Columns

**Use case:** Config explicitly disables all filtering (e.g. a feed that publishes without mandator/recon segmentation).

**What it tests:** `METADATA_FILTER_COLUMNS=[]` — all messages pass through without any field comparison.

**Config override:** `METADATA_FILTER_COLUMNS: []`, `METADATA_FILTER_FIELD_MAP: {}`. `make_metadata_line(total_messages=3)`.

**Expected result:**
- Exit `0`
- `.par` written with 3 lines (all messages, unfiltered)

---

### Scenario 4 — Metadata File Missing

**Use case:** `kafka_trigger_status_messages.py` has not run yet, or ran on a different date. No metadata file at the expected path.

**What it tests:** Hard exit when metadata file is absent. Script must not proceed to consume Kafka.

**How triggered:** `metadata_content=None` — harness skips writing the file.

**Expected result:**
- Exit `1` immediately
- `.par` NOT written
- Log line: `[ERROR] METADATA FILE NOT FOUND: ...`

---

### Scenario 5 — Metadata File: 1 Line (no data row)

**Use case:** Upstream script started writing but was interrupted — file has only the header.

**What it tests:** Validation that `len(lines) < 2` triggers hard exit.

**How triggered:** Metadata content = header row only (no data row).

**Expected result:**
- Exit `1`
- Log line about incomplete/empty metadata file

---

### Scenario 6 — Metadata File: Extra Data Row

**Use case:** Metadata file contains entries from two separate trigger runs (e.g. a file was appended to instead of overwritten).

**What it tests:** Validation that `len(lines) > 2` triggers hard exit (ambiguous which row to use).

**How triggered:** Metadata content = header + 2 identical data rows.

**Expected result:**
- Exit `1`
- Log line: `[ERROR] Metadata file ... has 3 non-blank lines — expected exactly 2`

---

### Scenario 7 — Metadata Count Field Not an Integer

**Use case:** Upstream script wrote a placeholder or error string instead of a number (e.g. `"N/A"`, `""`, `"ERROR"`).

**What it tests:** `int("N/A")` raises `ValueError` → hard exit.

**How triggered:** `make_metadata_line(total_messages="N/A")`.

**Expected result:**
- Exit `1`
- Log line: `[ERROR] Count field 'total_messages_published' value 'N/A' is not a valid integer.`

---

### Scenario 8 — Filter Column Has No Mapping

**Use case:** Config was partially updated — a new filter column was added to `METADATA_FILTER_COLUMNS` but its entry was forgotten from `METADATA_FILTER_FIELD_MAP`.

**What it tests:** Script detects the missing map entry at startup and exits before consuming anything.

**Config override:** `METADATA_FILTER_COLUMNS: ["mandatorCode", "unknownField"]`, `METADATA_FILTER_FIELD_MAP` only has `mandator → mandatorCode`.

**Expected result:**
- Exit `1`
- Log line: `[ERROR] Filter column 'unknownField' listed in METADATA_FILTER_COLUMNS has no entry in METADATA_FILTER_FIELD_MAP.`

---

### Scenario 9 — Partial Filter Match

**Use case:** Kafka topic contains messages from multiple mandators. Only the correct mandator's messages should be written.

**What it tests:** Messages that don't match the filter are silently dropped. Only matching messages reach the `.par` file. Count validation uses the filtered count.

**Data:** Inline — T001 (`mandatorCode=022`), T002 (`mandatorCode=999` — wrong), T003 (`mandatorCode=022`). `make_metadata_line(total_messages=2)`.

**Expected result:**
- Exit `0`
- `.par` written with 2 lines (T001 + T003 only)
- All written lines have `mandatorCode=022`

**To change:** Adjust inline `msgs` list in `scenario_partial_filter_match()` to test different mismatch patterns.

---

### Scenario 10 — Count Outside Tolerance (retry exhausted)

**Use case:** Kafka topic is far short of the expected count and retries are exhausted. Failure analysis is logged.

**What it tests:** Retry window expiry → failure analysis output → temp file cleanup → exit 1 with offsets NOT committed.

**Config override:** `MAX_LISTEN_DURATION_HOURS: "0"` (expire immediately), `RETRY_WAIT_SECONDS: "0"`. `make_metadata_line(total_messages=100)` (3 actual vs 100 expected).

**Expected result:**
- Exit `1`
- `.par` NOT written
- Temp file cleaned up
- Log lines:
  ```
  [ERROR] Result: OUTSIDE TOLERANCE — actual is 97.0000% lower than expected
  [ERROR] Retry window of 0.0 hour(s) exhausted after 1 attempt(s).
  [ERROR] FAILURE ANALYSIS — FILTER / COUNT MISMATCH DETAIL
  [ERROR] Offsets have NOT been committed.
  ```

---

## Tracking Test Results

### Console output

Each scenario prints a `[PASS]` / `[FAIL]` line per assertion. Running `all` prints a summary table at the end:

```
============================================================
SUMMARY
============================================================
  [PASS]  happy_path
  [PASS]  count_within_tolerance
  [FAIL]  metadata_missing
  ...
============================================================
SOME FAILED — see [FAIL] lines above
============================================================
```

Exit code of `run_test_get_kafka.py` is `0` if all pass, `1` if any fail — suitable for CI.

### Output files (last run only)

Every run writes to `test_output_get_kafka/` and wipes it at the start:

```
test_output_get_kafka/
├── config/
│   └── 1001_CPSB4QST_config.json          ← config used for the last scenario
├── data/
│   ├── CPSB4Q00_2026-05-07_metadata.txt    ← metadata file (absent for metadata_missing)
│   ├── CPSB4QST_get_kafka_validation.log   ← append-only success log (one row per exit 0)
│   └── CPSB4QST_2026-05-07/
│       └── CPSB4QST_2026-05-07.par         ← written only on exit 0
└── logs/
    └── (script log files)
```

### Log naming convention

All log files must be prefixed with `get_kafka_` so it is clear which harness produced them:

```
get_kafka_YYYYMMDD_HHMMSS.log
```

```bash
/Users/karthikkommalapati/Desktop/dev/personal/ubs_projects/.venv/bin/python3 \
  run_test_get_kafka.py all 2>&1 | tee get_kafka_$(date +%Y%m%d_%H%M%S).log
```

This captures both stdout (harness assertions) and stderr (script log lines) in a timestamped file. Never drop the `get_kafka_` prefix — both harnesses write to the same folder and the prefix is the only way to tell them apart.

### Baseline run — 2026-05-11 (all 11 pass)

Run command:
```bash
python3 run_test_get_kafka.py all 2>&1 | tee get_kafka_20260511_all.log
```

Output (assertions + summary only — full verbose log in `get_kafka_20260511_all.log`):
```
============================================================
  Scenario 1 — Happy Path
============================================================
  [PASS]  exit code = 0
  [PASS]  .par file written with 3 line(s)
  [PASS]  temp file cleaned up
  [PASS]  get_kafka_validation.log has username ('karthikkommalapati')

============================================================
  Scenario 2 — Count Within Tolerance
============================================================
  [PASS]  exit code = 0
  [PASS]  .par written with 3 line(s) (expected 3)
  [PASS]  get_kafka_validation.log has username ('karthikkommalapati')

============================================================
  Scenario 3 — No Filter Columns (all messages pass)
============================================================
  [PASS]  exit code = 0
  [PASS]  .par written with 3 line(s) (expected 3)
  [PASS]  get_kafka_validation.log has username ('karthikkommalapati')

============================================================
  Scenario 4 — Metadata File Missing
============================================================
  [PASS]  exit code = 1 (correct hard stop)
  [PASS]  .par file NOT written (correct)

============================================================
  Scenario 5 — Metadata File: 1 Line (no data row)
============================================================
  [PASS]  exit code = 1 (correct hard stop)

============================================================
  Scenario 6 — Metadata File: Extra Data Row
============================================================
  [PASS]  exit code = 1 (correct hard stop)

============================================================
  Scenario 7 — Metadata Count Field Not an Integer
============================================================
  [PASS]  exit code = 1 (correct hard stop)

============================================================
  Scenario 8 — Filter Column Has No Mapping
============================================================
  [PASS]  exit code = 1 (correct hard stop)

============================================================
  Scenario 9 — Partial Filter Match (wrong mandator on 1 message)
============================================================
  [PASS]  exit code = 0
  [PASS]  .par written with 2 line(s) (expected 2)
  [PASS]  all written lines have mandatorCode=022
  [PASS]  get_kafka_validation.log has username ('karthikkommalapati')

============================================================
  Scenario 10 — Count Outside Tolerance (retry exhausted)
============================================================
  [PASS]  exit code = 1 (count mismatch, retry exhausted)
  [PASS]  .par file NOT written (correct)
  [PASS]  temp file cleaned up after failure (correct)

============================================================
SUMMARY
============================================================
  [PASS]  happy_path
  [PASS]  count_within_tolerance
  [PASS]  no_filter_columns
  [PASS]  metadata_missing
  [PASS]  metadata_one_line
  [PASS]  metadata_extra_row
  [PASS]  metadata_count_not_int
  [PASS]  filter_column_no_mapping
  [PASS]  partial_filter_match
  [PASS]  count_outside_tolerance
  [PASS]  real_data
============================================================
ALL PASSED
============================================================
```

---

## Using Real Data (no hardcoded values)

Run the `real_data` scenario with your actual production files. No values need to be changed in code — everything is derived from the files you provide.

### Files to drop into `test_harness/`

| File | What it contains | Format |
|---|---|---|
| `test_data_get_kafka.par` | Real Kafka messages (replaces the synthetic file) | JSONL — one JSON object per line |
| `real_metadata_get_kafka.txt` | Real metadata produced by `kafka_trigger_status_messages.py` | Pipe-delimited, header row + 1 data row |

### Required fields in `test_data_get_kafka.par`

Each line must be a flat JSON object with at least these top-level fields (the filter fields):

```json
{"mandatorCode": "022", "businessDate": "2026-05-07", "reconciliationGroupId": "5001", ...rest of message...}
```

Nested fields are fine for the rest of the message — only the filter fields must be at the top level.

### Required format for `real_metadata_get_kafka.txt`

```
export_datetime|username|business_date|mandator|producer_name|feed_name|reconciliation_group_id|instances_counted|total_expected_instances|total_messages_published
2026-05-07T17:00:00|alice|2026-05-07|022|CLIENT_STRUCTURES|CPSB4Q00|5001|3|3|47823
```

Exactly 2 non-blank lines: header + 1 data row. This is the same file `kafka_trigger_status_messages.py` writes.

### Run the scenario

```bash
cd /Users/karthikkommalapati/Desktop/dev/personal/ubs_projects/trigger_based_new/test_harness
/Users/karthikkommalapati/Desktop/dev/personal/ubs_projects/.venv/bin/python3 \
  run_test_get_kafka.py real_data 2>&1 | tee get_kafka_real_$(date +%Y%m%d_%H%M%S).log
```

### What the harness derives from your files (nothing hardcoded)

| Value | Derived from |
|---|---|
| `business_date` / `ASOF_DT` | `business_date` field in metadata file |
| `mandator` | `mandator` field in metadata file |
| Expected filtered count | `total_messages_published` in metadata file |
| Tolerance | `METADATA_COUNT_TOLERANCE_PCT` in BASE_CONFIG (currently `"10"`) |

If the metadata file is absent the scenario prints `[SKIP]` and counts as pass — it does not fail the run.

---

## Adding a New Scenario

1. Add a message or modify `test_data_get_kafka.par` if new data is needed.
2. Write a new function `scenario_<name>()` in `run_test_get_kafka.py` following the existing pattern:
   - Call `run_script(msgs, config_overrides={...}, metadata_content=...)`.
   - Assert `exit_code`, `.par` line count, `.tmp` cleanup.
   - Return `True` / `False`.
3. Register it in the `SCENARIOS` dict at the bottom of `run_test_get_kafka.py`.

Example — test a different recon ID in metadata vs messages:

```python
def scenario_wrong_recon_id():
    section("Scenario 12 — Recon ID mismatch in metadata")
    msgs = load_test_messages()  # all have reconciliationGroupId=5001
    code, par, tmp, vlog = run_script(
        msgs,
        metadata_content=make_metadata_line(recon_id="9999", total_messages=0),
    )
    passed = code == 0
    # 0 messages pass filter → count 0 vs expected 0 → passes
    print_result(f"exit code = 0 (got {code})", passed)
    return passed
```

Then add to `SCENARIOS`:
```python
"wrong_recon_id": scenario_wrong_recon_id,
```

---

## Bugs Fixed in 00_get_kafka.py During Harness Work

| Line | Bug | Fix |
|---|---|---|
| 571 | `STOP_DT_OFFFSET` (3 F's) — NameError | → `STOP_DT_OFFSET` |
| 606 | `STOP_DT_OFFFSET` (3 F's) — NameError | → `STOP_DT_OFFSET` |
| 811 | `int(TS_UTC_END * 100)` — wrong unit, stop offset always 0, only 1 message per partition | → `* 1000` |
| 640 | `METADATA_FILE_PATH` built from `DATA_FOLDER` (own feed subfolder) — never matched production path | → `DATA_PATH` (= `PC_LOD_PROC_PATH` root) |
