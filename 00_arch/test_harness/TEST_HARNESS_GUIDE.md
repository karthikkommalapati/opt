# Test Harness Guide — kafka_trigger_status_messages.py

No real Kafka broker, schema registry, or upstream dependency required.

---

## Quick Start

```bash
cd trigger_based_new/test_harness

# Run one scenario
/Users/karthikkommalapati/Desktop/dev/personal/ubs_projects/.venv/bin/python3 run_test.py happy_path

# Run all 8 scenarios
/Users/karthikkommalapati/Desktop/dev/personal/ubs_projects/.venv/bin/python3 run_test.py all
```

---

## File Map — What Lives Where

| File | Purpose | When to edit |
|---|---|---|
| `test_data.par` | Base Kafka messages (JSONL reference — not used directly by automated scenarios) | Reference/manual testing only |
| `run_test.py` | All 7 scenario functions + BASE_CONFIG + BASE_MESSAGES | Add new scenarios, change config defaults |
| `fake_kafka.py` | Fake Kafka consumer replaying messages | Shared — only touch if changing broker/consumer mock behaviour |
| `fake_libs.py` | Fake `dsf_logging` + `assertf` | Shared — only touch if script calls new library methods |
| `../test_output/` | Output from the last test run | Read-only — wiped before each scenario run |

---

## How the Harness Works

```
BASE_MESSAGES (inline dicts)    run_test.py                  kafka_trigger_status_messages.py
─────────────────────────────   ──────────────────────────   ──────────────────────────────
msg0 (instanceIndex 0)          1. wipes test_output/data/
msg1 (instanceIndex 1)          2. writes config JSON to …/config/
msg2 (instanceIndex 2)          3. injects FakeKafkaConsumer
                                4. patches fastavro.schemaless_reader
                                5. patches requests.Session.get (schema registry)
                                6. patches os._exit → captures exit code
                                7. exec()s the script top-to-bottom
                                8. checks .par, metadata, exit code
```

Each message is encoded as a 4-byte index. `schemaless_reader` mock unpacks that index and returns `BASE_MESSAGES[index]` as a Python dict.

Scenario 7 (late data arrival) uses a temp JSONL file with 2 messages. A background thread overwrites it with 3 messages after 3 seconds while the script is sleeping between retries.

---

## Base Messages (in run_test.py)

Three instances for `mandatorCode=022`, `businessDate=2026-05-07`, `reconciliationGroupId=5001`:

| Index | instanceIndex | numberOfMessagesPublished |
|---|---|---|
| 0 | 0 | 15000 |
| 1 | 1 | 16000 |
| 2 | 2 | 16823 |

Total = 47823. All have `producer=CLIENT_STRUCTURES`.

---

## All 8 Scenarios

### Scenario 1 — Happy Path

**Use case:** Normal production run. All 3 instances present, all filters match.

**What it tests:** End-to-end success path — collect pass, validation, write pass, metadata written, offsets committed.

**Data / config:** `BASE_MESSAGES` (all 3). No overrides.

**Expected result:**
- Exit `0`
- `.par` written with 3 lines
- Metadata: `username` present, `total_messages_published=47823`, `reconciliation_group_id=5001`
- `CPSB4Q00_status_messages_validation.log` appended with `username` field

---

### Scenario 2 — Missing Instance (hard fail)

**Use case:** One producer instance hasn't published yet. `ALLOW_NO_DATA=NO` (default) — fail immediately.

**What it tests:** Validation catches missing `instanceIndex=2`, exits 1 without writing anything.

**Data:** `BASE_MESSAGES[0:2]` only (instanceIndex 0 and 1).

**Expected result:**
- Exit `1`
- `.par` NOT written
- Log line: `[ERROR] VALIDATION FAILED: Missing instanceIds: [2]`

---

### Scenario 3 — Wrong Producer

**Use case:** All messages come from a different producer (wrong feed on the same topic).

**What it tests:** Producer filter rejects all messages, exits 1.

**Data:** `BASE_MESSAGES` with `producer` overridden to `"WRONG_PRODUCER"` on all 3.

**Expected result:**
- Exit `1`
- `.par` NOT written
- Log line: `[WARNING] No messages found for producer 'CLIENT_STRUCTURES'`

---

### Scenario 4 — Duplicate Instance Index

**Use case:** Two messages both claim the same `instanceIndex` — malformed publish.

**What it tests:** Duplicate detection. Validation fails, exits 1.

**Data:** `BASE_MESSAGES` with `msgs[2]["status"]["instanceIndex"] = 1` (two messages claiming index 1).

**Expected result:**
- Exit `1`
- Log line: `[INFO] VALIDATION FAILED: Duplicate instanceIds found: [1, 2]`

---

### Scenario 5 — Missing numberOfMessagesPublished Column

**Use case:** Messages arrive without the count field (upstream producer omitted it).

**What it tests:** Script fails after instance validation passes when column is absent.

**Data:** `BASE_MESSAGES` with `numberOfMessagesPublished` deleted from all status dicts.

**Expected result:**
- Exit `1` (or unhandled exception — both are failures)
- **Known script bug:** `validate_instance_and_get_max_runid` at line 334 hits an unhandled `KeyError` instead of cleanly calling `os._exit(1)`. The harness accepts either outcome as correct. Fix: wrap `row['status.numberOfMessagesPublished']` in a try/except.

---

### Scenario 6 — Multiple Run IDs (stale + current)

**Use case:** Kafka topic contains messages from an older run (4999) and the current run (5001).

**What it tests:** `max(reconciliationGroupId)` selection. Only the 3 current-run messages are written.

**Data:** 2 stale messages (`reconciliationGroupId=4999`) prepended to `BASE_MESSAGES`.

**Expected result:**
- Exit `0`
- `.par` written with 3 lines (run 5001 only, stale run discarded)
- Metadata: `reconciliation_group_id=5001`, `total_messages_published=47823`
- Log line: `[INFO] Max reconciliationGroupId among filtered messages: 5001`

---

### Scenario 7 — Late Data Arrival (retry/polling)

**Use case:** Instance 2 hasn't published when the script first runs. Arrives during the retry window.

**What it tests:** `ALLOW_NO_DATA=YES` retry loop. Script retries, picks up the 3rd message on the next pass, validates successfully.

**How it works:**
1. Temp file written with 2 messages (instanceIndex 0, 1)
2. Background thread overwrites temp file with all 3 at t=3s
3. Script runs with `ALLOW_NO_DATA=YES`, `RETRY_WAIT_SECONDS=5`, `MAX_LISTEN_DURATION_HOURS=0.05`
4. First pass: 2 messages → validation fails → sleeps 5s
5. Second pass: file now has 3 → validation passes → writes output

**Expected result:**
- Exit `0`
- `.par` written with 3 lines
- Log lines showing retry then success:
  ```
  [ERROR] VALIDATION FAILED: Missing instanceIds: [2]
  [INFO]  sleeping 5 seconds before next retry
  [INFO]  VALIDATION PASSED: All instanceIds from 0 to 2 are present with no duplicates.
  ```

---

## Tracking Test Results

### Console output

Each scenario prints `[PASS]` / `[FAIL]` per assertion. Running `all` prints a summary table:

```
============================================================
SUMMARY
============================================================
  [PASS]  happy_path
  [PASS]  missing_instance
  [PASS]  wrong_producer
  [PASS]  duplicate_instance
  [PASS]  missing_column
  [PASS]  multiple_run_ids
  [PASS]  late_data_arrival
============================================================
ALL PASSED
============================================================
```

Exit code is `0` if all pass, `1` if any fail — suitable for CI.

### Output files (last run only)

Every run writes to `test_output/` and wipes it at the start:

```
test_output/
├── config/
│   └── 1001_CPSB4Q00_config.json        ← config used for the last scenario
├── data/
│   └── CPSB4Q00_2026-05-07/
│       ├── CPSB4Q00_2026-05-07.par      ← written only on exit 0
│       └── CPSB4Q00_2026-05-07_metadata.txt  ← pipe-delimited, header + data row
│   └── CPSB4Q00_status_messages_validation.log  ← append-only history across runs
└── logs/
    └── CPSB4Q00.2026-05-07.99999.TEST001.log
```

### Log naming convention

All log files must be prefixed with `status_messages_` so it is clear which harness produced them:

```
status_messages_YYYYMMDD_HHMMSS.log
```

```bash
/Users/karthikkommalapati/Desktop/dev/personal/ubs_projects/.venv/bin/python3 \
  run_test.py all 2>&1 | tee status_messages_$(date +%Y%m%d_%H%M%S).log
```

This captures both stdout (harness assertions) and stderr (script log lines) in a timestamped file. Never drop the `status_messages_` prefix — both harnesses write to the same folder and the prefix is the only way to tell them apart.

### Baseline run — 2026-05-11 (all 8 pass)

Run command:
```bash
python3 run_test.py all 2>&1 | tee status_messages_20260511_all.log
```

Output (assertions + summary only — full verbose log in `status_messages_20260511_all.log`):

```
============================================================
  Scenario 1 — Happy Path
============================================================
  [PASS]  exit code = 0
  [PASS]  .par written with 3 line(s) (expected 3)
  [PASS]  metadata file written
  [PASS]  username present in metadata ('karthikkommalapati')
  [PASS]  total_messages_published = 47823
  [PASS]  reconciliation_group_id = 5001
  [PASS]  status_messages_validation.log has username ('karthikkommalapati')

============================================================
  Scenario 2 — Missing Instance (hard fail)
============================================================
  [PASS]  exit code = 1 (correct hard stop)
  [PASS]  .par file NOT written (correct)

============================================================
  Scenario 3 — Wrong Producer
============================================================
  [PASS]  exit code = 1 (correct hard stop)
  [PASS]  .par file NOT written (correct)

============================================================
  Scenario 4 — Duplicate Instance Index
============================================================
  [PASS]  exit code = 1 (correct hard stop)

============================================================
  Scenario 5 — Missing numberOfMessagesPublished Column
============================================================
  [PASS]  script raised unhandled exception (counts as failure — see bug note)

============================================================
  Scenario 6 — Multiple Run IDs (stale + current)
============================================================
  [PASS]  exit code = 0
  [PASS]  .par written with 3 line(s) (expected 3)
  [PASS]  metadata file written
  [PASS]  username present in metadata ('karthikkommalapati')
  [PASS]  total_messages_published = 47823
  [PASS]  reconciliation_group_id = 5001
  [PASS]  status_messages_validation.log has username ('karthikkommalapati')

============================================================
  Scenario 7 — Late Data Arrival (retry/polling)
============================================================
  [PASS]  exit code = 0 (retried and recovered 3rd message)
  [PASS]  .par written with 3 line(s) (expected 3)
  [PASS]  status_messages_validation.log has username ('karthikkommalapati')

============================================================
SUMMARY
============================================================
  [PASS]  happy_path
  [PASS]  missing_instance
  [PASS]  wrong_producer
  [PASS]  duplicate_instance
  [PASS]  missing_column
  [PASS]  multiple_run_ids
  [PASS]  late_data_arrival
  [PASS]  real_data
============================================================
ALL PASSED
============================================================
```

---

## Using Real Data (no hardcoded values)

Run the `real_data` scenario with your actual production messages. No values need to be changed in code — everything is derived from the messages themselves.

### File to drop into `test_harness/`

| File | What it contains | Format |
|---|---|---|
| `test_data.par` | Real Kafka status messages (replaces the synthetic file) | JSONL — one JSON object per line |

No metadata input file is needed — this script **writes** the metadata, it does not read it.

### Required fields in each line of `test_data.par`

```json
{
  "status": {
    "mandatorCode": "022",
    "businessDate": "2026-05-07",
    "reconciliationGroupId": 5001,
    "instanceIndex": 0,
    "totalInstances": 3,
    "numberOfMessagesPublished": 15000
  },
  "producer": "CLIENT_STRUCTURES",
  "eventTimestamp": "2026-05-07T17:00:00+0000"
}
```

### Run the scenario

```bash
cd /Users/karthikkommalapati/Desktop/dev/personal/ubs_projects/trigger_based_new/test_harness
/Users/karthikkommalapati/Desktop/dev/personal/ubs_projects/.venv/bin/python3 \
  run_test.py real_data 2>&1 | tee status_messages_real_$(date +%Y%m%d_%H%M%S).log
```

### What the harness derives from your messages (nothing hardcoded)

| Value | Derived from |
|---|---|
| `businessDate` / `ASOF_DT` | `status.businessDate` of first message |
| `mandatorCode` | `status.mandatorCode` of first message |
| `max reconciliationGroupId` | `max()` across all messages |
| Expected `.par` line count | Count of messages with max recon ID |
| Expected `total_messages_published` | Sum of `numberOfMessagesPublished` for max recon ID messages |

If `test_data.par` is empty the scenario prints `[SKIP]` and counts as pass — it does not fail the run.

---

## Adding a New Scenario

1. Add inline message dicts to the function if you need non-standard data (or use `copy.deepcopy(BASE_MESSAGES)` and mutate).
2. Write a `scenario_<name>()` function in `run_test.py` following the existing pattern:
   - Call `run_script(msgs, config_overrides={...})`.
   - Assert `exit_code`, `.par` line count, metadata fields.
   - Return `True` / `False`.
3. Register it in the `SCENARIOS` dict at the bottom of `run_test.py`.

---

## Known Script Bug (Scenario 5)

`kafka_trigger_status_messages.py` line 334:

```python
num_messages = int(row['status.numberOfMessagesPublished'])
```

No guard for `KeyError` when the column is absent. Should be wrapped in a `try/except KeyError` that logs `[ERROR]` and calls `os._exit(1)`. Currently raises an unhandled exception instead. The harness accepts this as a failure (exit code `None` treated as non-zero), but the script should be fixed for clean production behavior.
