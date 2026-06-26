# Plugin Testing Guide

Testing `filter_recon_group.py` and the `odp_asi_trans` plugin locally.

---

## Table of Contents

1. [What Can and Cannot Be Tested Locally](#1-what-can-and-cannot-be-tested-locally)
2. [Step 1 — Create the dsf_logging Stub](#2-step-1--create-the-dsf_logging-stub)
3. [Step 2 — Set Up Test Data](#3-step-2--set-up-test-data)
4. [Step 3 — Run the Filter Directly](#4-step-3--run-the-filter-directly)
5. [Step 4 — Failure Scenarios](#5-step-4--failure-scenarios)
6. [Step 5 — Simulate the Plugin Call](#6-step-5--simulate-the-plugin-call)
7. [Troubleshooting](#7-troubleshooting)

---

## 1. What Can and Cannot Be Tested Locally

| Component | Testable locally? | Notes |
|---|---|---|
| `filter_recon_group.py` | Yes — directly | Needs a `dsf_logging` stub and test data on disk |
| `odp_asi_trans` (full plugin) | No | Calls framework tools (`cli.py`, `splitter.py`, `$DWP_ROOT`, `$log_filename`) that only exist in the production environment |
| `odp_asi_trans` (filter step only) | Yes — via local runner | Create a runner script that sets the same env vars and calls `filter_recon_group.py` directly |

---

## 2. Step 1 — Create the `dsf_logging` Stub

`filter_recon_group.py` imports `dsf_logging`, which is a framework library not available outside the production environment. Create a local stub in `00_plugins/` so Python finds it when running from that directory.

Create `00_plugins/dsf_logging.py`:

```python
import logging, os

class DSF_logging:
    def get_logger(self, name, output_path="", level=20, verbose=True):
        log_path = os.path.join(output_path, name) if output_path else name
        log_path = os.path.abspath(log_path)
        self._log_path = log_path
        logging.basicConfig(
            filename=log_path, filemode="a", level=logging.DEBUG,
            format="%(asctime)s %(levelname)-8s %(message)s",
            datefmt="%Y-%m-%dT%H:%M:%S"
        )
        self._logger = logging.getLogger(name)

    def log_msg(self, msg, level=20):
        lvl = {10: logging.DEBUG, 20: logging.INFO,
               30: logging.WARNING, 40: logging.ERROR, 50: logging.CRITICAL}
        self._logger.log(lvl.get(level, logging.INFO), msg)

    def log_starting_process(self, name, level=20):
        self.log_msg(f"Starting process: {name}", level=level)

    def log_error_msg(self, msg, err=None):
        self.log_msg(f"{msg}: {err}", level=40)
        return RuntimeError(f"{msg}: {err}")
```

---

## 3. Step 2 — Set Up Test Data

`filter_recon_group.py` requires two files to already exist on disk:

1. A **metadata file** — normally written by the status messages pipeline
2. A **`.par` file** — normally written by the business data pipeline (`00_get_kafka.py`)

If you have already run both pipelines successfully for your target date, these files already exist — skip to Step 3.

**To create them manually for a standalone test**, substitute your own values for the variables below:

```
ASOF_DT      = 2026-04-22          ← business date
STATUS_FEED  = STATUS_FEED_00      ← status messages feed name (matches --status-feed-name)
DATA_FEED    = DATA_FEED_ST        ← business data feed name
RECON_ID     = 1                   ← reconciliationGroupId expected in the .par file
RECORD_COUNT = 3                   ← must match the number of records you put in the .par file
```

```bash
ASOF_DT="2026-04-22"
STATUS_FEED="STATUS_FEED_00"
DATA_FEED="DATA_FEED_ST"
RECON_ID=1
RECORD_COUNT=3

# 1 — Metadata file
#     Path mirrors exactly what the status messages pipeline writes:
#     {PC_LOD_PROC_PATH}/{STATUS_FEED}_{ASOF_DT}/{STATUS_FEED}_{ASOF_DT}_metadata.txt
mkdir -p output/get_kafka/${STATUS_FEED}_${ASOF_DT}
cat > output/get_kafka/${STATUS_FEED}_${ASOF_DT}/${STATUS_FEED}_${ASOF_DT}_metadata.txt << EOF
export_datetime|username|business_date|mandator|producer_name|feed_name|reconciliation_group_id|instances_counted|total_expected_instances|total_messages_published
2026-04-22T23:59:00|testuser|${ASOF_DT}|022|PRODUCER_A|${STATUS_FEED}|${RECON_ID}|2|2|${RECORD_COUNT}
EOF

# 2 — .par file with 3 JSONL records, all matching RECON_ID=1
mkdir -p output/get_kafka/${DATA_FEED}_${ASOF_DT}
cat > output/get_kafka/${DATA_FEED}_${ASOF_DT}/${DATA_FEED}.par << 'EOF'
{"reconciliationGroupId": 1, "accountId": "ACC-001", "tradeDate": "2026-04-22", "mandatorCode": "022"}
{"reconciliationGroupId": 1, "accountId": "ACC-002", "tradeDate": "2026-04-22", "mandatorCode": "022"}
{"reconciliationGroupId": 1, "accountId": "ACC-003", "tradeDate": "2026-04-22", "mandatorCode": "022"}
EOF
```

**Verify the files are in place before continuing:**

```bash
ls output/get_kafka/STATUS_FEED_00_2026-04-22/
# STATUS_FEED_00_2026-04-22_metadata.txt

ls output/get_kafka/DATA_FEED_ST_2026-04-22/
# DATA_FEED_ST.par

wc -l output/get_kafka/DATA_FEED_ST_2026-04-22/DATA_FEED_ST.par
# 3
```

---

## 4. Step 3 — Run the Filter Directly

Run from inside `00_plugins/` so Python finds the `dsf_logging` stub. Set `PC_LOD_PROC_PATH` to the directory that contains the metadata folder.

```bash
cd 00_plugins

export PC_LOD_PROC_PATH="/full/path/to/trigger_based_new_podman/output/get_kafka"

python3 filter_recon_group.py \
    --log-file          plugin_test.log \
    --kafka-data        ../output/get_kafka/DATA_FEED_ST_2026-04-22/DATA_FEED_ST.par \
    --status-feed-name  STATUS_FEED_00 \
    --asof-date         2026-04-22 \
    --metadata-suffix   _metadata.txt \
    --separator         "|"

echo "Exit code: $?"
```

**Expected exit code: 0**

The first line of the log tells you the resolved path where logs are being written:

```
2026-06-26T10:00:00 INFO     filterReconGroup log file resolved to: /full/path/to/00_plugins/plugin_test.log
```

**Expected log output (key lines):**

```
======================================================================
FILTER_RECON_GROUP — startup parameters
  PC_LOD_PROC_PATH        : /full/path/to/output/get_kafka
  ASOF_DT (--asof-date)   : 2026-04-22
  STATUSMESSAGES_FEED_NAME: STATUS_FEED_00
  METADATA_FILE_SUFFIX    : _metadata.txt
  Metadata file path      : .../STATUS_FEED_00_2026-04-22/STATUS_FEED_00_2026-04-22_metadata.txt
  Input .par file         : .../DATA_FEED_ST_2026-04-22/DATA_FEED_ST.par
======================================================================
...
Target reconciliationGroupId : 1
Expected message count       : 3
...
======================================================================
FILTER SUMMARY
  Total lines in .par file              : 3
  Matched  (reconciliationGroupId=1  )  : 3
  Dropped  (other reconciliation groups): 0
  Skipped  (parse / missing field)      : 0
  Expected count (from metadata) : 3
  Matched  count (after filter)  : 3
======================================================================
...
Done. 3 records written to .../DATA_FEED_ST.par. Ready for splitter.
```

**Check the filtered output:**

```bash
wc -l ../output/get_kafka/DATA_FEED_ST_2026-04-22/DATA_FEED_ST.par
# 3  (same — all records matched, none dropped)

cat ../output/get_kafka/DATA_FEED_ST_2026-04-22/DATA_FEED_ST.par
# only reconciliationGroupId=1 records
```

---

## 5. Step 4 — Failure Scenarios

### Scenario A — Count mismatch

Metadata says 5 records expected, but `.par` only has 3. Script must hard-fail and leave the `.par` file unchanged.

```bash
# Change total_messages_published in the metadata from 3 to 5
sed -i '' 's/|3$/|5/' ../output/get_kafka/STATUS_FEED_00_2026-04-22/STATUS_FEED_00_2026-04-22_metadata.txt

python3 filter_recon_group.py \
    --log-file plugin_test.log \
    --kafka-data ../output/get_kafka/DATA_FEED_ST_2026-04-22/DATA_FEED_ST.par \
    --status-feed-name STATUS_FEED_00 \
    --asof-date 2026-04-22 \
    --metadata-suffix _metadata.txt \
    --separator "|"

echo "Exit code: $?"   # expect 1
```

**Expected log:**
```
ERROR    COUNT MISMATCH: expected 5 records for reconciliationGroupId=1, got 3. Splitter will NOT run.
```

---

### Scenario B — Metadata file missing

Status messages pipeline has not run yet, or ran for a different date.

```bash
# Remove the metadata file
rm ../output/get_kafka/STATUS_FEED_00_2026-04-22/STATUS_FEED_00_2026-04-22_metadata.txt

python3 filter_recon_group.py \
    --log-file plugin_test.log \
    --kafka-data ../output/get_kafka/DATA_FEED_ST_2026-04-22/DATA_FEED_ST.par \
    --status-feed-name STATUS_FEED_00 \
    --asof-date 2026-04-22 \
    --metadata-suffix _metadata.txt \
    --separator "|"

echo "Exit code: $?"   # expect 1
```

**Expected log:**
```
ERROR    Metadata file not found: .../STATUS_FEED_00_2026-04-22_metadata.txt
         The status-messages script must complete successfully before this step.
```

---

### Scenario C — Mixed reconciliation groups

`.par` contains records from two different `reconciliationGroupId` values (e.g. a stale run mixed with the current run). Only records matching the ID in the metadata must survive. Count must still match after filtering.

```bash
# Restore metadata: expects 3 records for reconciliationGroupId=1
cat > ../output/get_kafka/STATUS_FEED_00_2026-04-22/STATUS_FEED_00_2026-04-22_metadata.txt << 'EOF'
export_datetime|username|business_date|mandator|producer_name|feed_name|reconciliation_group_id|instances_counted|total_expected_instances|total_messages_published
2026-04-22T23:59:00|testuser|2026-04-22|022|PRODUCER_A|STATUS_FEED_00|1|2|2|3
EOF

# .par with 5 records: 3 for group 1 (current), 2 for group 2 (stale)
cat > ../output/get_kafka/DATA_FEED_ST_2026-04-22/DATA_FEED_ST.par << 'EOF'
{"reconciliationGroupId": 2, "accountId": "OLD-001", "tradeDate": "2026-04-22", "mandatorCode": "022"}
{"reconciliationGroupId": 1, "accountId": "ACC-001", "tradeDate": "2026-04-22", "mandatorCode": "022"}
{"reconciliationGroupId": 2, "accountId": "OLD-002", "tradeDate": "2026-04-22", "mandatorCode": "022"}
{"reconciliationGroupId": 1, "accountId": "ACC-002", "tradeDate": "2026-04-22", "mandatorCode": "022"}
{"reconciliationGroupId": 1, "accountId": "ACC-003", "tradeDate": "2026-04-22", "mandatorCode": "022"}
EOF

python3 filter_recon_group.py \
    --log-file plugin_test.log \
    --kafka-data ../output/get_kafka/DATA_FEED_ST_2026-04-22/DATA_FEED_ST.par \
    --status-feed-name STATUS_FEED_00 \
    --asof-date 2026-04-22 \
    --metadata-suffix _metadata.txt \
    --separator "|"

echo "Exit code: $?"                                                         # expect 0
wc -l ../output/get_kafka/DATA_FEED_ST_2026-04-22/DATA_FEED_ST.par          # expect 3
```

**Expected log summary:**
```
FILTER SUMMARY
  Total lines in .par file              : 5
  Matched  (reconciliationGroupId=1  )  : 3
  Dropped  (other reconciliation groups): 2
  Breakdown of dropped records by reconciliationGroupId:
    reconciliationGroupId=2   : 2 record(s)
  Expected count (from metadata) : 3
  Matched  count (after filter)  : 3
```

---

## 6. Step 5 — Simulate the Plugin Call

`odp_asi_trans` can't be run in full locally because it depends on `$DWP_ROOT`, `$log_filename`, `cli.py`, and `splitter.py` which are all framework-injected in production. This local runner script simulates the filter step exactly as the plugin calls it.

Create `run_filter_local.sh` in the project root:

```bash
#!/usr/bin/env bash
# Simulates the filter_recon_group step from odp_asi_trans.
# Usage:   bash run_filter_local.sh <ASOF_DT> [MANDATOR]
# Example: bash run_filter_local.sh 2026-04-22 022

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ASOF_DT="${1:-$(date +%Y-%m-%d)}"
MANDATOR="${2:-022}"

STATUS_FEED="STATUS_FEED_00"
DATA_FEED="DATA_FEED_ST"
PAR_FILE="$SCRIPT_DIR/output/get_kafka/${DATA_FEED}_${ASOF_DT}/${DATA_FEED}.par"
LOG_FILE="plugin_filter_${ASOF_DT}.log"

export PC_LOD_PROC_PATH="$SCRIPT_DIR/output/get_kafka"
export PYTHONPATH="$SCRIPT_DIR/00_plugins:$PYTHONPATH"

echo ">>> FILTER STEP — local simulation"
echo "    ASOF_DT          : $ASOF_DT"
echo "    MANDATOR         : $MANDATOR"
echo "    PAR_FILE         : $PAR_FILE"
echo "    PC_LOD_PROC_PATH : $PC_LOD_PROC_PATH"
echo "    Log written to   : 00_plugins/$LOG_FILE  (full path shown inside log)"
echo ""

python3 "$SCRIPT_DIR/00_plugins/filter_recon_group.py" \
    --log-file         "$LOG_FILE" \
    --kafka-data       "$PAR_FILE" \
    --status-feed-name "$STATUS_FEED" \
    --asof-date        "$ASOF_DT" \
    --metadata-suffix  "_metadata.txt" \
    --separator        "|"

rc=$?
if [[ $rc -ne 0 ]]; then
    echo "FAILED (rc=$rc) — check 00_plugins/$LOG_FILE"
    exit 1
fi

echo ""
echo "SUCCESS — filtered .par is ready for the splitter"
echo "    $(wc -l < "$PAR_FILE") record(s) in $PAR_FILE"
```

Run it:

```bash
bash run_filter_local.sh 2026-04-22 022
```

**How this maps to the real `odp_asi_trans` plugin:**

| `odp_asi_trans` variable | Local equivalent |
|---|---|
| `$log_filename` (framework-injected) | `plugin_filter_${ASOF_DT}.log` (local file in `00_plugins/`) |
| `$1` (input `.par` file path) | `output/get_kafka/${DATA_FEED}_${ASOF_DT}/${DATA_FEED}.par` |
| `$DWP_ROOT/feeds/.../bin/filter_recon_group.py` | `00_plugins/filter_recon_group.py` |
| `${B4Q_ASOF_DT}` | `$ASOF_DT` argument |
| `$PC_LOD_PROC_PATH` | `output/get_kafka` |

---

## 7. Troubleshooting

| Symptom | Cause | Fix |
|---|---|---|
| `No module named 'dsf_logging'` | Stub not in `00_plugins/` or not on `PYTHONPATH` | Create `00_plugins/dsf_logging.py` — see Step 1 |
| `Metadata file not found` | Pipeline hasn't run yet, or `PC_LOD_PROC_PATH` points to the wrong directory | Run the status messages pipeline first, or create the metadata file manually — see Step 2 |
| `COUNT MISMATCH` | `.par` record count does not match `total_messages_published` in metadata | Check that the metadata was written by the same pipeline run that produced the `.par` |
| Exit 0 but `.par` has fewer lines than before | Working correctly — stale reconciliation groups were dropped | Check the FILTER SUMMARY in the log for the breakdown by group |
| `--asof-date` missing error | Argument not passed | `--asof-date` is required — always pass it explicitly |
| `ERROR: --log-file argument is required` | `--log-file` missing | Always pass `--log-file` — even a bare filename like `test.log` is accepted |
| Log written to unexpected location | Bare filename resolves to the script's working directory | Check the first log line — it always prints the full resolved path |
