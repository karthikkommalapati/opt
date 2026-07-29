# DSF Trigger-Based Pipeline — L2 Support Guide

This document covers two things:
1. How the two pipeline scripts are tied together — what each does, how they hand off to each other
2. L2 support procedures — how to diagnose a failure and restart a job

---

## Part 1 — How the Two Scripts Work Together

### The Big Picture

The pipeline runs in two sequential phases. **Phase 1 must complete successfully before Phase 2 can start.** The handoff between them is a metadata file written to disk.

```
[ Upstream Publisher ]
        │
        │  publishes STATUS messages to inflow-topic
        │  (one message per instance, contains expected business record count)
        ▼
┌─────────────────────────────────────────┐
│  PHASE 1                                │
│  kafka_trigger_status_messages.py       │
│                                         │
│  Reads:   inflow-topic (Kafka)          │
│  Checks:  all instances arrived?        │
│  Writes:  metadata file  ◄──── KEY      │
│           output data file              │
└──────────────────┬──────────────────────┘
                   │
                   │  metadata file contains:
                   │    - expected business record count
                   │    - filter values (mandator, date, group)
                   ▼
┌─────────────────────────────────────────┐
│  PHASE 2                                │
│  00_get_kafka.py                        │
│                                         │
│  Reads:   metadata file (from Phase 1)  │
│  Reads:   business-topic (Kafka)        │
│  Checks:  actual count vs expected      │
│  Writes:  business data output file     │
└─────────────────────────────────────────┘
```

---

### Phase 1 — kafka_trigger_status_messages.py

**What it does:**

1. Connects to the **status/inflow Kafka topic** (`inflow-topic`)
2. Scans only messages within the configured **time window** (e.g. 23:59–00:00 around midnight)
3. Filters to messages from the configured producer (`CLIENT_STRUCTURES`)
4. Validates that **every expected instance** has reported in — instanceIndex 0, 1, 2, ... N-1 must all be present for the highest `reconciliationGroupId`
5. Sums `numberOfMessagesPublished` across all instances → total expected business record count
6. Writes two output files:
   - **Metadata file** — the handshake for Phase 2 (expected count + filter values)
   - **Output data file** — the consumed status messages

**Run command:**
```bash
bash run_local.sh YYYY-MM-DD [MANDATOR]
# Example:
bash run_local.sh 2026-04-22 022
```

**Config file:** `status_messages_config.json`

**Output files:**
```
output/data/CPSB4Q00_2026-04-22.par           ← status messages data
output/data/CPSB4Q00_2026-04-22_metadata.txt  ← handshake file for Phase 2
output/logs/                                  ← log file
```

**Phase 1 succeeds when:** All instances are present, data is written, metadata file exists, exit code 0.

**Phase 1 enters retry mode when:** Some instances are missing. It waits up to `MAX_LISTEN_DURATION_HOURS` (default 2h in production) for the missing instances to arrive.

**Phase 1 exits cleanly with no output when:** `ALLOW_NO_DATA=YES` and no messages arrived within the time window. No metadata file is written. Phase 2 will also exit cleanly.

---

### Phase 2 — 00_get_kafka.py

**What it does:**

1. Reads the **metadata file** written by Phase 1 to learn: expected count + filter values
2. Connects to the **business Kafka topic** (`business-topic`)
3. Scans messages within its own configured **time window** (e.g. 16:00 yesterday → 16:00 today)
4. Filters messages that match the filter values from the metadata (mandator, business date, reconciliation group)
5. Counts matching messages and compares to expected count using a tolerance threshold (default ±10%)
6. Accepts if actual count is at or above the tolerance floor; retries if under; fails if deadline reached and still below floor

**Run command:**
```bash
bash run_get_kafka_local.sh YYYY-MM-DD [MANDATOR]
# Example:
bash run_get_kafka_local.sh 2026-04-22 022
```

**Config file:** `get_kafka_config.json`

**Output files:**
```
output/get_kafka/CPSB4QST_2026-04-22/CPSB4QST.par      ← business records
output/get_kafka/CPSB4QST_get_kafka_validation_log.txt  ← SUCCESS/FAILED row appended each run
output/logs/                                            ← log file
```

**Phase 2 succeeds when:** Actual count ≥ tolerance floor, data written, exit code 0.

**Phase 2 retries when:** Actual count < expected. It waits up to `MAX_LISTEN_DURATION_HOURS` for more records to arrive.

**Phase 2 fails when:** Deadline reached and actual count is still below the tolerance floor, OR the metadata file does not exist.

---

### The Metadata File — The Handshake

This file is what ties the two phases together. Phase 1 writes it; Phase 2 reads it.

**Location:** `output/data/CPSB4Q00_<ASOF_DT>_metadata.txt`
(path controlled by `STATUS_MESSAGES_FEED_NAME` and `METADATA_FILE_SUFFIX` in both config files)

**Contents (example):**
```
export_datetime|username|business_date|mandator|producer_name|feed_name|reconciliation_group_id|instances_counted|total_expected_instances|total_messages_published
2026-04-22T23:59:00|svc_user|2026-04-22|022|CLIENT_STRUCTURES|CPSB4Q00|1|2|2|1100
```

The critical field is `total_messages_published` — this is the expected count Phase 2 will validate against.

**If this file does not exist:** Phase 2 exits immediately with exit code 1 before even connecting to Kafka.

---

### How Filter Values Cross Between Scripts

Status messages use different field names from business messages. The `METADATA_FILTER_FIELD_MAP` config key bridges them:

| Status message field | → | Business message field |
|---|---|---|
| `mandator` | → | `mandatorCode` |
| `business_date` | → | `businessDate` |
| `reconciliation_group_id` | → | `reconciliationGroupId` |

Phase 1 reads `mandator=022` from the status message and writes it into the metadata file.
Phase 2 reads `mandatorCode=022` from the metadata file and uses it to filter business records.

---

### Count Tolerance

Phase 2 does not require an exact match. It computes a floor:

```
floor = expected_count − (expected_count × tolerance_pct / 100)

Example: expected = 1100, tolerance = 10%
  floor = 990

  actual = 1050 → 1050 ≥ 990 → ACCEPT ✓
  actual = 900  → 900  < 990  → FAIL ✗
```

Tolerance is set in `get_kafka_config.json`:
- `METADATA_COUNT_TOLERANCE_PCT` — global default (e.g. `"10"`)
- `LOCATION_TOLERANCE_PCT` — per-mandator override (e.g. `{"022": "10"}`)

---

### End-to-End Happy Path

```
T+00:00  Publisher sends 2 status messages to inflow-topic:
           Instance 0: mandator=022, date=2026-04-22, group=1, count=500
           Instance 1: mandator=022, date=2026-04-22, group=1, count=600

T+00:01  Phase 1 runs:
           → reads both messages
           → both instances present (0 of 2, 1 of 2) ✓
           → total expected = 500 + 600 = 1,100
           → writes CPSB4Q00_2026-04-22_metadata.txt
           → writes CPSB4Q00_2026-04-22.par
           → EXIT code 0 ✓

T+00:05  1,100 business records published to business-topic

T+00:06  Phase 2 runs:
           → reads metadata file → expected=1100, filter: mandator=022, date=2026-04-22, group=1
           → scans business-topic
           → filters records: mandatorCode=022, businessDate=2026-04-22, reconciliationGroupId=1
           → count = 1,100 ≥ expected → stable for 2 reads → ACCEPT ✓
           → writes CPSB4QST_2026-04-22/CPSB4QST.par
           → EXIT code 0 ✓
```

---

## Part 2 — L2 Support Guide

### Step 1 — Confirm Which Phase Failed

Check the validation log and the presence of the metadata file:

```bash
# Check Phase 2 validation log (most recent entry is at the bottom)
tail -5 output/get_kafka/CPSB4QST_get_kafka_validation_log.txt

# Check whether the metadata file exists
ls -l output/data/CPSB4Q00_<ASOF_DT>_metadata.txt

# Check exit codes by looking at the log files
ls -lt output/logs/    # most recent log is first
```

**Decision tree:**

```
Metadata file MISSING?
  └── YES → Phase 1 failed or never ran → go to Section A
  └── NO  → Phase 1 succeeded
              └── Phase 2 validation log shows FAILED?
                    └── YES → go to Section B
                    └── NO  → Phase 2 may still be running, or never ran → go to Section C
```

---

### Section A — Phase 1 Failed (Metadata File Missing or Empty)

The metadata file was not written. Phase 2 cannot run until this is resolved.

#### A1 — Check the Phase 1 log

```bash
# Find the most recent Phase 1 log
ls -lt output/logs/ | head -5
# Open the relevant log and search for the failure reason
grep -i "error\|fail\|missing\|timeout\|warning" output/logs/<log_file>
```

#### A2 — Identify the cause

| Log message | Cause | Fix |
|---|---|---|
| `Missing instances: [2, 3]` / retry loop running | Some instance messages not yet published by upstream | Wait for upstream or increase `MAX_LISTEN_DURATION_HOURS` then rerun |
| `No data consumed` (with `ALLOW_NO_DATA=YES`) | No status messages arrived in the time window | Check upstream publisher; check time window with `show_window.py` |
| `No data consumed from any partition !!!` (exit 1) | No messages AND `ALLOW_NO_DATA=NO` | Investigate upstream publisher |
| `VALIDATION FAILED: Duplicate instanceIds found` | Topic was produced into twice without wiping | Wipe topic, produce once; OR check if upstream publisher double-published |
| `Schema not found` / `Schema registry error` | Schema not registered | Re-register schema (see Section D) |
| `Connection refused` / broker error | Kafka broker is down | Restart broker (see Section E) |
| `eventTimestamp outside time window` | Messages published with wrong timestamp | Check `show_window.py` output; contact upstream publisher |

#### A3 — Restart Phase 1

Once the root cause is resolved:

```bash
# Confirm time window is correct for your date
python3 show_window.py 2026-04-22 022

# Rerun Phase 1
bash run_local.sh 2026-04-22 022

# Confirm metadata file was written
ls -l output/data/CPSB4Q00_2026-04-22_metadata.txt

# Check it has a data row (should be 2 lines: header + 1 data row)
wc -l output/data/CPSB4Q00_2026-04-22_metadata.txt
```

Then proceed to restart Phase 2 (Section B3).

---

### Section B — Phase 2 Failed (Metadata File Exists, Count Validation Failed)

#### B1 — Check the validation log

```bash
tail -20 output/get_kafka/CPSB4QST_get_kafka_validation_log.txt
```

Look for `failure_reason` in the most recent FAILED row. Common values:

| `failure_reason` | Meaning |
|---|---|
| `COUNT_BELOW_TOLERANCE` | Actual record count stayed below the floor for the entire retry window |
| `METADATA_NOT_FOUND` | Phase 1 metadata file was absent when Phase 2 started |
| `METADATA_MALFORMED` | Metadata file exists but has wrong number of rows or columns |
| `NO_DATA` | Zero records found in the time window |

#### B2 — Identify the cause

**COUNT_BELOW_TOLERANCE**

```bash
# Check the Phase 2 log for the actual vs expected counts
grep -i "count\|expected\|tolerance\|floor" output/logs/<log_file>
```

Possible causes:
- Business records not yet published to Kafka when the retry window ran out → wait and rerun
- Records published with the wrong `mandatorCode`, `tradeDate`, or `reconciliationGroupId` → contact upstream publisher
- Records published outside the time window (wrong `std_enqueueTime`) → contact upstream publisher
- `METADATA_COUNT_TOLERANCE_PCT` is set too tightly → raise with L3 before changing config

**METADATA_NOT_FOUND**

Phase 1 must be rerun first. Go to Section A.

**METADATA_MALFORMED**

```bash
# Inspect the metadata file
cat output/data/CPSB4Q00_2026-04-22_metadata.txt

# It must have exactly 2 non-blank lines: 1 header + 1 data row
wc -l output/data/CPSB4Q00_2026-04-22_metadata.txt
```

If the file has more than 2 lines (duplicate rows), delete it and rerun Phase 1 to regenerate it cleanly:
```bash
rm output/data/CPSB4Q00_2026-04-22_metadata.txt
bash run_local.sh 2026-04-22 022
```

**NO_DATA**

Business records may not be in the topic or may be outside the configured time window.

```bash
# How many messages are in business-topic?
podman exec redpanda rpk topic describe business-topic -p
# Look at HIGH-WATERMARK — that is the total message count

# What time window is Phase 2 using?
python3 show_window.py 2026-04-22 022
# The business data window will differ from the status messages window
# Phase 2 uses START_TS/STOP_TS from get_kafka_config.json
```

#### B3 — Restart Phase 2

```bash
# Rerun Phase 2
bash run_get_kafka_local.sh 2026-04-22 022

# Check the validation log for the result
tail -5 output/get_kafka/CPSB4QST_get_kafka_validation_log.txt

# Confirm output file exists and has content
wc -l output/get_kafka/CPSB4QST_2026-04-22/CPSB4QST.par
```

A successful run writes `SUCCESS` to the validation log and exit code 0.

---

### Section C — Phase 2 Never Ran

If the validation log has no entry for today's date, Phase 2 was never triggered.

1. Confirm Phase 1 completed and the metadata file exists:
```bash
ls -l output/data/CPSB4Q00_2026-04-22_metadata.txt
```

2. If Phase 1 is still running, wait for it. Check with:
```bash
ps aux | grep kafka_trigger_status_messages
```

3. Once Phase 1 is complete, run Phase 2:
```bash
bash run_get_kafka_local.sh 2026-04-22 022
```

---

### Section D — Schema Registry Errors

**Symptom:** Either script fails immediately with a schema registry error.

```
SchemaRegistryError / Schema not found / Connection refused to :8081
```

**Check schema registry:**
```bash
curl http://localhost:8081/subjects
# Expected: ["business-topic-value","inflow-topic-value"]
# If empty [] or connection refused, schemas need to be re-registered
```

**Re-register schemas:**
```bash
# Delete old subjects first (safe to run even if they don't exist)
curl -X DELETE http://localhost:8081/subjects/inflow-topic-value
curl -X DELETE http://localhost:8081/subjects/business-topic-value

# Re-register
python3 register_schema.py           # for Phase 1 (inflow-topic)
python3 register_get_kafka_schema.py # for Phase 2 (business-topic)

# Verify
curl http://localhost:8081/subjects
```

Then rerun the failed phase.

---

### Section E — Kafka Broker Down

**Symptom:** Either script fails with a connection error to `localhost:9092`.

**Check broker state:**
```bash
podman ps -a | grep redpanda
```

| Output | Meaning | Action |
|---|---|---|
| `Up X minutes` | Broker is running | Try `podman exec redpanda rpk cluster info`; if that also fails, broker may be starting up — wait 30s |
| `Exited` | Container stopped (e.g. after pod restart) | `podman start redpanda` |
| _(no output)_ | Container destroyed | `bash start_kafka.sh` — then re-register schemas (Section D) |

**Restart a stopped container (topics and schemas preserved):**
```bash
podman start redpanda

# Wait for it to be ready
podman exec redpanda rpk cluster info

# Verify schemas are still there
curl http://localhost:8081/subjects
```

**Full restart after container destruction:**
```bash
bash start_kafka.sh
python3 register_schema.py
python3 register_get_kafka_schema.py
```

After broker is back up, rerun the failed phase from the appropriate section above.

---

### Quick-Check Checklist (use this first on any alert)

```
□ 1. Is the broker running?
      podman ps -a | grep redpanda
      → should show "Up"

□ 2. Are both schemas registered?
      curl http://localhost:8081/subjects
      → should show both inflow-topic-value and business-topic-value

□ 3. Does the metadata file exist for today's date?
      ls -l output/data/CPSB4Q00_<DATE>_metadata.txt
      → missing = Phase 1 did not complete

□ 4. What does the Phase 2 validation log say?
      tail -5 output/get_kafka/CPSB4QST_get_kafka_validation_log.txt
      → look for SUCCESS or FAILED and the failure_reason

□ 5. How many messages are in each topic?
      podman exec redpanda rpk topic describe inflow-topic   -p   # HIGH-WATERMARK
      podman exec redpanda rpk topic describe business-topic -p   # HIGH-WATERMARK
```

---

### Restart Sequence (full pipeline rerun)

Use this when you need to rerun both phases for a given date from scratch.

```bash
# Step 1 — confirm broker is up
podman ps -a | grep redpanda
# If Exited: podman start redpanda
# If gone:   bash start_kafka.sh && python3 register_schema.py && python3 register_get_kafka_schema.py

# Step 2 — confirm schemas are registered
curl http://localhost:8081/subjects

# Step 3 — check the time window for your date
python3 show_window.py 2026-04-22 022

# Step 4 — rerun Phase 1
bash run_local.sh 2026-04-22 022

# Step 5 — confirm metadata file written
ls -l output/data/CPSB4Q00_2026-04-22_metadata.txt
wc -l output/data/CPSB4Q00_2026-04-22_metadata.txt   # must be 2

# Step 6 — rerun Phase 2
bash run_get_kafka_local.sh 2026-04-22 022

# Step 7 — confirm success
tail -3 output/get_kafka/CPSB4QST_get_kafka_validation_log.txt
# → should show SUCCESS
wc -l output/get_kafka/CPSB4QST_2026-04-22/CPSB4QST.par
# → should match expected record count
```

---

### When to Escalate to L3

Escalate if:
- Upstream publisher is confirmed down or has published wrong data — this is outside L2 scope
- Count is consistently 0 and the broker, schemas, and time window are all confirmed correct
- The metadata file is being written with an unexpected `total_messages_published` value (wrong expected count)
- Schema changes are needed (schema file has changed; compatibility errors from the registry)
- Config changes beyond tolerance or retry settings are needed
- The pipeline has never run successfully for a new feed or mandator

---

### Key Files Reference

| File | Purpose |
|---|---|
| `status_messages_config.json` | Phase 1 configuration |
| `get_kafka_config.json` | Phase 2 configuration |
| `output/data/CPSB4Q00_<DATE>_metadata.txt` | Metadata handshake file (Phase 1 → Phase 2) |
| `output/data/CPSB4Q00_<DATE>.par` | Phase 1 output (status messages) |
| `output/get_kafka/CPSB4QST_<DATE>/CPSB4QST.par` | Phase 2 output (business records) |
| `output/get_kafka/CPSB4QST_get_kafka_validation_log.txt` | Phase 2 SUCCESS/FAILED audit trail |
| `output/logs/` | Log files for both phases |
