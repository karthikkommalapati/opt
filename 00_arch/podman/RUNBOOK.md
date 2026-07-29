# Local Simulation Runbook

This guide covers everything needed to set up and run the local Kafka simulation environment — from first-time setup through daily use, scenario testing, and diagnosis. Follow sections 3 → 9 to get up and running from scratch.

---

## Table of Contents

1. [What's in This Directory](#1-whats-in-this-directory)
2. [How the Two Pipelines Work](#2-how-the-two-pipelines-work)
3. [One-Time Setup](#3-one-time-setup)
4. [Starting the Environment (Fresh Start)](#4-starting-the-environment-fresh-start)
5. [After Pod Restart](#5-after-pod-restart)
6. [Checking the Environment](#6-checking-the-environment)
7. [Registering Schemas](#7-registering-schemas)
8. [Publishing Messages](#8-publishing-messages)
9. [Running the Pipelines](#9-running-the-pipelines)
10. [Reset and Cleanup](#10-reset-and-cleanup)
11. [Scenarios — Status Messages Pipeline](#11-scenarios--status-messages-pipeline)
12. [Scenarios — Business Data Pipeline](#12-scenarios--business-data-pipeline)
13. [Command Index](#13-command-index)
14. [Troubleshooting](#14-troubleshooting)

---

## 1. What's in This Directory

| File | Purpose |
|---|---|
| `start_kafka.sh` | Start Redpanda container, create both topics |
| `stop_kafka.sh` | Stop and remove the Redpanda container |
| `reset_kafka.sh` | Wipe topics + schemas without stopping the broker |
| `run_local.sh` | Run the status messages pipeline (`kafka_trigger_status_messages.py`) |
| `run_get_kafka_local.sh` | Run the business data pipeline (`00_get_kafka.py`) |
| `kafka_trigger_status_messages.py` | Status messages consumer — reads `inflow-topic`, validates instance completeness, writes output |
| `00_get_kafka.py` | Business data consumer — reads `business-topic`, validates count, writes output |
| `register_schema.py` | Register status messages Avro schema with local Schema Registry |
| `register_get_kafka_schema.py` | Register business data Avro schema with local Schema Registry |
| `produce_messages.py` | Produce status messages into `inflow-topic` from a JSONL file |
| `produce_get_kafka_messages.py` | Produce business records into `business-topic` from a JSONL file |
| `show_window.py` | Print the exact time window the script will use for a given date + mandator |
| `status_messages_config.json` | Config for the status messages pipeline |
| `get_kafka_config.json` | Config for the business data pipeline |
| `assertf.py` | Stub for the UBS assertion framework (used by both scripts) |
| `CHANGES.md` | SSL removal changes applied to both scripts for local use |
| `input/status_messages_schema.json` | Avro schema for status messages |
| `input/status_messages_data.jsonl` | Sample status message records |
| `input/business_data_schema.json` | Avro schema for business data records |
| `input/business_data.jsonl` | Sample business data records |

---

## 2. How the Two Pipelines Work

One Redpanda broker (a Kafka-compatible message broker) runs in a container and serves both pipelines. Each pipeline has its own topic and its own schema.

```
┌─────────────────────────────────────────────────────────┐
│  Redpanda container  (localhost:9092 Kafka / :8081 Schema Registry)  │
│                                                         │
│   inflow-topic           business-topic                 │
│   (status messages)      (business records)             │
└───────────┬──────────────────────┬──────────────────────┘
            │                      │
   produce_messages.py    produce_get_kafka_messages.py
            │                      │
   kafka_trigger_         00_get_kafka.py
   status_messages.py
   → output/data/         → output/get_kafka/
```

**Status messages pipeline** (`inflow-topic`):
- Messages describe how many records a downstream producer has published for a given business date
- Script validates that ALL expected instances have arrived for the maximum `reconciliationGroupId` — by default assumes 0-based contiguous indices (0 to N-1); set `INSTANCE_VALIDATION_MODE="UNIQUE_COUNT"` in config to instead just require N distinct instance IDs regardless of value (see `STATUS_MESSAGES_HOW_TO.md`)
- Only when all instances are present does it write the output and commit offsets

**Business data pipeline** (`business-topic`):
- Messages contain actual business records (trades, positions, etc.)
- Script validates the count against an expected number from a metadata file
- Uses a time window (`std_enqueueTime`) to filter which records to consume

Both pipelines are independent — they use the same broker but different topics. They can be run in sequence or independently.

---

## 3. One-Time Setup

### 3.1 Pull the Redpanda image

This only needs to be done once per machine/DevPod.

```bash
podman pull container-registry.ubs.net/base-images/redpanda:latest-23.2-alpine-20231028
```

> If `podman pull` fails with an EOF error, confirm you are using the full internal registry URL above — not Docker Hub.

### 3.2 Prepare input files

Two sets of input files are needed — one per pipeline. Place them in the `input/` directory.

**Status messages pipeline:**

| File | What to put here |
|---|---|
| `input/status_messages_schema.json` | Avro schema for the `inflow-topic` messages |
| `input/status_messages_data.jsonl` | Test messages — one JSON record per line |

**Business data pipeline:**

| File | What to put here |
|---|---|
| `input/business_data_schema.json` | Avro schema for the `business-topic` messages |
| `input/business_data.jsonl` | Test business records — one JSON record per line |

Sample files are already present in `input/` — use them as-is for initial testing.

### 3.3 Verify Python dependencies

Both scripts require `kafka-python`, `fastavro`, `pandas`, `requests`, and `pytz`. Confirm they are installed:

```bash
python3 -c "import kafka, fastavro, pandas, requests, pytz; print('OK')"
```

---

## 4. Starting the Environment (Fresh Start)

Use this section when starting for the first time, or after running `bash stop_kafka.sh` (which destroys the container).

### Step 1 — Start the broker

```bash
bash start_kafka.sh
```

This starts the Redpanda container and creates both topics (`inflow-topic` and `business-topic`) with 1 partition each.

### Step 2 — Verify the broker is ready

```bash
podman exec redpanda rpk cluster info
```

Expected output includes a broker ID and node address. If this fails, the container is not ready yet — wait a few seconds and try again.

### Step 3 — Register schemas

Both schemas must be registered before producing any messages. Schema registration is lost when the container is destroyed — you must redo this step after every `bash stop_kafka.sh` / `bash start_kafka.sh` cycle.

```bash
python3 register_schema.py           # status messages schema → inflow-topic-value
python3 register_get_kafka_schema.py # business data schema   → business-topic-value
```

Expected output: `Schema registered successfully. ID: <number>` for each.

### Step 4 — Verify schemas are registered

```bash
curl http://localhost:8081/subjects
```

Expected: `["business-topic-value","inflow-topic-value"]`

### Step 5 — Produce test messages

```bash
python3 produce_messages.py            # status messages → inflow-topic
python3 produce_get_kafka_messages.py  # business data   → business-topic
```

### Step 6 — Run the pipelines

Replace `2026-04-22` with the `businessDate` / `tradeDate` that appears in your JSONL data.

```bash
bash run_local.sh 2026-04-22 022          # status messages pipeline
bash run_get_kafka_local.sh 2026-04-22 022 # business data pipeline
```

Check output:
```
output/data/          ← status messages output
output/get_kafka/     ← business data output
output/logs/          ← log files for both
```

---

## 5. After Pod Restart

When a DevPod disconnects or the machine restarts, the Redpanda container **stops but is not destroyed**. The container, its topics, and its schemas are preserved. Do NOT run `start_kafka.sh` again — it will fail because a container named `redpanda` already exists.

### Step 1 — Check the container state

```bash
podman ps -a | grep redpanda
```

You will see one of:
- `Up X minutes` — container is already running, go to Step 3
- `Exited` — container is stopped, continue to Step 2
- _(no output)_ — container is gone, run `bash start_kafka.sh` from scratch (see Section 4)

### Step 2 — Restart the stopped container

```bash
podman start redpanda
```

This is fast — no image pull, no topic creation. Topics and their messages are preserved.

### Step 3 — Verify the broker is ready

```bash
podman exec redpanda rpk cluster info
```

If this succeeds, the broker is ready.

### Step 4 — Check schema state

Schemas survive a container stop/start. Verify they are still registered:

```bash
curl http://localhost:8081/subjects
```

If both subjects appear (`inflow-topic-value`, `business-topic-value`), you are ready to produce and run immediately.

If subjects are missing (empty response `[]`), re-register:

```bash
python3 register_schema.py
python3 register_get_kafka_schema.py
```

### Step 5 — Continue from where you left off

Topics and their messages are still intact. You can run the pipelines directly:

```bash
bash run_local.sh 2026-04-22 022
bash run_get_kafka_local.sh 2026-04-22 022
```

Or reproduce messages first if you want fresh data (see Section 8).

---

## 6. Checking the Environment

Use these checks at any time to understand the current state.

### Is the container running?

```bash
podman ps -a | grep redpanda
```

Look for `Up` (running) or `Exited` (stopped).

### Is the broker healthy?

```bash
podman exec redpanda rpk cluster info
```

Success = broker is ready. Failure = container is stopped or starting up.

### Are both topics present?

```bash
podman exec redpanda rpk topic list
```

Expected: `inflow-topic` and `business-topic` both listed.

### How many messages are in each topic?

```bash
podman exec redpanda rpk topic describe inflow-topic  -p
podman exec redpanda rpk topic describe business-topic -p
```

Look at the `HIGH-WATERMARK` column — that is the total message count per partition.

### Are schemas registered?

```bash
curl http://localhost:8081/subjects
```

Expected: `["business-topic-value","inflow-topic-value"]`

---

## 7. Registering Schemas

### When you need to register (and when you don't)

| Situation | Action |
|---|---|
| First-time setup | Register both — see Section 4 |
| Container stopped then restarted (`podman start redpanda`) | Nothing — schemas survive |
| Container destroyed and recreated (`stop_kafka.sh` → `start_kafka.sh`) | Re-register both |
| `bash reset_kafka.sh` run | Re-register both — reset deletes schemas |
| Topic wiped (delete + create) without reset | Nothing — schema is independent of topic |
| Schema file changed | Delete old subject, re-register |
| Schema version mismatch error from script | Delete old subject, re-register |

### 7.1 Register status messages schema

```bash
python3 register_schema.py
```

Reads from `input/status_messages_schema.json`. Registers as subject `inflow-topic-value`.

### 7.2 Register business data schema

```bash
python3 register_get_kafka_schema.py
```

Reads from `input/business_data_schema.json`. Registers as subject `business-topic-value`.

### 7.3 Delete and re-register a schema

Only needed when the schema file has changed or you are getting schema mismatch errors.

```bash
# Delete the old subject
curl -X DELETE http://localhost:8081/subjects/inflow-topic-value   # status messages
curl -X DELETE http://localhost:8081/subjects/business-topic-value # business data

# Re-register
python3 register_schema.py
python3 register_get_kafka_schema.py
```

### 7.4 View a registered schema

```bash
curl http://localhost:8081/subjects/inflow-topic-value/versions/latest  | python3 -m json.tool
curl http://localhost:8081/subjects/business-topic-value/versions/latest | python3 -m json.tool
```

---

## 8. Publishing Messages

Schemas must be registered before producing (see Section 7). Producing into an already-populated topic ADDS messages — it does not replace them. Wipe the topic first if you want a clean set (see Section 10.1).

### 8.1 Status messages (inflow-topic)

```bash
# Default input file (input/status_messages_data.jsonl)
python3 produce_messages.py

# Custom file
python3 produce_messages.py --file input/my_data.jsonl
```

**Message format** — each line in the JSONL must be:

```json
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 0, "totalInstances": 3, "numberOfMessagesPublished": 100}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:10+00:00"}
```

Key fields:
- `mandatorCode` — must match `DSF_MANDATOR` you run with (e.g. `022`)
- `businessDate` — must match `ASOF_DT` you pass to `run_local.sh`
- `reconciliationGroupId` — the run ID; script picks the highest value
- `instanceIndex` / `totalInstances` — 0-based; all indices 0 to N-1 must be present (default `INSTANCE_VALIDATION_MODE="SEQUENTIAL"`; set to `"UNIQUE_COUNT"` if your producer uses non-sequential/arbitrary instance IDs — see `STATUS_MESSAGES_HOW_TO.md`)
- `eventTimestamp` — **must fall inside the time window** (check with `show_window.py`)

Check the time window before writing JSONL data:
```bash
python3 show_window.py 2026-04-22 022
```

Verify messages landed in the topic:
```bash
podman exec redpanda rpk topic describe inflow-topic -p
```

`HIGH-WATERMARK` must equal the number of messages you produced.

### 8.2 Business data (business-topic)

```bash
# Default input file (input/business_data.jsonl)
python3 produce_get_kafka_messages.py

# Custom file
python3 produce_get_kafka_messages.py --file input/my_records.jsonl
```

**Message format** — each line:

```json
{"std_enqueueTime": "2026-04-22T17:00:00+00:00", "std_legalEntity": "UBS_AG", "accountId": "ACC-001", "productType": "EQUITY", "quantity": 500.0, "currency": "CHF", "tradeDate": "2026-04-22", "settlementDate": "2026-04-24", "mandatorCode": "022"}
```

Key fields:
- `std_enqueueTime` — timestamp used for Kafka offset filtering; must be between `2026-04-22T16:00:00+00:00` and `2026-04-23T16:00:00+00:00` for `ASOF_DT=2026-04-22`
- `tradeDate` — must match `ASOF_DT`
- `mandatorCode` — must match the mandator you run with

Verify:
```bash
podman exec redpanda rpk topic describe business-topic -p
```

---

## 9. Running the Pipelines

### Before running — check the time window

Always confirm the time window for your chosen date and mandator before writing test data:

```bash
python3 show_window.py 2026-04-22 022
```

All `eventTimestamp` (status messages) and `std_enqueueTime` (business data) values in your JSONL files must fall inside the printed START → END range.

### 9.1 Status messages pipeline

```bash
bash run_local.sh ASOF_DT [MANDATOR]

# Examples
bash run_local.sh 2026-04-22          # uses default mandator 022
bash run_local.sh 2026-04-22 023      # explicit mandator
```

> Always run via `bash run_local.sh` — never run `python3 kafka_trigger_status_messages.py` directly. The runner sets all required DSF framework environment variables.

Output:
```
output/data/CPSB4Q00_2026-04-22.par         ← pipe-delimited data file
output/data/CPSB4Q00_2026-04-22_metadata.txt ← metadata file
output/logs/                                 ← log file
```

### 9.2 Business data pipeline

```bash
bash run_get_kafka_local.sh ASOF_DT [MANDATOR]

# Examples
bash run_get_kafka_local.sh 2026-04-22
bash run_get_kafka_local.sh 2026-04-22 023
```

> Always run via `bash run_get_kafka_local.sh` — never run `python3 00_get_kafka.py` directly. The runner creates a config symlink that the script needs (`1001_CPSB4QST_config.json → get_kafka_config.json`). Without it, `VALIDATE_TOPIC_MANDATOR` defaults to `YES` and the script fails immediately because the local topic name does not end with the mandator code.

Output:
```
output/get_kafka/CPSB4QST_2026-04-22/CPSB4QST.par ← business records
output/logs/                                        ← log file
```

### 9.3 Integrated flow (status messages → business data)

In production, `00_get_kafka.py` waits for the metadata file written by `kafka_trigger_status_messages.py` before consuming. To simulate this locally:

```bash
# Terminal 1 — run status messages first; it writes a metadata file when done
bash run_local.sh 2026-04-22 022

# Once Terminal 1 exits successfully, run business data
bash run_get_kafka_local.sh 2026-04-22 022
```

The metadata file is written to `output/data/` and read by `get_kafka` from that same path (controlled by `PC_LOD_PROC_PATH` in `run_get_kafka_local.sh`).

---

## 10. Reset and Cleanup

Three levels of reset — pick the one that matches what you need.

### 10.1 Wipe topic messages only (keep broker and schemas)

Use when you want fresh messages but do not need to change the schema.

```bash
# Status messages topic
podman exec redpanda rpk topic delete inflow-topic
podman exec redpanda rpk topic create inflow-topic --partitions 1 --replicas 1

# Business data topic
podman exec redpanda rpk topic delete business-topic
podman exec redpanda rpk topic create business-topic --partitions 1 --replicas 1
```

Schemas remain registered. Produce messages immediately after:

```bash
python3 produce_messages.py
python3 produce_get_kafka_messages.py
```

### 10.2 Full reset — wipe topics and schemas (keep broker running)

Use when starting a new test scenario, changing schemas, or clearing all data without restarting the broker.

```bash
# Wipe topics + schemas only
bash reset_kafka.sh

# Wipe topics + schemas + output/ directory
bash reset_kafka.sh --output
```

What `reset_kafka.sh` does:
1. Deletes `inflow-topic` and `business-topic` (all messages gone)
2. Permanently deletes both Schema Registry subjects
3. Recreates both topics empty

After running, **schemas are gone** — re-register before producing:

```bash
python3 register_schema.py
python3 register_get_kafka_schema.py
python3 produce_messages.py
python3 produce_get_kafka_messages.py
```

### 10.3 Full teardown — stop and destroy container

Use when you are done for the day or want a completely clean slate next time.

```bash
bash stop_kafka.sh
```

This stops and removes the container. All messages and schemas are lost. Next time, start from Section 4 (fresh start).

---

## 11. Scenarios — Status Messages Pipeline

All scenarios use `inflow-topic` and `run_local.sh`. Before each scenario:

1. Run `python3 show_window.py DATE MANDATOR` to get your exact time window
2. Reset the topic (Section 10.1) to start from a clean state
3. Write your JSONL data with `eventTimestamp` values inside the window

---

### Scenario A — Late publication (missing instance arrives during retry)

**What it tests**: Script starts with an incomplete instance set. Enters retry mode. The missing instance is published while it is waiting. Script detects it on next poll and completes.

**Rule**: ALL messages — both initial and late — must have `eventTimestamp` inside the time window. The retry loop filters by Kafka timestamp, not by wall-clock time.

**Step 1 — Check window:**
```bash
python3 show_window.py 2026-04-22 022
```

**Step 2 — Create `input/part1.jsonl`** (instances 0–2, `totalInstances=4`):
```json
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 0, "totalInstances": 4, "numberOfMessagesPublished": 100}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:10+00:00"}
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 1, "totalInstances": 4, "numberOfMessagesPublished": 200}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:20+00:00"}
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 2, "totalInstances": 4, "numberOfMessagesPublished": 150}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:30+00:00"}
```

**Step 3 — Create `input/part2.jsonl`** (instance 3 — the late one):
```json
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 3, "totalInstances": 4, "numberOfMessagesPublished": 175}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:50+00:00"}
```

**Step 4 — Reset topic and produce partial set:**
```bash
podman exec redpanda rpk topic delete inflow-topic
podman exec redpanda rpk topic create inflow-topic --partitions 1 --replicas 1
python3 produce_messages.py --file input/part1.jsonl
```

**Step 5 — Two terminals:**
```bash
# Terminal 1 — start script (finds 3 instances, enters retry loop)
bash run_local.sh 2026-04-22 022

# Terminal 2 — while Terminal 1 is waiting, publish the late instance
python3 produce_messages.py --file input/part2.jsonl
```

**Expected**: Terminal 1 logs `Missing instances: [3]` → polls → finds instance 3 → `Validation passed` → writes output.

---

### Scenario B — Mixed mandator messages

**What it tests**: Topic contains messages from two mandators. Script must process only its mandator and ignore all others.

**Step 1 — Check window:**
```bash
python3 show_window.py 2026-04-22 022
```

**Step 2 — Create `input/scenario_b.jsonl`** (mandators 022 and 023 interleaved):
```json
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 0, "totalInstances": 2, "numberOfMessagesPublished": 100}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:05+00:00"}
{"status": {"mandatorCode": "023", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 0, "totalInstances": 2, "numberOfMessagesPublished": 300}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:10+00:00"}
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 1, "totalInstances": 2, "numberOfMessagesPublished": 200}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:15+00:00"}
{"status": {"mandatorCode": "023", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 1, "totalInstances": 2, "numberOfMessagesPublished": 400}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:20+00:00"}
```

**Step 3 — Reset topic and produce:**
```bash
podman exec redpanda rpk topic delete inflow-topic
podman exec redpanda rpk topic create inflow-topic --partitions 1 --replicas 1
python3 produce_messages.py --file input/scenario_b.jsonl
```

**Step 4 — Run for mandator 022:**
```bash
bash run_local.sh 2026-04-22 022
```

**Expected**: Only 022 messages processed (counts 100, 200). 023 messages ignored.

**Optional — verify the other side:**
```bash
bash run_local.sh 2026-04-22 023
```

Only 023 messages (counts 300, 400) appear in output.

---

### Scenario C — Publishing while script is running (empty topic start)

**What it tests**: Script starts against an empty topic, enters retry loop immediately, then messages are published while it is waiting. Tests that it detects new messages without a restart.

**Step 1 — Check window:**
```bash
python3 show_window.py 2026-04-22 022
```

**Step 2 — Create `input/scenario_c.jsonl`** (complete set):
```json
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 0, "totalInstances": 3, "numberOfMessagesPublished": 100}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:10+00:00"}
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 1, "totalInstances": 3, "numberOfMessagesPublished": 200}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:20+00:00"}
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 2, "totalInstances": 3, "numberOfMessagesPublished": 150}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:30+00:00"}
```

**Step 3 — Reset to empty topic:**
```bash
podman exec redpanda rpk topic delete inflow-topic
podman exec redpanda rpk topic create inflow-topic --partitions 1 --replicas 1
```

**Step 4 — Two terminals:**
```bash
# Terminal 1 — start script against empty topic
bash run_local.sh 2026-04-22 022
# Logs: 0 messages found → enters retry loop

# Terminal 2 — produce while script is waiting (must be before MAX_LISTEN_DURATION_HOURS expires)
python3 produce_messages.py --file input/scenario_c.jsonl
```

Default `MAX_LISTEN_DURATION_HOURS=0.05` (~3 minutes). Produce before it expires.

**Expected**: Terminal 1 picks up messages on next poll → validation passes → output written.

---

### Scenario D — Multiple runIds (max runId selection)

**What it tests**: Topic has messages for two different `reconciliationGroupId` values. Script must select the highest runId only — a core business rule.

**Step 1 — Check window:**
```bash
python3 show_window.py 2026-04-22 022
```

**Step 2 — Create `input/scenario_d.jsonl`** (runId 1 and runId 2, same mandator/date):
```json
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 0, "totalInstances": 2, "numberOfMessagesPublished": 100}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:05+00:00"}
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 1, "totalInstances": 2, "numberOfMessagesPublished": 200}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:10+00:00"}
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 2, "instanceIndex": 0, "totalInstances": 2, "numberOfMessagesPublished": 150}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:15+00:00"}
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 2, "instanceIndex": 1, "totalInstances": 2, "numberOfMessagesPublished": 250}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:20+00:00"}
```

**Step 3 — Reset topic and produce:**
```bash
podman exec redpanda rpk topic delete inflow-topic
podman exec redpanda rpk topic create inflow-topic --partitions 1 --replicas 1
python3 produce_messages.py --file input/scenario_d.jsonl
```

**Step 4 — Run:**
```bash
bash run_local.sh 2026-04-22 022
```

**Expected**: Script logs `max reconciliationGroupId = 2`. Output contains only runId=2 messages (counts 150, 250). RunId=1 messages are in the topic but not written.

---

### Scenario E — Timeout with permanently incomplete data

**What it tests**: Script enters retry loop but the missing instance never arrives. Script must exhaust `MAX_LISTEN_DURATION_HOURS` and exit cleanly.

**Step 1 — Check window:**
```bash
python3 show_window.py 2026-04-22 022
```

**Step 2 — Create `input/scenario_e.jsonl`** (instances 0 and 1 only — instance 2 will never arrive):
```json
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 0, "totalInstances": 3, "numberOfMessagesPublished": 100}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:05+00:00"}
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 1, "totalInstances": 3, "numberOfMessagesPublished": 200}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:10+00:00"}
```

**Step 3 — Set a short timeout** in `status_messages_config.json` (restore after testing):
```json
"MAX_LISTEN_DURATION_HOURS": "0.02",
"RETRY_WAIT_SECONDS": "5"
```

**Step 4 — Reset topic and produce:**
```bash
podman exec redpanda rpk topic delete inflow-topic
podman exec redpanda rpk topic create inflow-topic --partitions 1 --replicas 1
python3 produce_messages.py --file input/scenario_e.jsonl
```

**Step 5 — Run (do not produce instance 2):**
```bash
bash run_local.sh 2026-04-22 022
```

**Expected**: Script logs `Missing instances: [2]` on every retry → exits after ~1 minute with a validation failure message. No output file written. Restore `MAX_LISTEN_DURATION_HOURS` after.

---

### Scenario F — Messages outside the time window

**What it tests**: Messages are in the topic but have `eventTimestamp` outside the configured window. Script must ignore them — the time filter is the first gate before any business logic.

**Step 1 — Find your exact window:**
```bash
python3 show_window.py 2026-04-22 022
```

Example output:
```
Window START : 2026-04-22T23:59:00+00:00
Window END   : 2026-04-23T00:00:00+00:00
```

**Step 2 — Create `input/scenario_f.jsonl`** using a timestamp BEFORE the window start:
```json
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 0, "totalInstances": 1, "numberOfMessagesPublished": 100}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T17:59:00+00:00"}
```

`17:59:00` is 6 hours before the `23:59` window start.

**Step 3 — Reset topic and produce the outside-window message:**
```bash
podman exec redpanda rpk topic delete inflow-topic
podman exec redpanda rpk topic create inflow-topic --partitions 1 --replicas 1
python3 produce_messages.py --file input/scenario_f.jsonl
```

**Step 4 — Confirm message is in topic but script cannot see it:**
```bash
podman exec redpanda rpk topic describe inflow-topic -p
# HIGH-WATERMARK = 1 (message is there)
```

**Step 5 — Run:**
```bash
bash run_local.sh 2026-04-22 022
```

**Expected**: Script finds 0 messages in the 23:59–00:00 window → retry loop → exits after timeout. No output file.

**Optional — confirm isolation**: produce an inside-window message and re-run. Only the inside-window message appears in output:
```bash
python3 produce_messages.py --file input/status_messages_data.jsonl
bash run_local.sh 2026-04-22 022
```

---

### Scenario G — Re-run behaviour and backup mechanism

**What it tests**: Running the script twice for the same `ASOF_DT`. Verifies data is re-exported correctly, and that `USR_VAL=1` (production re-run mode) renames the previous output to a backup file.

**Part 1 — Default re-run (USR_VAL=0)**

```bash
podman exec redpanda rpk topic delete inflow-topic
podman exec redpanda rpk topic create inflow-topic --partitions 1 --replicas 1
python3 produce_messages.py --file input/status_messages_data.jsonl
bash run_local.sh 2026-04-22 022   # first run
ls -l output/data/                 # note file timestamp
bash run_local.sh 2026-04-22 022   # second run — no topic wipe
ls -l output/data/                 # same file, updated timestamp
```

On the second run the script detects committed offsets at the end of the window, enters retry mode, re-reads from start offset, and rewrites the same output.

**Part 2 — Production re-run mode (USR_VAL=1) — backup**

Temporarily edit `run_local.sh`: change `TEST_USR_VAL="0"` to `TEST_USR_VAL="1"`, then:

```bash
bash run_local.sh 2026-04-22 022   # first run
bash run_local.sh 2026-04-22 022   # second run with USR_VAL=1
ls -l output/data/
```

Expected after second run:
```
output/data/CPSB4Q00_2026-04-22.par    ← fresh output
output/data/CPSB4Q00_2026-04-22.par.1  ← backup of first run
```

Restore `TEST_USR_VAL="0"` when done.

---

### Scenario H — Empty topic

**What it tests**: Script runs against a topic with zero messages. Must enter retry loop and exit gracefully without crashing.

```bash
podman exec redpanda rpk topic delete inflow-topic
podman exec redpanda rpk topic create inflow-topic --partitions 1 --replicas 1
bash run_local.sh 2026-04-22 022
```

**Expected**: Script logs no messages found → retries every `RETRY_WAIT_SECONDS` → exits after `MAX_LISTEN_DURATION_HOURS`. No crash. No output file.

---

## 12. Scenarios — Business Data Pipeline

All scenarios use `business-topic` and `run_get_kafka_local.sh`. The time window for `ASOF_DT=2026-04-22` (mandator 022) is `2026-04-22 16:00:00 UTC → 2026-04-23 16:00:00 UTC`. All `std_enqueueTime` values must fall in this range unless the scenario is specifically testing out-of-window behaviour.

**Default config values** (from `get_kafka_config.json`) that affect these scenarios:

| Setting | Value | Meaning |
|---|---|---|
| `MAX_LISTEN_DURATION_HOURS` | `0.05` (~3 min) | How long the retry loop runs before giving up |
| `RETRY_WAIT_SECONDS` | `5` | Seconds between retry attempts |
| `STABLE_COUNT_REQUIRED_ATTEMPTS` | `2` | Consecutive equal counts required before accepting |
| `METADATA_COUNT_TOLERANCE_PCT` | `10` | Acceptable under-count percentage (floor = expected − 10%) |

When a scenario says **"edit `get_kafka_config.json`"**, restore the original value immediately after the test.

---

### Setting up the metadata file

Every scenario requires a metadata file that is normally written by the status messages pipeline. To test without running that pipeline first, create it manually. Replace the last field (`3`) with whatever `total_messages_published` the scenario needs.

```bash
mkdir -p output/get_kafka/CPSB4Q00_2026-04-22
cat > output/get_kafka/CPSB4Q00_2026-04-22/CPSB4Q00_2026-04-22_metadata.txt << 'EOF'
export_datetime|username|business_date|mandator|producer_name|feed_name|reconciliation_group_id|instances_counted|total_expected_instances|total_messages_published
2026-04-22T23:59:00|testuser|2026-04-22|022|CLIENT_STRUCTURES|CPSB4Q00|1|2|2|3
EOF
```

This tells the business data pipeline to expect **3 records** from Kafka.

---

### Scenario A — Normal run (count met, stability confirmed)

**What it tests**: Full happy path. Metadata exists, records are in the topic within the time window, the filtered count reaches `total_messages_published`, and the count is stable for `STABLE_COUNT_REQUIRED_ATTEMPTS` (2) consecutive reads before the output is written.

The script never accepts on the first match — it always sleeps `RETRY_WAIT_SECONDS` and re-reads to confirm the count has not changed. With the default `STABLE_COUNT_REQUIRED_ATTEMPTS=2` you will always see at least 2 collection attempts in the log before the script exits.

**Step 1 — Create metadata file** (expecting 3 records):
```bash
mkdir -p output/get_kafka/CPSB4Q00_2026-04-22
cat > output/get_kafka/CPSB4Q00_2026-04-22/CPSB4Q00_2026-04-22_metadata.txt << 'EOF'
export_datetime|username|business_date|mandator|producer_name|feed_name|reconciliation_group_id|instances_counted|total_expected_instances|total_messages_published
2026-04-22T23:59:00|testuser|2026-04-22|022|CLIENT_STRUCTURES|CPSB4Q00|1|2|2|3
EOF
```

**Step 2 — Reset topic and produce 3 records**:
```bash
podman exec redpanda rpk topic delete business-topic
podman exec redpanda rpk topic create business-topic --partitions 1 --replicas 1
python3 produce_get_kafka_messages.py
```

`input/business_data.jsonl` must have exactly 3 records with `std_enqueueTime` inside `2026-04-22T16:00:00+00:00` → `2026-04-23T16:00:00+00:00`.

**Step 3 — Run**:
```bash
bash run_get_kafka_local.sh 2026-04-22 022
```

**Expected log sequence**:
```
Count changed: None → 3 (>= expected 3). Stability streak reset to 1/2.   ← attempt 1: streak not met, sleeps 5s
Count stable at 3 for 2/2 consecutive attempt(s).                         ← attempt 2: streak met, accepts
Temp file promoted: ...CPSB4QST.par.tmp → ...CPSB4QST.par
Exiting (code 0): data successfully consumed and written to ...CPSB4QST.par
```

**Expected output files**:
```
output/get_kafka/CPSB4QST_2026-04-22/CPSB4QST.par           ← 3 JSON lines
output/get_kafka/CPSB4QST_get_kafka_validation_log.txt       ← one SUCCESS row appended
```

---

### Scenario B — Metadata file missing

**What it tests**: `00_get_kafka.py` hard-exits immediately (before connecting to Kafka) if the metadata file does not exist. The business data pipeline cannot run until the status messages pipeline has completed successfully.

**Step 1 — Remove the metadata file if it exists**:
```bash
rm -f output/get_kafka/CPSB4Q00_2026-04-22/CPSB4Q00_2026-04-22_metadata.txt
```

**Step 2 — Run**:
```bash
bash run_get_kafka_local.sh 2026-04-22 022
```

**Expected**: Script exits 1 immediately. No Kafka connection is attempted. No output file.

**Expected log**:
```
METADATA FILE NOT FOUND: .../CPSB4Q00_2026-04-22_metadata.txt
The trigger-based status-messages script must complete successfully before this script runs.
Cannot continue without the metadata file.
```

---

### Scenario C — Metadata file malformed

**What it tests**: Three distinct structural errors the script detects and refuses before doing any work. Each causes an immediate exit 1 with a descriptive message.

Before each sub-case, reset the topic so Kafka state does not interfere:
```bash
podman exec redpanda rpk topic delete business-topic
podman exec redpanda rpk topic create business-topic --partitions 1 --replicas 1
```

---

**C1 — Header only (no data row)**

```bash
mkdir -p output/get_kafka/CPSB4Q00_2026-04-22
printf 'export_datetime|username|business_date|mandator|producer_name|feed_name|reconciliation_group_id|instances_counted|total_expected_instances|total_messages_published\n' \
  > output/get_kafka/CPSB4Q00_2026-04-22/CPSB4Q00_2026-04-22_metadata.txt
bash run_get_kafka_local.sh 2026-04-22 022
```

**Expected log**: `Metadata file ... has 1 non-blank line(s) — expected exactly 2 (header row + 1 data row). File may be incomplete or empty.`

---

**C2 — Multiple data rows** (can happen if the status messages script appended instead of replacing)

```bash
mkdir -p output/get_kafka/CPSB4Q00_2026-04-22
cat > output/get_kafka/CPSB4Q00_2026-04-22/CPSB4Q00_2026-04-22_metadata.txt << 'EOF'
export_datetime|username|business_date|mandator|producer_name|feed_name|reconciliation_group_id|instances_counted|total_expected_instances|total_messages_published
2026-04-22T23:59:00|testuser|2026-04-22|022|CLIENT_STRUCTURES|CPSB4Q00|1|2|2|3
2026-04-22T23:59:30|testuser|2026-04-22|022|CLIENT_STRUCTURES|CPSB4Q00|1|2|2|5
EOF
bash run_get_kafka_local.sh 2026-04-22 022
```

**Expected log**: `Metadata file ... has 3 non-blank lines — expected exactly 2 ... Multiple data rows are not permitted ... Remove the extra rows and rerun.`

---

**C3 — Column count mismatch** (header has 10 fields, data row has 9)

```bash
mkdir -p output/get_kafka/CPSB4Q00_2026-04-22
cat > output/get_kafka/CPSB4Q00_2026-04-22/CPSB4Q00_2026-04-22_metadata.txt << 'EOF'
export_datetime|username|business_date|mandator|producer_name|feed_name|reconciliation_group_id|instances_counted|total_expected_instances|total_messages_published
2026-04-22T23:59:00|testuser|2026-04-22|022|CLIENT_STRUCTURES|CPSB4Q00|1|2|2
EOF
bash run_get_kafka_local.sh 2026-04-22 022
```

**Expected log**: `Metadata file ... header count (10) does not match value count (9).`

---

### Scenario D — Under-count, messages arrive during retry → eventual success

**What it tests**: Script starts with fewer records than the metadata expects and enters the retry loop. The missing records are produced while it is waiting. The next attempt finds the full count and the script accepts.

**Step 1 — Create metadata file** (expecting 5 records):
```bash
mkdir -p output/get_kafka/CPSB4Q00_2026-04-22
cat > output/get_kafka/CPSB4Q00_2026-04-22/CPSB4Q00_2026-04-22_metadata.txt << 'EOF'
export_datetime|username|business_date|mandator|producer_name|feed_name|reconciliation_group_id|instances_counted|total_expected_instances|total_messages_published
2026-04-22T23:59:00|testuser|2026-04-22|022|CLIENT_STRUCTURES|CPSB4Q00|1|2|2|5
EOF
```

**Step 2 — Create `input/partial_3.jsonl`** (3 of the 5 expected records):
```json
{"std_enqueueTime": "2026-04-22T17:00:00+00:00", "std_legalEntity": "UBS_AG", "accountId": "ACC-001", "productType": "EQUITY", "quantity": 100.0, "currency": "USD", "tradeDate": "2026-04-22", "settlementDate": "2026-04-24", "mandatorCode": "022"}
{"std_enqueueTime": "2026-04-22T17:01:00+00:00", "std_legalEntity": "UBS_AG", "accountId": "ACC-002", "productType": "BOND", "quantity": 200.0, "currency": "EUR", "tradeDate": "2026-04-22", "settlementDate": "2026-04-24", "mandatorCode": "022"}
{"std_enqueueTime": "2026-04-22T17:02:00+00:00", "std_legalEntity": "UBS_AG", "accountId": "ACC-003", "productType": "FX", "quantity": 300.0, "currency": "CHF", "tradeDate": "2026-04-22", "settlementDate": "2026-04-24", "mandatorCode": "022"}
```

**Step 3 — Create `input/late_2.jsonl`** (the 2 records that arrive after the script starts):
```json
{"std_enqueueTime": "2026-04-22T17:10:00+00:00", "std_legalEntity": "UBS_AG", "accountId": "ACC-004", "productType": "EQUITY", "quantity": 400.0, "currency": "USD", "tradeDate": "2026-04-22", "settlementDate": "2026-04-24", "mandatorCode": "022"}
{"std_enqueueTime": "2026-04-22T17:11:00+00:00", "std_legalEntity": "UBS_AG", "accountId": "ACC-005", "productType": "BOND", "quantity": 500.0, "currency": "GBP", "tradeDate": "2026-04-22", "settlementDate": "2026-04-24", "mandatorCode": "022"}
```

**Step 4 — Reset topic and produce the partial set**:
```bash
podman exec redpanda rpk topic delete business-topic
podman exec redpanda rpk topic create business-topic --partitions 1 --replicas 1
python3 produce_get_kafka_messages.py --file input/partial_3.jsonl
```

**Step 5 — Two terminals**:
```bash
# Terminal 1 — start the script (finds 3 of 5, enters retry loop)
bash run_get_kafka_local.sh 2026-04-22 022
# Watch for: "Under-count on attempt 1: got 3, expected 5 ... Sleeping 5s."

# Terminal 2 — produce the 2 late records during the 5-second sleep window
python3 produce_get_kafka_messages.py --file input/late_2.jsonl
```

**Expected log sequence in Terminal 1**:
```
Under-count on attempt 1: got 3, expected 5 ... Sleeping 5s.              ← attempt 1: under
Count changed: None → 5 (>= expected 5). Stability streak reset to 1/2.  ← attempt 2: at target
Count stable at 5 for 2/2 consecutive attempt(s).                         ← attempt 3: stable, accepts
Exiting (code 0): data successfully consumed and written to ...CPSB4QST.par
```

---

### Scenario E — Count permanently below tolerance floor (hard failure)

**What it tests**: The topic has far fewer records than expected and none are ever added. The script exhausts its retry window and exits 1. No output file is written. A FAILED row is appended to the validation log.

With `METADATA_COUNT_TOLERANCE_PCT=10` and `total_messages_published=10`, the acceptable floor is **9** (10 − 10%). Producing 5 records puts the count well below that floor.

**Step 1 — Shorten the timeout** in `get_kafka_config.json` (restore after testing):
```json
"MAX_LISTEN_DURATION_HOURS": "0.01",
"RETRY_WAIT_SECONDS": "5"
```

**Step 2 — Create metadata file** (expecting 10 records):
```bash
mkdir -p output/get_kafka/CPSB4Q00_2026-04-22
cat > output/get_kafka/CPSB4Q00_2026-04-22/CPSB4Q00_2026-04-22_metadata.txt << 'EOF'
export_datetime|username|business_date|mandator|producer_name|feed_name|reconciliation_group_id|instances_counted|total_expected_instances|total_messages_published
2026-04-22T23:59:00|testuser|2026-04-22|022|CLIENT_STRUCTURES|CPSB4Q00|1|2|2|10
EOF
```

**Step 3 — Create `input/five_records.jsonl`** with 5 records (all with `std_enqueueTime` inside the window), then reset and produce:
```bash
podman exec redpanda rpk topic delete business-topic
podman exec redpanda rpk topic create business-topic --partitions 1 --replicas 1
python3 produce_get_kafka_messages.py --file input/five_records.jsonl
```

**Step 4 — Run (do not produce more records)**:
```bash
bash run_get_kafka_local.sh 2026-04-22 022
```

**Expected log sequence**:
```
COUNT VALIDATION REPORT
  Expected count (metadata): 10
  Actual filtered count    : 5
  Result: BELOW TOLERANCE — got 5 of 10 expected (5 short, 50.0% below). Floor is 9. Will retry.
...
[repeats every 5 seconds until MAX_LISTEN_DURATION_HOURS expires]
...
Retry window exhausted after N attempt(s). Final count 5 is BELOW lower tolerance threshold 9 ...  Failing.
FAILURE ANALYSIS — FILTER / COUNT MISMATCH DETAIL
```

**Expected outcome**: Exit code 1. No `CPSB4QST.par`. One FAILED row appended to `output/get_kafka/CPSB4QST_get_kafka_validation_log.txt` with `failure_reason=COUNT_BELOW_TOLERANCE`.

**Restore config**:
```json
"MAX_LISTEN_DURATION_HOURS": "0.05",
"RETRY_WAIT_SECONDS": "5"
```

---

### Scenario F — Count within lower tolerance at exhaustion (accepted with warning)

**What it tests**: The topic has slightly fewer records than expected — within the ±10% tolerance floor. The script retries until the deadline, then accepts the under-count with a WARNING-level log and writes the output file anyway.

With `total_messages_published=10` and `METADATA_COUNT_TOLERANCE_PCT=10`, the floor is **9**. Producing 9 records means count (9) < expected (10) so the script keeps retrying. At exhaustion: `9 >= floor(9)` → accepts.

**Step 1 — Shorten the timeout** in `get_kafka_config.json` (restore after testing):
```json
"MAX_LISTEN_DURATION_HOURS": "0.01",
"RETRY_WAIT_SECONDS": "5"
```

**Step 2 — Create metadata file** (expecting 10 records):
```bash
mkdir -p output/get_kafka/CPSB4Q00_2026-04-22
cat > output/get_kafka/CPSB4Q00_2026-04-22/CPSB4Q00_2026-04-22_metadata.txt << 'EOF'
export_datetime|username|business_date|mandator|producer_name|feed_name|reconciliation_group_id|instances_counted|total_expected_instances|total_messages_published
2026-04-22T23:59:00|testuser|2026-04-22|022|CLIENT_STRUCTURES|CPSB4Q00|1|2|2|10
EOF
```

**Step 3 — Create `input/nine_records.jsonl`** with exactly 9 records inside the window, then reset and produce:
```bash
podman exec redpanda rpk topic delete business-topic
podman exec redpanda rpk topic create business-topic --partitions 1 --replicas 1
python3 produce_get_kafka_messages.py --file input/nine_records.jsonl
```

**Step 4 — Run (do not produce the 10th record)**:
```bash
bash run_get_kafka_local.sh 2026-04-22 022
```

**Expected log sequence**:
```
Result: BELOW EXPECTED — got 9 of 10 expected (1 short, 10.0% below). Within ±10% tolerance. Will retry.
...
[retries until deadline]
...
Retry window exhausted after N attempt(s). Final count 9 is within lower tolerance (9–10, ±10%). Accepting.
Temp file promoted: ...CPSB4QST.par.tmp → ...CPSB4QST.par
Exiting (code 0): data successfully consumed and written to ...CPSB4QST.par
```

**Expected outcome**: Exit code 0. `CPSB4QST.par` written with 9 records. One SUCCESS row in the validation log with `actual_count=9`, `expected_count=10`.

**Restore config**:
```json
"MAX_LISTEN_DURATION_HOURS": "0.05",
"RETRY_WAIT_SECONDS": "5"
```

---

### Scenario G — Stability confirmation (STABLE_COUNT_REQUIRED_ATTEMPTS)

**What it tests**: When the count first meets or exceeds the expected value, the script does not accept immediately — it requires `STABLE_COUNT_REQUIRED_ATTEMPTS` consecutive reads with the same count. Increase that setting to 3 to see the streak working through the logs.

**Step 1 — Temporarily increase stability requirement** in `get_kafka_config.json`:
```json
"STABLE_COUNT_REQUIRED_ATTEMPTS": "3"
```

**Step 2 — Create metadata file** (expecting 3 records):
```bash
mkdir -p output/get_kafka/CPSB4Q00_2026-04-22
cat > output/get_kafka/CPSB4Q00_2026-04-22/CPSB4Q00_2026-04-22_metadata.txt << 'EOF'
export_datetime|username|business_date|mandator|producer_name|feed_name|reconciliation_group_id|instances_counted|total_expected_instances|total_messages_published
2026-04-22T23:59:00|testuser|2026-04-22|022|CLIENT_STRUCTURES|CPSB4Q00|1|2|2|3
EOF
```

**Step 3 — Reset topic and produce 3 records**:
```bash
podman exec redpanda rpk topic delete business-topic
podman exec redpanda rpk topic create business-topic --partitions 1 --replicas 1
python3 produce_get_kafka_messages.py
```

**Step 4 — Run**:
```bash
bash run_get_kafka_local.sh 2026-04-22 022
```

**Expected log sequence** — three attempts before accepting:
```
Count changed: None → 3 (>= expected 3). Stability streak reset to 1/3.  ← attempt 1: streak 1, sleeps
Count stable at 3 for 2/3 consecutive attempt(s).                         ← attempt 2: streak 2, sleeps
Count stable at 3 for 3/3 consecutive attempt(s).                         ← attempt 3: streak met, accepts
Temp file promoted: ...CPSB4QST.par.tmp → ...CPSB4QST.par
Exiting (code 0): data successfully consumed and written to ...CPSB4QST.par
```

**Restore config**:
```json
"STABLE_COUNT_REQUIRED_ATTEMPTS": "2"
```

---

### Scenario H — Records outside the time window

**What it tests**: Records are in the topic but have `std_enqueueTime` outside `2026-04-22 16:00:00 → 2026-04-23 16:00:00`. The script finds 0 matching records, retries until the deadline, and exits 1 (count 0 is below the tolerance floor for any non-zero expected count).

**Step 1 — Create metadata file** (expecting 3 records):
```bash
mkdir -p output/get_kafka/CPSB4Q00_2026-04-22
cat > output/get_kafka/CPSB4Q00_2026-04-22/CPSB4Q00_2026-04-22_metadata.txt << 'EOF'
export_datetime|username|business_date|mandator|producer_name|feed_name|reconciliation_group_id|instances_counted|total_expected_instances|total_messages_published
2026-04-22T23:59:00|testuser|2026-04-22|022|CLIENT_STRUCTURES|CPSB4Q00|1|2|2|3
EOF
```

**Step 2 — Create `input/outside_window.jsonl`** with `std_enqueueTime` **before** `2026-04-22T16:00:00+00:00`:
```json
{"std_enqueueTime": "2026-04-22T10:00:00+00:00", "std_legalEntity": "UBS_AG", "accountId": "ACC-001", "productType": "EQUITY", "quantity": 100.0, "currency": "USD", "tradeDate": "2026-04-22", "settlementDate": "2026-04-24", "mandatorCode": "022"}
```

**Step 3 — Reset topic and produce**:
```bash
podman exec redpanda rpk topic delete business-topic
podman exec redpanda rpk topic create business-topic --partitions 1 --replicas 1
python3 produce_get_kafka_messages.py --file input/outside_window.jsonl
```

Confirm the message is in the topic but the script cannot reach it:
```bash
podman exec redpanda rpk topic describe business-topic -p
# HIGH-WATERMARK = 1 — message is there
```

**Step 4 — Run**:
```bash
bash run_get_kafka_local.sh 2026-04-22 022
```

**Expected**: `No data on the Kafka Topic for partition 0` on every attempt → retry loop exhausts → `BELOW lower tolerance threshold` → exit 1. No `CPSB4QST.par`.

---

### Scenario I — Empty topic

**What it tests**: Script runs against a topic with zero messages. No records are found on any attempt. Retry loop exhausts and script exits 1.

**Step 1 — Create metadata file** (expecting 3 records):
```bash
mkdir -p output/get_kafka/CPSB4Q00_2026-04-22
cat > output/get_kafka/CPSB4Q00_2026-04-22/CPSB4Q00_2026-04-22_metadata.txt << 'EOF'
export_datetime|username|business_date|mandator|producer_name|feed_name|reconciliation_group_id|instances_counted|total_expected_instances|total_messages_published
2026-04-22T23:59:00|testuser|2026-04-22|022|CLIENT_STRUCTURES|CPSB4Q00|1|2|2|3
EOF
```

**Step 2 — Ensure topic is empty**:
```bash
podman exec redpanda rpk topic delete business-topic
podman exec redpanda rpk topic create business-topic --partitions 1 --replicas 1
```

**Step 3 — Run (do not produce any messages)**:
```bash
bash run_get_kafka_local.sh 2026-04-22 022
```

**Expected**: `No data on the Kafka Topic for partition 0` on every attempt → retry loop exhausts after `MAX_LISTEN_DURATION_HOURS` → exit 1. No output file.

---

### Scenario J — Re-run for same ASOF_DT (restart merge)

**What it tests**: Running the script a second time for the same date when an output file already exists. The existing `.par` file is backed up as `.par.1`, the second run collects fresh data from Kafka, and at the end both files are merged into the final `.par`. The `.par.1` backup is deleted. **Output grows with each re-run** — the same Kafka records are re-read every time because the script always seeks to `start_offset`.

**Step 1 — Run once to create the initial output**:

Create metadata (expecting 2 records):
```bash
mkdir -p output/get_kafka/CPSB4Q00_2026-04-22
cat > output/get_kafka/CPSB4Q00_2026-04-22/CPSB4Q00_2026-04-22_metadata.txt << 'EOF'
export_datetime|username|business_date|mandator|producer_name|feed_name|reconciliation_group_id|instances_counted|total_expected_instances|total_messages_published
2026-04-22T23:59:00|testuser|2026-04-22|022|CLIENT_STRUCTURES|CPSB4Q00|1|2|2|2
EOF
```

Produce 2 records and run:
```bash
podman exec redpanda rpk topic delete business-topic
podman exec redpanda rpk topic create business-topic --partitions 1 --replicas 1
python3 produce_get_kafka_messages.py
bash run_get_kafka_local.sh 2026-04-22 022
wc -l output/get_kafka/CPSB4QST_2026-04-22/CPSB4QST.par
# → 2
```

**Step 2 — Run a second time without wiping anything**:
```bash
bash run_get_kafka_local.sh 2026-04-22 022
```

**Expected log on second run**:
```
First run for this date detected, data in ...CPSB4QST.par.1   ← existing .par backed up
...
[reads same 2 records from Kafka — seek always goes to start_offset]
...
Temp file promoted: ...CPSB4QST.par.tmp → ...CPSB4QST.par
[restart merge] cat .par.1 + .par → .par_ASI → mv to .par → rm .par.1
Exiting (code 0): ...
```

**Check the merged result**:
```bash
wc -l output/get_kafka/CPSB4QST_2026-04-22/CPSB4QST.par
# → 4  (2 from run 1 + 2 from run 2 — same records, duplicated)

ls output/get_kafka/CPSB4QST_2026-04-22/
# CPSB4QST.par only — .par.1 was deleted after the merge
```

**To avoid accumulation**, remove the output folder before a clean re-run:
```bash
rm -rf output/get_kafka/CPSB4QST_2026-04-22/
```

---

## 13. Command Index

Quick reference for all commands. Use this for diagnosis — no need to scroll the full document.

### Container management

| Command | What it does |
|---|---|
| `podman ps -a \| grep redpanda` | Check container state: `Up` = running, `Exited` = stopped, nothing = destroyed |
| `podman start redpanda` | Restart a stopped container (fast, preserves topics and schemas) |
| `bash start_kafka.sh` | Create and start a new container with both topics (use only when container is gone) |
| `bash stop_kafka.sh` | Stop and destroy the container (loses all schemas) |

### Broker health

| Command | What it does |
|---|---|
| `podman exec redpanda rpk cluster info` | Verify broker is ready and healthy |
| `podman exec redpanda rpk topic list` | List all topics currently in the broker |

### Topic inspection

| Command | What it does |
|---|---|
| `podman exec redpanda rpk topic describe <topic> -p` | Show partition details including `HIGH-WATERMARK` (= total message count) |
| `podman exec redpanda rpk topic consume <topic> --num 10` | Read the last 10 messages (raw bytes for Avro-encoded topics) |
| `podman exec redpanda rpk topic consume <topic> --offset start` | Read all messages from the beginning |

> `rpk topic consume` shows raw Avro bytes — use the snippets below to read actual field values.

### Inspect message content (readable format)

**Status messages — `inflow-topic`**

Prints one line per message showing the key fields:

```bash
python3 -c "
import io, json, requests, fastavro
from kafka import KafkaConsumer

schema = fastavro.parse_schema(json.loads(
    requests.get('http://localhost:8081/subjects/inflow-topic-value/versions/latest').json()['schema']
))
consumer = KafkaConsumer('inflow-topic', bootstrap_servers='localhost:9092',
    auto_offset_reset='earliest', consumer_timeout_ms=3000)
count = 0
for m in consumer:
    rec = fastavro.schemaless_reader(io.BytesIO(m.value[5:]), schema)
    s = rec.get('status', {})
    print(f\"[{count}] mandator={s.get('mandatorCode')}  date={s.get('businessDate')}  \"
          f\"runId={s.get('reconciliationGroupId')}  instance={s.get('instanceIndex')}/{s.get('totalInstances')}  \"
          f\"producer={rec.get('producer')}  msgs={s.get('numberOfMessagesPublished')}\")
    count += 1
consumer.close()
print(f'--- {count} message(s) in inflow-topic ---')
"
```

Example output:
```
[0] mandator=022  date=2026-05-25  runId=1  instance=0/2  producer=CLIENT_STRUCTURES  msgs=500
[1] mandator=022  date=2026-05-25  runId=1  instance=1/2  producer=CLIENT_STRUCTURES  msgs=600
--- 2 message(s) in inflow-topic ---
```

**Business data — `business-topic`**

Prints one line per record showing the key fields:

```bash
python3 -c "
import io, json, requests, fastavro
from kafka import KafkaConsumer

schema = fastavro.parse_schema(json.loads(
    requests.get('http://localhost:8081/subjects/business-topic-value/versions/latest').json()['schema']
))
consumer = KafkaConsumer('business-topic', bootstrap_servers='localhost:9092',
    auto_offset_reset='earliest', consumer_timeout_ms=3000)
count = 0
for m in consumer:
    rec = fastavro.schemaless_reader(io.BytesIO(m.value[5:]), schema)
    print(f\"[{count}] mandator={rec.get('mandatorCode')}  tradeDate={rec.get('tradeDate')}  \"
          f\"account={rec.get('accountId')}  product={rec.get('productType')}  \"
          f\"qty={rec.get('quantity')}  ccy={rec.get('currency')}\")
    count += 1
consumer.close()
print(f'--- {count} record(s) in business-topic ---')
"
```

Example output:
```
[0] mandator=022  tradeDate=2026-04-22  account=ACC-001  product=EQUITY  qty=1000.0  ccy=USD
[1] mandator=022  tradeDate=2026-04-22  account=ACC-002  product=BOND    qty=500.0   ccy=EUR
--- 2 record(s) in business-topic ---
```

### Topic management

| Command | What it does |
|---|---|
| `podman exec redpanda rpk topic delete <topic>` | Delete topic and all its messages |
| `podman exec redpanda rpk topic create <topic> --partitions 1 --replicas 1` | Create an empty topic |

### Schema registry

| Command | What it does |
|---|---|
| `curl http://localhost:8081/subjects` | List all registered schema subjects |
| `curl http://localhost:8081/subjects/<subject>/versions/latest \| python3 -m json.tool` | View current schema for a subject (pretty-printed) |
| `curl -X DELETE http://localhost:8081/subjects/<subject>` | Delete a schema subject (do before re-registering) |
| `python3 register_schema.py` | Register status messages schema (`inflow-topic-value`) |
| `python3 register_get_kafka_schema.py` | Register business data schema (`business-topic-value`) |

Subject names used in this environment:
- `inflow-topic-value` — status messages schema
- `business-topic-value` — business data schema

### Producing messages

| Command | What it does |
|---|---|
| `python3 produce_messages.py` | Produce status messages from `input/status_messages_data.jsonl` |
| `python3 produce_messages.py --file input/custom.jsonl` | Produce from a custom JSONL file |
| `python3 produce_get_kafka_messages.py` | Produce business records from `input/business_data.jsonl` |
| `python3 produce_get_kafka_messages.py --file input/custom.jsonl` | Produce business records from a custom file |

### Running pipelines

| Command | What it does |
|---|---|
| `bash run_local.sh DATE [MANDATOR]` | Run the status messages pipeline |
| `bash run_get_kafka_local.sh DATE [MANDATOR]` | Run the business data pipeline |
| `python3 show_window.py DATE MANDATOR` | Print exact time window for a given date + mandator |

### Reset

| Command | What it does |
|---|---|
| `bash reset_kafka.sh` | Wipe both topics and all schemas (broker keeps running) |
| `bash reset_kafka.sh --output` | Same as above, also clears `output/` directory |

---

## 14. Troubleshooting

| Symptom | Cause | Fix |
|---|---|---|
| `podman pull` fails with EOF | Not using internal registry | Use `container-registry.ubs.net/base-images/redpanda:latest-23.2-alpine-20231028` |
| `bash start_kafka.sh` fails — container name already in use | Container already exists (stopped) | Run `podman start redpanda` instead |
| Port mapping warnings in `start_kafka.sh` output | Expected with `--network=host` — ports bind directly | Ignore; ports work fine |
| `register_schema.py` fails — file not found | Schema file missing | Place schema at `input/status_messages_schema.json` |
| `register_get_kafka_schema.py` fails — file not found | Schema file missing | Place schema at `input/business_data_schema.json` |
| Script exits with no data | `ASOF_DT` does not match `businessDate` in JSONL | Confirm date matches. Run `show_window.py` to verify window |
| `Topic validation failed — does not contain mandator 022` | Config symlink missing — running script directly | Always use `bash run_get_kafka_local.sh`; if symlink is gone: `ln -s get_kafka_config.json 1001_CPSB4QST_config.json` |
| `VALIDATION FAILED: Duplicate instanceIds found` | Topic not wiped between produce runs | Delete + recreate topic, produce once only |
| `No module named 'dsf_logging'` | DSF stub missing | Ensure `dsf_logging.py` stub is in the directory |
| `No module named 'assertf'` | `assertf.py` stub missing | `assertf.py` is already in this folder; check Python path |
| `Cannot check mandator — 'status.mandatorCode' column missing` | Message structure issue during topic match report | Check messages in topic with `rpk topic consume inflow-topic --offset start` |
| Script hangs for a long time then exits with no output | Messages are in topic but `eventTimestamp` is outside window | Run `show_window.py` and check all JSONL timestamps are inside the range |
| `Schema not found` error from script | Schema not registered or registry was wiped | Run `python3 register_schema.py` (and `register_get_kafka_schema.py` for business data) |
