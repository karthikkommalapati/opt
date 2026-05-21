# Local Simulation Runbook

## Prerequisites (one-time setup)

### 1. Redpanda image
Already pulled from internal registry:
```bash
podman pull container-registry.ubs.net/base-images/redpanda:latest-23.2-alpine-20231028
```

### 2. Input files — place in `input/`
| File | What to put here |
|---|---|
| `input/status_messages_schema.json` | Your Avro schema (raw schema or full Confluent API response — both handled) |
| `input/status_messages_data.jsonl` | Your test messages — one JSON record per line |

See `input/README.md` for file format details.

### 3. Main scripts
Copy from `trigger_based_new/` and apply SSL-removal changes per `CHANGES.md`:
- `kafka_trigger_status_messages.py`
- `00_get_kafka.py`

---

## Run Sequence (each test session)

```bash
# Step 1 — Start broker (Redpanda + Schema Registry + topic creation)
bash start_kafka.sh

# Step 2 — Register your Avro schema with the local registry
python3 register_schema.py

# Step 3 — Produce test messages from your JSONL file into the topic
python3 produce_messages.py

# Step 4 — Run the pipeline (replace date with businessDate from your JSONL data)
bash run_local.sh 2026-05-20
```

## Check output

```
output/logs/    ← log files
output/data/    ← processed data files
```

---

## Inspecting the Kafka Topic

All commands use `rpk` via `podman exec` — no extra tools needed.

**How many messages are in the topic:**
```bash
podman exec redpanda rpk topic describe inflow-topic -p
```
Look at the `HIGH-WATERMARK` column — that's the total message count per partition.

**Read the last 10 messages:**
```bash
podman exec redpanda rpk topic consume inflow-topic --num 10
```

**Read ALL messages from the beginning:**
```bash
podman exec redpanda rpk topic consume inflow-topic --offset start
```

**List all topics:**
```bash
podman exec redpanda rpk topic list
```

**Wipe the topic and start fresh (before re-producing):**
```bash
podman exec redpanda rpk topic delete inflow-topic
podman exec redpanda rpk topic create inflow-topic --partitions 1 --replicas 1
```

---

## Schema Registry

The schema registry and Kafka topics are **completely independent**. Deleting a topic removes its messages — the registered schema stays untouched. You almost never need to touch the schema registry unless the schema itself changes.

### When you DO need to re-register the schema

| Situation | Action needed |
|---|---|
| First-time setup | Register once with `register_schema.py` |
| Wipe and re-produce topic | Nothing — schema stays in registry |
| DevPod reconnect (container restarted) | Nothing — schema stays in registry |
| `bash stop_kafka.sh` then `bash start_kafka.sh` | Re-register — registry is wiped when container is destroyed |
| Schema file changed (`input/status_messages_schema.json`) | Delete old subject, re-register |
| Schema version mismatch error from script | Delete old subject, re-register |

### View registered schemas

```bash
# list all registered subjects
curl http://localhost:8081/subjects

# view the current schema for a subject
curl http://localhost:8081/subjects/inflow-topic-value/versions/latest | python3 -m json.tool
curl http://localhost:8081/subjects/business-topic-value/versions/latest | python3 -m json.tool
```

### Delete a schema subject (before re-registering)

Only do this when you have changed the schema file and need to register the new version.

```bash
# delete status messages schema
curl -X DELETE http://localhost:8081/subjects/inflow-topic-value

# delete business data schema (get_kafka)
curl -X DELETE http://localhost:8081/subjects/business-topic-value
```

Then re-register:

```bash
# re-register status messages schema
python3 register_schema.py

# re-register business data schema
python3 register_get_kafka_schema.py
```

### What happens when the container is destroyed

`bash stop_kafka.sh` stops and removes the Redpanda container. When you run `bash start_kafka.sh` again, the registry starts empty — all schemas are gone. You must re-register both schemas before producing:

```bash
bash start_kafka.sh
python3 register_schema.py           # status messages schema
python3 register_get_kafka_schema.py # business data schema
```

---

## Re-run without restarting broker

If Redpanda is already running, skip steps 1–2 and repeat from step 3:

```bash
python3 produce_messages.py
bash run_local.sh 2026-05-20
```

---

## After DevPod Reconnect

The Redpanda container stops when the DevPod disconnects. Do NOT run `start_kafka.sh` again — it will fail because a container named `redpanda` already exists.

Instead, restart the existing stopped container:

```bash
# Check current state (look for Exited or Running)
podman ps -a | grep redpanda

# Restart the stopped container (fast — no image pull, no topic re-creation)
podman start redpanda

# Verify it's ready
podman exec redpanda rpk cluster info
```

If `rpk cluster info` succeeds, the broker is up and you can continue from Step 3 (produce messages).

If the container is gone entirely (e.g. DevPod was rebuilt), run `bash start_kafka.sh` from scratch.

---

## Running with a specific mandator

`run_local.sh` accepts mandator as an optional second argument. If omitted it falls back to `022` with a warning:

```bash
# Default — shows WARNING: No mandator provided — falling back to default '022'
bash run_local.sh 2026-04-22

# Explicit mandator
bash run_local.sh 2026-04-22 023
```

---

## Testing real scenarios

Before running any scenario, always check the time window for your chosen `ASOF_DT` and mandator:

```bash
python3 show_window.py 2026-04-22 022
```

This prints the exact START and END timestamps the script will use. All `eventTimestamp` values in your JSONL data must fall inside this range unless the scenario specifically tests outside-window behaviour (Scenario F).

---

### Scenario A — Late publication (messages arrive after script starts)

Simulates messages being published to Kafka after the script has already started consuming.
The script enters retry mode (waiting for missing instances) and picks up the late messages on the next poll.

**Critical rule**: ALL messages — both the initial partial set AND the late ones — must have `eventTimestamp` within the time window. If the late messages have an `eventTimestamp` outside the window, the retry loop will NOT pick them up regardless of `MAX_LISTEN_DURATION_HOURS`.

**Step 1 — Find your window first**:
```bash
python3 show_window.py 2026-04-22 022
```

Note the START and END timestamps. All `eventTimestamp` values in both JSONL files must fall between these two values.

**Step 2 — Create `input/part1.jsonl`** (instances 0–2, timestamps inside window):
```json
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 0, "totalInstances": 4, "numberOfMessagesPublished": 100}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:10+00:00"}
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 1, "totalInstances": 4, "numberOfMessagesPublished": 200}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:20+00:00"}
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 2, "totalInstances": 4, "numberOfMessagesPublished": 150}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:30+00:00"}
```

**Step 3 — Create `input/part2.jsonl`** (instance 3 — the late one, timestamp still inside window):
```json
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 3, "totalInstances": 4, "numberOfMessagesPublished": 175}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:50+00:00"}
```

Note: `23:59:50` is still inside the `23:59:00 → 00:00:00` window. The message arrives LATE in real time (produced after the script starts) but its `eventTimestamp` is within the window — this is why the retry loop picks it up.

**Step 4 — Run** (requires two terminals):
```bash
# Terminal 1 — produce partial set, then start script
python3 produce_messages.py --file input/part1.jsonl
bash run_local.sh 2026-04-22 022
# Script finds instances 0,1,2 — fails validation (missing instance 3) — enters retry loop
```

```bash
# Terminal 2 — while script is in retry loop, publish the late message
python3 produce_messages.py --file input/part2.jsonl
# Script in Terminal 1 picks it up on next poll (within RETRY_WAIT_SECONDS=5)
```

**Expected**: Script logs `Missing instances: [3]` → retries → finds instance 3 → validation passes → writes output.

---

### Scenario B — Mixed mandator messages

Simulates a topic containing messages from multiple mandators. Script must only process messages matching `DSF_MANDATOR` and ignore all others.

**Step 1 — Find your window**:
```bash
python3 show_window.py 2026-04-22 022
```

All `eventTimestamp` values below must fall within the printed START → END range.

**Step 2 — Create `input/scenario_b.jsonl`** — two complete sets, mandators 022 and 023 interleaved:
```json
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 0, "totalInstances": 2, "numberOfMessagesPublished": 100}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:05+00:00"}
{"status": {"mandatorCode": "023", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 0, "totalInstances": 2, "numberOfMessagesPublished": 300}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:10+00:00"}
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 1, "totalInstances": 2, "numberOfMessagesPublished": 200}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:15+00:00"}
{"status": {"mandatorCode": "023", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 1, "totalInstances": 2, "numberOfMessagesPublished": 400}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:20+00:00"}
```

4 messages total — 2 for mandator 022 (counts 100, 200), 2 for mandator 023 (counts 300, 400).

**Step 3 — Wipe topic and produce**:
```bash
podman exec redpanda rpk topic delete inflow-topic
podman exec redpanda rpk topic create inflow-topic --partitions 1 --replicas 1
python3 produce_messages.py --file input/scenario_b.jsonl
```

Verify 4 messages in topic:
```bash
podman exec redpanda rpk topic describe inflow-topic -p
```

**Step 4 — Run targeting mandator 022**:
```bash
bash run_local.sh 2026-04-22 022
```

**Expected**:
- Script filters to mandatorCode=022 only → finds instances 0 and 1 (counts 100, 200)
- Mandator 023 messages are in the topic but completely ignored
- Output file contains only 022 data

**Step 5 — Verify the other side** (optional): re-run targeting 023:
```bash
bash run_local.sh 2026-04-22 023
```

This time only the 023 messages (counts 300, 400) are processed.

---

### Scenario C — Publishing while script is running

Simulates starting the script against an empty topic, then producing messages while it is actively listening. Tests that the script detects new messages during its retry cycle without needing a restart.

**Step 1 — Find your window**:
```bash
python3 show_window.py 2026-04-22 022
```

**Step 2 — Wipe topic so it starts empty**:
```bash
podman exec redpanda rpk topic delete inflow-topic
podman exec redpanda rpk topic create inflow-topic --partitions 1 --replicas 1
```

**Step 3 — Create `input/scenario_c.jsonl`** — complete set with timestamps inside window:
```json
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 0, "totalInstances": 3, "numberOfMessagesPublished": 100}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:10+00:00"}
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 1, "totalInstances": 3, "numberOfMessagesPublished": 200}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:20+00:00"}
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 2, "totalInstances": 3, "numberOfMessagesPublished": 150}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:30+00:00"}
```

**Step 4 — Run** (requires two terminals):
```bash
# Terminal 1 — start script against empty topic
# ALLOW_NO_DATA=YES means it enters retry loop immediately and keeps polling
bash run_local.sh 2026-04-22 022
# Script finds 0 messages → enters retry loop → polls every RETRY_WAIT_SECONDS
```

```bash
# Terminal 2 — produce all messages while script is in retry loop
python3 produce_messages.py --file input/scenario_c.jsonl
# Script in Terminal 1 detects new messages on next poll and processes them
```

**Key timing**: produce the messages BEFORE `MAX_LISTEN_DURATION_HOURS` expires. With the default `0.05` hours (~3 min) and `RETRY_WAIT_SECONDS=5`, you have ~3 minutes from script start to produce.

**Expected**:
- Terminal 1 logs: `0 messages found` → retries → `Found 3 messages` → validation passes → writes output
- `output/data/` — output file written with all 3 instances

---

### Scenario D — Multiple runIds (max runId selection test)

**What it tests**: Topic has messages from two different `reconciliationGroupId` values. Script must select max runId only — core business logic.

**Step 1 — Find your window**:
```bash
python3 show_window.py 2026-04-22 022
```

**Step 2 — Create `input/scenario_d.jsonl`**: two complete sets, same mandator/date, different runIds:
```json
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 0, "totalInstances": 2, "numberOfMessagesPublished": 100}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:05+00:00"}
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 1, "totalInstances": 2, "numberOfMessagesPublished": 200}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:10+00:00"}
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 2, "instanceIndex": 0, "totalInstances": 2, "numberOfMessagesPublished": 150}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:15+00:00"}
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 2, "instanceIndex": 1, "totalInstances": 2, "numberOfMessagesPublished": 250}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:20+00:00"}
```

**Step 3 — Wipe topic and produce**:
```bash
podman exec redpanda rpk topic delete inflow-topic
podman exec redpanda rpk topic create inflow-topic --partitions 1 --replicas 1
python3 produce_messages.py --file input/scenario_d.jsonl
```

**Step 4 — Run**:
```bash
bash run_local.sh 2026-04-22 022
```

**Expected**: Script logs `max reconciliationGroupId = 2` and outputs only the runId=2 messages (instances 0,1 with counts 150, 250).

---

### Scenario E — Timeout with incomplete data

**What it tests**: Script enters retry loop, `MAX_LISTEN_DURATION_HOURS` expires before missing instances arrive. Script must exit cleanly.

**Step 1 — Find your window**:
```bash
python3 show_window.py 2026-04-22 022
```

**Step 2 — Create `input/scenario_e.jsonl`**: only instances 0 and 1 of a 3-instance set (instance 2 never arrives):
```json
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 0, "totalInstances": 3, "numberOfMessagesPublished": 100}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:05+00:00"}
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 1, "totalInstances": 3, "numberOfMessagesPublished": 200}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:10+00:00"}
```

**Step 3 — Set a short timeout** in `status_messages_config.json` before running (restore after):
```json
"MAX_LISTEN_DURATION_HOURS": "0.02",
"RETRY_WAIT_SECONDS": "5"
```

**Step 4 — Wipe topic and produce**:
```bash
podman exec redpanda rpk topic delete inflow-topic
podman exec redpanda rpk topic create inflow-topic --partitions 1 --replicas 1
python3 produce_messages.py --file input/scenario_e.jsonl
```

**Step 5 — Run**:
```bash
bash run_local.sh 2026-04-22 022
```

**Expected**: Script retries every 5s, logs `Missing instances: [2]`, exits after ~1 min with `ALLOW_NO_DATA` result. Do NOT produce instance 2 — let it time out.

---

### Scenario F — Messages outside time window

**What it tests**: Topic contains messages with `eventTimestamp` outside the configured window. Script must ignore them — the time-window filter is the first gate before any business logic runs.

**How the window works**:

The script computes a start and end Kafka offset from `LOCATION_TIME_WINDOW` in the config. Only messages whose Kafka timestamp (set from `eventTimestamp` by `produce_messages.py`) falls inside that range are visible to the script. Messages outside it are in the topic but never read.

**Step 1 — Find your exact window**:

Always run this first — the window changes with every `ASOF_DT`:
```bash
python3 show_window.py 2026-04-22 022
```

Example output for `2026-04-22`:
```
Window START : 2026-04-22T23:59:00+00:00
Window END   : 2026-04-23T00:00:00+00:00

Timestamps INSIDE window  → script will process these:
  e.g.  "2026-04-22T23:59:30+00:00"

Timestamps OUTSIDE window → script will IGNORE these:
  before window:  "2026-04-22T17:59:00+00:00"
  after  window:  "2026-04-23T06:00:00+00:00"
```

The window is exactly 1 minute (23:59 → 00:00 crossing midnight). Anything before 23:59 or after 00:00 is outside.

**Step 2 — Create `input/scenario_f.jsonl` using an outside-window timestamp**:

Use the `before window` value from `show_window.py` output as `eventTimestamp`:
```json
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 0, "totalInstances": 1, "numberOfMessagesPublished": 100}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T17:59:00+00:00"}
```

`17:59:00` is 6 hours before the `23:59` window start — the script will never see it.

**Step 3 — Wipe topic and produce the outside-window message**:
```bash
podman exec redpanda rpk topic delete inflow-topic
podman exec redpanda rpk topic create inflow-topic --partitions 1 --replicas 1
python3 produce_messages.py --file input/scenario_f.jsonl
```

**Step 4 — Confirm the message IS in the topic**:
```bash
podman exec redpanda rpk topic describe inflow-topic -p
```

`HIGH-WATERMARK` must show `1` — the message is there, the script just won't find it.

**Step 5 — Run the script**:
```bash
bash run_local.sh 2026-04-22 022
```

**Expected**:
- Script finds 0 messages in the 23:59–00:00 window
- Enters retry loop, waits `RETRY_WAIT_SECONDS` between attempts
- Exits after `MAX_LISTEN_DURATION_HOURS` with `ALLOW_NO_DATA` result
- `output/data/` — no output file written

**Step 6 — Optional: confirm filter isolation**

Now produce an inside-window message and re-run — the script must process only the inside-window message and still ignore the outside one:
```bash
python3 produce_messages.py --file input/status_messages_data.jsonl
bash run_local.sh 2026-04-22 022
```

Both messages are in the topic but only the `23:59` one appears in output.

---

### Scenario G — Re-run behaviour and USR_VAL=1 backup mechanism

**What it tests**: what the script does when run twice for the same `ASOF_DT`, and how the `{SDA}_USR_VAL=1` production re-run mode backs up the previous output.

---

**Re-running always re-exports the same data** (verified in script)

Running the script twice for the same date re-reads and re-exports the same messages. This happens regardless of USR_VAL:

- **USR_VAL=0** (local simulation default): collect pass sees committed offsets past the window end and skips the partition. But `ALLOW_NO_DATA=YES` then fires the retry loop, which seeks directly back to `start_offset` with no committed offset check — reads the same messages again and writes output.
- **USR_VAL=1** (production re-run): script always seeks to `start_offset`, ignores committed offsets entirely. Old output file is renamed to `.1` before writing.

The committed offset in the collect pass is useful for **mid-run crash recovery** — if the script crashes after partially committing, the next run resumes from the committed position instead of re-reading from the start of the window. It does not prevent a full re-run.

---

**Part 1 — Default re-run (USR_VAL=0, ALLOW_NO_DATA=YES)**

**Data** — create `input/scenario_g.jsonl`:

```json
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 0, "totalInstances": 3, "numberOfMessagesPublished": 100}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:10+00:00"}
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 1, "totalInstances": 3, "numberOfMessagesPublished": 200}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:20+00:00"}
{"status": {"mandatorCode": "022", "businessDate": "2026-04-22", "reconciliationGroupId": 1, "instanceIndex": 2, "totalInstances": 3, "numberOfMessagesPublished": 150}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-04-22T23:59:30+00:00"}
```

**Steps**:

```bash
podman exec redpanda rpk topic delete inflow-topic
podman exec redpanda rpk topic create inflow-topic --partitions 1 --replicas 1
python3 produce_messages.py --file input/scenario_g.jsonl
bash run_local.sh 2026-04-22 022    # first run
ls -l output/data/                  # note file timestamp and size
bash run_local.sh 2026-04-22 022    # second run — NO topic wipe
ls -l output/data/                  # file is rewritten with same content
```

**Expected log sequence on second run**:

```
Consumer mode: TIME_WINDOW — seeking from time window start offset (SDA_USR_DEF_VAl != 1)
last committed offset: 2
No valid uncommitted data on the kafka topic for partition ... between 0 and 2
Total messages collected from all partitions: 0
ALLOW_NO_DATA is set to YES, will enter wait-and-listen mode
=== Retry mode: collecting messages for validation ====
seek to start offset: 0
Retry mode: validation passed
writing validated data to output/data/CPSB4Q00_2026-04-22.par
Exiting (code 0): data successfully consumed and written to output/data/CPSB4Q00_2026-04-22.par
```

**Expected output/data/ after second run**:

```
output/data/CPSB4Q00_2026-04-22.par    ← rewritten with same 3 rows
```

---

**Part 2 — Production re-run mode (USR_VAL=1) — output backup**

When the framework schedules a deliberate re-run it sets `USR_VAL=1`. The script renames the existing output file to `.1` before writing — this is the backup mechanism.

To simulate locally, temporarily change `TEST_USR_VAL="0"` to `TEST_USR_VAL="1"` in `run_local.sh`, then run:

```bash
bash run_local.sh 2026-04-22 022    # first run — writes CPSB4Q00_2026-04-22.par
bash run_local.sh 2026-04-22 022    # second run with USR_VAL=1
ls -l output/data/
```

**Expected log sequence on second run**:

```
Consumer mode: CONSUMED_COMMITTED — seeking from last committed offset (SDA_USR_DEF_VAl=1)
First run for this date detected, data in output/data/CPSB4Q00_2026-04-22.par.1
Seeked to offset 0 for partition 0
Validation passed; Writing data for, max run id is 1
writing validated data to output/data/CPSB4Q00_2026-04-22.par
Exiting (code 0): data successfully consumed and written to output/data/CPSB4Q00_2026-04-22.par
```

**Expected output/data/ after second run**:

```
output/data/CPSB4Q00_2026-04-22.par      ← fresh file — same 3 rows re-exported
output/data/CPSB4Q00_2026-04-22.par.1    ← backup of the first run output
```

Restore `TEST_USR_VAL="0"` in `run_local.sh` after testing.

---

### Scenario H — Empty topic

**What it tests**: Script runs against a topic with zero messages and `ALLOW_NO_DATA=YES`. Must wait and exit gracefully, not crash.

**Setup**: Wipe the topic so it is empty:
```bash
podman exec redpanda rpk topic delete inflow-topic
podman exec redpanda rpk topic create inflow-topic --partitions 1 --replicas 1
```

**Run**:
```bash
bash run_local.sh 2026-04-22 022
```

**Expected**: Script logs no messages found, retries every `RETRY_WAIT_SECONDS`, exits after `MAX_LISTEN_DURATION_HOURS` with a clean `ALLOW_NO_DATA` result. No crash, no exception.

---

## Fixing Duplicate Messages in Topic

If the script reports `VALIDATION FAILED: Duplicate instanceIds found`, it usually means `produce_messages.py` was run more than once without clearing the topic first.

**Verify duplicates (decodes Avro, counts per instanceIndex):**
```bash
python3 -c "
import io, json, requests
import fastavro
from kafka import KafkaConsumer
from collections import Counter

schema = fastavro.parse_schema(json.loads(
    requests.get('http://localhost:8081/subjects/inflow-topic-value/versions/latest').json()['schema']
))
consumer = KafkaConsumer('inflow-topic', bootstrap_servers='localhost:9092',
    auto_offset_reset='earliest', consumer_timeout_ms=3000)
counts = Counter(
    fastavro.schemaless_reader(io.BytesIO(m.value[5:]), schema)['status']['instanceIndex']
    for m in consumer
)
consumer.close()
for idx, count in sorted(counts.items()):
    flag = ' <-- DUPLICATE' if count > 1 else ''
    print(f'  instanceIndex={idx}  count={count}{flag}')
"
```

**Fix — wipe the topic and re-produce once:**
```bash
podman exec redpanda rpk topic delete inflow-topic
podman exec redpanda rpk topic create inflow-topic --partitions 1 --replicas 1
python3 produce_messages.py
```

Then re-run the pipeline:
```bash
bash run_local.sh 2026-04-22
```

---

## Teardown

```bash
bash stop_kafka.sh
```

---

## Troubleshooting

| Symptom | Likely cause |
|---|---|
| `podman pull` fails with EOF | Use internal registry: `container-registry.ubs.net/base-images/redpanda:latest-23.2-alpine-20231028` |
| Port mapping warnings | Expected — `--network=host` mode ignores `-p` flags; ports bind directly |
| `register_schema.py` fails — file not found | Put schema in `input/status_messages_schema.json` |
| Script exits with no data | Check `ASOF_DT` matches the `businessDate` in your JSONL records |
| `register_get_kafka_schema.py` fails — file not found | Put schema in `input/business_data_schema.json` |
| `00_get_kafka.py` exits with no data | Check `ASOF_DT` matches `tradeDate` in JSONL and `std_enqueueTime` is inside the 16:00–16:00 window |
| `No module named 'dsf_logging'` | Copy `dsf_logging.py` stub from UBS environment |
| `No module named 'assertf'` | `assertf.py` stub is already in this folder — make sure Python path includes it |

---

# 00_get_kafka.py Simulation

`00_get_kafka.py` consumes business data records from `business-topic` and writes them to a `.par` output file. It is separate from the status messages pipeline — both use the same Redpanda broker but different topics.

## Files added for this simulation

| File | Purpose |
|---|---|
| `00_get_kafka.py` | Copied from `trigger_based_new/`, SSL removed |
| `get_kafka_config.json` | Local sim config — points to `localhost:9092` and `business-topic` |
| `run_get_kafka_local.sh` | Runner script — sets all required env vars |
| `register_get_kafka_schema.py` | Registers business data schema with local registry |
| `produce_get_kafka_messages.py` | Produces Avro business records into `business-topic` |
| `input/business_data_schema.json` | Sample Avro schema with `std_enqueueTime` timestamp field |
| `input/business_data.jsonl` | Sample business records for testing |

## Time window for business data

The business data window is `START_TS=16:00:00, START_DT_OFFSET=0, STOP_TS=16:00:00, STOP_DT_OFFSET=-1`.

For `ASOF_DT=2026-04-22` this means:

```
Window START : 2026-04-22T16:00:00  (same day, 16:00)
Window END   : 2026-04-23T16:00:00  (next day, 16:00)  ← STOP_DT_OFFSET=-1 means day+1
```

That is a full 24-hour window. All `std_enqueueTime` values in your JSONL must fall between `2026-04-22T16:00:00+00:00` and `2026-04-23T16:00:00+00:00`.

Use `show_window.py` to verify:

```bash
python3 show_window.py 2026-04-22 022
```

Note: `show_window.py` reads from `status_messages_config.json` (status messages window). The business data window is in `get_kafka_config.json` under `LOCATION_TIME_WINDOW`. Verify manually using the formula above if the windows differ.

## Run sequence (each test session)

Broker must already be running from `bash start_kafka.sh`. Then:

```bash
# Step 1 — Register business data schema
python3 register_get_kafka_schema.py

# Step 2 — Produce test business records
python3 produce_get_kafka_messages.py

# Step 3 — Run the consumer
bash run_get_kafka_local.sh 2026-04-22
```

## Check output

```
output/get_kafka/CPSB4QST_2026-04-22/CPSB4QST.par    ← business records (JSON lines)
output/logs/                                           ← log file
```

## Inspect the business topic

```bash
# How many messages are in the topic
podman exec redpanda rpk topic describe business-topic -p

# Read all messages (raw bytes — not human-readable because Avro-encoded)
podman exec redpanda rpk topic consume business-topic --offset start --num 5

# Wipe and re-produce
podman exec redpanda rpk topic delete business-topic
podman exec redpanda rpk topic create business-topic --partitions 1 --replicas 1
python3 produce_get_kafka_messages.py
```

## What WAIT_FOR_SUBMIT does

In production `WAIT_FOR_SUBMIT=YES` means `00_get_kafka.py` waits for the status messages metadata file (written by `kafka_trigger_status_messages.py`) before consuming. This tells it how many records to expect and validates the count.

In the local simulation `get_kafka_config.json` sets `WAIT_FOR_SUBMIT=NO` so the two scripts run independently. To test the full integrated flow (status messages → metadata → get_kafka validation), run:

```bash
# Terminal 1 — run status messages first
bash run_local.sh 2026-04-22 022

# Then run get_kafka once metadata file exists
bash run_get_kafka_local.sh 2026-04-22 022
```

The metadata file is written to `output/data/` by the status messages script and read from `output/get_kafka/` by get_kafka. Adjust `PC_LOD_PROC_PATH` in `run_get_kafka_local.sh` if you want both scripts to share the same output directory.

## SSL changes applied to 00_get_kafka.py

The same 3 changes as `kafka_trigger_status_messages.py` — verified at these lines:

| Change | Line | Before | After |
|---|---|---|---|
| Schema registry session | 106 | `session.verify = ca_file` + `session.cert = ...` | `session.verify = False` |
| Schema registry URL | 112 | `https://` | `http://` |
| KafkaConsumer | 806 | `security_protocol="SSL"` + 4 ssl_ lines | `security_protocol="PLAINTEXT"` |

## Test scenarios for get_kafka

The same scenario principles apply as for status messages — the key difference is the topic (`business-topic`), the timestamp field (`std_enqueueTime`), and the 24-hour window.

### Scenario — Normal run (all records in window)

```bash
podman exec redpanda rpk topic delete business-topic
podman exec redpanda rpk topic create business-topic --partitions 1 --replicas 1
python3 produce_get_kafka_messages.py
bash run_get_kafka_local.sh 2026-04-22 022
```

**Expected**:

```
output/get_kafka/CPSB4QST_2026-04-22/CPSB4QST.par  ← 5 JSON lines (one per record)
```

Log will show:

```
INFLOW_TOPIC: business-topic
DT_UTC_START: 2026-04-22 16:00:00
DT_UTC_END:   2026-04-23 16:00:00
Exiting (code 0): data successfully consumed and written to ...CPSB4QST.par
```

### Scenario — Records outside time window

Create `input/business_outside_window.jsonl` using `std_enqueueTime` before `2026-04-22T16:00:00`:

```json
{"std_enqueueTime": "2026-04-22T10:00:00+00:00", "std_legalEntity": "UBS_AG", "accountId": "ACC-999", "productType": "EQUITY", "quantity": 100.0, "currency": "USD", "tradeDate": "2026-04-22", "settlementDate": "2026-04-24", "mandatorCode": "022"}
```

```bash
podman exec redpanda rpk topic delete business-topic
podman exec redpanda rpk topic create business-topic --partitions 1 --replicas 1
python3 produce_get_kafka_messages.py --file input/business_outside_window.jsonl
bash run_get_kafka_local.sh 2026-04-22 022
```

**Expected**: script finds 0 records in the 16:00–16:00 window, enters retry loop, exits after `MAX_LISTEN_DURATION_HOURS` with `ALLOW_NO_DATA` result. No output file written.

### Scenario — Empty topic

```bash
podman exec redpanda rpk topic delete business-topic
podman exec redpanda rpk topic create business-topic --partitions 1 --replicas 1
bash run_get_kafka_local.sh 2026-04-22 022
```

**Expected**: script finds 0 records, retries every `RETRY_WAIT_SECONDS=5`, exits cleanly after timeout.
