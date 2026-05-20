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

## Teardown

```bash
bash stop_kafka.sh
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

## Re-run without restarting broker

If Redpanda is already running, skip steps 1–2 and repeat from step 3:

```bash
python3 produce_messages.py
bash run_local.sh 2026-05-20
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

## Troubleshooting

| Symptom | Likely cause |
|---|---|
| `podman pull` fails with EOF | Use internal registry: `container-registry.ubs.net/base-images/redpanda:latest-23.2-alpine-20231028` |
| Port mapping warnings | Expected — `--network=host` mode ignores `-p` flags; ports bind directly |
| `register_schema.py` fails — file not found | Put schema in `input/status_messages_schema.json` |
| Script exits with no data | Check `ASOF_DT` matches the `businessDate` in your JSONL records |
