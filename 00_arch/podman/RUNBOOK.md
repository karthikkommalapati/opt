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

## Re-run without restarting broker

If Redpanda is already running, skip steps 1–2 and repeat from step 3:

```bash
python3 produce_messages.py
bash run_local.sh 2026-05-20
```

---

## Troubleshooting

| Symptom | Likely cause |
|---|---|
| `podman pull` fails with EOF | Use internal registry: `container-registry.ubs.net/base-images/redpanda:latest-23.2-alpine-20231028` |
| Port mapping warnings | Expected — `--network=host` mode ignores `-p` flags; ports bind directly |
| `register_schema.py` fails — file not found | Put schema in `input/status_messages_schema.json` |
| Script exits with no data | Check `ASOF_DT` matches the `businessDate` in your JSONL records |
