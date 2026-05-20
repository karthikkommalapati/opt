# Input Files

Place your real files here with exactly these names:

| File | What to put here |
|---|---|
| `status_messages_schema.json` | Your Avro schema exported from Confluent (raw schema OR full Confluent API response — both handled) |
| `status_messages_data.jsonl` | Your real data — one JSON record per line |

## Schema file formats — both work

**Raw Avro schema** (starts with `{"type": "record", ...}`):
```json
{
  "type": "record",
  "name": "StatusMessage",
  ...
}
```

**Full Confluent API response** (has a `"schema"` key):
```json
{
  "id": 42,
  "version": 3,
  "schema": "{\"type\":\"record\",\"name\":\"StatusMessage\",...}"
}
```

## Data file format

One JSON object per line (JSON Lines / JSONL):
```
{"status": {"mandatorCode": "022", "businessDate": "2026-05-07", ...}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-05-07T17:00:00+0000"}
{"status": {"mandatorCode": "022", "businessDate": "2026-05-07", ...}, "producer": "CLIENT_STRUCTURES", "eventTimestamp": "2026-05-07T17:01:00+0000"}
```

## Important: ASOF_DT must match your data

When running the script, pass the businessDate from your data:
```bash
bash run_local.sh 2026-05-07    # replace with the date in your JSONL records
```
