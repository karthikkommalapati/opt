"""
Produces real Avro-encoded messages from input/status_messages_data.jsonl
using the schema registered in the local Redpanda Schema Registry.

Encodes each record in Confluent wire format: 0x00 + 4-byte schema_id + avro payload.
The Kafka message timestamp is taken from the record's eventTimestamp field so that
the script's time-window filter sees the correct timestamp.

Usage:
    python produce_messages.py
"""
import io
import json
import struct
import sys
from datetime import datetime, timezone

import fastavro
import requests
from kafka import KafkaProducer

BROKER        = "localhost:9092"
REGISTRY_URL  = "http://localhost:8081"
TOPIC         = "inflow-topic"          # must match local_config.json
DATA_FILE     = "input/status_messages_data.jsonl"
TIMESTAMP_KEY = "eventTimestamp"        # field used by the script for time filtering
TIMESTAMP_FMT = "%Y-%m-%dT%H:%M:%S%z"


def fetch_schema_from_registry(topic):
    """Fetch schema and ID from the local registry (registered by register_schema.py)."""
    subject = f"{topic}-value"
    resp = requests.get(f"{REGISTRY_URL}/subjects/{subject}/versions/latest")
    resp.raise_for_status()
    data = resp.json()
    schema_id = data["id"]
    schema    = json.loads(data["schema"])
    return schema_id, fastavro.parse_schema(schema)


def parse_timestamp(record):
    """Extract Kafka timestamp (ms) from the record's eventTimestamp field."""
    ts_str = record.get(TIMESTAMP_KEY)
    if not ts_str:
        return int(datetime.now(timezone.utc).timestamp() * 1000)
    try:
        dt = datetime.strptime(ts_str, TIMESTAMP_FMT)
        return int(dt.timestamp() * 1000)
    except ValueError:
        return int(datetime.now(timezone.utc).timestamp() * 1000)


def encode(record, schema, schema_id):
    """Confluent wire format: magic byte + 4-byte schema_id + schemaless Avro."""
    buf = io.BytesIO()
    buf.write(b'\x00')
    buf.write(struct.pack('>I', schema_id))
    fastavro.schemaless_writer(buf, schema, record)
    return buf.getvalue()


# ── load data ────────────────────────────────────────────────────────────────
records = []
with open(DATA_FILE) as f:
    for lineno, line in enumerate(f, 1):
        line = line.strip()
        if not line:
            continue
        try:
            records.append(json.loads(line))
        except json.JSONDecodeError as e:
            print(f"WARNING: skipping line {lineno} (JSON error: {e})", file=sys.stderr)

if not records:
    print(f"ERROR: no records found in {DATA_FILE}", file=sys.stderr)
    sys.exit(1)

print(f"Data file:    {DATA_FILE}  ({len(records)} records)")

# ── fetch schema ──────────────────────────────────────────────────────────────
schema_id, parsed_schema = fetch_schema_from_registry(TOPIC)
print(f"Schema ID:    {schema_id}")
print(f"Topic:        {TOPIC}")
print()

# ── produce ───────────────────────────────────────────────────────────────────
producer = KafkaProducer(
    bootstrap_servers=BROKER,
    security_protocol="PLAINTEXT",
)

ok = 0
for i, record in enumerate(records):
    try:
        ts_ms   = parse_timestamp(record)
        payload = encode(record, parsed_schema, schema_id)
        future  = producer.send(TOPIC, value=payload, timestamp_ms=ts_ms)
        future.get(timeout=10)
        ok += 1
        if ok <= 5 or ok % 100 == 0:
            print(f"  [{ok}/{len(records)}] published — ts={datetime.fromtimestamp(ts_ms/1000, tz=timezone.utc).isoformat()}")
    except Exception as e:
        print(f"  WARNING: record {i+1} failed: {e}", file=sys.stderr)

producer.flush()
producer.close()

print()
print(f"Done — {ok}/{len(records)} messages published to topic '{TOPIC}'.")
if ok > 0:
    print("Next: bash run_local.sh <YYYY-MM-DD>   # use the businessDate from your data")
