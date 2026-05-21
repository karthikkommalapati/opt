"""
Produces real Avro-encoded messages from a JSONL file into the local Redpanda topic.

Encodes each record in Confluent wire format: 0x00 + 4-byte schema_id + avro payload.
The Kafka message timestamp is taken from the record's eventTimestamp field so that
the script's time-window filter sees the correct timestamp.

Usage:
    python produce_messages.py                           # uses input/status_messages_data.jsonl
    python produce_messages.py --file input/part2.jsonl  # use a specific file (e.g. late messages)
"""
import argparse
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
TIMESTAMP_KEY = "eventTimestamp"        # field used by the script for time filtering
TIMESTAMP_FMT = "%Y-%m-%dT%H:%M:%S%z"

parser = argparse.ArgumentParser()
parser.add_argument("--file", "-f", default="input/status_messages_data.jsonl",
                    help="JSONL file to produce (default: input/status_messages_data.jsonl)")
DATA_FILE = parser.parse_args().file


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


def find_timestamp_paths(schema, path=()):
    """Recursively find all timestamp-micros/millis fields. Returns list of (tuple_path, unit)."""
    results = []
    if not isinstance(schema, dict) or schema.get("type") != "record":
        return results
    for field in schema.get("fields", []):
        fname = field["name"]
        ftype = field["type"]
        if isinstance(ftype, list):
            ftype = next((t for t in ftype if t != "null"), ftype[0])
        if isinstance(ftype, dict):
            lt = ftype.get("logicalType", "")
            if lt in ("timestamp-micros", "timestamp-millis"):
                results.append((path + (fname,), lt))
            elif ftype.get("type") == "record":
                results.extend(find_timestamp_paths(ftype, path + (fname,)))
    return results


def prepare_record(record, timestamp_paths):
    """Convert string timestamp fields → int (micros or millis since epoch) per schema logical type."""
    record = dict(record)
    for path, unit in timestamp_paths:
        obj = record
        for key in path[:-1]:
            if isinstance(obj, dict) and key in obj:
                obj = obj[key]
            else:
                obj = None
                break
        if obj is None:
            continue
        leaf = path[-1]
        val = obj.get(leaf)
        if isinstance(val, str):
            dt = datetime.fromisoformat(val)
            factor = 1_000_000 if unit == "timestamp-micros" else 1_000
            obj[leaf] = int(dt.timestamp() * factor)
    return record


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
timestamp_paths = find_timestamp_paths(parsed_schema)
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
        payload = encode(prepare_record(record, timestamp_paths), parsed_schema, schema_id)
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
