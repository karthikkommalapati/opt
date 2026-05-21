"""
Produces real Avro-encoded business data messages into the local business-topic.

Encodes each record in Confluent wire format: 0x00 + 4-byte schema_id + avro payload.
The Kafka message timestamp is taken from std_enqueueTime so the script's
time-window filter sees the correct timestamp.

Usage:
    python3 produce_get_kafka_messages.py                           # uses input/business_data.jsonl
    python3 produce_get_kafka_messages.py --file input/custom.jsonl
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
TOPIC         = "business-topic"
TIMESTAMP_KEY = "std_enqueueTime"

parser = argparse.ArgumentParser()
parser.add_argument("--file", "-f", default="input/business_data.jsonl",
                    help="JSONL file to produce (default: input/business_data.jsonl)")
DATA_FILE = parser.parse_args().file


def fetch_schema_from_registry(topic):
    """Returns (schema_id, raw_schema_dict, parsed_schema).
    raw_schema_dict is used for find_timestamp_paths — the fastavro parsed schema
    has a different internal structure that can cause timestamp fields to be missed.
    parsed_schema is used for encoding only.
    """
    subject = f"{topic}-value"
    resp = requests.get(f"{REGISTRY_URL}/subjects/{subject}/versions/latest")
    resp.raise_for_status()
    data = resp.json()
    schema_id  = data["id"]
    raw_schema = json.loads(data["schema"])
    parsed     = fastavro.parse_schema(raw_schema)
    return schema_id, raw_schema, parsed


def parse_ts_string(ts_str):
    """Parse a timestamp string to a datetime, handling both T and space separators."""
    # "2026-04-22 03:44:33.949595+00:00" → normalise to ISO format with T
    return datetime.fromisoformat(ts_str.replace(" ", "T", 1))


def parse_timestamp_ms(record):
    """Extract Kafka message timestamp (ms) from the record's timestamp key field."""
    ts_str = record.get(TIMESTAMP_KEY)
    if not ts_str:
        return int(datetime.now(timezone.utc).timestamp() * 1000)
    try:
        return int(parse_ts_string(str(ts_str)) .timestamp() * 1000)
    except (ValueError, AttributeError):
        return int(datetime.now(timezone.utc).timestamp() * 1000)


def find_timestamp_paths(schema, path=()):
    """Walk the raw JSON schema dict recursively and return (path_tuple, unit) for every
    timestamp-micros / timestamp-millis field at any nesting depth.
    Must be called on the raw schema dict, not the fastavro-parsed object.
    """
    results = []
    if not isinstance(schema, dict) or schema.get("type") != "record":
        return results
    for field in schema.get("fields", []):
        fname = field["name"]
        ftype = field["type"]
        # unwrap union ["null", <type>] or [<type>, "null"]
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
    """Convert every timestamp string field → int (micros or millis since epoch).
    Handles:
      - "YYYY-MM-DDTHH:MM:SS.ffffff+00:00"  (T separator)
      - "YYYY-MM-DD HH:MM:SS.ffffff+00:00"   (space separator)
    Already-integer values and None (nullable) are left unchanged.
    """
    record = dict(record)
    for path, unit in timestamp_paths:
        obj = record
        for key in path[:-1]:
            obj = obj.get(key) if isinstance(obj, dict) else None
            if obj is None:
                break
        if obj is None:
            continue
        leaf = path[-1]
        val  = obj.get(leaf)
        if isinstance(val, str):
            try:
                dt = parse_ts_string(val)
                factor = 1_000_000 if unit == "timestamp-micros" else 1_000
                obj[leaf] = int(dt.timestamp() * factor)
            except ValueError as e:
                print(f"WARNING: cannot convert timestamp field '{leaf}' value '{val}': {e}",
                      file=sys.stderr)
        # int → already correct; None → nullable null; both fine as-is
    return record


def encode(record, schema, schema_id):
    buf = io.BytesIO()
    buf.write(b'\x00')
    buf.write(struct.pack('>I', schema_id))
    fastavro.schemaless_writer(buf, schema, record)
    return buf.getvalue()


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

schema_id, raw_schema, parsed_schema = fetch_schema_from_registry(TOPIC)
timestamp_paths = find_timestamp_paths(raw_schema)  # raw dict — not the fastavro-parsed object
print(f"Schema ID:    {schema_id}")
print(f"Topic:        {TOPIC}")
print()

producer = KafkaProducer(bootstrap_servers=BROKER, security_protocol="PLAINTEXT")

ok = 0
for i, record in enumerate(records):
    try:
        ts_ms   = parse_timestamp_ms(record)
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
    print("Next: bash run_get_kafka_local.sh <YYYY-MM-DD>   # use the tradeDate from your data")
