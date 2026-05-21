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


def _unwrap_union(ftype):
    """From a union list, return the first timestamp type if present, else first non-null type."""
    if not isinstance(ftype, list):
        return ftype
    for t in ftype:
        if isinstance(t, dict) and t.get("logicalType", "") in ("timestamp-micros", "timestamp-millis"):
            return t
    return next((t for t in ftype if t != "null"), ftype[0])


def _get_logical_type(ftype, field):
    """logicalType can live inside the type dict OR at the field level."""
    if isinstance(ftype, dict):
        return ftype.get("logicalType", "")
    return field.get("logicalType", "")


def find_timestamp_paths(schema, path=()):
    """Recursively find all timestamp-micros/millis fields at any depth.
    Traverses records, arrays, and maps.
    Must be called on the raw JSON schema dict, not the fastavro-parsed object.
    """
    results = []
    if not isinstance(schema, dict):
        return results
    t = schema.get("type")
    if t == "record":
        for field in schema.get("fields", []):
            fname = field["name"]
            ftype = _unwrap_union(field["type"])
            lt    = _get_logical_type(ftype, field)
            if lt in ("timestamp-micros", "timestamp-millis"):
                results.append((path + (fname,), lt))
            elif isinstance(ftype, dict):
                results.extend(find_timestamp_paths(ftype, path + (fname,)))
    elif t == "array":
        results.extend(find_timestamp_paths(schema.get("items", {}), path))
    elif t == "map":
        results.extend(find_timestamp_paths(schema.get("values", {}), path))
    return results


def _convert_ts(obj, path, unit):
    """Recursively navigate obj by path, converting string timestamps to int.
    Handles dicts and lists (arrays of records) at any level of the path.
    """
    if isinstance(obj, list):
        for item in obj:
            _convert_ts(item, path, unit)
        return
    if not isinstance(obj, dict):
        return
    if len(path) == 1:
        leaf = path[0]
        val  = obj.get(leaf)
        if isinstance(val, str):
            try:
                dt = parse_ts_string(val)
                factor = 1_000_000 if unit == "timestamp-micros" else 1_000
                obj[leaf] = int(dt.timestamp() * factor)
            except ValueError as e:
                print(f"WARNING: cannot convert '{leaf}' value '{val}': {e}", file=sys.stderr)
    else:
        child = obj.get(path[0])
        if child is not None:
            _convert_ts(child, path[1:], unit)


def prepare_record(record, timestamp_paths):
    """Convert every timestamp string field → int (micros or millis since epoch).
    Handles T and space separators, nested records, and arrays of records.
    Already-integer values and None are left unchanged.
    """
    record = dict(record)
    for path, unit in timestamp_paths:
        _convert_ts(record, path, unit)
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
print(f"Timestamp fields detected: {[(('.'.join(p)), u) for p, u in timestamp_paths] or 'NONE — check schema logicalType declarations'}")
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
