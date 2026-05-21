"""
Registers the business data Avro schema with the local Redpanda Schema Registry
for the business-topic. Run once after start_kafka.sh.

Usage:
    python3 register_get_kafka_schema.py
"""
import json
import sys
import requests

REGISTRY_URL = "http://localhost:8081"
TOPIC        = "business-topic"
SCHEMA_FILE  = "input/business_data_schema.json"

with open(SCHEMA_FILE) as f:
    raw = json.load(f)

subject = f"{TOPIC}-value"
payload = {"schema": json.dumps(raw)}

resp = requests.post(
    f"{REGISTRY_URL}/subjects/{subject}/versions",
    headers={"Content-Type": "application/vnd.schemaregistry.v1+json"},
    json=payload,
)

if resp.status_code in (200, 201):
    schema_id = resp.json()["id"]
    print(f"Schema registered — subject: {subject}  id: {schema_id}")
    print(f"Next: python3 produce_get_kafka_messages.py")
else:
    print(f"ERROR {resp.status_code}: {resp.text}", file=sys.stderr)
    sys.exit(1)
