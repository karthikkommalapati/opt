"""
Registers input/status_messages_schema.json with the Redpanda Schema Registry.
Handles both raw Avro schema and full Confluent API response format.
Run once after start_kafka.sh, before producing messages.

Usage:
    python register_schema.py
"""
import json
import sys
import requests

REGISTRY_URL = "http://localhost:8081"
TOPIC        = "inflow-topic"          # must match local_config.json
SCHEMA_FILE  = "input/status_messages_schema.json"


def load_avro_schema(path):
    """Return (schema_dict) regardless of whether the file is a raw Avro schema
    or a full Confluent API response (which wraps the schema as a JSON string)."""
    with open(path) as f:
        data = json.load(f)

    if "schema" in data and isinstance(data["schema"], str):
        # Full Confluent response: schema is a JSON string inside the "schema" key
        return json.loads(data["schema"])
    else:
        # Raw Avro schema
        return data


schema  = load_avro_schema(SCHEMA_FILE)
subject = f"{TOPIC}-value"
payload = {"schema": json.dumps(schema)}

print(f"Schema file:  {SCHEMA_FILE}")
print(f"Subject:      {subject}")
print(f"Registry:     {REGISTRY_URL}")
print()

resp = requests.post(
    f"{REGISTRY_URL}/subjects/{subject}/versions",
    json=payload,
    headers={"Content-Type": "application/vnd.schemaregistry.v1+json"},
)

if resp.status_code in (200, 201):
    schema_id = resp.json()["id"]
    print(f"OK — schema registered with ID: {schema_id}")
    print(f"Verify: curl http://localhost:8081/subjects/{subject}/versions/latest")
else:
    print(f"ERROR {resp.status_code}: {resp.text}", file=sys.stderr)
    sys.exit(1)
