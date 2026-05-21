#!/usr/bin/env bash
# Wipes all Kafka topic data and Schema Registry subjects, then recreates empty topics.
# Leaves the container running — no need to restart Redpanda.
#
# Usage:
#   bash reset_kafka.sh           # reset topics + registry only
#   bash reset_kafka.sh --output  # also delete output/ directory contents

set -e

CONTAINER_NAME="redpanda"
STATUS_TOPIC="inflow-topic"
BUSINESS_TOPIC="business-topic"
REGISTRY_URL="http://localhost:8081"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

CLEAR_OUTPUT=false
if [[ "$1" == "--output" ]]; then
  CLEAR_OUTPUT=true
fi

# ── sanity check ─────────────────────────────────────────────────────────────
if ! podman container exists "$CONTAINER_NAME" 2>/dev/null; then
  echo "ERROR: container '$CONTAINER_NAME' is not running. Start it first with:"
  echo "  bash start_kafka.sh"
  exit 1
fi

echo "=== Kafka Reset ==="
echo ""

# ── delete topics (wipes all messages) ───────────────────────────────────────
echo ">>> Deleting topic: $STATUS_TOPIC"
podman exec "$CONTAINER_NAME" rpk topic delete "$STATUS_TOPIC" 2>/dev/null \
  && echo "    Deleted." \
  || echo "    Topic not found — skipping."

echo ">>> Deleting topic: $BUSINESS_TOPIC"
podman exec "$CONTAINER_NAME" rpk topic delete "$BUSINESS_TOPIC" 2>/dev/null \
  && echo "    Deleted." \
  || echo "    Topic not found — skipping."

# ── delete Schema Registry subjects ──────────────────────────────────────────
# Both deletes run unconditionally (|| true) so a subject stuck in soft-deleted
# state from a previous run still gets permanently removed.
echo ""
echo ">>> Deleting Schema Registry subject: ${STATUS_TOPIC}-value"
curl -sf -X DELETE "${REGISTRY_URL}/subjects/${STATUS_TOPIC}-value" > /dev/null 2>&1 || true
curl -sf -X DELETE "${REGISTRY_URL}/subjects/${STATUS_TOPIC}-value?permanent=true" > /dev/null 2>&1 || true
echo "    Done."

echo ">>> Deleting Schema Registry subject: ${BUSINESS_TOPIC}-value"
curl -sf -X DELETE "${REGISTRY_URL}/subjects/${BUSINESS_TOPIC}-value" > /dev/null 2>&1 || true
curl -sf -X DELETE "${REGISTRY_URL}/subjects/${BUSINESS_TOPIC}-value?permanent=true" > /dev/null 2>&1 || true
echo "    Done."

# ── recreate empty topics ─────────────────────────────────────────────────────
echo ""
echo ">>> Recreating topic: $STATUS_TOPIC"
podman exec "$CONTAINER_NAME" rpk topic create "$STATUS_TOPIC" \
  --partitions 1 \
  --replicas 1

echo ">>> Recreating topic: $BUSINESS_TOPIC"
podman exec "$CONTAINER_NAME" rpk topic create "$BUSINESS_TOPIC" \
  --partitions 1 \
  --replicas 1

# ── optionally clear output ───────────────────────────────────────────────────
if [[ "$CLEAR_OUTPUT" == true ]]; then
  echo ""
  echo ">>> Clearing output/ directory..."
  rm -rf "${SCRIPT_DIR}/output"
  echo "    Done."
fi

echo ""
echo "=== Reset complete ==="
echo "  Topics recreated (empty):  $STATUS_TOPIC, $BUSINESS_TOPIC"
echo "  Schema Registry:           subjects deleted"
echo ""
echo "Next — re-register schemas and produce fresh data:"
echo "  Status messages:"
echo "    python3 register_schema.py"
echo "    python3 produce_messages.py"
echo "    bash run_local.sh <YYYY-MM-DD>"
echo ""
echo "  Business data (get_kafka):"
echo "    python3 register_get_kafka_schema.py"
echo "    python3 produce_get_kafka_messages.py"
echo "    bash run_get_kafka_local.sh <YYYY-MM-DD>"
