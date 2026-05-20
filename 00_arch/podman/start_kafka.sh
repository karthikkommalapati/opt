#!/usr/bin/env bash
# Starts a single-node Redpanda container (Kafka-compatible + built-in Schema Registry).
# Ports:  9092 = Kafka API,  8081 = Schema Registry,  8082 = Admin API
set -e

CONTAINER_NAME="redpanda"
TOPIC="inflow-topic"

IMAGE="container-registry.ubs.net/base-images/redpanda:latest-23.2-alpine-20231028"

echo ">>> Pulling Redpanda image..."
podman pull "$IMAGE"

echo ">>> Starting Redpanda..."
podman run -d \
  --name "$CONTAINER_NAME" \
  --network=host \
  "$IMAGE" \
  redpanda start \
    --overprovisioned \
    --smp 1 \
    --memory 1G \
    --reserve-memory 0M \
    --node-id 0 \
    --kafka-addr PLAINTEXT://0.0.0.0:9092 \
    --advertise-kafka-addr PLAINTEXT://localhost:9092 \
    --schema-registry-addr 0.0.0.0:8081 \
    --pandaproxy-addr 0.0.0.0:8082

echo ">>> Waiting for broker to be ready..."
for i in $(seq 1 30); do
  if podman exec "$CONTAINER_NAME" rpk cluster info >/dev/null 2>&1; then
    echo "    Broker ready."
    break
  fi
  echo "    Attempt $i/30 — waiting 2s..."
  sleep 2
done

echo ">>> Creating topic: $TOPIC"
podman exec "$CONTAINER_NAME" rpk topic create "$TOPIC" \
  --partitions 1 \
  --replicas 1

echo ""
echo "=== Redpanda is up ==="
echo "  Kafka:           localhost:9092"
echo "  Schema Registry: http://localhost:8081"
echo "  Admin API:       http://localhost:8082"
echo ""
echo "Next steps:"
echo "  python register_schema.py"
echo "  python produce_messages.py"
echo "  bash run_local.sh"
