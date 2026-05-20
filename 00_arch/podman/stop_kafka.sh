#!/usr/bin/env bash
# Stops and removes the Redpanda container.
set -e

CONTAINER_NAME="redpanda"

echo ">>> Stopping Redpanda..."
podman stop "$CONTAINER_NAME" 2>/dev/null || echo "  (not running)"
podman rm   "$CONTAINER_NAME" 2>/dev/null || echo "  (already removed)"
echo "Done."
