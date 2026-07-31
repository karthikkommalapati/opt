#!/usr/bin/env bash
# Runs 00_get_kafka.py against local Redpanda.
# Config used: get_kafka_config.json (symlinked as 1001_CPSB4QST_config.json)
# Adjust ASOF_DT to match the businessDate in your JSONL data.
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# ── business date (default: today) ──────────────────────────────────────────
ASOF_DT="${1:-$(date +%Y-%m-%d)}"
echo ">>> Running with ASOF_DT=$ASOF_DT"

# ── mandator (optional second arg, default 022) ──────────────────────────────
if [ -n "${2:-}" ]; then
  DSF_MANDATOR="$2"
  echo ">>> Using mandator: $DSF_MANDATOR"
else
  DSF_MANDATOR="022"
  echo "WARNING: No mandator provided — falling back to default '022'."
  echo "         To specify: bash run_get_kafka_local.sh <YYYY-MM-DD> <mandator>"
fi

# ── output directories ───────────────────────────────────────────────────────
mkdir -p "$SCRIPT_DIR/output/logs"
mkdir -p "$SCRIPT_DIR/output/get_kafka"

# ── DSF framework variables ──────────────────────────────────────────────────
export SDA="TEST"
export DSF_MANDATOR
export DSF_DOMAIN="T"
export DSF_CHARMAP="UTF-8"
export DSF_JAVA_TZ="UTC"
export DSF_MAIN_ID="1001"

# SDA-prefixed vars (pattern: {SDA}_ASOF_DT etc.)
export TEST_ASOF_DT="$ASOF_DT"
export TEST_ITERATION_NR="1"
export TEST_KNW_FRO_TS="$(date -u +%Y-%m-%d-%H.%M.%S)"
export TEST_USR_VAL="0"

# Path vars — get_kafka writes to output/get_kafka/ (separate from status messages output)
export CFG_PROC_PATH="$SCRIPT_DIR"
export PC_LOD_PROC_PATH="$SCRIPT_DIR/output/get_kafka"
export LOG_PROC_PATH="$SCRIPT_DIR/output/logs"

# Framework-injected execution-control vars
export FEED_NAME="CPSB4QST"
export CFG_CTL="MAIN"
export KEEP_TGT="YES"

# Process/audit IDs (any value works locally)
export PARENT_PID="$$"
export AUDIT_ID="LOCAL_TEST_001"

# ── symlink config so the script finds it ───────────────────────────────────
# The script looks for: {CFG_PROC_PATH}/{DSF_MAIN_ID}_{FEED_NAME}_config.json
#                     = $SCRIPT_DIR/1001_CPSB4QST_config.json
EXPECTED_CONFIG="$SCRIPT_DIR/1001_CPSB4QST_config.json"
if [ ! -L "$EXPECTED_CONFIG" ] && [ ! -e "$EXPECTED_CONFIG" ]; then
  ln -s "$SCRIPT_DIR/get_kafka_config.json" "$EXPECTED_CONFIG"
  echo ">>> Symlinked get_kafka_config.json → 1001_CPSB4QST_config.json"
fi

# ── run ──────────────────────────────────────────────────────────────────────
echo ">>> Starting 00_get_kafka.py..."
echo ""
python3 "$SCRIPT_DIR/00_get_kafka.py"
