#!/usr/bin/env python3
"""
Standalone filter test — no Kafka, no Podman, no Avro required.

Tests the exact get_nested_value + message_matches_filters logic from 00_get_kafka.py
using plain Python dicts as stand-ins for decoded Avro messages.

Usage:
    python3 test_filter.py                          # run built-in test cases
    python3 test_filter.py --meta path/to/meta.txt  # build filter from a real metadata file
    python3 test_filter.py --records path/to/msgs.jsonl  # test real message records
    python3 test_filter.py --meta meta.txt --records msgs.jsonl  # both
"""

import json
import sys
import argparse
import os

CONFIG_FILE = os.path.join(os.path.dirname(__file__), "get_kafka_config.json")

# ---------------------------------------------------------------------------
# Filter functions — copied verbatim from 00_get_kafka.py
# ---------------------------------------------------------------------------

def get_nested_value(msg_dict: dict, dot_path: str):
    parts = dot_path.split('.')
    current = msg_dict
    for part in parts:
        if not isinstance(current, dict) or part not in current:
            return None
        current = current[part]
    return current


def message_matches_filters(msg_dict: dict, filter_values: dict) -> bool:
    for field, expected in filter_values.items():
        actual = get_nested_value(msg_dict, field)
        if actual is None:
            print(f"    [WARN] field '{field}' not found in message — message excluded")
            return False
        if str(actual) != str(expected):
            return False
    return True


# ---------------------------------------------------------------------------
# Config + metadata loading
# ---------------------------------------------------------------------------

def load_config():
    with open(CONFIG_FILE, "r") as f:
        return json.load(f)


def load_metadata_from_file(path: str, separator: str) -> dict:
    with open(path, "r", encoding="utf-8", errors="ignore") as f:
        lines = [ln.rstrip("\n") for ln in f if ln.strip()]
    if len(lines) != 2:
        raise ValueError(f"Metadata file must have exactly 2 non-blank lines; got {len(lines)}")
    headers = [h.strip() for h in lines[0].split(separator)]
    values  = [v.strip() for v in lines[1].split(separator)]
    if len(headers) != len(values):
        raise ValueError(f"Header count {len(headers)} != value count {len(values)}")
    return dict(zip(headers, values))


def build_filter_values(metadata: dict, filter_columns: list, field_map: dict) -> dict:
    """Exact logic from 00_get_kafka.py block C."""
    fv = {}
    for col in filter_columns:
        meta_field = None
        for mf, kf in field_map.items():
            if kf == col:
                meta_field = mf
                break
        if meta_field is None:
            raise KeyError(f"Kafka field '{col}' has no entry in METADATA_FILTER_FIELD_MAP")
        if meta_field not in metadata:
            raise KeyError(
                f"Metadata field '{meta_field}' (for kafka field '{col}') "
                f"not in metadata. Available: {list(metadata.keys())}"
            )
        fv[col] = metadata[meta_field]
    return fv


# ---------------------------------------------------------------------------
# Test runner
# ---------------------------------------------------------------------------

PASS = "\033[32mPASS\033[0m"
FAIL = "\033[31mFAIL\033[0m"

def run_case(label: str, msg: dict, filter_values: dict, expect_match: bool):
    result = message_matches_filters(msg, filter_values)
    ok = result == expect_match
    status = PASS if ok else FAIL
    outcome = "INCLUDED" if result else "EXCLUDED"
    expected_outcome = "INCLUDED" if expect_match else "EXCLUDED"
    print(f"  [{status}] {label}")
    if not ok:
        print(f"          expected={expected_outcome}  got={outcome}")
        print(f"          msg={msg}")
        print(f"          filter={filter_values}")


def run_builtin_tests(filter_values: dict):
    print("\n=== Built-in test cases ===\n")
    print(f"Active filter_values: {filter_values}\n")

    # Exact match — should pass
    run_case(
        "Exact match (all fields correct)",
        {"mandatorCode": "022", "businessDate": "2026-04-22"},
        filter_values, expect_match=True
    )

    # Wrong businessDate — should be excluded
    run_case(
        "Wrong businessDate (2026-04-15 vs expected 2026-04-22)",
        {"mandatorCode": "022", "businessDate": "2026-04-15"},
        filter_values, expect_match=False
    )

    # Wrong mandatorCode — should be excluded
    run_case(
        "Wrong mandatorCode (023 vs expected 022)",
        {"mandatorCode": "023", "businessDate": "2026-04-22"},
        filter_values, expect_match=False
    )

    # Both fields wrong — should be excluded
    run_case(
        "Both fields wrong",
        {"mandatorCode": "023", "businessDate": "2026-04-15"},
        filter_values, expect_match=False
    )

    # Missing Kafka field — should be excluded
    run_case(
        "businessDate field missing from message",
        {"mandatorCode": "022"},
        filter_values, expect_match=False
    )

    # Integer value for mandatorCode — str() comparison means "22" != "022"
    run_case(
        "mandatorCode as integer 22 (str(22)='22', expected='022') — EXCLUDED",
        {"mandatorCode": 22, "businessDate": "2026-04-22"},
        filter_values, expect_match=False
    )

    # Integer value matching as string — str(22) would fail but str(022) not possible in Python
    run_case(
        "mandatorCode as string '022' (correct)",
        {"mandatorCode": "022", "businessDate": "2026-04-22"},
        filter_values, expect_match=True
    )

    # Nested field test (dot-notation) — only relevant if a filter field contains a dot
    nested_fv = {"nested.field": "expected_value"}
    run_case(
        "Nested dot-notation field present and matching",
        {"nested": {"field": "expected_value"}},
        nested_fv, expect_match=True
    )
    run_case(
        "Nested dot-notation field present but wrong value",
        {"nested": {"field": "wrong_value"}},
        nested_fv, expect_match=False
    )
    run_case(
        "Nested dot-notation field missing (parent key absent)",
        {"other": {"field": "expected_value"}},
        nested_fv, expect_match=False
    )

    # Case sensitivity check
    run_case(
        "Field name case mismatch: 'BusinessDate' vs filter key 'businessDate' — EXCLUDED (field not found)",
        {"mandatorCode": "022", "BusinessDate": "2026-04-22"},
        filter_values, expect_match=False
    )


def run_records_file(path: str, filter_values: dict):
    print(f"\n=== Testing records from: {path} ===\n")
    print(f"Active filter_values: {filter_values}\n")
    with open(path, "r", encoding="utf-8") as f:
        lines = [ln.strip() for ln in f if ln.strip()]
    for i, line in enumerate(lines, 1):
        try:
            msg = json.loads(line)
        except json.JSONDecodeError as e:
            print(f"  [SKIP] Line {i}: invalid JSON — {e}")
            continue
        result = message_matches_filters(msg, filter_values)
        outcome = "\033[32mINCLUDED\033[0m" if result else "\033[31mEXCLUDED\033[0m"
        filter_fields = {k: msg.get(k, "<MISSING>") for k in filter_values}
        print(f"  Line {i:3d}: {outcome}  filter_fields={filter_fields}")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Test 00_get_kafka.py filter logic without Kafka/Podman")
    parser.add_argument("--meta",    help="Path to metadata file (pipe-delimited, header+1 data row)")
    parser.add_argument("--records", help="Path to JSONL file of decoded messages to test")
    parser.add_argument("--config",  help=f"Path to config JSON (default: {CONFIG_FILE})", default=CONFIG_FILE)
    args = parser.parse_args()

    # Load config
    try:
        cfg = json.load(open(args.config))
    except Exception as e:
        print(f"ERROR loading config {args.config}: {e}")
        sys.exit(1)

    filter_columns = cfg.get("METADATA_FILTER_COLUMNS", [])
    field_map      = cfg.get("METADATA_FILTER_FIELD_MAP", {})
    separator      = cfg.get("SEPERATOR", "|")
    count_field    = cfg.get("METADATA_COUNT_FIELD", "total_messages_published")

    if not filter_columns:
        print("WARNING: METADATA_FILTER_COLUMNS is empty — no filtering will occur")

    print(f"Config loaded from:        {args.config}")
    print(f"METADATA_FILTER_COLUMNS:   {filter_columns}")
    print(f"METADATA_FILTER_FIELD_MAP: {field_map}")
    print(f"SEPERATOR:                 '{separator}'")

    # Build filter_values
    if args.meta:
        try:
            metadata = load_metadata_from_file(args.meta, separator)
            print(f"\nMetadata loaded from: {args.meta}")
            print(f"Metadata dict: {metadata}")
        except Exception as e:
            print(f"ERROR loading metadata file: {e}")
            sys.exit(1)
    else:
        # Default metadata values matching get_kafka_config.json reference setup
        metadata = {
            "mandator":      "022",
            "business_date": "2026-04-22",
            count_field:     "5",
        }
        print(f"\nNo --meta file given. Using default metadata: {metadata}")

    try:
        filter_values = build_filter_values(metadata, filter_columns, field_map)
    except KeyError as e:
        print(f"\nERROR building filter_values: {e}")
        sys.exit(1)

    if args.records:
        run_records_file(args.records, filter_values)
    else:
        run_builtin_tests(filter_values)

    print()


if __name__ == "__main__":
    main()
