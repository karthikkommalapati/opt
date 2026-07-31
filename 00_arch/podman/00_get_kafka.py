

verbose=1

import os

import dsf_logging

logging_level = 20
verbose_log=True
SDA, FEED_NAME = os.environ["SDA"].upper(), "CPSB4QST"
ASOF_DT, PARENT_PID, AUDIT_ID = os.environ[f"{SDA}_ASOF_DT"], os.environ["PARENT_PID"], os.environ["AUDIT_ID"]
get_kafka_log_path = os.environ["LOG_PROC_PATH"]
get_kafak_log_name = f"{FEED_NAME}.{ASOF_DT}.{PARENT_PID}.{AUDIT_ID}_getKafka.log"
dsf_logger = dsf_logging.DSF_logging()

dsf_logger.get_logger(
    name=get_kafak_log_name,
    output_path=get_kafka_log_path,
    level=logging_level,
    verbose=verbose_log
)

dsf_logger.log_msg(f"Get Kafka logging to be stored at: {get_kafka_log_path}/{get_kafak_log_name}", level=20)


# Pytho programme to conver
# Kafka data to CSV

try:
    import base64
    import collections
    import dataclasses
    import glob
    import io
    import json
    import logging
    import os
    import pathlib
    import pprint
    import re
    import sched
    import subprocess
    import sys
    import time as py_time
    import traceback
    from datetime import date, datetime, time, timedelta
    from decimal import Decimal
    from typing import Dict, List, Optional, Set, Tuple
    from urllib.parse import unquote

    import dsf_logging
    import fastavro
    import kafka
    import pandas as pd
    import pytz
    import requests
    from assertf import *
    from fastavro import parse_schema, reader
    from kafka import OffsetAndMetadata
    from requests.auth import HTTPBasicAuth
except Exception as err:

    raise dsf_logger.log_error_msg(msg=f'ERROR_LOADING_LIBRARIES', err=err)


# dub

for key, val in os.environ.items():
    dsf_logger.log_msg(f'{key}={val}', level = 20)



#  list files

def list_files_in_folder(folder) -> List[str]:
    return glob.glob(folder)



# Return dict from file

def path_to_dict(file_path: Optional[str]) -> dict:
    try:
        with open(assert_path(file_path)) as file_dict:
            return assert_dict(json.load(file_dict))

    except Exception as err:
        raise dsf_logger.log_error_msg(
            msg=f'Unexpected error opening file: {file_path}', err=err
        )

# convert timestamp t string
def ts_to_str(ts:float) -> str:
    return datetime.fromtimestamp(ts).strftime(DATETIME_FORMAT)

# get avro schema

def get_avro_schema(topic, reg_addrs, ca_file, sslcert, sslkey, rversion):
    # API end point for retreiving the latest schema version fro a topic
    endpoint = f'/subjects/{topic}-value/versions/{rversion}'
    reg_addrs = reg_addrs.split(",")

    # set up connection properties
    session = requests.Session()
    session.verify = False


    # Iterate through the schema registry addresses and try each one

    for reg_addr in reg_addrs:
        url = f"http://{reg_addr}{endpoint}"

        dsf_logger.log_msg(f"Using url {url}", level=20)

        try:

            # make a get request with ssl
            response = session.get(url)
            response.raise_for_status() # raise an exception for non-successful responses

            # Extract the schema from the response JSON
            schema = json.loads(response.json()['schema']) # could be an avro_schema directly (would check if the avro schema is ok by parsing it to avro)
            version = response.json()['version']
            schema_id = response.json()['id']

            dsf_logger.log_msg(f"Schema version found: {version}", level = 20)
            dsf_logger.log_msg(f"Schema id found: {schema_id}", level = 20)
            return version, schema_id, schema

        except requests.exceptions.RequestException as e:

            dsf_logger.log_msg(
                f"Failed to retrieve schema from {reg_addr}. Error: {str(e)}", level = 30
            )

    if rversion == 'latest':
        raise dsf_logger.log_error('Unable to retrieve the schema. All schema registry address failed.')
    else:
        dsf_logger.log_msg('Unable to retrieve the schema. All schema registry address failed.', level=30)


# AVRO check for fields containing decimal

def is_dec_type(ftype):
    if (isinstance(ftype, str)) and (ftype.lower() == 'decimal'):
        return True
    elif (isinstance(ftype, dict) and ftype.get('type') == 'bytes') and (ftype.get('logicalType') == 'decimal'):
        return True
    return False


def get_deicmal_feilds(avschema):
    decattrs = []
    def traverse_schema(schema):
        for field in schema['fields']:
            ftype = field['type']
            if is_dec_type(ftype):
                decattrs.append(field['name'])
            if 'fields' in field:
                decattrs.extend(traverse_schema(field))

    traverse_schema(avschema)

    if not decattrs:
        dsf_logger.log_msg(f"Decimal conversion requested, but no decimal fileds found", level=30)

    else:
        dsf_logger.log_msg(f"Decimal fields found {decattrs}", level=30)

    return decattrs


# ---------------------------------------------------------------------------
# Metadata-filtering helpers
# ---------------------------------------------------------------------------

def get_nested_value(msg_dict: dict, dot_path: str):
    """Traverse msg_dict using dot-notation path. Returns None if any key missing."""
    parts = dot_path.split('.')
    current = msg_dict
    for part in parts:
        if not isinstance(current, dict) or part not in current:
            return None
        current = current[part]
    return current


def load_metadata_file(path: str, separator: str) -> dict:
    """
    Load the status-messages metadata file.
    - Fails immediately (os._exit(1)) if file not found.
    - Fails immediately if there are not exactly 2 non-blank lines (header + 1 data row).
    Returns a dict of {field_name: value}.
    """
    dsf_logger.log_msg(f"Looking for metadata file at: {path}", level=20)

    if not os.path.isfile(path):
        dsf_logger.log_msg(
            f"METADATA FILE NOT FOUND: {path}  "
            f"The trigger-based status-messages script must complete successfully before this script runs. "
            f"Cannot continue without the metadata file.",
            level=40
        )
        os._exit(1)

    try:
        with open(path, 'r', encoding='utf-8', errors='ignore') as mf:
            lines = [ln.rstrip('\n') for ln in mf if ln.strip()]
    except Exception as e:
        dsf_logger.log_msg(f"Error reading metadata file {path}: {e}", level=40)
        os._exit(1)

    if len(lines) < 2:
        dsf_logger.log_msg(
            f"Metadata file {path} has {len(lines)} non-blank line(s) — "
            f"expected exactly 2 (header row + 1 data row). "
            f"File may be incomplete or empty.",
            level=40
        )
        os._exit(1)

    if len(lines) > 2:
        dsf_logger.log_msg(
            f"Metadata file {path} has {len(lines)} non-blank lines — "
            f"expected exactly 2 (header row + 1 data row). "
            f"Multiple data rows are not permitted; the file may have been appended incorrectly. "
            f"Data rows found: {len(lines) - 1}. "
            f"Remove the extra rows and rerun.",
            level=40
        )
        os._exit(1)

    headers = [h.strip() for h in lines[0].split(separator)]
    values  = [v.strip() for v in lines[1].split(separator)]

    if len(headers) != len(values):
        dsf_logger.log_msg(
            f"Metadata file {path} header count ({len(headers)}) does not match "
            f"value count ({len(values)}). "
            f"Headers: {headers}  Values: {values}",
            level=40
        )
        os._exit(1)

    metadata = dict(zip(headers, values))
    dsf_logger.log_msg(f"Metadata file loaded successfully from: {path}", level=20)
    dsf_logger.log_msg(f"Metadata fields: {list(metadata.keys())}", level=20)
    dsf_logger.log_msg(f"Metadata values: {metadata}", level=20)
    return metadata


def message_matches_filters(msg_dict: dict, filter_values: dict) -> bool:
    """
    Returns True only if ALL filter_values match in msg_dict.
    filter_values: {kafka_field (dot-notation ok) -> expected_str_value or list of expected_str_values}
    Top-level fields checked directly; dot-notation traverses nested dicts.
    A list value means OR-match: the field passes if it matches any value in the list.
    Matching is case-insensitive on values (field names are matched as-is).
    """
    for field, expected in filter_values.items():
        actual = get_nested_value(msg_dict, field)
        if actual is None:
            dsf_logger.log_msg(
                f"Filter field '{field}' not found in message "
                f"(checked via dot-notation path). Message excluded.",
                level=30
            )
            return False
        if isinstance(expected, list):
            if str(actual).upper() not in [str(e).upper() for e in expected]:
                return False
        else:
            if str(actual).upper() != str(expected).upper():
                return False
    return True


_FILTER_CONFIG_FORMAT_HELP = (
    "Correct format examples:\n"
    '  Normal (top-level) field:   "{name}": {{"fieldName": "VALUE"}}\n'
    '  OR-match (multiple values): "{name}": {{"fieldName": ["VALUE1", "VALUE2"]}}\n'
    "  Nested field (dot-notation in the KEY, not the value):\n"
    '                               "{name}": {{"parent.child": "VALUE"}}'
)


def load_and_validate_filter_config(name: str, config: dict) -> dict:
    """
    Loads a PRE_FILTER_VALUES / VALIDATION_FILTER_VALUES-shaped config entry, lowercases
    its field-name keys, and validates its shape. Hard-exits with a clear message and
    format examples on any structural error, rather than crashing with a raw traceback
    or silently matching nothing.
    """
    raw = config.get(name, {})
    try:
        raw_dict = dict(raw)
    except (TypeError, ValueError):
        dsf_logger.log_msg(
            f"CONFIG ERROR: {name} must be a JSON object ({{...}}), got: {raw!r}.\n"
            f"{_FILTER_CONFIG_FORMAT_HELP.format(name=name)}",
            level=40
        )
        os._exit(9)

    result = {str(k).lower(): v for k, v in raw_dict.items()}

    for field, expected in result.items():
        values_to_check = expected if isinstance(expected, list) else [expected]
        for v in values_to_check:
            if isinstance(v, (dict, list)):
                dsf_logger.log_msg(
                    f"CONFIG ERROR: {name} field '{field}' has an unsupported value type "
                    f"({type(v).__name__}): {expected!r}. Values must be a single "
                    f"string/number, or a list of strings/numbers for OR-matching — not a "
                    f"nested object or list-of-lists.\n"
                    f"{_FILTER_CONFIG_FORMAT_HELP.format(name=name)}",
                    level=40
                )
                os._exit(9)

    return result


def log_count_comparison(
    expected: int, actual: int, tolerance_pct: float, metadata_path: str
) -> bool:
    """
    Logs a detailed count validation report.
    Returns True if actual >= expected (at or above — accept immediately).
    Returns False if actual < expected (under-count — retry or accept at exhaustion).
    No upper cap: over-count is always acceptable once stable.
    """
    sep = "=" * 70
    dsf_logger.log_msg(sep, level=20)
    dsf_logger.log_msg("COUNT VALIDATION REPORT", level=20)
    dsf_logger.log_msg(f"  Source of expected count : {metadata_path}", level=20)
    dsf_logger.log_msg(f"  Expected count (metadata): {expected:,}", level=20)
    dsf_logger.log_msg(f"  Actual filtered count    : {actual:,}", level=20)

    if expected == 0 and actual == 0:
        dsf_logger.log_msg("  Result: EXACT MATCH — both expected and actual are 0", level=20)
        dsf_logger.log_msg(sep, level=20)
        return True

    if expected == 0:
        dsf_logger.log_msg(
            f"  Result: MISMATCH — expected 0 but got {actual:,} messages. "
            f"Cannot compute percentage when expected is 0.",
            level=40
        )
        dsf_logger.log_msg(sep, level=20)
        return False

    diff      = actual - expected
    pct_diff  = abs(diff) / expected * 100.0
    direction = "higher" if diff > 0 else ("lower" if diff < 0 else "equal")

    dsf_logger.log_msg(
        f"  Absolute difference      : {abs(diff):,} ({'+' if diff >= 0 else ''}{diff:,})",
        level=20
    )
    dsf_logger.log_msg(
        f"  Percentage difference    : {pct_diff:.1f}% {direction} than expected",
        level=20
    )
    dsf_logger.log_msg(f"  Lower tolerance          : -{tolerance_pct}%  (upper: unlimited)", level=20)

    low = expected - expected * tolerance_pct / 100.0
    dsf_logger.log_msg(
        f"  Acceptable range         : [{low:,.0f}, unlimited]",
        level=20
    )

    if actual >= expected:
        dsf_logger.log_msg(
            f"  Result: AT OR ABOVE EXPECTED — got {actual:,}, expected {expected:,} "
            f"({abs(diff):,} {direction}, {pct_diff:.1f}%). "
            f"No upper cap. Checking stability before accepting.",
            level=20
        )
        at_or_above = True
    elif pct_diff <= tolerance_pct:
        dsf_logger.log_msg(
            f"  Result: BELOW EXPECTED — got {actual:,} of {expected:,} expected "
            f"({abs(diff):,} short, {pct_diff:.1f}% below). "
            f"Within ±{tolerance_pct}% tolerance. Will retry.",
            level=30
        )
        at_or_above = False
    else:
        dsf_logger.log_msg(
            f"  Result: BELOW TOLERANCE — got {actual:,} of {expected:,} expected "
            f"({abs(diff):,} short, {pct_diff:.1f}% below). "
            f"Floor is {int(low):,} (±{tolerance_pct}%). Will retry.",
            level=40
        )
        at_or_above = False

    dsf_logger.log_msg(sep, level=20)
    return at_or_above


def log_failure_analysis(
    seen_combinations: dict,
    filter_values: dict,
    filter_columns: List[str]
) -> None:
    """
    On retry exhaustion: logs which filter values were/weren't matched
    and what combinations were actually available in the Kafka time window.
    seen_combinations: {tuple_of_field_values_in_filter_columns_order: count}
    """
    sep = "=" * 70
    dsf_logger.log_msg(sep, level=40)
    dsf_logger.log_msg("FAILURE ANALYSIS — FILTER / COUNT MISMATCH DETAIL", level=40)

    dsf_logger.log_msg("Expected filter values (from metadata):", level=40)
    for field, expected in filter_values.items():
        dsf_logger.log_msg(f'  {field} = "{expected}"', level=40)

    if not seen_combinations:
        dsf_logger.log_msg(
            "No messages were found at all in the Kafka time window. "
            "Check that the time window parameters (START_TS, STOP_TS, offsets) are correct.",
            level=40
        )
        dsf_logger.log_msg(sep, level=40)
        return

    total_seen = sum(seen_combinations.values())
    dsf_logger.log_msg(f"Total messages seen in Kafka time window: {total_seen}", level=40)
    dsf_logger.log_msg(f"Distinct filter-key combinations found  : {len(seen_combinations)}", level=40)

    # Per-field analysis: which values were seen for each filter field
    field_values_seen: Dict[str, set] = {col: set() for col in filter_columns}
    for combo_tuple in seen_combinations:
        for i, col in enumerate(filter_columns):
            field_values_seen[col].add(combo_tuple[i])

    dsf_logger.log_msg("Per-filter field analysis:", level=40)
    for col in filter_columns:
        expected_val = filter_values.get(col, "N/A")
        seen_vals = sorted([str(v) for v in field_values_seen[col]])
        if str(expected_val) in seen_vals:
            dsf_logger.log_msg(
                f'  {col} = "{expected_val}" — MATCHED (value present in Kafka data)',
                level=40
            )
        else:
            dsf_logger.log_msg(
                f'  {col} = "{expected_val}" — NOT MATCHED. '
                f'Values found in data: {seen_vals}',
                level=40
            )

    date_idx    = filter_columns.index("businessDate") if "businessDate" in filter_columns else None
    mandate_idx = filter_columns.index("mandatorCode") if "mandatorCode" in filter_columns else None

    # Section 1: mandates available per business date
    if date_idx is not None and mandate_idx is not None:
        date_to_mandates: Dict[str, set] = {}
        for combo_tuple in seen_combinations:
            date_to_mandates.setdefault(combo_tuple[date_idx], set()).add(combo_tuple[mandate_idx])

        expected_date = filter_values.get("businessDate")
        dsf_logger.log_msg("Mandates by business date (all data in Kafka window):", level=40)
        for d in sorted(date_to_mandates):
            dsf_logger.log_msg(
                f"  {d} : [{', '.join(sorted(date_to_mandates[d]))}]", level=40
            )
        if expected_date and expected_date not in date_to_mandates:
            dsf_logger.log_msg(
                f"  (Expected date {expected_date} — no data found for this date at all)",
                level=40
            )

    # Section 2: for the expected mandate, what dates have data?
    if mandate_idx is not None and date_idx is not None:
        expected_mandate = filter_values.get("mandatorCode")
        expected_date    = filter_values.get("businessDate")
        if expected_mandate:
            mandate_date_counts: Dict[str, int] = {}
            for combo_tuple, cnt in seen_combinations.items():
                if combo_tuple[mandate_idx] == expected_mandate:
                    d = combo_tuple[date_idx]
                    mandate_date_counts[d] = mandate_date_counts.get(d, 0) + cnt

            dsf_logger.log_msg(
                f"Dates with data for expected mandate {expected_mandate}:", level=40
            )
            if mandate_date_counts:
                for d in sorted(mandate_date_counts):
                    dsf_logger.log_msg(
                        f"  {d} : {mandate_date_counts[d]:,} message(s)", level=40
                    )
                if expected_date and expected_date not in mandate_date_counts:
                    dsf_logger.log_msg(
                        f"  (Expected date {expected_date} — NOT present for this mandate)",
                        level=40
                    )
            else:
                dsf_logger.log_msg(
                    f"  (none — mandate {expected_mandate} not found in Kafka window at all)",
                    level=40
                )

    # Section 3: for the expected date, what mandates have data?
    if date_idx is not None and mandate_idx is not None:
        expected_date    = filter_values.get("businessDate")
        expected_mandate = filter_values.get("mandatorCode")
        if expected_date:
            date_mandate_counts: Dict[str, int] = {}
            for combo_tuple, cnt in seen_combinations.items():
                if combo_tuple[date_idx] == expected_date:
                    m = combo_tuple[mandate_idx]
                    date_mandate_counts[m] = date_mandate_counts.get(m, 0) + cnt

            dsf_logger.log_msg(
                f"Mandates with data on expected date {expected_date}:", level=40
            )
            if date_mandate_counts:
                for m in sorted(date_mandate_counts):
                    dsf_logger.log_msg(
                        f"  {m} : {date_mandate_counts[m]:,} message(s)", level=40
                    )
                if expected_mandate and expected_mandate not in date_mandate_counts:
                    dsf_logger.log_msg(
                        f"  (Expected mandate {expected_mandate} — NOT among them)",
                        level=40
                    )
            else:
                dsf_logger.log_msg(
                    f"  (none — no messages found for this date in the Kafka window)",
                    level=40
                )

    # Section 4: one line per combination, key=value format
    dsf_logger.log_msg("Available combinations in Kafka window:", level=40)
    for idx, (combo_tuple, count) in enumerate(
        sorted(seen_combinations.items(), key=lambda x: x[0]), start=1
    ):
        parts = [f"{col}={combo_tuple[i]}" for i, col in enumerate(filter_columns)]
        parts.append(f"count={count:,}")
        dsf_logger.log_msg(f"  [{idx}] {', '.join(parts)}", level=40)

    dsf_logger.log_msg(sep, level=40)


def write_get_kafka_validation_log(
    log_file: str,
    username: str,
    business_date: str,
    mandator: str,
    feed_name: str,
    reconciliation_group_id: str,
    expected_count: int,
    actual_count: int,
    tolerance_pct: float,
    separator: str = "|",
    status: str = "SUCCESS",
    failure_reason: str = "",
) -> None:
    """Append one row to the get_kafka validation log (success or failure)."""
    low = expected_count - expected_count * tolerance_pct / 100.0
    fields = {
        "export_datetime":          datetime.now().strftime("%Y-%m-%dT%H:%M:%S"),
        "username":                 username,
        "business_date":            business_date,
        "mandator":                 mandator,
        "feed_name":                feed_name,
        "reconciliation_group_id":  reconciliation_group_id,
        "expected_count":           str(expected_count),
        "actual_count":             str(actual_count),
        "tolerance_pct":            str(tolerance_pct),
        "tolerance_lower":          str(int(low)),
        "tolerance_upper":          "unlimited",
        "status":                   status,
        "failure_reason":           failure_reason,
    }
    header_row = separator.join(fields.keys())
    data_row   = separator.join(fields.values())
    write_header = not os.path.exists(log_file)
    with open(log_file, 'a', encoding='UTF-8') as f:
        if write_header:
            f.write(header_row + "\n")
        f.write(data_row + "\n")
    dsf_logger.log_msg(f"Get-Kafka validation log appended: {log_file}", level=20)


########### Main functon starts ############

# read in AIX and DSF vars

sda = os.environ["SDA"]
SDA = sda.upper()
SDA_HOME = os.environ["SDA_HOME"]
ASOF_DT = os.environ[f"{SDA}_ASOF_DT"]
COR_SEQ_NR = int(os.environ[f"{SDA}_ITERATION_NR"])
CFG_CTL = "MAIN"
CFG_DIR = os.environ["CFG_PROC_PATH"]
DATE_FORMAT = "%Y-%m-%d"
DATA_PATH = os.environ["PC_LOD_PROC_PATH"]
DOC_DIR = CFG_DIR.replace("/cfg", "/doc")
DOMAIN = os.environ["DSF_MAIN_ID"]
KEEP_TGT = 'YES'
MAIN = os.environ["DSF_MAIN_ID"]
DSF_CHARMAP = os.environ["DSF_CHARMAP"]
DSF_MANDATOR = os.environ["DSF_MANDATOR"]
FEED_NAME = "CPSB4QST"
KNW_FRO_TS = os.environ[f"{SDA}_KNW_FRO_TS"]
LOC_TZ = os.environ["DSF_JAVA_TZ"]
SDA_USR_DEF_VAl = os.environ[f"{SDA}_USR_VAL"]
username = os.getenv('USER', 'unknown')

if (SDA_USR_DEF_VAl == "1"):
    CONSUMED_COMMITTED = "Yes"
else:
    CONSUMED_COMMITTED = "No"

# set initial state and constants
DATA_CONSUME=0
DATETIME_FORMAT = "%Y-%m-%d-%H.%M.%S"


# load domain and mandator specific config file - may need to change SSL file as well

if (CFG_CTL == "MAIN"):
    DCFG = MAIN
else:
    DCFG = DOMAIN

CONFIG: dict = {}
CONFIG.update(
    path_to_dict(
        f"{CFG_DIR}/{DCFG}_{FEED_NAME}_config.json"
    )
)

# Keep Option

if (KEEP_TGT == "YES"):
    FMOVE = 0
else:
    FMOVE = 1

CONFIG: dict = {}
CONFIG.update(
    path_to_dict(
        f"{CFG_DIR}/{DCFG}_{FEED_NAME}_config.json"
    )
)

# Assign vars from config file
VALIDATE_TOPIC_MANDATOR = str(CONFIG.get("VALIDATE_TOPIC_MANDATOR", "YES")).upper()
MANDATOR_EKY = CONFIG.get("STREAMING_MANDATOR_KEY")
SSL_DIR = "/etc/ssl/"+str(CONFIG["KAFKA_USER"][DSF_MANDATOR])

# Kafka Variables
INFLOW_TOPIC = str(CONFIG["STREAMING_KAFKA_INFLOW_TOPIC"][DSF_MANDATOR])
KAFKA_BROKER = str(CONFIG["STREAMING_KAFKA_BROKER"][DSF_MANDATOR])

# Data format and selection
INPUT_FORMAT = CONFIG["INPUT_DATA"]
OUTPUT_FORMAT = CONFIG["OUTPUT_DATA"]
SEPERATOR = CONFIG["SEPERATOR"]

# AVRRO Variables
AVRO_COLUMNS = CONFIG["AVRO_COLUMNS"].split(",")
AVRO_FILE = CONFIG.get("AVRO_SCHEMA_FILE", CONFIG.get("AVRO_SHCEMA_FILE"))
AVRO_SCHEMA = f"{CFG_DIR}/{AVRO_FILE}"
AVRO_REG_ADDRS = CONFIG.get("AVRO_SCHEMA_REGISTRY")
DECIMAL_CONV = CONFIG.get("DECIMAL_CONV")
DECIMAL_SCALE: int = int(CONFIG.get("DECIMAL_SCALE"))
oschema_id = ""

# Time related
TIME_WINDOW = CONFIG.get("LOCATION_TIME_WINDOW", {} ).get(DSF_MANDATOR, {})
if TIME_WINDOW:
    dsf_logger.log_msg(f"Location specific time window found for mandator {DSF_MANDATOR}: {TIME_WINDOW}. Using location values", level=20)
    START_TS = TIME_WINDOW.get("START_TS")
    START_DT_OFFSET : int = int(TIME_WINDOW.get("START_DT_OFFSET"))
    STOP_TS = TIME_WINDOW.get("STOP_TS")
    STOP_DT_OFFSET: int = int(TIME_WINDOW.get("STOP_DT_OFFSET"))
else:
    dsf_logger.log_msg(f"No location specific time window found for mandator {DSF_MANDATOR}. Using default values", level=20)
    START_TS = CONFIG.get("START_TS")
    START_DT_OFFSET : int = int(CONFIG.get("START_DT_OFFSET"))
    STOP_TS = CONFIG.get("STOP_TS")
    STOP_DT_OFFSET: int = int(CONFIG.get("STOP_DT_OFFSET"))

TIMESTAMP_KEY = CONFIG.get("STREAMING_TIMESTAMP_KEY")
TIMESTAMP_FORMAT = CONFIG.get("STREAMING_TIMESTAMP_FORMAT")
VERBROSE: bool = eval(CONFIG.get("STREAMING_VERBOSE"))
STORE_MIDLAYER: bool = eval(CONFIG.get("STREAMING_STORE_MIDLAYER"))
WAIT_UNTIL_DONE_HOURS: int = int(CONFIG.get("STREAMING_WAIT_UNTIL_DONE_HOURS"))
IDLE_TIMEOUT_MINUTES: int = int(CONFIG.get("STREAMING_IDLE_TIMEOUT_MINUTES"))

# Performance and Control
COMMIT_CNT = int(CONFIG.get("COMMIT_CNT"))
WAIT_FOR_SUBMIT = str(CONFIG.get("WAIT_FOR_SUBMIT", "YES")).upper()
if WAIT_FOR_SUBMIT not in ["YES", "NO"]:
    dsf_logger.log_msg(f"WAIT_FOR_SUBMIT must be YES or NO", level=40)
    os._exit(9)

ALLOW_NO_DATA = CONFIG["ALLOW_NO_DATA"]

# ---------------------------------------------------------------------------
# Block A — Metadata-filtering config vars (loaded from config with defaults)
# ---------------------------------------------------------------------------

STATUS_MESSAGES_FEED_NAME = str(CONFIG.get("STATUS_MESSAGES_FEED_NAME", "CPSB4Q00"))
METADATA_FILE_SUFFIX: str = str(CONFIG.get("METADATA_FILE_SUFFIX", "_metadata.txt"))
METADATA_FILTER_COLUMNS: List[str] = list(CONFIG.get("METADATA_FILTER_COLUMNS", []))
METADATA_FILTER_FIELD_MAP: dict = dict(CONFIG.get("METADATA_FILTER_FIELD_MAP", {}))
METADATA_COUNT_FIELD: str = str(CONFIG.get("METADATA_COUNT_FIELD", "total_messages_published"))
METADATA_COUNT_TOLERANCE_PCT: float = float(CONFIG.get("METADATA_COUNT_TOLERANCE_PCT", 10))
_location_tol = CONFIG.get("LOCATION_TOLERANCE_PCT", {}).get(DSF_MANDATOR)
if _location_tol is not None:
    METADATA_COUNT_TOLERANCE_PCT = float(_location_tol)
MAX_LISTEN_DURATION_HOURS: float = float(CONFIG.get("MAX_LISTEN_DURATION_HOURS", 2))
MAX_LISTEN_DURATION_MINUTES: int = int(MAX_LISTEN_DURATION_HOURS * 60)
RETRY_WAIT_SECONDS: int = int(CONFIG.get("RETRY_WAIT_SECONDS", 300))
STABLE_COUNT_REQUIRED_ATTEMPTS: int = int(CONFIG.get("STABLE_COUNT_REQUIRED_ATTEMPTS", 2))
OVER_COUNT_BEHAVIOR: str = str(CONFIG.get("OVER_COUNT_BEHAVIOR", "STABILITY")).upper()
if OVER_COUNT_BEHAVIOR not in ("STABILITY", "WAIT"):
    dsf_logger.log_msg(f"Invalid OVER_COUNT_BEHAVIOR '{OVER_COUNT_BEHAVIOR}' — must be STABILITY or WAIT. Defaulting to STABILITY.", level=30)
    OVER_COUNT_BEHAVIOR = "STABILITY"
OVER_COUNT_WAIT_MINUTES: int = int(CONFIG.get("OVER_COUNT_WAIT_MINUTES", 5))
PRE_FILTER_VALUES: dict = load_and_validate_filter_config("PRE_FILTER_VALUES", CONFIG)
VALIDATION_FILTER_VALUES: dict = load_and_validate_filter_config("VALIDATION_FILTER_VALUES", CONFIG)

for _filter_name, _filter_dict in (
    ("PRE_FILTER_VALUES", PRE_FILTER_VALUES),
    ("VALIDATION_FILTER_VALUES", VALIDATION_FILTER_VALUES),
):
    for _field, _expected in _filter_dict.items():
        _has_blank = (_expected == "") or (
            isinstance(_expected, list) and any(_e == "" for _e in _expected)
        )
        if _has_blank:
            dsf_logger.log_msg(
                f"WARNING: {_filter_name} field '{_field}' has an empty-string value. "
                f"This does NOT disable filtering on that field — it means only messages "
                f"where '{_field}' is itself an empty string will match; messages with a "
                f"real value will be excluded. To disable filtering on this field, remove "
                f"the key from {_filter_name} entirely (or set {_filter_name} to {{}}).",
                level=30
            )

dsf_logger.log_msg(f"STATUS_MESSAGES_FEED_NAME          : {STATUS_MESSAGES_FEED_NAME}", level=20)
dsf_logger.log_msg(f"METADATA_FILE_SUFFIX               : {METADATA_FILE_SUFFIX}", level=20)
dsf_logger.log_msg(f"METADATA_FILTER_COLUMNS            : {METADATA_FILTER_COLUMNS}", level=20)
dsf_logger.log_msg(f"METADATA_FILTER_FIELD_MAP          : {METADATA_FILTER_FIELD_MAP}", level=20)
dsf_logger.log_msg(f"PRE_FILTER_VALUES                  : {PRE_FILTER_VALUES}", level=20)
dsf_logger.log_msg(f"VALIDATION_FILTER_VALUES           : {VALIDATION_FILTER_VALUES}", level=20)
if VALIDATION_FILTER_VALUES and VALIDATION_FILTER_VALUES != PRE_FILTER_VALUES:
    dsf_logger.log_msg(
        "NOTE: VALIDATION_FILTER_VALUES is set and differs from PRE_FILTER_VALUES. "
        "All messages consumed from the topic will be written to the output file. "
        "Only messages matching VALIDATION_FILTER_VALUES count toward EXPECTED_COUNT. "
        "Expect the output file's row count to exceed the validated count — see the "
        "'Output summary' logged at acceptance for the exact numbers.",
        level=30
    )
dsf_logger.log_msg(f"METADATA_COUNT_FIELD               : {METADATA_COUNT_FIELD}", level=20)
if _location_tol is not None:
    dsf_logger.log_msg(
        f"METADATA_COUNT_TOLERANCE_PCT       : {METADATA_COUNT_TOLERANCE_PCT}%  "
        f"(location-specific for mandator {DSF_MANDATOR})", level=20
    )
else:
    dsf_logger.log_msg(
        f"METADATA_COUNT_TOLERANCE_PCT       : {METADATA_COUNT_TOLERANCE_PCT}%  "
        f"(default — mandator {DSF_MANDATOR} not in LOCATION_TOLERANCE_PCT)", level=20
    )
dsf_logger.log_msg(f"MAX_LISTEN_DURATION_HOURS          : {MAX_LISTEN_DURATION_HOURS}", level=20)
dsf_logger.log_msg(f"RETRY_WAIT_SECONDS                 : {RETRY_WAIT_SECONDS}", level=20)
dsf_logger.log_msg(f"STABLE_COUNT_REQUIRED_ATTEMPTS     : {STABLE_COUNT_REQUIRED_ATTEMPTS}", level=20)
if OVER_COUNT_BEHAVIOR == "STABILITY":
    dsf_logger.log_msg(
        f"OVER_COUNT_BEHAVIOR                : STABILITY "
        f"(wait for {STABLE_COUNT_REQUIRED_ATTEMPTS} consecutive stable counts before accepting). "
        f"To use a fixed wait window instead: set OVER_COUNT_BEHAVIOR=WAIT and OVER_COUNT_WAIT_MINUTES=<n>",
        level=20
    )
else:
    dsf_logger.log_msg(
        f"OVER_COUNT_BEHAVIOR                : WAIT "
        f"({OVER_COUNT_WAIT_MINUTES}-min fixed window once count exceeds expected). "
        f"To use stability streak instead: set OVER_COUNT_BEHAVIOR=STABILITY and STABLE_COUNT_REQUIRED_ATTEMPTS=<n>",
        level=20
    )

if METADATA_FILTER_COLUMNS and INPUT_FORMAT != "AVRO":
    dsf_logger.log_msg(
        f"WARNING: METADATA_FILTER_COLUMNS is set but INPUT_FORMAT is '{INPUT_FORMAT}'. "
        f"Metadata filtering is only applied when INPUT_FORMAT=AVRO. "
        f"All messages will pass through unfiltered.",
        level=30
    )

if VALIDATION_FILTER_VALUES and INPUT_FORMAT != "AVRO":
    dsf_logger.log_msg(
        f"WARNING: VALIDATION_FILTER_VALUES is set but INPUT_FORMAT is '{INPUT_FORMAT}'. "
        f"Validation filtering is only applied when INPUT_FORMAT=AVRO. "
        f"All written messages will count toward validation.",
        level=30
    )

# General Log
dsf_logger.log_starting_process(__file__, level=20)
dsf_logger.log_msg(f"INFLOW_TOPIC: {INFLOW_TOPIC}", level=20)
dsf_logger.log_msg(f"KAFKA_BROKER: {KAFKA_BROKER}", level=20)
dsf_logger.log_msg(f"INFO: SSL_DIR {SSL_DIR}", level=20)


# SSL Config

SSL_CA_FILE = f"{SSL_DIR}/ca_root.pem"
SSL_CLIENT_CERT = f"{SSL_DIR}/certificate.pem"
SSL_KEY = f"{SSL_DIR}/cert_key.pem"


# Stop timestamp option, adjust to current if specified

if (STOP_TS == "CURRENT"):
    STOP_TS = KNW_FRO_TS
    if (STOP_DT_OFFSET !=0):
        dsf_logger.log_msg(f"STOP_DT_OFFSET has to be zero if STOP_TS set to CURRENT_TS!!", level=40)
        os._exit(9)

    STOP_TS: datetime = datetime.strptime(KNW_FRO_TS, DATETIME_FORMAT)
    pst = pytz.timezone(LOC_TZ)
    STOP_TS = pst.localize(STOP_TS)
    DT_UTC_END = STOP_TS.astimezone(pytz.UTC)
    TS_UTC_END: float = datetime.timestamp(DT_UTC_END)

    dsf_logger.log_msg(f"KNW_FRO_TS changed as follows: DT_UTC_END: {DT_UTC_END}: TS_UTC_END: {TS_UTC_END}", level=20)


# SET timestamp boundaries for stadn and end

# both start adn end dates orient around ASOF_DT alone
DT_UTC: datetime.date = datetime.strptime(ASOF_DT, DATE_FORMAT).date()

# start offset calc on timestamp
START_TS = datetime.strptime(START_TS, "%H:%M:%S").time()
START_TS = datetime.combine(DT_UTC, START_TS)


DT_UTC_START = START_TS - timedelta(days = START_DT_OFFSET)
TS_UTC_START = datetime.timestamp(DT_UTC_START)

dsf_logger.log_msg(f"DT_UTC_START: {DT_UTC_START}: TS_UTC_START: {TS_UTC_START}", level=20)

# ENd offset calc on timestamp

try: TS_UTC_END
except NameError:
    STOP_TS: datetime.time = datetime.strptime(STOP_TS, "%H:%M:%S").time()
    STOP_TS = datetime.combine(DT_UTC, STOP_TS)

    DT_UTC_END: datetime = STOP_TS - timedelta(days = STOP_DT_OFFSET)
    TS_UTC_END: float = datetime.timestamp(DT_UTC_END)

dsf_logger.log_msg(f"DT_UTC_END: {DT_UTC_END}: TS_UTC_END: {TS_UTC_END}", level=20)

# Sanity Check

if (TS_UTC_START - TS_UTC_END >=0):
    dsf_logger.log_msg(f"TS_UTC_START {TS_UTC_START} is equal to or greater than TS_UTC_END {TS_UTC_END}", level=40)
    os._exit(9)


# Prepare directory and files for data

DATA_FOLDER = (
    f"{DATA_PATH}/{FEED_NAME}_{DT_UTC.strftime(DATE_FORMAT)}/"
)

dsf_logger.log_msg(f"DT_UTC: {DT_UTC.strftime(DATE_FORMAT)}", level = 20)
dsf_logger.log_msg(f"DATA_FOLDER: {DATA_FOLDER}", level= 20)


DATA_FILE = f"{DATA_FOLDER}{FEED_NAME}.par"
dsf_logger.log_msg(f"DATA_FILE: {DATA_FILE}", level=20)
pathlib.Path(DATA_FOLDER).mkdir(mode=0o755, parents=True, exist_ok=True)
APP_ID = FEED_NAME
LOG_FILE_NAME = f"{DATA_FILE}_log.json"
dsf_logger.log_msg(f"LOG_FILE_NAME: {LOG_FILE_NAME}", level=20)

# ---------------------------------------------------------------------------
# Block A (continued) — Metadata file path + temp file path
# Both depend on DATA_FOLDER which is now defined above.
# ---------------------------------------------------------------------------

METADATA_FILE_PATH = os.path.join(
    DATA_PATH,
    f"{STATUS_MESSAGES_FEED_NAME}_{ASOF_DT}",
    f"{STATUS_MESSAGES_FEED_NAME}_{ASOF_DT}{METADATA_FILE_SUFFIX}"
)
TEMP_DATA_FILE = DATA_FILE + ".tmp"
GET_KAFKA_VALIDATION_LOG = os.path.join(DATA_PATH, f"{FEED_NAME}_get_kafka_validation_log.txt")

dsf_logger.log_msg(f"METADATA_FILE_PATH (looking here for metadata)    : {METADATA_FILE_PATH}", level=20)
dsf_logger.log_msg(f"TEMP_DATA_FILE (intermediate write location)       : {TEMP_DATA_FILE}", level=20)
dsf_logger.log_msg(f"GET_KAFKA_VALIDATION_LOG (append-only success log) : {GET_KAFKA_VALIDATION_LOG}", level=20)

# ---------------------------------------------------------------------------
# Block C — Load and validate metadata file. Hard exit if anything is wrong.
# ---------------------------------------------------------------------------

metadata = load_metadata_file(METADATA_FILE_PATH, SEPERATOR)

# Build filter_values in METADATA_FILTER_COLUMNS order so it matches combo tuples
filter_values: Dict[str, str] = {}
for _col in METADATA_FILTER_COLUMNS:
    _meta_field = None
    for _mf, _kf in METADATA_FILTER_FIELD_MAP.items():
        if _kf == _col:
            _meta_field = _mf
            break
    if _meta_field is None:
        dsf_logger.log_msg(
            f"Filter column '{_col}' listed in METADATA_FILTER_COLUMNS has no entry in "
            f"METADATA_FILTER_FIELD_MAP. Cannot build filter. "
            f"Add a mapping '{_col}' -> '<metadata_field_name>' to METADATA_FILTER_FIELD_MAP.",
            level=40
        )
        os._exit(1)
    if _meta_field not in metadata:
        dsf_logger.log_msg(
            f"Metadata field '{_meta_field}' (mapped from kafka field '{_col}') "
            f"not found in metadata file. "
            f"Available fields: {list(metadata.keys())}",
            level=40
        )
        os._exit(1)
    filter_values[_col] = metadata[_meta_field]
    dsf_logger.log_msg(
        f"Filter built: kafka_field='{_col}' expected_value='{metadata[_meta_field]}' "
        f"(sourced from metadata field '{_meta_field}')",
        level=20
    )

# Extract expected count
if METADATA_COUNT_FIELD not in metadata:
    dsf_logger.log_msg(
        f"Count field '{METADATA_COUNT_FIELD}' not found in metadata file. "
        f"Available fields: {list(metadata.keys())}",
        level=40
    )
    os._exit(1)

try:
    EXPECTED_COUNT: int = int(metadata[METADATA_COUNT_FIELD])
except ValueError:
    dsf_logger.log_msg(
        f"Count field '{METADATA_COUNT_FIELD}' value '{metadata[METADATA_COUNT_FIELD]}' "
        f"is not a valid integer.",
        level=40
    )
    os._exit(1)

dsf_logger.log_msg(
    f"Expected message count from metadata: {EXPECTED_COUNT} "
    f"(field '{METADATA_COUNT_FIELD}' in {METADATA_FILE_PATH})",
    level=20
)
dsf_logger.log_msg(f"Active filters ({len(filter_values)}): {filter_values}", level=20)
dsf_logger.log_msg(
    f"Retry policy: max {MAX_LISTEN_DURATION_HOURS}h, "
    f"interval {RETRY_WAIT_SECONDS}s, "
    f"tolerance ±{METADATA_COUNT_TOLERANCE_PCT}%",
    level=20
)


# Check if partial file for this ASOF_DT exists

restart = "no"
CHK_DATA_FILE = pathlib.Path(DATA_FILE)
FIRST_DATA_FILE = DATA_FILE + ".1"
if (CHK_DATA_FILE.is_file()):
    os.rename(CHK_DATA_FILE, FIRST_DATA_FILE)
    restart = "YES"
    dsf_logger.log_msg(f"First run for this date detected, data in {FIRST_DATA_FILE}", level=20)


# Adjust Kafka & urllib logging system(acquired while importing)

for key, value in logging.root.manager.loggerDict.items():
    if ('kafka' in key) or ('urllib3' in key) or ('charset_normalizer' in key):
        logging.getLogger(key).setLevel(logging.ERROR)


# Instantiate consumer and


try:
    consumer: kafka.KafkaConsumer = kafka.KafkaConsumer(
        auto_offset_reset = 'latest'
        ,bootstrap_servers=KAFKA_BROKER
        ,security_protocol="PLAINTEXT"
        ,client_id=APP_ID
        ,group_id=APP_ID
        ,enable_auto_commit=False
        ,fetch_max_wait_ms=2000
        ,request_timeout_ms=3600000
        ,retry_backoff_ms=1000
        ,metric_num_samples=2
        ,metrics_sample_window_ms=30000
    )

except Exception as err:
    dsf_logger.log_msg(f"{SSL_CA_FILE} exists: {os.access(SSL_CA_FILE, os.F_OK)}", level = 20)
    dsf_logger.log_msg(f"{SSL_CA_FILE} can read: {os.access(SSL_CA_FILE, os.R_OK)}", level = 20)
    dsf_logger.log_msg(f"{SSL_CLIENT_CERT} exists: {os.access(SSL_CLIENT_CERT, os.F_OK)}", level = 20)
    dsf_logger.log_msg(f"{SSL_CLIENT_CERT} can read: {os.access(SSL_CLIENT_CERT, os.R_OK)}", level = 20)
    dsf_logger.log_msg(f"{SSL_KEY} exists: {os.access(SSL_KEY, os.F_OK)}", level = 20)
    dsf_logger.log_msg(f"{SSL_KEY} can read: {os.access(SSL_KEY, os.R_OK)}", level = 20)
    dsf_logger.log_error_msg(msg=f"Unexpected error at kafak.KafkaConsumer", err=err)


if VALIDATE_TOPIC_MANDATOR == "YES":
    # check topic
    if not INFLOW_TOPIC.endswith(DSF_MANDATOR):
        dsf_logger.log_msg(f"INFLOW_TOPIC {INFLOW_TOPIC} does not end with mandator {DSF_MANDATOR}", level=40)
        os._exit(9)
    else:
        dsf_logger.log_msg(f"INFLOW_TOPIC {INFLOW_TOPIC} validation passed for mandator {DSF_MANDATOR}", level=20)

topic = INFLOW_TOPIC

if consumer.bootstrap_connected():
    dsf_logger.log_msg(f"Connection sucessful", level=20)
else:
    dsf_logger.log_msg(f"Connection unsucessful", level=20)
    os._exit(9)


# Get set of Partitions for this topic
try:
    dsf_logger.log_msg(f"checking partitions for topic {INFLOW_TOPIC}", level=20)
    partition_indexes: Set[int] = assert_set(
        consumer.partitions_for_topic(topic)
    )
except Exception as err:
    dsf_logger.log_msg(f"{err} when checking partitions for topic {INFLOW_TOPIC}, possible KAFKA server side problem!!", level=40)
    os._exit(9)


partition_request: List[kafka.TopicPartition] = [
    kafka.TopicPartition(topic, index) for index in partition_indexes
]

dsf_logger.log_msg(f"partition_request ({type(partition_request)}): {partition_request}", level=20)

print

consumer.assign(partition_request)
partitions: Set[kafka.TopicPartition] = assert_set(consumer.assignment())
dsf_logger.log_msg(f"partitions ({type(partitions)}): {partitions}", level=20)
dsf_logger.log_msg(f"partitions ({type(partitions)}): {partitions}", level=20)

partition_stop_ts: Dict[kafka.TopicPartition, int] = {
    part: int(TS_UTC_END * 1000) for part in partitions
}

all_stop_offset_ts: Dict[
    kafka.TopicPartition, Tuple[int, int]
] = assert_dict(consumer.offsets_for_times(partition_stop_ts))

dsf_logger.log_msg(
    f"all_stop_offset_ts: {all_stop_offset_ts}", level=20
)

partition_start_ts: Dict[kafka.TopicPartition, int] = {
    part: int(TS_UTC_START * 1000) for part in partitions
}

dsf_logger.log_msg(f"partition_start_ts: {list(partition_start_ts.items())}", level=20)


# consumer.offsets_for_times takes a dict of partition:timestamps
# the returned offset for each partition is the earlier offset
# whose timestamoo is greater than or equal to the given timestamp in the corresponding partition

all_start_offset_ts: Dict[
    kafka.TopicPartition, Tuple[int, int]
] = assert_dict(consumer.offsets_for_times(partition_start_ts))

dsf_logger.log_msg(
    f"all_start_offset_ts: {all_start_offset_ts}", level=20
)



################################################################################################################################################################
################################################################################################################################################################

# fastavro read in schema as json from file if no registry, also get previous version

if (INPUT_FORMAT == "AVRO" and AVRO_FILE == "LATEST"):
    try:
        dsf_logger.log_msg(f"INFO: Get registry for {topic}", level=20)
        lversion, lschema_id, lavschema = get_avro_schema(
            topic
            ,AVRO_REG_ADDRS
            ,SSL_CA_FILE
            ,SSL_CLIENT_CERT
            ,SSL_KEY
            ,'latest'
        )

        # get previous version
        pversion = lversion - 1

        if (pversion>0):
            try:
                pversion, pschema_id, pavschema = get_avro_schema(
                    topic
                    ,AVRO_REG_ADDRS
                    ,SSL_CA_FILE
                    ,SSL_CLIENT_CERT
                    ,SSL_KEY
                    ,pversion
                )
            except:
                dsf_logger.log_msg(
                    f"INFO: Setting previous schema to latest as previous not found for {topic}",
                    level=20
                )
                pversion = lversion
                pschema_id = lschema_id
                pavschema = lavschema

        else:
            dsf_logger.log_msg(
                    f"INFO: Setting previous schema to latest only 1 version {topic}",
                    level=20
                )
            pversion = lversion
            pschema_id = lschema_id
            pavschema = lavschema

        # default is to use latest
        avschema = lavschema
        avflag = 0

        dsf_logger.log_msg(f"schema version {pavschema}", level=20)

    except Exception as e:
        dsf_logger.log_msg(f"{e}", level=40)
        os._exit(1)

elif (INPUT_FORMAT == "AVRO"):
    try:
        with open(AVRO_SCHEMA, 'r') as schema_file:
            jschema = json.loads(schema_file.read())
            avschema = fastavro.schema.parse_schema(jschema)
    except:
        dsf_logger.log_msg(f"schema file {AVRO_SCHEMA} is not valid", level=40)
        os._exit(1)

# AVRO check for fields containing decimal, will be converted to default later if dec default defined

if (INPUT_FORMAT == "AVRO" and DECIMAL_CONV == "YES"):
    if (AVRO_FILE == "LATEST"):
        ldecattrs = get_deicmal_feilds(lavschema)
        pdecattrs = get_deicmal_feilds(pavschema)

        dsf_logger.log_msg(f"ldecattrs is {ldecattrs}", level=20)
        dsf_logger.log_msg(f"pdecattrs is {pdecattrs}", level=20)
    else:
        ldecattrs = get_deicmal_feilds(lavschema)
        pdecattrs = ldecattrs


# ---------------------------------------------------------------------------
# Block D + E — Temp-file guarded retry loop.
# Collects filtered messages to TEMP_DATA_FILE, validates count against
# metadata, then renames to DATA_FILE and commits offsets on success.
# On any exception the temp file is removed before propagating.
# ---------------------------------------------------------------------------

retry_deadline   = datetime.now() + timedelta(hours=MAX_LISTEN_DURATION_HOURS)
RETRY_TS_UTC_END = datetime.timestamp(retry_deadline)
attempt       = 0
stable_streak  = 0     # consecutive at-or-above attempts with the same validation_count
prev_count     = None  # validation_count from the previous attempt
grace_extended = False # True after one deadline extension granted near target
over_count_deadline: Optional[datetime] = None  # WAIT mode: set when count first exceeds expected

dsf_logger.log_msg(
    f"Kafka publication window (messages searched on topic) : "
    f"{DT_UTC_START.strftime('%Y-%m-%d %H:%M:%S')}  →  {DT_UTC_END.strftime('%Y-%m-%d %H:%M:%S')}",
    level=20
)
dsf_logger.log_msg(
    f"Retry deadline (script stops retrying after)          : "
    f"{retry_deadline.strftime('%Y-%m-%d %H:%M:%S')}  "
    f"({MAX_LISTEN_DURATION_HOURS}h / {MAX_LISTEN_DURATION_MINUTES} min from now)",
    level=20
)

try:
    while True:
        attempt += 1
        dsf_logger.log_msg(
            f"--- Collection attempt {attempt} started at "
            f"{datetime.now().strftime('%Y-%m-%d %H:%M:%S')} ---",
            level=20
        )
        dsf_logger.log_msg(
            f"Scanning Kafka window : "
            f"{DT_UTC_START.strftime('%Y-%m-%d %H:%M:%S')}  →  "
            f"{datetime.fromtimestamp(RETRY_TS_UTC_END).strftime('%Y-%m-%d %H:%M:%S')}  "
            f"| Retry deadline: {retry_deadline.strftime('%Y-%m-%d %H:%M:%S')}",
            level=20
        )

        # Remove temp file left by a previous attempt
        if os.path.exists(TEMP_DATA_FILE):
            try:
                os.remove(TEMP_DATA_FILE)
                dsf_logger.log_msg(
                    f"Removed temp file from previous attempt: {TEMP_DATA_FILE}", level=20
                )
            except Exception as _e:
                dsf_logger.log_msg(
                    f"Cannot remove temp file {TEMP_DATA_FILE}: {_e}. "
                    f"Cannot continue safely — appending to an existing temp file would corrupt the output. "
                    f"Manually remove the file and rerun.",
                    level=40
                )
                os._exit(1)

        filtered_count = 0
        validation_count = 0
        total_seen     = 0
        pre_filter_matched_count = 0
        # seen_combinations key = tuple of field values in METADATA_FILTER_COLUMNS order
        seen_combinations: Dict[tuple, int] = {}
        last_offsets: Dict[kafka.TopicPartition, Tuple[int, int]] = {}

        # Recompute stop offsets on every attempt using the extended end time so that
        # messages published after the original DT_UTC_END (e.g. delayed producers) are found.
        retry_partition_stop_ts = {part: int(RETRY_TS_UTC_END * 1000) for part in partitions}
        retry_all_stop_offset_ts = assert_dict(consumer.offsets_for_times(retry_partition_stop_ts))

        # Recompute start offsets for partitions still None from startup
        # (handles empty topic at job start — publisher may have been delayed)
        _none_parts = {p for p, v in all_start_offset_ts.items() if v is None}
        if _none_parts:
            _refreshed = assert_dict(consumer.offsets_for_times(
                {p: int(TS_UTC_START * 1000) for p in _none_parts}
            ))
            for _p, _v in _refreshed.items():
                if _v is not None:
                    all_start_offset_ts[_p] = _v
                    dsf_logger.log_msg(
                        f"Start offset for partition {_p.partition} found on retry attempt {attempt}: "
                        f"offset={_v[0]} ts={ts_to_str(_v[1]/1000)}",
                        level=20
                    )

        for partition, start_offset_ts in all_start_offset_ts.items():

            if not start_offset_ts:
                dsf_logger.log_msg(
                    f"No data on the Kafka Topic for partition {partition.partition}", level=30
                )
                continue

            # Reassign partition one at a time for reliable processing
            consumer.assign([])
            try:
                consumer.assign([kafka.TopicPartition(topic, partition.partition)])
                dsf_logger.log_msg(f"Assigned partition {partition.partition}", level=20)
            except Exception as e:
                dsf_logger.log_msg(
                    f"Assign partition {partition.partition} failed: {e} ({type(e)})", level=40
                )
                os._exit(1)

            dsf_logger.log_msg(f"Reassigned partition {partition.partition} successfully", level=20)

            (start_offset, start_ts) = assert_tuple(start_offset_ts)
            dsf_logger.log_msg(f"start_ts {start_ts} {TS_UTC_START}", level=20)

            if (start_ts >= TS_UTC_START * 1000) and (start_ts < RETRY_TS_UTC_END * 1000):
                dsf_logger.log_msg(f"start_offset {start_offset}, start_ts {start_ts}", level=20)
                dsf_logger.log_msg(f"partition: {partition.partition}", level=20)
                dsf_logger.log_msg(
                    f"first message found for start {start_offset}: {ts_to_str(start_ts/1000)}",
                    level=20
                )
            else:
                dsf_logger.log_msg(
                    f"Start timestamp not found in range, no valid data on the Kafka Topic "
                    f"for partition {partition.partition}",
                    level=30
                )
                continue

            # Determine end offset using the extended retry window end
            if retry_all_stop_offset_ts.get(partition):
                (end_offset, end_ts) = assert_tuple(retry_all_stop_offset_ts.get(partition))
                dsf_logger.log_msg(
                    f"Last candidate message for stop point: offset={end_offset} "
                    f"ts={ts_to_str(end_ts/1000)}",
                    level=20
                )
                end_offset -= 1
            else:
                end_offset = assert_int(
                    assert_dict(consumer.end_offsets(partitions)).get(partition)
                )
                end_offset -= 1
                dsf_logger.log_msg(
                    f"Last candidate message for stop point has offset {end_offset}",
                    level=20
                )

            # Always seek to start_offset on every attempt.
            # This ensures a full re-read of the time window each retry (no messages missed),
            # and preserves rerun behavior (same as CONSUMED_COMMITTED="Yes").
            seek_offset = start_offset
            dsf_logger.log_msg(
                f"Seeking partition {partition.partition} to start_offset={seek_offset} "
                f"(attempt {attempt})",
                level=20
            )

            try:
                consumer.seek(partition, seek_offset)
                dsf_logger.log_msg(f"Seek to offset {seek_offset} successful", level=20)
            except Exception as e:
                dsf_logger.log_msg(f"seek failed {e}({type(e)})", level=40)
                os._exit(1)

            dsf_logger.log_msg(
                f"Expected {end_offset - seek_offset + 1} messages in time window "
                f"for partition {partition.partition}",
                level=20
            )
            dsf_logger.log_msg(
                f"Current offset after seek: {consumer.position(partition)}",
                level=20
            )

            # Collect, filter, and write matching messages to temp file
            part_total    = 0
            part_filtered = 0
            part_last_offset = seek_offset - 1
            part_last_ts     = 0

            with open(TEMP_DATA_FILE, 'a', encoding=DSF_CHARMAP, errors='ignore') as f:
                dsf_logger.log_msg(
                    f"Writing filtered messages to temp file: {TEMP_DATA_FILE}", level=20
                )

                for msg in consumer:
                    # Guard against seeking issues
                    if msg.offset < start_offset:
                        continue

                    omsg_ts  = msg.timestamp
                    omsg_off = msg.offset
                    part_total += 1
                    total_seen += 1
                    part_last_offset = omsg_off
                    part_last_ts     = omsg_ts

                    # --- Avro deserialization → raw_msg_dict ---
                    raw_msg_dict = None
                    cur_decattrs: list = []

                    if INPUT_FORMAT == "AVRO":
                        msg_io = io.BytesIO(msg.value)

                        if AVRO_FILE == "LATEST":
                            msg_io.seek(1)
                            mschema_by = msg_io.read(4)
                            schema_id  = int.from_bytes(mschema_by, byteorder='big')
                            if schema_id != oschema_id:
                                dsf_logger.log_msg(f"Schema_id changed to {schema_id}", level=20)
                                oschema_id = schema_id

                            try:
                                if schema_id == lschema_id:
                                    raw_msg_dict = fastavro.schemaless_reader(msg_io, lavschema)
                                    if DECIMAL_CONV == "YES":
                                        cur_decattrs = ldecattrs
                                elif schema_id == pschema_id:
                                    raw_msg_dict = fastavro.schemaless_reader(msg_io, pavschema)
                                    if DECIMAL_CONV == "YES":
                                        cur_decattrs = pdecattrs
                                else:
                                    raise ValueError(
                                        f"No matching schema for schema_id={schema_id} "
                                        f"at offset {omsg_off}"
                                    )
                            except Exception as e:
                                dsf_logger.log_msg(
                                    f"Unable to match schema {schema_id} with data "
                                    f"for message offset {omsg_off}: {e}",
                                    level=40
                                )
                                os._exit(1)

                        else:
                            msg_io.seek(5)
                            try:
                                raw_msg_dict = fastavro.schemaless_reader(msg_io, avschema)
                                if DECIMAL_CONV == "YES":
                                    cur_decattrs = ldecattrs
                            except Exception as e:
                                dsf_logger.log_msg(
                                    f"Unable to match schema with data at offset {omsg_off}: {e}",
                                    level=40
                                )
                                os._exit(1)

                    # --- Apply pre-filter first (checked before metadata filter / seen_combinations) ---
                    if raw_msg_dict is not None and PRE_FILTER_VALUES:
                        if not message_matches_filters(raw_msg_dict, PRE_FILTER_VALUES):
                            if omsg_off >= end_offset:
                                break
                            continue
                        pre_filter_matched_count += 1

                    # --- Track seen combinations for failure analysis (before filtering) ---
                    if raw_msg_dict is not None and METADATA_FILTER_COLUMNS:
                        combo = tuple(
                            str(get_nested_value(raw_msg_dict, col))
                            if get_nested_value(raw_msg_dict, col) is not None
                            else "N/A"
                            for col in METADATA_FILTER_COLUMNS
                        )
                        seen_combinations[combo] = seen_combinations.get(combo, 0) + 1

                    # --- Apply metadata filter ---
                    if raw_msg_dict is not None and filter_values and INPUT_FORMAT == "AVRO":
                        if not message_matches_filters(raw_msg_dict, filter_values):
                            if omsg_off >= end_offset:
                                break
                            continue

                    part_filtered += 1
                    filtered_count += 1

                    # --- Track validation-count subset (defaults to counting everything written) ---
                    if VALIDATION_FILTER_VALUES and raw_msg_dict is not None:
                        if message_matches_filters(raw_msg_dict, VALIDATION_FILTER_VALUES):
                            validation_count += 1
                    else:
                        validation_count += 1

                    # --- Apply decimal conversion on filtered message ---
                    msg_out = raw_msg_dict if raw_msg_dict is not None else {}
                    if INPUT_FORMAT == "AVRO" and DECIMAL_CONV == "YES" and cur_decattrs:
                        for decf in cur_decattrs:
                            if isinstance(msg_out, dict) and msg_out.get(decf) is not None:
                                msg_out[decf] = f"{Decimal(msg_out[decf]):.{DECIMAL_SCALE}f}"

                    # --- Apply AVRO_COLUMNS subset on filtered message ---
                    if INPUT_FORMAT == "AVRO" and len(AVRO_COLUMNS) > 1 and isinstance(msg_out, dict):
                        msg_out = {column: msg_out[column] for column in AVRO_COLUMNS}

                    # --- Format output ---
                    if (INPUT_FORMAT in ("AVRO", "JSON")) and OUTPUT_FORMAT == "CSV":
                        df   = pd.json_normalize(msg_out, sep=SEPERATOR)
                        line = df.to_csv(header=False, index=False, sep=SEPERATOR)
                        if DSF_CHARMAP != "UTF-8":
                            line = str(line).encode(DSF_CHARMAP, errors='ignore').decode(DSF_CHARMAP, errors='ignore')

                    elif INPUT_FORMAT == "AVRO" and OUTPUT_FORMAT == "JSON":
                        line = json.dumps(msg_out, default=str, ensure_ascii=False)
                        line = line.encode(DSF_CHARMAP, errors='ignore').decode(DSF_CHARMAP, errors='ignore')
                        line = line + "\n"

                    elif OUTPUT_FORMAT == "JSON":
                        line = str(msg_out) + "\n"

                    else:
                        line = msg.value.decode('UTF-8', 'ignore') + "\n"
                        if DSF_CHARMAP != "UTF-8":
                            line = str(line).encode(DSF_CHARMAP, errors='ignore').decode(DSF_CHARMAP, errors='ignore')

                    f.write(line)

                    if omsg_off >= end_offset:
                        dsf_logger.log_msg(
                            f"Reached end offset for partition {partition.partition}: "
                            f"offset={omsg_off}",
                            level=20
                        )
                        break

                f.flush()
                os.fsync(f.fileno())

            if part_last_offset >= seek_offset:
                last_offsets[partition] = (part_last_offset, part_last_ts)

            dsf_logger.log_msg(
                f"Partition {partition.partition} done: "
                f"total_seen={part_total}, filtered_written={part_filtered}",
                level=20
            )

        # End of partition loop for this attempt
        pre_filter_note = f", pre_filter_matched={pre_filter_matched_count}" if PRE_FILTER_VALUES else ""
        validation_note = f", validation_matched={validation_count}" if VALIDATION_FILTER_VALUES else ""
        dsf_logger.log_msg(
            f"Attempt {attempt} complete: "
            f"total_messages_seen_in_window={total_seen}, "
            f"messages_matching_all_filters={filtered_count}"
            f"{pre_filter_note}"
            f"{validation_note}",
            level=20
        )

        # --- Count validation (validation_count == filtered_count when VALIDATION_FILTER_VALUES is unset) ---
        count_at_or_above = log_count_comparison(
            EXPECTED_COUNT, validation_count, METADATA_COUNT_TOLERANCE_PCT, METADATA_FILE_PATH
        )

        time_remaining = retry_deadline - datetime.now()

        # Helper: promote temp file, write validation log, commit offsets, mark consumed.
        # Called from all accept paths — temp file must exist before calling.
        def _save_and_break(accept_msg: str, accept_level: int) -> None:
            dsf_logger.log_msg(accept_msg, level=accept_level)
            dsf_logger.log_msg("Output summary:", level=20)
            dsf_logger.log_msg(f"  Rows written to output file    : {filtered_count}", level=20)
            dsf_logger.log_msg(f"  Rows counted for validation    : {validation_count}", level=20)
            dsf_logger.log_msg(f"  Expected count (metadata)      : {EXPECTED_COUNT}", level=20)
            if filtered_count != validation_count:
                dsf_logger.log_msg(
                    f"  NOTE: file row count ({filtered_count}) differs from the validated "
                    f"count ({validation_count}) because VALIDATION_FILTER_VALUES="
                    f"{VALIDATION_FILTER_VALUES} narrows which messages count toward "
                    f"EXPECTED_COUNT — all consumed messages are still written to the file.",
                    level=30
                )
            if not os.path.exists(TEMP_DATA_FILE):
                pathlib.Path(TEMP_DATA_FILE).touch()
                dsf_logger.log_msg(
                    f"Temp file did not exist (0 filtered messages); created empty file "
                    f"before promote.",
                    level=20
                )
            os.rename(TEMP_DATA_FILE, DATA_FILE)
            dsf_logger.log_msg(
                f"Temp file promoted: {TEMP_DATA_FILE} → {DATA_FILE}", level=20
            )
            write_get_kafka_validation_log(
                log_file=GET_KAFKA_VALIDATION_LOG,
                username=username,
                business_date=ASOF_DT,
                mandator=DSF_MANDATOR,
                feed_name=FEED_NAME,
                reconciliation_group_id=str(filter_values.get("reconciliationGroupId", "")),
                expected_count=EXPECTED_COUNT,
                actual_count=validation_count,
                tolerance_pct=METADATA_COUNT_TOLERANCE_PCT,
                separator=SEPERATOR,
            )
            dsf_logger.log_msg(f"Output data file       : {DATA_FILE}", level=20)
            dsf_logger.log_msg(
                f"Committing offsets for {len(last_offsets)} partition(s) "
                f"after successful .par write.",
                level=20
            )
            for _part, (_off, _ts) in last_offsets.items():
                consumer.commit({_part: OffsetAndMetadata(_off, _ts)})
                dsf_logger.log_msg(
                    f"Committed partition {_part.partition} at offset {_off}", level=20
                )

        def _remove_temp() -> None:
            if os.path.exists(TEMP_DATA_FILE):
                try:
                    os.remove(TEMP_DATA_FILE)
                    dsf_logger.log_msg(
                        f"Temp file removed: {TEMP_DATA_FILE}", level=20
                    )
                except Exception as _e:
                    dsf_logger.log_msg(
                        f"Cannot remove temp file {TEMP_DATA_FILE}: {_e}. "
                        f"Cannot continue safely — appending to an existing temp file would corrupt the output. "
                        f"Manually remove the file and rerun.",
                        level=40
                    )
                    os._exit(1)

        if count_at_or_above:
            if OVER_COUNT_BEHAVIOR == "STABILITY":
                # ── STABILITY mode: wait for count to stop changing ──────────────
                if validation_count == prev_count:
                    stable_streak += 1
                    dsf_logger.log_msg(
                        f"Count stable at {validation_count} for {stable_streak}/"
                        f"{STABLE_COUNT_REQUIRED_ATTEMPTS} consecutive attempt(s).",
                        level=20
                    )
                else:
                    stable_streak = 1
                    dsf_logger.log_msg(
                        f"Count changed: {prev_count} → {validation_count} "
                        f"(>= expected {EXPECTED_COUNT}). Stability streak reset to 1/"
                        f"{STABLE_COUNT_REQUIRED_ATTEMPTS}.",
                        level=20
                    )
                prev_count = validation_count

                if stable_streak == 1 and not grace_extended \
                        and time_remaining.total_seconds() < RETRY_WAIT_SECONDS:
                    retry_deadline  += timedelta(seconds=RETRY_WAIT_SECONDS)
                    RETRY_TS_UTC_END = datetime.timestamp(retry_deadline)
                    grace_extended   = True
                    time_remaining   = retry_deadline - datetime.now()
                    dsf_logger.log_msg(
                        f"Count first reached target with less than {RETRY_WAIT_SECONDS}s remaining — "
                        f"extending retry deadline by {RETRY_WAIT_SECONDS}s to allow stability confirmation. "
                        f"New retry deadline : {retry_deadline.strftime('%Y-%m-%d %H:%M:%S')}. "
                        f"Kafka scan window now : "
                        f"{DT_UTC_START.strftime('%Y-%m-%d %H:%M:%S')}  →  "
                        f"{datetime.fromtimestamp(RETRY_TS_UTC_END).strftime('%Y-%m-%d %H:%M:%S')}",
                        level=30
                    )

                if stable_streak >= STABLE_COUNT_REQUIRED_ATTEMPTS:
                    _save_and_break(
                        f"Count stable at {validation_count} for {stable_streak} consecutive "
                        f"attempt(s) (expected {EXPECTED_COUNT}). Accepting.",
                        accept_level=20
                    )
                    DATA_CONSUME = "Yes"
                    if last_offsets:
                        dsf_logger.log_msg(
                            "All offsets committed. Data successfully consumed and written.", level=20
                        )
                    break

                elif time_remaining.total_seconds() <= 0:
                    # Window exhausted — accept as-is (>= expected, stability unconfirmed)
                    _save_and_break(
                        f"Retry window exhausted after {attempt} attempt(s). "
                        f"Count {validation_count} >= expected {EXPECTED_COUNT} but stability not "
                        f"confirmed (streak {stable_streak}/{STABLE_COUNT_REQUIRED_ATTEMPTS}). "
                        f"Accepting final count.",
                        accept_level=30
                    )
                    DATA_CONSUME = "Yes"
                    if last_offsets:
                        dsf_logger.log_msg(
                            "All offsets committed. Data successfully consumed and written.", level=20
                        )
                    break

                else:
                    # Still time — keep retrying to check for more messages
                    hours_remaining = time_remaining.total_seconds() / 3600
                    next_attempt_at = (
                        datetime.now() + timedelta(seconds=RETRY_WAIT_SECONDS)
                    ).strftime('%Y-%m-%d %H:%M:%S')
                    dsf_logger.log_msg(
                        f"Count {validation_count} >= expected {EXPECTED_COUNT}, "
                        f"but stability streak {stable_streak}/{STABLE_COUNT_REQUIRED_ATTEMPTS} "
                        f"not yet met. Time remaining: {hours_remaining:.2f}h. "
                        f"Sleeping {RETRY_WAIT_SECONDS}s. Next attempt at: {next_attempt_at}",
                        level=20
                    )
                    _remove_temp()
                    py_time.sleep(RETRY_WAIT_SECONDS)

            else:
                # ── WAIT mode: fixed window once count exceeds expected ───────────
                if validation_count == EXPECTED_COUNT:
                    # Exact match — accept immediately, no timer needed
                    _save_and_break(
                        f"Count exactly matches expected ({validation_count}). Accepting immediately.",
                        accept_level=20
                    )
                    DATA_CONSUME = "Yes"
                    if last_offsets:
                        dsf_logger.log_msg(
                            "All offsets committed. Data successfully consumed and written.", level=20
                        )
                    break

                else:
                    # Count is strictly over expected
                    if over_count_deadline is None:
                        # First time count has exceeded expected — start the fixed wait window
                        over_count_deadline = datetime.now() + timedelta(minutes=OVER_COUNT_WAIT_MINUTES)
                        # Extend the retry deadline so the loop stays alive until the window expires
                        if over_count_deadline > retry_deadline:
                            retry_deadline   = over_count_deadline
                            RETRY_TS_UTC_END = datetime.timestamp(retry_deadline)
                        dsf_logger.log_msg(
                            f"Count EXCEEDS expected: got {validation_count:,}, expected {EXPECTED_COUNT:,}. "
                            f"WAIT mode: starting {OVER_COUNT_WAIT_MINUTES}-min window. "
                            f"Will accept at {over_count_deadline.strftime('%Y-%m-%d %H:%M:%S')} regardless of further changes. "
                            f"(To use stability streak instead: set OVER_COUNT_BEHAVIOR=STABILITY)",
                            level=30
                        )
                    else:
                        secs_left = max(0.0, (over_count_deadline - datetime.now()).total_seconds())
                        if validation_count != prev_count:
                            dsf_logger.log_msg(
                                f"Count grew: {prev_count:,} → {validation_count:,} "
                                f"(expected {EXPECTED_COUNT:,}). "
                                f"Timer unchanged — {secs_left:.0f}s remaining until acceptance.",
                                level=20
                            )
                        else:
                            dsf_logger.log_msg(
                                f"Count stable at {validation_count:,} (over expected {EXPECTED_COUNT:,}). "
                                f"{secs_left:.0f}s remaining until acceptance.",
                                level=20
                            )

                    prev_count = validation_count

                    if datetime.now() >= over_count_deadline:
                        _save_and_break(
                            f"Over-count wait window of {OVER_COUNT_WAIT_MINUTES} min expired. "
                            f"Accepting final count {validation_count:,} (expected {EXPECTED_COUNT:,}).",
                            accept_level=20
                        )
                        DATA_CONSUME = "Yes"
                        if last_offsets:
                            dsf_logger.log_msg(
                                "All offsets committed. Data successfully consumed and written.", level=20
                            )
                        break
                    else:
                        # Still inside the wait window — sleep and retry
                        secs_left = max(0.0, (over_count_deadline - datetime.now()).total_seconds())
                        next_attempt_at = (
                            datetime.now() + timedelta(seconds=RETRY_WAIT_SECONDS)
                        ).strftime('%Y-%m-%d %H:%M:%S')
                        dsf_logger.log_msg(
                            f"Over-count window in progress. {secs_left:.0f}s remaining. "
                            f"Sleeping {RETRY_WAIT_SECONDS}s. Next attempt at: {next_attempt_at}",
                            level=20
                        )
                        _remove_temp()
                        py_time.sleep(RETRY_WAIT_SECONDS)

        else:
            # Under expected — never accept early; wait for more messages or exhaustion
            prev_count    = validation_count
            stable_streak = 0  # reset in case count dropped back below expected

            if time_remaining.total_seconds() <= 0:
                # Exhausted — final lower-tolerance check
                low = EXPECTED_COUNT - EXPECTED_COUNT * METADATA_COUNT_TOLERANCE_PCT / 100.0
                if validation_count >= low:
                    # Temp file still exists (not deleted yet) — promote it
                    _save_and_break(
                        f"Retry window exhausted after {attempt} attempt(s). "
                        f"Final count {validation_count} is within lower tolerance "
                        f"({int(low)}–{EXPECTED_COUNT}, ±{METADATA_COUNT_TOLERANCE_PCT}%). "
                        f"Accepting.",
                        accept_level=30
                    )
                    DATA_CONSUME = "Yes"
                    if last_offsets:
                        dsf_logger.log_msg(
                            "All offsets committed. Data successfully consumed and written.",
                            level=20
                        )
                    break
                else:
                    _remove_temp()
                    if PRE_FILTER_VALUES and pre_filter_matched_count == 0 and total_seen > 0:
                        dsf_logger.log_msg(
                            f"PRE_FILTER_VALUES={PRE_FILTER_VALUES} matched 0 of {total_seen} total "
                            f"message(s) seen in the scan window — check the filter field name(s)/"
                            f"value(s) are correct for this topic. This is likely a configuration "
                            f"issue, not a timing issue.",
                            level=40
                        )
                    dsf_logger.log_msg(
                        f"Retry window exhausted after {attempt} attempt(s). "
                        f"Final count {validation_count} is BELOW lower tolerance threshold "
                        f"{int(low)} (expected {EXPECTED_COUNT}, "
                        f"±{METADATA_COUNT_TOLERANCE_PCT}%). Failing.",
                        level=40
                    )
                    log_failure_analysis(seen_combinations, filter_values, METADATA_FILTER_COLUMNS)
                    dsf_logger.log_msg(
                        "Offsets have NOT been committed. "
                        "Rerun this script to re-consume and re-validate messages.",
                        level=40
                    )
                    write_get_kafka_validation_log(
                        GET_KAFKA_VALIDATION_LOG, username, ASOF_DT, DSF_MANDATOR, FEED_NAME,
                        str(filter_values.get("reconciliationGroupId", "")),
                        EXPECTED_COUNT, validation_count, METADATA_COUNT_TOLERANCE_PCT,
                        SEPERATOR, "FAILED",
                        f"COUNT_BELOW_TOLERANCE: expected={EXPECTED_COUNT} actual={validation_count} tolerance_lower={int(low)}",
                    )
                    os._exit(1)

            else:
                # Still time — delete temp and sleep
                hours_remaining = time_remaining.total_seconds() / 3600
                next_attempt_at = (
                    datetime.now() + timedelta(seconds=RETRY_WAIT_SECONDS)
                ).strftime('%Y-%m-%d %H:%M:%S')
                dsf_logger.log_msg(
                    f"Under-count on attempt {attempt}: got {validation_count}, "
                    f"expected {EXPECTED_COUNT}. "
                    f"Time remaining: {hours_remaining:.2f}h. "
                    f"Sleeping {RETRY_WAIT_SECONDS}s. "
                    f"Next attempt at: {next_attempt_at}",
                    level=30
                )
                _remove_temp()
                py_time.sleep(RETRY_WAIT_SECONDS)

except Exception as _loop_err:
    dsf_logger.log_msg(
        f"Unexpected error in collection/validation loop: {_loop_err}", level=40
    )
    if os.path.exists(TEMP_DATA_FILE):
        try:
            os.remove(TEMP_DATA_FILE)
            dsf_logger.log_msg(
                f"Temp file cleaned up after exception: {TEMP_DATA_FILE}", level=20
            )
        except Exception as _cleanup_err:
            dsf_logger.log_msg(
                f"Could not clean up temp file {TEMP_DATA_FILE}: {_cleanup_err}", level=30
            )
    raise


# if DATA_CONSUME is set we know we have consumed at least one message
if(DATA_CONSUME != "Yes" and ALLOW_NO_DATA == "No"):
    dsf_logger.log_msg(f"No data consumed from any partition!!!", level=40)
    write_get_kafka_validation_log(
        GET_KAFKA_VALIDATION_LOG, username, ASOF_DT, DSF_MANDATOR, FEED_NAME,
        str(filter_values.get("reconciliationGroupId", "")),
        EXPECTED_COUNT, 0, METADATA_COUNT_TOLERANCE_PCT,
        SEPERATOR, "FAILED", "NO_DATA_CONSUMED",
    )
    os._exit(1)

# redundant for clarity
elif (DATA_CONSUME != "Yes" and ALLOW_NO_DATA == "Yes"):
    pathlib.Path(DATA_FILE).touch()
    dsf_logger.log_msg(f"No data consumed from any partition!!!", level=30)


# Do with restart file if exists
if(restart == "YES"):
    merged_files = subprocess.run([f"/usr/bin/cat {FIRST_DATA_FILE} {DATA_FILE} > {DATA_FILE}_ASI"], shell=True)
    if merged_files.returncode !=0:
        dsf_logger.log_msg("Error merging RESTART file into ASI file", level=40)
        os._exit(9)

    move_file = subprocess.run([f"/usr/bin/mv {DATA_FILE}_ASI {DATA_FILE}"], shell=True)
    del_file = subprocess.run([f"/usr/bin/rm {FIRST_DATA_FILE}"], shell=True)

    if move_file.returncode != 0 or del_file.returncode != 0:
        dsf_logger.log_msg("Error with move of merged file", level=40)
        os._exit(9)


dsf_logger.log_msg(f"Exiting (code 0): data successfully consumed and written to {DATA_FILE}", level=20)
os._exit(0)
