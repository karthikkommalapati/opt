
import os
from datetime import datetime
from typing import Any, Dict, List, Optional, Set, Tuple, Union

import dsf_logging

verbose = 1
timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

# Configure logging
logging_level = 20
verbose_log = True
SDA, FEED_NAME = os.environ["SDA"], os.environ["FEED_NAME"]
ASOF_DT = os.environ[f'{SDA}_ASOF_DT']
PARENT_PID = os.environ["PARENT_PID"]
AUDIT_ID = os.environ["AUDIT_ID"]
get_kafka_log_path = os.environ["LOG_PROC_PATH"]
get_kafka_log_name = f'{FEED_NAME}.{ASOF_DT}.{PARENT_PID}.{AUDIT_ID}.log'
username = os.getenv('USER', 'unknown')


dsf_logger = dsf_logging.DSF_logging()
dsf_logger.get_logger(
    name=get_kafka_log_name,
    output_path=get_kafka_log_path,
    level =logging_level,
    verbose=verbose_log
)

dsf_logger.log_msg(f'Get Kafka logging at : {get_kafka_log_path}/{get_kafka_log_name}', level=20)

try:
    
    import base64
    import collections
    import dataclasses
    import glob
    import io
    import json
    import logging
    import pathlib
    import pprint
    import re
    import sched
    import subprocess
    import sys
    import time as py_time
    import traceback
    from datetime import date, datetime, time, timedelta, timezone
    from decimal import Decimal
    from queue import Empty, Queue
    from threading import Thread
    from urllib.parse import unquote

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
    raise dsf_logger.log_error_msg(msg='Error_LOADING_LIBRARIES', err=err)


for key, val in os.environ.items():
    dsf_logger.log_msg(f'ENV_VAR: {key}={val}', level=20)
    

def list_files_in_folder(folder) -> List[str]:
    """Return a list of file paths matching the given glob pattern."""
    return glob.glob(folder)

def exit_with_error(message:str, exit_code:int=1):
    """Log message at ERROR level and immediately terminate the process with exit_code."""
    dsf_logger.log_msg(message, level=40)
    os._exit(exit_code)

# logging
def log_it(message):
    """Print message to stdout with immediate flush (debug helper)."""
    print(message, flush=True)
    
    


def path_to_dict(file_path: Optional[str]) -> dict:
    """Open a JSON file at file_path and return its contents as a validated dict."""
    try:
        with open(assert_path(file_path)) as file_dict:
            return assert_dict(json.load(file_dict))
        
    except Exception as err:
        raise dsf_logger.log_error_msg(
            msg=f'Unexpected error opening file: {file_path}', err=err
        )
    
# convert timestamp to string
def ts_to_str(ts:float) -> str:
    """Convert a Unix float timestamp to a formatted datetime string."""
    return datetime.fromtimestamp(ts).strftime(DATETIME_FORMAT)

def get_avro_schema(topic, reg_addrs, ca_file, sslcert, sslkey, rversion):
    """Fetch the Avro schema for a topic from the Schema Registry.

    Tries each comma-separated address in reg_addrs in order.
    Returns (version, schema_id, schema) on success.
    Raises on failure if rversion is 'latest'; logs a warning otherwise.
    """
    # API end point for retrieving the latest schema version for a topic
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
    """Return True if ftype represents an Avro decimal logical type."""
    if (isinstance(ftype, str)) and (ftype.lower() == 'decimal'):
        return True
    elif (isinstance(ftype, dict) and ftype.get('type') == 'bytes') and (ftype.get('logicalType') == 'decimal'): 
        return True
    return False


def get_decimal_fields(avschema):
    """Return a list of field names in avschema that have a decimal logical type."""
    decattrs = []
    def traverse_schema(schema):
        """Recursively collect decimal field names from schema fields."""
        for field in schema['fields']:
            ftype = field['type']
            if is_dec_type(ftype):
                decattrs.append(field['name'])
            if 'fields' in field:
                decattrs.extend(traverse_schema(field))
                
    traverse_schema(avschema)
    
    if not decattrs:
        dsf_logger.log_msg(f"Decimal conversion requested, but no decimal fields found", level=30)
        
    else:
        dsf_logger.log_msg(f"Decimal fields found {decattrs}", level=30)
    return decattrs


def validate_instance_and_get_max_runid(messages_list, mandator_filter, producer_filter, business_date, validation_mode="SEQUENTIAL"):
    """Validate that all instances are present for the max reconciliationGroupId and return validation result.

    Filters messages by mandator, producer and business_date, then finds the max reconciliationGroupId.
    Validates that all instanceIndex values 0..N-1 are present (no gaps, no duplicates).

    Required columns (configurable via VALIDATION_REQUIRED_COLUMNS):
        status.mandatorCode, producer, status.businessDate,
        status.reconciliationGroupId, status.totalInstances, status.instanceIndex

    Returns:
        (is_valid, max_run_id, missing_instances, dataframe)
        - is_valid: True if all instances present and no duplicates
        - max_run_id: int or None
        - missing_instances: list of missing instanceIndex values (empty if valid)
        - dataframe: filtered DataFrame for the max reconciliationGroupId (or None on early exit)
    """
    dsf_logger.log_msg(f"starting validation ===", level=20)
    dsf_logger.log_msg(f"filter criteria: mandator={mandator_filter}, producer={producer_filter}, business_date={business_date}", level=20)
    
    if not messages_list:
        dsf_logger.log_msg(f"No messages found in the topic for the given filters", level=30)
        return False, None, [], None
    
    df = pd.DataFrame(messages_list)
    
    if 'status' in df.columns and isinstance(df['status'].iloc[0], dict):
        status_df = pd.json_normalize(df['status'].tolist())
        status_df.columns = [f'status.{col}' for col in status_df.columns]
        df = pd.concat([df.drop(columns=['status']), status_df], axis=1)
        
    dsf_logger.log_msg(f"Total messages found: {len(df)}", level=20)
    dsf_logger.log_msg(f"Columns in df: {df.columns.tolist()}", level=20)

    missing_cols = [col for col in VALIDATION_REQUIRED_COLUMNS if col not in df.columns]
    if missing_cols:
        dsf_logger.log_msg(f"VALIDATION FAILED: Required columns missing from messages: {missing_cols}", level=40)
        return False, None, [], None

    if len(df) > 0:
        dsf_logger.log_msg(f"sample columns in df: {df.columns[:5].tolist()}", level=20)
        
        first_mandator = df['status.mandatorCode'].iloc[0]
        first_producer = df['producer'].iloc[0]
        first_date = df['status.businessDate'].iloc[0]

        dsf_logger.log_msg(f"Sample data - mandatorCode: {first_mandator} (type: {type(first_mandator).__name__})", level=20)
        dsf_logger.log_msg(f"Sample data - producer: {first_producer} (type: {type(first_producer).__name__})", level=20)
        dsf_logger.log_msg(f"Sample data - businessDate: {first_date} (type: {type(first_date).__name__})", level=20)
        dsf_logger.log_msg(f"Filter - mandator: {mandator_filter} (type: {type(mandator_filter).__name__})", level=20)
        dsf_logger.log_msg(f"Filter - producer: {producer_filter} (type: {type(producer_filter).__name__})", level=20)
        dsf_logger.log_msg(f"Filter - business_date: {business_date} (type: {type(business_date).__name__})", level=20)
        
    df['status.mandatorCode'] = df['status.mandatorCode'].astype(str)
    df['producer'] = df['producer'].astype(str)
    df['status.businessDate'] = df['status.businessDate'].astype(str)
    
    mandator_filter = str(mandator_filter)
    producer_filter = str(producer_filter)
    business_date = str(business_date)
    
    mandator_match = df['status.mandatorCode'] == mandator_filter
    producer_match = df['producer'] == producer_filter
    date_match = df['status.businessDate'] == business_date
    
    dsf_logger.log_msg(f"Messages matching mandator filter: {mandator_match.sum()}", level=20)
    dsf_logger.log_msg(f"Messages matching producer filter: {producer_match.sum()}", level=20)
    dsf_logger.log_msg(f"Messages matching business date filter: {date_match.sum()}", level=20)
    
    filtered_df = df[mandator_match & producer_match & date_match] 
    
    if filtered_df.empty:
        
        producer_df = df[df['producer'].astype(str) == producer_filter]
        if not producer_df.empty:
            date_mandator_map = (
                producer_df.groupby('status.businessDate')['status.mandatorCode']
                .apply(lambda x: sorted(x.astype(str).unique().tolist()))
                .to_dict()
            )
            dsf_logger.log_msg(f"Data availability for producer '{producer_filter}':", level=30)
            for date, mandators in sorted(date_mandator_map.items()):
                dsf_logger.log_msg(f"  Date {date}: mandators {mandators}", level=30)
        else:
            dsf_logger.log_msg(f"No messages found for producer '{producer_filter}'", level=30)
        dsf_logger.log_msg(
            f"No messages found matching filters: mandator={mandator_filter}, producer={producer_filter}, business_date={business_date}",
            level=30
        )
        
        return False, None, [], filtered_df
    
    dsf_logger.log_msg(f"Messages found matching all filters: {len(filtered_df)}", level=20)
    
    valid_run_ids = filtered_df['status.reconciliationGroupId'].dropna()
    if valid_run_ids.empty:
        dsf_logger.log_msg("VALIDATION FAILED: No valid reconciliationGroupId values found in filtered messages", level=40)
        return False, None, [], filtered_df
    max_run_id = int(valid_run_ids.max())
    dsf_logger.log_msg(f"Max reconciliationGroupId among filtered messages: {max_run_id}", level=20)
    
    max_runid_df = filtered_df[filtered_df['status.reconciliationGroupId'] == max_run_id]
    dsf_logger.log_msg(f"Messages with max reconciliationGroupId ({max_run_id}) count: {len(max_runid_df)}", level=20)
    
    total_instances_values = max_runid_df['status.totalInstances'].unique()
    if len(total_instances_values) > 1:
        dsf_logger.log_msg(
            f"WARNING: Multiple totalInstances values found: {total_instances_values}. Using Max.", level=30
        )
    
    total_instances = int(max_runid_df['status.totalInstances'].max())
    dsf_logger.log_msg(f"expected totalinstance: {total_instances}", level=20)
    
    present_instances = set(max_runid_df['status.instanceIndex'].unique())
    dsf_logger.log_msg(f"Present instancesIndex count: {len(present_instances)}", level=20)
    dsf_logger.log_msg(f"Expected total instances: {total_instances}", level=20)

    if validation_mode == "UNIQUE_COUNT":
        present_count = len(present_instances)
        if present_count != total_instances:
            reason = "short" if present_count < total_instances else "over"
            missing_instances = [f"{present_count} of {total_instances} unique instances received ({reason})"]
            dsf_logger.log_msg(
                f"VALIDATION FAILED: {present_count} unique instanceIndex values received, expected exactly {total_instances} ({reason}). "
                f"Present: {sorted(present_instances)}", level=40
            )
            return False, max_run_id, missing_instances, max_runid_df
    else:
        expected_instances = set(range(total_instances))
        missing_instances = sorted(int(x) for x in expected_instances - present_instances)

        if missing_instances:
            dsf_logger.log_msg(
                f"VALIDATION FAILED: Missing instanceIds: {missing_instances}", level=40
            )

            dsf_logger.log_msg(
                f"Present instanceIds: {sorted(int(x) for x in present_instances)}", level=30
            )
            return False, max_run_id, missing_instances, max_runid_df
    
    duplicate_instances = max_runid_df[max_runid_df.duplicated(subset=['status.instanceIndex'], keep=False)]
    
    if not duplicate_instances.empty:
        dup_indices = sorted(duplicate_instances['status.instanceIndex'].unique().tolist())
        dsf_logger.log_msg(
            f"VALIDATION FAILED: Duplicate instanceIds found: {dup_indices}", level=40
        )
        return False, max_run_id, [], max_runid_df
    
    dsf_logger.log_msg(f"VALIDATION PASSED: All instanceIds from 0 to {total_instances-1} are present with no duplicates.", level=20)
    
    instance_messages_dict = {}
    
    for _, row in max_runid_df.iterrows():
        instance_idx = int(row['status.instanceIndex'])
        num_messages = int(row['status.numberOfMessagesPublished'])
        instance_messages_dict[instance_idx] = num_messages
        
    sorted_dict = dict(sorted(instance_messages_dict.items()))
    dsf_logger.log_msg(f"Instance messages count: {sorted_dict}", level=20)
    
    total_messages = sum(instance_messages_dict.values())
    dsf_logger.log_msg(f"Total messages across all instances: {total_messages}", level=20)
    
    return True, max_run_id, [], max_runid_df


def get_committed_offset(committed, default_offset: int) -> int:
    """Safely extract integer offset from consumer.committed() return value.

    consumer.committed() can return None, an int, an OffsetAndMetadata object,
    or a tuple depending on the kafka-python version and broker response.
    Returns default_offset if committed is None or unrecognised.
    """
    if committed is None:
        return default_offset
    if isinstance(committed, int):
        return committed
    if hasattr(committed, "offset"):
        return int(committed.offset)
    if isinstance(committed, (tuple, list)) and committed:
        return int(committed[0])
    return default_offset


def extract_message_filter_fields(msg_data: Dict[str, Any]) -> Tuple[str, str, str, str]:
    """Extract mandator, producer, business_date and run_id from a status message.

    Expected message structure:
        {"status": {"mandatorCode": ..., "businessDate": ..., "reconciliationGroupId": ...}, "producer": ...}

    Returns:
        (mandator, producer, business_date, run_id) all as strings.
    """
    status = msg_data.get("status", {}) if isinstance(msg_data, dict) else {}

    producer = msg_data.get("producer", "")
    mandator = status.get("mandatorCode", "")
    business_date = status.get("businessDate", "")
    run_id = status.get("reconciliationGroupId", -1)

    if hasattr(business_date, "strftime"):
        business_date = business_date.strftime(DATE_FORMAT)
    else:
        business_date = str(business_date)

    return str(mandator), str(producer), business_date, str(run_id)


def matches_filter(msg_mandator, msg_producer, msg_business_date, msg_run_id,
                   mandator, producer_filter, asof_dt, max_run_id) -> bool:
    """Return True if a message matches all filter criteria and belongs to the max reconciliationGroupId.

    Used in the write pass to select only messages that passed validation.
    All comparisons are done as strings to avoid type mismatch issues.
    """
    return (
        str(msg_mandator) == str(mandator) and
        str(msg_producer) == str(producer_filter) and
        msg_business_date == str(asof_dt) and
        str(msg_run_id) == str(max_run_id)
    )

def format_output_message(msg_data, raw_value, input_fmt, output_fmt, separator, charmap) -> str:
    """Format a decoded message for writing to the output .par file.

    Handles all input/output format combinations:
        AVRO/JSON → CSV  : flattens with pd.json_normalize, writes as CSV row
        AVRO → JSON      : json.dumps with charmap encoding
        * → JSON         : json.dumps
        * → *            : raw bytes decoded as UTF-8

    Note: charmap on AIX is ISO88591-1 (non-standard name). Encoding errors are ignored.
    """
    if input_fmt in ["AVRO", "JSON"] and output_fmt == "CSV":
        df = pd.json_normalize(msg_data, sep=separator)
        output_msg = df.to_csv(header=False, index=False, sep=separator)
        if charmap != "UTF-8":
            output_msg = output_msg.encode(charmap, errors='ignore').decode(charmap, errors='ignore')
    elif input_fmt == "AVRO" and output_fmt == "JSON":
        output_msg = json.dumps(msg_data, default=str, ensure_ascii=False).encode(charmap).decode(charmap) + "\n"
    elif output_fmt == "JSON":
        output_msg = json.dumps(msg_data, default=str, ensure_ascii=False) + "\n"
    else:
        output_msg = raw_value.decode("UTF-8", 'ignore') + "\n"
        if charmap != "UTF-8":
            output_msg = output_msg.encode(charmap, errors='ignore').decode(charmap, errors='ignore')
    return output_msg


def write_validation_metadata(meta_file, log_file, mandator, producer_name, feed_name,
                               run_id, validated_df, separator, username='unknown'):
    """Write validation metadata to two files after successful instance validation.

    File 1 (key=value txt, meta_file): always replaced.
    File 2 (separator-delimited txt, log_file): append-only history log.
    Errors out if status.numberOfMessagesPublished column is absent.
    """
    if 'status.numberOfMessagesPublished' not in validated_df.columns:
        dsf_logger.log_msg(
            "METADATA ERROR: 'status.numberOfMessagesPublished' column not found in validated messages — cannot export metadata",
            level=40
        )
        os._exit(1)

    total_messages_published = int(validated_df['status.numberOfMessagesPublished'].sum())
    if total_messages_published == 0:
        dsf_logger.log_msg(
            "METADATA ERROR: total_messages_published is 0 — all instances reported zero messages published",
            level=40
        )
        os._exit(1)
    instances_counted        = int(validated_df['status.instanceIndex'].nunique())
    total_expected_instances = int(validated_df['status.totalInstances'].max())
    business_date            = str(validated_df['status.businessDate'].iloc[0])
    export_dt                = datetime.now().strftime("%Y-%m-%dT%H:%M:%S")

    fields = {
        "export_datetime":          export_dt,
        "username":                 username,
        "business_date":            business_date,
        "mandator":                 mandator,
        "producer_name":            producer_name,
        "feed_name":                feed_name,
        "reconciliation_group_id":  str(run_id),
        "instances_counted":        str(instances_counted),
        "total_expected_instances": str(total_expected_instances),
        "total_messages_published": str(total_messages_published),
    }

    # File 1 — fresh pipe-delimited file (header row + data row)
    if os.path.exists(meta_file):
        os.remove(meta_file)
    header_row_meta = separator.join(fields.keys())
    data_row_meta   = separator.join(fields.values())
    with open(meta_file, 'w', encoding='UTF-8') as f:
        f.write(header_row_meta + "\n")
        f.write(data_row_meta + "\n")
    dsf_logger.log_msg(f"Metadata file written  : {meta_file}", level=20)

    # File 2 — append-only history log
    header_row   = separator.join(fields.keys())
    data_row     = separator.join(fields.values())
    write_header = not os.path.exists(log_file)
    with open(log_file, 'a', encoding='UTF-8') as f:
        if write_header:
            f.write(header_row + "\n")
        f.write(data_row + "\n")
    dsf_logger.log_msg(f"Validation log appended: {log_file}", level=20)
    

def write_failure_log_entry(log_file, mandator, producer_name, feed_name,
                             reason, max_run_id, missing_instances_list,
                             validated_df, separator, username='unknown'):
    """Append one FAILED row to the validation log."""
    if validated_df is not None and not validated_df.empty:
        expected_instances = (
            str(int(validated_df['status.totalInstances'].max()))
            if 'status.totalInstances' in validated_df.columns else ""
        )
        found_count = (
            str(len(set(validated_df['status.instanceIndex'].unique())))
            if 'status.instanceIndex' in validated_df.columns else ""
        )
        business_date = (
            str(validated_df['status.businessDate'].iloc[0])
            if 'status.businessDate' in validated_df.columns else ASOF_DT
        )
    else:
        expected_instances = ""
        found_count = "0"
        business_date = ASOF_DT

    fields = {
        "export_datetime":          datetime.now().strftime("%Y-%m-%dT%H:%M:%S"),
        "username":                 username,
        "business_date":            business_date,
        "mandator":                 mandator,
        "producer_name":            producer_name,
        "feed_name":                feed_name,
        "reconciliation_group_id":  str(max_run_id) if max_run_id is not None else "",
        "instances_counted":        found_count,
        "total_expected_instances": expected_instances,
        "total_messages_published": "",
        "status":                   "FAILED",
        "failure_reason":           reason,
        "missing_indices":          ",".join(str(i) for i in missing_instances_list),
    }
    header_row   = separator.join(fields.keys())
    data_row     = separator.join(fields.values())
    write_header = not os.path.exists(log_file)
    with open(log_file, 'a', encoding='UTF-8') as f:
        if write_header:
            f.write(header_row + "\n")
        f.write(data_row + "\n")
    dsf_logger.log_msg(f"Failure log entry appended: {log_file}", level=40)


def log_topic_match_report(messages_list, mandator, producer_filter, target_date, topic):
    """Log a drill-down match report on failure: topic → mandator → producer → date.

    Stops at the first mismatch so the operator knows exactly what to tell the sender.
    """
    if not messages_list:
        dsf_logger.log_msg(
            "\n".join([
                f"TOPIC MATCH REPORT  topic='{topic}'  looking for: mandator={mandator}, producer={producer_filter}, date={target_date}",
                f"  No messages were collected from the topic — cannot analyse data availability",
                f"  Possible causes:",
                f"   - No messages published to topic '{topic}' within the configured time window",
                f"   - Mandator '{mandator}' has not published any status messages yet",
                f"   - Time window (START_TS/STOP_TS) does not cover the publish time — check config",
                f"  Action: confirm with sender that messages for mandator '{mandator}' are being published to '{topic}'",
            ]),
            level=40
        )
        return

    try:
        df = pd.json_normalize(messages_list)

        # flatten status sub-dict if present
        if 'status' in df.columns:
            status_df = pd.json_normalize(df['status'].tolist())
            status_df.columns = ['status.' + str(c) for c in status_df.columns]
            df = pd.concat([df.drop('status', axis=1), status_df], axis=1)

        lines = [f"TOPIC MATCH REPORT  topic='{topic}'  looking for: mandator={mandator}, producer={producer_filter}, date={target_date}"]

        # Step 1 — mandator
        if 'status.mandatorCode' not in df.columns:
            lines.append("  [?] Cannot check mandator — 'status.mandatorCode' column missing from messages")
            dsf_logger.log_msg("\n".join(lines), level=40)
            return

        all_mandators = sorted(df['status.mandatorCode'].astype(str).unique().tolist())
        mandator_match = str(mandator) in all_mandators
        lines.append(f"  Mandator {mandator} in topic : {'YES' if mandator_match else 'NO  — mandators found: ' + str(all_mandators)}")

        if not mandator_match:
            lines.append(f"  Action: confirm with sender that mandator '{mandator}' is publishing to topic '{topic}'")
            dsf_logger.log_msg("\n".join(lines), level=40)
            return

        # Step 2 — producer (scoped to our mandator)
        mandator_df = df[df['status.mandatorCode'].astype(str) == str(mandator)]
        if 'producer' not in mandator_df.columns:
            lines.append("  [?] Cannot check producer — 'producer' column missing from messages")
            dsf_logger.log_msg("\n".join(lines), level=40)
            return

        all_producers = sorted(mandator_df['producer'].astype(str).unique().tolist())
        producer_match = str(producer_filter) in all_producers
        lines.append(f"  Producer '{producer_filter}' found : {'YES' if producer_match else 'NO  — producers found for mandator ' + str(mandator) + ': ' + str(all_producers)}")

        if not producer_match:
            lines.append(f"  Action: confirm with sender that producer '{producer_filter}' is publishing for mandator '{mandator}'")
            dsf_logger.log_msg("\n".join(lines), level=40)
            return

        # Step 3 — date (scoped to our mandator + producer)
        prod_df = mandator_df[mandator_df['producer'].astype(str) == str(producer_filter)]
        if 'status.businessDate' not in prod_df.columns:
            lines.append("  [?] Cannot check date — 'status.businessDate' column missing from messages")
            dsf_logger.log_msg("\n".join(lines), level=40)
            return

        all_dates = sorted(prod_df['status.businessDate'].astype(str).unique().tolist())
        date_match = str(target_date) in all_dates
        lines.append(f"  Date {target_date} found      : {'YES' if date_match else 'NO  — dates available for mandator ' + str(mandator) + ' / producer ' + str(producer_filter) + ': ' + str(all_dates)}")

        if not date_match:
            lines.append(f"  Action: confirm with sender that data for date '{target_date}' has been published")

        dsf_logger.log_msg("\n".join(lines), level=40)

    except Exception as e:
        dsf_logger.log_msg(f"TOPIC MATCH REPORT: could not analyse messages — {e}", level=40)


########### Main function starts ############

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
DOMAIN = os.environ["DSF_DOMAIN"]
KEEP_TGT = 'YES'
MAIN = os.environ["DSF_MAIN_ID"]
DSF_CHARMAP = os.environ["DSF_CHARMAP"]
DSF_MANDATOR = os.environ["DSF_MANDATOR"]
FEED_NAME = "CPSB4Q00"
KNW_FRO_TS = os.environ[f"{SDA}_KNW_FRO_TS"]
LOC_TZ = os.environ["DSF_JAVA_TZ"]
SDA_USR_DEF_VAl = os.environ[f"{SDA}_USR_VAL"]

if (SDA_USR_DEF_VAl == "1"):
    CONSUMED_COMMITTED = "Yes"
    dsf_logger.log_msg(f"Consumer mode: CONSUMED_COMMITTED — seeking from last committed offset (SDA_USR_DEF_VAl=1)", level=20)
else:
    CONSUMED_COMMITTED = "No"
    dsf_logger.log_msg(f"Consumer mode: TIME_WINDOW — seeking from time window start offset (SDA_USR_DEF_VAl != 1)", level=20)
    
# set initial state and constants
DATA_CONSUME = "No"
DATETIME_FORMAT = "%Y-%m-%d-%H.%M.%S"
TIME_FORMAT= "%H.%M.%S"


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

# Assign vars from config file

MANDATOR_KEY = CONFIG.get("STREAMING_MANDATOR_KEY")
PRODUCER_FILTER = CONFIG.get("PRODUCER_FILTER","")
INSTANCE_VALIDATION_MODE = str(CONFIG.get("INSTANCE_VALIDATION_MODE", "SEQUENTIAL")).upper()
if INSTANCE_VALIDATION_MODE not in ("SEQUENTIAL", "UNIQUE_COUNT"):
    dsf_logger.log_msg(f"INSTANCE_VALIDATION_MODE must be SEQUENTIAL or UNIQUE_COUNT, got '{INSTANCE_VALIDATION_MODE}'", level=40)
    os._exit(9)
VALIDATE_TOPIC_MANDATOR = CONFIG.get("VALIDATE_TOPIC_MANDATOR", False)
SSL_DIR = "/etc/ssl/"+str(CONFIG["KAFKA_USER"][DSF_MANDATOR])

# Kafka Variables
INFLOW_TOPIC = str(CONFIG["STREAMING_KAFKA_INFLOW_TOPIC"][DSF_MANDATOR])
KAFKA_BROKER = str(CONFIG["STREAMING_KAFKA_BROKER"][DSF_MANDATOR])

# Data format and selection
INPUT_FORMAT = CONFIG["INPUT_DATA"]
OUTPUT_FORMAT = CONFIG["OUTPUT_DATA"]
SEPERATOR = CONFIG["SEPERATOR"]

# Validation columns
VALIDATION_REQUIRED_COLUMNS = [col.strip() for col in CONFIG.get(
    "VALIDATION_REQUIRED_COLUMNS",
    "status.mandatorCode,producer,status.businessDate,status.reconciliationGroupId,status.totalInstances,status.instanceIndex"
).split(",")]

# AVRO Variables
AVRO_COLUMNS = CONFIG["AVRO_COLUMNS"].split(",")
AVRO_FILE = CONFIG.get("AVRO_SCHEMA_FILE", CONFIG.get("AVRO_SHCEMA_FILE"))
AVRO_SCHEMA = f"{CFG_DIR}/{AVRO_FILE}"
AVRO_REG_ADDRS = CONFIG.get("AVRO_SCHEMA_REGISTRY")
DECIMAL_CONV = CONFIG.get("DECIMAL_CONV")
DECIMAL_SCALE: int = int(CONFIG.get("DECIMAL_SCALE"))
oschema_id = -1

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
VERBOSE: bool = eval(CONFIG.get("STREAMING_VERBOSE"))
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
EXTENDED_TR = str(CONFIG.get("EXTEND_ON_ITERATE", "NO")).upper()
MOVE_START = str(CONFIG.get("START_FROM_LAST", "NO")).upper()


_min_listen_minutes = CONFIG.get("MIN_LISTENING_DURATION_MINUTES")
_default_listen_hours = (float(_min_listen_minutes) / 60) if _min_listen_minutes is not None else 0.0
MAX_LISTEN_DURATION_HOURS = float(CONFIG.get("MAX_LISTEN_DURATION_HOURS", _default_listen_hours))
MAX_LISTEN_DURATION_MINUTES = int(MAX_LISTEN_DURATION_HOURS * 60)
MAX_RETRY_ATTEMPTS = int(CONFIG.get("MAX_RETRY_ATTEMPTS", 0))
RETRY_WAIT_SECONDS = int(CONFIG.get("RETRY_WAIT_SECONDS"))

dsf_logger.log_msg(f"max listen duration hours: {MAX_LISTEN_DURATION_HOURS}", level=20)

# General Log
dsf_logger.log_starting_process(__file__, level=20)
dsf_logger.log_msg(f"INFLOW_TOPIC: {INFLOW_TOPIC}", level=20)
dsf_logger.log_msg(f"KAFKA_BROKER: {KAFKA_BROKER}", level=20)
dsf_logger.log_msg(f"INFO: SSL_DIR {SSL_DIR}", level=20)


# SSL Config

SSL_CA_FILE = f"{SSL_DIR}/ca_root.pem"
SSL_CLIENT_CERT = f"{SSL_DIR}/certificate.pem"
SSL_KEY = f"{SSL_DIR}/cert_key.pem"


if COR_SEQ_NR > 1 and EXTENDED_TR == "YES":
    if MOVE_START == "YES":
        START_DT_OFFSET = STOP_DT_OFFSET
    
    STOP_TS = "CURRENT"
    STOP_DT_OFFSET = 0
    dsf_logger.log_msg(f"Override to set STOP_TS set to current timestamp!!", level=20)
    
    
TS_UTC_END = None

if (STOP_TS == "CURRENT"):
    STOP_TS = KNW_FRO_TS
    if (STOP_DT_OFFSET != 0):
        dsf_logger.log_msg(f"STOP_DT_OFFSET has to be zero if STOP_TS set to CURRENT_TS", level=40)
        os._exit(9)
    STOP_TS: datetime = datetime.strptime(KNW_FRO_TS, DATETIME_FORMAT)
    pst = pytz.timezone(LOC_TZ)
    STOP_TS = pst.localize(STOP_TS)
    DT_UTC_END = STOP_TS.astimezone(pytz.utc)
    TS_UTC_END: float = datetime.timestamp(DT_UTC_END)

    dsf_logger.log_msg(f"KNOW_FRO_TS changes as follows: DT_UTC_END: {DT_UTC_END}, TS_UTC_END: {TS_UTC_END}", level=20)


DT_UTC: datetime.date = datetime.strptime(ASOF_DT, DATE_FORMAT).date()

START_TS = datetime.strptime(START_TS, "%H:%M:%S").time()
START_TS = datetime.combine(DT_UTC, START_TS)

DT_UTC_START = START_TS - timedelta(days=START_DT_OFFSET)
TS_UTC_START = datetime.timestamp(DT_UTC_START)

dsf_logger.log_msg(f"DT_UTC_START: {DT_UTC_START}, TS_UTC_START: {TS_UTC_START}", level=20)

if TS_UTC_END is None:
    STOP_TS: datetime.time = datetime.strptime(STOP_TS, "%H:%M:%S").time()
    STOP_TS = datetime.combine(DT_UTC, STOP_TS)

    DT_UTC_END: datetime = STOP_TS - timedelta(days=STOP_DT_OFFSET)
    TS_UTC_END: float = datetime.timestamp(DT_UTC_END)
dsf_logger.log_msg(f"DT_UTC_END: {DT_UTC_END}, TS_UTC_END: {TS_UTC_END}", level=20)

if (TS_UTC_START - TS_UTC_END) > 0:
    dsf_logger.log_msg(f"Start timestamp {TS_UTC_START} is greater than end timestamp {TS_UTC_END}. Please check the configuration.", level=40)
    os._exit(9)

DATA_FOLDER = (
f"{DATA_PATH}/{FEED_NAME}_{DT_UTC.strftime(DATE_FORMAT)}"
)

dsf_logger.log_msg(f"DT_UTC: {DT_UTC.strftime(DATE_FORMAT)}", level=20)

dsf_logger.log_msg(f"DATA_FOLDER: {DATA_FOLDER}", level=20)

DATA_FILE = f"{DATA_FOLDER}/{FEED_NAME}_{DT_UTC.strftime(DATE_FORMAT)}.par"

dsf_logger.log_msg(f"DATA_FILE: {DATA_FILE}", level=20)
pathlib.Path(DATA_FOLDER).mkdir(parents=True, exist_ok=True)
APP_ID = FEED_NAME
LOG_FILE_NAME = f"{DATA_FILE}_log.json"
dsf_logger.log_msg(f"LOG_FILE_NAME: {LOG_FILE_NAME}", level=20)

METADATA_OUTPUT_PATH = str(CONFIG.get("METADATA_OUTPUT_PATH", "")).strip()
METADATA_DIR = METADATA_OUTPUT_PATH if METADATA_OUTPUT_PATH else DATA_FOLDER
METADATA_FILE_SUFFIX = str(CONFIG.get("METADATA_FILE_SUFFIX", "_metadata.txt")).strip()
METADATA_FILE = os.path.join(METADATA_DIR, f"{FEED_NAME}_{ASOF_DT}{METADATA_FILE_SUFFIX}")
VALIDATION_LOG_FILE = os.path.join(DATA_PATH, f"{FEED_NAME}_status_messages_validation.log")
dsf_logger.log_msg(
    f"\n  Metadata file (File 1) : {METADATA_FILE}"
    f"\n  Validation log (File 2): {VALIDATION_LOG_FILE}",
    level=20
)

dsf_logger.log_msg(
    f"\n--- Config Summary ---"
    f"\n  Feed          : {FEED_NAME} | Mandator: {DSF_MANDATOR} | ASOF_DT: {ASOF_DT}"
    f"\n  Kafka         : topic={INFLOW_TOPIC} | broker={KAFKA_BROKER}"
    f"\n  Format        : input={INPUT_FORMAT} | output={OUTPUT_FORMAT} | sep='{SEPERATOR}'"
    f"\n  Time window   : {DT_UTC_START} → {DT_UTC_END}"
    f"\n  Validation    : producer='{PRODUCER_FILTER}' | ALLOW_NO_DATA={ALLOW_NO_DATA} | VALIDATE_TOPIC_MANDATOR={VALIDATE_TOPIC_MANDATOR}"
    f"\n  Retry         : max_listen={MAX_LISTEN_DURATION_HOURS}h | retry_wait={RETRY_WAIT_SECONDS}s | WAIT_FOR_SUBMIT={WAIT_FOR_SUBMIT}"
    f"\n  Output file   : {DATA_FILE}"
    f"\n----------------------",
    level=20
)

restart = 'no'
CHK_DATA_FILE = pathlib.Path(DATA_FILE)
FIRST_DATA_FILE = DATA_FILE + ".1"
if (CHK_DATA_FILE.is_file()):
    os.rename(CHK_DATA_FILE, FIRST_DATA_FILE)
    restart = 'yes'
    dsf_logger.log_msg(f"First run for this date detected, data in {FIRST_DATA_FILE}", level=20)
    

for key, value in logging.root.manager.loggerDict.items():
    if ('kafka' in key) or ('urllib3' in key) or ('charset_normalizer' in key):
        logging.getLogger(key).setLevel(logging.ERROR)
        

try:
    consumer: kafka.KafkaConsumer = kafka.KafkaConsumer(
        auto_offset_reset = 'latest'
        ,bootstrap_servers=KAFKA_BROKER
        ,isolation_level="read_committed"
        ,security_protocol="PLAINTEXT",
        client_id=APP_ID
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
    dsf_logger.log_error_msg(msg=f"Unexpected error at kafka.KafkaConsumer", err=err)
    

if VALIDATE_TOPIC_MANDATOR == "YES":
    # check topic
    if not INFLOW_TOPIC.endswith(DSF_MANDATOR):
        dsf_logger.log_msg(f"INFLOW_TOPIC {INFLOW_TOPIC} does not end with mandator {DSF_MANDATOR}", level=40)
        os._exit(9)
    else:
        dsf_logger.log_msg(f"INFLOW_TOPIC {INFLOW_TOPIC} validation passed for mandator {DSF_MANDATOR}", level=20)

topic = INFLOW_TOPIC
    

if consumer.bootstrap_connected():
    dsf_logger.log_msg(f"Connection successful", level=20)
else:
    dsf_logger.log_msg(f"Connection unsuccessful", level=40)
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


consumer.assign(partition_request)
partitions: Set[kafka.TopicPartition] = assert_set(consumer.assignment())
dsf_logger.log_msg(f"partitions ({type(partitions)}): {partitions}", level=20)

partition_stop_ts: Dict[kafka.TopicPartition, int] = {
    part: int(TS_UTC_END * 1000) for part in partitions
}

all_stop_offset_ts: Dict[
    kafka.TopicPartition, Tuple[int, int]
] = assert_dict(consumer.offsets_for_times(partition_stop_ts))

dsf_logger.log_msg(
    f"stop offset timestamp: {all_stop_offset_ts}", level=20
)

partition_start_ts: Dict[kafka.TopicPartition, int] = {
    part: int(TS_UTC_START * 1000) for part in partitions
}

dsf_logger.log_msg(f"partition_start_ts: {dict(partition_start_ts)}", level=20)


all_start_offset_ts: Dict[
    kafka.TopicPartition, Tuple[int, int]
] = assert_dict(consumer.offsets_for_times(partition_start_ts))

dsf_logger.log_msg(
    f"all_start_offset_ts: {all_start_offset_ts}", level=20
)


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
            except Exception as e:
                dsf_logger.log_msg(
                    f"INFO: Setting previous schema to latest as previous not found for {topic}: {e}", level=20
                )
                pversion = lversion
                pschema_id = lschema_id
                pavschema = lavschema
                
        else:
            dsf_logger.log_msg(
                    f"INFO: Setting previous schema to latest only 1 version {topic}", level=20
                )
            pversion = lversion
            pschema_id = lschema_id
            pavschema = lavschema
            
        # default is to use latest
        avschema = lavschema
        avflag = 0
        
        schema_str = str(pavschema).replace('<', '[').replace('>', ']')
        dsf_logger.log_msg(
            f"Schema loaded — latest version: {lversion} (id: {lschema_id}, name: {lavschema.get('name')}/{lavschema.get('namespace')}), "
            f"previous version: {pversion} (id: {pschema_id}, name: {pavschema.get('name')}/{pavschema.get('namespace')})",
            level=20
        )
        dsf_logger.log_msg(f"Previous schema (XML-safe): {schema_str}", level=20)
        
    except Exception as e:
        dsf_logger.log_msg(f"Error retrieving schema for {topic}: {e}", level=40)
        os._exit(1)
        
elif (INPUT_FORMAT == "AVRO"):
    try:
        with open(AVRO_SCHEMA, 'r') as schema_file:
            jschema = json.loads(schema_file.read())
            avschema = fastavro.schema.parse_schema(jschema)
    except Exception as e:
        dsf_logger.log_msg(f"schema file {AVRO_SCHEMA} is not valid: {e}", level=40)
        os._exit(1)
        
# AVRO check for fields containing decimal, will be converted to default later if dec default defined

if (INPUT_FORMAT == "AVRO" and DECIMAL_CONV == "YES"):
    if (AVRO_FILE == "LATEST"):
        ldecattrs = get_decimal_fields(lavschema)
        pdecattrs = get_decimal_fields(pavschema)
        
        dsf_logger.log_msg(f"ldecattrs is {ldecattrs}", level=20)
        dsf_logger.log_msg(f"pdecattrs is {pdecattrs}", level=20)
    else:
        ldecattrs = get_decimal_fields(avschema)
        pdecattrs = ldecattrs
        


collected_messages = []

for partition, start_offset_ts in all_start_offset_ts.items():
    
    # exit if start not defined
    
    if not start_offset_ts:
        dsf_logger.log_msg(f"No data on the Kafka Topic for partition {partition.partition}", level=30)
        continue
    
    # reassign each partition one by one to process reliably
    consumer.assign([])
    try:
        consumer.assign([kafka.TopicPartition(topic, partition.partition)])
        dsf_logger.log_msg(f"Assigned partition {partition.partition}", level=20)
    except Exception as e:
        dsf_logger.log_msg(f"Assign partition {partition.partition} failed{e}({type(e)})", level=40)
        os._exit(1)
        
    dsf_logger.log_msg(f"Reassign {partition.partition} successfully", level=20)

    # define vars

    start_offset: int
    start_ts: int
    stop_offset: int
    stop_ts: int
    
    # get start offset and ts for partition and check in range, important as we have earliest set
    (start_offset, start_ts) = assert_tuple(start_offset_ts)
    dsf_logger.log_msg(f"start_ts {start_ts} {TS_UTC_START}", level=20)
    if (start_ts >= TS_UTC_START * 1000) and (start_ts < TS_UTC_END * 1000):
        
        dsf_logger.log_msg(f"start_offset {start_offset}, start_ts {start_ts}", level=20)
        dsf_logger.log_msg(f"partition: {partition.partition}", level=20)
        dsf_logger.log_msg(f"first message found for start {start_offset}: {ts_to_str(start_ts/1000)}", level=20)
        
    else:
        dsf_logger.log_msg(f"Start timestamp not found in range, no valid data on the Kafka Topic for partition {partition.partition}", level=30)
        continue
    
    # Get stop offset and t
    # s for partition, or set to last offset
    if all_stop_offset_ts.get(partition):
        end_offset: int
        end_ts: int
        (end_offset, end_ts) = assert_tuple(
            all_stop_offset_ts.get(partition)
        )
        dsf_logger.log_msg(f"last candidate message found for stop point with UTC timestamp as follows, last message before {end_offset}: {ts_to_str(end_ts/1000)}", level=20)
        end_offset -= 1
    else:
        # no stop timestamp found - use timestamp based stopping instead of offset
        end_offset: int = assert_int(assert_dict(consumer.end_offsets([partition])).get(partition) )
        end_offset -= 1
        dsf_logger.log_msg(f"Last candidate message for stop point has offset {end_offset}", level=20)
        
        
    committed = consumer.committed(partition, metadata=True)
    committed_offset = get_committed_offset(committed, start_offset - 1)
    dsf_logger.log_msg(f"last committed offset: {committed_offset}", level=20)

    if (committed_offset < start_offset and CONSUMED_COMMITTED == "No"):
        dsf_logger.log_msg(f"committed {committed_offset}, before start_offset {start_offset}", level=20)
        seek_offset = start_offset
    elif (committed_offset >= start_offset and committed_offset <= end_offset and CONSUMED_COMMITTED == "No"):
        dsf_logger.log_msg(f"committed {committed_offset}, between start_offset {start_offset} and end_offset {end_offset}", level=20)
        if committed_offset + 1 <= end_offset:
            seek_offset = committed_offset + 1
        else:
            dsf_logger.log_msg(f"seek_offset +1 exceeds end offset", level=30)
            continue
    elif CONSUMED_COMMITTED == "Yes":
        seek_offset = start_offset
    else:
        dsf_logger.log_msg(f"No valid uncommitted data on the kafka topic for partition {partition} between {start_offset} and {end_offset}", level=30)
        continue
    
    
    try:
        consumer.seek(partition, seek_offset)
        dsf_logger.log_msg(f"Seeked to offset {seek_offset} for partition {partition.partition}", level=20)
    except Exception as e:
        dsf_logger.log_msg(f"Seek to offset {seek_offset} for partition {partition.partition} failed: {e}({type(e)})", level=40)
        os._exit(1)
    dsf_logger.log_msg(f"Expected: {end_offset - seek_offset + 1} messages to consume for partition {partition.partition}", level=20)
    
    current_offset = consumer.position(partition)
    dsf_logger.log_msg(f"Current offset after seek {current_offset} for partition {partition.partition}", level=20)
    
    msg_num = 0
    for msg in consumer:
        
        if (msg.offset < seek_offset):
            dsf_logger.log_msg(f"Skipping message with offset {msg.offset} as it is before seek offset {seek_offset}", level=20)
            continue
        
        msg_num += 1
        
        omsg_ts = msg.timestamp
        omsg_off = msg.offset
        
        original_msg = None
        
        if (INPUT_FORMAT == "AVRO"):
            msg_io = io.BytesIO(msg.value)
            if (AVRO_FILE == "LATEST"):
                if len(msg.value) < 5:
                    dsf_logger.log_msg(f"Skipping corrupt message at offset {omsg_off} — too short ({len(msg.value)} bytes)", level=30)
                    continue
                msg_io.seek(1)
                
                mschema_by = msg_io.read(4)
                schema_id = int.from_bytes(mschema_by, byteorder='big')
                
                if (schema_id != oschema_id):
                    dsf_logger.log_msg(f"schema id {schema_id}", level=20)
                    oschema_id = schema_id
                    
                try:
                    if (schema_id == lschema_id):
                        msg = fastavro.schemaless_reader(msg_io, lavschema)
                        original_msg = dict(msg)
                        if (DECIMAL_CONV == "YES"):
                            decattrs = ldecattrs
                    elif (schema_id == pschema_id):
                        msg = fastavro.schemaless_reader(msg_io, pavschema)
                        original_msg = dict(msg)
                        if (DECIMAL_CONV == "YES"):
                            decattrs = pdecattrs
                    else:
                        raise ValueError(f"unable to find matching schema for message offset {omsg_off}")
                except Exception as e:
                    dsf_logger.log_msg(f"Error reading avro message with schema id {schema_id} for message offset {omsg_off}: {e}", level=40)
                    os._exit(1)
            else:
                msg_io.seek(5)
                try:
                    msg = fastavro.schemaless_reader(msg_io, avschema)
                    original_msg = dict(msg)
                except Exception as e:
                    dsf_logger.log_msg(f"unable to match schema with data {e}", level=40)
                    os._exit(1)
                    
                if (DECIMAL_CONV == "YES"):
                    for decf in ldecattrs:
                        if (msg[decf] is not None):
                            msg[decf] = f"{Decimal(msg[decf]):.{DECIMAL_SCALE}f}"
            
            if original_msg:
                collected_messages.append(original_msg)
            
            if (len(AVRO_COLUMNS) > 1):
                msg = {column: msg[column] for column in AVRO_COLUMNS}
                
        elif(INPUT_FORMAT == "JSON"):
            try:
                original_msg = json.loads(msg.value.decode(DSF_CHARMAP, 'ignore'))
                collected_messages.append(original_msg)
            except Exception as e:
                dsf_logger.log_msg(
                    f"Failed to decode JSON message at offset {omsg_off} for partition {partition.partition} — skipping: {e}",
                    level=30
                )
            
        
        if (omsg_off >= end_offset):
            dsf_logger.log_msg(f"Reached end offset: offset={omsg_off}, collected {msg_num} messages for partition {partition.partition}", level=20)
            break
        
    dsf_logger.log_msg(f"Finished reading partition {partition.partition}, total messages collected: {msg_num}", level=20)
    
    
dsf_logger.log_msg(f"starting validation of collected messages", level=20) 
dsf_logger.log_msg(f"Total messages collected from all partitions: {len(collected_messages)}", level=20) 

is_valid, max_run_id, missing_instances, validated_df = validate_instance_and_get_max_runid(
    collected_messages, DSF_MANDATOR, PRODUCER_FILTER, ASOF_DT, INSTANCE_VALIDATION_MODE
)


if not is_valid:
    if missing_instances:
        dsf_logger.log_msg(f"Data extraction aborted: Instance Validation failed. Missing instances: {missing_instances}", level=40)
    else:
        dsf_logger.log_msg(f"Data Extraction aborted: Validation failed (no matching data or other validation error).", level=40)
    if ALLOW_NO_DATA != "YES":
        log_topic_match_report(collected_messages, DSF_MANDATOR, PRODUCER_FILTER, ASOF_DT, INFLOW_TOPIC)
        dsf_logger.log_msg(f"Exiting (code 1): validation failed and ALLOW_NO_DATA is not YES — no retry will be attempted", level=40)
        write_failure_log_entry(
            VALIDATION_LOG_FILE, DSF_MANDATOR, PRODUCER_FILTER, FEED_NAME,
            "INSTANCE_VALIDATION_FAILED_NO_RETRY", max_run_id,
            missing_instances if missing_instances else [],
            validated_df, SEPERATOR, username
        )
        os._exit(1)
    dsf_logger.log_msg(f"ALLOW_NO_DATA is set to YES, will enter wait-and-listen mode", level=20)

if is_valid:
    dsf_logger.log_msg(f"Validation passed; Writing data for, max run id is {max_run_id}", level=20)
    write_validation_metadata(
        METADATA_FILE, VALIDATION_LOG_FILE, DSF_MANDATOR, PRODUCER_FILTER,
        FEED_NAME, max_run_id, validated_df, SEPERATOR, username
    )
    dsf_logger.log_msg(f"Output data file       : {DATA_FILE}", level=20)

    # re-read partitions and write only validated messages
    for partition, start_offset_ts in all_start_offset_ts.items():
        
        if not start_offset_ts:
            dsf_logger.log_msg(f"No data on the Kafka Topic for partition {partition.partition}", level=30)
            continue
        
        # reassign each partition one by one to process reliably
        consumer.assign([])
        try:
            consumer.assign([kafka.TopicPartition(topic, partition.partition)])
            dsf_logger.log_msg(f"Re-Assigned partition {partition.partition} for writing", level=20)
        except Exception as e:
            dsf_logger.log_msg(f"Re-Assigning partition {partition.partition} failed{e}({type(e)})", level=40)
            os._exit(1)
            
        (start_offset, start_ts) = assert_tuple(start_offset_ts)
        
        if not ((start_ts >= TS_UTC_START * 1000) and (start_ts < TS_UTC_END * 1000)):
            continue
        
        if all_stop_offset_ts.get(partition):
            (end_offset, end_ts) = assert_tuple(
                all_stop_offset_ts.get(partition)
            )
            end_offset -= 1
        else:
            end_offset: int = assert_int(assert_dict(consumer.end_offsets([partition])).get(partition))
            end_offset -= 1
            
        # capture snapshot of end_offset to prevent infinite loop if new message arrives
        snapshot_end_offset = end_offset
        dsf_logger.log_msg(f"snapshot_end_offset for partition {partition.partition} is {snapshot_end_offset}", level=20)
        
        # Get last committed offset to know where to start
        committed = consumer.committed(partition, metadata=True)
        committed_offset = get_committed_offset(committed, start_offset - 1)

        # determine seek offset based on committed and config
        if (committed_offset < start_offset and CONSUMED_COMMITTED == "No"):
            seek_offset = start_offset
        elif (committed_offset >= start_offset and committed_offset <= snapshot_end_offset and CONSUMED_COMMITTED == "No"):
            seek_offset = committed_offset + 1
        elif CONSUMED_COMMITTED == "Yes":
            seek_offset = start_offset
        else:
            continue
        
        try:
            consumer.seek(partition, seek_offset)
        except Exception as e:
            exit_with_error(f"seek failed  {e}({type(e)}) error")
        
        
        with open(DATA_FILE, 'a', encoding=DSF_CHARMAP) as f:
            dsf_logger.log_msg(f"writing validated data to {DATA_FILE}", level=20)
            
            cnt =1
            msg_num=0
            written_num=0
            
            # use poll() with timeout instead of iterator to prevent hanging
            current_offset=seek_offset
            while current_offset <= snapshot_end_offset:
                msg_dict = consumer.poll(timeout_ms=500, max_records=100)
                
                if not msg_dict:
                    dsf_logger.log_msg(f"No more messages available (poll timeout), stopping at offset {current_offset-1}", level=30)
                    break
                
                for tp, messages in msg_dict.items():
                    for msg in messages:
                        
                        # skip messages before start offset
                        if (msg.offset < start_offset):
                            continue
                        
                        # update current position
                        
                        current_offset = msg.offset + 1
                        
                        # save control info
                        omsg_ts = msg.timestamp
                        omsg_off = msg.offset
                        
                        # check if we have passed the snapshot end offset
                        if (omsg_off > snapshot_end_offset):
                            dsf_logger.log_msg(f"Passed snapshot end offset {snapshot_end_offset}, stopping", level=20)
                            break
                        
                        cnt += 1
                        msg_num +=1
                        
                        # Decode messages
                        
                        msg_data = None
                        should_write = False
                        
                        # preprocessing if AVRO, returns dict
                        
                        if (INPUT_FORMAT == "AVRO"):
                            
                            msg_io = io.BytesIO(msg.value)
                            if (AVRO_FILE == "LATEST"):
                                msg_io.seek(1)
                                mschema_by = msg_io.read(4)
                                schema_id = int.from_bytes(mschema_by, byteorder='big')
                                
                                try:
                                    if (schema_id==lschema_id):
                                        msg_data=fastavro.schemaless_reader(msg_io, lavschema)
                                        if (DECIMAL_CONV == "YES"):
                                            decattrs = ldecattrs
                                    elif (schema_id == pschema_id):
                                        msg_data = fastavro.schemaless_reader(msg_io, pavschema)
                                        if(DECIMAL_CONV=="YES"):
                                            decattrs = pdecattrs
                                    else:
                                        continue
                                except Exception  as e:
                                    dsf_logger.log_msg(f"schema read error at offset {omsg_off}: {e}", level=30)
                                    continue
                            else:
                                msg_io.seek(5)
                                
                                try:
                                    msg_data = fastavro.schemaless_reader(msg_io, avschema)
                                except Exception as e:
                                    dsf_logger.log_msg(f"schema read error at offset {omsg_off}: {e}", level=30)
                                    continue
                            
                            # check if this message matched our filter criteria
                            
                            if msg_data:
                                msg_mandator, msg_producer, msg_business_date, msg_run_id = extract_message_filter_fields(msg_data)
                                    
                                # only write if it matched our filter and max run id
                                if matches_filter(msg_mandator, msg_producer, msg_business_date, msg_run_id,
                                                  DSF_MANDATOR, PRODUCER_FILTER, ASOF_DT, max_run_id):
                                    should_write = True

                                    # apply decimal conversion
                                    if(DECIMAL_CONV == "YES"):
                                        for decf in ldecattrs:
                                            if (msg_data[decf] is not None):
                                                msg_data[decf] = f"{Decimal(msg_data[decf]):.{DECIMAL_SCALE}f}"
                                            
                                    # apply column filter
                                    if (len(AVRO_COLUMNS) > 1):
                                        msg_data = {column: msg_data[column] for column in AVRO_COLUMNS if column in msg_data}
                        elif(INPUT_FORMAT == "JSON"):
                            try:
                                msg_data = json.loads(msg.value.decode(DSF_CHARMAP, 'ignore'))
                                msg_mandator, msg_producer, msg_business_date, msg_run_id = extract_message_filter_fields(msg_data)
                                    
                                # only write if it matched our filter and max run id
                                if matches_filter(msg_mandator, msg_producer, msg_business_date, msg_run_id,
                                                  DSF_MANDATOR, PRODUCER_FILTER, ASOF_DT, max_run_id):
                                    should_write = True
                            except Exception as e:
                                dsf_logger.log_msg(f"JSON decode/filter error at offset {omsg_off}: {e}", level=30)
                                continue
                            
                        
                        if not should_write:
                            continue
                        
                        # format output
                        output_msg = format_output_message(msg_data, msg.value, INPUT_FORMAT, OUTPUT_FORMAT, SEPERATOR, DSF_CHARMAP)
                        
                        # write to file
                        f.write(output_msg)
                        written_num += 1
                        
                        # check if at finish or commit point reached
                        
                        if (cnt == COMMIT_CNT or omsg_off >= snapshot_end_offset):
                            dsf_logger.log_msg(f"Flush and commit at {omsg_off}", level=20)
                            f.flush()
                            os.fsync(f.fileno())
                            
                            consumer.commit(
                                {partition: OffsetAndMetadata(omsg_off, omsg_ts, -1)}
                            )
                            
                            dsf_logger.log_msg(f"Committed message number {msg_num}", level=20)
                            DATA_CONSUME = "Yes"
                            cnt = 1
                            
                            # Break outer while loop if we have reached the end
                            
                            if (omsg_off >= snapshot_end_offset):
                                break
                    # break from while loop if we have reached the end
                    if (omsg_off >= snapshot_end_offset):
                        break
                
                # break from while loop if we have reached the end
                if current_offset > snapshot_end_offset:
                    break
                
            # mark partition as successfully processed
            # This handles cases where messages were processed but none matched filters
            if DATA_CONSUME != "Yes":
                DATA_CONSUME = "Yes"

            if written_num == 0:
                dsf_logger.log_msg(
                    f"Write pass — partition {partition.partition}: {msg_num} read, 0 matched filter — nothing written",
                    level=30
                )
            else:
                dsf_logger.log_msg(
                    f"Write pass — partition {partition.partition}: {msg_num} read, {written_num} written to {DATA_FILE}",
                    level=20
                )

        dsf_logger.log_msg(f"Closed file for partition {partition.partition}", level=20)
        
        # Check if data was consumed or if retry mode is needed
        if (DATA_CONSUME != "Yes" and ALLOW_NO_DATA== "NO"):
            dsf_logger.log_msg(f"No data consumed from any partition !!!", level=40)
            os._exit(1)
if not is_valid and ALLOW_NO_DATA == "YES":
            
    LATEST_DT_UTC_END = datetime.now() + timedelta(minutes=MAX_LISTEN_DURATION_MINUTES)
    LATEST_TS_UTC_END = datetime.timestamp(LATEST_DT_UTC_END)
    
    dsf_logger.log_msg(
        f"No initial data found. Entering wait-and-retry mode.\n"
        f"  Kafka search window     : {DT_UTC_START} UTC → {LATEST_DT_UTC_END.strftime('%Y-%m-%d %H:%M:%S')} UTC (extends forward each retry, up to polling deadline)\n"
        f"  Polling deadline (wall clock): {LATEST_DT_UTC_END.strftime('%Y-%m-%d %H:%M:%S')} UTC "
        f"({MAX_LISTEN_DURATION_HOURS}h / {MAX_LISTEN_DURATION_MINUTES} min from now)",
        level=20
    )
            
    # validate retry window before entering loop
    if (LATEST_TS_UTC_END - TS_UTC_START <= 0):
        dsf_logger.log_msg(
            f"Invalid retry window: end time {LATEST_DT_UTC_END} is not after start time {DT_UTC_START}"
            ,level=40
        )
        os._exit(1)

    # repeat every 60 seconds until time out
    retry_count = 0

    while datetime.timestamp(datetime.now()) < LATEST_TS_UTC_END:

        retry_collected_messages = []
        found_data = False
                
        new_partition_start_ts = {part: int(TS_UTC_START * 1000) for part in partitions}
        new_partition_stop_ts = {part: int(LATEST_TS_UTC_END * 1000) for part in partitions}
        
        new_all_start_offset_ts = assert_dict(consumer.offsets_for_times(new_partition_start_ts))
        new_all_stop_offset_ts = assert_dict(consumer.offsets_for_times(new_partition_stop_ts))
        
        # First pass: collect all messages for validation
        
        dsf_logger.log_msg(f"=== Retry mode: collecting messages for validation ==== ", level=20)
        
        for partition, start_offset_ts in new_all_start_offset_ts.items():
            if not start_offset_ts:
                continue
            
            consumer.assign([kafka.TopicPartition(topic, partition.partition)])
            
            try:
                dsf_logger.log_msg(f"processing partition: {partition}", level=20)
                (start_offset, start_ts) = assert_tuple(start_offset_ts)
                dsf_logger.log_msg(f"start offset: {start_offset}, start timestamp: {start_ts} is before query start {TS_UTC_START * 1000}", level=20)
                
                # skip partition if start timestamp is before query window
                
                if start_ts < TS_UTC_START * 1000:
                    dsf_logger.log_msg(f"Skipping partition {partition.partition}: start timestamp {start_ts} is before query start {TS_UTC_START * 1000}", level =20)
                    continue
                
            
                end_offset_ts = new_all_stop_offset_ts.get(partition)
                        
                if end_offset_ts:
                    (end_offset, end_ts) = assert_tuple(end_offset_ts)
                    end_offset -= 1
                    dsf_logger.log_msg(f"End offset: {end_offset}, end timestamp: {end_ts}", level=20)
                else:
                    end_offset = assert_int(consumer.end_offsets([partition])[partition]) -1
                    dsf_logger.log_msg(f"End Offset (no end time stamp): {end_offset}", level= 20)
                    
                # skip partition if start timestamp is after extended window
                
                if start_ts > LATEST_TS_UTC_END * 1000:
                    dsf_logger.log_msg(f"Skipping partition {partition.partition}: start timestamp {start_ts} is after extended end {LATEST_TS_UTC_END * 1000}", level=20)
                    continue
                
                consumer.seek(partition, start_offset)
                dsf_logger.log_msg(f"seek to start offset: {start_offset}", level = 20)
                
                # collect messages from this partition with timeout protection
                
                message_count = 0
                matching_count = 0
                current_offset = start_offset
                        
                # use poll with timeout instead of iterator to avoid hanging
                while current_offset <= end_offset:
                    msg_dict = consumer.poll(timeout_ms=10000, max_records = 100)
                    
                    if not msg_dict:
                        # No messages received within timeout
                        dsf_logger.log_msg(f"No more messages in partition {partition.partition} (poll timeout)", level =20)
                        break
                    
                    for tp, messages in msg_dict.items():
                        for msg in messages:
                            if msg.offset > end_offset:
                                dsf_logger.log_msg(f"reached end offset {end_offset} at message offset {msg.offset}", level=20)
                                current_offset = end_offset + 1
                                break
                            
                            current_offset = msg.offset + 1
                            message_count += 1
                                    
                            try:
                                if INPUT_FORMAT == "AVRO":
                                    msg_io = io.BytesIO(msg.value)
                                    if AVRO_FILE == "LATEST":
                                        if len(msg.value) < 5:
                                            dsf_logger.log_msg(f"retry mode: skipping corrupt message at offset {msg.offset} — too short ({len(msg.value)} bytes)", level=30)
                                            continue

                                        msg_io.seek(1)
                                        schema_id = int.from_bytes(msg_io.read(4), byteorder='big')
                                        
                                        if schema_id == lschema_id:
                                            msg_data = fastavro.schemaless_reader(msg_io, lavschema)
                                            retry_collected_messages.append(msg_data)
                                            matching_count += 1
                                        elif schema_id == pschema_id:
                                            msg_data = fastavro.schemaless_reader(msg_io, pavschema)
                                            retry_collected_messages.append(msg_data)
                                            matching_count += 1
                                    else:
                                        msg_io.seek(5)
                                        msg_data = fastavro.schemaless_reader(msg_io, avschema)
                                        retry_collected_messages.append(msg_data)
                                        matching_count += 1
                                elif INPUT_FORMAT == "JSON":
                                    msg_data = json.loads(msg.value.decode(DSF_CHARMAP, 'ignore'))
                                    retry_collected_messages.append(msg_data)
                                    matching_count += 1
                            except Exception as err:
                                dsf_logger.log_msg(f"Retry mode decode error at offset {msg.offset}: {err}", level=30)
                                continue
                # log collecteion summary for this partition
                dsf_logger.log_msg(f"Partition {partition.partition}: processed {message_count} messages, {matching_count} matched filter", level=20)
                        
            except Exception as err:
                dsf_logger.log_msg(f"Error during rety collection: {err}", level=40)
                        
        # Validate collected messages
        
        dsf_logger.log_msg(f"Retry mode: Validating {len(retry_collected_messages)} collected messages", level=20)
        
        is_valid, max_run_id, missing_instances, validated_df = validate_instance_and_get_max_runid(
            retry_collected_messages,
            DSF_MANDATOR,
            PRODUCER_FILTER,
            ASOF_DT,
            INSTANCE_VALIDATION_MODE
        )
                
        if is_valid:
            dsf_logger.log_msg(f"Retry mode: validation passed", level=20)
            write_validation_metadata(
                METADATA_FILE, VALIDATION_LOG_FILE, DSF_MANDATOR, PRODUCER_FILTER,
                FEED_NAME, max_run_id, validated_df, SEPERATOR, username
            )
            dsf_logger.log_msg(f"Output data file       : {DATA_FILE}", level=20)

            # Mark as successful immediately after validation passed
            DATA_CONSUME = "Yes"
            found_data = True

            # second pass: write only validated messages
            
            for partition, start_offset_ts in new_all_start_offset_ts.items():
                if not start_offset_ts:
                    continue
                
                consumer.assign(
                    [kafka.TopicPartition(topic, partition.partition)]
                )
                (start_offset, start_ts) = assert_tuple(start_offset_ts)
                
                if start_ts < TS_UTC_START * 1000:
                    continue
                
                end_offset_ts = new_all_stop_offset_ts.get(partition)
                if end_offset_ts:
                    (end_offset, end_ts) = assert_tuple(end_offset_ts)
                    end_offset -= 1
                else:
                    end_offset = assert_int(consumer.end_offsets([partition])[partition]) - 1
                    
                
                # capture the end offset before starting to consume - prevent infinite loop
                # if new messages keep arriving
                        
                snapshot_end_offset = end_offset
                dsf_logger.log_msg(f"Retry mode: snapshot end offset for partition {partition.partition}: {snapshot_end_offset}", level=20)
                
                consumer.seek(partition, start_offset)
                
                with open(DATA_FILE, 'a', encoding=DSF_CHARMAP) as f:
                    dsf_logger.log_msg(f" Retry mode writing validated data from partition {partition.partition}", level=20)
                    
                    cnt = 1
                    msg_num = 0
                    skipped = 0
                    
                    # use poll with timeout instead of iterator to prevent hanging
                            
                    current_offset = start_offset
                    while current_offset <= snapshot_end_offset:
                        msg_dict = consumer.poll(timeout_ms=10000, max_records=100)
                        
                        if not msg_dict:
                            dsf_logger.log_msg(f"Retry mode: no more messages available (poll timeout), stopping at offset {current_offset -1 }", level=30)
                            break
                        
                        for tp, messages in msg_dict.items():
                            for msg in messages:
                                # skip messages before start offset
                                if msg.offset < start_offset:
                                    continue
                                
                                # update current position
                                current_offset = msg.offset + 1
                                
                                # use snapshot_end_offset to prevent infinite loop with arriving messages
                                
                                if msg.offset > snapshot_end_offset:
                                    dsf_logger.log_msg(f"Retry mode: Reached snapshot end offset at {msg.offset}", level=20)
                                    break
                                
                                omsg_ts = msg.timestamp
                                omsg_off = msg.offset
                                
                                try:
                                    msg_data = None
                                    should_write = False
                                    
                                    if INPUT_FORMAT == "AVRO":
                                        msg_io = io.BytesIO(msg.value)
                                        if AVRO_FILE == "LATEST":
                                            if len(msg.value) < 5:
                                                dsf_logger.log_msg(f"retry write pass: skipping corrupt message at offset {msg.offset} — too short ({len(msg.value)} bytes)", level=30)
                                                skipped += 1
                                                continue
                                            
                                            msg_io.seek(1)
                                            schema_id = int.from_bytes(msg_io.read(4), byteorder='big')
                                            
                                            if schema_id == lschema_id:
                                                msg_data = fastavro.schemaless_reader(msg_io, lavschema)
                                                if (DECIMAL_CONV == "YES"):
                                                    decattrs = ldecattrs
                                            elif schema_id == pschema_id:
                                                msg_data = fastavro.schemaless_reader(msg_io, pavschema)
                                                if (DECIMAL_CONV == "YES"):
                                                    decattrs = pdecattrs
                                            else:
                                                skipped += 1
                                                continue
                                        else:
                                            msg_io.seek(5)
                                            msg_data = fastavro.schemaless_reader(msg_io, avschema)
                                    
                                        # check if mesages matched our fileter criteris
                                        
                                        if msg_data:
                                            msg_mandator, msg_producer, msg_business_date, msg_run_id = extract_message_filter_fields(msg_data)
                                                
                                            if matches_filter(msg_mandator, msg_producer, msg_business_date, msg_run_id,
                                                              DSF_MANDATOR, PRODUCER_FILTER, ASOF_DT, max_run_id):
                                                should_write = True

                                                if DECIMAL_CONV == "YES":
                                                    for decf in ldecattrs:
                                                        if msg_data.get(decf) is not None:
                                                            msg_data[decf] = f"{Decimal(msg_data[decf]):.{DECIMAL_SCALE}f}"
                                                            
                                                if len(AVRO_COLUMNS) > 1:
                                                    msg_data = {k: msg_data[k] for k in AVRO_COLUMNS if k in msg_data}
                                    
                                
                                
                                    elif INPUT_FORMAT == "JSON":
                                        msg_data = json.loads(msg.value.decode(DSF_CHARMAP, 'ignore'))
                                        msg_mandator, msg_producer, msg_business_date, msg_run_id = extract_message_filter_fields(msg_data)
                                            
                                        if matches_filter(msg_mandator, msg_producer, msg_business_date, msg_run_id,
                                                          DSF_MANDATOR, PRODUCER_FILTER, ASOF_DT, max_run_id):
                                            should_write = True
                                    if not should_write:
                                        continue
                                    
                                    # format output
                                    output_msg = format_output_message(msg_data, msg.value, INPUT_FORMAT, OUTPUT_FORMAT, SEPERATOR, DSF_CHARMAP)
                                            
                                    
                                    f.write(output_msg)
                                    cnt += 1
                                    msg_num += 1
                                    
                                    if(cnt == COMMIT_CNT or omsg_off >= snapshot_end_offset):
                                        dsf_logger.log_msg(f"retry mode: flush and commit at {omsg_off}", level=20)
                                        f.flush()
                                        os.fsync(f.fileno())
                                        consumer.commit({partition: OffsetAndMetadata(omsg_off, omsg_ts, -1)})
                                        dsf_logger.log_msg(f"Retry mode: committed at offset {omsg_off}", level=20)
                                        DATA_CONSUME = "Yes"
                                        cnt = 1
                                        found_data = True
                                        
                                        if (omsg_off >= snapshot_end_offset):
                                            dsf_logger.log_msg(f"retry reached snapshot end offset, breaking", level=20)
                                            break
                                except Exception as err:
                                    dsf_logger.log_msg(f"retry mode write error at offset {omsg_off}: {err}", level = 40)
                                    skipped += 1
                                    continue
                            # Break from for loop if we have reached the end
                            if ( omsg_off >= snapshot_end_offset):
                                break
                            
                        # Break from the while lpp if we have reached the end
                        if current_offset > snapshot_end_offset:
                            break
                        
                    dsf_logger.log_msg(f"retry mode: partition {partition.partition} - written {msg_num}, skipped: {skipped}", level=20)
                    
            found_data= True
            break # exit retry loop
        else:
            # validation failed
            if missing_instances:
                dsf_logger.log_msg(f"Retry mode: Validation failed - missing instances: {missing_instances}", level=30)
            else:
                dsf_logger.log_msg(f"Retry mode: Validation failed - no matching data or validation error", level=30)
        
        if found_data:
            break
        
        # Calculate remaining time
        retry_count +=1
        time_elapsed_seconds = retry_count * RETRY_WAIT_SECONDS
        time_elapsed_minutes = time_elapsed_seconds / 60.0
        time_remaining_minutes = MAX_LISTEN_DURATION_MINUTES - time_elapsed_minutes
        time_remaining_hours = time_remaining_minutes / 60

        if missing_instances and validated_df is not None and not validated_df.empty and 'status.instanceIndex' in validated_df.columns:
            have_instances = sorted(int(x) for x in validated_df['status.instanceIndex'].unique())
        else:
            have_instances = []

        if time_remaining_minutes > 0:
            dsf_logger.log_msg(
                f"Still waiting... instance(s) {missing_instances} not yet received (attempt {retry_count}) | "
                f"Have: {have_instances} | "
                f"Elapsed: {time_elapsed_minutes:.1f} min | "
                f"Remaining: {time_remaining_minutes:.1f} min | "
                f"Next retry in {RETRY_WAIT_SECONDS}s",
                level=20
            )
        else:
            dsf_logger.log_msg(
                f"Still waiting... instance(s) {missing_instances} not yet received (attempt {retry_count}). "
                f"Have: {have_instances}. Sleeping {RETRY_WAIT_SECONDS}s",
                level=20
            )
        py_time.sleep(RETRY_WAIT_SECONDS)
                    
    
    # Final Summary after retry loop ends
    if DATA_CONSUME != "Yes":
        error_msg_parts = [
            f"Data Extraction failed after extended listening period",
            f"",
            f"Configuration parameters:",
            f" - Mandator: {DSF_MANDATOR}",
            f" - Producer Filter: {PRODUCER_FILTER}",
            f" - Business Date: {ASOF_DT}",
            f" - Topic: {INFLOW_TOPIC}",
            f"",
            f"Time Range Attempted:",
            f" - Start Time: {DT_UTC_START.strftime('%Y-%m-%d %H:%M:%S')} UTC (timestamp: {TS_UTC_START})",
            f" - Initial End Time: {DT_UTC_END.strftime('%Y-%m-%d %H:%M:%S')} UTC (timestamp: {TS_UTC_END})",
            f" - Extended End Time: {LATEST_DT_UTC_END.strftime('%Y-%m-%d %H:%M:%S')} UTC (timestamp: {LATEST_TS_UTC_END})",
            f" - Total Listening Duration: {MAX_LISTEN_DURATION_HOURS}hours ({MAX_LISTEN_DURATION_MINUTES} minutes)",
            f" - Retry Attempts: {retry_count}"

        ]

        # add specific failure reason with detailed analysis

        if 'missing_instances' in locals() and missing_instances:
            # Instance validation failed - show which instances are missing
            required_columns = ['status.instanceIndex', 'status.totalInstances', 'status.reconciliationGroupId']
            if validated_df is not None and not validated_df.empty:
                missing_cols = [col for col in required_columns if col not in validated_df.columns]
                if missing_cols:
                    dsf_logger.log_msg(f"VALIDATION ERROR: Expected columns missing from messages: {missing_cols}", level=40)
                    total_instances = 'N/A'
                    present_indices = []
                else:
                    total_instances = int(validated_df['status.totalInstances'].max())
                    present_indices = sorted(int(x) for x in validated_df['status.instanceIndex'].unique())

                received_data_msgs = (
                    int(validated_df['status.numberOfMessagesPublished'].sum())
                    if 'status.numberOfMessagesPublished' in validated_df.columns else 'N/A'
                )
            else:
                total_instances = 'N/A'
                present_indices = []
                received_data_msgs = 'N/A'

            if INSTANCE_VALIDATION_MODE == "UNIQUE_COUNT":
                expected_range = f"{total_instances} unique instance(s), any index values" if total_instances != 'N/A' else 'N/A'
                publisher_issue = False
            else:
                expected_range = f"indices 0 to {total_instances - 1}" if total_instances != 'N/A' else 'N/A'
                expected_indices = list(range(total_instances)) if total_instances != 'N/A' else []

                # Detect publisher-side issue: right count but wrong indices
                publisher_issue = (
                    total_instances != 'N/A' and
                    len(present_indices) >= total_instances and
                    present_indices != expected_indices
                )

            received_str = f"{len(present_indices)} (indices {present_indices})"

            parts = [
                f"Failure Reason: Incomplete instance set",
                f"- Expected instances   : {total_instances} ({expected_range})",
                f"- Received instances   : {received_str}",
                f"- Missing indices      : {missing_instances}",
                f"- Data messages so far : {received_data_msgs} (from received instances)",
                f"- Max reconciliationGroupId            : {max_run_id if max_run_id is not None else 'N/A'}",
            ]

            if publisher_issue:
                parts.append(
                    f"- WARNING              : Received the correct number of instances ({total_instances}) but with "
                    f"unexpected index(es) {present_indices}. Expected 0-based index(es) {expected_indices}. "
                    f"This is likely a publisher-side configuration issue — waiting will not resolve this."
                )

            parts.append(
                f"- Action               : Waiting for instance index(es) {missing_instances} to publish their status message"
            )

            # Show businessDate breakdown across all collected messages — mirrors get_kafka failure analysis
            if 'retry_collected_messages' in locals() and retry_collected_messages:
                try:
                    _df = pd.json_normalize(retry_collected_messages)
                    if 'status' in _df.columns:
                        _status = pd.json_normalize(_df['status'].tolist())
                        _status.columns = ['status.' + c for c in _status.columns]
                        _df = pd.concat([_df.drop('status', axis=1), _status], axis=1)
                    if 'status.businessDate' in _df.columns:
                        _df['status.businessDate'] = _df['status.businessDate'].astype(str)
                        date_counts = _df.groupby('status.businessDate').size().sort_index()
                        parts.append(f"- Business dates seen in topic (all messages, all mandators):")
                        for _date, _cnt in date_counts.items():
                            _marker = "<-- target (ASOF_DT)" if _date == str(ASOF_DT) else "<-- filtered out"
                            parts.append(f"    businessDate={_date!r}: {_cnt} message(s) {_marker}")
                except Exception:
                    pass

            error_msg_parts.extend(parts)
        elif 'retry_collected_messages' in locals() and len(retry_collected_messages) == 0:
            # No messages at all
            error_msg_parts.extend([
                f" Failure Reason: No Status messages found",
                f" - No Messages for the filter criteria were received from kafka",
                f" - Verify that data is being published to topic: {INFLOW_TOPIC}",
                f" - Check if mandator '{DSF_MANDATOR}' and producer '{PRODUCER_FILTER}' are correct",
            ])
        else:
            # messages exist but validation failed - analyze why
            error_msg_parts.append(f"Failure Reason: Validation Failed - Detail Analysis")
            error_msg_parts.append(f" - Total messages collected: {len(retry_collected_messages) if 'retry_collected_messages' in locals() else 0}")

            # Analyse the actual data to show that what's wrong
            if 'retry_collected_messages' in locals() and len(retry_collected_messages) > 0:
                df_analysis = pd.DataFrame(retry_collected_messages)

                # check if status field exists
                if 'status' in df_analysis.columns:
                    # Flatten status for analysis
                    status_df = pd.json_normalize(df_analysis['status'].tolist())
                    status_df.columns = ['status.' + col for col in status_df.columns]
                    df_analysis = pd.concat([df_analysis.drop('status', axis=1), status_df], axis=1)

                # Show data availability for the target producer, grouped by date → mandators
                if all(col in df_analysis.columns for col in ['status.mandatorCode', 'producer', 'status.businessDate']):
                    df_analysis['status.mandatorCode'] = df_analysis['status.mandatorCode'].astype(str)
                    df_analysis['producer'] = df_analysis['producer'].astype(str)
                    df_analysis['status.businessDate'] = df_analysis['status.businessDate'].astype(str)

                    producer_df = df_analysis[df_analysis['producer'] == str(PRODUCER_FILTER)]
                    if not producer_df.empty:
                        date_mandator_map = (
                            producer_df.groupby('status.businessDate')['status.mandatorCode']
                            .apply(lambda x: sorted(x.unique().tolist()))
                            .to_dict()
                        )
                        error_msg_parts.append(f" - Data availability for producer '{PRODUCER_FILTER}':")
                        for date, mandators in sorted(date_mandator_map.items()):
                            error_msg_parts.append(f"     Date {date}: mandators {mandators}")
                    else:
                        error_msg_parts.append(f" - No messages found for producer '{PRODUCER_FILTER}'")

                    error_msg_parts.append(
                        f" - Expected: mandator={DSF_MANDATOR}, producer={PRODUCER_FILTER}, date={ASOF_DT}"
                    )

                    matching = df_analysis[
                        (df_analysis['status.mandatorCode'] == str(DSF_MANDATOR)) &
                        (df_analysis['producer'] == str(PRODUCER_FILTER)) &
                        (df_analysis['status.businessDate'] == str(ASOF_DT))
                    ]
                    error_msg_parts.append(
                        f" - Messages matching all criteria (mandator+producer+date): {len(matching)}"
                    )

                    if len(matching) > 0 and 'status.reconciliationGroupId' in df_analysis.columns:
                        valid_run_ids = matching['status.reconciliationGroupId'].dropna()
                        run_ids = valid_run_ids.unique().tolist()
                        error_msg_parts.append(f" - RunIds in matching messages: {run_ids}")

                        if 'status.instanceIndex' in matching.columns and 'status.totalInstances' in matching.columns and not valid_run_ids.empty:
                            max_runid = int(valid_run_ids.max())
                            max_runid_msgs = matching[matching['status.reconciliationGroupId'] == max_runid]
                            present_instances = sorted(max_runid_msgs['status.instanceIndex'].unique().tolist())
                            expected_instances = int(max_runid_msgs['status.totalInstances'].max())
                            missing = sorted(set(range(expected_instances)) - set(present_instances))

                            error_msg_parts.append(f" - Max RunId: {max_runid}")
                            error_msg_parts.append(f" - Expected instances: 0-{expected_instances-1} (total: {expected_instances})")
                            error_msg_parts.append(f" - Present instances: {present_instances} (count: {len(present_instances)})")

                            if missing:
                                error_msg_parts.append(f" - Missing instances: {missing}")
                                error_msg_parts.append(f" - Action: wait for instances {missing} to publish status messages")
                            else:
                                error_msg_parts.append(f" - All instances present but duplicates detected — topic has been produced more than once. Wipe topic and re-produce.")
            else:
                error_msg_parts.append(f" - unable to analyse message content (no messages available)")


        full_error_msg = "\n".join(error_msg_parts)
        dsf_logger.log_msg(full_error_msg, level=40)

        _report_msgs = retry_collected_messages if retry_collected_messages else collected_messages
        log_topic_match_report(_report_msgs, DSF_MANDATOR, PRODUCER_FILTER, ASOF_DT, INFLOW_TOPIC)
        dsf_logger.log_msg(f"Exiting with error due to validation failure", level=40)
        write_failure_log_entry(
            VALIDATION_LOG_FILE, DSF_MANDATOR, PRODUCER_FILTER, FEED_NAME,
            "RETRY_EXHAUSTED_VALIDATION_FAILED", max_run_id,
            missing_instances if 'missing_instances' in locals() and missing_instances else [],
            validated_df if 'validated_df' in locals() else None,
            SEPERATOR, username
        )
        os._exit(1)
                        
                        
# exit successfully
dsf_logger.log_msg(f"Exiting (code 0): data successfully consumed and written to {DATA_FILE}", level=20)
os._exit(0)
                        
                        
                                                
                                            
                                                    
                                                    
                                                        
                                                
                                                
                                                    
                                                    
                                                            
                                                    
                                        
                        
                        
                                                
                    
                    
                
                
            
            
                                            
                                
                                
                                    
                            
                            
            
            
