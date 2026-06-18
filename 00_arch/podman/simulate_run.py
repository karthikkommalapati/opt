"""
Standalone simulation of kafka_trigger_status_messages.py.
Reads directly from input/status_messages_data.jsonl instead of Kafka.
Runs the real validation + diagnostic logic and writes a log file.

Usage:
    python3 simulate_run.py --mandator 022 --date 2026-05-24
"""
import argparse
import json
import logging
import os
import sys
from datetime import datetime

import pandas as pd

# ── CLI args ──────────────────────────────────────────────────────────────────
parser = argparse.ArgumentParser()
parser.add_argument("--mandator",  default="022")
parser.add_argument("--date",      default="2026-05-24")
parser.add_argument("--producer",  default="CLIENT_STRUCTURES")
parser.add_argument("--data-file", default="input/status_messages_data.jsonl")
parser.add_argument("--topic",     default="inflow-topic")
parser.add_argument("--feed",      default="CPSB4Q00")
args = parser.parse_args()

DSF_MANDATOR    = args.mandator
ASOF_DT         = args.date
PRODUCER_FILTER = args.producer
DATA_FILE       = args.data_file
INFLOW_TOPIC    = args.topic
FEED_NAME       = args.feed

# ── logging setup ─────────────────────────────────────────────────────────────
os.makedirs("output/logs", exist_ok=True)
LOG_FILE = f"output/logs/{FEED_NAME}.{ASOF_DT}.SIM.LOCAL_TEST_001.log"

fmt = logging.Formatter("%(asctime)s [%(levelname)s] %(message)s", datefmt="%Y-%m-%dT%H:%M:%S")
file_handler   = logging.FileHandler(LOG_FILE, mode="w")
stdout_handler = logging.StreamHandler(sys.stdout)
for h in (file_handler, stdout_handler):
    h.setFormatter(fmt)

log = logging.getLogger("sim")
log.setLevel(logging.DEBUG)
log.addHandler(file_handler)
log.addHandler(stdout_handler)


def info(msg):  log.info(msg)
def warn(msg):  log.warning(msg)
def error(msg): log.error(msg)


# ── load messages ─────────────────────────────────────────────────────────────
info(f"=== STATUS MESSAGES SIMULATION ===")
info(f"Feed          : {FEED_NAME}")
info(f"Mandator      : {DSF_MANDATOR}")
info(f"Business date : {ASOF_DT}")
info(f"Producer      : {PRODUCER_FILTER}")
info(f"Topic         : {INFLOW_TOPIC}")
info(f"Data file     : {DATA_FILE}")
info(f"Log file      : {LOG_FILE}")
info(f"----------------------------------")

if not os.path.exists(DATA_FILE):
    error(f"Data file not found: {DATA_FILE}")
    sys.exit(1)

messages_list = []
with open(DATA_FILE) as f:
    for lineno, line in enumerate(f, 1):
        line = line.strip()
        if not line:
            continue
        try:
            messages_list.append(json.loads(line))
        except json.JSONDecodeError as e:
            warn(f"Skipping line {lineno} (JSON error): {e}")

info(f"Total messages loaded from data file: {len(messages_list)}")
if messages_list:
    info(f"Sample message: {json.dumps(messages_list[0])}")


# ── validate_instance_and_get_max_runid ───────────────────────────────────────
def validate(messages_list, mandator_filter, producer_filter, business_date):
    info(f"starting validation ===")
    info(f"filter criteria: mandator={mandator_filter}, producer={producer_filter}, business_date={business_date}")

    if not messages_list:
        warn("No messages found in the topic for the given filters")
        return False, None, [], None

    df = pd.DataFrame(messages_list)

    if "status" in df.columns and isinstance(df["status"].iloc[0], dict):
        status_df = pd.json_normalize(df["status"].tolist())
        status_df.columns = [f"status.{c}" for c in status_df.columns]
        df = pd.concat([df.drop(columns=["status"]), status_df], axis=1)

    info(f"Total messages found: {len(df)}")
    info(f"Columns in df: {df.columns.tolist()}")

    required = ["status.mandatorCode", "producer", "status.businessDate",
                "status.reconciliationGroupId", "status.totalInstances", "status.instanceIndex"]
    missing_cols = [c for c in required if c not in df.columns]
    if missing_cols:
        error(f"VALIDATION FAILED: Required columns missing from messages: {missing_cols}")
        return False, None, [], None

    if len(df) > 0:
        info(f"Sample data - mandatorCode: {df['status.mandatorCode'].iloc[0]}")
        info(f"Sample data - producer: {df['producer'].iloc[0]}")
        info(f"Sample data - businessDate: {df['status.businessDate'].iloc[0]}")
        info(f"Filter - mandator: {mandator_filter}")
        info(f"Filter - producer: {producer_filter}")
        info(f"Filter - business_date: {business_date}")

    df["status.mandatorCode"] = df["status.mandatorCode"].astype(str)
    df["producer"]            = df["producer"].astype(str)
    df["status.businessDate"] = df["status.businessDate"].astype(str)

    mandator_match = df["status.mandatorCode"] == str(mandator_filter)
    producer_match = df["producer"]            == str(producer_filter)
    date_match     = df["status.businessDate"] == str(business_date)

    info(f"Messages matching mandator filter: {mandator_match.sum()}")
    info(f"Messages matching producer filter: {producer_match.sum()}")
    info(f"Messages matching business date filter: {date_match.sum()}")

    filtered_df = df[mandator_match & producer_match & date_match]

    if filtered_df.empty:
        producer_df = df[df["producer"] == str(producer_filter)]
        if not producer_df.empty:
            date_mandator_map = (
                producer_df.groupby("status.businessDate")["status.mandatorCode"]
                .apply(lambda x: sorted(x.astype(str).unique().tolist()))
                .to_dict()
            )
            warn(f"Data availability for producer '{producer_filter}':")
            for date, mandators in sorted(date_mandator_map.items()):
                warn(f"  Date {date}: mandators {mandators}")
        else:
            warn(f"No messages found for producer '{producer_filter}'")
        warn(f"No messages found matching filters: mandator={mandator_filter}, "
             f"producer={producer_filter}, business_date={business_date}")
        return False, None, [], filtered_df

    info(f"Messages found matching all filters: {len(filtered_df)}")

    valid_run_ids = filtered_df["status.reconciliationGroupId"].dropna()
    if valid_run_ids.empty:
        error("VALIDATION FAILED: No valid reconciliationGroupId values found")
        return False, None, [], filtered_df

    max_run_id = int(valid_run_ids.max())
    info(f"Max reconciliationGroupId among filtered messages: {max_run_id}")

    max_runid_df     = filtered_df[filtered_df["status.reconciliationGroupId"] == max_run_id]
    total_instances  = int(max_runid_df["status.totalInstances"].max())
    present_instances = set(int(x) for x in max_runid_df["status.instanceIndex"].unique())
    expected_instances = set(range(total_instances))
    missing_instances  = sorted(expected_instances - present_instances)

    info(f"Expected instances: {sorted(expected_instances)}")
    info(f"Present instances : {sorted(present_instances)}")

    if missing_instances:
        error(f"VALIDATION FAILED: Missing instances: {missing_instances}")
        return False, None, missing_instances, max_runid_df

    duplicates = max_runid_df[max_runid_df.duplicated(subset=["status.instanceIndex"], keep=False)]
    if not duplicates.empty:
        error(f"VALIDATION FAILED: Duplicate instanceIndex values detected")
        return False, None, [], max_runid_df

    info(f"Validation passed — all {total_instances} instance(s) present for runId {max_run_id}")
    return True, max_run_id, [], max_runid_df


# ── log_topic_match_report ────────────────────────────────────────────────────
def log_topic_match_report(messages_list, mandator, producer_filter, target_date, topic):
    if not messages_list:
        error("\n".join([
            f"TOPIC MATCH REPORT  topic='{topic}'  looking for: mandator={mandator}, producer={producer_filter}, date={target_date}",
            f"  No messages were collected from the topic — cannot analyse data availability",
            f"  Possible causes:",
            f"   - No messages published to topic '{topic}' within the configured time window",
            f"   - Mandator '{mandator}' has not published any status messages yet",
            f"  Action: confirm with sender that messages for mandator '{mandator}' are being published to '{topic}'",
        ]))
        return

    try:
        df = pd.json_normalize(messages_list)

        if "status" in df.columns:
            status_df = pd.json_normalize(df["status"].tolist())
            status_df.columns = ["status." + c for c in status_df.columns]
            df = pd.concat([df.drop("status", axis=1), status_df], axis=1)

        lines = [f"TOPIC MATCH REPORT  topic='{topic}'  looking for: mandator={mandator}, producer={producer_filter}, date={target_date}"]

        if "status.mandatorCode" not in df.columns:
            lines.append("  [?] Cannot check mandator — 'status.mandatorCode' column missing from messages")
            error("\n".join(lines))
            return

        all_mandators  = sorted(df["status.mandatorCode"].astype(str).unique().tolist())
        mandator_match = str(mandator) in all_mandators
        lines.append(f"  Mandator {mandator} in topic : {'YES' if mandator_match else 'NO  — mandators found: ' + str(all_mandators)}")

        if not mandator_match:
            lines.append(f"  Action: confirm with sender that mandator '{mandator}' is publishing to topic '{topic}'")
            error("\n".join(lines))
            return

        mandator_df    = df[df["status.mandatorCode"].astype(str) == str(mandator)]
        all_producers  = sorted(mandator_df["producer"].astype(str).unique().tolist())
        producer_match = str(producer_filter) in all_producers
        lines.append(f"  Producer '{producer_filter}' found : {'YES' if producer_match else 'NO  — producers found for mandator ' + str(mandator) + ': ' + str(all_producers)}")

        if not producer_match:
            lines.append(f"  Action: confirm with sender that producer '{producer_filter}' is publishing for mandator '{mandator}'")
            error("\n".join(lines))
            return

        prod_df    = mandator_df[mandator_df["producer"].astype(str) == str(producer_filter)]
        all_dates  = sorted(prod_df["status.businessDate"].astype(str).unique().tolist())
        date_match = str(target_date) in all_dates
        lines.append(f"  Date {target_date} found      : {'YES' if date_match else 'NO  — dates available for mandator ' + str(mandator) + ' / producer ' + str(producer_filter) + ': ' + str(all_dates)}")

        if not date_match:
            lines.append(f"  Action: confirm with sender that data for date '{target_date}' has been published")

        error("\n".join(lines))

    except Exception as e:
        error(f"TOPIC MATCH REPORT: could not analyse messages — {e}")


# ── main flow ─────────────────────────────────────────────────────────────────
info(f"starting validation of collected messages")
info(f"Total messages collected from all partitions: {len(messages_list)}")

is_valid, max_run_id, missing_instances, validated_df = validate(
    messages_list, DSF_MANDATOR, PRODUCER_FILTER, ASOF_DT
)

if not is_valid:
    if missing_instances:
        error(f"Data extraction aborted: Instance Validation failed. Missing instances: {missing_instances}")
    else:
        error(f"Data Extraction aborted: Validation failed (no matching data or other validation error).")

    # ── detail analysis block (mirrors real script lines 1877-1951) ───────────
    # In the real script this runs after the retry loop using retry_collected_messages.
    # Here messages_list is the equivalent.
    error_msg_parts = []

    if not messages_list:
        error_msg_parts.extend([
            f" Failure Reason: No Status messages found",
            f" - No Messages for the filter criteria were received from kafka",
            f" - Verify that data is being published to topic: {INFLOW_TOPIC}",
            f" - Check if mandator '{DSF_MANDATOR}' and producer '{PRODUCER_FILTER}' are correct",
        ])
    else:
        error_msg_parts.append(f"Failure Reason: Validation Failed - Detail Analysis")
        error_msg_parts.append(f" - Total messages collected: {len(messages_list)}")

        df_analysis = pd.DataFrame(messages_list)

        if "status" in df_analysis.columns:
            status_df = pd.json_normalize(df_analysis["status"].tolist())
            status_df.columns = ["status." + c for c in status_df.columns]
            df_analysis = pd.concat([df_analysis.drop("status", axis=1), status_df], axis=1)

        if all(c in df_analysis.columns for c in ["status.mandatorCode", "producer", "status.businessDate"]):
            df_analysis["status.mandatorCode"] = df_analysis["status.mandatorCode"].astype(str)
            df_analysis["producer"]            = df_analysis["producer"].astype(str)
            df_analysis["status.businessDate"] = df_analysis["status.businessDate"].astype(str)

            producer_df = df_analysis[df_analysis["producer"] == str(PRODUCER_FILTER)]
            if not producer_df.empty:
                date_mandator_map = (
                    producer_df.groupby("status.businessDate")["status.mandatorCode"]
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
                (df_analysis["status.mandatorCode"] == str(DSF_MANDATOR)) &
                (df_analysis["producer"]            == str(PRODUCER_FILTER)) &
                (df_analysis["status.businessDate"] == str(ASOF_DT))
            ]
            error_msg_parts.append(f" - Messages matching all criteria (mandator+producer+date): {len(matching)}")

            if len(matching) > 0 and "status.reconciliationGroupId" in df_analysis.columns:
                valid_run_ids = matching["status.reconciliationGroupId"].dropna()
                run_ids = valid_run_ids.unique().tolist()
                error_msg_parts.append(f" - RunIds in matching messages: {run_ids}")

                if ("status.instanceIndex" in matching.columns
                        and "status.totalInstances" in matching.columns
                        and not valid_run_ids.empty):
                    max_runid          = int(valid_run_ids.max())
                    max_runid_msgs     = matching[matching["status.reconciliationGroupId"] == max_runid]
                    present_instances  = sorted(int(x) for x in max_runid_msgs["status.instanceIndex"].unique())
                    expected_instances = int(max_runid_msgs["status.totalInstances"].max())
                    missing_inst       = sorted(set(range(expected_instances)) - set(present_instances))

                    error_msg_parts.append(f" - Max RunId: {max_runid}")
                    error_msg_parts.append(f" - Expected instances: 0-{expected_instances-1} (total: {expected_instances})")
                    error_msg_parts.append(f" - Present instances: {present_instances} (count: {len(present_instances)})")

                    if missing_inst:
                        error_msg_parts.append(f" - Missing instances: {missing_inst}")
                        error_msg_parts.append(f" - Action: wait for instances {missing_inst} to publish status messages")
                    else:
                        error_msg_parts.append(
                            f" - All instances present but duplicates detected — "
                            f"topic has been produced more than once. Wipe topic and re-produce."
                        )
        else:
            error_msg_parts.append(f" - unable to analyse message content (no messages available)")

    error("\n".join(error_msg_parts))

    log_topic_match_report(messages_list, DSF_MANDATOR, PRODUCER_FILTER, ASOF_DT, INFLOW_TOPIC)
    error(f"Exiting with error due to validation failure")
    info(f"Log written to: {LOG_FILE}")
    sys.exit(1)

info(f"Validation passed; Writing data for, max run id is {max_run_id}")
info(f"Exiting (code 0): data successfully validated")
info(f"Log written to: {LOG_FILE}")
