#!/usr/bin/env python3
"""
filter_recon_group.py

Filters a JSONL .par file to retain only records belonging to the
reconciliationGroupId that the status-messages script validated.
Validates the filtered count against the expected count from metadata.
Hard-fails if count mismatches, blocking the splitter from running.

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
INPUT PARAMETERS (CLI arguments)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  --log-file            Full path to the log file (passed as $log_filename
                        from the shell script — framework-injected).

  --kafka-data          Full path to the JSONL .par file produced by
                        00_get_kafka.py.  Each line is one JSON record
                        containing a top-level "reconciliationGroupId" field.
                        This file is filtered IN-PLACE (temp -> rename).

  --status-feed-name    Feed name of the status-messages script
                        (STATUSMESSAGES_FEED_NAME, e.g. CPSB4Q00).
                        Used to locate the metadata file.

  --metadata-suffix     Suffix appended to the metadata filename.
                        Default: _metadata.txt
                        Must match METADATA_FILE_SUFFIX in the
                        status-messages config.

  --separator           Field delimiter used inside the metadata file.
                        Default: |
                        Must match SEPERATOR in the status-messages config.

REQUIRED ENVIRONMENT VARIABLES
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  PC_LOD_PROC_PATH      Root data directory.  Combined with
                        --status-feed-name and ASOF_DT to build the
                        metadata file path — mirrors exactly what
                        kafka_trigger_status_messages.py writes at
                        line 824:
                          {PC_LOD_PROC_PATH}/
                            {STATUSMESSAGES_FEED_NAME}_{ASOF_DT}/
                              {STATUSMESSAGES_FEED_NAME}_{ASOF_DT}{suffix}

  SDA                   DSF stream domain abbreviation.  Used to resolve
                        {SDA}_ASOF_DT.

  {SDA}_ASOF_DT         Business date (YYYY-MM-DD).

OUTPUT
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  --kafka-data file     Replaced in-place with the filtered content
                        (only records matching the target
                        reconciliationGroupId).  Written atomically via
                        a .tmp file that is renamed on success.

  --log-file            All log output written here via dsf_logger.

  Exit code 0           Filter succeeded; count matches metadata.
  Exit code 1           Any failure (missing file, parse error, count
                        mismatch).  Temp file is cleaned up.  Splitter
                        must NOT run after a non-zero exit.
"""

import os
import sys

# Pre-extract --log-file from sys.argv so dsf_logger can be set up at
# module level before full argparse runs — consistent with get_kafka.py
# and kafka_trigger_status_messages.py.
try:
    _log_file = sys.argv[sys.argv.index("--log-file") + 1]
except (ValueError, IndexError):
    print("ERROR: --log-file argument is required", flush=True)
    sys.exit(1)

import dsf_logging

logging_level = 20
verbose_log   = True
log_name      = os.path.basename(_log_file)
log_dir       = os.path.dirname(_log_file)

dsf_logger = dsf_logging.DSF_logging()
dsf_logger.get_logger(
    name=log_name,
    output_path=log_dir,
    level=logging_level,
    verbose=verbose_log,
)
dsf_logger.log_msg(f"filterReconGroup logging to: {_log_file}", level=20)

try:
    import argparse
    import json
except Exception as err:
    raise dsf_logger.log_error_msg(msg="ERROR_LOADING_LIBRARIES", err=err)


def hard_fail(msg: str, tmp_file: str = None) -> None:
    dsf_logger.log_msg(msg, level=40)
    if tmp_file and os.path.exists(tmp_file):
        try:
            os.remove(tmp_file)
            dsf_logger.log_msg(f"Temp file removed after failure: {tmp_file}", level=20)
        except Exception as e:
            dsf_logger.log_msg(f"Could not remove temp file {tmp_file}: {e}", level=30)
    os._exit(1)


def load_metadata(path: str, separator: str) -> dict:
    dsf_logger.log_msg(f"Loading metadata file: {path}", level=20)
    if not os.path.isfile(path):
        hard_fail(
            f"Metadata file not found: {path}  "
            f"The status-messages script must complete successfully before this step."
        )
    with open(path, "r", encoding="utf-8", errors="ignore") as f:
        lines = [ln.rstrip("\n") for ln in f if ln.strip()]
    if len(lines) != 2:
        hard_fail(
            f"Metadata file has {len(lines)} non-blank line(s) — "
            f"expected exactly 2 (header row + 1 data row): {path}"
        )
    headers = [h.strip() for h in lines[0].split(separator)]
    values  = [v.strip() for v in lines[1].split(separator)]
    if len(headers) != len(values):
        hard_fail(
            f"Metadata header count ({len(headers)}) != value count ({len(values)}). "
            f"Headers: {headers}  Values: {values}"
        )
    metadata = dict(zip(headers, values))
    dsf_logger.log_msg(f"Metadata fields : {list(metadata.keys())}", level=20)
    dsf_logger.log_msg(f"Metadata values : {metadata}", level=20)
    return metadata


def main() -> None:
    dsf_logger.log_starting_process(__file__, level=20)

    parser = argparse.ArgumentParser()
    parser.add_argument("--log-file",          required=True,           help="Full path to log file (framework-injected $log_filename)")
    parser.add_argument("--kafka-data",        required=True,           help="Full path to the JSONL .par file to filter (in-place)")
    parser.add_argument("--status-feed-name",  required=True,           help="Status-messages feed name (STATUSMESSAGES_FEED_NAME, e.g. CPSB4Q00)")
    parser.add_argument("--metadata-suffix",   default="_metadata.txt", help="Metadata filename suffix (default: _metadata.txt)")
    parser.add_argument("--separator",         default="|",             help="Field separator in the metadata file (default: |)")
    args = parser.parse_args()

    data_file = args.kafka_data
    sep       = args.separator
    tmp_file  = data_file + ".tmp"

    # ── Derive metadata file path ────────────────────────────────────────────
    # Mirrors kafka_trigger_status_messages.py lines 621-824:
    #   DATA_PATH     = $PC_LOD_PROC_PATH
    #   ASOF_DT       = ${SDA}_ASOF_DT
    #   DATA_FOLDER   = {DATA_PATH}/{STATUSMESSAGES_FEED_NAME}_{ASOF_DT}
    #   METADATA_FILE = {DATA_FOLDER}/{STATUSMESSAGES_FEED_NAME}_{ASOF_DT}{suffix}

    if "PC_LOD_PROC_PATH" not in os.environ:
        hard_fail("Required environment variable not set: PC_LOD_PROC_PATH")
    if "SDA" not in os.environ:
        hard_fail("Required environment variable not set: SDA")

    data_path   = os.environ["PC_LOD_PROC_PATH"]
    sda         = os.environ["SDA"].upper()
    asof_dt_var = f"{sda}_ASOF_DT"

    if asof_dt_var not in os.environ:
        hard_fail(f"Required environment variable not set: {asof_dt_var}")

    asof_dt          = os.environ[asof_dt_var]
    status_feed_name = args.status_feed_name
    metadata_suffix  = args.metadata_suffix
    metadata_dir     = os.path.join(data_path, f"{status_feed_name}_{asof_dt}")
    metadata_file    = os.path.join(metadata_dir, f"{status_feed_name}_{asof_dt}{metadata_suffix}")

    sep_line = "=" * 70
    dsf_logger.log_msg(sep_line, level=20)
    dsf_logger.log_msg("FILTER_RECON_GROUP — startup parameters", level=20)
    dsf_logger.log_msg(f"  PC_LOD_PROC_PATH        : {data_path}", level=20)
    dsf_logger.log_msg(f"  SDA                     : {sda}", level=20)
    dsf_logger.log_msg(f"  ASOF_DT ({asof_dt_var:<20}): {asof_dt}", level=20)
    dsf_logger.log_msg(f"  STATUSMESSAGES_FEED_NAME: {status_feed_name}", level=20)
    dsf_logger.log_msg(f"  METADATA_FILE_SUFFIX    : {metadata_suffix}", level=20)
    dsf_logger.log_msg(f"  Metadata file path      : {metadata_file}", level=20)
    dsf_logger.log_msg(f"  Input .par file         : {data_file}", level=20)
    dsf_logger.log_msg(f"  Temp file (during write): {tmp_file}", level=20)
    dsf_logger.log_msg(f"  Separator               : '{sep}'", level=20)
    dsf_logger.log_msg(sep_line, level=20)

    # ── Load metadata and extract required fields ────────────────────────────
    metadata = load_metadata(metadata_file, sep)

    for required in ("reconciliation_group_id", "total_messages_published"):
        if required not in metadata:
            hard_fail(
                f"Required field '{required}' not found in metadata. "
                f"Available fields: {list(metadata.keys())}"
            )

    try:
        target_recon_id = int(metadata["reconciliation_group_id"])
    except ValueError:
        hard_fail(
            f"'reconciliation_group_id' value '{metadata['reconciliation_group_id']}' "
            f"is not a valid integer."
        )

    try:
        expected_count = int(metadata["total_messages_published"])
    except ValueError:
        hard_fail(
            f"'total_messages_published' value '{metadata['total_messages_published']}' "
            f"is not a valid integer."
        )

    dsf_logger.log_msg(f"Target reconciliationGroupId : {target_recon_id}", level=20)
    dsf_logger.log_msg(f"Expected message count       : {expected_count}", level=20)

    if not os.path.isfile(data_file):
        hard_fail(f"Input .par file not found: {data_file}")

    # ── Stream, filter, write matched lines to temp file ─────────────────────
    dsf_logger.log_msg(f"Streaming input file and writing matched lines to temp: {tmp_file}", level=20)

    total_lines:      int  = 0
    matched_count:    int  = 0
    parse_errors:     int  = 0
    dropped_by_group: dict = {}   # {str(recon_id): count}

    with open(data_file, "r", encoding="utf-8", errors="ignore") as fin, \
         open(tmp_file,  "w", encoding="utf-8") as fout:

        for line_num, raw_line in enumerate(fin, start=1):
            line = raw_line.rstrip("\n")
            if not line:
                continue
            total_lines += 1

            try:
                record = json.loads(line)
            except json.JSONDecodeError as e:
                parse_errors += 1
                dsf_logger.log_msg(f"Line {line_num}: JSON parse error — {e} — skipped", level=30)
                continue

            line_recon_id = record.get("reconciliationGroupId")
            if line_recon_id is None:
                parse_errors += 1
                dsf_logger.log_msg(
                    f"Line {line_num}: 'reconciliationGroupId' field missing — skipped", level=30
                )
                continue

            if line_recon_id == target_recon_id:
                matched_count += 1
                fout.write(line + "\n")
            else:
                key = str(line_recon_id)
                dropped_by_group[key] = dropped_by_group.get(key, 0) + 1

    # ── Filter summary ───────────────────────────────────────────────────────
    total_dropped = sum(dropped_by_group.values())

    dsf_logger.log_msg(sep_line, level=20)
    dsf_logger.log_msg("FILTER SUMMARY", level=20)
    dsf_logger.log_msg(f"  Total lines in .par file              : {total_lines:,}", level=20)
    dsf_logger.log_msg(f"  Matched  (reconciliationGroupId={target_recon_id:<3})    : {matched_count:,}", level=20)
    dsf_logger.log_msg(f"  Dropped  (other reconciliation groups): {total_dropped:,}", level=20)
    dsf_logger.log_msg(f"  Skipped  (parse / missing field)      : {parse_errors:,}", level=20)

    if dropped_by_group:
        dsf_logger.log_msg("  Breakdown of dropped records by reconciliationGroupId:", level=20)
        for gid in sorted(
            dropped_by_group,
            key=lambda x: (int(x) if x.lstrip("-").isdigit() else float("inf"), x),
        ):
            dsf_logger.log_msg(
                f"    reconciliationGroupId={gid:<5} : {dropped_by_group[gid]:,} record(s)", level=20
            )
    else:
        dsf_logger.log_msg(
            "  No records dropped — .par file contained only the target reconciliationGroupId",
            level=20,
        )

    dsf_logger.log_msg(f"  Expected count (from metadata) : {expected_count:,}", level=20)
    dsf_logger.log_msg(f"  Matched  count (after filter)  : {matched_count:,}", level=20)
    dsf_logger.log_msg(sep_line, level=20)

    # ── Count validation — hard fail if mismatch ─────────────────────────────
    if matched_count != expected_count:
        hard_fail(
            f"COUNT MISMATCH: expected {expected_count:,} records for "
            f"reconciliationGroupId={target_recon_id}, got {matched_count:,}. "
            f"Splitter will NOT run.",
            tmp_file=tmp_file,
        )

    # ── Atomic in-place replace ──────────────────────────────────────────────
    os.rename(tmp_file, data_file)
    dsf_logger.log_msg(f"Filtered file promoted: {tmp_file} -> {data_file}", level=20)
    dsf_logger.log_msg(
        f"Done. {matched_count:,} records written to {data_file}. Ready for splitter.", level=20
    )
    os._exit(0)


if __name__ == "__main__":
    main()
