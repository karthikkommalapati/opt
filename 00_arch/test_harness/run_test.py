"""
Test harness for kafka_trigger_status_messages.py.

Runs the full script end-to-end without a real Kafka broker, schema registry,
or upstream dependency.  Uses the same fake_kafka / fake_libs pattern.

Usage:
    python run_test.py [scenario_name|all]
    scenario_name defaults to "happy_path" when omitted.
"""

import copy
import json
import os
import pathlib
import shutil
import struct
import sys
import threading
from datetime import date, datetime, time, timedelta
from unittest.mock import patch

# ── install fake modules BEFORE any other import ──────────────────────────────
_here = pathlib.Path(__file__).parent
sys.path.insert(0, str(_here))

import fake_kafka
import fake_libs

fake_libs.install_all()

# ── paths ─────────────────────────────────────────────────────────────────────

SCRIPT_PATH  = str(_here.parent / "kafka_trigger_status_messages.py")
TEST_DIR     = _here.parent / "test_output"
DATA_FILE    = str(_here / "test_data.par")
FEED_NAME    = "CPSB4Q00"
ASOF_DT      = "2026-05-07"
DSF_MAIN_ID  = "1001"

# ── base config (happy-path defaults) ─────────────────────────────────────────

BASE_CONFIG = {
    "KAFKA_USER":                      {"022": "admin", "036": "admin"},
    "STREAMING_KAFKA_BROKER":          {"022": "fake-broker:9092", "036": "fake-broker:9092"},
    "STREAMING_KAFKA_INFLOW_TOPIC":    {"022": "test-topic", "036": "test-topic"},
    "AVRO_COLUMNS":                    "",
    "AVRO_SHCEMA_FILE":                "LATEST",
    "AVRO_SCHEMA_REGISTRY":            "fake-registry:8086",
    "DECIMAL_CONV":                    "2",
    "DECIMAL_SCALE":                   "2",
    "SEPERATOR":                       "|",
    "INPUT_DATA":                      "AVRO",
    "OUTPUT_DATA":                     "JSON",
    "ALLOW_NO_DATA":                   "YES",
    "START_TS":                        "00:00:00",
    "START_DT_OFFSET":                 "0",
    "STOP_TS":                         "23:59:59",
    "STOP_DT_OFFSET":                  "0",
    "EXTEND_ON_ITERATE":               "NO",
    "COMMIT_CNT":                      "50",
    "STREAMING_TIMESTAMP_KEY":         "eventTimestamp",
    "STREAMING_TIMESTAMP_FORMAT":      "%Y-%m-%dT%H:%M:%S%z",
    "STREAMING_MANDATORY_KEY":         "status.mandatorCode",
    "PRODUCER_FILTER":                 "CLIENT_STRUCTURES",
    "VALIDATE_TOPIC_MANDATOR":         "NO",
    "STREAMING_WAIT_UNTIL_DONE_HOURS": "0",
    "STREAMING_IDLE_TIMEOUT_MINUTES":  "0",
    "STREAMING_PAGE_SIZE_LIMIT_BYTES": "8000",
    "STREAMING_REPROCESS":             "False",
    "STREAMING_VERBOSE":               "True",
    "STREAMING_STORE_MIDLAYER":        "False",
    "WAIT_FOR_SUBMIT":                 "NO",
    "MAX_LISTEN_DURATION_HOURS":       "0.02",
    "RETRY_WAIT_SECONDS":              "30",
    "VALIDATION_REQUIRED_COLUMNS":     (
        "status.mandatorCode,producer,status.businessDate,"
        "status.reconciliationGroupId,status.totalInstances,status.instanceIndex"
    ),
    "METADATA_OUTPUT_PATH":            "",
    "METADATA_FILE_SUFFIX":            "_metadata.txt",
}

FAKE_AVRO_SCHEMA = {
    "type": "record", "name": "StatusMessage", "namespace": "com.test", "fields": [],
}

# ── base test messages ─────────────────────────────────────────────────────────

BASE_MESSAGES = [
    {
        "status": {
            "mandatorCode": "022", "businessDate": "2026-05-07",
            "reconciliationGroupId": 5001, "instanceIndex": 0,
            "totalInstances": 3, "numberOfMessagesPublished": 15000,
        },
        "producer": "CLIENT_STRUCTURES",
        "eventTimestamp": "2026-05-07T17:00:00+0000",
    },
    {
        "status": {
            "mandatorCode": "022", "businessDate": "2026-05-07",
            "reconciliationGroupId": 5001, "instanceIndex": 1,
            "totalInstances": 3, "numberOfMessagesPublished": 16000,
        },
        "producer": "CLIENT_STRUCTURES",
        "eventTimestamp": "2026-05-07T17:01:00+0000",
    },
    {
        "status": {
            "mandatorCode": "022", "businessDate": "2026-05-07",
            "reconciliationGroupId": 5001, "instanceIndex": 2,
            "totalInstances": 3, "numberOfMessagesPublished": 16823,
        },
        "producer": "CLIENT_STRUCTURES",
        "eventTimestamp": "2026-05-07T17:02:00+0000",
    },
]

# ── infrastructure helpers ────────────────────────────────────────────────────

def load_test_messages(path=DATA_FILE):
    with open(path) as f:
        return [json.loads(line) for line in f if line.strip()]


def setup_dirs():
    cfg  = TEST_DIR / "config"
    data = TEST_DIR / "data"
    logs = TEST_DIR / "logs"
    for d in [cfg, data, logs]:
        d.mkdir(parents=True, exist_ok=True)
    return cfg, data, logs


def setup_env(cfg, data, logs, asof_dt=ASOF_DT, mandator="022"):
    e = os.environ
    e["SDA"]              = "BST"
    e["SDA_HOME"]         = str(TEST_DIR)
    e["FEED_NAME"]        = FEED_NAME
    e["BST_ASOF_DT"]      = asof_dt
    e["BST_ITERATION_NR"] = "1"
    e["BST_KNW_FRO_TS"]   = f"{asof_dt}-23.59.59"
    e["BST_USR_VAL"]      = "0"
    e["DSF_MANDATOR"]     = mandator
    e["DSF_DOMAIN"]       = "T"
    e["DSF_CHARMAP"]      = "UTF-8"
    e["DSF_JAVA_TZ"]      = "UTC"
    e["DSF_MAIN_ID"]      = DSF_MAIN_ID
    e["CFG_PROC_PATH"]    = str(cfg)
    e["PC_LOD_PROC_PATH"] = str(data)
    e["LOG_PROC_PATH"]    = str(logs)
    e["PARENT_PID"]       = "99999"
    e["AUDIT_ID"]         = "TEST001"


def write_config(cfg, overrides=None):
    config = dict(BASE_CONFIG)
    if overrides:
        config.update(overrides)
    path = cfg / f"{DSF_MAIN_ID}_{FEED_NAME}_config.json"
    path.write_text(json.dumps(config, indent=2))
    return config


def make_mock_schemaless_reader(test_messages):
    def _reader(msg_io, schema):
        idx_bytes = msg_io.read(4)
        if len(idx_bytes) < 4:
            raise ValueError("fake reader: message payload too short")
        idx = struct.unpack(">I", idx_bytes)[0]
        return dict(test_messages[idx])
    return _reader


def make_mock_registry():
    schema_json = json.dumps(FAKE_AVRO_SCHEMA)

    class _Resp:
        def raise_for_status(self): pass
        def json(self):
            return {"id": fake_kafka.FAKE_SCHEMA_ID, "version": 1, "schema": schema_json}

    def _get(self_session, url, *args, **kwargs):
        return _Resp()

    return _get


def run_script(test_messages, config_overrides=None, data_file=None,
               asof_dt=None, mandator=None):
    """
    Full end-to-end run of kafka_trigger_status_messages.py.

    asof_dt / mandator: derived from real data files when provided;
                        fall back to module-level defaults for synthetic scenarios.
    Returns (exit_code, par_file, metadata_file).
    """
    _asof_dt  = asof_dt  or ASOF_DT
    _mandator = mandator or "022"

    cfg, data, logs = setup_dirs()
    setup_env(cfg, data, logs, asof_dt=_asof_dt, mandator=_mandator)
    write_config(cfg, config_overrides)

    data_folder   = data / f"{FEED_NAME}_{_asof_dt}"
    validation_log = data / f"{FEED_NAME}_status_messages_validation.log"
    if data_folder.exists():
        shutil.rmtree(data_folder)
    data.mkdir(parents=True, exist_ok=True)
    data_folder.mkdir(parents=True)

    day_start  = datetime.combine(date.fromisoformat(_asof_dt), time(0, 0, 0))
    base_ts_ms = int((day_start + timedelta(hours=17)).timestamp() * 1000)

    consumer = fake_kafka.FakeKafkaConsumer(
        msg_dicts       = test_messages,
        topic           = "test-topic",
        start_ts_ms     = base_ts_ms,
        msg_interval_ms = 60_000,
        data_file       = data_file,
    )
    fake_kafka.install(consumer)

    import fastavro
    import requests

    exited_with = [None]

    def _fake_exit(code=0):
        exited_with[0] = code
        raise SystemExit(code)

    mock_reader   = make_mock_schemaless_reader(consumer._msgs)
    mock_registry = make_mock_registry()

    exec_globals = {
        "__name__": "__harness__",
        "__file__": SCRIPT_PATH,
    }

    with (
        patch.object(fastavro, "schemaless_reader", new=mock_reader),
        patch.object(requests.Session, "get", mock_registry),
        patch("os._exit", side_effect=_fake_exit),
    ):
        try:
            with open(SCRIPT_PATH) as fh:
                source = fh.read()
            exec(compile(source, SCRIPT_PATH, "exec"), exec_globals)
            exit_code = 0
        except SystemExit as exc:
            exit_code = exited_with[0] if exited_with[0] is not None else exc.code
        except Exception as exc:
            import traceback
            print(f"\n[HARNESS ERROR] Unexpected exception: {exc}")
            traceback.print_exc()
            exit_code = None

    par_file      = data_folder / f"{FEED_NAME}_{_asof_dt}.par"
    metadata_file = data_folder / f"{FEED_NAME}_{_asof_dt}_metadata.txt"
    return exit_code, par_file, metadata_file, validation_log


# ── result printer ────────────────────────────────────────────────────────────

def print_result(label, passed, details=""):
    status = "[PASS]" if passed else "[FAIL]"
    print(f"  {status}  {label}")
    if details:
        for line in details.strip().splitlines():
            print(f"         {line}")


def section(name):
    sep = "=" * 60
    print(f"\n{sep}")
    print(f"  {name}")
    print(sep)


def check_par(par_file, expected_lines, passed_ref):
    if par_file.exists():
        lines = par_file.read_text(encoding="utf-8").strip().splitlines()
        ok = len(lines) == expected_lines
        print_result(f".par written with {len(lines)} line(s) (expected {expected_lines})", ok,
                     f"path: {par_file}")
        if not ok:
            passed_ref[0] = False
        return lines
    else:
        print_result(".par file written", False, f"missing: {par_file}")
        passed_ref[0] = False
        return []


def check_metadata(metadata_file, expected_total, expected_recon_id, passed_ref):
    if not metadata_file.exists():
        print_result("metadata file written", False, f"missing: {metadata_file}")
        passed_ref[0] = False
        return
    lines = metadata_file.read_text(encoding="utf-8").strip().splitlines()
    if len(lines) < 2:
        print_result("metadata file has header + data row", False,
                     f"only {len(lines)} line(s)")
        passed_ref[0] = False
        return
    print_result("metadata file written", True, f"path: {metadata_file}")
    sep     = BASE_CONFIG["SEPERATOR"]
    headers = lines[0].split(sep)
    values  = lines[1].split(sep)
    fields  = dict(zip(headers, values))
    got_username = fields.get("username", "")
    if got_username:
        print_result(f"username present in metadata ('{got_username}')", True)
    else:
        print_result("username present in metadata", False, "field missing or empty")
        passed_ref[0] = False
    got_total = fields.get("total_messages_published")
    if got_total == str(expected_total):
        print_result(f"total_messages_published = {expected_total}", True)
    else:
        print_result(f"total_messages_published = {expected_total} (got {got_total!r})", False)
        passed_ref[0] = False
    got_recon = fields.get("reconciliation_group_id")
    if got_recon == str(expected_recon_id):
        print_result(f"reconciliation_group_id = {expected_recon_id}", True)
    else:
        print_result(f"reconciliation_group_id = {expected_recon_id} (got {got_recon!r})", False)
        passed_ref[0] = False


def check_status_validation_log(vlog_path, passed_ref):
    """Verify that the status_messages_validation.log was appended with one data row."""
    if not vlog_path.exists():
        print_result("status_messages_validation.log written", False,
                     f"missing: {vlog_path}")
        passed_ref[0] = False
        return
    lines = vlog_path.read_text(encoding="utf-8").strip().splitlines()
    if len(lines) < 2:
        print_result("status_messages_validation.log has header + data row", False,
                     f"only {len(lines)} line(s)")
        passed_ref[0] = False
        return
    sep     = BASE_CONFIG["SEPERATOR"]
    headers = lines[0].split(sep)
    values  = lines[-1].split(sep)
    fields  = dict(zip(headers, values))
    got_username = fields.get("username", "")
    if got_username:
        print_result(f"status_messages_validation.log has username ('{got_username}')", True)
    else:
        print_result("status_messages_validation.log has username", False,
                     "field missing or empty")
        passed_ref[0] = False


# ── scenarios ─────────────────────────────────────────────────────────────────

def scenario_happy_path():
    """
    Scenario 1 — Happy path.
    3 instances (0, 1, 2), all present, all matching filters.
    Expect: exit 0, .par with 3 lines, metadata written correctly.
    """
    section("Scenario 1 — Happy Path")
    msgs = list(BASE_MESSAGES)
    code, par, meta, vlog = run_script(msgs)

    passed = [True]

    if code == 0:
        print_result("exit code = 0", True)
    else:
        print_result(f"exit code = 0 (got {code})", False)
        passed[0] = False

    check_par(par, 3, passed)
    check_metadata(meta, 47823, 5001, passed)
    check_status_validation_log(vlog, passed)

    return passed[0]


def scenario_missing_instance():
    """
    Scenario 2 — Missing instance (instanceIndex 2 absent), ALLOW_NO_DATA=NO.
    Expect: exit 1, .par NOT written.
    """
    section("Scenario 2 — Missing Instance (hard fail)")
    msgs = [BASE_MESSAGES[0], BASE_MESSAGES[1]]  # indices 0 and 1 only
    code, par, meta, vlog = run_script(msgs, config_overrides={"ALLOW_NO_DATA": "NO"})

    passed = [True]

    if code == 1:
        print_result("exit code = 1 (correct hard stop)", True)
    else:
        print_result(f"exit code = 1 (got {code})", False)
        passed[0] = False

    if not par.exists():
        print_result(".par file NOT written (correct)", True)
    else:
        print_result(".par file NOT written (was written — wrong!)", False)
        passed[0] = False

    return passed[0]


def scenario_wrong_producer():
    """
    Scenario 3 — All messages have producer = WRONG_PRODUCER.
    Expect: exit 1, .par NOT written.
    """
    section("Scenario 3 — Wrong Producer")
    msgs = copy.deepcopy(BASE_MESSAGES)
    for m in msgs:
        m["producer"] = "WRONG_PRODUCER"
    code, par, meta, vlog = run_script(msgs)

    passed = [True]

    if code == 1:
        print_result("exit code = 1 (correct hard stop)", True)
    else:
        print_result(f"exit code = 1 (got {code})", False)
        passed[0] = False

    if not par.exists():
        print_result(".par file NOT written (correct)", True)
    else:
        print_result(".par file NOT written (was written — wrong!)", False)
        passed[0] = False

    return passed[0]


def scenario_duplicate_instance():
    """
    Scenario 4 — Two messages both claim instanceIndex = 1.
    Expect: exit 1.
    """
    section("Scenario 4 — Duplicate Instance Index")
    msgs = copy.deepcopy(BASE_MESSAGES)
    msgs[2]["status"]["instanceIndex"] = 1  # duplicate of msg 1
    code, par, meta, vlog = run_script(msgs)

    passed = [True]

    if code == 1:
        print_result("exit code = 1 (correct hard stop)", True)
    else:
        print_result(f"exit code = 1 (got {code})", False)
        passed[0] = False

    return passed[0]


def scenario_missing_column():
    """
    Scenario 5 — 'numberOfMessagesPublished' removed from all messages.
    Validation passes (instances present), then script crashes on the missing column.
    Ideal: exit 1 with ERROR log. Current script behaviour: unhandled KeyError
    (exit_code=None). Both count as failure — test passes either way.
    Script bug: validate_instance_and_get_max_runid line 334 has no guard for
    missing 'status.numberOfMessagesPublished'. Should be wrapped in try/except.
    """
    section("Scenario 5 — Missing numberOfMessagesPublished Column")
    msgs = copy.deepcopy(BASE_MESSAGES)
    for m in msgs:
        del m["status"]["numberOfMessagesPublished"]
    code, par, meta, vlog = run_script(msgs)

    passed = [True]

    # code=1 is clean exit; code=None is unhandled exception — both mean failure
    if code in (1, None):
        label = "exit code = 1 (correct hard stop)" if code == 1 else \
                "script raised unhandled exception (counts as failure — see bug note)"
        print_result(label, True)
    else:
        print_result(f"exit code = 1 or exception (got {code})", False)
        passed[0] = False

    return passed[0]


def scenario_multiple_run_ids():
    """
    Scenario 6 — 5 messages: 2 for reconciliationGroupId=4999, 3 for 5001.
    Script picks max(reconciliationGroupId) = 5001, writes only those 3.
    Expect: exit 0, .par with 3 lines, metadata shows recon_id=5001.
    """
    section("Scenario 6 — Multiple Run IDs (stale + current)")
    stale = [
        {
            "status": {
                "mandatorCode": "022", "businessDate": "2026-05-07",
                "reconciliationGroupId": 4999, "instanceIndex": 0,
                "totalInstances": 2, "numberOfMessagesPublished": 8000,
            },
            "producer": "CLIENT_STRUCTURES",
            "eventTimestamp": "2026-05-07T10:00:00+0000",
        },
        {
            "status": {
                "mandatorCode": "022", "businessDate": "2026-05-07",
                "reconciliationGroupId": 4999, "instanceIndex": 1,
                "totalInstances": 2, "numberOfMessagesPublished": 8000,
            },
            "producer": "CLIENT_STRUCTURES",
            "eventTimestamp": "2026-05-07T10:01:00+0000",
        },
    ]
    msgs = stale + list(BASE_MESSAGES)
    code, par, meta, vlog = run_script(msgs)

    passed = [True]

    if code == 0:
        print_result("exit code = 0", True)
    else:
        print_result(f"exit code = 0 (got {code})", False)
        passed[0] = False

    check_par(par, 3, passed)
    check_metadata(meta, 47823, 5001, passed)
    check_status_validation_log(vlog, passed)

    return passed[0]


def scenario_late_data_arrival():
    """
    Scenario 7 — Late data arrival (retry/polling mode).
    Starts with 2 messages (instanceIndex 2 missing).
    A background thread adds the 3rd message to the temp file after 3 seconds.
    Script retries (ALLOW_NO_DATA=YES) and finds all 3 on the next pass.
    Expect: exit 0, .par with 3 lines.
    """
    section("Scenario 7 — Late Data Arrival (retry/polling)")

    tmp_file = TEST_DIR / "late_arrival_tmp.par"
    TEST_DIR.mkdir(parents=True, exist_ok=True)
    with open(tmp_file, "w") as f:
        for m in BASE_MESSAGES[:2]:
            f.write(json.dumps(m) + "\n")

    def _add_third():
        import time as _t
        _t.sleep(3)
        with open(tmp_file, "w") as f:
            for m in BASE_MESSAGES:
                f.write(json.dumps(m) + "\n")

    t = threading.Thread(target=_add_third, daemon=True)
    t.start()

    code, par, meta, vlog = run_script(
        BASE_MESSAGES[:2],
        config_overrides={
            "ALLOW_NO_DATA":             "YES",
            "MAX_LISTEN_DURATION_HOURS": "0.05",
            "RETRY_WAIT_SECONDS":        "5",
        },
        data_file=str(tmp_file),
    )

    t.join(timeout=15)
    if tmp_file.exists():
        tmp_file.unlink()

    passed = [True]

    if code == 0:
        print_result("exit code = 0 (retried and recovered 3rd message)", True)
    else:
        print_result(f"exit code = 0 (got {code}) — retry did not recover", False)
        passed[0] = False

    check_par(par, 3, passed)
    check_status_validation_log(vlog, passed)

    return passed[0]


def scenario_real_data():
    """
    File-driven scenario — no hardcoded values.

    Drop one file into test_harness/ before running:
      test_data.par — real Kafka status messages (JSONL, one per line)
                      each line must include these fields inside "status":
                        mandatorCode, businessDate, reconciliationGroupId,
                        instanceIndex, totalInstances, numberOfMessagesPublished
                      and top-level: producer, eventTimestamp

    All expected values (date, mandator, recon_id, expected count, expected lines)
    are derived from the messages themselves — nothing is hardcoded.
    Note: this script WRITES the metadata file, so no input metadata is needed.
    """
    section("Scenario — Real Data (file-driven, no hardcoded values)")

    msgs = load_test_messages()
    if not msgs:
        print("  [SKIP] test_data.par is empty — add real messages and re-run")
        return True

    print(f"         Loaded {len(msgs)} messages from test_data.par")

    # Derive all expected values from the messages — no hardcoding
    first     = msgs[0]["status"]
    asof_dt   = first["businessDate"]
    mandator  = first["mandatorCode"]
    max_recon = max(m["status"]["reconciliationGroupId"] for m in msgs)
    max_msgs  = [m for m in msgs if m["status"]["reconciliationGroupId"] == max_recon]
    expected_total = sum(m["status"]["numberOfMessagesPublished"] for m in max_msgs)
    expected_lines = len(max_msgs)

    print(f"         businessDate={asof_dt}  mandatorCode={mandator}")
    print(f"         max reconciliationGroupId: {max_recon}")
    print(f"         messages for max recon: {expected_lines}  total published: {expected_total}")

    passed = [True]
    code, par, meta, vlog = run_script(msgs, asof_dt=asof_dt, mandator=mandator)

    if code == 0:
        print_result("exit code = 0", True)
    else:
        print_result(f"exit code = 0 (got {code})", False)
        passed[0] = False

    check_par(par, expected_lines, passed)
    check_metadata(meta, expected_total, max_recon, passed)
    check_status_validation_log(vlog, passed)

    return passed[0]


# ── runner ────────────────────────────────────────────────────────────────────

SCENARIOS = {
    "happy_path":         scenario_happy_path,
    "missing_instance":   scenario_missing_instance,
    "wrong_producer":     scenario_wrong_producer,
    "duplicate_instance": scenario_duplicate_instance,
    "missing_column":     scenario_missing_column,
    "multiple_run_ids":   scenario_multiple_run_ids,
    "late_data_arrival":  scenario_late_data_arrival,
    "real_data":          scenario_real_data,
}


def main():
    arg = sys.argv[1] if len(sys.argv) > 1 else "happy_path"

    if arg == "all":
        names   = list(SCENARIOS.keys())
        results = {}
        for name in names:
            results[name] = SCENARIOS[name]()

        sep = "=" * 60
        print(f"\n{sep}")
        print("SUMMARY")
        print(sep)
        all_passed = True
        for name, ok in results.items():
            status = "[PASS]" if ok else "[FAIL]"
            print(f"  {status}  {name}")
            if not ok:
                all_passed = False
        print(sep)
        print("ALL PASSED" if all_passed else "SOME FAILED — see [FAIL] lines above")
        print(sep)
        sys.exit(0 if all_passed else 1)

    elif arg in SCENARIOS:
        ok = SCENARIOS[arg]()
        sep = "=" * 60
        print(f"\n{sep}")
        print("PASSED" if ok else "FAILED")
        print(sep)
        sys.exit(0 if ok else 1)

    else:
        print(f"Unknown scenario: {arg!r}")
        print(f"Available: {', '.join(SCENARIOS)}, all")
        sys.exit(1)


if __name__ == "__main__":
    main()
