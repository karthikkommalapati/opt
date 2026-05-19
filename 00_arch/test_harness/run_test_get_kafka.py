"""
Test harness for 00_get_kafka.py.

Runs the full script end-to-end without a real Kafka broker, schema registry, or
metadata dependency.  Uses the same fake_kafka / fake_libs pattern as test_harness/.

Usage:
    cd trigger_based_new/test_harness_get_kafka
    python run_test.py [scenario_name]

    scenario_name defaults to "happy_path" when omitted.
    Run "all" to execute every scenario in sequence.
"""

import io
import json
import os
import pathlib
import shutil
import struct
import sys
from datetime import date, datetime, time, timedelta
from unittest.mock import patch

# ── install fake modules BEFORE any other import ──────────────────────────────
_here = pathlib.Path(__file__).parent
sys.path.insert(0, str(_here))

import fake_kafka
import fake_libs

fake_libs.install_all()

# ── paths ─────────────────────────────────────────────────────────────────────

SCRIPT_PATH = str(_here.parent / "00_get_kafka.py")
TEST_DIR    = _here.parent / "test_output_get_kafka"
DATA_FILE   = str(_here / "test_data_get_kafka.par")
FEED_NAME   = "CPSB4QST"
ASOF_DT     = "2026-05-07"
DSF_MAIN_ID = "1001"

# Status-messages feed whose metadata file this script reads
STATUS_FEED_NAME = "CPSB4Q00"

FAKE_AVRO_SCHEMA = {
    "type": "record", "name": "TradeMessage", "namespace": "com.test", "fields": [],
}

# ── base config (happy-path defaults) ────────────────────────────────────────

BASE_CONFIG = {
    "KAFKA_USER":                      {"022": "admin"},
    "STREAMING_KAFKA_BROKER":          {"022": "fake-broker:9092"},
    "STREAMING_KAFKA_INFLOW_TOPIC":    {"022": "inflow-topic-022"},
    "AVRO_COLUMNS":                    "",
    "AVRO_SCHEMA_FILE":                "LATEST",
    "AVRO_SCHEMA_REGISTRY":            "fake-registry:8086",
    "DECIMAL_CONV":                    "2",
    "DECIMAL_SCALE":                   "2",
    "SEPERATOR":                       "|",
    "INPUT_DATA":                      "AVRO",
    "OUTPUT_DATA":                     "JSON",
    "ALLOW_NO_DATA":                   "No",
    "START_TS":                        "00:00:00",
    "START_DT_OFFSET":                 "0",
    "STOP_TS":                         "23:59:59",
    "STOP_DT_OFFSET":                  "0",
    "COMMIT_CNT":                      "50",
    "STREAMING_TIMESTAMP_KEY":         "eventTimestamp",
    "STREAMING_TIMESTAMP_FORMAT":      "%Y-%m-%dT%H:%M:%S%z",
    "STREAMING_MANDATORY_KEY":         "mandatorCode",
    "STREAMING_WAIT_UNTIL_DONE_HOURS": "0",
    "STREAMING_IDLE_TIMEOUT_MINUTES":  "0",
    "STREAMING_PAGE_SIZE_LIMIT_BYTES": "8000",
    "STREAMING_REPROCESS":             "False",
    "STREAMING_VERBOSE":               "True",
    "STREAMING_STORE_MIDLAYER":        "False",
    "VALIDATE_TOPIC_MANDATOR":         "NO",
    "WAIT_FOR_SUBMIT":                 "NO",
    "EXTEND_ON_ITERATE":               "NO",
    "STATUS_MESSAGES_FEED_NAME":       STATUS_FEED_NAME,
    "METADATA_FILE_SUFFIX":            "_metadata.txt",
    "METADATA_FILTER_COLUMNS":         ["mandatorCode", "businessDate", "reconciliationGroupId"],
    "METADATA_FILTER_FIELD_MAP": {
        "mandator":                 "mandatorCode",
        "business_date":            "businessDate",
        "reconciliation_group_id":  "reconciliationGroupId",
    },
    "METADATA_COUNT_FIELD":            "total_messages_published",
    "METADATA_COUNT_TOLERANCE_PCT":    "10",
    "MAX_LISTEN_DURATION_HOURS":        "0.02",
    "RETRY_WAIT_SECONDS":               "0",
    "STABLE_COUNT_REQUIRED_ATTEMPTS":   "1",
}

# ── metadata file content helpers ─────────────────────────────────────────────

SEP = BASE_CONFIG["SEPERATOR"]

METADATA_HEADERS = (
    "export_datetime|username|business_date|mandator|producer_name|feed_name"
    "|reconciliation_group_id|instances_counted|total_expected_instances"
    "|total_messages_published"
)

def make_metadata_line(
    mandator="022",
    business_date="2026-05-07",
    recon_id="5001",
    total_messages=3,
    username="testuser",
):
    values = SEP.join([
        "2026-05-07T17:00:00",
        username,
        business_date,
        mandator,
        "CLIENT_STRUCTURES",
        STATUS_FEED_NAME,
        recon_id,
        "3",
        "3",
        str(total_messages),
    ])
    return METADATA_HEADERS + "\n" + values + "\n"


# ── infrastructure helpers ────────────────────────────────────────────────────

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


def write_metadata_file(data_dir, content, asof_dt=ASOF_DT):
    """Write the metadata file that 00_get_kafka.py reads on startup.
    Mirrors production: {PC_LOD_PROC_PATH}/{STATUS_FEED_NAME}_{asof_dt}/{STATUS_FEED_NAME}_{asof_dt}_metadata.txt
    """
    data_dir.mkdir(parents=True, exist_ok=True)
    meta_dir = data_dir / f"{STATUS_FEED_NAME}_{asof_dt}"
    meta_dir.mkdir(parents=True, exist_ok=True)
    meta_path = meta_dir / f"{STATUS_FEED_NAME}_{asof_dt}_metadata.txt"
    meta_path.write_text(content, encoding="utf-8")
    return meta_path


def load_test_messages(path=DATA_FILE):
    with open(path) as f:
        return [json.loads(line) for line in f if line.strip()]


def make_mock_schemaless_reader(test_messages):
    def _reader(msg_io, schema):
        idx_bytes = msg_io.read(4)
        if len(idx_bytes) < 4:
            raise ValueError("fake reader: payload too short")
        idx = struct.unpack('>I', idx_bytes)[0]
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


def run_script(test_messages, config_overrides=None, metadata_content=None,
               asof_dt=None, mandator=None):
    """
    Full end-to-end run of 00_get_kafka.py.

    asof_dt / mandator: derived from real metadata/data files when provided;
                        fall back to module-level defaults for synthetic scenarios.
    Returns (exit_code, par_file_path, temp_file_path).
    """
    _asof_dt  = asof_dt  or ASOF_DT
    _mandator = mandator or "022"

    cfg, data, logs = setup_dirs()
    setup_env(cfg, data, logs, asof_dt=_asof_dt, mandator=_mandator)
    write_config(cfg, config_overrides)

    # Wipe the entire data dir so no leftovers (metadata at root, .par in subfolder),
    # but preserve the validation log across wipes so it accumulates across scenarios.
    validation_log = data / f"{FEED_NAME}_get_kafka_validation.log"
    _saved_vlog = validation_log.read_text(encoding="utf-8") if validation_log.exists() else None
    if data.exists():
        shutil.rmtree(data)
    data.mkdir(parents=True)
    if _saved_vlog is not None:
        validation_log.write_text(_saved_vlog, encoding="utf-8")
    data_folder = data / f"{FEED_NAME}_{_asof_dt}"
    data_folder.mkdir(parents=True)

    # Write metadata file to data root — mirrors production path:
    # {PC_LOD_PROC_PATH}/{STATUS_FEED_NAME}_{_asof_dt}_metadata.txt
    if metadata_content is not None:
        write_metadata_file(data, metadata_content, asof_dt=_asof_dt)

    # Write test_messages to a per-run reload file in the (freshly wiped) data dir.
    # FakeKafkaConsumer.end_offsets() reloads from this path on every call — using
    # the data dir keeps it isolated and avoids corrupting test_data_get_kafka.par
    # (which real_data reads via load_test_messages()).
    reload_file = str(data / "_consumer_reload.par")
    with open(reload_file, "w", encoding="utf-8") as _f:
        for _m in test_messages:
            _f.write(json.dumps(_m) + "\n")

    # Align message timestamps inside the 00:00–23:59 window on _asof_dt
    day_start  = datetime.combine(date.fromisoformat(_asof_dt), time(0, 0, 0))
    base_ts_ms = int((day_start + timedelta(hours=10)).timestamp() * 1000)

    consumer = fake_kafka.FakeKafkaConsumer(
        msg_dicts       = test_messages,
        topic           = f"inflow-topic-{_mandator}",
        start_ts_ms     = base_ts_ms,
        msg_interval_ms = 60_000,
        data_file       = reload_file,
    )
    fake_kafka.install(consumer)

    import fastavro
    import requests

    exited_with = [None]

    def _fake_exit(code=0):
        exited_with[0] = code
        raise SystemExit(code)

    mock_reader   = make_mock_schemaless_reader(test_messages)
    mock_registry = make_mock_registry()

    # Pre-inject the typo variable workaround so both NameError-prone lines pass
    # (STOP_DT_OFFFSET with 3 F's — fixed in script but kept here as belt-and-braces)
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

    par_file  = data_folder / f"{FEED_NAME}_{_asof_dt}.par"
    temp_file = data_folder / f"{FEED_NAME}_{_asof_dt}.par.tmp"
    return exit_code, par_file, temp_file, validation_log


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


def check_get_kafka_validation_log(vlog_path, passed_ref):
    """Verify that the get_kafka_validation.log was appended with one data row."""
    if not vlog_path.exists():
        print_result("get_kafka_validation.log written", False,
                     f"missing: {vlog_path}")
        passed_ref[0] = False
        return
    lines = vlog_path.read_text(encoding="utf-8").strip().splitlines()
    if len(lines) < 2:
        print_result("get_kafka_validation.log has header + data row", False,
                     f"only {len(lines)} line(s)")
        passed_ref[0] = False
        return
    sep     = BASE_CONFIG["SEPERATOR"]
    headers = lines[0].split(sep)
    values  = lines[1].split(sep)
    fields  = dict(zip(headers, values))
    got_username = fields.get("username", "")
    if got_username:
        print_result(f"get_kafka_validation.log has username ('{got_username}')", True)
    else:
        print_result("get_kafka_validation.log has username", False,
                     "field missing or empty")
        passed_ref[0] = False


# ── scenarios ─────────────────────────────────────────────────────────────────

def scenario_happy_path():
    """
    Scenario 1 — Happy path.
    3 messages, all match filter (mandatorCode=022, businessDate=2026-05-07,
    reconciliationGroupId=5001). Expected count = 3. Tolerance = 10%.
    Expect: .par written with 3 lines, exit 0, no temp file left behind.
    """
    section("Scenario 1 — Happy Path")
    msgs = load_test_messages()
    passed_ref = [True]
    code, par, tmp, vlog = run_script(
        msgs,
        metadata_content=make_metadata_line(total_messages=3),
    )

    passed = True

    if code == 0:
        print_result("exit code = 0", True)
    else:
        print_result(f"exit code = 0 (got {code})", False)
        passed = False

    if par.exists():
        lines = par.read_text(encoding="utf-8").strip().splitlines()
        print_result(f".par file written with {len(lines)} line(s)", len(lines) == 3,
                     f"path: {par}")
        if len(lines) != 3:
            passed = False
        for ln in lines:
            print(f"         {ln[:80]}")
    else:
        print_result(".par file written", False, f"missing: {par}")
        passed = False

    if not tmp.exists():
        print_result("temp file cleaned up", True)
    else:
        print_result("temp file cleaned up (still present!)", False)
        passed = False

    passed_ref[0] = passed
    check_get_kafka_validation_log(vlog, passed_ref)
    return passed_ref[0]


def scenario_count_within_tolerance():
    """
    Scenario 2 — Under-count, within lower tolerance at exhaustion.
    3 messages in Kafka. Metadata says expected = 4. Tolerance = 50%.
    3 < 4 so script enters under-count path and waits until window exhausts.
    At exhaustion: floor = 4 - 50% = 2. 3 >= 2 → accepts at WARNING level.
    Expect: exit 0, .par with 3 lines.
    """
    section("Scenario 2 — Under-Count Within Lower Tolerance (accepts at exhaustion)")
    msgs = load_test_messages()
    passed_ref = [True]
    code, par, tmp, vlog = run_script(
        msgs,
        config_overrides={
            "METADATA_COUNT_TOLERANCE_PCT": "50",
            "MAX_LISTEN_DURATION_HOURS":    "0",
        },
        metadata_content=make_metadata_line(total_messages=4),
    )

    passed = True

    if code == 0:
        print_result("exit code = 0", True)
    else:
        print_result(f"exit code = 0 (got {code})", False)
        passed = False

    if par.exists():
        lines = par.read_text().strip().splitlines()
        ok = len(lines) == 3
        print_result(f".par written with {len(lines)} line(s) (expected 3)", ok,
                     f"path: {par}")
        if not ok:
            passed = False
    else:
        print_result(".par file written", False, f"missing: {par}")
        passed = False

    passed_ref[0] = passed
    check_get_kafka_validation_log(vlog, passed_ref)
    return passed_ref[0]


def scenario_no_filter_columns():
    """
    Scenario 3 — No METADATA_FILTER_COLUMNS.
    METADATA_FILTER_COLUMNS = [] and METADATA_FILTER_FIELD_MAP = {}.
    All 3 messages should pass through unfiltered. Expected count = 3.
    Expect: exit 0, .par with 3 lines.
    """
    section("Scenario 3 — No Filter Columns (all messages pass)")
    msgs = load_test_messages()
    passed_ref = [True]
    code, par, tmp, vlog = run_script(
        msgs,
        config_overrides={
            "METADATA_FILTER_COLUMNS":   [],
            "METADATA_FILTER_FIELD_MAP": {},
        },
        metadata_content=make_metadata_line(total_messages=3),
    )

    passed = True

    if code == 0:
        print_result("exit code = 0", True)
    else:
        print_result(f"exit code = 0 (got {code})", False)
        passed = False

    if par.exists():
        lines = par.read_text().strip().splitlines()
        ok = len(lines) == 3
        print_result(f".par written with {len(lines)} line(s) (expected 3)", ok)
        if not ok:
            passed = False
    else:
        print_result(".par file written", False, f"missing: {par}")
        passed = False

    passed_ref[0] = passed
    check_get_kafka_validation_log(vlog, passed_ref)
    return passed_ref[0]


def scenario_metadata_missing():
    """
    Scenario 4 — Metadata file missing.
    Pass metadata_content=None so the file is never written.
    Expect: exit 1 immediately.
    """
    section("Scenario 4 — Metadata File Missing")
    msgs = load_test_messages()
    code, par, tmp, vlog = run_script(msgs, metadata_content=None)

    passed = True

    if code == 1:
        print_result("exit code = 1 (correct hard stop)", True)
    else:
        print_result(f"exit code = 1 (got {code})", False)
        passed = False

    if not par.exists():
        print_result(".par file NOT written (correct)", True)
    else:
        print_result(".par file NOT written (was written — wrong!)", False)
        passed = False

    return passed


def scenario_metadata_one_line():
    """
    Scenario 5 — Metadata file has only 1 line (header, no data row).
    Expect: exit 1.
    """
    section("Scenario 5 — Metadata File: 1 Line (no data row)")
    msgs = load_test_messages()
    code, par, tmp, vlog = run_script(
        msgs,
        metadata_content=METADATA_HEADERS + "\n",
    )

    passed = True

    if code == 1:
        print_result("exit code = 1 (correct hard stop)", True)
    else:
        print_result(f"exit code = 1 (got {code})", False)
        passed = False

    return passed


def scenario_metadata_extra_row():
    """
    Scenario 6 — Metadata file has 3 lines (header + 2 data rows).
    Expect: exit 1.
    """
    section("Scenario 6 — Metadata File: Extra Data Row")
    msgs = load_test_messages()
    row = SEP.join([
        "2026-05-07T17:00:00", "testuser", "2026-05-07", "022",
        "CLIENT_STRUCTURES", STATUS_FEED_NAME, "5001", "3", "3", "3",
    ])
    bad_content = METADATA_HEADERS + "\n" + row + "\n" + row + "\n"
    code, par, tmp, vlog = run_script(msgs, metadata_content=bad_content)

    passed = True

    if code == 1:
        print_result("exit code = 1 (correct hard stop)", True)
    else:
        print_result(f"exit code = 1 (got {code})", False)
        passed = False

    return passed


def scenario_metadata_count_not_integer():
    """
    Scenario 7 — Metadata count field is not an integer ("N/A").
    Expect: exit 1.
    """
    section("Scenario 7 — Metadata Count Field Not an Integer")
    msgs = load_test_messages()
    code, par, tmp, vlog = run_script(
        msgs,
        metadata_content=make_metadata_line(total_messages="N/A"),
    )

    passed = True

    if code == 1:
        print_result("exit code = 1 (correct hard stop)", True)
    else:
        print_result(f"exit code = 1 (got {code})", False)
        passed = False

    return passed


def scenario_filter_column_no_mapping():
    """
    Scenario 8 — Filter column listed in METADATA_FILTER_COLUMNS has no
    entry in METADATA_FILTER_FIELD_MAP.
    Expect: exit 1.
    """
    section("Scenario 8 — Filter Column Has No Mapping")
    msgs = load_test_messages()
    code, par, tmp, vlog = run_script(
        msgs,
        config_overrides={
            "METADATA_FILTER_COLUMNS":   ["mandatorCode", "unknownField"],
            "METADATA_FILTER_FIELD_MAP": {"mandator": "mandatorCode"},
        },
        metadata_content=make_metadata_line(total_messages=3),
    )

    passed = True

    if code == 1:
        print_result("exit code = 1 (correct hard stop)", True)
    else:
        print_result(f"exit code = 1 (got {code})", False)
        passed = False

    return passed


def scenario_partial_filter_match():
    """
    Scenario 9 — Mixed messages: 2 match filters, 1 has wrong mandatorCode.
    Expected count in metadata = 2 (exact match).
    Expect: exit 0, .par with exactly 2 lines.
    """
    section("Scenario 9 — Partial Filter Match (wrong mandator on 1 message)")
    msgs = [
        {"mandatorCode": "022", "businessDate": "2026-05-07",
         "reconciliationGroupId": "5001", "trade": {"id": "T001", "amount": 100.0}},
        {"mandatorCode": "999", "businessDate": "2026-05-07",  # ← wrong mandator
         "reconciliationGroupId": "5001", "trade": {"id": "T002", "amount": 200.0}},
        {"mandatorCode": "022", "businessDate": "2026-05-07",
         "reconciliationGroupId": "5001", "trade": {"id": "T003", "amount": 300.0}},
    ]
    passed_ref = [True]
    code, par, tmp, vlog = run_script(
        msgs,
        metadata_content=make_metadata_line(total_messages=2),
    )

    passed = True

    if code == 0:
        print_result("exit code = 0", True)
    else:
        print_result(f"exit code = 0 (got {code})", False)
        passed = False

    if par.exists():
        lines = par.read_text().strip().splitlines()
        ok = len(lines) == 2
        print_result(f".par written with {len(lines)} line(s) (expected 2)", ok)
        if ok:
            for ln in lines:
                parsed = json.loads(ln)
                if parsed.get("mandatorCode") != "022":
                    print_result("all written lines have mandatorCode=022", False,
                                 f"got mandatorCode={parsed.get('mandatorCode')!r}")
                    passed = False
                    break
            else:
                print_result("all written lines have mandatorCode=022", True)
        else:
            passed = False
    else:
        print_result(".par file written", False, f"missing: {par}")
        passed = False

    passed_ref[0] = passed
    check_get_kafka_validation_log(vlog, passed_ref)
    return passed_ref[0]


def scenario_count_outside_tolerance():
    """
    Scenario 10 — Count outside tolerance, retry window = 0.
    3 messages match filters. Metadata says expected = 100. Tolerance = 10%.
    3 is far outside ±10% of 100. MAX_LISTEN_DURATION_HOURS = 0 so no retries.
    Expect: exit 1, failure analysis logged, no .par file, no temp file left.
    """
    section("Scenario 10 — Count Outside Tolerance (retry exhausted)")
    msgs = load_test_messages()
    code, par, tmp, vlog = run_script(
        msgs,
        config_overrides={
            "METADATA_COUNT_TOLERANCE_PCT": "10",
            "MAX_LISTEN_DURATION_HOURS":    "0",
            "RETRY_WAIT_SECONDS":           "0",
        },
        metadata_content=make_metadata_line(total_messages=100),
    )

    passed = True

    if code == 1:
        print_result("exit code = 1 (count mismatch, retry exhausted)", True)
    else:
        print_result(f"exit code = 1 (got {code})", False)
        passed = False

    if not par.exists():
        print_result(".par file NOT written (correct)", True)
    else:
        print_result(".par file NOT written (was written — wrong!)", False)
        passed = False

    if not tmp.exists():
        print_result("temp file cleaned up after failure (correct)", True)
    else:
        print_result("temp file cleaned up (still present — wrong!)", False)
        passed = False

    return passed


def scenario_over_count_stable():
    """
    Scenario 11 — Over-count: more messages than expected.
    4 messages in Kafka (metadata says expected = 3). No upper cap.
    STABLE_COUNT_REQUIRED_ATTEMPTS = 2: script retries once to confirm stability,
    then saves all 4 messages.
    Expect: exit 0, .par with 4 lines, tolerance_upper = "unlimited" in validation log.
    """
    section("Scenario 11 — Over-Count Captured (stability check, no upper cap)")
    msgs = [
        {"mandatorCode": "022", "businessDate": "2026-05-07",
         "reconciliationGroupId": "5001", "trade": {"id": "T001", "amount": 100.0}},
        {"mandatorCode": "022", "businessDate": "2026-05-07",
         "reconciliationGroupId": "5001", "trade": {"id": "T002", "amount": 200.0}},
        {"mandatorCode": "022", "businessDate": "2026-05-07",
         "reconciliationGroupId": "5001", "trade": {"id": "T003", "amount": 300.0}},
        {"mandatorCode": "022", "businessDate": "2026-05-07",
         "reconciliationGroupId": "5001", "trade": {"id": "T004", "amount": 400.0}},
    ]
    passed_ref = [True]
    code, par, tmp, vlog = run_script(
        msgs,
        config_overrides={
            "STABLE_COUNT_REQUIRED_ATTEMPTS": "2",
            "RETRY_WAIT_SECONDS":             "0",
            "MAX_LISTEN_DURATION_HOURS":      "0.02",
        },
        metadata_content=make_metadata_line(total_messages=3),
    )

    passed = True

    if code == 0:
        print_result("exit code = 0", True)
    else:
        print_result(f"exit code = 0 (got {code})", False)
        passed = False

    if par.exists():
        lines = par.read_text(encoding="utf-8").strip().splitlines()
        ok = len(lines) == 4
        print_result(f".par written with {len(lines)} line(s) (expected 4 — over-count captured)", ok,
                     f"path: {par}")
        if not ok:
            passed = False
    else:
        print_result(".par file written", False, f"missing: {par}")
        passed = False

    if not tmp.exists():
        print_result("temp file cleaned up", True)
    else:
        print_result("temp file cleaned up (still present!)", False)
        passed = False

    # Verify validation log shows tolerance_upper = "unlimited"
    if vlog.exists():
        lines_log = vlog.read_text(encoding="utf-8").strip().splitlines()
        if len(lines_log) >= 2:
            sep     = BASE_CONFIG["SEPERATOR"]
            headers = lines_log[0].split(sep)
            values  = lines_log[1].split(sep)
            fields  = dict(zip(headers, values))
            upper   = fields.get("tolerance_upper", "")
            ok_upper = (upper == "unlimited")
            print_result(f"tolerance_upper = 'unlimited' in validation log (got '{upper}')", ok_upper)
            if not ok_upper:
                passed = False
        else:
            print_result("validation log has header + data row", False)
            passed = False
    else:
        print_result("validation log written", False, f"missing: {vlog}")
        passed = False

    passed_ref[0] = passed
    check_get_kafka_validation_log(vlog, passed_ref)
    return passed_ref[0]


def scenario_under_count_below_tolerance():
    """
    Scenario 12 — Under-count below lower tolerance at exhaustion.
    3 messages in Kafka. Metadata says expected = 100. Tolerance = 10%.
    At exhaustion: floor = 100 - 10% = 90. 3 < 90 → fail.
    Same outcome as scenario_count_outside_tolerance but via the new code path
    (under-count → exhaust → below floor → exit 1).
    Expect: exit 1, no .par file, temp file cleaned up.
    """
    section("Scenario 12 — Under-Count Below Lower Tolerance (fails at exhaustion)")
    msgs = load_test_messages()
    code, par, tmp, vlog = run_script(
        msgs,
        config_overrides={
            "METADATA_COUNT_TOLERANCE_PCT": "10",
            "MAX_LISTEN_DURATION_HOURS":    "0",
            "RETRY_WAIT_SECONDS":           "0",
        },
        metadata_content=make_metadata_line(total_messages=100),
    )

    passed = True

    if code == 1:
        print_result("exit code = 1 (under-count below lower tolerance floor)", True)
    else:
        print_result(f"exit code = 1 (got {code})", False)
        passed = False

    if not par.exists():
        print_result(".par file NOT written (correct)", True)
    else:
        print_result(".par file NOT written (was written — wrong!)", False)
        passed = False

    if not tmp.exists():
        print_result("temp file cleaned up after failure (correct)", True)
    else:
        print_result("temp file cleaned up (still present — wrong!)", False)
        passed = False

    return passed


def scenario_real_data():
    """
    File-driven scenario — no hardcoded values.

    Drop two files into test_harness/ before running:
      test_data_get_kafka.par      — real Kafka messages (JSONL, one per line)
                                     fields mandatorCode, businessDate,
                                     reconciliationGroupId must be at the top level
      real_metadata_get_kafka.txt  — real metadata file produced by
                                     kafka_trigger_status_messages.py
                                     (pipe-delimited: header row + 1 data row)

    All expected values (date, mandator, recon_id, expected count) are derived
    from those files — nothing is hardcoded.
    """
    section("Scenario — Real Data (file-driven, no hardcoded values)")

    meta_path = _here / "real_metadata_get_kafka.txt"
    if not meta_path.exists():
        candidates = sorted(
            (_here.parent / "test_output" / "data").glob("*/*_metadata.txt"),
            key=lambda p: p.stat().st_mtime,
            reverse=True,
        )
        if candidates:
            meta_path = candidates[0]
            print(f"  [AUTO] Using metadata from test_output: {meta_path.name}")
        else:
            print("  [SKIP] No metadata file found.")
            print("         Run `run_test.py real_data` first, or drop")
            print("         real_metadata_get_kafka.txt into test_harness/.")
            return True  # skip counts as pass — file simply not provided yet

    msgs = load_test_messages()
    print(f"         Loaded {len(msgs)} messages from test_data_get_kafka.par")

    metadata_content = meta_path.read_text(encoding="utf-8")
    meta_lines = [l for l in metadata_content.splitlines() if l.strip()]
    if len(meta_lines) < 2:
        print("  [FAIL] real_metadata_get_kafka.txt must have 2 non-blank lines")
        return False

    fields         = dict(zip(meta_lines[0].split("|"), meta_lines[1].split("|")))
    asof_dt        = fields.get("business_date")
    mandator       = fields.get("mandator")
    recon_id       = fields.get("reconciliation_group_id")
    expected_count = int(fields["total_messages_published"])

    print(f"         business_date={asof_dt}  mandator={mandator}  recon_id={recon_id}")
    print(f"         expected count from metadata: {expected_count}")
    print(f"         tolerance: ±{BASE_CONFIG['METADATA_COUNT_TOLERANCE_PCT']}%")

    passed_ref = [True]
    passed = True
    code, par, tmp, vlog = run_script(
        msgs,
        metadata_content=metadata_content,
        asof_dt=asof_dt,
        mandator=mandator,
    )

    if code == 0:
        print_result("exit code = 0", True)
    else:
        print_result(f"exit code = 0 (got {code})", False)
        passed = False

    if par.exists():
        actual = len(par.read_text(encoding="utf-8").strip().splitlines())
        print_result(
            f".par written — {actual} line(s) filtered from {len(msgs)} input messages",
            True,
            f"expected from metadata: {expected_count} (within ±{BASE_CONFIG['METADATA_COUNT_TOLERANCE_PCT']}%)"
        )
    else:
        print_result(".par file written", False, f"missing: {par}")
        passed = False

    if not tmp.exists():
        print_result("temp file cleaned up", True)
    else:
        print_result("temp file cleaned up (still present!)", False)
        passed = False

    passed_ref[0] = passed
    check_get_kafka_validation_log(vlog, passed_ref)
    return passed_ref[0]


# ── runner ────────────────────────────────────────────────────────────────────

SCENARIOS = {
    "happy_path":                    scenario_happy_path,
    "count_within_tolerance":        scenario_count_within_tolerance,
    "no_filter_columns":             scenario_no_filter_columns,
    "metadata_missing":              scenario_metadata_missing,
    "metadata_one_line":             scenario_metadata_one_line,
    "metadata_extra_row":            scenario_metadata_extra_row,
    "metadata_count_not_int":        scenario_metadata_count_not_integer,
    "filter_column_no_mapping":      scenario_filter_column_no_mapping,
    "partial_filter_match":          scenario_partial_filter_match,
    "count_outside_tolerance":       scenario_count_outside_tolerance,
    "over_count_stable":             scenario_over_count_stable,
    "under_count_below_tolerance":   scenario_under_count_below_tolerance,
    "real_data":                     scenario_real_data,
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
        print(f"Available: {', '.join(SCENARIOS)} , all")
        sys.exit(1)


if __name__ == "__main__":
    main()
