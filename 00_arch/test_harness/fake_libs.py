"""
Fake dsf_logging and assertf modules for the test harness.
Call install_all() before exec'ing the main script.
"""
import logging
import sys


# ── fake assertf ──────────────────────────────────────────────────────────────

def assert_dict(v):
    if not isinstance(v, dict):
        raise AssertionError(f"Expected dict, got {type(v).__name__}: {v!r}")
    return v

def assert_set(v):
    if v is None:
        raise AssertionError("Expected set-like, got None")
    return set(v) if not isinstance(v, set) else v

def assert_tuple(v):
    if v is None:
        raise AssertionError("Expected tuple-like, got None")
    return tuple(v) if not isinstance(v, (tuple, list)) else tuple(v)

def assert_int(v):
    return int(v)

def assert_path(v):
    return v


def _install_assertf():
    mod = type(sys)("assertf")
    mod.assert_dict = assert_dict
    mod.assert_set = assert_set
    mod.assert_tuple = assert_tuple
    mod.assert_int = assert_int
    mod.assert_path = assert_path
    mod.__all__ = ["assert_dict", "assert_set", "assert_tuple", "assert_int", "assert_path"]
    sys.modules["assertf"] = mod


# ── fake dsf_logging ──────────────────────────────────────────────────────────

class _FakeLogger:
    def __init__(self):
        self._log = logging.getLogger("dsf")
        if not self._log.handlers:
            h = logging.StreamHandler(sys.stdout)
            h.setFormatter(logging.Formatter("[%(levelname)s] %(message)s"))
            self._log.addHandler(h)
        self._log.setLevel(logging.DEBUG)

    def get_logger(self, name=None, output_path=None, level=20, verbose=True):
        pass

    def log_starting_process(self, file, level=20):
        self._log.info(f"Starting: {file}")

    def log_msg(self, msg, level=20):
        lv = {10: logging.DEBUG, 20: logging.INFO,
              30: logging.WARNING, 40: logging.ERROR}.get(level, logging.INFO)
        self._log.log(lv, msg)

    def log_error_msg(self, msg, err=None):
        self._log.error(f"{msg}: {err}")
        return RuntimeError(f"{msg}: {err}")

    def log_error(self, msg):
        self._log.error(msg)
        return RuntimeError(msg)


class DSF_logging:
    def __new__(cls):
        return _FakeLogger()


def _install_dsf_logging():
    mod = type(sys)("dsf_logging")
    mod.DSF_logging = DSF_logging
    sys.modules["dsf_logging"] = mod


def install_all():
    _install_assertf()
    _install_dsf_logging()
