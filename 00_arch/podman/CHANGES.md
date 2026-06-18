# Files to Copy + Changes Required

## Step 1 — Files to copy from `trigger_based_new/`

Copy these files into this folder (`trigger_based_new_podman/`):

| File | Copy as |
|---|---|
| `kafka_trigger_status_messages.py` | `kafka_trigger_status_messages.py` |
| `00_get_kafka.py` | `00_get_kafka.py` |

Do NOT copy the config files — use `local_config.json` in this folder instead.
Do NOT copy the test harness — it is not needed here.

---

## Step 2 — Changes in `kafka_trigger_status_messages.py`

### Change 1 — Schema registry: remove SSL (lines 118–120)

Find this block:
```python
session = requests.Session()
session.verify = ca_file
session.cert = (sslcert, sslkey)
```

Replace with:
```python
session = requests.Session()
session.verify = False
```

### Change 2 — Schema registry: use HTTP not HTTPS (line 126)

Find:
```python
url = f"https://{reg_addr}{endpoint}"
```

Replace with:
```python
url = f"http://{reg_addr}{endpoint}"
```

### Change 3 — KafkaConsumer: remove SSL, use PLAINTEXT (lines 818–822)

Find:
```python
        ,security_protocol="SSL"
        ,ssl_check_hostname=False
        ,ssl_cafile=SSL_CA_FILE
        ,ssl_certfile=SSL_CLIENT_CERT
        ,ssl_keyfile=SSL_KEY,
```

Replace with:
```python
        ,security_protocol="PLAINTEXT"
```

---

## Changes in `00_get_kafka.py`

Exactly the same 3 changes, at these line numbers:

| Change | Line(s) |
|---|---|
| `session.verify = False` + remove `session.cert` line | 105–107 |
| `http://` in URL | 113 |
| `security_protocol="PLAINTEXT"` + remove 4 ssl_ lines | 807–811 |

The edits are identical in structure to the ones above.

---

## Summary

- **Total lines changed per script**: ~6 lines
- **Business logic**: zero changes
- **Config**: use `local_config.json` in this folder (already created)

---

---

# 00_get_kafka.py — Logic Fixes and Enhancements (2026-06-17)

## Fix 1 — Start offset not refreshed when topic is empty at startup

**Problem:** `all_start_offset_ts` (the Kafka start offset for the time window) was computed
once before the retry loop. If the topic was empty when the job fired, `offsets_for_times`
returned `None` for all partitions and that `None` was cached forever. Messages published
after the script started were never seen, regardless of how many retries remained.

This affects both local Podman testing (empty topic) and production (upstream publisher
delayed at job start time).

**Fix:** At the top of each retry loop iteration, after the stop-offset refresh, any
partition still holding `None` in `all_start_offset_ts` is re-queried with the same
`TS_UTC_START`. If messages have arrived since the last attempt the offset is filled in
and picked up immediately on the same iteration.

```python
_none_parts = {p for p, v in all_start_offset_ts.items() if v is None}
if _none_parts:
    _refreshed = assert_dict(consumer.offsets_for_times(
        {p: int(TS_UTC_START * 1000) for p in _none_parts}
    ))
    for _p, _v in _refreshed.items():
        if _v is not None:
            all_start_offset_ts[_p] = _v
```

Zero overhead when all partitions already have valid offsets.

---

## Feature 2 — Configurable over-count behaviour (`OVER_COUNT_BEHAVIOR`)

**Background:** When the script receives more messages than `EXPECTED_COUNT` the existing
behaviour (STABILITY mode) waits for the count to stop changing across N consecutive
retry attempts before accepting. This is robust but can hold the job open indefinitely
while an overloaded publisher keeps sending.

**New config keys** (add to `get_kafka_config.json`):

| Key | Values | Default | Purpose |
|---|---|---|---|
| `OVER_COUNT_BEHAVIOR` | `"STABILITY"` / `"WAIT"` | `"STABILITY"` | Which mode to use when count exceeds expected |
| `OVER_COUNT_WAIT_MINUTES` | integer | `5` | Minutes to wait before accepting in WAIT mode |

### STABILITY mode (default — existing behaviour unchanged)

- Count ≥ expected → wait for `STABLE_COUNT_REQUIRED_ATTEMPTS` consecutive reads with
  the same count → accept.
- One-time grace extension of `RETRY_WAIT_SECONDS` if count first hits target near the
  deadline (prevents a false "stability unconfirmed" warning).

### WAIT mode (new — business-requested)

| Count at attempt | Action |
|---|---|
| == expected | Accept immediately — no timer started |
| > expected (first time) | Start a one-time `OVER_COUNT_WAIT_MINUTES` timer; extend `retry_deadline` to cover it |
| > expected (subsequent) | Log count / time remaining; do nothing to the timer |
| Timer expires | Accept whatever count is present at that moment |
| < expected | Unchanged — retry as normal until deadline, then tolerance check |

Key design rules:
- The timer starts from the **first moment** count goes over, not from script startup.
- If count starts low and only crosses expected mid-run, the full `OVER_COUNT_WAIT_MINUTES`
  is still available from that crossing point.
- The timer never resets or extends if more messages arrive during the wait.
- `retry_deadline` is extended to `over_count_deadline` automatically so the loop does
  not exit before the timer fires.

### Startup log (both modes)

The script logs which mode is active at startup and prints the exact config keys to
change to switch to the other mode. Example:

```
OVER_COUNT_BEHAVIOR : STABILITY (wait for 2 consecutive stable counts).
  To use a fixed wait window instead: set OVER_COUNT_BEHAVIOR=WAIT and OVER_COUNT_WAIT_MINUTES=5
```

### Switching modes

To switch to WAIT mode, update `get_kafka_config.json`:
```json
"OVER_COUNT_BEHAVIOR": "WAIT",
"OVER_COUNT_WAIT_MINUTES": 5
```

To revert to STABILITY mode:
```json
"OVER_COUNT_BEHAVIOR": "STABILITY"
```
