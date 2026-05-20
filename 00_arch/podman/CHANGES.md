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
