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

---

# kafka_trigger_status_messages.py — Configurable instance validation mode (2026-07-24)

## Feature: `INSTANCE_VALIDATION_MODE`

**Background:** `validate_instance_and_get_max_runid` assumed every producer numbers its
instances `0` to `totalInstances - 1`, contiguous and zero-based. A second producer
publishing to the same topic (same message structure, differentiated by `PRODUCER_FILTER`)
uses non-sequential/arbitrary `instanceIndex` values — the only requirement for that
producer is that `totalInstances` distinct index values show up, not that they form a
specific range.

**New config key** (`status_messages_config.json`):

| Key | Values | Default | Purpose |
|---|---|---|---|
| `INSTANCE_VALIDATION_MODE` | `"SEQUENTIAL"` / `"UNIQUE_COUNT"` | `"SEQUENTIAL"` | How instance completeness is validated |

### SEQUENTIAL mode (default — existing behaviour unchanged)

- Expected set = `{0, 1, ..., totalInstances-1}`. Missing = expected − present.
- Fails (retryable) if any expected index is absent.
- Fails (hard) if the same index appears more than once (existing duplicate check, unchanged).

### UNIQUE_COUNT mode (new)

- No fixed expected range. Just compares `len(distinct instanceIndex values)` to `totalInstances`.
- Exactly equal → pass.
- Fewer than expected → fails, retryable (same as missing instances today — waiting may resolve it).
- More than expected → fails too, through the same retry/exit path (an anomaly — extra unique
  instances won't be resolved by waiting, but no special early-exit was added; it just fails
  like any other validation failure and is subject to the existing `ALLOW_NO_DATA` / retry behaviour).
- Row-level duplicate `instanceIndex` values still hard-fail, same as `SEQUENTIAL` mode.

**Code changes** (`kafka_trigger_status_messages.py`):
- `validate_instance_and_get_max_runid(...)` takes a new `validation_mode="SEQUENTIAL"` parameter;
  branches only at the missing/expected computation (~line 292-326). Duplicate check untouched.
- Both call sites (initial pass + retry-loop pass) now pass `INSTANCE_VALIDATION_MODE` through.
- The final failure report's "publisher_issue" heuristic (compares present indices to an expected
  0..N-1 range) is skipped entirely when mode is `UNIQUE_COUNT`, since there's no range to compare against.

**Switching modes** — add/edit in `status_messages_config.json`:
```json
"INSTANCE_VALIDATION_MODE": "UNIQUE_COUNT"
```
Omit the key (or set `"SEQUENTIAL"`) to keep today's behaviour.

---

# 00_get_kafka.py — Multi-value support for `PRE_FILTER_VALUES` (2026-07-27)

## Feature: OR-match list values in `PRE_FILTER_VALUES`

**Background:** `PRE_FILTER_VALUES` (`get_kafka_config.json`) is a per-field filter applied
to each consumed business message via `message_matches_filters`. It only supported a single
expected value per field (exact string match), so a field like `timelines` couldn't accept
more than one valid value (e.g. `EOD` or `ITD`) without excluding messages.

**Code change** (`message_matches_filters`, ~line 253-273): if a filter's value is a list,
the field passes when the message's actual value matches **any** entry in the list
(OR-match). Plain string values keep doing exact match, unchanged. Filters across different
keys are still AND'd together — only the multi-value case within a single key is new.

```python
if isinstance(expected, list):
    if str(actual) not in [str(e) for e in expected]:
        return False
else:
    if str(actual) != str(expected):
        return False
```

**Example** — accept messages where `timelines` is either `EOD` or `ITD`:
```json
"PRE_FILTER_VALUES": {"timelines": ["EOD", "ITD"]}
```

Single-value filters are unchanged:
```json
"PRE_FILTER_VALUES": {"timelines": "EOD"}
```

Full worked example: Scenario 11 in `STATUS_MESSAGES_HOW_TO.md`.

---

# 00_get_kafka.py — Case-insensitive `PRE_FILTER_VALUES` keys + new `VALIDATION_FILTER_VALUES` (2026-07-30)

## Change 1: `PRE_FILTER_VALUES` keys are now lowercased on load

**Background:** `PRE_FILTER_VALUES` field-name keys were matched exactly against the
Avro message field names (e.g. `"timelines"`). Message field names are fixed by the
schema and are typically lowercase, so a config written with an uppercase key (e.g.
`"TIMELINES"`) would silently fail to match anything.

**Code change** (~line 683-687): `PRE_FILTER_VALUES` and the new `VALIDATION_FILTER_VALUES`
are both loaded with their keys lowercased:
```python
PRE_FILTER_VALUES: dict = {
    str(k).lower(): v for k, v in dict(CONFIG.get("PRE_FILTER_VALUES", {})).items()
}
VALIDATION_FILTER_VALUES: dict = {
    str(k).lower(): v for k, v in dict(CONFIG.get("VALIDATION_FILTER_VALUES", {})).items()
}
```
Only keys are lowercased — values (`"EOD"`, `"ITD"`) still match exactly as-is.

## Change 2: new `VALIDATION_FILTER_VALUES` config — decouple "what's written" from "what's counted"

**Background:** `PRE_FILTER_VALUES` does double duty — a message that doesn't match is
both dropped from the output file AND excluded from the count compared against
`EXPECTED_COUNT`. Some topics need to collect every message into the output file but
validate the count against only a subset of them (e.g. expected count only covers
`timelines=EOD/ITD`, but `RTD` messages on the same topic still need to be collected).

**Code changes** (`00_get_kafka.py`):
- New counter `validation_count`, reset alongside `filtered_count` each attempt
  (~line 1171). Incremented right after `filtered_count` (~line 1385-1390): unconditionally
  if `VALIDATION_FILTER_VALUES` is empty, otherwise only when the message matches it via
  the existing `message_matches_filters`.
- `filtered_count` is now purely a "rows written to file" tally, used only in the
  attempt-complete log line.
- `validation_count` replaces `filtered_count` everywhere the accept/retry/stability/
  tolerance logic and the validation-log writer compare against `EXPECTED_COUNT`
  (~lines 1459-1738). When `VALIDATION_FILTER_VALUES` is empty, `validation_count ==
  filtered_count`, so existing topics behave exactly as before.

**Example — collect everything, validate a subset:**
```json
"PRE_FILTER_VALUES": {},
"VALIDATION_FILTER_VALUES": {"timelines": ["EOD", "ITD"]}
```
Every message on the topic is written to the output file. Only messages where
`timelines` is `EOD` or `ITD` count toward `EXPECTED_COUNT`.

**Interaction warning:** `PRE_FILTER_VALUES` runs first and drops non-matching messages
before `VALIDATION_FILTER_VALUES` ever sees them. If both are set to different criteria,
the effective validation count becomes the intersection of the two filters, not
`VALIDATION_FILTER_VALUES` alone. Leave `PRE_FILTER_VALUES` empty on any topic using
`VALIDATION_FILTER_VALUES` for independent criteria.

Full worked example: Scenario 12 in `GET_KAFKA_HOW_TO.md`. Parameter reference in
`ADDED_CONFIG_PARAMETERS.md` §2.

---

# 00_get_kafka.py — Case-insensitive value matching, config validation, and clearer logging (2026-07-30)

## Change 1: filter values are now matched case-insensitively

**Background:** `message_matches_filters` matched values with exact string equality.
Local testing found a message field (`timeliness`) correctly named in config with the
correct value (`EOD`) still failing to match — the message's actual runtime value was a
different case than the config. Field-name keys were already case-normalized (previous
entry above); values were not.

**Code change** (`message_matches_filters`, ~line 270-275): both sides are uppercased
before comparing:
```python
if isinstance(expected, list):
    if str(actual).upper() not in [str(e).upper() for e in expected]:
        return False
else:
    if str(actual).upper() != str(expected).upper():
        return False
```
This applies to every caller of `message_matches_filters` — `PRE_FILTER_VALUES`,
`VALIDATION_FILTER_VALUES`, and the metadata-driven `filter_values`
(`mandatorCode`/`businessDate`/`reconciliationGroupId`) — uniformly.

## Change 2: startup warning for empty-string filter values

**Background:** `{"field": ""}` looks like it might mean "no filter on this field" but
actually means "only match messages where the field is itself an empty string" —
silently excluding every message with a real value.

**Code change** (~line 733-749): at startup, every `PRE_FILTER_VALUES` /
`VALIDATION_FILTER_VALUES` entry (including entries inside an OR-list) is checked for an
empty-string value; if found, a `WARNING` is logged explaining the correct way to
disable filtering on that field (remove the key, don't set it to `""`).

## Change 3: filter config shape is validated at startup

**Background:** two failure modes previously had no clear diagnostic: (1) setting
`PRE_FILTER_VALUES`/`VALIDATION_FILTER_VALUES` to something other than a JSON object
(e.g. a list) crashed with a raw Python `TypeError` traceback at config-load time; (2) a
nested-object value (e.g. `{"timelines": {"code": "EOD"}}`) produced no error at all — it
silently never matched any real message.

**Code change:** new `load_and_validate_filter_config(name, config)` function
(`00_get_kafka.py:288-322`), used to load both `PRE_FILTER_VALUES` and
`VALIDATION_FILTER_VALUES`. It lowercases keys (as before) and validates that every value
is a single string/number or a list of strings/numbers. On either failure mode it logs a
`CONFIG ERROR` with concrete format examples for both a normal field and a nested
(dot-notation) field, then hard-exits with code 9 — instead of a raw traceback or a
silent zero-match.

## Change 4: clearer logging when PRE_FILTER_VALUES and VALIDATION_FILTER_VALUES diverge

**Background:** when `VALIDATION_FILTER_VALUES` narrows the validated count below the
output file's row count (the "collect everything, validate a subset" pattern), the
resulting file can have far more rows than `EXPECTED_COUNT` — confusing for anyone
reading the output without also reading every attempt-log line.

**Code changes:**
- Startup `NOTE` (~line 757-764): fires only when `VALIDATION_FILTER_VALUES` is set and
  differs from `PRE_FILTER_VALUES`, stating upfront that the file will contain more rows
  than the validated count.
- `Output summary` block in `_save_and_break()` (~line 1536-1548), logged every time a
  run is accepted: reports rows written vs. rows validated vs. expected count, with an
  explicit `NOTE` line whenever `filtered_count != validation_count`.

Full details and log examples: `GET_KAFKA_HOW_TO.md` (`VALIDATION_FILTER_VALUES` section
and Scenario 12), `ADDED_CONFIG_PARAMETERS.md` §2.

---

# Consume null-`reconciliationGroupId` messages without inflating EXPECTED_COUNT (2026-08-03)

**Background:** upstream intraday (`ITD`) publishers legitimately send some messages
with no `reconciliationGroupId` at all — not bad data, just "no group applies." Before
this change, `message_matches_filters` treated a null/missing value on any
`METADATA_FILTER_COLUMNS` field as "exclude the whole message," so these were silently
dropped by `00_get_kafka.py` before ever reaching the output file. `filter_recon_group.py`
downstream had the same problem — it counted them as `parse_errors` and never wrote them
out. The requirement: consume these messages (write them through for the splitter) but
keep them out of the count validated against `total_messages_published`.

## Change 1 — `00_get_kafka.py`: `METADATA_FILTER_ALLOW_NULL_FIELDS`

**Code changes:**
- `message_matches_filters` (~line 253) takes a new optional `allow_null_fields: set`
  param. A null/missing actual value on a field in that set is treated as a pass instead
  of excluding the message; every other field is checked exactly as before.
- New config `METADATA_FILTER_ALLOW_NULL_FIELDS` (list of `METADATA_FILTER_COLUMNS`
  field names, default `[]`), loaded alongside `METADATA_FILTER_COLUMNS`/
  `METADATA_FILTER_FIELD_MAP` (~line 716).
- The write-gate call (~line 1453, `if raw_msg_dict is not None and filter_values and
  INPUT_FORMAT == "AVRO":`) now passes `allow_null_fields=METADATA_FILTER_ALLOW_NULL_FIELDS`
  — a null `reconciliationGroupId` no longer drops the message from the output file.
- The `validation_count` block (~line 1466) re-applies the same `filter_values`
  **without** `allow_null_fields` (strict), in addition to the existing
  `VALIDATION_FILTER_VALUES` check, and gained a new branch so the strict re-check also
  applies when `VALIDATION_FILTER_VALUES` is unset:
  ```python
  if VALIDATION_FILTER_VALUES and raw_msg_dict is not None:
      if message_matches_filters(raw_msg_dict, VALIDATION_FILTER_VALUES) and message_matches_filters(raw_msg_dict, filter_values):
          validation_count += 1
  elif raw_msg_dict is not None and filter_values:
      if message_matches_filters(raw_msg_dict, filter_values):
          validation_count += 1
  else:
      validation_count += 1
  ```
  A message let through the write-gate only via the null allowance always fails this
  strict re-check, so it's written to the file but never enters `validation_count` /
  `EXPECTED_COUNT` — regardless of whether `PRE_FILTER_VALUES` or
  `VALIDATION_FILTER_VALUES` are populated or empty.

**Config:**
```json
"METADATA_FILTER_COLUMNS": ["mandatorCode", "businessDate", "reconciliationGroupId"],
"METADATA_FILTER_ALLOW_NULL_FIELDS": ["reconciliationGroupId"]
```

## Change 2 — `00_plugins/filter_recon_group.py`: pass null-group lines through, uncounted

**Code changes:**
- A line whose `reconciliationGroupId` is null/missing is now written straight to the
  output file (previously: counted as a `parse_error` and dropped).
- New counter `null_recon_count` tracks these separately; they are never added to
  `matched_count`, so the existing strict `matched_count != expected_count` hard-fail
  check is unaffected.
- `FILTER SUMMARY` log gained a `Passed through (null reconciliationGroupId, uncounted)`
  line; the old `Skipped (parse / missing field)` line is now `Skipped (parse errors)`
  since missing-field is no longer a parse-error case.

Full worked examples: `GET_KAFKA_HOW_TO.md` Scenario 13, `ADDED_CONFIG_PARAMETERS.md`
(`METADATA_FILTER_ALLOW_NULL_FIELDS`), `00_plugins/PLUGIN_TESTING.md` Scenario E.

---

## Change 3 — `filter_recon_group.py`: `FILTER SUMMARY` log corrected and made explicit (2026-08-05)

**Background:** after Change 2 above, the `FILTER SUMMARY` log had two problems. (1) The
"no records dropped" line (~line 300, in the `else` of `if dropped_by_group:`) fired
whenever no *other-group* records were dropped — including when null-recon records had
been passed through — so it incorrectly claimed *"the .par file contained only the
target reconciliationGroupId"* even when it also contained passed-through nulls. (2) There
was no line stating the actual total record count written to the output file; only
`Total lines in .par file` (the *input* count) and `Matched count` (target-group only,
excludes passed-through nulls) were shown, so the true output size had to be inferred by
manually adding `matched_count + null_recon_count`.

**Code changes:**
- New `Total records written to output file (expected in .par)` line
  (`= matched {matched_count} + passed-through {null_recon_count}`).
- `Passed through (null reconciliationGroupId)` line now followed by an explanatory line
  noting a null/missing `reconciliationGroupId` is typically an EOD or ITD message
  published without a reconciliation group.
- The `dropped_by_group` / no-drop branch is now three-way: breakdown table (drops
  happened) / "no other-group drops, but N null-recon records passed through" / "no
  records dropped — file contained only the target reconciliationGroupId" (only when
  `null_recon_count == 0` too).
- The final `Done. N records written...` line now reports `total_written` (matched +
  passed-through) instead of just `matched_count`, which previously under-reported the
  actual output file size whenever null-recon records were present.

Full log example: `00_plugins/PLUGIN_TESTING.md` Scenario E and Step 3.

---

# Configurable zero-published status messages + fast-accept for EXPECTED_COUNT=0 (2026-08-14)

**Background:** some topics/mandators legitimately have days with zero business messages
to publish — the status message correctly reports `total_messages_published = 0` for
those days. Two problems on either side of the pipeline made this an error instead of a
valid case:

1. `kafka_trigger_status_messages.py`'s `write_validation_metadata()` unconditionally
   hard-exited (`os._exit(1)`) whenever `total_messages_published == 0`, with no way to
   say "this is expected for this feed."
2. Even if that were fixed, `00_get_kafka.py`'s `log_count_comparison()` treated
   `expected == 0, actual > 0` as a `MISMATCH` (`return False`), which routes into the
   "under expected — never accept early" retry branch. The run would then burn the
   **entire** `MAX_LISTEN_DURATION_HOURS` retry window every time before an
   exhaustion-time tolerance check (`low = 0 - 0 * pct/100 = 0`) accepted it anyway —
   correct outcome, just needlessly slow, every single run.

## Change 1 — `kafka_trigger_status_messages.py`: `ALLOW_ZERO_MESSAGES_PUBLISHED`

**Code changes:**
- `write_validation_metadata()` (~line 439-441) takes a new `allow_zero_published=False`
  parameter.
- The zero-count check (~line 455-464) only hard-exits when
  `total_messages_published == 0 and not allow_zero_published`; otherwise it logs an
  info-level accept and proceeds to write metadata normally with `total_messages_published=0`.
- New config `ALLOW_ZERO_MESSAGES_PUBLISHED` (`"YES"`/`"NO"`, default `"NO"`), read
  alongside `ALLOW_NO_DATA` (~line 750).
- Both call sites — the happy-path call (~line 1263) and the retry-loop call
  (~line 1645) — now pass `allow_zero_published=(ALLOW_ZERO_MESSAGES_PUBLISHED == "YES")`.

**Config:**

```json
"ALLOW_ZERO_MESSAGES_PUBLISHED": "YES"
```

Default (`"NO"`, or the key omitted entirely) preserves the original hard-exit behaviour.

## Change 2 — `00_get_kafka.py`: fast-accept when `EXPECTED_COUNT == 0`

**Code change** (`log_count_comparison()`, ~line 351-358): the `expected == 0` branch
(for `actual > 0`) now logs an acceptance message and returns `True` instead of `False`:

```python
if expected == 0:
    dsf_logger.log_msg(
        f"  Result: expected 0 but got {actual:,} messages — accepting "
        f"(no minimum required when expected count is 0).",
        level=30
    )
    dsf_logger.log_msg(sep, level=20)
    return True
```

This routes an `expected=0, actual>0` run through the normal `STABILITY` accept path
(`STABLE_COUNT_REQUIRED_ATTEMPTS` consecutive matching reads, a few `RETRY_WAIT_SECONDS`
apart) instead of the full retry-window wait. `expected=0, actual=0` is unchanged (still
an immediate exact-match accept).

**Trade-off, deliberately accepted:** fast-accept commits Kafka offsets and finalizes the
output file as soon as the count is stable for `STABLE_COUNT_REQUIRED_ATTEMPTS` reads —
typically ~10s with default config — not after riding out the full window. For a feed
where real messages could keep trickling in for hours after the first one appears, this
would risk finalizing on a partial/incomplete set. This was evaluated and accepted
specifically because the operational trigger for this pipeline is the status message
completing — once `kafka_trigger_status_messages.py` has validated and handed off,
messages arriving afterward are explicitly out of scope; the run does not need to keep
listening for them. If that assumption changes for some feed (e.g. it truly needs to
capture a same-day trickle even after `expected=0`), fast-accept is the wrong fix for
that feed — a bounded minimum-wait before allowing the fast path would be needed instead.

Full worked examples: `STATUS_MESSAGES_HOW_TO.md` Scenario 12, `GET_KAFKA_HOW_TO.md`
Scenario 14. Parameter reference: `ADDED_CONFIG_PARAMETERS.md` §1 and §3.
