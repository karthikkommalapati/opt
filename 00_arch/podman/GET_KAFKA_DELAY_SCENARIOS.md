# Get Kafka — Delay Scenarios & Business Date Guide

---

## The three dates you need to understand

Every run involves three distinct dates. Confusing them is the most
common source of support calls.

---

### Date 1 — The Business Date

This is the date the **data is about**, not the date anything runs.

> Example: the trading activity for Tuesday 26 May 2026 has business date **2026-05-26**.

You pass this date to the script when you run it.

---

### Date 2 — The Publication Window

The upstream system publishes the day's data to Kafka **after 16:00 on
the business date**. The window stays open until **16:00 the next calendar
day** — a 24-hour slot.

> Example: data for business date **Tuesday 26 May** is expected to land
> in Kafka between **Tuesday 26 May 16:00** and **Wednesday 27 May 16:00**.

The script only looks for messages published inside that 24-hour window.
A message published one minute outside the window — even with the correct
business date stamped on it — will never be found, no matter how long the
script waits.

| Business date | Window opens | Window closes |
| --- | --- | --- |
| Monday 25 May 2026 | Mon 25 May 16:00 | Tue 26 May 16:00 |
| Tuesday 26 May 2026 | Tue 26 May 16:00 | Wed 27 May 16:00 |
| Wednesday 27 May 2026 | Wed 27 May 16:00 | Thu 28 May 16:00 |
| Thursday 28 May 2026 | Thu 28 May 16:00 | Fri 29 May 16:00 |

---

### Date 3 — The Script Run Date

This is the calendar date when the collection script actually runs.
Normally this is **the morning after the business date**.

> Example: the script runs on **Wednesday 27 May 2026 at 09:00** to collect
> data for business date **Tuesday 26 May 2026**.

---

### All three dates together — the standard picture

```
Business date        :  Tuesday 26 May 2026
  ↓ producer publishes after close of business
Publication window   :  Tue 26 May 16:00  →  Wed 27 May 16:00
  ↓ script runs next morning
Script starts        :  Wednesday 27 May 2026 at 09:00
Script deadline      :  Wednesday 27 May 2026 at 11:00  (2 hours later)
```

---

## What you see in the log at startup

When the script starts it prints two lines that every operator should
check first:

```
[INFO] Kafka publication window (messages searched on topic) : 2026-05-26 16:00:00  →  2026-05-27 16:00:00
[INFO] Retry deadline (script stops retrying after)          : 2026-05-27 11:00:00  (2.0h / 120 min from now)
```

**Publication window** — the 24-hour band on the Kafka topic that the
script is scanning. Fixed from config. If your data is not in here, it
will never be found.

**Retry deadline** — the wall clock time the script gives up and either
accepts what it has or fails. This is separate from the publication window.
The script might stop retrying at 11:00 even though the window stays open
until 16:00.

---

## Fixed numbers used in all examples below

| What | Value | Where it comes from |
| --- | --- | --- |
| Business date | Tuesday 26 May 2026 | Passed to script at runtime |
| Publication window | Tue 26 May 16:00 → Wed 27 May 16:00 | Config: START/STOP_TS |
| Script starts | Wednesday 27 May, 09:00 | Scheduled job time |
| Retry deadline | Wednesday 27 May, 11:00 | 2 hours after start |
| Expected message count | 1,100 | Metadata file (500 + 600 from two instances) |
| Minimum acceptable count | 990 | 10% below 1,100 |
| Time between retries | 5 minutes | Config: RETRY_WAIT_SECONDS |
| Confirmations needed | 2 reads in a row at the same count | Config: STABLE_COUNT_REQUIRED_ATTEMPTS |

---

## The six scenarios

---

### Scenario 1 — Everything arrives on time

**What happened upstream:** The upstream batch finished overnight on
Tue 26 May. All 1,100 messages are already sitting on Kafka before the
script starts at 09:00 on Wed 27 May.

| Clock time (Wed 27 May) | Messages found | What the script does |
| --- | --- | --- |
| 09:00 | 1,100 | Meets the target. Needs one more read to confirm the producer has finished. Waits 5 minutes. |
| 09:05 | 1,100 | Same count. Confirmed stable. Writes the output file and finishes. |

**Outcome: success. Total wait: 5 minutes.**

Why it waits for a second read: if the producer were still writing, the
count at 09:00 might be 1,100 but grow to 1,250 by 09:05, making the
first read incomplete. Two identical counts in a row prove it has stopped.

**What the log shows:**

```
09:00:00 [INFO] Kafka publication window (messages searched on topic) : 2026-05-26 16:00:00  →  2026-05-27 16:00:00
09:00:00 [INFO] Retry deadline (script stops retrying after)          : 2026-05-27 11:00:00  (2.0h / 120 min from now)

09:00:01 [INFO] --- Collection attempt 1 started at 2026-05-27 09:00:01 ---
09:00:01 [INFO] Scanning Kafka window : 2026-05-26 16:00:00  →  2026-05-27 09:00:05  | Retry deadline: 2026-05-27 11:00:00
09:00:03 [INFO] messages_matching_all_filters=1100
09:00:03 [INFO] Count changed: None → 1100 (>= expected 1100). Stability streak reset to 1/2.
09:00:03 [INFO] Count 1100 >= expected 1100, but stability streak 1/2 not yet met. Sleeping 300s. Next attempt at: 09:05:03

09:05:03 [INFO] --- Collection attempt 2 started at 2026-05-27 09:05:03 ---
09:05:03 [INFO] Scanning Kafka window : 2026-05-26 16:00:00  →  2026-05-27 09:05:07  | Retry deadline: 2026-05-27 11:00:00
09:05:05 [INFO] messages_matching_all_filters=1100
09:05:05 [INFO] Count stable at 1100 for 2/2 consecutive attempt(s).
09:05:05 [INFO] Count stable at 1100 for 2 consecutive attempt(s) (expected 1100). Accepting.
09:05:05 [INFO] All offsets committed. Data successfully consumed and written.
```

---

### Scenario 2 — Producer is still publishing when the script starts

**What happened upstream:** The upstream batch started late and is still
running at 09:00 on Wed 27 May. Messages are arriving in chunks throughout
the morning, all within the Tue 16:00 → Wed 16:00 publication window.

| Clock time (Wed 27 May) | Messages found | What the script does |
| --- | --- | --- |
| 09:00 | 350 | Below 1,100. Waits 5 minutes. |
| 09:05 | 700 | Still below. Waits 5 minutes. |
| 09:10 | 1,100 | Reached 1,100. Needs confirmation. Waits 5 minutes. |
| 09:15 | 1,140 | Count grew — producer was still writing. Streak resets. Waits 5 minutes. |
| 09:20 | 1,140 | Same count as 09:15. Confirmed stable. Writes file and finishes. |

**Outcome: success. Total wait: 20 minutes.**

Each retry re-reads the full publication window from the beginning, so
messages that arrived between attempts are never missed.

**What the log shows (key lines):**

```
09:00:01 [INFO] Scanning Kafka window : 2026-05-26 16:00:00  →  2026-05-27 09:00:05  | Retry deadline: 2026-05-27 11:00:00
09:00:03 [INFO] messages_matching_all_filters=350
09:00:03 [WARN] Under-count on attempt 1: got 350, expected 1100. Time remaining: 2.00h. Sleeping 300s. Next attempt at: 09:05:03

09:05:03 [INFO] Scanning Kafka window : 2026-05-26 16:00:00  →  2026-05-27 09:05:07  | Retry deadline: 2026-05-27 11:00:00
09:05:05 [INFO] messages_matching_all_filters=700
09:05:05 [WARN] Under-count on attempt 2: got 700, expected 1100. Time remaining: 1.92h. Sleeping 300s. Next attempt at: 09:10:05

09:10:05 [INFO] Scanning Kafka window : 2026-05-26 16:00:00  →  2026-05-27 09:10:09  | Retry deadline: 2026-05-27 11:00:00
09:10:07 [INFO] messages_matching_all_filters=1100
09:10:07 [INFO] Count changed: None → 1100 (>= expected 1100). Stability streak reset to 1/2. Sleeping 300s.

09:15:07 [INFO] Scanning Kafka window : 2026-05-26 16:00:00  →  2026-05-27 09:15:11  | Retry deadline: 2026-05-27 11:00:00
09:15:09 [INFO] messages_matching_all_filters=1140
09:15:09 [INFO] Count changed: 1100 → 1140 (>= expected 1100). Stability streak reset to 1/2. Sleeping 300s.

09:20:09 [INFO] Scanning Kafka window : 2026-05-26 16:00:00  →  2026-05-27 09:20:13  | Retry deadline: 2026-05-27 11:00:00
09:20:11 [INFO] messages_matching_all_filters=1140
09:20:11 [INFO] Count stable at 1140 for 2/2 consecutive attempt(s).
09:20:11 [INFO] Count stable at 1140 for 2 consecutive attempt(s) (expected 1100). Accepting.
09:20:11 [INFO] All offsets committed. Data successfully consumed and written.
```

Notice the scan window end advances slightly with each attempt
(09:00:05 → 09:05:07 → 09:10:09 …). This ensures messages published
during the retry period are picked up on the next read.

---

### Scenario 3 — Batch finishes very late, near the deadline

**What happened upstream:** A long-running batch on Tue 26 May ran into
problems and did not finish publishing until around 10:55 on Wed 27 May —
only 5 minutes before the 11:00 retry deadline. All messages were published
within the valid window (Tue 16:00 → Wed 16:00), just very late in it.

When the count reaches the target with less than 5 minutes left, the script
automatically extends its deadline by one extra 5-minute interval so the
confirmation read can still happen.

| Clock time (Wed 27 May) | Messages found | What the script does |
| --- | --- | --- |
| 09:00 – 10:50 | 800 – 950 | Below 1,100 on every attempt. Keeps retrying every 5 minutes. |
| 10:55 | 1,100 | Reached 1,100 for the first time. Less than 5 minutes to the 11:00 deadline — grace period triggered. Deadline automatically extended to 11:05. Waits 5 minutes. |
| 11:00 | 1,100 | Same count. Confirmed stable. Writes file and finishes cleanly, no warning. |

**Outcome: success, no warning. Total wait: 2 hours 5 minutes at most.**

If the count keeps growing after the grace period (meaning the producer is
genuinely still writing), the script accepts at 11:05 with a warning — the
grace period does not hide a real problem.

**What the log shows at the key moments:**

```
09:00:00 [INFO] Kafka publication window (messages searched on topic) : 2026-05-26 16:00:00  →  2026-05-27 16:00:00
09:00:00 [INFO] Retry deadline (script stops retrying after)          : 2026-05-27 11:00:00  (2.0h / 120 min from now)

  ... attempts 1–22 (09:00 to 10:50): under-count, retrying every 5 minutes ...

10:55:01 [INFO] --- Collection attempt 23 started at 2026-05-27 10:55:01 ---
10:55:01 [INFO] Scanning Kafka window : 2026-05-26 16:00:00  →  2026-05-27 10:55:05  | Retry deadline: 2026-05-27 11:00:00
10:55:03 [INFO] messages_matching_all_filters=1100
10:55:03 [INFO] Count changed: None → 1100 (>= expected 1100). Stability streak reset to 1/2.
10:55:03 [WARN] Count first reached target with less than 300s remaining — extending retry deadline by 300s to allow stability confirmation.
                New retry deadline : 2026-05-27 11:05:00.
                Kafka scan window now : 2026-05-26 16:00:00  →  2026-05-27 11:05:00
10:55:03 [INFO] Sleeping 300s. Next attempt at: 11:00:03

11:00:03 [INFO] --- Collection attempt 24 started at 2026-05-27 11:00:03 ---
11:00:03 [INFO] Scanning Kafka window : 2026-05-26 16:00:00  →  2026-05-27 11:05:00  | Retry deadline: 2026-05-27 11:05:00
11:00:05 [INFO] messages_matching_all_filters=1100
11:00:05 [INFO] Count stable at 1100 for 2/2 consecutive attempt(s).
11:00:05 [INFO] Count stable at 1100 for 2 consecutive attempt(s) (expected 1100). Accepting.
11:00:05 [INFO] All offsets committed. Data successfully consumed and written.
```

Notice what changed after the grace extension: the scan window end moved
from 10:55:05 to 11:05:00, and the retry deadline moved from 11:00:00 to
11:05:00. The script then confirmed stability at 11:00 and accepted with no
warning.

---

### Scenario 4 — Count settles just below target, within the 10% tolerance band

**What happened upstream:** Most data arrived correctly. A small final batch
of 50 messages was published at Wed 27 May 16:05 — 5 minutes after the
publication window closed at Wed 16:00. Those 50 messages will never be
counted. The count settles at 1,050, which is 50 short of the 1,100 target
but above the 990 floor.

| Clock time (Wed 27 May) | Messages found | What the script does |
| --- | --- | --- |
| 09:00 – 11:00 | 1,050 on every attempt | Below 1,100 every time. The script can only accept early if the count reaches 1,100 or above. Keeps retrying until the deadline. |
| 11:00 | 1,050 | Deadline reached. 1,050 is above the 990 floor. Accepts the file and logs a warning. |

**Outcome: success with warning. Total wait: 2 hours.**

The output file contains 1,050 messages. The 10% tolerance band exists
precisely for this situation — a small number of late messages should not
cause a full failure.

**What the log shows at the deadline:**

```
11:00:01 [INFO] --- Collection attempt 24 started at 2026-05-27 11:00:01 ---
11:00:01 [INFO] Scanning Kafka window : 2026-05-26 16:00:00  →  2026-05-27 11:00:05  | Retry deadline: 2026-05-27 11:00:00
11:00:03 [INFO] messages_matching_all_filters=1050
11:00:03 [WARN] Retry window exhausted after 24 attempt(s).
                Final count 1050 is within lower tolerance (990–1100, ±10%). Accepting.
11:00:03 [INFO] All offsets committed. Data successfully consumed and written.
```

---

### Scenario 5 — Count is far below target and never recovers

**What happened upstream:** Something went seriously wrong. Either the
upstream batch aborted partway through, or — more commonly — the metadata
file used by the script has the wrong run ID, so the script is filtering
for messages that do not exist in large enough numbers on the topic.

The count stays at 300 all morning — far below the 990 floor.

| Clock time (Wed 27 May) | Messages found | What the script does |
| --- | --- | --- |
| 09:00 – 11:00 | 300 on every attempt | Far below both 1,100 and the 990 floor. Keeps retrying. |
| 11:00 | 300 | Deadline. 300 is below 990. Fails with an error. |

**Outcome: failure. Total wait: 2 hours.**

**What the log shows at the deadline — the failure analysis block:**

```
11:00:01 [INFO] Scanning Kafka window : 2026-05-26 16:00:00  →  2026-05-27 11:00:05  | Retry deadline: 2026-05-27 11:00:00
11:00:03 [ERROR] Retry window exhausted after 24 attempt(s).
                 Final count 300 is BELOW lower tolerance threshold 990 (expected 1100, ±10%). Failing.

11:00:03 [ERROR] FAILURE ANALYSIS — FILTER / COUNT MISMATCH DETAIL
                 Expected filter values (from metadata):
                   mandatorCode = "022"
                   businessDate = "2026-05-26"
                   reconciliationGroupId = "1"

                 Total messages seen in Kafka window: 2,800
                 Distinct combinations found: 3

                 Per-filter field analysis:
                   mandatorCode = "022"          — MATCHED
                   businessDate = "2026-05-26"   — MATCHED
                   reconciliationGroupId = "1"   — NOT MATCHED. Values found: ["2", "3"]

                 Available combinations in Kafka data:
                   mandatorCode="022", businessDate="2026-05-26", reconciliationGroupId="2" — 1,100 messages
                   mandatorCode="022", businessDate="2026-05-26", reconciliationGroupId="3" — 1,400 messages
                   mandatorCode="033", businessDate="2026-05-26", reconciliationGroupId="1" —   300 messages
```

**Reading this output:**

- 2,800 messages are on the topic for 26 May — the data is there.
- The script was looking for run ID "1" (from the metadata file).
- The producer ran a second time and used run ID "3". Run ID "1" belongs to
  a different mandator (033), which is why only 300 matched.
- **Fix:** re-run the status-messages job. It will write a new metadata file
  with run ID "3". Then retry this script.

**Common causes of Scenario 5:**

- Status-messages job re-ran with a new run ID after the metadata was written
- Upstream batch aborted — fewer than 990 messages were ever published
- Data published to the wrong Kafka topic
- Business date stamped inside the messages does not match the metadata file

---

### Scenario 6 — No messages at all in the publication window

**What happened:** The script's 24-hour window covers a period when nothing
was published to the topic. This almost always means the wrong business date
was passed to the script.

| Clock time (Wed 27 May) | Messages found | What the script does |
| --- | --- | --- |
| 09:00 – 11:00 | 0 on every attempt | Nothing in the window. Keeps retrying. |
| 11:00 | 0 | Deadline. Fails: no messages found in window. |

**Outcome: failure. Total wait: 2 hours.**

**Worked example — wrong business date:**

The producer published 1,100 messages on **Monday 25 May at 18:30** for
business date Monday 25 May. The script was run with business date
**Wednesday 27 May** by mistake.

| | Correct run | Mistaken run |
| --- | --- | --- |
| Business date passed | 2026-05-25 | 2026-05-27 |
| Publication window searched | Mon 25 May 16:00 → Tue 26 May 16:00 | Wed 27 May 16:00 → Thu 28 May 16:00 |
| Where the messages actually are | Mon 25 May 18:30 — inside the window | Mon 25 May 18:30 — two days outside this window |
| Result | 1,100 found, success | 0 found, fails after 2 hours |

**Fix:** re-run with business date **2026-05-25**.

**What the log shows — the key line to check:**

```
09:00:00 [INFO] Kafka publication window (messages searched on topic) : 2026-05-27 16:00:00  →  2026-05-28 16:00:00
```

Compare that window against when you know the producer published.
If the publication time falls outside it, the business date passed to the
script is wrong.

**Common causes:**

- Business date wrong by one or more days
- Script triggered before the producer had published anything — started too early
- Producer published to a different Kafka topic
- Publication window config (16:00 start/end) does not match when the producer actually runs

---

## Summary

| Scenario | Business date | Publication window | Script starts | Finishes | Messages found | Outcome |
| --- | --- | --- | --- | --- | --- | --- |
| 1 — On time | Tue 26 May | Tue 16:00 → Wed 16:00 | Wed 09:00 | Wed 09:05 | 1,100 stable | Success |
| 2 — Slow producer | Tue 26 May | Tue 16:00 → Wed 16:00 | Wed 09:00 | Wed 09:20 | 1,140 stable | Success |
| 3 — Very late (grace) | Tue 26 May | Tue 16:00 → Wed 16:00 | Wed 09:00 | Wed 11:00 | 1,100 stable after grace | Success |
| 4 — Slightly short | Tue 26 May | Tue 16:00 → Wed 16:00 | Wed 09:00 | Wed 11:00 | 1,050 (within 10%) | Success with warning |
| 5 — Far short / wrong run ID | Tue 26 May | Tue 16:00 → Wed 16:00 | Wed 09:00 | Wed 11:00 | 300 (below floor) | Failure |
| 6 — No data / wrong date | Wrong date | Wrong window | Wed 09:00 | Wed 11:00 | 0 | Failure |

---

## Quick diagnosis guide

**Step 1 — Read the two startup lines in the log.**

```
[INFO] Kafka publication window (messages searched on topic) : 2026-05-26 16:00:00  →  2026-05-27 16:00:00
[INFO] Retry deadline (script stops retrying after)          : 2026-05-27 11:00:00  (2.0h / 120 min from now)
```

Confirm the publication window matches the date and time the producer
published. If the window is wrong, the business date passed to the script
is wrong — fix the date and rerun.

**Step 2 — Check how many messages the script found.**

Look for this line after each attempt:

```
[INFO] messages_matching_all_filters=NNN
```

- **0** → Scenario 6. Window does not cover when the producer published.
- **Far below target (e.g. 300 of 1,100)** → Scenario 5. Filter mismatch — read the failure analysis.
- **Close to target (e.g. 1,050 of 1,100)** → Scenario 4. A few messages missed the window.
- **At or above target** → Scenarios 1, 2, or 3. Script will accept once stable.

**Step 3 — For Scenario 5, find the "NOT MATCHED" line.**

The failure analysis block lists each filter field. The one marked
`NOT MATCHED` and the values actually found in the data is the root cause.
Most often this is `reconciliationGroupId` — meaning the status-messages
job re-ran and produced a new run ID that the metadata file does not reflect.

**Step 4 — If the run ID is the problem, regenerate the metadata file.**

Re-run the status-messages job so it writes a fresh metadata file with the
current run ID, then retry this script with the same business date.
