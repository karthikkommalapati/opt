"""
Shows the exact time window the script will use for a given ASOF_DT and mandator.
Mirrors the script's own calculation so you can verify which eventTimestamps are
inside vs outside the window before producing messages.

Usage:
    python show_window.py 2026-04-22
    python show_window.py 2026-04-22 023
"""
import json
import sys
from datetime import datetime, timedelta, timezone

ASOF_DT  = sys.argv[1] if len(sys.argv) > 1 else datetime.now(timezone.utc).strftime("%Y-%m-%d")
MANDATOR = sys.argv[2] if len(sys.argv) > 2 else "022"

with open("status_messages_config.json") as f:
    config = json.load(f)

window = config.get("LOCATION_TIME_WINDOW", {}).get(MANDATOR)
if not window:
    print(f"ERROR: No LOCATION_TIME_WINDOW config for mandator '{MANDATOR}'")
    sys.exit(1)

DT_UTC = datetime.strptime(ASOF_DT, "%Y-%m-%d").date()

start_time = datetime.strptime(window["START_TS"], "%H:%M:%S").time()
start_dt   = datetime.combine(DT_UTC, start_time)
DT_UTC_START = start_dt - timedelta(days=int(window["START_DT_OFFSET"]))

stop_time = datetime.strptime(window["STOP_TS"], "%H:%M:%S").time()
stop_dt   = datetime.combine(DT_UTC, stop_time)
DT_UTC_END = stop_dt - timedelta(days=int(window["STOP_DT_OFFSET"]))

print(f"ASOF_DT:  {ASOF_DT}")
print(f"Mandator: {MANDATOR}")
print()
print(f"  Window START : {DT_UTC_START.isoformat()}+00:00"
      f"  (START_TS={window['START_TS']}, START_DT_OFFSET={window['START_DT_OFFSET']})")
print(f"  Window END   : {DT_UTC_END.isoformat()}+00:00"
      f"  (STOP_TS={window['STOP_TS']},  STOP_DT_OFFSET={window['STOP_DT_OFFSET']})")
print()
print("  Timestamps INSIDE window  → script will process these:")
inside = DT_UTC_START + (DT_UTC_END - DT_UTC_START) / 2
print(f"    e.g.  \"{inside.isoformat()}+00:00\"")
print()
print("  Timestamps OUTSIDE window → script will IGNORE these (use for Scenario F):")
before = DT_UTC_START - timedelta(hours=6)
after  = DT_UTC_END   + timedelta(hours=6)
print(f"    before window:  \"{before.isoformat()}+00:00\"")
print(f"    after  window:  \"{after.isoformat()}+00:00\"")
