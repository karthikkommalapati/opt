


# Get Kafka test

```bash
command used `python3 run_test_get_kafka.py real_data 2>&1 | tee ../test_output_get_kafka/logs/get_kafka_real_$(date +%Y%m%d_%H%M%S).log`


updated the date in the last row  under test_data_get_kafka.par to "businessDate": "2026-05-06" , the script started polling. Whilt the script is waiting mode after attempt 2 I udapted the "businessDate": "2026-05-07", and  still the script did not pick it `/test_output_get_kafka/logs/get_kafka_real_20260509_140735.log` is the log file. Have a look 
at the log and tell me what is clear and what is not and what is missing. Analyse and understand as a developer or business user what can you. make out of it and what you cannot.


```


#  Status Messages Test

```bash
command used `python3 run_test.py real_data 2>&1 | tee ../test_output/logs/status_messages_real_$(date +%Y%m%d_%H%M%S).log`

Why is the validatin.log gettintg deleting rather appended?

updated the date in the last row  under test_data.par to "businessDate": "2026-05-06" , the script started polling. Whilt the script is waiting mode after attempt 2
I udapted the "businessDate": "2026-05-07", and  still the script did not pick it `test_output/logs/status_messages_real_20260509_134723.log` is the log file. Have a look 
at the log and tell me what is clear and what is not and what is missing. Analyse and understand as a developer or business user what can you. make out of it and what you cannot.

Also, from this log `/test_output_get_kafka/logs/get_kafka_real_20260509_140735.log` there are some good failure analysis maybe the same can be adopted here too?

```



### Additionla Tips 
```bash
- add user to metaada log 
```

---

### 2026-05-11 — username + validation logs (all pass)

**Changes applied:**
- `kafka_trigger_status_messages.py`: `username = os.getenv('USER', 'unknown')` added; `write_validation_metadata()` now accepts `username` and writes it as second column (after `export_datetime`) in both File 1 (per-run metadata) and File 2 (history log); log renamed from `{FEED}_validation_log.txt` → `{FEED}_status_messages_validation.log`
- `00_get_kafka.py`: `username` captured; new `write_get_kafka_validation_log()` function; `GET_KAFKA_VALIDATION_LOG = {DATA_PATH}/{FEED}_get_kafka_validation.log`; called on every successful `.par` write; columns: `export_datetime|username|business_date|mandator|feed_name|reconciliation_group_id|expected_count|actual_count|tolerance_pct|tolerance_lower|tolerance_upper`
- Test harnesses updated: `run_test.py` (8 scenarios, new `check_status_validation_log()`), `run_test_get_kafka.py` (11 scenarios, new `check_get_kafka_validation_log()`), `METADATA_HEADERS` updated to include `username`

**Test run result:** ALL PASSED — 8/8 status_messages, 11/11 get_kafka