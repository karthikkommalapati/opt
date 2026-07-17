# Reviewing Kafka Message Files in Jupyter with DuckDB

Interactive workflow for loading Kafka message dumps (JSONL or Parquet) and
filtering/reviewing them cell-by-cell in Jupyter, using the DuckDB Python
library (no CLI binary needed — works with `duckdb==1.5.4` installed via pip).

---

## 1. Install what you need

```bash
pip3 install --user duckdb jupyterlab pandas
```

Verify:
```bash
python3 -c "import duckdb; print(duckdb.__version__)"
python3 -c "import jupyterlab; print('jupyterlab ok')"
```

---

## 2. Register a kernel for this environment

Jupyter needs an explicit **kernel** registered so it knows which Python
interpreter (and which installed packages — your `duckdb`, `pandas`, etc.)
to run notebook cells with. If you skip this, Jupyter may default to a
different Python than the one you just installed packages into, and
`import duckdb` will fail inside the notebook even though it works from
the terminal.

**If you're using a virtualenv or `uv` environment** (recommended — keeps
this isolated from other projects):
```bash
python3 -m venv ~/venvs/kafka-review
source ~/venvs/kafka-review/bin/activate
pip install duckdb jupyterlab pandas ipykernel
```

**Register that environment as a named Jupyter kernel:**
```bash
python -m ipykernel install --user --name kafka-review --display-name "Kafka Review (duckdb)"
```
- `--name` is the internal kernel id
- `--display-name` is what shows up in the Jupyter UI's kernel picker

**Confirm it registered:**
```bash
jupyter kernelspec list
```
You should see `kafka-review` in the list with a path to its `kernel.json`.

---

## 3. Launch Jupyter on the devpod

```bash
jupyter lab --no-browser --ip=0.0.0.0 --port=8888
```
Copy the token/URL it prints. If you're connecting from your laptop,
port-forward first (adjust for how you normally reach the devpod):
```bash
ssh -L 8888:localhost:8888 youruser@devpod
```
Then open `http://localhost:8888/?token=...` in your browser.

---

## 4. Select the correct kernel

When you open or create a notebook:
- **JupyterLab**: top-right corner shows the current kernel name — click it,
  pick **"Kafka Review (duckdb)"** from the dropdown.
- **Classic Jupyter Notebook**: menu bar → **Kernel → Change Kernel →
  Kafka Review (duckdb)**.
- **VS Code with Jupyter extension**: top-right of the notebook →
  **Select Kernel → Jupyter Kernel... → Kafka Review (duckdb)**.

**Sanity check inside a cell** — run this first, always, especially if
you've had kernel mismatches before:
```python
import sys, duckdb
print(sys.executable)
print(duckdb.__version__)
```
`sys.executable` should point into `~/venvs/kafka-review/...` — if it
points somewhere else, you've got the wrong kernel selected; switch it
via the picker above.

---

## 5. Load your Kafka message file

```python
import duckdb

con = duckdb.connect('review.duckdb')  # persistent file, reopen anytime

con.execute("""
    CREATE OR REPLACE TABLE messages AS
    SELECT * FROM read_json_auto('/path/to/messages.jsonl')
""")
```
Swap `read_json_auto(...)` for `read_parquet(...)` if your file is `.par`/Parquet.

Loading more than one file? Give each its own table name:
```python
con.execute("""
    CREATE OR REPLACE TABLE orders_messages AS
    SELECT * FROM read_json_auto('/path/to/orders.jsonl')
""")
```

---

## 6. Always check structure first

```python
con.sql("DESCRIBE messages").show()
```
Nested objects show as `STRUCT(...)`, arrays as `LIST(...)`. Check this
before writing filters — don't assume field names or nesting depth.

List everything loaded so far:
```python
con.sql("SHOW TABLES").show()
```

---

## 7. Preview rows

```python
con.sql("SELECT * FROM messages LIMIT 10").show()

# or as a pandas DataFrame for nicer notebook rendering / wide tables
df = con.sql("SELECT * FROM messages LIMIT 10").df()
df
```

---

## 8. Filter manually

```python
# simple field filter
con.sql("SELECT * FROM messages WHERE status = 'FAILED'").df()

# nested field filter (dot notation, based on DESCRIBE output)
con.sql("SELECT * FROM messages WHERE payload.event.type = 'purchase'").df()

# specific columns
con.sql("""
    SELECT user_id, payload.event.type, ts
    FROM messages
    WHERE ts > '2026-07-01'
""").df()

# multiple conditions
con.sql("""
    SELECT * FROM messages
    WHERE status = 'FAILED' AND payload.event.type = 'purchase'
""").df()

# text pattern match
con.sql("SELECT * FROM messages WHERE payload.user.email LIKE '%@example.com'").df()

# count / group by
con.sql("""
    SELECT payload.event.type, COUNT(*) AS cnt
    FROM messages
    GROUP BY 1
    ORDER BY cnt DESC
""").df()

# sort
con.sql("SELECT * FROM messages ORDER BY ts DESC LIMIT 20").df()
```

**Explode a nested array field into rows** (don't flatten arrays into
columns — use `UNNEST`):
```python
con.sql("SELECT msg_id, UNNEST(items) AS item FROM messages").df()
```

---

## 9. Iterate interactively

Since you're in Jupyter, keep re-running cells with adjusted `WHERE`
clauses as you narrow down what you're looking for — that's the point of
using a notebook over the CLI. E.g.:
```python
df = con.sql("SELECT * FROM messages WHERE status = 'FAILED'").df()
df.shape
```
```python
df[df['payload.event.type'] == 'purchase']  # further filter in pandas if you dropped to .df()
```

---

## 10. Export a filtered result

```python
con.sql("""
    COPY (SELECT * FROM messages WHERE status = 'FAILED')
    TO '/tmp/failed_messages.csv' (HEADER, DELIMITER ',')
""")
```

---

## 11. Close / resume later

```python
con.close()
```
The `review.duckdb` file on disk persists everything. Reopen anytime, even
in a fresh notebook or kernel restart:
```python
import duckdb
con = duckdb.connect('review.duckdb')
con.sql("SHOW TABLES").show()
```

---

## Quick reference

| Task | Command |
|---|---|
| Register kernel | `python -m ipykernel install --user --name kafka-review --display-name "Kafka Review (duckdb)"` |
| List kernels | `jupyter kernelspec list` |
| Check active kernel in notebook | `import sys; print(sys.executable)` |
| Launch Jupyter | `jupyter lab --no-browser --ip=0.0.0.0 --port=8888` |
| Connect DB | `con = duckdb.connect('review.duckdb')` |
| Load JSONL | `con.execute("CREATE OR REPLACE TABLE t AS SELECT * FROM read_json_auto('f.jsonl')")` |
| Load Parquet | `con.execute("CREATE OR REPLACE TABLE t AS SELECT * FROM read_parquet('f.par')")` |
| Show schema | `con.sql("DESCRIBE t").show()` |
| List tables | `con.sql("SHOW TABLES").show()` |
| Filter (df) | `con.sql("SELECT * FROM t WHERE <cond>").df()` |
| Explode array | `con.sql("SELECT UNNEST(arr_col) FROM t").df()` |
| Export CSV | `con.sql("COPY (SELECT ...) TO 'out.csv' (HEADER, DELIMITER ',')")` |
| Close connection | `con.close()` |

---

## Common kernel gotcha

If `import duckdb` fails in the notebook but works from the terminal: the
notebook is running a *different* Python than the one you installed
packages into. Fix:
```python
import sys
print(sys.executable)
```
Compare that path to `which python3` in the terminal where `pip install`
succeeded. If they differ, switch the notebook's kernel (Section 4) to the
one matching your venv, or re-register the kernel from inside the correct
venv (Section 2).
