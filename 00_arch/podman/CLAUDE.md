# Claude Instructions — trigger_based_new_podman

## Memory
- ALL project memory lives in `memory/` inside this directory — read and write there only
- Index: `memory/MEMORY.md`
- Do NOT save any memory to `~/.claude/projects/` for this project
- When starting a conversation, read `memory/MEMORY.md` and load referenced files for context

## Working Preferences
- **Changes one by one**: explain the change and reasoning first, wait for confirmation before applying
- **Never auto-commit**: user commits manually — do NOT run `git commit` unless explicitly asked
- **Verify after changes**: confirm the edit is present in the file before moving on
- **Show changes first**: when making multiple related changes, show the plan and get approval first

## Environment
- Local Podman simulation of the UBS DSF trigger-based pipeline
- Redpanda (Kafka-compatible) running via Podman with `--network=host`
- No SSL — plaintext connections only (localhost:9092 Kafka, localhost:8081 Schema Registry)
- Full runbook: `RUNBOOK.md`
