# Agent memory policy

Governs the persistent memory of the MOSAIC subagents (see `.claude/agents/`). Project memory
lives here under `<agent-name>/`; **local** memory (the calibration-doctor) lives under
`.claude/agent-memory-local/<agent-name>/` and is *not* version-controlled.

## Store
- Durable MOSAIC development patterns, architectural facts, recurring gotchas, and reviewed lessons.
- Stable invariants and conventions an agent should not have to re-derive.

## Do not store
- Private run outputs, unpublished data, or scenario specifics.
- Credentials or secrets.
- Absolute local paths or machine-specific details.
- User-specific troubleshooting transcripts.
- Speculative/unverified conclusions presented as fact.

## Format
- Concise notes with: date, agent name, the invariant/finding, evidence files, and a confidence level.
- Link related notes; prefer updating an existing note over adding a near-duplicate.

## Lifecycle
- **Promote** stable, project-wide invariants to `CLAUDE.md` after review — memory is a staging
  area, `CLAUDE.md` is the canonical record.
- **Delete or rewrite** stale memory when package behavior changes (e.g. an engine upgrade that
  flips a field's semantics).

## Why local for the calibration-doctor
Diagnostic work routinely involves country-specific outputs, unpublished scenarios, and local
file paths. Keeping that agent's memory **local** prevents such details from being committed to
the shared repository while still letting it accumulate a reusable triage playbook.
