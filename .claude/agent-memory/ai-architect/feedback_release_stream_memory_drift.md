---
name: release-stream-memory-drift
description: After a release stream, agent memory goes stale in two predictable places - worktree-written notes (agents write the worktree's own .claude/agent-memory) and RC red-team notes whose findings were fixed before release; how to consolidate and sweep
metadata:
  type: feedback
---

**Rule:** before release worktrees are removed, (1) consolidate worktree-written memory into the main
checkout and (2) sweep the release-window notes for RC-state claims. At v0.101.0 (2026-10-02), 6 notes
existed only in worktrees, and about 30 notes or index lines stated RC or superseded state as current.

**Why:** an agent running in `.claude/worktrees/<wt>` writes that worktree's `.claude/agent-memory`.
`.git/info/exclude` hides the worktrees dir, so nothing flags the stray notes. Red-team notes are written
hours before the fixes land in the same release, so "still broken / at vX it was not / awaits a user
decision" is often stale on arrival.

**How to apply:**
- Consolidate file by file (`cmp`). Never copy a worktree MEMORY.md wholesale: it is a stale snapshot of
  main plus one line for its own note, so port only that line. Also check for committed memory on each branch
  (`git diff main...<branch> -- .claude/agent-memory`).
- A worktree's gitignored `claude/` scratch dies with it. Grep memory for `.claude/worktrees/` paths
  and flag them to the user.
- Sweep notes edited during the release for "still|awaits|not done|not yet|HELD|BLOCKER|pins". Verify each
  hit against NEWS and `git log -S`. Correct it with a dated outcome line and keep the original record.
- Index hooks drift from their own note bodies (an index line said "HELD" while its note said "Fixed"; another
  said "BLOCKER" while its note said "RESOLVED"). Diff each hook against the note's description.
- Numbers "measured on config_default vN" go stale at the next data-object rebuild, so annotate them with the
  shipped values.

Related: [[feedback-skill-constants-rot-despite-hedge]], [[feedback_measured_numbers_need_one_home]].
