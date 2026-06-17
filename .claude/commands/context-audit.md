---
description: Run the AI-context hygiene audit over the .claude/ surface and CLAUDE.md pair
argument-hint: "[scope, e.g. 'CLAUDE.md pair' / 'roster' / 'everything' (default)]"
---
Use the **ai-architect** subagent with its **context-audit** skill to audit (and, where clearly
warranted, prune) this repo's AI-context surface and return the audit verdict:

    $ARGUMENTS

Steps the architect should follow:
1. Establish the budget: word/token counts for `MOSAIC/CLAUDE.md` + `MOSAIC-pkg/CLAUDE.md`, and
   confirm every `MEMORY.md` link resolves to a file whose `name:` slug matches its filename.
2. Work the skill's A–F checklist: CLAUDE.md duplication/contradiction/wrong-tier, memory staleness,
   roster overlap + README sync, skill/command/settings hygiene, cross-cutting stale references.
3. Bias to subtraction (remove / merge / relocate-to-skill-or-hook); never add a rule to fix an
   ignored rule. Verify any "stale" reference against the repo before flagging.
4. Return the CONTEXT-AUDIT VERDICT (CLEAN / NITS / NEEDS-PRUNING) with evidence per finding and
   line/token deltas for any edits. Hand R-code/Lessons issues to `reviewer`.
