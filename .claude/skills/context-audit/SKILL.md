---
name: context-audit
description: >
  Audit this repo's AI-context hygiene and prune toward the smallest set of high-signal tokens that
  still prevents mistakes. Checks the always-loaded CLAUDE.md pair, the MEMORY.md index + memory/*.md
  store, the agent roster, skills, slash commands, and settings for bloat, duplication, contradiction,
  stale references (paths/functions/versions/fields that no longer exist), agent-responsibility
  overlap, and over-broad tool grants. Use after adding or editing an agent/skill/command or a
  CLAUDE.md, or when Claude seems to ignore documented rules. Reorganizes content into the right tier
  (CLAUDE.md / skill / hook); biases to subtraction, not adding rules.
---

# context-audit — repo AI-context hygiene

Every session loads, before the user types anything: the CLAUDE.md pair, the MEMORY.md index, the
agent roster's `description` fields, and the skill descriptions. That surface grows and drifts. This
skill is the checklist for keeping it lean, internally consistent, and free of stale references.

**North star:** drive each *always-loaded* file toward *the smallest set of high-signal tokens that
still prevents Claude from making mistakes.* **Litmus test for any line:** *"Would removing this
cause Claude to make mistakes? If not, cut it."*

**Operating principles**
- **Subtraction over addition.** Default action is remove / merge / relocate. Never "add a rule to
  fix an ignored rule" — a chronically ignored rule signals bloat, not a missing rule.
- **Right tier.** Broadly-always → CLAUDE.md. Only-sometimes → a skill. Must-happen-every-time →
  a hook (deterministic), not advisory prose.
- **Evidence, not assertion.** Quote the duplication / contradiction / dangling reference. Verify a
  reference is truly stale (Grep the repo) before flagging it. Report line/token deltas for edits.
- **No make-work.** Flag only what would plausibly cause Claude to err or waste budget; mark the
  rest optional. A clean surface is a valid verdict.

## The checklist

### A. CLAUDE.md health (the always-loaded budget — highest leverage)
- [ ] **Budget.** Total length sane? Bloat causes instruction-dropping. Report approx word/token
      counts for both `MOSAIC/CLAUDE.md` and `MOSAIC-pkg/CLAUDE.md`.
- [ ] **Litmus per line.** Flag self-evident advice ("write clean code"), standard-language
      conventions, file-by-file narration, long tutorials, and frequently-changing facts — the
      documented ❌-exclude list. These fail "would removing this cause a mistake?"
- [ ] **Duplication across the pair.** The root and package files concatenate; the same rule stated
      in both is wasted budget. Quote both occurrences when flagging (repo-access rules, versioning
      protocol, and thread-safety blocks are the usual repeat offenders here).
- [ ] **Contradictions.** No two sections/files give conflicting instructions (e.g. one says median
      default, another mean). Contradiction is the worst noise — it actively misleads.
- [ ] **Wrong tier.** Only-sometimes content → a skill; must-happen-every-time → a hook; bulky
      reference → an `@path` import rather than inlined.

### B. Memory health
- [ ] **Every MEMORY.md link resolves**, and the target's frontmatter `name:` slug matches the
      filename (a mismatch is a latent dangling reference — see the live example below).
- [ ] **No stale references** inside memory bodies — paths, function names, version numbers, flags,
      URLs that no longer exist. Verify against the repo before flagging.
- [ ] **No contradiction** with CLAUDE.md or a sibling memory (memory must not silently override
      checked-in instructions).
- [ ] **Prune superseded entries**; one fact per file; `MEMORY.md` is one line per memory, no content.

### C. Agent roster health (`.claude/agents/`)
- [ ] **No undisambiguable overlap.** If a human can't say which agent owns a task, neither can
      Claude — sharpen the boundary or merge. (Overlap that's *intentionally* parallel is fine; the
      README's grey-zone rules are tie-breakers, not walls — keep them coherent.)
- [ ] **Description routes correctly** — specific enough to fire on the right requests, not so broad
      it over-triggers. The `name` + `description` is what the orchestrator matches on.
- [ ] **Tool grants scoped to the job** (read-only agents have no Write/Edit; nobody over-provisioned).
- [ ] **Prompt altitude** — not a brittle if-else script, not vague platitudes; strong heuristics +
      a few canonical examples.
- [ ] **README in sync.** `.claude/agents/README.md` has four places that must agree with the actual
      agent files: the **Quick-reference** table, the **roster** table, the **routing cheat-sheet**,
      and the **verification counts** ("eight"/"nine" agents, the `/agents` count). Frontmatter
      `name` values must be **unique** (duplicates are silently discarded).

### D. Skill health (`.claude/skills/`)
- [ ] `SKILL.md` lean; heavy reference pushed to supporting files (progressive disclosure — only the
      description is always-loaded; the body loads on invocation).
- [ ] `description` accurately scopes when it fires (mis-scoped → never triggers or triggers spuriously).
- [ ] No skill duplicating an agent or another skill.

### E. Settings / tools health (`.claude/settings*.json`)
- [ ] Permission allowlists present for safe, frequent commands (cut prompt churn without over-granting).
- [ ] "Must-happen-every-time" rules implemented as **hooks**, not advisory CLAUDE.md text.
- [ ] No bloated / overlapping tool or MCP surface.

### F. Cross-cutting staleness
- [ ] Any version, path, or renamed field/function/flag cited in *any* AI-context file still exists.
      (This repo's own Lessons #11/#12 are exactly this failure class — field renames that went
      stale across sibling files.) `grep -rn "<old-name>" .claude/ ../CLAUDE.md CLAUDE.md`.
- [ ] Each rule plausibly changes behavior; a chronically ignored rule means prune, not reinforce.

## Suggested sweep (fast, read-only)
```bash
# CLAUDE.md budget
for f in ../CLAUDE.md CLAUDE.md; do printf "%-24s " "$f"; wc -w < "$f"; done
# Dangling MEMORY.md links (link target must exist on disk)
cd <memory-dir>; grep -oE '\]\([a-z0-9_]+\.md\)' MEMORY.md | tr -d ']()' \
  | while read f; do [ -f "$f" ] || echo "MISSING: $f"; done
# memory filename vs its own name: slug
for f in <memory-dir>/*.md; do s=$(grep -m1 '^name:' "$f" | sed 's/name: *//'); \
  b=$(basename "$f" .md); [ "$s" = "$b" ] || echo "SLUG≠FILE: $b (name: $s)"; done
# Agent name uniqueness
grep -h '^name:' .claude/agents/*.md | sort | uniq -d
```

## Live reference example (the kind of defect to catch)
`MEMORY.md` linked to `project_bias_sweep_moz_2024_10.md`, but the file on disk was named
`project_bias_sweep_2024_10.md` (missing `_moz_`) — while the file's own `name:` slug *and* four
other references used the `_moz_` form. Fix: rename the file to match its slug and the references,
not the links. A single-character drift produced a dangling index link and four soft-broken prose
references.

## Output: the audit report
```
CONTEXT-AUDIT VERDICT — <CLEAN | NITS | NEEDS-PRUNING>

BUDGET
  MOSAIC/CLAUDE.md      <words> | MOSAIC-pkg/CLAUDE.md <words> | MEMORY.md <links, all resolve? y/n>

FINDINGS (each: severity · file[:line/section] · evidence · action)
  [CRITICAL] contradiction / dangling reference that will mislead or break
  [WARN]     duplication / bloat / wrong-tier / overlap that wastes budget or risks drift
  [NIT]      optional tidy-ups

EDITS MADE          file — what changed — Δ lines/≈tokens — (duplication/contradiction/link resolved)
RELOCATIONS         content moved CLAUDE.md → skill/hook (with rationale)
HAND-OFFS           R-code/Lessons → reviewer
```

## References (curated — cite, do not inline)
- Effective context engineering for AI agents — anthropic.com/engineering/effective-context-engineering-for-ai-agents
- Building effective agents — anthropic.com/engineering/building-effective-agents
- Writing effective tools for agents — anthropic.com/engineering/writing-tools-for-agents
- Claude Code best practices + memory — code.claude.com/docs/en/best-practices · code.claude.com/docs/en/memory
- Subagents / Skills / Commands specs — code.claude.com/docs/en/sub-agents · /skills · /commands
