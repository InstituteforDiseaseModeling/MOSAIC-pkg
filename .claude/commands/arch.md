---
description: Route to the ai-architect agent (AI-context hygiene — AA / ARCH)
argument-hint: "[what to audit, or 'the whole .claude/ surface']"
---
Use the **ai-architect** subagent — MOSAIC's keeper of AI-context hygiene. It audits and prunes the
`.claude/` meta-layer (agents, skills, commands, settings) and the always-loaded context economy (the
CLAUDE.md pair, MEMORY.md + memory/*.md) for bloat, duplication, contradiction, stale references, and
agent-responsibility overlap — driving each always-loaded file toward the smallest set of high-signal
tokens that still prevents mistakes. It biases to subtraction, shows evidence and deltas, and returns
a verdict (CLEAN / NITS / NEEDS-PRUNING). It does NOT review R-code correctness (that is `maintainer`).
Audit:

$ARGUMENTS
