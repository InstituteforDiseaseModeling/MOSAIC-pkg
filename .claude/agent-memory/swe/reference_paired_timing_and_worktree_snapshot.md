---
name: reference-paired-timing-and-worktree-snapshot
description: How to A/B the engine in ONE R process (source base sim_*.R into env with parent=HEAD namespace) and why to review from a git-archive snapshot when other agents share the worktree
metadata:
  type: reference
---

**In-process paired engine A/B (lessons 17/18 compliant).** `git worktree add --detach /tmp/base <sha>`, then in a
HEAD `pkgload::load_all()` session: `env_base <- new.env(parent = asNamespace("MOSAIC"))` and
`sys.source()` every `/tmp/base/R/sim_*.R` into it in alphabetical order (sim_engine.R builds
`.SIM_PHASE_FUNCTIONS` from the phase functions defined before it). `env_base$run_simulation()` reproduced the base
PROCESS output identical() on 40-loc and 1-loc. Interleave arms in random order per block, time with
user+sys CPU (robust when the laptop load average is 20-50 from other agents' PSOCK runs), report the median and
IQR of per-block ratios. Scripts: claude/cfr_v21_review/redteam/swe/perf_setup.R, perf_engine2.R.

**Worktree snapshot.** Other agents may edit the same worktree mid-review (2026-09-28: a half-applied rewrite
left `.d7_setup` with a new signature and an old call site → "unused arguments"). Review from
`git archive HEAD | tar -x -C /tmp/snap` and `load_all("/tmp/snap")`; for PSOCK paths `R CMD INSTALL --library=/tmp/lib
/tmp/snap` and prepend `/tmp/lib` to `.libPaths()` (workers inherit the parent's lib paths). Compare
`git show HEAD:file` rather than reading the working tree.

Related: [[cfr-v21-engine-review]], [[reference_rengine_cost_model]].
