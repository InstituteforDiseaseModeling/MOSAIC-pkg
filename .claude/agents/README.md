# MOSAIC Claude Code subagent roster

Claude Code **subagents** for the MOSAIC workspace — nine agents (six development & maintenance
specialists, two user-facing agents, and one AI-context architect) so domain work routes to a
focused system prompt with the right tool scope. They load `CLAUDE.md` automatically, so the
Lessons-Learned gotchas reach every agent.

> **Location:** these are **defined and git-tracked in `MOSAIC-pkg/.claude/`** — they ship with the
> package on GitHub. They load from sessions rooted in `MOSAIC-pkg`, and can **read/write data and
> outputs in sibling repos under `~/MOSAIC`** (country repos like `MOSAIC-Mozambique`, the shared
> `output/` tree) via the session's `additionalDirectories` grant. So: definitions live in the
> package; their working reach extends across the workspace.

> These are project-level Claude Code **subagents**, *not* the experimental "agent teams" feature.
> The **main session is the orchestrator**: it routes a request to a subagent by matching the
> request against each agent's `description`, and subagents report back to it. Subagents **cannot
> spawn other subagents** — for a complex investigation the main session invokes several
> sequentially and synthesizes. **Overlap is intentional:** you can ask several agents (e.g. `swe`
> + `disease-modeler` + `ml-scientist`) to review the same thing in parallel; the routing/grey-zone
> rules below are tie-breakers for *auto*-routing, not walls.

## Quick reference

Color matches each agent's `color:` frontmatter (how it appears in `/agents` and the transcript).
Aliases are the spoken short names; the shortcut is a working slash command that routes to the agent.

| Color | Agent | Aliases | Shortcut | One-liner |
|---|---|---|---|---|
| 🔵 blue | `swe` | **SWE**, SE | `/swe` | Software engineer — orchestration, infra, Python bridge, packaging, plots |
| 🟣 purple | `statistician` | **STAT**, STATS | `/stat` | Bayesian/math core — likelihood, weights, convergence, ensemble, WIS, R² |
| 🟢 green | `disease-modeler` | **EPI**, DM | `/dm` | Epidemiologist — priors, `est_*`, ICs, CFR, reporting chain |
| 🟠 orange | `ml-scientist` | **ML**, DS | `/ml` | ML / data scientist — LSTM/FiLM suitability, ψ rolling-CV, features |
| 🩷 pink | `data-engineer` | **ETL**, DE | `/etl` | Data ingestion — `process_*`, `download_*`, geospatial, source reconciliation |
| 🟡 yellow | `maintainer` | **MAINT**, REV | `/maint` | R-package maintainer — R CMD check/build hygiene, testthat suite, roxygen/NAMESPACE/pkgdown docs, DESCRIPTION/deps, versioning + independent review |
| 🩵 cyan | `run-guide` | **RG**, GUIDE | `/guide` | User how-to — install / configure / run / deploy / scenarios |
| 🔴 red | `calibration-doctor` | **DOC**, DR | `/doctor`, `/diagnose-fit` | Active result diagnosis — bias/convergence/ψ; runs `run_fit_sandbox` |
| ⚪ white | `ai-architect` | **AA**, ARCH | `/arch`, `/context-audit` | AI-context hygiene — prunes CLAUDE.md/memory/roster/skills/settings for bloat & drift |

## The roster

| Agent | Color | Role | Model | Memory | Tools |
|---|---|---|---|---|---|
| `swe` | 🔵 blue | Software engineer — orchestration, infra, Python bridge, parallel, packaging, docs/build mechanics, plot rendering | opus | project | Read/Edit/Write/Bash/Grep/Glob |
| `statistician` | 🟣 purple | Bayesian/math core — likelihood, weights, convergence, ensemble, WIS, R² | opus | project | Read/Edit/Write/Bash/Grep/Glob |
| `disease-modeler` | 🟢 green | Epidemiology — priors, `est_*`, ICs, CFR, reporting chain | opus | project | Read/Edit/Write/Bash/Grep/Glob |
| `ml-scientist` | 🟠 orange | Suitability ML — LSTM/FiLM, ψ rolling-CV, features, calibration | opus | project | Read/Edit/Write/Bash/Grep/Glob |
| `data-engineer` | 🩷 pink | Data ingestion/ETL — `process_*`, `download_*`, geospatial, ISO/format utils, source-repo refresh | opus | project | Read/Edit/Write/Bash/Grep/Glob |
| `maintainer` | 🟡 yellow | R-package maintainer — R CMD check/build hygiene & speed, testthat suite (prune/coverage/upkeep), roxygen/NAMESPACE docs, pkgdown site, DESCRIPTION/dependencies, versioning, README/NEWS, deprecation lifecycle + independent adversarial review | opus | project | Read/Edit/Write/Bash/Grep/Glob/WebFetch/WebSearch |
| `run-guide` | 🩵 cyan | User how-to — install/configure/run/deploy/scenarios | sonnet | off (no Write/Edit; Bash inspection-only) | Read/Grep/Glob/Bash |
| `calibration-doctor` | 🔴 red | Active result diagnosis — bias, convergence, ψ attenuation, output interpretation; runs deterministic `run_fit_sandbox` (diagnose-fit skill) | opus | **local** | Read/Grep/Glob/Bash (read-only is prompt-enforced) |
| `ai-architect` | ⚪ white | AI-context hygiene — prunes the CLAUDE.md pair, memory store, agent roster, skills, commands, settings for bloat/duplication/contradiction/stale refs (context-audit skill) | opus | project | Read/Edit/Write/Bash/Grep/Glob |

## Shared operating contract

Every agent's prompt restates the MOSAIC norms (CLAUDE.md is also inherited): follow package R
style and testthat/roxygen conventions; make small reviewable changes (no opportunistic
refactors); read roxygen + tests before editing an exported function; return a structured
handoff recommendation when a task belongs to another agent; never treat a calibration-output
symptom as a confirmed code bug without file-level evidence or a minimal reproduction.

**Canonical model knowledge.** When a task turns on what a parameter or model term *means*, the
authoritative source is **`MOSAIC-docs/04-model-description.Rmd`** — its inline "Table of model
parameters" defines every symbol, with per-parameter derivations in the sections below it
(calibration methodology in `05-model-calibration.Rmd`, data sources in `03-data.Rmd`). Read the
relevant `.Rmd` section on demand rather than inferring meaning from a variable name; the rendered
`docs/*.md` can be stale. The engine-side parameter contract is
`laser-cholera/src/laser/cholera/metapop/params.py`. (Reachable via the `additionalDirectories` grant.)

The six development & maintenance specialists share a workflow gate (per CLAUDE.md):
`devtools::test()` before/after → `devtools::document()` if signatures/roxygen changed →
`R CMD check .` → bump DESCRIPTION → commit with version. They ask before adding deps or changing
exported signatures / the `run_MOSAIC()` core loop.

## Routing cheat-sheet

| If the request is about… | Routes to |
|---|---|
| `run_MOSAIC()` loop, Dask/PSOCK, reticulate bridge, `run_LASER.R`, packaging, R CMD check, perf, test infra, docs/build mechanics, **all `plot_*` rendering** | `swe` |
| Likelihood math, Gibbs weights, ESS/agreement/CVW, ensemble/quantiles, WIS, R², distribution fitting | `statistician` |
| Priors / `est_*`, initial conditions, CFR, vaccination, seasonality, shedding, biological plausibility, literature anchoring | `disease-modeler` |
| What a parameter / model term *means* (symbol→definition), or where a model term comes from | `disease-modeler` — cites `MOSAIC-docs/04-model-description.Rmd` |
| LSTM suitability, FiLM, rolling-origin CV for ψ, feature engineering, ψ calibration, leakage/overfit | `ml-scientist` |
| Raw data ingestion/ETL — `process_*`, `download_*`, geospatial, ISO/format utils, source-repo refresh, data provenance/semantics | `data-engineer` |
| "Review this change", pre-commit audit, "is it complete & wired in?", R-package maintenance (R CMD check/build speed, tests, docs/pkgdown, DESCRIPTION/deps, versioning) | `maintainer` |
| "How do I install / configure / run / deploy / build a scenario?" | `run-guide` |
| "My run came out wrong / what does this diagnostic mean / why didn't it converge?" | `calibration-doctor` |
| CLAUDE.md/memory/agent/skill/command/settings hygiene — bloat, duplication, contradiction, stale refs, roster overlap, "Claude is ignoring a rule" | `ai-architect` |

## Grey-zone rules (tie-breakers for auto-routing — overlap is still fine)

| Grey zone | Owner | Rule |
|---|---|---|
| Raw `process_*`/`download_*` pipeline vs. the prior derived from it | pipeline → `data-engineer`; biological value → `disease-modeler` | data-engineer owns provenance/units; disease-modeler owns the parameter meaning |
| Distribution fitting | machinery → `statistician`; values → `disease-modeler` | `fit_*_from_ci` / `calc_model_posterior_distributions` is math; the biological CI it consumes is epi |
| Cross-validation | suitability CV → `ml-scientist`; general CV plumbing → `swe` | `rolling_cv_suitability*` vs `run_rolling_cv.R`/cluster setup |
| Pre-commit verification & package maintenance | `maintainer` | independent audit + build/test/doc/dep/version upkeep; dev agents still self-check, maintainer is the second pair of eyes |
| Diagnostic plot won't render / layout broken | `swe` | rendering/layout/file-output mechanics |
| Diagnostic plot is confusing to read | `calibration-doctor` | user-facing run interpretation |
| Plot shows a statistically wrong posterior/quantile | `statistician` | math/aggregation/scoring issue, not rendering |
| Prior value or prior label looks wrong | `disease-modeler` | biological meaning and prior semantics |
| "What does parameter X mean / where does a model term come from?" | docs definition → `disease-modeler` | `04-model-description.Rmd` owns symbol→meaning; route to `statistician` (scoring / R₀ / generation-time math) or `ml-scientist` (ψ internals) only for the *math/internals*, not the definition |
| Prior/config artifact rebuild or versioning fails | `swe` (with `disease-modeler` review) | packaging/data-artifact workflow |
| Suitability affects calibration behavior | `ml-scientist` first, then `calibration-doctor` for run-level triage | separate ψ signal quality from run diagnosis |
| `reported_deaths` / `disease_deaths` confusion | `statistician` (scoring), `disease-modeler` (meaning), `data-engineer` (processed-data semantics), `run-guide` (user output) | make the field convention explicit everywhere |
| Vignettes / user docs | `run-guide` **drafts** usage text; `swe` **edits** the `.Rmd`/build | run-guide proposes the prose; swe commits it |
| A user-support agent uncovers a real code bug | state it + hand off to the owning dev agent | run-guide/doctor don't edit source |
| `.claude/` context hygiene vs. code review | meta-layer **incl. Lessons-Learned placement** → `ai-architect`; R-code correctness → `maintainer` | architect prunes/owns the *context surface* (CLAUDE.md structure/budget incl. where Lessons entries sit, memory, roster, skills, settings); maintainer reviews code correctness and **drafts** the Lessons content |

## Skills

- **`diagnose-fit`** (`.claude/skills/diagnose-fit/`) — active model-fit diagnosis. Drives fast
  deterministic single-LASER experiments (`MOSAIC::run_fit_sandbox`) scored by
  `MOSAIC::calc_fit_diagnostics` to find what parameter changes improve fit, then writes a
  calibration-actionable brief. **Preloaded into `calibration-doctor`**; other agents/the main
  session can invoke it via the `Skill` tool where that tool is enabled. The parameter→behavior
  map in the skill is universal; country-specific facts live in the doctor's local memory.
- **`context-audit`** (`.claude/skills/context-audit/`) — repo AI-context hygiene. The A–F checklist
  for auditing the always-loaded CLAUDE.md pair, the memory store, the agent roster, skills,
  commands, and settings for bloat, duplication, contradiction, and stale references, plus the audit
  report format. **Preloaded into `ai-architect`**; biases to subtraction (prune/merge/relocate),
  not adding rules.
- **`hedgehog-run`** (`.claude/skills/hedgehog-run/`) — launch, monitor, and retrieve MOSAIC
  calibration runs on the hedgehog Azure VM (120 cores / 448 GB): backend choice (local PSOCK vs
  Coiled hybrid), the GLIBCXX R wrapper, surviving SSH disconnect (nohup/tmux), the control +
  `dask_spec` recipe, and pulling results. Invoke via the `Skill` tool for hedgehog run logistics.
- **`docker-image-update`** (`.claude/skills/docker-image-update/`) — rebuild and publish the MOSAIC
  Coiled worker image (`idmmosaicacr.azurecr.io/mosaic-worker`): backup-tag, cross-arch
  (`--platform linux/amd64`) build, push to ACR, delete-then-recreate the `mosaic-acr-workers` Coiled
  env, smoke test, roll back. Turnkey via `azure/rebuild_image.sh` + `azure/refresh_coiled_env.py`.
  Invoke via the `Skill` tool when updating the worker image after a version/dependency bump.

## Invoking

- **Automatic:** describe the task; the main session delegates based on the descriptions above.
- **Explicit:** `@statistician why is the WIS term off by a factor of two?`, or
  "use the calibration-doctor agent to diagnose this run".
- **Shortcuts (slash commands):** `/swe`, `/stat`, `/dm`, `/ml`, `/etl`, `/maint`, `/guide`, `/doctor`,
  `/arch` each route a request to the matching agent; `/diagnose-fit <run-dir>` runs the active
  fit-diagnostic workflow and `/context-audit` runs the AI-context hygiene audit. These live in
  `.claude/commands/`.
- Manage/inspect with the `/agents` command.

## Memory

- `memory: project` (the six dev/maintenance specialists + `ai-architect`) → `.claude/agent-memory/<name>/`.
- `memory: local` (`calibration-doctor`) → `.claude/agent-memory-local/<name>/`, **not**
  version-controlled — appropriate for diagnostics that touch country-specific/unpublished data.
- `run-guide` has memory **off**, so it has no Write/Edit tools. (Bash remains a theoretical write
  vector; its prompt restricts Bash to inspection/execution only — read-only is therefore
  prompt-enforced, not a hard sandbox.)
- Enabling memory auto-grants Write/Edit; the doctor's read-only-source guarantee is likewise
  prompt-enforced. See `.claude/agent-memory/README.md` for the storage policy.

## Verification (repeatable checklist)

1. Restart the session (or create via `/agents`) so file-based agents load; `/agents` shows all
   **nine** with correct tools/model/memory/color.
2. **Frontmatter:** YAML parses; required `name`/`description` present; valid model aliases,
   colors, tool names; `name` values unique (duplicates are silently discarded).
3. **Positive route matrix:** "Why is WIS off by 2×?" → statistician; "Port the FiLM embedding into
   est_suitability" → ml-scientist; "Was mu_j_baseline already rho_deaths-corrected?" →
   disease-modeler; "The WHO weekly counts look wrong after reprocessing" → data-engineer;
   "Review my diff before commit" → maintainer; "A Dask worker deadlocks" → swe;
   "Set control params for a 500-sim run" → run-guide; "My run over-predicts 3×, diagnose it" →
   calibration-doctor.
4. **Negative route matrix:** "The WIS diagnostic plot label overlaps" → swe (not statistician);
   "This run looks biased, why?" → calibration-doctor (not run-guide).
5. **Inheritance:** ask any agent the post-v0.13 deaths-field rule → must answer `reported_deaths`.
6. **Read-only enforcement:** ask `run-guide` and `calibration-doctor` to edit a source file →
   both must refuse and (for the doctor) confirm writes are confined to its memory dir.

## Using this roster (and MOSAIC-pkg files) from an adjacent repo

If you work in a *different* repo alongside a clone of `MOSAIC-pkg` (siblings under one parent):

```
your-workspace/
├── MOSAIC-pkg/      ← this clone
└── your-project/    ← you work here
```

two **independent** mechanisms give you the two things you want — they do **not** substitute for each other:

**1. Symlink the agents → makes this roster usable in your repo**
```bash
cd your-project && mkdir -p .claude
ln -s ../../MOSAIC-pkg/.claude/agents .claude/agents
```
The agents listed above then appear in `/agents` and can be invoked from your sessions — Claude Code discovers them by walking up from your working dir and following the symlink. (If MOSAIC-pkg is not a direct sibling, point the link at an absolute path instead.)

**2. Grant file access → lets Claude read/edit MOSAIC-pkg files** — add to `your-project/.claude/settings.local.json`:
```json
{ "permissions": { "additionalDirectories": ["/abs/path/to/MOSAIC-pkg"] } }
```
Claude (and the agents) can now open and edit MOSAIC-pkg source from your session; without it, MOSAIC-pkg sits outside your repo and is off-limits to the file tools.

**Why both:** the symlink only handles *agent discovery* (`additionalDirectories` does **not** load agents), and `additionalDirectories` only handles *file access* (the symlink does **not** grant it). Agents that also need to read MOSAIC-pkg source require both steps.

**Verify / shortcut:** restart Claude Code, then run `/agents` — the roster should appear. A one-off alternative that does both at once (no files to edit): launch with `claude --add-dir /abs/path/to/MOSAIC-pkg` (the flag grants file access *and* loads the agents). Note: the symlink is local/untracked by default — `git add .claude/agents` if you want it to travel with your repo.
