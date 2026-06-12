# MOSAIC Claude Code subagent roster

Claude Code **subagents** for the MOSAIC workspace — eight agents (six development & maintenance
specialists + two user-facing agents) so domain work routes to a focused system prompt with the
right tool scope. They load `CLAUDE.md` automatically, so the Lessons-Learned gotchas reach every
agent.

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
| 🟡 yellow | `reviewer` | **CR**, REV | `/cr` | Code review (adversarial) + housekeeping (docs/NAMESPACE/version/Lessons) |
| 🩵 cyan | `run-guide` | **RG**, GUIDE | `/guide` | User how-to — install / configure / run / deploy / scenarios |
| 🔴 red | `calibration-doctor` | **DOC**, DR | `/doctor`, `/diagnose-fit` | Active result diagnosis — bias/convergence/ψ; runs `run_fit_sandbox` |

## The roster

| Agent | Color | Role | Model | Memory | Tools |
|---|---|---|---|---|---|
| `swe` | 🔵 blue | Software engineer — orchestration, infra, Python bridge, parallel, packaging, docs/build mechanics, plot rendering | opus | project | Read/Edit/Write/Bash/Grep/Glob |
| `statistician` | 🟣 purple | Bayesian/math core — likelihood, weights, convergence, ensemble, WIS, R² | opus | project | Read/Edit/Write/Bash/Grep/Glob |
| `disease-modeler` | 🟢 green | Epidemiology — priors, `est_*`, ICs, CFR, reporting chain | opus | project | Read/Edit/Write/Bash/Grep/Glob |
| `ml-scientist` | 🟠 orange | Suitability ML — LSTM/FiLM, ψ rolling-CV, features, calibration | opus | project | Read/Edit/Write/Bash/Grep/Glob |
| `data-engineer` | 🩷 pink | Data ingestion/ETL — `process_*`, `download_*`, geospatial, ISO/format utils, source-repo refresh | opus | project | Read/Edit/Write/Bash/Grep/Glob |
| `reviewer` | 🟡 yellow | Independent adversarial review + housekeeping (docs/NAMESPACE/pkgdown/version/README/Lessons) | opus | project | Read/Edit/Write/Bash/Grep/Glob |
| `run-guide` | 🩵 cyan | User how-to — install/configure/run/deploy/scenarios | sonnet | off (no Write/Edit; Bash inspection-only) | Read/Grep/Glob/Bash |
| `calibration-doctor` | 🔴 red | Active result diagnosis — bias, convergence, ψ attenuation, output interpretation; runs deterministic `run_fit_sandbox` (diagnose-fit skill) | opus | **local** | Read/Grep/Glob/Bash (read-only is prompt-enforced) |

## Shared operating contract

Every agent's prompt restates the MOSAIC norms (CLAUDE.md is also inherited): follow package R
style and testthat/roxygen conventions; make small reviewable changes (no opportunistic
refactors); read roxygen + tests before editing an exported function; return a structured
handoff recommendation when a task belongs to another agent; never treat a calibration-output
symptom as a confirmed code bug without file-level evidence or a minimal reproduction.

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
| LSTM suitability, FiLM, rolling-origin CV for ψ, feature engineering, ψ calibration, leakage/overfit | `ml-scientist` |
| Raw data ingestion/ETL — `process_*`, `download_*`, geospatial, ISO/format utils, source-repo refresh, data provenance/semantics | `data-engineer` |
| "Review this change", pre-commit audit, "is it complete & wired in?", housekeeping (docs/NAMESPACE/version) | `reviewer` |
| "How do I install / configure / run / deploy / build a scenario?" | `run-guide` |
| "My run came out wrong / what does this diagnostic mean / why didn't it converge?" | `calibration-doctor` |

## Grey-zone rules (tie-breakers for auto-routing — overlap is still fine)

| Grey zone | Owner | Rule |
|---|---|---|
| Raw `process_*`/`download_*` pipeline vs. the prior derived from it | pipeline → `data-engineer`; biological value → `disease-modeler` | data-engineer owns provenance/units; disease-modeler owns the parameter meaning |
| Distribution fitting | machinery → `statistician`; values → `disease-modeler` | `fit_*_from_ci` / `calc_model_posterior_distributions` is math; the biological CI it consumes is epi |
| Cross-validation | suitability CV → `ml-scientist`; general CV plumbing → `swe` | `rolling_cv_suitability*` vs `run_rolling_cv.R`/cluster setup |
| Pre-commit verification & housekeeping | `reviewer` | independent audit + docs/NAMESPACE/version upkeep; dev agents still self-check, reviewer is the second pair of eyes |
| Diagnostic plot won't render / layout broken | `swe` | rendering/layout/file-output mechanics |
| Diagnostic plot is confusing to read | `calibration-doctor` | user-facing run interpretation |
| Plot shows a statistically wrong posterior/quantile | `statistician` | math/aggregation/scoring issue, not rendering |
| Prior value or prior label looks wrong | `disease-modeler` | biological meaning and prior semantics |
| Prior/config artifact rebuild or versioning fails | `swe` (with `disease-modeler` review) | packaging/data-artifact workflow |
| Suitability affects calibration behavior | `ml-scientist` first, then `calibration-doctor` for run-level triage | separate ψ signal quality from run diagnosis |
| `reported_deaths` / `disease_deaths` confusion | `statistician` (scoring), `disease-modeler` (meaning), `data-engineer` (processed-data semantics), `run-guide` (user output) | make the field convention explicit everywhere |
| Vignettes / user docs | `run-guide` **drafts** usage text; `swe` **edits** the `.Rmd`/build | run-guide proposes the prose; swe commits it |
| A user-support agent uncovers a real code bug | state it + hand off to the owning dev agent | run-guide/doctor don't edit source |

## Skills

- **`diagnose-fit`** (`.claude/skills/diagnose-fit/`) — active model-fit diagnosis. Drives fast
  deterministic single-LASER experiments (`MOSAIC::run_fit_sandbox`) scored by
  `MOSAIC::calc_fit_diagnostics` to find what parameter changes improve fit, then writes a
  calibration-actionable brief. **Preloaded into `calibration-doctor`**; other agents/the main
  session can invoke it via the `Skill` tool where that tool is enabled. The parameter→behavior
  map in the skill is universal; country-specific facts live in the doctor's local memory.

## Invoking

- **Automatic:** describe the task; the main session delegates based on the descriptions above.
- **Explicit:** `@statistician why is the WIS term off by a factor of two?`, or
  "use the calibration-doctor agent to diagnose this run".
- **Shortcuts (slash commands):** `/swe`, `/stat`, `/dm`, `/ml`, `/etl`, `/cr`, `/guide`, `/doctor`
  each route a request to the matching agent; `/diagnose-fit <run-dir>` runs the active
  fit-diagnostic workflow. These live in `.claude/commands/`.
- Manage/inspect with the `/agents` command.

## Memory

- `memory: project` (the six dev/maintenance specialists) → `.claude/agent-memory/<name>/`.
- `memory: local` (`calibration-doctor`) → `.claude/agent-memory-local/<name>/`, **not**
  version-controlled — appropriate for diagnostics that touch country-specific/unpublished data.
- `run-guide` has memory **off**, so it has no Write/Edit tools. (Bash remains a theoretical write
  vector; its prompt restricts Bash to inspection/execution only — read-only is therefore
  prompt-enforced, not a hard sandbox.)
- Enabling memory auto-grants Write/Edit; the doctor's read-only-source guarantee is likewise
  prompt-enforced. See `.claude/agent-memory/README.md` for the storage policy.

## Verification (repeatable checklist)

1. Restart the session (or create via `/agents`) so file-based agents load; `/agents` shows all
   **eight** with correct tools/model/memory/color.
2. **Frontmatter:** YAML parses; required `name`/`description` present; valid model aliases,
   colors, tool names; `name` values unique (duplicates are silently discarded).
3. **Positive route matrix:** "Why is WIS off by 2×?" → statistician; "Port the FiLM embedding into
   est_suitability" → ml-scientist; "Was mu_j_baseline already rho_deaths-corrected?" →
   disease-modeler; "The WHO weekly counts look wrong after reprocessing" → data-engineer;
   "Review my diff before commit" → reviewer; "A Dask worker deadlocks" → swe;
   "Set control params for a 500-sim run" → run-guide; "My run over-predicts 3×, diagnose it" →
   calibration-doctor.
4. **Negative route matrix:** "The WIS diagnostic plot label overlaps" → swe (not statistician);
   "This run looks biased, why?" → calibration-doctor (not run-guide).
5. **Inheritance:** ask any agent the post-v0.13 deaths-field rule → must answer `reported_deaths`.
6. **Read-only enforcement:** ask `run-guide` and `calibration-doctor` to edit a source file →
   both must refuse and (for the doctor) confirm writes are confined to its memory dir.
