---
name: packaging-env-review-v0999
description: v0.99.9 packaging/env deep review. R CMD check still 0E/0W/2N, but the docs and examples tell users to call attach_mosaic_env, 2 of 5 example regimes produce zero cases, and check_dependencies' <<- writes to globalenv
metadata:
  type: project
---
v0.99.9 packaging-env deep review (2026-09-29, frozen worktree .claude/worktrees/review-main):
- R CMD check (git-archive export, --ignore-vignettes --no-tests): 0E/0W/2N. Same NOTEs as the baseline (self ::: calls, .run_sim_worker globals). pkgdown::check_pkgdown() clean.
- The Running-MOSAIC vignette and vm/launch_mosaic*.R / run_mosaic_ETH.R call attach_mosaic_env() as their first line. That function STOPS when the r-mosaic env is absent, which contradicts "Python is optional". Its own Rd also still says .onAttach calls it, which is false since v0.78.
- The inst/examples/simulate_outbreak_settings.R "sporadic" and "rare" regimes produce 0 cases under the pure-R engine: the psi bump never re-ignites once the seeds die out. The Python-era recipe (v0.37) was never re-validated after the port.
- check_dependencies(): `suitability_working <<- FALSE` in the plain for-loop body (pip branch) assigns into globalenv, so the capability summary lies. This lesson applies generally: `<<-` is only safe inside a closure or a tryCatch handler.
- The roxygen ">=" at the start of a continuation line is read as a markdown block quote and eaten. make_simulation_config.Rd now reads "(numeric= 0". Grep for `^#'\s+>` in R/.
- withr::with_seed is used in calc_model_ensemble, but withr is only in Suggests. It is transitively satisfied via ggplot2 Imports.
- CI still never runs R CMD check. Tracked docs/ is v0.11-0.32 stale (it includes run_LASER.html and CLAUDE.html). vignettes/figures is gitignored AND the include_graphics chunks inherit eval=FALSE, so the article has no figures.
**How to apply:** re-run these exact probes on the next packaging review. See [[reviewer-checklist]].
