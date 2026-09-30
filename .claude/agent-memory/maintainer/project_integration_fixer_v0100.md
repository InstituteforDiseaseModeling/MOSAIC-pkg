---
name: project-integration-fixer-v0100
description: v0.100.0 deep-review integration pass (integrate/deep-review worktree) - cross-group interaction bugs found, sibling sets, and the data-raw-under-worktree rebuild trap
metadata:
  type: project
---

Integration pass on branch integrate/deep-review (v0.100.0, 2026-09-30). Merging 12 fixer groups produced
interaction bugs that no single group's review could see. Recurring shapes:

- **Duplicated selector drifts when one copy is fixed.** run_MOSAIC's medoid moved to
  `.mosaic_medoid_distances()` (all locations + artifact mask) while `calc_Reff.R`'s
  `.mosaic_reff_select_medoid_member` kept location-1-only. Sibling set for "medoid criterion":
  run_MOSAIC.R medoid block, calc_Reff.R selector, add_reproductive_numbers non-recompute path (reads config_medoid.json).
- **Clear-list vs conditional writers.** `.mosaic_clear_posterior_artifacts()` must list every
  conditionally-written artifact that a reader consumes-if-present (render reads parameter_sensitivity.csv
  if present; add_reproductive_numbers skips if reproductive_numbers.csv exists; promoter archives trajectories_*.csv).
  When a reader switches to "use existing file if present", the clear list must gain that file.
- **Two-stage truncation.** A bound carried onto an untruncated fit of truncated draws re-truncates every
  stage. Fix pattern: fit in the truncated family (`.fit_truncated_lognormal_ci`); lognormal core fields now
  include lower/upper. Test deterministically with analytic quantiles, not MC (MC random-walks ~6%/stage).
- **Attribute markers die in JSON.** psi_star_applied attribute lost through config_medoid.json -> added a field.
- **NEWS assembled from fix+address stages contradicts itself** (earlier design text survives next to the
  reversal). Always verify each NEWS bullet against code before release.

**data-raw builders under a worktree:** they call `library(MOSAIC)` / `data(..., package="MOSAIC")`, which load
the INSTALLED package (was 0.91.14) - stale data objects and missing new internals. To rebuild from the worktree,
load_all and rewrite those lines to `get(x, envir = asNamespace("MOSAIC"))` before eval (done for
make_estimated_parameters_inventory.R). The inventory builder asserts agreement with priors_default, which
catches the stale-install case loudly.

**Why:** these are the failure modes independent review exists for; see [[reviewer_checklist]].
**How to apply:** on any multi-branch integration, grep for duplicated selection/scoring logic and for
"read if exists" readers of artifacts that a cleanup routine is supposed to own.
