---
name: relic-audit-v084
description: v0.84 relic/orphan audit results — where the Python/Dask/LASER era still lies about current behaviour, and the specific orphan-scan false positives to expect
metadata:
  type: project
---

Full-package relic + orphan audit, 2026-09-16 (v0.84/0.85). Findings in
`claude/review_v084/findings/RELIC.md`; scan artifacts in
`claude/review_v084/scratch/RELIC/` (`orphan_table.tsv` = per-function caller census by zone).

**Orphan-scan false positives — expect these, do not re-report:**
- `sim_phase_*` (10 fns) look orphaned because `sim_engine.R:161-171` binds them as **function
  objects without parens** into `.SIM_PHASE_FUNCTIONS`. Same for
  `.mosaic_traj_render_worker` (`render_MOSAIC_figures.R:834`, `traj_worker <- .mosaic_...`).
  A `name\s*\(` regex misses every one. Grep bare names too.
- `make_simulation_config()` has **zero internal callers by design** — it is the public config
  constructor; internal validation lives in `sim_params()` (engine boundary) and
  `validate_sampled_config()` (`sample_parameters.R:319`). Not an orphan.
- The whole public `process_*`/`plot_*`/`est_*`/`get_*`/`download_*` surface has no R/ caller —
  those are user entry points called from `data-raw/` or interactively.

**Confirmed orphans / dead-by-construction (as of v0.85):**
- `.mosaic_parse_sim_ids()` (`run_MOSAIC_infrastructure.R:334`) — orphaned by its own fix
  commit `a314fcd52` (v0.80.1), zero refs anywhere. Lesson #14(iii): re-run the orphan grep
  *after* a path removal, not before.
- `make_forecast_cv_table()` + `plot_forecast_cv_grid()` — new, tested, `man`'d, **never
  called**. `inst/examples/forecast_cv_experiment.R` wires 2 of the 4 reporting fns
  (`plot_rolling_cv`, `plot_forecast_cv_skill`) and neither of these.
- `capture_in_gather` (`calc_model_ensemble.R:65,81-88,129,134,597`) — always FALSE, kept
  deliberately with a comment saying so. Dask relic.
- `requireNamespace("reticulate")` guards (3 sites) are dead: **reticulate is still in
  `Imports:`** even though v0.78.0 changed `SystemRequirements` to call Python optional.
  Dependency move needs user approval.

**Relics that LIE about current behaviour (vs legitimate provenance):**
`laser-cholera` / `laser_cholera` / `laser.cholera` / `laser-core` are real external package
names — per lesson #16(iii) most of the 46 `R/` hits are correct historical citations. The three
that are wrong are **present-tense consumption claims**: `priors_default.R:83,100`
("Consumed by laser-cholera v0.13+ at infectious.py:88-92") and
`make_simulation_config.R:73` ("Passed to laser-cholera v0.12+ which draws"). The
correctly-worded sibling to copy is `make_simulation_config.R:107` ("…originally
laser-cholera#49; the pure-R engine implements the same rule").
Others: numba/reticulate thread rationale in `make_mosaic_cluster.R:29,114`,
`optimize_ensemble_subset.R:29` and `zzz.R:83`; `vm/DUGONG.md:106` ("omit `dask_spec`") and
`vm/make_wrappers.sh:19,83` (libssl preload "needed only by the Coiled path").

**Biggest single relic: the ROOT `/Users/johngiles/MOSAIC/CLAUDE.md`** still documents the
Python engine as current (L24, L99, L106, L152, **L155-160 a whole "LASER Model Integration"
section with `reticulate::import("laser_cholera")` code**, L190), and L76 documents a launch
command `azure/azure_run_laser.R` that has never been tracked. It contradicts
`MOSAIC-pkg/CLAUDE.md` and is loaded first in every session → hand to `ai-architect`.

**Provenance hole worth remembering:** `tests/testthat/fixtures/ORACLE.md` points the
regeneration recipe for all seven engine parity fixtures at `claude/oracle/` — **git-ignored
(`.gitignore:12`) and absent from disk**, including `verify_draw_sites.py` that CLAUDE.md
lesson #15 cites as the trustworthy re-derivation. `sim_convert_tier_a_dump()` has zero refs
anywhere and ORACLE.md shows `—` for the Tier-A recipe.

**Two test-suite smells to reuse:**
- 11 `source("../../R/<file>.R")` calls in 9 test files are **provably inert** — a testthat
  file's env chain is `test env → asNamespace(MOSAIC) → … → globalenv`, so the namespace copy
  always wins. Probed: `identical(get_location_priors, MOSAIC::get_location_priors) == TRUE`
  after sourcing. They only pollute globalenv and run top-level code twice.
- The 58 baseline warnings contain **no defect signal**; 41 of 58 are expected warnings the
  tests emit instead of asserting (30 ggplot2 `Removed N rows` from the boundary mask, 11
  loess-singularity from a 10-row fixture in the deprecated `plot_model_parameters()`). The
  one worth escalating is `inflate_priors: uniform '…' yields new_min<0. Skipping.` ×6 — a real
  limitation the suite encodes in a test *name* ("all **non-uniform** families") rather than an
  assertion.

**Hygiene:** `calc_model_posterior_quantiles(output_dir = "./results")` defaults to a hardcoded
relative path, `dir.create`s it and writes into the caller's CWD — this is what makes a stray
`results/posterior_quantiles.csv` appear in the package root. `results/` is in neither
`.gitignore` nor `.Rbuildignore`.
