---
name: hedgehog-run-infra
description: hedgehog VM run mechanics + two traps in the vm/ helpers (broken ETH template, pull_results.sh does not compress)
metadata:
  type: project
---

The hedgehog Azure VM workflow (local PSOCK or hybrid Coiled/Dask) is documented in
`MOSAIC-pkg/claude/hedgehog_onboarding_for_country_repo.md` (slated to become a `hedgehog-run` skill).
Verified the doc's API claims against source at v0.42.0 — core API is accurate. Two traps live in the
`vm/` helper files themselves:

**Why these matter:** future hedgehog runs/skill work will reuse these helpers and inherit the bugs.

**Shebang root cause (audited 2026-06-16, live VM):** the broken `~/bin/r-mosaic-Rscript` shebang
(2-space indent on EVERY line) is NOT a heredoc bug — the `setup_mosaic_minimal.sh` heredocs write
column-0 correctly (proven by the healthy `r-mosaic-R`, which IS from the heredoc). The Rscript
wrapper was hand-edited/re-pasted later with editor auto-indent; its content also diverges from the
script (missing comments). `r-mosaic-Rx` is a stray byte-identical backup of `r-mosaic-R`, not on
PATH. Permanent fix: regenerate from the good `r-mosaic-R`:
`sed "s|exec R \"\$@\"|exec Rscript \"\$@\"|" ~/bin/r-mosaic-R > ~/bin/r-mosaic-Rscript`. Repo
hardening: add a `head -c2 … grep '^#!'` shebang guard after the heredoc in setup scripts. NOTE
`setup_mosaic.sh` (full installer) never creates the wrappers at all — only the minimal script does.

**How to apply:**
- The `vm/` launcher traps below were ALL FIXED in MOSAIC v0.43.0 (commit 35ddba73). The templates
  are now safe to copy. History (so the lesson survives): `vm/run_mosaic_ETH.R` used to assign
  calibration settings to a never-passed `control` object (built `control_ETH`) so they silently hit
  defaults; `vm/launch_mosaic.R` was titled ETH but ran SOM; `vm/launch_mosaic_individual.R` resume
  check looked for parquets at the dir root but v0.42+ writes them under `2_calibration/`
  (samples.parquet + diagnostics/convergence_results.parquet); both used the unsupported
  `n_simulations='auto'` (use NULL for adaptive, integer for fixed) and deprecated control names.
  Canonical control names: batch_size->batch_size_adaptive, min/max_batches->*_adaptive,
  target_r2->target_r2_adaptive, max_simulations->max_simulations_total, ess_method->ESS_method.
  Also fixed v0.43.0: setup_mosaic.sh now creates the r-mosaic-* wrappers (full installer used not
  to); BOTH setup scripts now shebang-guard each wrapper (head -1 must match ^#!) to fail loudly on
  indented-shebang corruption; mosaic_dask_worker.py per-task pin now sets all 6 thread vars
  (added NUMEXPR_NUM_THREADS). There is now a `hedgehog-run` skill at
  `.claude/skills/hedgehog-run/SKILL.md`.
- `vm/pull_results.sh` does NOT compress — it only scps a pre-existing `<ISO>.tar.gz`. Compression
  happens in `vm/launch_mosaic_individual.R` / `vm/launch_mosaic.R`, NOT in a plain `run_MOSAIC()`
  script. Its remote-dir default is `MOSAIC/output/individual`; a plain `run_MOSAIC(dir_output=
  "~/MOSAIC/output/MOZ")` writes one level up, so `HEDGEHOG_REMOTE_DIR=MOSAIC/output` override is
  required and the dir must be tarred manually first.

**Verified-correct API facts (v0.42.0):** Dask enabled by passing `dask_spec` (omit = local PSOCK);
`control$parallel$enable=FALSE` forces Dask client cores to 1 (`run_MOSAIC.R:1001-1003`); remote
worker count comes ONLY from `dask_spec$n_workers`, never `control$parallel$n_cores`;
`save_simresults` is hard-rejected (`stop()`) on the Dask path (`run_MOSAIC.R:815`); thread-safety env
vars set both R-side (`.mosaic_set_blas_threads(1L)`) and worker-side
(`inst/python/mosaic_dask_worker.py`). Deprecated control names migrate-with-warning via the shim at
`run_MOSAIC_helpers.R:343-355` (canonical: batch_size_adaptive, target_r2_adaptive,
max_simulations_total, min/max_batches_adaptive, ESS_method). See [[control-shim-deprecation]].
