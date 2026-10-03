---
name: pr133-artifact-verification
description: How to independently re-derive every MOSAIC default data artifact (panel, psi provenance, seasonal, peaks, priors, config) in <15 min, plus the ENSO-variant revert hazard found on PR #133 (v0.101.0, 2026-10-01)
metadata:
  type: project
---

PR #133 (v0.101.0, config v6.1 / priors v17.1, psi C3) verified 2026-10-01: every artifact rebuilt
byte-identical from MOSAIC-data HEAD 2ee3725 + PR code. Recipe worth reusing on any data-rebuild PR:
- Panel: `compile_suitability_data(PATHS, cutoff=NULL, use_epidemic_peaks=TRUE, date_start="2000-01-01",
  date_stop=NULL, forecast_mode=TRUE, forecast_horizon=9, include_lags=TRUE)` with DATA_CHOLERA_WEEKLY
  pointed at a scratch dir holding a symlink to the combined weekly file, DOCS_FIGURES scratch. ~2.6 min.
  Only writes the panel CSV + GAM diag figs (safe). Only `enso_weekly.csv` is read from DATA_ENSO.
- Seasonal: `est_seasonal_dynamics(2010-09-01..2025-09-01, min_obs=10, ward.D2, k=4, WHO/JHU/SUPP,
  envelope_floor=0.1)` with MODEL_INPUT/DOCS_TABLES redirected; deterministic, byte-identical. Window
  ends 2025-09, so ERA5 tail updates cannot move it.
- Peaks: `est_epidemic_peaks(PATHS)` with MODEL_INPUT redirected; config keeps rows in window AND in
  location_name (SDN/COM peaks are dropped, so "in window" 70 vs config 64 is expected).
- Priors/config builders use the INSTALLED pkg (`library(MOSAIC)`, `MOSAIC::config_default`): install PR
  head to a scratch lib (`R CMD INSTALL --no-test-load -l <lib> .`) and source the builder through a
  wrapper that sets `.libPaths()` (an `R_LIBS=` command prefix is blocked in worktree-isolated agents).
  Builders write into getwd()'s data/ + inst/extdata -- run from own worktree, compare md5, confirm
  `git status` clean.
- rda vs json: decoded fields equal at tolerance 0; `identical()` is FALSE by construction (JSON reads
  whole numbers as integer; rda keeps dimnames on the 3 post-validation matrices). Not a defect.

**ENSO hazard (open as of 2026-10-01):** psi C3 was trained on a Nino4 NMME gap-fill VARIANT
(`MOSAIC-data/processed/psi_provenance/v0.101.0_C3/enso_C/enso_weekly.csv`, md5 7805ecfe...), not on
`processed/ENSO/enso_weekly.csv` (51dbed25...). The in-tree gitignored panel in MOSAIC-data is the
canonical-ENSO twin (d8a28969...), so any default `est_suitability()`/prefit_rolling_cv_psi/
update_mosaic_data refit silently reverts to the dip. Reproducible from the committed bundle
(build_nino4_C.R + nmme_gapfill_anchor.py both re-ran exactly), but no code guard.
**How to apply:** on any psi refit or forecast-CV PR, check which enso_weekly the panel used
(psi manifest `provenance$source_csv_md5` vs the panel md5s above).

Also: get_paths() DATA_ENSO is lower-case `processed/enso` while the dir is `processed/ENSO` --
resolves only on case-insensitive macOS; fails loudly on Linux (dugong). See [[reviewer-checklist]].
