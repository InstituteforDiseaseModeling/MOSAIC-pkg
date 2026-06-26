---
name: est-suitability
description: >
  Fit the production environmental-suitability (psi) LSTM and generate psi predictions
  for a specific time frame, plus refresh its upstream inputs (climate + ENSO -> compiled
  training panel). Covers the open-meteo -> process -> compile -> est_suitability chain,
  the date-window args that make an out-of-sample/forecast psi, the side-effect-CSV output
  contract, the forecast-horizon ceiling, and the TF threading / RAM / determinism gotchas.
  Use for "refresh climate", "refit psi", "make a psi forecast for window X", or "bake psi
  into the config". Prerequisite for the forecast-cv skill.
---

# est-suitability — fit / predict the production ψ model

Documents existing exported functions; introduces no new workflow. **Reference, don't transcribe**
versions/constants — cite `?est_suitability`, the inline version-notes, and the spec `.Rmd`.

## Documentation tiers
1. **Roxygen:** `?est_suitability`, `?compile_suitability_data`, `?process_open_meteo_data`,
   `?process_enso_data`.
2. **MOSAIC-docs (sibling repo, read `.Rmd`):** `03-data.Rmd` (provenance), `04-model-description.Rmd`
   "### Modeling environmental suitability" (what ψ is + how the engine consumes it).

## Pipeline (the chain — each step feeds the next; skipping one is a silent no-op)
```
open-meteo-pipeline download (Python)  → raw ERA5 + CMIP6 parquets   (sibling repo, read-only edits)
process_open_meteo_data(PATHS)         → processed/climate/{daily,weekly}/{ISO}.parquet
process_enso_data(PATHS, source="nmme")→ ENSO weekly (co-limits the horizon)
compile_suitability_data(PATHS, ...)   → cholera_country_weekly_suitability_data.csv  (training panel)
est_suitability(PATHS, ...)            → model/input/pred_psi_suitability_day.csv      (the artifact)
```
- **`est_suitability()` reads the compiled CSV, NOT the climate parquets.** Editing climate without
  re-running `compile_suitability_data()` does nothing to ψ.
- `compile_suitability_data()`'s `cutoff` is a **numeric outbreak-definition threshold (NOT a date)**,
  used only when `use_epidemic_peaks = FALSE`. Production uses `use_epidemic_peaks = TRUE` with
  `cutoff = NULL` (the peaks branch ignores it); passing `cutoff = NULL` *without*
  `use_epidemic_peaks = TRUE` errors. Also relevant: `forecast_mode` / `forecast_horizon` /
  `date_start` / `include_lags`. See `?compile_suitability_data`.
- Two namespaces: the Python pipeline's own `process`/`compile` outputs are **not** consumed by
  MOSAIC; R's `process_open_meteo_data()` reads the raw parquets directly.
- Training-panel sources are **WHO + JHU + SUPP** (AI rows exist but anchors are set on non-AI rows).

## Refresh the inputs (when climate/ENSO are stale)
```bash
cd ~/MOSAIC/open-meteo-pipeline && git pull && source .venv/bin/activate
python -m openmeteo download --api-type both    # ERA5 (historical) + CMIP6 (projection)
```
```r
PATHS <- MOSAIC::get_paths()
MOSAIC::process_open_meteo_data(PATHS, force = TRUE)   # force=TRUE re-processes after a raw refresh
                                                       # (climate_model arg must be among models downloaded)
MOSAIC::process_enso_data(PATHS, source = "nmme")
MOSAIC::compile_suitability_data(PATHS, use_epidemic_peaks = TRUE, cutoff = NULL,
                                 forecast_mode = TRUE, forecast_horizon = 9, include_lags = TRUE)
```
Note: `force` belongs to `process_open_meteo_data()` — **`est_suitability()` has no `force` arg.**
Watch for stale legacy `processed/climate/climate_data_{ISO}_weekly.parquet` vs the live
`weekly/{ISO}.parquet` (consumers use the `weekly/` subdir).

## Fit / predict ψ
```r
MOSAIC::est_suitability(
  PATHS,
  architecture = "lstm_v2_hierarchical_film",
  response_var = "target_D_rate_per_country_floored",   # production default; see ?est_suitability
  bias_correct = TRUE,                                   # per-country affine post-correction
  arch_control = list(n_seeds = 5L, region_map = "snf_k5", parallel_seeds = 1L),
  fit_date_start = NULL, fit_date_stop = NULL,           # training window
  pred_date_start = NULL, pred_date_stop = NULL)         # prediction window
```

### Predicting for a specific time frame (the date args)
- `fit_date_stop` is the **leakage-critical cutoff**: data after it is unseen, so set it at the data
  edge (or a chosen cutoff) to produce an **out-of-sample / forecast ψ**.
- `pred_date_start` / `pred_date_stop` is the prediction window and **may extend past `fit_date_stop`**
  into the forecast horizon.
- Patterns: (a) full historical refresh — leave dates NULL; (b) OOS forecast ψ — `fit_date_stop` at
  the data edge, `pred_date_stop` into the horizon; (c) a single cutoff matching one `forecast-cv`
  fold.
- **Horizon ceiling:** `pred_date_stop` ≤ `min(climate horizon, ENSO horizon)`. The climate ceiling
  is CMIP6 `DEFAULT_END_YEAR`, computed dynamically as `date.today().year + 1` in
  `open-meteo-pipeline/src/openmeteo/config.py` (~12-month lead — encode the rule, not a year);
  ENSO/NMME gives ~8 months. In the production lstm_v2 path, usable ψ is bounded by the **ENSO-OK
  auto-cutoff** (`fit_date_stop`/`pred_date_stop` auto-detected from the last ENSO-covered date) plus
  the `pred_date_stop` filter — the daily grid is `na.locf`-filled, NOT trailing-dropped. (The
  `.drop_filled_prediction_tail` trailing-drop is **legacy-path only**, not the default architecture.)

### `response_var` ↔ `psi_star_b` coupling (load-bearing)
The `_per_country` response is **self-normalized per country**, so ψ levels are NOT comparable across
countries — workable only because the engine's two ψ channels absorb it differently
(`beta_env = beta_j0_env·(ψ*/ψ̄*)` self-normalizes; the δ reservoir-survival channel reads ψ\* on its
absolute level, which is why ψ\*'s `b` is recentered). **Swapping to a globally-normalized
`response_var` without re-centering `psi_star_b` pushes most countries to the survival floor.**
`bias_correct` (affine post-correction) is distinct from the Bayesian ψ\*/transmission calibration in
`run_MOSAIC()` — do not conflate. (`bias_correct` is the renamed `calibrate` arg; the old name still
works via `...` with a deprecation message.) See `04-model-description.Rmd` suitability section.

## Output contract
`est_suitability()` returns an **invisible list** (`daily`/`weekly`/`fit_info`/`config`) — **do not
rely on the return value.** The consumed artifact is the side-effect file
`model/input/pred_psi_suitability_day.csv` (canonical **`psi`** column — sourced from
`pred_bias_corrected` when `bias_correct=TRUE`, else `pred_smooth`). Also written: weekly CSV,
`psi_suitability_config.json`, weight files.
**Bake into the package default** (optional) via `data-raw/make_config_default.R` (reads the `psi`
column → `psi_jt`).

## Reproducibility, threading & where to run
- **Not bitwise-reproducible run-to-run** — `recurrent_dropout` defeats `tf$random$set_seed` even at
  threads=1. Rely on the `n_seeds` ensemble (logit-median pool); validate statistically, never expect
  parity.
- **`parallel_seeds = 1L` (serial) is the SAFE default.** `parallel_seeds > 1` opens one TF session
  per PSOCK worker at **~6 GB/worker** (`.PSI_PER_SEED_WORKER_GB`); the guard *warns* but does not cap,
  and the worker count is clamped to `min(parallel_seeds, n_seeds, cores−2)`. Raise cautiously — OOM
  risk scales with the count (≈ `parallel_seeds × 6 GB`; ≤4 on a 32 GB box, production used 10 on a
  448 GB host).
- **TF threads are NOT pinned in the suitability code**, and `run_MOSAIC()`'s thread block does not
  touch TF intra/inter-op threads. When raising `parallel_seeds` on a many-core VM, cap TF threads
  per worker (`tf$config$threading$set_intra_op_parallelism_threads`, or
  `TF_NUM_INTRAOP_THREADS` / `OMP_NUM_THREADS`) so total ≈ cores — **set at session start, before the
  first TF op**, or it silently no-ops.
- **Where to run:** a full `n_seeds`≥5 fit is large-RAM → use **`hedgehog-run`** / **`dugong-run`**.
  A standalone TF-only ψ fit may run *without* dugong's `r-mosaic-Rscript` wrapper (it skips the
  laser/pyexpat path); a fit run inside `forecast-cv` (which also runs laser) needs the wrapper.

## Disambiguation (avoid the grep trap — three distinct things)
- **`run_rolling_cv()`** (`R/run_rolling_cv.R`) — the forecast-CV harness (see the `forecast-cv`
  skill). NOT this skill.
- **`run_rolling_cv_suitability.R`** — the per-fit lstm_v2 orchestrator `est_suitability()` dispatches
  to.
- **`rolling_cv_suitability.R`** — the CV helper *inside* that orchestrator (picks `best_epoch`); part
  of ONE production fit, not a forecast backtest.
