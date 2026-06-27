---
name: forecast-cv
description: >
  Run systematic out-of-sample forecast cross-validation of MOSAIC via run_rolling_cv() —
  an expanding-window rolling-origin backtest that, per cutoff, re-fits psi, injects it,
  calibrates on data up to the cutoff, and projects forward. Covers the correct leakage-strict
  args (embargo, horizons, cutoffs), the raw-psi vs post-psi* attenuation check, per-cutoff
  ESS/convergence gating, and post-hoc scoring (WIS / R2 / coverage). Composes the run-mosaic
  and est-suitability skills. Use for "rolling-origin CV", "OOS forecast skill", "backtest the
  model across cutoffs".
---

# forecast-cv — systematic rolling-origin forecast validation

## SOURCE OF TRUTH — the experiment script
The reproducible experiment is defined in **`inst/examples/forecast_cv_experiment.R`** (git-tracked).
Its `SPEC` block is the single place that defines the experiment: **units (countries / coupled
vectors), start date, explicit cutoff dates, horizons, embargo, psi `n_seeds`**. Edit the SPEC, not
the engine. `FORECAST_CV_DRYRUN=1` (default) validates the SPEC + the D5 placement and prints the
unit×cutoff matrix with NO compute; `FORECAST_CV_DRYRUN=0` executes (heavy → hedgehog/dugong).
The locked design decisions D1–D5 live in the script header and in **`claude/plan_forecast_cv/PLAN.md`**
(the red-teamed dev plan + pending-upgrade ledger). **The covariate decision (D4): OOS psi uses
realized climate/ENSO, so results are HINDCAST / conditional MODEL skill — never call them forecast
skill.** Today the script drives existing `run_rolling_cv()`+`evaluate_rolling_cv()` one
(unit×cutoff) at a time; it collapses to one `run_forecast_cv_suite()` call once PLAN Phase 1–2 land.

Documents existing, tested machinery — invents nothing. Backbone: the exported `run_rolling_cv()`
(`R/run_rolling_cv.R`, with `tests/testthat/test-run_rolling_cv.R`) + `compile_rolling_cv_predictions()`,
`evaluate_rolling_cv()`, `plot_rolling_cv()`. Primary doc = **`?run_rolling_cv`** (roxygen is current and
thorough — read it, don't transcribe arg defaults here). Prototype invocation:
`claude/run_final_validation.R` (3-cutoff MOZ backtest; outputs in `claude/rolling_cv_lstm_v2_converged/`).
This skill canonicalizes the **corrected** invocation, not that script verbatim (it carries superseded
choices — deprecated `n_splits`, `embargo_weeks=1L`, pre-`config_medoid` assumptions).

## Mechanism (what run_rolling_cv does, per cutoff T)
Expanding-window, **fixed anchor** at `config$date_start`. For each monthly cutoff T:
1. **Re-fit psi** with `fit_date_stop = T` — the harness OWNS the leakage-critical date and overrides
   any date passed in `est_suitability_spec` (with a warning).
2. **Inject** that psi into the config as `psi_jt`.
3. **Mask** observed cases/deaths after T to `NA`; **calibrate** `run_MOSAIC()` scoring only weeks ≤ T.
4. **Project** forward; an `embargo_weeks` gap separates T from the OOS start (that week is neither
   trained nor labeled OOS).

One `run_MOSAIC` calibration **per cutoff** (joint over all locations if `iso` is a vector = coupled
metapop). It is a **fit-and-forecast engine ONLY — it computes NO metrics**; scoring is post-hoc.

## Disambiguation (3-way — avoid the grep trap)
- **`run_rolling_cv()`** (`R/run_rolling_cv.R`) — THIS forecast harness.
- **`run_rolling_cv_suitability.R`** — the per-fit lstm_v2 orchestrator `est_suitability()` dispatches to.
- **`rolling_cv_suitability.R`** — the CV helper inside that orchestrator (picks `best_epoch`).
Only the first is the backtest; the other two are internals of a single psi fit.

## Key args (see `?run_rolling_cv` for the full set / defaults)
- `iso` — one ISO3 or a vector (coupled metapop).
- `n_cutoffs` / `step_months` / `latest_cutoff` — the cutoff grid.
- `horizons_months` (default `c(1,3,5)`) — projection length + OOS labeling; the largest bounds `latest_cutoff`.
- **`embargo_weeks` — set `4L` for leakage-strict CV (the harness DEFAULT is `1L`).** Mechanism (not a
  magic number): reporting lag ~5 d + serial-interval autocorrelation ⇒ a ≥2–3 wk leakage zone; 4 wk is
  conservative. NB the embargo only buffers the **OOS-labeling boundary** — ψ-fit leakage is handled
  separately (the harness sets `fit_date_stop=T`; see est-suitability). Don't "fix" the 1L default — 4L is
  the strict-CV choice.
- `est_suitability_spec` — **modeling** knobs only (architecture, `response_var`, `arch_control`); date keys
  are ignored/harness-owned. Keep `arch_control$parallel_seeds = 1L` (see est-suitability for the RAM math).
- `control = NULL` → an experiment-grade cheap default (fixed `n_simulations`, plots off).
- `models = c("ensemble","ensemble_opt","medoid")`, `central_method = "median"`, `optimize_subset = TRUE`,
  `n_reps_best_medoid = 50` (medoid reruns — see where-to-run). `dask_spec` (optional) sends the per-cutoff
  *calibration* to Dask/Coiled (the medoid reruns stay local); local PSOCK is usually preferred (Coiled
  dies on >50 min jobs).
- **Two staleness/leakage subtleties:** (i) if `config$date_stop` runs past the climate/ENSO horizon, the
  per-cutoff ψ tail is `na.locf` flat-filled in the scored OOS segment (see est-suitability horizon
  ceiling); (ii) ICs are seeded at `ic_t0 = max(date_start, 2023-02-01)`, so cutoffs before 2023 seed
  initial conditions from 2023 data regardless of the cutoff — a leakage-adjacent subtlety.

## raw-ψ vs post-ψ* — the "did ψ help?" check
ψ enters as fixed `psi_jt` but is re-transformed **per simulation, per location** by the sampled ψ\* params.
`psi_star_a` is the **primary** shape lever (`a→0` ⇒ ψ\*→σ(b), a constant), but `psi_star_z` (causal-EWMA
smoothing) and `psi_star_k` (day offset) **also degrade shape/phasing** — check all three, per country.
`psi_star_b` shifts the ψ *level*, which sets baseline reservoir decay via the engine's **nonlinear Beta-CDF
ψ→δ map** (so the `a↔shape / b↔δ` split is approximate, not a clean factorization). A trained-good ψ can be
silently muted. **The evidence is the `psi_star_*` posterior** (esp. per-country `psi_star_a`) + the ensemble
`psi_jt` variance — the `config_medoid.json` `psi_jt` is just *one* draw (illustrative only: diff it against
the raw input `psi_jt` in `1_inputs/config.json`; there is no `config_best.json`). See run-mosaic §2 /
est-suitability for full ψ\* semantics.

## Outputs & post-hoc scoring
Under `dir_output`: `manifest.json` (settings + per-run status), `predictions.parquet` (one row per
cutoff×location×date×metric: `segment`=IS/embargo/OOS, held-out `observed`, `pred_central`+`pred_mean`/
`pred_median`+CI cols), and `runs/cutoff_<T>/` (native `run_MOSAIC` dirs; rebuildable via
`compile_rolling_cv_predictions()`). Score post-hoc with `evaluate_rolling_cv()` / `plot_rolling_cv()`
(**separate exported fns** — not inside `run_rolling_cv`; see `?evaluate_rolling_cv`).
- **Gate on per-cutoff ESS + coverage FIRST.** The cheap CV control is a *short, loosely-targeted* adaptive
  loop (`n_iterations=3`, `n_simulations=2000`, `ESS_param=100`) — weights can be under-converged, so a
  "win" may be a lucky draw. Read per-cutoff ESS before trusting a score.
- **Primary verdict = WIS-skill-vs-baseline + bias_ratio.** R²-corr is scale/offset-invariant — blind to
  the magnitude/level that `psi_star_b` controls — so it is **diagnostic-only**, never the ψ verdict. (Eval
  WIS uses the empirical 50/95 ensemble quantiles, so it is not directly comparable to the likelihood's
  parametric-NB WIS.)
- **CI caveats:** the cell bootstrap is anti-conservative — expanding windows **overlap** (adjacent cutoffs
  re-score the same outbreak) and points autocorrelate, so effective-n ≪ n; prefer a block bootstrap by
  cutoff. Cumulative horizons are nested (compounds this). Coverage is vs *predictive* (param+stochastic),
  not calibrated, intervals — mild under-dispersion is expected.
- **Multiplicity:** cutoffs × horizons × 3 models is a forking-paths grid — pre-register the primary
  (model, metric, horizon) cell; treat the rest as exploratory.
- Honest framing: ψ is a **weak signal** — better shape/phasing than climatology at 3–5 mo, not
  forecast-grade; wins show in WIS/bias after seed-ensemble pooling (the logit-median ψ `n_seeds` in the fit)
  + calibration reps, not single-run R². `central_method` `"median"` (default) vs `"mean"` changes the deaths
  central tendency (mean unmasks implied CFR) — relevant when scoring deaths. Validate across ≥3 cutoffs.

## Where to run
A multi-cutoff coupled CV (each cutoff = one lstm_v2 fit + one full `run_MOSAIC` calibration + ~50 local
LASER medoid reruns) is a **hedgehog/dugong** job. The `n_reps_best_medoid` reruns execute **locally in the
calling R process, not on Dask** — size RAM accordingly. Because it runs laser, **forecast-cv REQUIRES the
`r-mosaic-Rscript` wrapper on dugong** (a standalone TF psi fit would not). Cross-link `hedgehog-run` /
`dugong-run`; apply the TF-thread caps from est-suitability since each cutoff's psi fit has the same
oversubscription risk.

**Full-parallel pattern (two-phase, the contention-free way).** est_suitability sets NO tf.config thread
cap, so a TF fit auto-sizes its intra-op pool to the whole box — launching P per-cell processes that each
hit their psi fit at once oversubscribes (OMP/TF_* env caps are best-effort only). **The right axis is NOT
"parallelize across cutoffs" — it's "serialize the few psi fits, then parallelize the whole calibration
grid."** psi is fit GLOBALLY per cutoff (one fit covers ALL units jointly), so there are only `n_cutoffs`
fits, not `n_units × n_cutoffs`. The clean path: **(1) pre-fit psi ONCE per cutoff SERIALLY, each fit using
the whole box (a single fit still spreads its `n_seeds` across cores via `parallel_seeds`, so the box isn't
idle); freeze each to a cache (daily-psi CSV).** Then **(2) run ALL calibrations FULLY PARALLEL across BOTH
units AND cutoffs from the frozen cache — the parallel phase has NO TensorFlow at all**, so per-process
`control$parallel$n_cores` + the BLAS/numba=1 pins fully control core use. This is strictly better than
parallelizing across cutoffs only (it frees the unit dimension too) and avoids the global-psi-CSV write race
(every cell would otherwise refit/overwrite the same `pred_psi_suitability_day.csv`). This is exactly
PLAN.md Phase 1 (`prefit_rolling_cv_psi()` + `psi_cache`); build it before the big pilot. Interim (no cache yet): `forecast_cv_experiment.R` single-cell env launch
(`FORECAST_CV_UNIT/CUTOFF/CORES`, `parallel_seeds=1`, `plots=FALSE`) works but risks psi-phase
oversubscription if all cells start together — stagger launches or pre-fit psi first.

## Composition / hand-offs
- **Prerequisite:** fresh climate→ψ inputs — see **`est-suitability`** (the harness re-fits psi per cutoff,
  but the compiled training panel + climate/ENSO must be current first).
- **Config/prior/control assembly** (base_config, priors, control) — see **`run-mosaic`**.
- A specific cutoff fitting poorly / bias / convergence interpretation → **`diagnose-fit`** / `calibration-doctor`.
- Source bug → owning dev agent (`swe` for harness plumbing, `ml-scientist` for ψ, `statistician` for scoring).
