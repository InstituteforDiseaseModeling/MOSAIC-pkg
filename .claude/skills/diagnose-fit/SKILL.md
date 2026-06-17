---
name: diagnose-fit
description: >
  Actively diagnose why a completed MOSAIC calibration fits poorly and produce
  calibration-actionable parameter/prior recommendations. Use after a calibration run
  returns poor fit (e.g. case/death bias, low R², timing or shape errors) and you want
  to know WHAT to change before paying for another full calibration. Drives fast
  deterministic single-LASER experiments (MOSAIC::run_fit_sandbox) on the run's medoid
  config and scores them with MOSAIC::calc_fit_diagnostics. Not a passive metrics summary.
---

# diagnose-fit — active model-fit diagnosis

A full MOSAIC calibration takes hours and real compute. This skill compresses the
hypothesis-testing loop to minutes: it manipulates the **deterministic** model to build
mechanistic understanding of *why* the fit is off, then synthesizes a concrete brief of
parameter targets and prior changes for the next calibration. You drive it actively —
decide what to vary next based on what you just observed, like a modeler at a workbench.

> **Engine-version note:** the parameter→behavior facts below (e.g. `beta_j0_tot` is a dead
> parameter, the `alpha_2 < 0.4` bifurcation) are valid for the current laser-cholera engine.
> An engine upgrade can flip a field's semantics (it has before — see CLAUDE.md Lesson #12);
> re-verify against the engine after any laser-cholera bump.

## The two tools

Both are exported MOSAIC functions; call them from R / `Rscript`.

**`run_fit_sandbox(config, params = list(), seed = 42L, locations = NULL, full_metrics = TRUE, outdir = NULL, run_label = "...")`**
Runs ONE deterministic `run_LASER()` from a calibration config with point-value overrides
(~1-2 s), aggregates predicted vs observed across `locations`, and returns
`$predictions`, `$metrics` (incl. `fit_diagnostics` + merged `scorecard`), and
`$params_applied`. `config` may be a list or a path to a config JSON (use the run's
medoid, e.g. `.../2_calibration/best_model/config_medoid.json`).

**`calc_fit_diagnostics(observed, predicted, dates, epidemic_threshold = NULL)`**
The metrics engine (called for you by the sandbox; call directly to re-score series). Returns
`bias` (total, by_year, endemic, epidemic), `r2` (corr, sse, by_year), `shape`
(`peak_timing_error_by_year` — positive = model peaks later; `peak_magnitude_ratio_by_year`;
`onset_slope_ratio`; `seasonal_corr`; `shape_corr` — area-normalised, bias-free), `variance`
(`cv_ratio`, `residual_autocorr_lag7`), and a per-series `scorecard`.

Scorecard cut points: **bias** PASS <1.2× / WARN <2.0× / FAIL (symmetric, so 0.5× = 2×).
**peak_timing** PASS <7d / WARN <21d. **peak_shape** from `shape_corr` (bias-decoupled).
**variance** = worst of cv_ratio and |residual autocorr| (>0.3 ≈ reporting-delay mismatch).

Example:
```r
Rscript -e '
  library(MOSAIC); MOSAIC::use_mosaic_env()
  cfg <- "output/calibration/<run>/2_calibration/best_model/config_medoid.json"
  res <- MOSAIC::run_fit_sandbox(cfg, params = list(beta_j0_hum = 2.4e-6, beta_j0_env = 1.9e-6),
                                 outdir = "claude/diagnose_fit", run_label = "beta_x0.75")
  print(res$metrics$scorecard); print(res$metrics$fit_diagnostics$cases$bias)
'
```

## Investigation protocol (adapt based on findings)

1. **Baseline + plausibility.** Run the medoid with no overrides, `full_metrics = TRUE`. Read the
   scorecard; identify which dimensions are WARN/FAIL. **Then, regardless of the scorecard,** pull
   the medoid's point values and the posterior marginals (`2_calibration/samples.parquet`,
   `posterior/`) and check them against each parameter's prior support (`1_inputs/priors.json`) and
   its biologically plausible range. Flag any parameter at/over a bound, piling against a bound,
   drifting outside the plausible range, or taking an impossible value. A good fit does **not**
   clear this — implausible values under a good R² mean overfitting, non-identifiability, or
   compensation between correlated parameters (cf. `mu_j_baseline`↔`rho_deaths`). When you later
   recommend parameter targets, keep them inside the plausible range too — never buy fit with an
   implausible value. Carry flagged parameters into the brief.
2. **Prioritise bias → shape → variance.** Bias is most tractable and often the root cause;
   shape/variance diagnostics are confounded when total scale is wrong. Fix/understand bias first.
3. **Bias.** If cases bias WARN/FAIL: sweep `beta_j0_hum` and `beta_j0_env` ×{0.25,0.5,0.75,1,1.5,2}.
   If beta fixes scale but reporting looks off, check `sigma`, `rho`. Read `bias$by_year` — uniform
   or concentrated in specific years? If deaths bias WARN/FAIL with cases OK: `mu_j_baseline`, then
   `rho_deaths`; check `epidemic_threshold` if deaths cluster in an epidemic year.
4. **Shape** (only once bias is PASS or understood). Peak timing off → `psi_star_k` (±30d in 5d
   steps), Fourier `a_1_j/b_1_j` (±20%). Peak too sharp/broad → `iota` (higher = faster rise),
   `gamma_1` (lower = broader). Poor `seasonal_corr` → Fourier amplitude `a_2_j/b_2_j`, `psi_star_a`.
5. **Variance.** Too smooth (`cv_ratio` < 0.8) → `alpha_2`, `sigma`. Weekly residual autocorrelation
   (`residual_autocorr_lag7` > 0.3) → reporting-delay mismatch, try `delta_reporting_cases` ∈ {0,3,7}.
6. **Tradeoffs.** Run 3-5 joint combinations from the single-parameter findings. Does fixing bias
   break shape? Does fixing timing inflate bias? Document tradeoffs explicitly.
7. **Synthesise** the brief (below).

## Parameter → behavior map (universal)

What changing each parameter does to **bias / shape / variance**. (Universal across countries; the
data-driven context — observed CFR, epidemic history, well-fitting parameter ranges — is
country-specific and lives in the calibration-doctor's local memory, not here.)

**Transmission scale** — `beta_j0_hum` (human transmission): primary cases-scale lever, minimal
shape. `beta_j0_env` (environmental): primary cases-scale lever, shifts env/human balance, can
affect onset. `p_beta` (human vs env fraction): secondary; higher → more self-limiting dynamics.
`beta_j0_tot`: **DEAD PARAMETER — LASER reads `beta_j0_hum`/`beta_j0_env` directly and ignores it.**

**Timing / shape** — `psi_star_k` (suitability time offset, days): peak timing, negative shifts
earlier. `a_1_j,b_1_j` (Fourier phase): seasonal phase. `a_2_j,b_2_j` (Fourier amplitude): seasonal
contrast + variance. `epidemic_threshold`: epidemic onset / which years are epidemic; affects deaths
shape. `gamma_1,gamma_2` (recovery): epidemic duration (lower = longer). `iota` (incubation): onset
sharpness (higher = faster rise).

**Observation model (cases)** — `sigma` (symptomatic fraction), `rho` (care-seeking → reported):
linear cases-scale levers, uniform scaling. `chi_endemic,chi_epidemic` (PPV): cases scale with a
differential endemic/epidemic effect.

**Mortality** — `mu_j_baseline` (baseline IFR): deaths only. `mu_j_epidemic_factor` (epidemic IFR
multiplier): deaths during epidemics + deaths shape. `rho_deaths` (death detection): deaths only, linear.

**Initial conditions** — `prop_S_initial` (susceptible): early-period bias + early dynamics (high S =
faster early epidemic). `prop_R_initial` (immune): early-period bias, dampens early dynamics.

**Environmental shedding** — `zeta_1` (shedding per symptomatic): via env transmission, epidemic
explosive potential. `psi_star_b` (logit offset on suitability): cases (env), uniform shift.
`psi_star_a` (gain on suitability): cases (env), amplifies/damps suitability peaks + variance.

**Mixing** — `alpha_1` (susceptible mixing fraction): minor; affects reporting/death ratio.
`alpha_2` (frequency-dependent fraction): minor if >0.4, **catastrophic bifurcation below ~0.4**
(under-prediction collapse) — keep ≥0.4.

## Design principles

- **Deterministic first.** Single LASER runs are the unit of investigation; stochastic averaging is
  for calibration, not diagnosis.
- **Hypothesize before sweeping.** State the hypothesis before each experiment — no brute force.
- **Follow leads.** Investigate surprises rather than marching through the plan; the best findings
  come from unexpected results.
- **Separate concerns.** Bias, shape, variance are independent. Diagnose each before synthesising.
- **Calibration-actionable output.** Every finding maps to a specific parameter target or prior
  change. "The model is too sensitive to 2022" is not actionable; "lower `beta_j0_hum` to 2.4e-6" is.
- **Don't claim a code bug from a fit symptom.** A poor fit is a parameter/prior story until a
  minimal reproduction or file-level evidence shows otherwise.

## Output: the brief

```
DIAGNOSE-FIT BRIEF
==================
Run:    <path>
Model:  MOSAIC <ver> | laser.cholera <ver> | priors v<ver> | config v<ver>

SCORECARD
  Bias (cases):  {PASS|WARN|FAIL} {x.xx}x      Bias (deaths): {...} {x.xx}x
  Peak timing:   {...} mean {n}d               Peak shape:    {...} shape_corr={x.xx}
  Variance:      {...} cv_ratio={x.xx}

PLAUSIBILITY         <param>: <pt value> vs prior [lo,hi] / bio range — {OK|AT-BOUND|DRIFT|IMPOSSIBLE}  (report even if fit PASSes)
KEY FINDINGS         1. <primary finding + mechanism>  2. ...
EXPERIMENTS RUN      <label> bias_c=.. bias_d=.. params:{...}   (one line each)
RECOMMENDED PARAMETER ADJUSTMENTS   beta_j0_hum: <medoid> -> <target> (x<ratio>) ...
RECOMMENDED PRIOR CHANGES           <param>: <old dist> -> <new dist>   (hand to disease-modeler/statistician)
TRADEOFFS NOTED
NEXT CALIBRATION CONFIGURATION      <specific changes to the calibration script / priors>
```

Save experiments and the brief under `claude/diagnose_fit/` (temp/exploratory location). Prior-change
recommendations are *proposals* — the calibration-doctor proposes; `disease-modeler` /
`statistician` own committing prior/config edits.
