# Simulation-convergence status of a calibration (green / yellow / red)

Four checks, each coloured green, yellow or red; the overall status is
the **worst** of them.

|  |  |  |  |
|----|----|----|----|
| **Check** | **Green** | **Yellow** | **Red** |
| Ensemble ESS (`ess_best`) | \\\ge\\ target | \\\ge\\ 0.5 x target | below |
| Monte Carlo error (`mce_p90`) | \\\le\\ 0.10 | \\\le\\ 0.15 | above |
| Validity guard (`khat`) | \< 0.5 | \< 0.7 | \\\ge\\ 0.7 |
| Data informativeness (`ess_untempered_frac`) | \< 0.01 | \\\ge\\ 0.01 (flat likelihood) | – |

- **Ensemble ESS**: the Kish ESS of the best-fit ensemble. The Monte
  Carlo standard error of a posterior mean is about SD / \\\sqrt{ESS}\\,
  so 7 percent of the posterior SD at 200 and 10 percent at 100.

- **Monte Carlo error**: the 90th percentile over parameters of the
  seed-batch standard error of the ensemble's posterior means, in
  posterior SDs (Flegal & Jones 2010). It is a standard error: two
  independent reruns differ by about \\\sqrt{2}\\ times it (SD). The
  true value is nearly the same for every parameter (about
  \\1/\sqrt{ESS\_{best}}\\), so the spread over parameters is mostly
  estimation noise and the 90th percentile is a conservative summary
  (about 1.3 times the typical value).

- **Validity guard**: Pareto k-hat of the tempered importance weights
  (Vehtari et al. 2024). The tempered weights are bounded, so the true
  tail shape is \\\le 0\\ and k-hat falls as draws accumulate; a high
  value means a few draws carry much of the weight at this number of
  draws. The reason states the k-hat trend (`khat_trend`): when falling,
  the simulations at which it would reach 0.5 at that rate and whether
  that is within `max_simulations`; "uncertain" when the trend is within
  twice its standard error of zero. An undefined k-hat is green only
  when the tail ratios are tied (weights nearly flat, `khat_cause`
  containing "tied"); any other cause is yellow, "not assessed".

- **Data informativeness**: the Kish ESS of the untempered (\\\eta =
  1\\) likelihood weights as a share of the valid draws. With
  informative data it collapses to about \\1/N\\; a share of at least
  `thresholds$flat_ess_frac` (default 0.01) means the likelihood is flat
  over a sizeable part of the prior, so the posterior is close to the
  prior. The share does not depend on the number of draws. The sampling
  can be adequate while the result is uninformative, so it is yellow,
  never red. A flat verdict also needs an untempered Kish ESS of at
  least 20: the share is at least \\1/N\\, so on a small run a collapsed
  likelihood (ESS 1-3) would otherwise read as flat. It is not assessed
  (yellow) with fewer than two valid draws. Many draws tied at the best
  log-likelihood (a plateau, at least 10) add a note to the reason, and
  an undefined k-hat from too few distinct tail ratios then counts as
  tied.

A run with fewer than two valid draws has failed: its first reason says
so (no posterior can be formed and more simulations of the same setup
will not help) and every check reads "not assessed".

Missing values in the other checks are never green: an `NA` `ess_best`
is red (no ensemble), an `NA` `mce_p90` is yellow ("not assessed"). An
infinite `mce_p90` (fewer than two seed batches hold members) is red.

Every non-green check contributes one plain-language reason naming its
remedy. The ESS reason quotes the draws projected to reach the target,
\\N \times ESS\_{all}^{target} / ESS\_{all}\\ (supply `n_draws`,
`ess_all` and `ess_all_target`, or `n_draws_projected`); the Monte Carlo
error reason uses MCE \\\propto 1/\sqrt{N}\\. Both are converted from
valid draws to simulations at the run's success rate
(`n_draws / n_attempted`). A projection is reliable only when the ESS
growth slope is known and at least `thresholds$slope_stall`; every
reason that quotes one says so otherwise.

Flags never change the colour: `budget_cap_reached`,
`ess_growth_stalled` (`slope` below `thresholds$slope_stall`),
`ensemble_size_bound` (a member bound set the ensemble size) and
`fixed_budget` (the status is assessed, not used to stop).

The function is pure (no I/O, no RNG) and takes scalars; apply it across
runs with [`lapply()`](https://rdrr.io/r/base/lapply.html) /
[`Map()`](https://rdrr.io/r/base/funprog.html).

## Usage

``` r
calc_convergence_status(
  ess_best,
  ess_best_target = 200,
  mce_p90,
  khat,
  flat = NA,
  ess_all = NA_real_,
  ess_all_target = NA_real_,
  n_draws = NA_real_,
  n_draws_projected = NA_real_,
  slope = NA_real_,
  budget_exhausted = FALSE,
  fixed_mode = FALSE,
  size_bound = NA_character_,
  eta_star = NA_real_,
  thresholds = list(),
  ess_untempered_frac = NA_real_,
  n_tied_max = NA_real_,
  khat_cause = NA_character_,
  khat_trend = NA_character_,
  khat_trend_slope = NA_real_,
  n_attempted = NA_real_,
  max_simulations = NA_real_,
  batch_size_min = 500,
  batch_size_max = 10000
)
```

## Arguments

- ess_best:

  Kish ESS of the best-fit ensemble.

- ess_best_target:

  Required ensemble ESS (`control$targets$ESS_best`). Default 200.

- mce_p90:

  90th percentile over parameters of the members' Monte Carlo error /
  posterior SD.

- khat:

  Pareto k-hat of the tempered weights (`NA` = undefined).

- flat:

  Optional logical flat-likelihood verdict, used only when
  `ess_untempered_frac` is `NA`.

- ess_all:

  Optional Kish ESS of the posterior over all draws.

- ess_all_target:

  Optional ESS_all target (`ess_best_target / best_subset_ratio`).

- n_draws:

  Optional valid draws (finite log-likelihood) so far.

- n_draws_projected:

  Optional total valid draws projected to reach the ESS target; computed
  from `n_draws`, `ess_all` and `ess_all_target` when `NA`.

- slope:

  Optional log-log ESS growth slope over the latter part of the run.

- budget_exhausted:

  Logical; the run stopped at `max_simulations_total`.

- fixed_mode:

  Logical; a fixed-budget run.

- size_bound:

  Which bound set the ensemble size: `"ratio"`, `"min_members"`,
  `"max_members"` (or `NA`).

- eta_star:

  Optional supported resolution \\\eta^\*\\ (a diagnostic; not used by
  the rule).

- thresholds:

  Named list overriding the band thresholds: `ess_yellow_frac` (0.5),
  `mce_green` (0.10), `mce_yellow` (0.15), `khat_green` (0.5),
  `khat_yellow` (0.7), `slope_stall` (0.8), `flat_ess_frac` (0.01).

- ess_untempered_frac:

  Optional Kish ESS of the untempered (\\\eta = 1\\) weights / valid
  draws.

- n_tied_max:

  Optional number of valid draws tied at the best log-likelihood.

- khat_cause:

  Optional reason k-hat is undefined (the
  [`calc_is_diagnostics()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_is_diagnostics.md)
  `khat_status`).

- khat_trend:

  Optional `"falling"`, `"not falling"` or `"uncertain"`: k-hat over
  growing numbers of draws (`.mosaic_khat_trend()`).

- khat_trend_slope:

  Optional change of k-hat per doubling of the draws, quoted when k-hat
  is falling.

- n_attempted:

  Optional simulations attempted (valid or not); converts projected
  valid draws to simulations.

- max_simulations:

  Optional simulation budget (`max_simulations_total`); the k-hat reason
  says whether its projection is within it.

- batch_size_min, batch_size_max:

  Bounds of the confirming batch in `sims_projected_for_green`
  (`batch_size_adaptive`, `max_batch_predictive`). Defaults 500 and
  10,000.

## Value

A list with

- `status`:

  `"green"`, `"yellow"` or `"red"`.

- `label`:

  The status text: "Converged: enough simulations", "Usable with
  caution" or "Not converged: more simulations needed".

- `checks`:

  Data frame, one row per check (`check` is `"ess"`, `"mce"`, `"khat"`
  or `"informativeness"`): `label`, `value`, `threshold_green`,
  `threshold_yellow` (`NA` for the informativeness check), `colour` and
  `reason`.

- `reasons`:

  Character vector, one reason per non-green check.

- `flags`:

  Character vector of flag identifiers, zero or more of
  `"budget_cap_reached"`, `"ess_growth_stalled"`,
  `"ensemble_size_bound"` and `"fixed_budget"` (summary.json schema 2
  `convergence_flags`).

- `flag_text`:

  Character vector parallel to `flags`: the plain-language text of each
  flag (joined into `convergence_flags_text`).

- `n_draws_projected`, `n_draws_more`:

  Projected total valid draws for the ESS target and the shortfall (`NA`
  when unknown).

- `sims_projected_for_green`:

  Total simulations projected for GREEN (units: simulations): the larger
  of the ESS and Monte Carlo error projections that exceed the current
  valid draws, divided by the success rate, plus, in adaptive mode, one
  confirming batch (a quarter of that total within
  `[batch_size_min, batch_size_max]`: the second green check of the
  stopping rule; a fixed-budget run has none); `NA` when the three
  sampling checks are green or when no check that more draws can fix is
  behind.

- `projection_reliable`:

  `FALSE` when the growth slope is unknown or below
  `thresholds$slope_stall`.

- `success_rate`:

  `n_draws / n_attempted` (1 when unknown).

- `sampling_green`:

  `TRUE` when the three sampling checks (ensemble ESS, Monte Carlo
  error, k-hat) are green, whatever the informativeness check: the
  adaptive stopping condition.

- `flat`:

  The flat-likelihood verdict (`NA` when not assessed).

- `thresholds`:

  The thresholds used.

## Details

Answers one question: are there enough simulations to represent the
model's best-fitting ensemble reliably? The status is the single rule
behind the adaptive stopping gate, `summary.json`, the calibration
report and the backfill
([`calc_run_weight_health`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_run_weight_health.md)).
Whether the model *fits* the data (R², bias, coverage) is reported
elsewhere and never enters it.

## References

Vehtari A, Gelman A, Simpson D, Carpenter B, Bürkner P-C (2021).
Rank-normalization, folding, and localization: an improved R-hat for
assessing convergence of MCMC. *Bayesian Analysis* 16(2):667-718.

Vehtari A, Simpson D, Gelman A, Yao Y, Gabry J (2024). Pareto smoothed
importance sampling. *JMLR* 25(72):1-58.

Flegal JM, Jones GL (2010). Batch means and spectral variance estimators
in Markov chain Monte Carlo. *Annals of Statistics* 38(2):1034-1070.

## See also

[`calc_weight_health`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_weight_health.md),
[`calc_run_weight_health`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_run_weight_health.md)

Other calibration-metrics:
[`calc_convergence_diagnostics()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_diagnostics.md),
[`calc_model_agreement_index()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_agreement_index.md),
[`calc_model_cvw()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_cvw.md),
[`calc_model_ess()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ess.md),
[`calc_run_weight_health()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_run_weight_health.md),
[`calc_weight_health()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_weight_health.md)

## Examples

``` r
st <- calc_convergence_status(ess_best = 140, ess_best_target = 200,
                              mce_p90 = 0.08, khat = 0.2,
                              ess_untempered_frac = 2e-5,
                              ess_all = 700, ess_all_target = 1000,
                              n_draws = 50000, slope = 0.98)
st$status
#> [1] "yellow"
st$reasons
#> [1] "ensemble ESS 140 of 200: about 21k more simulations would reach the target"
st$checks
#>             check                               label   value threshold_green
#> 1             ess                        Ensemble ESS 1.4e+02           2e+02
#> 2             mce Monte Carlo error (90th percentile) 8.0e-02           1e-01
#> 3            khat       Validity guard (Pareto k-hat) 2.0e-01           5e-01
#> 4 informativeness                Data informativeness 2.0e-05           1e-02
#>   threshold_yellow colour
#> 1           100.00 yellow
#> 2             0.15  green
#> 3             0.70  green
#> 4               NA  green
#>                                                                                                                  reason
#> 1                                            ensemble ESS 140 of 200: about 21k more simulations would reach the target
#> 2 Monte Carlo standard error of the posterior means 8.0% of the posterior SD (90th percentile over parameters) <= 10.0%
#> 3                                                                       Pareto k-hat of the tempered weights 0.20 < 0.5
#> 4                            the data inform the posterior (the untempered likelihood ESS is 0.002% of the draws, < 1%)
```
