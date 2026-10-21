# Health of the posterior weights behind a BFRS calibration

Draws are sampled from the prior and scored by the log-likelihood
\\\ell\\, so the posterior weights of the draws are a coarsened
(generalised-Bayes) likelihood, \\w_i \propto \exp\\\eta(\ell_i - \max_j
\ell_j)\\\\, with \\\eta = \alpha / n\_{obs}\\: \\\alpha\\
(`coarsening_alpha`) is the effective number of independent observations
the likelihood is trusted with and \\n\_{obs}\\ the number of scored
observation cells (Miller and Dunson 2019; Bissiri, Holmes and Walker
2016). \\\eta\\ is fixed before the weights are seen, so the ESS of
these weights is not circular.

The result holds:

- `coarsened`:

  The posterior over every valid draw: `eta`, `ess_kish` (ESS_all),
  `ess_prop`, the largest weight share, `khat` (Pareto k-hat of the
  tempered weights, `calc_is_diagnostics(eta * log_lik)`) with
  `khat_status`, and `khat_trend` / `khat_trend_slope` (whether k-hat
  falls as draws accumulate, `.mosaic_khat_trend()`); `n_locations` and
  `alpha_per_location` (\\\eta\\ is pooled over locations).

- `bestfit`:

  The best-fit ensemble: draws in descending log-likelihood until their
  subset ESS reaches `best_subset_ratio * ESS_all`, bounded by
  `[min_members, max_members]`. Members (`idx`, `sim`), their
  renormalised weights, `ess_best`, `mass` (posterior weight held),
  `binding` and `capped`.

- `mce`:

  Monte Carlo standard error of the members' posterior means per
  parameter (seed batches by `(sim - 1) mod n_batches`; SD of the batch
  means / posterior SD / \\\sqrt{B'}\\, over the \\B'\\ batches holding
  at least 1 percent of the members' weight), with median, p90 and max.

- `informativeness`:

  The N-invariant informativeness measure: `ess_untempered` (Kish ESS of
  the untempered, \\\eta = 1\\, weights), `ess_untempered_frac` (that
  ESS / valid draws), `n_tied_max` (valid draws tied at the best
  log-likelihood) and `flat_ess_frac` (the threshold). `flat` is
  `ess_untempered_frac >= flat_ess_frac`, `NA` when not assessable
  (fewer than `1 / flat_ess_frac` valid draws).

- `eta_star`:

  A diagnostic only (since red-team round 1): the largest \\\eta \le 1\\
  at which the Kish ESS over all draws still reaches the ESS_all target
  (`ess_best_target / best_subset_ratio`), by bisection on \\\log\eta\\;
  `n_eff = n_obs * eta`. It grows with the draws, so it no longer
  decides flatness.

- `slope`:

  The log-log ESS growth slope, supplied by the caller from a batch
  history (`NA` for a single snapshot).

- `convergence`:

  The
  [`calc_convergence_status`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_status.md)
  result (through `.mosaic_health_status()`); `status` and `reasons`
  repeat its colour and its reasons (joined by "; ").

- `context`:

  `budget_exhausted`, `fixed_mode` and `n_attempted`, so the status can
  be re-derived for other members.

The \\\eta = 1\\ likelihood-concentration numbers are not part of this
result (they are in `convergence_diagnostics.json` only, from
[`calc_is_diagnostics`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_is_diagnostics.md)),
nor is the all-draws seed-batch ratio.

No Monte Carlo-noise ceiling is applied to \\\eta^\*\\. It would be
\\\eta \le 1.7/\sigma\_{seed}\\, where \\\sigma\_{seed}\\ is the
seed-to-seed SD of the log-likelihood at fixed parameters (Doucet et al.
2015), which needs per-replicate log-likelihoods that
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
collapses by log-mean-exp and does not store.

## Usage

``` r
calc_weight_health(
  log_lik,
  weights = NULL,
  params,
  n_obs,
  sim = NULL,
  coarsening_alpha = 10,
  best_subset_ratio = 0.2,
  ess_best_target = 200,
  min_members = 30L,
  max_members = 1000L,
  n_batches = 10L,
  thresholds = list(),
  slope = NA_real_,
  budget_exhausted = FALSE,
  fixed_mode = FALSE,
  n_attempted = NULL,
  n_locations = NULL,
  ess_all_target = NULL,
  max_simulations = NULL,
  batch_size_min = 500L,
  batch_size_max = 10000L,
  ess_target = NULL,
  batch_max = NULL,
  ess_method = NULL
)
```

## Arguments

- log_lik:

  Numeric vector of per-draw log-likelihoods. Non-finite entries get
  zero weight.

- weights, ess_target, batch_max, ess_method:

  Deprecated since MOSAIC 1.4.0 and ignored (with a warning): the
  posterior summaries use the best-fit members, the ESS target is
  `ess_best_target`, the Monte Carlo error bands replace the batch-ratio
  limit, and the \\\eta = 1\\ block is gone.

- params:

  Data frame or matrix of sampled parameter values, one row per draw.
  Non-numeric columns and columns constant over the valid draws are
  dropped.

- n_obs:

  Number of scored observation cells (see
  [`calc_run_weight_health()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_run_weight_health.md));
  sets \\\eta = \alpha / n\_{obs}\\.

- sim:

  Simulation ids, one per draw (seed batches and selection ties).
  Default `seq_along(log_lik)`.

- coarsening_alpha:

  Positive number: the effective number of independent observations of
  the posterior. Default `10`.

- best_subset_ratio:

  Share of ESS_all the best-fit ensemble must reach, in (0, 1\]. Default
  `0.2`.

- ess_best_target:

  Required ensemble ESS. Default `200`.

- min_members, max_members:

  Bounds on the ensemble size. Defaults `30` and `1000`.

- n_batches:

  Number of seed batches. Default `10`.

- thresholds:

  Named list overriding the status bands (see
  [`calc_convergence_status`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_status.md)).

- slope:

  Log-log ESS growth slope over recent batches (from `run_state.json`);
  `NA` for a single snapshot.

- budget_exhausted:

  Logical; the run stopped at its simulation cap.

- fixed_mode:

  Logical; a fixed-budget run (the status is assessed, not used to
  stop).

- n_attempted:

  Simulations attempted, valid or not (converts projected valid draws to
  simulations). Default `NULL`: `length(log_lik)`.

- n_locations:

  Number of locations in the run (recorded with `alpha_per_location`;
  \\\eta\\ is pooled over locations). Default `NULL` (unknown).

- ess_all_target:

  ESS_all target of the reasons and projections. Default `NULL`:
  `ess_best_target / best_subset_ratio` (the best-fit rule). Under
  `"coarsened"` pass the scheme's own target (the resample size, so
  about `ess_best_target`).

- max_simulations:

  The run's `max_simulations_total` (adaptive), so the k-hat reason can
  say whether its projection is within the budget. Default `NULL`
  (unknown).

- batch_size_min, batch_size_max:

  `batch_size_adaptive` and `max_batch_predictive`: bound the confirming
  batch included in `sims_projected_for_green`. Defaults 500 and 10,000.

## Value

A list with elements `n_draws`, `n_valid`, `targets`, `coarsened`,
`bestfit`, `mce`, `informativeness`, `eta_star`, `slope`, `flat`,
`context`, `convergence`, `status` (`"green"`, `"yellow"` or `"red"`)
and `reasons` (one string).

## Details

Computes every quantity behind the MOSAIC 1.4.0 simulation-convergence
status from the scored draws, and the status itself through
[`calc_convergence_status`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_status.md)
(the only place the rule lives). The calculation changes no weight,
subset or posterior.

## References

Miller JW, Dunson DB (2019). Robust Bayesian inference via coarsening.
*JASA* 114(527):1113-1125.

Bissiri PG, Holmes CC, Walker SG (2016). A general framework for
updating belief distributions. *JRSS B* 78(5):1103-1130.

Doucet A, Pitt MK, Deligiannidis G, Kohn R (2015). Efficient
implementation of Markov chain Monte Carlo when using an unbiased
likelihood estimator. *Biometrika* 102(2):295-313.

## See also

[`calc_convergence_status`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_status.md),
[`calc_run_weight_health`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_run_weight_health.md)

Other calibration-metrics:
[`calc_convergence_diagnostics()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_diagnostics.md),
[`calc_convergence_status()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_status.md),
[`calc_model_agreement_index()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_agreement_index.md),
[`calc_model_cvw()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_cvw.md),
[`calc_model_ess()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ess.md),
[`calc_run_weight_health()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_run_weight_health.md)

## Examples

``` r
set.seed(1)
n <- 20000
theta <- runif(n)
ll <- -2e5 * (theta - 0.4)^2            # a likelihood over 100,000 cells
wh <- calc_weight_health(ll, params = data.frame(theta = theta), n_obs = 1e5)
wh$bestfit$ess_best
#> [1] 999.9865
wh$status
#> [1] "green"
wh$convergence$checks
#>             check                               label         value
#> 1             ess                        Ensemble ESS 999.986519079
#> 2             mce Monte Carlo error (90th percentile)   0.020762833
#> 3            khat       Validity guard (Pareto k-hat)  -1.793246790
#> 4 informativeness                Data informativeness   0.005554321
#>   threshold_green threshold_yellow colour
#> 1           2e+02           100.00  green
#> 2           1e-01             0.15  green
#> 3           5e-01             0.70  green
#> 4           1e-02               NA  green
#>                                                                                                                  reason
#> 1                                                                                             ensemble ESS 1,000 >= 200
#> 2 Monte Carlo standard error of the posterior means 2.1% of the posterior SD (90th percentile over parameters) <= 10.0%
#> 3                                                                      Pareto k-hat of the tempered weights -1.79 < 0.5
#> 4                             the data inform the posterior (the untempered likelihood ESS is 0.56% of the draws, < 1%)
```
