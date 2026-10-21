# Calculate comprehensive convergence diagnostics with status indicators

Assembles the convergence diagnostics of a BFRS calibration
(`2_calibration/diagnostics/convergence_diagnostics.json`). Since MOSAIC
1.4.0 the verdict is the simulation-convergence status of
[`calc_convergence_status`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_status.md)
(green / yellow / red), carried by `weight_health`;
`summary$convergence_status` mirrors `summary.json` for one release, and
new consumers read `3_results/calibration_diagnostics.json`. Under the
legacy `"saturated"` / `"tempered"` schemes the tier-searched best
subset is also reported, with its pass/warn/fail statuses.

## Usage

``` r
calc_convergence_diagnostics(
  n_total,
  n_successful,
  n_retained,
  n_best_subset,
  ess_best,
  A_best,
  cvw_best,
  percentile_used,
  convergence_tier,
  param_ess_results = NULL,
  target_ess_best = 300,
  target_A_best = 0.95,
  target_cvw_best = 0.5,
  target_max_best_subset = 1000,
  target_ess_param = 300,
  target_ess_param_prop = 0.95,
  ess_method = c("kish", "perplexity"),
  temperature = 1,
  is_diagnostics = NULL,
  weight_health = NULL,
  weighting = c("saturated", "tempered", "coarsened", "bestfit"),
  target_batch_ratio_max = 0.2,
  verbose = TRUE
)
```

## Arguments

- n_total:

  Integer total number of simulations attempted

- n_successful:

  Integer number of simulations that completed successfully (finite
  likelihood)

- n_retained:

  Integer number of simulations retained after outlier removal

- n_best_subset:

  Integer number of simulations in the optimized best subset

- ess_best:

  Numeric effective sample size within the best subset

- A_best:

  Numeric agreement index (entropy-based) for the best subset

- cvw_best:

  Numeric coefficient of variation of weights in the best subset

- percentile_used:

  Numeric size of the best subset as a percentage of all `n_total` draws
  (e.g., 5.0 for the top 5%)

- convergence_tier:

  Character string indicating which tier criteria were used (e.g.,
  "tier_3", "fallback")

- param_ess_results:

  Optional data frame with columns `parameter` and `ess` (or the
  deprecated `ess_marginal`) containing parameter-specific ESS values.
  If NULL, parameter ESS metrics are omitted. Under the 1.4.0 status
  (`weight_health` supplied) the per-parameter ESS is a diagnostic
  (status `"info"`); only a standalone legacy call gates it.

- target_ess_best:

  Numeric target for both subset size and ESS (default 300). Both B_size
  and ESS_B must be \>= target_ess_best.

- target_A_best:

  Numeric target agreement index (default 0.95)

- target_cvw_best:

  Numeric target coefficient of variation (default 0.5)

- target_max_best_subset:

  Numeric maximum best subset size in absolute count (default 1000).
  Replaced target_percentile_max for absolute count-based control.

- target_ess_param:

  Numeric target ESS for individual parameters (default 300); used only
  by a standalone legacy call (no `weight_health`).

- target_ess_param_prop:

  Numeric target proportion of parameters that must meet ESS threshold
  (default 0.95); standalone legacy calls only.

- ess_method:

  Character string specifying ESS calculation method: "kish" (default)
  or "perplexity". This should match the method used for all ESS
  calculations in the workflow.

- temperature:

  Numeric temperature parameter used for weight scaling (default 1, for
  documentation purposes)

- is_diagnostics:

  Optional list of \\\eta = 1\\ diagnostics from
  [`calc_is_diagnostics`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_is_diagnostics.md),
  with elements `best` and `all` (and `optimized` when the subset
  optimiser ran). Their ESS and positive-ratio counts are reported under
  `$likelihood_concentration` with an explanatory note and are never
  gated: they describe the untempered likelihood, which collapses onto
  one draw for any informative stochastic likelihood whatever the fit.

- weight_health:

  Optional result of
  [`calc_weight_health`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_weight_health.md)
  for the members the posterior is built from
  ([`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
  passes it under every scheme). When it carries a convergence status,
  that status is the verdict: `summary$convergence_status` is `"green"`,
  `"yellow"` or `"red"` (mirroring `summary.json`), `$status` holds the
  label, reasons, flags and the four checks, and `$metrics` the ESS_all,
  ensemble ESS, Monte Carlo error, k-hat and informativeness rows.
  Required under `weighting = "bestfit"` or `"coarsened"`.

- weighting:

  Posterior weighting scheme the inputs come from
  (`control$targets$posterior_weighting`): `"saturated"` (default, the
  pre-1.3.0 behaviour) or `"tempered"` also report the tier-searched
  best subset (B_size, its cap, ESS_B, A_B, CV_B) with the legacy PASS /
  WARN / FAIL verdict, which becomes `summary$tier_status` when
  `weight_health` supplies the 1.4.0 status (and is
  `summary$convergence_status` otherwise, for a standalone legacy call).
  `"bestfit"` and `"coarsened"` report no tier rows.

- target_batch_ratio_max:

  Deprecated since MOSAIC 1.4.0 and ignored: the seed-batch-ratio rule
  was retired in favour of the Monte Carlo error bands of
  [`calc_convergence_status`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_status.md).

- verbose:

  Logical indicating whether to print diagnostic messages (default TRUE)

## Value

A list with the following structure:

- settings:

  Weighting scheme, eta, coarsening alpha, ESS method, description (plus
  optimization_tier, percentile_used and temperature under the legacy
  schemes)

- targets:

  Target values with descriptions

- metrics:

  Metric values with descriptions and statuses: ess_all, ess_best, mce,
  khat and informativeness (1.4.0 status colours), the legacy tier rows
  under "saturated" / "tempered", and param_ess

- summary:

  Counts and the overall convergence status (plus the legacy tier
  verdict `tier_status`)

- status:

  (1.4.0) status, label, reasons, flags and the four checks

- likelihood_concentration, weight_health:

  When supplied

The returned list is ready for JSON serialization and contains all
information needed for visualization by
[`plot_model_convergence_status`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_convergence_status.md).

## Status Thresholds

Status indicators are calculated using multiplicative thresholds
relative to targets:

**For "higher is better" metrics (ESS, Agreement):**

- **PASS**: value \>= target \* pass_threshold

- **WARN**: value \>= target \* warn_threshold but \< pass_threshold

- **FAIL**: value \< target \* warn_threshold

**For "lower is better" metrics (CVw):**

- **PASS**: value \<= target \* pass_threshold

- **WARN**: value \<= target \* warn_threshold but \> pass_threshold

- **FAIL**: value \> target \* warn_threshold

**Default thresholds by metric:**

- ESS_B (target `target_ess_best`): pass=1.0 (100%), warn=0.8 (80%)

- A_B: pass=0.9 (90%), warn=0.7 (70%)

- CVw_B: pass=1.2 (120%), warn=2.0 (200%)

- B_size (lower bound, target `target_ess_best`): pass=1.0 (100%),
  warn=0.5 (50%)

- B_size_upper (upper cap, target `target_max_best_subset`, lower is
  better): pass when `n_best_subset <= target`, warn when it exceeds the
  cap by at most 20%, fail beyond that.
  [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
  enforces the cap when it selects the subset, so in a normal run this
  is an invariant check.

- Param ESS: pass=1.0 (100%), warn=0.8 (80%)

The subset percentile (`percentile_used` against
`target_max_best_subset / n_total * 100`) carries the same information
as B_size_upper on the same denominator, so its status is reported in
`summary$percentile_status` but is not gated.

The function defaults for the targets (300 / 0.95 / 0.5) are not the
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
control defaults (`ESS_best = 100`, `A_best = 0.70`, `CVw_best = 1.0`);
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
always passes its targets explicitly.

## Overall Status

With `weight_health` carrying a status, `summary$convergence_status` is
that status (`"green"`, `"yellow"` or `"red"`: the worst of the
ensemble-ESS, Monte Carlo error, k-hat and informativeness checks). A
standalone legacy call (`"saturated"` / `"tempered"`, no
`weight_health`) aggregates the tier statuses and the per-parameter ESS
proportion instead:

- **FAIL**: Any metric has status "fail"

- **WARN**: No failures, but at least one metric has status "warn"

- **PASS**: All metrics have status "pass"

## See also

[`plot_model_convergence_status`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_convergence_status.md),
[`calc_model_ess`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ess.md),
[`calc_model_agreement_index`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_agreement_index.md)

Other calibration-metrics:
[`calc_convergence_status()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_status.md),
[`calc_model_agreement_index()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_agreement_index.md),
[`calc_model_cvw()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_cvw.md),
[`calc_model_ess()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ess.md),
[`calc_run_weight_health()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_run_weight_health.md),
[`calc_weight_health()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_weight_health.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# After running BFRS calibration and calculating metrics
diagnostics <- calc_convergence_diagnostics(
    n_total = 10000,
    n_successful = 9500,
    n_retained = 8500,
    n_best_subset = 500,
    ess_best = 280,
    A_best = 0.92,
    cvw_best = 0.55,
    percentile_used = 4.8,
    convergence_tier = "tier_3",
    param_ess_results = param_ess_df,
    target_ess_best = 300,
    target_A_best = 0.95,
    target_cvw_best = 0.5,
    target_max_best_subset = 1000,
    ess_method = "kish"
)

# Save to JSON. digits = NA preserves full numerical precision;
# the default digits = 4 silently rounds small diagnostic values
# (p-values, ESS ratios, etc.) to zero.
jsonlite::write_json(diagnostics, "convergence_diagnostics.json",
                     pretty = TRUE, auto_unbox = TRUE, digits = NA)

# Check overall status
print(diagnostics$summary$convergence_status)
} # }
```
