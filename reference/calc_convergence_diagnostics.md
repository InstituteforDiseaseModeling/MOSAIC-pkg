# Calculate comprehensive convergence diagnostics with status indicators

Centralized function for computing all convergence diagnostics and
status indicators for BFRS calibration results. Takes metrics and
targets as input, calculates pass/warn/fail status for each metric using
consistent thresholds, and aggregates to an overall convergence status.

## Usage

``` r
calc_convergence_diagnostics(
  n_total,
  n_successful,
  n_retained,
  n_best_subset,
  ess_retained,
  ess_best,
  A_best,
  cvw_best,
  percentile_used,
  convergence_tier,
  param_ess_results = NULL,
  target_ess_best = 300,
  target_A_best = 0.95,
  target_cvw_best = 0.5,
  target_B_min = 100,
  target_percentile_max = 5,
  target_ess_param = 300,
  target_ess_param_prop = 0.95,
  temperature = 1,
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

- ess_retained:

  Numeric effective sample size across all retained simulations

- ess_best:

  Numeric effective sample size within the best subset

- A_best:

  Numeric agreement index (entropy-based) for the best subset

- cvw_best:

  Numeric coefficient of variation of weights in the best subset

- percentile_used:

  Numeric percentile of likelihood distribution used for best subset
  selection (e.g., 5.0 for top 5%)

- convergence_tier:

  Character string indicating which tier criteria were used (e.g.,
  "tier_3", "fallback")

- param_ess_results:

  Optional data frame with columns `parameter` and `ess_marginal`
  containing parameter-specific ESS values. If NULL, parameter ESS
  metrics are omitted.

- target_ess_best:

  Numeric target ESS for the best subset (default 300)

- target_A_best:

  Numeric target agreement index (default 0.95)

- target_cvw_best:

  Numeric target coefficient of variation (default 0.5)

- target_B_min:

  Integer minimum acceptable size for best subset (default 100)

- target_percentile_max:

  Numeric maximum percentile for subset selection (default 5.0, meaning
  top 5%)

- target_ess_param:

  Numeric target ESS for individual parameters (default 300)

- target_ess_param_prop:

  Numeric target proportion of parameters that must meet ESS threshold
  (default 0.95, meaning 95%)

- temperature:

  Numeric temperature parameter used for weight scaling (default 1, for
  documentation purposes)

- verbose:

  Logical indicating whether to print diagnostic messages (default TRUE)

## Value

A list with the following structure:

- settings:

  List with optimization_tier, percentile_used, temperature, description

- targets:

  List of target values with descriptions for each metric

- metrics:

  List of metric values with descriptions and status indicators

- summary:

  List with counts, tier info, and overall convergence status

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

- ESS_B: pass=0.8 (80%), warn=0.5 (50%)

- A_B: pass=0.9 (90%), warn=0.7 (70%)

- CVw_B: pass=1.2 (120%), warn=2.0 (200%)

- B_size: pass=1.0 (100%), warn=0.5 (50%)

- Percentile: pass=1.0 (100%), warn=1.5 (150%)

- Param ESS: pass=1.0 (100%), warn=0.8 (80%)

## Overall Status

The overall convergence status is determined by aggregating individual
metric statuses:

- **FAIL**: Any metric has status "fail"

- **WARN**: No failures, but at least one metric has status "warn"

- **PASS**: All metrics have status "pass"

## See also

[`plot_model_convergence_status`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_convergence_status.md),
[`calc_model_ess`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ess.md),
[`calc_model_agreement_index`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_agreement_index.md)

Other calibration-metrics:
[`calc_model_agreement_index()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_agreement_index.md),
[`calc_model_aic_delta()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_aic_delta.md),
[`calc_model_akaike_weights()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_akaike_weights.md),
[`calc_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_convergence.md),
[`calc_model_cvw()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_cvw.md),
[`calc_model_ess()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ess.md),
[`calc_model_max_weight()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_max_weight.md),
[`plot_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_convergence.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# After running BFRS calibration and calculating metrics
diagnostics <- calc_convergence_diagnostics(
    n_total = 10000,
    n_successful = 9500,
    n_retained = 8500,
    n_best_subset = 500,
    ess_retained = 450,
    ess_best = 280,
    A_best = 0.92,
    cvw_best = 0.55,
    percentile_used = 4.8,
    convergence_tier = "tier_3",
    param_ess_results = param_ess_df,
    target_ess_best = 300,
    target_A_best = 0.95,
    target_cvw_best = 0.5,
    target_B_min = 100,
    target_percentile_max = 5.0
)

# Save to JSON
jsonlite::write_json(diagnostics, "convergence_diagnostics.json",
                     pretty = TRUE, auto_unbox = TRUE)

# Check overall status
print(diagnostics$summary$convergence_status)
} # }
```
