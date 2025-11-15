# Calculate convergence diagnostics and write to files

Calculates model-weight agreement diagnostics from calibration results
and writes both per-simulation data (Parquet) and aggregate diagnostics
(JSON) to files. This function measures **model-weight agreement** and
**evidence concentration**, not MCMC convergence.

## Usage

``` r
calc_model_convergence(
  PATHS,
  results,
  output_dir,
  delta_max = 6,
  temperature = 1,
  ess_min = 1000,
  A_min = 0.75,
  cvw_max = 1,
  B_min = 2,
  max_w_max = 0.5,
  verbose = TRUE
)
```

## Arguments

- PATHS:

  MOSAIC paths object from
  [`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md).

- results:

  Data frame with columns: sim, seed, likelihood (and optionally
  others).

- output_dir:

  Character string specifying directory to write output files.

- delta_max:

  Numeric Δ cutoff used for truncation (default `6`).

- temperature:

  Positive scalar temperature for weight scaling (default `1`). See
  [`calc_model_akaike_weights()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_akaike_weights.md)
  for guidance.

- ess_min:

  Numeric target minimum effective sample size (ESS; default `1000`).

- A_min:

  Numeric target minimum agreement index (default `0.75`).

- cvw_max:

  Numeric target maximum coefficient of variation of weights (default
  `1.0`).

- B_min:

  Integer target minimum retained set size \\B\\ (default `2`).

- max_w_max:

  Numeric in (0, 1\] for the maximum allowed normalized weight (default
  `0.5`).

- verbose:

  Logical indicating whether to print progress messages (default
  `TRUE`).

## Value

A list with summary information:

- `status` — Overall convergence status ("PASS", "WARN", "FAIL")

- `n_total` — Total number of simulations

- `n_successful` — Number of successful simulations (finite likelihood)

- `n_retained` — Number of simulations in retained set

- `files_written` — Vector of output file names

## Important Note

This function measures **model-weight agreement** and **evidence
concentration**, not MCMC convergence. For MCMC diagnostics, use R-hat,
effective sample size from chains, trace plots, etc. The "convergence"
terminology here refers to agreement across the ensemble of parameter
draws, not chain convergence.

## File Outputs

The function writes two files to the output directory:

- `convergence_results.parquet` — Per-simulation data with columns: sim,
  seed, likelihood, aic, delta_aic, w, w_tilde, retained

- `convergence_diagnostics.json` — Aggregate metrics with human-readable
  descriptions, targets, and pass/fail status

## See also

[`plot_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_convergence.md),
[`calc_model_aic_delta()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_aic_delta.md),
[`calc_model_akaike_weights()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_akaike_weights.md)

Other calibration-metrics:
[`calc_convergence_diagnostics()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_diagnostics.md),
[`calc_model_agreement_index()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_agreement_index.md),
[`calc_model_aic_delta()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_aic_delta.md),
[`calc_model_akaike_weights()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_akaike_weights.md),
[`calc_model_cvw()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_cvw.md),
[`calc_model_ess()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ess.md),
[`calc_model_max_weight()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_max_weight.md),
[`plot_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_convergence.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Prepare results data frame
results <- data.frame(
  sim = 1:1000,
  seed = 1001:2000,
  likelihood = 1500 + rnorm(1000, sd = 3)
)

# Calculate convergence and write files
summary <- calc_model_convergence(
  PATHS = get_paths(),
  results = results,
  output_dir = "path/to/output",
  temperature = 1.5
)

print(summary)
} # }
```
