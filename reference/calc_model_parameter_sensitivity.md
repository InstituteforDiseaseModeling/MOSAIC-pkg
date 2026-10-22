# Compute Parameter Sensitivity Ranking (R2-HSIC)

Computes R2-HSIC (Hilbert-Schmidt Independence Criterion) between each
sampled parameter and the log-likelihood from a calibration
`samples.parquet` file. HSIC measures statistical dependence without
assuming linearity, monotonicity, or independence among inputs, making
it valid for importance-weighted posterior samples where parameters are
correlated (Da Veiga 2015). The R2-HSIC index equals zero iff the
parameter and log-likelihood are statistically independent (under the
RBF kernel).

## Usage

``` r
calc_model_parameter_sensitivity(
  results_file,
  priors_file = NULL,
  output_dir = NULL,
  n_samples = 2000,
  kernel = "rbf",
  test_method = "Asymptotic",
  subset_col = "is_best_subset",
  verbose = TRUE
)
```

## Arguments

- results_file:

  Path to `samples.parquet` containing calibration results.

- priors_file:

  Path to `priors.json` (used for parameter descriptions).

- output_dir:

  Optional directory to write `parameter_sensitivity.csv` (the `sens_df`
  columns plus run-level `weighting` and `n_used`). When `NULL`
  (default) no file is written.

- n_samples:

  Maximum number of posterior draws to use. Samples are drawn with
  replacement using the ensemble members' weights (`weight_best`, or
  `weight_best_opt` when `subset_col = "is_best_subset_opt"`), the
  posterior the other outputs use; a file without them falls back to
  `weight_all` (all valid draws). Default 2000.

- kernel:

  Kernel type for input parameters passed to
  [`sensiHSIC`](https://rdrr.io/pkg/sensitivity/man/sensiHSIC.html). One
  of `"rbf"` (default), `"laplace"`, or `"dcov"`.

- test_method:

  Significance test method. `"Asymptotic"` (default) or `"Permutation"`.

- subset_col:

  Boolean subset-membership column (the ensemble members) used instead
  when the posterior weights are degenerate (Kish ESS below
  `max(10, 0.05 * n_samples)`) or absent. Default `"is_best_subset"`.

- verbose:

  Print progress messages.

## Value

A list with `sens_df` (data.frame with columns `parameter`, `hsic_r2`,
`p_value`, `sig`, `description`), `descriptions` (named character),
`subset_label`, and `n_used`. Returns `NULL` when the computation cannot
proceed (missing file, too few sims/params, or HSIC failure).

## Details

This is the data-layer companion to
[`plot_model_parameter_sensitivity`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_parameter_sensitivity.md):
the computation (and the `parameter_sensitivity.csv` write) live here so
the sensitivity table is produced independently of plotting, and the
plot consumes the returned/loaded data.frame.

The indices are computed a chunk of parameters at a time, so the Gram
matrices held at once are bounded (1 GiB: 33 parameters at
`n_samples = 2000`) instead of growing with the number of parameters
(MOSAIC 1.5.0). On a synthetic 900-parameter run the peak memory fell
from 51.6 GB to 3.7 GB (a 916-parameter Kenya run had held 67 GB). Each
parameter's R2-HSIC and asymptotic p-value depend only on its own column
and the log-likelihood, so the results equal a single call over every
parameter. With `test_method = "Permutation"` each chunk draws its own
permutations, so those p-values are a different Monte Carlo draw.

[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
calls it in every run, whatever `plots` is, and writes
`parameter_sensitivity.csv` (atomically: a temporary file renamed into
place, so a killed run never leaves a truncated table);
[`render_MOSAIC_figures`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/render_MOSAIC_figures.md)
plots that table and recomputes it only when it is missing.

## References

Da Veiga S (2015). Global sensitivity analysis with dependence measures.
*Journal of Statistical Computation and Simulation* 85(7):1283-1305.
