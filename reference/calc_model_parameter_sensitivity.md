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

  Optional directory to write `parameter_sensitivity.csv`. When `NULL`
  (default) no file is written and the data.frame is returned.

- n_samples:

  Maximum number of posterior draws to use. Samples are drawn with
  replacement using importance weights. Default 2000.

- kernel:

  Kernel type for input parameters passed to
  [`sensiHSIC`](https://rdrr.io/pkg/sensitivity/man/sensiHSIC.html). One
  of `"rbf"` (default), `"laplace"`, or `"dcov"`.

- test_method:

  Significance test method. `"Asymptotic"` (default) or `"Permutation"`.

- subset_col:

  Boolean subset-membership column used as the fallback when
  importance-weight ESS is degenerate. Default `"is_best_subset"`.

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

## References

Da Veiga S (2015). Global sensitivity analysis with dependence measures.
*Journal of Statistical Computation and Simulation* 85(7):1283-1305.
