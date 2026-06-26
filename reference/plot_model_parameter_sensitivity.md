# Plot Parameter Sensitivity Ranking (HSIC)

Computes R²-HSIC (Hilbert-Schmidt Independence Criterion) between each
sampled parameter and the log-likelihood, then plots a ranked horizontal
bar chart. HSIC measures statistical dependence without assuming
linearity, monotonicity, or independence among inputs, making it valid
for importance-weighted posterior samples where parameters are
correlated (Da Veiga 2015).

## Usage

``` r
plot_model_parameter_sensitivity(
  results_file,
  priors_file = NULL,
  output_dir = ".",
  sensitivity = NULL,
  max_params = 30,
  n_samples = 2000,
  kernel = "rbf",
  test_method = "Asymptotic",
  subset_col = "is_best_subset",
  verbose = TRUE
)
```

## Arguments

- results_file:

  Path to samples.parquet file containing calibration results.

- priors_file:

  Path to priors.json (used for parameter descriptions in labels).

- output_dir:

  Directory to write the output figure. The CSV is written by
  [`calc_model_parameter_sensitivity`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_parameter_sensitivity.md),
  not here.

- sensitivity:

  Optional precomputed result list from
  [`calc_model_parameter_sensitivity`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_parameter_sensitivity.md)
  (`$sens_df`, `$subset_label`, `$n_used`). When supplied the
  recomputation is skipped and the CSV is not (re)written.

- max_params:

  Maximum number of parameters to display. Default 30.

- n_samples:

  Maximum number of posterior draws to use. Samples are drawn with
  replacement using importance weights. Default 2000.

- kernel:

  Kernel type for input parameters passed to
  [`sensiHSIC`](https://rdrr.io/pkg/sensitivity/man/sensiHSIC.html). One
  of `"rbf"` (default), `"laplace"`, or `"dcov"`.

- test_method:

  Significance test method. `"Asymptotic"` (default, fast, requires n
  \>= 100) or `"Permutation"` (exact, slower).

- subset_col:

  Character name of the boolean subset-membership column used as the
  fallback when importance-weight ESS is degenerate. Defaults to
  `"is_best_subset"`. Pass `"is_best_subset_opt"` to read the
  optimizer-refined subset.

- verbose:

  Print progress messages.

## Value

A data.frame with columns `parameter`, `hsic_r2`, `p_value`, `sig`, and
`description` (invisibly). Writes a PNG to `output_dir`.

## Details

The R²-HSIC index equals zero if and only if the parameter and
log-likelihood are statistically independent (under the RBF kernel), and
equals one if the log-likelihood is a deterministic function of that
parameter alone.

Significance is assessed via an asymptotic Gamma approximation of the
null distribution (valid when n \>= 100). Bars are coloured by
significance level.

The HSIC computation and the `parameter_sensitivity.csv` write live in
[`calc_model_parameter_sensitivity`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_parameter_sensitivity.md);
this function renders the ranked bar chart from that result. Pass a
precomputed `sensitivity` object to avoid recomputing, or let the
function compute it from `results_file`.

## References

Da Veiga S (2015). Global sensitivity analysis with dependence measures.
*Journal of Statistical Computation and Simulation* 85(7):1283-1305.
<https://arxiv.org/abs/1311.2483>

Gretton A, Bousquet O, Smola A, Scholkopf B (2005). Measuring
statistical dependence with Hilbert-Schmidt norms. *Algorithmic Learning
Theory*, LNCS 3734:63-77.
