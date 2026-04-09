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
  max_params = 30,
  n_samples = 2000,
  kernel = "rbf",
  test_method = "Asymptotic",
  verbose = TRUE
)
```

## Arguments

- results_file:

  Path to samples.parquet file containing calibration results.

- priors_file:

  Path to priors.json (used for parameter descriptions in labels).

- output_dir:

  Directory to write the output figure and CSV.

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

- verbose:

  Print progress messages.

## Value

A data.frame with columns `parameter`, `hsic_r2`, `p_value`, `sig`, and
`description` (invisibly). Writes a PNG and CSV to `output_dir`.

## Details

The R²-HSIC index equals zero if and only if the parameter and
log-likelihood are statistically independent (under the RBF kernel), and
equals one if the log-likelihood is a deterministic function of that
parameter alone.

Significance is assessed via an asymptotic Gamma approximation of the
null distribution (valid when n \>= 100). Bars are coloured by
significance level.

## References

Da Veiga S (2015). Global sensitivity analysis with dependence measures.
*Journal of Statistical Computation and Simulation* 85(7):1283-1305.
<https://arxiv.org/abs/1311.2483>

Gretton A, Bousquet O, Smola A, Scholkopf B (2005). Measuring
statistical dependence with Hilbert-Schmidt norms. *Algorithmic Learning
Theory*, LNCS 3734:63-77.
