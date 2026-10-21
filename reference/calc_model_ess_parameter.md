# Calculate Parameter-Specific ESS

Computes the effective sample size (ESS) of each parameter's weighted
posterior mean: a function-specific, variance-ratio (delta-method) ESS
of the weights, bounded above by their Kish ESS.
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
reports it in `parameter_ess.csv` and the auto-mode stopping rule reads
it.

## Usage

``` r
calc_model_ess_parameter(
  results,
  param_names,
  likelihood_col = "likelihood",
  weights = NULL,
  verbose = FALSE,
  ...
)
```

## Arguments

- results:

  Data frame containing simulation results

- param_names:

  Character vector of parameter names to analyze (required)

- likelihood_col:

  Character name of the column containing log-likelihood values
  (default: "likelihood")

- weights:

  Optional non-negative weights, one per row of `results`. `NULL`
  (default) uses the raw importance weights \\\exp(\ell - \max \ell)\\.

- verbose:

  Logical whether to print progress messages (default: FALSE)

- ...:

  Defunct arguments of the pre-1.3.0 grid estimate (`n_bins`, `n_grid`,
  `method`, `marginal_method`); a warning is given and they are ignored.
  Any other argument is an error.

## Value

Data frame with columns:

- parameter: Parameter name

- type: Parameter type ('global' or 'location')

- iso_code: ISO code for location parameters (NA for global)

- ess_marginal: Function-specific ESS of the parameter's posterior mean

## Details

For parameter \\\theta\\ with normalised weights \\\tilde w_i\\,
posterior mean \\\tilde\mu = \sum_i \tilde w_i \theta_i\\ and posterior
variance \\\tilde\sigma^2 = \sum_i \tilde w_i (\theta_i -
\tilde\mu)^2\\, the delta-method variance of the self-normalised
estimate \\\tilde\mu\\ is \\\sum_i \tilde w_i^2 (\theta_i -
\tilde\mu)^2\\ (Owen 2013, eq. 9.9). This is a variance ratio, the
definition behind Owen's \\n_e\\ (eq. 9.13); it is not the weight-based
\\n_e(f)\\ of Owen's eq. 9.17, which uses weights proportional to \\w_i
\|f_i\|\\. The ESS is the number of independent posterior draws that
would give the same precision: \$\$\mathrm{ESS}\_\theta = \frac{\sum_i
\tilde w_i (\theta_i - \tilde\mu)^2} {\sum_i \tilde w_i^2 (\theta_i -
\tilde\mu)^2}.\$\$ It is computed directly from the weights, with no
bandwidth or grid, and is bounded above by the Kish ESS of the weights,
\\(\sum w)^2/\sum w^2\\. The bound is needed because both variances are
estimated from the same weights: when one weight dominates, the
unbounded ratio degenerates upward. For example, two draws weighted 0.99
and 0.01 give 50.5, and a near point mass on a 50,000-draw run gives
close to \\n\\. The bounded value is exactly \\n\\ for uniform weights
and about 1 for a point mass, and it falls as the weights concentrate.
It is lower than the Kish ESS for a parameter whose heavy weights sit in
its tails. When the weighted spread is zero (a point mass, or a
parameter constant over the weighted draws), the Kish ESS is returned.

The weights are `weights` when supplied.
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
passes the weights of the ensemble members the posterior is built from
(zero outside them), so `parameter_ess.csv` is a diagnostic of the
written posterior (since MOSAIC 1.4.0; it gates nothing). Otherwise they
are the raw importance weights \\w_i \propto \exp(\ell_i - \max \ell)\\,
which on production-scale likelihoods collapse to a point mass.

Until MOSAIC 1.3.0 this function returned a grid estimate (a weighted
KDE or binned marginal rescaled by the number of draws). That estimate
did not measure the posterior: its bandwidth came from the unweighted
draws, so a point mass on a randomly chosen draw of a 50,000-draw
production run scored a median of 2,518 against 2,541 for the run's own
weights. It was removed, with its arguments `n_bins`, `n_grid`, `method`
and `marginal_method`; passing any of them now warns and is otherwise
ignored.

## References

Owen AB (2013). *Monte Carlo theory, methods and examples*, chapter 9
(importance sampling), eqs. 9.9 and 9.13.
<https://artowen.su.domains/mc/>

## Examples

``` r
if (FALSE) { # \dontrun{
# Function-specific ESS under the raw importance weights
ess_results <- calc_model_ess_parameter(
  results = simulation_results,
  param_names = c("tau_i", "beta_j0_tot", "gamma_2")
)

# Under explicit posterior weights (e.g. the coarsened posterior)
ess_results <- calc_model_ess_parameter(
  results = simulation_results,
  param_names = c("tau_i", "beta_j0_tot", "gamma_2"),
  weights = simulation_results$weight_all
)
} # }
```
