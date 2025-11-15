# Gibbs-Posterior Model Weights

Converts a numeric vector of model scores (treated as **loss-like**,
i.e., lower-is-better) into normalized posterior weights using the Gibbs
(generalized Bayes) formulation: \$\$w_m(\eta) = \frac{\exp(-\eta \\
x_m)}{\sum\_{j=1}^K \exp(-\eta \\ x_j)} \\,\$\$ where \\x_m\\ is the
loss (or information criterion) for model \\m\\ and \\\eta \ge 0\\ is
the inverse temperature (sharpness) parameter.

## Usage

``` r
calc_model_weights_gibbs(x, temperature, verbose = FALSE)
```

## Arguments

- x:

  Numeric vector of model scores (treated as loss-like; lower is
  better).

- temperature:

  Non-negative scalar inverse temperature \\\eta\\. Higher values
  concentrate weight on the best models; lower values flatten weights.
  Typical choices: `0.5` for \\\Delta\\AIC (Akaike weights), `1` for
  \\-\log L\\.

- verbose:

  Logical; if `TRUE`, prints brief diagnostics (range of `x`, applied
  shift, entropy, and effective number of models).

## Value

A numeric vector of length `length(x)` containing weights that sum to 1.
Names are preserved when present.

## Details

This function is agnostic to the specific form of \\x\\; smaller values
are assumed to indicate better models. Common choices include:

- \\x = \Delta \mathrm{AIC}\\ \\ (use `temperature = 1/2` to recover
  Akaike weights),

- \\x = -\log L\\ \\ (use `temperature = 1` for normalized likelihood
  weights),

- Cross-validated losses (e.g., RMSE, MSLE), or any proper scoring rule.

If you start from a higher-is-better score (e.g., log-likelihood), pass
`x = -score` so that lower-is-better holds.

Numerical stability: the implementation subtracts \\\min(x)\\ prior to
exponentiation; this shift leaves the normalized weights unchanged but
helps avoid underflow/overflow.

Special cases:

- `temperature = 0`: uniform weights.

- `temperature = Inf`: hard selection (ties share weight equally).

- All `x` equal: uniform weights.

## References

Bissiri, P. G., Holmes, C. C., & Walker, S. G. (2016). A general
framework for updating belief distributions. *Journal of the Royal
Statistical Society: Series B*, 78(5), 1103–1130.
[doi:10.1111/rssb.12158](https://doi.org/10.1111/rssb.12158)

## Examples

``` r
# Three models with Delta AIC = (0, 2, 6): Akaike weights via temperature = 0.5
calc_model_weights_gibbs(x = c(0, 2, 6), temperature = 0.5)
#> [1] 0.70538451 0.25949646 0.03511903

# From negative log-likelihoods: normalized likelihood weights with temperature = 1
calc_model_weights_gibbs(x = c(120.3, 121.1, 124.8), temperature = 1)
#> [1] 0.68472611 0.30766727 0.00760662

# Using a CV loss (lower is better) with moderate sharpness
set.seed(1)
losses <- c(A = 0.83, B = 0.81, C = 0.92)
calc_model_weights_gibbs(losses, temperature = 3, verbose = TRUE)
#> Gibbs weights with temperature = 3
#> Input range: min = 0.81, max = 0.92, spread = 0.11
#> Stabilization: subtract min(x) = 0.81 before exponentiation.
#> Entropy = 1.0890; effective number of models = 2.971
#>         A         B         C 
#> 0.3539552 0.3758426 0.2702022 

# Temperature extremes
calc_model_weights_gibbs(c(1, 2, 5), temperature = 0)      # uniform
#> [1] 0.3333333 0.3333333 0.3333333
calc_model_weights_gibbs(c(1, 2, 5), temperature = Inf)    # hard selection
#> [1] 1 0 0
```
