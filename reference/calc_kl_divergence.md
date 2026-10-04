# Calculate Kullback-Leibler Divergence Between Two Distributions

Computes the Kullback-Leibler (KL) divergence between two probability
distributions represented by weighted samples. The KL divergence
measures how one probability distribution diverges from a reference
distribution.

## Usage

``` r
calc_kl_divergence(
  samples1,
  weights1 = NULL,
  samples2,
  weights2 = NULL,
  n_points = 1000,
  eps = 1e-10
)

calculate_kl_divergence(
  samples1,
  weights1 = NULL,
  samples2,
  weights2 = NULL,
  n_points = 1000,
  eps = 1e-10
)
```

## Arguments

- samples1:

  Numeric vector of samples from the first distribution (P).

- weights1:

  Numeric vector of weights for `samples1`. Must be the same length as
  `samples1`. If NULL, uniform weights are used.

- samples2:

  Numeric vector of samples from the second distribution (Q).

- weights2:

  Numeric vector of weights for `samples2`. Must be the same length as
  `samples2`. If NULL, uniform weights are used.

- n_points:

  Integer minimum number of grid points over the support of `samples1`
  (default 1000); the grid is refined further when needed to resolve the
  P bandwidth.

- eps:

  Numeric floor applied to the Q density before taking logs (default
  1e-10).

## Value

A non-negative numeric value representing the KL divergence. Returns 0
when the distributions are identical, and larger values indicate greater
divergence. Returns `NA` with a warning when either weight vector has
Kish effective sample size below 2 (the KDE bandwidth is then
undefined).

## Details

The KL divergence KL(P\|\|Q) is calculated as: \$\$KL(P\|\|Q) = \int
p(x) \log(p(x) / q(x)) dx\$\$

where P represents the distribution from `samples1` and Q represents the
distribution from `samples2`.

Both densities are weighted kernel density estimates whose bandwidths
use the weighted Silverman rule with the Kish effective sample size
\\n\_{eff} = (\sum w)^2 / \sum w^2\\, so a concentrated weight vector
yields a narrow density even when the draws are spread out (with equal
weights this is
[`stats::bw.nrd0()`](https://rdrr.io/r/stats/bandwidth.html)). The
integral \\\int p \log(p/q)\\ is evaluated by the trapezoidal rule on a
grid over the support of P only (where the integrand is non-zero), with
Q interpolated from a full-range KDE, so the value does not level off at
`log(n_points)` when P is much narrower than Q.

Before v0.100.0 both densities used unweighted bandwidths on one grid
over the pooled range and the densities were renormalised as discrete
probabilities, so a narrow P saturated near `log(n_points)`.

Note that KL divergence is not symmetric: KL(P\|\|Q) != KL(Q\|\|P).

## Examples

``` r
# Example 1: Compare two normal distributions
set.seed(123)
samples1 <- rnorm(1000, mean = 0, sd = 1)
samples2 <- rnorm(1000, mean = 0.5, sd = 1.2)
kl_div <- calc_kl_divergence(samples1, NULL, samples2, NULL)
print(paste("KL divergence:", round(kl_div, 4)))
#> [1] "KL divergence: 0.1353"

# Example 2: Using weighted samples
samples1 <- rnorm(500)
weights1 <- runif(500, 0.5, 1.5)
samples2 <- rnorm(500, mean = 1)
weights2 <- runif(500, 0.5, 1.5)
kl_div_weighted <- calc_kl_divergence(samples1, weights1, samples2, weights2)

# Example 3: Comparing posterior to prior in Bayesian analysis
# prior_samples <- rnorm(1000, mean = 0, sd = 2)  # Prior
# posterior_samples <- rnorm(1000, mean = 1, sd = 0.5)  # Posterior
# kl_div <- calc_kl_divergence(posterior_samples, NULL, prior_samples, NULL)
```
