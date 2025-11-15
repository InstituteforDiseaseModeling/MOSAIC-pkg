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

  Integer specifying the number of points for kernel density estimation.
  Higher values provide more accurate estimates but require more
  computation. Default is 1000.

- eps:

  Numeric value for numerical stability. Small positive value added to
  densities to avoid log(0). Default is 1e-10.

## Value

A non-negative numeric value representing the KL divergence. Returns 0
when the distributions are identical, and larger values indicate greater
divergence.

## Details

The KL divergence KL(P\|\|Q) is calculated as: \$\$KL(P\|\|Q) = \sum_i
P(x_i) \log(P(x_i) / Q(x_i))\$\$

where P represents the distribution from `samples1` and Q represents the
distribution from `samples2`.

The function uses weighted kernel density estimation to approximate the
continuous distributions from the discrete samples, then evaluates the
KL divergence using numerical integration.

Note that KL divergence is not symmetric: KL(P\|\|Q) ≠ KL(Q\|\|P).

## Examples

``` r
# Example 1: Compare two normal distributions
set.seed(123)
samples1 <- rnorm(1000, mean = 0, sd = 1)
samples2 <- rnorm(1000, mean = 0.5, sd = 1.2)
kl_div <- calc_kl_divergence(samples1, NULL, samples2, NULL)
print(paste("KL divergence:", round(kl_div, 4)))
#> [1] "KL divergence: 0.1338"

# Example 2: Using weighted samples
samples1 <- rnorm(500)
weights1 <- runif(500, 0.5, 1.5)
samples2 <- rnorm(500, mean = 1)
weights2 <- runif(500, 0.5, 1.5)
kl_div_weighted <- calc_kl_divergence(samples1, weights1, samples2, weights2)
#> Warning: Selecting bandwidth *not* using 'weights'
#> Warning: Selecting bandwidth *not* using 'weights'

# Example 3: Comparing posterior to prior in Bayesian analysis
# prior_samples <- rnorm(1000, mean = 0, sd = 2)  # Prior
# posterior_samples <- rnorm(1000, mean = 1, sd = 0.5)  # Posterior
# kl_div <- calc_kl_divergence(posterior_samples, NULL, prior_samples, NULL)
```
