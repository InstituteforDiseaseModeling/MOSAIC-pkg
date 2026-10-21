# Kullback-Leibler divergence of a weighted sample from a reference sample

Computes KL(P \|\| Q), how far the distribution P of `samples1`
(weighted by `weights1`) has moved from the reference distribution Q of
`samples2` (weighted by `weights2`), in nats. In MOSAIC, P is a
parameter's posterior and Q its prior, both represented by draws. The
estimate is made on Q's own percentile scale, so it does not depend on
the parameter's units, on heavy prior tails, on integer values or on
bounds.

## Usage

``` r
calc_kl_divergence(
  samples1,
  weights1 = NULL,
  samples2,
  weights2 = NULL,
  n_points = 1000,
  eps = 1e-10,
  n_bins = 20L,
  seed = 1L
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

  Numeric vector of samples from P (e.g. the posterior).

- weights1:

  Non-negative weights for `samples1`, same length. If NULL, uniform
  weights are used.

- samples2:

  Numeric vector of samples from Q (e.g. the prior).

- weights2:

  Non-negative weights for `samples2`, same length. If NULL, uniform
  weights are used.

- n_points:

  Ignored since MOSAIC 1.4.0 (kept so existing calls work; a warning is
  issued when supplied).

- eps:

  Ignored since MOSAIC 1.4.0 (as `n_points`).

- n_bins:

  Number of equal-probability bins on Q's percentile scale. Default 20.

- seed:

  Integer seed of the random tie-breaking. The caller's RNG stream is
  restored afterwards. Default 1.

## Value

A non-negative numeric value, at most `log(n_bins)`: about 0 when P
equals Q. `NA` with a warning when the Kish effective sample size of
`weights1` is below `n_bins`; 0 with a warning when all samples are
equal.

## Details

**Percentile-scale (PIT) estimator.** Each draw of P is mapped to its
percentile under Q, \\u = F_Q(x)\\. Under Q itself \\u\\ is uniform, so
the KL divergence is unchanged by this monotone map and becomes the
divergence of the distribution of \\u\\ from U(0, 1). The unit interval
is cut into \\B\\ = `n_bins` equal bins, \\p_b\\ is the P-weight in bin
\\b\\, and \$\$\widehat{KL} = \max\left(0,\\ \sum_b p_b \log(p_b B) -
\frac{B - 1}{2\\ESS_P}\right),\$\$ where \\ESS_P = (\sum w)^2 / \sum
w^2\\ is the Kish effective sample size of `weights1`. The subtracted
term is the leading finite-sample bias of the plug-in divergence of a
\\B\\-cell histogram (the Miller-Madow correction), so an unmoved
posterior scores about 0 rather than \\(B - 1)/(2 ESS_P)\\.

**Percentiles.** When `samples1` and `samples2` are the same draws (the
posterior is a reweighting of the prior draws), the percentile of a draw
is the midpoint of its cumulative Q-weight after ordering by value, ties
in random order; with uniform `weights2` this is
`(rank(x, ties.method = "random") - 0.5) / n`. Otherwise it is the
randomised probability integral transform against Q's weighted empirical
CDF, \\u = F_Q(x^-) + U\\\[F_Q(x) - F_Q(x^-)\]\\ with \\U \sim U(0,
1)\\. Random tie-breaking spreads an integer-valued or point-mass
parameter evenly over the percentiles its atom occupies.

**Range.** Binning can only lose information, so the estimate is at most
\\\log B\\ (3.0 nats at \\B = 20\\), reached when P falls into a single
bin (1/\\B\\ of Q's mass); it ranks parameters by how much they moved up
to that ceiling. It is computed when \\ESS_P \ge B\\; below that `NA` is
returned with a warning. P-mass outside Q's range is counted in the
outer bins. Near \\ESS_P = B\\ the correction leaves a positive bias for
an unmoved posterior (about 0.05 nats at \\ESS_P = 40\\), so MOSAIC
reports the descriptive bands (`.mosaic_kl_band()`) only from \\ESS_P
\ge 100\\.

Before MOSAIC 1.4.0 both densities were weighted kernel density
estimates integrated on a grid. That estimator failed for heavy-tailed
priors (bandwidth set by the tail), integer-valued parameters (spikes)
and priors bounded near the posterior (boundary bias), and was
unbounded; values from the two estimators are not comparable.

Note that KL divergence is not symmetric: KL(P\|\|Q) != KL(Q\|\|P).

## Examples

``` r
# Posterior as a reweighting of prior draws
set.seed(123)
prior <- rnorm(20000)
w <- dnorm(prior, mean = 0.5, sd = 0.5) / dnorm(prior)   # target N(0.5, 0.5^2)
calc_kl_divergence(prior, w, prior, NULL)
#> [1] 0.4366259
log(1 / 0.5) + (0.5^2 + 0.5^2) / 2 - 0.5                  # analytic KL
#> [1] 0.4431472

# Two separate samples
calc_kl_divergence(rnorm(5000, 1, 0.5), NULL, rnorm(5000), NULL)
#> [1] 0.7832714
```
