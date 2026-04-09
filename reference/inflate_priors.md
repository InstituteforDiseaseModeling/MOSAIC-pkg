# Inflate Prior or Posterior Distributions by a Variance Factor

Takes a priors or posteriors list object and inflates the variance of
each distribution by a multiplicative factor while preserving the mean.
This is used in staged Bayesian estimation to broaden tight posteriors
from one stage before passing them as priors to the next stage,
preventing weight collapse in importance sampling (particle filter /
BFRS) when consecutive stages target different objectives.

The technique is known as kernel smoothing, covariance inflation, or
variance tempering in the Sequential Monte Carlo literature (Liu & West
2001; Del Moral et al. 2006).

## Usage

``` r
inflate_priors(
  priors,
  inflation_factor = 2,
  params = NULL,
  n_samples = 10000L,
  verbose = TRUE
)
```

## Arguments

- priors:

  List. A priors or posteriors object with `parameters_global` and
  `parameters_location` slots.

- inflation_factor:

  Numeric scalar \> 0. Variance multiplier. `2.0` doubles variance while
  preserving the mean. Default 2.0.

- params:

  Optional character vector. Base parameter names to inflate. When
  `NULL` all non-fixed/frozen parameters are inflated.

- n_samples:

  Integer. Number of samples for empirical fallback methods (truncnorm
  fallback, gompertz). Default 10000L.

- verbose:

  Logical. Print per-parameter inflation messages. Default TRUE.

## Value

A priors list with inflated distribution parameters. Metadata updated.

## Methods

Each distribution is inflated using the most appropriate method:

**Analytic (exact):**

- beta(shape1, shape2):

  Reduce precision `s = α+β` by factor `f`. Maximum inflatable factor is
  `f_max = s+1`.

- gamma(shape, rate):

  `shape_new = shape/f, rate_new = rate/f`.

- lognormal(meanlog, sdlog):

  CV² scaled by `f`; `meanlog` adjusted to preserve natural-scale mean.

- lognormal(mean, sd):

  `sd_new = sqrt(f)*sd`.

- normal(mean, sd):

  `sd_new = sqrt(f)*sd`.

- uniform(min, max):

  Bounds extended symmetrically around midpoint. Skipped with a warning
  if new bounds violate positivity.

**Analytic with empirical fallback:**

- truncnorm(mean, sd, a, b):

  Root-finding preserves actual truncated mean. Due to active bounds,
  the achieved variance ratio may be less than `inflation_factor`. Falls
  back to empirical refit if root-find fails.

**Empirical (sample → inflate → refit):**

- gompertz(b, eta):

  No closed-form moments. Sampled empirically, variance inflated,
  distribution refit via MLE.

- Any other distribution:

  Universal empirical fallback.

## See also

[`update_priors_from_posteriors`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/update_priors_from_posteriors.md)
