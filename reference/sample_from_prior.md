# Sample from Prior Distribution (Simplified)

A simplified unified function to sample values from prior distributions.
Expects all priors to have consistent structure with \$distribution and
\$parameters slots.

## Usage

``` r
sample_from_prior(n = 1, prior, verbose = FALSE)
```

## Arguments

- n:

  Integer. Number of samples to draw (default: 1).

- prior:

  List. Prior object containing 'distribution' and 'parameters' slots.

- verbose:

  Logical. If TRUE, print warnings for NULL or invalid priors (default:
  FALSE).

## Value

Numeric vector of length n containing samples from the prior
distribution. Returns NA values if prior is NULL/invalid.

## Details

This simplified version expects every prior to have a consistent
structure:

- `$distribution`: Character string naming the distribution type

- `$parameters`: List containing the distribution parameters

Supported distributions:

- **beta**: parameters\$shape1, parameters\$shape2

- **gamma**: parameters\$shape, parameters\$rate

- **lognormal**: parameters\$meanlog, parameters\$sdlog OR
  parameters\$mean, parameters\$sd; optional parameters\$lower /
  parameters\$upper truncate the draw (meanlog/sdlog then describe the
  untruncated distribution)

- **normal**: parameters\$mean, parameters\$sd

- **truncnorm**: parameters\$mean, parameters\$sd, parameters\$a (lower
  bound), parameters\$b (upper bound)

- **uniform**: parameters\$min, parameters\$max

- **discrete_uniform**: parameters\$min, parameters\$max (returns
  integers)

- **gompertz**: parameters\$b, parameters\$eta

- **fixed**: parameters\$value (genuinely converged, returns constant)

- **frozen**: parameters\$value (frozen in previous stage, returns
  constant)

## Examples

``` r
# Sample from a beta distribution
prior_beta <- list(
  distribution = "beta",
  parameters = list(shape1 = 2, shape2 = 5)
)
sample_from_prior(n = 10, prior = prior_beta)
#>  [1] 0.19007042 0.72965473 0.48529669 0.43924840 0.11377246 0.26857255
#>  [7] 0.15743964 0.15911719 0.26804349 0.08400777

# Sample from a truncated normal distribution
prior_truncnorm <- list(
  distribution = "truncnorm",
  parameters = list(mean = 0, sd = 1, a = -2, b = 2)
)
sample_from_prior(n = 10, prior = prior_truncnorm)
#>  [1] -1.08609755 -0.68370851 -1.51885919 -0.29066904  0.24961982  1.49934126
#>  [7]  0.43712317 -0.90208633  0.01721895  0.22281779
```
