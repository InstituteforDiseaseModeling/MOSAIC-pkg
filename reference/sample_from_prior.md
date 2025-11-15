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
  parameters\$mean, parameters\$sd

- **normal**: parameters\$mean, parameters\$sd

- **truncnorm**: parameters\$mean, parameters\$sd, parameters\$a (lower
  bound), parameters\$b (upper bound)

- **uniform**: parameters\$min, parameters\$max

- **discrete_uniform**: parameters\$min, parameters\$max (returns
  integers)

- **gompertz**: parameters\$b, parameters\$eta

## Examples

``` r
# Sample from a beta distribution
prior_beta <- list(
  distribution = "beta",
  parameters = list(shape1 = 2, shape2 = 5)
)
sample_from_prior(n = 10, prior = prior_beta)
#>  [1] 0.27809463 0.10940744 0.32936690 0.07684005 0.21119152 0.32513663
#>  [7] 0.44467714 0.48580487 0.37734983 0.09207730

# Sample from a truncated normal distribution
prior_truncnorm <- list(
  distribution = "truncnorm",
  parameters = list(mean = 0, sd = 1, a = -2, b = 2)
)
sample_from_prior(n = 10, prior = prior_truncnorm)
#>  [1]  1.6883161  0.8298259 -0.3973745  0.3027850 -0.7890177  0.6191767
#>  [7]  0.2162437  1.2112046  0.4851827 -1.9091562
```
