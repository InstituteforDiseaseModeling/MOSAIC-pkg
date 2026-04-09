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
#>  [1] 0.58842776 0.47125524 0.41759665 0.10247331 0.37815582 0.10637791
#>  [7] 0.49731949 0.07843191 0.38134901 0.21904204

# Sample from a truncated normal distribution
prior_truncnorm <- list(
  distribution = "truncnorm",
  parameters = list(mean = 0, sd = 1, a = -2, b = 2)
)
sample_from_prior(n = 10, prior = prior_truncnorm)
#>  [1]  0.361730175 -0.660147606  0.165218882 -1.683158923 -0.366686670
#>  [6]  1.770533918  0.459259631 -0.288203653  1.322119592 -0.003683303
```
