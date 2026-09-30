# Fit Lognormal Distribution from Mode and 95% Confidence Intervals

This function calculates the meanlog and sdlog parameters of a lognormal
distribution that best matches a given mode and 95% confidence
intervals.

## Usage

``` r
fit_lognormal_from_ci(
  mode_val,
  ci_lower,
  ci_upper,
  method = "moment_matching",
  verbose = FALSE
)
```

## Arguments

- mode_val:

  Numeric. The mode of the distribution.

- ci_lower:

  Numeric. The lower bound of the 95% confidence interval.

- ci_upper:

  Numeric. The upper bound of the 95% confidence interval.

- method:

  Character. Method to use: "moment_matching" (default) or
  "optimization".

- verbose:

  Logical. If TRUE, print diagnostic information.

## Value

A list containing:

- meanlog: The mean of the logarithm (mu) of the lognormal distribution

- sdlog: The standard deviation of the logarithm (sigma) of the
  lognormal distribution

- mean: The mean of the distribution (not meanlog)

- sd: The standard deviation of the distribution (not sdlog)

- fitted_ci: The 95% CI of the fitted distribution

- mode: The mode of the fitted distribution

## Details

For a lognormal distribution with parameters meanlog (μ) and sdlog (σ):

- Mode = exp(μ - σ²)

- Mean = exp(μ + σ²/2)

- Variance = (exp(σ²) - 1) \* exp(2μ + σ²)

The moment matching method (default) matches the 95% CI exactly on the
log scale: a lognormal is fully determined by two quantiles, so
`meanlog` is the midpoint of `log(ci_lower)` and `log(ci_upper)` and
`sdlog` is their distance divided by `2 * qnorm(0.975)`. `mode_val` is
validated but does not move the fit; the implied mode
`exp(meanlog - sdlog^2)` is returned as `mode`. Anchoring on a
sample-based mode (for example a KDE mode of posterior draws) and then
adding `sdlog^2` shifted wide CIs upward by orders of magnitude, which
is why the CI is authoritative here.

The optimization method fits `meanlog` and `sdlog` jointly to the mode
and both quantiles, with all errors measured on the log scale and the
mode weighted 10x; use it when `mode_val` is a trusted anchor.

## Examples

``` r
# Example 1: Fit lognormal distribution
result <- fit_lognormal_from_ci(mode_val = 1,
                                 ci_lower = 0.5,
                                 ci_upper = 3)
print(result)
#> $meanlog
#> [1] 0.2027326
#> 
#> $sdlog
#> [1] 0.4570899
#> 
#> $mean
#> [1] 1.35961
#> 
#> $sd
#> [1] 0.6553832
#> 
#> $fitted_ci
#> [1] 0.5 3.0
#> 
#> $mode
#> [1] 0.9938206
#> 

# Example 2: Using optimization method
result <- fit_lognormal_from_ci(mode_val = 10,
                                 ci_lower = 2,
                                 ci_upper = 50,
                                 method = "optimization")
```
