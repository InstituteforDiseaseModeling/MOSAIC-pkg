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

The moment matching method estimates parameters from the mode and CI
width on the log scale, then adjusts for the lognormal relationship.

## Examples

``` r
# Example 1: Fit lognormal distribution
result <- fit_lognormal_from_ci(mode_val = 1,
                                 ci_lower = 0.5,
                                 ci_upper = 3)
print(result)
#> $meanlog
#> [1] 0.2089235
#> 
#> $sdlog
#> [1] 0.4570815
#> 
#> $mean
#> [1] 1.368048
#> 
#> $sd
#> [1] 0.6594373
#> 
#> $fitted_ci
#> [1] 0.5031134 3.0185807
#> 
#> $mode
#> [1] 1
#> 

# Example 2: Using optimization method
result <- fit_lognormal_from_ci(mode_val = 10,
                                 ci_lower = 2,
                                 ci_upper = 50,
                                 method = "optimization")
```
