# Fit Truncated Normal Distribution from Mode and 95% Confidence Intervals

This function calculates the mean, sd, and truncation bounds of a
truncated normal distribution that best matches a given mode and 95%
confidence intervals.

## Usage

``` r
fit_truncnorm_from_ci(
  mode_val,
  ci_lower,
  ci_upper,
  a = NULL,
  b = NULL,
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

- a:

  Numeric. Lower truncation bound (default: -Inf).

- b:

  Numeric. Upper truncation bound (default: Inf).

- method:

  Character. Method to use: "moment_matching" (default) or
  "optimization".

- verbose:

  Logical. If TRUE, print diagnostic information.

## Value

A list containing:

- mean: The mean parameter (mu) of the underlying normal distribution

- sd: The standard deviation parameter (sigma) of the underlying normal
  distribution

- a: The lower truncation bound

- b: The upper truncation bound

- fitted_mean: The mean of the truncated distribution

- fitted_ci: The 95% CI of the fitted truncated distribution

- mode: The mode of the fitted truncated distribution

## Details

For a truncated normal distribution:

- The mode may differ from the mean due to truncation

- If truncation is symmetric and far from the mean, mode ≈ mean

- Asymmetric truncation shifts the mode away from the mean

If truncation bounds are not specified, they are estimated from the CI,
extending slightly beyond to allow for a proper truncated distribution.

## Examples

``` r
# Example 1: Fit truncated normal with specified bounds
result <- fit_truncnorm_from_ci(mode_val = 0.5,
                                 ci_lower = 0.2,
                                 ci_upper = 0.8,
                                 a = 0, b = 1)
print(result)
#> $mean
#> [1] 0.5
#> 
#> $sd
#> [1] 0.127551
#> 
#> $a
#> [1] 0
#> 
#> $b
#> [1] 1
#> 
#> $fitted_mean
#> [1] 0.4990757
#> 
#> $fitted_ci
#> [1] 0.2500963 0.7499037
#> 
#> $mode
#> [1] 0.499617
#> 

# Example 2: Auto-detect truncation bounds
result <- fit_truncnorm_from_ci(mode_val = 5,
                                 ci_lower = 2,
                                 ci_upper = 8)
```
