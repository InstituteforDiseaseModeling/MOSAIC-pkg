# Fit Uniform Distribution from Mode and 95% Confidence Intervals

This function calculates the min and max parameters of a uniform
distribution that best matches a given mode and 95% confidence
intervals.

## Usage

``` r
fit_uniform_from_ci(
  mode_val,
  ci_lower,
  ci_upper,
  method = "exact",
  extend_factor = 1.026,
  verbose = FALSE
)
```

## Arguments

- mode_val:

  Numeric. The mode of the distribution (any value within range for
  uniform).

- ci_lower:

  Numeric. The lower bound of the 95% confidence interval.

- ci_upper:

  Numeric. The upper bound of the 95% confidence interval.

- method:

  Character. Method to use: "exact" (default) or "extended".

- extend_factor:

  Numeric. Factor to extend bounds beyond CI (default: 1.026 for exact
  95% CI).

- verbose:

  Logical. If TRUE, print diagnostic information.

## Value

A list containing:

- min: The minimum parameter of the uniform distribution

- max: The maximum parameter of the uniform distribution

- mean: The mean of the fitted distribution (min + max)/2

- sd: The standard deviation of the fitted distribution

- fitted_ci: The 95% CI of the fitted distribution

- mode: The mode (undefined for uniform, returns NA)

## Details

For a uniform distribution U(min, max):

- Mean = (min + max) / 2

- Variance = (max - min)² / 12

- Mode is undefined (any value in \code\[min, max\] is equally likely)

- 95% CI covers central 95% of the range

The "exact" method sets bounds so that the 95% CI exactly matches the
input CI. For U(a, b), the 2.5th percentile is a + 0.025\*(b-a) and the
97.5th percentile is a + 0.975\*(b-a).

The "extended" method allows for bounds slightly beyond the CI for a
more conservative estimate.

## Examples

``` r
# Example 1: Exact fit to CI
result <- fit_uniform_from_ci(mode_val = 0.5,
                               ci_lower = 0.1,
                               ci_upper = 0.9)
print(result)
#> $min
#> [1] 0.07894737
#> 
#> $max
#> [1] 0.9210526
#> 
#> $mean
#> [1] 0.5
#> 
#> $sd
#> [1] 0.2430949
#> 
#> $fitted_ci
#> [1] 0.1 0.9
#> 
#> $mode
#> [1] NA
#> 

# Example 2: Extended bounds for conservative estimate
result <- fit_uniform_from_ci(mode_val = 5,
                               ci_lower = 2,
                               ci_upper = 8,
                               method = "extended",
                               extend_factor = 1.05)
```
