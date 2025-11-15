# Fit Normal Distribution from Mode and 95% Confidence Intervals

This function calculates the mean and standard deviation parameters of a
normal distribution that best matches a given mode and 95% confidence
intervals.

## Usage

``` r
fit_normal_from_ci(
  mode_val,
  ci_lower,
  ci_upper,
  method = "moment_matching",
  verbose = FALSE
)
```

## Arguments

- mode_val:

  Numeric. The mode of the distribution (equals mean for normal).

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

- mean: The mean parameter (mu) of the normal distribution

- sd: The standard deviation parameter (sigma) of the normal
  distribution

- fitted_ci: The 95% CI of the fitted distribution

- mode: The mode of the fitted distribution (equals mean)

## Details

For a normal distribution:

- The mode equals the mean

- The 95% CI is approximately mean ± 1.96\*sd

- The distribution is symmetric around the mean

The moment matching method uses the fact that for a normal distribution,
the 95% CI width is approximately 3.92 standard deviations.

## Examples

``` r
# Example 1: Fit normal distribution
result <- fit_normal_from_ci(mode_val = 0,
                              ci_lower = -2,
                              ci_upper = 2)
print(result)
#> $mean
#> [1] 0
#> 
#> $sd
#> [1] 1.020408
#> 
#> $fitted_ci
#> [1] -1.999963  1.999963
#> 
#> $mode
#> [1] 0
#> 

# Example 2: Using optimization method
result <- fit_normal_from_ci(mode_val = 5,
                              ci_lower = 2,
                              ci_upper = 8,
                              method = "optimization")
```
