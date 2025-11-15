# Fit Gamma Distribution from Mode and 95% Confidence Intervals

This function calculates the shape and rate parameters of a gamma
distribution that best matches a given mode and 95% confidence
intervals.

## Usage

``` r
fit_gamma_from_ci(
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

- shape: The shape parameter (alpha) of the gamma distribution

- rate: The rate parameter (beta) of the gamma distribution

- scale: The scale parameter (1/rate) of the gamma distribution

- fitted_mean: The mean of the fitted distribution

- fitted_ci: The 95% CI of the fitted distribution

- mode: The mode of the fitted distribution (if shape \> 1)

## Examples

``` r
# Example 1: Fit gamma for omega_1
result <- fit_gamma_from_ci(mode_val = 0.000705, 
                             ci_lower = 0.000471, 
                             ci_upper = 0.001107)
print(result)
#> $shape
#> [1] 28.31043
#> 
#> $rate
#> [1] 38738.2
#> 
#> $scale
#> [1] 2.581431e-05
#> 
#> $fitted_mode
#> [1] 0.000705
#> 
#> $fitted_mean
#> [1] 0.0007308143
#> 
#> $fitted_sd
#> [1] 0.0001373516
#> 
#> $fitted_ci
#>        lower        upper 
#> 0.0004868256 0.0010235775 
#> 
#> $input_mode
#> [1] 0.000705
#> 
#> $input_ci
#>    lower    upper 
#> 0.000471 0.001107 
#> 

# Example 2: Using optimization method
result <- fit_gamma_from_ci(mode_val = 0.000337, 
                             ci_lower = 0.0000984, 
                             ci_upper = 0.000992,
                             method = "optimization")
```
