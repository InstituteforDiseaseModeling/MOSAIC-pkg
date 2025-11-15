# Fit Beta Distribution from Mode and 95% Confidence Intervals

This function calculates the shape parameters (alpha and beta) of a beta
distribution that best matches a given mode and 95% confidence
intervals.

## Usage

``` r
fit_beta_from_ci(
  mode_val,
  ci_lower,
  ci_upper,
  method = "moment_matching",
  verbose = FALSE
)
```

## Arguments

- mode_val:

  Numeric. The mode of the distribution (must be in (0,1)).

- ci_lower:

  Numeric. The lower bound of the 95% confidence interval (must be in
  (0,1)).

- ci_upper:

  Numeric. The upper bound of the 95% confidence interval (must be in
  (0,1)).

- method:

  Character. Method to use: "moment_matching" (default) or
  "optimization".

- verbose:

  Logical. If TRUE, print diagnostic information.

## Value

A list containing:

- shape1: The alpha shape parameter of the beta distribution

- shape2: The beta shape parameter of the beta distribution

- fitted_mode: The mode of the fitted distribution

- fitted_mean: The mean of the fitted distribution

- fitted_var: The variance of the fitted distribution

- fitted_ci: The 95% CI of the fitted distribution

- input_mode: The input mode value

- input_ci: The input confidence interval

## Examples

``` r
# Example 1: Fit beta for phi_1 (vaccine effectiveness)
result <- fit_beta_from_ci(mode_val = 0.788, 
                            ci_lower = 0.753, 
                            ci_upper = 0.822)
print(result)
#> $shape1
#> [1] 446.3372
#> 
#> $shape2
#> [1] 120.8115
#> 
#> $fitted_mode
#> [1] 0.788
#> 
#> $fitted_mean
#> [1] 0.7869844
#> 
#> $fitted_var
#> [1] 0.0002950635
#> 
#> $fitted_sd
#> [1] 0.01717741
#> 
#> $fitted_ci
#>     lower     upper 
#> 0.7523816 0.8196704 
#> 
#> $input_mode
#> [1] 0.788
#> 
#> $input_ci
#> lower upper 
#> 0.753 0.822 
#> 

# Example 2: Using optimization method
result <- fit_beta_from_ci(mode_val = 0.65, 
                            ci_lower = 0.50, 
                            ci_upper = 0.78,
                            method = "optimization")
```
