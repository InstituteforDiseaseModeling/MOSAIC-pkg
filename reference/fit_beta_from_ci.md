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

## Details

`"moment_matching"` (default) keeps the mode exact: every Beta with both
shapes above 1 and mode \\m\\ is \\\mathrm{Beta}(1 + mk, 1 + (1 - m)k)\\
for some concentration \\k \> 0\\, and \\k\\ is chosen to minimise the
squared error of the fitted 2.5% and 97.5% quantiles against the CI on
the logit scale. Logit-scale errors are relative errors for small
proportions, so a CI around 1e-6 is matched as closely as one around
0.5. When the CI is wider than any unimodal Beta with that mode allows,
the widest achievable interval is returned. Because errors are relative,
a bound far outside what a Beta with this mode can reach (for example a
lower bound clamped to 1e-10 after a linear widening) dominates the fit
and pulls the mean up; pass a CI the family can represent, or refit
samples by their moments instead. `mode_val` is a mode: to anchor a
mean, use a mean-based fit.

`"optimization"` fits both shapes freely to the mode and the two
quantiles (all on the logit scale, mode weighted 100x), so the mode is
matched closely but not exactly.

## Examples

``` r
# Example 1: Fit beta for phi_1 (vaccine effectiveness)
result <- fit_beta_from_ci(mode_val = 0.788, 
                            ci_lower = 0.753, 
                            ci_upper = 0.822)
print(result)
#> $shape1
#> [1] 419.8812
#> 
#> $shape2
#> [1] 113.6939
#> 
#> $fitted_mode
#> [1] 0.788
#> 
#> $fitted_mean
#> [1] 0.7869205
#> 
#> $fitted_var
#> [1] 0.0003136633
#> 
#> $fitted_sd
#> [1] 0.01771054
#> 
#> $fitted_ci
#>     lower     upper 
#> 0.7512149 0.8205891 
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
