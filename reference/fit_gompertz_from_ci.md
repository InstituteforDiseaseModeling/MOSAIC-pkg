# Fit Gompertz Distribution from Mode and Probability Interval

This function estimates the parameters of a Gompertz distribution on
\[0, Inf) with pdf f(x; b, eta) = b \* eta \* exp(b*x) \*
exp(-eta*(exp(b*x) - 1)), given a target interior mode and a two-sided
interval (default: central 95 percent). The interior mode condition is
enforced by eta = b \* exp(b*mode), which holds exactly for the Gompertz
mode when eta \> b.

## Usage

``` r
fit_gompertz_from_ci(
  mode_val,
  ci_lower,
  ci_upper,
  probs = c(0.025, 0.975),
  verbose = FALSE
)
```

## Arguments

- mode_val:

  Numeric greater than 0. Target mode of the distribution (very near
  zero is allowed).

- ci_lower:

  Numeric greater than or equal to 0. Lower bound of the target interval
  (e.g., 2.5 percent quantile).

- ci_upper:

  Numeric greater than ci_lower. Upper bound of the target interval
  (e.g., 97.5 percent quantile).

- probs:

  Numeric length-2 vector in (0, 1). Probability levels for the target
  bounds. Defaults to c(0.025, 0.975).

- verbose:

  Logical. If TRUE, prints a diagnostic summary.

## Value

A list containing:

- b: Gompertz shape parameter

- eta: Gompertz rate parameter

- f0: Density at zero (finite and positive)

- fitted_mode: The implied mode (matches mode_val up to numeric error)

- fitted_ci: Named vector of fitted quantiles at probs

- fitted_mean: Numerical estimate of the expected value via quadrature

- fitted_sd: Numerical estimate of the standard deviation via quadrature

- probs: The probability levels used

- input_mode: Echo of mode_val

- input_ci: Echo of c(lower = ci_lower, upper = ci_upper)

## Examples

``` r
# Example: Fit Gompertz for small positive quantity
result <- fit_gompertz_from_ci(
  mode_val = 1e-8,
  ci_lower = 1e-9,
  ci_upper = 1e-6,
  probs = c(0.025, 0.975)
)
print(result)
#> $b
#> [1] 1919.758
#> 
#> $eta
#> [1] 1919.795
#> 
#> $f0
#> [1] 3685543
#> 
#> $fitted_mode
#> [1] 1e-08
#> 
#> $fitted_ci
#>        lower        upper 
#> 6.869447e-09 9.999450e-07 
#> 
#> $fitted_mean
#> [1] 2.711853e-07
#> 
#> $fitted_sd
#> [1] 2.710226e-07
#> 
#> $probs
#> [1] 0.025 0.975
#> 
#> $input_mode
#> [1] 1e-08
#> 
#> $input_ci
#> lower upper 
#> 1e-09 1e-06 
#> 
```
