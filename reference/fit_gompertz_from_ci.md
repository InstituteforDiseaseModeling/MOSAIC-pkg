# Fit Gompertz Distribution from Mode and Probability Interval

This function estimates the parameters of a Gompertz distribution on
\[0, Inf) with pdf f(x; b, eta) = b \* eta \* exp(b*x) \*
exp(-eta*(exp(b\*x) - 1)) so that its quantiles at `probs` match a
target interval (default: the central 95 percent).

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

  Numeric \>= 0. Reference mode, reported next to the fitted mode; it
  does not constrain the fit and need not lie inside the interval.

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

- fitted_mode: The mode of the fitted density, -log(eta)/b (0 when eta
  \>= 1)

- fitted_ci: Named vector of fitted quantiles at probs

- fitted_mean: Numerical estimate of the expected value via quadrature

- fitted_sd: Numerical estimate of the standard deviation via quadrature

- probs: The probability levels used

- input_mode: Echo of mode_val

- input_ci: Echo of c(lower = ci_lower, upper = ci_upper)

## Details

The two quantiles determine the distribution: the ratio \\Q(p_2)/Q(p_1)
= \log(1 + c_2/\eta) / \log(1 + c_1/\eta)\\, with \\c_k = -\log(1 -
p_k)\\, depends on \\\eta\\ alone and increases monotonically from 1
(\\\eta \to 0\\) to \\c_2/c_1\\ (\\\eta \to \infty\\, the exponential
limit; about 146 for the central 95 percent), so \\\eta\\ is solved from
the target ratio and \\b\\ from the scale. A ratio beyond that limit (or
`ci_lower = 0`) is matched as closely as the family allows, anchored on
`ci_upper`.

Setting the derivative of log f to zero gives the mode x\* = -log(eta) /
b, which is interior only when eta \< 1; for eta \>= 1 the density is
monotone decreasing and the mode is 0. `mode_val` does not constrain the
fit (a sample-based mode near zero is poorly determined) and may lie
outside the interval, as it does for a monotone-decreasing target whose
KDE mode falls below the lower quantile; the mode of the fitted density
is returned as `fitted_mode`.

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
#> [1] 0.03688879
#> 
#> $eta
#> [1] 1e+08
#> 
#> $f0
#> [1] 3688879
#> 
#> $fitted_mode
#> [1] 0
#> 
#> $fitted_ci
#>        lower        upper 
#> 6.863279e-09 1.000000e-06 
#> 
#> $fitted_mean
#> [1] 2.71081e-07
#> 
#> $fitted_sd
#> [1] 2.710592e-07
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
