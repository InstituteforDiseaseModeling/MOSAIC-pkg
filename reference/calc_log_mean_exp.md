# Stable log-mean-exp computation

Computes the log of the mean of exponentials in a numerically stable
way. This is equivalent to log(mean(exp(x))) but avoids numerical
overflow/underflow by subtracting the maximum value before
exponentiation.

## Usage

``` r
calc_log_mean_exp(x)
```

## Arguments

- x:

  Numeric vector of log-values

## Value

Numeric scalar; the log-mean-exp of the input vector

## Details

The log-mean-exp for a vector \\x\\ is computed as: \$\$ \mathrm{LME}(x)
= \max(x) + \log\left(\frac{1}{\|x\|}\sum_i e^{x_i - \max(x)}\right)
\$\$

This is numerically stable because:

- The maximum value is subtracted before exponentiation, preventing
  overflow

- At least one exponentiated term equals 1, preventing underflow

- Equivalent to `log(mean(exp(x)))` but without numerical issues

## See also

calc_model_collapse_iterations which uses this for likelihood
aggregation

## Examples

``` r
# Basic usage
x <- c(-100, -101, -99)
calc_log_mean_exp(x)  # ≈ -99.59
#> [1] -99.69101

# Compare with naive approach (which would overflow for large negative values)
log(mean(exp(x)))     # Same result, but less stable
#> [1] -99.69101

# With missing/infinite values
calc_log_mean_exp(c(-50, -Inf, -60, NA))  # Returns log-mean-exp of finite values
#> [1] -50.6931

# Empty or all non-finite input
calc_log_mean_exp(c())           # Returns NA
#> [1] NA
calc_log_mean_exp(c(-Inf, NA))   # Returns NA
#> [1] NA
```
