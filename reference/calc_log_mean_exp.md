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

  Numeric vector of log-values. `-Inf` is a valid log-value (a zero
  likelihood) and enters the mean as `exp(-Inf) = 0`; `NA`/`NaN` entries
  (failed evaluations) are dropped.

## Value

Numeric scalar; the log-mean-exp of the non-missing entries. `-Inf` if
every non-missing entry is `-Inf`, `Inf` if any entry is `Inf`, and
`NA_real_` if no entry is non-missing.

## Details

The log-mean-exp for a vector \\x\\ is computed as: \$\$ \mathrm{LME}(x)
= \max(x) + \log\left(\frac{1}{\|x\|}\sum_i e^{x_i - \max(x)}\right)
\$\$

This is numerically stable because:

- The maximum value is subtracted before exponentiation, preventing
  overflow

- At least one exponentiated term equals 1, preventing underflow

- Equivalent to `log(mean(exp(x)))` but without numerical issues

Dropping `-Inf` instead would shrink the denominator \\\|x\|\\ and bias
the result upward: averaging one zero-likelihood replicate out of \\m\\
would gain \\\log(m/(m-1))\\.

## Examples

``` r
# Basic usage
x <- c(-100, -101, -99)
calc_log_mean_exp(x)  # -99.69
#> [1] -99.69101

# Compare with the naive form (underflows for very negative values)
log(mean(exp(x)))
#> [1] -99.69101

# -Inf counts as a zero likelihood; NA is dropped
calc_log_mean_exp(c(0, -Inf))        # log(0.5)
#> [1] -0.6931472
calc_log_mean_exp(c(-50, -60, NA))   # log-mean-exp of -50 and -60
#> [1] -50.6931

# Empty, all-missing, or all -Inf input
calc_log_mean_exp(c())           # NA
#> [1] NA
calc_log_mean_exp(c(NA, NaN))    # NA
#> [1] NA
calc_log_mean_exp(c(-Inf, -Inf)) # -Inf
#> [1] -Inf
```
