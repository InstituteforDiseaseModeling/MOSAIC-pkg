# Alternative Theoretically-Grounded Approaches for Effective AIC Range

These functions demonstrate more principled approaches based on
statistical theory

## Usage

``` r
get_effective_aic_range_chisq(top_percentile, df = 2)

get_effective_aic_range_likelihood(top_percentile)

get_effective_aic_range_empirical(top_percentile)
```

## Arguments

- top_percentile:

  Numeric percentage (0-100) of top simulations

- df:

  Degrees of freedom for chi-square approach (default: 2)

## Examples

``` r
# Compare different approaches
percentiles <- c(1, 5, 10, 20)

# Current ad hoc formula
current <- sapply(percentiles, function(p) 2 + 2.3 * log10(p))

# Chi-square based
chisq_based <- sapply(percentiles, get_effective_aic_range_chisq)

# Relative likelihood based
likelihood_based <- sapply(percentiles, get_effective_aic_range_likelihood)
```
