# Get Effective AIC Range for Gibbs Temperature Scaling

Calculates the expected delta AIC range for a given top percentile using
a continuous relationship based on log odds ratio.

## Usage

``` r
get_effective_aic_range(top_percentile)
```

## Arguments

- top_percentile:

  Numeric percentage (0-100) of top simulations

## Value

Numeric effective range for delta AIC normalization

## Details

The effective range represents the expected delta AIC span for the top
X% of models under typical (non-dispersed) conditions.

Uses a heuristic that maps percentiles to Burnham & Anderson's AIC
thresholds. Based on their interpretation:

- 1% → Δ AIC \< 2 (substantial support, ~37% relative likelihood)

- 5% → Δ AIC ≈ 4 (boundary of substantial/less support)

- 10% → Δ AIC ≈ 7 (upper end of "considerably less support")

- 20% → Δ AIC ≈ 10 (boundary to "essentially no support")

## Examples

``` r
get_effective_aic_range(1)   # 2.0
#> [1] 2
get_effective_aic_range(5)   # 4.0
#> [1] 4
get_effective_aic_range(10)  # 7.0
#> [1] 7
```
