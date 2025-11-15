# Get NPE Importance Weights

Retrieves or creates importance weights for NPE training from BFRS
results. This function selects the appropriate pre-calculated BFRS
weights or creates binary versions of them, avoiding recalculation
issues.

## Usage

``` r
get_npe_weights(bfrs_results, strategy = "binary_best", verbose = FALSE)
```

## Arguments

- bfrs_results:

  BFRS results object containing weight_retained and weight_best

- strategy:

  Weight strategy: "continuous_all" - Gibbs weights for all simulations
  (effective AIC range = 25) "binary_all" - Equal weights for all
  simulations "continuous_retained" - Use BFRS continuous weights for
  retained subset "binary_retained" - Equal weights for all retained
  simulations "continuous_best" - Use BFRS continuous weights for best
  subset "binary_best" - Equal weights for all best subset simulations

- verbose:

  Print weight statistics

## Value

Vector of normalized weights
