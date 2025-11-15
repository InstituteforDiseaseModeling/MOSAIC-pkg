# Calculate NPE Importance Weights (Deprecated)

DEPRECATED: Use get_npe_weights() for BFRS-based weights or direct
assignment. This function calculates importance weights from scratch,
which can lead to inconsistencies with BFRS weights.

Calculates importance weights for NPE training. Can either compute new
weights from likelihood values or use pre-calculated BFRS weights.

## Usage

``` r
calc_npe_weights(
  likelihoods = NULL,
  strategy = "continuous",
  bfrs_results = NULL,
  temperature = NULL,
  effective_range = 30,
  verbose = FALSE
)
```

## Arguments

- likelihoods:

  Vector of likelihood values from BFRS (optional if using BFRS weights)

- strategy:

  Weight strategy: "binary", "continuous", "uniform",
  "continuous_retained", "binary_retained", "continuous_best",
  "binary_best"

- bfrs_results:

  BFRS results object containing weight_retained and weight_best
  (required for BFRS weight strategies)

- temperature:

  Temperature parameter for continuous weights (if NULL, computed
  automatically)

- effective_range:

  Effective AIC range for temperature calculation (default 25)

- verbose:

  Print weight statistics

## Value

Vector of normalized weights
