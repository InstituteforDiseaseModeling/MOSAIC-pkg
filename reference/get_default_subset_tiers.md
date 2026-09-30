# Get Default Subset Tiers for Post-Hoc Optimization

Returns a structured list of tiered criteria for identifying optimal
subsets of calibration results. Uses a hybrid degradation strategy with
30 hardcoded tiers.

## Usage

``` r
get_default_subset_tiers(
  target_ESS_best = 500,
  target_A = 0.95,
  target_CVw = 0.7
)
```

## Arguments

- target_ESS_best:

  Numeric target ESS for the best subset (default: 500)

- target_A:

  Numeric target agreement index (default: 0.95)

- target_CVw:

  Numeric target coefficient of variation (default: 0.7)

## Value

A named list where each element contains:

- name: Tier identifier

- A: Target agreement index (0-1)

- CVw: Maximum coefficient of variation for weights

- ESS_B: Target effective sample size

## Details

The function generates 30 hardcoded tiers with a hybrid degradation
strategy:

**Tiers 1-20 (ESS-constant):**

- ESS_B remains constant at target_ESS_best

- A degrades by 5% per tier (multiplied by 0.95)

- CVw increases by 5% per tier (multiplied by 1.05)

- Prioritizes statistical power while relaxing quality criteria

**Tiers 21-30 (Fallback):**

- All three criteria degrade by 5% per tier

- ESS_B, A, CVw all relax simultaneously

- Graceful degradation when ESS target cannot be achieved

**Example with the function defaults (target_ESS_best=500,
target_A=0.95, target_CVw=0.7):**

    Tier  1: ESS=500, A=0.950, CVw=0.700  (Stringent)
    Tier  5: ESS=500, A=0.774, CVw=0.851
    Tier 10: ESS=500, A=0.599, CVw=1.086
    Tier 20: ESS=500, A=0.358, CVw=1.769  (Last ESS-constant)
    Tier 21: ESS=475, A=0.341, CVw=1.857  (Fallback begins)
    Tier 30: ESS=299, A=0.215, CVw=2.881  (Final fallback)

[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
calls this with its control targets (`ESS_best = 100`, `A_best = 0.70`,
`CVw_best = 1.0` by default), which gives:

    Tier  1: ESS=100, A=0.700, CVw=1.000
    Tier 20: ESS=100, A=0.264, CVw=2.527
    Tier 30: ESS=60,  A=0.158, CVw=4.116

## Examples

``` r
if (FALSE) { # \dontrun{
# Get default 30 tiers
tiers <- get_default_subset_tiers(
  target_ESS_best = 500,
  target_A = 0.95,
  target_CVw = 0.7
)

# Custom starting criteria
tiers_custom <- get_default_subset_tiers(
  target_ESS_best = 600,
  target_A = 0.90,
  target_CVw = 0.75
)

# Loop through tiers in calibration with grid search
for (tier_name in names(tiers)) {
  tier <- tiers[[tier_name]]
  result <- grid_search_best_subset(
    results = results,
    target_ESS = tier$ESS_B,
    target_A = tier$A,
    target_CVw = tier$CVw,
    min_size = 30,
    max_size = 1000
  )
  if (result$converged) break
}
} # }
```
