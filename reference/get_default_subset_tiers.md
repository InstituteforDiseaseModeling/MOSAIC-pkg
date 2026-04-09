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

**Example with defaults (target_ESS_best=500, target_A=0.95,
target_CVw=0.7):**

    Tier  1: ESS=500, A=0.950, CVw=0.700  (Stringent)
    Tier  5: ESS=500, A=0.773, CVw=0.817  (High quality)
    Tier 10: ESS=500, A=0.599, CVw=0.931  (Moderate quality)
    Tier 20: ESS=500, A=0.358, CVw=1.327  (Last ESS-constant)
    Tier 21: ESS=475, A=0.340, CVw=1.393  (Fallback begins)
    Tier 30: ESS=315, A=0.226, CVw=1.986  (Final fallback)

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
