# Get Default Subset Tiers for Post-Hoc Optimization

Returns a structured list of tiered criteria for identifying optimal
subsets of calibration results. Each tier provides progressively relaxed
criteria to ensure graceful degradation when stringent criteria cannot
be met.

## Usage

``` r
get_default_subset_tiers(
  target_ESS_best = 100,
  target_A = 0.95,
  target_CVw = 0.5,
  n_tiers = 15
)
```

## Arguments

- target_ESS_best:

  Numeric target ESS for the best subset (default: 100) Starting ESS
  value that degrades by 10% each tier.

- target_A:

  Numeric target agreement index (default: 0.95) Starting A value that
  degrades by 10% each tier.

- target_CVw:

  Numeric target coefficient of variation (default: 0.5) Starting CVw
  value that increases by 10% each tier (more dispersed).

- n_tiers:

  Integer number of tiers to generate (default: 15) Each tier relaxes
  all three criteria by 10%.

## Value

A named list where each element contains:

- name: Tier identifier

- A: Target agreement index (0-1)

- CVw: Maximum coefficient of variation for weights

- ESS_B: Target effective sample size

## Details

The function generates tiers with **simultaneous 10% degradation** of
all three criteria:

**Degradation Rules:**

- **A (agreement)**: Multiplied by 0.9 each tier (10% decrease = less
  agreement required)

- **CVw (concentration)**: Multiplied by 1.1 each tier (10% increase =
  more dispersed weights allowed)

- **ESS_B (effective sample size)**: Multiplied by 0.9 each tier (10%
  decrease = fewer samples required)

**Example with defaults (target_A=0.95, target_CVw=0.5,
target_ESS_best=100, n_tiers=15):**

    Tier  1: A=0.950, CVw=0.500, ESS=100.0  (Stringent)
    Tier  2: A=0.855, CVw=0.550, ESS=90.0   (Very high)
    Tier  3: A=0.770, CVw=0.605, ESS=81.0   (High)
    Tier  4: A=0.693, CVw=0.666, ESS=72.9   (Good-moderate)
    Tier  5: A=0.623, CVw=0.732, ESS=65.6   (Moderate)
    ...
    Tier 15: A=0.206, CVw=1.922, ESS=20.6   (Minimal)

This provides smooth, consistent degradation across all criteria
simultaneously, eliminating the need for separate tier groups.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get default tiers (15 tiers with 10% degradation)
tiers <- get_default_subset_tiers(
  target_ESS_best = 100,
  target_A = 0.95,
  target_CVw = 0.5,
  n_tiers = 15
)

# More aggressive degradation (20 tiers)
tiers_wide <- get_default_subset_tiers(n_tiers = 20)

# Custom starting criteria
tiers_custom <- get_default_subset_tiers(
  target_ESS_best = 150,
  target_A = 0.90,
  target_CVw = 0.75,
  n_tiers = 10
)

# Loop through tiers in calibration
for (tier_name in names(tiers)) {
  tier <- tiers[[tier_name]]
  result <- identify_best_subset(
    results = results,
    target_ESS_B = tier$ESS_B,
    target_A = tier$A,
    target_CVw = tier$CVw
  )
  if (result$converged) break
}
} # }
```
