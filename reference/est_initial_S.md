# Estimate Initial S Compartment from Other Compartment Priors

Estimates the initial proportion of the population in the Susceptible
(S) compartment using the constraint S + V1 + V2 + E + I + R = 1. This
function samples from the estimated priors of other compartments and
calculates S as the constrained residual, ensuring mathematical
consistency while propagating uncertainty from all compartments.

## Usage

``` r
est_initial_S(
  PATHS,
  priors,
  config,
  n_samples = 1000,
  t0 = NULL,
  variance_inflation = 0,
  verbose = TRUE,
  min_S_proportion = 0.01,
  seed = NULL
)
```

## Arguments

- PATHS:

  List containing paths to data directories (for compatibility)

- priors:

  Prior distributions object containing estimated priors for V1, V2, E,
  I, R

- config:

  Configuration object containing location codes

- n_samples:

  Integer, number of Monte Carlo samples for uncertainty quantification
  (default 1000)

- t0:

  Date object, target date for estimation (default NULL, used for
  metadata)

- variance_inflation:

  Multiplier on the SD of the S samples in the method-of-moments Beta
  refit, which keeps the sample mean (default 0 = no change; 1 is also
  no change). A scalar or a named per-ISO vector; the variance scales
  with its square (2 gives 4x) and values in (0, 1) tighten the prior.

- verbose:

  Logical, whether to print progress messages (default TRUE)

- min_S_proportion:

  Numeric, minimum allowed S proportion to prevent negative values
  (default 0.01 = 1%)

- seed:

  Optional integer seed. When given, each location's Monte Carlo draws
  use a seed derived from `seed` and the ISO code, so results are
  reproducible; the caller's RNG state is restored. NULL (default) draws
  from the session RNG.

## Value

List with priors_default-compatible structure containing:

- metadata:

  Information about the estimation run including summary statistics

- parameters_location:

  Parameter hierarchy matching priors_default:

  - prop_S_initial: Beta distribution parameters for S/N by location

  Each parameter contains \$parameters\$location\$ISO_CODE with shape1,
  shape2 and a `metadata` list (`estimated_from_constraints`, `mean`,
  `ci_lower`, `ci_upper`, `constraint_violation_rate`)

The object has class
`c("mosaic_initial_conditions_S", "mosaic_initial_conditions", "list")`.

## Details

**Diagnostic prior.**
[`sample_parameters()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sample_parameters.md)
does not draw `prop_S_initial`: it samples V1, V2, E, I and R and sets S
to the simplex residual (renormalising when the others sum to 1 or
more). The Beta built here is therefore not a sampling prior. It is a
summary of the S distribution those draws imply (up to the
`min_S_proportion` floor used here), kept in `priors_default` as the
reference for prior-vs-posterior comparisons of S; tuning
`variance_inflation` changes that reference only, not calibration.

The function implements a constrained residual approach:

**Method Overview:**

1.  Sample from estimated Beta priors for V1, V2, E, I, R compartments

2.  Calculate S as constrained residual: S = 1 - (V1 + V2 + E + I + R)

3.  Apply constraints: ensure S \>= min_S_proportion and handle
    over-allocation

4.  Fit Beta distribution to S samples using CI expansion method

5.  Return priors-compatible structure with metadata

**Constraint Handling:**

- If other compartments sum \> (1 - min_S_proportion), proportionally
  scale them down

- Ensures S is always \>= min_S_proportion for biological realism

- Maintains mathematical consistency: all compartments sum to 1

**Fallback Behavior:**

- If estimated priors unavailable for any compartment, uses default
  prior parameters

- Locations without any estimated compartments use independent S prior
  (Beta(30, 7.5))

- Graceful degradation ensures function always returns valid results

**Uncertainty Propagation:**

- S uncertainty incorporates uncertainty from all other compartments

- Variance inflation applied using CI expansion method consistent with
  other est_initial\_\* functions

- Countries with high vaccination/immunity naturally get lower and more
  certain S estimates

## See also

[`est_initial_E_I`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_initial_E_I.md)
for infection compartment estimation
[`est_initial_R`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_initial_R.md)
for recovered compartment estimation
[`fit_beta_from_ci`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/fit_beta_from_ci.md)
for Beta distribution fitting with CI constraints

## Examples

``` r
if (FALSE) { # \dontrun{
# After running est_initial_E_I, est_initial_R
PATHS <- get_paths()
priors_updated <- priors_default  # With updated V1,V2,E,I,R compartments

initial_S <- est_initial_S(
  PATHS = PATHS,
  priors = priors_updated,
  config = config_default,
  n_samples = 1000,
  variance_inflation = 2  # Double the SD (4x the variance)
)

# Access results for a location
eth_s_params <- initial_S$parameters_location$prop_S_initial$parameters$location$ETH
eth_s_mean <- eth_s_params$metadata$mean
} # }
```
