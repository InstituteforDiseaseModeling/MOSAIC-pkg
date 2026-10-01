# Estimate Initial R Compartment from Historical Cholera Surveillance

Estimates the initial proportion of the population in the Recovered (R)
compartment based on historical cholera surveillance data. The R
compartment represents individuals with natural immunity from previous
cholera infection, subject to waning immunity.

## Usage

``` r
est_initial_R(
  PATHS,
  priors,
  config,
  n_samples = 1000,
  t0 = NULL,
  disaggregate = TRUE,
  verbose = TRUE,
  parallel = FALSE,
  variance_inflation = 1,
  seed = NULL
)
```

## Arguments

- PATHS:

  List containing paths to data directories

- priors:

  List containing prior distributions for model parameters

- config:

  List containing model configuration including location codes

- n_samples:

  Integer, number of Monte Carlo samples for uncertainty quantification
  (default 1000)

- t0:

  Date object, target date for estimation (default NULL uses current
  date)

- disaggregate:

  Logical, whether to spread each year's cases over the year with the
  location's seasonal priors (`priors$parameters_location$a_1_j`,
  `b_1_j`, `a_2_j`, `b_2_j`; TRUE) or place them at mid-year (FALSE)

- verbose:

  Logical, whether to print progress messages (default TRUE)

- parallel:

  Logical, whether to use parallel processing for locations when
  length(location_codes) \>= 8 (default FALSE). Uses
  parallel::mclapply() with all available cores. Note: Not supported on
  Windows.

- variance_inflation:

  Multiplier on the SD of the Monte Carlo R/N samples in the
  method-of-moments Beta refit, which keeps the sample mean (default 1 =
  no change; 0 is also treated as no change). A scalar or a named
  per-ISO vector; the variance scales with its square (2 gives 4x) and
  values in (0, 1) tighten the prior.

- seed:

  Optional integer seed. When given, each location's Monte Carlo draws
  use a seed derived from `seed` and the ISO code, so results are
  reproducible and identical with or without `parallel`; the caller's
  RNG state is restored. NULL (default) draws from the session RNG.

## Value

List with structure matching priors_default for prop_R_initial
parameters

## Details

**Reporting chain.** Reported cases are converted to infections through
the engine's observation process: the engine reports
`Binomial(new_symptomatic, rho) / chi` (`sim_components.R`), so
infections = cases \* chi / (rho \* sigma), with `rho` and `chi_endemic`
drawn from the global priors. Before v0.100.0 those lookups never
resolved and every draw used the hardcoded fallbacks rho = 0.1, chi =
0.5 (chi / rho = 5). At priors_default v16.1 (rho ~ Beta(5.38, 7.10),
chi_endemic ~ Beta(5.43, 5.01)) \\E\[chi/rho\] \approx 1.36\\, so the
infection multiplier, and with it the prop_R_initial mean, is about 3.7x
lower than before, with S correspondingly higher through the simplex
residual. The IC is now consistent with the observation model the
calibration samples.

## Examples

``` r
if (FALSE) { # \dontrun{
PATHS <- get_paths()
priors <- priors_default
config <- config_default
initial_R <- est_initial_R(PATHS, priors, config, n_samples = 1000,
                           t0 = as.Date("2024-01-01"), disaggregate = TRUE)
} # }
```
