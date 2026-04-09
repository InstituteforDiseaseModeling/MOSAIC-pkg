# Sample Parameters from Prior Distributions

This function samples parameter values from prior distributions and
creates a MOSAIC config file with the sampled values. It supports all
distribution types used in MOSAIC priors and provides explicit control
over which parameters to sample.

## Usage

``` r
sample_parameters(
  PATHS = NULL,
  priors = NULL,
  config = NULL,
  seed,
  sample_args = NULL,
  verbose = TRUE,
  validate = TRUE,
  ...
)
```

## Arguments

- PATHS:

  A list containing paths to various directories. If NULL, will use
  get_paths().

- priors:

  A priors list object. If NULL, will use MOSAIC::priors_default.

- config:

  A config template list. If NULL, will use MOSAIC::config_default.

- seed:

  Random seed for reproducible sampling (required).

- sample_args:

  Named list of logical values controlling which parameters to sample.
  Each element should be named as `sample_[parameter]` with a logical
  value. Available options include:

  - sample_alpha_1: Population mixing within metapops (default TRUE)

  - sample_alpha_2: Degree of frequency driven transmission (default
    TRUE)

  - sample_decay_days_long: Maximum V. cholerae survival (default TRUE)

  - sample_decay_days_short: Minimum V. cholerae survival (default TRUE)

  - sample_decay_shape_1: First Beta shape for decay (default TRUE)

  - sample_decay_shape_2: Second Beta shape for decay (default TRUE)

  - sample_epsilon: Immunity (default TRUE)

  - sample_gamma_1: Recovery rate (default TRUE)

  - sample_gamma_2: Recovery rate (default TRUE)

  - sample_iota: Incubation rate (default TRUE)

  - sample_kappa: V. cholerae 50 percent infectious dose concentration
    (default TRUE)

  - sample_mobility_gamma: Mobility distance decay parameter (default
    TRUE)

  - sample_mobility_omega: Mobility population scaling parameter
    (default TRUE)

  - sample_omega_1: Vaccine waning rate one dose (default TRUE)

  - sample_omega_2: Vaccine waning rate two doses (default TRUE)

  - sample_phi_1: Initial vaccine effectiveness one dose (default TRUE)

  - sample_phi_2: Initial vaccine effectiveness two doses (default TRUE)

  - sample_chi_endemic: PPV among suspected cases during endemic periods
    (default TRUE)

  - sample_chi_epidemic: PPV among suspected cases during epidemic
    periods (default TRUE)

  - sample_rho: Care-seeking rate (default TRUE)

  - sample_sigma: Symptomatic fraction (default TRUE)

  - sample_zeta_1: Spatial (default TRUE)

  - sample_zeta_2: Spatial (default TRUE)

  - sample_beta_j0_tot: Total transmission rate (default TRUE)

  - sample_p_beta: Proportion of human-to-human transmission (default
    TRUE)

  - sample_tau_i: Diffusion (default TRUE)

  - sample_theta_j: WASH coverage (default TRUE)

  - sample_a_1_j: Seasonality (default TRUE)

  - sample_a_2_j: Seasonality (default TRUE)

  - sample_b_1_j: Seasonality (default TRUE)

  - sample_b_2_j: Seasonality (default TRUE)

  - sample_mu_j_baseline: Location-specific baseline IFR (default TRUE)

  - sample_mu_j_slope: Location-specific temporal IFR trend (default
    TRUE)

  - sample_mu_j_epidemic_factor: Location-specific epidemic IFR
    multiplier (default TRUE)

  - sample_epidemic_threshold: Location-specific epidemic activation
    threshold (default TRUE)

  - sample_delta_reporting_cases: Infection-to-case reporting delay in
    days (default TRUE)

  - sample_delta_reporting_deaths: Infection-to-death reporting delay in
    days (default TRUE)

  - sample_psi_star_a: Suitability calibration shape/gain (default TRUE)

  - sample_psi_star_b: Suitability calibration scale/offset (default
    TRUE)

  - sample_psi_star_z: Suitability calibration smoothing (default TRUE)

  - sample_psi_star_k: Suitability calibration time offset (default
    TRUE)

  - sample_initial_conditions: Initial condition proportions (default
    TRUE)

  - ic_moment_match: Derive E/I from observed week-1 cases and the
    sampled reporting chain (sigma, rho, chi_endemic, iota). Only active
    when sample_initial_conditions is TRUE. (default FALSE)

  If NULL, all parameters are sampled (default behavior).

- verbose:

  Logical indicating whether to print progress messages. Default TRUE.

- validate:

  Logical indicating whether to run post-sampling validation. Default
  TRUE.

- ...:

  Additional individual sample\_\* arguments for backward compatibility.
  These override values in sample_args if both are provided.

## Value

A MOSAIC config list with sampled parameter values.

## Examples

``` r
if (FALSE) { # \dontrun{
# Sample all parameters (default)
config_sampled <- sample_parameters(seed = 123)

# Sample only disease progression parameters using sample_args
config_sampled <- sample_parameters(
  seed = 123,
  sample_args = list(
    sample_mobility_omega = FALSE,
    sample_mobility_gamma = FALSE,
    sample_kappa = FALSE
  )
)

# Backward compatibility: still works with individual arguments
config_sampled <- sample_parameters(
  seed = 123,
  sample_mobility_omega = FALSE,
  sample_mobility_gamma = FALSE,
  sample_kappa = FALSE
)
} # }
```
