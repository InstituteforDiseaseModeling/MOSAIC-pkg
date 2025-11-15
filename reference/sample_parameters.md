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

  - sample_iota: Importation rate (default TRUE)

  - sample_kappa: Spatial correlation (default TRUE)

  - sample_mobility_gamma: Mobility parameter (default TRUE)

  - sample_mobility_omega: Mobility parameter (default TRUE)

  - sample_omega_1: Infection (default TRUE)

  - sample_omega_2: Infection (default TRUE)

  - sample_phi_1: Incubation (default TRUE)

  - sample_phi_2: Incubation (default TRUE)

  - sample_rho: Immunity waning (default TRUE)

  - sample_sigma: Immunity (default TRUE)

  - sample_zeta_1: Spatial (default TRUE)

  - sample_zeta_2: Spatial (default TRUE)

  - sample_beta_j0_tot: Total transmission rate (default TRUE)

  - sample_p_beta: Proportion of human-to-human transmission (default
    TRUE)

  - sample_tau_i: Diffusion (default TRUE)

  - sample_theta_j: WASH coverage (default TRUE)

  - sample_a1: Seasonality (default TRUE)

  - sample_a2: Seasonality (default TRUE)

  - sample_b1: Seasonality (default TRUE)

  - sample_b2: Seasonality (default TRUE)

  - sample_mu_j: Location-specific case fatality ratio (default TRUE)

  - sample_psi_star_a: Suitability calibration shape/gain (default TRUE)

  - sample_psi_star_b: Suitability calibration scale/offset (default
    TRUE)

  - sample_psi_star_z: Suitability calibration smoothing (default TRUE)

  - sample_psi_star_k: Suitability calibration time offset (default
    TRUE)

  - sample_initial_conditions: Initial condition proportions (default
    TRUE)

  If NULL, all parameters are sampled (default behavior).

- verbose:

  Logical indicating whether to print progress messages. Default TRUE.

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
