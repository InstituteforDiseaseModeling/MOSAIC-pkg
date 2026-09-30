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

  - sample_alpha_1: Population mixing within metapops (default FALSE;
    PINNED, see Details)

  - sample_alpha_2: Degree of frequency driven transmission (default
    FALSE; pinned, weakly identified)

  - sample_decay_days_short: Minimum V. cholerae survival (default TRUE)

  - sample_decay_days_spread: Spread between min and max V. cholerae
    survival; decay_days_long is derived as short + spread (default
    TRUE)

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

  - sample_rho_deaths: Surveillance capture rate of true cholera deaths
    (default FALSE; PINNED at `config_default$rho_deaths` = 0.42). The
    engine converts the reported CFR `mu_jt` to a per-onset fatality
    probability by dividing by `rho_deaths` and then thins true deaths
    by it, so the parameter cancels from reported deaths exactly and
    carries no likelihood information; it sets only the level of true
    deaths. The Beta(36.95, 51.02) prior is retained in `priors_default`
    as the literature record and for sensitivity runs; set TRUE to
    re-enable the draw.

  - sample_sigma: Symptomatic fraction (default TRUE)

  - sample_zeta_1: Symptomatic shedding rate (default TRUE)

  - sample_zeta_ratio: Symptomatic-to-asymptomatic shedding ratio
    (default TRUE)

  - sample_beta_j0_tot: Total transmission rate (default TRUE)

  - sample_p_beta: Proportion of human-to-human transmission (default
    TRUE)

  - sample_tau_i: Diffusion (default TRUE)

  - sample_theta_j: WASH coverage (default TRUE)

  - sample_a_1_j: Seasonality (default TRUE)

  - sample_a_2_j: Seasonality (default TRUE)

  - sample_b_1_j: Seasonality (default TRUE)

  - sample_b_2_j: Seasonality (default TRUE)

  - sample_epidemic_threshold: Location-specific case-reporting PPV
    switch threshold (default TRUE)

  - sample_delta_reporting_cases: Symptom-onset-to-case reporting delay
    in days (default TRUE)

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

  The reported case fatality ratio `mu_jt` is not sampled. It is a
  \[location x day\] matrix carried by the config, and calibration
  integrates its level and year-to-year deviations out of the deaths
  likelihood analytically
  ([`calc_log_likelihood_deaths_integrated`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_log_likelihood_deaths_integrated.md)).
  The flags `sample_CFR_target`, `sample_mu_j_baseline`,
  `sample_mu_j_epidemic_factor` and `sample_delta_reporting_deaths` were
  removed with the parameters they controlled in v0.96.0; supplying one
  raises a warning and has no effect.

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
