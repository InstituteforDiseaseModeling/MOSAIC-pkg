#!/usr/bin/env Rscript

#' make_estimated_parameters.R
#'
#' Create an estimated parameters inventory for the MOSAIC package
#' This inventory contains metadata for all stochastic parameters that are
#' estimated through Bayesian sampling in the MOSAIC modeling framework
#'
#' @author John Giles
#' @date 2025

library(MOSAIC)
library(dplyr)

# Set up paths
MOSAIC::set_root_directory("~/MOSAIC")
PATHS <- MOSAIC::get_paths()

# Load existing data objects to extract information
data("config_default", package = "MOSAIC")
data("priors_default", package = "MOSAIC")
data("iso_codes_mosaic", package = "MOSAIC")

# =============================================================================
# 1. DEFINE CORE PARAMETER METADATA
# =============================================================================

# Initialize estimated parameters inventory data frame
# Contains metadata for all stochastic parameters estimated through Bayesian sampling
estimated_parameters <- data.frame(
  parameter_name = character(),
  display_name = character(),
  description = character(),
  units = character(),
  distribution = character(),
  scale = character(),
  category = character(),
  order = integer(),
  order_scale = character(),
  order_category = character(),
  order_parameter = character(),
  stringsAsFactors = FALSE
)

# =============================================================================
# 2. GLOBAL PARAMETERS (ALPHABETICAL)
# =============================================================================

# Only stochastic global parameters - organized by category, then alphabetically
global_params <- data.frame(
  parameter_name = c(
    # Transmission (2 params)
    "alpha_1",
    "alpha_2",
    # Environmental (9 params) - reordered
    # decay_days_long is DERIVED (= decay_days_short + decay_days_spread) and
    # zeta_2 is DERIVED (= zeta_1 / zeta_ratio). Both are included in the inventory
    # (v0.28.11) so their posteriors are fit from the simulated sample quantiles and
    # rendered in plot_model_distributions. Priors are NOT defined for derived params,
    # so update_priors_from_posteriors will skip them during staged merges
    # (update_priors_from_posteriors.R:157-161 "not in original priors" guard).
    "decay_days_short",
    "decay_days_spread",
    "decay_days_long",
    "decay_shape_1",
    "decay_shape_2",
    "zeta_1",
    "zeta_ratio",
    "zeta_2",
    "kappa",
    # Disease (4 params) - reordered
    "iota",
    "sigma",
    "gamma_1",
    "gamma_2",
    # Immunity (5 params) - reordered
    "phi_1",
    "phi_2",
    "omega_1",
    "omega_2",
    "epsilon",
    # Surveillance (6 params)
    "chi_endemic",
    "chi_epidemic",
    "delta_reporting_cases",
    "delta_reporting_deaths",
    "rho",
    "rho_deaths",
    # Mobility (2 params)
    "mobility_gamma",
    "mobility_omega"
  ),
  display_name = c(
    # Transmission
    "Population Mixing",
    "Frequency-Driven Transmission",
    # Environmental - reordered (9 params, including derived decay_days_long and zeta_2)
    "Minimum V. cholerae Survival",
    "V. cholerae Survival Spread",
    "Maximum V. cholerae Survival (derived)",
    "Decay Shape Parameter 1",
    "Decay Shape Parameter 2",
    "Symptomatic Shedding Rate",
    "Shedding Ratio (Sym/Asym)",
    "Asymptomatic Shedding Rate (derived)",
    "50% Infectious Dose",
    # Disease - reordered
    "Incubation Rate",
    "Proportion Symptomatic",
    "Symptomatic Recovery Rate",
    "Asymptomatic Recovery Rate",
    # Immunity - reordered
    "One-Dose Vaccine Effectiveness",
    "Two-Dose Vaccine Effectiveness",
    "One-Dose Vaccine Waning Rate",
    "Two-Dose Vaccine Waning Rate",
    "Natural Immunity Waning Rate",
    # Surveillance
    "PPV in Endemic Periods",
    "PPV in Epidemic Periods",
    "Case Reporting Delay",
    "Death Reporting Delay",
    "Care-Seeking Probability",
    "Death Detection Rate",
    # Mobility
    "Mobility Distance Decay",
    "Mobility Population Scaling"
  ),
  description = c(
    # Transmission
    "Population mixing within metapops (0-1, 1 = well-mixed)",
    "Degree of frequency driven transmission (0-1)",
    # Environmental - reordered
    "Minimum V. cholerae survival time in environment",
    "Spread between min and max V. cholerae survival time (decay_days_long - decay_days_short)",
    "Maximum V. cholerae survival time in environment (DERIVED = decay_days_short + decay_days_spread)",
    "First shape parameter of Beta distribution for V. cholerae decay",
    "Second shape parameter of Beta distribution for V. cholerae decay",
    "Shedding rate for symptomatic infections",
    "Ratio of symptomatic to asymptomatic shedding rate (zeta_1 / zeta_2)",
    "Shedding rate for asymptomatic infections (DERIVED = zeta_1 / zeta_ratio)",
    "Concentration of V. cholerae for 50% infectious dose",
    # Disease - reordered
    "Rate parameter for incubation period (per day, 1/iota = mean incubation time)",
    "Proportion of infections that are symptomatic",
    "Recovery rate for symptomatic/severe infections",
    "Recovery rate for asymptomatic/mild infections",
    # Immunity - reordered
    "Initial vaccine effectiveness for one dose",
    "Initial vaccine effectiveness for two dose",
    "Vaccine waning rate for one-dose vaccination",
    "Vaccine waning rate for two-dose vaccination",
    "Natural immunity waning rate",
    # Surveillance
    "Positive predictive value of suspected cholera cases in endemic periods",
    "Positive predictive value of suspected cholera cases in epidemic periods",
    "Days from infection to case report in surveillance data",
    "Days from death event to death report in surveillance data (laser-cholera v0.13+: NOT symptom-onset-to-report; symptom-onset-to-death is in gamma_1^-1)",
    "Probability a symptomatic infection is reported as a suspected case",
    "Probability a true cholera death is captured by surveillance (informative prior derived from SSA meta-analysis: Routh 2017, Shikanga 2009, Bwire 2013)",
    # Mobility
    "Distance decay parameter for human mobility",
    "Population scaling parameter for human mobility"
  ),
  units = c(
    # Transmission
    "proportion", "proportion",
    # Environmental - reordered (decay_days_short, decay_days_spread, decay_days_long,
    # decay_shape_1, decay_shape_2, zeta_1, zeta_ratio, zeta_2, kappa)
    "days", "days", "days", "dimensionless", "dimensionless",
    "bacteria/day (rate)", "dimensionless (ratio)", "bacteria/day (rate)", "bacteria/mL",
    # Disease - reordered
    "per day (rate)", "proportion", "per day (rate)", "per day (rate)",
    # Immunity - reordered
    "proportion", "proportion", "per day (rate)", "per day (rate)", "per day (rate)",
    # Surveillance (6 params)
    "proportion", "proportion", "days", "days", "proportion", "proportion",
    # Mobility
    "dimensionless", "dimensionless"
  ),
  distribution = c(
    # Transmission
    "beta", "beta",
    # Environmental - reordered (9 params, including derived decay_days_long and zeta_2)
    # decay_days_short: TruncNorm; decay_days_spread: TruncNorm (replaces direct decay_days_long
    #   prior in v0.27.0); decay_days_long: DERIVED = short + spread, posterior fit as truncnorm
    #   (v0.28.11); decay_shape_1/2: TruncNorm ([0.1, 10]); zeta_1/zeta_ratio/kappa: lognormal;
    #   zeta_2: DERIVED = zeta_1/zeta_ratio, posterior fit as lognormal (v0.28.11).
    # Derived params have no prior row in priors_default — the `distribution` column below
    # records the posterior family we expect; update_priors_from_posteriors.R:157-161 safely
    # skips them during staged merges because updated$parameters_global[[derived]] is NULL.
    "truncnorm", "truncnorm", "truncnorm", "truncnorm", "truncnorm",
    "lognormal", "lognormal", "lognormal", "lognormal",
    # Disease - reordered
    "lognormal", "beta", "lognormal", "lognormal",
    # Immunity - reordered
    "beta", "beta", "gamma", "gamma", "lognormal",
    # Surveillance (6 params)
    # chi_endemic, chi_epidemic: Beta; delta_reporting_*: TruncNorm prior ([0,7],[0,14]); rho: Beta; rho_deaths: Beta
    "beta", "beta", "truncnorm", "truncnorm", "beta", "beta",
    # Mobility
    "gamma", "gamma"
  ),
  scale = "global",
  category = c(
    # Transmission
    "transmission", "transmission",
    # Environmental - reordered (9 params)
    "environmental", "environmental", "environmental", "environmental", "environmental",
    "environmental", "environmental", "environmental", "environmental",
    # Disease - reordered
    "disease", "disease", "disease", "disease",
    # Immunity - reordered
    "immunity", "immunity", "immunity", "immunity", "immunity",
    # Surveillance (6 params)
    "surveillance", "surveillance", "surveillance", "surveillance", "surveillance", "surveillance",
    # Mobility
    "mobility", "mobility"
  ),
  order = 1:28,
  order_scale = "01",
  order_category = c(
    # Transmission (01)
    "01", "01",
    # Environmental (02) - 9 params
    "02", "02", "02", "02", "02", "02", "02", "02", "02",
    # Disease (03)
    "03", "03", "03", "03",
    # Immunity (04)
    "04", "04", "04", "04", "04",
    # Surveillance (05): chi_endemic, chi_epidemic, delta_reporting_cases, delta_reporting_deaths, rho, rho_deaths
    "05", "05", "05", "05", "05", "05",
    # Mobility (06)
    "06", "06"
  ),
  order_parameter = c(
    # Transmission
    "01", "02",
    # Environmental (9 in plot order: decay_days_short, decay_days_spread, decay_days_long,
    # decay_shape_1, decay_shape_2, zeta_1, zeta_ratio, zeta_2, kappa)
    "01", "02", "03", "04", "05", "06", "07", "08", "09",
    # Disease (iota, sigma, gamma_1, gamma_2)
    "01", "02", "03", "04",
    # Immunity (phi_1, phi_2, omega_1, omega_2, epsilon)
    "01", "02", "03", "04", "05",
    # Surveillance (chi_endemic, chi_epidemic, delta_reporting_cases, delta_reporting_deaths, rho, rho_deaths)
    "01", "02", "03", "04", "05", "06",
    # Mobility
    "01", "02"
  ),
  stringsAsFactors = FALSE
)

# Add posterior distribution columns to global_params.
#
# RUNTIME BEHAVIOR (v0.19.25+, issue #64 fix):
# When priors are provided to calc_model_posterior_quantiles():
#   - Non-uniform priors: posterior is fitted in the SAME family as the prior
#   - Uniform priors: posterior family is inferred from parameter domain:
#       * min >= 0 and max <= 1 → beta  (proportion)
#       * min >= 0              → lognormal  (positive real)
#       * otherwise             → normal  (unconstrained)
#   - The posterior_distribution column below documents the expected runtime
#     result and serves as a fallback when priors are not provided.
#
# posterior_lower / posterior_upper supply hard truncation bounds passed to
# fit_truncnorm_from_ci() for bounded integer parameters (delta_reporting_*).
global_params$posterior_distribution <- c(
  # Transmission: beta prior → beta posterior
  "beta", "beta",
  # Environmental (9 params): all non-uniform after v0.27.0 — posterior family matches
  # prior family via the family-match guard in update_priors_from_posteriors.R.
  #   decay_days_short: truncnorm → truncnorm
  #   decay_days_spread: truncnorm → truncnorm
  #   decay_days_long (DERIVED): posterior fit as truncnorm from short+spread samples (v0.28.11)
  #   decay_shape_1/2: truncnorm → truncnorm (bounds [0.1, 10] preserved)
  #   zeta_1, zeta_ratio, kappa: lognormal → lognormal
  #   zeta_2 (DERIVED): posterior fit as lognormal from zeta_1/zeta_ratio samples (v0.28.11)
  "truncnorm", "truncnorm", "truncnorm", "truncnorm", "truncnorm",
  "lognormal", "lognormal", "lognormal", "lognormal",
  # Disease: unchanged
  "lognormal", "beta", "lognormal", "lognormal",
  # Immunity: unchanged
  "beta", "beta", "gamma", "gamma", "lognormal",
  # Surveillance: chi_endemic/epidemic unchanged; delta_reporting_* uniform → truncnorm
  #   posterior with hard bounds enforcing the integer support; rho unchanged; rho_deaths Beta→Beta
  "beta", "beta", "truncnorm", "truncnorm", "beta", "beta",
  # Mobility: unchanged
  "gamma", "gamma"
)
global_params$posterior_lower <- c(
  NA, NA,                                # transmission
  # environmental (9): short, spread, long (DERIVED min=1.01), shape_1, shape_2, zeta_1,
  # zeta_ratio, zeta_2 (DERIVED min=0, handled by lognormal support), kappa
  NA, NA, 1.01, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA,                        # disease
  NA, NA, NA, NA, NA,                    # immunity
  NA, NA, 0, 1, NA, NA,                  # surveillance (cases lower = 0, deaths lower = 1; rho/rho_deaths NA)
  NA, NA                                 # mobility
)
global_params$posterior_upper <- c(
  NA, NA,                                # transmission
  # environmental (9): decay_days_long DERIVED max = short_max + spread_max = 60 + 365 = 425
  NA, NA, 425, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA,                        # disease
  NA, NA, NA, NA, NA,                    # immunity
  NA, NA, 7, 14, NA, NA,                 # surveillance (delta_reporting_cases=7, _deaths=14; rho/rho_deaths NA)
  NA, NA                                 # mobility
)

# =============================================================================
# 3. LOCATION-SPECIFIC PARAMETERS (BY CATEGORY)
# =============================================================================

# Initial conditions parameters (only proportions)
initial_params <- data.frame(
  parameter_name = c(
    "prop_S_initial",
    "prop_E_initial",
    "prop_I_initial",
    "prop_R_initial",
    "prop_V1_initial",
    "prop_V2_initial"
  ),
  display_name = c(
    "Initial Susceptible Proportion",
    "Initial Exposed Proportion",
    "Initial Infected Proportion",
    "Initial Recovered Proportion",
    "Initial One-Dose Vaccinated Proportion",
    "Initial Two-Dose Vaccinated Proportion"
  ),
  description = c(
    "Proportion of population susceptible at start",
    "Proportion of population exposed at start",
    "Proportion of population infected at start",
    "Proportion of population recovered/immune at start",
    "Proportion with one vaccine dose at start",
    "Proportion with two vaccine doses at start"
  ),
  units = c(
    "proportion", "proportion",
    "proportion", "proportion",
    "proportion", "proportion"
  ),
  distribution = c(
    "beta", "beta",
    "beta", "beta",
    "beta", "beta"
  ),
  posterior_distribution = c(
    "beta", "beta",
    "beta", "beta",
    "beta", "beta"
  ),
  posterior_lower = rep(NA_real_, 6),
  posterior_upper = rep(NA_real_, 6),
  scale = "location",
  category = "initial_conditions",
  order = 23:28,
  order_scale = "02",
  order_category = "01",
  order_parameter = sprintf("%02d", 1:6),
  stringsAsFactors = FALSE
)

# Transmission parameters
# Note: beta_j0_hum and beta_j0_env are derived quantities (beta_j0_tot * p_beta and
# beta_j0_tot * (1-p_beta)) — they are not sampled parameters and must not appear here.
transmission_params <- data.frame(
  parameter_name = c(
    "beta_j0_tot",
    "p_beta"
  ),
  display_name = c(
    "Total Base Transmission Rate",
    "Human-to-Human Proportion"
  ),
  description = c(
    "Total base transmission rate (human + environmental)",
    "Proportion of total transmission that is human-to-human"
  ),
  units = c(
    "per day", "proportion"
  ),
  distribution = c("lognormal", "beta"),
  posterior_distribution = c("lognormal", "beta"),
  posterior_lower = rep(NA_real_, 2),
  posterior_upper = rep(NA_real_, 2),
  scale = "location",
  category = "transmission",
  order = 29:30,
  order_scale = "02",
  order_category = "02",
  order_parameter = sprintf("%02d", 1:2),
  stringsAsFactors = FALSE
)

# Seasonality parameters
seasonality_params <- data.frame(
  parameter_name = c("a_1_j", "a_2_j", "b_1_j", "b_2_j"),
  display_name = c(
    "Seasonality Coefficient a1", "Seasonality Coefficient a2",
    "Seasonality Coefficient b1", "Seasonality Coefficient b2"
  ),
  description = c(
    "First Fourier cosine coefficient for seasonality",
    "Second Fourier cosine coefficient for seasonality",
    "First Fourier sine coefficient for seasonality",
    "Second Fourier sine coefficient for seasonality"
  ),
  units = "dimensionless",
  distribution = c("normal", "normal", "normal", "normal"),
  posterior_distribution = c("normal", "normal", "normal", "normal"),
  posterior_lower = rep(NA_real_, 4),
  posterior_upper = rep(NA_real_, 4),
  scale = "location",
  category = "seasonality",
  order = 33:36,
  order_scale = "02",
  order_category = "03",
  order_parameter = sprintf("%02d", 1:4),
  stringsAsFactors = FALSE
)

# Spatial/mobility parameters
spatial_params <- data.frame(
  parameter_name = c("tau_i", "theta_j"),
  display_name = c(
    "Travel Probability",
    "WASH Coverage"
  ),
  description = c(
    "Country-level travel/mobility probability",
    "WASH coverage index (proportion with adequate water, sanitation, hygiene)"
  ),
  units = c("probability", "proportion"),
  distribution = c("beta", "beta"),
  posterior_distribution = c("beta", "beta"),
  posterior_lower = rep(NA_real_, 2),
  posterior_upper = rep(NA_real_, 2),
  scale = "location",
  category = c("mobility", "spatial"),
  order = 37:38,
  order_scale = "02",
  order_category = c("04", "05"),
  order_parameter = c("01", "01"),
  stringsAsFactors = FALSE
)

# Disease-specific parameters
#
# Includes 4 SAMPLED params (mu_j_baseline, mu_j_slope, mu_j_epidemic_factor,
# epidemic_threshold) plus 4 DERIVED policy-relevant CFR transformations
# (cfr_baseline, cfr_epidemic, cfr_clinical_baseline, cfr_clinical_epidemic).
# The derived CFRs are computed per posterior sample by
# .mosaic_add_implied_cfr_columns() inside run_MOSAIC() right before
# samples.parquet is written, then their posteriors are fit at runtime from
# the sample quantiles. Same pattern as decay_days_long / zeta_2 (DERIVED).
#
# Surveillance CFRs (cfr_baseline / cfr_epidemic) are deaths-among-
# reported-cases under endemic vs epidemic regime — matches the WHO/IDSR
# "annual CFR" definition. Clinical CFRs (cfr_clinical_*) are the
# per-symptomatic-episode death probabilities (1 - exp(-mu_eff / gamma_1))
# clinicians use against treatment-center benchmarks and the WHO 1%
# outbreak-response threshold.
#
# All four are evaluated at simulation tick 0 (i.e. with the temporal
# slope contribution at 1.0). Under the default Normal(0, 0.05) prior on
# mu_j_slope the implied 95% multiplier range is [0.90, 1.10] at the
# simulation midpoint, so the intercept CFRs are good proxies for the
# average endemic / epidemic CFR over the calibration window. For
# applications sensitive to slow CFR drift, recompute CFR per tick from
# the predictions matrices instead.
#
# Inventory `order` is overwritten dynamically below at the .cur_order
# accumulation step (see "REORGANIZE LOCATION-SPECIFIC PARAMETERS" section).
# The static `order` values here are placeholders — keeping them only so
# data.frame() has the right column type.
disease_params <- data.frame(
  parameter_name = c("mu_j_baseline", "mu_j_slope", "mu_j_epidemic_factor", "epidemic_threshold",
                     "cfr_baseline", "cfr_epidemic",
                     "cfr_clinical_baseline", "cfr_clinical_epidemic"),
  display_name = c(
    "Baseline IFR",
    "Temporal IFR Trend",
    "Epidemic IFR Multiplier",
    "Epidemic Threshold",
    "Implied Reported CFR (Endemic)",
    "Implied Reported CFR (Epidemic)",
    "Implied Per-Episode CFR (Endemic)",
    "Implied Per-Episode CFR (Epidemic)"
  ),
  description = c(
    "Baseline location-specific infection fatality ratio",
    "Temporal trend in IFR (change per year)",
    "Multiplier applied to IFR during epidemic periods",
    "Case incidence threshold for epidemic period classification",
    "DERIVED: Implied reported CFR under endemic regime, per posterior sample. cfr_baseline = mu_j_baseline * rho_deaths * chi_endemic / rho (laser-cholera v0.13+ steady-state identity, evaluated at slope=0).",
    "DERIVED: Implied reported CFR under epidemic regime, per posterior sample. cfr_epidemic = mu_j_baseline * (1 + mu_j_epidemic_factor) * rho_deaths * chi_epidemic / rho.",
    "DERIVED: Probability a symptomatic individual dies of cholera over their expected symptomatic period, under endemic regime. 1 - exp(-mu_j_baseline / gamma_1). Comparable to treatment-center per-case CFR and the WHO 1% outbreak-response threshold.",
    "DERIVED: Per-symptomatic-episode death probability under epidemic regime. 1 - exp(-mu_j_baseline * (1 + mu_j_epidemic_factor) / gamma_1)."
  ),
  units = c("proportion", "per year", "dimensionless", "cases/100k/week",
            "proportion", "proportion", "proportion", "proportion"),
  # mu_j_epidemic_factor prior is Gamma(1,2) in make_priors_default.R — corrected from lognormal (v0.14.35)
  # epidemic_threshold: Truncnorm with per-location proportional bounds (v0.28.0, was Lognormal).
  #   Bounds a/b are read from the prior template per location at fit time
  #   (calc_model_posterior_distributions.R:398-411), so posterior_lower/upper stay NA.
  # cfr_baseline / cfr_epidemic / cfr_clinical_*: DERIVED — no priors, posterior fit
  #   as beta from sample quantiles (bounded [0,1] domain, right-skewed).
  #   update_priors_from_posteriors will skip them during staged merges via the
  #   "not in original priors" guard.
  distribution = c("gamma", "normal", "gamma", "truncnorm",
                   "beta", "beta", "beta", "beta"),
  posterior_distribution = c("gamma", "normal", "gamma", "truncnorm",
                             "beta", "beta", "beta", "beta"),
  posterior_lower = rep(NA_real_, 8),
  posterior_upper = rep(NA_real_, 8),
  scale = "location",
  category = "disease",
  order = NA_integer_,           # OVERWRITTEN BELOW (.cur_order accumulator)
  order_scale = "02",
  order_category = "06",          # OVERWRITTEN BELOW
  order_parameter = NA_character_, # OVERWRITTEN BELOW
  stringsAsFactors = FALSE
)

# Calibration parameters for psi_star
calibration_params <- data.frame(
  parameter_name = c("psi_star_a", "psi_star_b", "psi_star_z", "psi_star_k"),
  display_name = c(
    "Suitability Shape/Gain",
    "Suitability Scale/Offset",
    "Suitability Smoothing",
    "Suitability Time Offset"
  ),
  description = c(
    "Shape/gain parameter for logit calibration of NN suitability (a>1 sharpens, a<1 flattens)",
    "Scale/offset parameter for logit calibration (shifts baseline up/down)",
    "Smoothing weight for causal EWMA (z=1: no smoothing, z<1: smoothing)",
    "Time offset in days for suitability calibration (k>0: delay, k<0: advance)"
  ),
  units = c("dimensionless", "dimensionless", "proportion", "days"),
  # psi_star_a prior is Truncnorm(1, 0.5, [0,Inf]) in make_priors_default.R — corrected from lognormal (v0.14.35)
  distribution = c("truncnorm", "normal", "beta", "truncnorm"),
  posterior_distribution = c("truncnorm", "normal", "beta", "truncnorm"),
  posterior_lower = rep(NA_real_, 4),
  posterior_upper = rep(NA_real_, 4),
  scale = "location",
  category = "environmental",
  order = 40:43,
  order_scale = "02",
  order_category = "07",
  order_parameter = sprintf("%02d", 1:4),
  stringsAsFactors = FALSE
)

# =============================================================================
# 4. REORGANIZE LOCATION-SPECIFIC PARAMETERS BY CATEGORY ORDER
# =============================================================================

# Combine location parameters in the requested order:
# initial_conditions, transmission, seasonality, environmental, other (mobility, spatial, disease)
#
# All order ranges are computed dynamically from group sizes so adding /
# removing rows from any group (e.g. rho_deaths, cfr_baseline / cfr_epidemic)
# doesn't require updating downstream constants.
.cur_order <- nrow(global_params)

# Initial conditions
initial_params$order <- (.cur_order + 1):(.cur_order + nrow(initial_params))
.cur_order <- .cur_order + nrow(initial_params)
# order_category = "01"

# Transmission parameters (2 params: beta_j0_tot, p_beta)
transmission_params$order <- (.cur_order + 1):(.cur_order + nrow(transmission_params))
transmission_params$order_category <- "02"
.cur_order <- .cur_order + nrow(transmission_params)

# Seasonality parameters
seasonality_params$order <- (.cur_order + 1):(.cur_order + nrow(seasonality_params))
seasonality_params$order_category <- "03"
.cur_order <- .cur_order + nrow(seasonality_params)

# Environmental parameters (psi_star calibration params)
calibration_params$order <- (.cur_order + 1):(.cur_order + nrow(calibration_params))
calibration_params$order_category <- "04"
.cur_order <- .cur_order + nrow(calibration_params)

# Other parameters: disease (mu_j_*, cfr_*), mobility (tau_i), spatial (theta_j)
other_params <- rbind(
  disease_params,    # mu_j_baseline, mu_j_slope, mu_j_epidemic_factor, epidemic_threshold, cfr_baseline, cfr_epidemic
  spatial_params     # tau_i, theta_j
)
n_disease <- nrow(disease_params)
n_spatial <- nrow(spatial_params)
other_params$order <- (.cur_order + 1):(.cur_order + n_disease + n_spatial)
other_params$order_category <- c(
  rep("05", n_disease),  # disease params
  rep("05", n_spatial)   # tau_i, theta_j
)
other_params$order_parameter <- sprintf("%02d", seq_len(n_disease + n_spatial))

# =============================================================================
# 5. COMBINE ALL PARAMETER GROUPS
# =============================================================================

estimated_parameters <- rbind(
  global_params,
  initial_params,
  transmission_params,
  seasonality_params,
  calibration_params,
  other_params
)

# =============================================================================
# 6. VALIDATE INVENTORY
# =============================================================================

validate_inventory <- function(inventory) {
  cat("\n========== PARAMETER INVENTORY VALIDATION ==========\n\n")

  # Check for duplicate parameter names
  dup_params <- inventory$parameter_name[duplicated(inventory$parameter_name)]
  if (length(dup_params) > 0) {
    cat("⚠ WARNING: Duplicate parameter names found:\n")
    print(dup_params)
  } else {
    cat("✓ No duplicate parameter names\n")
  }

  # Check for duplicate orders
  dup_orders <- inventory$order[duplicated(inventory$order)]
  if (length(dup_orders) > 0) {
    cat("⚠ WARNING: Duplicate orders found:\n")
    print(dup_orders)
  } else {
    cat("✓ No duplicate orders\n")
  }

  # Check parameters have distributions
  no_dist <- inventory[is.na(inventory$distribution), ]
  if (nrow(no_dist) > 0) {
    cat("⚠ WARNING: Parameters without distributions:\n")
    print(no_dist$parameter_name)
  } else {
    cat("✓ All parameters have distributions\n")
  }

  # Summary statistics
  cat("\n========== INVENTORY SUMMARY ==========\n")
  cat("Total parameters:", nrow(inventory), "\n")
  cat("Global parameters:", sum(inventory$scale == "global"), "\n")
  cat("Location-specific parameters:", sum(inventory$scale == "location"), "\n")

  # Category breakdown
  cat("\nParameters by category:\n")
  cat_summary <- table(inventory$category)
  for (cat in names(sort(cat_summary))) {
    cat("  ", cat, ":", cat_summary[cat], "\n")
  }

  # Scale breakdown
  cat("\nParameters by scale:\n")
  scale_summary <- table(inventory$scale)
  for (sc in names(sort(scale_summary))) {
    cat("  ", sc, ":", scale_summary[sc], "\n")
  }

  # Distribution breakdown
  cat("\nParameters by distribution:\n")
  dist_summary <- table(inventory$distribution)
  for (dist in names(sort(dist_summary))) {
    if (!is.na(dist)) {
      cat("  ", dist, ":", dist_summary[dist], "\n")
    }
  }

  cat("\n========================================\n")
}

# Run validation
validate_inventory(estimated_parameters)

# =============================================================================
# 6. SORT BY ORDER COLUMN FOR CONSISTENCY
# =============================================================================

estimated_parameters <- estimated_parameters[order(estimated_parameters$order), ]

# Reset row names
rownames(estimated_parameters) <- NULL

# =============================================================================
# 7. ADD METADATA TO INVENTORY
# =============================================================================

attr(estimated_parameters, "creation_date") <- Sys.Date()
attr(estimated_parameters, "version") <- "1.1.0"
attr(estimated_parameters, "description") <- paste(
  "Comprehensive parameter inventory for MOSAIC cholera transmission model.",
  "Includes metadata, categorization, and distribution information for all model parameters."
)

# =============================================================================
# 8. SAVE AS PACKAGE DATA
# =============================================================================

# Save to package data
usethis::use_data(estimated_parameters, overwrite = TRUE)

# Also save as CSV for reference
csv_path <- file.path(PATHS$MODEL_INPUT, "estimated_parameters.csv")
write.csv(estimated_parameters, csv_path, row.names = FALSE)

cat("\n✓ Parameters inventory successfully created and saved!\n")
cat("  - Package data: data/estimated_parameters.rda\n")
cat("  - CSV backup: ", csv_path, "\n\n")

# =============================================================================
# 9. DEMONSTRATE USAGE EXAMPLES
# =============================================================================

cat("========== USAGE EXAMPLES ==========\n\n")

# Example 1: Get parameters by distribution
cat("Example 1 - Parameters by distribution (first 5):\n")
print(head(estimated_parameters[, c("parameter_name", "distribution")], 5))

# Example 2: Get parameters by category
cat("\nExample 2 - Transmission parameters:\n")
transmission <- estimated_parameters[estimated_parameters$category == "transmission", ]
print(transmission[, c("parameter_name", "display_name", "units")])

# Example 3: Parameters by scale
cat("\nExample 3 - Parameters by scale:\n")
scale_counts <- table(estimated_parameters$scale)
print(scale_counts)

# Example 4: Generate sampling flags from inventory
cat("\nExample 4 - Generate sampling flags:\n")
sampling_flags <- setNames(
  as.list(rep(TRUE, nrow(estimated_parameters))),
  paste0("sample_", estimated_parameters$parameter_name)
)
cat("Total sampling flags generated:", length(sampling_flags), "\n")
cat("All flags set to TRUE for stochastic parameters:", sum(unlist(sampling_flags)), "\n")

cat("\n========================================\n")
