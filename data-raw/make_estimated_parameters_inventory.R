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
    # Environmental (7 params) - reordered
    "decay_days_short",
    "decay_days_long",
    "decay_shape_1",
    "decay_shape_2",
    "zeta_1",
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
    # Surveillance (4 params)
    "chi_endemic",
    "chi_epidemic",
    "delta_reporting_cases",
    "delta_reporting_deaths",
    "rho",
    # Mobility (2 params)
    "mobility_gamma",
    "mobility_omega"
  ),
  display_name = c(
    # Transmission
    "Population Mixing",
    "Frequency-Driven Transmission",
    # Environmental - reordered
    "Minimum V. cholerae Survival",
    "Maximum V. cholerae Survival",
    "Decay Shape Parameter 1",
    "Decay Shape Parameter 2",
    "Symptomatic Shedding Rate",
    "Asymptomatic Shedding Rate",
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
    "Maximum V. cholerae survival time in environment",
    "First shape parameter of Beta distribution for V. cholerae decay",
    "Second shape parameter of Beta distribution for V. cholerae decay",
    "Shedding rate for symptomatic infections",
    "Shedding rate for asymptomatic infections",
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
    "Days from infection to death report in surveillance data",
    "Probability a symptomatic infection is reported as a suspected case",
    # Mobility
    "Distance decay parameter for human mobility",
    "Population scaling parameter for human mobility"
  ),
  units = c(
    # Transmission
    "proportion", "proportion",
    # Environmental - reordered
    "days", "days", "dimensionless", "dimensionless", "bacteria/day (rate)", "bacteria/day (rate)", "bacteria/mL",
    # Disease - reordered
    "per day (rate)", "proportion", "per day (rate)", "per day (rate)",
    # Immunity - reordered
    "proportion", "proportion", "per day (rate)", "per day (rate)", "per day (rate)",
    # Surveillance
    "proportion", "proportion", "days", "days", "proportion",
    # Mobility
    "dimensionless", "dimensionless"
  ),
  distribution = c(
    # Transmission
    "beta", "beta",
    # Environmental - reordered
    # decay_days_short, decay_days_long, decay_shape_1, decay_shape_2: Uniform (unchanged)
    # zeta_1, zeta_2, kappa: updated to Lognormal in make_priors.R (v0.14.30)
    "uniform", "uniform", "uniform", "uniform", "lognormal", "lognormal", "lognormal",
    # Disease - reordered
    "lognormal", "beta", "lognormal", "lognormal",
    # Immunity - reordered
    "beta", "beta", "gamma", "gamma", "lognormal",
    # Surveillance
    # chi_endemic, chi_epidemic: Beta; delta_reporting_*: TruncNorm prior ([0,7],[0,14]); rho: Beta
    "beta", "beta", "truncnorm", "truncnorm", "beta",
    # Mobility
    "gamma", "gamma"
  ),
  scale = "global",
  category = c(
    # Transmission
    "transmission", "transmission",
    # Environmental - reordered
    "environmental", "environmental", "environmental", "environmental", "environmental", "environmental", "environmental",
    # Disease - reordered
    "disease", "disease", "disease", "disease",
    # Immunity - reordered
    "immunity", "immunity", "immunity", "immunity", "immunity",
    # Surveillance
    "surveillance", "surveillance", "surveillance", "surveillance", "surveillance",
    # Mobility
    "mobility", "mobility"
  ),
  order = 1:25,
  order_scale = "01",
  order_category = c(
    # Transmission (01)
    "01", "01",
    # Environmental (02)
    "02", "02", "02", "02", "02", "02", "02",
    # Disease (03)
    "03", "03", "03", "03",
    # Immunity (04)
    "04", "04", "04", "04", "04",
    # Surveillance (05): chi_endemic, chi_epidemic, delta_reporting_cases, delta_reporting_deaths, rho
    "05", "05", "05", "05", "05",
    # Mobility (06)
    "06", "06"
  ),
  order_parameter = c(
    # Transmission
    "01", "02",
    # Environmental (decay_days_short, decay_days_long, decay_shape_1, decay_shape_2, zeta_1, zeta_2, kappa)
    "01", "02", "03", "04", "05", "06", "07",
    # Disease (iota, sigma, gamma_1, gamma_2)
    "01", "02", "03", "04",
    # Immunity (phi_1, phi_2, omega_1, omega_2, epsilon)
    "01", "02", "03", "04", "05",
    # Surveillance (chi_endemic, chi_epidemic, delta_reporting_cases, delta_reporting_deaths, rho)
    "01", "02", "03", "04", "05",
    # Mobility
    "01", "02"
  ),
  stringsAsFactors = FALSE
)

# Add posterior distribution columns to global_params.
# For parameters with uniform priors the posterior family is decoupled so that
# the fitted posterior curve can reflect the actual shape of the weighted samples
# rather than being forced into a flat uniform. Parameters without a uniform
# prior retain the same family for both prior display and posterior fitting.
# posterior_lower / posterior_upper supply hard truncation bounds passed to
# fit_truncnorm_from_ci() for bounded integer parameters (delta_reporting_*).
global_params$posterior_distribution <- c(
  # Transmission: beta prior → beta posterior
  "beta", "beta",
  # Environmental: decay_days_short/long uniform → lognormal posterior (positive durations);
  #   decay_shape_1/2 uniform → gamma posterior (positive shape parameters);
  #   zeta_1, zeta_2, kappa: lognormal prior → lognormal posterior (unchanged)
  "lognormal", "lognormal", "gamma", "gamma", "lognormal", "lognormal", "lognormal",
  # Disease: unchanged
  "lognormal", "beta", "lognormal", "lognormal",
  # Immunity: unchanged
  "beta", "beta", "gamma", "gamma", "lognormal",
  # Surveillance: chi_endemic/epidemic unchanged; delta_reporting_* uniform → truncnorm
  #   posterior with hard bounds enforcing the integer support; rho unchanged
  "beta", "beta", "truncnorm", "truncnorm", "beta",
  # Mobility: unchanged
  "gamma", "gamma"
)
global_params$posterior_lower <- c(
  NA, NA,
  NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA,
  NA, NA, NA, NA, NA,
  NA, NA, 0, 0, NA,  # delta_reporting_cases lower bound = 0
  NA, NA
)
global_params$posterior_upper <- c(
  NA, NA,
  NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA,
  NA, NA, NA, NA, NA,
  NA, NA, 7, 14, NA,  # delta_reporting_cases upper = 7, delta_reporting_deaths upper = 14
  NA, NA
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
  distribution = c("gompertz", "beta"),
  posterior_distribution = c("gompertz", "beta"),
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
  parameter_name = c("a_1", "a_2", "b_1", "b_2"),
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
disease_params <- data.frame(
  parameter_name = c("mu_j_baseline", "mu_j_slope", "mu_j_epidemic_factor", "epidemic_threshold"),
  display_name = c(
    "Baseline IFR",
    "Temporal IFR Trend",
    "Epidemic IFR Multiplier",
    "Epidemic Threshold"
  ),
  description = c(
    "Baseline location-specific infection fatality ratio",
    "Temporal trend in IFR (change per year)",
    "Multiplier applied to IFR during epidemic periods",
    "Case incidence threshold for epidemic period classification"
  ),
  units = c("proportion", "per year", "dimensionless", "cases/100k/week"),
  # mu_j_epidemic_factor prior is Gamma(1,2) in make_priors.R — corrected from lognormal (v0.14.35)
  distribution = c("gamma", "normal", "gamma", "lognormal"),
  posterior_distribution = c("gamma", "normal", "gamma", "lognormal"),
  posterior_lower = rep(NA_real_, 4),
  posterior_upper = rep(NA_real_, 4),
  scale = "location",
  category = "disease",
  order = 39:42,
  order_scale = "02",
  order_category = "06",
  order_parameter = sprintf("%02d", 1:4),
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
  # psi_star_a prior is Truncnorm(1, 0.5, [0,Inf]) in make_priors.R — corrected from lognormal (v0.14.35)
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

# Initial conditions — global_params now has 25 rows, so location params start at 26
initial_params$order <- 26:31
# order_category = "01"

# Transmission parameters (2 params: beta_j0_tot, p_beta)
transmission_params$order <- 32:33
transmission_params$order_category <- "02"

# Seasonality parameters
seasonality_params$order <- 34:37
seasonality_params$order_category <- "03"

# Environmental parameters (psi_star calibration params)
calibration_params$order <- 38:41
calibration_params$order_category <- "04"

# Other parameters: disease (mu_j_*), mobility (tau_i), spatial (theta_j)
other_params <- rbind(
  disease_params,    # mu_j_baseline, mu_j_slope, mu_j_epidemic_factor, epidemic_threshold
  spatial_params     # tau_i, theta_j
)
n_disease <- nrow(disease_params)
n_spatial <- nrow(spatial_params)
other_params$order <- 42:(41 + n_disease + n_spatial)
other_params$order_category <- c(
  rep("05", n_disease),  # disease params
  "05", "05"             # tau_i, theta_j
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
