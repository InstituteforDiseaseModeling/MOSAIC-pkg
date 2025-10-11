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
    # Surveillance (2 params) - reordered
    "delta_reporting",
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
    # Surveillance - reordered
    "Reporting Delay",
    "Reporting Rate",
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
    "Days of reporting delay in surveillance data",
    "Proportion of suspected cases that are true cholera",
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
    # Surveillance - reordered
    "days", "proportion",
    # Mobility
    "dimensionless", "dimensionless"
  ),
  distribution = c(
    # Transmission
    "beta", "beta",
    # Environmental - reordered
    "uniform", "uniform", "uniform", "uniform", "uniform", "uniform", "uniform",
    # Disease - reordered
    "lognormal", "beta", "lognormal", "lognormal",
    # Immunity - reordered
    "beta", "beta", "gamma", "gamma", "lognormal",
    # Surveillance - reordered
    "lognormal", "beta",
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
    "surveillance", "surveillance",
    # Mobility
    "mobility", "mobility"
  ),
  order = 1:22,
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
    # Surveillance (05)
    "05", "05",
    # Mobility (06)
    "06", "06"
  ),
  order_parameter = c(
    # Transmission
    "01", "02",
    # Environmental - reordered (decay_days_short, decay_days_long, decay_shape_1, decay_shape_2, zeta_1, zeta_2, kappa)
    "01", "02", "03", "04", "05", "06", "07",
    # Disease - reordered (iota, sigma, gamma_1, gamma_2)
    "01", "02", "03", "04",
    # Immunity - reordered (phi_1, phi_2, omega_1, omega_2, epsilon)
    "01", "02", "03", "04", "05",
    # Surveillance
    "01", "02",
    # Mobility
    "01", "02"
  ),
  stringsAsFactors = FALSE
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
  scale = "location",
  category = "initial_conditions",
  order = 23:28,
  order_scale = "02",
  order_category = "01",
  order_parameter = sprintf("%02d", 1:6),
  stringsAsFactors = FALSE
)

# Transmission parameters
transmission_params <- data.frame(
  parameter_name = c(
    "beta_j0_tot",
    "p_beta",
    "beta_j0_hum",
    "beta_j0_env"
  ),
  display_name = c(
    "Total Base Transmission Rate",
    "Human-to-Human Proportion",
    "Human-to-Human Transmission",
    "Environmental Transmission"
  ),
  description = c(
    "Total base transmission rate (human + environmental)",
    "Proportion of total transmission that is human-to-human",
    "Human-to-human component of transmission (derived from beta_j0_tot * p_beta)",
    "Environmental component of transmission (derived from beta_j0_tot * (1-p_beta))"
  ),
  units = c(
    "per day", "proportion", "per day", "per day"
  ),
  distribution = c("gompertz", "beta", "derived", "derived"),
  scale = "location",
  category = "transmission",
  order = 29:32,
  order_scale = "02",
  order_category = "02",
  order_parameter = sprintf("%02d", 1:4),
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
  parameter_name = c("mu_j"),
  display_name = c("Infection Fatality Ratio"),
  description = c("Infection fatality ratio (IFR) - proportion of infections resulting in death"),
  units = c("proportion"),
  distribution = c("gamma"),
  scale = "location",
  category = "disease",
  order = 39L,
  order_scale = "02",
  order_category = "06",
  order_parameter = "01",
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
  distribution = c("lognormal", "normal", "beta", "truncnorm"),
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

# Initial conditions (already ordered correctly)
# order_category = "01"

# Transmission parameters
transmission_params$order <- 29:32
transmission_params$order_category <- "02"

# Seasonality parameters
seasonality_params$order <- 33:36
seasonality_params$order_category <- "03"

# Environmental parameters (psi_star calibration params)
calibration_params$order <- 37:40
calibration_params$order_category <- "04"

# Other parameters: mobility, spatial, disease (in alphabetical order by param name)
# mu_j (disease), tau_i (mobility), theta_j (spatial)
other_params <- rbind(
  disease_params,    # mu_j
  spatial_params     # tau_i, theta_j
)
other_params$order <- 41:43
other_params$order_category <- c("05", "05", "05")  # All in "other" category
other_params$order_parameter <- c("01", "02", "03") # mu_j, tau_i, theta_j

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
