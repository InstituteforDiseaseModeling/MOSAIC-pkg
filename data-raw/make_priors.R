library(MOSAIC)
library(jsonlite)

# make_priors.R - Generate default prior distributions for MOSAIC model parameters

# Set up paths
MOSAIC::set_root_directory("~/MOSAIC")
PATHS <- MOSAIC::get_paths()

# Load config_default to get the global date_start
config_default <- MOSAIC::config_default
date_start <- as.Date(config_default$date_start)

j <- MOSAIC::iso_codes_mosaic

#----------------------------------------
# Load surveillance and demographics data for epidemic_threshold priors
#----------------------------------------

surv_weekly <- read.csv(
     file.path(PATHS$DATA_PROCESSED, "cholera/weekly/cholera_surveillance_weekly_combined.csv"),
     stringsAsFactors = FALSE
)

dem_annual <- read.csv(
     file.path(PATHS$DATA_PROCESSED, "demographics/demographics_mosaic_countries_2000_2024_annual.csv"),
     stringsAsFactors = FALSE
)

priors_default <- list(
     metadata = list(
          version = "13.5",
          date = Sys.Date(),
          description = "Default informative prior distributions for MOSAIC model parameters"
     ),
     parameters_global = list(),    # Single parameters used by all locations
     parameters_location = list()   # Location specific parameters
)

#----------------------------------------
# Global parameters in alphabetical order
#----------------------------------------

# alpha_1 - Population mixing within metapops
beta_fit_alpha_1 <- fit_beta_from_ci(mode_val = 0.25, ci_lower = 0.05, ci_upper = 0.5)

priors_default$parameters_global$alpha_1 <- list(
     description = "Population mixing within metapops (0-1, 1 = well-mixed)",
     distribution = "beta",
     parameters = list(shape1 = beta_fit_alpha_1$shape1, shape2 = beta_fit_alpha_1$shape2)
)

# alpha_2 - Degree of frequency driven transmission
beta_fit_alpha_2 <- fit_beta_from_ci(mode_val = 0.5, ci_lower = 0.25, ci_upper = 0.75)

priors_default$parameters_global$alpha_2 <- list(
     description = "Degree of frequency driven transmission (0-1)",
     distribution = "beta",
     parameters = list(shape1 = beta_fit_alpha_2$shape1, shape2 = beta_fit_alpha_2$shape2)
)

# decay_days_long - Maximum V. cholerae survival time
# Old posterior 97.5th=147.4 was within 2.6 of upper bound=150 -- clear upper truncation.
# Extended to 365 days. Lower bound is 30; decay_days_short upper bound is 29,
# ensuring at least 1-day gap to prevent boundary equality violations.
priors_default$parameters_global$decay_days_long <- list(
     description = "Maximum V. cholerae survival time (days)",
     distribution = "uniform",
     parameters = list(min = 30, max = 365)
)

# decay_days_short - Minimum V. cholerae survival time
# Upper bound set to 29 (not 30) to guarantee a minimum 1-day gap from
# decay_days_long's lower bound of 30, preventing boundary equality violations
# in make_LASER_config(). The swap constraint in sample_parameters.R also
# protects against accidental crossings.
priors_default$parameters_global$decay_days_short <- list(
     description = "Minimum V. cholerae survival time (days)",
     distribution = "uniform",
     parameters = list(min = 0.01, max = 29)
)

# decay_shape_1 - First shape parameter of Beta distribution for V. cholerae decay rate transformation
# Bounds [0.1, 10]: lower bound 0.1 allows U-shaped (arcsine-type) mapping where survival responds
# mainly at environmental extremes. Upper bound 10 supported by empirical evidence: 85% of 84
# country-level calibrations had posterior q97.5 > 4.8 against old bound=5.
# Functionally, s1=s2=10 spans 99.7% of [days_short, days_long] across psi in [0.2,0.8] --
# steep sigmoid but not a step function; increment from s=5 to s=10 is only ~3 days of extra variation.
priors_default$parameters_global$decay_shape_1 <- list(
     description = "First shape parameter of Beta distribution for V. cholerae decay",
     distribution = "uniform",
     parameters = list(min = 0.1, max = 10.0)
)

# decay_shape_2 - Second shape parameter of Beta distribution for V. cholerae decay rate transformation
# Same rationale as decay_shape_1. Upper bound extended to 10: 85% of calibration runs (n=84)
# across all MOSAIC countries had posterior q97.5 > 4.8 against the old bound=5, confirming
# truncation. Functionally, s1=s2=10 spans 99.7% of the [days_short, days_long] survival range
# across psi in [0.2,0.8] -- a steep but continuous sigmoid, not a step function.
# Asymmetric extreme combinations (e.g. s1=10, s2=0.1) have ~5-6% prior mass and are
# biologically implausible, but acceptable given the strong empirical evidence for extension.
priors_default$parameters_global$decay_shape_2 <- list(
     description = "Second shape parameter of Beta distribution for V. cholerae decay",
     distribution = "uniform",
     parameters = list(min = 0.1, max = 10.0)
)

# epsilon - Natural immunity waning rate
priors_default$parameters_global$epsilon <- list(
     description = "Natural immunity waning rate (per day)",
     distribution = "lognormal",
     parameters = list(mean = 3.9e-4, sd = 2.0e-4)  # sd derived from 95% CI [1.7e-4, 1.03e-3]
)

# Apply variance inflation to epsilon
priors_default$parameters_global$epsilon$parameters$sd <-
     priors_default$parameters_global$epsilon$parameters$sd * 2

# gamma_1 - Symptomatic/severe shedding duration rate
priors_default$parameters_global$gamma_1 <- list(
     description = "Symptomatic/severe shedding duration rate (per day)",
     distribution = "lognormal",
     parameters = list(
          meanlog = log(1/10),  # log of median rate: 1/10 per day = 10 days shedding
          sdlog = 0.5          # uncertainty allowing 7-14 day range
     )
)

# gamma_2 - Asymptomatic/mild shedding duration rate
priors_default$parameters_global$gamma_2 <- list(
     description = "Asymptomatic/mild shedding duration rate (per day)",
     distribution = "lognormal",
     parameters = list(
          meanlog = log(1/2),   # log of median rate: 1/2 per day = 2 days shedding
          sdlog = 0.4           # uncertainty allowing 1-3 day range
     )
)


# iota - Incubation rate (1/days)

priors_default$parameters_global$iota <- list(
     description = "Incubation rate (1/days)",
     distribution = "lognormal",
     parameters = list(meanlog = -0.337, sdlog = 0.4)  # Wider distribution
)

# No variance inflation for iota (factor = 1)

# kappa - Concentration of V. cholerae which leads to 50% infectious dose
# Fixed near the config_default and laser-cholera default of 1e6.
# Kappa is structurally unidentifiable alongside zeta and beta_env from case
# data alone (Lee et al. 2017); standard practice in the cholera modeling
# literature (Codeço 2001, Hartley 2006) is to fix it. We use a tight
# Lognormal prior with mode exactly at 1e6 and sdlog=0.25.
# For lognormal: mode = exp(meanlog - sdlog^2), so
#   meanlog = log(1e6) + sdlog^2 = log(1e6) + 0.0625
# This gives 95% CI approximately [626k, 1.60M].
priors_default$parameters_global$kappa <- list(
     description = "Concentration of V. cholerae for 50% infectious dose",
     distribution = "lognormal",
     parameters = list(meanlog = log(10^6) + 0.25^2, sdlog = 0.25)
)


# load param_gravity_model.csv and get gamma distributions that have mode equal to the values in the .csv
param_gravity <- read.csv(file.path(PATHS$MODEL_INPUT, "param_gravity_model.csv"))
mobility_gamma_mode <- param_gravity$parameter_value[param_gravity$variable_name == "mobility_gamma"]
mobility_omega_mode <- param_gravity$parameter_value[param_gravity$variable_name == "mobility_omega"]

# Gamma distribution mode = (shape - 1) / rate when shape > 1
# Define rate and solve for shape: shape = mode * rate + 1
gamma_rate <- 2
mobility_gamma_shape <- mobility_gamma_mode * gamma_rate + 1
mobility_omega_shape <- mobility_omega_mode * gamma_rate + 1

# mobility_gamma - Mobility distance decay parameter
priors_default$parameters_global$mobility_gamma <- list(
     description = "Mobility distance decay parameter",
     distribution = "gamma",
     parameters = list(shape = mobility_gamma_shape, rate = gamma_rate)
)

# mobility_omega - Mobility population scaling parameter
priors_default$parameters_global$mobility_omega <- list(
     description = "Mobility population scaling parameter",
     distribution = "gamma",
     parameters = list(shape = mobility_omega_shape, rate = gamma_rate)
)

# Load vaccine effectiveness parameters from est_vaccine_effectiveness output
vaccine_param_file <- file.path(PATHS$MODEL_INPUT, "param_vaccine_effectiveness.csv")
if (file.exists(vaccine_param_file)) {
     param_vaccine <- read.csv(vaccine_param_file)
} else {
     warning("Vaccine effectiveness parameter file not found. Using default values.")
     # Define default values as fallback
     param_vaccine <- data.frame(
          variable_name = rep(c("omega_1", "omega_2", "phi_1", "phi_2"), each = 3),
          parameter_name = rep(c("mean", "low", "high"), 4),
          parameter_value = c(
               # omega_1 defaults
               0.0007, 0.0001, 0.002,
               # omega_2 defaults
               0.0005, 0.00001, 0.001,
               # phi_1 defaults
               0.787, 0.7, 0.85,
               # phi_2 defaults
               0.768, 0.65, 0.85
          )
     )
}

# Helper function to extract parameter value from the loaded data
get_vaccine_param <- function(var_name, param_name) {
     val <- param_vaccine$parameter_value[param_vaccine$variable_name == var_name &
                                               param_vaccine$parameter_name == param_name]
     if (length(val) == 0 || is.na(val)) {
          stop(paste("Missing parameter:", var_name, param_name))
     }
     return(val)
}

# omega_1 - Vaccine waning rate (one dose)
# Based on Xu et al. (2024) meta-regression, fitted using est_vaccine_effectiveness()
uncertainty_inflation <- 0.05  # Increase CI width

omega_1_mean <- get_vaccine_param("omega_1", "mean")
omega_1_low <- get_vaccine_param("omega_1", "low")
omega_1_high <- get_vaccine_param("omega_1", "high")

# Validate inputs
if (omega_1_low >= omega_1_high) {
     warning("omega_1: low >= high, swapping values")
     temp <- omega_1_low
     omega_1_low <- omega_1_high
     omega_1_high <- temp
}

# Ensure mean is within bounds
if (omega_1_mean < omega_1_low || omega_1_mean > omega_1_high) {
     warning("omega_1: mean outside of CI, adjusting to midpoint")
     omega_1_mean <- (omega_1_low + omega_1_high) / 2
}

# Inflate uncertainty
omega_1_range <- omega_1_high - omega_1_low
omega_1_low_inflated <- pmax(0.00001, omega_1_low - omega_1_range * uncertainty_inflation)
omega_1_high_inflated <- omega_1_high + omega_1_range * uncertainty_inflation

# Final check
if (omega_1_low_inflated >= omega_1_high_inflated) {
     stop("omega_1: Invalid CI after inflation. Check input data.")
}

omega_1_fit <- fit_gamma_from_ci(
     mode_val = omega_1_mean,
     ci_lower = omega_1_low_inflated,
     ci_upper = omega_1_high_inflated
)

priors_default$parameters_global$omega_1 <- list(
     description = "Vaccine waning rate (one dose, per day)",
     distribution = "gamma",
     parameters = list(
          shape = omega_1_fit$shape,
          rate = omega_1_fit$rate
     )
)

# omega_2 - Vaccine waning rate (two dose)
# Based on Xu et al. (2024) meta-regression, fitted using est_vaccine_effectiveness()
omega_2_mean <- get_vaccine_param("omega_2", "mean")
omega_2_low <- get_vaccine_param("omega_2", "low")
omega_2_high <- get_vaccine_param("omega_2", "high")

# Validate inputs
if (omega_2_low >= omega_2_high) {
     warning("omega_2: low >= high, swapping values")
     temp <- omega_2_low
     omega_2_low <- omega_2_high
     omega_2_high <- temp
}

# Ensure mean is within bounds
if (omega_2_mean < omega_2_low || omega_2_mean > omega_2_high) {
     warning("omega_2: mean outside of CI, adjusting to midpoint")
     omega_2_mean <- (omega_2_low + omega_2_high) / 2
}

# Inflate uncertainty
omega_2_range <- omega_2_high - omega_2_low
omega_2_low_inflated <- pmax(0.00001, omega_2_low - omega_2_range * uncertainty_inflation)
omega_2_high_inflated <- omega_2_high + omega_2_range * uncertainty_inflation

# Final check
if (omega_2_low_inflated >= omega_2_high_inflated) {
     stop("omega_2: Invalid CI after inflation. Check input data.")
}

omega_2_fit <- fit_gamma_from_ci(
     mode_val = omega_2_mean,
     ci_lower = omega_2_low_inflated,
     ci_upper = omega_2_high_inflated
)

priors_default$parameters_global$omega_2 <- list(
     description = "Vaccine waning rate (two dose, per day)",
     distribution = "gamma",
     parameters = list(
          shape = omega_2_fit$shape,
          rate = omega_2_fit$rate
     )
)


# phi_1 - Initial vaccine effectiveness (one dose)
# Based on Xu et al. (2024), fitted using est_vaccine_effectiveness()
uncertainty_inflation <- 0.05  # Reuse same inflation factor
phi_1_mean <- get_vaccine_param("phi_1", "mean")
phi_1_low <- get_vaccine_param("phi_1", "low")
phi_1_high <- get_vaccine_param("phi_1", "high")

phi_1_low_inflated <- pmax(0.001, phi_1_low * (1 - uncertainty_inflation))
phi_1_high_inflated <- pmin(0.999, phi_1_high * (1 + uncertainty_inflation))

# Final check
if (phi_1_low_inflated >= phi_1_high_inflated) {
     stop("phi_1: Invalid CI after inflation. Check input data.")
}

phi_1_fit <- fit_beta_from_ci(
     mode_val = phi_1_mean,
     ci_lower = phi_1_low_inflated,
     ci_upper = phi_1_high_inflated
)

priors_default$parameters_global$phi_1 <- list(
     description = "Initial vaccine effectiveness (one dose)",
     distribution = "beta",
     parameters = list(
          shape1 = phi_1_fit$shape1,
          shape2 = phi_1_fit$shape2
     )
)

# phi_2 - Initial vaccine effectiveness (two dose)
# Based on Xu et al. (2024), fitted using est_vaccine_effectiveness()
phi_2_mean <- get_vaccine_param("phi_2", "mean")
phi_2_low <- get_vaccine_param("phi_2", "low")
phi_2_high <- get_vaccine_param("phi_2", "high")

phi_2_low_inflated <- pmax(0.001, phi_2_low * (1 - uncertainty_inflation))
phi_2_high_inflated <- pmin(0.999, phi_2_high * (1 + uncertainty_inflation))

# Final check
if (phi_2_low_inflated >= phi_2_high_inflated) {
     stop("phi_2: Invalid CI after inflation. Check input data.")
}

phi_2_fit <- fit_beta_from_ci(
     mode_val = phi_2_mean,
     ci_lower = phi_2_low_inflated,
     ci_upper = phi_2_high_inflated
)

priors_default$parameters_global$phi_2 <- list(
     description = "Initial vaccine effectiveness (two dose)",
     distribution = "beta",
     parameters = list(
          shape1 = phi_2_fit$shape1,
          shape2 = phi_2_fit$shape2
     )
)



# ---- chi_endemic and chi_epidemic (PPV of clinical case definition) ----
# Beta distribution parameters from Weins et al. 2023 (PLOS Medicine,
# doi:10.1371/journal.pmed.1004286), fit in get_suspected_cases().
# Low estimate (all settings) -> chi_endemic; High estimate (outbreaks) -> chi_epidemic.
chi_param_file <- file.path(PATHS$MODEL_INPUT, "param_chi_suspected_cases.csv")
if (file.exists(chi_param_file)) {
     param_chi <- read.csv(chi_param_file)

     chi_endemic_shape1 <- param_chi$parameter_value[
          grepl("chi_endemic.*Low estimate", param_chi$variable_description) &
               param_chi$parameter_name == "shape1"
     ]
     chi_endemic_shape2 <- param_chi$parameter_value[
          grepl("chi_endemic.*Low estimate", param_chi$variable_description) &
               param_chi$parameter_name == "shape2"
     ]
     chi_epidemic_shape1 <- param_chi$parameter_value[
          grepl("chi_epidemic.*High estimate", param_chi$variable_description) &
               param_chi$parameter_name == "shape1"
     ]
     chi_epidemic_shape2 <- param_chi$parameter_value[
          grepl("chi_epidemic.*High estimate", param_chi$variable_description) &
               param_chi$parameter_name == "shape2"
     ]

     if (length(chi_endemic_shape1) == 0 || length(chi_endemic_shape2) == 0) {
          warning("Could not extract chi_endemic shape params from file. Using defaults.")
          chi_endemic_shape1 <- 5.43
          chi_endemic_shape2 <- 5.01
     }
     if (length(chi_epidemic_shape1) == 0 || length(chi_epidemic_shape2) == 0) {
          warning("Could not extract chi_epidemic shape params from file. Using defaults.")
          chi_epidemic_shape1 <- 4.79
          chi_epidemic_shape2 <- 1.53
     }
} else {
     warning("Chi PPV parameter file not found. Using default values.")
     chi_endemic_shape1  <- 5.43;  chi_endemic_shape2  <- 5.01
     chi_epidemic_shape1 <- 4.79;  chi_epidemic_shape2 <- 1.53
}

# chi_endemic - PPV among suspected cases during endemic periods (Weins et al. 2023 low estimate)
# Beta(5.43, 5.01) -> median ~0.52, 95% CI [0.24, 0.80]
priors_default$parameters_global$chi_endemic <- list(
     description = "PPV among suspected cases during endemic periods (Weins et al. 2023, all settings)",
     distribution = "beta",
     parameters = list(shape1 = chi_endemic_shape1, shape2 = chi_endemic_shape2)
)

# chi_epidemic - PPV among suspected cases during epidemic periods (Weins et al. 2023 high estimate)
# Beta(4.79, 1.53) -> median ~0.78, 95% CI [0.40, 0.99]
priors_default$parameters_global$chi_epidemic <- list(
     description = "PPV among suspected cases during epidemic periods (Weins et al. 2023, during outbreaks)",
     distribution = "beta",
     parameters = list(shape1 = chi_epidemic_shape1, shape2 = chi_epidemic_shape2)
)

# rho - Care-seeking rate (probability a symptomatic infection presents as a suspected case)
# Fitted from GEMS (Nasrin et al. 2013, PMC3748499) and Wiens et al. 2025 (PMC12013865)
# via get_rho_care_seeking_params().  Values read from param_rho_care_seeking.csv.
rho_param_file <- file.path(PATHS$MODEL_INPUT, "param_rho_care_seeking.csv")
if (file.exists(rho_param_file)) {
     param_rho    <- read.csv(rho_param_file, stringsAsFactors = FALSE)
     rho_shape1   <- param_rho$parameter_value[param_rho$parameter_name == "shape1"]
     rho_shape2   <- param_rho$parameter_value[param_rho$parameter_name == "shape2"]
     if (length(rho_shape1) == 0 || length(rho_shape2) == 0) {
          warning("Could not extract rho shape params from file. Using Beta(3, 7) fallback.")
          rho_shape1 <- 3.0; rho_shape2 <- 7.0
     }
} else {
     warning("param_rho_care_seeking.csv not found. Using Beta(3, 7) fallback.")
     rho_shape1 <- 3.0; rho_shape2 <- 7.0
}

priors_default$parameters_global$rho <- list(
     description = "Care-seeking rate: probability a symptomatic infection is reported as suspected (GEMS + Wiens 2025)",
     distribution = "beta",
     parameters = list(shape1 = rho_shape1, shape2 = rho_shape2)
)

# sigma - Proportion symptomatic
priors_default$parameters_global$sigma <- list(
     description = "Proportion symptomatic",
     distribution = "beta",
     parameters = list(shape1 = 4.30, shape2 = 13.51)
)

# zeta_1 - Symptomatic shedding rate (bacteria per infected person per day, LASER total-count units)
# Equilibrium analysis: W_eq = zeta_1 * Isym * (1-theta) / delta. With kappa fixed at 1e6,
# delta=1/30, Isym=1000, theta=0.5, the linear-saturation transition occurs at zeta_1* ~ 33.
# Prior centered at median=100 (3x above transition) with sdlog=3.0 gives approximately
# equal mass across regimes: ~36% linear (zeta<33), ~30% transition (33-330),
# ~32% mild saturation (330-33k), ~3% strong saturation (>33k).
# 95% CI=[0.28, 35777]; LASER default (7.5) at ~19th percentile.
priors_default$parameters_global$zeta_1 <- list(
     description = "Symptomatic shedding rate (total bacteria per infected person per day)",
     distribution = "lognormal",
     parameters = list(meanlog = log(100), sdlog = 3.0)
)

# zeta_2 - Asymptomatic shedding rate (bacteria per infected person per day, LASER total-count units)
# Maintains ~10:1 ratio to zeta_1 (symptomatic individuals shed substantially more).
# Median=10, sdlog=3.0 matches zeta_1 breadth; 95% CI=[0.03, 3578].
# LASER default (2.5) at ~32nd percentile.
priors_default$parameters_global$zeta_2 <- list(
     description = "Asymptomatic shedding rate (total bacteria per infected person per day)",
     distribution = "lognormal",
     parameters = list(meanlog = log(10), sdlog = 3.0)
)

# delta_reporting_cases - Symptom-onset-to-case reporting delay
# Incubation is already handled by the E compartment (iota parameter); this
# captures only the lag from symptom onset to surveillance report.
# Prior: TruncNorm(mean=2, sd=2, a=0, b=7) — mode near 1-2 days, hard ceiling at 7.
# sd=2 preserves exploration of 3-7 day delays, realistic for paper-based reporting in SSA.
# Sampled value is rounded to the nearest integer before passing to make_LASER_config().
priors_default$parameters_global$delta_reporting_cases <- list(
     description = "Symptom-onset-to-case reporting delay in days (integer, 0-7)",
     distribution = "truncnorm",
     parameters = list(mean = 2, sd = 2, a = 0, b = 7)
)

# delta_reporting_deaths - Symptom-onset-to-death reporting delay
# Incubation is already handled by the E compartment (iota parameter); this
# captures the lag from symptom onset through clinical progression to death report.
# Prior: TruncNorm(mean=4, sd=3, a=1, b=14) — mode near 3-5 days, hard ceiling at 14.
# Lower bound a=1: same-day death reporting is implausible (onset, death, and national
# registration cannot all occur within the same day in any realistic surveillance system).
# Sampled value is rounded to the nearest integer before passing to make_LASER_config().
priors_default$parameters_global$delta_reporting_deaths <- list(
     description = "Symptom-onset-to-death reporting delay in days (integer, 1-14)",
     distribution = "truncnorm",
     parameters = list(mean = 4, sd = 3, a = 1, b = 14)
)


#---------------------------------------------------
# Location specific parameters in alphabetical order
#---------------------------------------------------

# beta_j0_tot - Total base transmission rate (human + environmental)
#
# Switched from Gompertz to Lognormal. Rationale:
#   - The Gompertz fit was insensitive to the mode_val argument: the shape was
#     driven entirely by (ci_lower, ci_upper), and the intended ci_lower=1e-8
#     could not be achieved (actual Q2.5 = 6.93e-7, 100x off).
#   - Raising ci_upper in the Gompertz shifted the ENTIRE distribution upward,
#     inflating the prior median from 1.89e-5 to 3.80e-5 — too aggressive.
#   - Lognormal allows independent control of the median (via meanlog) and the
#     spread (via sdlog), so the upper tail can be extended without moving the
#     prior center.
#
# Design: median = 2e-5, Q97.5 = 2e-4
#   meanlog = log(2e-5) = -10.8198
#   sdlog   = (log(2e-4) - log(2e-5)) / qnorm(0.975) = log(10) / 1.96 = 1.1748
#
# This gives:
#   Q10  = 4.4e-6  Q50 = 2.0e-5  Q90 = 9.0e-5  Q97.5 = 2.0e-4
#   P(beta_j0_env > 5e-5) = 13%  (was 6% with Gompertz)
#   P(beta_j0_env > 1e-4) = 4%   (was <1% with Gompertz)
#   Fraction of samples below 3e-5 (MOZ-optimal range) = 64% (was 67%)
#
# The extended upper tail allows exploration of dominant-waterborne settings
# across the 40-country SSA ensemble without over-inflating the space for
# typical endemic settings.

beta_j0_tot_meanlog <- log(2e-5)
beta_j0_tot_sdlog   <- (log(2e-4) - log(2e-5)) / qnorm(0.975)

priors_default$parameters_location$beta_j0_tot <- list(
     description = "Total base transmission rate (human + environmental); lognormal with median=2e-5 and Q97.5=2e-4",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$beta_j0_tot$location[[iso]] <- list(
          distribution = "lognormal",
          parameters = list(
               meanlog = beta_j0_tot_meanlog,
               sdlog   = beta_j0_tot_sdlog
          )
     )
}


# p_beta - Proportion of human-to-human vs environmental transmission
beta_fit_p_beta <- fit_beta_from_ci(mode_val = 0.33, ci_lower = 0.1, ci_upper = 0.5)


priors_default$parameters_location$p_beta <- list(
     description = "Proportion of total base transmission that is human-to-human (0–1)",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$p_beta$location[[iso]] <- list(
          distribution = "beta",
          parameters = list(
               shape1 = beta_fit_p_beta$shape1,
               shape2 = beta_fit_p_beta$shape2
          )
     )
}













# tau_i - Country-level travel probabilities
# Beta distribution with parameters loaded from param_tau_departure.csv

tau_uncertainty_factor <- 0.001  # Increase tau uncertainty

priors_default$parameters_location$tau_i <- list(
     description = "Country-level travel probabilities",
     location = list()
)

# Load tau parameters from file
tau_param_file <- file.path(PATHS$MODEL_INPUT, "param_tau_departure.csv")
if (file.exists(tau_param_file)) {
     param_tau <- read.csv(tau_param_file)

     # Extract beta parameters for each location
     for (iso in j) {
          tau_shape1_orig <- param_tau$parameter_value[
               param_tau$i == iso &
                    param_tau$parameter_distribution == "beta" &
                    param_tau$parameter_name == "shape1"
          ]
          tau_shape2_orig <- param_tau$parameter_value[
               param_tau$i == iso &
                    param_tau$parameter_distribution == "beta" &
                    param_tau$parameter_name == "shape2"
          ]

          if (length(tau_shape1_orig) > 0 && length(tau_shape2_orig) > 0) {
               # Calculate the mean of the original distribution
               mean_tau <- tau_shape1_orig / (tau_shape1_orig + tau_shape2_orig)

               # Calculate the concentration (precision) of the original distribution
               concentration_orig <- tau_shape1_orig + tau_shape2_orig

               # Adjust concentration by uncertainty factor
               # Lower factor = lower concentration = higher uncertainty
               concentration_new <- concentration_orig * tau_uncertainty_factor

               # Recalculate shape parameters maintaining the same mean
               tau_shape1 <- mean_tau * concentration_new
               tau_shape2 <- (1 - mean_tau) * concentration_new

               priors_default$parameters_location$tau_i$location[[iso]] <- list(
                    distribution = "beta",
                    parameters = list(
                         shape1 = tau_shape1,
                         shape2 = tau_shape2
                    )
               )
          } else {
               # Default values if not found (very small travel probability)
               warning(paste("tau parameters not found for", iso, "- using defaults"))
               # Apply uncertainty factor to defaults as well
               default_shape1 <- 100 * tau_uncertainty_factor
               default_shape2 <- 1000000 * tau_uncertainty_factor
               priors_default$parameters_location$tau_i$location[[iso]] <- list(
                    distribution = "beta",
                    parameters = list(
                         shape1 = default_shape1,
                         shape2 = default_shape2
                    )
               )
          }
     }

} else {
     warning("tau parameter file not found. Using default values.")
     # Set default values for all locations with uncertainty adjustment
     for (iso in j) {
          priors_default$parameters_location$tau_i$location[[iso]] <- list(
               distribution = "beta",
               parameters = list(
                    shape1 = 100 * tau_uncertainty_factor,
                    shape2 = 1000000 * tau_uncertainty_factor
               )
          )
     }
}





# theta_j - WASH coverage
# Beta distribution fitted from weighted mean WASH estimates with uncertainty
priors_default$parameters_location$theta_j <- list(
     description = "WASH coverage index (proportion with adequate WASH)",
     location = list()
)

theta_uncertainty_one_sided <- 0.05

# Load WASH estimates
wash_param_file <- file.path(PATHS$MODEL_INPUT, "param_theta_WASH.csv")
if (file.exists(wash_param_file)) {

     param_wash <- read.csv(wash_param_file)

     for (iso in j) {

          wash_value <- param_wash$parameter_value[param_wash$j == iso]

          if (length(wash_value) > 0) {

               ci_lower <- pmax(0.001, wash_value - theta_uncertainty_one_sided)
               ci_upper <- pmin(0.999, wash_value + theta_uncertainty_one_sided)

          } else {

               wash_value <- mean(param_wash$parameter_value, na.rm=T)
               ci_lower <- pmax(0.001, wash_value - theta_uncertainty_one_sided*1.25)
               ci_upper <- pmin(0.999, wash_value + theta_uncertainty_one_sided*1.25)

          }

          theta_fit <- fit_beta_from_ci(
               mode_val = wash_value,
               ci_lower = ci_lower,
               ci_upper = ci_upper
          )

          priors_default$parameters_location$theta_j$location[[iso]] <- list(
               distribution = "beta",
               parameters = list(
                    shape1 = theta_fit$shape1,
                    shape2 = theta_fit$shape2
               )
          )

     }

} else {

     # Fallback defaults
     for (iso in j) {
          priors_default$parameters_location$theta_j$location[[iso]] <- list(
               distribution = "beta",
               parameters = list(
                    shape1 = 13.44396,
                    shape2 = 8.236964
               )
          )
     }
}




# Seasonality parameters (a_1, a_2, b_1, b_2) - Fourier wave function parameters
# Normal distributions with parameters loaded from param_seasonal_dynamics.csv
seasonality_uncertainty_factor <- 0.5  # Increase seasonality uncertainty

# Load seasonal dynamics parameters
seasonal_param_file <- file.path(PATHS$MODEL_INPUT, "param_seasonal_dynamics.csv")
seasonal_params_exist <- file.exists(seasonal_param_file)

if (seasonal_params_exist) {
     param_seasonal <- read.csv(seasonal_param_file)
     # Filter for cases response only
     param_seasonal <- param_seasonal[param_seasonal$response == "cases", ]
} else {
     warning("Seasonal dynamics parameter file not found. Using default values.")
}

# Create priors for each seasonality parameter
# Map R config keys (a_1_j etc.) to CSV parameter column values (a1 etc.)
seasonality_csv_lookup <- c("a_1_j" = "a1", "a_2_j" = "a2", "b_1_j" = "b1", "b_2_j" = "b2")

for (param in names(seasonality_csv_lookup)) {
     param_name <- param  # Storage key in priors list
     csv_param  <- seasonality_csv_lookup[[param]]  # CSV column value

     priors_default$parameters_location[[param_name]] <- list(
          description = paste0("Seasonality Fourier coefficient ", param, " (cases)"),
          location = list()
     )

     # Load parameters for each location
     for (iso in j) {
          if (seasonal_params_exist) {
               # Extract mean and standard error for this parameter and location
               param_row <- param_seasonal[
                    param_seasonal$country_iso_code == iso &
                         param_seasonal$parameter == csv_param,
               ]

               if (nrow(param_row) > 0) {
                    mean_orig <- param_row$mean[1]
                    se_orig <- param_row$se[1]

                    # Adjust standard error by uncertainty factor
                    # Lower factor = higher SE = higher uncertainty
                    se_adjusted <- se_orig / sqrt(seasonality_uncertainty_factor)

                    priors_default$parameters_location[[param_name]]$location[[iso]] <- list(
                         distribution = "normal",
                         parameters = list(
                              mean = mean_orig,
                              sd = se_adjusted
                         )
                    )
               } else {
                    # Default values if not found
                    warning(paste("Seasonal parameter", param, "not found for", iso, "- using defaults"))
                    priors_default$parameters_location[[param_name]]$location[[iso]] <- list(
                         distribution = "normal",
                         parameters = list(
                              mean = 0,
                              sd = 0.5 / sqrt(seasonality_uncertainty_factor)
                         )
                    )
               }
          } else {
               # Default values if file doesn't exist
               priors_default$parameters_location[[param_name]]$location[[iso]] <- list(
                    distribution = "normal",
                    parameters = list(
                         mean = 0,
                         sd = 0.5 / sqrt(seasonality_uncertainty_factor)
                    )
               )
          }
     }
}



#---------------------------------------------------
# Initial conditions parameters (location-specific)
#---------------------------------------------------

# Initial condition proportions for each compartment
# Using biologically plausible Beta priors as defaults
# These can be replaced by more informative priors when est_initial_conditions() is called
# Priors are designed to sum to approximately 1.0 in expectation while reflecting
# typical epidemiological patterns in African cholera settings

# prop_S_initial will be estimated using constrained residual method

# prop_V1_initial - Initial proportion with one vaccine dose
priors_default$parameters_location$prop_V1_initial <- list(
     description = "Initial proportion in one-dose vaccine (V1) compartment",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$prop_V1_initial$location[[iso]] <- list(
          distribution = "beta",
          parameters = list(shape1 = 0.5, shape2 = 49.5)
     )
}

# prop_V2_initial - Initial proportion with two vaccine doses
priors_default$parameters_location$prop_V2_initial <- list(
     description = "Initial proportion in two-dose vaccine (V2) compartment",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$prop_V2_initial$location[[iso]] <- list(
          distribution = "beta",
          parameters = list(shape1 = 0.5, shape2 = 99.5)
     )
}

# prop_E_initial - Initial proportion exposed
priors_default$parameters_location$prop_E_initial <- list(
     description = "Initial proportion in exposed (E) compartment",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$prop_E_initial$location[[iso]] <- list(
          distribution = "beta",
          parameters = list(shape1 = 0.01, shape2 = 99999.99)
     )
}

# prop_I_initial - Initial proportion infected
priors_default$parameters_location$prop_I_initial <- list(
     description = "Initial proportion in infected (I) compartment",
     location = list()
)

# Load population data to calculate location-specific proportions
pop_file <- file.path(PATHS$DATA_DEMOGRAPHICS, "UN_world_population_prospects_daily.csv")
if (file.exists(pop_file)) {
     population_data <- read.csv(pop_file, stringsAsFactors = FALSE)
     population_data$date <- as.Date(population_data$date)

     for (iso in j) {
          # Get population at model start date
          pop_loc <- population_data[population_data$iso_code == iso, ]
          if (nrow(pop_loc) > 0) {
               time_diffs <- abs(as.numeric(difftime(pop_loc$date, date_start, units = "days")))
               closest_idx <- which.min(time_diffs)
               population <- pop_loc$total_population[closest_idx]

               if (!is.na(population) && population > 0) {
                    # Calculate proportion for 100 infected
                    target_infected <- 100
                    mean_prop <- target_infected / population

                    # Beta(1, b) where mean = 1/(1+b)
                    # Solve for b: mean_prop = 1/(1+b) => b = (1/mean_prop) - 1
                    shape2_val <- max(2, (1 / mean_prop) - 1)  # Ensure b >= 2 for stability

                    priors_default$parameters_location$prop_I_initial$location[[iso]] <- list(
                         distribution = "beta",
                         parameters = list(
                              shape1 = 1,
                              shape2 = shape2_val
                         )
                    )
               } else {
                    # Fallback if population invalid
                    priors_default$parameters_location$prop_I_initial$location[[iso]] <- list(
                         distribution = "beta",
                         parameters = list(
                              shape1 = 1,
                              shape2 = 9999
                         )
                    )
               }
          } else {
               # Fallback if no population data
               priors_default$parameters_location$prop_I_initial$location[[iso]] <- list(
                    distribution = "beta",
                    parameters = list(
                         shape1 = 1,
                         shape2 = 9999
                    )
               )
          }
     }
} else {
     # Fallback if population file doesn't exist
     warning("Population file not found. Using default I compartment priors.")
     for (iso in j) {
          priors_default$parameters_location$prop_I_initial$location[[iso]] <- list(
               distribution = "beta",
               parameters = list(
                    shape1 = 1,
                    shape2 = 9999
               )
          )
     }
}

# prop_R_initial - Initial proportion recovered/immune
priors_default$parameters_location$prop_R_initial <- list(
     description = "Initial proportion in recovered/immune (R) compartment",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$prop_R_initial$location[[iso]] <- list(
          distribution = "beta",
          parameters = list(shape1 = 3.5, shape2 = 14)
     )
}



# Update default priors with estimated initial conditions for V1 and V2

initial_conditions_V1_V2 <- est_initial_V1_V2(
     PATHS = PATHS,
     priors = priors_default,
     config = config_default,
     n_samples = 100,
     t0 = date_start,
     variance_inflation = 0,
     parallel = TRUE,
     verbose = FALSE
)


# Update initial conditions priors with priors from est_initial_V1_V2()
# The new structure matches priors_default exactly, so integration is simple
# Only update locations that already exist - do NOT create new locations

n_updated <- 0

# Update prop_V1_initial for each location
for (loc in names(initial_conditions_V1_V2$parameters_location$prop_V1_initial$parameters$location)) {
     # Only update if location already exists in priors_default
     if (!is.null(priors_default$parameters_location$prop_V1_initial$location[[loc]])) {
          loc_estimate <- initial_conditions_V1_V2$parameters_location$prop_V1_initial$parameters$location[[loc]]

          if (!is.na(loc_estimate$shape1)) {
               # Extract parameters and create proper structure
               priors_default$parameters_location$prop_V1_initial$location[[loc]] <- list(
                    distribution = "beta",
                    parameters = list(
                         shape1 = loc_estimate$shape1,
                         shape2 = loc_estimate$shape2
                    )
               )
               n_updated <- n_updated + 1
          }
     }
}

# Update prop_V2_initial for each location
for (loc in names(initial_conditions_V1_V2$parameters_location$prop_V2_initial$parameters$location)) {
     # Only update if location already exists in priors_default
     if (!is.null(priors_default$parameters_location$prop_V2_initial$location[[loc]])) {
          loc_estimate <- initial_conditions_V1_V2$parameters_location$prop_V2_initial$parameters$location[[loc]]

          if (!is.na(loc_estimate$shape1)) {
               # Extract parameters and create proper structure
               priors_default$parameters_location$prop_V2_initial$location[[loc]] <- list(
                    distribution = "beta",
                    parameters = list(
                         shape1 = loc_estimate$shape1,
                         shape2 = loc_estimate$shape2
                    )
               )
          }
     }
}



# Update default priors with estimated initial conditions for E and I

# Define location-specific variance inflation for E/I compartments
# Higher values = more uncertainty in initial E/I estimates
# E/I have higher baseline uncertainty due to short-term dynamics
# Only includes ISO codes in MOSAIC::iso_codes_mosaic
variance_inflation_E_I <- c(
     "AGO" = 120,  # Angola: Improving surveillance
     "BDI" = 110,  # Burundi: Limited resources
     "BEN" = 110,  # Benin: Moderate surveillance
     "BFA" = 110,  # Burkina Faso: Moderate uncertainty
     "BWA" = 65,   # Botswana: Good health systems
     "CAF" = 140,  # Central African Republic: Limited data quality
     "CIV" = 105,  # Côte d'Ivoire: Moderate systems
     "CMR" = 105,  # Cameroon: Moderate data quality
     "COD" = 160,  # Democratic Republic of Congo: Large, varied conditions
     "COG" = 100,  # Congo: Moderate uncertainty
     "ERI" = 140,  # Eritrea: Limited international data
     "ETH" = 100,  # Ethiopia: Large system, variable quality
     "GAB" = 100,  # Gabon: Moderate surveillance (default)
     "GHA" = 80,   # Ghana: Good health systems
     "GIN" = 110,  # Guinea: Moderate data quality
     "GMB" = 110,  # Gambia: Small, limited data
     "GNB" = 140,  # Guinea-Bissau: Poor data quality
     "GNQ" = 100,  # Equatorial Guinea: Moderate uncertainty
     "KEN" = 40,   # Kenya: Good surveillance
     "LBR" = 100,  # Liberia: Better data quality
     "MLI" = 80,  # Mali: Some data limitations
     "MOZ" = 30,  # Mozambique: Moderate uncertainty increase
     "MRT" = 130,  # Mauritania: Limited resources
     "MWI" = 30,   # Malawi: Moderate uncertainty increase
     "NAM" = 80,   # Namibia: Good health systems (default)
     "NER" = 130,  # Niger: Limited resources
     "NGA" = 80,   # Nigeria: Large system, better data
     "RWA" = 55,   # Rwanda: Excellent health systems
     "SEN" = 95,   # Senegal: Moderate surveillance
     "SLE" = 100,  # Sierra Leone: Improved surveillance
     "SOM" = 200,  # Somalia: Very limited surveillance data
     "SSD" = 180,  # South Sudan: Conflict-affected, high uncertainty
     "SWZ" = 90,   # Eswatini: Small, moderate systems
     "TCD" = 150,  # Chad: High cholera burden, more uncertainty
     "TGO" = 120,  # Togo: Limited resources
     "TZA" = 90,   # Tanzania: Moderate data quality
     "UGA" = 80,   # Uganda: Good health systems
     "ZAF" = 40,   # South Africa: Excellent surveillance
     "ZMB" = 30,   # Zambia: Good surveillance system
     "ZWE" = 30   # Zimbabwe: Moderate uncertainty increase
)

# Use location-specific variance inflation with single function call
initial_conditions_E_I <- est_initial_E_I(
     PATHS = PATHS,
     priors = priors_default,
     config = config_default,
     n_samples = 100,
     t0 = date_start,
     lookback_days = 3,
     variance_inflation = variance_inflation_E_I,  # Named vector for location-specific values
     verbose = FALSE,
     parallel = TRUE
)

     n_updated_E_I <- 0

     # Update prop_E_initial for each location
     for (loc in names(initial_conditions_E_I$parameters_location$prop_E_initial$parameters$location)) {
          # Only update if location already exists in priors_default
          if (!is.null(priors_default$parameters_location$prop_E_initial$location[[loc]])) {
               loc_estimate <- initial_conditions_E_I$parameters_location$prop_E_initial$parameters$location[[loc]]

               if (!is.na(loc_estimate$shape1)) {
                    # Extract parameters and create proper structure
                    priors_default$parameters_location$prop_E_initial$location[[loc]] <- list(
                         distribution = "beta",
                         parameters = list(
                              shape1 = loc_estimate$shape1,
                              shape2 = loc_estimate$shape2
                         )
                    )
                    n_updated_E_I <- n_updated_E_I + 1
               }
          }
     }

     # Update prop_I_initial for each location
     for (loc in names(initial_conditions_E_I$parameters_location$prop_I_initial$parameters$location)) {
          # Only update if location already exists in priors_default
          if (!is.null(priors_default$parameters_location$prop_I_initial$location[[loc]])) {
               loc_estimate <- initial_conditions_E_I$parameters_location$prop_I_initial$parameters$location[[loc]]

               if (!is.na(loc_estimate$shape1)) {
                    # Extract parameters and create proper structure
                    priors_default$parameters_location$prop_I_initial$location[[loc]] <- list(
                         distribution = "beta",
                         parameters = list(
                              shape1 = loc_estimate$shape1,
                              shape2 = loc_estimate$shape2
                         )
                    )
               }
          }
     }


# =============================================================================
# POST-ESTIMATION ADJUSTMENT: Lower mean initial E/I for specific countries
# =============================================================================

# Countries showing systematic overestimation of initial conditions
# Apply scaling factors to reduce mean E and I while preserving relative uncertainty

adjustment_factors_E_I <- list(
     AGO = 0.01,
     BEN = 0.01,
     BFA = 0.01,
     BWA = 0.00,  # Near-zero: no active cholera at model start (uses Beta(0.01, 99999.99))
     CAF = 0.01,
     CIV = 0.01,
     CMR = 0.01,
     COD = 0.75,
     COG = 0.05,
     ERI = 0.00,  # Near-zero: very limited international data; no active cholera
     GAB = 0.00,  # Near-zero: no active cholera at model start
     GHA = 0.001,
     GIN = 0.001,
     GMB = 0.001,
     GNB = 0.001,
     GNQ = 0.00,  # Near-zero: no active cholera at model start
     KEN = 1.2,
     LBR = 0.001,
     MLI = 0.00,  # Near-zero: no active cholera at model start
     MOZ = 0.4,
     MRT = 0.00,  # Near-zero: no active cholera at model start
     MWI = 0.3,
     NAM = 0.2,
     NER = 0.1,
     NGA = 1.1,
     RWA = 0.01,
     SEN = 0.001,
     SLE = 0.001,
     SOM = 0.5,
     SSD = 0.1,
     SWZ = 0.00,  # Near-zero: no active cholera at model start
     TCD = 0.001,
     TGO = 0.1,
     TZA = 0.05,
     UGA = 0.3,
     ZAF = 0.01,
     ZMB = 0.3,
     ZWE = 0.075
)

cat("\nApplying post-estimation mean adjustments for initial E and I:\n")

for (iso in names(adjustment_factors_E_I)) {
     scaling_factor <- adjustment_factors_E_I[[iso]]

     # Adjust prop_E_initial
     if (!is.null(priors_default$parameters_location$prop_E_initial$location[[iso]])) {
          old_params_E <- priors_default$parameters_location$prop_E_initial$location[[iso]]$parameters
          old_mean_E <- old_params_E$shape1 / (old_params_E$shape1 + old_params_E$shape2)

          if (scaling_factor == 0) {
               # Zero scaling: no active E at model start.
               # Cannot compute new Beta via mean/CV rescale (0/0 = NaN).
               # Use the minimum-mass default prior instead: Beta(0.01, 99999.99)
               # gives mean ~1e-7, placing virtually all mass at 0.
               priors_default$parameters_location$prop_E_initial$location[[iso]]$parameters <- list(
                    shape1 = 0.01,
                    shape2 = 99999.99
               )
               cat(sprintf("  %s E: %.6f -> ~0 (near-zero prior, 100%% reduction)\n",
                           iso, old_mean_E))
          } else {
               # Calculate new mean (scaled down)
               new_mean_E <- old_mean_E * scaling_factor

               # Preserve relative uncertainty (CV)
               # CV = sqrt(variance) / mean for Beta distribution
               old_var_E <- (old_params_E$shape1 * old_params_E$shape2) /
                            ((old_params_E$shape1 + old_params_E$shape2)^2 *
                             (old_params_E$shape1 + old_params_E$shape2 + 1))
               old_cv_E <- sqrt(old_var_E) / old_mean_E

               # Fit new Beta with scaled mean and same CV
               new_var_E <- (new_mean_E * old_cv_E)^2

               # Beta parameters from mean and variance
               common_term_E <- new_mean_E * (1 - new_mean_E) / new_var_E - 1
               new_shape1_E <- max(0.01, new_mean_E * common_term_E)
               new_shape2_E <- max(0.01, (1 - new_mean_E) * common_term_E)

               priors_default$parameters_location$prop_E_initial$location[[iso]]$parameters <- list(
                    shape1 = new_shape1_E,
                    shape2 = new_shape2_E
               )

               cat(sprintf("  %s E: %.6f -> %.6f (%.0f%% reduction)\n",
                           iso, old_mean_E, new_mean_E, (1 - scaling_factor) * 100))
          }
     }

     # Adjust prop_I_initial
     if (!is.null(priors_default$parameters_location$prop_I_initial$location[[iso]])) {
          old_params_I <- priors_default$parameters_location$prop_I_initial$location[[iso]]$parameters
          old_mean_I <- old_params_I$shape1 / (old_params_I$shape1 + old_params_I$shape2)

          if (scaling_factor == 0) {
               # Zero scaling: no active I at model start.
               # Use the minimum-mass default prior: Beta(0.01, 99999.99) ~ mean 1e-7.
               priors_default$parameters_location$prop_I_initial$location[[iso]]$parameters <- list(
                    shape1 = 0.01,
                    shape2 = 99999.99
               )
               cat(sprintf("  %s I: %.6f -> ~0 (near-zero prior, 100%% reduction)\n",
                           iso, old_mean_I))
          } else {
               # Calculate new mean (scaled down)
               new_mean_I <- old_mean_I * scaling_factor

               # Preserve relative uncertainty (CV)
               old_var_I <- (old_params_I$shape1 * old_params_I$shape2) /
                            ((old_params_I$shape1 + old_params_I$shape2)^2 *
                             (old_params_I$shape1 + old_params_I$shape2 + 1))
               old_cv_I <- sqrt(old_var_I) / old_mean_I

               # Fit new Beta with scaled mean and same CV
               new_var_I <- (new_mean_I * old_cv_I)^2

               # Beta parameters from mean and variance
               common_term_I <- new_mean_I * (1 - new_mean_I) / new_var_I - 1
               new_shape1_I <- max(0.01, new_mean_I * common_term_I)
               new_shape2_I <- max(0.01, (1 - new_mean_I) * common_term_I)

               priors_default$parameters_location$prop_I_initial$location[[iso]]$parameters <- list(
                    shape1 = new_shape1_I,
                    shape2 = new_shape2_I
               )

               cat(sprintf("  %s I: %.6f -> %.6f (%.0f%% reduction)\n",
                           iso, old_mean_I, new_mean_I, (1 - scaling_factor) * 100))
          }
     }
}

cat("\nPost-estimation adjustments complete.\n")


# Update default priors with estimated initial conditions for R

# Define location-specific variance inflation for R compartment
# Higher values = more uncertainty, allowing for greater variation in estimates
# Only includes ISO codes in MOSAIC::iso_codes_mosaic
variance_inflation_R <- c(
     "AGO" = 3,   # Angola: Further reduced
     "BDI" = 4,   # Burundi: Further reduced
     "BEN" = 4,   # Benin: Further reduced
     "BFA" = 14,  # Burkina Faso: Moderate uncertainty
     "BWA" = 50,  # Botswana: Maximum uncertainty
     "CAF" = 24,  # Central African Republic: Increased - limited data quality
     "CIV" = 13,  # Côte d'Ivoire: Moderate systems
     "CMR" = 4,   # Cameroon: Further reduced
     "COD" = 2,   # Democratic Republic of Congo: Further decreased
     "COG" = 4,   # Congo: Further reduced
     "ERI" = 100, # Eritrea: Extreme maximum uncertainty - very limited international data
     "ETH" = 13,  # Ethiopia: Large system, variable quality
     "GAB" = 20,  # Gabon: High uncertainty
     "GHA" = 3,   # Ghana: Further reduced
     "GIN" = 4,   # Guinea: Further reduced
     "GMB" = 60,  # Gambia: Maximum uncertainty - small, limited data
     "GNB" = 0.5, # Guinea-Bissau: Increased slightly from ultra-low
     "GNQ" = 4,   # Equatorial Guinea: Further reduced
     "KEN" = 5,   # Kenya: Good surveillance
     "LBR" = 1,   # Liberia: Minimum variance inflation
     "MLI" = 14,  # Mali: Slightly decreased - data limitations
     "MOZ" = 1.5, # Mozambique: Further decreased
     "MRT" = 5,   # Mauritania: Further reduced
     "MWI" = 2,   # Malawi: Decreased further
     "NAM" = 8,   # Namibia: Good health systems (default)
     "NER" = 5,   # Niger: Further reduced
     "NGA" = 3,   # Nigeria: Further reduced
     "RWA" = 7,   # Rwanda: Excellent health systems
     "SEN" = 3,   # Senegal: Further reduced
     "SLE" = 2,   # Sierra Leone: Decreased further
     "SOM" = 2,   # Somalia: Increased uncertainty
     "SSD" = 4,   # South Sudan: Decreased further
     "SWZ" = 2,   # Eswatini: Decreased more
     "TCD" = 4,   # Chad: Decreased further
     "TGO" = 6,   # Togo: Further reduced
     "TZA" = 4,   # Tanzania: Further reduced
     "UGA" = 8,   # Uganda: Increased uncertainty
     "ZAF" = 6,   # South Africa: Excellent surveillance
     "ZMB" = 3,   # Zambia: Further reduced
     "ZWE" = 1.5  # Zimbabwe: Further decreased
)

# Use location-specific variance inflation with single function call
initial_conditions_R <- est_initial_R(
     PATHS = PATHS,
     priors = priors_default,
     config = config_default,
     n_samples = 100,
     t0 = date_start,
     disaggregate = TRUE,
     variance_inflation = variance_inflation_R,  # Named vector for location-specific values
     verbose = FALSE,
     parallel = TRUE
)


# Update initial conditions priors with priors from est_initial_R()
# The new structure matches priors_default exactly, so integration is simple
# Only update locations that already exist - do NOT create new locations

n_updated_R <- 0

# Update prop_R_initial for each location
for (loc in names(initial_conditions_R$parameters_location$prop_R_initial$parameters$location)) {

     # Only update if location already exists in priors_default
     if (!is.null(priors_default$parameters_location$prop_R_initial$location[[loc]])) {
          loc_estimate <- initial_conditions_R$parameters_location$prop_R_initial$parameters$location[[loc]]

          if (!is.na(loc_estimate$shape1)) {
               # Extract parameters and create proper structure
               priors_default$parameters_location$prop_R_initial$location[[loc]] <- list(
                    distribution = "beta",
                    parameters = list(
                         shape1 = loc_estimate$shape1,
                         shape2 = loc_estimate$shape2
                    )
               )
               n_updated_R <- n_updated_R + 1
          }
     }
}


# Update default priors with estimated initial conditions for S (constrained residual)

# Define location-specific variance inflation for S compartment
# S is calculated as constrained residual, so variance inflation is typically 0 or very small
# Only use non-zero values for locations where you want to allow more uncertainty in the residual
# Only includes ISO codes in MOSAIC::iso_codes_mosaic
variance_inflation_S <- c(
     "AGO" = 0.00,  # Angola: Standard residual calculation
     "BDI" = 0.02,  # Burundi: Slight flexibility
     "BEN" = 0.00,  # Benin: Standard residual calculation
     "BFA" = 0.02,  # Burkina Faso: Slight flexibility
     "BWA" = 0.00,  # Botswana: Good systems, no inflation needed
     "CAF" = 0.05,  # Central African Republic: Allow slight flexibility
     "CIV" = 0.00,  # Côte d'Ivoire: Standard residual calculation
     "CMR" = 0.00,  # Cameroon: Standard residual calculation
     "COD" = 0.05,  # Democratic Republic of Congo: Large varied conditions
     "COG" = 0.02,  # Congo: Slight flexibility
     "ERI" = 0.05,  # Eritrea: Limited data
     "ETH" = 0.01,  # Ethiopia: Minimal flexibility
     "GAB" = 0.00,  # Gabon: Standard residual calculation (default)
     "GHA" = 0.00,  # Ghana: Good systems, no inflation needed
     "GIN" = 0.02,  # Guinea: Slight flexibility
     "GMB" = 0.02,  # Gambia: Slight flexibility
     "GNB" = 0.04,  # Guinea-Bissau: More flexibility for poor data
     "GNQ" = 0.02,  # Equatorial Guinea: Slight flexibility
     "KEN" = 0.00,  # Kenya: Good systems, no inflation needed
     "LBR" = 0.01,  # Liberia: Minimal flexibility
     "MLI" = 0.03,  # Mali: Some flexibility for data limitations
     "MOZ" = 0.03,  # Mozambique: Some flexibility for variable quality
     "MRT" = 0.03,  # Mauritania: Some flexibility
     "MWI" = 0.00,  # Malawi: Standard residual calculation
     "NAM" = 0.00,  # Namibia: Good systems, no inflation needed (default)
     "NER" = 0.03,  # Niger: Some flexibility for limited resources
     "NGA" = 0.00,  # Nigeria: Standard residual calculation
     "RWA" = 0.00,  # Rwanda: Good systems, no inflation needed
     "SEN" = 0.00,  # Senegal: Standard residual calculation
     "SLE" = 0.01,  # Sierra Leone: Minimal flexibility
     "SOM" = 0.10,  # Somalia: Most flexibility due to very limited data
     "SSD" = 0.08,  # South Sudan: More flexibility due to poor data
     "SWZ" = 0.00,  # Eswatini: Standard residual calculation
     "TCD" = 0.05,  # Chad: Allow slight flexibility
     "TGO" = 0.02,  # Togo: Slight flexibility
     "TZA" = 0.00,  # Tanzania: Standard residual calculation
     "UGA" = 0.00,  # Uganda: Standard residual calculation
     "ZAF" = 0.00,  # South Africa: Good systems, no inflation needed
     "ZMB" = 0.00,  # Zambia: Good systems, no inflation needed
     "ZWE" = 0.03   # Zimbabwe: Some flexibility for economic challenges
)

# Use location-specific variance inflation with single function call
initial_conditions_S <- est_initial_S(
     PATHS = PATHS,
     priors = priors_default,
     config = config_default,
     n_samples = 100,
     t0 = date_start,
     variance_inflation = variance_inflation_S,  # Named vector for location-specific values
     verbose = FALSE,
     min_S_proportion = 0.001  # Default minimum S proportion
)


# Update initial conditions priors with estimates from est_initial_S()
# The new structure matches priors_default exactly, so integration is simple
# Only update locations that already exist - do NOT create new locations

n_updated_S <- 0

# Update prop_S_initial for each location
for (loc in names(initial_conditions_S$parameters_location$prop_S_initial$parameters$location)) {
     # Only update if location already exists in config
     if (loc %in% config_default$location_name) {
          loc_estimate <- initial_conditions_S$parameters_location$prop_S_initial$parameters$location[[loc]]

          if (!is.na(loc_estimate$shape1)) {
               # Create the S compartment in priors_default if it doesn't exist
               if (is.null(priors_default$parameters_location$prop_S_initial)) {
                    priors_default$parameters_location$prop_S_initial <- list(
                         description = "Initial proportion in susceptible (S) compartment from constrained residual",
                         location = list()
                    )
               }

               # Extract parameters and create proper structure
               priors_default$parameters_location$prop_S_initial$location[[loc]] <- list(
                    distribution = "beta",
                    parameters = list(
                         shape1 = loc_estimate$shape1,
                         shape2 = loc_estimate$shape2
                    )
               )
               n_updated_S <- n_updated_S + 1

          }
     }
}



# Add mu_j_baseline priors from disease mortality data
# mu_j_baseline is the daily per-capita mortality rate applied to the Isym stock in LASER.
# In LASER: reported_cases = Isym × rho / chi, disease_deaths = Isym × mu_j_baseline
# Therefore: deaths/reported_cases = mu_j_baseline × chi / rho
# Solving: mu_j_baseline = observed_reported_CFR × rho / chi
#
# Previous formula (WRONG): mu_j_baseline = CFR × sigma × rho (≈ CFR × 0.09)
# Correct formula:           mu_j_baseline = CFR × rho / chi    (≈ CFR × 0.43)
# The error caused a ~5× under-estimate of mu_j_baseline.

# Load disease mortality parameter data
mu_file <- file.path(PATHS$MODEL_INPUT, "param_mu_disease_mortality.csv")
if (file.exists(mu_file)) {
     mu_data <- read.csv(mu_file, stringsAsFactors = FALSE)

     # Calculate location-specific priors from Beta parameters
     # We'll use data from recent years (2020-2025) for more current estimates
     recent_years <- 2021:2025
     mu_recent <- mu_data[mu_data$t %in% recent_years, ]

     # Initialize mu_j_baseline prior structure
     priors_default$parameters_location$mu_j_baseline <- list(
          description = "Baseline daily mortality rate per symptomatic infected (per day), derived from observed reported CFR: mu_j_baseline = reported_CFR × rho / chi",
          location = list()
     )

     n_mu_j_added <- 0

     # Calculate statistics for each location
     for (loc in j) {
          loc_data <- mu_recent[mu_recent$j == loc, ]

          if (nrow(loc_data) > 0) {
               # Get mean values
               mean_data <- loc_data[loc_data$parameter_name == "mean", ]

               if (nrow(mean_data) > 0) {
                    # Get Beta parameters for CI calculation
                    shape1_data <- loc_data[loc_data$parameter_name == "shape1", ]
                    shape2_data <- loc_data[loc_data$parameter_name == "shape2", ]

                    # Calculate mean CFR across recent years
                    mean_cfr <- mean(mean_data$parameter_value, na.rm = TRUE)

                    # Calculate approximate CI from Beta parameters
                    ci_lower_vals <- c()
                    ci_upper_vals <- c()

                    for (year in recent_years) {
                         s1_row <- shape1_data[shape1_data$t == year, ]
                         s2_row <- shape2_data[shape2_data$t == year, ]

                         if (nrow(s1_row) > 0 && nrow(s2_row) > 0) {
                              s1 <- s1_row$parameter_value[1]
                              s2 <- s2_row$parameter_value[1]

                              if (!is.na(s1) && !is.na(s2) && s1 > 0 && s2 > 0) {
                                   ci_lower_vals <- c(ci_lower_vals, qbeta(0.025, s1, s2))
                                   ci_upper_vals <- c(ci_upper_vals, qbeta(0.975, s1, s2))
                              }
                         }
                    }

                    # Use mean of CIs across years
                    ci_lower <- mean(ci_lower_vals, na.rm = TRUE)
                    ci_upper <- mean(ci_upper_vals, na.rm = TRUE)

                    # Handle edge cases
                    if (is.na(mean_cfr) || mean_cfr <= 0) {
                         mean_cfr <- 0.02  # Default 2% CFR
                         ci_lower <- 0.005
                         ci_upper <- 0.08
                    }

                    # Clamp mean CFR to plausible range; ci_lower/ci_upper not used in fitting
                    mean_cfr <- max(min(mean_cfr, 0.4), 0.002)  # Keep mean in [0.2%, 40%]

                    #----------------------------------------
                    # Convert observed reported CFR to mu_j_baseline
                    #----------------------------------------
                    # In LASER: reported_cases = Isym × rho / chi, disease_deaths = Isym × mu_j_baseline
                    # Therefore: deaths/reported_cases = mu_j_baseline × chi / rho
                    # Solving: mu_j_baseline = observed_reported_CFR × rho / chi
                    #
                    # rho: mean of rho prior Beta(6.8143, 17.8944) ≈ 0.275
                    # chi: weighted average of chi_endemic (0.52) and chi_epidemic (0.76),
                    #      approximately 50/50 across surveillance period ≈ 0.64

                    rho_mean <- 0.275   # Mean of rho prior Beta(6.8143, 17.8944)
                    chi_mean <- 0.64    # Weighted avg chi_endemic(0.52) / chi_epidemic(0.76)

                    # Correct adjustment factor
                    cfr_to_mu_adjustment <- rho_mean / chi_mean  # ≈ 0.43

                    # Convert observed reported CFR to mu_j_baseline mean
                    mean_mu <- mean_cfr * cfr_to_mu_adjustment

                    cat(sprintf("  %s: reported_CFR=%.3f%% -> mu_j_baseline=%.6f (adjustment=%.3f)\n",
                                loc, mean_cfr * 100, mean_mu, cfr_to_mu_adjustment))

                    # Fit gamma using derived mean and a fixed target CV of 50%.
                    # The CFR data CIs are extremely tight (large Beta shape params)
                    # which would produce an over-confident prior. CV=50% (shape=4)
                    # allows the data to substantially update mu_j_baseline.
                    target_cv <- 0.5   # 50% CV → shape = 4
                    target_shape <- 1 / target_cv^2   # shape = 1/CV^2 = 4
                    target_rate  <- target_shape / mean_mu

                    priors_default$parameters_location$mu_j_baseline$location[[loc]] <- list(
                         distribution = "gamma",
                         parameters = list(
                              shape = target_shape,
                              rate  = target_rate
                         )
                    )

                    n_mu_j_added <- n_mu_j_added + 1
               }
          }

          # Add default if location not found
          if (is.null(priors_default$parameters_location$mu_j_baseline$location[[loc]])) {
               # Default: assume 0.35% reported CFR × (rho/chi = 0.43) ≈ 0.0015 mean
               # Shape=4 (CV=50%) to allow substantial data updating
               priors_default$parameters_location$mu_j_baseline$location[[loc]] <- list(
                    distribution = "gamma",
                    parameters = list(
                         shape = 4,
                         rate  = 2667   # Gives mean = 4/2667 ≈ 0.0015
                    )
               )
               n_mu_j_added <- n_mu_j_added + 1
          }
     }


} else {
     warning("Disease mortality parameter file not found. Using default mu_j_baseline priors.")

     # Add default gamma priors for all locations
     priors_default$parameters_location$mu_j_baseline <- list(
          description = "Baseline daily mortality rate per symptomatic infected (per day), derived from observed reported CFR: mu_j_baseline = reported_CFR × rho / chi",
          location = list()
     )

     for (loc in j) {
          # Default: 0.35% reported CFR × (0.275/0.64) ≈ 0.0015, CV=50% (shape=4)
          priors_default$parameters_location$mu_j_baseline$location[[loc]] <- list(
               distribution = "gamma",
               parameters = list(
                    shape = 4,
                    rate  = 2667   # Gives mean = 4/2667 ≈ 0.0015
               )
          )
     }
}

#----------------------------------------
# Additional IFR parameters for threshold-dependent model
#----------------------------------------

# mu_j_slope - Temporal trend in IFR per location
priors_default$parameters_location$mu_j_slope <- list(
     description = "Temporal trend in baseline IFR (proportion change over simulation period)",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$mu_j_slope$location[[iso]] <- list(
          distribution = "normal",
          parameters = list(
               mean = 0,      # No trend by default
               sd = 0.05      # ±10% change over simulation period (95% CI)
          )
     )
}

# mu_j_epidemic_factor - Proportional IFR increase during epidemics
# Gamma(shape=1, rate=2): mode=0 (no epidemic effect most probable), mean=0.5,
# 95th pct ~1.5. Encodes weakly informative belief that epidemic-mode CFR increase
# is modest but uncertain, with exponential decay away from zero.
priors_default$parameters_location$mu_j_epidemic_factor <- list(
     description = "Proportional increase in IFR during epidemic periods (e.g., 0.5 = 50% increase)",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$mu_j_epidemic_factor$location[[iso]] <- list(
          distribution = "gamma",
          parameters = list(
               shape = 1,  # Exponential: mode=0, most mass near zero
               rate  = 2   # Mean=0.5, 95th pct ~1.5
          )
     )
}

# epidemic_threshold - Location-specific epidemic regime activation threshold
#
# Units: dimensionless daily Isym/N point prevalence fraction.
# The LASER engine compares epidemic_threshold against
#   Isym[t - delta_reporting_cases] / N[t - delta_reporting_cases]
# at every daily tick to decide whether to apply epidemic-mode IFR and chi_epidemic.
#
# Derivation of prior means:
#   Reported weekly incidence (cases/100k/wk) is converted to Isym/N via:
#     Isym/N = (reported_per_100k / 1e5) * (chi_endemic / rho) / (7 * gamma_1)
#   (Little's Law under approximate steady state; sdlog = 0.5 captures factor-of-2
#    uncertainty from this approximation.)
#
# Data source: PATHS$DATA_PROCESSED cholera/weekly/cholera_surveillance_weekly_combined.csv
#   Median weekly reported incidence per 100k across outbreak-positive weeks (cases > 0).
#   Countries with < 10 outbreak weeks use the Zheng global reference (0.7/100k/wk),
#   the published SSA median from Zheng et al. (2022) IJID.
#
# Distribution: Lognormal(meanlog = log(prior_mean), sdlog = 0.5)

# Helper: convert Zheng weekly reported incidence to Isym/N point prevalence
convert_zheng_threshold <- function(zheng_weekly_per_100k, rho, chi, gamma_1) {
     (zheng_weekly_per_100k / 1e5) * (chi / rho) / (7 * gamma_1)
}

# Extract model parameters from config — do NOT hardcode these values
rho_val    <- config_default$rho
chi_val    <- config_default$chi_endemic
gamma1_val <- config_default$gamma_1

# Compute per-country median weekly incidence per 100k during outbreak-positive weeks.
# Cap the population join year at the maximum available year in demographics (2024)
# to handle surveillance records in 2025 that have no matching population row.
dem_max_year  <- max(dem_annual$year)

outbreak_rows <- surv_weekly[
     surv_weekly$iso_code %in% j &
     !is.na(surv_weekly$cases) &
     surv_weekly$cases > 0,
]
outbreak_rows$dem_year <- pmin(outbreak_rows$year, dem_max_year)

merged_surv <- merge(
     outbreak_rows,
     dem_annual[, c("iso_code", "year", "population")],
     by.x = c("iso_code", "dem_year"),
     by.y = c("iso_code", "year"),
     all.x = TRUE
)
merged_surv$weekly_incidence_per_100k <- merged_surv$cases / merged_surv$population * 1e5

# Per-country summary: outbreak week count and median weekly incidence per 100k
country_threshold_data <- do.call(rbind, lapply(j, function(iso) {
     rows <- merged_surv[
          merged_surv$iso_code == iso &
          !is.na(merged_surv$weekly_incidence_per_100k),
     ]
     data.frame(
          iso_code                  = iso,
          n_outbreak_weeks          = nrow(rows),
          median_incidence_per_100k = if (nrow(rows) > 0) median(rows$weekly_incidence_per_100k, na.rm = TRUE) else NA_real_,
          stringsAsFactors          = FALSE
     )
}))

# Countries with < 10 outbreak weeks fall back to the Zheng global SSA reference value.
# 0.7 per 100k per week is the published median from Zheng et al. (2022) IJID across
# SSA districts — a conservative (upper-side) choice for low-burden / data-sparse countries.
ZHENG_GLOBAL_FALLBACK_PER_100K   <- 0.7
MIN_OUTBREAK_WEEKS_FOR_DATA_PRIOR <- 10

country_threshold_data$use_fallback <- (
     country_threshold_data$n_outbreak_weeks < MIN_OUTBREAK_WEEKS_FOR_DATA_PRIOR |
     is.na(country_threshold_data$median_incidence_per_100k)
)

country_threshold_data$prior_mean <- ifelse(
     !country_threshold_data$use_fallback,
     convert_zheng_threshold(
          country_threshold_data$median_incidence_per_100k,
          rho_val, chi_val, gamma1_val
     ),
     convert_zheng_threshold(
          ZHENG_GLOBAL_FALLBACK_PER_100K,
          rho_val, chi_val, gamma1_val
     )
)

EPIDEMIC_THRESHOLD_SDLOG <- 0.5   # captures ~factor-of-2 uncertainty around Zheng conversion

priors_default$parameters_location$epidemic_threshold <- list(
     description = paste0(
          "Dimensionless daily Isym/N prevalence threshold for epidemic regime activation. ",
          "Compared against Isym[t - delta_reporting_cases] / N[t - delta_reporting_cases] in LASER. ",
          "Derived from observed median weekly reported incidence per 100k (outbreak-positive weeks) ",
          "converted via Zheng formula using config rho, chi_endemic, and gamma_1. ",
          "Lognormal(meanlog = log(prior_mean), sdlog = 0.5)."
     ),
     location = list()
)

for (iso in j) {
     idx <- which(country_threshold_data$iso_code == iso)
     priors_default$parameters_location$epidemic_threshold$location[[iso]] <- list(
          distribution = "lognormal",
          parameters   = list(
               meanlog = log(country_threshold_data$prior_mean[idx]),
               sdlog   = EPIDEMIC_THRESHOLD_SDLOG
          )
     )
}

# Verification summary
n_data_prior <- sum(!country_threshold_data$use_fallback)
n_fallback   <- sum(country_threshold_data$use_fallback)
all_ml <- sapply(j, function(iso)
     priors_default$parameters_location$epidemic_threshold$location[[iso]]$parameters$meanlog)

cat(sprintf(
     "\n[epidemic_threshold priors] Data-derived: %d | Fallback: %d | Total: %d\n",
     n_data_prior, n_fallback, length(j)
))
cat(sprintf(
     "[epidemic_threshold priors] prior_mean range: [%.2e, %.2e]\n",
     exp(min(all_ml)), exp(max(all_ml))
))
for (iso in c("ETH", "COD", "SLE", "BWA")) {
     pm <- priors_default$parameters_location$epidemic_threshold$location[[iso]]$parameters$meanlog
     ps <- priors_default$parameters_location$epidemic_threshold$location[[iso]]$parameters$sdlog
     fb <- if (country_threshold_data$use_fallback[country_threshold_data$iso_code == iso]) " [fallback]" else ""
     cat(sprintf("  %s%s: median=%.2e  95%% CI=[%.2e, %.2e]\n",
                 iso, fb, exp(pm), exp(pm - 1.96 * ps), exp(pm + 1.96 * ps)))
}

#----------------------------------------
# Psi star calibration parameters (location-specific)
#----------------------------------------

# psi_star_a - Shape/gain parameter for logit-scale suitability calibration (location-specific)
priors_default$parameters_location$psi_star_a <- list(
     description = "Shape/gain parameter for logit calibration of NN suitability psi (a>1 sharpens peaks, a<1 flattens)",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$psi_star_a$location[[iso]] <- list(
          distribution = "truncnorm",
          parameters = list(
               mean = 1,    # Neutral value: a=1 is identity (no transformation); mode=1
               sd   = 1.0,  # 95% CI: ~[0.08, 3.03]; P(a>2)=18.9% matches old Lognormal(0,0.9) at 22.1%
               a    = 0,    # Lower bound enforces a > 0 (required by calc_psi_star)
               b    = Inf   # No upper bound
          )
     )
}

# psi_star_b - Scale/offset parameter for logit-scale suitability calibration (location-specific)
priors_default$parameters_location$psi_star_b <- list(
     description = "Scale/offset parameter for logit calibration of NN suitability psi (shifts baseline up/down)",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$psi_star_b$location[[iso]] <- list(
          distribution = "normal",
          parameters = list(
               mean = 0,        # Centered at no offset
               sd = 2.5         # 95% CI: [-4.90, 4.90]; old posterior lower tail (-4.65) was below sd=2.0 95th
          )
     )
}

# psi_star_z - Smoothing weight parameter for causal EWMA (location-specific)
priors_default$parameters_location$psi_star_z <- list(
     description = "Smoothing weight for causal EWMA of calibrated suitability (z=1: no smoothing, z<1: smoothing)",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$psi_star_z$location[[iso]] <- list(
          distribution = "beta",
          parameters = list(
               shape1 = 1,      # Beta(1,1): uniform on [0,1]; no prior preference for smoothing level
               shape2 = 1       # Old posterior (MOZ) median=0.56, 95% CI=[0.07, 0.95] -- full range used
          )
     )
}

# psi_star_k - Time offset parameter for suitability calibration (location-specific)
# Biological rationale: k > 0 delays the suitability signal (epidemic follows/lags the peak),
# k < 0 advances it (epidemic precedes the peak).
# Both directions are permitted: in some settings epidemics may precede a broad suitability
# peak (e.g. early season explosive outbreaks) or lag it (slow accumulation in low-WASH areas).
# Bounds [-90, 90] cover the full plausible range; centred at 0 with sd=25 keeps most mass
# within ±50 days while allowing the data to identify the direction.
priors_default$parameters_location$psi_star_k <- list(
     description = "Time offset in days for suitability calibration (k>0: epidemic lags suitability peak; k<0: epidemic precedes suitability peak). Bounded to [-90, 90]: both lag and advance are permitted.",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$psi_star_k$location[[iso]] <- list(
          distribution = "truncnorm",
          parameters = list(
               mean = 0,        # Centered at no offset; data identifies direction
               sd = 25,         # Most mass within ±50 days
               a = -90,         # Lower bound: up to 90 days advance
               b = 90           # Upper bound: up to 90 days lag
          )
     )
}


# Save to file and add to MOSAIC R package

fp <- file.path(PATHS$ROOT, 'MOSAIC-pkg/inst/extdata/priors_default.json')

# save to file
jsonlite::write_json(priors_default, fp, pretty = TRUE, auto_unbox = TRUE)

# Read back to verify
tmp_priors <- jsonlite::fromJSON(fp, simplifyVector = FALSE)

identical(priors_default$parameters_global$alpha_1, tmp_priors$parameters_global$alpha_1)

# Note: R data object is saved as priors_default to match config_default naming convention
usethis::use_data(priors_default, overwrite = TRUE)

