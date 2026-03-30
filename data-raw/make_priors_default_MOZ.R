library(MOSAIC)
library(jsonlite)

# make_priors_default_MOZ.R - Generate MOZ-specific prior distributions
# Reuses all global priors from priors_default, extracts MOZ location priors,
# and re-estimates initial conditions for the 2017-01-01 start date.

MOSAIC::set_root_directory("~/MOSAIC")
PATHS <- MOSAIC::get_paths()

# Load MOZ config for date_start and population context
config_default_MOZ <- MOSAIC::config_default_MOZ
date_start <- as.Date(config_default_MOZ$date_start)

j <- "MOZ"

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

priors_default_MOZ <- list(
     metadata = list(
          version = "2.0",
          date = Sys.Date(),
          description = "MOZ-specific informative prior distributions for extended 2017-2026 calibration (updated from test_31 MOZ 6-7 analysis)"
     ),
     parameters_global = list(),
     parameters_location = list()
)

#========================================
# Global parameters: identical to priors_default
#========================================

# alpha_1 - Population mixing within metapops
# MOZ 3 best=0.49, #1 HSIC driver. Default prior mode=0.25 was too low.
# Beta(7, 8): mode=0.46, mean=0.47, 95% CI [0.22, 0.73]
priors_default_MOZ$parameters_global$alpha_1 <- list(
     description = "Population mixing within metapops (0-1, 1 = well-mixed)",
     distribution = "beta",
     parameters = list(shape1 = 7, shape2 = 8)
)

# alpha_2 - Degree of frequency driven transmission
beta_fit_alpha_2 <- fit_beta_from_ci(mode_val = 0.5, ci_lower = 0.25, ci_upper = 0.75)

priors_default_MOZ$parameters_global$alpha_2 <- list(
     description = "Degree of frequency driven transmission (0-1)",
     distribution = "beta",
     parameters = list(shape1 = beta_fit_alpha_2$shape1, shape2 = beta_fit_alpha_2$shape2)
)

# decay_days_long - Constrained to max 1 year (biological realism).
# MOZ 7 best model: 133 days. With U(30,1095), 66-80% of top sims used >365 days,
# creating degenerate environmental reservoirs. The tight prior forces proper
# seasonal forcing via suitability and immunity dynamics.
priors_default_MOZ$parameters_global$decay_days_long <- list(
     description = "Maximum V. cholerae survival time (days)",
     distribution = "uniform",
     parameters = list(min = 30, max = 365)
)

# decay_days_short - Tighter, biologically realistic. MOZ 7 best: 19.8 days.
# Field literature supports minimum Vibrio survival on the order of days to low weeks.
priors_default_MOZ$parameters_global$decay_days_short <- list(
     description = "Minimum V. cholerae survival time (days)",
     distribution = "truncnorm",
     parameters = list(mean = 16, sd = 7, a = 0.01, b = 29)
)

# decay_shape_1
priors_default_MOZ$parameters_global$decay_shape_1 <- list(
     description = "First shape parameter of Beta distribution for V. cholerae decay",
     distribution = "uniform",
     parameters = list(min = 0.1, max = 10.0)
)

# decay_shape_2
priors_default_MOZ$parameters_global$decay_shape_2 <- list(
     description = "Second shape parameter of Beta distribution for V. cholerae decay",
     distribution = "uniform",
     parameters = list(min = 0.1, max = 10.0)
)

# epsilon - Natural immunity waning rate
# Revert to data-informed prior: MOZ 2 best model found epsilon=4e-4, consistent
# with the original literature-derived estimate. The wider prior (mean=8e-4) didn't
# help — sampler stayed near 4e-4 regardless, indicating the data supports ~7yr duration.
priors_default_MOZ$parameters_global$epsilon <- list(
     description = "Natural immunity waning rate (per day)",
     distribution = "lognormal",
     parameters = list(mean = 3.9e-4, sd = 2.0e-4 * 2)  # Variance inflated 2x
)

# gamma_1 - Symptomatic/severe shedding duration rate
priors_default_MOZ$parameters_global$gamma_1 <- list(
     description = "Symptomatic/severe shedding duration rate (per day)",
     distribution = "lognormal",
     parameters = list(meanlog = log(1/10), sdlog = 0.5)
)

# gamma_2 - Asymptomatic/mild shedding duration rate
priors_default_MOZ$parameters_global$gamma_2 <- list(
     description = "Asymptomatic/mild shedding duration rate (per day)",
     distribution = "lognormal",
     parameters = list(meanlog = log(1/2), sdlog = 0.4)
)

# iota - Incubation rate
priors_default_MOZ$parameters_global$iota <- list(
     description = "Incubation rate (1/days)",
     distribution = "lognormal",
     parameters = list(meanlog = -0.337, sdlog = 0.4)
)

# kappa - Infectious dose
priors_default_MOZ$parameters_global$kappa <- list(
     description = "Concentration of V. cholerae for 50% infectious dose",
     distribution = "lognormal",
     parameters = list(meanlog = log(10^6) + 0.25^2, sdlog = 0.25)
)

# mobility_gamma and mobility_omega
param_gravity <- read.csv(file.path(PATHS$MODEL_INPUT, "param_gravity_model.csv"))
mobility_gamma_mode <- param_gravity$parameter_value[param_gravity$variable_name == "mobility_gamma"]
mobility_omega_mode <- param_gravity$parameter_value[param_gravity$variable_name == "mobility_omega"]

gamma_rate <- 2
mobility_gamma_shape <- mobility_gamma_mode * gamma_rate + 1
mobility_omega_shape <- mobility_omega_mode * gamma_rate + 1

priors_default_MOZ$parameters_global$mobility_gamma <- list(
     description = "Mobility distance decay parameter",
     distribution = "gamma",
     parameters = list(shape = mobility_gamma_shape, rate = gamma_rate)
)

priors_default_MOZ$parameters_global$mobility_omega <- list(
     description = "Mobility population scaling parameter",
     distribution = "gamma",
     parameters = list(shape = mobility_omega_shape, rate = gamma_rate)
)

# Vaccine effectiveness parameters
vaccine_param_file <- file.path(PATHS$MODEL_INPUT, "param_vaccine_effectiveness.csv")
if (file.exists(vaccine_param_file)) {
     param_vaccine <- read.csv(vaccine_param_file)
} else {
     warning("Vaccine effectiveness parameter file not found. Using default values.")
     param_vaccine <- data.frame(
          variable_name = rep(c("omega_1", "omega_2", "phi_1", "phi_2"), each = 3),
          parameter_name = rep(c("mean", "low", "high"), 4),
          parameter_value = c(0.0007, 0.0001, 0.002, 0.0005, 0.00001, 0.001,
                              0.787, 0.7, 0.85, 0.768, 0.65, 0.85)
     )
}

get_vaccine_param <- function(var_name, param_name) {
     val <- param_vaccine$parameter_value[param_vaccine$variable_name == var_name &
                                               param_vaccine$parameter_name == param_name]
     if (length(val) == 0 || is.na(val)) stop(paste("Missing parameter:", var_name, param_name))
     return(val)
}

uncertainty_inflation <- 0.05

# omega_1
omega_1_mean <- get_vaccine_param("omega_1", "mean")
omega_1_low <- get_vaccine_param("omega_1", "low")
omega_1_high <- get_vaccine_param("omega_1", "high")
if (omega_1_low >= omega_1_high) { temp <- omega_1_low; omega_1_low <- omega_1_high; omega_1_high <- temp }
if (omega_1_mean < omega_1_low || omega_1_mean > omega_1_high) omega_1_mean <- (omega_1_low + omega_1_high) / 2
omega_1_range <- omega_1_high - omega_1_low
omega_1_low_inflated <- pmax(0.00001, omega_1_low - omega_1_range * uncertainty_inflation)
omega_1_high_inflated <- omega_1_high + omega_1_range * uncertainty_inflation
omega_1_fit <- fit_gamma_from_ci(mode_val = omega_1_mean, ci_lower = omega_1_low_inflated, ci_upper = omega_1_high_inflated)

priors_default_MOZ$parameters_global$omega_1 <- list(
     description = "Vaccine waning rate (one dose, per day)",
     distribution = "gamma",
     parameters = list(shape = omega_1_fit$shape, rate = omega_1_fit$rate)
)

# omega_2
omega_2_mean <- get_vaccine_param("omega_2", "mean")
omega_2_low <- get_vaccine_param("omega_2", "low")
omega_2_high <- get_vaccine_param("omega_2", "high")
if (omega_2_low >= omega_2_high) { temp <- omega_2_low; omega_2_low <- omega_2_high; omega_2_high <- temp }
if (omega_2_mean < omega_2_low || omega_2_mean > omega_2_high) omega_2_mean <- (omega_2_low + omega_2_high) / 2
omega_2_range <- omega_2_high - omega_2_low
omega_2_low_inflated <- pmax(0.00001, omega_2_low - omega_2_range * uncertainty_inflation)
omega_2_high_inflated <- omega_2_high + omega_2_range * uncertainty_inflation
omega_2_fit <- fit_gamma_from_ci(mode_val = omega_2_mean, ci_lower = omega_2_low_inflated, ci_upper = omega_2_high_inflated)

priors_default_MOZ$parameters_global$omega_2 <- list(
     description = "Vaccine waning rate (two dose, per day)",
     distribution = "gamma",
     parameters = list(shape = omega_2_fit$shape, rate = omega_2_fit$rate)
)

# phi_1
phi_1_mean <- get_vaccine_param("phi_1", "mean")
phi_1_low <- get_vaccine_param("phi_1", "low")
phi_1_high <- get_vaccine_param("phi_1", "high")
phi_1_low_inflated <- pmax(0.001, phi_1_low * (1 - uncertainty_inflation))
phi_1_high_inflated <- pmin(0.999, phi_1_high * (1 + uncertainty_inflation))
phi_1_fit <- fit_beta_from_ci(mode_val = phi_1_mean, ci_lower = phi_1_low_inflated, ci_upper = phi_1_high_inflated)

priors_default_MOZ$parameters_global$phi_1 <- list(
     description = "Initial vaccine effectiveness (one dose)",
     distribution = "beta",
     parameters = list(shape1 = phi_1_fit$shape1, shape2 = phi_1_fit$shape2)
)

# phi_2
phi_2_mean <- get_vaccine_param("phi_2", "mean")
phi_2_low <- get_vaccine_param("phi_2", "low")
phi_2_high <- get_vaccine_param("phi_2", "high")
phi_2_low_inflated <- pmax(0.001, phi_2_low * (1 - uncertainty_inflation))
phi_2_high_inflated <- pmin(0.999, phi_2_high * (1 + uncertainty_inflation))
phi_2_fit <- fit_beta_from_ci(mode_val = phi_2_mean, ci_lower = phi_2_low_inflated, ci_upper = phi_2_high_inflated)

priors_default_MOZ$parameters_global$phi_2 <- list(
     description = "Initial vaccine effectiveness (two dose)",
     distribution = "beta",
     parameters = list(shape1 = phi_2_fit$shape1, shape2 = phi_2_fit$shape2)
)

# chi_endemic and chi_epidemic
chi_param_file <- file.path(PATHS$MODEL_INPUT, "param_chi_suspected_cases.csv")
if (file.exists(chi_param_file)) {
     param_chi <- read.csv(chi_param_file)
     chi_endemic_shape1 <- param_chi$parameter_value[
          grepl("chi_endemic.*Low estimate", param_chi$variable_description) & param_chi$parameter_name == "shape1"]
     chi_endemic_shape2 <- param_chi$parameter_value[
          grepl("chi_endemic.*Low estimate", param_chi$variable_description) & param_chi$parameter_name == "shape2"]
     chi_epidemic_shape1 <- param_chi$parameter_value[
          grepl("chi_epidemic.*High estimate", param_chi$variable_description) & param_chi$parameter_name == "shape1"]
     chi_epidemic_shape2 <- param_chi$parameter_value[
          grepl("chi_epidemic.*High estimate", param_chi$variable_description) & param_chi$parameter_name == "shape2"]
     if (length(chi_endemic_shape1) == 0 || length(chi_endemic_shape2) == 0) {
          chi_endemic_shape1 <- 5.43; chi_endemic_shape2 <- 5.01
     }
     if (length(chi_epidemic_shape1) == 0 || length(chi_epidemic_shape2) == 0) {
          chi_epidemic_shape1 <- 4.79; chi_epidemic_shape2 <- 1.53
     }
} else {
     chi_endemic_shape1 <- 5.43; chi_endemic_shape2 <- 5.01
     chi_epidemic_shape1 <- 4.79; chi_epidemic_shape2 <- 1.53
}

priors_default_MOZ$parameters_global$chi_endemic <- list(
     description = "PPV among suspected cases during endemic periods (Weins et al. 2023, all settings)",
     distribution = "beta",
     parameters = list(shape1 = chi_endemic_shape1, shape2 = chi_endemic_shape2)
)

priors_default_MOZ$parameters_global$chi_epidemic <- list(
     description = "PPV among suspected cases during epidemic periods (Weins et al. 2023, during outbreaks)",
     distribution = "beta",
     parameters = list(shape1 = chi_epidemic_shape1, shape2 = chi_epidemic_shape2)
)

# rho - Care-seeking rate
rho_param_file <- file.path(PATHS$MODEL_INPUT, "param_rho_care_seeking.csv")
if (file.exists(rho_param_file)) {
     param_rho <- read.csv(rho_param_file, stringsAsFactors = FALSE)
     rho_shape1 <- param_rho$parameter_value[param_rho$parameter_name == "shape1"]
     rho_shape2 <- param_rho$parameter_value[param_rho$parameter_name == "shape2"]
     if (length(rho_shape1) == 0 || length(rho_shape2) == 0) {
          rho_shape1 <- 3.0; rho_shape2 <- 7.0
     }
} else {
     rho_shape1 <- 3.0; rho_shape2 <- 7.0
}

priors_default_MOZ$parameters_global$rho <- list(
     description = "Care-seeking rate: probability a symptomatic infection is reported as suspected (GEMS + Wiens 2025)",
     distribution = "beta",
     parameters = list(shape1 = rho_shape1, shape2 = rho_shape2)
)

# sigma - Proportion symptomatic
priors_default_MOZ$parameters_global$sigma <- list(
     description = "Proportion symptomatic",
     distribution = "beta",
     parameters = list(shape1 = 4.30, shape2 = 13.51)
)

# zeta_1 - Symptomatic shedding rate
priors_default_MOZ$parameters_global$zeta_1 <- list(
     description = "Symptomatic shedding rate (total bacteria per infected person per day)",
     distribution = "lognormal",
     parameters = list(meanlog = log(665), sdlog = 2.0)
)

# zeta_2 - Asymptomatic shedding rate
priors_default_MOZ$parameters_global$zeta_2 <- list(
     description = "Asymptomatic shedding rate (total bacteria per infected person per day)",
     distribution = "lognormal",
     parameters = list(meanlog = log(10), sdlog = 3.0)
)

# delta_reporting_cases
priors_default_MOZ$parameters_global$delta_reporting_cases <- list(
     description = "Symptom-onset-to-case reporting delay in days (integer, 0-7)",
     distribution = "truncnorm",
     parameters = list(mean = 1, sd = 1.5, a = 0, b = 7)
)

# delta_reporting_deaths
priors_default_MOZ$parameters_global$delta_reporting_deaths <- list(
     description = "Symptom-onset-to-death reporting delay in days (integer, 1-14)",
     distribution = "truncnorm",
     parameters = list(mean = 4, sd = 3, a = 1, b = 14)
)

#========================================
# Location-specific parameters: MOZ only
#========================================

# beta_j0_tot - Total base transmission rate (MOZ uses same prior as default)
beta_j0_tot_meanlog <- log(2e-5)
beta_j0_tot_sdlog <- (log(2e-4) - log(2e-5)) / qnorm(0.975)

priors_default_MOZ$parameters_location$beta_j0_tot <- list(
     description = "Total base transmission rate (human + environmental); lognormal with median=2e-5 and Q97.5=2e-4",
     location = list(
          MOZ = list(distribution = "lognormal",
                     parameters = list(meanlog = beta_j0_tot_meanlog, sdlog = beta_j0_tot_sdlog))
     )
)

# p_beta - Proportion human-to-human transmission
beta_fit_p_beta <- fit_beta_from_ci(mode_val = 0.33, ci_lower = 0.1, ci_upper = 0.5)

priors_default_MOZ$parameters_location$p_beta <- list(
     description = "Proportion of total base transmission that is human-to-human (0-1)",
     location = list(
          MOZ = list(distribution = "beta",
                     parameters = list(shape1 = beta_fit_p_beta$shape1, shape2 = beta_fit_p_beta$shape2))
     )
)

# tau_i - Travel probability (from fitted Beta)
tau_uncertainty_factor <- 0.001

tau_param_file <- file.path(PATHS$MODEL_INPUT, "param_tau_departure.csv")
if (file.exists(tau_param_file)) {
     param_tau <- read.csv(tau_param_file)
     tau_shape1_orig <- param_tau$parameter_value[
          param_tau$i == j & param_tau$parameter_distribution == "beta" & param_tau$parameter_name == "shape1"]
     tau_shape2_orig <- param_tau$parameter_value[
          param_tau$i == j & param_tau$parameter_distribution == "beta" & param_tau$parameter_name == "shape2"]

     if (length(tau_shape1_orig) > 0 && length(tau_shape2_orig) > 0) {
          mean_tau <- tau_shape1_orig / (tau_shape1_orig + tau_shape2_orig)
          concentration_new <- (tau_shape1_orig + tau_shape2_orig) * tau_uncertainty_factor
          tau_shape1 <- mean_tau * concentration_new
          tau_shape2 <- (1 - mean_tau) * concentration_new
     } else {
          tau_shape1 <- 100 * tau_uncertainty_factor
          tau_shape2 <- 1000000 * tau_uncertainty_factor
     }
} else {
     tau_shape1 <- 100 * tau_uncertainty_factor
     tau_shape2 <- 1000000 * tau_uncertainty_factor
}

priors_default_MOZ$parameters_location$tau_i <- list(
     description = "Country-level travel probabilities",
     location = list(
          MOZ = list(distribution = "beta", parameters = list(shape1 = tau_shape1, shape2 = tau_shape2))
     )
)

# theta_j - WASH coverage
theta_uncertainty_one_sided <- 0.05
wash_param_file <- file.path(PATHS$MODEL_INPUT, "param_theta_WASH.csv")
if (file.exists(wash_param_file)) {
     param_wash <- read.csv(wash_param_file)
     wash_value <- param_wash$parameter_value[param_wash$j == j]
     if (length(wash_value) > 0) {
          ci_lower <- pmax(0.001, wash_value - theta_uncertainty_one_sided)
          ci_upper <- pmin(0.999, wash_value + theta_uncertainty_one_sided)
     } else {
          wash_value <- mean(param_wash$parameter_value, na.rm = TRUE)
          ci_lower <- pmax(0.001, wash_value - theta_uncertainty_one_sided * 1.25)
          ci_upper <- pmin(0.999, wash_value + theta_uncertainty_one_sided * 1.25)
     }
     theta_fit <- fit_beta_from_ci(mode_val = wash_value, ci_lower = ci_lower, ci_upper = ci_upper)
} else {
     theta_fit <- list(shape1 = 13.44396, shape2 = 8.236964)
}

priors_default_MOZ$parameters_location$theta_j <- list(
     description = "WASH coverage index (proportion with adequate WASH)",
     location = list(
          MOZ = list(distribution = "beta", parameters = list(shape1 = theta_fit$shape1, shape2 = theta_fit$shape2))
     )
)

# Seasonality parameters
seasonality_uncertainty_factor <- 0.5
seasonal_param_file <- file.path(PATHS$MODEL_INPUT, "param_seasonal_dynamics.csv")
seasonal_params_exist <- file.exists(seasonal_param_file)

if (seasonal_params_exist) {
     param_seasonal <- read.csv(seasonal_param_file)
     param_seasonal <- param_seasonal[param_seasonal$response == "cases",]
}

seasonality_csv_lookup <- c("a_1_j" = "a1", "a_2_j" = "a2", "b_1_j" = "b1", "b_2_j" = "b2")

for (param in names(seasonality_csv_lookup)) {
     csv_param <- seasonality_csv_lookup[[param]]

     if (seasonal_params_exist) {
          param_row <- param_seasonal[param_seasonal$country_iso_code == j & param_seasonal$parameter == csv_param,]
          if (nrow(param_row) > 0) {
               mean_orig <- param_row$mean[1]
               se_adjusted <- param_row$se[1] / sqrt(seasonality_uncertainty_factor)
          } else {
               mean_orig <- 0
               se_adjusted <- 0.5 / sqrt(seasonality_uncertainty_factor)
          }
     } else {
          mean_orig <- 0
          se_adjusted <- 0.5 / sqrt(seasonality_uncertainty_factor)
     }

     priors_default_MOZ$parameters_location[[param]] <- list(
          description = paste0("Seasonality Fourier coefficient ", param, " (cases)"),
          location = list(
               MOZ = list(distribution = "normal", parameters = list(mean = mean_orig, sd = se_adjusted))
          )
     )
}

#----------------------------------------
# Initial conditions for 2017-06-19 start
# Zero reported cases for first 8 weeks; endemic setting with recent outbreaks (2015, 2016).
# Priors informed by MOZ 3 calibration (S=52%, R=45%) and observed data constraints.
#----------------------------------------

# prop_V1_initial - ~2.9% from Oct 2016 + May 2017 OCV campaigns (~780k doses in 28M pop)
# est_initial_V1_V2 derives this from campaign data — keep as default, updated below
priors_default_MOZ$parameters_location$prop_V1_initial <- list(
     description = "Initial proportion in one-dose vaccine (V1) compartment",
     location = list(
          MOZ = list(distribution = "beta", parameters = list(shape1 = 0.5, shape2 = 49.5))
     )
)

# prop_V2_initial - No two-dose campaigns before June 2017
priors_default_MOZ$parameters_location$prop_V2_initial <- list(
     description = "Initial proportion in two-dose vaccine (V2) compartment",
     location = list(
          MOZ = list(distribution = "beta", parameters = list(shape1 = 0.5, shape2 = 99.5))
     )
)

# prop_E_initial - Near-zero: 8 weeks of zero cases after t0
# Beta(0.1, 99999): mode=0, mean=28 people, upper 95%=275
priors_default_MOZ$parameters_location$prop_E_initial <- list(
     description = "Initial proportion in exposed (E) compartment",
     location = list(
          MOZ = list(distribution = "beta", parameters = list(shape1 = 0.1, shape2 = 99999))
     )
)

# prop_I_initial - Near-zero: 8 weeks of zero reported cases after t0
# Beta(0.125, 99999): mode=0, mean=35 people, upper 95%=345
priors_default_MOZ$parameters_location$prop_I_initial <- list(
     description = "Initial proportion in infected (I) compartment",
     location = list(
          MOZ = list(distribution = "beta", parameters = list(shape1 = 0.125, shape2 = 99999))
     )
)

# prop_R_initial - Substantial immunity from endemic cholera + recent outbreaks
# 2015: 8739 cases, 2016: 883 cases. True infections ~20-50x reported.
# MOZ 3 calibration found 45%. Beta(4, 6): mode=37.5%, mean=40%, 95% CI [14%, 70%]
priors_default_MOZ$parameters_location$prop_R_initial <- list(
     description = "Initial proportion in recovered/immune (R) compartment",
     location = list(
          MOZ = list(distribution = "beta", parameters = list(shape1 = 4, shape2 = 6))
     )
)

# prop_S_initial - Susceptible proportion (constrained residual)
# Beta(12, 5): mode=73%, mean=71%, 95% CI [48%, 89%]
# Covers MOZ 3 (52%) and MOZ 4 (81%) findings
priors_default_MOZ$parameters_location$prop_S_initial <- list(
     description = "Initial proportion in susceptible (S) compartment",
     location = list(
          MOZ = list(distribution = "beta", parameters = list(shape1 = 12, shape2 = 5))
     )
)

# Update V1/V2 priors from vaccination history
# For 2017 start: first MOZ OCV campaign was Oct 2016 (425k doses, ~1.5% of pop)
# est_initial_V1_V2 estimates from campaign data; run with error handling for single-location
tryCatch({
     initial_conditions_V1_V2 <- est_initial_V1_V2(
          PATHS = PATHS,
          priors = priors_default_MOZ,
          config = config_default_MOZ,
          n_samples = 100,
          t0 = date_start,
          variance_inflation = 0,
          parallel = FALSE,
          verbose = TRUE
     )
     for (compartment in c("prop_V1_initial", "prop_V2_initial")) {
          locs <- initial_conditions_V1_V2$parameters_location[[compartment]]$parameters$location
          if (!is.null(locs[[j]]) && !is.na(locs[[j]]$shape1)) {
               priors_default_MOZ$parameters_location[[compartment]]$location[[j]] <- list(
                    distribution = "beta",
                    parameters = list(shape1 = locs[[j]]$shape1, shape2 = locs[[j]]$shape2)
               )
          }
     }
     message("V1/V2 priors updated from est_initial_V1_V2()")
}, error = function(e) {
     message("est_initial_V1_V2 failed for single-location; using default V1/V2 priors: ", e$message)
})

# Update E/I priors from surveillance data near model start
# June 2017: no cases reported yet (first cases Aug 2017), so E/I should be near-zero
tryCatch({
     initial_conditions_E_I <- est_initial_E_I(
          PATHS = PATHS,
          priors = priors_default_MOZ,
          config = config_default_MOZ,
          n_samples = 100,
          t0 = date_start,
          lookback_days = 3,
          variance_inflation = c("MOZ" = 30),
          verbose = TRUE,
          parallel = FALSE
     )
     for (compartment in c("prop_E_initial", "prop_I_initial")) {
          locs <- initial_conditions_E_I$parameters_location[[compartment]]$parameters$location
          if (!is.null(locs[[j]]) && !is.na(locs[[j]]$shape1)) {
               priors_default_MOZ$parameters_location[[compartment]]$location[[j]] <- list(
                    distribution = "beta",
                    parameters = list(shape1 = locs[[j]]$shape1, shape2 = locs[[j]]$shape2)
               )
          }
     }
     message("E/I priors updated from est_initial_E_I()")
}, error = function(e) {
     message("est_initial_E_I failed for single-location; using default E/I priors: ", e$message)
})

# Update R prior from historical surveillance
tryCatch({
     initial_conditions_R <- est_initial_R(
          PATHS = PATHS,
          priors = priors_default_MOZ,
          config = config_default_MOZ,
          n_samples = 100,
          t0 = date_start,
          disaggregate = TRUE,
          variance_inflation = c("MOZ" = 1.5),
          verbose = TRUE,
          parallel = FALSE
     )
     loc_est <- initial_conditions_R$parameters_location$prop_R_initial$parameters$location[[j]]
     if (!is.null(loc_est) && !is.na(loc_est$shape1)) {
          priors_default_MOZ$parameters_location$prop_R_initial$location[[j]] <- list(
               distribution = "beta",
               parameters = list(shape1 = loc_est$shape1, shape2 = loc_est$shape2)
          )
          message("R prior updated from est_initial_R()")
     }
}, error = function(e) {
     message("est_initial_R failed for single-location; using default R prior: ", e$message)
})

# Update S prior (constrained residual)
tryCatch({
     initial_conditions_S <- est_initial_S(
          PATHS = PATHS,
          priors = priors_default_MOZ,
          config = config_default_MOZ,
          n_samples = 100,
          t0 = date_start,
          variance_inflation = c("MOZ" = 0.03),
          verbose = TRUE,
          min_S_proportion = 0.001
     )
     loc_est <- initial_conditions_S$parameters_location$prop_S_initial$parameters$location[[j]]
     if (!is.null(loc_est) && !is.na(loc_est$shape1)) {
          priors_default_MOZ$parameters_location$prop_S_initial <- list(
               description = "Initial proportion in susceptible (S) compartment from constrained residual",
               location = list(
                    MOZ = list(distribution = "beta",
                               parameters = list(shape1 = loc_est$shape1, shape2 = loc_est$shape2))
               )
          )
          message("S prior updated from est_initial_S()")
     }
}, error = function(e) {
     message("est_initial_S failed for single-location; using default S prior: ", e$message)
     # Add a reasonable default S prior: Beta centered at ~80% susceptible
     priors_default_MOZ$parameters_location$prop_S_initial <- list(
          description = "Initial proportion in susceptible (S) compartment (default for 2017 start)",
          location = list(
               MOZ = list(distribution = "beta", parameters = list(shape1 = 30, shape2 = 7.5))
          )
     )
})

#----------------------------------------
# Override initial condition priors with calibration-informed values
# The est_initial_* functions above use surveillance data near t0, but for
# June 2017 there are no cases (8 weeks of zeros), so the estimates are
# unreliable. Override with priors informed by MOZ 3 calibration + data.
#----------------------------------------

# E: near-zero — no active transmission at t0. Slightly wider than v1.0.
priors_default_MOZ$parameters_location$prop_E_initial$location$MOZ <- list(
     distribution = "beta", parameters = list(shape1 = 0.3, shape2 = 99999))

# I: near-zero — zero reported cases for 8 weeks after t0. Slightly wider than v1.0.
priors_default_MOZ$parameters_location$prop_I_initial$location$MOZ <- list(
     distribution = "beta", parameters = list(shape1 = 0.3, shape2 = 99999))

# R: Lower initial immunity than v1.0. MOZ 6-7 best model: 6% recovered.
# The 2017 start is pre-epidemic; high initial immunity is inconsistent with the
# large 2022-2023 outbreak. Beta(1.6, 11.54): mean=12%, 95% CI [0%, 30%].
priors_default_MOZ$parameters_location$prop_R_initial$location$MOZ <- list(
     distribution = "beta", parameters = list(shape1 = 1.6, shape2 = 11.54))

# S: Higher susceptibility. MOZ 6-7 best model: 91% susceptible.
# Beta(30, 7.5): mean=80%, mode=82%, 95% CI [64%, 91%].
priors_default_MOZ$parameters_location$prop_S_initial <- list(
     description = "Initial proportion in susceptible (S) compartment",
     location = list(
          MOZ = list(distribution = "beta", parameters = list(shape1 = 30, shape2 = 7.5))
     )
)

message("Initial condition priors overridden with calibration-informed values")

#----------------------------------------
# mu_j_baseline - MOZ-specific override from calibration evidence
#----------------------------------------

# MOZ 6-7 breakthrough: Gamma(2, 1176) allows realistic deaths.
# Old Gamma(4, 13333) had P(>0.001) = 0.08% — virtually no sims produced deaths.
# New prior: mean=0.0017/day, P(>0.001) = 67%. MOZ 7 best model: 0.0028/day.
priors_default_MOZ$parameters_location$mu_j_baseline <- list(
     description = "Baseline daily mortality rate per symptomatic infected (per day)",
     location = list(
          MOZ = list(distribution = "gamma", parameters = list(shape = 2, rate = 1176))
     )
)

# mu_j_slope
priors_default_MOZ$parameters_location$mu_j_slope <- list(
     description = "Temporal trend in baseline IFR (proportion change over simulation period)",
     location = list(
          MOZ = list(distribution = "normal", parameters = list(mean = 0, sd = 0.05))
     )
)

# mu_j_epidemic_factor - MOZ 6-7 best model: 1.78. Deaths are concentrated in
# 2022-2023 epidemic; the model needs 2-3x baseline mortality during outbreaks.
# Gamma(1.5, 0.5): mean=3.0, mode=1.0. Old Gamma(1.5, 2.5) had mean=0.6 — too low.
priors_default_MOZ$parameters_location$mu_j_epidemic_factor <- list(
     description = "Proportional increase in IFR during epidemic periods",
     location = list(
          MOZ = list(distribution = "gamma", parameters = list(shape = 1.5, rate = 0.5))
     )
)

#----------------------------------------
# epidemic_threshold - MOZ-specific from surveillance data
#----------------------------------------

convert_zheng_threshold <- function(zheng_weekly_per_100k, rho, chi, gamma_1) {
     (zheng_weekly_per_100k / 1e5) * (chi / rho) / (7 * gamma_1)
}

rho_val <- config_default_MOZ$rho
chi_val <- config_default_MOZ$chi_endemic
gamma1_val <- config_default_MOZ$gamma_1

# MOZ-specific: use all surveillance data (2017-2025) for threshold estimation
dem_max_year <- max(dem_annual$year)
outbreak_rows <- surv_weekly[
     surv_weekly$iso_code == j & !is.na(surv_weekly$cases) & surv_weekly$cases > 0,]
outbreak_rows$dem_year <- pmin(outbreak_rows$year, dem_max_year)

merged_surv <- merge(
     outbreak_rows,
     dem_annual[, c("iso_code", "year", "population")],
     by.x = c("iso_code", "dem_year"),
     by.y = c("iso_code", "year"),
     all.x = TRUE
)
merged_surv$weekly_incidence_per_100k <- merged_surv$cases / merged_surv$population * 1e5

if (nrow(merged_surv) >= 10) {
     median_incidence <- median(merged_surv$weekly_incidence_per_100k, na.rm = TRUE)
     prior_mean <- convert_zheng_threshold(median_incidence, rho_val, chi_val, gamma1_val)
} else {
     prior_mean <- convert_zheng_threshold(0.7, rho_val, chi_val, gamma1_val)
}

priors_default_MOZ$parameters_location$epidemic_threshold <- list(
     description = "Dimensionless daily Isym/N prevalence threshold for epidemic regime activation",
     location = list(
          MOZ = list(distribution = "lognormal",
                     parameters = list(meanlog = log(prior_mean), sdlog = 0.5))
     )
)

#----------------------------------------
# psi_star calibration parameters - MOZ-specific overrides
#----------------------------------------

# psi_star_a - Wider prior centered on neutral (a=1). MOZ 7 best: 0.38.
# Old TN(0.57, 0.5) biased toward flattening; wider sd=1.0 allows both sharpening and flattening.
priors_default_MOZ$parameters_location$psi_star_a <- list(
     description = "Shape/gain parameter for logit calibration of NN suitability psi",
     location = list(
          MOZ = list(distribution = "truncnorm",
                     parameters = list(mean = 1.0, sd = 1.0, a = 0, b = Inf))
     )
)

# psi_star_b - Shifted positive. Old N(-0.8, 2) biased suitability downward.
# MOZ 6-7 top-50 median was +0.6. N(0.4, 2) allows higher baseline suitability.
priors_default_MOZ$parameters_location$psi_star_b <- list(
     description = "Scale/offset parameter for logit calibration of NN suitability psi",
     location = list(
          MOZ = list(distribution = "normal", parameters = list(mean = 0.4, sd = 2.0))
     )
)

# psi_star_z - Completely uninformative. Beta(1,1) = Uniform(0,1).
# Old Beta(3, 1.5) biased toward light smoothing. MOZ 6-7 posterior width=0.95,
# confirming the data doesn't constrain this — let it be flat.
priors_default_MOZ$parameters_location$psi_star_z <- list(
     description = "Smoothing weight for causal EWMA of calibrated suitability",
     location = list(
          MOZ = list(distribution = "beta", parameters = list(shape1 = 1, shape2 = 1))
     )
)

# psi_star_k - Allow positive offsets. Old TN(-22, 15, [-90, 0]) forced suitability
# to lag cases. In MOZ 6-7, 18-28% of top-50 models used positive k.
# TN(-5, 20, [-90, 90]) is centered near zero with much wider range.
priors_default_MOZ$parameters_location$psi_star_k <- list(
     description = "Time offset in days for suitability calibration",
     location = list(
          MOZ = list(distribution = "truncnorm",
                     parameters = list(mean = -5, sd = 20, a = -90, b = 90))
     )
)

#========================================
# Save
#========================================

fp <- file.path(PATHS$ROOT, 'MOSAIC-pkg/inst/extdata/priors_default_MOZ.json')
jsonlite::write_json(priors_default_MOZ, fp, pretty = TRUE, auto_unbox = TRUE)

tmp_priors <- jsonlite::fromJSON(fp, simplifyVector = FALSE)
identical(priors_default_MOZ$parameters_global$alpha_1, tmp_priors$parameters_global$alpha_1)

usethis::use_data(priors_default_MOZ, overwrite = TRUE)

message("Done. priors_default_MOZ saved to data/ and inst/extdata/")
