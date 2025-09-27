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

priors_default <- list(
     metadata = list(
          version = "6.3.0",
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
beta_fit_alpha_2 <- fit_beta_from_ci(mode_val = 0.33, ci_lower = 0.1, ci_upper = 0.66)

priors_default$parameters_global$alpha_2 <- list(
     description = "Degree of frequency driven transmission (0-1)",
     distribution = "beta",
     parameters = list(shape1 = beta_fit_alpha_2$shape1, shape2 = beta_fit_alpha_2$shape2)
)

# decay_days_long - Maximum V. cholerae survival time
priors_default$parameters_global$decay_days_long <- list(
     description = "Maximum V. cholerae survival time (days)",
     distribution = "uniform",
     parameters = list(min = 30, max = 150)
)

# decay_days_short - Minimum V. cholerae survival time
priors_default$parameters_global$decay_days_short <- list(
     description = "Minimum V. cholerae survival time (days)",
     distribution = "uniform",
     parameters = list(min = 0.01, max = 30)
)

# decay_shape_1 - First shape parameter of Beta distribution for V. cholerae decay rate transformation
priors_default$parameters_global$decay_shape_1 <- list(
     description = "First shape parameter of Beta distribution for V. cholerae decay",
     distribution = "uniform",
     parameters = list(min = 0.5, max = 5.0)
)

# decay_shape_2 - Second shape parameter of Beta distribution for V. cholerae decay rate transformation
priors_default$parameters_global$decay_shape_2 <- list(
     description = "Second shape parameter of Beta distribution for V. cholerae decay",
     distribution = "uniform",
     parameters = list(min = 0.5, max = 5.0)
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
          sdlog = 0.5           # uncertainty allowing 1-3 day range
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
priors_default$parameters_global$kappa <- list(
     description = "Concentration of V. cholerae for 50% infectious dose",
     distribution = "uniform",
     parameters = list(min = 10^6, max = 10^9)
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



# Configuration parameter for rho (reporting rate)
# Set outbreak = TRUE to use high estimate (during outbreaks, ~78% mean)
# Set outbreak = FALSE to use low estimate (all settings, ~52% mean)
outbreak <- FALSE  # Default to outbreak scenario for active transmission modeling

# Load rho (reporting rate) parameters
# Check if file exists first
rho_param_file <- file.path(PATHS$MODEL_INPUT, "param_rho_suspected_cases.csv")
if (file.exists(rho_param_file)) {
     param_rho <- read.csv(rho_param_file)

     # Select parameters based on outbreak setting
     if (outbreak) {
          # Extract high estimate parameters (during outbreaks)
          # Mean ~78%, appropriate for active outbreak modeling
          rho_shape1 <- param_rho$parameter_value[
               grepl("High estimate", param_rho$variable_description) &
                    param_rho$parameter_name == "shape1"
          ]
          rho_shape2 <- param_rho$parameter_value[
               grepl("High estimate", param_rho$variable_description) &
                    param_rho$parameter_name == "shape2"
          ]
          rho_description <- "High estimate (during outbreaks)"

          # Default values for high estimate if not found
          default_shape1 <- 4.79
          default_shape2 <- 1.53
     } else {
          # Extract low estimate parameters (all settings)
          # Mean ~52%, appropriate for endemic/inter-outbreak periods
          rho_shape1 <- param_rho$parameter_value[
               grepl("Low estimate", param_rho$variable_description) &
                    param_rho$parameter_name == "shape1"
          ]
          rho_shape2 <- param_rho$parameter_value[
               grepl("Low estimate", param_rho$variable_description) &
                    param_rho$parameter_name == "shape2"
          ]
          rho_description <- "Low estimate (all settings)"

          # Default values for low estimate if not found
          default_shape1 <- 5.43
          default_shape2 <- 5.01
     }

     if (length(rho_shape1) == 0 || length(rho_shape2) == 0) {
          warning(paste("Could not find rho parameters for", rho_description, "in file. Using defaults."))
          rho_shape1 <- default_shape1
          rho_shape2 <- default_shape2
     }
} else {
     warning("Rho parameter file not found. Using default values.")
     if (outbreak) {
          rho_shape1 <- 4.79  # High estimate defaults
          rho_shape2 <- 1.53
          rho_description <- "High estimate (during outbreaks) - DEFAULT"
     } else {
          rho_shape1 <- 5.43  # Low estimate defaults
          rho_shape2 <- 5.01
          rho_description <- "Low estimate (all settings) - DEFAULT"
     }
}

# rho - Reporting rate (proportion of suspected cases that are true cholera)
# Using selected estimate based on outbreak parameter
priors_default$parameters_global$rho <- list(
     description = "Reporting rate (proportion of suspected cases that are true cholera)",
     distribution = "beta",
     parameters = list(shape1 = rho_shape1, shape2 = rho_shape2)
)

# sigma - Proportion symptomatic
priors_default$parameters_global$sigma <- list(
     description = "Proportion symptomatic",
     distribution = "beta",
     parameters = list(shape1 = 4.30, shape2 = 13.51)
)

# zeta_1 - Symptomatic shedding rate
priors_default$parameters_global$zeta_1 <- list(
     description = "Symptomatic shedding rate (bacteria per day)",
     distribution = "uniform",
     parameters = list(min = 1e5, max = 1e10)
)

# zeta_2 - Asymptomatic shedding rate
priors_default$parameters_global$zeta_2 <- list(
     description = "Asymptomatic shedding rate (bacteria per day)",
     distribution = "uniform",
     parameters = list(min = 100, max = 1e5)
)


#---------------------------------------------------
# Location specific parameters in alphabetical order
#---------------------------------------------------

# beta_j0_tot - Total base transmission rate (human + environmental)
# Fit Gompertz distribution for beta_j0_tot
beta_j0_tot_fit <- fit_gompertz_from_ci(
     mode_val = 1e-6,        # Very low baseline transmission rate
     ci_lower = 1e-8,        # Near-zero lower bound
     ci_upper = 1e-4,        # Upper bound allows for higher transmission scenarios
     probs = c(0.025, 0.975),
     verbose = TRUE
)

priors_default$parameters_location$beta_j0_tot <- list(
     description = "Total base transmission rate (human + environmental)",
     location = list()
)

for (iso in j) {
     # Use the same Gompertz parameters for all locations
     priors_default$parameters_location$beta_j0_tot$location[[iso]] <- list(
          distribution = "gompertz",
          parameters = list(
               b = beta_j0_tot_fit$b,
               eta = beta_j0_tot_fit$eta
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
for (param in c("a1", "a2", "b1", "b2")) {
     param_name <- param  # Use parameter name directly without suffix

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
                         param_seasonal$parameter == param,
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
     n_samples = 1000,
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
if (TRUE) {

     initial_conditions_E_I <- est_initial_E_I(
          PATHS = PATHS,
          priors = priors_default,
          config = config_default,
          n_samples = 1000,
          t0 = date_start,
          lookback_days = 7,
          variance_inflation = 100,
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


}


# Update default priors with estimated initial conditions for R

initial_conditions_R <- est_initial_R(
     PATHS = PATHS,
     priors = priors_default,
     config = config_default,
     n_samples = 1000,
     t0 = date_start,
     disaggregate = TRUE,
     variance_inflation = 12,
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
initial_conditions_S <- est_initial_S(
     PATHS = PATHS,
     priors = priors_default,
     config = config_default,
     n_samples = 1000,
     t0 = date_start,
     variance_inflation = 0,
     verbose = FALSE,
     min_S_proportion = 0.001
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



# Add mu_j priors from disease mortality data

mu_inflation <- 0.2  # Inflate mu variance

# Load disease mortality parameter data
mu_file <- file.path(PATHS$MODEL_INPUT, "param_mu_disease_mortality.csv")
if (file.exists(mu_file)) {
     mu_data <- read.csv(mu_file, stringsAsFactors = FALSE)

     # Calculate location-specific priors from Beta parameters
     # We'll use data from recent years (2020-2025) for more current estimates
     recent_years <- 2021:2025
     mu_recent <- mu_data[mu_data$t %in% recent_years, ]

     # Initialize mu_j prior structure
     priors_default$parameters_location$mu_j <- list(
          description = "Base disease mortality rate (case fatality ratio) per location",
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

                    # Ensure bounds are reasonable
                    ci_lower <- max(ci_lower*(1-mu_inflation), 0.001)  # Minimum 0.1% CFR
                    ci_upper <- min(ci_upper*(1+mu_inflation), 0.5)    # Maximum 50% CFR
                    mean_cfr <- max(min(mean_cfr, 0.4), 0.002)  # Keep mean in reasonable range

                    # Try to fit gamma distribution
                    tryCatch({
                         gamma_fit <- MOSAIC::fit_gamma_from_ci(
                              mode_val = mean_cfr,
                              ci_lower = ci_lower,
                              ci_upper = ci_upper,
                              method = "optimization",
                              verbose = FALSE
                         )

                         priors_default$parameters_location$mu_j$location[[loc]] <- list(
                              distribution = "gamma",
                              parameters = list(
                                   shape = gamma_fit$shape,
                                   rate = gamma_fit$rate
                              )
                         )

                         n_mu_j_added <- n_mu_j_added + 1

                    }, error = function(e) {
                         # Fallback to simple moment matching
                         var_cfr <- ((ci_upper - ci_lower) / 4)^2  # Approximate variance
                         shape <- mean_cfr^2 / var_cfr
                         rate <- mean_cfr / var_cfr

                         # Ensure reasonable parameters
                         shape <- max(shape, 1.5)
                         rate <- max(rate, 10)

                         priors_default$parameters_location$mu_j$location[[loc]] <- list(
                              distribution = "gamma",
                              parameters = list(
                                   shape = shape,
                                   rate = rate
                              )
                         )

                         n_mu_j_added <- n_mu_j_added + 1
                    })
               }
          }

          # Add default if location not found
          if (is.null(priors_default$parameters_location$mu_j$location[[loc]])) {
               # Default gamma parameters for ~2% CFR
               priors_default$parameters_location$mu_j$location[[loc]] <- list(
                    distribution = "gamma",
                    parameters = list(
                         shape = 2,
                         rate = 100
                    )
               )
               n_mu_j_added <- n_mu_j_added + 1
          }
     }


} else {
     warning("Disease mortality parameter file not found. Using default mu_j priors.")

     # Add default gamma priors for all locations
     priors_default$parameters_location$mu_j <- list(
          description = "Base disease mortality rate (case fatality ratio) per location",
          location = list()
     )

     for (loc in j) {
          # Default gamma parameters for ~2% CFR
          priors_default$parameters_location$mu_j$location[[loc]] <- list(
               distribution = "gamma",
               parameters = list(
                    shape = 2,
                    rate = 100
               )
          )
     }
}

#----------------------------------------
# Psi star calibration parameters (location-specific)
#----------------------------------------

# psi_star - Logit-scale calibration of environmental suitability with optional EWMA smoothing
# Applied per location to allow location-specific suitability calibration during model fitting
priors_default$parameters_location$psi_star <- list(
     description = "Logit calibration of NN suitability psi with optional causal EWMA (a: shape/gain, b: scale/offset, z: smoothing).",
     a = list(
          distribution = "lognormal",
          parameters   = list(meanlog = 0, sdlog = 0.35)  # Mean ~1.06, 95% CI: [0.50, 1.99]
     ),
     b = list(
          distribution = "normal",
          parameters   = list(mean = 0, sd = 0.75)         # 95% CI: [-1.47, 1.47]
     ),
     z = list(
          distribution = "beta",
          parameters   = list(shape1 = 6, shape2 = 1)     # Mean: 0.86, Mode: 1.0, 95% CI: [0.54, 1.00]
     )
)


# Save to file and add to MOSAIC R package

fp <- file.path(PATHS$ROOT, 'MOSAIC-pkg/inst/extdata/priors_default.json')

# save to file
jsonlite::write_json(priors_default, fp, pretty = TRUE, auto_unbox = TRUE)

# Read back to verify
tmp_priors <- jsonlite::fromJSON(fp, simplifyVector = FALSE)

identical(priors_default$parameters_global$alpha_1, tmp_priors$parameters_global$alpha_1)

# Note: R data object is saved as priors_default to match config_default naming convention
usethis::use_data(priors_default, overwrite = TRUE)

