library(MOSAIC)
library(jsonlite)

# Enhanced make_priors.R with flexible Beta fitting for E/I compartments
# 
# Key changes:
# - est_initial_E_I now uses enhanced flexible Beta fitting with left-skewed defaults
# - Prior method: "left_skewed" for biological realism
# - Expansion factor: 4.0 for wide scenario exploration
# - Conservatism bias: 0.3 for moderate zero-bias in production
# - Other functions (est_initial_R, est_initial_S, est_initial_V1_V2) still use old variance_inflation

# Set up paths - critical for finding input files
# Set root to parent directory containing all MOSAIC repos
MOSAIC::set_root_directory("~/MOSAIC")
PATHS <- MOSAIC::get_paths()

# Load config_default to get the global date_start
# This ensures priors are aligned with the model configuration
config_default <- MOSAIC::config_default
date_start <- as.Date(config_default$date_start)
cat(sprintf("Using date_start from config_default: %s\n", date_start))

j <- MOSAIC::iso_codes_mosaic

priors_default <- list(
     metadata = list(
          version = "5.0.0",
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
# Assuming close to 1 on mixing param. Not because we believe pops at country level are that well-mixed,
# but trying to enable faster epidemic growth rates
beta_fit_alpha_1 <- fit_beta_from_ci(mode_val = 0.5, ci_lower = 0.2, ci_upper = 0.8)

priors_default$parameters_global$alpha_1 <- list(
     parameter_name = "alpha_1",
     distribution = "beta",
     parameters = list(shape1 = beta_fit_alpha_1$shape1, shape2 = beta_fit_alpha_1$shape2)
)

# alpha_2 - Degree of frequency driven transmission
# assuming more frequency dependent transmission with wide uncertainty
beta_fit_alpha_2 <- fit_beta_from_ci(mode_val = 0.66, ci_lower = 0.33, ci_upper = 0.99)

priors_default$parameters_global$alpha_2 <- list(
     parameter_name = "alpha_2",
     distribution = "beta",
     parameters = list(shape1 = beta_fit_alpha_2$shape1, shape2 = beta_fit_alpha_2$shape2)
)

# decay_days_long - Maximum V. cholerae survival time
priors_default$parameters_global$decay_days_long <- list(
     parameter_name = "decay_days_long",
     distribution = "uniform",
     parameters = list(min = 30, max = 150)
)

# decay_days_short - Minimum V. cholerae survival time
priors_default$parameters_global$decay_days_short <- list(
     parameter_name = "decay_days_short",
     distribution = "uniform",
     parameters = list(min = 0.01, max = 30)
)

# decay_shape_1 - First shape parameter of Beta distribution for V. cholerae decay rate transformation
priors_default$parameters_global$decay_shape_1 <- list(
     parameter_name = "decay_shape_1",
     distribution = "uniform",
     parameters = list(min = 0.01, max = 10.0)
)

# decay_shape_2 - Second shape parameter of Beta distribution for V. cholerae decay rate transformation
priors_default$parameters_global$decay_shape_2 <- list(
     parameter_name = "decay_shape_2",
     distribution = "uniform",
     parameters = list(min = 0.01, max = 10.0)
)

# epsilon - Natural immunity waning rate
priors_default$parameters_global$epsilon <- list(
     parameter_name = "epsilon",
     distribution = "lognormal",
     parameters = list(mean = 3.9e-4, sd = 2.0e-4)  # sd derived from 95% CI [1.7e-4, 1.03e-3]
)

variance_inflation_epsilon <- 2

priors_default$parameters_global$epsilon$parameters$sd <-
     priors_default$parameters_global$epsilon$parameters$sd * variance_inflation_epsilon

# gamma_1 - Symptomatic/severe shedding duration rate
# Based on literature: symptomatic cases shed V. cholerae for 7-14 days without treatment
# This parameter represents the rate at which symptomatic individuals stop shedding bacteria
# Median shedding ~10 days, allowing range of 7-14 days (untreated populations)
# Note: With effective antibiotic treatment, shedding reduces to 1-2 days
priors_default$parameters_global$gamma_1 <- list(
     parameter_name = "gamma_1",
     distribution = "lognormal",
     parameters = list(
          meanlog = log(1/10),  # log of median rate: 1/10 per day = 10 days shedding
          sdlog = 0.8          # uncertainty allowing 7-14 day range
     )
)

# gamma_2 - Asymptomatic/mild shedding duration rate
# Based on literature: asymptomatic cases typically shed for only 1-2 days
# Bangladesh study shows average 2.0 days (95% CI: 1.7-2.4 days)
# This parameter represents the rate at which asymptomatic individuals stop shedding bacteria
# Median shedding ~2 days, allowing range of 1-3 days
# Note: Asymptomatic cases have ~10³ vibrios/gram vs 10¹⁰-10¹² for symptomatic
priors_default$parameters_global$gamma_2 <- list(
     parameter_name = "gamma_2",
     distribution = "lognormal",
     parameters = list(
          meanlog = log(1/2),   # log of median rate: 1/2 per day = 2 days shedding
          sdlog = 0.8           # uncertainty allowing 1-3 day range
     )
)


# iota - Incubation rate (1/days)
# Based on Azman et al. (2013) J Infect 66(5):432-438
# Median incubation period = 1.4 days (95% CI: 1.3-1.6 days)
# 5th percentile = 0.5 days, 95th percentile = 4.4 days

priors_default$parameters_global$iota <- list(
     parameter_name = "iota",
     distribution = "lognormal",
     parameters = list(meanlog = -0.337, sdlog = 0.4)  # Wider distribution
)

variance_inflation_iota <- 6

priors_default$parameters_global$iota$parameters$sd <-
     priors_default$parameters_global$iota$parameters$sd * variance_inflation_iota





# kappa - Concentration of V. cholerae which leads to 50% infectious dose
priors_default$parameters_global$kappa <- list(
     parameter_name = "kappa",
     distribution = "uniform",
     parameters = list(min = 10^5, max = 10^10)
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
     parameter_name = "mobility_gamma",
     distribution = "gamma",
     parameters = list(shape = mobility_gamma_shape, rate = gamma_rate)
)

# mobility_omega - Mobility population scaling parameter
priors_default$parameters_global$mobility_omega <- list(
     parameter_name = "mobility_omega",
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
# Fit gamma distribution from confidence intervals with uncertainty inflation
uncertainty_inflation <- 0.25  # Increase CI width by 20%

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
     parameter_name = "omega_1",
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
     parameter_name = "omega_2",
     distribution = "gamma",
     parameters = list(
          shape = omega_2_fit$shape,
          rate = omega_2_fit$rate
     )
)


uncertainty_inflation <- 0.4  # Increase CI width by 20%

# phi_1 - Initial vaccine effectiveness (one dose)
# Based on Xu et al. (2024), fitted using est_vaccine_effectiveness()
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
     parameter_name = "phi_1",
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
     parameter_name = "phi_2",
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
     parameter_name = "rho",
     distribution = "beta",
     parameters = list(shape1 = rho_shape1, shape2 = rho_shape2)
)

# sigma - Proportion symptomatic
priors_default$parameters_global$sigma <- list(
     parameter_name = "sigma",
     distribution = "beta",
     parameters = list(shape1 = 4.30, shape2 = 13.51)
)

# zeta_1 - Symptomatic shedding rate
priors_default$parameters_global$zeta_1 <- list(
     parameter_name = "zeta_1",
     distribution = "uniform",
     parameters = list(min = 1e5, max = 1e10)
)

# zeta_2 - Asymptomatic shedding rate
priors_default$parameters_global$zeta_2 <- list(
     parameter_name = "zeta_2",
     distribution = "uniform",
     parameters = list(min = 100, max = 1e5)
)


#---------------------------------------------------
# Location specific parameters in alphabetical order
#---------------------------------------------------

# beta_j0_tot - Total base transmission rate (human + environmental)
# Lognormal prior on the log scale; median set near current combined mean (~1.5e-4)
# and wide uncertainty to allow multi-fold variation across locations.
#   meanlog = log(1.5e-4), sdlog = 0.9  ->  ~95% prior ≈ median * exp(±1.96*0.9) ≈ ×(1/5.8 to 5.8)
# Used with p_beta to derive components:
#   beta_j0_hum = p_beta * beta_j0_tot
#   beta_j0_env = (1 - p_beta) * beta_j0_tot
# Option (Gamma alternative with similar breadth; mean = 1.5e-4):
#   shape = 2, rate = shape/mean = 2 / 1.5e-4 ≈ 13333.33  (CV ≈ 0.71)

priors_default$parameters_location$beta_j0_tot <- list(
     parameter_name = "beta_j0_tot",
     description    = "Total base transmission rate (human + environmental)",
     distribution   = "lognormal",
     parameters     = list(
          location = list()
     )
)

for (iso in j) {

     priors_default$parameters_location$beta_j0_tot$parameters$location[[iso]] <- list(
          meanlog = log(1e-6),
          sdlog   = 2.5
     )

     # --- Alternative (Gamma) ---
     # To use a Gamma prior instead of Lognormal, uncomment and comment out the block above:
     # priors_default$parameters_location$beta_j0_tot$parameters$location[[iso]] <- list(
     #      shape = 2,
     #      rate  = 2 / 1.5e-4
     # )
}


# p_beta - Proportion of human-to-human vs environmental transmission
# Beta distribution with a weak bias toward human transmission ≈ 2× environmental.
# Center at p_beta ≈ 2/3 with light concentration:
#   shape1 = 2.25, shape2 = 1.125  -> Mean ≈ 0.667, SD ≈ 0.226
# Option for 1:1 (neutral) ratio:
#   shape1 = 1.5, shape2 = 1.5      -> Mean = 0.5,   SD = 0.25
# Used to derive: beta_j0_hum = p_beta * beta_j0_total; beta_j0_env = (1 - p_beta) * beta_j0_total

 # Default: weak 2:1 bias toward human transmission
beta_fit_p_beta <- fit_beta_from_ci(mode_val = 0.66, ci_lower = 0.1, ci_upper = 0.9)


priors_default$parameters_location$p_beta <- list(
     parameter_name = "p_beta",
     description = "Proportion of total base transmission that is human-to-human (0–1)",
     distribution = "beta",
     parameters = list(
          location = list()
     )
)

for (iso in j) {

     priors_default$parameters_location$p_beta$parameters$location[[iso]] <- list(shape1 = beta_fit_p_beta$shape1, shape2 = beta_fit_p_beta$shape2)

}













# tau_i - Country-level travel probabilities
# Beta distribution with parameters loaded from param_tau_departure.csv

# Uncertainty adjustment parameter for tau_i
# Values < 1 increase uncertainty (wider distributions)
# Values > 1 decrease uncertainty (narrower distributions)
# Value = 1 keeps original uncertainty from file
# Value = 0.1 makes distributions 10x wider (much more uncertain)
# Value = 10 makes distributions 10x narrower (much more certain)
tau_uncertainty_factor <- 0.001  # Default: use original uncertainty from file

priors_default$parameters_location$tau_i <- list(
     parameter_name = "tau_i",
     description = "Country-level travel probabilities",
     distribution = "beta",
     uncertainty_factor = tau_uncertainty_factor,
     parameters = list(
          location = list()
     )
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

               priors_default$parameters_location$tau_i$parameters$location[[iso]] <- list(
                    shape1 = tau_shape1,
                    shape2 = tau_shape2
               )

               # Add metadata about the adjustment
               if (tau_uncertainty_factor != 1.0) {
                    priors_default$parameters_location$tau_i$parameters$location[[iso]]$original_shape1 <- tau_shape1_orig
                    priors_default$parameters_location$tau_i$parameters$location[[iso]]$original_shape2 <- tau_shape2_orig
                    priors_default$parameters_location$tau_i$parameters$location[[iso]]$uncertainty_adjusted <- TRUE
               }
          } else {
               # Default values if not found (very small travel probability)
               warning(paste("tau parameters not found for", iso, "- using defaults"))
               # Apply uncertainty factor to defaults as well
               default_shape1 <- 100 * tau_uncertainty_factor
               default_shape2 <- 1000000 * tau_uncertainty_factor
               priors_default$parameters_location$tau_i$parameters$location[[iso]] <- list(
                    shape1 = default_shape1,
                    shape2 = default_shape2
               )
          }
     }

     # Print adjustment message
     if (tau_uncertainty_factor != 1.0) {
          cat(paste0("\nTau_i uncertainty adjusted by factor of ", tau_uncertainty_factor, "\n"))
          if (tau_uncertainty_factor < 1) {
               cat("  -> Distributions are WIDER (more uncertain)\n")
          } else {
               cat("  -> Distributions are NARROWER (less uncertain)\n")
          }
     }
} else {
     warning("tau parameter file not found. Using default values.")
     # Set default values for all locations with uncertainty adjustment
     for (iso in j) {
          priors_default$parameters_location$tau_i$parameters$location[[iso]] <- list(
               shape1 = 100 * tau_uncertainty_factor,
               shape2 = 1000000 * tau_uncertainty_factor
          )
     }
}





# theta_j - WASH coverage
# Beta distribution fitted from weighted mean WASH estimates with uncertainty
priors_default$parameters_location$theta_j <- list(
     parameter_name = "theta_j",
     description = "WASH coverage index (proportion with adequate WASH)",
     distribution = "beta",
     parameters = list(
          location = list()
     )
)

theta_uncertainty_one_sided <- 0.2

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

          priors_default$parameters_location$theta_j$parameters$location[[iso]] <- list(
               shape1 = theta_fit$shape1,
               shape2 = theta_fit$shape2,
               point_estimate = wash_value
          )

     }

} else {

     # Fallback defaults
     for (iso in j) {
          priors_default$parameters_location$theta_j$parameters$location[[iso]] <- list(
               shape1 = 13.44396,
               shape2 = 8.236964,
               point_estimate = 0.6322853
          )
     }
}




# Seasonality parameters (a_1, a_2, b_1, b_2) - Fourier wave function parameters
# Normal distributions with parameters loaded from param_seasonal_dynamics.csv

# Uncertainty adjustment parameter for seasonality
# Values < 1 increase uncertainty (wider distributions)
# Values > 1 decrease uncertainty (narrower distributions)
# Value = 1 keeps original uncertainty from file
seasonality_uncertainty_factor <- 0.95  # Default: use original uncertainty from file

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
          parameter_name = param_name,
          description = paste0("Seasonality Fourier coefficient ", param, " (cases)"),
          distribution = "normal",
          uncertainty_factor = seasonality_uncertainty_factor,
          parameters = list(
               location = list()
          )
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

                    priors_default$parameters_location[[param_name]]$parameters$location[[iso]] <- list(
                         mean = mean_orig,
                         sd = se_adjusted
                    )

                    # Add metadata about the adjustment
                    if (seasonality_uncertainty_factor != 1.0) {
                         priors_default$parameters_location[[param_name]]$parameters$location[[iso]]$original_se <- se_orig
                         priors_default$parameters_location[[param_name]]$parameters$location[[iso]]$uncertainty_adjusted <- TRUE
                    }

                    # Add info if inferred from neighbor
                    if (!is.na(param_row$inferred_from_neighbor[1])) {
                         priors_default$parameters_location[[param_name]]$parameters$location[[iso]]$inferred_from <- param_row$inferred_from_neighbor[1]
                    }
               } else {
                    # Default values if not found
                    warning(paste("Seasonal parameter", param, "not found for", iso, "- using defaults"))
                    priors_default$parameters_location[[param_name]]$parameters$location[[iso]] <- list(
                         mean = 0,
                         sd = 0.5 / sqrt(seasonality_uncertainty_factor)
                    )
               }
          } else {
               # Default values if file doesn't exist
               priors_default$parameters_location[[param_name]]$parameters$location[[iso]] <- list(
                    mean = 0,
                    sd = 0.5 / sqrt(seasonality_uncertainty_factor)
               )
          }
     }
}

# Print adjustment message for seasonality
if (seasonality_uncertainty_factor != 1.0) {
     cat(paste0("\nSeasonality uncertainty adjusted by factor of ", seasonality_uncertainty_factor, "\n"))
     if (seasonality_uncertainty_factor < 1) {
          cat("  -> Distributions are WIDER (more uncertain)\n")
     } else {
          cat("  -> Distributions are NARROWER (less uncertain)\n")
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
# This ensures S + V1 + V2 + E + I + R = 1 mathematically
# Placeholder - will be replaced by est_initial_S() below

# prop_V1_initial - Initial proportion with one vaccine dose
# Beta(0.5, 49.5): mean = 1%, heavily right-skewed
# OCV coverage typically <5% in Africa
priors_default$parameters_location$prop_V1_initial <- list(
     parameter_name = "prop_V1_initial",
     description = "Initial proportion in one-dose vaccine (V1) compartment",
     distribution = "beta",
     parameters = list(
          location = list()
     )
)

for (iso in j) {
     priors_default$parameters_location$prop_V1_initial$parameters$location[[iso]] <- list(shape1 = 0.5, shape2 = 49.5)
}

# prop_V2_initial - Initial proportion with two vaccine doses
# Beta(0.5, 99.5): mean = 0.5%, very heavily right-skewed
# Lower than V1 as not everyone completes the series
priors_default$parameters_location$prop_V2_initial <- list(
     parameter_name = "prop_V2_initial",
     description = "Initial proportion in two-dose vaccine (V2) compartment",
     distribution = "beta",
     parameters = list(
          location = list()
     )
)

for (iso in j) {
     priors_default$parameters_location$prop_V2_initial$parameters$location[[iso]] <- list(shape1 = 0.5, shape2 = 99.5)
}

# prop_E_initial - Initial proportion exposed
# Beta(0.01, 9999.99): mean = 0.0001% (1 per million)
# Near-zero during inter-epidemic periods, short incubation keeps it small
priors_default$parameters_location$prop_E_initial <- list(
     parameter_name = "prop_E_initial",
     description = "Initial proportion in exposed (E) compartment",
     distribution = "beta",
     parameters = list(
          location = list()
     )
)

for (iso in j) {
     priors_default$parameters_location$prop_E_initial$parameters$location[[iso]] <- list(shape1 = 0.01, shape2 = 9999.99)
}

# prop_I_initial - Initial proportion infected
# Beta(0.01, 9999.99): mean = 0.0001% (1 per million)
# Even during outbreaks rarely exceeds 0.05% of population
priors_default$parameters_location$prop_I_initial <- list(
     parameter_name = "prop_I_initial",
     description = "Initial proportion in infected (I) compartment",
     distribution = "beta",
     parameters = list(
          location = list()
     )
)

for (iso in j) {
     priors_default$parameters_location$prop_I_initial$parameters$location[[iso]] <- list(shape1 = 0.01, shape2 = 9999.99)
}

# prop_R_initial - Initial proportion recovered/immune
# Beta(3.5, 14): mean = 20%, allows 5-40% range
# Highly variable depending on historical cholera exposure
priors_default$parameters_location$prop_R_initial <- list(
     parameter_name = "prop_R_initial",
     description = "Initial proportion in recovered/immune (R) compartment",
     distribution = "beta",
     parameters = list(
          location = list()
     )
)

for (iso in j) {
     priors_default$parameters_location$prop_R_initial$parameters$location[[iso]] <- list(shape1 = 3.5, shape2 = 14)
}



#--------------------------------------------------------------
# Update default priors with estimated initial conditions
# for Vaccination compartments (V1 and V2)
#--------------------------------------------------------------

# Use est_initial_V1_V2() with the in-progress priors_default object as input, use config_default
cat("Estimating initial V1/V2 vaccination compartments for all locations...\n")

# Run estimation for all locations in config_default
initial_conditions_V1_V2 <- est_initial_V1_V2(
     PATHS = PATHS,
     priors = priors_default,  # Use the priors being built in this script
     config = config_default,  # Use all locations from config_default
     n_samples = 1000,         # Production-quality uncertainty quantification
     t0 = date_start,         # Use date_start from config_default
     variance_inflation = 3,   # Triple variance for increased uncertainty (still supported)
     parallel = TRUE,           # Enable parallel processing for efficiency
     verbose = TRUE            # Quiet mode for cleaner output
)

cat(sprintf("  Completed V1/V2 estimation for %d locations\n",
            initial_conditions_V1_V2$metadata$initial_conditions_V1_V2$n_locations_processed))

# Update initial conditions priors with priors from est_initial_V1_V2()
# The new structure matches priors_default exactly, so integration is simple
# Only update locations that already exist - do NOT create new locations

n_updated <- 0

# Update prop_V1_initial for each location
for (loc in names(initial_conditions_V1_V2$parameters_location$prop_V1_initial$parameters$location)) {
     # Only update if location already exists in priors_default
     if (!is.null(priors_default$parameters_location$prop_V1_initial$parameters$location[[loc]])) {
          loc_estimate <- initial_conditions_V1_V2$parameters_location$prop_V1_initial$parameters$location[[loc]]

          if (!is.na(loc_estimate$shape1)) {
               # Direct replacement with data-based estimate
               priors_default$parameters_location$prop_V1_initial$parameters$location[[loc]] <- loc_estimate
               n_updated <- n_updated + 1
          }
     }
}

# Update prop_V2_initial for each location
for (loc in names(initial_conditions_V1_V2$parameters_location$prop_V2_initial$parameters$location)) {
     # Only update if location already exists in priors_default
     if (!is.null(priors_default$parameters_location$prop_V2_initial$parameters$location[[loc]])) {
          loc_estimate <- initial_conditions_V1_V2$parameters_location$prop_V2_initial$parameters$location[[loc]]

          if (!is.na(loc_estimate$shape1)) {
               # Direct replacement with data-based estimate
               priors_default$parameters_location$prop_V2_initial$parameters$location[[loc]] <- loc_estimate
          }
     }
}

cat(sprintf("  Added V1/V2 initial condition priors for %d locations\n", n_updated))

# Add metadata about the V1/V2 estimation
if (is.null(priors_default$metadata$initial_conditions)) {
     priors_default$metadata$initial_conditions <- list()
}

# Copy metadata from the estimation results
priors_default$metadata$initial_conditions$V1_V2 <- initial_conditions_V1_V2$metadata$initial_conditions_V1_V2
priors_default$metadata$initial_conditions$V1_V2$summary <- initial_conditions_V1_V2$metadata$summary

cat("  V1/V2 initial condition priors successfully added to priors_default\n\n")

#--------------------------------------------------------------
# Update default priors with estimated initial conditions
# for the Infection compartments (E and I)
#--------------------------------------------------------------

if (T) {

     cat("Estimating initial E/I compartments from recent surveillance data...\n")

     # Run estimation for all locations using enhanced flexible Beta fitting
     initial_conditions_E_I <- est_initial_E_I(
          PATHS = PATHS,
          priors = priors_default,  # Use the priors being built in this script
          config = config_default,  # Use all locations from config_default
          n_samples = 1000,         # Production-quality uncertainty quantification
          t0 = date_start,         # Use date_start from config_default
          lookback_days = 14,      # Default lookback window
          prior_method = "left_skewed",    # Biologically motivated default
          expansion_factor = 4.0,          # Wide CIs for scenario exploration
          conservatism_bias = 0.3,         # Moderate bias toward zero for production
          verbose = TRUE,          # Verbose output for monitoring
          parallel = TRUE          # Can enable for faster processing
     )

     cat(sprintf("  Completed E/I estimation for %d locations\n",
                 length(initial_conditions_E_I$parameters_location$prop_E_initial$parameters$location)))

     # Update initial conditions priors with estimates from est_initial_E_I()
     # The new structure matches priors_default exactly, so integration is simple
     # Only update locations that already exist - do NOT create new locations

     n_updated_E_I <- 0

     # Update prop_E_initial for each location
     for (loc in names(initial_conditions_E_I$parameters_location$prop_E_initial$parameters$location)) {
          # Only update if location already exists in priors_default
          if (!is.null(priors_default$parameters_location$prop_E_initial$parameters$location[[loc]])) {
               loc_estimate <- initial_conditions_E_I$parameters_location$prop_E_initial$parameters$location[[loc]]

               if (!is.na(loc_estimate$shape1)) {
                    # Direct replacement with data-based estimate
                    priors_default$parameters_location$prop_E_initial$parameters$location[[loc]] <- loc_estimate
                    n_updated_E_I <- n_updated_E_I + 1
               }
          }
     }

     # Update prop_I_initial for each location
     for (loc in names(initial_conditions_E_I$parameters_location$prop_I_initial$parameters$location)) {
          # Only update if location already exists in priors_default
          if (!is.null(priors_default$parameters_location$prop_I_initial$parameters$location[[loc]])) {
               loc_estimate <- initial_conditions_E_I$parameters_location$prop_I_initial$parameters$location[[loc]]

               if (!is.na(loc_estimate$shape1)) {
                    # Direct replacement with data-based estimate
                    priors_default$parameters_location$prop_I_initial$parameters$location[[loc]] <- loc_estimate
               }
          }
     }

     cat(sprintf("  Added E/I initial condition priors for %d locations\n", n_updated_E_I))

     # Add metadata about the E/I estimation
     if (is.null(priors_default$metadata$initial_conditions)) {
          priors_default$metadata$initial_conditions <- list()
     }

     # Copy metadata from the estimation results
     priors_default$metadata$initial_conditions$E_I <- initial_conditions_E_I$metadata
     if (!is.null(initial_conditions_E_I$metadata$summary)) {
          priors_default$metadata$initial_conditions$E_I$summary <- initial_conditions_E_I$metadata$summary
     }

     cat("  E/I initial condition priors successfully added to priors_default\n\n")

}


#--------------------------------------------------------------
# Update default priors with estimated initial conditions
# for the Recovered compartment (R)
#--------------------------------------------------------------

cat("Estimating initial R compartment from historical cholera surveillance data...\n")

# Use the package version of est_initial_R (already loaded via library(MOSAIC))
# Note: Removed source() to use the updated package function with new variance inflation method

# Run estimation for all locations using temporal disaggregation
# NOTE: est_initial_R still uses the old variance_inflation parameter
initial_conditions_R <- est_initial_R(
     PATHS = PATHS,
     priors = priors_default,  # Use the priors being built in this script
     config = config_default,  # Use all locations from config_default
     n_samples = 1000,         # Production-quality uncertainty quantification
     t0 = date_start,         # Use date_start from config_default
     disaggregate = TRUE,      # Use Fourier disaggregation for better accuracy
     variance_inflation = 10,  # Higher inflation for wider uncertainty (old parameter)
     verbose = TRUE,          # Verbose output for monitoring
     parallel = TRUE           # Enable parallel processing for faster computation
)

cat(sprintf("  Completed R estimation for %d locations\n",
            initial_conditions_R$metadata$initial_conditions_R$n_locations_processed))

# Update initial conditions priors with priors from est_initial_R()
# The new structure matches priors_default exactly, so integration is simple
# Only update locations that already exist - do NOT create new locations

n_updated_R <- 0

# Update prop_R_initial for each location
for (loc in names(initial_conditions_R$parameters_location$prop_R_initial$parameters$location)) {
     # Only update if location already exists in priors_default
     if (!is.null(priors_default$parameters_location$prop_R_initial$parameters$location[[loc]])) {
          loc_estimate <- initial_conditions_R$parameters_location$prop_R_initial$parameters$location[[loc]]

          if (!is.na(loc_estimate$shape1)) {
               # Direct replacement with data-based estimate
               priors_default$parameters_location$prop_R_initial$parameters$location[[loc]] <- loc_estimate
               n_updated_R <- n_updated_R + 1

               # Optional: Print summary for each location
               if (!is.null(loc_estimate$metadata)) {
                    cat(sprintf("    %s: R = %.2f%% (%.2f%% - %.2f%%)\n",
                                loc,
                                loc_estimate$metadata$mean * 100,
                                loc_estimate$metadata$ci_lower * 100,
                                loc_estimate$metadata$ci_upper * 100))
               }
          }
     }
}

cat(sprintf("  Added R initial condition priors for %d locations\n", n_updated_R))

# Add metadata about the R estimation
if (is.null(priors_default$metadata$initial_conditions)) {
     priors_default$metadata$initial_conditions <- list()
}

# Copy metadata from the estimation results
priors_default$metadata$initial_conditions$R <- initial_conditions_R$metadata$initial_conditions_R
if (!is.null(initial_conditions_R$metadata$summary)) {
     priors_default$metadata$initial_conditions$R$summary <- initial_conditions_R$metadata$summary
}

cat("  R initial condition priors successfully added to priors_default\n\n")

#--------------------------------------------------------------
# Update default priors with estimated initial conditions
# for the Susceptible compartment (S) - CONSTRAINED RESIDUAL
#--------------------------------------------------------------

cat("Estimating initial S compartment using constrained residual method...\n")

# Use est_initial_S() with all other compartments already estimated
# This ensures S + V1 + V2 + E + I + R = 1 mathematically

# Note that this distribution for the S compartment is not directly sampled from in the sample_parameters() function.
# The value for S is still determined as the remainder: (S = N - (V1 + V2 + E + I + R)), but its implied prior is effectively
# the same as what is sampled below in the est_initial_S function

# NOTE: est_initial_S still uses the old variance_inflation parameter
initial_conditions_S <- est_initial_S(
     PATHS = PATHS,
     priors = priors_default,  # Use priors with all other compartments estimated
     config = config_default,  # Use all locations from config_default
     n_samples = 1000,         # Production-quality uncertainty quantification
     t0 = date_start,         # Use date_start from config_default
     variance_inflation = 0,   # No inflation for S (constrained residual, old parameter)
     verbose = TRUE,          # Verbose output for monitoring
     min_S_proportion = 0.001   # Minimum 0.1% susceptible for biological realism
)

cat(sprintf("  Completed S estimation for %d locations\n",
            initial_conditions_S$metadata$initial_conditions_S$n_locations_processed))

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
                         parameter_name = "prop_S_initial",
                         description = "Initial proportion in susceptible (S) compartment from constrained residual",
                         distribution = "beta",
                         parameters = list(location = list())
                    )
               }

               # Direct replacement with constrained estimate
               priors_default$parameters_location$prop_S_initial$parameters$location[[loc]] <- loc_estimate
               n_updated_S <- n_updated_S + 1

               # Optional: Print summary for each location
               if (!is.null(loc_estimate$metadata)) {
                    cat(sprintf("    %s: S = %.1f%% (%.1f%% - %.1f%%), violations: %.1f%%\n",
                                loc,
                                loc_estimate$metadata$mean * 100,
                                loc_estimate$metadata$ci_lower * 100,
                                loc_estimate$metadata$ci_upper * 100,
                                loc_estimate$metadata$constraint_violation_rate * 100))
               }
          }
     }
}

cat(sprintf("  Added S initial condition priors for %d locations\n", n_updated_S))

# Add metadata about the S estimation
if (is.null(priors_default$metadata$initial_conditions)) {
     priors_default$metadata$initial_conditions <- list()
}

# Copy metadata from the estimation results
priors_default$metadata$initial_conditions$S <- initial_conditions_S$metadata$initial_conditions_S
if (!is.null(initial_conditions_S$metadata$summary)) {
     priors_default$metadata$initial_conditions$S$summary <- initial_conditions_S$metadata$summary
}

cat("  S initial condition priors successfully added to priors_default\n")
cat("  Mathematical constraint S + V1 + V2 + E + I + R = 1 is now enforced\n\n")

# Verify compartment consistency for a sample location
if (n_updated_S > 0) {
     sample_loc <- names(initial_conditions_S$parameters_location$prop_S_initial$parameters$location)[1]
     if (!is.null(priors_default$parameters_location$prop_S_initial$parameters$location[[sample_loc]])) {
          cat(sprintf("Example compartment verification for %s:\n", sample_loc))

          # Calculate expected means for each compartment
          S_mean <- priors_default$parameters_location$prop_S_initial$parameters$location[[sample_loc]]$metadata$mean
          V1_mean <- if(!is.null(priors_default$parameters_location$prop_V1_initial$parameters$location[[sample_loc]]$metadata)) {
               priors_default$parameters_location$prop_V1_initial$parameters$location[[sample_loc]]$metadata$mean
          } else { 0.01 }
          V2_mean <- if(!is.null(priors_default$parameters_location$prop_V2_initial$parameters$location[[sample_loc]]$metadata)) {
               priors_default$parameters_location$prop_V2_initial$parameters$location[[sample_loc]]$metadata$mean
          } else { 0.005 }
          E_mean <- if(!is.null(priors_default$parameters_location$prop_E_initial$parameters$location[[sample_loc]]$metadata)) {
               priors_default$parameters_location$prop_E_initial$parameters$location[[sample_loc]]$metadata$mean
          } else { 0.000001 }
          I_mean <- if(!is.null(priors_default$parameters_location$prop_I_initial$parameters$location[[sample_loc]]$metadata)) {
               priors_default$parameters_location$prop_I_initial$parameters$location[[sample_loc]]$metadata$mean
          } else { 0.000001 }
          R_mean <- if(!is.null(priors_default$parameters_location$prop_R_initial$parameters$location[[sample_loc]]$metadata)) {
               priors_default$parameters_location$prop_R_initial$parameters$location[[sample_loc]]$metadata$mean
          } else { 0.2 }

          total_mean <- S_mean + V1_mean + V2_mean + E_mean + I_mean + R_mean
          cat(sprintf("  S=%.1f%% + V1=%.1f%% + V2=%.1f%% + E=%.3f%% + I=%.3f%% + R=%.1f%% = %.1f%%\n",
                      S_mean*100, V1_mean*100, V2_mean*100, E_mean*100, I_mean*100, R_mean*100, total_mean*100))

          if (abs(total_mean - 1.0) < 0.01) {
               cat("  ✓ Compartment consistency verified (sum ≈ 100%)\n")
          } else {
               cat("  ⚠ Potential inconsistency detected - check compartment estimates\n")
          }
          cat("\n")
     }
}

#-----------------------------------------
# Save to file and add to MOSAIC R package
#-----------------------------------------

fp <- file.path(getwd(), 'inst/extdata/priors_default.json')

# save to file
jsonlite::write_json(priors_default, fp, pretty = TRUE, auto_unbox = TRUE)

# Read back to verify
tmp_priors <- jsonlite::fromJSON(fp, simplifyVector = FALSE)

identical(priors_default$parameters_global$alpha_1, tmp_priors$parameters_global$alpha_1)

# Note: R data object is saved as priors_default to match config_default naming convention
usethis::use_data(priors_default, overwrite = TRUE)

