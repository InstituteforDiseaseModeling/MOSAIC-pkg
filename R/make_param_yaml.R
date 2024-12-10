#' Create a YAML File for MOSAIC Model Parameters
#'
#' This function generates a YAML file for MOSAIC model simulation parameters or returns the parameters as a list if no file output is specified.
#' It validates all input parameters to ensure they meet the required constraints for the MOSAIC simulation framework.
#'
#' This function is designed for use with the MOSAIC R package and ensures compatibility with downstream Python-based tools.
#'
#' @param output_file_path A character string representing the full file path of the output YAML file (e.g., 'path/to/parameters.yaml'). If `NULL`, no file is written.
#' @param date_start Start date for the simulation period in "YYYY-MM-DD" format.
#' @param date_stop End date for the simulation period in "YYYY-MM-DD" format.
#' @param location_id A vector of integers giving the numerical index for metapopulations.
#' @param location_name A character vector giving the names of each metapopulation location, matching the length of `location_id`.
#' @param b_j Birth rate of population j. Must be numeric, non-negative, and the same length as `location_id`.
#' @param d_j Mortality rate of population j. Must be numeric, non-negative, and the same length as `location_id`.
#' @param N_j Population size of destination j. Must be an integer vector, positive, and the same length as `location_id`.
#' @param phi Effectiveness of Oral Cholera Vaccine (OCV). Must be numeric and within the range [0, 1].
#' @param nu_jt A matrix of vaccination rates for each location (`j`) and time step (`t`). Must have rows equal to `length(location_id)` and columns equal to the daily sequence from `date_start` to `date_stop`.
#' @param omega Waning immunity rate of vaccinated individuals. Must be a numeric scalar greater than or equal to zero.
#' @param epsilon Waning immunity rate of recovered individuals. Must be a numeric scalar greater than or equal to zero.
#' @param gamma Recovery rate of infected individuals. Must be a numeric scalar greater than or equal to zero.
#' @param mu Mortality rate due to V. cholerae infection. Must be a numeric scalar greater than or equal to zero.
#' @param sigma Proportion of symptomatic V. cholerae infections. Must be a numeric scalar between 0 and 1.
#' @param rho Proportion of suspected cholera cases that are true infections. Must be a numeric scalar between 0 and 1.
#' @param zeta Shedding rate of V. cholerae into the environment. Must be a numeric scalar greater than zero.
#' @param kappa Concentration of V. cholerae required for 50% infection probability. Must be a numeric scalar greater than zero.
#' @param delta_min Minimum environmental decay rate of V. cholerae. Must be a numeric scalar greater than zero and less than `delta_max`.
#' @param delta_max Maximum environmental decay rate of V. cholerae. Must be a numeric scalar greater than zero and greater than `delta_min`.
#' @param psi_jt A 2D matrix of environmental suitability values for V. cholerae. Must have rows equal to `length(location_id)` and columns equal to the daily sequence from `date_start` to `date_stop`, with values between 0 and 1.
#' @param beta_j0_hum Baseline human-to-human transmission rate for each location. Must be a numeric vector of length equal to `location_id` and values greater than or equal to zero.
#' @param beta_j0_env Baseline environment-to-human transmission rate for each location. Must be a numeric vector of length equal to `location_id` and values greater than or equal to zero.
#' @param alpha Population mixing parameter. Must be a numeric scalar between 0 and 1.
#' @param tau_i Departure probability for each origin location. Must be a numeric vector of length equal to `location_id` and values between 0 and 1.
#' @param pi_ij A matrix of travel probabilities between origin and destination locations. Must have dimensions equal to `length(location_id)` x `length(location_id)` and values between 0 and 1.
#' @param theta_j Proportion of the population with adequate Water, Sanitation, and Hygiene (WASH). Must be a numeric vector of length equal to `location_id` and values between 0 and 1.
#'
#' @return Returns the validated list of parameters. If `output_file_path` is provided, the parameters are also written to a YAML file.
#'
#' @examples
#' # Generate parameters and write to file
#'
#'
#' make_param_yaml(
#'      output_file_path = "parameters.yaml",
#'      date_start = "2024-12-01",
#'      date_stop = "2024-12-31",
#'      location_id = 1:2,
#'      location_name = c("Location A", "Location B"),
#'      b_j = c(0.01, 0.02),
#'      d_j = c(0.01, 0.02),
#'      N_j = c(1000L, 2000L),
#'      phi = 0.8,
#'      nu_jt = matrix(data = 0, nrow = 2, ncol = 31),
#'      omega = 0.1,
#'      epsilon = 0.05,
#'      gamma = 0.2,
#'      mu = 0.01,
#'      sigma = 0.5,
#'      rho = 0.9,
#'      zeta = 0.5,
#'      kappa = 10^5,
#'      delta_min = 0.001,
#'      delta_max = 0.01,
#'      psi_jt = matrix(data = 0, nrow = 2, ncol = 31),
#'      beta_j0_hum = c(0.05, 0.03),
#'      beta_j0_env = c(0.02, 0.04),
#'      alpha = 0.95,
#'      tau_i = c(0.1, 0.2),
#'      pi_ij = matrix(c(0.8, 0.2, 0.2, 0.8), nrow = 2),
#'      theta_j = c(0.6, 0.7)
#' )
#'
#' @export

make_param_yaml <- function(output_file_path = NULL,
                            date_start = NULL,
                            date_stop = NULL,
                            location_id = NULL,
                            location_name = NULL,
                            b_j = NULL,
                            d_j = NULL,
                            N_j = NULL,
                            phi = NULL,
                            nu_jt = NULL,
                            omega = NULL,
                            epsilon = NULL,
                            gamma = NULL,
                            mu = NULL,
                            sigma = NULL,
                            rho = NULL,
                            zeta = NULL,
                            kappa = NULL,
                            delta_min = NULL,
                            delta_max = NULL,
                            psi_jt = NULL,
                            beta_j0_hum = NULL,
                            beta_j0_env = NULL,
                            alpha = NULL,
                            tau_i = NULL,
                            pi_ij = NULL,
                            theta_j = NULL)
{

     # Combine all parameters into a named list
     params <- list(
          date_start = date_start,
          date_stop = date_stop,
          location_id = location_id,
          location_name = location_name,
          b_j = b_j,
          d_j = d_j,
          N_j = N_j,
          phi = phi,
          nu_jt = nu_jt,
          omega = omega,
          epsilon = epsilon,
          gamma = gamma,
          mu = mu,
          sigma = sigma,
          rho = rho,
          zeta = zeta,
          kappa = kappa,
          delta_min = delta_min,
          delta_max = delta_max,
          psi_jt = psi_jt,
          beta_j0_hum = beta_j0_hum,
          beta_j0_env = beta_j0_env,
          alpha = alpha,
          tau_i = tau_i,
          pi_ij = pi_ij,
          theta_j = theta_j
     )


     # Check for NULL values
     null_fields <- names(params)[sapply(params, is.null)]
     if (length(null_fields) > 0) {
          stop("The following parameters are NULL and must be provided: ", paste(null_fields, collapse = ", "))
     }

     # Generate the index t from date_start to date_stop
     t <- seq.Date(as.Date(date_start), as.Date(date_stop), by = "day")

     # Additional validation checks
     current_date <- Sys.Date()

     if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", date_start)) {
          stop("date_start must be in the format 'YYYY-MM-DD'. Provided: ", date_start)
     }

     if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", date_stop)) {
          stop("date_stop must be in the format 'YYYY-MM-DD'. Provided: ", date_stop)
     }

     if (as.Date(date_start) >= current_date) {
          stop("date_start must be earlier than the current date. Provided: ", date_start)
     }

     sim_days <- as.Date(date_stop) - as.Date(date_start)
     days_prior <- current_date - as.Date(date_start)
     days_post <- as.Date(date_stop) - current_date

     message("Total simulation days: ", sim_days)
     message("Days prior to current date: ", days_prior)
     message("Days post current date: ", days_post)

     if (!is.integer(location_id)) {
          stop("location_id must be a vector of integers.")
     }

     if (!is.character(location_name)) {
          stop("location_name must be a character vector.")
     }

     if (length(location_id) != length(location_name)) {
          stop("location_id and location_name must have the same length.")
     }

     if (!is.numeric(b_j) || any(b_j < 0)) {
          stop("b_j must be a numeric vector with values greater than or equal to zero.")
     }

     if (!is.numeric(d_j) || any(d_j < 0)) {
          stop("d_j must be a numeric vector with values greater than or equal to zero.")
     }

     if (length(b_j) != length(location_id)) {
          stop("b_j must have the same length as location_id.")
     }

     if (length(d_j) != length(location_id)) {
          stop("d_j must have the same length as location_id.")
     }

     if (!is.integer(N_j)) {
          stop("N_j must be an integer vector.")
     }

     if (any(N_j <= 0)) {
          stop("N_j must contain values greater than zero.")
     }

     if (length(N_j) != length(location_id)) {
          stop("N_j must have the same length as location_id.")
     }

     if (!is.numeric(phi) || phi < 0 || phi > 1) {
          stop("phi must be numeric and within the range [0, 1].")
     }

     if (!is.numeric(omega) || omega < 0) {
          stop("omega must be a numeric scalar greater than or equal to zero.")
     }

     if (!is.numeric(epsilon) || epsilon < 0) {
          stop("epsilon must be a numeric scalar greater than or equal to zero.")
     }

     if (!is.numeric(gamma) || gamma < 0) {
          stop("gamma must be a numeric scalar greater than or equal to zero.")
     }

     if (!is.numeric(mu) || mu < 0) {
          stop("mu must be a numeric scalar greater than or equal to zero.")
     }

     if (!is.numeric(sigma) || sigma < 0 || sigma > 1) {
          stop("sigma must be a numeric scalar between 0 and 1.")
     }

     if (!is.numeric(rho) || rho < 0 || rho > 1) {
          stop("rho must be a numeric scalar between 0 and 1.")
     }

     if (!is.numeric(zeta) || zeta <= 0) {
          stop("zeta must be a numeric scalar greater than zero.")
     }

     if (!is.numeric(kappa) || kappa <= 0) {
          stop("kappa must be a numeric scalar greater than zero.")
     }

     if (!is.numeric(delta_min) || delta_min <= 0 || delta_min >= Inf) {
          stop("delta_min must be a numeric scalar greater than zero and less than Inf.")
     }

     if (!is.numeric(delta_max) || delta_max <= 0 || delta_max >= Inf) {
          stop("delta_max must be a numeric scalar greater than zero and less than Inf.")
     }

     if (delta_min >= delta_max) {
          stop("delta_min must be less than delta_max.")
     }

     if (!is.null(psi_jt)) {
          if (!is.matrix(psi_jt)) {
               stop("psi_jt must be a 2D matrix.")
          }

          if (!all(psi_jt >= 0 & psi_jt <= 1)) {
               stop("psi_jt must contain values between 0 and 1.")
          }

          if (nrow(psi_jt) != length(location_id)) {
               stop("The number of rows in psi_jt must equal the number of location_id.")
          }

          if (ncol(psi_jt) != length(t)) {
               stop("The number of columns in psi_jt must equal the length of the daily vector from date_start to date_stop.")
          }
     }

     if (!is.numeric(beta_j0_hum) || any(beta_j0_hum < 0) || length(beta_j0_hum) != length(location_id)) {
          stop("beta_j0_hum must be a numeric vector of length equal to location_id and values greater than or equal to zero.")
     }

     if (!is.numeric(beta_j0_env) || any(beta_j0_env < 0) || length(beta_j0_env) != length(location_id)) {
          stop("beta_j0_env must be a numeric vector of length equal to location_id and values greater than or equal to zero.")
     }

     if (!is.numeric(alpha) || alpha < 0 || alpha > 1) {
          stop("alpha must be a numeric scalar between 0 and 1.")
     }

     if (!is.numeric(tau_i) || any(tau_i < 0 | tau_i > 1) || length(tau_i) != length(location_id)) {
          stop("tau_i must be a numeric vector of length equal to location_id and values between 0 and 1.")
     }

     if (!is.null(pi_ij)) {
          if (!is.matrix(pi_ij)) {
               stop("pi_ij must be a 2D matrix.")
          }

          if (nrow(pi_ij) != length(location_id) || ncol(pi_ij) != length(location_id)) {
               stop("pi_ij must have rows and columns equal to length of location_id.")
          }

          if (!all(pi_ij >= 0 & pi_ij <= 1)) {
               stop("pi_ij must contain values between 0 and 1.")
          }
     }

     if (!is.numeric(theta_j) || any(theta_j < 0 | theta_j > 1) || length(theta_j) != length(location_id)) {
          stop("theta_j must be a numeric vector of length equal to location_id and values between 0 and 1.")
     }

     # Write to file if output_file_path is provided
     if (!is.null(output_file_path)) {

          if (!grepl("\\.yaml$", output_file_path, ignore.case = TRUE)) {
               stop("The output_file_path must end with '.yaml'. Provided: ", output_file_path)
          }

          yaml::write_yaml(params, file = output_file_path)
          message("YAML file written to: ", output_file_path)

     }

     return(params)
}
