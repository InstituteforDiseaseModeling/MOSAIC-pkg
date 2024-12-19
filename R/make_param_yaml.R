#' Create a YAML File for MOSAIC Model Parameters
#'
#' This function generates a YAML file for MOSAIC model simulation parameters or returns the parameters as a list if no file output is specified.
#' It validates all input parameters to ensure they meet the required constraints for the MOSAIC simulation framework. The YAML file output is designed
#' for use with the MOSAIC R package and ensures compatibility with downstream Python-based tools such as the Light-agent Spatial Model for ERadication (LASER) platform.
#'
#' @param output_file_path A character string representing the full file path of the output YAML file (e.g., 'path/to/parameters.yaml'). If `NULL`, no file is written.
#'
#' ## Initialization
#' @param date_start Start date for the simulation period in "YYYY-MM-DD" format.
#' @param date_stop End date for the simulation period in "YYYY-MM-DD" format.
#' @param location_id A vector of integers giving the numerical index for metapopulations.
#' @param location_name A character vector giving the names of each metapopulation location, matching the length of `location_id`.
#' @param N_j_initial An integer vector of length `location_id` that gives the total initial population size of each j location.
#' @param S_j_initial An integer vector of length `location_id` that gives the starting values for the number of susceptible individuals in each j location.
#' @param I_j_initial An integer vector of length `location_id` that gives the starting values for the number of infected individuals in each j location.
#' @param R_j_initial An integer vector of length `location_id` that gives the starting values for the number of recovered individuals in each j location.
#'
#' ## Demographics
#' @param b_j Birth rate of population j. Must be numeric, non-negative, and the same length as `location_id`.
#' @param d_j Mortality rate of population j. Must be numeric, non-negative, and the same length as `location_id`.
#'
#' ## Immune Dynamics
#' @param nu_jt A matrix of vaccination rates for each location (`j`) and time step (`t`). Must have rows equal to `length(location_id)` and columns equal to the daily sequence from `date_start` to `date_stop`.
#' @param phi Effectiveness of Oral Cholera Vaccine (OCV). Must be numeric and within the range [0, 1].
#' @param omega Waning immunity rate of vaccinated individuals. Must be a numeric scalar greater than or equal to zero.
#' @param epsilon Waning immunity rate of recovered individuals. Must be a numeric scalar greater than or equal to zero.
#' @param gamma Recovery rate of infected individuals. Must be a numeric scalar greater than or equal to zero.
#'
#' ## Observation Processes
#' @param mu Mortality rate due to V. cholerae infection. Must be a numeric scalar greater than or equal to zero.
#' @param rho Proportion of suspected cholera cases that are true infections. Must be a numeric scalar between 0 and 1.
#' @param sigma Proportion of symptomatic V. cholerae infections. Must be a numeric scalar between 0 and 1.
#'
#' ## Force of Infection (human-to-human)
#' @param beta_j0_hum Baseline human-to-human transmission rate for each location. Must be a numeric vector of length equal to `location_id` and values greater than or equal to zero.
#' @param beta_j_seasonality The seasonal derivation from mean transmission (beta_j0_hum) for each j location. The seasonality term is given in the Z-score indicating the number of standard deviations
#' away from the mean rate of transmission. Length should be 365 for each day of the year. Values are estimated in the `est_seasonal_dynamics()` function.
#' @param tau_i Departure probability for each origin location. Must be a numeric vector of length equal to `location_id` and values between 0 and 1.
#' @param pi_ij A matrix of travel probabilities between origin and destination locations. Must have dimensions equal to `length(location_id)` x `length(location_id)` and values between 0 and 1.
#' @param alpha Population mixing parameter. Must be a numeric scalar between 0 and 1.
#'
#' ## Force of Infection (environment-to-human)
#' @param beta_j0_env Baseline environment-to-human transmission rate for each location. Must be a numeric vector of length equal to `location_id` and values greater than or equal to zero.
#' @param theta_j Proportion of the population with adequate Water, Sanitation, and Hygiene (WASH). Must be a numeric vector of length equal to `location_id` and values between 0 and 1.
#' @param psi_jt A 2D matrix of environmental suitability values for V. cholerae. Must have rows equal to `length(location_id)` and columns equal to the daily sequence from `date_start` to `date_stop`, with values between 0 and 1.
#' @param zeta Shedding rate of V. cholerae into the environment. Must be a numeric scalar greater than zero.
#' @param kappa Concentration of V. cholerae required for 50% infection probability. Must be a numeric scalar greater than zero.
#' @param delta_min Minimum environmental decay rate of V. cholerae. Must be a numeric scalar greater than zero and less than `delta_max` (indicating a faster decay rate).
#' @param delta_max Maximum environmental decay rate of V. cholerae.
#'
#' @return Returns the validated list of parameters. If `output_file_path` is provided, the parameters are also written to a YAML file.
#'
#' @examples
#' make_param_yaml(
#'      output_file_path = "parameters.yaml",
#'      date_start = "2024-12-01",
#'      date_stop = "2024-12-31",
#'      location_id = 1:2,
#'      location_name = c("Location A", "Location B"),
#'      N_j_initial = as.integer(c(1000, 1000)),
#'      S_j_initial = as.integer(c(999, 999)),
#'      I_j_initial = as.integer(c(1, 1)),
#'      R_j_initial = as.integer(c(0, 0)),
#'      b_j = c(0.01, 0.02),
#'      d_j = c(0.01, 0.02),
#'      nu_jt = matrix(data = 0, nrow = 2, ncol = 31),
#'      phi = 0.8,
#'      omega = 0.1,
#'      epsilon = 0.05,
#'      gamma = 0.2,
#'      mu = 0.01,
#'      rho = 0.9,
#'      sigma = 0.5,
#'      beta_j0_hum = c(0.05, 0.03),
#'      beta_j_seasonality = rep(0, 365),
#'      tau_i = c(0.1, 0.2),
#'      pi_ij = matrix(c(0.8, 0.2, 0.2, 0.8), nrow = 2),
#'      alpha = 0.95,
#'      beta_j0_env = c(0.02, 0.04),
#'      theta_j = c(0.6, 0.7),
#'      psi_jt = matrix(data = 0, nrow = 2, ncol = 31),
#'      zeta = 0.5,
#'      kappa = 10^5,
#'      delta_min = 0.001,
#'      delta_max = 0.01
#' )
#'
#' @export

make_param_yaml <- function(output_file_path = NULL,

                            # Initialization
                            date_start = NULL,
                            date_stop = NULL,
                            location_id = NULL,
                            location_name = NULL,
                            N_j_initial = NULL,
                            S_j_initial = NULL,
                            I_j_initial = NULL,
                            R_j_initial = NULL,

                            # Demographics
                            b_j = NULL,
                            d_j = NULL,

                            # Immune Dynamics
                            nu_jt = NULL,
                            phi = NULL,
                            omega = NULL,
                            epsilon = NULL,
                            gamma = NULL,

                            # Observation Processes
                            mu = NULL,
                            rho = NULL,
                            sigma = NULL,

                            # Force of Infection (human-to-human)
                            beta_j0_hum = NULL,
                            beta_j_seasonality = NULL,
                            tau_i = NULL,
                            pi_ij = NULL,
                            alpha = NULL,

                            # Force of Infection (environment-to-human)
                            beta_j0_env = NULL,
                            theta_j = NULL,
                            psi_jt = NULL,
                            zeta = NULL,
                            kappa = NULL,
                            delta_min = NULL,
                            delta_max = NULL) {

     # Combine all parameters into a named list
     params <- list(
          date_start = date_start,
          date_stop = date_stop,
          location_id = location_id,
          location_name = location_name,
          N_j_initial = N_j_initial,
          S_j_initial = S_j_initial,
          I_j_initial = I_j_initial,
          R_j_initial = R_j_initial,
          b_j = b_j,
          d_j = d_j,
          nu_jt = nu_jt,
          phi = phi,
          omega = omega,
          epsilon = epsilon,
          gamma = gamma,
          mu = mu,
          rho = rho,
          sigma = sigma,
          beta_j0_hum = beta_j0_hum,
          beta_j_seasonality = beta_j_seasonality,
          tau_i = tau_i,
          pi_ij = pi_ij,
          alpha = alpha,
          beta_j0_env = beta_j0_env,
          theta_j = theta_j,
          psi_jt = psi_jt,
          zeta = zeta,
          kappa = kappa,
          delta_min = delta_min,
          delta_max = delta_max
     )

     message('Validating parameter values')

     # Check for NULL values
     null_fields <- names(params)[sapply(params, is.null)]
     if (length(null_fields) > 0) {
          stop("The following parameters are NULL and must be provided: ", paste(null_fields, collapse = ", "))
     }

     # Initialization validation
     if (!lubridate::is.Date(date_start) || !grepl("^\\d{4}-\\d{2}-\\d{2}$", date_start)) {
          stop("date_start must be in the format 'YYYY-MM-DD'. Provided: ", date_start)
     }

     if (!lubridate::is.Date(date_stop) || !grepl("^\\d{4}-\\d{2}-\\d{2}$", date_stop)) {
          stop("date_stop must be in the format 'YYYY-MM-DD'. Provided: ", date_stop)
     }

     if (!is.integer(location_id)) {
          stop("location_id must be a vector of integers.")
     }

     t <- seq.Date(as.Date(date_start), as.Date(date_stop), by = "day")
     message(paste0('Number of daily time steps: ', length(t)))
     message(paste0('Number of metapopulation locations: ', length(location_id)))

     if (!is.character(location_name)) {
          stop("location_name must be a character vector.")
     }

     if (length(location_id) != length(location_name)) {
          stop("location_id and location_name must have the same length.")
     }

     if (!is.integer(N_j_initial) || length(N_j_initial) != length(location_id) || any(N_j_initial <= 0)) {
          stop("N_j_initial must be an integer vector of length equal to location_id with values greater than zero.")
     }

     if (!is.integer(S_j_initial) || length(S_j_initial) != length(location_id) || any(S_j_initial < 0)) {
          stop("S_j_initial must be an integer vector of length equal to location_id with values greater than or equal to zero.")
     }

     if (!is.integer(I_j_initial) || length(I_j_initial) != length(location_id) || any(I_j_initial < 0)) {
          stop("I_j_initial must be an integer vector of length equal to location_id with values greater than or equal to zero.")
     }

     if (!is.integer(R_j_initial) || length(R_j_initial) != length(location_id) || any(R_j_initial < 0)) {
          stop("R_j_initial must be an integer vector of length equal to location_id with values greater than or equal to zero.")
     }

     # Check that N_j_initial equals the sum of S_j_initial, I_j_initial, and R_j_initial
     calculated_N <- S_j_initial + I_j_initial + R_j_initial
     if (!all(N_j_initial == calculated_N)) {
          mismatch <- which(N_j_initial != calculated_N)
          stop("N_j_initial must equal the sum of S_j_initial, I_j_initial, and R_j_initial for all locations. Mismatched indices: ", paste(mismatch, collapse = ", "))
     }

     # Demographics validation
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

     # Immune Dynamics validation
     if (!is.null(nu_jt)) {
          if (!is.matrix(nu_jt) || nrow(nu_jt) != length(location_id) || ncol(nu_jt) != length(t)) {
               stop("nu_jt must be a matrix with rows equal to length(location_id) and columns equal to the daily sequence from date_start to date_stop.")
          }
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

     # Observation Processes validation
     if (!is.numeric(mu) || mu < 0) {
          stop("mu must be a numeric scalar greater than or equal to zero.")
     }

     if (!is.numeric(rho) || rho < 0 || rho > 1) {
          stop("rho must be a numeric scalar between 0 and 1.")
     }

     if (!is.numeric(sigma) || sigma < 0 || sigma > 1) {
          stop("sigma must be a numeric scalar between 0 and 1.")
     }

     # Force of Infection (human-to-human) validation
     if (!is.numeric(beta_j0_hum) || any(beta_j0_hum < 0) || length(beta_j0_hum) != length(location_id)) {
          stop("beta_j0_hum must be a numeric vector of length equal to location_id and values greater than or equal to zero.")
     }

     if (!is.numeric(beta_j_seasonality) ||
         !is.matrix(beta_j_seasonality) ||
         nrow(beta_j_seasonality) != length(location_id) ||
         ncol(beta_j_seasonality) != 366) {
          stop("beta_j_seasonality must be a matrix with rows equal to length(location_id), columns equal to 366 for annual seasonality on the daily scale, and values greater than or equal to zero.")
     }

     if (!is.numeric(tau_i) || any(tau_i < 0 | tau_i > 1) || length(tau_i) != length(location_id)) {
          stop("tau_i must be a numeric vector of length equal to location_id and values between 0 and 1.")
     }

     if (!is.null(pi_ij)) {
          if (!is.matrix(pi_ij) || nrow(pi_ij) != length(location_id) || ncol(pi_ij) != length(location_id)) {
               stop("pi_ij must be a matrix with dimensions equal to length(location_id) x length(location_id).")
          }
     }

     if (!is.numeric(alpha) || alpha < 0 || alpha > 1) {
          stop("alpha must be a numeric scalar between 0 and 1.")
     }

     # Force of Infection (environment-to-human) validation
     if (!is.numeric(beta_j0_env) || any(beta_j0_env < 0) || length(beta_j0_env) != length(location_id)) {
          stop("beta_j0_env must be a numeric vector of length equal to location_id and values greater than or equal to zero.")
     }

     if (!is.numeric(theta_j) || any(theta_j < 0 | theta_j > 1) || length(theta_j) != length(location_id)) {
          stop("theta_j must be a numeric vector of length equal to location_id and values between 0 and 1.")
     }

     if (!is.null(psi_jt)) {
          if (!is.matrix(psi_jt) || nrow(psi_jt) != length(location_id) || ncol(psi_jt) != length(t)) {
               stop("psi_jt must be a matrix with rows equal to length(location_id) and columns equal to the daily sequence from date_start to date_stop.")
          }
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

     if (delta_min <= delta_max) {
          stop("The decay rate delta_min must be greater than decay rate for delta_max.")
     }

     tmp <- split(params$nu_jt, row(params$nu_jt))
     params$nu_jt <- lapply(tmp, as.integer)

     tmp <- split(params$beta_j_seasonality, row(params$beta_j_seasonality))
     params$beta_j_seasonality <- lapply(tmp, as.numeric)

     tmp <- split(params$pi_ij, row(params$pi_ij))
     params$pi_ij <- lapply(tmp, as.numeric)

     tmp <- split(params$psi_jt, row(params$psi_jt))
     params$psi_jt <- lapply(tmp, as.numeric)


     if (!is.null(output_file_path)) {

          if (grepl("\\.yaml$", output_file_path, ignore.case = TRUE)) {

               yaml::write_yaml(params, file = output_file_path, indent.mapping.sequence = TRUE)
               message("YAML file written to: ", output_file_path)

          } else if (grepl("\\.json$", output_file_path, ignore.case = TRUE)) {

               jsonlite::write_json(params, path = output_file_path, auto_unbox = TRUE, pretty = TRUE)
               message("JSON file written to: ", output_file_path)

          } else {

               stop("Unsupported file format. The output file must have a .yaml or .json extension.")

          }

     } else {

          return(params)

     }


}
