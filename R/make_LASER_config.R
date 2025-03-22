#' Create a Configuration File for LASER
#'
#' This function generates a JSON/HDF5/YAML/OBJ configuration file to be used as LASER model simulation parameters.
#' It validates all input parameters and, if an output file path is provided, writes the parameters to a file.
#' The file extension determines which output format is used:
#' - .json or .json.gz → written with write_list_to_json,
#' - .h5, or .h5.gz → written with write_list_to_hdf5,
#' - .yaml or .yaml.gz → written with write_list_to_yaml.
#' - .obj or .obj.gz → written with write_list_to_obj (Python-compatible).
#'
#' @param output_file_path A character string representing the full file path of the output file.
#'        Must have a .json, .json.gz, .h5, .hdf5, .h5.gz, .yaml, or .yaml.gz extension.
#'        If NULL, no file is written and the parameters are returned.
#' @param seed Integer scalar giving the random seed value for the simulation run.
#'
#' ## Initialization
#' @param date_start Start date for the simulation period in "YYYY-MM-DD" format. If provided as a character string,
#'        it will be converted to a Date object.
#' @param date_stop End date for the simulation period in "YYYY-MM-DD" format. If provided as a character string,
#'        it will be converted to a Date object.
#' @param location_id A vector of integers giving the numerical index for metapopulations.
#' @param location_name A character vector giving the names of each metapopulation location.
#'        The order and names here must match those used in the initial population vectors.
#' @param N_j_initial A named numeric or integer vector of length equal to location_id giving the total initial
#'        population size for each location. Names must match the values in location_name.
#' @param S_j_initial A named numeric or integer vector of length equal to location_id giving the starting number
#'        of susceptible individuals for each location. Names must match location_name.
#' @param E_j_initial A named numeric or integer vector of length equal to location_id giving the starting number
#'        of exposed individuals for each location. Names must match location_name.
#' @param I_j_initial A named numeric or integer vector of length equal to location_id giving the starting number
#'        of infected individuals for each location. Names must match location_name.
#' @param R_j_initial A named numeric or integer vector of length equal to location_id giving the starting number
#'        of recovered individuals for each location. Names must match location_name.
#' @param V1_j_initial A named numeric or integer vector of length equal to location_id giving the starting number
#'        of individuals in vaccine compartment V1 for each location. Names must match location_name.
#' @param V2_j_initial A named numeric or integer vector of length equal to location_id giving the starting number
#'        of individuals in vaccine compartment V2 for each location. Names must match location_name.
#'
#' ## Demographics
#' @param b_jt A matrix of birth rates with rows equal to length(location_id) and columns equal to the daily
#'        sequence from date_start to date_stop.
#' @param d_jt A matrix of mortality rates with rows equal to length(location_id) and columns equal to the daily
#'        sequence from date_start to date_stop.
#'
#' ## Vaccination
#' @param nu_1_jt A matrix of first-dose OCV vaccinations for each location and time step.
#' @param nu_2_jt A matrix of second-dose OCV vaccinations for each location and time step.
#' @param phi_1 Effectiveness of one dose of OCV (numeric in [0, 1]).
#' @param phi_2 Effectiveness of two doses of OCV (numeric in [0, 1]).
#' @param omega_1 Waning immunity rate for one dose (numeric >= 0).
#' @param omega_2 Waning immunity rate for two doses (numeric >= 0).
#'
#' ## Infection dynamics
#' @param iota Incubation period (numeric > 0).
#' @param gamma_1 Recovery rate for severe infection (numeric >= 0).
#' @param gamma_2 Recovery rate for mild infection (numeric >= 0).
#' @param epsilon Waning immunity rate (numeric >= 0).
#' @param mu_jt A matrix of time-varying probabilities of mortality due to infection, with rows equal to length(location_id)
#'        and columns equal to length(t). All values must be numeric and between 0 and 1.
#'
#' ## Observation Processes
#' @param rho Proportion of true infections (numeric in [0, 1]).
#' @param sigma Proportion of symptomatic infections (numeric in [0, 1]).
#'
#' ## Force of Infection (human-to-human)
#' @param beta_j0_hum Baseline human-to-human transmission rate (numeric vector of length(location_id)).
#' @param beta_j_seasonality Seasonal variation in transmission (matrix with rows = length(location_id) and 366 columns).
#' @param tau_i Departure probability for each origin location (numeric vector of length(location_id) in [0, 1]).
#' @param pi_ij Matrix of travel probabilities (dimensions: length(location_id) x length(location_id)).
#' @param alpha_1 Transmission parameter for mixing (numeric in [0, 1]).
#' @param alpha_2 Transmission parameter for density dependence (numeric in [0, 1]).
#'
#' ## Force of Infection (environment-to-human)
#' @param beta_j0_env Baseline environment-to-human transmission rate (numeric vector of length(location_id)).
#' @param theta_j Proportion with adequate WASH (numeric vector of length(location_id) in [0, 1]).
#' @param psi_jt Matrix of environmental suitability values (matrix with rows = length(location_id) and columns
#'        equal to the daily sequence from date_start to date_stop).
#' @param zeta_1 Shedding rate (numeric > 0).
#' @param zeta_2 Shedding rate (numeric > 0; must be less than zeta_1).
#' @param kappa Concentration required for 50% infection (numeric > 0).
#' @param delta_min Minimum environmental decay rate (numeric > 0; must be > delta_max).
#' @param delta_max Maximum environmental decay rate (numeric > 0).
#'
#' @return Returns the validated list of parameters. If output_file_path is provided, the parameters are written to a file
#'         in the format determined by the file extension.
#'
#' @examples
#' \dontrun{
#' make_LASER_config(
#'      output_file_path = "parameters.json",
#'      compress = TRUE,
#'      seed = 123,
#'      date_start = "2024-12-01",
#'      date_stop = "2024-12-31",
#'      location_id = 1:2,
#'      location_name = c("Location A", "Location B"),
#'      N_j_initial = c("Location A" = 1000, "Location B" = 1000),
#'      S_j_initial = c("Location A" = 900, "Location B" = 900),
#'      E_j_initial = c("Location A" = 0, "Location B" = 0),
#'      I_j_initial = c("Location A" = 50, "Location B" = 50),
#'      R_j_initial = c("Location A" = 50, "Location B" = 50),
#'      V1_j_initial = c("Location A" = 0, "Location B" = 0),
#'      V2_j_initial = c("Location A" = 0, "Location B" = 0),
#'      b_jt = matrix(data = 0.0015, nrow = 2, ncol = 366),
#'      d_jt = matrix(data = 0.001, nrow = 2, ncol = 366),
#'      nu_1_jt = matrix(data = 0, nrow = 2, ncol = 366),
#'      nu_2_jt = matrix(data = 0, nrow = 2, ncol = 366),
#'      phi_1 = 0.8,
#'      phi_2 = 0.85,
#'      omega_1 = 0.1,
#'      omega_2 = 0.12,
#'      iota = 1.4,
#'      gamma_1 = 0.2,
#'      gamma_2 = 0.25,
#'      epsilon = 0.05,
#'      mu_jt = 0.01,
#'      rho = 0.9,
#'      sigma = 0.5,
#'      beta_j0_hum = c(0.05, 0.03),
#'      beta_j_seasonality = matrix(0, nrow = 2, ncol = 366),
#'      tau_i = c(0.1, 0.2),
#'      pi_ij = matrix(c(0.8, 0.2, 0.2, 0.8), nrow = 2),
#'      alpha_1 = 0.95,
#'      alpha_2 = 1,
#'      beta_j0_env = c(0.02, 0.04),
#'      theta_j = c(0.6, 0.7),
#'      psi_jt = matrix(data = 0, nrow = 2, ncol = 366),
#'      zeta_1 = 0.5,
#'      zeta_2 = 0.4,
#'      kappa = 10^5,
#'      delta_min = 0.1,
#'      delta_max = 0.01
#' )
#' }
#'
#' @export
#'

make_LASER_config <- function(output_file_path = NULL,
                              compress = FALSE,
                              seed = NULL,
                              # Initialization
                              date_start = NULL,
                              date_stop = NULL,
                              location_id = NULL,
                              location_name = NULL,
                              N_j_initial = NULL,
                              S_j_initial = NULL,
                              E_j_initial = NULL,
                              I_j_initial = NULL,
                              R_j_initial = NULL,
                              V1_j_initial = NULL,
                              V2_j_initial = NULL,
                              # Demographics
                              b_jt = NULL,
                              d_jt = NULL,
                              ## Vaccination
                              nu_1_jt = NULL,
                              nu_2_jt = NULL,
                              phi_1 = NULL,
                              phi_2 = NULL,
                              omega_1 = NULL,
                              omega_2 = NULL,
                              ## Infection dynamics
                              iota = NULL,
                              gamma_1 = NULL,
                              gamma_2 = NULL,
                              epsilon = NULL,
                              mu_jt = NULL,

                              # Observation Processes
                              rho = NULL,
                              sigma = NULL,
                              # Force of Infection (human-to-human)
                              beta_j0_hum = NULL,
                              beta_j_seasonality = NULL,
                              tau_i = NULL,
                              pi_ij = NULL,
                              alpha_1 = NULL,
                              alpha_2 = NULL,
                              # Force of Infection (environment-to-human)
                              beta_j0_env = NULL,
                              theta_j = NULL,
                              psi_jt = NULL,
                              zeta_1 = NULL,
                              zeta_2 = NULL,
                              kappa = NULL,
                              delta_min = NULL,
                              delta_max = NULL) {

     message('Validating parameter values')

     if (is.null(seed) || !is.numeric(seed) || length(seed) != 1 || seed <= 0 || seed %% 1 != 0) {
          stop("'seed' must be provided as an integer scalar greater than zero.")
     }

     # Convert date_start and date_stop to Date objects if provided as character strings.
     if (is.character(date_start)) {
          date_start_converted <- as.Date(date_start)
          if (is.na(date_start_converted)) {
               stop("date_start is not in a valid date format. Expected 'YYYY-MM-DD'.")
          } else {
               date_start <- date_start_converted
          }
     }

     if (is.character(date_stop)) {
          date_stop_converted <- as.Date(date_stop)
          if (is.na(date_stop_converted)) {
               stop("date_stop is not in a valid date format. Expected 'YYYY-MM-DD'.")
          } else {
               date_stop <- date_stop_converted
          }
     }

     # Combine all parameters into a named list.
     params <- list(
          seed              = seed,
          date_start        = date_start,
          date_stop         = date_stop,
          location_id       = location_id,
          location_name     = location_name,
          N_j_initial       = N_j_initial,
          S_j_initial       = S_j_initial,
          E_j_initial       = E_j_initial,
          I_j_initial       = I_j_initial,
          R_j_initial       = R_j_initial,
          V1_j_initial      = V1_j_initial,
          V2_j_initial      = V2_j_initial,
          b_jt              = b_jt,
          d_jt              = d_jt,
          nu_1_jt           = nu_1_jt,
          nu_2_jt           = nu_2_jt,
          phi_1             = phi_1,
          phi_2             = phi_2,
          omega_1           = omega_1,
          omega_2           = omega_2,
          iota              = iota,
          gamma_1           = gamma_1,
          gamma_2           = gamma_2,
          epsilon           = epsilon,
          mu_jt             = mu_jt,
          rho               = rho,
          sigma             = sigma,
          beta_j0_hum       = beta_j0_hum,
          beta_j_seasonality= beta_j_seasonality,
          tau_i             = tau_i,
          pi_ij             = pi_ij,
          alpha_1           = alpha_1,
          alpha_2           = alpha_2,
          beta_j0_env       = beta_j0_env,
          theta_j           = theta_j,
          psi_jt            = psi_jt,
          zeta_1            = zeta_1,
          zeta_2            = zeta_2,
          kappa             = kappa,
          delta_min         = delta_min,
          delta_max         = delta_max
     )

     # Check for NULL values.
     null_fields <- names(params)[sapply(params, is.null)]
     if (length(null_fields) > 0) {
          stop("The following parameters are NULL and must be provided: ", paste(null_fields, collapse = ", "))
     }

     # Validate date formats.
     if (!lubridate::is.Date(date_start) || !grepl("^\\d{4}-\\d{2}-\\d{2}$", as.character(date_start))) {
          stop("date_start must be in the format 'YYYY-MM-DD'. Provided: ", date_start)
     }

     if (!lubridate::is.Date(date_stop) || !grepl("^\\d{4}-\\d{2}-\\d{2}$", as.character(date_stop))) {
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

     # Validate initial population vectors.
     for (v in c("N_j_initial", "S_j_initial", "E_j_initial", "I_j_initial", "R_j_initial", "V1_j_initial", "V2_j_initial")) {
          vec <- params[[v]]
          # If supplied as numeric (i.e. double) then convert to integer.
          if (is.numeric(vec) && !is.integer(vec)) {
               vec <- as.integer(vec)
               names(vec) <- names(params[[v]])
          }
          if (is.null(names(vec)) || any(names(vec) == "")) {
               stop(v, " must be a named vector with names corresponding to each location.")
          }
          if (length(vec) != length(location_id)) {
               stop(v, " must be a vector of length equal to the number of locations.")
          }
          # For N_j_initial, values must be > 0; for compartments, values must be >= 0.
          if (v == "N_j_initial") {
               if (any(vec <= 0)) {
                    stop("N_j_initial must have values greater than zero.")
               }
          } else {
               if (any(vec < 0)) {
                    stop(v, " must have values greater than or equal to zero.")
               }
          }
          # Check that the names match exactly the location_name vector.
          if (!all(names(vec) == location_name)) {
               stop(v, " names must exactly match the provided location_name values.")
          }
          params[[v]] <- vec
     }

     # Check that N_j_initial equals the sum of the compartment vectors.
     calculated_N <- params[["S_j_initial"]] + params[["E_j_initial"]] + params[["I_j_initial"]] +
          params[["R_j_initial"]] + params[["V1_j_initial"]] + params[["V2_j_initial"]]
     if (!all(params[["N_j_initial"]] == calculated_N)) {
          mismatch <- which(params[["N_j_initial"]] != calculated_N)
          stop("N_j_initial must equal the sum of S_j_initial, E_j_initial, I_j_initial, R_j_initial, V1_j_initial, and V2_j_initial for all locations. Mismatched indices: ", paste(mismatch, collapse = ", "))
     }

     # Demographics validation: b_jt and d_jt should be matrices with rows equal to length(location_id) and columns equal to length(t).
     if (!is.matrix(b_jt) || nrow(b_jt) != length(location_id) || ncol(b_jt) != length(t)) {
          stop("b_jt must be a matrix with rows equal to length(location_id) and columns equal to the daily sequence from date_start to date_stop.")
     }

     if (!is.matrix(d_jt) || nrow(d_jt) != length(location_id) || ncol(d_jt) != length(t)) {
          stop("d_jt must be a matrix with rows equal to length(location_id) and columns equal to the daily sequence from date_start to date_stop.")
     }

     ## Vaccination validation.
     if (!is.matrix(nu_1_jt) || nrow(nu_1_jt) != length(location_id) || ncol(nu_1_jt) != length(t)) {
          stop("nu_1_jt must be a matrix with rows equal to length(location_id) and columns equal to the daily sequence from date_start to date_stop.")
     }
     if (!is.matrix(nu_2_jt) || nrow(nu_2_jt) != length(location_id) || ncol(nu_2_jt) != length(t)) {
          stop("nu_2_jt must be a matrix with rows equal to length(location_id) and columns equal to the daily sequence from date_start to date_stop.")
     }

     if (!is.numeric(phi_1) || phi_1 < 0 || phi_1 > 1) {
          stop("phi_1 must be numeric and within the range [0, 1].")
     }
     if (!is.numeric(phi_2) || phi_2 < 0 || phi_2 > 1) {
          stop("phi_2 must be numeric and within the range [0, 1].")
     }

     if (!is.numeric(omega_1) || omega_1 < 0) {
          stop("omega_1 must be a numeric scalar greater than or equal to zero.")
     }
     if (!is.numeric(omega_2) || omega_2 < 0) {
          stop("omega_2 must be a numeric scalar greater than or equal to zero.")
     }

     ## Infection dynamics validation.
     if (!is.numeric(iota) || length(iota) != 1 || iota <= 0) {
          stop("iota must be a numeric scalar greater than zero.")
     }

     if (!is.numeric(gamma_1) || gamma_1 < 0) {
          stop("gamma_1 must be a numeric scalar greater than or equal to zero.")
     }
     if (!is.numeric(gamma_2) || gamma_2 < 0) {
          stop("gamma_2 must be a numeric scalar greater than or equal to zero.")
     }
     # Check that the recovery rate for severe infection is slower than for mild infection.
     if (gamma_1 >= gamma_2) {
          stop("gamma_1 must be less than gamma_2, as the recovery rate for severe infection should be slower than for mild infection.")
     }

     if (!is.numeric(epsilon) || epsilon < 0) {
          stop("epsilon must be a numeric scalar greater than or equal to zero.")
     }

     # Ensure mu_jt follows required structure (n_locations x time_steps) and values are in [0,1]
     if (!is.matrix(mu_jt) || nrow(mu_jt) != length(location_id) || ncol(mu_jt) != length(seq.Date(as.Date(date_start), as.Date(date_stop), by = "day"))) {
          stop("mu_jt must be a numeric matrix with rows equal to length(location_id) and columns equal to the daily sequence from date_start to date_stop.")
     }
     if (any(mu_jt < 0 | mu_jt > 1)) {
          stop("All values in mu_jt must be between 0 and 1.")
     }

     # Observation Processes validation.
     if (!is.numeric(rho) || rho < 0 || rho > 1) {
          stop("rho must be a numeric scalar between 0 and 1.")
     }

     if (!is.numeric(sigma) || sigma < 0 || sigma > 1) {
          stop("sigma must be a numeric scalar between 0 and 1.")
     }

     # Force of Infection (human-to-human) validation.
     if (!is.numeric(beta_j0_hum) || any(beta_j0_hum < 0) || length(beta_j0_hum) != length(location_id)) {
          stop("beta_j0_hum must be a numeric vector of length equal to location_id and values greater than or equal to zero.")
     }

     if (!is.matrix(beta_j_seasonality) ||
         nrow(beta_j_seasonality) != length(location_id) ||
         ncol(beta_j_seasonality) != 366) {
          stop("beta_j_seasonality must be a matrix with rows equal to length(location_id) and 366 columns for annual seasonality.")
     }

     # Ensure beta_j_seasonality values conform to affine normalization
     for (i in 1:nrow(beta_j_seasonality)) {

          tryCatch({
               MOSAIC::check_affine_normalization(beta_j_seasonality[i, ], verbose=FALSE)
          }, error = function(e) {
               stop(sprintf("beta_j_seasonality for location '%s' failed affine normalization check: %s", location_name[i], e$message))
          })

     }

     if (!is.numeric(tau_i) || any(tau_i < 0 | tau_i > 1) || length(tau_i) != length(location_id)) {
          stop("tau_i must be a numeric vector of length equal to location_id and values between 0 and 1.")
     }

     if (!is.matrix(pi_ij) || nrow(pi_ij) != length(location_id) || ncol(pi_ij) != length(location_id)) {
          stop("pi_ij must be a matrix with dimensions equal to length(location_id) x length(location_id).")
     }

     if (!is.numeric(alpha_1) || alpha_1 < 0 || alpha_1 > 1) {
          stop("alpha_1 must be a numeric scalar between 0 and 1.")
     }
     if (!is.numeric(alpha_2) || alpha_2 < 0 || alpha_2 > 1) {
          stop("alpha_2 must be a numeric scalar between 0 and 1.")
     }

     # Force of Infection (environment-to-human) validation.
     if (!is.numeric(beta_j0_env) || any(beta_j0_env < 0) || length(beta_j0_env) != length(location_id)) {
          stop("beta_j0_env must be a numeric vector of length equal to location_id and values greater than or equal to zero.")
     }

     if (!is.numeric(theta_j) || any(theta_j < 0 | theta_j > 1) || length(theta_j) != length(location_id)) {
          stop("theta_j must be a numeric vector of length equal to location_id and values between 0 and 1.")
     }

     if (!is.matrix(psi_jt) || nrow(psi_jt) != length(location_id) || ncol(psi_jt) != length(t)) {
          stop("psi_jt must be a matrix with rows equal to location_id and columns equal to the daily sequence from date_start to date_stop.")
     }

     if (!is.numeric(zeta_1) || zeta_1 <= 0) {
          stop("zeta_1 must be a numeric scalar greater than zero.")
     }
     if (!is.numeric(zeta_2) || zeta_2 <= 0) {
          stop("zeta_2 must be a numeric scalar greater than zero.")
     }
     # Check that zeta_1 is greater than zeta_2.
     if (zeta_1 <= zeta_2) {
          stop("zeta_1 must be greater than zeta_2.")
     }

     if (!is.numeric(kappa) || kappa <= 0) {
          stop("kappa must be a numeric scalar greater than zero.")
     }

     if (!is.numeric(delta_min) || delta_min <= 0) {
          stop("delta_min must be a numeric scalar greater than zero.")
     }

     if (!is.numeric(delta_max) || delta_max <= 0) {
          stop("delta_max must be a numeric scalar greater than zero.")
     }

     if (delta_min <= delta_max) {
          stop("The decay rate delta_min must be greater than decay rate delta_max.")
     }


     tmp <- split(params$b_jt, row(params$b_jt))
     params$b_jt <- lapply(tmp, as.numeric)

     tmp <- split(params$d_jt, row(params$d_jt))
     params$d_jt <- lapply(tmp, as.numeric)

     tmp <- split(params$nu_1_jt, row(params$nu_1_jt))
     params$nu_1_jt <- lapply(tmp, as.integer)

     tmp <- split(params$nu_2_jt, row(params$nu_2_jt))
     params$nu_2_jt <- lapply(tmp, as.integer)

     tmp <- split(params$beta_j_seasonality, row(params$beta_j_seasonality))
     params$beta_j_seasonality <- lapply(tmp, as.numeric)

     tmp <- split(params$pi_ij, row(params$pi_ij))
     params$pi_ij <- lapply(tmp, as.numeric)

     tmp <- split(params$psi_jt, row(params$psi_jt))
     params$psi_jt <- lapply(tmp, as.numeric)

     tmp <- split(mu_jt, row(mu_jt))
     params$mu_jt <- lapply(tmp, as.numeric)


     if (!is.null(output_file_path)) {

          if (grepl("\\.json(\\.gz)?$", output_file_path, ignore.case = TRUE)) {

               MOSAIC::write_list_to_json(params, output_file_path, compress = grepl("\\.gz$", output_file_path))

          } else if (grepl("\\.(h5|hdf5)(\\.gz)?$", output_file_path, ignore.case = TRUE)) {

               MOSAIC::write_list_to_hdf5(params, output_file_path, compress_chunks = TRUE, compress_file = grepl("\\.gz$", output_file_path))

          } else if (grepl("\\.yaml(\\.gz)?$", output_file_path, ignore.case = TRUE)) {

               MOSAIC::write_list_to_yaml(params, output_file_path, compress = grepl("\\.gz$", output_file_path))

          } else if (grepl("\\.obj(\\.gz)?$", output_file_path, ignore.case = TRUE)) {

               MOSAIC::write_list_to_obj(params, output_file_path, compress = grepl("\\.gz$", output_file_path))

          } else {

               stop("Unsupported file format. The output file must have a .json, .json.gz, .h5, .hdf5, .h5.gz, .yaml, .yaml.gz, .obj, or .obj.gz extension.")

          }

     } else {

          return(params)

     }

}
