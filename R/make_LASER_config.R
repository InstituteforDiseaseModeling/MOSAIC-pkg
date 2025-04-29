#' Create a Configuration File for LASER
#'
#' This function generates a JSON/HDF5/YAML configuration file to be used as LASER model simulation parameters.
#' It validates all input parameters and, if an output file path is provided, writes the parameters to a file.
#' The file extension determines which output format is used:
#' - .json or .json.gz → written with write_list_to_json,
#' - .h5, or .h5.gz → written with write_list_to_hdf5,
#' - .yaml or .yaml.gz → written with write_list_to_yaml.
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
#' @param location_name A character vector giving the names of each metapopulation location.
#'        The order and names here must match those used in the initial population vectors.
#' @param N_j_initial A named numeric or integer vector of length equal to \code{location_name} giving the total
#' initial population size for each location. Note that total population size must be the sum of all model compartments.
#' @param S_j_initial A named numeric or integer vector of length equal to location_name giving the starting number
#'        of susceptible individuals for each location. Names must match location_name.
#' @param E_j_initial A named numeric or integer vector of length equal to location_name giving the starting number
#'        of exposed individuals for each location. Names must match location_name.
#' @param I_j_initial A named numeric or integer vector of length equal to location_name giving the starting number
#'        of infected individuals for each location. Names must match location_name.
#' @param R_j_initial A named numeric or integer vector of length equal to location_name giving the starting number
#'        of recovered individuals for each location. Names must match location_name.
#' @param V1_j_initial A named numeric or integer vector of length equal to location_name giving the starting number
#'        of individuals in vaccine compartment V1 for each location. Names must match location_name.
#' @param V2_j_initial A named numeric or integer vector of length equal to location_name giving the starting number
#'        of individuals in vaccine compartment V2 for each location. Names must match location_name.
#'
#' ## Demographics
#' @param b_jt A matrix of birth rates with rows equal to length(location_name) and columns equal to the daily
#'        sequence from date_start to date_stop.
#' @param d_jt A matrix of mortality rates with rows equal to length(location_name) and columns equal to the daily
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
#' @param mu_jt A matrix of time-varying probabilities of mortality due to infection, with rows equal to
#'        length(location_name) and columns equal to length(t). All values must be numeric and between 0 and 1.
#'
#' ## Observation Processes
#' @param rho Proportion of true infections (numeric in [0, 1]).
#' @param sigma Proportion of symptomatic infections (numeric in [0, 1]).
#'
#' ## Spatial model
#' @param longitude A numeric vector of longitudes for each location. Must be same length as location_name.
#' @param latitude A numeric vector of latitudes for each location. Must be same length as location_name.
#' @param mobility_omega Exponent weight for destination population in the gravity mobility model. Must be numeric ≥ 0.
#' @param mobility_gamma Exponent weight for distance decay in the gravity mobility model. Must be numeric ≥ 0.
#' @param tau_i Departure probability for each origin location (numeric vector of length(location_name) in [0, 1]).
#'
#' ## Force of Infection (human-to-human)
#' @param beta_j0_hum Baseline human-to-human transmission rate (numeric vector of length(location_name)).
#' @param a_1_j Vector of sine amplitude coefficients (1st harmonic) for each location. Numeric, length = length(location_name).
#' @param a_2_j Vector of sine amplitude coefficients (2nd harmonic) for each location. Numeric, length = length(location_name).
#' @param b_1_j Vector of cosine amplitude coefficients (1st harmonic) for each location. Numeric, length = length(location_name).
#' @param b_2_j Vector of cosine amplitude coefficients (2nd harmonic) for each location. Numeric, length = length(location_name).
#' @param p Period of the seasonal forcing function. Scalar numeric > 0. Default is 365 for daily annual seasonality.
#' @param alpha_1 Transmission parameter for mixing (numeric in [0, 1]).
#' @param alpha_2 Transmission parameter for density dependence (numeric in [0, 1]).
#'
#' ## Force of Infection (environment-to-human)
#' @param beta_j0_env Baseline environment-to-human transmission rate (numeric vector of length(location_name)).
#' @param theta_j Proportion with adequate WASH (numeric vector of length(location_name) in [0, 1]).
#' @param psi_jt Matrix of environmental suitability values (matrix with rows = length(location_name) and columns
#'        equal to the daily sequence from date_start to date_stop).
#' @param zeta_1 Shedding rate (numeric > 0).
#' @param zeta_2 Shedding rate (numeric > 0; must be less than zeta_1).
#' @param kappa Concentration required for 50% infection (numeric > 0).
#' @param decay_days_short Time constant (in days) for short-term survival of *V. cholerae* in the environment.
#'        Must be > 0 and < decay_days_long.
#' @param decay_days_long Time constant (in days) for long-term survival of *V. cholerae* in the environment.
#'        Must be > 0 and > decay_days_short.
#' @param decay_shape_1 First shape parameter for beta distribution controlling how environmental suitability maps to
#'        the decay rate of *V. cholerae* in the environment. Must be numeric > 0.
#' @param decay_shape_2 Second shape parameter for beta distribution controlling how environmental suitability maps to
#'        the decay rate of *V. cholerae* in the environment. Must be numeric > 0.
#'
#' ## Reported data
#' @param reported_cases Matrix of daily reported cholera cases. Must be integer. NA allowed.
#'        nrow=length(location_name), ncol=length(t).
#' @param reported_deaths Matrix of daily reported cholera deaths. Must be integer. NA allowed.
#'        nrow=length(location_name), ncol=length(t).
#'
#' ## Outputs
#' @param return A character vector of model quantities to return after running the LASER model. Each element should
#'        be one of the following:
#'        \describe{
#'          \item{"LL"}{Log-likelihood of the fitted model.}
#'          \item{"N"}{Total population size.}
#'          \item{"S"}{Number of susceptible individuals.}
#'          \item{"E"}{Number of exposed (latent) individuals.}
#'          \item{"I"}{Number of infectious individuals.}
#'          \item{"R"}{Number of recovered individuals.}
#'          \item{"V1"}{Number of individuals with one dose of vaccine. Includes all V1 subcompartments.}
#'          \item{"V2"}{Number of individuals with two doses of vaccine. Includes all V2 subcompartments.}
#'          \item{"W"}{Environmental concentration of *V. cholerae*.}
#'          \item{"C"}{Estimated number of symptomatic cholera cases.}
#'          \item{"D"}{Estimated number of cholera deaths.}
#'        }
#'
#' @param sigfigs Integer; number of significant figures to round all numeric values to. Default is 4.
#'
#' @return Returns the validated list of parameters. If output_file_path is provided, the parameters are written to a file
#'         in the format determined by the file extension.
#'
#' @examples
#' \dontrun{
#' make_LASER_config(
#'      output_file_path = "parameters.json",
#'      seed = 123,
#'      date_start = "2024-12-01",
#'      date_stop = "2024-12-31",
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
#'      mu_jt = matrix(0.01, nrow = 2, ncol = 31),
#'      rho = 0.9,
#'      sigma = 0.5,
#'      beta_j0_hum = c(0.05, 0.03),
#'      a_1_j = c(0.02, 0.02),
#'      a_2_j = c(0.01, 0.01),
#'      b_1_j = c(0.03, 0.03),
#'      b_2_j = c(0.01, 0.01),
#'      p     = 365,
#'      longitude = c(36.8, 37.0),
#'      latitude = c(-1.3, -1.2),
#'      mobility_omega = 1.0,
#'      mobility_gamma = 2.0,
#'      tau_i = c(0.1, 0.2),
#'      alpha_1 = 0.95,
#'      alpha_2 = 1,
#'      beta_j0_env = c(0.02, 0.04),
#'      theta_j = c(0.6, 0.7),
#'      psi_jt = matrix(data = 0, nrow = 2, ncol = 366),
#'      zeta_1 = 0.5,
#'      zeta_2 = 0.4,
#'      kappa = 10^5,
#'      decay_days_short  = 3,
#'      decay_days_long   = 90,
#'      decay_shape_1     = 1,
#'      decay_shape_2     = 1,
#'      reported_cases    = matrix(NA, nrow=2, ncol=366),
#'      reported_deaths   = matrix(NA, nrow=2, ncol=366),
#'      return = "LL"
#' )
#' }
#'
#' @export
#'

make_LASER_config <- function(output_file_path = NULL,
                              seed = NULL,

                              # Initialization
                              date_start = NULL,
                              date_stop = NULL,
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

                              # Spatial model
                              longitude = NULL,
                              latitude = NULL,
                              mobility_omega = NULL,
                              mobility_gamma = NULL,
                              tau_i = NULL,

                              # Force of Infection (human-to-human)
                              beta_j0_hum = NULL,
                              a_1_j = NULL,
                              a_2_j = NULL,
                              b_1_j = NULL,
                              b_2_j = NULL,
                              p = 365L,
                              alpha_1 = NULL,
                              alpha_2 = NULL,

                              # Force of Infection (environment-to-human)
                              beta_j0_env = NULL,
                              theta_j = NULL,
                              psi_jt = NULL,
                              zeta_1 = NULL,
                              zeta_2 = NULL,
                              kappa = NULL,
                              decay_days_short = NULL,
                              decay_days_long = NULL,
                              decay_shape_1 = NULL,
                              decay_shape_2 = NULL,

                              ## Reported data
                              reported_cases = NULL,
                              reported_deaths = NULL,

                              # Outputs
                              sigfigs = 4,
                              return = NULL
) {

     message('Validating parameter values...')

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
          longitude         = longitude,
          latitude          = latitude,
          mobility_omega    = mobility_omega,
          mobility_gamma    = mobility_gamma,
          tau_i             = tau_i,
          beta_j0_hum       = beta_j0_hum,
          a_1_j             = a_1_j,
          a_2_j             = a_2_j,
          b_1_j             = b_1_j,
          b_2_j             = b_2_j,
          p                 = p,
          alpha_1           = alpha_1,
          alpha_2           = alpha_2,
          beta_j0_env       = beta_j0_env,
          theta_j           = theta_j,
          psi_jt            = psi_jt,
          zeta_1            = zeta_1,
          zeta_2            = zeta_2,
          kappa             = kappa,
          decay_days_short  = decay_days_short,
          decay_days_long   = decay_days_long,
          decay_shape_1     = decay_shape_1,
          decay_shape_2     = decay_shape_2,
          reported_cases    = reported_cases,
          reported_deaths   = reported_deaths,
          return            = return
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

     t <- seq.Date(as.Date(date_start), as.Date(date_stop), by = "day")

     if (!is.character(location_name)) {
          stop("location_name must be a character vector.")
     }


     # Validate initial population vectors.
     for (v in c("N_j_initial", "S_j_initial", "E_j_initial",
                 "I_j_initial", "R_j_initial", "V1_j_initial", "V2_j_initial")) {

          vec <- params[[v]]

          # Allow numeric or integer, but require all entries are whole numbers ≥ 0
          if (!is.numeric(vec) || any(vec < 0) || any(vec != floor(vec))) {
               stop(v, " must be numeric and integer-valued (no fractions).", call. = FALSE)
          }

          # Must match number of locations
          if (length(vec) != length(location_name)) {
               stop(v, " must be a vector of length equal to number of locations.", call. = FALSE)
          }

          # If named, names must match location_name
          if (!is.null(names(vec)) && !all(names(vec) == location_name)) {
               stop(v, " names must match the provided location_name values.", call. = FALSE)
          }

          # finally coerce to integer
          params[[v]] <- as.integer(vec)
     }


     # Check compartments sum to N_j_initial (allow small tolerance)
     total_j <- params$S_j_initial +
          params$E_j_initial +
          params$I_j_initial +
          params$R_j_initial +
          params$V1_j_initial +
          params$V2_j_initial

     mismatch_idx <- which(abs(total_j - params$N_j_initial) > .Machine$double.eps^0.5)
     if (length(mismatch_idx)) {
          stop(
               "For location(s): ", paste(params$location_name[mismatch_idx], collapse = ", "),
               " the sum of S,E,I,R,V1,V2 does not match N_j_initial."
          )
     }

     # Demographics validation.
     if (!is.matrix(b_jt) || nrow(b_jt) != length(location_name) || ncol(b_jt) != length(t)) {
          stop("b_jt must be a matrix with rows equal to length(location_name) and columns equal to the daily sequence from date_start to date_stop.")
     }

     if (!is.matrix(d_jt) || nrow(d_jt) != length(location_name) || ncol(d_jt) != length(t)) {
          stop("d_jt must be a matrix with rows equal to length(location_name) and columns equal to the daily sequence from date_start to date_stop.")
     }

     ## Vaccination validation.
     if (!is.matrix(nu_1_jt) || nrow(nu_1_jt) != length(location_name) || ncol(nu_1_jt) != length(t)) {
          stop("nu_1_jt must be a matrix with rows equal to length(location_name) and columns equal to the daily sequence from date_start to date_stop.")
     }
     if (!is.matrix(nu_2_jt) || nrow(nu_2_jt) != length(location_name) || ncol(nu_2_jt) != length(t)) {
          stop("nu_2_jt must be a matrix with rows equal to length(location_name) and columns equal to the daily sequence from date_start to date_stop.")
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
     # Check that the recovery rate for severe infection is faster than for mild infection.
     if (gamma_1 <= gamma_2) {
          stop("gamma_1 must be greater than gamma_2, as the recovery rate for severe infection should be faster than for mild infection.")
     }

     if (!is.numeric(epsilon) || epsilon < 0) {
          stop("epsilon must be a numeric scalar greater than or equal to zero.")
     }

     # Ensure mu_jt follows required structure (n_locations x time_steps) and values are in [0,1].
     if (!is.matrix(mu_jt) || nrow(mu_jt) != length(location_name) ||
         ncol(mu_jt) != length(seq.Date(as.Date(date_start), as.Date(date_stop), by = "day"))) {
          stop("mu_jt must be a numeric matrix with rows equal to length(location_name) and columns equal to the daily sequence from date_start to date_stop.")
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

     # Force of Infection (human-to-human).
     if (!is.numeric(beta_j0_hum) || any(beta_j0_hum < 0) || length(beta_j0_hum) != length(location_name)) {
          stop("beta_j0_hum must be a numeric vector of length equal to location_name and values greater than or equal to zero.")
     }

     if (!is.numeric(a_1_j) || length(a_1_j) != length(location_name)) {
          stop("a_1_j must be a numeric vector of length equal to location_name.")
     }
     if (!is.numeric(a_2_j) || length(a_2_j) != length(location_name)) {
          stop("a_2_j must be a numeric vector of length equal to location_name.")
     }
     if (!is.numeric(b_1_j) || length(b_1_j) != length(location_name)) {
          stop("b_1_j must be a numeric vector of length equal to location_name.")
     }
     if (!is.numeric(b_2_j) || length(b_2_j) != length(location_name)) {
          stop("b_2_j must be a numeric vector of length equal to location_name.")
     }
     if (!is.numeric(p) || length(p) != 1 || p <= 0) {
          stop("p must be a numeric scalar greater than zero.")
     }

     # Gravity mobility parameters.
     if (!is.numeric(longitude) || length(longitude) != length(location_name)) {
          stop("longitude must be a numeric vector of length equal to location_name.")
     }
     if (!is.numeric(latitude) || length(latitude) != length(location_name)) {
          stop("latitude must be a numeric vector of length equal to location_name.")
     }
     if (!is.numeric(mobility_omega) || length(mobility_omega) != 1 || mobility_omega < 0) {
          stop("mobility_omega must be a numeric scalar greater than or equal to zero.")
     }
     if (!is.numeric(mobility_gamma) || length(mobility_gamma) != 1 || mobility_gamma < 0) {
          stop("mobility_gamma must be a numeric scalar greater than or equal to zero.")
     }
     if (!is.numeric(tau_i) || any(tau_i < 0 | tau_i > 1) || length(tau_i) != length(location_name)) {
          stop("tau_i must be a numeric vector of length equal to location_name and values between 0 and 1.")
     }
     if (!is.numeric(alpha_1) || alpha_1 < 0 || alpha_1 > 1) {
          stop("alpha_1 must be a numeric scalar between 0 and 1.")
     }
     if (!is.numeric(alpha_2) || alpha_2 < 0 || alpha_2 > 1) {
          stop("alpha_2 must be a numeric scalar between 0 and 1.")
     }

     # Force of Infection (environment-to-human).
     if (!is.numeric(beta_j0_env) || any(beta_j0_env < 0) || length(beta_j0_env) != length(location_name)) {
          stop("beta_j0_env must be a numeric vector of length equal to location_name and values greater than or equal to zero.")
     }
     if (!is.numeric(theta_j) || any(theta_j < 0 | theta_j > 1) || length(theta_j) != length(location_name)) {
          stop("theta_j must be a numeric vector of length equal to location_name and values between 0 and 1.")
     }
     if (!is.matrix(psi_jt) || nrow(psi_jt) != length(location_name) || ncol(psi_jt) != length(t)) {
          stop("psi_jt must be a matrix with rows equal to location_name and columns equal to the daily sequence from date_start to date_stop.")
     }
     if (!is.numeric(zeta_1) || zeta_1 <= 0) {
          stop("zeta_1 must be a numeric scalar greater than zero.")
     }
     if (!is.numeric(zeta_2) || zeta_2 <= 0) {
          stop("zeta_2 must be a numeric scalar greater than zero.")
     }
     if (zeta_1 <= zeta_2) {
          stop("zeta_1 must be greater than zeta_2.")
     }
     if (!is.numeric(kappa) || kappa <= 0) {
          stop("kappa must be a numeric scalar greater than zero.")
     }

     # Environmental decay parameter validation.
     if (!is.numeric(decay_days_short) || decay_days_short <= 0) {
          stop("decay_days_short must be a numeric scalar greater than zero.")
     }
     if (!is.numeric(decay_days_long) || decay_days_long <= 0) {
          stop("decay_days_long must be a numeric scalar greater than zero.")
     }
     if (decay_days_short >= decay_days_long) {
          stop("decay_days_short must be less than decay_days_long.")
     }
     if (!is.numeric(decay_shape_1) || decay_shape_1 <= 0) {
          stop("decay_shape_1 must be a numeric scalar greater than zero.")
     }
     if (!is.numeric(decay_shape_2) || decay_shape_2 <= 0) {
          stop("decay_shape_2 must be a numeric scalar greater than zero.")
     }

     ## Reported data checks
     # If reported_cases or reported_deaths exist, they must be integer matrices with dimensions matching (locations x t).
     if (!is.matrix(reported_cases) || nrow(reported_cases) != length(location_name) || ncol(reported_cases) != length(t)) {
          stop("reported_cases must be a matrix with rows = length(location_name) and columns = length(t).")
     }
     not_na_rc <- !is.na(reported_cases)
     if (any(reported_cases[not_na_rc] != floor(reported_cases[not_na_rc]))) {
          stop("reported_cases must be integer (NA allowed).")
     }

     if (!is.matrix(reported_deaths) || nrow(reported_deaths) != length(location_name) || ncol(reported_deaths) != length(t)) {
          stop("reported_deaths must be a matrix with rows = length(location_name) and columns = length(t).")
     }
     not_na_rd <- !is.na(reported_deaths)
     if (any(reported_deaths[not_na_rd] != floor(reported_deaths[not_na_rd]))) {
          stop("reported_deaths must be integer (NA allowed).")
     }

     # Validate return vector.
     valid_return_keys <- c("LL", "N", "S", "E", "I", "R", "V1", "V2", "W", "C", "D")
     if (!is.character(return) || length(return) == 0) {
          stop("return must be a non-empty character vector.")
     }
     invalid <- setdiff(return, valid_return_keys)
     if (length(invalid) > 0) {
          stop("Invalid entries in return: ", paste(invalid, collapse = ", "),
               ". Allowed values are: ", paste(valid_return_keys, collapse = ", "))
     }

     message("All parameters have passed config checks.")

     # Convert date objects to character in the final param list
     params$date_start <- as.character(date_start)
     params$date_stop  <- as.character(date_stop)

     # Remove dimnames from all list objects
     message("Cleaning parameter list for output...")
     for (nm in names(params)) {

          val <- params[[nm]]

          if (is.matrix(val) || length(dim(val)) > 1) {

               dimnames(val) <- NULL
               params[[nm]] <- val

          } else if (is.vector(val) && !is.list(val)) {

               names(val) <- NULL
               params[[nm]] <- val

          }
     }


     if (!is.null(output_file_path)) {
          if (grepl("\\.json(\\.gz)?$", output_file_path, ignore.case = TRUE)) {
               MOSAIC::write_list_to_json(params, output_file_path, compress = grepl("\\.gz$", output_file_path))
          } else if (grepl("\\.(h5|hdf5)(\\.gz)?$", output_file_path, ignore.case = TRUE)) {
               MOSAIC::write_list_to_hdf5(params, output_file_path,
                                          compress_chunks = TRUE,
                                          compress_file = grepl("\\.gz$", output_file_path))
          } else if (grepl("\\.yaml(\\.gz)?$", output_file_path, ignore.case = TRUE)) {
               MOSAIC::write_list_to_yaml(params, output_file_path, compress = grepl("\\.gz$", output_file_path))
          } else {
               stop("Unsupported file format. The output file must have a .json, .json.gz, .h5, .hdf5, .h5.gz, .yaml, or .yaml.gz extension.")
          }
     } else {
          message("LASER config returned as list object (no file written as output).")
          return(params)
     }

}
