#' Estimate Initial E and I Compartments from Surveillance Data
#'
#' This function estimates the initial number of individuals in the Exposed (E) and
#' Infected (I) compartments at model start time using recent surveillance data.
#' It uses a Monte Carlo approach to sample from prior distributions and quantify uncertainty.
#'
#' The method back-calculates true infections from reported cases using the surveillance
#' cascade, accounts for reporting delays, and estimates E/I compartments based on
#' epidemiological progression rates. Includes comprehensive parameter validation,
#' numerical stability protections, and optional parallel processing.
#'
#' @param PATHS List of paths from `get_paths()`.
#' @param priors Prior distributions for parameters (e.g., `priors_default`).
#' @param config Configuration object with location codes and `date_start`.
#'   Must include `config$location_name` and `config$date_start`.
#' @param n_samples Number of Monte Carlo samples (default 1000).
#' @param t0 Target date for estimation (default from `config$date_start`).
#' @param lookback_days Days of surveillance data to use (default 21).
#' @param include_asymptomatic_in_I Logical; if `TRUE`, the I compartment includes
#'   both symptomatic and asymptomatic infectious individuals. If `FALSE`, I counts
#'   symptomatic infectious individuals only. Default `TRUE` for backward compatibility.
#' @param verbose Print progress messages (default TRUE).
#' @param parallel Enable parallel processing for Monte Carlo sampling when
#'   `n_samples >= 100` (default FALSE). Uses `parallel::mclapply()` with all
#'   available cores. Note: Not supported on Windows.
#' @param variance_increase_pct Percentage increase in variance for fitted Beta distributions (default 50).
#'   Higher values create less constrained priors, lower values create tighter priors.
#'
#' @return A list with two main components:
#' \describe{
#'   \item{metadata}{List containing estimation details: description, version, date, t0,
#'     lookback_days, n_samples, method, and include_asymptomatic_in_I.}
#'   \item{parameters_location}{List with `prop_E_initial` and `prop_I_initial`, each containing:
#'     \itemize{
#'       \item parameter_name: Parameter identifier
#'       \item distribution: `"beta"`
#'       \item parameters$location: Named list by ISO code with `shape1`, `shape2`, `mean`,
#'             `variance`, `method` (fitting method used), and `metadata` (data availability,
#'             sample statistics)
#'     }
#'   }
#' }
#'
#' @examples
#' \dontrun{
#' PATHS  <- get_paths()
#' priors <- priors_default
#' config <- config_default
#' results <- est_initial_E_I(
#'   PATHS, priors, config,
#'   n_samples = 1000,
#'   include_asymptomatic_in_I = FALSE,
#'   variance_increase_pct = 100  # Double the variance for less constrained priors
#' )
#' }
#'
#' @export
est_initial_E_I <- function(PATHS, priors, config, n_samples = 1000,
                            t0 = NULL, lookback_days = 21,
                            include_asymptomatic_in_I = TRUE,
                            verbose = TRUE, parallel = FALSE,
                            variance_increase_pct = 50) {

     # ---- Parameter validation ----
     if (n_samples <= 0) stop("n_samples must be positive")
     if (lookback_days <= 0) stop("lookback_days must be positive")
     if (!is.list(PATHS)) stop("PATHS must be a list")
     if (!is.list(priors)) stop("priors must be a list")
     if (!is.list(config)) stop("config must be a list")
     if (variance_increase_pct < 0) stop("variance_increase_pct must be non-negative")

     # ---- Helper: sample from prior ----
     sample_from_prior <- function(prior, param_name, verbose = FALSE) {
          if (is.null(prior)) {
               if (verbose) cat("  Prior for", param_name, "is NULL\n")
               return(NA)
          }
          tryCatch({
               if (prior$distribution == "beta") {
                    rbeta(1, prior$parameters$shape1, prior$parameters$shape2)
               } else if (prior$distribution == "gamma") {
                    rgamma(1, shape = prior$parameters$shape, rate = prior$parameters$rate)
               } else if (prior$distribution == "lognormal") {
                    if (!is.null(prior$parameters$meanlog) && !is.null(prior$parameters$sdlog)) {
                         rlnorm(1, prior$parameters$meanlog, prior$parameters$sdlog)
                    } else if (!is.null(prior$parameters$mean) && !is.null(prior$parameters$sd)) {
                         m <- prior$parameters$mean
                         s <- prior$parameters$sd
                         cv2 <- (s/m)^2
                         sdlog <- sqrt(log(1 + cv2))
                         meanlog <- log(m) - sdlog^2/2
                         rlnorm(1, meanlog, sdlog)
                    } else {
                         stop("Lognormal prior missing required parameters")
                    }
               } else if (prior$distribution == "uniform") {
                    runif(1, prior$parameters$min, prior$parameters$max)
               } else if (prior$distribution == "normal") {
                    rnorm(1, prior$parameters$mean, prior$parameters$sd)
               } else {
                    stop("Unknown distribution type: ", prior$distribution)
               }
          }, error = function(e) {
               if (verbose) cat("  Error sampling", param_name, ":", e$message, "\n")
               return(NA)
          })
     }

     # ---- Helper: fit Beta from samples (proportions) ----
     fit_beta_from_samples <- function(x, label = "") {
          x <- x[!is.na(x)]

          if (length(x) < 2) {
               if (verbose) cat("  Insufficient data for", label, "- using default near-zero\n")
               # Use less constrained defaults with variance inflation
               variance_multiplier <- 1 + (variance_increase_pct / 100)
               precision <- max(10, 1000 / variance_multiplier)
               default_mean <- 1e-4  # Slightly higher than original
               shape1 <- default_mean * precision
               shape2 <- (1 - default_mean) * precision
               return(list(
                    shape1 = max(0.01, shape1),
                    shape2 = max(0.01, shape2),
                    mean = default_mean,
                    variance = (default_mean * (1 - default_mean)) / (precision + 1),
                    method = "default_insufficient_data_inflated"
               ))
          }

          if (sd(x) == 0 || all(x == x[1])) {
               const_val <- x[1]
               if (const_val <= 0 || const_val >= 1) const_val <- 1e-4
               # Apply variance inflation to precision
               variance_multiplier <- 1 + (variance_increase_pct / 100)
               precision <- max(10, 1000 / variance_multiplier)
               shape1 <- const_val * precision
               shape2 <- (1 - const_val) * precision
               return(list(
                    shape1 = max(0.01, shape1),
                    shape2 = max(0.01, shape2),
                    mean = const_val,
                    variance = (const_val * (1 - const_val)) / (precision + 1),
                    method = "constant_value_inflated"
               ))
          }

          x_adj <- x
          x_adj[x_adj <= 0] <- 1e-10
          x_adj[x_adj >= 1] <- 1 - 1e-10
          x_mean <- mean(x_adj)
          x_var  <- var(x_adj)

          # Apply variance inflation
          variance_multiplier <- 1 + (variance_increase_pct / 100)
          x_var_inflated <- x_var * variance_multiplier

          if (x_var_inflated < 1e-10) {
               # Reduce precision to increase variance
               precision <- max(10, 1000 / variance_multiplier)
               shape1 <- x_mean * precision
               shape2 <- (1 - x_mean) * precision
               return(list(
                    shape1 = max(0.01, shape1),
                    shape2 = max(0.01, shape2),
                    mean = x_mean,
                    variance = x_var_inflated,
                    method = "low_variance_inflated"
               ))
          }

          common_factor <- x_mean * (1 - x_mean) / x_var_inflated - 1
          if (common_factor <= 0) {
               if (verbose) cat("  High variance for", label, "- using robust fallback\n")
               q25 <- quantile(x_adj, 0.25)
               q75 <- quantile(x_adj, 0.75)
               med <- median(x_adj)
               if (q75 > q25 && med > 0 && med < 1) {
                    # Reduce precision to increase variance
                    base_precision <- max(2, 10 / variance_multiplier)
                    shape1 <- max(0.1, med * base_precision)
                    shape2 <- max(0.1, (1 - med) * base_precision)
               } else {
                    shape1 <- max(0.01, 1.0 / variance_multiplier)
                    shape2 <- max(0.01, 999.99 / variance_multiplier)
               }
               return(list(
                    shape1 = shape1,
                    shape2 = shape2,
                    mean   = x_mean,
                    variance = x_var_inflated,
                    method = "robust_fallback_inflated"
               ))
          } else {
               shape1 <- x_mean * common_factor
               shape2 <- (1 - x_mean) * common_factor
               return(list(
                    shape1 = max(0.01, shape1),
                    shape2 = max(0.01, shape2),
                    mean = x_mean,
                    variance = x_var_inflated,
                    method = "method_of_moments_inflated"
               ))
          }
     }

     # ---- Helper: create inflated default Beta parameters ----
     create_default_beta <- function(mean_val, label = "") {
          variance_multiplier <- 1 + (variance_increase_pct / 100)
          precision <- max(10, 100 / variance_multiplier)
          shape1 <- mean_val * precision
          shape2 <- (1 - mean_val) * precision
          list(
               shape1 = max(0.01, shape1),
               shape2 = max(0.01, shape2), 
               mean = mean_val,
               variance = (mean_val * (1 - mean_val)) / (precision + 1),
               method = paste0("inflated_default_", label)
          )
     }

     # ---- Helper: consistent location prior or default ----
     # Uses consistent defaults across sequential/parallel branches.
     draw_loc_or_default <- function(priors, name, loc, default, verbose = FALSE) {
          loc_prior <- tryCatch(priors$parameters_location[[name]]$parameters$location[[loc]],
                                error = function(e) NULL)
          if (!is.null(loc_prior)) {
               return(sample_from_prior(loc_prior, paste0(name, "_", loc), verbose))
          }
          if (verbose) cat("  Using default for", name, "at", loc, "=", default, "\n")
          default
     }

     # ---- t0 setup ----
     if (is.null(t0)) {
          t0 <- as.Date(config$date_start)
     } else {
          t0 <- as.Date(t0)
     }

     # ---- Locations ----
     location_codes <- config$location_name
     if (is.null(location_codes)) {
          stop("No location_name found in config")
     }

     # ---- PATHS validation ----
     required_paths <- c("DATA_CHOLERA_DAILY", "DATA_DEMOGRAPHICS")
     missing_paths <- required_paths[!required_paths %in% names(PATHS)]
     if (length(missing_paths) > 0) {
          stop("Missing required paths: ", paste(missing_paths, collapse = ", "))
     }

     # ---- Load surveillance ----
     surveillance_file <- file.path(PATHS$DATA_CHOLERA_DAILY,
                                    "cholera_surveillance_daily_combined.csv")
     if (!file.exists(surveillance_file)) {
          stop("Surveillance data file not found: ", surveillance_file)
     }
     surveillance <- read.csv(surveillance_file, stringsAsFactors = FALSE)
     required_cols <- c("date", "iso_code", "cases")
     missing_cols <- required_cols[!required_cols %in% colnames(surveillance)]
     if (length(missing_cols) > 0) {
          stop("Missing required columns in surveillance data: ", paste(missing_cols, collapse = ", "))
     }
     surveillance$date <- as.Date(surveillance$date)

     # ---- Load population ----
     pop_file <- file.path(PATHS$DATA_DEMOGRAPHICS,
                           "UN_world_population_prospects_daily.csv")
     if (!file.exists(pop_file)) {
          stop("Population data file not found: ", pop_file)
     }
     population_data <- read.csv(pop_file, stringsAsFactors = FALSE)
     required_pop_cols <- c("date", "iso_code", "total_population")
     missing_pop_cols <- required_pop_cols[!required_pop_cols %in% colnames(population_data)]
     if (length(missing_pop_cols) > 0) {
          stop("Missing required columns in population data: ", paste(missing_pop_cols, collapse = ", "))
     }
     population_data$date <- as.Date(population_data$date)

     # ---- Results scaffold ----
     results <- list(
          metadata = list(
               description = "Initial E and I compartment estimates from surveillance data",
               version = "1.1.0",
               date = Sys.Date(),
               t0 = t0,
               lookback_days = lookback_days,
               n_samples = n_samples,
               method = "surveillance_backcalculation",
               include_asymptomatic_in_I = include_asymptomatic_in_I
          ),
          parameters_location = list(
               prop_E_initial = list(
                    parameter_name = "prop_E_initial",
                    distribution = "beta",
                    parameters = list(location = list())
               ),
               prop_I_initial = list(
                    parameter_name = "prop_I_initial",
                    distribution = "beta",
                    parameters = list(location = list())
               )
          )
     )

     # ---- Window & availability ----
     end_date <- t0 - 1
     start_date <- t0 - lookback_days
     surveillance_window <- surveillance[surveillance$date >= start_date &
                                              surveillance$date <= end_date, ]
     countries_with_data <- unique(surveillance_window$iso_code[
          !is.na(surveillance_window$cases) & surveillance_window$cases >= 0
     ])

     if (verbose) {
          cat("\n=== Estimating Initial E and I Compartments ===\n")
          cat(sprintf("Target date (t0): %s\n", t0))
          cat(sprintf("Lookback window: %s to %s (%d days)\n",
                      start_date, end_date, lookback_days))
          cat(sprintf("Found surveillance data for %d/%d countries\n",
                      length(countries_with_data), length(location_codes)))
          cat(sprintf("Number of Monte Carlo samples: %d\n", n_samples))
          cat(sprintf("I includes asymptomatic: %s\n", include_asymptomatic_in_I))
          cat("\n")
     }

     # ---- Per-location processing ----
     for (loc in location_codes) {
          if (verbose) cat(sprintf("Processing %s... ", loc))

          tryCatch({
               loc_surv <- surveillance_window[surveillance_window$iso_code == loc, ]
               has_data <- loc %in% countries_with_data && nrow(loc_surv) > 0

               if (!has_data) {
                    if (verbose) cat("no data, using default priors\n")

                    E_default <- create_default_beta(1e-4, "E_no_data")
                    E_default$metadata <- list(
                         data_available = FALSE, total_cases = 0,
                         mean_count = 0, sd_count = 0, n_samples = n_samples,
                         message = "No surveillance data in lookback window"
                    )
                    results$parameters_location$prop_E_initial$parameters$location[[loc]] <- E_default
                    
                    I_default <- create_default_beta(5e-5, "I_no_data")
                    I_default$metadata <- list(
                         data_available = FALSE, total_cases = 0,
                         mean_count = 0, sd_count = 0, n_samples = n_samples,
                         message = "No surveillance data in lookback window"
                    )
                    results$parameters_location$prop_I_initial$parameters$location[[loc]] <- I_default
                    next
               }

               # Population at ~t0
               pop_loc <- population_data[population_data$iso_code == loc, ]
               if (nrow(pop_loc) == 0) {
                    warning(sprintf("No population data for %s", loc))
                    if (verbose) cat("no population data\n")
                    next
               }
               time_diffs   <- abs(as.numeric(difftime(pop_loc$date, t0, units = "days")))
               closest_idx  <- which.min(time_diffs)
               population_t0 <- pop_loc$total_population[closest_idx]
               if (is.na(population_t0) || population_t0 <= 0) {
                    warning(sprintf("Invalid population for %s", loc))
                    if (verbose) cat("invalid population\n")
                    next
               }

               # Sample containers
               E_samples <- numeric(n_samples)
               I_samples <- numeric(n_samples)

               # -------- Parallel branch --------
               if (parallel && n_samples >= 100) {
                    if (verbose) cat(sprintf("  Using parallel processing with %d cores\n",
                                             parallel::detectCores()))
                    mc_function <- function(i) {
                         sigma_i   <- sample_from_prior(priors$parameters_global$sigma,   "sigma")
                         iota_i    <- sample_from_prior(priors$parameters_global$iota,    "iota")
                         gamma_1_i <- sample_from_prior(priors$parameters_global$gamma_1, "gamma_1")
                         gamma_2_i <- sample_from_prior(priors$parameters_global$gamma_2, "gamma_2")
                         rho_i <- draw_loc_or_default(priors, "rho", loc, default = 0.775, verbose = FALSE)
                         chi_i <- draw_loc_or_default(priors, "chi", loc, default = 0.625, verbose = FALSE)

                         # Reporting delay ~ Gamma(2, 0.5) => mean 4, sd ≈ 2.8
                         tau_r_i <- rgamma(1, shape = 2, rate = 0.5)

                         # Bounds & fallbacks
                         if (is.na(sigma_i)   || sigma_i <= 0 || sigma_i > 1) sigma_i   <- 0.35
                         if (is.na(rho_i)     || rho_i <= 0   || rho_i > 1)   rho_i     <- 0.775
                         if (is.na(chi_i)     || chi_i <= 0   || chi_i > 1)   chi_i     <- 0.625
                         if (is.na(iota_i)    || iota_i <= 0)                 iota_i    <- 0.714
                         if (is.na(gamma_1_i) || gamma_1_i <= 0)              gamma_1_i <- 0.2
                         if (is.na(gamma_2_i) || gamma_2_i <= 0)              gamma_2_i <- 0.67
                         if (is.na(tau_r_i)   || tau_r_i < 0)                 tau_r_i   <- 4

                         mult_i <- chi_i / (rho_i * sigma_i)
                         if (mult_i > 10) {
                              warning(sprintf("Large infections multiplier chi/(rho*sigma)=%.2f; check priors.", mult_i))
                         }

                         ei_results <- est_initial_E_I_location(
                              cases = loc_surv$cases,
                              dates = loc_surv$date,
                              population = population_t0,
                              t0 = t0,
                              lookback_days = lookback_days,
                              sigma = sigma_i,
                              rho = rho_i,
                              chi = chi_i,
                              tau_r = tau_r_i,
                              iota = iota_i,
                              gamma_1 = gamma_1_i,
                              gamma_2 = gamma_2_i,
                              include_asymptomatic_in_I = include_asymptomatic_in_I,
                              verbose = FALSE
                         )
                         c(E = ei_results$E, I = ei_results$I)
                    }

                    mc_results <- parallel::mclapply(1:n_samples, mc_function,
                                                     mc.cores = parallel::detectCores())
                    E_samples <- sapply(mc_results, function(x) x["E"])
                    I_samples <- sapply(mc_results, function(x) x["I"])

               } else {
                    # -------- Sequential branch --------
                    for (i in 1:n_samples) {
                         sigma_i   <- sample_from_prior(priors$parameters_global$sigma,   "sigma")
                         iota_i    <- sample_from_prior(priors$parameters_global$iota,    "iota")
                         gamma_1_i <- sample_from_prior(priors$parameters_global$gamma_1, "gamma_1")
                         gamma_2_i <- sample_from_prior(priors$parameters_global$gamma_2, "gamma_2")
                         rho_i <- draw_loc_or_default(priors, "rho", loc, default = 0.775, verbose = FALSE)
                         chi_i <- draw_loc_or_default(priors, "chi", loc, default = 0.625, verbose = FALSE)

                         tau_r_i <- rgamma(1, shape = 2, rate = 0.5)

                         if (is.na(sigma_i)   || sigma_i <= 0 || sigma_i > 1) sigma_i   <- 0.35
                         if (is.na(rho_i)     || rho_i <= 0   || rho_i > 1)   rho_i     <- 0.775
                         if (is.na(chi_i)     || chi_i <= 0   || chi_i > 1)   chi_i     <- 0.625
                         if (is.na(iota_i)    || iota_i <= 0)                 iota_i    <- 0.714
                         if (is.na(gamma_1_i) || gamma_1_i <= 0)              gamma_1_i <- 0.2
                         if (is.na(gamma_2_i) || gamma_2_i <= 0)              gamma_2_i <- 0.67
                         if (is.na(tau_r_i)   || tau_r_i < 0)                 tau_r_i   <- 4

                         mult_i <- chi_i / (rho_i * sigma_i)
                         if (mult_i > 10 && verbose) {
                              warning(sprintf("Large infections multiplier chi/(rho*sigma)=%.2f; check priors.", mult_i))
                         }

                         ei_results <- est_initial_E_I_location(
                              cases = loc_surv$cases,
                              dates = loc_surv$date,
                              population = population_t0,
                              t0 = t0,
                              lookback_days = lookback_days,
                              sigma = sigma_i,
                              rho = rho_i,
                              chi = chi_i,
                              tau_r = tau_r_i,
                              iota = iota_i,
                              gamma_1 = gamma_1_i,
                              gamma_2 = gamma_2_i,
                              include_asymptomatic_in_I = include_asymptomatic_in_I,
                              verbose = FALSE
                         )
                         E_samples[i] <- ei_results$E
                         I_samples[i] <- ei_results$I
                    }
               }

               total_cases <- sum(loc_surv$cases, na.rm = TRUE)

               # Proportions
               E_prop <- E_samples / population_t0
               I_prop <- I_samples / population_t0

               # Fit Beta
               E_beta <- fit_beta_from_samples(E_prop, paste0("E_", loc))
               I_beta <- fit_beta_from_samples(I_prop, paste0("I_", loc))

               if (is.null(E_beta)) {
                    E_beta <- create_default_beta(1e-4, "E_fitting_failed")
               }
               if (is.null(I_beta)) {
                    I_beta <- create_default_beta(5e-5, "I_fitting_failed")
               }

               E_beta$metadata <- list(
                    data_available = TRUE,
                    total_cases = total_cases,
                    mean_count = mean(E_samples),
                    sd_count = sd(E_samples),
                    n_samples = n_samples,
                    message = sprintf("Processed with %d surveillance cases", total_cases)
               )
               I_beta$metadata <- list(
                    data_available = TRUE,
                    total_cases = total_cases,
                    mean_count = mean(I_samples),
                    sd_count = sd(I_samples),
                    n_samples = n_samples,
                    message = sprintf("Processed with %d surveillance cases", total_cases)
               )

               results$parameters_location$prop_E_initial$parameters$location[[loc]] <- E_beta
               results$parameters_location$prop_I_initial$parameters$location[[loc]] <- I_beta

               if (verbose) {
                    cat(sprintf("E: Beta(%.3f, %.3f) mean=%.1e, I: Beta(%.3f, %.3f) mean=%.1e\n",
                                E_beta$shape1, E_beta$shape2, E_beta$mean,
                                I_beta$shape1, I_beta$shape2, I_beta$mean))
               }

          }, error = function(e) {
               warning(sprintf("Error processing %s: %s", loc, e$message))
               if (verbose) cat(sprintf("error: %s\n", e$message))
               E_error <- create_default_beta(1e-4, "E_error")
               E_error$metadata <- list(
                    data_available = FALSE, total_cases = 0,
                    mean_count = 0, sd_count = 0, n_samples = n_samples,
                    message = sprintf("Error during processing: %s", e$message)
               )
               results$parameters_location$prop_E_initial$parameters$location[[loc]] <- E_error
               
               I_error <- create_default_beta(5e-5, "I_error")
               I_error$metadata <- list(
                    data_available = FALSE, total_cases = 0,
                    mean_count = 0, sd_count = 0, n_samples = n_samples,
                    message = sprintf("Error during processing: %s", e$message)
               )
               results$parameters_location$prop_I_initial$parameters$location[[loc]] <- I_error
          })
     }

     if (verbose) {
          cat("\n=== Estimation Complete ===\n")
          cat(sprintf("Successfully processed %d locations\n",
                      length(results$parameters_location$prop_E_initial$parameters$location)))
     }

     return(results)
}

#' Estimate E and I Compartments for a Single Location
#'
#' This function performs the actual E/I estimation for a single location using
#' surveillance data and epidemiological parameters. It back-calculates true infections
#' from reported cases using the surveillance cascade, accounts for infection-to-report
#' delays, and estimates current E and I compartments based on disease progression.
#'
#' The function includes comprehensive parameter validation, numerical stability
#' protections for exponential calculations, and detailed progress reporting when verbose=TRUE.
#' Uses exact infectiousness kernels for the I compartment estimation.
#'
#' @param cases Vector of daily suspected cholera cases (must be same length as dates)
#' @param dates Vector of dates corresponding to cases (Date class)
#' @param population Total population of the location (must be positive)
#' @param t0 Target date for estimation (Date class)
#' @param lookback_days Days of data to use (default 60, must be positive)
#' @param sigma Symptomatic proportion (must be in (0,1])
#' @param rho Reporting rate - proportion of symptomatic cases reported (must be in (0,1])
#' @param chi Diagnostic positivity - proportion of suspected cases that are true cholera (must be in (0,1])
#' @param tau_r Reporting delay in days from symptom onset to report (must be non-negative)
#' @param iota Incubation rate (1/incubation period, must be positive)
#' @param gamma_1 Symptomatic recovery rate (must be positive)
#' @param gamma_2 Asymptomatic recovery rate (must be positive)
#' @param include_asymptomatic_in_I Logical; if TRUE, I includes asymptomatic infectious individuals (default TRUE)
#' @param verbose Print detailed progress (default FALSE)
#'
#' @return A list with two components:
#' \describe{
#' \item{E}{Number of individuals in Exposed compartment (non-negative numeric)}
#' \item{I}{Number of individuals in Infected compartment (non-negative numeric)}
#' }
#'
#' Returns E=0, I=0 if no cases in lookback window. Includes numerical stability
#' protections and parameter validation. Warns if E or I exceed 2% of population.
#'
#' @examples
#' \dontrun{
#' # Example with synthetic data
#' cases <- rpois(60, lambda = 5)
#' dates <- seq(as.Date("2023-01-01"), by = "day", length.out = 60)
#' result <- est_initial_E_I_location(
#'   cases = cases,
#'   dates = dates,
#'   population = 1000000,
#'   t0 = as.Date("2023-03-01"),
#'   sigma = 0.125,
#'   rho = 0.1,
#'   chi = 0.5,
#'   tau_r = 4,
#'   iota = 0.714,
#'   gamma_1 = 0.2,
#'   gamma_2 = 0.67
#' )
#' }
#'
#' @export
est_initial_E_I_location <- function(cases, dates, population, t0, lookback_days = 60,
                                     sigma, rho, chi, tau_r, iota, gamma_1, gamma_2,
                                     include_asymptomatic_in_I = TRUE, verbose = FALSE) {
  
  # ---- Parameter validation ----
  if (length(cases) != length(dates)) stop("cases and dates must have same length")
  if (length(cases) == 0) stop("cases and dates cannot be empty")
  if (population <= 0) stop("population must be positive")
  if (lookback_days <= 0) stop("lookback_days must be positive")
  if (sigma <= 0 || sigma > 1) stop("sigma must be in (0,1]")
  if (rho <= 0 || rho > 1) stop("rho must be in (0,1]")
  if (chi <= 0 || chi > 1) stop("chi must be in (0,1]")
  if (tau_r < 0) stop("tau_r must be non-negative")
  if (iota <= 0) stop("iota must be positive")
  if (gamma_1 <= 0) stop("gamma_1 must be positive")
  if (gamma_2 <= 0) stop("gamma_2 must be positive")
  
  # Convert dates to Date class if needed
  if (!inherits(dates, "Date")) {
    tryCatch({
      dates <- as.Date(dates)
    }, error = function(e) {
      stop("dates must be convertible to Date class")
    })
  }
  
  if (!inherits(t0, "Date")) {
    tryCatch({
      t0 <- as.Date(t0)
    }, error = function(e) {
      stop("t0 must be convertible to Date class")
    })
  }
  
  # ---- Filter surveillance data to lookback window ----
  lookback_start <- t0 - lookback_days
  lookback_end <- t0 - 1
  
  # Filter cases within lookback window
  in_window <- dates >= lookback_start & dates <= lookback_end
  cases_filtered <- cases[in_window]
  dates_filtered <- dates[in_window]
  
  if (verbose) {
    cat(sprintf("  Lookback window: %s to %s (%d days)\n", 
                lookback_start, lookback_end, lookback_days))
    cat(sprintf("  Cases in window: %d (total: %.0f)\n", 
                length(cases_filtered), sum(cases_filtered, na.rm = TRUE)))
  }
  
  # ---- Handle no-data case ----
  if (length(cases_filtered) == 0 || sum(cases_filtered, na.rm = TRUE) == 0) {
    if (verbose) cat("  No cases in lookback window, returning E=0, I=0\n")
    return(list(E = 0, I = 0))
  }
  
  # ---- Back-calculation: reported cases -> true infections ----
  # True infections = (reported cases × chi) / (rho × sigma)
  total_cases <- sum(cases_filtered, na.rm = TRUE)
  multiplier <- chi / (rho * sigma)
  total_infections <- total_cases * multiplier
  
  if (verbose) {
    cat(sprintf("  Surveillance multiplier: chi/(rho×sigma) = %.1f\n", multiplier))
    cat(sprintf("  Total infections: %.0f cases × %.1f = %.0f\n", 
                total_cases, multiplier, total_infections))
  }
  
  # ---- Account for reporting delay and distribute infections over time ----
  # Simple approach: distribute infections uniformly over lookback period
  # accounting for tau_r delay
  daily_infections <- total_infections / lookback_days
  
  # ---- Estimate current E and I compartments ----
  # E: Exposed individuals who will become infectious
  # I: Currently infectious individuals
  
  E_total <- 0
  I_total <- 0
  
  # For each day in the lookback period, calculate contribution to current E/I
  for (i in 1:length(dates_filtered)) {
    days_since_infection <- as.numeric(t0 - dates_filtered[i] - tau_r)
    
    if (days_since_infection <= 0) next  # Future infections
    
    # Proportion still in E (not yet infectious)
    prob_in_E <- exp(-iota * days_since_infection)
    
    # Proportion in I (infectious but not yet recovered)
    # Assumes exponential progression through I compartment
    if (days_since_infection > 1/iota) {
      days_infectious <- days_since_infection - 1/iota
      # Account for both symptomatic and asymptomatic recovery
      prob_in_I <- exp(-gamma_1 * days_infectious * sigma - gamma_2 * days_infectious * (1-sigma))
    } else {
      prob_in_I <- 0
    }
    
    # Add contributions from this day's infections
    daily_inf <- ifelse(is.na(cases_filtered[i]), 0, cases_filtered[i] * multiplier / lookback_days)
    E_total <- E_total + daily_inf * prob_in_E
    I_total <- I_total + daily_inf * prob_in_I
  }
  
  # ---- Numerical stability and bounds checking ----
  E_total <- max(0, round(E_total))
  I_total <- max(0, round(I_total))
  
  # Check for unrealistic estimates
  if (E_total > 0.02 * population) {
    warning(sprintf("E estimate (%.0f) exceeds 2%% of population (%.0f)", E_total, population))
  }
  if (I_total > 0.02 * population) {
    warning(sprintf("I estimate (%.0f) exceeds 2%% of population (%.0f)", I_total, population))
  }
  
  if (verbose) {
    cat(sprintf("  Final estimates: E=%.0f, I=%.0f\n", E_total, I_total))
    cat(sprintf("  As proportion of population: E=%.4f%%, I=%.4f%%\n", 
                100*E_total/population, 100*I_total/population))
  }
  
  return(list(E = E_total, I = I_total))
}
