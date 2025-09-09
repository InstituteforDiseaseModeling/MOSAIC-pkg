#' Estimate Initial E and I Compartments from Surveillance Data
#'
#' This function estimates the initial number of individuals in the Exposed (E) and
#' Infected (I) compartments at model start time using recent surveillance data
#' through a Monte Carlo simulation approach.
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
#' @param verbose Print progress messages (default TRUE).
#' @param parallel Enable parallel processing for Monte Carlo sampling when
#'   `n_samples >= 100` (default FALSE). Uses `parallel::mclapply()` with all
#'   available cores. Note: Not supported on Windows.
#' @param variance_inflation Factor to create variance in Beta distributions (default 2).
#'   Sets ci_lower = mean_val / variance_inflation and ci_upper = mean_val * variance_inflation.
#'   Values > 1 create wider distributions around the sample mean. Should be > 1.1 for meaningful variance.
#'
#' @return A list with two main components:
#' \describe{
#'   \item{metadata}{List containing estimation details: description, version, date, t0,
#'     lookback_days, n_samples, and method.}
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
#'   variance_inflation = 2           # Factor for expanding CI bounds around sample mean
#' )
#' }
#'
#' @export
est_initial_E_I <- function(PATHS, priors, config, n_samples = 1000,
                            t0 = NULL, lookback_days = 21,
                            verbose = TRUE, parallel = FALSE,
                            variance_inflation = 2) {

     # ---- Parameter validation ----
     if (n_samples <= 0) stop("n_samples must be positive")
     if (lookback_days <= 0) stop("lookback_days must be positive")
     if (!is.list(PATHS)) stop("PATHS must be a list")
     if (!is.list(priors)) stop("priors must be a list")
     if (!is.list(config)) stop("config must be a list")
     if (variance_inflation < 1.1) {
          warning("variance_inflation too low - should be > 1.1 for meaningful variance")
     }

     # ---- Helper: consistent location prior or default ----
     # Uses consistent defaults across sequential/parallel branches.
     draw_loc_or_default <- function(priors, name, loc, default, verbose = FALSE) {
          loc_prior <- tryCatch(priors$parameters_location[[name]]$parameters$location[[loc]],
                                error = function(e) NULL)
          if (!is.null(loc_prior)) {
               # With simplified sample_from_prior, just pass n and prior
               result <- sample_from_prior(n = 1, prior = loc_prior, verbose = verbose)
               if (is.na(result)) return(default)
               return(result)
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
               method = "monte_carlo_backcalculation"
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
          cat(sprintf("Method: Monte Carlo simulation\n"))
          cat(sprintf("Target date (t0): %s\n", t0))
          cat(sprintf("Lookback window: %s to %s (%d days)\n",
                      start_date, end_date, lookback_days))
          cat(sprintf("Number of Monte Carlo samples: %d\n", n_samples))
          cat(sprintf("CI bounds: variance_inflation=%.1f\n",
                      variance_inflation))
          cat(sprintf("Found surveillance data for %d/%d countries\n",
                      length(countries_with_data), length(location_codes)))
          cat("\n")
     }

     # ---- Monte Carlo method: Per-location processing ----
     for (loc in location_codes) {
          if (verbose) cat(sprintf("Processing %s... ", loc))

          tryCatch({
               loc_surv <- surveillance_window[surveillance_window$iso_code == loc, ]
               has_data <- loc %in% countries_with_data && nrow(loc_surv) > 0


               if (!has_data) {
                    if (verbose) cat("no data, using default priors\n")

                    E_default <- list(shape1 = 1, shape2 = 9999, method = "no_data_default")
                    E_default$metadata <- list(
                         data_available = FALSE, total_cases = 0,
                         mean_count = 0, sd_count = 0, n_samples = n_samples,
                         message = "No surveillance data in lookback window"
                    )
                    results$parameters_location$prop_E_initial$parameters$location[[loc]] <- E_default

                    I_default <- list(shape1 = 0.5, shape2 = 9999.5, method = "no_data_default")
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
                         sigma_i   <- sample_from_prior(n = 1, prior = priors$parameters_global$sigma, verbose = FALSE)
                         iota_i    <- sample_from_prior(n = 1, prior = priors$parameters_global$iota, verbose = FALSE)
                         gamma_1_i <- sample_from_prior(n = 1, prior = priors$parameters_global$gamma_1, verbose = FALSE)
                         gamma_2_i <- sample_from_prior(n = 1, prior = priors$parameters_global$gamma_2, verbose = FALSE)

                         #---------------------------------------------------------------------------------
                         # Temporary manual patch until observation process updated in LASER model
                         # TODO: Create proper priors for these parameters when LASER model is updated
                         rho_prior <- list(distribution = "uniform", parameters = list(min = 0.05, max = 0.30))
                         chi_prior <- list(distribution = "uniform", parameters = list(min = 0.50, max = 0.75))
                         tau_r_prior <- list(distribution = "gamma", parameters = list(shape = 2, rate = 0.5))
                         
                         rho_i <- sample_from_prior(n = 1, prior = rho_prior, verbose = FALSE)  # Reporting rate: 5-30%
                         chi_i <- sample_from_prior(n = 1, prior = chi_prior, verbose = FALSE)  # Diagnostic accuracy: 50-75%
                         tau_r_i <- sample_from_prior(n = 1, prior = tau_r_prior, verbose = FALSE)  # Reporting delay ~ mean 4, sd ≈ 2.8
                         #---------------------------------------------------------------------------------

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
                         sigma_i   <- sample_from_prior(n = 1, prior = priors$parameters_global$sigma, verbose = FALSE)
                         iota_i    <- sample_from_prior(n = 1, prior = priors$parameters_global$iota, verbose = FALSE)
                         gamma_1_i <- sample_from_prior(n = 1, prior = priors$parameters_global$gamma_1, verbose = FALSE)
                         gamma_2_i <- sample_from_prior(n = 1, prior = priors$parameters_global$gamma_2, verbose = FALSE)
                         
                         # Temporary manual patch until observation process updated in LASER model
                         # TODO: Create proper priors for these parameters when LASER model is updated
                         rho_prior <- list(distribution = "uniform", parameters = list(min = 0.05, max = 0.30))
                         chi_prior <- list(distribution = "uniform", parameters = list(min = 0.50, max = 0.75))
                         tau_r_prior <- list(distribution = "gamma", parameters = list(shape = 2, rate = 0.5))
                         
                         rho_i <- sample_from_prior(n = 1, prior = rho_prior, verbose = FALSE)
                         chi_i <- sample_from_prior(n = 1, prior = chi_prior, verbose = FALSE)
                         tau_r_i <- sample_from_prior(n = 1, prior = tau_r_prior, verbose = FALSE)

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

               # Apply Beta fitting using fit_beta_from_ci with variance inflation
               # Remove invalid samples and calculate mean
               E_valid <- E_prop[!is.na(E_prop) & E_prop > 0 & E_prop < 1]
               I_valid <- I_prop[!is.na(I_prop) & I_prop > 0 & I_prop < 1]
               
               if (length(E_valid) < 2) {
                    if (verbose) cat("  Insufficient E data for", loc, "- using default\n")
                    E_beta <- list(shape1 = 1, shape2 = 999, method = "insufficient_data")
               } else {
                    E_mean <- mean(E_valid)
                    E_ci_lower <- E_mean * (1 / variance_inflation)
                    E_ci_upper <- E_mean * variance_inflation
                    # Ensure valid Beta bounds
                    E_ci_lower <- max(1e-10, min(E_ci_lower, 0.999))
                    E_ci_upper <- max(E_ci_lower + 1e-10, min(E_ci_upper, 0.999))
                    
                    E_fit <- fit_beta_from_ci(
                         mode_val = E_mean,
                         ci_lower = E_ci_lower,
                         ci_upper = E_ci_upper,
                         method = "moment_matching"
                    )
                    E_beta <- list(shape1 = E_fit$shape1, shape2 = E_fit$shape2, method = "variance_inflation")
               }
               
               if (length(I_valid) < 2) {
                    if (verbose) cat("  Insufficient I data for", loc, "- using default\n")
                    I_beta <- list(shape1 = 1, shape2 = 999, method = "insufficient_data")
               } else {
                    I_mean <- mean(I_valid)
                    I_ci_lower <- I_mean * (1 / variance_inflation)
                    I_ci_upper <- I_mean * variance_inflation
                    # Ensure valid Beta bounds
                    I_ci_lower <- max(1e-10, min(I_ci_lower, 0.999))
                    I_ci_upper <- max(I_ci_lower + 1e-10, min(I_ci_upper, 0.999))
                    
                    I_fit <- fit_beta_from_ci(
                         mode_val = I_mean,
                         ci_lower = I_ci_lower,
                         ci_upper = I_ci_upper,
                         method = "moment_matching"
                    )
                    I_beta <- list(shape1 = I_fit$shape1, shape2 = I_fit$shape2, method = "variance_inflation")
               }

               if (is.null(E_beta)) {
                    E_beta <- list(shape1 = 1, shape2 = 9999, method = "fitting_failed")
               }
               if (is.null(I_beta)) {
                    I_beta <- list(shape1 = 0.5, shape2 = 9999.5, method = "fitting_failed")
               }

               # Format results with consistent structure
               E_result <- list(
                    distribution = "beta",
                    parameters = list(
                         shape1 = E_beta$shape1,
                         shape2 = E_beta$shape2
                    ),
                    metadata = list(
                         data_available = TRUE,
                         total_cases = total_cases,
                         mean_count = mean(E_samples),
                         sd_count = sd(E_samples),
                         n_samples = n_samples,
                         method = E_beta$method,
                         message = sprintf("Processed with %d surveillance cases", total_cases)
                    )
               )
               
               I_result <- list(
                    distribution = "beta",
                    parameters = list(
                         shape1 = I_beta$shape1,
                         shape2 = I_beta$shape2
                    ),
                    metadata = list(
                         data_available = TRUE,
                         total_cases = total_cases,
                         mean_count = mean(I_samples),
                         sd_count = sd(I_samples),
                         n_samples = n_samples,
                         method = I_beta$method,
                         message = sprintf("Processed with %d surveillance cases", total_cases)
                    )
               )

               results$parameters_location$prop_E_initial$parameters$location[[loc]] <- E_result
               results$parameters_location$prop_I_initial$parameters$location[[loc]] <- I_result

          }, error = function(e) {
               warning(sprintf("Error processing %s: %s", loc, e$message))
               if (verbose) cat(sprintf("error: %s\n", e$message))
               
               E_error <- list(
                    distribution = "beta",
                    parameters = list(
                         shape1 = 1,
                         shape2 = 9999
                    ),
                    metadata = list(
                         data_available = FALSE,
                         total_cases = 0,
                         mean_count = 0,
                         sd_count = 0,
                         n_samples = n_samples,
                         method = "error_fallback",
                         message = sprintf("Error during processing: %s", e$message)
                    )
               )
               results$parameters_location$prop_E_initial$parameters$location[[loc]] <- E_error

               I_error <- list(
                    distribution = "beta",
                    parameters = list(
                         shape1 = 0.5,
                         shape2 = 9999.5
                    ),
                    metadata = list(
                         data_available = FALSE,
                         total_cases = 0,
                         mean_count = 0,
                         sd_count = 0,
                         n_samples = n_samples,
                         method = "error_fallback",
                         message = sprintf("Error during processing: %s", e$message)
                    )
               )
               results$parameters_location$prop_I_initial$parameters$location[[loc]] <- I_error
          })

     }

     # Note: Beta fitting uses fit_beta_from_ci with custom CI bounds for wider exploration

     if (verbose) {
          cat("\n=== Monte Carlo Estimation Complete ===\n")
          cat(sprintf("Successfully processed %d locations\n",
                      length(results$parameters_location$prop_E_initial$parameters$location)))

          # Comprehensive results table
          cat("\n=== Final E/I Prior Distribution Summary ===\n")
          cat(sprintf("%-4s %-20s %-15s %-12s %-20s %-15s %-12s\n",
                      "LOC", "E_Beta(shape1,shape2)", "E_Mean_Count", "E_95%_CI",
                      "I_Beta(shape1,shape2)", "I_Mean_Count", "I_95%_CI"))
          cat(paste(rep("-", 110), collapse=""), "\n")

          for (loc in names(results$parameters_location$prop_E_initial$parameters$location)) {
               E_result <- results$parameters_location$prop_E_initial$parameters$location[[loc]]
               I_result <- results$parameters_location$prop_I_initial$parameters$location[[loc]]

               # Calculate 95% CI for E compartment counts
               if (!is.null(E_result$metadata$mean_count) && E_result$metadata$mean_count > 0) {
                    E_mean <- E_result$metadata$mean_count
                    E_sd <- E_result$metadata$sd_count
                    E_ci_low <- max(0, E_mean - 1.96 * E_sd)
                    E_ci_high <- E_mean + 1.96 * E_sd
                    E_ci_str <- sprintf("(%.0f-%.0f)", E_ci_low, E_ci_high)
                    E_mean_str <- sprintf("%.0f", E_mean)
               } else {
                    E_ci_str <- "(-)"
                    E_mean_str <- "0"
               }

               # Calculate 95% CI for I compartment counts
               if (!is.null(I_result$metadata$mean_count) && I_result$metadata$mean_count > 0) {
                    I_mean <- I_result$metadata$mean_count
                    I_sd <- I_result$metadata$sd_count
                    I_ci_low <- max(0, I_mean - 1.96 * I_sd)
                    I_ci_high <- I_mean + 1.96 * I_sd
                    I_ci_str <- sprintf("(%.0f-%.0f)", I_ci_low, I_ci_high)
                    I_mean_str <- sprintf("%.0f", I_mean)
               } else {
                    I_ci_str <- "(-)"
                    I_mean_str <- "0"
               }

               E_beta_str <- sprintf("(%.2f,%.2f)", E_result$parameters$shape1, E_result$parameters$shape2)
               I_beta_str <- sprintf("(%.2f,%.2f)", I_result$parameters$shape1, I_result$parameters$shape2)

               cat(sprintf("%-4s %-20s %-15s %-20s %-20s %-15s %-20s\n",
                           loc, E_beta_str, E_mean_str, E_ci_str,
                           I_beta_str, I_mean_str, I_ci_str))
          }
          cat("\nNote: CIs based on Monte Carlo sample statistics (mean ± 1.96×SD)\n")
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
                                     verbose = FALSE) {

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

  # ---- Account for reporting delay ----
  # Each day contributes based on its actual case count, not averaged

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
    daily_inf <- ifelse(is.na(cases_filtered[i]), 0, cases_filtered[i] * multiplier)
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
