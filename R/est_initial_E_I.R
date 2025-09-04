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
#' @param verbose Print progress messages (default TRUE).
#' @param parallel Enable parallel processing for Monte Carlo sampling when
#'   `n_samples >= 100` (default FALSE). Uses `parallel::mclapply()` with all
#'   available cores. Note: Not supported on Windows.
#' @param prior_method Method for creating Beta priors (default "left_skewed"):
#'   - "left_skewed": Naturally biased toward lower E/I values (recommended)
#'   - "expanded_ci": Wide confidence intervals for broad exploration  
#'   - "conservative": Bias toward zero for cautious estimates
#'   - "wide_uniform": Nearly uniform for maximum exploration
#'   - "adaptive": Auto-select method based on sample characteristics
#' @param expansion_factor Factor for confidence interval expansion (default 2.5).
#'   Values > 1.0 create wider priors. Recommended range: 1.5-4.0.
#' @param conservatism_bias Strength of bias toward zero (default 0.2). 
#'   Range 0.0-0.5 where higher values create stronger bias toward lower E/I values.
#' @param variance_inflation DEPRECATED. Use prior_method, expansion_factor, and conservatism_bias instead.
#'   If provided, will be converted to equivalent expansion_factor for backward compatibility.
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
#'   prior_method = "left_skewed",    # Biologically motivated default
#'   expansion_factor = 2.5,          # Wide CIs for scenario exploration  
#'   conservatism_bias = 0.2          # Moderate bias toward zero
#' )
#' }
#'
#' @export
est_initial_E_I <- function(PATHS, priors, config, n_samples = 1000,
                            t0 = NULL, lookback_days = 21,
                            verbose = TRUE, parallel = FALSE,
                            prior_method = "left_skewed",
                            expansion_factor = 2.5,
                            conservatism_bias = 0.2,
                            variance_inflation = NULL) {

     # ---- Parameter validation ----
     if (n_samples <= 0) stop("n_samples must be positive")
     if (lookback_days <= 0) stop("lookback_days must be positive")
     if (!is.list(PATHS)) stop("PATHS must be a list")
     if (!is.list(priors)) stop("priors must be a list")
     if (!is.list(config)) stop("config must be a list")
     
     # Validate new parameters
     valid_methods <- c("left_skewed", "expanded_ci", "conservative", "wide_uniform", "adaptive")
     if (!prior_method %in% valid_methods) {
          stop("prior_method must be one of: ", paste(valid_methods, collapse = ", "))
     }
     if (expansion_factor < 1.0) stop("expansion_factor must be >= 1.0")
     if (conservatism_bias < 0 || conservatism_bias > 0.5) {
          stop("conservatism_bias must be between 0.0 and 0.5")
     }
     
     # Handle deprecated variance_inflation parameter
     if (!is.null(variance_inflation)) {
          warning("variance_inflation is deprecated. Use expansion_factor instead.")
          if (variance_inflation < -0.5 || variance_inflation > 0.5) {
               warning("variance_inflation is outside recommended range")
          }
          # Convert to expansion_factor for backward compatibility
          expansion_factor <- 1.0 + abs(variance_inflation) * 2
     }

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

     # ---- Helper: Simple Beta fitting from samples (proportions) ----
     fit_beta_from_samples <- function(x, label = "") {
          x <- x[!is.na(x) & x > 0 & x < 1]

          if (length(x) < 2) {
               if (verbose) cat("  Insufficient data for", label, "- using default\n")
               return(list(shape1 = 1, shape2 = 999, method = "insufficient_data"))
          }

          if (var(x) == 0) {
               mean_val <- mean(x)
               precision <- 1000
               return(list(
                    shape1 = mean_val * precision,
                    shape2 = (1 - mean_val) * precision,
                    method = "constant_value"
               ))
          }

          # Standard method of moments
          mu <- mean(x)
          v <- var(x)
          precision <- (mu * (1 - mu) / v) - 1

          if (precision <= 0) {
               if (verbose) cat("  High variance for", label, "- using robust fallback\n")
               precision <- 10  # Fallback precision
          }

          return(list(
               shape1 = max(0.01, mu * precision),
               shape2 = max(0.01, (1 - mu) * precision),
               method = "method_of_moments"
          ))
     }

     # ---- Helper: Enhanced Beta fitting with flexible prior creation ----
     fit_beta_flexible_wrapper <- function(samples, prior_method, expansion_factor, conservatism_bias, label = "") {
          # Remove invalid samples
          valid_samples <- samples[!is.na(samples) & samples > 0 & samples < 1]

          if (length(valid_samples) < 2) {
               if (verbose) cat("  Insufficient data for", label, "- using default\n")
               return(list(shape1 = 1.02, shape2 = 999, method = "insufficient_data"))
          }

          # Use the new flexible Beta fitting
          tryCatch({
               beta_fit <- fit_beta_flexible(
                    samples = valid_samples,
                    method = prior_method,
                    expansion_factor = expansion_factor,
                    conservatism_bias = conservatism_bias,
                    min_precision = 4.0,
                    max_precision = 100.0,
                    target_percentile = 0.25,  # For left-skewed: use 25th percentile as mode
                    verbose = FALSE
               )
               
               return(list(
                    shape1 = beta_fit$shape1,
                    shape2 = beta_fit$shape2,
                    method = paste0("flexible_", beta_fit$method)
               ))
               
          }, error = function(e) {
               if (verbose) cat("  Flexible fitting failed for", label, "- using fallback\n")
               
               # Simple fallback using method of moments
               sample_mean <- mean(valid_samples)
               sample_var <- var(valid_samples)
               precision <- (sample_mean * (1 - sample_mean) / sample_var) - 1
               precision <- max(4.0, precision)  # Minimum for valid Beta
               
               return(list(
                    shape1 = max(1.02, sample_mean * precision),
                    shape2 = max(1.02, (1 - sample_mean) * precision),
                    method = "fallback_method_of_moments"
               ))
          })
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
               method = "surveillance_backcalculation"
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
          cat(sprintf("Prior method: %s (expansion_factor=%.1f, conservatism_bias=%.1f)\n",
                      prior_method, expansion_factor, conservatism_bias))
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

                    E_default <- list(shape1 = 1.02, shape2 = 9999, method = "no_data_default")
                    E_default$metadata <- list(
                         data_available = FALSE, total_cases = 0,
                         mean_count = 0, sd_count = 0, n_samples = n_samples,
                         message = "No surveillance data in lookback window"
                    )
                    results$parameters_location$prop_E_initial$parameters$location[[loc]] <- E_default

                    I_default <- list(shape1 = 1.02, shape2 = 9999, method = "no_data_default")
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

                         #---------------------------------------------------------------------------------
                         # Temporary manual patch until observation process updated in LASER model
                         rho_i <- runif(1, min = 0.05, max = 0.30)  # Reporting rate: 5-30%
                         chi_i <- runif(1, min = 0.50, max = 0.75)  # Diagnostic accuracy: 50-75%
                         #---------------------------------------------------------------------------------

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
                         # Temporary manual patch until observation process updated in LASER model
                         rho_i <- runif(1, min = 0.05, max = 0.30)  # Reporting rate: 5-30%
                         chi_i <- runif(1, min = 0.50, max = 0.75)  # Diagnostic accuracy: 50-75%

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

               # Apply new flexible Beta fitting method
               E_beta <- fit_beta_flexible_wrapper(
                    samples = E_prop,
                    prior_method = prior_method,
                    expansion_factor = expansion_factor,
                    conservatism_bias = conservatism_bias,
                    label = paste0("E_", loc)
               )
               I_beta <- fit_beta_flexible_wrapper(
                    samples = I_prop,
                    prior_method = prior_method,
                    expansion_factor = expansion_factor,
                    conservatism_bias = conservatism_bias,
                    label = paste0("I_", loc)
               )

               if (is.null(E_beta)) {
                    E_beta <- list(shape1 = 1.02, shape2 = 9999, method = "fitting_failed")
               }
               if (is.null(I_beta)) {
                    I_beta <- list(shape1 = 1.02, shape2 = 9999, method = "fitting_failed")
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

          }, error = function(e) {
               warning(sprintf("Error processing %s: %s", loc, e$message))
               if (verbose) cat(sprintf("error: %s\n", e$message))
               E_error <- list(shape1 = 1.02, shape2 = 9999, method = "error_fallback")
               E_error$metadata <- list(
                    data_available = FALSE, total_cases = 0,
                    mean_count = 0, sd_count = 0, n_samples = n_samples,
                    message = sprintf("Error during processing: %s", e$message)
               )
               results$parameters_location$prop_E_initial$parameters$location[[loc]] <- E_error

               I_error <- list(shape1 = 1.02, shape2 = 9999, method = "error_fallback")
               I_error$metadata <- list(
                    data_available = FALSE, total_cases = 0,
                    mean_count = 0, sd_count = 0, n_samples = n_samples,
                    message = sprintf("Error during processing: %s", e$message)
               )
               results$parameters_location$prop_I_initial$parameters$location[[loc]] <- I_error
          })

     }

     # Note: Enhanced flexible Beta fitting is applied with biologically-motivated left-skewed priors

     if (verbose) {
          cat("\n=== Estimation Complete ===\n")
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

               E_beta_str <- sprintf("(%.2f,%.2f)", E_result$shape1, E_result$shape2)
               I_beta_str <- sprintf("(%.2f,%.2f)", I_result$shape1, I_result$shape2)

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
