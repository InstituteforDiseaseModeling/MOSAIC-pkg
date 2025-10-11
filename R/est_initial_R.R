#' Estimate Initial R Compartment from Historical Cholera Surveillance
#'
#' @description
#' Estimates the initial proportion of the population in the Recovered (R) compartment
#' based on historical cholera surveillance data. The R compartment represents individuals
#' with natural immunity from previous cholera infection, subject to waning immunity.
#'
#' @param PATHS List containing paths to data directories
#' @param priors List containing prior distributions for model parameters
#' @param config List containing model configuration including location codes
#' @param n_samples Integer, number of Monte Carlo samples for uncertainty quantification (default 1000)
#' @param t0 Date object, target date for estimation (default NULL uses current date)
#' @param disaggregate Logical, whether to use Fourier disaggregation (TRUE) or mid-year point estimate (FALSE)
#' @param verbose Logical, whether to print progress messages (default TRUE)
#' @param parallel Logical, whether to use parallel processing for locations when length(location_codes) >= 8 (default FALSE).
#'   Uses parallel::mclapply() with all available cores. Note: Not supported on Windows.
#' @param variance_inflation Numeric factor to inflate variance of fitted Beta distributions (default 1 = no inflation).
#'   Values > 1 increase uncertainty while preserving the mean. For example, 2 doubles the variance.
#'
#' @return List with structure matching priors_default for prop_R_initial parameters
#' @export
#'
#' @examples
#' \dontrun{
#' PATHS <- get_paths()
#' priors <- priors_default
#' config <- config_default
#' initial_R <- est_initial_R(PATHS, priors, config, n_samples = 1000,
#'                            t0 = as.Date("2024-01-01"), disaggregate = TRUE)
#' }
est_initial_R <- function(
          PATHS,
          priors,
          config,
          n_samples = 1000,
          t0 = NULL,
          disaggregate = TRUE,
          verbose = TRUE,
          parallel = FALSE,
          variance_inflation = 1
) {
     # Set default t0 if not provided
     if (is.null(t0)) {
          t0 <- Sys.Date()
     }
     t0 <- as.Date(t0)

     # Load WHO annual data
     who_path <- file.path(PATHS$DATA_PROCESSED, "WHO/annual/who_afro_annual_1949_2024.csv")
     if (!file.exists(who_path)) {
          stop("WHO annual data not found at: ", who_path)
     }
     who_data <- read.csv(who_path, stringsAsFactors = FALSE)

     # Load population data - use daily or annual based on disaggregate setting
     if (disaggregate) {
          # Use daily population data when disaggregating for more precise matching
          pop_path <- file.path(PATHS$DATA_DEMOGRAPHICS, "UN_world_population_prospects_daily.csv")
          if (!file.exists(pop_path)) {
               stop("Daily population data not found at: ", pop_path)
          }
          pop_data <- read.csv(pop_path, stringsAsFactors = FALSE)
          pop_data$date <- as.Date(pop_data$date)
     } else {
          # Use annual population data for NULL model (faster, less memory)
          pop_path <- file.path(PATHS$DATA_DEMOGRAPHICS, "UN_world_population_prospects_annual.csv")
          if (!file.exists(pop_path)) {
               stop("Annual population data not found at: ", pop_path)
          }
          pop_data <- read.csv(pop_path, stringsAsFactors = FALSE)
          # Convert year to date for compatibility (use January 1st of each year)
          pop_data$date <- as.Date(paste0(pop_data$year, "-01-01"))
     }

     # Initialize results storage
     results_list <- list()

     # Get location codes from config
     location_codes <- config$location_name
     if (is.null(location_codes)) {
          stop("No location_name found in config")
     }

     # Set up progress bar if verbose
     if (verbose) {
          cat("\n=== Starting est_initial_R ===\n")
          cat("Target date (t0):", as.character(t0), "\n")
          cat("Disaggregation mode:", ifelse(disaggregate, "Fourier series", "Mid-year point estimate"), "\n")
          cat("Number of samples per location:", n_samples, "\n")
          if (variance_inflation != 1) {
               cat("Variance inflation factor:", variance_inflation, "\n")
          }
          cat("Processing", length(location_codes), "locations")
          if (parallel && length(location_codes) >= 8) {
               cat(sprintf(" using parallel processing with %d cores\n", parallel::detectCores()))
          } else {
               cat(":\n")
               pb <- txtProgressBar(min = 0, max = length(location_codes), style = 3)
               loc_counter <- 0
          }
     }

     # Define location processing function
     process_location <- function(loc, loc_index = NULL) {
          if (verbose && !parallel && !is.null(loc_index)) {
               setTxtProgressBar(pb, loc_index)
          }

          # Filter data for location
          loc_data <- who_data[who_data$iso_code == loc, ]

          if (nrow(loc_data) == 0) {
               if (!parallel) warning("No WHO data for ", loc)
               return(list(
                    location = loc,
                    result = list(
                         samples = NA,
                         proportions = NA,
                         beta_fit = NULL,
                         mean = NA,
                         ci_lower = NA,
                         ci_upper = NA,
                         population = NA,
                         count_mean = NA,
                         count_ci_lower = NA,
                         count_ci_upper = NA
                    )
               ))
          }

          # Remove rows with missing or zero cases
          loc_data <- loc_data[!is.na(loc_data$cases_total) & loc_data$cases_total > 0, ]

          if (nrow(loc_data) == 0) {
               if (!parallel) warning("No non-zero cases for ", loc)
               return(list(
                    location = loc,
                    result = list(
                         samples = NA,
                         proportions = NA,
                         beta_fit = NULL,
                         mean = NA,
                         ci_lower = NA,
                         ci_upper = NA,
                         population = NA,
                         count_mean = NA,
                         count_ci_lower = NA,
                         count_ci_upper = NA
                    )
               ))
          }

          # Get population at t0 for this location
          pop_loc <- pop_data[pop_data$iso_code == loc, ]
          if (nrow(pop_loc) == 0) {
               if (!parallel) warning("No population data for ", loc)
               return(NULL)
          }

          # Find closest population data to t0
          time_diffs <- abs(as.numeric(difftime(pop_loc$date, t0, units = "days")))
          closest_idx <- which.min(time_diffs)
          population_t0 <- pop_loc$total_population[closest_idx]

          if (is.na(population_t0) || population_t0 <= 0) {
               if (!parallel) warning("Invalid population for ", loc)
               return(NULL)
          }

          # Get Fourier parameter priors for location (if disaggregating)
          fourier_priors <- NULL
          if (disaggregate) {
               if (!is.null(priors$parameters_location$fourier_params)) {
                    fourier_loc <- priors$parameters_location$fourier_params$parameters$location[[loc]]
                    if (!is.null(fourier_loc)) {
                         fourier_priors <- fourier_loc
                    } else {
                         if (!parallel) warning("No Fourier parameters for ", loc, ", using uniform seasonality")
                    }
               }
          }

          # Initialize sample storage
          R_samples <- numeric(n_samples)

          # Monte Carlo loop with full uncertainty propagation
          for (i in 1:n_samples) {
               # Sample epidemiological parameters
               epsilon_i <- sample_from_prior(n = 1, prior = priors$parameters_global$epsilon, verbose = verbose)
               sigma_i <- sample_from_prior(n = 1, prior = priors$parameters_global$sigma, verbose = verbose)
               iota_i <- sample_from_prior(n = 1, prior = priors$parameters_global$iota, verbose = verbose)
               gamma_1_i <- sample_from_prior(n = 1, prior = priors$parameters_global$gamma_1, verbose = verbose)
               gamma_2_i <- sample_from_prior(n = 1, prior = priors$parameters_global$gamma_2, verbose = verbose)

               # Sample location-specific surveillance parameters
               if (!is.null(priors$parameters_location$rho$parameters$location[[loc]])) {
                    rho_i <- sample_from_prior(n = 1, prior = priors$parameters_location$rho$parameters$location[[loc]], verbose = verbose)
               } else {
                    rho_i <- 0.1  # Default reporting rate
               }

               if (!is.null(priors$parameters_location$chi$parameters$location[[loc]])) {
                    chi_i <- sample_from_prior(n = 1, prior = priors$parameters_location$chi$parameters$location[[loc]], verbose = verbose)
               } else {
                    chi_i <- 0.5  # Default diagnostic positivity
               }

               # Handle missing parameters with defaults
               if (is.na(epsilon_i)) epsilon_i <- 0.0004  # ~4 year half-life
               if (is.na(sigma_i)) sigma_i <- 0.25  # Endemic symptomatic proportion
               if (is.na(rho_i)) rho_i <- 0.2
               if (is.na(chi_i)) chi_i <- 0.66
               if (is.na(iota_i)) iota_i <- 0.714  # ~1.4 day incubation
               if (is.na(gamma_1_i)) gamma_1_i <- 0.2  # ~5 day symptomatic recovery
               if (is.na(gamma_2_i)) gamma_2_i <- 0.67  # ~1.5 day asymptomatic recovery

               if (disaggregate && !is.null(fourier_priors)) {
                    # Sample Fourier seasonality parameters
                    a1_i <- sample_from_prior(n = 1, prior = fourier_priors$a1, verbose = verbose)
                    b1_i <- sample_from_prior(n = 1, prior = fourier_priors$b1, verbose = verbose)
                    a2_i <- sample_from_prior(n = 1, prior = fourier_priors$a2, verbose = verbose)
                    b2_i <- sample_from_prior(n = 1, prior = fourier_priors$b2, verbose = verbose)

                    # Use defaults if sampling failed
                    if (is.na(a1_i)) a1_i <- 0
                    if (is.na(b1_i)) b1_i <- 0
                    if (is.na(a2_i)) a2_i <- 0
                    if (is.na(b2_i)) b2_i <- 0

                    # Disaggregate with sampled seasonality parameters
                    daily_data <- disagg_annual_cases_to_daily(
                         annual_cases = loc_data$cases_total,
                         years = loc_data$year,
                         a1 = a1_i,
                         b1 = b1_i,
                         a2 = a2_i,
                         b2 = b2_i
                    )
               } else {
                    # NULL model: Place all annual cases at mid-year (July 1st)
                    daily_data <- data.frame(
                         date = as.Date(paste0(loc_data$year, "-07-01")),
                         cases = loc_data$cases_total,
                         year = loc_data$year,
                         day_of_year = 182
                    )
               }

               # Estimate R with all sampled parameters
               R_samples[i] <- est_initial_R_location(
                    cases = daily_data$cases,
                    dates = daily_data$date,
                    population = population_t0,
                    t0 = t0,
                    epsilon = epsilon_i,
                    sigma = sigma_i,
                    rho = rho_i,
                    chi = chi_i,
                    iota = iota_i,
                    gamma_1 = gamma_1_i,
                    gamma_2 = gamma_2_i
               )
          }

          # Remove any NA or invalid samples
          valid_samples <- R_samples[!is.na(R_samples) & R_samples >= 0 & R_samples <= population_t0]

          if (length(valid_samples) < 10) {
               if (!parallel) warning("Too few valid samples for ", loc)
               return(list(
                    location = loc,
                    result = list(
                         samples = NA,
                         proportions = NA,
                         beta_fit = NULL,
                         mean = NA,
                         ci_lower = NA,
                         ci_upper = NA,
                         population = population_t0,
                         count_mean = NA,
                         count_ci_lower = NA,
                         count_ci_upper = NA
                    )
               ))
          }

          # Fit Beta distribution to R/N proportions with variance inflation
          R_proportions <- valid_samples / population_t0
          beta_fit <- fit_beta_with_variance_inflation_R(
               samples = R_proportions,
               variance_inflation = variance_inflation,
               label = paste0("R_", loc)
          )

          # Return results for this location
          return(list(
               location = loc,
               result = list(
                    samples = valid_samples,
                    proportions = R_proportions,
                    beta_fit = beta_fit,
                    mean = mean(R_proportions),
                    ci_lower = quantile(R_proportions, 0.025),
                    ci_upper = quantile(R_proportions, 0.975),
                    population = population_t0,
                    count_mean = mean(valid_samples),
                    count_ci_lower = quantile(valid_samples, 0.025),
                    count_ci_upper = quantile(valid_samples, 0.975)
               )
          ))
     }

     # Process locations either in parallel or sequentially
     if (parallel && length(location_codes) >= 8) {
          # Parallel processing
          location_results <- parallel::mclapply(location_codes, process_location, mc.cores = parallel::detectCores())

          # Extract results into results_list format, handling errors
          for (i in seq_along(location_results)) {
               loc_result <- location_results[[i]]

               # Check if result is an error
               if (inherits(loc_result, "try-error")) {
                    warning(sprintf("Error processing location %s in parallel: %s",
                                  location_codes[i], as.character(loc_result)))
                    next
               }

               # Check if result is NULL or has expected structure
               if (!is.null(loc_result) && is.list(loc_result) && "location" %in% names(loc_result)) {
                    results_list[[loc_result$location]] <- loc_result$result
               }
          }
     } else {
          # Sequential processing with progress bar
          for (i in seq_along(location_codes)) {
               loc <- location_codes[i]
               if (verbose) {
                    loc_counter <- loc_counter + 1
               }

               loc_result <- process_location(loc, i)
               if (!is.null(loc_result)) {
                    results_list[[loc_result$location]] <- loc_result$result
               }
          }
     }

     # Close progress bar
     if (verbose && !parallel) {
          close(pb)
          cat("\n")
     }

     # Format output to match priors_default structure
     output <- list(
          metadata = list(
               description = "Initial R recovered compartment estimates",
               version = "1.0.0",
               date = as.character(Sys.Date()),
               initial_conditions_R = list(
                    estimated_date = as.character(Sys.Date()),
                    method = "est_initial_R",
                    n_samples = n_samples,
                    t0 = as.character(t0),
                    disaggregate = disaggregate,
                    n_locations_processed = sum(!sapply(results_list, function(x) is.na(x$mean))),
                    locations_processed = names(results_list)[!sapply(results_list, function(x) is.na(x$mean))],
                    priors_used = list(
                         epsilon = "epsilon",
                         sigma = "sigma",
                         rho = "rho",
                         chi = "chi",
                         iota = "iota",
                         gamma_1 = "gamma_1",
                         gamma_2 = "gamma_2"
                    )
               )
          ),

          # Match priors_default structure: parameter -> location hierarchy
          parameters_location = list(
               prop_R_initial = list(
                    parameter_name = "prop_R_initial",
                    description = "Initial proportion of population with natural immunity",
                    distribution = "beta",
                    parameters = list(
                         location = list()
                    )
               )
          )
     )

     # Populate location-specific parameters
     for (loc in names(results_list)) {
          loc_data <- results_list[[loc]]

          if (!is.null(loc_data$beta_fit)) {
               # Extract Beta parameters (variance inflation already applied during fitting)
               alpha <- as.numeric(loc_data$beta_fit$shape1)
               beta <- as.numeric(loc_data$beta_fit$shape2)

               output$parameters_location$prop_R_initial$parameters$location[[loc]] <- list(
                    shape1 = alpha,
                    shape2 = beta
               )
          } else {
               # Include NA values for locations without data
               output$parameters_location$prop_R_initial$parameters$location[[loc]] <- list(
                    shape1 = NA,
                    shape2 = NA
               )
          }
     }

     class(output) <- c("mosaic_priors", "list")
     return(output)
}

#' Estimate Initial R Compartment for Single Location
#'
#' @description
#' Estimates the recovered population at time t0 for a single location based on
#' historical infection data, accounting for waning immunity.
#'
#' @param cases Vector of reported suspected cholera cases
#' @param dates Vector of dates corresponding to cases
#' @param population Population size at t0
#' @param t0 Target date for estimation
#' @param epsilon Waning immunity rate (per day)
#' @param sigma Proportion of infections that are symptomatic
#' @param rho Reporting rate of symptomatic cases
#' @param chi Diagnostic positivity rate
#' @param iota Incubation rate (per day)
#' @param gamma_1 Symptomatic recovery rate (per day)
#' @param gamma_2 Asymptomatic recovery rate (per day)
#'
#' @return Numeric value of R compartment size at t0
#' @export
est_initial_R_location <- function(
          cases,
          dates,
          population,
          t0,
          epsilon,
          sigma = 0.01,
          rho = 0.1,
          chi = 0.5,
          iota,
          gamma_1,
          gamma_2
) {
     # Convert dates to Date objects
     dates <- as.Date(dates)
     t0 <- as.Date(t0)

     # Convert rates to periods (days) for internal calculations
     iota_inv <- 1 / iota        # Incubation period
     gamma1_inv <- 1 / gamma_1   # Symptomatic infectious period
     gamma2_inv <- 1 / gamma_2   # Asymptomatic infectious period

     # Calculate effective infectious period (weighted harmonic mean)
     gamma_eff_inv <- sigma * gamma1_inv + (1 - sigma) * gamma2_inv

     # Total duration from infection to recovery
     total_duration <- iota_inv + gamma_eff_inv

     # Calculate total infections from reported cases
     # I_t = (C_t × χ) / (ρ × σ)
     infections <- (cases * chi) / (rho * sigma)

     # Initialize R compartment
     R_t0 <- 0

     # Apply waning immunity with recovery delay
     # R(t0) = Σ I_t × exp(-ε × max(0, t0 - (t + total_duration)))
     for (i in seq_along(infections)) {
          infection_date <- dates[i]
          recovery_date <- infection_date + total_duration

          # Only count infections that have completed recovery
          if (recovery_date < t0) {
               # Time since recovery
               time_since_recovery <- as.numeric(difftime(t0, recovery_date, units = "days"))

               # Apply exponential waning
               if (time_since_recovery > 0) {
                    remaining_immunity <- exp(-epsilon * time_since_recovery)
                    R_t0 <- R_t0 + infections[i] * remaining_immunity
               }
          }
     }

     # Ensure R doesn't exceed population
     R_t0 <- min(R_t0, population * 0.99)  # Cap at 99% of population

     return(R_t0)
}

#' Temporal Disaggregation of Annual Cases to Daily Resolution
#'
#' @description
#' Disaggregates annual cholera case counts to daily resolution using Fourier series
#' seasonal patterns. Ensures exact preservation of annual totals.
#'
#' @param annual_cases Vector of annual case counts
#' @param years Vector of years corresponding to cases
#' @param a1 First harmonic cosine coefficient
#' @param b1 First harmonic sine coefficient
#' @param a2 Second harmonic cosine coefficient
#' @param b2 Second harmonic sine coefficient
#'
#' @return Data frame with columns: date, year, day_of_year, cases, annual_total
#' @export
disagg_annual_cases_to_daily <- function(
          annual_cases,
          years,
          a1,
          b1,
          a2,
          b2
) {
     # Input validation
     if (length(annual_cases) != length(years)) {
          stop("annual_cases and years must have the same length")
     }

     # Initialize results
     all_daily <- list()

     # Process each year to ensure exact preservation of annual totals
     for (i in seq_along(years)) {
          year <- years[i]
          annual_total <- annual_cases[i]

          # Skip if no cases
          if (is.na(annual_total) || annual_total <= 0) {
               next
          }

          # Handle leap years
          is_leap <- (year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0))
          n_days <- ifelse(is_leap, 366, 365)

          # Day of year sequence
          t <- 1:n_days

          # Generate daily weights from Fourier series
          # Use 365.25 for period to handle leap years consistently
          fourier_daily <- a1 * cos(2 * pi * t / 365.25) +
               b1 * sin(2 * pi * t / 365.25) +
               a2 * cos(4 * pi * t / 365.25) +
               b2 * sin(4 * pi * t / 365.25)

          # Ensure non-negative weights
          weights <- pmax(0, fourier_daily)

          # Normalize weights to sum to 1
          weight_sum <- sum(weights)
          if (weight_sum > 0) {
               weights <- weights / weight_sum
          } else {
               # If all weights are zero (shouldn't happen), use uniform distribution
               weights <- rep(1/n_days, n_days)
          }

          # Distribute annual cases using normalized weights
          daily_cases_year <- annual_total * weights

          # CRITICAL: Validate total preservation (automatic)
          total_check <- sum(daily_cases_year)
          if (abs(total_check - annual_total) > 1e-6) {
               stop(sprintf("Disaggregation failed to preserve total for year %d: original = %.2f, disaggregated = %.2f",
                            year, annual_total, total_check))
          }

          # Create date sequence for the year
          start_date <- as.Date(paste0(year, "-01-01"))
          end_date <- as.Date(paste0(year, "-12-31"))
          date_seq <- seq(start_date, end_date, by = "day")

          # Ensure we have the right number of dates
          date_seq <- date_seq[1:n_days]

          # Create data frame for this year
          year_df <- data.frame(
               date = date_seq,
               year = year,
               day_of_year = t,
               cases = daily_cases_year,
               annual_total = annual_total,
               stringsAsFactors = FALSE
          )

          all_daily[[length(all_daily) + 1]] <- year_df
     }

     # Handle case with no valid data
     if (length(all_daily) == 0) {
          return(data.frame(
               date = as.Date(character()),
               year = integer(),
               day_of_year = integer(),
               cases = numeric(),
               annual_total = numeric(),
               stringsAsFactors = FALSE
          ))
     }

     # Combine all years
     result <- do.call(rbind, all_daily)
     rownames(result) <- NULL

     # Final validation: check all years preserved their totals
     year_totals <- aggregate(cases ~ year, data = result, FUN = sum)
     for (i in seq_along(years)) {
          if (!is.na(annual_cases[i]) && annual_cases[i] > 0) {
               expected <- annual_cases[i]
               actual_row <- year_totals[year_totals$year == years[i], ]
               if (nrow(actual_row) > 0) {
                    actual <- actual_row$cases[1]
                    if (abs(expected - actual) > 1e-6) {
                         stop(sprintf("Final validation failed for year %d: expected %.2f, got %.2f",
                                      years[i], expected, actual))
                    }
               }
          }
     }

     return(result)
}

#' Helper Function to Safely Fit Beta Distribution
#'
#' @description
#' Fits a Beta distribution to data using method of moments estimation,
#' with error handling for edge cases.
#'
#' @param x Numeric vector of proportions in (0,1)
#' @param label Character string for error messages
#'
#' @return List with shape1 and shape2 parameters, or NULL if fitting fails
#' @export
fit_beta_safe <- function(x, label = "") {
     # Remove NA values
     x <- x[!is.na(x)]

     if (length(x) < 2) {
          return(NULL)
     }

     if (sd(x) == 0 || all(x == x[1])) {
          return(NULL)
     }

     # Ensure values are in (0,1)
     x_adj <- x
     x_adj[x_adj <= 0] <- 1e-10
     x_adj[x_adj >= 1] <- 1 - 1e-10

     # Method of moments estimation
     x_mean <- mean(x_adj)
     x_var <- var(x_adj)

     # Check if variance is too small
     if (x_var < 1e-10) {
          return(NULL)
     }

     # Calculate Beta parameters using method of moments
     common_factor <- x_mean * (1 - x_mean) / x_var - 1

     if (common_factor <= 0) {
          # Fall back to fitdistrplus if method of moments fails
          tryCatch({
               if (!requireNamespace("fitdistrplus", quietly = TRUE)) {
                    warning("fitdistrplus package not available for ", label)
                    return(NULL)
               }
               fit <- fitdistrplus::fitdist(x_adj, "beta", method = "mme")
               return(list(
                    shape1 = as.numeric(fit$estimate["shape1"]),
                    shape2 = as.numeric(fit$estimate["shape2"])
               ))
          }, error = function(e) {
               warning("Beta fitting failed for ", label, ": ", e$message)
               return(NULL)
          })
     } else {
          shape1 <- x_mean * common_factor
          shape2 <- (1 - x_mean) * common_factor

          return(list(
               shape1 = shape1,
               shape2 = shape2
          ))
     }
}

#' Helper Function to Fit Beta Distribution with Variance Inflation for est_initial_R
#'
#' @description
#' Fits Beta distribution using CI expansion method with variance inflation.
#' Uses sqrt(variance_inflation) for CI expansion since variance scales as the square
#' of standard deviation. Falls back to method of moments with direct variance scaling.
#'
#' @param samples Numeric vector of proportions in (0,1)
#' @param variance_inflation Numeric inflation factor
#' @param label Character string for error messages
#'
#' @return List with shape1 and shape2 parameters, or NULL if fitting fails
fit_beta_with_variance_inflation_R <- function(samples, variance_inflation=0, label = "") {
     # Remove invalid samples
     valid_samples <- samples[!is.na(samples) & samples > 0 & samples < 1]

     if (length(valid_samples) < 2) {
          return(NULL)
     }

     # Calculate sample statistics
     sample_mean <- mean(valid_samples)
     sample_quantiles <- quantile(valid_samples, c(0.025, 0.975))
     ci_lower <- sample_quantiles[1]
     ci_upper <- sample_quantiles[2]

     ci_lower <- pmax(1e-10, ci_lower*(1-variance_inflation))
     ci_upper <- pmin(0.999, ci_upper*(1+variance_inflation))

     # Wrap in tryCatch to handle potential errors
     tryCatch({
          beta_fit <- fit_beta_from_ci(
               mode_val = sample_mean,
               ci_lower = ci_lower,
               ci_upper = ci_upper,
               method = "moment_matching"
          )

          return(list(
               shape1 = beta_fit$shape1,
               shape2 = beta_fit$shape2
          ))
     }, error = function(e) {
          # Fallback to simple method of moments
          sample_var <- var(valid_samples)
          precision <- (sample_mean * (1 - sample_mean) / sample_var) - 1
          precision <- max(2.1, precision)

          return(list(
               shape1 = max(1.01, sample_mean * precision),
               shape2 = max(1.01, (1 - sample_mean) * precision)
          ))
     })
}
