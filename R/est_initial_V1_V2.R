#' Estimate Initial V1 and V2 Compartments for All Locations
#'
#' Estimates initial V1 (one-dose) and V2 (two-dose) vaccination compartment values
#' for all locations specified in a config, with uncertainty quantification through
#' Monte Carlo sampling from prior distributions. Returns results formatted as a
#' priors-like object with Beta distribution parameters for V1/N and V2/N proportions.
#'
#' @param PATHS List object from get_paths() containing data directory paths
#' @param priors Prior distributions object (e.g., from priors_default or custom)
#' @param config Configuration object containing location names (location_name or location_codes) and parameters
#' @param n_samples Integer, number of Monte Carlo samples for uncertainty (default 1000)
#' @param t0 Date object for initial condition estimation (default from config$date_start or config$t0)
#' @param verbose Logical, whether to print progress messages (default TRUE)
#' @param parallel Logical, whether to use parallel processing (default FALSE).
#'        When TRUE, automatically detects cores and handles platform differences.
#' @param variance_inflation Numeric factor to inflate variance of fitted Beta distributions (default 1 = no inflation).
#'        Values > 1 increase uncertainty while preserving the mean. For example, 2 doubles the variance.
#'
#' @return A list with priors_default-compatible structure containing:
#' \describe{
#'   \item{metadata}{Information about the estimation run including summary statistics}
#'   \item{parameters_location}{Parameter-first hierarchy matching priors_default:
#'     \itemize{
#'       \item prop_V1_initial: Beta distribution parameters for V1/N by location
#'       \item prop_V2_initial: Beta distribution parameters for V2/N by location
#'     }
#'     Each parameter contains $parameters$location$[ISO_CODE] with shape1, shape2, and metadata
#'   }
#' }
#'
#' @details
#' The function performs the following steps for each location:
#' 1. Loads vaccination and population data
#' 2. Runs Monte Carlo simulation sampling from priors for omega and phi
#' 3. Calculates V1 and V2 for each sample
#' 4. Fits Beta distributions to V1/N and V2/N proportions
#' 5. Compiles results in standardized format
#'
#' @examples
#' \dontrun{
#' # Standard usage
#' PATHS <- get_paths()
#' initial_conditions <- est_initial_V1_V2(
#'   PATHS = PATHS,
#'   priors = priors_default,
#'   config = config_example,
#'   n_samples = 1000
#' )
#'
#' # Access results for a location (priors_default-compatible structure)
#' eth_v1_params <- initial_conditions$parameters_location$prop_V1_initial$parameters$location$ETH
#' eth_v1_shape1 <- eth_v1_params$shape1
#' eth_v1_mean <- eth_v1_params$metadata$mean
#' }
#'
#' @importFrom parallel mclapply detectCores
#' @importFrom fitdistrplus fitdist
#' @importFrom stats rbeta rgamma quantile sd
#' @export

est_initial_V1_V2 <- function(PATHS,
                             priors,
                             config,
                             n_samples = 1000,
                             t0 = NULL,
                             verbose = TRUE,
                             parallel = FALSE,
                             variance_inflation = 1) {

  # Input validation
  if (!is.list(PATHS)) {
    stop("PATHS must be a list from get_paths()")
  }

  if (!is.list(priors)) {
    stop("priors must be a priors object")
  }

  if (!is.list(config)) {
    stop("config must be a configuration object")
  }

  # Extract t0 from config if not provided
  if (is.null(t0)) {
    # Check for date_start (standard MOSAIC config) or t0 (backwards compatibility)
    if (!is.null(config$date_start)) {
      t0 <- as.Date(config$date_start)
    } else if (!is.null(config$t0)) {
      t0 <- as.Date(config$t0)
    } else {
      stop("t0 must be provided or available in config$date_start (or config$t0)")
    }
    if (is.na(t0)) {
      stop("Invalid date in config$date_start (or config$t0)")
    }
  } else {
    t0 <- as.Date(t0)
  }

  # Get location codes from config (supports both location_name and location_codes)
  if (!is.null(config$location_name)) {
    location_codes <- config$location_name
  } else if (!is.null(config$location_codes)) {
    location_codes <- config$location_codes
  } else {
    stop("No location codes found in config (checked location_name and location_codes)")
  }

  if (length(location_codes) == 0) {
    stop("Location list is empty in config")
  }

  if (verbose) {
    cat("Processing", length(location_codes), "locations with",
        n_samples, "samples each\n")
    cat("t0 =", as.character(t0), "\n")
    if (variance_inflation != 1) {
      cat("Variance inflation factor:", variance_inflation, "\n")
    }
    cat("\n")
  }

  # Load data using PATHS
  vacc_file <- file.path(PATHS$MODEL_INPUT, 'data_vaccinations_GTFCC_redistributed.csv')
  if (!file.exists(vacc_file)) {
    vacc_file <- file.path(PATHS$MODEL_INPUT, 'data_vaccinations_GTFCC_WHO_redistributed.csv')
  }
  if (!file.exists(vacc_file)) {
    stop("Vaccination data file not found")
  }
  vaccination_data <- read.csv(vacc_file)

  pop_file <- file.path(PATHS$DATA_DEMOGRAPHICS, 'UN_world_population_prospects_daily.csv')
  if (!file.exists(pop_file)) {
    stop("Population data file not found")
  }
  population_data <- read.csv(pop_file)

  # Extract prior distributions for omega and phi
  omega1_prior <- priors$parameters_global$omega_1
  omega2_prior <- priors$parameters_global$omega_2
  phi1_prior <- priors$parameters_global$phi_1
  phi2_prior <- priors$parameters_global$phi_2

  # Check prior distributions exist
  if (is.null(omega1_prior) || is.null(omega2_prior) ||
      is.null(phi1_prior) || is.null(phi2_prior)) {
    stop("Required prior distributions (omega_1, omega_2, phi_1, phi_2) not found")
  }

  # Function to process a single location
  process_location <- function(iso_code, variance_inflation) {

    # Progress will be handled outside this function

    # Filter data for this location
    doses_loc <- vaccination_data[vaccination_data$iso_code == iso_code, ]
    pop_loc <- population_data[population_data$iso_code == iso_code, ]

    if (nrow(doses_loc) == 0) {
      warning("No vaccination data for ", iso_code)
      return(NULL)
    }

    if (nrow(pop_loc) == 0) {
      warning("No population data for ", iso_code)
      return(NULL)
    }

    # Get population at t0
    pop_loc$date <- as.Date(pop_loc$date)
    N_at_t0 <- pop_loc$total_population[which.min(abs(pop_loc$date - t0))]

    if (is.na(N_at_t0) || N_at_t0 <= 0) {
      warning("Invalid population for ", iso_code, " at t0")
      return(NULL)
    }

    # Monte Carlo sampling
    v1_results <- numeric(n_samples)
    v2_results <- numeric(n_samples)

    for (i in 1:n_samples) {
      # Sample from priors
      if (omega1_prior$distribution == "gamma") {
        omega1_sample <- rgamma(1,
                               shape = omega1_prior$parameters$shape,
                               rate = omega1_prior$parameters$rate)
      } else {
        stop("Unsupported distribution for omega_1: ", omega1_prior$distribution)
      }

      if (omega2_prior$distribution == "gamma") {
        omega2_sample <- rgamma(1,
                               shape = omega2_prior$parameters$shape,
                               rate = omega2_prior$parameters$rate)
      } else {
        stop("Unsupported distribution for omega_2: ", omega2_prior$distribution)
      }

      if (phi1_prior$distribution == "beta") {
        phi1_sample <- rbeta(1,
                            shape1 = phi1_prior$parameters$shape1,
                            shape2 = phi1_prior$parameters$shape2)
      } else {
        stop("Unsupported distribution for phi_1: ", phi1_prior$distribution)
      }

      if (phi2_prior$distribution == "beta") {
        phi2_sample <- rbeta(1,
                            shape1 = phi2_prior$parameters$shape1,
                            shape2 = phi2_prior$parameters$shape2)
      } else {
        stop("Unsupported distribution for phi_2: ", phi2_prior$distribution)
      }

      # Run estimation with sampled parameters
      # Use N_at_t0 which was already calculated above
      result <- est_initial_V1_V2_location(
        dates = doses_loc$date,
        doses = doses_loc$doses_distributed,
        N = N_at_t0,  # Use the scalar population at t0
        t0 = t0,
        omega1 = omega1_sample,
        omega2 = omega2_sample,
        phi1 = phi1_sample,
        phi2 = phi2_sample,
        t_lag = 14,
        min_interdose_days = 40,
        vacc_ceiling_frac = 0.6,
        allocation_logic = "LIFO"
      )

      v1_results[i] <- result["V1"]
      v2_results[i] <- result["V2"]
    }

    # Calculate proportions
    v1_proportions <- v1_results / N_at_t0
    v2_proportions <- v2_results / N_at_t0

    # Fit Beta distributions to proportions with variance inflation
    v1_beta_fit <- fit_beta_with_variance_inflation_V(
        samples = v1_proportions,
        variance_inflation = variance_inflation,
        label = "V1/N"
    )
    v2_beta_fit <- fit_beta_with_variance_inflation_V(
        samples = v2_proportions,
        variance_inflation = variance_inflation,
        label = "V2/N"
    )

    # Compile results
    result <- list(
      location_code = iso_code,
      t0 = t0,
      population = N_at_t0,

      # V1 count statistics
      V1_count = list(
        mean = mean(v1_results),
        median = median(v1_results),
        sd = sd(v1_results),
        ci_lower = quantile(v1_results, 0.025),
        ci_upper = quantile(v1_results, 0.975),
        samples = v1_results  # Keep samples for downstream use
      ),

      # V2 count statistics
      V2_count = list(
        mean = mean(v2_results),
        median = median(v2_results),
        sd = sd(v2_results),
        ci_lower = quantile(v2_results, 0.025),
        ci_upper = quantile(v2_results, 0.975),
        samples = v2_results
      ),

      # V1 proportion Beta parameters
      V1_proportion = list(
        parameter_name = paste0("V1_proportion_", iso_code),
        distribution = "beta",
        parameters = if (!is.null(v1_beta_fit)) {
          list(shape1 = v1_beta_fit$shape1, shape2 = v1_beta_fit$shape2)
        } else {
          list(shape1 = NA, shape2 = NA)
        },
        mean = mean(v1_proportions),
        ci_lower = quantile(v1_proportions, 0.025),
        ci_upper = quantile(v1_proportions, 0.975)
      ),

      # V2 proportion Beta parameters
      V2_proportion = list(
        parameter_name = paste0("V2_proportion_", iso_code),
        distribution = "beta",
        parameters = if (!is.null(v2_beta_fit)) {
          list(shape1 = v2_beta_fit$shape1, shape2 = v2_beta_fit$shape2)
        } else {
          list(shape1 = NA, shape2 = NA)
        },
        mean = mean(v2_proportions),
        ci_lower = quantile(v2_proportions, 0.025),
        ci_upper = quantile(v2_proportions, 0.975)
      )
    )

    return(result)
  }

  # Helper function to safely fit Beta distribution
  fit_beta_safe <- function(x, label = "") {
    if (sd(x) == 0 || all(x == x[1])) {
      return(NULL)
    }

    # Ensure values are in (0,1)
    x_adj <- x
    x_adj[x_adj <= 0] <- 1e-10
    x_adj[x_adj >= 1] <- 1 - 1e-10

    tryCatch({
      fit <- fitdistrplus::fitdist(x_adj, "beta", method = "mme")
      return(list(
        shape1 = fit$estimate["shape1"],
        shape2 = fit$estimate["shape2"]
      ))
    }, error = function(e) {
      warning("Beta fitting failed for ", label, ": ", e$message)
      return(NULL)
    })
  }

  # Helper function to fit Beta distribution with variance inflation for V1/V2
  fit_beta_with_variance_inflation_V <- function(samples, variance_inflation, label = "") {
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

    # Apply variance inflation using the new pattern
    ci_lower <- pmax(1e-10, ci_lower*(1-variance_inflation))
    ci_upper <- pmin(0.999, ci_upper*(1+variance_inflation))

    # Use fit_beta_from_ci with sample_mean as mode_val
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
      # Simple fallback using method of moments
      sample_var <- var(valid_samples)
      precision <- (sample_mean * (1 - sample_mean) / sample_var) - 1
      precision <- max(2.1, precision)

      return(list(
        shape1 = max(0.01, sample_mean * precision),
        shape2 = max(0.01, (1 - sample_mean) * precision)
      ))
    })
  }

  # Process all locations with platform-specific parallel processing
  if (parallel && length(location_codes) > 1) {
    # Check platform and use appropriate parallel method
    if (.Platform$OS.type == "windows") {
      # Windows doesn't support mclapply, use sequential with progress bar
      if (verbose) {
        cat("Note: Parallel processing not available on Windows, using sequential processing\n")
        cat("Processing", length(location_codes), "locations:\n")
        pb <- txtProgressBar(min = 0, max = length(location_codes), style = 3)
      }

      results_list <- list()
      for (i in seq_along(location_codes)) {
        if (verbose) {
          setTxtProgressBar(pb, i)
        }
        results_list[[i]] <- process_location(location_codes[i], variance_inflation)
      }

      if (verbose) {
        close(pb)
        cat("\n")
      }
    } else {
      # Unix-based systems (Mac, Linux) can use mclapply
      n_cores <- parallel::detectCores()
      cores_to_use <- max(1, n_cores - 1)  # Leave one core free
      if (verbose) {
        cat("Using parallel processing with", cores_to_use, "cores\n")
        cat("Processing", length(location_codes), "locations (progress bar not available in parallel mode)...\n")
      }
      results_list <- parallel::mclapply(location_codes, function(x) process_location(x, variance_inflation),
                                        mc.cores = cores_to_use)
    }
  } else {
    # Sequential processing with progress bar
    if (verbose) {
      cat("Processing", length(location_codes), "locations:\n")
      pb <- txtProgressBar(min = 0, max = length(location_codes), style = 3)
    }

    results_list <- list()
    for (i in seq_along(location_codes)) {
      if (verbose) {
        setTxtProgressBar(pb, i)
      }
      results_list[[i]] <- process_location(location_codes[i], variance_inflation)
    }

    if (verbose) {
      close(pb)
      cat("\n")
    }
  }

  # Remove NULL results
  results_list <- results_list[!sapply(results_list, is.null)]

  # Format output to match priors_default structure (parameter-first hierarchy)
  output <- list(
    metadata = list(
      description = "Initial V1 and V2 vaccination compartment estimates",
      version = "1.0.0",
      date = as.character(Sys.Date()),
      initial_conditions_V1_V2 = list(
        estimated_date = as.character(Sys.Date()),
        method = "est_initial_V1_V2",
        n_samples = n_samples,
        t0 = as.character(t0),
        n_locations_processed = length(results_list),
        locations_processed = sapply(results_list, function(x) x$location_code),
        priors_used = list(
          omega_1 = omega1_prior$parameter_name,
          omega_2 = omega2_prior$parameter_name,
          phi_1 = phi1_prior$parameter_name,
          phi_2 = phi2_prior$parameter_name
        )
      )
    ),

    # Match priors_default structure: parameter -> location hierarchy
    parameters_location = list(
      prop_V1_initial = list(
        parameter_name = "prop_V1_initial",
        description = "Initial proportion of population with one vaccine dose",
        distribution = "beta",
        parameters = list(
          location = list()  # Will be populated below
        )
      ),

      prop_V2_initial = list(
        parameter_name = "prop_V2_initial",
        description = "Initial proportion of population with two vaccine doses",
        distribution = "beta",
        parameters = list(
          location = list()  # Will be populated below
        )
      )
    )
  )

  # Populate location-specific parameters in the priors_default-compatible structure
  for (i in seq_along(results_list)) {
    loc_data <- results_list[[i]]
    loc <- loc_data$location_code

    # Add V1 proportion for this location
    if (!is.null(loc_data$V1_proportion$parameters) &&
        !is.na(loc_data$V1_proportion$parameters$shape1)) {

      # Extract Beta parameters for V1 (variance inflation already applied during fitting)
      alpha_v1 <- as.numeric(loc_data$V1_proportion$parameters$shape1)
      beta_v1 <- as.numeric(loc_data$V1_proportion$parameters$shape2)

      output$parameters_location$prop_V1_initial$parameters$location[[loc]] <- list(
        shape1 = alpha_v1,
        shape2 = beta_v1,
        metadata = list(
          estimated_from_data = TRUE,
          mean = loc_data$V1_proportion$mean,
          ci_lower = loc_data$V1_proportion$ci_lower,
          ci_upper = loc_data$V1_proportion$ci_upper,
          t0 = as.character(loc_data$t0),
          population_at_t0 = loc_data$population,
          count_mean = loc_data$V1_count$mean,
          count_ci = c(loc_data$V1_count$ci_lower, loc_data$V1_count$ci_upper),
          variance_inflation = variance_inflation
        )
      )
    } else {
      # Include NA values for locations without data
      output$parameters_location$prop_V1_initial$parameters$location[[loc]] <- list(
        shape1 = NA,
        shape2 = NA,
        metadata = list(
          estimated_from_data = FALSE,
          note = "No vaccination data available for estimation"
        )
      )
    }

    # Add V2 proportion for this location
    if (!is.null(loc_data$V2_proportion$parameters) &&
        !is.na(loc_data$V2_proportion$parameters$shape1)) {

      # Extract Beta parameters for V2 (variance inflation already applied during fitting)
      alpha_v2 <- as.numeric(loc_data$V2_proportion$parameters$shape1)
      beta_v2 <- as.numeric(loc_data$V2_proportion$parameters$shape2)

      output$parameters_location$prop_V2_initial$parameters$location[[loc]] <- list(
        shape1 = alpha_v2,
        shape2 = beta_v2,
        metadata = list(
          estimated_from_data = TRUE,
          mean = loc_data$V2_proportion$mean,
          ci_lower = loc_data$V2_proportion$ci_lower,
          ci_upper = loc_data$V2_proportion$ci_upper,
          t0 = as.character(loc_data$t0),
          population_at_t0 = loc_data$population,
          count_mean = loc_data$V2_count$mean,
          count_ci = c(loc_data$V2_count$ci_lower, loc_data$V2_count$ci_upper),
          variance_inflation = variance_inflation
        )
      )
    } else {
      output$parameters_location$prop_V2_initial$parameters$location[[loc]] <- list(
        shape1 = NA,
        shape2 = NA,
        metadata = list(
          estimated_from_data = FALSE,
          note = "No two-dose vaccination data or all zeros"
        )
      )
    }
  }

  # Add summary statistics
  output$metadata$summary <- list(
    total_V1 = sum(sapply(results_list, function(x) x$V1_count$mean), na.rm = TRUE),
    total_V2 = sum(sapply(results_list, function(x) x$V2_count$mean), na.rm = TRUE),
    locations_with_V1 = sum(sapply(output$parameters_location$prop_V1_initial$parameters$location,
                                  function(x) !is.na(x$shape1))),
    locations_with_V2 = sum(sapply(output$parameters_location$prop_V2_initial$parameters$location,
                                  function(x) !is.na(x$shape1) && x$shape1 > 0))
  )

  class(output) <- c("mosaic_initial_conditions", "list")

  if (verbose) {
    cat("\nCompleted processing", length(results_list), "locations\n")
    cat("Locations with V1 > 0:", output$metadata$summary$locations_with_V1, "\n")
    cat("Locations with V2 > 0:", output$metadata$summary$locations_with_V2, "\n")
  }

  return(output)
}

#' Print method for mosaic_initial_conditions
#' @export
print.mosaic_initial_conditions <- function(x, ...) {
  cat("MOSAIC Initial V1/V2 Conditions (priors_default-compatible format)\n")
  cat("==================================================================\n")
  cat("t0:", x$metadata$initial_conditions_V1_V2$t0, "\n")
  cat("Locations processed:", x$metadata$initial_conditions_V1_V2$n_locations_processed, "\n")
  cat("Samples per location:", x$metadata$initial_conditions_V1_V2$n_samples, "\n\n")

  # Summary statistics
  cat("Summary:\n")
  cat("  Total V1 across all locations:", format(round(x$metadata$summary$total_V1), big.mark = ","), "\n")
  cat("  Total V2 across all locations:", format(round(x$metadata$summary$total_V2), big.mark = ","), "\n")
  cat("  Locations with V1 > 0:", x$metadata$summary$locations_with_V1, "\n")
  cat("  Locations with V2 > 0:", x$metadata$summary$locations_with_V2, "\n\n")

  # Summary table for locations with data
  locs <- names(x$parameters_location$prop_V1_initial$parameters$location)
  if (length(locs) > 0) {
    summary_data <- lapply(locs[1:min(10, length(locs))], function(loc) {
      v1_data <- x$parameters_location$prop_V1_initial$parameters$location[[loc]]
      v2_data <- x$parameters_location$prop_V2_initial$parameters$location[[loc]]

      if (!is.na(v1_data$shape1)) {
        data.frame(
          Location = loc,
          V1_mean_pct = round(v1_data$metadata$mean * 100, 2),
          V1_count = round(v1_data$metadata$count_mean),
          V2_mean_pct = ifelse(is.na(v2_data$shape1), 0, round(v2_data$metadata$mean * 100, 4)),
          V2_count = ifelse(is.na(v2_data$shape1), 0, round(v2_data$metadata$count_mean))
        )
      } else {
        NULL
      }
    })

    summary_df <- do.call(rbind, summary_data[!sapply(summary_data, is.null)])
    if (nrow(summary_df) > 0) {
      cat("Location-specific estimates (showing first 10):\n")
      print(summary_df, row.names = FALSE)
    }
  }
}
