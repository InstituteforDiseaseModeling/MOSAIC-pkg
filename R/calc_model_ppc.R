#' Calculate Posterior Predictive Checks for Best Model
#'
#' Generates posterior predictive samples from the single best-fitting model
#' (highest log-likelihood) to assess model adequacy. Supports both simulation-based
#' (re-running model) and statistical resampling approaches.
#'
#' @param results Data frame or matrix containing calibration results with columns
#'   for likelihood and parameters
#' @param schema Optional calibration_results_schema object from create_results_schema().
#'   If NULL, will attempt to detect structure from results.
#' @param n_replications Integer. Number of predictive samples to generate (default 100)
#' @param method Character. Method for generating samples: "simulation" (re-run model)
#'   or "statistical" (resample from distributions)
#' @param config_base Base configuration list containing model settings and observed data
#' @param PATHS List of file paths from get_paths()
#' @param overdispersion Numeric. Overdispersion parameter for statistical resampling.
#'   If NULL, will estimate from residuals (default NULL)
#' @param parallel Logical. Use parallel processing for simulation method (default TRUE)
#' @param n_cores Integer. Number of cores for parallel execution (default 4)
#' @param verbose Logical. Print progress messages (default TRUE)
#'
#' @return A list of class "mosaic_ppc" containing:
#'   \itemize{
#'     \item \code{observed}: List with observed \code{cases} and \code{deaths} matrices
#'     \item \code{predicted}: Array of predictions [n_reps, n_locations, n_time] for cases and deaths
#'     \item \code{best_model}: Information about the best model (index, likelihood, parameters)
#'     \item \code{method}: Method used for generating samples
#'     \item \code{diagnostics}: Runtime and diagnostic information
#'   }
#'
#' @details
#' This function implements posterior predictive checking for the single best model:
#' 
#' **Simulation Method**: Re-runs the best model multiple times with different random
#' seeds to capture stochastic variability. More accurate but computationally intensive.
#' 
#' **Statistical Method**: Samples from negative binomial distributions centered on
#' the best model's predictions. Much faster but requires appropriate overdispersion
#' parameter.
#' 
#' The function automatically identifies the best model based on maximum likelihood
#' and generates replicate samples to assess model fit quality.
#'
#' @examples
#' \dontrun{
#' # Using schema (recommended)
#' schema <- create_results_schema(config_base)
#' ppc <- calc_model_ppc(
#'   results = calibration_results,
#'   schema = schema,
#'   n_replications = 100,
#'   method = "statistical",
#'   config_base = config_base,
#'   PATHS = PATHS
#' )
#' 
#' # Without schema (auto-detect)
#' ppc <- calc_model_ppc(
#'   results = calibration_results,
#'   n_replications = 50,
#'   method = "simulation",
#'   config_base = config_base,
#'   PATHS = PATHS
#' )
#' }
#'
#' @export
#' @seealso [plot_model_ppc()], [calc_ppc_diagnostics()]
calc_model_ppc <- function(results,
                          schema = NULL,
                          n_replications = 100,
                          method = c("statistical", "simulation"),
                          config_base,
                          PATHS,
                          overdispersion = NULL,
                          parallel = TRUE,
                          n_cores = 4,
                          verbose = TRUE) {
  
  # ============================================================================
  # Input validation
  # ============================================================================
  if (!is.data.frame(results) && !is.matrix(results)) {
    stop("results must be a data.frame or matrix")
  }
  if (is.matrix(results)) {
    results <- as.data.frame(results)
  }
  
  method <- match.arg(method)
  
  if (n_replications < 1) {
    stop("n_replications must be at least 1")
  }
  
  if (missing(config_base) || is.null(config_base)) {
    stop("config_base is required")
  }
  
  if (method == "simulation" && (missing(PATHS) || is.null(PATHS))) {
    stop("PATHS is required for simulation method")
  }
  
  # ============================================================================
  # Schema handling
  # ============================================================================
  if (is.null(schema)) {
    # Auto-detect structure
    if (verbose) cat("No schema provided, auto-detecting structure...\n")
    
    # Look for likelihood column
    likelihood_cols <- grep("^(likelihood|loglik|ll)$", names(results), ignore.case = TRUE)
    if (length(likelihood_cols) == 0) {
      stop("Could not find likelihood column in results")
    }
    col_ll <- likelihood_cols[1]
    
    # Look for seed column
    seed_cols <- grep("^seed$", names(results), ignore.case = TRUE)
    col_seed <- if (length(seed_cols) > 0) seed_cols[1] else NULL
    
    # Assume all other numeric columns are parameters
    numeric_cols <- sapply(results, is.numeric)
    param_cols <- which(numeric_cols)
    param_cols <- setdiff(param_cols, c(col_ll, col_seed))
    
    # Also exclude common metadata columns
    metadata_patterns <- c("^sim$", "^iter$", "^iteration$", "^chain$")
    for (pattern in metadata_patterns) {
      exclude <- grep(pattern, names(results), ignore.case = TRUE)
      param_cols <- setdiff(param_cols, exclude)
    }
    
  } else {
    # Use provided schema
    if (!inherits(schema, "calibration_results_schema")) {
      stop("schema must be a calibration_results_schema object")
    }
    col_ll <- which(names(results) == "likelihood")
    col_seed <- which(names(results) == "seed")
    param_cols <- which(names(results) %in% schema$param_cols)
  }
  
  if (length(param_cols) == 0) {
    stop("No parameter columns identified")
  }
  
  # ============================================================================
  # Extract observed data
  # ============================================================================
  observed <- extract_observed_data(config_base)
  n_locations <- nrow(observed$cases)
  n_time <- ncol(observed$cases)
  
  if (verbose) {
    cat("PPC Setup:\n")
    cat("- Observed data: ", n_locations, " locations x ", n_time, " time points\n", sep = "")
    cat("- Method: ", method, "\n", sep = "")
    cat("- Replications requested: ", n_replications, "\n", sep = "")
  }
  
  # ============================================================================
  # Identify best model
  # ============================================================================
  best_model_info <- identify_best_model(results, col_ll, param_cols, col_seed, verbose)
  
  # ============================================================================
  # Generate predictive samples
  # ============================================================================
  start_time <- Sys.time()
  
  if (method == "simulation") {
    if (verbose) cat("Generating predictive samples via simulation...\n")
    predicted <- generate_ppc_simulation(
      best_model_info = best_model_info,
      n_replications = n_replications,
      config_base = config_base,
      PATHS = PATHS,
      parallel = parallel,
      n_cores = n_cores,
      verbose = verbose
    )
  } else {
    if (verbose) cat("Generating predictive samples via statistical resampling...\n")
    
    # First need to get the best model's predictions
    best_predictions <- run_best_model_once(
      best_model_info = best_model_info,
      config_base = config_base,
      PATHS = PATHS
    )
    
    # Estimate overdispersion if not provided
    if (is.null(overdispersion)) {
      overdispersion <- estimate_overdispersion(
        observed = observed,
        predicted = best_predictions,
        verbose = verbose
      )
    }
    
    predicted <- generate_ppc_statistical(
      best_predictions = best_predictions,
      n_replications = n_replications,
      overdispersion = overdispersion,
      verbose = verbose
    )
  }
  
  runtime <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  
  # ============================================================================
  # Compile results
  # ============================================================================
  result <- list(
    observed = observed,
    predicted = predicted,
    best_model = best_model_info,
    method = method,
    diagnostics = list(
      n_replications = n_replications,
      n_valid = dim(predicted$cases)[1],
      runtime_minutes = runtime,
      overdispersion = if (method == "statistical") overdispersion else NULL,
      parallel_used = if (method == "simulation") parallel else FALSE,
      n_cores_used = if (method == "simulation" && parallel) n_cores else 1
    )
  )
  
  class(result) <- c("mosaic_ppc", "list")
  
  if (verbose) {
    cat("\nPPC generation complete:\n")
    cat("- Best model: index ", best_model_info$index, 
        " (LL = ", sprintf("%.2f", best_model_info$likelihood), ")\n", sep = "")
    cat("- Valid replications: ", result$diagnostics$n_valid, "\n", sep = "")
    cat("- Runtime: ", sprintf("%.2f", runtime), " minutes\n", sep = "")
    if (method == "statistical") {
      cat("- Overdispersion: ", sprintf("%.2f", overdispersion), "\n", sep = "")
    }
  }
  
  return(result)
}


#' Extract Observed Data from Configuration
#'
#' @param config_base Configuration object
#' @return List with cases and deaths matrices
#' @keywords internal
extract_observed_data <- function(config_base) {
  
  # Standard field names
  cases_fields <- c("reported_cases", "observed_cases", "cases")
  deaths_fields <- c("reported_deaths", "observed_deaths", "deaths")
  
  # Find cases
  cases <- NULL
  for (field in cases_fields) {
    if (!is.null(config_base[[field]])) {
      cases <- as.matrix(config_base[[field]])
      break
    }
  }
  
  # Find deaths
  deaths <- NULL
  for (field in deaths_fields) {
    if (!is.null(config_base[[field]])) {
      deaths <- as.matrix(config_base[[field]])
      break
    }
  }
  
  if (is.null(cases)) {
    stop("Could not find observed cases in config_base. Expected fields: ", 
         paste(cases_fields, collapse = ", "))
  }
  
  if (is.null(deaths)) {
    stop("Could not find observed deaths in config_base. Expected fields: ", 
         paste(deaths_fields, collapse = ", "))
  }
  
  # Validate dimensions match
  if (!identical(dim(cases), dim(deaths))) {
    stop("Dimensions of cases and deaths do not match")
  }
  
  return(list(cases = cases, deaths = deaths))
}


#' Identify Best Model from Results
#'
#' @param results Calibration results data frame
#' @param col_ll Column index for likelihood
#' @param param_cols Column indices for parameters
#' @param col_seed Column index for seed (can be NULL)
#' @param verbose Print messages
#' @return List with best model information
#' @keywords internal
identify_best_model <- function(results, col_ll, param_cols, col_seed, verbose = TRUE) {
  
  # Filter valid likelihoods
  valid_idx <- !is.na(results[[col_ll]]) & is.finite(results[[col_ll]])
  if (!any(valid_idx)) {
    stop("No valid likelihood values found")
  }
  
  valid_results <- results[valid_idx, , drop = FALSE]
  
  # Find best
  best_idx_valid <- which.max(valid_results[[col_ll]])
  best_idx_original <- which(valid_idx)[best_idx_valid]
  
  best_row <- valid_results[best_idx_valid, , drop = FALSE]
  
  # Extract information
  info <- list(
    index = best_idx_original,
    likelihood = best_row[[col_ll]],
    parameters = as.numeric(best_row[, param_cols, drop = TRUE]),
    param_names = names(results)[param_cols]
  )
  
  # Add seed if available
  if (!is.null(col_seed) && length(col_seed) > 0) {
    info$seed <- best_row[[col_seed]]
  } else {
    info$seed <- 12345  # Default seed
  }
  
  # Add sim_id if available
  if ("sim" %in% names(best_row)) {
    info$sim_id <- best_row$sim
  } else {
    info$sim_id <- best_idx_original
  }
  
  names(info$parameters) <- info$param_names
  
  if (verbose) {
    cat("Best model identified:\n")
    cat("- Index: ", info$index, "\n", sep = "")
    cat("- Likelihood: ", sprintf("%.3f", info$likelihood), "\n", sep = "")
    cat("- Parameters: ", length(info$parameters), "\n", sep = "")
  }
  
  return(info)
}


#' Run Best Model Once
#'
#' @param best_model_info Best model information
#' @param config_base Base configuration
#' @param PATHS Path information
#' @return List with cases and deaths predictions
#' @keywords internal
run_best_model_once <- function(best_model_info, config_base, PATHS) {
  
  # Build configuration
  param_values <- best_model_info$parameters
  names(param_values) <- best_model_info$param_names
  
  # Use MOSAIC functions to build config
  sampling_flags <- extract_sampling_metadata(config_base)
  current_config <- convert_matrix_to_config(
    param_vector = param_values,
    config_base = config_base,
    sampling_flags = sampling_flags
  )
  
  current_config$seed <- best_model_info$seed
  
  # Run model
  tryCatch({
    lc <- reticulate::import("laser_cholera.metapop.model")
    model <- lc$run_model(paramfile = current_config, quiet = TRUE)
    
    if (!is.null(model) && !is.null(model$results)) {
      return(list(
        cases = as.matrix(model$results$expected_cases),
        deaths = as.matrix(model$results$disease_deaths)
      ))
    } else {
      stop("Model run returned NULL results")
    }
  }, error = function(e) {
    stop("Failed to run best model: ", e$message)
  })
}


#' Generate PPC Samples via Simulation
#'
#' @param best_model_info Best model information
#' @param n_replications Number of replications
#' @param config_base Base configuration
#' @param PATHS Path information
#' @param parallel Use parallel processing
#' @param n_cores Number of cores
#' @param verbose Print progress
#' @return List with cases and deaths prediction arrays
#' @keywords internal
generate_ppc_simulation <- function(best_model_info, n_replications, 
                                   config_base, PATHS,
                                   parallel = TRUE, n_cores = 4, 
                                   verbose = TRUE) {
  
  # Worker function
  run_replication <- function(rep_idx) {
    # Generate unique seed
    base_seed <- as.integer(best_model_info$seed)
    if (is.na(base_seed) || base_seed <= 0) base_seed <- 12345
    rep_seed <- (base_seed + rep_idx * 1000L) %% 2147483647L
    if (rep_seed <= 0) rep_seed <- rep_seed + 1L
    
    # Build configuration
    param_values <- best_model_info$parameters
    names(param_values) <- best_model_info$param_names
    
    tryCatch({
      sampling_flags <- extract_sampling_metadata(config_base)
      current_config <- convert_matrix_to_config(
        param_vector = param_values,
        config_base = config_base,
        sampling_flags = sampling_flags
      )
      current_config$seed <- rep_seed
      
      # Run model
      lc <- reticulate::import("laser_cholera.metapop.model")
      model <- lc$run_model(paramfile = current_config, quiet = TRUE)
      
      if (!is.null(model) && !is.null(model$results)) {
        return(list(
          cases = as.matrix(model$results$expected_cases),
          deaths = as.matrix(model$results$disease_deaths),
          seed = rep_seed,
          rep_idx = rep_idx
        ))
      }
      return(NULL)
    }, error = function(e) {
      if (verbose) message("Replication ", rep_idx, " failed: ", e$message)
      return(NULL)
    })
  }
  
  # Run replications
  if (parallel && n_cores > 1) {
    if (verbose) cat("Running parallel with ", n_cores, " cores...\n", sep = "")
    
    cl <- parallel::makeCluster(n_cores, type = "PSOCK")
    parallel::clusterEvalQ(cl, {
      library(MOSAIC)
      library(reticulate)
    })
    parallel::clusterExport(cl, 
                           c("best_model_info", "config_base", "PATHS", "verbose"),
                           envir = environment())
    
    if (requireNamespace("pbapply", quietly = TRUE)) {
      all_results <- pbapply::pblapply(seq_len(n_replications), 
                                       run_replication, cl = cl)
    } else {
      all_results <- parallel::parLapply(cl, seq_len(n_replications), 
                                         run_replication)
    }
    parallel::stopCluster(cl)
    
  } else {
    if (requireNamespace("pbapply", quietly = TRUE)) {
      all_results <- pbapply::pblapply(seq_len(n_replications), run_replication)
    } else {
      all_results <- lapply(seq_len(n_replications), run_replication)
    }
  }
  
  # Filter valid results
  valid_results <- all_results[!sapply(all_results, is.null)]
  n_valid <- length(valid_results)
  
  if (n_valid == 0) {
    stop("No valid replications generated")
  }
  
  if (verbose && n_valid < n_replications) {
    cat("Warning: Only ", n_valid, " of ", n_replications, 
        " replications succeeded\n", sep = "")
  }
  
  # Build arrays
  dims_cases <- dim(valid_results[[1]]$cases)
  dims_deaths <- dim(valid_results[[1]]$deaths)
  
  cases_array <- array(NA_real_, dim = c(n_valid, dims_cases[1], dims_cases[2]))
  deaths_array <- array(NA_real_, dim = c(n_valid, dims_deaths[1], dims_deaths[2]))
  
  for (i in seq_len(n_valid)) {
    cases_array[i, , ] <- valid_results[[i]]$cases
    deaths_array[i, , ] <- valid_results[[i]]$deaths
  }
  
  return(list(cases = cases_array, deaths = deaths_array))
}


#' Generate PPC Samples via Statistical Resampling
#'
#' @param best_predictions Best model predictions
#' @param n_replications Number of replications
#' @param overdispersion Overdispersion parameter
#' @param verbose Print progress
#' @return List with cases and deaths prediction arrays
#' @keywords internal
generate_ppc_statistical <- function(best_predictions, n_replications,
                                   overdispersion, verbose = TRUE) {
  
  # Get dimensions
  n_locations <- nrow(best_predictions$cases)
  n_time <- ncol(best_predictions$cases)
  
  # Initialize arrays
  cases_array <- array(NA_real_, dim = c(n_replications, n_locations, n_time))
  deaths_array <- array(NA_real_, dim = c(n_replications, n_locations, n_time))
  
  # Generate samples
  for (rep in seq_len(n_replications)) {
    # Cases - use negative binomial to account for overdispersion
    for (loc in seq_len(n_locations)) {
      for (t in seq_len(n_time)) {
        mu_cases <- best_predictions$cases[loc, t]
        if (!is.na(mu_cases) && mu_cases > 0) {
          # Negative binomial parameterization
          size_cases <- mu_cases / overdispersion
          if (size_cases > 0) {
            cases_array[rep, loc, t] <- rnbinom(1, size = size_cases, mu = mu_cases)
          } else {
            cases_array[rep, loc, t] <- 0
          }
        } else {
          cases_array[rep, loc, t] <- 0
        }
        
        mu_deaths <- best_predictions$deaths[loc, t]
        if (!is.na(mu_deaths) && mu_deaths > 0) {
          # Deaths typically have less overdispersion
          size_deaths <- mu_deaths / (overdispersion * 0.5)
          if (size_deaths > 0) {
            deaths_array[rep, loc, t] <- rnbinom(1, size = size_deaths, mu = mu_deaths)
          } else {
            deaths_array[rep, loc, t] <- 0
          }
        } else {
          deaths_array[rep, loc, t] <- 0
        }
      }
    }
    
    if (verbose && rep %% 10 == 0) {
      cat("\rGenerated ", rep, "/", n_replications, " samples", sep = "")
    }
  }
  
  if (verbose) cat("\n")
  
  return(list(cases = cases_array, deaths = deaths_array))
}


#' Estimate Overdispersion from Residuals
#'
#' @param observed Observed data
#' @param predicted Predicted data
#' @param verbose Print messages
#' @return Overdispersion parameter
#' @keywords internal
estimate_overdispersion <- function(observed, predicted, verbose = TRUE) {
  
  # Flatten and align
  obs_cases <- as.numeric(observed$cases)
  pred_cases <- as.numeric(predicted$cases)
  
  # Remove NAs and zeros
  valid <- !is.na(obs_cases) & !is.na(pred_cases) & pred_cases > 0
  obs_cases <- obs_cases[valid]
  pred_cases <- pred_cases[valid]
  
  if (length(obs_cases) < 10) {
    if (verbose) cat("Insufficient data for overdispersion estimation, using default = 2\n")
    return(2.0)
  }
  
  # Calculate variance/mean ratio
  residuals <- obs_cases - pred_cases
  squared_residuals <- residuals^2
  
  # Estimate overdispersion as variance/mean ratio
  # For negative binomial: Var = mu + mu^2/k, so overdispersion = mu/k
  overdispersion <- mean(squared_residuals) / mean(pred_cases)
  
  # Bound it to reasonable range
  overdispersion <- max(0.5, min(overdispersion, 10))
  
  if (verbose) {
    cat("Estimated overdispersion: ", sprintf("%.2f", overdispersion), "\n", sep = "")
  }
  
  return(overdispersion)
}


#' Calculate PPC Diagnostics
#'
#' Computes diagnostic metrics comparing observed data to posterior predictive samples
#'
#' @param ppc_results Results from calc_model_ppc()
#' @param credible_levels Credible interval levels to evaluate (default c(0.5, 0.8, 0.95))
#' 
#' @return List of diagnostic metrics including coverage, RMSE, and calibration
#' @export
calc_ppc_diagnostics <- function(ppc_results, credible_levels = c(0.5, 0.8, 0.95)) {
  
  if (!inherits(ppc_results, "mosaic_ppc")) {
    stop("ppc_results must be output from calc_model_ppc()")
  }
  
  observed <- ppc_results$observed
  predicted <- ppc_results$predicted
  
  # Calculate coverage for each credible level
  coverage_cases <- numeric(length(credible_levels))
  coverage_deaths <- numeric(length(credible_levels))
  
  for (i in seq_along(credible_levels)) {
    level <- credible_levels[i]
    alpha <- (1 - level) / 2
    
    # Cases
    lower_cases <- apply(predicted$cases, c(2, 3), quantile, probs = alpha, na.rm = TRUE)
    upper_cases <- apply(predicted$cases, c(2, 3), quantile, probs = 1 - alpha, na.rm = TRUE)
    in_interval_cases <- observed$cases >= lower_cases & observed$cases <= upper_cases
    coverage_cases[i] <- mean(in_interval_cases, na.rm = TRUE)
    
    # Deaths
    lower_deaths <- apply(predicted$deaths, c(2, 3), quantile, probs = alpha, na.rm = TRUE)
    upper_deaths <- apply(predicted$deaths, c(2, 3), quantile, probs = 1 - alpha, na.rm = TRUE)
    in_interval_deaths <- observed$deaths >= lower_deaths & observed$deaths <= upper_deaths
    coverage_deaths[i] <- mean(in_interval_deaths, na.rm = TRUE)
  }
  
  # Calculate RMSE
  mean_pred_cases <- apply(predicted$cases, c(2, 3), mean, na.rm = TRUE)
  mean_pred_deaths <- apply(predicted$deaths, c(2, 3), mean, na.rm = TRUE)
  
  rmse_cases <- sqrt(mean((observed$cases - mean_pred_cases)^2, na.rm = TRUE))
  rmse_deaths <- sqrt(mean((observed$deaths - mean_pred_deaths)^2, na.rm = TRUE))
  
  # Calculate mean absolute error
  mae_cases <- mean(abs(observed$cases - mean_pred_cases), na.rm = TRUE)
  mae_deaths <- mean(abs(observed$deaths - mean_pred_deaths), na.rm = TRUE)
  
  # Compile diagnostics
  diagnostics <- list(
    coverage = data.frame(
      level = credible_levels,
      cases = coverage_cases,
      deaths = coverage_deaths
    ),
    rmse = list(cases = rmse_cases, deaths = rmse_deaths),
    mae = list(cases = mae_cases, deaths = mae_deaths),
    n_observations = list(
      cases = sum(!is.na(observed$cases)),
      deaths = sum(!is.na(observed$deaths))
    )
  )
  
  class(diagnostics) <- c("mosaic_ppc_diagnostics", "list")
  return(diagnostics)
}


#' Print Method for PPC Results
#' @param x mosaic_ppc object
#' @param ... Additional arguments (unused)
#' @export
print.mosaic_ppc <- function(x, ...) {
  cat("MOSAIC Posterior Predictive Check Results\n")
  cat("==========================================\n")
  cat("Method: ", x$method, "\n")
  cat("Best model likelihood: ", sprintf("%.2f", x$best_model$likelihood), "\n")
  cat("Replications: ", x$diagnostics$n_valid, "\n")
  cat("Runtime: ", sprintf("%.2f", x$diagnostics$runtime_minutes), " minutes\n")
  if (x$method == "statistical" && !is.null(x$diagnostics$overdispersion)) {
    cat("Overdispersion: ", sprintf("%.2f", x$diagnostics$overdispersion), "\n")
  }
  cat("\nData dimensions:\n")
  cat("- Locations: ", nrow(x$observed$cases), "\n")
  cat("- Time points: ", ncol(x$observed$cases), "\n")
  invisible(x)
}


#' Print Method for PPC Diagnostics
#' @param x mosaic_ppc_diagnostics object
#' @param ... Additional arguments (unused)
#' @export
print.mosaic_ppc_diagnostics <- function(x, ...) {
  cat("PPC Diagnostic Metrics\n")
  cat("======================\n")
  cat("\nCoverage (observed in credible intervals):\n")
  print(x$coverage)
  cat("\nRMSE:\n")
  cat("- Cases: ", sprintf("%.2f", x$rmse$cases), "\n")
  cat("- Deaths: ", sprintf("%.2f", x$rmse$deaths), "\n")
  cat("\nMAE:\n")
  cat("- Cases: ", sprintf("%.2f", x$mae$cases), "\n")
  cat("- Deaths: ", sprintf("%.2f", x$mae$deaths), "\n")
  invisible(x)
}