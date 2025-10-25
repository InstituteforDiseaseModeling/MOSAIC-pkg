#' Get NPE Simulated Data from LASER Model Output
#'
#' Extracts simulated outbreak data from a LASER model result object and converts
#' it to the same long-format data frame structure as \code{\link{get_npe_observed_data}}.
#' This ensures consistency between observed and simulated data formats for NPE workflows.
#'
#' @param laser_result A LASER model result object returned from \code{run_LASER_model()}
#'   or direct Python laser_cholera execution. Must contain results$expected_cases.
#' @param include_deaths Logical; whether to include deaths column in output if
#'   available in laser_result. Default is FALSE since NPE is typically trained
#'   on cases only.
#' @param validate Logical; whether to validate the output data structure.
#'   Default is TRUE. Checks for required columns and data types.
#' @param verbose Logical; whether to print informative messages about data
#'   processing. Default is FALSE.
#'
#' @return A data frame in long format with columns:
#'   \itemize{
#'     \item \code{j} — Location identifier (character)
#'     \item \code{t} — Time index (numeric)
#'     \item \code{cases} — Simulated case counts (numeric)
#'     \item \code{deaths} — Simulated death counts (numeric, optional)
#'   }
#'
#' @details
#' This function provides the simulated data counterpart to \code{\link{get_npe_observed_data}},
#' ensuring that both observed and simulated data use identical formats. This is essential
#' for:
#' \itemize{
#'   \item Comparing simulated vs observed data
#'   \item Computing likelihoods
#'   \item NPE training with simulated data
#'   \item Posterior predictive checks
#' }
#'
#' The function handles both R and Python LASER result objects, automatically
#' converting Python numpy arrays to R matrices when necessary.
#'
#' @section Location Extraction:
#' Location identifiers are extracted from laser_result$params in the following order:
#' \enumerate{
#'   \item laser_result$params$location_name
#'   \item laser_result$params$iso_code
#'   \item laser_result$params$location_code
#'   \item Numeric indices as fallback ("1", "2", "3", ...)
#' }
#'
#' @examples
#' \dontrun{
#' # Run LASER model
#' laser_result <- run_LASER_model(
#'   paramfile = config,
#'   seed = 123
#' )
#'
#' # Extract simulated data in NPE format
#' simulated_data <- get_npe_simulated_data(laser_result)
#'
#' # Compare with observed data format
#' observed_data <- get_npe_observed_data(config)
#'
#' # Both have identical structure:
#' # j   t  cases
#' # ETH 1    10
#' # ETH 2    15
#' # ...
#'
#' # Use for NPE inference
#' posterior <- est_npe_posterior(
#'   model_dir = "path/to/npe",
#'   observed_data = simulated_data,  # Can use simulated data
#'   n_samples = 10000
#' )
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{get_npe_observed_data}} — Parallel function for observed data
#'   \item \code{\link{run_LASER_model}} — Generates laser_result objects
#'   \item \code{\link{extract_laser_output}} — Lower-level extraction function
#'   \item \code{\link{est_npe_posterior}} — Uses this data format for inference
#' }
#'
#' @export
get_npe_simulated_data <- function(
    laser_result,
    include_deaths = FALSE,
    validate = TRUE,
    verbose = FALSE
) {

    # Helper function for logging
    log_msg <- function(msg, ...) {
        if (verbose) {
            message(sprintf(msg, ...))
        }
    }

    # =============================================================================
    # VALIDATE LASER RESULT
    # =============================================================================

    if (is.null(laser_result)) {
        stop("laser_result cannot be NULL")
    }

    # Handle both R list structure and Python object structure
    results <- NULL

    # Try to access results field
    if (!is.null(laser_result$results)) {
        results <- laser_result$results
    } else if (!is.null(laser_result[["results"]])) {
        results <- laser_result[["results"]]
    } else {
        # Try Python-style access
        tryCatch({
            results <- reticulate::py_get_attr(laser_result, "results")
        }, error = function(e) {
            stop("Cannot access 'results' field in laser_result: ", e$message)
        })
    }

    if (is.null(results)) {
        stop("laser_result must contain 'results' field")
    }

    # =============================================================================
    # EXTRACT CASES DATA
    # =============================================================================

    cases_data <- NULL

    # Try different access patterns for expected_cases
    if (!is.null(results$expected_cases)) {
        cases_data <- results$expected_cases
    } else if (!is.null(results[["expected_cases"]])) {
        cases_data <- results[["expected_cases"]]
    } else {
        # Try Python-style access
        tryCatch({
            cases_data <- reticulate::py_get_attr(results, "expected_cases")
        }, error = function(e) {
            stop("Cannot access 'expected_cases' in results: ", e$message)
        })
    }

    if (is.null(cases_data)) {
        stop("laser_result must contain 'results$expected_cases'")
    }

    # Convert Python numpy array to R if needed
    if (inherits(cases_data, "python.builtin.object")) {
        cases_data <- reticulate::py_to_r(cases_data)
    }

    # Ensure matrix format
    if (is.vector(cases_data) && !is.matrix(cases_data)) {
        # Single location case - convert to matrix
        log_msg("Processing single location simulation data")
        cases_data <- matrix(cases_data, nrow = 1)
        n_locations <- 1
        n_timesteps <- length(cases_data)
    } else if (is.matrix(cases_data)) {
        n_locations <- nrow(cases_data)
        n_timesteps <- ncol(cases_data)
        log_msg("Processing %d location(s) with %d time points", n_locations, n_timesteps)
    } else {
        stop("expected_cases must be a vector or matrix")
    }

    # =============================================================================
    # EXTRACT LOCATION IDENTIFIERS
    # =============================================================================

    location_codes <- NULL
    params <- NULL

    # Try to access params field
    if (!is.null(laser_result$params)) {
        params <- laser_result$params
    } else if (!is.null(laser_result[["params"]])) {
        params <- laser_result[["params"]]
    } else {
        # Try Python-style access
        tryCatch({
            params <- reticulate::py_get_attr(laser_result, "params")
        }, error = function(e) {
            params <- NULL
        })
    }

    if (!is.null(params)) {
        # Try different field names in params
        if (!is.null(params$location_name)) {
            location_codes <- params$location_name
            log_msg("Using location codes from laser_result$params$location_name")
        } else if (!is.null(params$iso_code)) {
            location_codes <- params$iso_code
            log_msg("Using location codes from laser_result$params$iso_code")
        } else if (!is.null(params$location_code)) {
            location_codes <- params$location_code
            log_msg("Using location codes from laser_result$params$location_code")
        }
    }

    # Validate location codes
    if (!is.null(location_codes)) {
        if (length(location_codes) != n_locations) {
            warning(sprintf(
                "Number of location codes (%d) doesn't match number of locations in data (%d). Using numeric indices.",
                length(location_codes), n_locations
            ))
            location_codes <- as.character(seq_len(n_locations))
        } else {
            location_codes <- as.character(location_codes)
        }
    } else {
        # No location identifiers found - use numeric indices
        location_codes <- as.character(seq_len(n_locations))
        log_msg("No location identifiers found. Using numeric indices: %s",
                paste(location_codes, collapse = ", "))
    }

    # =============================================================================
    # DETERMINE TIME INDICES
    # =============================================================================

    # Always use 1:n_timesteps for consistency
    time_index <- seq_len(n_timesteps)

    # =============================================================================
    # CREATE LONG-FORMAT DATA FRAME
    # =============================================================================

    log_msg("Converting to long format...")

    # Initialize list to store each location's data
    result_list <- list()

    for (i in seq_len(n_locations)) {
        location_data <- data.frame(
            j = location_codes[i],
            t = time_index,
            cases = as.numeric(cases_data[i, ])
        )

        # Add deaths if requested and available
        if (include_deaths) {
            deaths_data <- NULL

            # Try different access patterns for disease_deaths
            if (!is.null(results$disease_deaths)) {
                deaths_data <- results$disease_deaths
            } else if (!is.null(results[["disease_deaths"]])) {
                deaths_data <- results[["disease_deaths"]]
            } else {
                # Try Python-style access
                tryCatch({
                    deaths_data <- reticulate::py_get_attr(results, "disease_deaths")
                }, error = function(e) {
                    deaths_data <- NULL
                })
            }

            if (!is.null(deaths_data)) {
                # Convert Python numpy array to R if needed
                if (inherits(deaths_data, "python.builtin.object")) {
                    deaths_data <- reticulate::py_to_r(deaths_data)
                }

                # Validate deaths data structure
                if (is.vector(deaths_data) && !is.matrix(deaths_data)) {
                    if (n_locations == 1) {
                        deaths_data <- matrix(deaths_data, nrow = 1)
                    } else {
                        warning("Deaths data is vector but multiple locations present. Skipping deaths.")
                        deaths_data <- NULL
                    }
                }

                if (!is.null(deaths_data) && is.matrix(deaths_data)) {
                    if (nrow(deaths_data) == n_locations && ncol(deaths_data) == n_timesteps) {
                        location_data$deaths <- as.numeric(deaths_data[i, ])
                    } else {
                        warning(sprintf(
                            "Deaths data dimensions (%d x %d) don't match cases (%d x %d). Skipping deaths.",
                            nrow(deaths_data), ncol(deaths_data), n_locations, n_timesteps
                        ))
                    }
                }
            }
        }

        result_list[[i]] <- location_data
    }

    # Combine all locations
    simulated_data <- do.call(rbind, result_list)

    # =============================================================================
    # CLEAN AND VALIDATE
    # =============================================================================

    # Handle NA/NULL/Inf values
    simulated_data$cases[!is.finite(simulated_data$cases)] <- 0
    if ("deaths" %in% names(simulated_data)) {
        simulated_data$deaths[!is.finite(simulated_data$deaths)] <- 0
    }

    # Ensure proper data types
    simulated_data$j <- as.character(simulated_data$j)
    simulated_data$t <- as.numeric(simulated_data$t)
    simulated_data$cases <- as.numeric(simulated_data$cases)

    if (validate) {
        log_msg("Validating output structure...")

        # Check required columns
        required_cols <- c("j", "t", "cases")
        missing_cols <- setdiff(required_cols, names(simulated_data))
        if (length(missing_cols) > 0) {
            stop("Output validation failed. Missing columns: ", paste(missing_cols, collapse = ", "))
        }

        # Check data types
        if (!is.character(simulated_data$j)) {
            stop("Column 'j' must be character type")
        }
        if (!is.numeric(simulated_data$t)) {
            stop("Column 't' must be numeric type")
        }
        if (!is.numeric(simulated_data$cases)) {
            stop("Column 'cases' must be numeric type")
        }

        # Check for negative values
        if (any(simulated_data$cases < 0, na.rm = TRUE)) {
            warning("Negative case counts detected and will be set to 0")
            simulated_data$cases[simulated_data$cases < 0] <- 0
        }

        if ("deaths" %in% names(simulated_data) && any(simulated_data$deaths < 0, na.rm = TRUE)) {
            warning("Negative death counts detected and will be set to 0")
            simulated_data$deaths[simulated_data$deaths < 0] <- 0
        }

        log_msg("✓ Validation passed")
    }

    # =============================================================================
    # SUMMARY STATISTICS
    # =============================================================================

    if (verbose) {
        log_msg("Data extraction complete:")
        log_msg("  Locations: %d (%s)", n_locations,
                paste(head(unique(simulated_data$j), 3), collapse = ", "))
        log_msg("  Time points: %d (t = %d to %d)", n_timesteps,
                min(simulated_data$t), max(simulated_data$t))
        log_msg("  Total observations: %d rows", nrow(simulated_data))
        log_msg("  Total simulated cases: %d", sum(simulated_data$cases, na.rm = TRUE))
        log_msg("  Max simulated cases: %d", max(simulated_data$cases, na.rm = TRUE))

        if ("deaths" %in% names(simulated_data)) {
            log_msg("  Total simulated deaths: %d", sum(simulated_data$deaths, na.rm = TRUE))
            log_msg("  Max simulated deaths: %d", max(simulated_data$deaths, na.rm = TRUE))
        }
    }

    # Reset row names
    rownames(simulated_data) <- NULL

    return(simulated_data)
}