#' Get NPE Observed Data from Configuration
#'
#' Converts observed outbreak data from LASER configuration format into the
#' long-format data frame required for NPE inference. This function handles
#' both single and multi-location outbreak data.
#'
#' @param config A LASER configuration object or list containing outbreak data.
#'   Must have either \code{reported_cases} field (matrix or vector) or both
#'   \code{reported_cases} and \code{reported_deaths} fields.
#' @param location_codes DEPRECATED - location codes are now extracted from
#'   the config object. This parameter is kept for backward compatibility but
#'   will be ignored with a warning if provided.
#' @param include_deaths Logical; whether to include deaths column in output
#'   if available in config. Default is FALSE since NPE is typically trained
#'   on cases only.
#' @param time_index Integer vector specifying time indices. If NULL, uses
#'   1:ncol(reported_cases) for matrix data or 1:length(reported_cases) for
#'   vector data.
#' @param validate Logical; whether to validate the output data structure.
#'   Default is TRUE. Checks for required columns and data types.
#' @param verbose Logical; whether to print informative messages about data
#'   processing. Default is FALSE.
#'
#' @return A data frame in long format with columns:
#'   \itemize{
#'     \item \code{j} — Location identifier (character)
#'     \item \code{t} — Time index (numeric)
#'     \item \code{cases} — Observed case counts (numeric)
#'     \item \code{deaths} — Observed death counts (numeric, optional)
#'   }
#'
#' @details
#' This function standardizes the conversion from various LASER configuration
#' formats to the long-format data frame required by NPE functions like
#' \code{\link{est_npe_posterior}}. It handles:
#'
#' \itemize{
#'   \item Single location data (vector format)
#'   \item Multi-location data (matrix format with locations as rows)
#'   \item Optional deaths data
#'   \item Custom location identifiers
#'   \item Missing or NA values (converted to 0)
#' }
#'
#' The output format matches the expected input for NPE inference functions,
#' with one row per location-time combination.
#'
#' @section Data Structure:
#' **Input** (LASER config format):
#' \itemize{
#'   \item Matrix: [n_locations × n_timesteps]
#'   \item Each row represents a location
#'   \item Each column represents a time point
#' }
#'
#' **Output** (NPE long format):
#' \itemize{
#'   \item Data frame: [n_locations * n_timesteps × 3-4 columns]
#'   \item Each row represents one location-time observation
#'   \item Columns: j (location), t (time), cases, [deaths]
#' }
#'
#' @examples
#' # Single location example
#' config <- list(
#'   reported_cases = c(10, 15, 22, 18, 12, 8, 5, 3),
#'   location_name = "ETH"
#' )
#' observed_data <- get_npe_observed_data(config)
#'
#' # Multi-location example
#' config <- list(
#'   reported_cases = matrix(
#'     c(10, 15, 22, 18,  # Location 1
#'       5,  8, 12, 10), # Location 2
#'     nrow = 2, byrow = TRUE
#'   ),
#'   location_name = c("ETH", "KEN")
#' )
#' observed_data <- get_npe_observed_data(config)
#'
#' # With deaths data
#' config <- list(
#'   reported_cases = matrix(c(10, 15, 22, 5, 8, 12), nrow = 2),
#'   reported_deaths = matrix(c(1, 2, 3, 0, 1, 1), nrow = 2),
#'   location_name = c("ETH", "KEN")
#' )
#' observed_data <- get_npe_observed_data(config, include_deaths = TRUE)
#'
#' # Custom time indices (e.g., for specific weeks)
#' observed_data <- get_npe_observed_data(
#'   config,
#'   time_index = 10:17  # Weeks 10-17
#' )
#'
#' # Config with iso_code field
#' config <- list(
#'   reported_cases = matrix(c(10, 15, 5, 8), nrow = 2),
#'   iso_code = c("ETH", "KEN")  # Alternative field name
#' )
#' observed_data <- get_npe_observed_data(config)
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{est_npe_posterior}} — Uses this data format for NPE inference
#'   \item \code{\link{train_npe}} — Expects training data in this format
#'   \item \code{\link{extract_laser_output}} — Similar function for simulation outputs
#' }
#'
#' @export
get_npe_observed_data <- function(
    config,
    location_codes = NULL,
    include_deaths = FALSE,
    time_index = NULL,
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
    # EXTRACT AND VALIDATE CASES DATA
    # =============================================================================

    # Check for reported_cases field
    if (is.null(config$reported_cases)) {
        stop("config must contain 'reported_cases' field")
    }

    cases_data <- config$reported_cases

    # Determine data structure (vector or matrix)
    if (is.vector(cases_data) && !is.matrix(cases_data)) {
        # Single location case - convert to matrix format
        log_msg("Processing single location data (vector format)")
        cases_data <- matrix(cases_data, nrow = 1)
        n_locations <- 1
        n_timesteps <- length(config$reported_cases)
    } else if (is.matrix(cases_data)) {
        # Multi-location case
        n_locations <- nrow(cases_data)
        n_timesteps <- ncol(cases_data)
        log_msg("Processing %d location(s) with %d time points", n_locations, n_timesteps)
    } else {
        stop("reported_cases must be a vector or matrix")
    }

    # =============================================================================
    # DETERMINE LOCATION IDENTIFIERS (FROM CONFIG ONLY)
    # =============================================================================

    # Warn if deprecated parameter is used
    if (!is.null(location_codes)) {
        warning("Parameter 'location_codes' is deprecated. Location codes are now extracted from the config object. Ignoring provided location_codes.")
    }

    # Extract location codes from config
    extracted_codes <- NULL

    # Try different possible field names in order of preference
    if (!is.null(config$location_name)) {
        extracted_codes <- config$location_name
        log_msg("Using location codes from config$location_name")
    } else if (!is.null(config$location_code)) {
        extracted_codes <- config$location_code
        log_msg("Using location codes from config$location_code")
    } else if (!is.null(config$iso_code)) {
        extracted_codes <- config$iso_code
        log_msg("Using location codes from config$iso_code")
    } else if (!is.null(config$location)) {
        extracted_codes <- config$location
        log_msg("Using location codes from config$location")
    }

    # Validate extracted codes
    if (!is.null(extracted_codes)) {
        if (length(extracted_codes) != n_locations) {
            warning(sprintf(
                "Number of location codes in config (%d) doesn't match number of locations in data (%d). Using numeric indices.",
                length(extracted_codes), n_locations
            ))
            location_codes <- as.character(seq_len(n_locations))
        } else {
            location_codes <- as.character(extracted_codes)
        }
    } else {
        # No location identifiers found in config - use numeric indices
        location_codes <- as.character(seq_len(n_locations))
        log_msg("No location identifiers found in config. Using numeric indices: %s",
                paste(location_codes, collapse = ", "))
    }

    # =============================================================================
    # DETERMINE TIME INDICES
    # =============================================================================

    if (is.null(time_index)) {
        time_index <- seq_len(n_timesteps)
    } else {
        if (length(time_index) != n_timesteps) {
            stop(sprintf(
                "Length of time_index (%d) must match number of time points (%d)",
                length(time_index), n_timesteps
            ))
        }
    }

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
        if (include_deaths && !is.null(config$reported_deaths)) {
            deaths_data <- config$reported_deaths

            # Validate deaths data structure
            if (is.vector(deaths_data) && !is.matrix(deaths_data)) {
                if (n_locations == 1) {
                    deaths_data <- matrix(deaths_data, nrow = 1)
                } else {
                    warning("Deaths data is vector but multiple locations present. Skipping deaths.")
                    deaths_data <- NULL
                }
            }

            if (!is.null(deaths_data)) {
                if (!is.matrix(deaths_data)) {
                    warning("reported_deaths must be a matrix. Skipping deaths.")
                } else if (nrow(deaths_data) != n_locations || ncol(deaths_data) != n_timesteps) {
                    warning(sprintf(
                        "Deaths data dimensions (%d x %d) don't match cases (%d x %d). Skipping deaths.",
                        nrow(deaths_data), ncol(deaths_data), n_locations, n_timesteps
                    ))
                } else {
                    location_data$deaths <- as.numeric(deaths_data[i, ])
                }
            }
        }

        result_list[[i]] <- location_data
    }

    # Combine all locations
    observed_data <- do.call(rbind, result_list)

    # =============================================================================
    # CLEAN AND VALIDATE
    # =============================================================================

    # Handle NA/NULL/Inf values
    observed_data$cases[!is.finite(observed_data$cases)] <- 0
    if ("deaths" %in% names(observed_data)) {
        observed_data$deaths[!is.finite(observed_data$deaths)] <- 0
    }

    # Ensure proper data types
    observed_data$j <- as.character(observed_data$j)
    observed_data$t <- as.numeric(observed_data$t)
    observed_data$cases <- as.numeric(observed_data$cases)

    if (validate) {
        log_msg("Validating output structure...")

        # Check required columns
        required_cols <- c("j", "t", "cases")
        missing_cols <- setdiff(required_cols, names(observed_data))
        if (length(missing_cols) > 0) {
            stop("Output validation failed. Missing columns: ", paste(missing_cols, collapse = ", "))
        }

        # Check data types
        if (!is.character(observed_data$j)) {
            stop("Column 'j' must be character type")
        }
        if (!is.numeric(observed_data$t)) {
            stop("Column 't' must be numeric type")
        }
        if (!is.numeric(observed_data$cases)) {
            stop("Column 'cases' must be numeric type")
        }

        # Check for negative values
        if (any(observed_data$cases < 0, na.rm = TRUE)) {
            warning("Negative case counts detected and will be set to 0")
            observed_data$cases[observed_data$cases < 0] <- 0
        }

        if ("deaths" %in% names(observed_data) && any(observed_data$deaths < 0, na.rm = TRUE)) {
            warning("Negative death counts detected and will be set to 0")
            observed_data$deaths[observed_data$deaths < 0] <- 0
        }

        log_msg("✓ Validation passed")
    }

    # =============================================================================
    # SUMMARY STATISTICS
    # =============================================================================

    if (verbose) {
        log_msg("Data conversion complete:")
        log_msg("  Locations: %d (%s)", n_locations,
                paste(head(unique(observed_data$j), 3), collapse = ", "))
        log_msg("  Time points: %d (t = %d to %d)", n_timesteps,
                min(observed_data$t), max(observed_data$t))
        log_msg("  Total observations: %d rows", nrow(observed_data))
        log_msg("  Total cases: %d", sum(observed_data$cases, na.rm = TRUE))
        log_msg("  Max cases: %d", max(observed_data$cases, na.rm = TRUE))

        if ("deaths" %in% names(observed_data)) {
            log_msg("  Total deaths: %d", sum(observed_data$deaths, na.rm = TRUE))
            log_msg("  Max deaths: %d", max(observed_data$deaths, na.rm = TRUE))
        }
    }

    # Reset row names
    rownames(observed_data) <- NULL

    return(observed_data)
}