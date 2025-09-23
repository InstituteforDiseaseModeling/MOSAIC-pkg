#' Generate Cross-Validation Split for LSTM Environmental Suitability Models
#'
#' This function creates train/validation splits for LSTM training using three different
#' cross-validation strategies: random sampling, spatial leave-one-out, or temporal cutoff.
#' Each approach provides different insights into model generalization capability for
#' cholera environmental suitability prediction.
#'
#' @param data A data frame containing the suitability modeling dataset with columns:
#'   \itemize{
#'     \item \strong{iso_code}: ISO country codes
#'     \item \strong{date}: Date column (will be converted to Date class)
#'     \item \strong{cases_binary}: Binary suitability indicator (target variable)
#'     \item Additional environmental covariates for modeling
#'   }
#' @param cv_mode Character string specifying the cross-validation approach:
#'   \itemize{
#'     \item \code{"random"}: Random train/validation split (default)
#'     \item \code{"spatial"}: Spatial leave-one-out validation (hold out specific countries)
#'     \item \code{"temporal"}: Temporal split (train on historical, test on future data)
#'   }
#' @param train_prop Numeric proportion (0-1) for training data in random split mode.
#'   Default 0.8 (80% train, 20% validation). Only used when cv_mode = "random".
#' @param cutoff_date Date string (YYYY-MM-DD) for temporal split mode. Training data
#'   includes all observations before this date, validation includes all observations
#'   from this date onward. Default "2022-01-01". Only used when cv_mode = "temporal".
#' @param holdout_locations Character vector of ISO country codes to hold out for
#'   validation in spatial mode. Default NULL uses a predefined set of high-quality
#'   countries. Only used when cv_mode = "spatial".
#'
#' @details
#' \strong{Random Split (cv_mode = "random"):}
#' \itemize{
#'   \item Randomly samples observations for training/validation
#'   \item Maintains class balance and temporal distribution
#'   \item Good baseline for model development and comparison
#'   \item Provides optimistic performance estimates
#' }
#'
#' \strong{Spatial Split (cv_mode = "spatial"):}
#' \itemize{
#'   \item Holds out entire countries for validation
#'   \item Tests geographic generalization capability
#'   \item More realistic for deployment to new regions
#'   \item Uses predefined high-quality countries if holdout_locations not specified
#' }
#'
#' \strong{Temporal Split (cv_mode = "temporal"):}
#' \itemize{
#'   \item Trains on historical data, validates on future periods
#'   \item Tests temporal generalization capability
#'   \item Most realistic for operational early warning systems
#'   \item Prevents data leakage from future to past
#'   \item Maintains LSTM sequence integrity across temporal boundary
#' }
#'
#' \strong{Recommended CV Strategies:}
#' \itemize{
#'   \item \strong{Development}: Random split for rapid iteration
#'   \item \strong{Geographic evaluation}: Spatial split with representative countries
#'   \item \strong{Operational validation}: Temporal split with recent cutoff
#'   \item \strong{Comprehensive evaluation}: All three modes for robust assessment
#' }
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{train_data}: Training dataset
#'     \item \code{val_data}: Validation dataset
#'     \item \code{split_info}: List with split metadata including:
#'       \itemize{
#'         \item \code{cv_mode}: Cross-validation mode used
#'         \item \code{train_n}: Number of training observations
#'         \item \code{val_n}: Number of validation observations
#'         \item \code{train_positive_rate}: Proportion of positive cases in training
#'         \item \code{val_positive_rate}: Proportion of positive cases in validation
#'         \item Mode-specific metadata (split proportion, cutoff date, or holdout countries)
#'       }
#'   }
#'
#' @examples
#' \dontrun{
#' # Load suitability data
#' data_file <- file.path(PATHS$DATA_CHOLERA_WEEKLY,
#'                       'cholera_country_weekly_suitability_data.csv')
#' data <- read.csv(data_file, stringsAsFactors = FALSE)
#'
#' # Random split (default)
#' split_random <- get_lstm_cv_folds(data, cv_mode = "random")
#'
#' # Random split with custom proportion
#' split_random_70 <- get_lstm_cv_folds(data, cv_mode = "random", train_prop = 0.7)
#'
#' # Spatial split with default high-quality countries
#' split_spatial <- get_lstm_cv_folds(data, cv_mode = "spatial")
#'
#' # Spatial split with custom countries
#' split_custom <- get_lstm_cv_folds(data, cv_mode = "spatial",
#'                                  holdout_locations = c("AGO", "KEN", "NGA"))
#'
#' # Temporal split with default cutoff (2022-01-01)
#' split_temporal <- get_lstm_cv_folds(data, cv_mode = "temporal")
#'
#' # Temporal split with custom cutoff
#' split_temporal_2021 <- get_lstm_cv_folds(data, cv_mode = "temporal",
#'                                         cutoff_date = "2021-01-01")
#'
#' # Access split information
#' print(split_random$split_info)
#'
#' # Use in LSTM training
#' train_sequences <- create_lstm_sequences(split_random$train_data, ...)
#' val_sequences <- create_lstm_sequences(split_random$val_data, ...)
#' }
#'
#' @note
#' This function is designed to work with the MOSAIC cholera environmental suitability
#' modeling pipeline. Different CV modes provide complementary insights into model
#' performance and should be used together for comprehensive evaluation.
#'
#' @seealso
#' \code{\link{est_suitability}} for the main LSTM training function that uses these splits.
#' \code{\link{process_suitability_data}} for data preprocessing before split generation.
#'
#' @export
get_lstm_cv_folds <- function(data,
                            cv_mode = c("random", "spatial", "temporal"),
                            train_prop = 0.8,
                            cutoff_date = "2022-01-01",
                            holdout_locations = NULL) {

    # Input validation
    cv_mode <- match.arg(cv_mode)

    required_cols <- c("iso_code", "date", "cases_binary")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
        stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    # Ensure date is Date class
    data$date <- as.Date(data$date)

    # Filter to observations with case data (non-missing cases_binary)
    data_complete <- data[!is.na(data$cases_binary), ]

    if (nrow(data_complete) == 0) {
        stop("No observations with non-missing cases_binary found")
    }

    # Apply the selected CV mode
    if (cv_mode == "random") {

        # Random split validation
        if (train_prop <= 0 || train_prop >= 1) {
            stop("train_prop must be between 0 and 1")
        }

        set.seed(123)  # For reproducibility
        n_total <- nrow(data_complete)
        n_train <- floor(train_prop * n_total)

        train_indices <- sample(1:n_total, n_train, replace = FALSE)

        train_data <- data_complete[train_indices, ]
        val_data <- data_complete[-train_indices, ]

        train_positive <- sum(train_data$cases_binary == 1, na.rm = TRUE)
        val_positive <- sum(val_data$cases_binary == 1, na.rm = TRUE)

        split_info <- list(
            cv_mode = "random",
            train_prop = train_prop,
            train_n = nrow(train_data),
            val_n = nrow(val_data),
            train_positive_count = train_positive,
            val_positive_count = val_positive,
            train_positive_rate = train_positive / nrow(train_data),
            val_positive_rate = val_positive / nrow(val_data),
            description = sprintf("Random split: %.0f%% train (%d obs), %.0f%% validation (%d obs)",
                                 100*train_prop, nrow(train_data),
                                 100*(1-train_prop), nrow(val_data))
        )

        message(sprintf("Random split created:"))
        message(sprintf("  Training: %d obs (%.1f%%, %d positive)",
                       split_info$train_n, 100*train_prop, train_positive))
        message(sprintf("  Validation: %d obs (%.1f%%, %d positive)",
                       split_info$val_n, 100*(1-train_prop), val_positive))

    } else if (cv_mode == "spatial") {

        # Spatial leave-out validation
        if (is.null(holdout_locations)) {
            # Use predefined high-quality countries
            holdout_locations <- c("AGO", "BDI", "CMR", "COD", "ETH", "GHA", "KEN",
                                 "MOZ", "MWI", "NGA", "SOM", "SSD", "TZA", "ZMB", "ZWE")
        }

        available_countries <- unique(data_complete$iso_code)
        valid_holdouts <- intersect(holdout_locations, available_countries)

        if (length(valid_holdouts) == 0) {
            stop("None of the specified holdout_locations found in data: ",
                 paste(holdout_locations, collapse = ", "))
        }

        val_data <- data_complete[data_complete$iso_code %in% valid_holdouts, ]
        train_data <- data_complete[!data_complete$iso_code %in% valid_holdouts, ]

        train_positive <- sum(train_data$cases_binary == 1, na.rm = TRUE)
        val_positive <- sum(val_data$cases_binary == 1, na.rm = TRUE)

        split_info <- list(
            cv_mode = "spatial",
            holdout_locations = sort(valid_holdouts),
            train_countries = sort(unique(train_data$iso_code)),
            val_countries = sort(unique(val_data$iso_code)),
            train_n = nrow(train_data),
            val_n = nrow(val_data),
            train_positive_count = train_positive,
            val_positive_count = val_positive,
            train_positive_rate = train_positive / nrow(train_data),
            val_positive_rate = val_positive / nrow(val_data),
            description = sprintf("Spatial split: %d training countries, %d holdout countries (%s)",
                                 length(unique(train_data$iso_code)),
                                 length(valid_holdouts),
                                 paste(valid_holdouts, collapse = ", "))
        )

        message(sprintf("Spatial split created:"))
        message(sprintf("  Training: %d countries, %d obs (%d positive)",
                       length(split_info$train_countries), split_info$train_n, train_positive))
        message(sprintf("  Validation: %d countries (%s), %d obs (%d positive)",
                       length(valid_holdouts), paste(valid_holdouts, collapse = ", "),
                       split_info$val_n, val_positive))

    } else if (cv_mode == "temporal") {

        # Temporal split validation - simple YYYY-MM-DD format only
        tryCatch({
            cutoff_date <- as.Date(cutoff_date)
            if (is.na(cutoff_date)) {
                stop("Invalid date - check that the date exists (e.g., June 31st doesn't exist)")
            }
        }, error = function(e) {
            stop("cutoff_date must be a valid date in YYYY-MM-DD format (e.g., '2022-01-01'). Check that the date exists - common error: June 31st doesn't exist.")
        })

        data_date_range <- range(data_complete$date, na.rm = TRUE)
        if (cutoff_date <= data_date_range[1] || cutoff_date >= data_date_range[2]) {
            stop(sprintf("cutoff_date (%s) must be within data date range (%s to %s)",
                        cutoff_date, data_date_range[1], data_date_range[2]))
        }

        train_data <- data_complete[data_complete$date < cutoff_date, ]
        val_data <- data_complete[data_complete$date >= cutoff_date, ]

        if (nrow(train_data) == 0) {
            stop("No training data before cutoff_date. Choose a later cutoff_date.")
        }
        if (nrow(val_data) == 0) {
            stop("No validation data after cutoff_date. Choose an earlier cutoff_date.")
        }

        train_positive <- sum(train_data$cases_binary == 1, na.rm = TRUE)
        val_positive <- sum(val_data$cases_binary == 1, na.rm = TRUE)

        split_info <- list(
            cv_mode = "temporal",
            cutoff_date = as.character(cutoff_date),
            train_date_range = c(min(train_data$date, na.rm = TRUE),
                                max(train_data$date, na.rm = TRUE)),
            val_date_range = c(min(val_data$date, na.rm = TRUE),
                              max(val_data$date, na.rm = TRUE)),
            train_countries = sort(unique(train_data$iso_code)),
            val_countries = sort(unique(val_data$iso_code)),
            train_n = nrow(train_data),
            val_n = nrow(val_data),
            train_positive_count = train_positive,
            val_positive_count = val_positive,
            train_positive_rate = train_positive / nrow(train_data),
            val_positive_rate = val_positive / nrow(val_data),
            description = sprintf("Temporal split: Train pre-%s (%d obs), Validate post-%s (%d obs)",
                                 cutoff_date, nrow(train_data), cutoff_date, nrow(val_data))
        )

        message(sprintf("Temporal split created:"))
        message(sprintf("  Cutoff date: %s", cutoff_date))
        message(sprintf("  Training: %s to %s, %d obs (%d positive)",
                       split_info$train_date_range[1], split_info$train_date_range[2],
                       split_info$train_n, train_positive))
        message(sprintf("  Validation: %s to %s, %d obs (%d positive)",
                       split_info$val_date_range[1], split_info$val_date_range[2],
                       split_info$val_n, val_positive))
    }

    # Final validation checks
    if (nrow(train_data) == 0) {
        stop("Training data is empty")
    }
    if (nrow(val_data) == 0) {
        stop("Validation data is empty")
    }

    # Add full data context to train/val data (including prediction periods)
    train_data_full <- data[data$iso_code %in% unique(train_data$iso_code), ]
    val_data_full <- data[data$iso_code %in% unique(val_data$iso_code), ]

    # For temporal split, respect the cutoff boundary
    if (cv_mode == "temporal") {
        train_data_full <- data[data$date < cutoff_date, ]
        val_data_full <- data[data$date >= cutoff_date, ]
    }

    return(list(
        train_data = train_data_full,
        val_data = val_data_full,
        split_info = split_info
    ))
}