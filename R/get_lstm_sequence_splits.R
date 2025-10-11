#' Create Train/Validation Splits for LSTM Sequences
#'
#' This function creates train/validation splits from LSTM sequences using either
#' random sampling or location-month blocking. The location-month blocking approach
#' groups sequences by country-month combinations and randomly assigns entire blocks
#' to train/test, which better preserves spatial-temporal structure and tests
#' generalization capability.
#'
#' @param train_sequences List containing LSTM sequences with X, y, countries, and dates
#'   (output from create_lstm_sequences function)
#' @param split_method Character. Either "random" (default) or "location_month_block"
#' @param train_prop Numeric. Proportion of data for training (default 0.6)
#' @param seed Integer. Random seed for reproducibility
#' @param verbose Logical. Whether to print detailed information about splits (default TRUE)
#'
#' @return List containing train/validation indices and metadata:
#' \itemize{
#'   \item train_indices: Indices for training sequences
#'   \item val_indices: Indices for validation sequences
#'   \item split_info: List with split statistics and metadata
#' }
#'
#' @details
#' **Random Method (existing approach)**:
#' \itemize{
#'   \item Randomly samples individual sequences for train/test split
#'   \item Good for general model development and comparison
#'   \item May allow data leakage for sequences from same location-month
#' }
#'
#' **Location-Month Block Method (new approach)**:
#' \itemize{
#'   \item Groups sequences by iso_code + year-month combinations
#'   \item Randomly assigns entire location-month blocks to train or validation
#'   \item Ensures no data leakage between train/validation for the same location-month
#'   \item Better tests spatial-temporal generalization capability
#'   \item More realistic evaluation for deployment to new regions/time periods
#' }
#'
#' The location-month blocking approach is particularly valuable for:
#' \itemize{
#'   \item Testing model generalization to unseen location-time combinations
#'   \item Preventing overfitting to specific location-month patterns
#'   \item Evaluating suitability for early warning systems
#'   \item Assessing transferability across different epidemiological contexts
#' }
#'
#' @examples
#' \dontrun{
#' # After creating LSTM sequences
#' train_sequences <- create_lstm_sequences(...)
#'
#' # Random splitting (existing method)
#' splits_random <- get_lstm_sequence_splits(
#'   train_sequences,
#'   split_method = "random",
#'   train_prop = 0.6,
#'   seed = 99
#' )
#'
#' # Location-month blocking (new method)
#' splits_blocked <- get_lstm_sequence_splits(
#'   train_sequences,
#'   split_method = "location_month_block",
#'   train_prop = 0.6,
#'   seed = 99
#' )
#'
#' # Use the splits in LSTM training
#' X_train <- train_sequences$X[splits_blocked$train_indices, , ]
#' y_train <- train_sequences$y[splits_blocked$train_indices]
#' X_val <- train_sequences$X[splits_blocked$val_indices, , ]
#' y_val <- train_sequences$y[splits_blocked$val_indices]
#'
#' # Check split information
#' print(splits_blocked$split_info)
#' }
#'
#' @importFrom lubridate year month
#' @export
get_lstm_sequence_splits <- function(train_sequences,
                                   split_method = "random",
                                   train_prop = 0.6,
                                   seed = 99,
                                   verbose = TRUE) {

  # Validate inputs
  if (!is.list(train_sequences) ||
      !all(c("X", "y", "countries", "dates") %in% names(train_sequences))) {
    stop("train_sequences must be a list with components: X, y, countries, dates")
  }

  if (!split_method %in% c("random", "location_month_block")) {
    stop("split_method must be either 'random' or 'location_month_block'")
  }

  if (train_prop <= 0 || train_prop >= 1) {
    stop("train_prop must be between 0 and 1")
  }

  # Extract sequence information
  n_sequences <- length(train_sequences$y)
  countries <- train_sequences$countries
  dates <- as.Date(train_sequences$dates)

  if (verbose) {
    message(sprintf("Creating %s split with %.1f%% training data from %d sequences",
                    split_method, train_prop * 100, n_sequences))
    message(sprintf("  Sequence date range: %s to %s",
                    min(dates), max(dates)))
    message(sprintf("  Countries: %s", paste(sort(unique(countries)), collapse = ", ")))
  }

  set.seed(seed)

  if (split_method == "random") {
    # Original random splitting method
    train_indices <- sample(1:n_sequences, floor(train_prop * n_sequences))
    val_indices <- setdiff(1:n_sequences, train_indices)

    split_info <- list(
      method = "random",
      seed = seed,
      n_train = length(train_indices),
      n_val = length(val_indices),
      train_prop_actual = length(train_indices) / n_sequences,
      n_countries_train = length(unique(countries[train_indices])),
      n_countries_val = length(unique(countries[val_indices])),
      train_pos_rate = mean(train_sequences$y[train_indices]),
      val_pos_rate = mean(train_sequences$y[val_indices])
    )

    if (verbose) {
      message(sprintf("  Random split created:"))
      message(sprintf("    Training: %d sequences (%.1f%%) from %d countries",
                      split_info$n_train, split_info$train_prop_actual * 100,
                      split_info$n_countries_train))
      message(sprintf("    Validation: %d sequences (%.1f%%) from %d countries",
                      split_info$n_val, (1 - split_info$train_prop_actual) * 100,
                      split_info$n_countries_val))
    }

  } else if (split_method == "location_month_block") {
    # Location-month blocking method

    # Create location-month identifiers
    year_month <- paste0(lubridate::year(dates), "-",
                         sprintf("%02d", lubridate::month(dates)))
    location_month <- paste(countries, year_month, sep = "_")

    # Get unique location-month blocks with sequence counts
    unique_blocks <- unique(location_month)
    n_blocks <- length(unique_blocks)

    # Create block metadata
    block_info <- data.frame(
      block = unique_blocks,
      n_sequences = sapply(unique_blocks, function(b) sum(location_month == b)),
      country = sapply(strsplit(unique_blocks, "_"), `[`, 1),
      year_month = sapply(strsplit(unique_blocks, "_"), `[`, 2),
      stringsAsFactors = FALSE
    )

    if (verbose) {
      message(sprintf("  Found %d unique location-month blocks", n_blocks))
      message(sprintf("    Sequences per block - min: %d, max: %d, mean: %.1f",
                      min(block_info$n_sequences), max(block_info$n_sequences),
                      mean(block_info$n_sequences)))
    }

    # Randomly assign blocks to train/validation to approximate desired proportion
    n_train_blocks <- floor(train_prop * n_blocks)
    train_blocks <- sample(unique_blocks, n_train_blocks)
    val_blocks <- setdiff(unique_blocks, train_blocks)

    # Map back to sequence indices
    train_indices <- which(location_month %in% train_blocks)
    val_indices <- which(location_month %in% val_blocks)

    # Calculate block statistics
    train_block_info <- block_info[block_info$block %in% train_blocks, ]
    val_block_info <- block_info[block_info$block %in% val_blocks, ]

    split_info <- list(
      method = "location_month_block",
      seed = seed,
      n_train = length(train_indices),
      n_val = length(val_indices),
      train_prop_actual = length(train_indices) / n_sequences,
      n_train_blocks = length(train_blocks),
      n_val_blocks = length(val_blocks),
      n_countries_train = length(unique(countries[train_indices])),
      n_countries_val = length(unique(countries[val_indices])),
      train_pos_rate = mean(train_sequences$y[train_indices]),
      val_pos_rate = mean(train_sequences$y[val_indices]),
      train_blocks = sort(train_blocks),
      val_blocks = sort(val_blocks),
      avg_sequences_per_train_block = mean(train_block_info$n_sequences),
      avg_sequences_per_val_block = mean(val_block_info$n_sequences),
      min_sequences_per_block = min(block_info$n_sequences),
      max_sequences_per_block = max(block_info$n_sequences),
      block_size_distribution = table(block_info$n_sequences)
    )

    if (verbose) {
      message(sprintf("  Location-month block split created:"))
      message(sprintf("    Training: %d blocks → %d sequences (%.1f%%) from %d countries",
                      split_info$n_train_blocks, split_info$n_train,
                      split_info$train_prop_actual * 100, split_info$n_countries_train))
      message(sprintf("    Validation: %d blocks → %d sequences (%.1f%%) from %d countries",
                      split_info$n_val_blocks, split_info$n_val,
                      (1 - split_info$train_prop_actual) * 100, split_info$n_countries_val))
      message(sprintf("    Actual train proportion: %.3f (target: %.3f)",
                      split_info$train_prop_actual, train_prop))

      # Check for countries appearing in both splits
      countries_both <- intersect(unique(countries[train_indices]),
                                  unique(countries[val_indices]))
      if (length(countries_both) > 0) {
        message(sprintf("    Countries in both splits: %s",
                        paste(countries_both, collapse = ", ")))
        message("    (This is expected - different months for same country)")
      }
    }
  }

  # Validate that we have both train and validation data
  if (length(train_indices) == 0 || length(val_indices) == 0) {
    stop("Split resulted in empty training or validation set")
  }

  # Additional validation for transmission intensity distribution
  train_mean_intensity <- mean(train_sequences$y[train_indices], na.rm = TRUE)
  val_mean_intensity <- mean(train_sequences$y[val_indices], na.rm = TRUE)

  if (train_mean_intensity == 0) {
    warning("Training set has no transmission intensity")
  }
  if (val_mean_intensity == 0) {
    warning("Validation set has no transmission intensity")
  }

  if (verbose) {
    message(sprintf("  Transmission intensity distribution:"))
    message(sprintf("    Training: mean = %.3f, n = %d",
                    train_mean_intensity, length(train_indices)))
    message(sprintf("    Validation: mean = %.3f, n = %d",
                    val_mean_intensity, length(val_indices)))
  }

  return(list(
    train_indices = train_indices,
    val_indices = val_indices,
    split_info = split_info
  ))
}