# Create Train/Validation Splits for LSTM Sequences

This function creates train/validation splits from LSTM sequences using
either random sampling or location-month blocking. The location-month
blocking approach groups sequences by country-month combinations and
randomly assigns entire blocks to train/test, which better preserves
spatial-temporal structure and tests generalization capability.

## Usage

``` r
get_lstm_sequence_splits(
  train_sequences,
  split_method = "random",
  train_prop = 0.6,
  seed = 99,
  verbose = TRUE
)
```

## Arguments

- train_sequences:

  List containing LSTM sequences with components `X`, `y`, `countries`,
  and `dates` (the sequence structure built internally by the legacy
  [`est_suitability()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
  path).

- split_method:

  Character. Either "random" (default) or "location_month_block"

- train_prop:

  Numeric. Proportion of data for training (default 0.6)

- seed:

  Integer. Random seed for reproducibility

- verbose:

  Logical. Whether to print detailed information about splits (default
  TRUE)

## Value

List containing train/validation indices and metadata:

- train_indices: Indices for training sequences

- val_indices: Indices for validation sequences

- split_info: List with split statistics and metadata

## Details

**Random Method (existing approach)**:

- Randomly samples individual sequences for train/test split

- Good for general model development and comparison

- May allow data leakage for sequences from same location-month

**Location-Month Block Method (new approach)**:

- Groups sequences by iso_code + year-month combinations

- Randomly assigns entire location-month blocks to train or validation

- Ensures no data leakage between train/validation for the same
  location-month

- Better tests spatial-temporal generalization capability

- More realistic evaluation for deployment to new regions/time periods

The location-month blocking approach is particularly valuable for:

- Testing model generalization to unseen location-time combinations

- Preventing overfitting to specific location-month patterns

- Evaluating suitability for early warning systems

- Assessing transferability across different epidemiological contexts

## Examples

``` r
if (FALSE) { # \dontrun{
# train_sequences is a list with X, y, countries, and dates components
# (built internally by the legacy est_suitability() path).

# Random splitting (existing method)
splits_random <- get_lstm_sequence_splits(
  train_sequences,
  split_method = "random",
  train_prop = 0.6,
  seed = 99
)

# Location-month blocking (new method)
splits_blocked <- get_lstm_sequence_splits(
  train_sequences,
  split_method = "location_month_block",
  train_prop = 0.6,
  seed = 99
)

# Use the splits in LSTM training
X_train <- train_sequences$X[splits_blocked$train_indices, , ]
y_train <- train_sequences$y[splits_blocked$train_indices]
X_val <- train_sequences$X[splits_blocked$val_indices, , ]
y_val <- train_sequences$y[splits_blocked$val_indices]

# Check split information
print(splits_blocked$split_info)
} # }
```
