# Generate Cross-Validation Split for LSTM Environmental Suitability Models

This function creates train/validation splits for LSTM training using
three different cross-validation strategies: random sampling, spatial
leave-one-out, or temporal cutoff. Each approach provides different
insights into model generalization capability for cholera environmental
suitability prediction.

## Usage

``` r
get_lstm_cv_folds(
  data,
  cv_mode = c("random", "spatial", "temporal"),
  train_prop = 0.8,
  cutoff_date = "2022-01-01",
  holdout_locations = NULL
)
```

## Arguments

- data:

  A data frame containing the suitability modeling dataset with columns:

  - **iso_code**: ISO country codes

  - **date**: Date column (will be converted to Date class)

  - **cases_binary**: Binary suitability indicator (target variable)

  - Additional environmental covariates for modeling

- cv_mode:

  Character string specifying the cross-validation approach:

  - `"random"`: Random train/validation split (default)

  - `"spatial"`: Spatial leave-one-out validation (hold out specific
    countries)

  - `"temporal"`: Temporal split (train on historical, test on future
    data)

- train_prop:

  Numeric proportion (0-1) for training data in random split mode.
  Default 0.8 (80% train, 20% validation). Only used when cv_mode =
  "random".

- cutoff_date:

  Date string (YYYY-MM-DD) for temporal split mode. Training data
  includes all observations before this date, validation includes all
  observations from this date onward. Default "2022-01-01". Only used
  when cv_mode = "temporal".

- holdout_locations:

  Character vector of ISO country codes to hold out for validation in
  spatial mode. Default NULL uses a predefined set of high-quality
  countries. Only used when cv_mode = "spatial".

## Value

A list containing:

- `train_data`: Training dataset

- `val_data`: Validation dataset

- `split_info`: List with split metadata including:

  - `cv_mode`: Cross-validation mode used

  - `train_n`: Number of training observations

  - `val_n`: Number of validation observations

  - `train_positive_rate`: Proportion of positive cases in training

  - `val_positive_rate`: Proportion of positive cases in validation

  - Mode-specific metadata (split proportion, cutoff date, or holdout
    countries)

## Details

**Random Split (cv_mode = "random"):**

- Randomly samples observations for training/validation

- Maintains class balance and temporal distribution

- Good baseline for model development and comparison

- Provides optimistic performance estimates

**Spatial Split (cv_mode = "spatial"):**

- Holds out entire countries for validation

- Tests geographic generalization capability

- More realistic for deployment to new regions

- Uses predefined high-quality countries if holdout_locations not
  specified

**Temporal Split (cv_mode = "temporal"):**

- Trains on historical data, validates on future periods

- Tests temporal generalization capability

- Most realistic for operational early warning systems

- Prevents data leakage from future to past

- Maintains LSTM sequence integrity across temporal boundary

**Recommended CV Strategies:**

- **Development**: Random split for rapid iteration

- **Geographic evaluation**: Spatial split with representative countries

- **Operational validation**: Temporal split with recent cutoff

- **Comprehensive evaluation**: All three modes for robust assessment

## Note

This function is designed to work with the MOSAIC cholera environmental
suitability modeling pipeline. Different CV modes provide complementary
insights into model performance and should be used together for
comprehensive evaluation.

## See also

[`est_suitability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
for the main LSTM training function that uses these splits.
[`process_suitability_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_suitability_data.md)
for data preprocessing before split generation.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load suitability data
data_file <- file.path(PATHS$DATA_CHOLERA_WEEKLY,
                      'cholera_country_weekly_suitability_data.csv')
data <- read.csv(data_file, stringsAsFactors = FALSE)

# Random split (default)
split_random <- get_lstm_cv_folds(data, cv_mode = "random")

# Random split with custom proportion
split_random_70 <- get_lstm_cv_folds(data, cv_mode = "random", train_prop = 0.7)

# Spatial split with default high-quality countries
split_spatial <- get_lstm_cv_folds(data, cv_mode = "spatial")

# Spatial split with custom countries
split_custom <- get_lstm_cv_folds(data, cv_mode = "spatial",
                                 holdout_locations = c("AGO", "KEN", "NGA"))

# Temporal split with default cutoff (2022-01-01)
split_temporal <- get_lstm_cv_folds(data, cv_mode = "temporal")

# Temporal split with custom cutoff
split_temporal_2021 <- get_lstm_cv_folds(data, cv_mode = "temporal",
                                        cutoff_date = "2021-01-01")

# Access split information
print(split_random$split_info)

# Use in LSTM training
train_sequences <- create_lstm_sequences(split_random$train_data, ...)
val_sequences <- create_lstm_sequences(split_random$val_data, ...)
} # }
```
