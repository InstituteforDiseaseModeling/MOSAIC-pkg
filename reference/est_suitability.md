# Estimate Environmental Suitability (psi) for Cholera Transmission

This function loads climate data, ENSO data, and weekly cholera cases
data to estimate the environmental suitability for cholera transmission
based on various environmental factors. It scales the climate covariates
and then fits an LSTM-based RNN model to predict cholera outbreaks. The
model is trained using past cholera case data and climate conditions,
and predictions are made for environmental suitability (psi) based on
the climate covariates.

## Usage

``` r
est_suitability(
  PATHS,
  fit_date_start = NULL,
  fit_date_stop = NULL,
  pred_date_start = NULL,
  pred_date_stop = NULL,
  n_splits = 10,
  seed_base = 99,
  fine_tune_epochs = 12,
  fine_tune_lr = 0.001,
  split_method = "random",
  train_prop = 0.6
)
```

## Arguments

- PATHS:

  A list containing paths where the data is stored. Typically generated
  by the
  [`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md)
  function and should include:

  - **DATA_CHOLERA_WEEKLY**: Path to the directory containing processed
    combined weekly cholera cases data (WHO+JHU+SUPP sources).

  - **DATA_ELEVATION**: Path to the directory where elevation data is
    stored.

  - **MODEL_INPUT**: Path to save the processed model inputs and
    outputs.

  - **DOCS_FIGURES**: Path to save the generated plots.

- fit_date_start:

  Date string or NULL. Start date for model fitting period. If NULL,
  auto-detects from first cholera case data.

- fit_date_stop:

  Date string or NULL. End date for model fitting period. If NULL,
  auto-detects from last date with both cholera cases and complete ENSO
  data.

- pred_date_start:

  Date string or NULL. Start date for prediction period. If NULL, uses
  fit_date_start.

- pred_date_stop:

  Date string or NULL. End date for prediction period. If NULL,
  auto-detects from last date with complete ENSO data.

- n_splits:

  Integer. Number of sequential random train/test splits for transfer
  learning. Default 1 (no fine-tuning).

- seed_base:

  Integer. Base seed for reproducible random splits. Each split uses
  seed_base + split_number.

- fine_tune_epochs:

  Integer. Number of epochs for fine-tuning on additional splits.
  Default 10.

- fine_tune_lr:

  Numeric. Lower learning rate for fine-tuning additional splits.
  Default 0.00001.

- split_method:

  Character. Method for train/validation splitting: "random" (default)
  or "location_month_block". The "location_month_block" method groups
  sequences by country-month and randomly assigns entire blocks, which
  better tests spatial-temporal generalization by preventing data
  leakage within location-months.

- train_prop:

  Numeric. Proportion of data for training in initial split (default
  0.6). Used for both splitting methods.

## Value

This function processes climate and cholera case data, fits an LSTM
model, makes predictions on environmental suitability (psi), and saves
both the predictions and covariate data. It also generates a plot
showing the model fit (MAE and loss over training epochs) and saves it
to a specified directory.

## Details

The function performs the following steps:

- Loads processed climate and cholera data.

- Determines appropriate date ranges for fitting and prediction based on
  data availability.

- Validates data completeness within specified date ranges.

- Scales the climate covariates using training data statistics.

- Splits the training data into training and validation sets using
  either random sampling or location-month blocking.

- Creates temporal LSTM sequences respecting country boundaries and
  temporal gaps.

- Builds an LSTM-based recurrent neural network (RNN) model for
  predicting cholera outbreaks.

- Trains the model on the training set and evaluates performance on the
  validation set.

- Makes predictions on the full prediction period dataset.

- Applies temporal smoothing to predictions and saves results to
  specified directories.

- Optional: Performs sequential fine-tuning on additional random splits
  for transfer learning.

## Note

The LSTM model uses climate variables to predict cholera suitability.
The model's predictions are saved as a CSV file and a plot showing the
model fit (MAE and loss) is generated.

## See also

`layer_lstm`, `fit`,
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Set up paths
PATHS <- get_paths()

# Basic usage with default settings
est_suitability(PATHS)

# Custom date ranges for fitting and prediction
est_suitability(PATHS,
               fit_date_start = "2015-01-01",
               fit_date_stop = "2023-12-31",
               pred_date_start = "2020-01-01",
               pred_date_stop = "2025-12-31")

# Transfer learning with sequential fine-tuning on 3 random splits
est_suitability(PATHS,
               n_splits = 3,
               seed_base = 123,
               fine_tune_epochs = 15,
               fine_tune_lr = 0.00003)

# Use location-month blocking for better spatial-temporal generalization
est_suitability(PATHS,
               split_method = "location_month_block",
               train_prop = 0.7)

# Location-month blocking with transfer learning
est_suitability(PATHS,
               split_method = "location_month_block",
               n_splits = 5,
               train_prop = 0.6,
               seed_base = 42)
} # }

if (FALSE) { # \dontrun{
# Basic usage with auto-detected date ranges
est_suitability(PATHS)

# Historical validation: fit on 2010-2020, predict on 2021-2023
est_suitability(PATHS,
                fit_date_start = "2010-01-01",
                fit_date_stop = "2020-12-31",
                pred_date_start = "2021-01-01",
                pred_date_stop = "2023-12-31")

} # }
```
