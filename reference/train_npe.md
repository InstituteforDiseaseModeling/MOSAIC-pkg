# Train NPE Model

Trains a normalizing flow model for neural posterior estimation using
v5.2 advanced features including transform ramping, guards, and
auto-tune.

## Usage

``` r
train_npe(
  X,
  y,
  weights = NULL,
  bounds,
  architecture,
  output_dir,
  n_epochs = 1000,
  batch_size = 512,
  learning_rate = 0.001,
  validation_split = 0.15,
  early_stopping = TRUE,
  patience = 20,
  use_gpu = TRUE,
  seed = 42,
  verbose = TRUE
)
```

## Arguments

- X:

  Matrix of parameters (n_samples x n_params)

- y:

  Matrix of observations (n_samples x n_obs)

- weights:

  Vector of importance weights (NULL for uniform)

- bounds:

  Matrix of parameter bounds (n_params x 2)

- architecture:

  List with architecture specification

- output_dir:

  Directory to save trained model

- n_epochs:

  Maximum training epochs (default 1000)

- batch_size:

  Batch size for training (default 256)

- learning_rate:

  Initial learning rate (default 1e-3)

- validation_split:

  Proportion for validation (default 0.2)

- early_stopping:

  Use early stopping (default TRUE)

- patience:

  Early stopping patience (default 10, reduced to prevent overfitting)

- use_gpu:

  Use GPU if available (default TRUE)

- seed:

  Random seed

- verbose:

  Print progress

## Value

Trained NPE model object
