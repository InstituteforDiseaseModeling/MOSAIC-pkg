# Estimate NPE Posterior

Generates posterior samples using a trained NPE model and CRITICALLY
extracts log q(theta\|x) values needed for SMC importance weights.

## Usage

``` r
estimate_npe_posterior(
  model,
  observed_data,
  n_samples = 10000,
  return_log_probs = TRUE,
  quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975),
  output_dir = NULL,
  verbose = TRUE,
  rejection_sampling = FALSE,
  max_rejection_rate = 0.2,
  max_attempts = 10
)
```

## Arguments

- model:

  Trained NPE model object

- observed_data:

  Data frame with observed outbreak data

- n_samples:

  Number of posterior samples to generate

- return_log_probs:

  If TRUE, return log q(theta\|x) for SMC (default TRUE)

- quantiles:

  Quantile levels to compute

- output_dir:

  Directory to save results (NULL = don't save)

- verbose:

  Print progress messages

- rejection_sampling:

  Logical. If TRUE, perform rejection sampling to ensure all returned
  samples are within parameter bounds (default FALSE).

- max_rejection_rate:

  Numeric. Maximum acceptable rejection rate (0-1). If exceeded after 3
  attempts, sampling stops early (default 0.20).

- max_attempts:

  Integer. Maximum number of sampling attempts before giving up (default
  10).

## Value

List containing:

- samples - Matrix of posterior samples (n_samples x n_params)

- log_probs - Vector of log q(theta\|x) values for SMC

- quantiles - Data frame of parameter quantiles

- param_names - Names of parameters

- n_samples - Actual number of samples returned

- rejection_info - List with rejection sampling diagnostics (NULL if
  disabled):

  - enabled - TRUE

  - requested - Target number of samples

  - achieved - Actual samples obtained

  - total_drawn - Total samples generated (including rejected)

  - rejection_rate - Proportion of samples rejected

  - n_attempts - Number of sampling iterations
