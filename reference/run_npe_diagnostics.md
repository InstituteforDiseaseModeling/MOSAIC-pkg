# Run NPE Diagnostics

Performs comprehensive diagnostics on a trained NPE model including
coverage tests and simulation-based calibration (SBC).

## Usage

``` r
run_npe_diagnostics(
  model,
  test_data = NULL,
  n_test_samples = 200,
  diagnostics = c("coverage", "sbc"),
  output_dir = NULL,
  verbose = TRUE
)
```

## Arguments

- model:

  Trained NPE model object

- test_data:

  Test data (if NULL, uses subset of training data)

- n_test_samples:

  Number of test samples for diagnostics

- diagnostics:

  Which diagnostics to run

- output_dir:

  Directory to save diagnostic results

- verbose:

  Print progress messages

## Value

List containing:

- coverage_50 - Coverage at 50% credible interval

- coverage_80 - Coverage at 80% credible interval

- sbc_pass_rate - Proportion of parameters passing SBC

- needs_retraining - Whether model should be retrained

- detailed_results - Detailed diagnostic metrics
