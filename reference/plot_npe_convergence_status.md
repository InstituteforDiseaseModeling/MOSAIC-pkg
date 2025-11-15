# Plot NPE convergence status table

Creates a comprehensive status table showing NPE model convergence,
diagnostics, and performance metrics from the calibration workflow
outputs.

## Usage

``` r
plot_npe_convergence_status(
  npe_dir = NULL,
  output_dir = NULL,
  coverage_targets = list(`50` = 0.5, `95` = 0.95),
  ks_threshold = 0.05,
  show_param_details = 15,
  verbose = TRUE
)
```

## Arguments

- npe_dir:

  Path to NPE directory containing model outputs (default: searches for
  it)

- output_dir:

  Path to output directory for PDF (default: npe_dir/plots)

- coverage_targets:

  Named list of coverage targets (default: list("50" = 0.50, "95" =
  0.95))

- ks_threshold:

  P-value threshold for KS test (default: 0.05)

- show_param_details:

  Maximum number of parameters to show in detail (default: 15)

- verbose:

  Logical indicating whether to print messages

## Value

Invisible NULL. Creates a PDF file with the NPE convergence status
table.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create NPE convergence status table
plot_npe_convergence_status(
  npe_dir = "./local/calibration/calibration_test_17/2_npe"
)
} # }
```
