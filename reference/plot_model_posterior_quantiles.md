# Plot Model Posterior Quantiles

Creates visualizations of posterior parameter quantiles from calibration
or NPE results. Generates both global parameter plots and
location-specific parameter plots. Supports comparison of multiple
posterior sources.

## Usage

``` r
plot_model_posterior_quantiles(
  csv_files,
  output_dir = "./figures",
  plot_types = "both",
  verbose = TRUE
)
```

## Arguments

- csv_files:

  Character vector of paths to posterior_quantiles.csv files from
  calc_model_posterior_quantiles(), estimate_parameters_npe(), or
  estimate_parameters_lampe()

- output_dir:

  Directory to save plots (default: "./figures")

- plot_types:

  Character vector specifying which plot types to create ("global",
  "location", "both"). Default: "both"

- verbose:

  Logical, whether to print progress messages (default: TRUE)

## Value

List of ggplot objects (invisibly)

## Details

The function creates:

- Caterpillar plots showing median and credible intervals for each
  estimation type

- Separate plots for global and location-specific parameters

- Color-coded visualization by estimation type (automatically detected):

  - Prior samples (dark grey)

  - Calibration posterior (blue)

  - NPE estimates (red/orange)

  - Lampe estimates (bright red)

  - Other types (automatically assigned from color palette)

- Supports comparison of any number of estimation types

- Dynamically adapts to whatever types are present in the data

- Expects standardized CSV format with consistent column names

- Saves plots as PNG files in the output directory

## Examples

``` r
if (FALSE) { # \dontrun{
# Single posterior file
plot_model_posterior_quantiles("posterior_quantiles.csv")

# Compare multiple sources
plot_model_posterior_quantiles(
  csv_files = c("bfrs_quantiles.csv", "npe_quantiles.csv"),
  output_dir = "./comparison_plots"
)
} # }
```
