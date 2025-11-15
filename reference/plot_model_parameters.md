# Plot Model Parameters vs Likelihood

Creates faceted plots showing the relationship between model parameters
and likelihood values from calibration results. Generates separate plots
for global and location-specific parameters.

## Usage

``` r
plot_model_parameters(results, output_dir, verbose = TRUE)
```

## Arguments

- results:

  A data frame containing calibration results with columns for
  parameters, likelihood values, and simulation identifiers (sim, iter,
  seed). This is typically the output from a model calibration run. The
  function will automatically infer the number of simulations and
  iterations from the data.

- output_dir:

  Character string specifying the directory where plots should be saved.
  Directory will be created if it doesn't exist.

- verbose:

  Logical indicating whether to print messages. Default is TRUE.

## Value

Invisibly returns a list containing the plot objects:

- `global`: ggplot object for global parameters

- `location`: Named list of ggplot objects for each location

## Details

This function creates publication-quality faceted plots showing how each
parameter relates to the model likelihood. Points are colored by
density, with a LOESS smooth curve showing the trend. The best parameter
value (highest likelihood) is highlighted with a red point.

The function automatically:

- Separates global and location-specific parameters

- Categorizes parameters by type (epidemiological, environmental, etc.)

- Creates separate plots for each location found in the data

- Saves plots as PDF files in the specified output directory

## Examples

``` r
if (FALSE) { # \dontrun{
# Run calibration and collect results
results <- run_calibration()

# Create plots (automatically infers n_sim and n_iter from results)
plots <- plot_model_parameters(results, output_dir = "calibration_output")

# Access individual plots
print(plots$global)
print(plots$location[["ETH"]])
} # }
```
