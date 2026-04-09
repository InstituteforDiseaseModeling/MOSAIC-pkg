# Plot Multi-Method Parameter Distributions

Creates visualizations comparing parameter distributions across multiple
methods (e.g., priors, BFRS posteriors) using a flexible vector-based
interface. Parameters are organized by biological category and ordered
consistently across all plots.

## Usage

``` r
plot_model_distributions(
  json_files,
  method_names,
  output_dir,
  custom_colors = NULL,
  verbose = FALSE
)
```

## Arguments

- json_files:

  Vector of paths to JSON distribution files

- method_names:

  Vector of method names corresponding to each JSON file (for legends
  and colors)

- output_dir:

  Directory to save generated plots

- custom_colors:

  Optional named vector of colors for each method (e.g., c("Prior" =
  "#4a4a4a", "BFRS" = "#1f77b4"))

## Value

Invisibly returns a list of generated plot objects

## Examples

``` r
if (FALSE) { # \dontrun{
# Compare prior and posterior
plot_model_distributions(
  json_files = c("priors.json", "posteriors.json"),
  method_names = c("Prior", "Posterior"),
  output_dir = "plots"
)

# Use custom colors
plot_model_distributions(
  json_files = c("priors.json", "posteriors.json"),
  method_names = c("Prior", "Posterior"),
  output_dir = "plots",
  custom_colors = c(
    "Prior" = "#4a4a4a",     # Dark gray
    "Posterior" = "#1f77b4"  # Blue
  )
)
} # }
```
