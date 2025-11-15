# Plot Multi-Method Parameter Distributions

Creates visualizations comparing parameter distributions across multiple
methods (e.g., priors, BFRS posteriors, NPE posteriors) using a flexible
vector-based interface. Parameters are organized by biological category
and ordered consistently across all plots.

## Usage

``` r
plot_model_distributions(
  json_files,
  method_names,
  output_dir,
  custom_colors = NULL
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
  "#4a4a4a", "BFRS" = "#1f77b4", "NPE" = "#d00000"))

## Value

Invisibly returns a list of generated plot objects

## Examples

``` r
if (FALSE) { # \dontrun{
# Compare three methods
plot_model_distributions(
  json_files = c("priors.json", "posteriors_bfrs.json", "posterior/posteriors.json"),
  method_names = c("Prior", "BFRS", "NPE"),
  output_dir = "plots"
)

# Compare just prior and NPE
plot_model_distributions(
  json_files = c("priors.json", "posterior/posteriors.json"),
  method_names = c("Prior", "NPE"),
  output_dir = "plots"
)

# Use custom colors (MOSAIC defaults shown)
plot_model_distributions(
  json_files = c("priors.json", "posteriors_bfrs.json", "posterior/posteriors.json"),
  method_names = c("Prior", "BFRS", "NPE"),
  output_dir = "plots",
  custom_colors = c(
    "Prior" = "#4a4a4a",  # Dark gray
    "BFRS" = "#1f77b4",   # Blue
    "NPE" = "#d00000"     # Red
  )
)
} # }
```
