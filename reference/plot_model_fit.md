# Plot Model Fit: Timeseries of Predicted vs Observed

Creates timeseries plots comparing model predictions with observed data
from a laser-cholera model run.

## Usage

``` r
plot_model_fit(
  model,
  output_dir,
  verbose = TRUE,
  precalculated_likelihood = NULL
)
```

## Arguments

- model:

  A laser-cholera Model object (class
  'laser_cholera.metapop.model.Model') output from
  lc\$run_model(paramfile = config). Must contain:

  - results\$expected_cases - predicted cases

  - results\$disease_deaths - predicted deaths

  - params\$reported_cases - observed cases

  - params\$reported_deaths - observed deaths

  - params\$location_name - location identifiers

  - params\$date_start and params\$date_stop - time range for the plot

  - params\$seed - seed used for the model run (optional, for display)

- output_dir:

  Character string specifying the directory where the plot should be
  saved. Directory will be created if it doesn't exist.

- verbose:

  Logical indicating whether to print messages. Default is TRUE.

- precalculated_likelihood:

  Numeric value of precalculated likelihood to use in plot titles
  instead of recalculating. If NULL, likelihood will be calculated using
  calc_model_likelihood. Default is NULL.

## Value

Invisibly returns a list of ggplot objects:

- `individual`: Named list of plots for each location

- `cases_faceted`: Faceted plot of cases by location (if n_locations \>
  1)

- `deaths_faceted`: Faceted plot of deaths by location (if n_locations
  \> 1)

## Details

This function creates three types of publication-quality timeseries
plots:

1.  Individual location plots: For each location, a plot with cases and
    deaths as rows

2.  Cases faceted plot: All locations' cases in one faceted plot
    (skipped if only 1 location)

3.  Deaths faceted plot: All locations' deaths in one faceted plot
    (skipped if only 1 location)

Each plot shows:

- Observed data as black points

- Predicted data as colored lines (blue for cases, dark red for deaths)

- Automatic scaling and layout based on the number of locations

- Model performance statistics in the subtitle and caption:

  - Seed value (from model\$params\$seed if available)

  - Log-likelihood (calculated using calc_model_likelihood)

  - Total observed and predicted counts

  - Correlation between observed and predicted values

## Examples

``` r
if (FALSE) { # \dontrun{
# Run a model
config <- sample_parameters(priors = priors, config = base_config, seed = 123)
model <- lc$run_model(paramfile = config)

# Create timeseries plots
plots <- plot_model_fit(
    model = model,
    output_dir = "output/plots"
)

# Access individual plots
plots$individual[["ETH"]]  # Individual location plot for Ethiopia
plots$cases_faceted         # Faceted cases plot (if n_locations > 1)
plots$deaths_faceted        # Faceted deaths plot (if n_locations > 1)
} # }
```
