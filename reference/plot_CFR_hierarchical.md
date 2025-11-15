# Plot Hierarchical CFR Model Results

Creates professional visualizations of the hierarchical GAM CFR model
results including observed data points and modeled time series
predictions. Generates an 8.5x11 inch PDF with faceted plots showing
country-specific CFR trends over time.

## Usage

``` r
plot_CFR_hierarchical(
  PATHS,
  countries_to_plot = NULL,
  pdf_width = 8.5,
  pdf_height = 11,
  ncol_facet = 4,
  show_ci = TRUE,
  verbose = TRUE
)
```

## Arguments

- PATHS:

  List containing paths to data directories, typically from get_paths()

- countries_to_plot:

  Character vector of ISO codes to plot. If NULL, plots top 20 countries
  by data availability

- pdf_width:

  Numeric, width of PDF in inches (default 8.5)

- pdf_height:

  Numeric, height of PDF in inches (default 11)

- ncol_facet:

  Integer, number of columns for facet wrap (default 4)

- show_ci:

  Logical, whether to show confidence intervals (default TRUE)

- verbose:

  Logical, whether to print progress messages (default TRUE)

## Value

Invisibly returns a list of ggplot objects. Saves PDF to DOCS_FIGURES
directory.

## Details

The function creates a multi-page PDF containing:

- Page 1: Overview plot with temporal trend and data summary

- Following pages: Faceted plots of country-specific CFR trends

- Each panel shows observed CFR (points) and model predictions (lines
  with CI)

- Color coding indicates data quality and model fit

## Examples

``` r
if (FALSE) { # \dontrun{
PATHS <- get_paths()
# First run the model
est_CFR_hierarchical(PATHS)
# Then create plots
plot_CFR_hierarchical(PATHS)

# Plot specific countries
plot_CFR_hierarchical(PATHS, 
  countries_to_plot = c("COD", "NGA", "KEN", "MOZ", "ETH", "ZWE"))
} # }
```
