# Plot Vaccination Data

This function creates plots based on vaccination request data from WHO,
GTFCC, or combined sources, displaying both raw and redistributed doses,
and highlighting the proportion of the population vaccinated in each
country. It produces two types of plots: (1) a faceted plot showing the
cumulative vaccination data for all countries, and (2) a detailed plot
zooming in on a specific country for a selected date range.

## Usage

``` r
plot_vaccination_data(PATHS, data_source = "WHO")
```

## Arguments

- PATHS:

  A list of file paths, which should include the following elements:

  MODEL_INPUT

  :   The path to the folder containing the processed vaccination data.

  DOCS_FIGURES

  :   The path to the folder where the plot images will be saved.

- data_source:

  The source of the vaccination data. Must be one of `"WHO"`, `"GTFCC"`,
  or `"BOTH"`. Default is `"WHO"`.

## Value

This function returns no R object but saves two PNG files:

- A faceted plot for vaccination data across all countries saved as
  `"vaccination_by_country.png"`.

- A zoomed-in plot for a selected country saved as
  `"vaccination_example_<iso_code>.png"`.

## Details

The function performs the following steps:

- Loads the processed vaccination data from a CSV file.

- Extracts the last cumulative value for each country, displaying the
  maximum proportion of the population vaccinated.

- Creates a faceted plot showing raw doses shipped and redistributed
  doses over time, for each country, with independent y-axes.

- Saves the faceted plot as a PNG file in the specified folder.

- Creates a zoomed-in plot for a specific country and date range, adding
  text labels to highlight important milestones.

- Saves the zoomed-in plot as a PNG file.

## Examples

``` r
if (FALSE) { # \dontrun{
PATHS <- list(
  MODEL_INPUT = "path/to/input/data",
  DOCS_FIGURES = "path/to/save/figures"
)
plot_vaccination_data(PATHS)
plot_vaccination_data(PATHS, data_source = "GTFCC")
plot_vaccination_data(PATHS, data_source = "BOTH")
} # }
```
