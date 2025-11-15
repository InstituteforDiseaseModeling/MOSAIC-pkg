# Plot Vaccination Maps

Generates choropleth maps and corresponding bar plots to visualize OCV
vaccination data from WHO, GTFCC, or combined sources across African
countries.

## Usage

``` r
plot_vaccination_maps(PATHS, data_source = "WHO")
```

## Arguments

- PATHS:

  A named list containing file paths required for input and output
  operations. It should include:

  `MODEL_INPUT`

  :   Directory path where the processed vaccination data files are
      located.

  `DOCS_FIGURES`

  :   Directory path where the generated plots will be saved.

- data_source:

  The source of the vaccination data. Must be one of `"WHO"`, `"GTFCC"`,
  or `"BOTH"`. Default is `"WHO"`.

## Value

The function does not return any value. It generates and saves a PNG
file containing the vaccination maps and barplots with annotations.

## Details

The function performs the following steps:

1.  **Data Loading and Processing**: Reads the vaccination data from a
    CSV file, converts the `date` column to `Date` objects, and extracts
    the latest cumulative doses distributed and corresponding
    vaccination proportions for each country.

2.  **Map Data Retrieval**: Obtains map data for African countries using
    the `rnaturalearth` package.

3.  **Data Merging**: Merges the vaccination data with the map data,
    identifying Sub-Saharan African countries based on ISO codes
    provided by the
    [`MOSAIC::iso_codes_ssa`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/iso_codes_ssa.md)
    dataset.

4.  **Color Scale Definition**: Defines distinct color scales for
    cumulative doses distributed (green gradient) and proportion
    vaccinated (purple gradient), including appropriate breaks and
    labels.

5.  **Choropleth Map Creation**: Creates two choropleth maps—one for
    cumulative doses distributed and another for the proportion
    vaccinated—using `ggplot2` and layering map data accordingly.

6.  **Barplot Generation**: Constructs barplots for each metric,
    ordering countries by descending values and applying the
    corresponding color scales.

7.  **Plot Arrangement**: Utilizes the `patchwork` and `cowplot`
    packages to arrange the maps and barplots into a structured grid
    with panel labels "A" and "B" and adds a footnote.

8.  **Saving the Plot**: Saves the final combined plot as a PNG file in
    the specified `DOCS_FIGURES` directory.

## Examples

``` r
if (FALSE) { # \dontrun{
# Define the PATHS list with appropriate directories
PATHS <- list(
  MODEL_INPUT = "path/to/model/input",
  DOCS_FIGURES = "path/to/docs/figures"
)

# Call the function to generate and save the vaccination maps
plot_vaccination_maps(PATHS)
plot_vaccination_maps(PATHS, data_source = "GTFCC")
plot_vaccination_maps(PATHS, data_source = "BOTH")
} # }
```
