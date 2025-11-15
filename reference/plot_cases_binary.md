# Plot Binary Environmental Suitability Indicator with Case Data

This function creates a visualization showing cholera case counts over
time with shaded regions indicating periods identified as
environmentally suitable for cholera transmission. The environmental
suitability is determined using sophisticated temporal logic that
considers not only outbreak periods but also lead-up weeks when
conditions become favorable.

## Usage

``` r
plot_cases_binary(PATHS)
```

## Arguments

- PATHS:

  A list containing paths where the data is stored. Must include:

  - **DATA_CHOLERA_WEEKLY**: Path to combined weekly cholera
    surveillance data (WHO+JHU+SUPP)

  - **DOCS_FIGURES**: Path where the generated plot will be saved

## Value

Invisibly returns the ggplot object after displaying and saving it.

## Details

The function performs the following steps:

- Loads processed suitability data containing pre-computed
  `cases_binary` indicators

- Uses the existing binary environmental suitability indicators from the
  main pipeline

- Generates a multi-panel plot showing case counts as bars with shaded
  regions indicating environmentally suitable periods

- Saves the plot as a PNG file to the figures directory

The shaded regions represent periods identified as environmentally
suitable by the
[`process_suitability_data()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_suitability_data.md)
pipeline, including both outbreak periods and lead-up weeks when
conditions become favorable.

## See also

[`process_suitability_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_suitability_data.md)
for the main pipeline that creates the processed data with
`cases_binary`.
[`get_cases_binary`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_cases_binary.md)
for details on how the environmental suitability indicator is created.

## Examples

``` r
if (FALSE) { # \dontrun{
PATHS <- get_paths()
plot_cases_binary(PATHS)
} # }
```
