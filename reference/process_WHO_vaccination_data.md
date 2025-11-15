# Process Vaccination Data for MOSAIC Model

This function processes raw vaccination data from WHO and associated
population data to create a clean, structured dataset for the MOSAIC
model. It handles data cleaning, infers missing campaign dates,
validates the dataset, and saves the output for use in modeling cholera
vaccination efforts.

## Usage

``` r
process_WHO_vaccination_data(PATHS)
```

## Arguments

- PATHS:

  A list containing file paths for input and output data. The list
  should include:

  - **DATA_DEMOGRAPHICS**: Path to the directory where demographic data
    is stored.

  - **DATA_SCRAPE_WHO_VACCINATION**: Path to the directory containing
    raw WHO vaccination data.

  - **MODEL_INPUT**: Path to the directory where processed vaccination
    data will be saved.

## Value

The function saves the processed vaccination data to a CSV file in the
directory specified by `PATHS$MODEL_INPUT`. It also returns the
processed data as a data frame for further use in R.

## Details

This function performs the following steps:

1.  **Load and Filter Demographic Data**: - Reads demographic data for
    African countries from a CSV file. - Filters the data for the year
    2023 and retains population sizes.

2.  **Load and Clean Vaccination Data**: - Reads raw WHO vaccination
    data. - Converts country names to ISO codes for consistency. -
    Filters the data to include only countries in the MOSAIC database.

3.  **Infer Missing Campaign Dates**: - Computes the delay between
    decision and campaign dates. - Infers missing campaign dates based
    on the mean delay where possible. - Fixes missing decision dates for
    grouped request numbers.

4.  **Validate and Filter Data**: - Ensures all rows have valid campaign
    dates. - Removes rows where `doses_shipped` is zero or missing.

5.  **Save Processed Data**: - Writes the cleaned and processed dataset
    to a CSV file for further modeling.

## Examples

``` r
# Example usage
PATHS <- list(
  DATA_DEMOGRAPHICS = "path/to/demographics",
  DATA_SCRAPE_WHO_VACCINATION = "path/to/who/vaccination",
  MODEL_INPUT = "path/to/model/input"
)

processed_data <- process_WHO_vaccination_data(PATHS)
#> Loading vaccination and population data
#> Warning: cannot open file 'path/to/demographics/demographics_africa_2000_2023.csv': No such file or directory
#> Error in file(file, "rt"): cannot open the connection
```
