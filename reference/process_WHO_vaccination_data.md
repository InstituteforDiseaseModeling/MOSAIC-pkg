# Process Vaccination Data for MOSAIC Model

This function processes raw vaccination data from WHO to create a clean,
structured dataset for the MOSAIC model. It handles data cleaning,
infers missing campaign dates, validates the dataset, and saves the
output for use in modeling cholera vaccination efforts.

## Usage

``` r
process_WHO_vaccination_data(PATHS)
```

## Arguments

- PATHS:

  A list containing file paths for input and output data. The list
  should include:

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

1.  **Load and Clean Vaccination Data**: - Reads raw WHO vaccination
    data. - Converts country names to ISO codes for consistency. -
    Filters the data to include only countries in the MOSAIC database.

2.  **Infer Missing Campaign Dates**: - Computes the delay between
    decision and campaign dates. - Infers missing campaign dates based
    on the mean delay where possible. - Fixes missing decision dates for
    grouped request numbers.

3.  **Validate and Filter Data**: - Ensures all rows have valid campaign
    dates. - Removes rows where `doses_shipped` is zero or missing.

4.  **Save Processed Data**: - Writes the cleaned and processed dataset
    to a CSV file for further modeling.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage
PATHS <- list(
  DATA_SCRAPE_WHO_VACCINATION = "path/to/who/vaccination",
  MODEL_INPUT = "path/to/model/input"
)

processed_data <- process_WHO_vaccination_data(PATHS)
} # }
```
