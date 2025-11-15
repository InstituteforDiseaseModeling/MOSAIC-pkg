# Process GTFCC Vaccination Data for MOSAIC Model

This function processes raw GTFCC vaccination data that has been scraped
from the GTFCC OCV dashboard by the ees-cholera-mapping repository. It
transforms the event-based data structure into a clean, structured
dataset matching the WHO vaccination data format for use in the MOSAIC
model.

## Usage

``` r
process_GTFCC_vaccination_data(PATHS)
```

## Arguments

- PATHS:

  A list containing file paths for input and output data. The list
  should include:

  - **DATA_DEMOGRAPHICS**: Path to the directory where demographic data
    is stored.

  - **MODEL_INPUT**: Path to the directory where processed vaccination
    data will be saved.

## Value

The function saves the processed vaccination data to a CSV file in the
directory specified by `PATHS$MODEL_INPUT`. It also returns the
processed data as a data frame for further use in R.

## Details

This function performs the following steps:

1.  **Load and Transform GTFCC Data**: - Reads the scraped GTFCC data
    from ees-cholera-mapping repository. - Transforms event-based
    structure (Request, Decision, Delivery, Round events) to
    request-based structure.

2.  **Map to WHO Format**: - Aggregates events by request ID to extract
    doses requested, approved, and shipped. - Uses Delivery event dates
    as campaign dates. - Converts country names to ISO codes for
    consistency.

3.  **Load and Merge Demographic Data**: - Reads demographic data for
    African countries from a CSV file. - Filters the data for the year
    2023 and retains population sizes.

4.  **Infer Missing Campaign Dates**: - Computes the delay between
    decision and campaign dates. - Infers missing campaign dates based
    on the mean delay where possible. - Fixes missing decision dates for
    grouped request numbers.

5.  **Validate and Filter Data**: - Ensures all rows have valid campaign
    dates. - Removes rows where `doses_shipped` is zero or missing. -
    Filters for MOSAIC countries only.

6.  **Save Processed Data**: - Writes the cleaned and processed dataset
    to a CSV file for further modeling.

## Examples

``` r
# Example usage
PATHS <- list(
  DATA_DEMOGRAPHICS = "path/to/demographics",
  MODEL_INPUT = "path/to/model/input"
)

processed_data <- process_GTFCC_vaccination_data(PATHS)
#> Loading GTFCC vaccination and population data
#> Warning: cannot open file 'path/to/demographics/demographics_africa_2000_2023.csv': No such file or directory
#> Error in file(file, "rt"): cannot open the connection
```
