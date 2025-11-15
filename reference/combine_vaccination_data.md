# Combine WHO and GTFCC Vaccination Data

This function intelligently combines vaccination data from WHO and GTFCC
sources, prioritizing GTFCC data while identifying and including WHO
campaigns that are missing from GTFCC. It uses multiple matching
criteria to determine if campaigns are duplicates, accounting for date
discrepancies and dose variations.

## Usage

``` r
combine_vaccination_data(PATHS, date_tolerance = 60, dose_tolerance = 0.2)
```

## Arguments

- PATHS:

  A list containing file paths for input and output data. The list
  should include:

  - **MODEL_INPUT**: Path to the directory containing processed
    vaccination data files and where combined data will be saved.

- date_tolerance:

  Number of days tolerance for matching campaign dates between sources
  (default: 60 days)

- dose_tolerance:

  Proportion tolerance for matching doses between sources (default: 0.2
  = 20%)

## Value

The function saves the combined vaccination data to a CSV file in the
directory specified by `PATHS$MODEL_INPUT`. It also returns the combined
data as a data frame for further use in R.

## Details

The function performs intelligent campaign matching using the following
approach:

1.  **Load Processed Data**: - Reads WHO and GTFCC processed vaccination
    data files

2.  **Intelligent Matching**: - Matches campaigns by country,
    approximate date (within tolerance), and dose similarity - Uses
    multiple passes with different tolerance levels to maximize accurate
    matching - Identifies truly unique WHO campaigns not present in
    GTFCC

3.  **Data Combination**: - Prioritizes GTFCC data (marked as source =
    "GTFCC") - Adds unique WHO campaigns (marked as source = "WHO") -
    Includes campaigns present in both (marked as source =
    "GTFCC_WHO_matched")

4.  **Quality Assurance**: - Validates data structure matches downstream
    requirements - Ensures all required columns are present - Maintains
    date and ID formatting standards

## Examples

``` r
# Example usage
PATHS <- get_paths()
#> Error in get_paths(): Cannot find root_directory
combined_data <- combine_vaccination_data(PATHS)
#> ==========================================
#> Combining WHO and GTFCC Vaccination Data
#> ==========================================
#> Loading processed vaccination data...
#> Error: object 'PATHS' not found

# With custom tolerance settings
combined_data <- combine_vaccination_data(PATHS, date_tolerance = 30, dose_tolerance = 0.1)
#> ==========================================
#> Combining WHO and GTFCC Vaccination Data
#> ==========================================
#> Loading processed vaccination data...
#> Error: object 'PATHS' not found
```
