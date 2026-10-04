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
    "GTFCC") - Adds unique WHO campaigns (marked as source =
    "WHO_only") - Includes campaigns present in both (marked as source =
    "GTFCC_WHO_matched") - `match_confidence` is "high" (Step 1: +/-7
    days, +/-5% doses), "medium" (Step 2: within the tolerances), "low"
    (Step 3: +/-30 days, any dose, or Step 4 only), or "GTFCC_only" /
    "WHO_only" - In Steps 1-3 each GTFCC campaign absorbs at most one
    WHO campaign, so a genuinely separate second campaign within 30 days
    is kept - When GTFCC records a campaign under the WHO row's ICG
    request number, only that request's campaign(s) are candidates in
    every step - Step 4 (same ICG request): GTFCC rows are per-request
    totals (all deliveries summed) while WHO rows are per shipment, so a
    still-unmatched WHO shipment is absorbed by the GTFCC campaign with
    the same ICG request number (WHO `20174` = GTFCC `201704`) as long
    as the WHO doses assigned to that campaign stay within
    `(1 + dose_tolerance)` of its GTFCC doses (e.g. MOZ 2017-I04: 709.1K
    GTFCC vs 329.6K + 354.6K WHO); a WHO row whose request number GTFCC
    does not use is matched on the ICG decision date (+/-7 days) within
    the dose tolerance instead - Step 5 (repeated WHO-only rows):
    WHO-only rows sharing country, ICG request and decision date are
    kept in campaign-date order only while their summed doses stay
    within `(1 + dose_tolerance)` of the request's approved total;
    further rows are dropped as duplicate listings (MWI 20182: two
    500,600-dose rows against 500,600 approved) - The GTFCC request
    columns (`req_id`, `delivery_schedule`, `round_sequence`,
    `round_basis`; see
    [`process_GTFCC_vaccination_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_GTFCC_vaccination_data.md))
    are carried on every GTFCC row, matched or not. WHO-only rows have
    no delivery schedule and no round information
    (`round_basis = "unknown"`), so
    [`est_vaccination_rate`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_vaccination_rate.md)
    releases their doses from the campaign date and counts them as first
    doses.

4.  **Quality Assurance**: - Validates data structure matches downstream
    requirements - Ensures all required columns are present - Maintains
    date and ID formatting standards

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage (requires set_root_directory() + the MOSAIC-data repo)
PATHS <- get_paths()
combined_data <- combine_vaccination_data(PATHS)

# With custom tolerance settings
combined_data <- combine_vaccination_data(PATHS, date_tolerance = 30, dose_tolerance = 0.1)
} # }
```
