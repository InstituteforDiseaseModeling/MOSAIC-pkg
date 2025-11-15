# Create Binary Environmental Suitability Indicator from Case Data

This function creates a binary indicator of environmental suitability
for cholera transmission based on case count data. It uses sophisticated
temporal logic to identify not only outbreak periods but also the
environmental conditions leading up to outbreaks, which may be more
predictive of future transmission risk.

## Usage

``` r
get_cases_binary(data, cutoff = 1)
```

## Arguments

- data:

  A data frame containing cholera surveillance data with columns:

  - **iso_code**: ISO country code

  - **cases**: Weekly cholera case counts

  - **date_start**: Start date of each week (for ordering)

- cutoff:

  Numeric threshold for case counts. Weeks with cases \>= cutoff are
  considered outbreak periods. Default is 1 (any cases).

## Value

The input data frame with an additional `cases_binary` column containing
the binary environmental suitability indicator (0 = unsuitable, 1 =
suitable).

## Details

The environmental suitability logic works as follows:

1.  **Primary identification**: Weeks with cases \>= cutoff are marked
    as suitable (1)

2.  **Lead-up period**: The week immediately before any outbreak week is
    marked as suitable

3.  **Extended lead-up**: If two consecutive weeks have cases \>=
    cutoff, then 2-3 weeks before are also marked as suitable

This approach recognizes that environmental conditions favorable for
cholera transmission typically develop in the weeks preceding observable
outbreaks. By marking these lead-up periods as "suitable," the model can
learn to predict environmental suitability before cases are reported,
which is more useful for early warning systems.

**Missing Data Handling**: Weeks with missing case data (NA) are
preserved as NA in the binary indicator and are never retroactively
marked as suitable during the lead-up period logic. This prevents false
environmental suitability signals during periods with poor surveillance
coverage.

The function processes each country separately to ensure that temporal
relationships are maintained within each location's outbreak history.

## Note

The temporal logic requires data to be ordered by date within each
country. The function will sort the data appropriately, but for best
performance, input data should already be ordered by iso_code and
date_start.

## See also

[`process_suitability_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_suitability_data.md)
for the main data processing pipeline where this function is used.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load cholera case data
data <- read.csv("cholera_weekly_data.csv")

# Create binary suitability indicator with default cutoff
data_with_binary <- get_cases_binary(data)

# Use higher cutoff for more conservative outbreak definition
data_with_binary <- get_cases_binary(data, cutoff = 10)
} # }
```
