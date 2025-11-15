# Check Forecast Data Freshness

Check if ENSO and DMI forecast data needs updating by examining the
expiration dates in the JSON configuration files.

## Usage

``` r
check_forecast_freshness(warn_threshold_days = 30)
```

## Arguments

- warn_threshold_days:

  Number of days before expiration to start warning (default: 30)

## Value

List with freshness status for both datasets

## Details

This function reads the JSON configuration files and checks:

- Whether forecast data has already expired

- Whether data expires soon (within warning threshold)

- Provides recommendations for updating

## Examples

``` r
if (FALSE) { # \dontrun{
# Check current freshness status
freshness <- check_forecast_freshness()
print(freshness)

# Use shorter warning threshold
freshness <- check_forecast_freshness(warn_threshold_days = 14)
} # }
```
