# Get DMI Forecast from JSON Configuration

Load DMI forecast data from the JSON configuration file instead of
hardcoded values. This replaces the manual text parsing approach.

## Usage

``` r
get_DMI_forecast_from_json()
```

## Value

A data frame with columns: year, month, month_name, variable, value

## Details

This function reads forecast data from
inst/extdata/dmi_forecast_current.json and converts it to the standard
MOSAIC format. It automatically checks for data freshness and issues
warnings if the data is stale.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get current DMI forecasts  
dmi_forecast <- get_DMI_forecast_from_json()
head(dmi_forecast)
} # }
```
