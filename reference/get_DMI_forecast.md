# Get DMI (Dipole Mode Index) Forecast Data

This function retrieves DMI forecast data from JSON configuration files
or falls back to hardcoded values. DMI data comes from the Bureau of
Meteorology's IOD forecast page.

## Usage

``` r
get_DMI_forecast(use_json = TRUE)
```

## Arguments

- use_json:

  Logical. Whether to try loading from JSON configuration first
  (default: TRUE)

## Value

A data frame with columns for year, month (numeric), month_name,
variable (DMI), and value. The DMI is a measure of the difference in sea
surface temperature anomalies between the western and eastern Indian
Ocean and is used to describe the Indian Ocean Dipole (IOD).

## Details

This function first attempts to load forecast data from
inst/extdata/dmi_forecast_current.json. If the JSON file doesn't exist
or use_json=FALSE, it falls back to hardcoded values extracted from the
Bureau of Meteorology's IOD forecast page. The data includes DMI values
representing forecasts of sea surface temperature anomalies. Negative
DMI values indicate cooler waters in the west, while positive DMI values
indicate warmer waters in the west of the Indian Ocean.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get DMI forecast data (tries JSON first)
dmi_forecast <- get_DMI_forecast()

# Force use of hardcoded values
dmi_forecast <- get_DMI_forecast(use_json = FALSE)

# Display the DMI forecast data
print(dmi_forecast)
} # }
```
