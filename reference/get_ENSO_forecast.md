# Get ENSO (Niño3, Niño3.4, and Niño4) Forecast Data

This function retrieves manually extracted ENSO forecast data, including
Niño3, Niño3.4, and Niño4 sea surface temperature (SST) anomalies, from
the Bureau of Meteorology.

## Usage

``` r
get_ENSO_forecast()
```

## Value

A data frame with columns for year, month (numeric), month_name,
variable (ENSO3, ENSO34, and ENSO4), and value.

## Details

The ENSO forecast is manually extracted from the Bureau of Meteorology's
ocean outlook page. The data includes SST anomalies for Niño3, Niño3.4,
and Niño4 regions. Negative values indicate cooler sea surface
temperatures, while positive values indicate warmer temperatures.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get the ENSO forecast data
enso_forecast <- get_ENSO_forecast()

# Display the ENSO forecast data
print(enso_forecast)
} # }
```
