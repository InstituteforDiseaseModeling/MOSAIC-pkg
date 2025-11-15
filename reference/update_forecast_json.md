# Update Forecast JSON Files

Helper function to programmatically update the forecast JSON files. This
is useful for batch updates or automated processes.

## Usage

``` r
update_forecast_json(
  enso_forecasts = NULL,
  dmi_forecasts = NULL,
  last_updated = NULL,
  expires_after = NULL
)
```

## Arguments

- enso_forecasts:

  Named list of ENSO forecasts where names are ENSO34/ENSO3/ENSO4 and
  values are named vectors with date keys (YYYY-MM) and numeric values

- dmi_forecasts:

  Named vector of DMI forecasts with date keys (YYYY-MM) and numeric
  values

- last_updated:

  Date string in YYYY-MM-DD format (default: current date)

- expires_after:

  Date string in YYYY-MM-DD format (default: auto-calculated)

## Examples

``` r
if (FALSE) { # \dontrun{
# Update with new forecast data
enso_data <- list(
  ENSO34 = c("2025-03" = -0.2, "2025-04" = -0.1),
  ENSO3 = c("2025-03" = -0.1, "2025-04" = 0.0),
  ENSO4 = c("2025-03" = 0.1, "2025-04" = 0.2)
)
dmi_data <- c("2025-03" = -0.1, "2025-04" = 0.0)

update_forecast_json(enso_data, dmi_data)
} # }
```
