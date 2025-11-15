# Download and Process Historical ENSO Data (Niño3, Niño3.4, Niño4)

This function downloads the historical ENSO data for Niño3, Niño3.4, and
Niño4 from NOAA and processes it into a single data frame with columns
for year (as an integer), month, month_name, variable (ENSO3, ENSO34,
ENSO4), and value.

## Usage

``` r
get_ENSO_historical()
```

## Value

A data frame with columns for year, month, month_name, variable, and
value.

## Details

The historical ENSO data is downloaded from NOAA's historical ENSO data
pages. The data includes sea surface temperature anomalies for Niño3,
Niño3.4, and Niño4 regions.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get the historical ENSO data
enso_historical <- get_ENSO_historical()

# Display the historical ENSO data
print(enso_historical)
} # }
```
