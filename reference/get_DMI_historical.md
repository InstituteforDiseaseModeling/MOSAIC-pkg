# Download and Process Historical DMI Data

This function downloads the historical Dipole Mode Index (DMI) data from
NOAA and processes it into a data frame with columns for year, month
(1-12), month_name, variable (DMI), and value.

## Usage

``` r
get_DMI_historical()
```

## Value

A data frame with columns for year, month, month_name, variable (DMI),
and value.

## Details

The DMI data is downloaded from NOAA's historical DMI data page:
<https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/dmi.had.long.data>. The
data includes DMI values representing the difference in sea surface
temperature anomalies between the western and eastern Indian Ocean.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get the historical DMI data
dmi_historical <- get_DMI_historical()

# Display the historical DMI data
print(dmi_historical)
} # }
```
