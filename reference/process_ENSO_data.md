# Compile ENSO and DMI Data (Historical and Forecast)

This function compiles historical and forecast data for DMI and ENSO
(Niño3, Niño3.4, and Niño4) into a single data frame. The data is
filtered to only include years from a specified `year_start` onwards.
The function also allows for disaggregation of monthly data into daily
or weekly values using either linear interpolation or spline
interpolation. When historical and forecast data overlap for the same
dates, historical data takes precedence.

## Usage

``` r
process_ENSO_data(year_start = NULL, frequency = "monthly", method = "linear")
```

## Arguments

- year_start:

  An integer representing the start year for filtering the data. Only
  data from this year onward will be included in the compiled data. The
  value must be greater than or equal to 1870.

- frequency:

  A character string specifying the time resolution of the output data.
  Valid options are "daily", "weekly", or "monthly".

- method:

  A character string specifying the interpolation method to use. Valid
  options are "linear" (for linear interpolation using
  [`zoo::na.approx()`](https://rdrr.io/pkg/zoo/man/na.approx.html)) or
  "spline" (for spline interpolation using
  [`zoo::na.spline()`](https://rdrr.io/pkg/zoo/man/na.approx.html)).

## Value

A data frame with combined historical and forecast data for DMI, ENSO3,
ENSO34, and ENSO4. The data frame includes the following columns:

- `date`: The date of the data (daily, weekly, or monthly) in YYYY-MM-DD
  format.

- `variable`: The variable name, which can be "DMI", "ENSO3", "ENSO34",
  or "ENSO4".

- `value`: The value of the variable (sea surface temperature anomaly).

- `year`: The year corresponding to the date.

- `month`: The month corresponding to the date.

- `month_name`: The name of the month (e.g., "Jan").

- `week`: The week of the year (for "weekly" and "daily" frequency).

- `doy`: The day of the year (only for "daily" frequency).

- `date_start`: Start date for the week or month.

- `date_stop`: End date for the week or month.

## Details

The function automatically handles overlapping data between historical
and forecast sources:

- When the same date exists in both historical and forecast data,
  historical data is preferred

- The function reports any overlaps found, including the date range and
  number of overlapping months

- Data source tracking is added internally but not included in the final
  output

## Examples

``` r
if (FALSE) { # \dontrun{
# Compile the ENSO and DMI data from the year 2000 onwards
compiled_enso_data <- process_ENSO_data(2010, "monthly")

# Display the compiled data
head(compiled_enso_data)
} # }
```
