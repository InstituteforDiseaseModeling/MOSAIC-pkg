# Create Lagged Versions of Data

This function generates lagged versions of all columns in a given data
frame. It creates new columns for each lag, allowing time series data to
be structured in a way that includes previous time steps as predictors.

## Usage

``` r
make_lagged_data(x, lags)
```

## Arguments

- x:

  A data frame or tibble containing the variables for which lagged
  versions are to be created.

- lags:

  A vector of lag values indicating how many time steps to lag the data.
  For example, `lags = 1:3` will create 1-lagged, 2-lagged, and 3-lagged
  versions of each variable.

## Value

A data frame containing the original data and the lagged versions of the
variables. The lagged columns are named in the format
`<column_name>_lag_<lag>`.

## Details

This function takes a data frame and creates new columns that are lagged
versions of the original columns. The number of lags is specified by the
`lags` argument. The function creates new columns for each lag value,
appending the lag number to the original column names.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage of create_lagged_data:
data <- data.frame(var1 = 1:10, var2 = 11:20)
make_lagged_data(data, lags = 1:2)
} # }
```
