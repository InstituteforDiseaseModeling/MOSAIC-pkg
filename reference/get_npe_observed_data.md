# Get NPE Observed Data

Extracts and formats observed outbreak data for NPE conditioning.
Properly handles LASER config format where reported_cases can be:

- Vector: single location time series

- Matrix: rows = locations, columns = time points

## Usage

``` r
get_npe_observed_data(config, aggregate_locations = FALSE, verbose = TRUE)
```

## Arguments

- config:

  Configuration with outbreak data

- aggregate_locations:

  Aggregate across locations (default FALSE)

- verbose:

  Print progress

## Value

Data frame formatted for NPE with columns: j, t, cases
