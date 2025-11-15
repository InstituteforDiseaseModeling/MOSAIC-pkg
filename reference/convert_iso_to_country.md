# Convert ISO2 or ISO3 Country Codes to Country Names

This function converts a vector or scalar of ISO2 or ISO3 country codes
to their corresponding country names using the `countrycode` package.

## Usage

``` r
convert_iso_to_country(iso)
```

## Arguments

- iso:

  A vector or scalar of ISO2 or ISO3 country codes (e.g., "USA", "GB",
  "DZA").

## Value

A character vector of country names corresponding to the provided ISO2
or ISO3 country codes.

## Details

The function automatically detects if the input is in ISO2 or ISO3
format and returns the corresponding country names. Special handling is
applied for Congo and the Democratic Republic of Congo.

## Examples

``` r
# Convert a vector of ISO3 and ISO2 codes to country names
convert_iso_to_country(c("USA", "GBR", "DZA", "COD"))
#> [1] "United States"                "United Kingdom"              
#> [3] "Algeria"                      "Democratic Republic of Congo"
```
