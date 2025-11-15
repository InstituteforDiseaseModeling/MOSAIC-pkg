# Convert ISO3 to ISO2 or ISO2 to ISO3 Country Codes

This function converts a vector or scalar of ISO3 country codes to ISO2
country codes, or vice versa, based on the input. The function uses the
`countrycode` package and auto-detects the input format.

## Usage

``` r
convert_iso_codes(iso_codes)
```

## Arguments

- iso_codes:

  A vector or scalar of ISO2 or ISO3 country codes (e.g., "USA", "GBR",
  "ZA", "GB").

## Value

A character vector of converted ISO2 or ISO3 country codes corresponding
to the provided input.

## Details

The function detects whether the input is ISO2 or ISO3 format based on
the code length and converts accordingly using the `countrycode`
package.

## Examples

``` r
# Convert a vector of ISO3 codes to ISO2 codes
convert_iso_codes(c("USA", "GBR", "DZA"))
#> [1] "US" "GB" "DZ"

# Convert a vector of ISO2 codes to ISO3 codes
convert_iso_codes(c("US", "GB", "DZ"))
#> [1] "USA" "GBR" "DZA"
```
