# Convert Country Names to ISO3 or ISO2 Country Codes

This function converts country names to their corresponding ISO3 or ISO2
country codes. The function uses the `countrycode` package, which allows
for multiple spellings and variations of country names.

## Usage

``` r
convert_country_to_iso(x, iso3 = TRUE)
```

## Arguments

- x:

  A character vector of country names (e.g., "United States", "UK",
  "Democratic Republic of Congo").

- iso3:

  A logical value. If TRUE (default), returns ISO3 country codes. If
  FALSE, returns ISO2 country codes.

## Value

A character vector of ISO3 or ISO2 country codes corresponding to the
country names.

## Details

The function converts country names to ISO3 or ISO2 codes using the
`countrycode` package. It handles various common and alternative
spellings of country names.

## Examples

``` r
# Convert a single country name to ISO3
convert_country_to_iso("United States")
#> [1] "USA"

# Convert a vector of country names to ISO2
convert_country_to_iso(c("United States", "UK", "Democratic Republic of Congo"), iso3 = FALSE)
#> [1] "US" "GB" "CD"
```
