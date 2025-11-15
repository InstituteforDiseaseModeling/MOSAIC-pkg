# Get WHO Region for ISO Country Codes

This function maps ISO 3-letter country codes to WHO African regional
classifications. Uses the pre-loaded regional ISO code vectors available
in the MOSAIC package data.

## Usage

``` r
get_who_region(iso_codes)
```

## Arguments

- iso_codes:

  Character vector of ISO 3-letter country codes (e.g., "NGA", "KEN")

## Value

Character vector of the same length as input with WHO regional
classifications:

- **West Africa**: Countries in West African region

- **East Africa**: Countries in East African region

- **Central Africa**: Countries in Central African region

- **North Africa**: Countries in North African region

- **Southern Africa**: Countries in Southern African region

- **Unknown**: Countries not found in any regional classification

## Details

The function uses the following MOSAIC package data objects:

- `iso_codes_africa_west`: West African countries

- `iso_codes_africa_east`: East African countries

- `iso_codes_africa_central`: Central African countries

- `iso_codes_africa_north`: North African countries

- `iso_codes_africa_south`: Southern African countries

## Examples

``` r
if (FALSE) { # \dontrun{
# Single country
get_who_region("NGA")  # Returns "West Africa"

# Multiple countries
countries <- c("NGA", "KEN", "COD", "ZAF")
get_who_region(countries)  # Returns regional classifications

# Handle missing/unknown codes
get_who_region(c("NGA", "XXX"))  # Returns c("West Africa", "Unknown")
} # }
```
