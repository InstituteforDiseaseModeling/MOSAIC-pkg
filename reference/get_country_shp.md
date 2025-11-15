# Get a country shapefile from GeoBoundaries API

This function retrieves a shapefile for the given iso3 code from the
GeoBoundaries data API.

## Usage

``` r
get_country_shp(iso3, path_output = NULL)
```

## Arguments

- iso3:

  A three-letter capitalized character string. Must follow the ISO-3166
  Alpha-3 country code standard
  (<https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3>).

- path_output:

  A file path of where to save the country shapefile

## Value

sf class shapefile

## Examples

``` r
if (FALSE) { # \dontrun{

tmp <- get_country_shp(iso3 = 'MCO')
plot(tmp)

} # }
```
