# Calculate the Centroid of a Country from its Shapefile

This function reads a country shapefile, calculates its centroid, and
returns the centroid coordinates (longitude and latitude) along with the
ISO3 country code.

## Usage

``` r
get_centroid(shapefile)
```

## Arguments

- shapefile:

  A character string representing the file path to a country shapefile
  (in `.shp` format).

## Value

A data frame with the following columns:

- **iso3**: The ISO3 country code extracted from the shapefile name.

- **lon**: The longitude of the country's centroid.

- **lat**: The latitude of the country's centroid.

## Details

The function reads the shapefile using
**[`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html)**,
calculates the centroid of the country using
**[`sf::st_centroid()`](https://r-spatial.github.io/sf/reference/geos_unary.html)**,
and extracts the coordinates of the centroid with
**[`sf::st_coordinates()`](https://r-spatial.github.io/sf/reference/st_coordinates.html)**.
The ISO3 code is extracted from the filename of the shapefile, assuming
the first three characters of the shapefile name represent the ISO3
country code.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage:
# Assuming you have a shapefile "ZAF_ADM0.shp" for South Africa
shapefile_path <- "path/to/shapefile/ZAF_ADM0.shp"
centroid_data <- get_centroid(shapefile_path)
print(centroid_data)
} # }
```
