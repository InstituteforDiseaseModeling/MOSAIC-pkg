# Get Mean or Median Elevation from Downloaded DEM Data Using Country Boundaries

This function calculates the mean or median elevation for a given set of
ISO3 country codes by loading the downloaded DEM files and extracting
raster values within the country's boundary.

## Usage

``` r
get_elevation(PATHS, iso_codes, method = "median")
```

## Arguments

- PATHS:

  A list containing paths where DEM data is stored. Typically generated
  by the
  [`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md)
  function and should include:

  - **DATA_DEM**: Path to the directory where DEM rasters are saved
    (GeoTIFF format).

  - **DATA_SHAPEFILES**: Path to the directory where shapefiles are
    saved.

- iso_codes:

  A character vector of ISO3 country codes for which the elevation data
  will be calculated.

- method:

  A character string specifying the method to use for calculating
  elevation statistics. Can be `"mean"` or `"median"` (default is
  `"median"`).

## Value

A data frame with ISO3 codes, country names, and the calculated mean or
median elevation for each country.

## Examples

``` r
if (FALSE) { # \dontrun{
# Define the ISO3 country codes
iso_codes <- c("ZAF", "KEN", "NGA")

# Get paths for data storage
PATHS <- get_paths()

# Calculate mean elevation for the countries
mean_elevations <- get_elevation(iso_codes, PATHS, method = "mean")
print(mean_elevations)
} # }
```
