# Download and Save Shapefiles for All African Countries

This function downloads shapefiles for each African country and saves
them in a specified directory. The function iterates through ISO3
country codes for Africa (from the MOSAIC framework) and saves each
country's shapefile individually.

## Usage

``` r
download_all_country_shapefiles(PATHS)
```

## Arguments

- PATHS:

  A list containing the paths where processed data should be saved.
  PATHS is typically the output of the
  [`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md)
  function and should include:

  - **DATA_PROCESSED**: Path to the directory where the processed
    shapefiles should be saved.

## Value

The function does not return a value. It downloads the shapefiles for
each African country and saves them as individual shapefiles in the
processed data directory.

## Details

The function performs the following steps:

1.  Creates the output directory for shapefiles if it does not exist.

2.  Loops through the list of ISO3 country codes for African countries.

3.  Downloads each country's shapefile using
    [`MOSAIC::get_country_shp()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_country_shp.md).

4.  Saves the shapefile in the processed data directory as
    `ISO3_ADM0.shp`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Define paths for processed data using get_paths()
PATHS <- get_paths()

# Download and save shapefiles for all African countries
download_all_country_shapefiles(PATHS)
} # }
```
