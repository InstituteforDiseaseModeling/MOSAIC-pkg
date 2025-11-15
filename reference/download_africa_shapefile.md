# Download and Save Africa Shapefile

This function downloads the shapefile for African countries using the
`rnaturalearth` package and saves it in a specified directory. If the
output directory does not exist, it will be created.

## Usage

``` r
download_africa_shapefile(PATHS)
```

## Arguments

- PATHS:

  A list or environment containing the path structure, where
  `PATHS$DATA_PROCESSED` specifies the directory to save the processed
  shapefile.

## Value

A message indicating that the shapefile has been saved, including the
full path to the saved file.

## Details

This function uses the `rnaturalearth` package to download the shapefile
for the African continent. The shapefile is saved as `AFRICA_ADM0.shp`
in the directory specified by `PATHS$DATA_PROCESSED/shapefiles`. The
function will create the directory if it does not already exist.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Example PATHS object
  PATHS <- list(DATA_PROCESSED = "path/to/processed/data")

  # Download and save the Africa shapefile
  download_africa_shapefile(PATHS)
} # }
```
