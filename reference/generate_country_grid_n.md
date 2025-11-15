# Generate a Grid of Points Within a Country's Shapefile Based on Number of Points

This function generates a grid of points within a country's shapefile,
where the grid contains a specified number of evenly spaced points.

## Usage

``` r
generate_country_grid_n(country_shp, n_points)
```

## Arguments

- country_shp:

  An `sf` object representing the country's shapefile.

- n_points:

  A numeric value specifying the total number of evenly spaced points to
  generate within the country boundary.

## Value

An `sf` object containing the generated grid of points within the
country boundary.

## Details

The function creates a regular grid of points within the polygon defined
by the country's shapefile, containing the specified number of points.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage with a specified number of points
grid_points_sf <- generate_country_grid_n(country_shapefile, n_points = 100)
} # }
```
