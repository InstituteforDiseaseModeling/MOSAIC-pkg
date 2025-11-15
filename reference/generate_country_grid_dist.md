# Generate a Grid of Points Within a Country's Shapefile Based on Distance

This function generates a grid of points within a country's shapefile,
where the points are spaced by a specified distance in kilometers.

## Usage

``` r
generate_country_grid_dist(country_shp, distance_km)
```

## Arguments

- country_shp:

  An `sf` object representing the country's shapefile.

- distance_km:

  A numeric value specifying the distance (in kilometers) between grid
  points.

## Value

An `sf` object containing the generated grid of points within the
country boundary.

## Details

The function creates a regular grid of points within the polygon defined
by the country's shapefile, spaced according to the specified distance
in kilometers.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage with distance in kilometers
grid_points_sf <- generate_country_grid_dist(country_shapefile, distance_km = 50)
} # }
```
