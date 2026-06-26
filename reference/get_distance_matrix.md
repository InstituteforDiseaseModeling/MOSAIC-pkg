# Build distance matrix from longitude/latitude coordinates

This function builds the pairwise distance matrix (in kilometers) from
vectors of longitude (`x`) and latitude (`y`), either by planar
approximation or great-circle (Haversine) formula.

## Usage

``` r
get_distance_matrix(x, y, id, method = c("spherical", "planar"), sort = TRUE)
```

## Arguments

- x:

  Numeric vector of longitudes (degrees).

- y:

  Numeric vector of latitudes (degrees).

- id:

  Character vector of names for each location.

- method:

  Character string, either `"spherical"` (default) for Haversine
  distances on a sphere of radius 6371 km, or `"planar"` for Euclidean
  distances in degree‐space converted by 111.35 km/deg.

- sort:

  Logical. When `TRUE` (default, preserving historical behavior) the
  returned matrix is sorted alphabetically by `id`. When `FALSE` the
  rows/columns are kept in the order of `id` (config order), so the axes
  stay aligned to any value vector indexed the same way. Set `FALSE` for
  the spatial-figures path, where labels must follow config order rather
  than be silently re-sorted (F3).

## Value

A square matrix of pairwise distances (km) among locations, with row and
column names from `id`. By default sorted alphabetically; when
`sort = FALSE`, kept in `id` (input) order.

## Examples

``` r
if (FALSE) { # \dontrun{
# Sample lon/lat for three cities
lon <- c(-122.33, -118.24, -74.00)
lat <- c(  47.61,   34.05,   40.71)
id  <- c("Seattle", "LosAngeles", "NewYork")

# 1. Great‐circle (Haversine) distances in km
D_spherical <- get_distance_matrix(lon, lat, id)

# 2. Planar approximation (× 111.35 km/deg)
D_planar <- get_distance_matrix(lon, lat, id, method = "planar")

# Inspect results
print(D_spherical)
print(D_planar)
} # }
```
