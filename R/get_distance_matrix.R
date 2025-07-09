##' Build distance matrix from longitude/latitude coordinates
##'
##' This function builds the pairwise distance matrix (in kilometers)
##' from vectors of longitude (`x`) and latitude (`y`), either by
##' planar approximation or great-circle (Haversine) formula.
##'
##' @param x   Numeric vector of longitudes (degrees).
##' @param y   Numeric vector of latitudes (degrees).
##' @param id  Character vector of names for each location.
##' @param method Character string, either `"spherical"` (default) for
##'   Haversine distances on a sphere of radius 6371 km, or `"planar"`
##'   for Euclidean distances in degree‐space converted by 111.35 km/deg.
##'
##' @return A square matrix of pairwise distances (km) among locations,
##'   with row and column names from `id`, sorted alphabetically.
##'
##' @examples
##' \dontrun{
##' # Sample lon/lat for three cities
##' lon <- c(-122.33, -118.24, -74.00)
##' lat <- c(  47.61,   34.05,   40.71)
##' id  <- c("Seattle", "LosAngeles", "NewYork")
##'
##' # 1. Great‐circle (Haversine) distances in km
##' D_spherical <- get_distance_matrix(lon, lat, id)
##'
##' # 2. Planar approximation (× 111.35 km/deg)
##' D_planar <- get_distance_matrix(lon, lat, id, method = "planar")
##'
##' # Inspect results
##' print(D_spherical)
##' print(D_planar)
##' }
##'
##' @export
##'

get_distance_matrix <- function(x,
                                y,
                                id,
                                method = c("spherical", "planar")) {

     method <- match.arg(method)

     if (!is.numeric(x) || !is.numeric(y)) {
          stop("`x` and `y` must be numeric vectors of equal length.")
     }

     if (length(x) != length(y) || length(x) != length(id)) {
          stop("`x`, `y`, and `id` must have the same length.")
     }

     n <- length(x)
     out <- matrix(0, nrow = n, ncol = n)

     if (method == "spherical") {

          # Haversine formula
          rad       <- pi / 180
          lon_rad   <- x * rad
          lat_rad   <- y * rad
          R         <- 6371  # Earth radius in km

          for (i in seq_len(n)) {

               dlat          <- lat_rad - lat_rad[i]
               dlon          <- lon_rad - lon_rad[i]
               a             <- sin(dlat/2)^2 + cos(lat_rad[i]) * cos(lat_rad) * sin(dlon/2)^2
               central_angle <- 2 * atan2(sqrt(a), sqrt(1 - a))
               out[i, ]      <- R * central_angle

          }

     } else {

          # Planar approximation: Euclidean on degrees × 111.35 km/deg
          factor <- 111.35

          for (i in seq_len(n)) {

               dx       <- (x - x[i]) * factor
               dy       <- (y - y[i]) * factor
               out[i, ] <- sqrt(dx^2 + dy^2)

          }
     }

     dimnames(out) <- list(origin = id, destination = id)
     out[order(dimnames(out)$origin), order(dimnames(out)$destination)]

}
