#' Compute the model-implied mobility flux from a run configuration
#'
#' Derives the **model's own implied daily mobility flux** from a LASER
#' configuration's population sizes and calibrated mobility point estimates.
#' This is the single source of truth for the four mobility figures in the
#' \code{"spatial"} group of \code{\link{render_MOSAIC_figures}} (\eqn{\pi_{ij}}
#' diffusion, \eqn{\tau_i} departure, modeled flux matrix, mobility network).
#'
#' The modeled daily flux is
#' \deqn{M_{ij} = N_i \, \tau_i \, \pi_{ij}}
#' the expected number of travelers per day moving from origin \eqn{i} to
#' destination \eqn{j}, where \eqn{N_i} is the origin population, \eqn{\tau_i}
#' is the (daily) departure probability, and \eqn{\pi_{ij}} is the normalized
#' gravity connectivity (\code{\link{calc_diffusion_matrix_pi}}). The diagonal
#' is \code{NA} (no self-travel), so \code{rowSums(flux, na.rm = TRUE)} equals
#' \eqn{N_i\,\tau_i}, the total daily travelers leaving each origin.
#'
#' This differs from \code{\link{plot_mobility}}, which visualizes the
#' \emph{observed} OAG flight matrix used to fit the mobility model. This
#' function returns the model's \emph{implied} flux from its calibrated
#' parameters, and is therefore run-specific and fully computable from the
#' run's \code{config.json} alone (no observed flight data, no disk, no
#' estimation, no API call).
#'
#' @section Ordering:
#' All outputs are carried in **config order** (the order locations appear in
#' the configuration, which is the order the engine consumes them). The
#' \code{location_name} element is aligned element-wise to every vector and to
#' both axes of every matrix. This function never sorts.
#'
#' @param config A LASER configuration list (as read from \code{1_inputs/config.json}
#'   or produced by \code{\link{make_LASER_config}}). Must contain
#'   \code{longitude}, \code{latitude}, \code{N_j_initial}, \code{tau_i},
#'   \code{location_name}, and the scalars \code{mobility_omega},
#'   \code{mobility_gamma}.
#'
#' @return A list with elements, all in config order:
#' \describe{
#'   \item{location_name}{Character vector of location labels (length J).}
#'   \item{coords}{A J x 2 numeric matrix of \code{longitude}, \code{latitude}.}
#'   \item{N}{Numeric vector of origin populations (length J), named by location.}
#'   \item{tau}{Numeric vector of daily departure probabilities (length J), named.}
#'   \item{omega}{Numeric scalar gravity population exponent.}
#'   \item{gamma}{Numeric scalar gravity distance-decay exponent.}
#'   \item{D}{J x J Haversine distance matrix (km), config order.}
#'   \item{pi}{J x J normalized gravity connectivity, diagonal \code{NA}.}
#'   \item{flux}{J x J modeled daily flux \eqn{M_{ij}=N_i\tau_i\pi_{ij}}, diagonal \code{NA}.}
#' }
#'
#' @seealso \code{\link{calc_diffusion_matrix_pi}}, \code{\link{get_distance_matrix}},
#'   \code{\link{plot_diffusion_pi}}, \code{\link{plot_departure_tau}},
#'   \code{\link{plot_mobility_flux_matrix}}, \code{\link{plot_mobility_flux_network}},
#'   \code{\link{render_MOSAIC_figures}}.
#'
#' @examples
#' \dontrun{
#' cfg  <- jsonlite::fromJSON(system.file("extdata", "config_default.json",
#'                                         package = "MOSAIC"), simplifyVector = TRUE)
#' flux <- calc_mobility_flux(cfg)
#' # Identity: row sums equal N * tau (daily travelers leaving each origin)
#' stopifnot(all.equal(rowSums(flux$flux, na.rm = TRUE), flux$N * flux$tau,
#'                     check.attributes = FALSE))
#' }
#'
#' @export
calc_mobility_flux <- function(config) {

  if (is.null(config) || !is.list(config))
    stop("`config` must be a LASER configuration list.")

  required <- c("longitude", "latitude", "N_j_initial", "tau_i",
                "location_name", "mobility_omega", "mobility_gamma")
  missing_fields <- setdiff(required, names(config))
  if (length(missing_fields))
    stop("config is missing required mobility field(s): ",
         paste(missing_fields, collapse = ", "))

  loc   <- as.character(config$location_name)
  lon   <- as.numeric(config$longitude)
  lat   <- as.numeric(config$latitude)
  N     <- as.numeric(config$N_j_initial)
  tau   <- as.numeric(config$tau_i)
  omega <- as.numeric(config$mobility_omega)[1]
  gamma <- as.numeric(config$mobility_gamma)[1]

  J <- length(loc)
  if (length(lon) != J || length(lat) != J || length(N) != J || length(tau) != J)
    stop("longitude, latitude, N_j_initial, and tau_i must all have length ",
         J, " (number of locations).")
  if (J < 2L)
    stop("calc_mobility_flux requires at least 2 locations.")

  names(N)   <- loc
  names(tau) <- loc

  # Distance matrix in config order. get_distance_matrix() re-sorts its output
  # alphabetically by default, which would silently mislabel the downstream
  # gravity / flux matrices relative to the value vectors; sort = FALSE keeps
  # every axis aligned to `loc` (F3).
  D <- get_distance_matrix(lon, lat, loc, sort = FALSE)
  dimnames(D) <- list(origin = loc, destination = loc)

  # Normalized gravity connectivity (diagonal NA by construction).
  pi_mat <- calc_diffusion_matrix_pi(D, stats::setNames(N, loc), omega, gamma)
  dimnames(pi_mat) <- list(origin = loc, destination = loc)

  # Modeled daily flux M_ij = N_i * tau_i * pi_ij; diagonal NA (no self-travel).
  flux <- (N * tau) * pi_mat            # row-scaling: each row i times N_i*tau_i
  diag(flux) <- NA_real_
  dimnames(flux) <- list(origin = loc, destination = loc)

  coords <- cbind(longitude = lon, latitude = lat)
  rownames(coords) <- loc

  list(
    location_name = loc,
    coords        = coords,
    N             = N,
    tau           = tau,
    omega         = omega,
    gamma         = gamma,
    D             = D,
    pi            = pi_mat,
    flux          = flux
  )
}


# Adaptive log-scale colorbar breaks for the model-implied flux figures.
#
# The observed-OAG panels of plot_mobility() use fixed 0/10/100/1,000/10,000
# ticks (flights span up to ~10k/day). A run's MODEL-IMPLIED flux spans a
# smaller range (often ~10s-100s/day), so the fixed upper ticks fall off the
# colorbar. This returns round breaks -- 0 plus the powers of 10 spanning the
# data -- on the log(value + 1) scale the flux figures plot on, so the bar fits
# each run's range. Shared by plot_mobility_flux_matrix() and
# plot_mobility_flux_network().
#
# @param values Numeric vector of raw flux/trip counts (NA/Inf/<=0 ignored).
# @return list(breaks = <on log(value+1) scale>, labels = <formatted raw>).
# @noRd
.mosaic_flux_log_breaks <- function(values) {
  v <- values[is.finite(values) & values > 0]
  if (!length(v)) {
    raw <- c(0, 1)
  } else {
    pmax <- max(0L, floor(log10(max(v))))   # largest power of 10 <= max
    raw  <- c(0, 10^(0:pmax))               # 0, 1, 10, ..., 10^pmax
  }
  raw <- unique(raw)
  list(breaks = log(raw + 1),
       labels = formatC(raw, format = "d", big.mark = ","))
}
