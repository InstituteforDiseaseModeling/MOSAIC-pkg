#' Deterministic precomputation for the R transmission engine
#'
#' These four matrices are pure functions of the config with no randomness
#' involved, which makes them the sharpest part of the parity story: they can be
#' checked against the Python oracle exactly, with no PRNG to align first
#' (Tier A, \code{migrate-laser-r.md} section 7). Between them they validate the
#' gravity model, the two-harmonic seasonality, the psi-normalisation and the
#' beta-CDF decay map.
#'
#' In the Python engine each is built once in a component constructor and then
#' held constant for the whole run: \code{pi_ij} and \code{beta_jt_human} in
#' \code{HumanToHuman.__init__}, \code{beta_jt_env} in
#' \code{EnvToHuman.__init__}, \code{delta_jt} in
#' \code{Environmental.__init__}.
#'
#' @name laser_precompute
#' @keywords internal
NULL

#' Great-circle distance matrix between patch centroids
#'
#' Haversine, matching \code{laser.core.migration.distance()} — which is the
#' only reason \pkg{laser-core} was a dependency of the engine at all. Earth
#' radius 6371 km.
#'
#' @section Precision vs. the Python oracle:
#' This computes in double throughout. The Python implementation does not: the
#' config's \code{latitude} / \code{longitude} are coerced to \code{float32}
#' by \code{params.py}, and \code{np.radians} of a \code{float32} array stays
#' \code{float32}, so the whole haversine \code{a} term
#' (\code{sin^2(dlat/2) + cos cos sin^2(dlon/2)}) is evaluated in single
#' precision before being widened for the \code{arcsin}. The result is that the
#' oracle's distance matrix carries ~1e-6 relative error and \strong{this
#' version is the more accurate of the two}.
#'
#' That was established empirically rather than assumed: mimicking each
#' float32 truncation in turn (inputs only, then the \code{a} term) monotonically
#' closed the observed gap — 1.09e-6 to 8.5e-7 to 5.2e-7 — which confirms
#' cumulative single-precision arithmetic rather than a difference of formula.
#' The consequence is that \code{pi_ij} cannot match the oracle to better than
#' ~1e-6 (observed worst case 1.17e-6), and its Tier A tolerance is set
#' accordingly. No other precomputed matrix is affected: they are element-wise
#' in their inputs and match to 1e-7 or better.
#'
#' @param latitude,longitude Numeric vectors of length \code{npatches}, in
#'   decimal degrees.
#' @return An \code{[npatches, npatches]} matrix of distances in km, zero on the
#'   diagonal.
#' @keywords internal
laser_distance_matrix <- function(latitude, longitude) {

     if (length(latitude) != length(longitude)) {
          stop(sprintf("latitude (%d) and longitude (%d) must be the same length.",
                       length(latitude), length(longitude)), call. = FALSE)
     }
     if (any(latitude < -90 | latitude > 90)) {
          stop("latitude must lie in [-90, 90].", call. = FALSE)
     }
     if (any(longitude < -180 | longitude > 180)) {
          stop("longitude must lie in [-180, 180].", call. = FALSE)
     }

     lat <- latitude * pi / 180
     lon <- longitude * pi / 180
     n <- length(lat)

     # a = sin^2(dlat/2) + cos(lat_i) cos(lat_j) sin^2(dlon/2), then
     # d = 2 R asin(sqrt(a)). Built by outer differences rather than a loop;
     # the arithmetic is identical.
     dlat <- outer(lat, lat, function(a, b) b - a)
     dlon <- outer(lon, lon, function(a, b) b - a)
     coslat <- outer(cos(lat), cos(lat), `*`)

     a <- sin(dlat / 2)^2 + coslat * sin(dlon / 2)^2
     # Clamp into [0, 1]: rounding can push a diagonal element a hair above 1,
     # and asin() would return NaN rather than pi/2.
     a[a > 1] <- 1
     a[a < 0] <- 0

     d <- 2 * asin(sqrt(a)) * 6371.0
     dim(d) <- c(n, n)
     d
}

#' Row-stochastic gravity connectivity matrix
#'
#' \code{x_ij = N_j^omega * d_ij^(-gamma)} for \code{i != j}, row-normalised.
#' The diagonal is zero: there is no self-mobility. The migrating fraction
#' \code{tau_i} is deliberately \emph{not} applied here — the Python engine
#' factors it in at runtime inside \code{HumanToHuman.__call__} so it can vary
#' per patch without rebuilding \code{pi_ij}, and that split is preserved.
#'
#' @param N Numeric vector of initial patch populations, length
#'   \code{npatches}.
#' @param d Distance matrix from \code{laser_distance_matrix()}.
#' @param omega,gamma Gravity-model exponents
#'   (\code{mobility_omega}, \code{mobility_gamma}).
#' @return An \code{[npatches, npatches]} row-stochastic matrix.
#' @keywords internal
laser_pi_ij <- function(N, d, omega, gamma) {

     n <- length(N)
     if (!identical(dim(d), c(n, n))) {
          stop(sprintf("distance matrix is %s; expected %d x %d.",
                       paste(dim(d), collapse = " x "), n, n), call. = FALSE)
     }

     # Replace the diagonal with 1 so d^(-gamma) stays finite, then zero the
     # diagonal back out. This mirrors how the Python engine vectorised its
     # original `if j == i: continue` loop -- the diagonal was left at its
     # zeros-init value, never computed.
     d_safe <- d
     diag(d_safe) <- 1.0

     # N^omega varies along the DESTINATION index, so it broadcasts across rows.
     x <- matrix(N^omega, nrow = n, ncol = n, byrow = TRUE) * d_safe^(-gamma)
     diag(x) <- 0

     row_sum <- rowSums(x)
     out <- matrix(0, nrow = n, ncol = n)
     # A single-location config has one row whose only cell is the zeroed
     # diagonal, so its row sum is 0. The Python loop skipped the division in
     # that case (its inner `continue`); dividing would produce NaN. The result
     # is trivially [[0]].
     nz <- row_sum != 0
     if (any(nz)) out[nz, ] <- x[nz, , drop = FALSE] / row_sum[nz]
     out
}

#' Two-harmonic seasonal human-transmission envelope
#'
#' \code{beta_j0_hum * (1 + a1 cos(2 pi t/p) + b1 sin(2 pi t/p) +
#' a2 cos(4 pi t/p) + b2 sin(4 pi t/p))}.
#'
#' \strong{\code{t} is 1-indexed}, not 0-indexed — \code{t = 1:nticks}. The
#' Python implementation writes \code{np.arange(0, nticks) + 1} with the comment
#' "R is 1-indexed, so we start at 1", i.e. it already matches an R convention.
#' Using \code{0:(nticks-1)} here would phase-shift the whole seasonal envelope
#' by one day.
#'
#' @param par Parameters from \code{laser_params()}.
#' @return A \code{[nticks, npatches]} matrix.
#' @keywords internal
laser_beta_jt_human <- function(par) {

     t <- seq_len(par$nticks)
     p <- par$p

     # Each harmonic term is an outer product of a per-tick wave and a
     # per-patch amplitude.
     term <- function(amp, wave) outer(wave, amp)

     env <- 1 +
          term(par$a_1_j, cos(2 * pi * t / p)) +
          term(par$b_1_j, sin(2 * pi * t / p)) +
          term(par$a_2_j, cos(4 * pi * t / p)) +
          term(par$b_2_j, sin(4 * pi * t / p))

     matrix(par$beta_j0_hum, nrow = par$nticks, ncol = par$npatches,
            byrow = TRUE) * env
}

#' Environmental transmission rate from suitability
#'
#' \code{beta_j0_env * (1 + (psi - psi_bar) / psi_bar)}, where \code{psi_bar} is
#' each patch's mean suitability \emph{over time}. The normalisation makes the
#' matrix a relative-suitability modulation of a per-patch baseline rather than
#' an absolute rate, so \code{beta_j0_env} stays interpretable and calibratable.
#'
#' @param par Parameters from \code{laser_params()}.
#' @return A \code{[nticks, npatches]} matrix.
#' @keywords internal
laser_beta_jt_env <- function(par) {

     psi <- par$psi_jt   # [nticks, npatches]

     # Column means: the average over TIME within each patch. Averaging over
     # patches instead would silently produce a plausible-looking matrix, which
     # is why this is asserted in the Tier A test rather than left implicit.
     psi_bar <- colMeans(psi)

     if (any(psi_bar == 0)) {
          stop(sprintf(paste0("psi_jt has a zero time-mean at patch(es) %s, so the ",
                              "(psi - psi_bar) / psi_bar normalisation divides by zero. ",
                              "A patch with identically zero suitability cannot be ",
                              "normalised this way."),
                       .laser_fmt(which(psi_bar == 0))), call. = FALSE)
     }

     bar <- matrix(psi_bar, nrow = par$nticks, ncol = par$npatches, byrow = TRUE)
     matrix(par$beta_j0_env, nrow = par$nticks, ncol = par$npatches, byrow = TRUE) *
          (1 + (psi - bar) / bar)
}

#' Environmental decay rate from suitability
#'
#' \code{1 / (fast + pbeta(psi, a, b) * (slow - fast))}. The beta CDF maps
#' suitability in \[0, 1\] to \[0, 1\], potentially non-linearly, and that factor
#' interpolates the survival time between \code{decay_days_short} and
#' \code{decay_days_long}. So decay is FAST where suitability is low
#' (\code{psi = 0} gives \code{1 / fast}) and SLOW where it is high
#' (\code{psi = 1} gives \code{1 / slow}).
#'
#' This is the engine's only \pkg{scipy} call (\code{scipy.stats.beta.cdf});
#' base R's \code{pbeta()} is a drop-in.
#'
#' @param par Parameters from \code{laser_params()}.
#' @return A \code{[nticks, npatches]} matrix of per-day decay rates.
#' @keywords internal
laser_delta_jt <- function(par) {

     fast <- par$decay_days_short
     slow <- par$decay_days_long

     f <- pbeta(par$psi_jt, par$decay_shape_1, par$decay_shape_2)
     out <- 1 / (fast + f * (slow - fast))
     dim(out) <- dim(par$psi_jt)
     out
}

#' Build all four deterministic matrices
#'
#' @param par Parameters from \code{laser_params()}.
#' @return \code{par} with \code{pi_ij}, \code{beta_jt_human},
#'   \code{beta_jt_env} and \code{delta_jt} attached.
#' @keywords internal
laser_precompute <- function(par) {

     if ("HumanToHuman" %in% par$components) {
          d <- laser_distance_matrix(par$latitude, par$longitude)
          # The gravity model's populations are the INITIAL compartment sums,
          # not the running N -- pi_ij is built once and held constant.
          par$pi_ij <- laser_pi_ij(par$N_j_gravity, d, par$mobility_omega,
                                   par$mobility_gamma)
          par$beta_jt_human <- laser_beta_jt_human(par)
     }
     if ("EnvToHuman" %in% par$components) {
          par$beta_jt_env <- laser_beta_jt_env(par)
     }
     if ("Environmental" %in% par$components) {
          par$delta_jt <- laser_delta_jt(par)
     }
     par
}
