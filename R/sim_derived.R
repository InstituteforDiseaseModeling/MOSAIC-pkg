#' End-of-run derived diagnostics: spatial hazard and coupling
#'
#' The tenth and last component of the pipeline (\code{derivedvalues.py}).
#' Unlike the other nine it computes nothing per tick: it is a whole-run
#' summary that fires once, on the final tick, and its two outputs
#' (\code{spatial_hazard}, \code{coupling}) are read by
#' \code{calc_model_ensemble()} and the spatial plots but by no other part of
#' the simulation. Nothing downstream of it is stochastic, and it consumes no
#' randomness, so it adds no draw sites.
#'
#' @section Which rows each quantity uses:
#' The two are sliced differently, and the difference is load-bearing:
#' \code{spatial_hazard} is computed from the oracle's \code{[1:, :]} slices --
#' the seed row dropped, so column \code{t} is the state at the \emph{end} of
#' tick \code{t} -- while \code{coupling} is handed the untrimmed arrays and so
#' correlates \code{nticks + 1} observations, seed row included.
#'
#' @section Not the same as the exported analysis helpers:
#' The package also exports \code{calc_spatial_hazard()} and
#' \code{calc_spatial_correlation_matrix()}, which compute quantities of the
#' same names. They are \strong{not} interchangeable with these and must not be
#' substituted for them: \code{calc_spatial_hazard()} counts waned vaccinees
#' (\code{V1_sus}, \code{V2_sus}) in the susceptible pool and zeroes the
#' mobility diagonal itself, and
#' \code{calc_spatial_correlation_matrix()} fills the diagonal with 1
#' unconditionally where the engine leaves a constant patch undefined. Either
#' substitution would change the engine's output and break parity with the
#' oracle. Neither helper has a production caller today; they are analysis-side
#' functions with their own definitions.
#'
#' @name sim_derived
#' @keywords internal
NULL

#' @rdname sim_derived
#' @keywords internal
sim_phase_derived_values <- function(state, par, ctl, tick) {

     .sim_at(ctl, tick, "DerivedValues")

     # `if tick == model.params.nticks - 1` (derivedvalues.py:143). A no-op on
     # every other tick -- there is no per-tick accumulation to do.
     if (tick != par$nticks - 1L) return(state)

     nticks   <- par$nticks
     npatches <- state$.npatches

     # State rows 2..nticks+1 are Python's `[1:, :]`; row 1 is the seed row the
     # oracle slices off. `beta_jt_human` is nticks-shaped already and is NOT
     # sliced, so its row t lines up with state row t + 1.
     rows <- seq.int(2L, nticks + 1L)
     S    <- .sim_gather(state, "S", rows)
     Njt  <- .sim_gather(state, "N", rows)
     Ijt  <- .sim_gather(state, "Isym", rows) + .sim_gather(state, "Iasym", rows)

     # Nothing here reaches an integer, so `1 - tau_i` stays in double rather
     # than going through `par$local_frac` -- see .sim_f32() for the rule.
     stay <- 1 - par$tau_i

     S_star <- .sim_by_patch(S, stay)
     beta_S <- par$beta_jt_human * S_star

     # `(tau_i * pi_ij.T).T`: row i scaled by tau_i[i], i.e. the fraction of
     # patch i that both leaves and arrives in j. The oracle's vectorised form
     # includes i == j, which contributes nothing because `pi_ij` has a zero
     # diagonal (sim_pi_ij) -- its own commented-out reference loop skips
     # the diagonal explicitly, and the two agree only for that reason.
     xfer_ij <- par$tau_i * par$pi_ij

     # Prevalence is measured against the METAPOPULATION total at t, not the
     # patch's own N. A length-nticks divisor recycles down the columns, which
     # is one value per row -- the orientation we want.
     y_bar <- (.sim_by_patch(Ijt, stay) + Ijt %*% xfer_ij) / rowSums(Njt)

     H <- beta_S * (-expm1(-(S_star / Njt) * y_bar)) / (1 + beta_S)

     for (i in seq_len(nticks)) state$rows[[rows[i]]]$spatial_hazard <- H[i, ]

     # Coupling takes the untrimmed series: nticks + 1 observations.
     all_rows <- seq_len(nticks + 1L)
     y <- (.sim_gather(state, "Isym", all_rows) +
                .sim_gather(state, "Iasym", all_rows)) /
          .sim_gather(state, "N", all_rows)
     state$coupling <- .sim_coupling(y)

     state
}

#' Pearson coupling matrix between per-patch prevalence series
#'
#' @param y \code{[nobs, npatches]} matrix of prevalence fractions.
#' @return An \code{[npatches, npatches]} correlation matrix; rows and columns
#'   of constant-prevalence patches are \code{NaN}.
#' @keywords internal
.sim_coupling <- function(y) {

     npatches <- ncol(y)
     out <- matrix(NaN, npatches, npatches)

     # Pearson correlation against a constant series is 0/0. The usual cause is
     # a patch that was never seeded and that no force of infection reached,
     # which is a legitimate model state rather than an error, so the oracle
     # fills those rows and columns with NaN rather than raising -- and
     # `plot_spatial_correlation_heatmap()` already masks them.
     #
     # The oracle's test is `y.var(axis=0) == 0`. Equality to the first
     # observation is the same test for any column that can actually arise (a
     # patch the epidemic never reached has y identically zero) and it cannot
     # be fooled by the rounding noise a two-pass variance leaves on a
     # constant non-zero column. NA counts as varying, as it does in numpy,
     # where `nan == 0` is False.
     varies <- apply(y, 2L, function(v) anyNA(v) || any(v != v[1L]))
     if (any(varies)) out[varies, varies] <- stats::cor(y[, varies, drop = FALSE])
     out
}

# Whole-series [rows, npatches] arrays come from .sim_gather() in sim_state.R.
# This is the only component that needs them, and it needs them once.

# Scale each patch's column by `v[patch]`. Written out rather than calling
# sweep() because the recycling direction is the thing most easily got wrong
# here: R recycles down columns, so the multiplier must be repeated per row.
.sim_by_patch <- function(m, v) m * rep(v, each = nrow(m))
