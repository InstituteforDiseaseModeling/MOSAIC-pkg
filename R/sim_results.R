#' Assemble engine results in the downstream contract
#'
#' Equivalent of the Python engine's \code{RInterface}. State is held
#' time-major (one environment per tick, each holding every channel); results
#' are patch-major (\code{[npatches, nticks]}). The three trimming rules are not
#' interchangeable and are reproduced exactly from \code{model.py:82-165}:
#'
#' \itemize{
#'   \item compartments, incidence, hazards, \code{N}, \code{W} drop the
#'     \code{t = 0} seed row (\code{[1:, :].T});
#'   \item event counts (births, deaths, reported) drop the \emph{last} row
#'     (\code{[:-1, :].T}), because they are written at \code{tick} rather than
#'     \code{tick + 1};
#'   \item doses and the precomputed beta/delta matrices are already
#'     \code{nticks}-shaped and are only transposed.
#' }
#'
#' Storage mode is per field: counts are \code{integer}, rates and hazards are
#' \code{double}. No dimnames -- the Python return has none and no consumer
#' reads any.
#'
#' @param state State environment from the run loop.
#' @param par Parameters from \code{sim_params()}.
#' @return Named list of \code{[npatches, nticks]} matrices.
#' @keywords internal
sim_results <- function(state, par) {

     nticks <- par$nticks
     out <- list()

     # State is one environment per tick (see sim_alloc_state); the [tick, patch]
     # matrix the trim/transpose rules operate on is assembled here, once,
     # rather than being maintained through the tick loop. The three rules
     # differ only in which rows they gather, so the trim is expressed as a row
     # index set rather than as a negative subscript on an assembled matrix.
     first_dropped <- seq.int(2L, nticks + 1L)   # `[1:, :]`
     last_dropped  <- seq_len(nticks)            # `[:-1, :]`

     for (nm in intersect(SIM_CHANNELS_TRIM_FIRST, state$.channels)) {
          out[[nm]] <- .sim_emit(.sim_gather(state, nm, first_dropped))
     }
     for (nm in intersect(SIM_CHANNELS_TRIM_LAST, state$.channels)) {
          out[[nm]] <- .sim_emit(.sim_gather(state, nm, last_dropped))
     }
     # Doses are already nticks-shaped: `sim_alloc_state()` does not give the
     # final row environment those two channels at all, so the row set is the
     # whole series rather than a trim.
     for (nm in intersect(SIM_CHANNELS_TRANSPOSE_ONLY, state$.channels)) {
          out[[nm]] <- .sim_emit(.sim_gather(state, nm, last_dropped))
     }
     for (nm in intersect(SIM_CHANNELS_PRECOMPUTED, names(par))) {
          out[[nm]] <- .sim_emit(par[[nm]])
     }
     # Both [npatches, npatches] and both passed through un-transposed
     # (`coupling` is symmetric, `pi_ij` is not but is already in the
     # [origin, destination] orientation the contract wants). They differ only
     # in where they live: `pi_ij` is a precomputed input, `coupling` is
     # written into the state by DerivedValues on the final tick.
     for (nm in intersect(SIM_CHANNELS_PASSTHROUGH, names(par)))   out[[nm]] <- par[[nm]]
     for (nm in intersect(SIM_CHANNELS_PASSTHROUGH, names(state))) out[[nm]] <- state[[nm]]

     # The two derived diagnostics exist only if `DerivedValues` ran: the
     # Python component allocates them in its own `__init__`, so a pipeline
     # subset without it returns neither channel. Returning the zero-filled
     # allocation instead would be a plausible-looking wrong value -- zero
     # correlation everywhere, no hazard anywhere -- rather than an obvious
     # absence. (The other components' channels are allocated unconditionally
     # and a subset run does return those as zeros; only these two are
     # consumed as standalone diagnostics, where a zero reads as a result.)
     if (!("DerivedValues" %in% par$components)) {
          out$spatial_hazard <- NULL
          out$coupling       <- NULL
     }

     out[SIM_CHANNELS[SIM_CHANNELS %in% names(out)]]
}

# Transpose to [npatches, nticks], preserving storage mode. `t()` on an
# integer matrix returns integer, so the per-field contract is inherited
# rather than re-asserted here.
.sim_emit <- function(m) t(m)

#' @rdname sim_results
#' @keywords internal
SIM_CHANNELS_TRIM_FIRST <- c(
     "S", "E", "Isym", "Iasym", "R", "V1", "V2",
     "new_symptomatic", "incidence", "incidence_env", "incidence_human",
     "Lambda", "N", "Psi", "spatial_hazard", "W"
)

#' @rdname sim_results
#' @keywords internal
SIM_CHANNELS_TRIM_LAST <- c(
     "births", "disease_deaths", "non_disease_deaths",
     "reported_cases", "reported_deaths"
)

#' @rdname sim_results
#' @keywords internal
SIM_CHANNELS_TRANSPOSE_ONLY <- c("dose_one_doses", "dose_two_doses")

#' @rdname sim_results
#' @keywords internal
SIM_CHANNELS_PRECOMPUTED <- c("beta_jt_env", "beta_jt_human", "delta_jt")

#' @rdname sim_results
#' @keywords internal
SIM_CHANNELS_PASSTHROUGH <- c("pi_ij", "coupling")

#' The 28 result channels, in a stable order
#' @rdname sim_results
#' @keywords internal
SIM_CHANNELS <- c(
     SIM_CHANNELS_TRIM_FIRST, SIM_CHANNELS_TRIM_LAST,
     SIM_CHANNELS_TRANSPOSE_ONLY, SIM_CHANNELS_PRECOMPUTED,
     SIM_CHANNELS_PASSTHROUGH
)

# Which channels are integer counts vs continuous quantities. Asserted by the
# return-contract test rather than left as documentation, so the table cannot
# drift from the code.
#' @rdname sim_results
#' @keywords internal
SIM_CHANNELS_INTEGER <- c(
     "S", "E", "Isym", "Iasym", "R", "V1", "V2", "N",
     "new_symptomatic", "incidence", "incidence_env", "incidence_human",
     "births", "disease_deaths", "non_disease_deaths",
     "reported_cases", "reported_deaths", "dose_one_doses", "dose_two_doses"
)

#' @rdname sim_results
#' @keywords internal
SIM_CHANNELS_DOUBLE <- setdiff(SIM_CHANNELS, SIM_CHANNELS_INTEGER)
