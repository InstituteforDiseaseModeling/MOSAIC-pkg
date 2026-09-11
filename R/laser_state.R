#' Allocate simulation state for the R transmission engine
#'
#' State mirrors the Python engine's layout exactly: every per-patch series is
#' a \code{(nticks + 1) x npatches} matrix with **time in rows and patch in
#' columns**, and the transpose to the \code{[patch, time]} result orientation
#' happens once at the end in \code{laser_results.R}. Flipping to
#' \code{[patch, time]} early would mean re-deriving every off-by-one in the
#' port, so it is deliberately not done.
#'
#' Storage mode is per field, not uniform: anything counting people or events
#' is \code{integer} (inheriting \code{np.int32}'s rounding discipline), while
#' rates, hazards and continuous quantities are \code{double}.
#'
#' @section Why a list of per-tick vectors:
#' Each channel is a \strong{list of \code{nticks + 1} vectors}, one per tick,
#' rather than a \code{(nticks + 1) x npatches} matrix. Reading a tick is
#' \code{state$S[[row]]}; writing one is \code{state$S[[row]] <- v}.
#'
#' This is a measured decision, not a preference. Held as a matrix, a row write
#' costs about \strong{15 microseconds} against 0.65 for a row read, because the
#' subassignment copies the entire matrix. Line profiling attributed roughly
#' \strong{55 percent} of a full 1398-tick run to those writes alone, and the
#' engine performs about 40 of them per tick; converting to per-tick vectors took
#' the run from \strong{2.91 s to 1.18 s} and allocation from 4.9 GB to 987 MB.
#' A list element write is a pointer store, so it does not copy. The
#' \code{[tick, patch]} matrices the results contract requires are assembled
#' once, at the end, in \code{laser_results()}.
#'
#' @section Why an environment, not a list of channels:
#' The channel container is an \strong{environment}, so it has reference
#' semantics and the phase functions mutate it in place rather than returning a
#' modified copy.
#'
#' The consequence is that \code{state} is mutated by every phase, and the
#' \code{state <- phase(state, ...)} idiom in the tick loop is a convention
#' rather than a copy: the value returned is the same environment that went in.
#' Do not rely on a pre-call snapshot of the state remaining unchanged.
#'
#' @param nticks Integer number of simulation ticks.
#' @param npatches Integer number of patches.
#' @return An environment of state matrices (mutated in place by the phases).
#' @keywords internal
laser_alloc_state <- function(nticks, npatches) {

     nticks   <- as.integer(nticks)
     npatches <- as.integer(npatches)
     rows <- nticks + 1L

     # One list per channel, holding `rows` per-tick vectors of length npatches.
     # See "Why a list of per-tick vectors" below.
     int_series <- function(n = rows) rep(list(integer(npatches)), n)
     dbl_series <- function(n = rows) rep(list(numeric(npatches)), n)

     # Reference semantics -- see the note in this function's documentation.
     state <- new.env(parent = emptyenv())

     # Compartments (integer counts of people).
     for (nm in c("S", "E", "Isym", "Iasym", "R", "V1", "V2")) {
          state[[nm]] <- int_series()
     }

     # Per-patch integer event counts.
     for (nm in c("N", "births", "non_disease_deaths", "disease_deaths",
                  "new_symptomatic", "incidence", "incidence_env",
                  "incidence_human", "reported_cases", "reported_deaths")) {
          state[[nm]] <- int_series()
     }

     # Continuous per-patch quantities.
     for (nm in c("Lambda", "Psi", "W", "spatial_hazard")) {
          state[[nm]] <- dbl_series()
     }

     # `dose_one_doses` / `dose_two_doses` are nticks-shaped in the Python
     # engine, not nticks+1, and RInterface only transposes them (no trim).
     state$dose_one_doses <- int_series(nticks)
     state$dose_two_doses <- int_series(nticks)

     # `coupling` is the one channel that is not a time series: a single
     # [npatches, npatches] correlation matrix written once by DerivedValues on
     # the final tick. Allocated zero-filled, as the oracle does.
     state$coupling <- matrix(0, npatches, npatches)

     state$.nticks   <- nticks
     state$.npatches <- npatches
     state
}

#' Assert per-tick invariants on engine state
#'
#' These are checked independently of the Python oracle, which is the point:
#' they catch the class of bug where R and Python agree because both are
#' wrong, and they are the only correctness checks that survive once the
#' oracle is gone.
#'
#' @param state State environment from \code{laser_alloc_state()}.
#' @param tick Tick index just written (1-based row into the state matrices).
#' @param compartments Character vector of compartments in play; \code{N} is
#'   checked against the sum of exactly these.
#' @return Invisibly \code{TRUE}; errors on violation.
#' @keywords internal
laser_check_invariants <- function(state, tick, compartments) {

     row <- tick + 1L

     for (nm in compartments) {
          v <- state[[nm]][[row]]
          if (anyNA(v)) {
               stop(sprintf("Tick %d: %s contains NA at patch(es) %s.",
                            tick, nm, .laser_fmt(which(is.na(v)))), call. = FALSE)
          }
          if (any(v < 0L)) {
               stop(sprintf("Tick %d: %s is negative at patch(es) %s.",
                            tick, nm, .laser_fmt(which(v < 0L))), call. = FALSE)
          }
     }

     expected <- Reduce(`+`, lapply(compartments, function(nm) state[[nm]][[row]]))
     actual <- state$N[[row]]
     bad <- which(actual != expected)
     if (length(bad)) {
          stop(sprintf("Tick %d: N does not equal the sum of %s at patch(es) %s (N %s vs sum %s).",
                       tick, paste(compartments, collapse = "+"),
                       .laser_fmt(bad), .laser_fmt(actual[bad]),
                       .laser_fmt(expected[bad])), call. = FALSE)
     }

     for (nm in intersect(c("Lambda", "Psi", "W"), names(state))) {
          v <- state[[nm]][[row]]
          if (any(!is.finite(v))) {
               stop(sprintf("Tick %d: %s is not finite at patch(es) %s.",
                            tick, nm, .laser_fmt(which(!is.finite(v)))), call. = FALSE)
          }
          if (any(v < 0)) {
               stop(sprintf("Tick %d: %s is negative at patch(es) %s.",
                            tick, nm, .laser_fmt(which(v < 0))), call. = FALSE)
          }
     }

     invisible(TRUE)
}
