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
#' @param nticks Integer number of simulation ticks.
#' @param npatches Integer number of patches.
#' @return A list of state matrices.
#' @keywords internal
laser_alloc_state <- function(nticks, npatches) {

     nticks   <- as.integer(nticks)
     npatches <- as.integer(npatches)
     rows <- nticks + 1L

     int_mat <- function() matrix(0L, nrow = rows, ncol = npatches)
     dbl_mat <- function() matrix(0, nrow = rows, ncol = npatches)

     state <- list()

     # Compartments (integer counts of people).
     for (nm in c("S", "E", "Isym", "Iasym", "R", "V1", "V2")) {
          state[[nm]] <- int_mat()
     }

     # Per-patch integer event counts.
     for (nm in c("N", "births", "non_disease_deaths", "disease_deaths",
                  "new_symptomatic", "incidence", "incidence_env",
                  "incidence_human", "reported_cases", "reported_deaths")) {
          state[[nm]] <- int_mat()
     }

     # Continuous per-patch quantities.
     for (nm in c("Lambda", "Psi", "W", "spatial_hazard")) {
          state[[nm]] <- dbl_mat()
     }

     # `dose_one_doses` / `dose_two_doses` are nticks-shaped in the Python
     # engine, not nticks+1, and RInterface only transposes them (no trim).
     state$dose_one_doses <- matrix(0L, nrow = nticks, ncol = npatches)
     state$dose_two_doses <- matrix(0L, nrow = nticks, ncol = npatches)

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
#' @param state State list from \code{laser_alloc_state()}.
#' @param tick Tick index just written (1-based row into the state matrices).
#' @param compartments Character vector of compartments in play; \code{N} is
#'   checked against the sum of exactly these.
#' @return Invisibly \code{TRUE}; errors on violation.
#' @keywords internal
laser_check_invariants <- function(state, tick, compartments) {

     row <- tick + 1L

     for (nm in compartments) {
          v <- state[[nm]][row, ]
          if (anyNA(v)) {
               stop(sprintf("Tick %d: %s contains NA at patch(es) %s.",
                            tick, nm, .laser_fmt(which(is.na(v)))), call. = FALSE)
          }
          if (any(v < 0L)) {
               stop(sprintf("Tick %d: %s is negative at patch(es) %s.",
                            tick, nm, .laser_fmt(which(v < 0L))), call. = FALSE)
          }
     }

     expected <- Reduce(`+`, lapply(compartments, function(nm) state[[nm]][row, ]))
     actual <- state$N[row, ]
     bad <- which(actual != expected)
     if (length(bad)) {
          stop(sprintf("Tick %d: N does not equal the sum of %s at patch(es) %s (N %s vs sum %s).",
                       tick, paste(compartments, collapse = "+"),
                       .laser_fmt(bad), .laser_fmt(actual[bad]),
                       .laser_fmt(expected[bad])), call. = FALSE)
     }

     for (nm in intersect(c("Lambda", "Psi", "W"), names(state))) {
          v <- state[[nm]][row, ]
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
