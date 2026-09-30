#' Allocate simulation state for the R transmission engine
#'
#' State mirrors the Python engine's layout exactly: every per-patch series is
#' conceptually a \code{(nticks + 1) x npatches} array with **time in rows and
#' patch in columns**, and the transpose to the \code{[patch, time]} result
#' orientation happens once at the end in \code{sim_results.R}. Flipping to
#' \code{[patch, time]} early would mean re-deriving every off-by-one in the
#' port, so it is deliberately not done.
#'
#' Storage mode is per field, not uniform: anything counting people or events
#' is \code{integer} (inheriting \code{np.int32}'s rounding discipline), while
#' rates, hazards and continuous quantities are \code{double}.
#'
#' @section Why one environment per tick:
#' State is \code{state$rows}, a list of \code{nticks + 1} \strong{environments},
#' one per tick, each holding every channel for that tick. Reading is
#' \code{state$rows[[row]]$S}; writing is \code{state$rows[[row]]$S <- v}. The
#' phases bind the two rows they need once (\code{rh <- state$rows[[here]]},
#' \code{rn <- state$rows[[nxt]]}) and then address channels by name.
#'
#' This is a measured decision, not a preference, and it is the second such
#' change to this structure -- the history is worth keeping because both moves
#' were driven by the same mechanism.
#'
#' Held as one \code{(nticks + 1) x npatches} matrix per channel, a row write
#' cost about \strong{15 microseconds} against 0.65 for a row read, because the
#' subassignment copied the entire matrix. Moving to a per-channel \emph{list}
#' of per-tick vectors took a full 1398-tick run from \strong{2.91 s to 1.18 s}
#' and allocation from 4.9 GB to 987 MB, on the reasoning that "a list element
#' write is a pointer store, so it does not copy".
#'
#' That reasoning was wrong, and measurably so. The channel lists were reached
#' through an environment, so \code{state$S[[i]] <- v} is a
#' \emph{subassignment} into \code{state$S}: the \code{*tmp*} fetch raises the
#' list's reference count, and \code{[[<-} therefore duplicated the whole
#' \code{nticks + 1} pointer vector on \strong{every write}. \code{tracemem()}
#' confirms a copy per write, and the cost was linear in \code{nticks}
#' (1.5 / 3.3 / 5.9 / 11.6 microseconds per write at 200 / 700 / 1399 / 2800
#' rows) -- so it was invisible at fixture scale and worst in production.
#' Padding the channel lists to 4x their length without touching the dynamics
#' moved a 1.02 s run to 2.08 s, pricing the copies at \strong{0.354 s, about
#' 35 percent of the run}.
#'
#' An environment binding is a pointer store with no such copy, and there is no
#' longer any long vector to duplicate: \code{state$rows} is written once, here,
#' and never again. Per-write cost falls from \strong{6.15 to 0.20
#' microseconds} and stops depending on \code{nticks}; reads go from 0.13 to
#' 0.18, which is why the phases hoist the row lookup out of the channel
#' accesses rather than repeating \code{state$rows[[here]]}.
#'
#' The profiler had attributed this cost to the phase bodies (52.65 percent
#' self time) and to \code{<GC>} (11.16 percent), which is where allocation
#' churn always lands -- not to anything that looks like a state write. See
#' lesson 17 in \code{CLAUDE.md}.
#'
#' @section Reference semantics:
#' Both \code{state} and each element of \code{state$rows} are environments, so
#' the phase functions mutate them in place rather than returning a modified
#' copy. The \code{state <- phase(state, ...)} idiom in the tick loop is a
#' convention rather than a copy: the value returned is the same environment
#' that went in. Do not rely on a pre-call snapshot of the state remaining
#' unchanged.
#'
#' @param nticks Integer number of simulation ticks.
#' @param npatches Integer number of patches.
#' @return An environment holding \code{rows} (the per-tick environments),
#'   \code{coupling}, and the shape/prototype metadata the results assembler
#'   and the invariant checker read.
#' @keywords internal
sim_alloc_state <- function(nticks, npatches) {

     nticks   <- as.integer(nticks)
     npatches <- as.integer(npatches)
     rows <- nticks + 1L

     zi <- integer(npatches)
     zd <- numeric(npatches)

     # Compartments and per-patch integer event counts.
     int_channels <- c(
          "S", "E", "Isym", "Iasym", "R", "V1", "V2",
          "N", "births", "non_disease_deaths", "disease_deaths",
          "new_symptomatic", "incidence", "incidence_env", "incidence_human",
          "reported_cases", "reported_deaths"
     )
     # Continuous per-patch quantities.
     dbl_channels <- c("Lambda", "Psi", "W", "spatial_hazard")

     # `dose_one_doses` / `dose_two_doses` are nticks-shaped in the Python
     # engine, not nticks+1, and RInterface only transposes them (no trim). The
     # final row therefore does not carry them at all, so a read of row
     # nticks+1 returns NULL rather than a plausible-looking zero -- the same
     # contract the old `int_series(nticks)` gave by erroring on the index.
     dose_channels <- c("dose_one_doses", "dose_two_doses")

     protos <- function(nms, z) stats::setNames(rep(list(z), length(nms)), nms)
     proto <- c(protos(int_channels, zi), protos(dbl_channels, zd),
                protos(dose_channels, zi))

     # Every row starts at the zero value for its storage mode. The zero
     # vectors are shared across rows, which is safe because no channel is ever
     # mutated in place: every write rebinds the name to a freshly computed
     # vector.
     tmpl_mid  <- proto
     tmpl_last <- proto[!(names(proto) %in% dose_channels)]

     rowlist <- vector("list", rows)
     for (i in seq_len(nticks)) {
          rowlist[[i]] <- list2env(tmpl_mid,
                                   envir = new.env(hash = TRUE, parent = emptyenv()))
     }
     rowlist[[rows]] <- list2env(tmpl_last,
                                 envir = new.env(hash = TRUE, parent = emptyenv()))

     # Reference semantics -- see the note in this function's documentation.
     state <- new.env(parent = emptyenv())
     state$rows <- rowlist

     # `coupling` is the one channel that is not a time series: a single
     # [npatches, npatches] correlation matrix written once by DerivedValues on
     # the final tick. Allocated zero-filled, as the oracle does.
     state$coupling <- matrix(0, npatches, npatches)

     state$.nticks   <- nticks
     state$.npatches <- npatches

     # `.channels` is the single source of truth for which channels exist:
     # `sim_results()` intersects against it rather than against `names(state)`,
     # which no longer lists them. `.proto` records the storage mode each
     # channel was allocated with; nothing in the engine reads it, but
     # test-sim_alloc_state.R asserts the two agree with what the row
     # environments actually hold, so the registry cannot drift from the
     # allocation (lesson 15: assert membership against the source).
     state$.proto    <- proto
     state$.channels <- names(proto)
     state
}

# Assemble a [length(rows), npatches] matrix from one channel of the per-tick
# row environments -- the replacement for `do.call(rbind, series)`.
#
# Deliberately `rbind` and not `vapply(..., state$.proto[[nm]])`, which would be
# faster and is the obvious thing to reach for. `vapply` enforces its prototype's
# storage mode, and that is STRICTER than what this used to do: in replay mode
# the draws come back from the recorded fixture rather than from `rbinom()`, so
# an integer-allocated channel can legitimately hold doubles, and `rbind`
# promoted the matrix where `vapply` errors. Eight Tier B replay tests fail on
# the vapply form. Whether replay ought to preserve storage mode is a separate
# question from making the engine faster, so the promotion behaviour is
# reproduced rather than tightened.
#
# `rbind` also gives the right orientation for free, including the npatches == 1
# case: rbind-ing `nrows` length-1 vectors yields [nrows, 1], where transposing
# a vapply result would have yielded [1, nrows] -- time in the columns -- and
# silently returned a transposed single-patch series.
.sim_gather <- function(state, nm, rows) {
     do.call(rbind, lapply(state$rows[rows], function(e) e[[nm]]))
}

#' Assert per-tick invariants on engine state
#'
#' These are checked independently of the Python oracle, which is the point:
#' they catch the class of bug where R and Python agree because both are
#' wrong, and they are the only correctness checks that survive once the
#' oracle is gone.
#'
#' @param state State environment from \code{sim_alloc_state()}.
#' @param tick Tick index just written (1-based row into the state series).
#' @param compartments Character vector of compartments in play; \code{N} is
#'   checked against the sum of exactly these.
#' @return Invisibly \code{TRUE}; errors on violation.
#' @keywords internal
sim_check_invariants <- function(state, tick, compartments) {

     # The row environment is looked up once; every check below addresses
     # channels by name within it.
     r <- state$rows[[tick + 1L]]

     # NA and negativity share a single `min()` traversal: an NA anywhere makes
     # `min()` NA, so both checks fall out of one pass that allocates nothing.
     # `which()` is paid only on failure. The previous `anyNA(v)` +
     # `any(v < 0L)` pair walked the vector twice and allocated a logical
     # vector per compartment per tick.
     for (nm in compartments) {
          v <- r[[nm]]
          mn <- min(v)
          if (is.na(mn)) {
               stop(sprintf("Tick %d: %s contains NA at patch(es) %s.",
                            tick, nm, .sim_fmt(which(is.na(v)))), call. = FALSE)
          }
          if (mn < 0L) {
               stop(sprintf("Tick %d: %s is negative at patch(es) %s.",
                            tick, nm, .sim_fmt(which(v < 0L))), call. = FALSE)
          }
     }

     # N == sum(compartments), accumulated in a loop. The previous
     # `Reduce(`+`, lapply(compartments, ...))` allocated a list of one vector
     # per compartment plus an intermediate sum per step, on every tick, and
     # that allocation churn -- not the arithmetic -- is what made this
     # assertion cost 20% of engine runtime.
     if (length(compartments)) {
          expected <- r[[compartments[1L]]]
          for (k in seq_along(compartments)[-1L]) {
               expected <- expected + r[[compartments[k]]]
          }
          actual <- r$N
          bad <- which(actual != expected)
          if (length(bad)) {
               stop(sprintf("Tick %d: N does not equal the sum of %s at patch(es) %s (N %s vs sum %s).",
                            tick, paste(compartments, collapse = "+"),
                            .sim_fmt(bad), .sim_fmt(actual[bad]),
                            .sim_fmt(expected[bad])), call. = FALSE)
          }
     }

     # Continuous channels: finite and non-negative. `min()`/`max()` rather
     # than `any(!is.finite(v))`, which allocated two logical vectors per
     # channel per tick; NA, NaN and +/-Inf all make one of the two extrema
     # non-finite, so nothing is missed. The channel list is iterated with a
     # NULL skip instead of `intersect(..., names(state))`, which built and
     # matched against the whole environment's name vector every tick to
     # rediscover three names that `sim_alloc_state()` always creates.
     for (nm in c("Lambda", "Psi", "W")) {
          v <- r[[nm]]
          if (is.null(v)) next
          mn <- min(v)
          if (!is.finite(mn) || !is.finite(max(v))) {
               stop(sprintf("Tick %d: %s is not finite at patch(es) %s.",
                            tick, nm, .sim_fmt(which(!is.finite(v)))), call. = FALSE)
          }
          if (mn < 0) {
               stop(sprintf("Tick %d: %s is negative at patch(es) %s.",
                            tick, nm, .sim_fmt(which(v < 0))), call. = FALSE)
          }
     }

     invisible(TRUE)
}
