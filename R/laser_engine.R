#' Run the R transmission engine
#'
#' Pure-R port of the laser-cholera metapopulation engine. During the migration
#' this is reached as \code{run_LASER_R()}; at cutover it becomes
#' \code{run_LASER()} and the Python path is removed. There is deliberately no
#' \code{engine =} switch -- the point of the migration is that there is one
#' engine.
#'
#' @param config Config list, or a path to a \code{.json} / \code{.json.gz}
#'   file. See \code{laser_params()} for the normalisation and validation
#'   applied.
#' @param seed Integer seed. If \code{NULL}, uses \code{config$seed} when
#'   present, otherwise \code{123L}.
#' @param quiet Logical; suppress progress reporting.
#' @param components Character vector naming the pipeline subset to run.
#'   Defaults to the full dynamics pipeline. Used by the parity harness to
#'   compare a component at a time against the oracle; production callers
#'   should not set it.
#' @param rng Either \code{"rng"} (draw normally) or \code{"replay"} (consume
#'   \code{record} and assert every draw matches). Replay is a test mode.
#' @param record Replay fixture from \code{laser_read_fixture()}.
#'
#' @return A list with \code{params} (the normalised config), \code{results}
#'   (the result channels as \code{[patch, time]} matrices) and \code{seed}.
#'
#' @keywords internal
run_LASER_R <- function(config,
                        seed       = NULL,
                        quiet      = FALSE,
                        components = LASER_PIPELINE,
                        rng        = c("rng", "replay"),
                        record     = NULL) {

     rng <- match.arg(rng)

     # Validate the requested pipeline before touching the config: "you asked
     # for a component that does not exist" is both cheaper to detect and more
     # actionable than a downstream config error, and checking it second would
     # let a config problem mask it.
     unported <- setdiff(components, names(LASER_PHASE_FUNCTIONS))
     if (length(unported)) {
          stop(sprintf(paste0("Component(s) not yet ported to R: %s. Requesting ",
                              "an unported component errors rather than being ",
                              "skipped -- a silently short pipeline would look ",
                              "green while simulating the wrong model."),
                       paste(unported, collapse = ", ")), call. = FALSE)
     }

     par <- laser_params(config, components = components)

     if (is.null(seed)) {
          seed <- if (!is.null(par$seed)) as.integer(par$seed) else 123L
     } else {
          seed <- as.integer(seed)
     }

     # Isolate the RNG stream and restore the caller's on the way out, so
     # calling the engine never perturbs the caller. Registered before any
     # work so an error mid-run still restores.
     if (rng == "rng") {
          rng_state <- .laser_rng_begin(seed)
          on.exit(.laser_rng_end(rng_state), add = TRUE)
     }

     ctl <- laser_draws(mode = rng, seed = seed, record = record)

     state <- laser_alloc_state(par$nticks, par$npatches)
     state <- laser_seed_state(state, par)
     state <- .laser_seed_census(state, par, ctl)

     phases <- LASER_PHASE_FUNCTIONS[components]

     for (tick in seq.int(0L, par$nticks - 1L)) {
          for (phase in phases) {
               state <- phase(state, par, ctl, tick)
          }
          if (isTRUE(par$check_invariants)) {
               laser_check_invariants(state, tick + 1L, par$compartments)
          }
     }

     if (rng == "replay") laser_assert_replay_complete(ctl)

     out <- list(
          params  = par$config,
          results = laser_results(state, par),
          seed    = seed
     )
     attr(out, "laser_provenance") <- list(
          r_version   = R.version.string,
          rng_kind    = RNGkind(),
          mosaic_version = tryCatch(
               as.character(utils::packageVersion("MOSAIC")),
               error = function(e) NA_character_),
          components  = components
     )
     attr(out, "laser_coverage") <- laser_draw_coverage(ctl)
     out
}

#' The dynamics pipeline, in canonical order
#'
#' Fixed by \code{model.py:566-579}. \code{Analyzer}, \code{Recorder} and
#' \code{Parameters} follow it in the Python pipeline but are I/O and
#' diagnostics, consume no randomness, and are not part of the R contract.
#'
#' @keywords internal
LASER_PIPELINE <- c(
     "Susceptible", "Exposed", "Recovered", "Infectious", "Vaccinated",
     "Census", "HumanToHuman", "EnvToHuman", "Environmental", "DerivedValues"
)

# Phase dispatch table. Components are added here as they are ported; a
# requested component with no entry errors rather than being silently skipped,
# which is what would otherwise let a half-finished pipeline look green.
LASER_PHASE_FUNCTIONS <- list(
     Susceptible = laser_phase_susceptible,
     Census      = laser_phase_census
)
