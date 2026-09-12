#' Run the cholera transmission model
#'
#' Simulates the metapopulation SEIR model over the configured window and
#' returns the result channels. This is the package's only engine entry point:
#' there is deliberately no \code{engine =} switch, because there is only one
#' engine. Prior to v0.66.0 this function was a \pkg{reticulate} bridge to the
#' Python \code{laser-cholera} package; it is now pure R and the two agree to
#' the tolerances recorded in \code{tests/testthat/fixtures/ORACLE.md}.
#'
#' @param config Config list, or a path to a \code{.json} / \code{.json.gz}
#'   file. See \code{laser_params()} for the normalisation and validation
#'   applied.
#' @param seed Integer seed. If \code{NULL}, uses \code{config$seed} when
#'   present, otherwise \code{123L}. The engine draws from an isolated RNG
#'   stream and restores the caller's on exit, so calling it never perturbs
#'   the caller's \code{.Random.seed}.
#' @param quiet Logical; suppress progress reporting. Accepted for call
#'   compatibility -- the R engine reports nothing either way.
#' @param components Character vector naming the pipeline subset to run.
#'   Defaults to the full dynamics pipeline. Used by the parity harness to
#'   compare a component at a time against the oracle; production callers
#'   should not set it.
#' @param rng Either \code{"rng"} (draw normally) or \code{"replay"} (consume
#'   \code{record} and assert every draw matches). Replay is a test mode.
#' @param record Replay fixture from \code{laser_read_fixture()}.
#' @param ... Reserved. Supplying an argument removed with the Python engine
#'   (\code{py_module}, \code{visualize}, \code{pdf}, \code{outdir}) raises an
#'   error naming it rather than silently ignoring it. See
#'   \link{deprecated_dask}.
#'
#' @return A list with \code{params} (the normalised config), \code{results}
#'   (the 28 result channels as \code{[patch, time]} matrices, except
#'   \code{pi_ij} and \code{coupling} which are \code{[patch, patch]}) and
#'   \code{seed}.
#'
#' @examples
#' \dontrun{
#' # Run from a config file:
#' result <- run_LASER(config = "path/to/laser_params.json", seed = 20250418L)
#'
#' # Run from a config object (uses config$seed if present, else 123L):
#' result <- run_LASER(config = config_default, quiet = TRUE)
#'
#' dim(result$results$reported_cases)   # [locations, days]
#' }
#'
#' @export
run_LASER <- function(config,
                      seed       = NULL,
                      quiet      = FALSE,
                      components = LASER_PIPELINE,
                      rng        = c("rng", "replay"),
                      record     = NULL,
                      ...) {

     .mosaic_reject_removed_args(list(...), "run_LASER")

     rng <- match.arg(rng)

     # Validate the requested pipeline before touching the config: "you asked
     # for a component that does not exist" is both cheaper to detect and more
     # actionable than a downstream config error, and checking it second would
     # let a config problem mask it.
     unknown <- setdiff(components, LASER_PIPELINE)
     if (length(unknown)) {
          stop(sprintf("Unknown component(s): %s. The pipeline is: %s.",
                       paste(unknown, collapse = ", "),
                       paste(LASER_PIPELINE, collapse = ", ")), call. = FALSE)
     }
     unported <- setdiff(components, names(.LASER_PHASE_FUNCTIONS))
     if (length(unported)) {
          stop(sprintf(paste0("Component(s) not yet ported to R: %s. Requesting ",
                              "an unported component errors rather than being ",
                              "skipped -- a silently short pipeline would look ",
                              "green while simulating the wrong model."),
                       paste(unported, collapse = ", ")), call. = FALSE)
     }
     if (anyDuplicated(components)) {
          stop("`components` names the same component more than once: ",
               paste(unique(components[duplicated(components)]), collapse = ", "),
               ".", call. = FALSE)
     }

     # Phase order is fixed by the engine (`model.py:566-579`), not by the
     # caller. Running the requested subset in the order it happened to be
     # written in would silently simulate a different model -- `Census` before
     # `Susceptible`, say, sums the previous tick's compartments.
     components <- LASER_PIPELINE[LASER_PIPELINE %in% components]

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

     phases <- .LASER_PHASE_FUNCTIONS[components]

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

#' @rdname run_LASER
#' @export
run_laser <- run_LASER

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

# Phase dispatch table. Complete as of A-3: every component of LASER_PIPELINE
# has an entry. The unported-component guard in run_LASER() is kept even so
# -- it is what stops a half-finished pipeline from looking green, and it is
# the check that would fire if a future component were added to the pipeline
# list without an implementation.
.LASER_PHASE_FUNCTIONS <- list(
     Susceptible   = laser_phase_susceptible,
     Exposed       = laser_phase_exposed,
     Recovered     = laser_phase_recovered,
     Infectious    = laser_phase_infectious,
     Vaccinated    = laser_phase_vaccinated,
     Census        = laser_phase_census,
     HumanToHuman  = laser_phase_human_to_human,
     EnvToHuman    = laser_phase_env_to_human,
     Environmental = laser_phase_environmental,
     DerivedValues = laser_phase_derived_values
)
