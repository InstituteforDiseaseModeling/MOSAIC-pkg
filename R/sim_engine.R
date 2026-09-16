#' Run the cholera transmission model
#'
#' Simulates the metapopulation SEIR model over the configured window and
#' returns the result channels. This is the package's only engine entry point:
#' there is deliberately no \code{engine =} switch, because there is only one
#' engine. Prior to v0.68.0 this function was a \pkg{reticulate} bridge to the
#' Python \code{laser-cholera} package; it is now pure R and the two agree to
#' the tolerances recorded in \code{tests/testthat/fixtures/ORACLE.md}.
#'
#' @param config Config list, or a path to a \code{.json} / \code{.json.gz}
#'   file. See \code{sim_params()} for the normalisation and validation
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
#' @param record Replay fixture from \code{sim_read_fixture()}.
#' @param ... Reserved. Supplying an argument removed with the Python engine
#'   (\code{py_module}, \code{visualize}, \code{pdf}, \code{outdir}) raises an
#'   error naming it rather than silently ignoring it. See
#'   \link{removed_api}.
#'
#' @return A list with \code{params} (the normalised config), \code{results}
#'   (the 28 result channels as \code{[patch, time]} matrices, except
#'   \code{pi_ij} and \code{coupling} which are \code{[patch, patch]}) and
#'   \code{seed}.
#'
#' @examples
#' \dontrun{
#' # Run from a config file:
#' result <- run_simulation(config = "path/to/sim_params.json", seed = 20250418L)
#'
#' # Run from a config object (uses config$seed if present, else 123L):
#' result <- run_simulation(config = config_default, quiet = TRUE)
#'
#' dim(result$results$reported_cases)   # [locations, days]
#' }
#'
#' @export
run_simulation <- function(config,
                           seed       = NULL,
                           quiet      = FALSE,
                           components = SIM_PIPELINE,
                           rng        = c("rng", "replay"),
                           record     = NULL,
                           ...) {

     .mosaic_reject_removed_args(list(...), "run_simulation")

     rng <- match.arg(rng)

     # Validate the requested pipeline before touching the config: "you asked
     # for a component that does not exist" is both cheaper to detect and more
     # actionable than a downstream config error, and checking it second would
     # let a config problem mask it.
     unknown <- setdiff(components, SIM_PIPELINE)
     if (length(unknown)) {
          stop(sprintf("Unknown component(s): %s. The pipeline is: %s.",
                       paste(unknown, collapse = ", "),
                       paste(SIM_PIPELINE, collapse = ", ")), call. = FALSE)
     }
     unported <- setdiff(components, names(.SIM_PHASE_FUNCTIONS))
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
     components <- SIM_PIPELINE[SIM_PIPELINE %in% components]

     par <- sim_params(config, components = components)

     if (is.null(seed)) {
          seed <- if (!is.null(par$seed)) as.integer(par$seed) else 123L
     } else {
          seed <- as.integer(seed)
     }

     # Isolate the RNG stream and restore the caller's on the way out, so
     # calling the engine never perturbs the caller. Registered before any
     # work so an error mid-run still restores.
     if (rng == "rng") {
          rng_state <- .sim_rng_begin(seed)
          on.exit(.sim_rng_end(rng_state), add = TRUE)
     }

     ctl <- sim_draws(mode = rng, seed = seed, record = record)

     state <- sim_alloc_state(par$nticks, par$npatches)
     state <- sim_seed_state(state, par)
     state <- .sim_seed_census(state, par, ctl)

     phases <- .SIM_PHASE_FUNCTIONS[components]

     for (tick in seq.int(0L, par$nticks - 1L)) {
          for (phase in phases) {
               state <- phase(state, par, ctl, tick)
          }
          if (isTRUE(par$check_invariants)) {
               sim_check_invariants(state, tick + 1L, par$compartments)
          }
     }

     if (rng == "replay") sim_assert_replay_complete(ctl)

     out <- list(
          params  = par$config,
          results = sim_results(state, par),
          seed    = seed
     )
     attr(out, "sim_provenance") <- list(
          r_version   = R.version.string,
          rng_kind    = RNGkind(),
          mosaic_version = tryCatch(
               as.character(utils::packageVersion("MOSAIC")),
               error = function(e) NA_character_),
          components  = components
     )
     attr(out, "sim_coverage") <- sim_draw_coverage(ctl)
     out
}


#' The dynamics pipeline, in canonical order
#'
#' Fixed by \code{model.py:566-579}. \code{Analyzer}, \code{Recorder} and
#' \code{Parameters} follow it in the Python pipeline but are I/O and
#' diagnostics, consume no randomness, and are not part of the R contract.
#'
#' @keywords internal
SIM_PIPELINE <- c(
     "Susceptible", "Exposed", "Recovered", "Infectious", "Vaccinated",
     "Census", "HumanToHuman", "EnvToHuman", "Environmental", "DerivedValues"
)

# Phase dispatch table. Complete as of A-3: every component of SIM_PIPELINE
# has an entry. The unported-component guard in run_simulation() is kept even so
# -- it is what stops a half-finished pipeline from looking green, and it is
# the check that would fire if a future component were added to the pipeline
# list without an implementation.
.SIM_PHASE_FUNCTIONS <- list(
     Susceptible   = sim_phase_susceptible,
     Exposed       = sim_phase_exposed,
     Recovered     = sim_phase_recovered,
     Infectious    = sim_phase_infectious,
     Vaccinated    = sim_phase_vaccinated,
     Census        = sim_phase_census,
     HumanToHuman  = sim_phase_human_to_human,
     EnvToHuman    = sim_phase_env_to_human,
     Environmental = sim_phase_environmental,
     DerivedValues = sim_phase_derived_values
)
