# =============================================================================
# calc_model_ensemble_task.R
#
# The per-(parameter set, stochastic replicate) simulation task used by
# calc_model_ensemble(). This lives at package level rather than as a closure
# inside calc_model_ensemble() for three reasons:
#
#   1. It is the engine invocation site for the ensemble path. The migration to
#      the R transmission model swaps the engine here and nowhere else in this
#      file (see migrate-laser-r.md, phase A-4).
#   2. PSOCK workers resolve it from the loaded MOSAIC namespace, so the task
#      closure carries no function payload and needs no clusterExport().
#   3. It is the seam the ensemble tests mock. Until v0.65.0 those tests fed
#      synthetic engine output in through calc_model_ensemble(precomputed_results=),
#      an argument whose only production callers were the Dask gather and the
#      Dask medoid dispatch. Removing the Dask backend removed both callers, but
#      the tests were never about Dask -- they assert weight/seed alignment,
#      artifact masking and trajectory reduction, all properties of the
#      surviving local path. Mocking this function preserves every one of those
#      assertions and exercises strictly more production code than the old
#      argument did, because the task list, dispatch and gather now run for real.
# =============================================================================

#' Run one ensemble simulation task
#'
#' Executes a single (parameter set, stochastic replicate) simulation and
#' returns a flat result record. Never throws: engine failures are caught and
#' returned as \code{success = FALSE} records so one bad parameter set cannot
#' abort the ensemble.
#'
#' @param task_info One-row data.frame (or list) with \code{param_idx} and
#'   \code{stoch_idx}.
#' @param param_configs_list List of per-parameter-set LASER configs.
#' @param capture_traj Logical. Capture the trajectory channels.
#' @param traj_channels Character vector of result channels to capture.
#' @param traj_scratch Directory to stream captured channels to, or NULL to
#'   attach them to the returned record.
#'
#' @return A list with \code{param_idx}, \code{stoch_idx}, \code{success},
#'   and on success \code{reported_cases}, \code{reported_deaths},
#'   \code{spatial_hazard}, \code{coupling}, \code{pi_ij} (and optionally
#'   \code{traj}/\code{traj_epi}); on failure \code{error}.
#'
#' @keywords internal
#' @noRd
.mosaic_ensemble_sim_task <- function(task_info, param_configs_list,
                                  capture_traj = FALSE,
                                  traj_channels = character(0),
                                  traj_scratch = NULL) {
  param_idx <- task_info$param_idx
  stoch_idx <- task_info$stoch_idx
  tryCatch({
    if (!exists("lc", where = .GlobalEnv, inherits = FALSE)) {
      lc <- reticulate::import("laser.cholera.metapop.model")
      .mosaic_strip_laser_file_handler()
    } else {
      lc <- get("lc", envir = .GlobalEnv)
    }
    param_config <- param_configs_list[[param_idx]]
    param_config$seed <- (param_idx * 1000L) + stoch_idx
    model <- lc$run_model(
      paramfile = .mosaic_prepare_config_for_python(param_config),
      quiet = TRUE
    )
    # Extract the engine's spatial-structure arrays (J x T hazard, J x J
    # coupling, J x J pi_ij) BEFORE the model is discarded below (F1). These
    # are computed by the engine's DerivedValues component at the final tick
    # and otherwise lost when the model object is gc'd. tryCatch each so an
    # engine that drops DerivedValues simply yields NULL (warn+skip downstream).
    sh  <- tryCatch(model$results$spatial_hazard, error = function(e) NULL)
    cpl <- tryCatch(model$results$coupling,       error = function(e) NULL)
    pij <- tryCatch(model$results$pi_ij,          error = function(e) NULL)
    result <- list(param_idx = param_idx, stoch_idx = stoch_idx,
                   reported_cases = model$results$reported_cases,
                   reported_deaths = model$results$reported_deaths,
                   spatial_hazard = sh,
                   coupling       = cpl,
                   pi_ij          = pij,
                   success = TRUE)
    # Trajectory channels (comprehensive internal-state capture). Harvested
    # here, where model$results is in hand, at zero marginal sim cost --
    # NEVER re-simulated downstream (PLAN sec 3, capture-don't-replay). Each
    # channel tryCatch'd -> omitted if the engine build lacks it. Also carry
    # the per-member epidemic-flag inputs (sampled per set) so epidemic_frac
    # is reconstructable on the master regardless of backend (DM F5).
    if (isTRUE(capture_traj) && length(traj_channels)) {
      traj <- list()
      for (ch in traj_channels) {
        v <- tryCatch(model$results[[ch]], error = function(e) NULL)
        if (!is.null(v)) traj[[ch]] <- v
      }
      traj_epi <- list(
        delta_reporting_cases = tryCatch(param_config$delta_reporting_cases,
                                         error = function(e) NULL),
        epidemic_threshold    = tryCatch(param_config$epidemic_threshold,
                                         error = function(e) NULL)
      )
      # STREAM-TO-DISK: write channels to local scratch (PSOCK workers share
      # the master's filesystem) and do NOT carry them back on the record --
      # this is what keeps the gather (and peak RAM) flat regardless of
      # n_param. If no scratch dir was provided (direct call without spill),
      # fall back to attaching them to the record for the in-process reduce.
      if (length(traj) && !is.null(traj_scratch) && nzchar(traj_scratch)) {
        tryCatch(
          saveRDS(list(traj = traj, traj_epi = traj_epi),
                  file.path(traj_scratch, sprintf("sim_%d_%d.rds",
                                                  param_idx, stoch_idx))),
          error = function(e) NULL)
      } else if (length(traj)) {
        result$traj <- traj
        result$traj_epi <- traj_epi
      }
    }
    gc(verbose = FALSE)
    reticulate::import("gc")$collect()
    result
  }, error = function(e) {
    list(param_idx = param_idx, stoch_idx = stoch_idx,
         success = FALSE, error = as.character(e))
  })
}
