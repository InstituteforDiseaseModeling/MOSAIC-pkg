# =============================================================================
# helper-ensemble-mock.R
#
# Feeds canned engine records into calc_model_ensemble() by mocking its
# per-task simulation worker, MOSAIC:::.mosaic_ensemble_sim_task().
#
# Until v0.65.0 these tests injected the same records through
# calc_model_ensemble(precomputed_results = ). That argument's only production
# callers were the Dask gather and the Dask medoid dispatch, so it went with the
# Dask backend -- but the tests that used it were never about Dask. They assert
# weight/seed alignment, artifact masking and trajectory reduction, which are
# properties of the surviving local path. Mocking the task function preserves
# every one of those assertions and exercises strictly MORE production code,
# because the task list, the dispatch and the gather now all run for real
# instead of being bypassed.
#
# Two behavioural notes, both matching the real worker:
#   * Records are served by (param_idx, stoch_idx). A task with no matching
#     record comes back success = FALSE, exactly as an engine failure would.
#   * When trajectory capture is on and a scratch dir is supplied, captured
#     channels are streamed to scratch and NOT carried on the returned record.
#     The old precomputed path did this spilling on the master; the real worker
#     does it worker-side. The mock follows the worker.
#
# Mocking only affects this process, so a test using it must run
# calc_model_ensemble(parallel = FALSE) (the default).
# =============================================================================

# Serve `records` (a list of engine result records, each with param_idx and
# stoch_idx) through the real ensemble task dispatch for the duration of `env`.
local_mocked_ensemble_sims <- function(records, env = parent.frame()) {
  tbl <- list()
  for (r in records) {
    tbl[[paste(r$param_idx, r$stoch_idx, sep = "_")]] <- r
  }

  fake_task <- function(task_info, param_configs_list,
                        capture_traj = FALSE,
                        traj_channels = character(0),
                        traj_scratch = NULL) {
    param_idx <- task_info$param_idx
    stoch_idx <- task_info$stoch_idx
    rec <- tbl[[paste(param_idx, stoch_idx, sep = "_")]]

    if (is.null(rec)) {
      return(list(param_idx = param_idx, stoch_idx = stoch_idx,
                  success = FALSE,
                  error = "no canned record for this task (mocked engine)"))
    }

    # Stream captured channels to scratch and strip them off the record, as the
    # real worker does. Without a scratch dir they stay attached for the
    # in-process reduce.
    if (isTRUE(capture_traj) && !is.null(rec$traj) && length(rec$traj) &&
        !is.null(traj_scratch) && nzchar(traj_scratch)) {
      saveRDS(list(traj = rec$traj, traj_epi = rec$traj_epi),
              file.path(traj_scratch, sprintf("sim_%d_%d.rds",
                                              as.integer(param_idx),
                                              as.integer(stoch_idx))))
      rec$traj <- NULL
      rec$traj_epi <- NULL
    }
    rec
  }

  testthat::local_mocked_bindings(
    .mosaic_ensemble_sim_task = fake_task,
    .package = "MOSAIC",
    .env = env
  )
}
