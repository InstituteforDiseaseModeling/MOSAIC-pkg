# =============================================================================
# test-review-runmosaic-worker-errors.R
#
#   * An R-level error in a parallel calibration task came back as a
#     'snow-try-error' string and crashed the tally sum(unlist(...)); it is now
#     recorded as FALSE with a warning.
#   * A failed shard write in the per-sim worker now returns FALSE instead of
#     raising.
#   * The gather idle timeout scales with the engine runs per task.
#   * The worker installer no longer serialises the run_MOSAIC frame.
# =============================================================================

.skip_if_no_psock_we <- function() {
  skip_if_testthat_parallel()
  ok <- tryCatch({
    cl <- parallel::makeCluster(1L, type = "PSOCK"); parallel::stopCluster(cl); TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) testthat::skip("PSOCK cluster unavailable in this environment")
}

test_that("an R-level error in a parallel task is counted as a failed sim", {
  .skip_if_no_psock_we()
  cl <- parallel::makeCluster(2L, type = "PSOCK")
  .wpids <- cluster_worker_pids(cl)
  on.exit(stop_cluster_hard(cl, .wpids), add = TRUE)
  parallel::clusterCall(cl, function() {
    assign(".run_sim_worker", function(sim_id) if (sim_id == 2L) stop("disk full") else TRUE,
           envir = .GlobalEnv)
    NULL
  })
  expect_warning(
    out <- MOSAIC:::.mosaic_run_batch(1:3, function(sim_id) .run_sim_worker(sim_id),
                                      cl = cl, show_progress = FALSE),
    "disk full")
  expect_identical(unlist(out), c(TRUE, FALSE, TRUE))
  expect_identical(sum(unlist(out)), 2L)
})

test_that("the calibration idle timeout scales with the engine runs per task", {
  withr::local_options(MOSAIC.ensemble_worker_timeout_sec = NULL,
                       MOSAIC.calibration_sec_per_engine_run = NULL)
  expect_identical(MOSAIC:::.mosaic_calibration_idle_timeout(3L), 1800)
  expect_identical(MOSAIC:::.mosaic_calibration_idle_timeout(100L * 10L), 30000)
  withr::local_options(MOSAIC.calibration_sec_per_engine_run = 2)
  expect_identical(MOSAIC:::.mosaic_calibration_idle_timeout(100L * 10L), 2000)
  expect_identical(MOSAIC:::.mosaic_calibration_idle_timeout(NA), 1800)
})

test_that("a failed shard write makes the per-sim worker return FALSE, not error", {
  cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = "MOZ")
  for (f in c("reported_cases_weight", "reported_deaths_weight")) cfg[[f]] <- NULL
  oc <- cfg$reported_cases; oc[!is.finite(oc)] <- 0
  od <- cfg$reported_deaths; od[!is.finite(od)] <- 0
  cfg$reported_cases <- oc; cfg$reported_deaths <- od
  ls <- list(weight_cases = 1, weight_deaths = 1, eps_rel_cases = 0.02, eps_rel_deaths = 0.25,
             .nb_k_cases_resolved = 0.5, .nb_k_deaths_resolved = 1, weights_location = 1,
             weight_peak_timing = 0, weight_peak_magnitude = 0, weight_cumulative_total = 0,
             weight_wis = 0, sigma_peak_time = 1, sigma_peak_log = 0.5)
  local_mocked_bindings(
    sample_parameters = function(...) cfg,
    run_simulation = function(config, seed = NULL, quiet = TRUE, ...)
      list(results = list(reported_cases = oc + 1, reported_deaths = od + 1)),
    .mosaic_write_parquet = function(...) stop("disk full"),
    .package = "MOSAIC")
  expect_warning(
    ok <- MOSAIC:::.mosaic_run_simulation_worker(
      sim_id = 1L, n_iterations = 1L, priors = NULL, config = cfg, PATHS = NULL,
      dir_cal_samples = withr::local_tempdir(), param_names_all = "gamma_1",
      param_lookup = MOSAIC:::.mosaic_build_param_lookup("gamma_1", cfg$location_name),
      sampling_args = list(), io = mosaic_control_defaults()$io,
      likelihood_settings = ls, write_shard = TRUE),
    "shard write failed for sim 1")
  expect_identical(ok, FALSE)
})

test_that("the worker installer does not carry the calling frame", {
  big <- runif(2e6)   # would be serialised with a closure created in this frame
  f <- MOSAIC:::.mosaic_worker_installer()
  expect_identical(environment(f), globalenv())
  # The run frame held ~10 MB per worker at 40 locations; `big` is 16 MB.
  expect_lt(length(serialize(f, NULL)), 2e6)
  g <- function() NULL   # a closure made in this frame does carry it
  expect_gt(length(serialize(g, NULL)), 1.5e7)
})

test_that("the installed workers resolve the clusterExport'ed globals", {
  .skip_if_no_psock_we()
  cl <- parallel::makeCluster(1L, type = "PSOCK")
  .wpids <- cluster_worker_pids(cl)
  on.exit(stop_cluster_hard(cl, .wpids), add = TRUE)
  parallel::clusterCall(cl, MOSAIC:::.mosaic_worker_installer())
  got <- parallel::clusterCall(cl, function() {
    c(exists(".run_sim_worker", envir = .GlobalEnv),
      exists(".run_sim_worker_chunk", envir = .GlobalEnv),
      identical(parent.env(environment(.run_sim_worker)), globalenv()))
  })[[1]]
  expect_identical(got, c(TRUE, TRUE, TRUE))
})
