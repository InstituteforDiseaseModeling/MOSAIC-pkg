# Post-calibration resume (MOSAIC 1.5.0).
#
# A run killed after sampling finished -- in the CFR pilot, the ensembles, the
# trajectory reduction or the figures -- resumes from samples.parquet and the
# sampling marker (2_calibration/state/sampling_complete.rds) instead of redoing
# the sampling, and gives the posteriors, ensembles and medoid of an
# uninterrupted run. The kill is .mosaic_test_kill_point(), driven by the
# MOSAIC_TEST_KILL_AT environment variable: it signals a non-error condition,
# so none of the pipeline's tryCatch(error = ) blocks absorbs it.
#
# The runs are the stubbed single-location calibration of helper-stub-run.R
# (sequential, ~10 s each in fixed mode).

.kill_run <- function(where, ...) {
  tryCatch(withr::with_envvar(c(MOSAIC_TEST_KILL_AT = where), .stub_run_mosaic(...)),
           mosaic_test_kill = function(c) c)
}

.expect_same_run <- function(a_dir, b_dir) {
  a <- .stub_posterior_snapshot(a_dir)
  b <- .stub_posterior_snapshot(b_dir)
  for (k in names(a)) expect_identical(b[[k]], a[[k]], info = k)
  # The medoid ensemble and the per-draw tables beside them.
  for (f in c("medoid_ensemble.rds")) {
    ea <- readRDS(file.path(a_dir, "2_calibration", f))
    eb <- readRDS(file.path(b_dir, "2_calibration", f))
    for (k in c("cases_median", "cases_mean", "deaths_median", "deaths_mean", "ci_bounds", "seeds"))
      expect_identical(eb[[k]], ea[[k]], info = paste(f, k))
  }
  for (f in c("diagnostics/parameter_ess.csv", "posterior/posterior_quantiles.csv"))
    expect_identical(unname(tools::md5sum(file.path(b_dir, "2_calibration", f))),
                     unname(tools::md5sum(file.path(a_dir, "2_calibration", f))), info = f)
  expect_identical(
    as.data.frame(arrow::read_parquet(file.path(b_dir, "2_calibration", "diagnostics",
                                                "convergence_results.parquet"))),
    as.data.frame(arrow::read_parquet(file.path(a_dir, "2_calibration", "diagnostics",
                                                "convergence_results.parquet"))))
  expect_identical(
    as.data.frame(arrow::read_parquet(file.path(b_dir, "2_calibration", "samples.parquet"))),
    as.data.frame(arrow::read_parquet(file.path(a_dir, "2_calibration", "samples.parquet"))))
  sa <- jsonlite::read_json(file.path(a_dir, "3_results", "summary.json"))
  sb <- jsonlite::read_json(file.path(b_dir, "3_results", "summary.json"))
  # Everything but the run's own provenance (when, how long, resumed or not).
  same <- setdiff(names(sa), c("timestamp", "wall_time_seconds", "resumed", "n_simulations_reused"))
  expect_identical(sb[same], sa[same])
}

test_that("a run killed in the ensemble resumes at post-calibration and reproduces the uninterrupted run", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  local_test_root()
  a <- withr::local_tempdir()
  .stub_run_mosaic(a)

  b <- withr::local_tempdir()
  k <- .kill_run("ensemble", b)
  expect_s3_class(k, "mosaic_test_kill")
  # Killed after sampling finished: the draws are consolidated, the marker
  # records it, the shards are gone and the run is not complete. The candidate
  # ensemble was already written -- a stale artifact the resume must replace.
  expect_true(file.exists(file.path(b, "2_calibration", "samples.parquet")))
  expect_true(file.exists(file.path(b, "2_calibration", "state", "sampling_complete.rds")))
  expect_length(list.files(file.path(b, "2_calibration", "samples"), pattern = "^sim_"), 0L)
  expect_false(file.exists(file.path(b, "3_results", "summary.json")))
  expect_true(file.exists(file.path(b, "2_calibration", "ensemble_candidate.rds")))

  r <- attr(.stub_run_mosaic(b, resume = TRUE), "result")
  .expect_same_run(a, b)
  # The resumed session simulated nothing for the calibration and combined
  # nothing: it read the draws back.
  log <- readLines(file.path(b, "run.log"))
  from <- max(grep("[RESUME] Sampling finished in an earlier session", log, fixed = TRUE))
  resumed_log <- log[seq(from, length(log))]
  expect_false(any(grepl("Combining simulation files|Running [0-9]+ simulations|Fixed batch complete",
                         resumed_log)))
  expect_true(any(grepl("[RESUME] Reading the sampled draws", resumed_log, fixed = TRUE)))
  expect_true(r$summary$resumed)
  expect_identical(r$summary$sims_reused, 150L)
  expect_true(isTRUE(jsonlite::read_json(file.path(b, "3_results", "summary.json"))$resumed))
})

test_that("resuming a completed run is a no-op that leaves the directory untouched", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  local_test_root()
  a <- withr::local_tempdir()
  .stub_run_mosaic(a)
  files <- c(file.path(a, "3_results", "summary.json"),
             file.path(a, "1_inputs", c("environment.json", "control.json", "config.json")),
             file.path(a, "2_calibration", "samples.parquet"),
             file.path(a, "2_calibration", "ensemble_candidate.rds"))
  before <- tools::md5sum(files)
  s0 <- jsonlite::read_json(file.path(a, "3_results", "summary.json"))

  res <- testthat::with_mocked_bindings(
    attr(.stub_run_mosaic(a, resume = TRUE), "result"),
    calc_model_ensemble = function(...) stop("post-calibration was re-run"),
    .package = "MOSAIC")
  expect_identical(tools::md5sum(files), before)
  log <- readLines(file.path(a, "run.log"))
  expect_true(any(grepl("is a completed run (3_results/summary.json present): nothing to resume",
                        log, fixed = TRUE)))
  expect_false(any(grepl("RUN_FATAL", utils::tail(log, 3L))))

  # The return value describes the completed run.
  expect_identical(res$summary$sims_total, 150L)
  expect_identical(res$summary$convergence_status, s0$convergence_status)
  expect_true(res$summary$resumed)
  expect_identical(res$summary$runtime_min, 0)
  expect_named(res, c("dirs", "files", "summary"))
})

test_that("a run directory without a sampling marker keeps the pre-1.5.0 refusal", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  local_test_root()
  a <- withr::local_tempdir()
  .stub_run_mosaic(a)
  unlink(file.path(a, "2_calibration", "state", "sampling_complete.rds"))
  expect_error(.stub_run_mosaic(a, resume = TRUE),
               "holds a completed run \\(3_results/summary.json present\\): nothing to resume")
  # Killed in post-calibration with no marker (a run before 1.5.0): the shards
  # are gone, so it cannot resume.
  unlink(file.path(a, "3_results", "summary.json"))
  expect_error(.stub_run_mosaic(a, resume = TRUE), "already holds a consolidated posterior")
})

test_that("a run killed during sampling still resumes from its shards (auto mode)", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("MOSAIC_RUN_INTEGRATION")),
              "set MOSAIC_RUN_INTEGRATION=1 to run the auto-mode stub runs")
  skip_if_not_installed("arrow")
  local_test_root()
  a <- withr::local_tempdir()
  .stub_run_mosaic(a, mode = "auto")

  b <- withr::local_tempdir()
  expect_s3_class(.kill_run("batch_2", b, mode = "auto"), "mosaic_test_kill")
  # Mid-sampling: shards and the resume checkpoint, no consolidated draws and
  # no sampling marker.
  expect_gt(length(list.files(file.path(b, "2_calibration", "samples"), pattern = "^sim_")), 0L)
  expect_false(file.exists(file.path(b, "2_calibration", "samples.parquet")))
  expect_false(file.exists(file.path(b, "2_calibration", "state", "sampling_complete.rds")))
  expect_true(file.exists(file.path(b, "2_calibration", "state", "resume_checkpoint.rds")))
  .stub_run_mosaic(b, mode = "auto", resume = TRUE)
  .expect_same_run(a, b)

  # A fresh run (resume = FALSE, without clean_output) into a finished
  # directory removes the earlier marker before it samples, so a resume after
  # it is killed cannot skip its sampling: it meets the earlier samples.parquet
  # and refuses rather than shrinking it.
  expect_s3_class(.kill_run("batch_1", a, mode = "auto", resume = FALSE, clean_output = FALSE),
                  "mosaic_test_kill")
  expect_false(file.exists(file.path(a, "2_calibration", "state", "sampling_complete.rds")))
  expect_error(.stub_run_mosaic(a, mode = "auto", resume = TRUE), "already holds a consolidated posterior")
})

test_that("plots = TRUE writes parameter_sensitivity.csv and its figure in the same run", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("MOSAIC_RUN_INTEGRATION")),
              "set MOSAIC_RUN_INTEGRATION=1 to run the stub run with figures")
  skip_if_not_installed("arrow")
  skip_if_not_installed("sensitivity")
  local_test_root()
  a <- withr::local_tempdir()
  .stub_run_mosaic(a, plots = TRUE)
  diag <- file.path(a, "3_results", "figures", "diagnostics")
  expect_true(file.exists(file.path(diag, "parameter_sensitivity.csv")))
  expect_true(file.exists(file.path(diag, "parameter_sensitivity.png")))
})

test_that("plots = FALSE defers parameter_sensitivity.csv to render_MOSAIC_figures()", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  skip_if_not_installed("sensitivity")
  skip_if_not_installed("ggplot2")
  local_test_root()
  a <- withr::local_tempdir()
  # run_MOSAIC() no longer computes it: calc_model_parameter_sensitivity() is
  # not reached during the run.
  testthat::with_mocked_bindings(
    .stub_run_mosaic(a),
    calc_model_parameter_sensitivity = function(...) stop("HSIC ran inside run_MOSAIC()"),
    .package = "MOSAIC")
  csv <- file.path(a, "3_results", "figures", "diagnostics", "parameter_sensitivity.csv")
  expect_false(file.exists(csv))
  expect_true(file.exists(file.path(a, "3_results", "summary.json")))
  suppressWarnings(render_MOSAIC_figures(a, which = "sensitivity", verbose = FALSE))
  expect_true(file.exists(csv))
  expect_gt(nrow(utils::read.csv(csv)), 0L)
})
