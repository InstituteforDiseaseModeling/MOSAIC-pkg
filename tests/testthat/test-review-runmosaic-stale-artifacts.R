# =============================================================================
# test-review-runmosaic-stale-artifacts.R
#
# A default (non-resume, clean_output = FALSE) re-run into an existing
# dir_output must not reuse the previous run's files:
#   * ensemble_optimized.rds was only written when absent, so run 1's posterior
#     survived run 2 and was what render_MOSAIC_figures()/MOSAIC-OCV read;
#   * sim_*.parquet shards from an interrupted run were pooled into run 2.
# =============================================================================

.stale_dirs <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  MOSAIC:::.mosaic_ensure_dir_tree(root, clean_output = FALSE)
}

test_that("post-calibration artifacts from an earlier run are removed before rebuild", {
  dirs <- .stale_dirs()
  files <- c(file.path(dirs$calibration, c("ensemble_candidate.rds", "ensemble_optimized.rds",
                                           "subset_opt.rds", "medoid_ensemble.rds",
                                           "trajectories_ensemble.rds", "pi_ij_ensemble.rds")),
             file.path(dirs$cal_best_model, "config_medoid.json"))
  keep <- file.path(dirs$calibration, c("samples.parquet", "deaths_integration.rds"))
  for (f in c(files, keep)) writeLines("old", f)
  removed <- MOSAIC:::.mosaic_clear_posterior_artifacts(dirs)
  expect_setequal(removed, files)
  expect_false(any(file.exists(files)))
  expect_true(all(file.exists(keep)))
})

test_that("per-location prediction CSVs and 3_results tables from an earlier run are removed", {
  dirs <- .stale_dirs()
  for (d in c(dirs$res_predictions, dirs$res_fig_diag, dirs$res_posterior))
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  # Run 1 had three locations; run 2 has two, so AGO's CSVs would otherwise be
  # pooled into run 2's predictions_*_all.csv by the combine glob.
  preds <- file.path(dirs$res_predictions,
                     c(sprintf("predictions_ensemble_%s.csv", c("MOZ", "AGO", "ETH")),
                       sprintf("predictions_medoid_%s.csv", c("MOZ", "AGO", "ETH")),
                       "predictions_ensemble_all.csv", "predictions_medoid_all.csv",
                       sprintf("trajectories_%s.csv", c("MOZ", "AGO", "ETH"))))
  tables <- c(file.path(dirs$res_fig_diag, c("model_fit_windows.csv",
                                             "optimization_diagnostics.csv",
                                             "parameter_sensitivity.csv")),
              file.path(dirs$res_posterior, c("cfr_posterior.csv",
                                              "reproductive_numbers.csv",
                                              "reproductive_numbers.rds")))
  keep <- c(file.path(dirs$res_posterior, "parameter_estimates.csv"),
            file.path(dirs$res_predictions, "notes.csv"))
  for (f in c(preds, tables, keep)) writeLines("old", f)
  removed <- MOSAIC:::.mosaic_clear_posterior_artifacts(dirs)
  expect_setequal(removed, c(preds, tables))
  expect_true(all(file.exists(keep)))
  expect_length(list.files(dirs$res_predictions, pattern = "^(predictions|trajectories)_"), 0L)
})

test_that("run_MOSAIC's ensemble_optimized.rds fallback is not keyed on file.exists()", {
  body_txt <- paste(deparse(body(MOSAIC::run_MOSAIC)), collapse = "\n")
  expect_false(grepl("!file.exists(ensemble_opt_path)", body_txt, fixed = TRUE))
  expect_true(grepl(".mosaic_clear_posterior_artifacts(", body_txt, fixed = TRUE))
  expect_true(grepl(".mosaic_quarantine_stale_shards(", body_txt, fixed = TRUE))
})

test_that("a fresh run moves shards from an earlier run aside instead of pooling them", {
  dirs <- .stale_dirs()
  shards <- file.path(dirs$cal_samples, c("sim_0000001.parquet", "sim_0000002-0000100.parquet"))
  for (f in shards) writeLines("old", f)
  writeLines("other", file.path(dirs$cal_samples, "notes.txt"))
  msgs <- character()
  qdir <- MOSAIC:::.mosaic_quarantine_stale_shards(
    dirs, function(msg, ...) msgs <<- c(msgs, sprintf(msg, ...)))
  expect_length(list.files(dirs$cal_samples, pattern = "^sim_.*\\.parquet$"), 0L)
  expect_true(file.exists(file.path(dirs$cal_samples, "notes.txt")))
  expect_setequal(list.files(qdir), basename(shards))
  expect_identical(dirname(qdir), dirs$calibration)
  expect_length(msgs, 1L)
  expect_match(msgs, "moved 2 shard")
  # A second quarantine in the same second gets its own directory.
  writeLines("old", shards[1])
  qdir2 <- MOSAIC:::.mosaic_quarantine_stale_shards(dirs)
  expect_false(identical(qdir, qdir2))
  expect_identical(list.files(qdir2), basename(shards[1]))
})

test_that("an empty samples directory is left alone", {
  dirs <- .stale_dirs()
  expect_null(MOSAIC:::.mosaic_quarantine_stale_shards(dirs))
  expect_length(list.files(dirs$calibration, pattern = "^samples_stale_"), 0L)
})
