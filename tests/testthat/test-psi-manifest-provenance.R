# DA-02: the psi run manifest must record enough to reconstruct the fit, or to
# prove two artefacts are not comparable.
#
# It previously recorded 18 keys and none of: the source panel, the sequence/CV
# geometry, the smoothing/clamp constants, or any software version. With lstm_v2's
# known cross-process non-determinism that made a psi artefact unreconstructible
# in principle. This test pins the contract so it cannot silently regress.

test_that("the manifest provenance contract lists every field needed to reproduce a fit", {
     src <- readLines(testthat::test_path("..", "..", "R", "run_rolling_cv_suitability.R"),
                      warn = FALSE)
     skip_if(length(src) == 0L, "R/ source not available (installed check)")
     blk <- paste(src, collapse = "\n")
     required <- c(
          # what data went in
          "source_csv", "source_csv_md5",
          # the geometry that determines the fold grid and the sequences
          "timesteps", "lead", "rw_gap_weeks", "rw_subsample",
          "rw_step_days", "rw_test_days", "rw_min_test_days", "rw_min_train_years",
          "n_rw_steps",
          # constants that survive into psi itself
          "smooth_span", "ensemble_logit_eps",
          # software identity
          "mosaic_version", "tf_version", "host", "written_at")
     missing <- required[!vapply(required, function(k) grepl(k, blk, fixed = TRUE), logical(1))]
     expect_equal(missing, character(0),
                  info = paste("manifest provenance lost:", paste(missing, collapse = ", ")))
})

test_that("provenance is additive -- the pre-existing manifest keys are still written", {
     src <- paste(readLines(testthat::test_path("..", "..", "R",
                                                "run_rolling_cv_suitability.R"),
                            warn = FALSE), collapse = "\n")
     skip_if(!nzchar(src), "R/ source not available")
     legacy <- c("architecture", "fit_date_start", "fit_date_stop", "feature_set",
                 "response_var", "bias_correct", "region_map", "n_seeds", "seeds",
                 "n_countries", "n_features", "fit_info", "rw_diagnostics")
     gone <- legacy[!vapply(legacy, function(k) grepl(k, src, fixed = TRUE), logical(1))]
     expect_equal(gone, character(0),
                  info = paste("a legacy manifest key was dropped:", paste(gone, collapse = ", ")))
})
