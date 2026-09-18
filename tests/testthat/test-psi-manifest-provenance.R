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

test_that("the suitability writers reference no undefined variables (catches cross-branch drift)", {
     # WHY THIS EXISTS. The provenance test above greps the SOURCE for field names.
     # It passed while the writer was broken: the block referenced `backend`, a
     # variable that exists only on the feature/psi-torch-port branch, and every
     # shard of an arm died at the END of its first cutoff -- after all the fitting
     # work -- when the manifest was written. A static field-name check cannot see
     # that; an unbound-global check can, and it guards the whole class.
     skip_if_not_installed("codetools")
     fns <- c(".est_suitability_lstm_v2", ".psi_fit_predict_rw_cv",
              ".psi_slice_rw_step", ".psi_slice_full_is", ".psi_make_rw_cv_steps",
              ".drop_filled_prediction_tail")
     known <- c(
          # operators / base-ish things findGlobals reports but which are bound
          "%||%", "%in%", "%%", ":", "c", "list", "length", "seq_along", "seq.int",
          # package-internal helpers these functions legitimately call
          ".psi_build_sequences", ".psi_build_data", ".psi_load_arch_control",
          ".psi_resolve_features", ".psi_resolve_region_map", ".psi_run_seed_ensemble",
          ".psi_fit_predict_lstm", ".psi_make_rw_cv_steps", ".psi_slice_rw_step",
          ".psi_slice_full_is", ".drop_filled_prediction_tail",
          ".psi_check_parallel_seeds_ram", ".psi_weekly_to_daily_smooth",
          "calibrate_psi_predictions", "check_psi_amplitude", "get_feature_set")
     offenders <- list()
     for (fn in fns) {
          f <- tryCatch(get(fn, envir = asNamespace("MOSAIC")), error = function(e) NULL)
          if (is.null(f)) next
          g <- codetools::findGlobals(f, merge = FALSE)$variables
          # anything not exported/bound in base, the namespace, or the allowlist
          bad <- g[!vapply(g, function(v)
               exists(v, envir = asNamespace("MOSAIC")) ||
               exists(v, envir = baseenv()) ||
               v %in% known, logical(1))]
          if (length(bad)) offenders[[fn]] <- bad
     }
     expect_equal(offenders, list(),
                  info = paste("undefined variable(s):",
                               paste(names(offenders), vapply(offenders, paste,
                                                              character(1), collapse = ","),
                                     collapse = "; ")))
})
