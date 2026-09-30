# DA-02: the psi run manifest must record enough to reconstruct the fit, or to
# prove two artefacts are not comparable.
#
# It previously recorded 18 keys and none of: the source panel, the sequence/CV
# geometry, the smoothing/clamp constants, or any software version. With lstm_v2's
# known cross-process non-determinism that made a psi artefact unreconstructible
# in principle. This test pins the contract so it cannot silently regress.

# The keys are read from the `config_info <- list(...)` constructor that feeds
# write_json(), not grepped from the whole file: most of these names also occur
# in unrelated code (the data-build call, the arch-control merge), so a
# file-wide grep kept passing after a key was dropped from the manifest.
.manifest_keys <- function() {
     fn <- get(".est_suitability_lstm_v2", envir = asNamespace("MOSAIC"))
     found <- NULL
     walk <- function(e) {
          if (!is.null(found) || !is.call(e)) return(invisible())
          if (identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("config_info")) &&
              is.call(e[[3]]) && identical(e[[3]][[1]], as.name("list"))) {
               found <<- e[[3]]
               return(invisible())
          }
          el <- as.list(e)
          for (i in seq_along(el)) {
               if (identical(el[[i]], quote(expr = ))) next
               walk(el[[i]])
          }
     }
     walk(body(fn))
     if (is.null(found)) return(NULL)
     top <- setdiff(names(found), "")
     prov <- found[["provenance"]]
     list(top = top,
          provenance = if (is.call(prov)) setdiff(names(prov), "") else character(0))
}

test_that("the manifest provenance block writes every field needed to reproduce a fit", {
     keys <- .manifest_keys()
     expect_false(is.null(keys), info = "config_info <- list(...) not found in .est_suitability_lstm_v2")
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
     expect_equal(setdiff(required, keys$provenance), character(0),
                  info = "manifest provenance lost a key")
})

test_that("provenance is additive -- the pre-existing manifest keys are still written", {
     keys <- .manifest_keys()
     legacy <- c("architecture", "fit_date_start", "fit_date_stop", "feature_set",
                 "response_var", "bias_correct", "region_map", "n_seeds", "seeds",
                 "n_countries", "n_features", "fit_info", "rw_diagnostics", "provenance")
     expect_equal(setdiff(legacy, keys$top), character(0),
                  info = "a legacy manifest key was dropped")
})

test_that("the suitability writers reference no undefined variables (catches cross-branch drift)", {
     # WHY THIS EXISTS. The provenance tests above read field names statically.
     # A source-grep version of them passed while the writer was broken: the block referenced `backend`, a
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
