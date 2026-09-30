# DA-02: the psi run manifest must record enough to reconstruct the fit, or to
# prove two artefacts are not comparable.
#
# It previously recorded 18 keys and none of: the source panel, the sequence/CV
# geometry, the smoothing/clamp constants, or any software version. With lstm_v2's
# known cross-process non-determinism that made a psi artefact unreconstructible
# in principle. This test pins the contract so it cannot silently regress.

# Top-level keys are read from the `config_info <- c(list(...), <seed fields>,
# list(...))` constructor that feeds write_json(), not grepped from the whole
# file: most of these names also occur in unrelated code (the data-build call,
# the arch-control merge), so a file-wide grep kept passing after a key was
# dropped from the manifest. The provenance block is built by
# .psi_manifest_provenance(), which is called directly and round-tripped
# through JSON below.
.manifest_top_keys <- function() {
     fn <- get(".est_suitability_lstm_v2", envir = asNamespace("MOSAIC"))
     found <- NULL
     walk <- function(e) {
          if (!is.null(found) || !is.call(e)) return(invisible())
          if (identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("config_info")) &&
              is.call(e[[3]])) {
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
     # list(...) directly, or c() over list(...) pieces and helper calls
     pieces <- if (identical(found[[1]], as.name("c"))) as.list(found)[-1] else list(found)
     keys <- character(0); calls <- character(0)
     for (p in pieces) {
          if (!is.call(p)) next
          if (identical(p[[1]], as.name("list"))) keys <- c(keys, setdiff(names(p), ""))
          else calls <- c(calls, deparse(p[[1]]))
     }
     list(top = keys, calls = calls)
}

.manifest_provenance_json <- function(ac_override = list()) {
     csv <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
     utils::write.csv(data.frame(iso_code = "MOZ", cases = 1), csv, row.names = FALSE)
     ac <- utils::modifyList(list(
          country_pool = "all_mosaic", timesteps = 11L, lead = 0L, max_gap_days = 14,
          rw_step_months = 3, rw_test_months = 3, rw_subsample = 1, rw_gap_weeks = 4,
          step_days = 84, test_days = 91, min_test_days = 28, min_train_years = 3,
          smooth_span = 0.1, loess_surface = "interpolate", loess_degree = 1L,
          ensemble_logit_eps = 0.01, loss_kind = "bce", use_confidence_weight = FALSE),
          ac_override)
     arch_hp <- list(arch_kind = "hierarchical", hier_mode = "film", units_1 = 64L,
                     dropout = 0.2, rec_dropout = 0.1, l2 = 1e-4, lr = 1e-3,
                     epochs = 40L, patience = 6L, partial_pool_lambda = 0.5,
                     sample_weights = "balanced_uniform", balance_R = 1,
                     country_balance = TRUE, restore_best_weights = TRUE)
     prov <- MOSAIC:::.psi_manifest_provenance(source_csv = csv, ac = ac,
                                              arch_hp = arch_hp, n_rw_steps = 7L)
     out <- withr::local_tempfile(fileext = ".json", .local_envir = parent.frame())
     jsonlite::write_json(list(provenance = prov), out, pretty = TRUE,
                          auto_unbox = TRUE, digits = NA, null = "null")
     jsonlite::read_json(out)$provenance
}

test_that("the manifest provenance block writes every field needed to reproduce a fit", {
     prov <- .manifest_provenance_json()
     required <- c(
          # what data went in, and which countries were pooled
          "source_csv", "source_csv_md5", "country_pool",
          # the geometry that determines the fold grid and the sequences
          "timesteps", "lead", "rw_gap_weeks", "rw_subsample",
          "rw_step_days", "rw_test_days", "rw_min_test_days", "rw_min_train_years",
          "n_rw_steps",
          # constants that survive into psi itself
          "smooth_span", "loess_surface", "loess_degree", "ensemble_logit_eps",
          # the architecture/loss hyperparameters the seeds were trained with
          "arch_hp",
          # software identity
          "mosaic_version", "tf_version", "host", "written_at")
     expect_equal(setdiff(required, names(prov)), character(0),
                  info = "manifest provenance lost a key")
     # values, not just names: a research override must be distinguishable
     expect_equal(prov$loess_surface, "interpolate")
     expect_equal(prov$loess_degree, 1L)
     expect_equal(prov$n_rw_steps, 7L)
     expect_equal(prov$country_pool, "all_mosaic")
     expect_equal(prov$arch_hp$epochs, 40L)
     expect_equal(prov$arch_hp$partial_pool_lambda, 0.5)
     expect_true(prov$arch_hp$country_balance)
     expect_equal(nchar(prov$source_csv_md5), 32L)
})

test_that("two runs differing only in a psi-changing override get different manifests", {
     base <- .manifest_provenance_json()
     alt  <- .manifest_provenance_json(list(loess_degree = 2L, country_pool = "target_only"))
     expect_false(identical(base$loess_degree, alt$loess_degree))
     expect_false(identical(base$country_pool, alt$country_pool))
     # defaults mirror the .psi_run_seed_ensemble() call when the fixture omits them
     dflt <- .manifest_provenance_json(list(loess_surface = NULL, loess_degree = NULL,
                                            country_pool = NULL))
     expect_equal(dflt$loess_surface, "direct")
     expect_equal(dflt$loess_degree, 2L)
     expect_equal(dflt$country_pool, "all_mosaic")
})

test_that("the lstm_v2 writer uses the provenance and seed-field helpers", {
     keys <- .manifest_top_keys()
     expect_false(is.null(keys), info = "config_info <- ... not found in .est_suitability_lstm_v2")
     expect_true(".psi_manifest_seed_fields" %in% keys$calls)
     src <- paste(deparse(get(".est_suitability_lstm_v2", envir = asNamespace("MOSAIC"))),
                  collapse = "\n")
     expect_true(grepl(".psi_manifest_provenance(", src, fixed = TRUE))
     # the successful-seed count comes from the seed helper
     f <- MOSAIC:::.psi_manifest_seed_fields(list(seeds_ok = c(1L, 3L), seeds_failed = 2L))
     expect_equal(f$n_seeds_ok, 2L)
})

test_that("provenance is additive -- the pre-existing manifest keys are still written", {
     keys <- .manifest_top_keys()
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
          ".psi_manifest_provenance", ".psi_manifest_seed_fields",
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
