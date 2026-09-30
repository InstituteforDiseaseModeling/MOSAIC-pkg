# Deep review suitability-03: partial seed failures shrank the ensemble with no
# warning; the parallel worker discarded the error text; the manifest reported
# the requested n_seeds/seeds.
# Deep review suitability-13: parallel seed workers load the INSTALLED MOSAIC,
# so under load_all() they silently ran different fitting code; and the inline
# fit closure captured the data bundle, shipping it to each worker twice.
# Deep review suitability-14: a serial fit ignored MOSAIC_PSI_CORE_BUDGET.
# Deep review suitability-16: the internal orchestrator's response_var default
# differed from the public est_suitability() default.

mk_seed_bundle <- function() {
     dts <- seq(as.Date("2021-01-07"), by = "week", length.out = 60)
     list(dates_pred = dts, countries_pred = rep("AAA", length(dts)),
          pred_date_stop = max(dts),
          obs_all = data.frame(date = dts, iso_code = "AAA", cases = 0, intensity = 0),
          target_iso = "AAA", X_pred = array(0, dim = c(length(dts), 3, 2)))
}

flaky_fit <- function(data_bundle, seed, hyperparams) {
     if (seed %in% c(22L, 44L)) stop(sprintf("TF OOM on seed %d", seed))
     n <- length(data_bundle$dates_pred)
     list(pred = rep(0.3 + 0.01 * (seed %% 7), n), val_loss = 0.1,
          val_metric = 0.1, n_epochs = 5L, loss_type = "bce")
}

test_that("a partial seed failure warns with the failed seeds and their errors", {
     b <- mk_seed_bundle()
     w <- NULL
     ens <- withCallingHandlers(
          suppressMessages(MOSAIC:::.psi_run_seed_ensemble(
               flaky_fit, b, seeds = c(11L, 22L, 33L, 44L, 55L), verbose = FALSE)),
          warning = function(cnd) { w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning") })
     expect_length(w, 1L)
     expect_match(w, "2 of 5 seed\\(s\\) failed")
     expect_match(w, "TF OOM on seed 22")
     expect_match(w, "TF OOM on seed 44")
     expect_equal(ens$seeds_ok, c(11L, 33L, 55L))
     expect_equal(ens$seeds_failed, c(22L, 44L))
     expect_true("error" %in% names(ens$fit_info))
     expect_equal(ens$fit_info$error[ens$fit_info$seed == 22L], "TF OOM on seed 22")
     expect_true(all(is.na(ens$fit_info$error[ens$fit_info$status == "ok"])))
})

test_that("no warning when every seed succeeds", {
     ok_fit <- function(data_bundle, seed, hyperparams)
          list(pred = rep(0.4, length(data_bundle$dates_pred)), n_epochs = 5L)
     expect_no_warning(ens <- MOSAIC:::.psi_run_seed_ensemble(
          ok_fit, mk_seed_bundle(), seeds = c(11L, 22L), verbose = FALSE))
     expect_length(ens$seeds_failed, 0L)
})

test_that("the manifest records the seeds actually pooled and the aggregation rule", {
     bad_fit <- function(data_bundle, seed, hyperparams) {
          if (seed != 22L) stop("simulated OOM")
          list(pred = rep(0.4, length(data_bundle$dates_pred)), n_epochs = 5L)
     }
     ens <- suppressWarnings(MOSAIC:::.psi_run_seed_ensemble(
          bad_fit, mk_seed_bundle(), seeds = c(11L, 22L, 33L), verbose = FALSE))
     f <- MOSAIC:::.psi_manifest_seed_fields(ens)
     expect_identical(f$n_seeds_ok, 1L)
     expect_identical(as.integer(f$seeds_ok), 22L)
     expect_identical(as.integer(f$seeds_failed), c(11L, 33L))
     expect_match(f$seed_aggregation, "logit")
     # seed vectors are JSON arrays whatever their length (auto_unbox = TRUE)
     js <- jsonlite::fromJSON(jsonlite::toJSON(f, auto_unbox = TRUE), simplifyVector = FALSE)
     expect_true(is.list(js$seeds_ok) && length(js$seeds_ok) == 1L)
     expect_length(js$seeds_failed, 2L)
     f0 <- MOSAIC:::.psi_manifest_seed_fields(list(seeds_ok = c(1L, 2L), seeds_failed = integer(0)))
     expect_match(as.character(jsonlite::toJSON(f0, auto_unbox = TRUE)), '"seeds_failed":\\[\\]')
})

test_that("parallel seed fitting refuses a load_all() namespace and falls back to serial", {
     skip_if_not(MOSAIC:::.psi_is_dev_namespace(), "only meaningful under devtools::load_all()")
     skip_if(isTRUE(parallel::detectCores() < 4L), "needs >= 4 cores to reach the dev check")
     withr::local_envvar(MOSAIC_PSI_CORE_BUDGET = "8")
     expect_warning(
          res <- MOSAIC:::.psi_fit_seeds_parallel(
               seeds = c(11L, 22L), parallel_seeds = 2L, fit_predict_fn = flaky_fit,
               data_bundle = mk_seed_bundle(), hyperparams = list(), verbose = FALSE),
          "load_all")
     expect_null(res)
})

test_that("the per-seed fit closure does not capture the data bundle", {
     fn <- MOSAIC:::.psi_make_rw_cv_fit_fn(verbose = FALSE)
     expect_setequal(ls(environment(fn), all.names = TRUE), "verbose")
     expect_identical(names(formals(fn)), c("data_bundle", "seed", "hyperparams"))
})

test_that("TF intra-op threads fall back to MOSAIC_PSI_CORE_BUDGET for serial fits", {
     withr::local_envvar(MOSAIC_PSI_TF_INTRAOP = NA, MOSAIC_PSI_CORE_BUDGET = NA)
     expect_true(is.na(MOSAIC:::.psi_tf_intraop_threads()))
     withr::local_envvar(MOSAIC_PSI_CORE_BUDGET = "12")
     expect_equal(MOSAIC:::.psi_tf_intraop_threads(), 12L)
     withr::local_envvar(MOSAIC_PSI_TF_INTRAOP = "3")
     expect_equal(MOSAIC:::.psi_tf_intraop_threads(), 3L)       # worker pin wins
     withr::local_envvar(MOSAIC_PSI_TF_INTRAOP = "junk")
     expect_equal(MOSAIC:::.psi_tf_intraop_threads(), 12L)
})

test_that("internal orchestrator response_var default matches est_suitability()", {
     expect_identical(formals(MOSAIC:::.est_suitability_lstm_v2)$response_var,
                      formals(MOSAIC::est_suitability)$response_var)
})
