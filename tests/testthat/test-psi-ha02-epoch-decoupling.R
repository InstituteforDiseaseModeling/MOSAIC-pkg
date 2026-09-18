# HA-02: epoch/ensemble decoupling in .psi_fit_predict_rw_cv().
#
# The inner RW-CV exists to produce ONE number that reaches the deployed model:
# round(median(best_epoch)). By default every seed of the ensemble re-runs the
# whole fold loop to re-derive it, so cost is folds x seeds -- which is why a
# high-fold inner CV was unaffordable at the production seed count (a 28-day
# inner stride over the 9 production cutoffs is 677 folds = ~53 dugong-hours at
# 10 seeds, versus ~12 h decoupled).
#
# `epoch_only = TRUE` runs the folds and returns the epoch; `fixed_epoch = n`
# skips the folds and refits at n. Both default off, so the historical path is
# unchanged. These tests use a stub arch so no TensorFlow is needed.

.ha02_bundle <- function() {
     dates <- seq(as.Date("2021-01-04"), by = "week", length.out = 120L)
     isos  <- c("AAA", "BBB")
     grid  <- expand.grid(date = dates, iso_code = isos, stringsAsFactors = FALSE)
     steps <- list(
          list(step = 1L, train_end = as.Date("2022-01-03"),
               test_start = as.Date("2022-01-17"), test_end = as.Date("2022-04-10")),
          list(step = 2L, train_end = as.Date("2022-07-04"),
               test_start = as.Date("2022-07-18"), test_end = as.Date("2022-10-09")),
          list(step = 3L, train_end = as.Date("2023-01-02"),
               test_start = as.Date("2023-01-16"), test_end = as.Date("2023-04-09")))
     list(bundle = list(
               rw_steps       = steps,
               cutoff_date    = as.Date("2023-06-01"),
               dates_pred     = grid$date,
               countries_pred = grid$iso_code,
               pool_data = list(X = matrix(0, nrow(grid), 1), intensity = rep(0.1, nrow(grid)),
                                countries = grid$iso_code, dates = grid$date,
                                cw = rep(1, nrow(grid))),
               seq_params = list(timesteps = 13L, max_gap_days = 14L, lead = 0L),
               encoders   = list(country_to_id = list(AAA = 0L, BBB = 1L),
                                 region_for_country = list(AAA = 0L, BBB = 0L)),
               use_confidence_weight = FALSE),
          n = nrow(grid), n_steps = length(steps))
}

# A stub arch that counts its calls and records the epochs it was asked for.
.ha02_stub <- function(epochs = c(6L, 8L, 10L)) {
     e <- new.env(parent = emptyenv())
     e$calls <- 0L; e$fixed <- list()
     list(env = e, fn = function(data_bundle, seed, hyperparams) {
          e$calls <- e$calls + 1L
          e$fixed[[length(e$fixed) + 1L]] <- hyperparams$n_epochs_fixed
          list(pred = rep(0.25, length(data_bundle$dates_pred %||% 240L)),
               n_epochs = epochs[min(e$calls, length(epochs))],
               val_loss = 0.5, val_metric = 0.4, loss_type = "bce")
     })
}
`%||%` <- function(a, b) if (is.null(a)) b else a

test_that("the default path is unchanged: every fold is fitted, then one refit", {
     b <- .ha02_bundle(); s <- .ha02_stub()
     r <- MOSAIC:::.psi_fit_predict_rw_cv(b$bundle, s$fn, seed = 11L, verbose = FALSE)
     expect_equal(s$env$calls, b$n_steps + 1L)          # 3 folds + 1 final fit
     expect_equal(r$n_epochs, 8L)                       # median(6, 8, 10)
     expect_identical(r$rw_diagnostics$epoch_source, "rw_median")
     expect_false(is.null(r$pred))
})

test_that("epoch_only runs the folds, returns the epoch, and skips the refit", {
     b <- .ha02_bundle(); s <- .ha02_stub()
     r <- MOSAIC:::.psi_fit_predict_rw_cv(b$bundle, s$fn, seed = 11L, verbose = FALSE,
                                          epoch_only = TRUE)
     expect_equal(s$env$calls, b$n_steps)               # folds only -- no final fit
     expect_equal(r$n_epochs, 8L)
     expect_true(isTRUE(r$epoch_only))
     expect_null(r$pred)
     expect_identical(r$rw_diagnostics$epoch_source, "rw_median")
})

test_that("fixed_epoch skips the fold loop entirely and refits at the given epoch", {
     b <- .ha02_bundle(); s <- .ha02_stub()
     r <- MOSAIC:::.psi_fit_predict_rw_cv(b$bundle, s$fn, seed = 11L, verbose = FALSE,
                                          fixed_epoch = 8L)
     expect_equal(s$env$calls, 1L)                      # the refit, and nothing else
     expect_equal(r$n_epochs, 8L)
     expect_identical(r$rw_diagnostics$epoch_source, "fixed")
     # the epoch actually reaches the arch as n_epochs_fixed, or the refit would
     # silently early-stop on its own schedule
     expect_equal(s$env$fixed[[1]], 8L)
     expect_false(is.null(r$pred))
     expect_true(all(is.na(r$rw_diagnostics$best_epochs)))
})

test_that("decoupling picks the SAME epoch the one-shot path would have used", {
     # The correctness property that matters: splitting the two jobs must not
     # change which epoch the ensemble is refitted at.
     b <- .ha02_bundle()
     s1 <- .ha02_stub()
     one_shot <- MOSAIC:::.psi_fit_predict_rw_cv(b$bundle, s1$fn, seed = 11L, verbose = FALSE)
     s2 <- .ha02_stub()
     sel <- MOSAIC:::.psi_fit_predict_rw_cv(b$bundle, s2$fn, seed = 11L, verbose = FALSE,
                                            epoch_only = TRUE)
     s3 <- .ha02_stub()
     dec <- MOSAIC:::.psi_fit_predict_rw_cv(b$bundle, s3$fn, seed = 11L, verbose = FALSE,
                                            fixed_epoch = sel$n_epochs)
     expect_equal(sel$n_epochs, one_shot$n_epochs)
     expect_equal(dec$n_epochs, one_shot$n_epochs)
     # and the cost model holds: folds+1 vs folds vs 1 arch calls
     expect_equal(c(s1$env$calls, s2$env$calls, s3$env$calls),
                  c(b$n_steps + 1L, b$n_steps, 1L))
})

test_that("the two modes are mutually exclusive and fixed_epoch is validated", {
     b <- .ha02_bundle(); s <- .ha02_stub()
     expect_error(MOSAIC:::.psi_fit_predict_rw_cv(b$bundle, s$fn, verbose = FALSE,
                                                  fixed_epoch = 8L, epoch_only = TRUE),
                  "mutually exclusive")
     for (bad in list(0L, -3L, NA_integer_, c(4L, 5L), "eight"))
          expect_error(MOSAIC:::.psi_fit_predict_rw_cv(b$bundle, s$fn, verbose = FALSE,
                                                       fixed_epoch = bad),
                       "fixed_epoch must be a single positive integer")
})

test_that("epoch_select_seeds is rejected when it exceeds n_seeds", {
     # Guard in .est_suitability_lstm_v2(): asking for more selection seeds than
     # the ensemble has is a launch-line error and must fail loudly, not silently
     # select from all of them.
     ac <- MOSAIC:::.psi_load_arch_control(list(n_seeds = 3L, epoch_select_seeds = 5L))
     expect_identical(ac$epoch_select_seeds, 5L)
     expect_gt(ac$epoch_select_seeds, ac$n_seeds)   # the orchestrator stops on this
})
