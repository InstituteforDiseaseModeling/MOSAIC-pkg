# Deep review suitability-04: the per-country daily grid ran to pred_date_stop,
# so the carry-forward fill past a country's last genuine weekly prediction was
# LOESS-smoothed together with the genuine days (and fed the bias-correction
# amplitude reference) before the writer dropped it.
#
# Deep review suitability-06: prediction sequences reused the training builder,
# which needs a covariate row AT the target date, so a lead-h model could never
# forecast past covariate coverage -- the point of the lead knob.

mk_ens_bundle <- function(pred_stop_extra_days = 98L) {
     isos <- c("AAA", "BBB")
     dts_a <- seq(as.Date("2015-01-01"), by = "week", length.out = 520L)
     dts_b <- dts_a[seq_len(500L)]                   # BBB coverage ends 20 weeks earlier
     countries_pred <- c(rep("AAA", length(dts_a)), rep("BBB", length(dts_b)))
     dates_pred     <- c(dts_a, dts_b)
     list(dates_pred = dates_pred, countries_pred = countries_pred,
          pred_date_stop = max(dts_a) + pred_stop_extra_days,
          obs_all = data.frame(date = dates_pred, iso_code = countries_pred,
                               cases = 0, intensity = 0, stringsAsFactors = FALSE),
          target_iso = "AAA",
          X_pred = array(0, dim = c(length(dates_pred), 3, 2)))
}

wavy_fit <- function(data_bundle, seed, hyperparams) {
     t <- as.numeric(data_bundle$dates_pred)
     list(pred = stats::plogis(sin(t / 30) + 0.01 * (seed %% 5)),
          val_loss = 0.1, val_metric = 0.1, n_epochs = 5L, loss_type = "bce")
}

test_that("each country's daily ensemble ends at its last genuine prediction date", {
     b <- mk_ens_bundle()
     ens <- MOSAIC:::.psi_run_seed_ensemble(wavy_fit, b, seeds = c(11L, 22L),
                                            verbose = FALSE)
     el <- ens$ensemble_long
     last <- tapply(el$date, el$iso, max)
     expect_equal(as.Date(last[["AAA"]], origin = "1970-01-01"),
                  max(b$dates_pred[b$countries_pred == "AAA"]))
     expect_equal(as.Date(last[["BBB"]], origin = "1970-01-01"),
                  max(b$dates_pred[b$countries_pred == "BBB"]))
     # the writer's tail drop is now a no-op
     out <- MOSAIC:::.drop_filled_prediction_tail(
          transform(el, iso_code = iso), ens$genuine_last_pred)
     expect_equal(nrow(out), nrow(el))
})

test_that("the retained genuine days are smoothed without the carry-forward tail", {
     b <- mk_ens_bundle()
     ens <- MOSAIC:::.psi_run_seed_ensemble(wavy_fit, b, seeds = 11L, verbose = FALSE)
     bbb <- ens$by_country$BBB
     idx <- b$countries_pred == "BBB"
     ref <- MOSAIC:::.psi_weekly_to_daily_smooth(
          b$dates_pred[idx], pmax(0.01, pmin(0.99, wavy_fit(b, 11L)$pred[idx])),
          day_start = min(b$dates_pred[idx]), day_stop = max(b$dates_pred[idx]))
     m <- merge(bbb[, c("date", "pred_smooth")], ref[, c("date", "pred_smooth")],
                by = "date", suffixes = c("_ens", "_ref"))
     expect_equal(nrow(m), nrow(bbb))
     expect_equal(m$pred_smooth_ens, m$pred_smooth_ref, tolerance = 1e-6)
})

test_that("genuine_last_pred is capped at pred_date_stop", {
     b <- mk_ens_bundle(pred_stop_extra_days = -70L)
     ens <- MOSAIC:::.psi_run_seed_ensemble(wavy_fit, b, seeds = 11L, verbose = FALSE)
     expect_true(all(ens$genuine_last_pred$last_genuine_date <= b$pred_date_stop))
     expect_true(all(ens$ensemble_long$date <= b$pred_date_stop))
})

mk_seq <- function(n_weeks = 60L) {
     d <- seq(as.Date("2020-01-02"), by = "week", length.out = n_weeks)
     data.frame(iso_code = "AAA", date = d, y = seq_along(d) / 100,
                stringsAsFactors = FALSE)
}

test_that("predict_mode at lead 0 reproduces the training mapping", {
     p <- mk_seq(); X <- matrix(p$y, ncol = 1)
     a <- MOSAIC:::.psi_build_sequences(X, p$y, p$iso_code, p$date, timesteps = 13L)
     b <- MOSAIC:::.psi_build_sequences(X, p$y, p$iso_code, p$date, timesteps = 13L,
                                        predict_mode = TRUE)
     expect_identical(a$dates, b$dates)
     expect_identical(a$X, b$X)
     expect_true(all(is.na(b$y)))
})

test_that("predict_mode with a lead forecasts lead weeks past the last covariate week", {
     p <- mk_seq(); X <- matrix(p$y, ncol = 1)
     s <- MOSAIC:::.psi_build_sequences(X, p$y, p$iso_code, p$date, timesteps = 13L,
                                        lead = 12L, predict_mode = TRUE)
     expect_equal(max(s$dates), max(p$date) + 84L)
     expect_equal(length(s$dates), nrow(p) - 12L)      # one per complete window
     # the last window's inputs end at the last covariate week
     expect_equal(s$X[length(s$dates), 13L, 1L], p$y[nrow(p)])
     # training mapping is unchanged: it still needs the target row
     tr <- MOSAIC:::.psi_build_sequences(X, p$y, p$iso_code, p$date, timesteps = 13L,
                                         lead = 12L)
     expect_equal(max(tr$dates), max(p$date))
})

test_that(".psi_build_data prediction sequences extend by the lead", {
     set.seed(9)
     dts <- seq(as.Date("2015-01-01"), by = "week", length.out = 300L)
     n <- length(dts)
     p <- do.call(rbind, lapply(c("MOZ", "MWI"), function(iso)
          data.frame(iso_code = iso, date = dts, cases = stats::rpois(n, 10),
                     region = "SOUTH",
                     f1 = stats::rnorm(n), f2 = stats::rnorm(n), f3 = stats::rnorm(n),
                     f4 = stats::rnorm(n), f5 = stats::rnorm(n),
                     stringsAsFactors = FALSE)))
     csv <- withr::local_tempfile(fileext = ".csv")
     utils::write.csv(p, csv, row.names = FALSE)
     b <- suppressWarnings(MOSAIC:::.psi_build_data(
          source_csv = csv, cutoff_date = dts[250], fit_date_start = min(dts),
          pred_date_stop = max(dts) + 28L, country_pool = "regional",
          features = c("f1", "f2", "f3", "f4", "f5"), lead = 4L,
          split_params = list(rw_step_months = 6L, rw_test_months = 5L),
          verbose = FALSE))
     expect_equal(max(b$dates_pred), max(dts) + 28L)
})
