# Tests for impute_flood_probability()
#
# Strategy: build a synthetic country-week panel where flood probability
# is a deterministic-plus-noise function of a single climate predictor.
# Verify the GAM recovers that signal, the contract holds (no NAs, in
# [0,1], correct shape), and the function fails informatively when the
# inputs are wrong. Keep the diagnostics block disabled for speed except
# in one focused diagnostics test.

.mk_synth_suitability <- function(seed = 7L,
                                   n_iso = 5L, n_years = 4L,
                                   na_forecast_year = TRUE) {
     set.seed(seed)
     isos  <- LETTERS[seq_len(n_iso)]
     years <- 2018:(2018 + n_years - 1L)
     weeks <- 1:52
     d <- expand.grid(iso_code = isos, year = years, week = weeks,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
     d$date <- as.Date(paste0(d$year, "-01-01")) + (d$week - 1L) * 7L
     d <- d[order(d$iso_code, d$date), ]
     row.names(d) <- NULL

     # Generate every covariate the GAM now requires. precip_anom remains
     # the true driver so the synthetic-recovery test still asserts on it.
     d$precip_anom               <- stats::rnorm(nrow(d))
     d$precipitation_sum         <- abs(stats::rnorm(nrow(d), mean = 50, sd = 20))
     d$precip_sum_2w             <- abs(stats::rnorm(nrow(d), mean = 100, sd = 40))
     d$precip_sum_4w             <- abs(stats::rnorm(nrow(d), mean = 200, sd = 80))
     d$precip_sum_8w             <- abs(stats::rnorm(nrow(d), mean = 400, sd = 150))
     d$precip_sum_12w            <- abs(stats::rnorm(nrow(d), mean = 600, sd = 200))
     d$precip_extreme_p90_count  <- stats::rbinom(nrow(d), 1, 0.1)
     d$soil_moisture_0_to_10cm_mean <- stats::rnorm(nrow(d), mean = 0.3, sd = 0.05)
     d$soil_moisture_anom        <- stats::rnorm(nrow(d))
     d$spei_approx               <- stats::rnorm(nrow(d))
     d$relative_humidity_2m_mean <- stats::rnorm(nrow(d), mean = 60, sd = 15)
     d$rh_mean_12w               <- stats::rnorm(nrow(d), mean = 60, sd = 10)
     d$wind_speed_10m_max        <- abs(stats::rnorm(nrow(d), mean = 5, sd = 2))
     d$ENSO3                     <- stats::rnorm(nrow(d))
     d$ENSO34                    <- stats::rnorm(nrow(d))
     d$ENSO4                     <- stats::rnorm(nrow(d))
     d$IOD                       <- stats::rnorm(nrow(d))

     # WHO subregion factor required by the region-conditional smooth.
     # Round-robin across 4 regions deterministically by ISO.
     who_regions <- c("Central Africa", "East Africa", "Southern Africa", "West Africa")
     d$region <- who_regions[(match(d$iso_code, isos) - 1L) %% 4L + 1L]

     # True P(flood) = logistic(2 * precip_anom). Other covariates are
     # uncorrelated noise so the GAM should still find precip_anom as the
     # dominant predictor.
     prob <- stats::plogis(2 * d$precip_anom)
     d$emdat_flood_active <- stats::rbinom(nrow(d), 1, prob)

     # Severity target for the Tweedie GAM (v0.30.22+): log1p of
     # synthetic Total Affected. Severity scales deterministically with
     # precip_anom so the Tweedie GAM has a strong signal to recover on
     # tiny synthetic data (5 ISOs x 4 years x 52 weeks).
     severity_raw <- ifelse(
          d$emdat_flood_active == 1,
          pmax(0, 200 * exp(d$precip_anom)) + stats::rexp(nrow(d), rate = 0.05),
          0
     )
     d$emdat_flood_affected <- log1p(severity_raw)

     # Optionally hide the final year (forecast window) -> NA in target
     if (na_forecast_year) {
          forecast <- d$year == max(years)
          d$emdat_flood_active[forecast]    <- NA_integer_
          d$emdat_flood_affected[forecast]  <- NA_real_
     }
     d
}


testthat::test_that("recovers a known precip-driven flood signal", {
     d <- .mk_synth_suitability()
     out <- MOSAIC::impute_flood_probability(d, diagnostics = FALSE, verbose = FALSE)

     # Output has the new column, no NAs, in [0, 1], same nrow
     testthat::expect_equal(nrow(out), nrow(d))
     testthat::expect_true("emdat_flood_prob" %in% names(out))
     testthat::expect_false(any(is.na(out$emdat_flood_prob)))
     testthat::expect_true(all(out$emdat_flood_prob >= 0))
     testthat::expect_true(all(out$emdat_flood_prob <= 1))

     # Training-period AUC against the observed binary should be well
     # above chance. Binomial-on-binary GAM separates classes cleanly.
     train <- !is.na(d$emdat_flood_active)
     p <- out$emdat_flood_prob[train]
     y <- d$emdat_flood_active[train]
     r <- rank(p)
     n_pos <- sum(y == 1); n_neg <- sum(y == 0)
     auc <- (sum(r[y == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
     testthat::expect_gt(auc, 0.80)

     # Mean predicted prob at high precip_anom should be substantially
     # higher than at low precip_anom (binomial logit produces a clear
     # separation in mean predicted probability across the predictor
     # range).
     hi <- out$emdat_flood_prob[d$precip_anom > 1.5]
     lo <- out$emdat_flood_prob[d$precip_anom < -1.5]
     testthat::expect_gt(mean(hi), mean(lo) + 0.3)
})


testthat::test_that("populates forecast-window rows where input is NA", {
     d <- .mk_synth_suitability(na_forecast_year = TRUE)
     out <- MOSAIC::impute_flood_probability(d, diagnostics = FALSE, verbose = FALSE)

     forecast <- d$year == max(d$year)
     testthat::expect_true(any(forecast))
     testthat::expect_true(all(is.na(d$emdat_flood_active[forecast])))
     # Yet every forecast row now has a non-NA probability
     testthat::expect_false(any(is.na(out$emdat_flood_prob[forecast])))
     # Distribution shape similar to historical (broad sanity check, not strict)
     hist_p <- out$emdat_flood_prob[!forecast]
     fcst_p <- out$emdat_flood_prob[forecast]
     testthat::expect_lt(abs(mean(fcst_p) - mean(hist_p)), 0.2)
})


testthat::test_that("errors when a required predictor is missing", {
     d <- .mk_synth_suitability()
     d$precip_anom <- NULL
     testthat::expect_error(
          MOSAIC::impute_flood_probability(d, diagnostics = FALSE, verbose = FALSE),
          "missing required column"
     )
})


testthat::test_that("is deterministic on the same input (no hidden randomness)", {
     d <- .mk_synth_suitability(seed = 11L)
     a <- MOSAIC::impute_flood_probability(d, diagnostics = FALSE, verbose = FALSE)
     b <- MOSAIC::impute_flood_probability(d, diagnostics = FALSE, verbose = FALSE)
     testthat::expect_equal(a$emdat_flood_prob, b$emdat_flood_prob)
})


testthat::test_that("diagnostics writes the four expected artefacts", {
     d <- .mk_synth_suitability(seed = 13L)
     tmp <- withr::local_tempdir()
     # The diagnostics path includes a rolling-year CV; with year_val >= 2018
     # and our synthetic years 2018:2021, only 2019/2020 are eligible folds.
     out <- MOSAIC::impute_flood_probability(
          d, diagnostics = TRUE, diag_dir = tmp, verbose = FALSE
     )
     testthat::expect_true(file.exists(file.path(tmp, "flood_gam_smooths.png")))
     testthat::expect_true(file.exists(file.path(tmp, "flood_gam_calibration.png")))
     testthat::expect_true(file.exists(file.path(tmp, "flood_gam_summary.txt")))
     # CV metrics CSV may exist if at least one fold ran; if so, validate cols
     cv_csv <- file.path(tmp, "flood_gam_cv_metrics.csv")
     if (file.exists(cv_csv)) {
          cv <- utils::read.csv(cv_csv)
          testthat::expect_true(all(c("year_val", "auc", "brier", "log_loss") %in%
                                    names(cv)))
     }
})


testthat::test_that("rolling-window features land alongside the probability when called via compile path", {
     # Direct lightweight check of the rolling-window contract we wire into
     # compile_suitability_data() right after impute_flood_probability(). We
     # don't exercise compile_suitability_data end-to-end here (that needs the
     # real climate / WHO pipeline). Instead, replicate the exact mutate block
     # we added at line ~631 of compile_suitability_data.R and assert shapes.
     d   <- .mk_synth_suitability(seed = 17L)
     out <- MOSAIC::impute_flood_probability(d, diagnostics = FALSE, verbose = FALSE)
     out <- out %>%
          dplyr::group_by(iso_code) %>%
          dplyr::arrange(date, .by_group = TRUE) %>%
          dplyr::mutate(
               emdat_flood_prob_4w_max  = slider::slide_dbl(emdat_flood_prob, max,
                                                             .before = 3,  .complete = TRUE),
               emdat_flood_prob_12w_max = slider::slide_dbl(emdat_flood_prob, max,
                                                             .before = 11, .complete = TRUE),
               emdat_flood_prob_12w_sum = slider::slide_dbl(emdat_flood_prob, sum,
                                                             .before = 11, .complete = TRUE),
               emdat_flood_prob_anom    = emdat_flood_prob - mean(emdat_flood_prob, na.rm = TRUE)
          ) %>%
          dplyr::ungroup()
     # Leading-edge NAs match window width - 1, per country
     by_iso <- split(out, out$iso_code)
     for (g in by_iso) {
          testthat::expect_equal(sum(is.na(g$emdat_flood_prob_4w_max)),  3)
          testthat::expect_equal(sum(is.na(g$emdat_flood_prob_12w_max)), 11)
          testthat::expect_equal(sum(is.na(g$emdat_flood_prob_12w_sum)), 11)
     }
     # Max-window >= point estimate everywhere it's defined
     ok <- !is.na(out$emdat_flood_prob_12w_max)
     testthat::expect_true(all(out$emdat_flood_prob_12w_max[ok] >=
                                out$emdat_flood_prob[ok] - 1e-12))
})
