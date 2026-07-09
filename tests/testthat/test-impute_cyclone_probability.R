# Tests for impute_cyclone_probability()
#
# Synthetic country-week panel where cyclone activity is a deterministic-
# plus-noise function of the wind proxy. Verify the wind/coastal GAM recovers
# that signal, the contract holds (no NAs, in [0,1], expected columns), and the
# function fails informatively when a required predictor is missing.

# 2 ISOs x 4 years x 52 weeks = 416 rows -> ~210 training rows after the 24-week
# lag warm-up. Wind is the true driver so the recovery test asserts on it.
.mk_synth_cyclone <- function(seed = 7L, n_iso = 2L, n_years = 4L,
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

     d$wind_speed_10m_max <- abs(stats::rnorm(nrow(d), mean = 5, sd = 2))
     d$precipitation_sum  <- abs(stats::rnorm(nrow(d), mean = 50, sd = 20))
     d$precip_sum_2w      <- abs(stats::rnorm(nrow(d), mean = 100, sd = 40))
     d$precip_sum_4w      <- abs(stats::rnorm(nrow(d), mean = 200, sd = 80))
     d$ENSO34             <- stats::rnorm(nrow(d))
     d$IOD                <- stats::rnorm(nrow(d))

     who_regions <- c("Central Africa", "East Africa", "Southern Africa", "West Africa")
     d$region <- who_regions[(match(d$iso_code, isos) - 1L) %% 4L + 1L]

     # True P(cyclone) = logistic(-3 + 1.2 * (wind - 5)): rare, wind-driven.
     prob <- stats::plogis(-3 + 1.2 * (d$wind_speed_10m_max - 5))
     d$emdat_cyclone_active <- stats::rbinom(nrow(d), 1, prob)

     if (na_forecast_year) {
          forecast <- d$year == max(years)
          d$emdat_cyclone_active[forecast] <- NA_integer_
     }
     d
}


testthat::test_that("recovers a known wind-driven cyclone signal", {
     d <- .mk_synth_cyclone()
     out <- MOSAIC::impute_cyclone_probability(d, diagnostics = FALSE, verbose = FALSE)

     testthat::expect_equal(nrow(out), nrow(d))
     testthat::expect_true("emdat_cyclone_prob" %in% names(out))
     testthat::expect_false(any(is.na(out$emdat_cyclone_prob)))
     testthat::expect_true(all(out$emdat_cyclone_prob >= 0))
     testthat::expect_true(all(out$emdat_cyclone_prob <= 1))

     # Mean predicted prob at high wind >> at low wind.
     hi <- out$emdat_cyclone_prob[d$wind_speed_10m_max > 7]
     lo <- out$emdat_cyclone_prob[d$wind_speed_10m_max < 3]
     testthat::expect_gt(mean(hi), mean(lo))
})


testthat::test_that("populates forecast-window rows where input is NA", {
     d <- .mk_synth_cyclone(na_forecast_year = TRUE)
     out <- MOSAIC::impute_cyclone_probability(d, diagnostics = FALSE, verbose = FALSE)
     forecast <- d$year == max(d$year)
     testthat::expect_true(all(is.na(d$emdat_cyclone_active[forecast])))
     testthat::expect_false(any(is.na(out$emdat_cyclone_prob[forecast])))
})


testthat::test_that("errors when a required predictor is missing", {
     d <- .mk_synth_cyclone()
     d$wind_speed_10m_max <- NULL
     testthat::expect_error(
          MOSAIC::impute_cyclone_probability(d, diagnostics = FALSE, verbose = FALSE),
          "missing required column"
     )
})
