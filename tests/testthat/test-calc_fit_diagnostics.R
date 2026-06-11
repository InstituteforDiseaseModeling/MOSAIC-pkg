test_that("perfect prediction passes bias/shape and reports bias = 1, R2 = 1", {
  dts <- seq(as.Date("2020-01-01"), as.Date("2021-12-31"), by = "day")
  doy <- as.integer(format(dts, "%j"))
  obs <- 40 * exp(-((doy - 80)^2) / 1500) + 25 * exp(-((doy - 290)^2) / 1200) + 1

  d <- calc_fit_diagnostics(obs, obs, dts)

  expect_equal(d$bias$total, 1, tolerance = 1e-8)
  expect_equal(d$r2$corr, 1, tolerance = 1e-8)
  expect_equal(d$shape$shape_corr, 1, tolerance = 1e-8)
  expect_equal(unname(d$scorecard["bias"]), "PASS")
  expect_equal(unname(d$scorecard["peak_timing"]), "PASS")
  expect_equal(unname(d$scorecard["peak_shape"]), "PASS")
})

test_that("pure scale bias is detected and decoupled from shape", {
  dts <- seq(as.Date("2020-01-01"), as.Date("2021-12-31"), by = "day")
  doy <- as.integer(format(dts, "%j"))
  obs <- 40 * exp(-((doy - 80)^2) / 1500) + 25 * exp(-((doy - 290)^2) / 1200) + 1
  pred <- 2.5 * obs

  d <- calc_fit_diagnostics(obs, pred, dts)

  expect_equal(d$bias$total, 2.5, tolerance = 1e-6)
  expect_equal(unname(d$scorecard["bias"]), "FAIL")          # 2.5x > 2.0 FAIL threshold
  # shape is identical after area-normalisation: peak_shape must NOT re-flag the bias
  expect_equal(d$shape$shape_corr, 1, tolerance = 1e-8)
  expect_equal(unname(d$scorecard["peak_shape"]), "PASS")
  expect_equal(unname(d$scorecard["peak_timing"]), "PASS")
})

test_that("a later predicted peak yields a positive timing error in the WARN band", {
  dts <- seq(as.Date("2020-01-01"), as.Date("2021-12-31"), by = "day")
  doy <- as.integer(format(dts, "%j"))
  obs  <- 50 * exp(-((doy - 80)^2) / 1200) + 1
  pred <- 50 * exp(-((doy - 95)^2) / 1200) + 1   # peak 15 days later, same magnitude

  d <- calc_fit_diagnostics(obs, pred, dts)

  pt <- d$shape$peak_timing_error_by_year
  expect_true(all(pt > 0, na.rm = TRUE))                      # model peaks later
  expect_true(mean(abs(pt), na.rm = TRUE) >= 7 && mean(abs(pt), na.rm = TRUE) < 21)
  expect_equal(unname(d$scorecard["peak_timing"]), "WARN")
})

test_that("bias_by_year and r2_by_year are named per calendar year", {
  dts <- seq(as.Date("2019-01-01"), as.Date("2021-12-31"), by = "day")
  doy <- as.integer(format(dts, "%j"))
  obs <- 30 * exp(-((doy - 150)^2) / 2000) + 1
  d <- calc_fit_diagnostics(obs, 1.3 * obs, dts)
  expect_setequal(names(d$bias$by_year), c("2019", "2020", "2021"))
  expect_setequal(names(d$r2$by_year), c("2019", "2020", "2021"))
  expect_true(all(abs(d$bias$by_year - 1.3) < 1e-6))
})

test_that("cv_ratio reflects relative dispersion and short input errors", {
  dts <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")
  obs <- rep(c(10, 30), length.out = length(dts))            # higher dispersion
  pred <- rep(20, length(dts))                               # flat -> CV 0
  d <- calc_fit_diagnostics(obs, pred, dts)
  expect_equal(d$variance$cv_ratio, 0, tolerance = 1e-8)     # pred CV 0 / obs CV > 0

  expect_error(calc_fit_diagnostics(1, 1, as.Date("2020-01-01")),
               "at least two")
})

test_that("all-NA / sparse series does not error (seasonal cor is guarded)", {
  dts <- seq(as.Date("2020-01-01"), as.Date("2021-12-31"), by = "day")
  d <- expect_no_error(calc_fit_diagnostics(rep(NA_real_, length(dts)),
                                            rep(NA_real_, length(dts)), dts))
  expect_true(is.na(d$shape$seasonal_corr))
  expect_equal(unname(d$scorecard["bias"]), "NA")
})

test_that("anti-correlated shape grades peak_shape FAIL (monotone, not NA)", {
  dts <- seq(as.Date("2020-01-01"), by = "day", length.out = 120)
  obs  <- as.numeric(seq_along(dts))            # rising
  pred <- rev(obs)                              # falling -> shape_corr ~ -1
  d <- calc_fit_diagnostics(obs, pred, dts)
  expect_lt(d$shape$shape_corr, 0)
  expect_equal(unname(d$scorecard["peak_shape"]), "FAIL")
})
