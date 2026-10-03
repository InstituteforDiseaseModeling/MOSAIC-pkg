# calibrate_psi_predictions(): v0.34 logit-scale, outbreak-weeks-only bias
# correction. (Rewritten from the v0.33 raw-scale lm(obs~pred) tests — the
# method legitimately changed per plan_v034_RECOMMENDED §9.4.)

test_that("logit-scale affine is recovered from outbreak weeks in the train window only", {
  dates    <- seq(as.Date("2024-01-01"), by = "week", length.out = 40)
  fit_stop <- dates[30]
  a <- 1.5; b <- -0.5
  pred <- seq(0.05, 0.95, length.out = 40)
  obs  <- stats::plogis(a * stats::qlogis(pred) + b)   # logit(obs) = a*logit(pred)+b
  pred_df <- data.frame(iso_code = "A", date = dates, pred_smooth = pred)
  obs_df  <- data.frame(iso_code = "A", date = dates, transmission_intensity = obs)
  # Corrupt OOS observations: if they leaked into the fit the map would break.
  obs_df$transmission_intensity[obs_df$date > fit_stop] <- 0.99

  out <- calibrate_psi_predictions(pred_df, obs_df, fit_stop)

  expect_true("pred_bias_corrected" %in% names(out))
  # bounded in (0,1) with no hard clip (monotone inverse-logit)
  expect_true(all(out$pred_bias_corrected > 0 & out$pred_bias_corrected < 1))
  # a training point is mapped back onto its (uncorrupted) observed value
  expect_equal(out$pred_bias_corrected[5], obs[5], tolerance = 1e-6)
  # the recovered map equals plogis(a*logit(pred)+b) everywhere (incl. OOS rows)
  expect_equal(out$pred_bias_corrected,
               stats::plogis(a * stats::qlogis(pred) + b), tolerance = 1e-5)
})

test_that("zero observed weeks are excluded from the fit (only outbreak weeks count)", {
  dates    <- seq(as.Date("2024-01-01"), by = "week", length.out = 40)
  fit_stop <- dates[40]
  a <- 1.2; b <- 0.3
  pred <- seq(0.05, 0.95, length.out = 40)
  obs  <- stats::plogis(a * stats::qlogis(pred) + b)
  # Zero out half the observed weeks: they carry no level info and must be
  # dropped from the fit; the recovered affine must be unchanged.
  obs[seq(2, 40, by = 2)] <- 0
  pred_df <- data.frame(iso_code = "A", date = dates, pred_smooth = pred)
  obs_df  <- data.frame(iso_code = "A", date = dates, transmission_intensity = obs)

  out <- calibrate_psi_predictions(pred_df, obs_df, fit_stop)
  # Recovered from the surviving outbreak weeks; equals the true map everywhere.
  expect_equal(out$pred_bias_corrected,
               stats::plogis(a * stats::qlogis(pred) + b), tolerance = 1e-5)
})

test_that("sparse ISOs (< min_train outbreak weeks) fall back to identity and warn", {
  dates   <- seq(as.Date("2024-01-01"), by = "week", length.out = 10)
  pred_df <- data.frame(iso_code = "C", date = dates, pred_smooth = seq(0.05, 0.9, length.out = 10))
  # only 3 outbreak weeks -> below min_train (8)
  obs_df  <- data.frame(iso_code = "C", date = dates[1:3],
                        transmission_intensity = c(0.2, 0.3, 0.4))
  expect_warning(out <- calibrate_psi_predictions(pred_df, obs_df, max(dates)), "identity")
  expect_equal(out$pred_bias_corrected, pred_df$pred_smooth)   # identity = uncorrected
})

test_that("zero-history ISOs (no outbreak weeks at all) fall back to identity", {
  dates   <- seq(as.Date("2024-01-01"), by = "week", length.out = 30)
  pred_df <- data.frame(iso_code = "Z", date = dates, pred_smooth = seq(0.05, 0.5, length.out = 30))
  obs_df  <- data.frame(iso_code = "Z", date = dates,
                        transmission_intensity = rep(0, 30))   # never any cases
  expect_warning(out <- calibrate_psi_predictions(pred_df, obs_df, max(dates)), "identity")
  expect_equal(out$pred_bias_corrected, pred_df$pred_smooth)
})

test_that("output is in (0,1) even when the affine projects predictions far out", {
  dates    <- seq(as.Date("2024-01-01"), by = "week", length.out = 20)
  a <- 4.0; b <- 6.0   # steep map that would exceed 1 on a raw scale
  pred <- seq(0.1, 0.5, length.out = 20)
  obs  <- stats::plogis(a * stats::qlogis(pred) + b)
  pred_df <- data.frame(iso_code = "D", date = dates, pred_smooth = pred)
  obs_df  <- data.frame(iso_code = "D", date = dates, transmission_intensity = obs)
  # The steep a=4/b=6 map trips the offset and amplitude guards (expected); we
  # only assert the (0,1) bound here.
  out <- suppressWarnings(calibrate_psi_predictions(pred_df, obs_df, max(dates)))
  expect_true(all(out$pred_bias_corrected > 0 & out$pred_bias_corrected < 1))
})

# ---- B1: robustness of the per-country bias correction --------------------

test_that("near-flat (low-variance) predictor falls back to identity (no ZAF-style blow-up)", {
  # ZAF failure mode: outbreak-week predictor has almost no logit variance, so
  # an unguarded lm extrapolates a wild slope and blows the amplitude up.
  set.seed(1)
  n     <- 60
  dates <- seq(as.Date("2020-01-01"), by = "week", length.out = n)
  # Predictions span a real range, but the OUTBREAK weeks (obs > 0) are all
  # clustered at a near-constant low prediction -> degenerate fit input.
  pred  <- runif(n, 0.001, 0.9)
  obs   <- rep(0, n)
  ob_idx <- 1:30
  pred[ob_idx] <- 0.01 + rnorm(30, 0, 1e-4)   # near-flat predictor on outbreak wks
  obs[ob_idx]  <- runif(30, 0.05, 0.7)        # but wildly varying observed level

  pred_df <- data.frame(iso_code = "ZAF", date = dates, pred_smooth = pred)
  obs_df  <- data.frame(iso_code = "ZAF", date = dates, transmission_intensity = obs)

  out <- suppressWarnings(
    calibrate_psi_predictions(pred_df, obs_df, max(dates), min_pred_sd = 0.05))

  diag <- attr(out, "calibration_diagnostics")
  expect_identical(diag$status[diag$iso_code == "ZAF"], "identity")
  # identity -> output equals input exactly, so amplitude is preserved (not blown up)
  expect_equal(out$pred_bias_corrected, pred_df$pred_smooth)
})

test_that("a fit that would collapse the amplitude falls back to identity (v0.102.0)", {
  # GMB/SEN/SWZ failure mode: a fit with enough variance to run, but outbreak
  # weeks implying a near-zero slope. Up to v0.101.0 the map was blended toward
  # identity until it sat on the 0.5x amplitude floor; the slope is not
  # identified, so the fit is now not applied at all.
  set.seed(2)
  n     <- 80
  dates <- seq(as.Date("2018-01-01"), by = "week", length.out = n)
  pred  <- plogis(seq(-3, 1, length.out = n))         # real spread
  obs   <- rep(0, n)
  ob_idx <- 1:40
  obs[ob_idx] <- plogis(0.01 * qlogis(pred[ob_idx]) - 3 + rnorm(40, 0, 0.01))

  pred_df <- data.frame(iso_code = "GMB", date = dates, pred_smooth = pred)
  obs_df  <- data.frame(iso_code = "GMB", date = dates, transmission_intensity = obs)

  expect_warning(
    out <- calibrate_psi_predictions(pred_df, obs_df, max(dates),
                                     slope_range = c(0.25, 4), amp_range = c(0.5, 2)),
    "\\(GMB\\).*identity")

  row <- attr(out, "calibration_diagnostics")
  row <- row[row$iso_code == "GMB", ]
  expect_identical(row$status, "collapsed")
  expect_equal(row$slope, 0.25)          # the rejected fit, clamped to the floor
  expect_equal(row$amp_ratio, 1)
  expect_identical(out$pred_bias_corrected, pred_df$pred_smooth)
})

test_that("CIV-style collapse (negative outbreak-week slope at the clamp) keeps the model's seasonal contrast", {
  # C3 CIV: on the outbreak weeks the prediction sits near the 0.01 ensemble
  # clamp and the target is unrelated to it, so the OLS slope is negative and
  # its intercept far below the offset floor. v0.101.0 turned that into the
  # guard-constant map logit(psi) = 0.505 * logit(pred) - 2.64, which takes
  # the square root of the seasonal odds contrast.
  set.seed(11)
  n     <- 156
  dates <- seq(as.Date("2021-01-07"), by = "week", length.out = n)
  pred  <- plogis(-3.2 + 1.6 * sin(2 * pi * seq_len(n) / 52))
  obs   <- rep(0, n)
  ob_idx <- seq(10, n, by = 5)
  pred[ob_idx] <- runif(length(ob_idx), 0.0100, 0.0125)
  obs[ob_idx]  <- plogis(-2 * qlogis(pred[ob_idx]) - 12 + rnorm(length(ob_idx), 0, 0.3))

  pred_df <- data.frame(iso_code = "CIV", date = dates, pred_smooth = pred)
  obs_df  <- data.frame(iso_code = "CIV", date = dates, transmission_intensity = obs)
  out <- suppressWarnings(calibrate_psi_predictions(pred_df, obs_df, max(dates)))

  row <- attr(out, "calibration_diagnostics")
  expect_identical(row$status, "collapsed")
  expect_equal(c(row$slope, row$offset), c(0.25, -4))   # both guards had bound
  expect_identical(out$pred_bias_corrected, pred)
  expect_equal(max(out$pred_bias_corrected) / min(out$pred_bias_corrected),
               max(pred) / min(pred))
})

test_that("negative: an identified slope between the floor and 1 is still applied", {
  # A real flattening (true slope 0.7, inside [amp_range[1], 1)) must not be
  # mistaken for a collapse.
  dates    <- seq(as.Date("2019-01-03"), by = "week", length.out = 90)
  a <- 0.7; b <- -0.4
  pred <- plogis(seq(-4, 1, length.out = 90))
  obs  <- plogis(a * qlogis(pred) + b)
  pred_df <- data.frame(iso_code = "MOZ", date = dates, pred_smooth = pred)
  obs_df  <- data.frame(iso_code = "MOZ", date = dates, transmission_intensity = obs)

  expect_no_warning(out <- calibrate_psi_predictions(pred_df, obs_df, max(dates)))
  row <- attr(out, "calibration_diagnostics")
  expect_identical(row$status, "fit")
  expect_equal(row$amp_ratio, a, tolerance = 1e-6)
  expect_equal(out$pred_bias_corrected, plogis(a * qlogis(pred) + b), tolerance = 1e-6)
})

test_that("negative: an inflating fit is still shrunk to the ceiling, not reset to identity", {
  dates <- seq(as.Date("2019-01-03"), by = "week", length.out = 90)
  pred  <- plogis(seq(-3, 1, length.out = 90))
  obs   <- plogis(3 * qlogis(pred) + 1)          # amplitude 3x > amp_range[2]
  pred_df <- data.frame(iso_code = "ZAF", date = dates, pred_smooth = pred)
  obs_df  <- data.frame(iso_code = "ZAF", date = dates, transmission_intensity = obs)

  w <- character(0)
  out <- withCallingHandlers(calibrate_psi_predictions(pred_df, obs_df, max(dates)),
                             warning = function(cnd) {
                               w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning")
                             })
  row <- attr(out, "calibration_diagnostics")
  expect_identical(row$status, "guarded")
  expect_equal(row$amp_ratio, 2, tolerance = 0.03)
  expect_false(any(grepl("outbreak-week slope", w)))
  expect_false(identical(out$pred_bias_corrected, pred_df$pred_smooth))
})

test_that("negative: a collapsing country does not change its neighbours' correction", {
  set.seed(5)
  dates <- seq(as.Date("2019-01-03"), by = "week", length.out = 80)
  pred  <- plogis(seq(-3, 1, length.out = 80))
  good  <- data.frame(iso_code = "KEN", date = dates, pred_smooth = pred,
                      transmission_intensity = plogis(1.4 * qlogis(pred) + 0.3))
  bad_obs <- rep(0, 80)
  bad_obs[1:40] <- plogis(0.01 * qlogis(pred[1:40]) - 3 + rnorm(40, 0, 0.01))
  bad <- data.frame(iso_code = "UGA", date = dates, pred_smooth = pred,
                    transmission_intensity = bad_obs)
  both <- rbind(good, bad)

  alone <- calibrate_psi_predictions(good[, 1:3], good[, c(1, 2, 4)], max(dates))
  expect_warning(
    mixed <- calibrate_psi_predictions(both[, 1:3], both[, c(1, 2, 4)], max(dates)),
    "1 country/countries \\(UGA\\)")
  expect_identical(mixed$pred_bias_corrected[mixed$iso_code == "KEN"],
                   alone$pred_bias_corrected)
  expect_identical(mixed$pred_bias_corrected[mixed$iso_code == "UGA"],
                   bad$pred_smooth)
  d <- attr(mixed, "calibration_diagnostics")
  expect_identical(d$status[match(c("KEN", "UGA"), d$iso_code)], c("fit", "collapsed"))
})

test_that("well-behaved country with a moderate true affine is left essentially unchanged", {
  # COD/SOM/MOZ-style: ample outbreak weeks, real variance, modest slope/offset
  # inside the guard ranges -> guard is a no-op, recovered map matches exactly.
  dates    <- seq(as.Date("2017-01-01"), by = "week", length.out = 120)
  fit_stop <- dates[100]
  a <- 1.3; b <- 0.4   # inside slope_range/offset_range; amplitude *1.3 (<2x)
  pred <- plogis(seq(-3, 2, length.out = 120))
  obs  <- plogis(a * qlogis(pred) + b)
  pred_df <- data.frame(iso_code = "COD", date = dates, pred_smooth = pred)
  obs_df  <- data.frame(iso_code = "COD", date = dates, transmission_intensity = obs)

  out  <- calibrate_psi_predictions(pred_df, obs_df, fit_stop)
  diag <- attr(out, "calibration_diagnostics")
  expect_identical(diag$status[diag$iso_code == "COD"], "fit")  # not guarded
  expect_equal(out$pred_bias_corrected, plogis(a * qlogis(pred) + b),
               tolerance = 1e-5)
})

# ---- B5: amplitude guard ---------------------------------------------------

test_that("check_psi_amplitude flags a collapsed (near-constant) country", {
  df <- data.frame(
    iso_code   = rep("GMB", 50),
    psi        = rep(0.0057, 50),                      # collapsed to a constant
    pred_smooth = plogis(seq(-3, 2, length.out = 50))) # real input amplitude
  expect_warning(d <- check_psi_amplitude(df), "collapsed")
  expect_identical(d$flag[d$iso_code == "GMB"], "collapsed")
  expect_true(d$collapsed[d$iso_code == "GMB"])
})

test_that("check_psi_amplitude flags an inflated country and stays silent on a good one", {
  good <- plogis(seq(-3, 2, length.out = 60))
  df <- rbind(
    data.frame(iso_code = "ZAF", psi = plogis(5 * qlogis(good)),  # blown-up amplitude
               pred_smooth = good),
    data.frame(iso_code = "MOZ", psi = plogis(1.1 * qlogis(good)),# ~unchanged
               pred_smooth = good))
  expect_warning(d <- check_psi_amplitude(df), "ZAF")
  expect_identical(d$flag[d$iso_code == "ZAF"], "inflated")
  expect_identical(d$flag[d$iso_code == "MOZ"], "ok")
})

test_that("calibrate_psi_predictions attaches an amplitude diagnostic", {
  dates <- seq(as.Date("2019-01-01"), by = "week", length.out = 60)
  a <- 1.2; b <- 0.2
  pred <- plogis(seq(-3, 2, length.out = 60))
  obs  <- plogis(a * qlogis(pred) + b)
  pred_df <- data.frame(iso_code = "SOM", date = dates, pred_smooth = pred)
  obs_df  <- data.frame(iso_code = "SOM", date = dates, transmission_intensity = obs)
  out <- calibrate_psi_predictions(pred_df, obs_df, max(dates))
  amp <- attr(out, "amplitude_diagnostics")
  expect_true(is.data.frame(amp))
  expect_true(all(c("iso_code", "flag", "amp_ratio") %in% names(amp)))
})
