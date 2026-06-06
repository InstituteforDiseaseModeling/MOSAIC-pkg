test_that("calibrate_psi_predictions fits per-ISO affine on the training window only and clips [0,1]", {
  dates    <- seq(as.Date("2024-01-01"), by = "week", length.out = 40)
  fit_stop <- dates[30]
  predA <- seq(0, 1, length.out = 40); obsA <- 0.5 * predA + 0.1
  predB <- rev(seq(0, 1, length.out = 40)); obsB <- 0.8 * predB + 0.05
  pred_df <- rbind(data.frame(iso_code = "A", date = dates, pred_smooth = predA),
                   data.frame(iso_code = "B", date = dates, pred_smooth = predB))
  obs_df  <- rbind(data.frame(iso_code = "A", date = dates, transmission_intensity = obsA),
                   data.frame(iso_code = "B", date = dates, transmission_intensity = obsB))
  # Corrupt OOS observations: if they leaked into the fit, the recovered map would break.
  obs_df$transmission_intensity[obs_df$date > fit_stop] <- 99

  out <- calibrate_psi_predictions(pred_df, obs_df, fit_stop)

  expect_true("pred_calibrated" %in% names(out))
  expect_true(all(out$pred_calibrated >= 0 & out$pred_calibrated <= 1))

  ca <- out[out$iso_code == "A", ]
  # pred=0 -> intercept 0.1 (recovered from TRAIN only, not corrupted by OOS 99s)
  expect_equal(ca$pred_calibrated[1], 0.1, tolerance = 1e-3)
  # a training point matches the affine map a*pred+b
  expect_equal(ca$pred_calibrated[5], 0.5 * predA[5] + 0.1, tolerance = 1e-6)
  # ISO B slope/intercept recovered
  cb <- out[out$iso_code == "B", ]
  expect_equal(cb$pred_calibrated[which.min(predB)], 0.05, tolerance = 1e-3)  # pred=0 -> 0.05
})

test_that("calibrate_psi_predictions falls back to identity for sparse ISOs (warns)", {
  dates   <- seq(as.Date("2024-01-01"), by = "week", length.out = 10)
  pred_df <- data.frame(iso_code = "C", date = dates, pred_smooth = seq(0, 0.9, length.out = 10))
  obs_df  <- data.frame(iso_code = "C", date = dates[1:3],
                        transmission_intensity = c(0.2, 0.3, 0.4))  # 3 train pts < min_train (8)
  expect_warning(out <- calibrate_psi_predictions(pred_df, obs_df, max(dates)), "identity")
  expect_equal(out$pred_calibrated, pmax(0, pmin(1, pred_df$pred_smooth)))
})

test_that("calibrate_psi_predictions clips out-of-range affine outputs to [0,1]", {
  dates    <- seq(as.Date("2024-01-01"), by = "week", length.out = 20)
  # obs >> pred so the affine map projects some predictions above 1
  pred_df <- data.frame(iso_code = "D", date = dates, pred_smooth = seq(0.1, 0.5, length.out = 20))
  obs_df  <- data.frame(iso_code = "D", date = dates,
                        transmission_intensity = seq(0.5, 1.0, length.out = 20) * 3)  # forces slope>1, intercept
  out <- calibrate_psi_predictions(pred_df, obs_df, max(dates))
  expect_true(all(out$pred_calibrated >= 0 & out$pred_calibrated <= 1))
})
