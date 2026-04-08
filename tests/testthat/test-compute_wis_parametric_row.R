# Tests for compute_wis_parametric_row() — WIS helper with 0.5 MAE coefficient

test_that("WIS returns finite for simple case", {
  y <- c(10, 20, 30)
  est <- c(10, 20, 30)
  w <- c(1, 1, 1)
  probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
  wis <- MOSAIC:::compute_wis_parametric_row(y, est, w, probs, k_use = 3)
  expect_true(is.finite(wis))
  expect_true(wis >= 0)  # WIS is non-negative
})

test_that("WIS returns NA for all-NA input", {
  y <- rep(NA_real_, 5)
  est <- rep(10, 5)
  w <- rep(1, 5)
  probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
  wis <- MOSAIC:::compute_wis_parametric_row(y, est, w, probs, k_use = 3)
  expect_true(is.na(wis))
})

test_that("WIS works with k=Inf (Poisson quantiles)", {
  y <- c(10, 20, 30)
  est <- c(10, 20, 30)
  w <- c(1, 1, 1)
  probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
  wis <- MOSAIC:::compute_wis_parametric_row(y, est, w, probs, k_use = Inf)
  expect_true(is.finite(wis))
})

test_that("WIS includes 0.5 MAE coefficient (Bracher et al. 2021)", {
  # With only median (no interval pairs), K=0, denom = 0.5
  # WIS = (0.5 * MAE) / 0.5 = MAE
  # If the 0.5 factor were missing, WIS would be (1.0 * MAE) / 0.5 = 2*MAE
  y <- c(100)
  est <- c(50)
  w <- c(1)
  probs <- c(0.5)

  wis <- MOSAIC:::compute_wis_parametric_row(y, est, w, probs, k_use = 3)
  q_med <- qnbinom(0.5, mu = 50, size = 3)
  mae <- abs(100 - q_med)
  # With 0.5 coefficient: WIS = (0.5 * mae) / 0.5 = mae
  expect_equal(wis, mae, tolerance = 0.01)
})

test_that("WIS works with non-standard quantiles", {
  y <- c(10, 20, 30)
  est <- c(10, 20, 30)
  w <- c(1, 1, 1)
  # Non-standard quantiles where (1-p) %in% uppers might fail with exact float comparison
  probs <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  wis <- MOSAIC:::compute_wis_parametric_row(y, est, w, probs, k_use = 3)
  expect_true(is.finite(wis))
})
