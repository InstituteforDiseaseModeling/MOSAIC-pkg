test_that("calc_model_R2 corr method returns correct R2", {
  set.seed(42)
  obs <- 1:100
  est <- obs + rnorm(100, 0, 5)

  r2 <- calc_model_R2(obs, est, method = "corr")
  expect_true(is.numeric(r2))
  expect_true(r2 >= 0 && r2 <= 1)

  # Perfect correlation
  r2_perfect <- calc_model_R2(obs, obs)
  expect_equal(r2_perfect, 1.0)

  # Compare to manual cor()^2
  expected <- stats::cor(obs, est)^2
  expect_equal(r2, expected, tolerance = 1e-10)
})

test_that("calc_model_R2 sse method returns correct R2", {
  obs <- 1:100
  est <- obs  # perfect predictions

  r2 <- calc_model_R2(obs, est, method = "sse")
  expect_equal(r2, 1.0)

  # Predictions equal to mean baseline => R2 = 0
  est_mean <- rep(mean(obs), 100)
  r2_baseline <- calc_model_R2(obs, est_mean, method = "sse")
  expect_equal(r2_baseline, 0.0)
})

test_that("cor2 R2 is insensitive to scale (bias-invariant)", {
  set.seed(42)
  obs <- 1:100 + rnorm(100, 0, 2)
  est <- obs * 3.5  # 3.5x over-prediction

  r2_original <- calc_model_R2(obs, obs, method = "corr")
  r2_scaled   <- calc_model_R2(obs, est, method = "corr")

  # cor2 only cares about shape, not magnitude
  expect_equal(r2_original, r2_scaled, tolerance = 1e-10)
})

test_that("SSE R2 IS sensitive to scale", {
  set.seed(42)
  obs <- 1:100
  est <- obs * 3.5

  r2_perfect <- calc_model_R2(obs, obs, method = "sse")
  r2_scaled  <- calc_model_R2(obs, est, method = "sse")

  expect_equal(r2_perfect, 1.0)
  expect_true(r2_scaled < r2_perfect)
})

test_that("cor2 R2 == SSE R2 when predictions are unbiased", {
  set.seed(42)
  obs <- 1:50
  # Unbiased noise: same mean, same scale
  est <- obs + rnorm(50, 0, 0.01)

  r2_corr <- calc_model_R2(obs, est, method = "corr")
  r2_sse  <- calc_model_R2(obs, est, method = "sse")

  # When predictions are nearly unbiased, both methods should be very close

  expect_equal(r2_corr, r2_sse, tolerance = 0.01)
})

test_that("calc_model_R2 handles NA values", {
  obs <- c(1, 2, NA, 4, 5)
  est <- c(1, 2, 3, NA, 5)

  r2 <- calc_model_R2(obs, est, na_rm = TRUE)
  expect_true(is.numeric(r2))
  expect_true(!is.na(r2))

  r2_no_rm <- calc_model_R2(obs, est, na_rm = FALSE)
  expect_true(is.na(r2_no_rm))
})

test_that("calc_model_R2 handles edge cases", {
  # Fewer than 2 valid pairs
  r2 <- calc_model_R2(c(1, NA), c(NA, 2))
  expect_true(is.na(r2))

  # Length mismatch
  r2 <- calc_model_R2(1:5, 1:3)
  expect_true(is.na(r2))

  # Constant observed (SST = 0)
  r2 <- calc_model_R2(rep(5, 10), 1:10, method = "sse")
  expect_true(is.na(r2))
})

test_that("calc_bias_ratio returns correct values", {
  # Perfect predictions
  expect_equal(calc_bias_ratio(1:10, 1:10), 1.0)

  # 2x over-prediction
  expect_equal(calc_bias_ratio(1:10, (1:10) * 2), 2.0)

  # Under-prediction
  expect_equal(calc_bias_ratio(1:10, (1:10) * 0.5), 0.5)

  # Handles NAs
  result <- calc_bias_ratio(c(1, 2, NA), c(1, NA, 3))
  expect_true(is.numeric(result))

  # Zero mean observed
  result <- calc_bias_ratio(rep(0, 5), 1:5)
  expect_true(is.na(result))
})
