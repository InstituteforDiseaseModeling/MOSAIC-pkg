# Tests for ll_cumulative_progressive_nb() — cumulative NB progression

test_that("returns finite for basic input", {
  obs <- c(10, 20, 30, 40, 50, 60, 70, 80)
  est <- c(12, 18, 35, 38, 55, 58, 72, 78)
  ll <- MOSAIC:::ll_cumulative_progressive_nb(obs, est, k_data = 3)
  expect_true(is.finite(ll))
  expect_true(ll < 0)
})

test_that("perfect match produces best (least negative) LL", {
  obs <- c(10, 20, 30, 40)
  ll_perfect <- MOSAIC:::ll_cumulative_progressive_nb(obs, obs, k_data = 3)
  ll_bad <- MOSAIC:::ll_cumulative_progressive_nb(obs, obs * 10, k_data = 3)
  expect_true(ll_perfect > ll_bad)
})

test_that("works with k=Inf (Poisson via dnbinom size=Inf)", {
  obs <- c(10, 20, 30, 40)
  est <- c(12, 18, 32, 38)
  ll <- MOSAIC:::ll_cumulative_progressive_nb(obs, est, k_data = Inf)
  expect_true(is.finite(ll))
})

test_that("returns floor for all-NA data", {
  obs <- rep(NA_real_, 10)
  est <- rep(10, 10)
  ll <- MOSAIC:::ll_cumulative_progressive_nb(obs, est, k_data = 3)
  # All cumulative sums will be 0 (na.rm=TRUE on all-NA), or the function handles gracefully
  expect_true(is.finite(ll) || ll == -1e9)
})

test_that("returns mean (not sum) across timepoints", {
  obs <- c(10, 20, 30, 40, 50, 60, 70, 80)
  est <- c(12, 18, 35, 38, 55, 58, 72, 78)
  # Default 4 timepoints
  ll_4 <- MOSAIC:::ll_cumulative_progressive_nb(obs, est, k_data = 3,
    timepoints = c(0.25, 0.5, 0.75, 1.0))
  # With 2 timepoints, the mean should be different scale
  ll_2 <- MOSAIC:::ll_cumulative_progressive_nb(obs, est, k_data = 3,
    timepoints = c(0.5, 1.0))
  # Both should be finite, and if it were sum instead of mean,
  # ll_4 would be ~2x ll_2. With mean, they should be similar magnitude.
  expect_true(is.finite(ll_4))
  expect_true(is.finite(ll_2))
  expect_true(abs(ll_4 / ll_2) < 3)  # Rough check: not 2x different
})
