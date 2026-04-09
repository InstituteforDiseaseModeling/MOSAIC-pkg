# Tests for nb_size_from_obs_weighted() — internal helper for NB dispersion estimation

test_that("Poisson data (var <= mean) returns Inf", {
  # Constant data: var=0 <= mean=10
  x <- rep(10, 20)
  w <- rep(1, 20)
  k <- MOSAIC:::nb_size_from_obs_weighted(x, w)
  expect_true(is.infinite(k))
})

test_that("overdispersed data returns finite k", {
  set.seed(42)
  x <- rnbinom(100, mu = 10, size = 2)
  w <- rep(1, 100)
  k <- MOSAIC:::nb_size_from_obs_weighted(x, w)
  expect_true(is.finite(k))
  expect_true(k >= 3)  # k_min floor
})

test_that("k_min floor is applied", {
  # Very overdispersed data: true k ~ 0.5
  set.seed(42)
  x <- rnbinom(200, mu = 10, size = 0.5)
  w <- rep(1, 200)
  k <- MOSAIC:::nb_size_from_obs_weighted(x, w, k_min = 3)
  expect_true(k >= 3)
})

test_that("k_max cap is applied", {
  # Low overdispersion: k will be very large
  set.seed(42)
  x <- rpois(200, lambda = 100)  # Poisson -> var = mean, but sample fluctuation
  w <- rep(1, 200)
  k <- MOSAIC:::nb_size_from_obs_weighted(x, w, k_min = 1, k_max = 100)
  expect_true(k <= 100 || is.infinite(k))  # Either capped or Inf (Poisson)
})

test_that("all-NA input returns Inf", {
  x <- rep(NA_real_, 10)
  w <- rep(1, 10)
  k <- MOSAIC:::nb_size_from_obs_weighted(x, w)
  expect_true(is.infinite(k))
})

test_that("zero-weight entries are excluded", {
  x <- c(rep(10, 10), 1000)  # Outlier at end
  w <- c(rep(1, 10), 0)       # Zero-weight the outlier
  k <- MOSAIC:::nb_size_from_obs_weighted(x, w)
  # Without outlier, data is constant -> Inf
  expect_true(is.infinite(k))
})
