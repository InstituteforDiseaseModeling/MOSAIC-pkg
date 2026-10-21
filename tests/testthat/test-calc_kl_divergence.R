# test-calc_kl_divergence.R
# The percentile-scale (PIT) estimator (MOSAIC 1.4.0): draws mapped to their
# prior percentiles, B = 20 equal bins, KL = sum p log(p B) minus the
# finite-ESS correction (B - 1) / (2 ESS). Analytic references in comments.

# ---- hand-computed fixture -----------------------------------------------------

test_that("the binned estimator matches a hand-computed fixture", {
  # 40 prior draws 1..40, uniform reference: percentiles (r - 0.5)/40, 2 draws per bin.
  # Posterior weight 3 on draws 1..20 (bins 1..10), 1 on 21..40 (bins 11..20):
  # p_b = 6/80 for b <= 10 and 2/80 above, so the binned divergence is
  # 10 (3/40) log(20 * 3/40) + 10 (1/40) log(20 * 1/40) = (3/4) log(3/2) + (1/4) log(1/2)
  # = 0.13081; ESS = 80^2 / (20 * 9 + 20 * 1) = 32, correction 19 / 64 = 0.297 > 0.131 -> 0.
  x <- 1:40
  w <- c(rep(3, 20), rep(1, 20))
  kl_plug <- (3 / 4) * log(3 / 2) + (1 / 4) * log(1 / 2)
  expect_equal(calc_kl_divergence(x, w, x, NULL), 0)
  # 50 tied copies of every draw: each value still spans half a bin, ESS 1600,
  # correction 19 / 3200
  xx <- rep(x, each = 50); ww <- rep(w, each = 50)
  expect_equal(calc_kl_divergence(xx, ww, xx, NULL), kl_plug - 19 / (2 * 1600), tolerance = 1e-12)
  expect_equal(kl_plug - 19 / 3200, 0.124875, tolerance = 1e-5)
})

# ---- reference cases -----------------------------------------------------------

test_that("an identified normal posterior agrees with the analytic normal-normal KL", {
  set.seed(42)
  x <- rnorm(50000)                                    # prior N(0, 1)
  for (pr in list(c(0.3, 0.5), c(0, 0.8), c(0.5, 0.3))) {
    w <- stats::dnorm(x, pr[1], pr[2]) / stats::dnorm(x)
    analytic <- log(1 / pr[2]) + (pr[2]^2 + pr[1]^2) / 2 - 0.5
    kl <- calc_kl_divergence(x, w, x, NULL)
    expect_lt(abs(kl - analytic), 0.05 + 0.05 * analytic)
    expect_lte(kl, analytic + 0.01)                    # binning can only lose information
  }
  # two separate samples (randomised PIT against the reference ECDF)
  kl2 <- calc_kl_divergence(rnorm(20000, 0.5, 0.5), NULL, rnorm(20000), NULL)
  expect_lt(abs(kl2 - (log(2) + (0.25 + 0.25) / 2 - 0.5)), 0.06)
})

test_that("a heavy-tailed lognormal prior gives the same KL as on the log scale", {
  set.seed(3)
  y <- rlnorm(50000, 0, 2)
  w <- stats::dnorm(log(y), 1, 0.5) / stats::dnorm(log(y), 0, 2)
  analytic <- log(2 / 0.5) + (0.25 + 1) / (2 * 4) - 0.5     # KL(N(1, .5^2) || N(0, 2^2)) = 1.0431
  kl <- calc_kl_divergence(y, w, y, NULL)
  expect_lt(abs(kl - analytic), 0.08)
  # invariance: any monotone transform gives the identical estimate
  expect_identical(calc_kl_divergence(log(y), w, log(y), NULL), kl)
  expect_identical(calc_kl_divergence(-1 / y, w, -1 / y, NULL), kl)
})

test_that("integer-valued parameters: unmoved is 0, shifted is close to the discrete KL", {
  set.seed(4)
  z <- rpois(50000, 5)
  expect_equal(calc_kl_divergence(z, rep(1, 50000), z, NULL), 0)
  w <- stats::dpois(z, 7) / stats::dpois(z, 5)
  k <- 0:60
  analytic <- sum(stats::dpois(k, 7) * log(stats::dpois(k, 7) / stats::dpois(k, 5)))  # 0.355
  kl <- calc_kl_divergence(z, w, z, NULL)
  expect_lt(abs(kl - analytic), 0.06)
  expect_gt(kl, 0.2)
})

test_that("a bounded beta prior with the posterior near its boundary is estimated without boundary bias", {
  set.seed(5)
  b <- rbeta(50000, 0.5, 20)
  w <- stats::dbeta(b, 2, 60) / stats::dbeta(b, 0.5, 20)
  f <- function(t) { p <- stats::dbeta(t, 2, 60); q <- stats::dbeta(t, 0.5, 20)
                     ifelse(p > 0, p * log(p / q), 0) }
  analytic <- stats::integrate(f, 0, 1, subdivisions = 2000L)$value   # 0.419
  expect_lt(abs(calc_kl_divergence(b, w, b, NULL) - analytic), 0.05)
})

test_that("flat or prior-like weights give about 0", {
  set.seed(6)
  x <- rnorm(20000)
  expect_equal(calc_kl_divergence(x, rep(1, 20000), x, NULL), 0)
  expect_equal(calc_kl_divergence(x, NULL, x, NULL), 0)
  expect_lt(calc_kl_divergence(x, rexp(20000), x, NULL), 0.005)       # noise weights
  w <- numeric(20000); w[sample(20000, 300)] <- 1                     # random prior subsample
  expect_lt(calc_kl_divergence(x, w, x, NULL), 0.02)
  expect_lt(calc_kl_divergence(x[w > 0], NULL, x, NULL), 0.02)
})

test_that("the estimate is bounded by log(n_bins) and ranks concentration up to it", {
  set.seed(7)
  x <- runif(20000)
  kl <- vapply(c(0.2, 0.08, 0.04), function(s) {
    w <- stats::dnorm(x, 0.5, s)
    calc_kl_divergence(x, w, x, NULL)
  }, numeric(1))
  expect_true(all(diff(kl) > 0))
  expect_true(all(kl <= log(20)))
  r <- rank(x)
  pm <- numeric(20000); pm[r > 10200 & r <= 10800] <- 1                # percentiles .51-.54: one bin
  expect_equal(calc_kl_divergence(x, pm, x, NULL), log(20) - 19 / (2 * 600), tolerance = 1e-12)
  expect_lte(calc_kl_divergence(x, pm, x, NULL, n_bins = 50L), log(50))
})

test_that("a posterior with fewer effective draws than bins returns NA with a warning", {
  x <- rnorm(1000)
  w <- numeric(1000); w[1:10] <- 1
  expect_warning(kl <- calc_kl_divergence(x, w, x, NULL), "Kish ESS of weights1 is 10.0, below n_bins = 20")
  expect_true(is.na(kl))
  expect_false(is.na(suppressWarnings(calc_kl_divergence(x, w, x, NULL, n_bins = 5L))))
})

test_that("the estimate is deterministic and leaves the caller's RNG untouched", {
  set.seed(8)
  z <- rpois(5000, 3); w <- stats::dpois(z, 4) / stats::dpois(z, 3) * runif(5000)
  set.seed(99); a <- runif(1)
  set.seed(99); k1 <- calc_kl_divergence(z, w, z, NULL); b <- runif(1)
  expect_identical(a, b)
  expect_identical(calc_kl_divergence(z, w, z, NULL), k1)
  expect_gt(k1, 0)
  # the seed only moves which tied draws fall either side of a bin edge
  expect_gt(abs(calc_kl_divergence(z, w, z, NULL, seed = 2) - k1), 1e-6)
  expect_lt(abs(calc_kl_divergence(z, w, z, NULL, seed = 2) - k1), 0.02)
})

test_that("weights are normalised internally and reference weights are honoured", {
  set.seed(9)
  x <- rnorm(5000); w <- runif(5000)
  expect_identical(calc_kl_divergence(x, w, x, NULL), calc_kl_divergence(x, 7 * w, x, NULL))
  # a weighted reference equal to the posterior gives 0
  q <- stats::dnorm(x, 0.5, 0.7) / stats::dnorm(x)
  expect_equal(calc_kl_divergence(x, q, x, q), 0)
  expect_gt(calc_kl_divergence(x, q, x, NULL), 0.1)
})

# ---- validation and compatibility -----------------------------------------------

test_that("calc_kl_divergence validates input parameters", {
  s1 <- rnorm(100); s2 <- rnorm(100)
  expect_error(calc_kl_divergence(c("a", "b"), NULL, s2, NULL), "must be numeric")
  expect_error(calc_kl_divergence(s1, NULL, c("a", "b"), NULL), "must be numeric")
  expect_error(calc_kl_divergence(numeric(0), NULL, s2, NULL), "must not be empty")
  expect_error(calc_kl_divergence(1, NULL, s2, NULL), "at least 2 points")
  expect_error(calc_kl_divergence(c(1, 2, NA), NULL, s2, NULL), "finite values")
  expect_error(calc_kl_divergence(s1, NULL, c(1, 2, Inf), NULL), "finite values")
  expect_error(calc_kl_divergence(s1, rep(1, 50), s2, NULL), "same length as samples1")
  expect_error(calc_kl_divergence(s1, NULL, s2, rep(1, 50)), "same length as samples2")
  expect_error(calc_kl_divergence(s1, c(-1, rep(1, 99)), s2, NULL), "non-negative")
  expect_error(calc_kl_divergence(s1, rep(0, 100), s2, NULL), "non-zero sum")
  expect_error(calc_kl_divergence(s1, "a", s2, NULL), "numeric or NULL")
  expect_error(calc_kl_divergence(s1, NULL, s2, NULL, n_bins = 1), "n_bins")
  expect_error(calc_kl_divergence(s1, NULL, s2, NULL, seed = NA), "seed")
})

test_that("n_points and eps are accepted for compatibility but ignored, with a warning", {
  set.seed(10)
  s1 <- rnorm(500, 0.5); s2 <- rnorm(500)
  ref <- calc_kl_divergence(s1, NULL, s2, NULL)
  expect_warning(k1 <- calc_kl_divergence(s1, NULL, s2, NULL, n_points = 100), "ignored since MOSAIC 1.4.0")
  expect_warning(k2 <- calc_kl_divergence(s1, NULL, s2, NULL, eps = 1e-3), "ignored since MOSAIC 1.4.0")
  expect_identical(k1, ref)
  expect_identical(k2, ref)
})

test_that("identical constants return 0 with a warning", {
  expect_warning(kl <- calc_kl_divergence(rep(5, 100), NULL, rep(5, 100), NULL),
                 "All samples have the same value")
  expect_equal(kl, 0)
})

test_that("deprecated calculate_kl_divergence still works", {
  set.seed(987)
  s1 <- rnorm(100); s2 <- rnorm(100, mean = 1)
  expect_warning(old <- calculate_kl_divergence(s1, NULL, s2, NULL), "deprecated")
  expect_identical(old, calc_kl_divergence(s1, NULL, s2, NULL))
})

test_that("the posterior-quantile KL is calc_kl_divergence on the members against all draws", {
  set.seed(11)
  prior <- rnorm(20000)
  post <- prior[1:400] * 0.4 + 0.3
  w <- runif(400)
  expect_identical(MOSAIC:::.mosaic_posterior_kl(prior, post, w),
                   calc_kl_divergence(post, w / sum(w), prior, NULL))
  expect_true(is.na(MOSAIC:::.mosaic_posterior_kl(prior, post[1:5])))          # < 10 draws
  w1 <- c(1, rep(1e-9, 399))                                                  # Kish ESS ~ 1
  expect_true(is.na(MOSAIC:::.mosaic_posterior_kl(prior, post, w1)))
  expect_equal(MOSAIC:::.mosaic_posterior_kl(rep(2, 50), rep(2, 20)), 0)
  expect_true(is.na(MOSAIC:::.mosaic_posterior_kl(rep(2, 50), rep(3, 20))))
})

test_that("exactly n_bins equal weights is assessed, not NA (T12 tolerance)", {
  set.seed(12)
  prior <- seq(0.00005, 0.99995, length.out = 10000)      # uniform reference, percentiles = values
  # 20 equal weights all in the first bin: KL = log(20) - 19 / (2 * 20)
  inside <- seq(0.01, 0.04, length.out = 20)
  expect_silent(k <- calc_kl_divergence(inside, rep(1, 20), prior, NULL))
  expect_rel_equal(k, log(20) - 19 / 40, rel = 1e-12)
  # one draw per bin: plug-in 0, so 0 after the correction
  expect_identical(calc_kl_divergence((1:20 - 0.5) / 20, rep(1, 20), prior, NULL), 0)
  # 19 equal weights is below n_bins: NA with a warning
  expect_warning(k19 <- calc_kl_divergence(inside[1:19], rep(1, 19), prior, NULL), "below n_bins")
  expect_true(is.na(k19))
})

test_that("KL bands are suppressed below ESS 100 (T12)", {
  band <- MOSAIC:::.mosaic_kl_band
  kl <- c(0.01, 0.049999, 0.05, 0.3, 0.5, 2, NA)
  expect_identical(band(kl, ess = 200),
                   c("largely prior", "largely prior", "partly informed", "partly informed",
                     "data-informed", "data-informed", NA))
  expect_identical(band(kl, ess = 100)[1], "largely prior")
  expect_identical(band(kl, ess = 99.9), rep(NA_character_, 7))
  expect_identical(band(kl, ess = NA), rep(NA_character_, 7))
  expect_identical(band(c(0.1, 0.4), ess = 500, edges = c(0.2, 0.3)), c("largely prior", "data-informed"))
})
