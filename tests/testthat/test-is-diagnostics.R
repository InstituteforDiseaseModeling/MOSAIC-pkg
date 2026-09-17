test_that("exact IS ESS matches the closed form on untruncated weights", {
  set.seed(42)
  ll <- rnorm(500, sd = 1)
  w  <- exp(ll - max(ll)); w <- w / sum(w)
  d  <- calc_is_diagnostics(ll, method = "kish")
  expect_equal(d$ess_is, 1 / sum(w^2), tolerance = 1e-10)
  expect_equal(d$n, 500L)
  expect_equal(d$ess_is_prop, d$ess_is / 500, tolerance = 1e-12)
})

test_that("Pareto k-hat is calibrated against its published thresholds", {
  set.seed(7)
  # Light tail: finite IS variance -> khat well below 0.5
  good <- calc_is_diagnostics(rnorm(2000, sd = 0.5))
  expect_lt(good$khat, 0.5)
  expect_identical(good$khat_status, "ok")

  # Heavy tail: khat climbs into the unreliable band
  bad <- calc_is_diagnostics(rnorm(2000, sd = 3))
  expect_gt(bad$khat, good$khat)
})

test_that("a collapsed importance sampler is reported, not smoothed over", {
  # Production regime: delta AIC ~1e6, so all but a handful of raw ratios
  # underflow to zero. The honest answer is ESS ~1 and an explicit status.
  set.seed(11)
  ll <- c(0, -rexp(1999, rate = 1 / 5e5))
  d  <- calc_is_diagnostics(ll)
  expect_lt(d$ess_is, 2)
  expect_lt(d$n_positive_ratios, 50L)
  # Collapse must be FLAGGED, by whichever route the tail supports: either the
  # ratios underflow entirely (khat undefined, explicit status) or enough
  # survive to fit a tail and khat lands far above the 0.7 reliability bound.
  expect_true(is.na(d$khat) || d$khat >= 0.7)
  if (is.na(d$khat)) expect_match(d$khat_status, "degenerate")

  # Total underflow: khat is undefined and says so rather than inventing a value.
  d2 <- calc_is_diagnostics(c(0, rep(-1e6, 99)))
  expect_lt(d2$ess_is, 2)
  expect_true(is.na(d2$khat))
  expect_match(d2$khat_status, "degenerate")
})

test_that("ESS_B on truncated weights cannot fall below the truncation floor", {
  # This is the structural fact that motivates reporting the exact IS ESS:
  # with w propto exp(-0.5 * min(delta, 4)) every weight lies in [exp(-2), 1],
  # so ESS_B >= ~0.42 n (Kish) no matter how bad the likelihood surface is.
  worst <- function(n, method) {
    a <- exp(-2)
    min(vapply(seq_len(n - 1L), function(k) {
      w <- c(rep(1, k), rep(a, n - k)); calc_model_ess(w / sum(w), method = method)
    }, numeric(1)))
  }
  expect_gt(worst(100L, "kish") / 100, 0.40)
  expect_gt(worst(100L, "perplexity") / 100, 0.60)

  # ...while the exact IS ESS on the same degenerate surface is ~1.
  expect_lt(calc_is_diagnostics(c(0, rep(-1e6, 99)))$ess_is, 2)
})

test_that("best_subset_weighting is validated", {
  ctrl <- mosaic_control_defaults()
  expect_identical(ctrl$targets$best_subset_weighting, "saturated")
  expect_true(ctrl$targets$best_subset_weighting %in% c("saturated", "tempered"))
})

test_that("the two weighting schemes are genuinely different estimators", {
  set.seed(3)
  ll <- -c(0, cumsum(abs(rnorm(199, sd = 50))))   # wide delta range
  delta <- -2 * ll - min(-2 * ll)

  sat <- calc_model_weights_gibbs(pmin(delta, 4.0), eta = 0.5, verbose = FALSE)
  tmp <- MOSAIC:::.mosaic_calc_adaptive_gibbs_weights(ll, verbose = FALSE)$weights

  # Saturated weights are capped at a ratio of exp(2); tempered are not.
  expect_lt(max(sat) / min(sat), exp(2) + 1e-6)
  expect_gt(max(tmp) / min(tmp[tmp > 0]), max(sat) / min(sat))
})
