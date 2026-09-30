# Deep-review follow-ups (statistics): IC Beta anchoring, best-subset
# weighting consistency, and the weighted KDE bandwidth in the KL estimators.

.beta_mean_h5 <- function(x) x$shape1 / (x$shape1 + x$shape2)

# ---- Item 1: the IC estimate is the prior MEAN, also for skewed draws --------

test_that("E/I prior mean equals a right-skewed Monte Carlo mean at VI = 30", {
  # Right-skewed counts (median well below the mean), as for a small active
  # outbreak. Treating the mean as the Beta mode put the prior mean ~4.5x above
  # the estimate at VI = 30 (MWI).
  set.seed(11)
  counts <- round(stats::rlnorm(500, log(40), 1.2))
  N <- 2e7
  fit <- MOSAIC:::.est_initial_E_I_fit(counts, N, "E", "MWI", 30, 500L,
                                      total_cases = 400, verbose = FALSE)
  expect_rel_equal(.beta_mean_h5(fit), mean(counts / N), rel = 1e-9)
  # The centre is the mean, not the median: the J-shaped fit's median is lower
  expect_lt(stats::qbeta(0.5, fit$shape1, fit$shape2), mean(counts / N))
})

test_that("R and S refits keep the sample mean exactly under SD inflation", {
  set.seed(12)
  x <- stats::rbeta(400, 2, 30)
  for (vi in c(0.5, 1, 3, 30)) {
    sh <- MOSAIC:::.fit_beta_inflated_samples(x, vi)
    expect_rel_equal(sh[1] / sum(sh), mean(x), rel = 1e-12)
  }
  # Hand-computed: mean 0.2, sd 0.1, VI 2 -> var 0.04, nu = 0.16 / 0.04 - 1 = 3
  y <- 0.2 + 0.1 * c(-1, 1) / sd(c(-1, 1))
  expect_equal(MOSAIC:::.fit_beta_inflated_samples(y, 2), c(0.6, 2.4), tolerance = 1e-12)
})

# ---- Item 2: tier search uses the configured best-subset weighting ----------

test_that("grid_search_best_subset certifies n under the scheme it is given", {
  set.seed(3)
  ll <- -cumsum(stats::rexp(600, 2))
  res <- data.frame(sim = seq_along(ll), likelihood = ll)
  for (scheme in c("saturated", "tempered")) {
    g <- grid_search_best_subset(res, target_ESS = 20, target_A = 0.01, target_CVw = 100,
                                 min_size = 10, max_size = 600, weighting = scheme)
    expect_true(g$converged)
    w <- MOSAIC:::.mosaic_best_subset_weights(ll[1:g$n], scheme)$weights
    expect_equal(g$metrics$ESS, calc_model_ess(w, "kish"), tolerance = 1e-12)
    expect_gte(g$metrics$ESS, 20)
    w_prev <- MOSAIC:::.mosaic_best_subset_weights(ll[1:(g$n - 1)], scheme)$weights
    expect_lt(calc_model_ess(w_prev, "kish"), 20)
  }
  # Tempered weights are sharper, so the same target needs a larger subset
  g_sat <- grid_search_best_subset(res, 20, 0.01, 100, 10, 600, weighting = "saturated")
  g_tmp <- grid_search_best_subset(res, 20, 0.01, 100, 10, 600, weighting = "tempered")
  expect_gt(g_tmp$n, g_sat$n)
})

test_that("saturated tier-search ESS matches a hand-computed value", {
  # delta = -2 * (ll - max) = 0, 2, 4, 6 -> capped 0, 2, 4, 4
  # w ~ (1, e^-1, e^-2, e^-2); Kish ESS = (sum w)^2 / sum w^2
  ll <- c(0, -1, -2, -3)
  w <- c(1, exp(-1), exp(-2), exp(-2))
  ess_hand <- sum(w)^2 / sum(w^2)
  g <- grid_search_best_subset(data.frame(sim = 1:4, likelihood = ll),
                               target_ESS = 100, target_A = 0.5, target_CVw = 1,
                               min_size = 4, max_size = 4)
  expect_false(g$converged)
  expect_equal(g$metrics$ESS, ess_hand, tolerance = 1e-12)
})

test_that("run_MOSAIC passes best_subset_weighting to the tier search and optimizer", {
  calls <- list()
  walk <- function(e) {
    if (is.call(e)) {
      fn <- e[[1]]
      if (is.name(fn) && as.character(fn) %in% c("grid_search_best_subset",
                                                 "optimize_ensemble_subset")) {
        calls[[length(calls) + 1L]] <<- e
      }
      for (a in as.list(e)[-1]) if (!missing(a)) walk(a)
    }
  }
  walk(body(MOSAIC::run_MOSAIC))
  fns <- vapply(calls, function(e) as.character(e[[1]]), character(1))
  expect_setequal(unique(fns), c("grid_search_best_subset", "optimize_ensemble_subset"))
  for (e in calls) {
    arg <- e$weighting
    expect_false(is.null(arg))
    expect_true(any(grepl("best_subset_weighting", deparse(arg), fixed = TRUE)))
  }
})

test_that(".mosaic_best_subset_weights has a single definition", {
  r_files <- list.files(test_path("..", "..", "R"), full.names = TRUE, pattern = "[.]R$")
  skip_if(length(r_files) == 0, "R sources not available")
  defs <- unlist(lapply(r_files, function(f) {
    grep("^\\.mosaic_best_subset_weights\\s*<-", readLines(f, warn = FALSE), value = TRUE)
  }))
  expect_length(defs, 1L)
})

# ---- Items 3 and 4: weighted (Kish) KDE bandwidth, shared KL core -----------

test_that("weighted nrd0 bandwidth reduces to bw.nrd0 and uses Kish n_eff", {
  set.seed(4)
  x <- stats::rnorm(300)
  expect_identical(MOSAIC:::.bw_nrd0_weighted(x, rep(2, 300)), stats::bw.nrd0(x))
  # Three-point hand check: x = (0, 1, 2), w = (0.5, 0.25, 0.25):
  # n_eff = 1 / 0.375 = 2.666667, mu = 0.75, weighted var = 0.6875,
  # corrected by n_eff / (n_eff - 1) = 1.6 -> sd_w = sqrt(1.1) = 1.048809
  q <- weighted_quantiles(c(0, 1, 2), c(0.5, 0.25, 0.25), c(0.25, 0.75))
  lo <- min(sqrt(1.1), diff(q) / 1.34)
  expect_equal(MOSAIC:::.bw_nrd0_weighted(c(0, 1, 2), c(0.5, 0.25, 0.25)),
               0.9 * lo * (8 / 3)^(-0.2), tolerance = 1e-12)
  # All mass on one value: bandwidth undefined
  expect_true(is.na(MOSAIC:::.bw_nrd0_weighted(c(0, 1, 2), c(0, 1, 0))))
})

test_that("near-one-hot weights (Kish n_eff < 2) give NA, not an inflated bandwidth", {
  # w = (0.999, 0.001): n_eff = 1 / (0.999^2 + 0.001^2) = 1.002002; the
  # n_eff / (n_eff - 1) correction would be ~501, inflating the SD ~22x and
  # smoothing a near point mass into a wide density (posterior KL ~2 on U(0,1)).
  w <- c(0.999, 0.001)
  expect_equal(1 / sum(w^2), 1.002002, tolerance = 1e-6)
  expect_true(is.na(MOSAIC:::.bw_nrd0_weighted(c(0.2, 0.1), w)))
  set.seed(1)
  prior <- stats::runif(20000)
  x <- stats::runif(1000)
  expect_true(is.na(MOSAIC:::.mosaic_posterior_kl(prior, x, c(w, rep(0, 998)))))
  expect_warning(kl <- calc_kl_divergence(x, weights1 = c(w, rep(0, 998)), samples2 = prior),
                 "non-finite|NA")
  expect_true(is.na(kl))
  # Boundary: n_eff = 1.9 -> NA; n_eff = 2 (two equal weights) -> finite,
  # equal to the hand value 0.9 * min(sd_w, IQR_w / 1.34) * 2^-0.2
  p <- (1 + sqrt(2 / 1.9 - 1)) / 2
  expect_true(is.na(MOSAIC:::.bw_nrd0_weighted(c(0, 1), c(p, 1 - p))))
  xx <- c(0, 1, 5)
  ww <- c(1, 1, 0)
  sd_w <- sqrt(0.25 * 2 / 1)
  q <- weighted_quantiles(xx, ww / 2, c(0.25, 0.75))
  expect_equal(MOSAIC:::.bw_nrd0_weighted(xx, ww),
               0.9 * min(sd_w, diff(q) / 1.34) * 2^(-0.2), tolerance = 1e-12)
})

test_that("posterior KL uses the weighted bandwidth for importance-weighted draws", {
  set.seed(1)
  prior <- stats::runif(20000)
  # Importance-weighted U(0,1) draws targeting N(0.5, 0.01):
  # KL = -0.5 * log(2 * pi * e * 0.01^2) = 3.186. The unweighted bandwidth
  # (~0.066) smoothed this back to ~1.3.
  x <- stats::runif(1000)
  w <- stats::dnorm(x, 0.5, 0.01)
  analytic <- -0.5 * log(2 * pi * exp(1) * 0.01^2)
  kl <- MOSAIC:::.mosaic_posterior_kl(prior, x, w)
  expect_lt(abs(kl - analytic), 0.2)
  # Unweighted-bandwidth reference, computed inline, is far below
  p <- stats::density(x, weights = w / sum(w), bw = stats::bw.nrd0(x), from = 0, to = 1, n = 2048)
  dx <- diff(p$x)
  kl_unw <- sum(dx * (p$y * log(pmax(p$y, 1e-300)))[-1])
  expect_lt(kl_unw, 2)
  # All weight on one draw: undefined, not a spurious small number
  w1 <- numeric(1000); w1[7] <- 1
  expect_true(is.na(MOSAIC:::.mosaic_posterior_kl(prior, x, w1)))
})

test_that("calc_kl_divergence no longer plateaus near log(n_points)", {
  set.seed(2)
  prior <- stats::runif(20000)
  for (s in c(1e-3, 1e-4)) {
    post <- stats::rnorm(500, 0.5, s)
    analytic <- -0.5 * log(2 * pi * exp(1) * stats::sd(post)^2)
    kl <- calc_kl_divergence(post, NULL, prior, NULL, n_points = 1000)
    expect_lt(abs(kl - analytic), 0.1)
  }
  expect_gt(calc_kl_divergence(stats::rnorm(500, 0.5, 1e-4), NULL, prior, NULL), log(1000) + 0.5)
  # Same variance, shifted normals: KL = 0.5 * (mu1 - mu2)^2 / sigma^2 = 0.5
  a <- stats::rnorm(1e4); b <- stats::rnorm(1e4, 1)
  expect_lt(abs(calc_kl_divergence(a, NULL, b, NULL) - 0.5), 0.05)
  # One core: the posterior-quantile KL equals calc_kl_divergence on the same scale
  post <- stats::rnorm(200, 0.3, 0.02)
  expect_equal(MOSAIC:::.mosaic_posterior_kl(prior, post, n_grid = 1000L),
               calc_kl_divergence(post, NULL, prior, NULL, n_points = 1000,
                                  eps = .Machine$double.xmin),
               tolerance = 1e-12)
})
