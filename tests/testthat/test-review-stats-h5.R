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

test_that("run_MOSAIC passes posterior_weighting to the tier search and optimizer", {
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
  # The tier search lives in .mosaic_tier_best_subset() (shared by run_MOSAIC and
  # the auto-mode stopping rule); the optimizer call stays in run_MOSAIC, where
  # the scheme is read once into weighting_scheme.
  walk(body(MOSAIC:::.mosaic_tier_best_subset))
  walk(body(MOSAIC::run_MOSAIC))
  fns <- vapply(calls, function(e) as.character(e[[1]]), character(1))
  expect_setequal(unique(fns), c("grid_search_best_subset", "optimize_ensemble_subset"))
  for (e in calls) {
    arg <- e$weighting
    expect_false(is.null(arg))
    expect_true(any(grepl("^(scheme|weighting_scheme|opt_weighting)$", deparse(arg))))
  }
  body_txt <- paste(deparse(body(MOSAIC::run_MOSAIC)), collapse = "\n")
  expect_true(grepl("weighting_scheme <- control$targets$posterior_weighting", body_txt, fixed = TRUE))
  tier_txt <- paste(deparse(MOSAIC:::.mosaic_tier_best_subset), collapse = "\n")
  expect_true(grepl("scheme <- control$targets$posterior_weighting", tier_txt, fixed = TRUE))
})

test_that(".mosaic_best_subset_weights has a single definition", {
  r_files <- list.files(test_path("..", "..", "R"), full.names = TRUE, pattern = "[.]R$")
  skip_if(length(r_files) == 0, "R sources not available")
  defs <- unlist(lapply(r_files, function(f) {
    grep("^\\.mosaic_best_subset_weights\\s*<-", readLines(f, warn = FALSE), value = TRUE)
  }))
  expect_length(defs, 1L)
})

# ---- Items 3 and 4 (revised in 1.4.0): one KL core, percentile scale ---------
# The weighted-KDE KL and its weighted bandwidth were replaced by the
# percentile-scale estimator of calc_kl_divergence(); the properties these items
# pinned (no smoothing of concentrated importance weights back to the prior, NA
# rather than a spurious number for near-one-hot weights, one shared core) are
# re-asserted on it.

test_that("near-one-hot weights give NA, not a spurious small number", {
  w <- c(0.999, 0.001)
  set.seed(1)
  prior <- stats::runif(20000)
  x <- stats::runif(1000)
  expect_true(is.na(MOSAIC:::.mosaic_posterior_kl(prior, x, c(w, rep(0, 998)))))
  expect_warning(kl <- calc_kl_divergence(x, weights1 = c(w, rep(0, 998)), samples2 = prior),
                 "below n_bins")
  expect_true(is.na(kl))
  w1 <- numeric(1000); w1[7] <- 1
  expect_true(is.na(MOSAIC:::.mosaic_posterior_kl(prior, x, w1)))
})

test_that("concentrated importance weights are not smoothed back towards the prior", {
  set.seed(1)
  prior <- stats::runif(20000)
  # Importance-weighted U(0,1) draws targeting N(0.5, 0.01^2): the posterior sits
  # in the two central percentile bins, so the estimate is near log(10) = 2.30
  # less the finite-ESS correction; the old unweighted-bandwidth KDE gave ~1.3.
  x <- stats::runif(5000)
  w <- stats::dnorm(x, 0.5, 0.01)
  kl <- MOSAIC:::.mosaic_posterior_kl(prior, x, w)
  ess <- sum(w)^2 / sum(w^2)
  expect_gt(kl, log(10) - 19 / (2 * ess) - 0.1)
  expect_lte(kl, log(20))
})

test_that("one KL core: the posterior-quantile KL is calc_kl_divergence", {
  set.seed(2)
  prior <- stats::runif(20000)
  post <- stats::rnorm(200, 0.3, 0.02)
  expect_identical(MOSAIC:::.mosaic_posterior_kl(prior, post),
                   calc_kl_divergence(post, rep(1 / 200, 200), prior, NULL))
  # shifted normals, same variance: KL = 0.5 (binned estimate slightly lower)
  a <- stats::rnorm(2e4); b <- stats::rnorm(2e4, 1)
  kl <- calc_kl_divergence(a, NULL, b, NULL)
  expect_lt(abs(kl - 0.5), 0.06)
})
