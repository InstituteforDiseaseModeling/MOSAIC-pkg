# Regression tests from the production-readiness review (group "ensemble"):
# R2 / correlation / bias-ratio weighting and NA handling, the fit-diagnostic
# scorecard, and run_fit_sandbox()'s scoring. Every expected value is hand
# computed.

# ---- calc_model_R2: scalar weights (ensemble-results-03) --------------------

test_that("calc_model_R2 recycles a scalar weight instead of returning NA", {
  y  <- c(1, 3, 2, 5, 4)
  yh <- c(1.2, 2.5, 2.2, 4.1, 4.4)
  # SSE = 0.04+0.25+0.04+0.81+0.16 = 1.30; SST = 10 (mean 3) -> R2 = 0.87
  expect_equal(calc_model_R2(y, yh, method = "sse"), 0.87, tolerance = 1e-12)
  expect_equal(calc_model_R2(y, yh, method = "sse", weights = 1), 0.87, tolerance = 1e-12)
  expect_equal(calc_model_R2(y, yh, method = "corr", weights = 2),
               cor(y, yh)^2, tolerance = 1e-12)
})

test_that("calc_model_R2 aligns full-length weights with the validity mask", {
  y  <- c(1, NA, 3, 2, 5, 4)
  yh <- c(1.2, 9, 2.5, 2.2, 4.1, 4.4)
  w  <- c(1, 100, 1, 1, 1, 1)          # weight of the dropped pair must not matter
  expect_equal(calc_model_R2(y, yh, method = "sse", weights = w), 0.87, tolerance = 1e-12)
  # Hand-computed weighted SSE R2 with unequal weights on the valid pairs.
  w2 <- c(2, 100, 1, 1, 1, 1)
  yv <- c(1, 3, 2, 5, 4); yhv <- c(1.2, 2.5, 2.2, 4.1, 4.4); wv <- c(2, 1, 1, 1, 1)
  yb <- sum(wv * yv) / sum(wv)                      # 17/6
  expected <- 1 - sum(wv * (yv - yhv)^2) / sum(wv * (yv - yb)^2)
  expect_equal(calc_model_R2(y, yh, method = "sse", weights = w2), expected, tolerance = 1e-12)
  # Pre-filtered weights (length = number of valid pairs) are accepted, the
  # same contract as calc_model_cor.
  expect_equal(calc_model_R2(y, yh, method = "sse", weights = wv), expected, tolerance = 1e-12)
  # Wrong-length weights are rejected.
  expect_true(is.na(calc_model_R2(y, yh, method = "sse", weights = c(1, 2))))
})

# ---- calc_model_cor: weights subset with the pairwise mask (ensemble-results-04)

test_that("weighted calc_model_cor drops the weights of invalid pairs", {
  x <- c(1, NA, 3, 4, 2); y <- c(1, 2, 3, 5, 1)
  r_unw <- calc_model_cor(x, y)
  expect_equal(r_unw, cor(c(1, 3, 4, 2), c(1, 3, 5, 1)), tolerance = 1e-12)
  expect_equal(calc_model_cor(x, y, weights = rep(1, 5)), r_unw, tolerance = 1e-12)
  expect_equal(calc_model_cor(x, y, weights = 3), r_unw, tolerance = 1e-12)
  # Unequal weights on the valid pairs: hand-computed weighted Pearson.
  w <- c(1, 50, 2, 1, 1)
  xv <- c(1, 3, 4, 2); yv <- c(1, 3, 5, 1); wv <- c(1, 2, 1, 1)
  mx <- sum(wv * xv) / sum(wv); my <- sum(wv * yv) / sum(wv)
  r_w <- sum(wv * (xv - mx) * (yv - my)) /
    sqrt(sum(wv * (xv - mx)^2) * sum(wv * (yv - my)^2))
  expect_equal(calc_model_cor(x, y, weights = w), r_w, tolerance = 1e-12)
  # Pre-filtered weights (length = number of valid pairs) keep working.
  expect_equal(calc_model_cor(x, y, weights = wv), r_w, tolerance = 1e-12)
})

# ---- calc_bias_ratio: na_rm honoured (ensemble-results-05) ------------------

test_that("calc_bias_ratio honours na_rm = FALSE like calc_model_R2", {
  expect_equal(calc_bias_ratio(c(1, NA, 3), c(2, 2, 2)), 1)
  expect_true(is.na(calc_bias_ratio(c(1, NA, 3), c(2, 2, 2), na_rm = FALSE)))
  expect_true(is.na(calc_model_R2(c(1, NA, 3), c(2, 2, 2), na_rm = FALSE)))
  expect_equal(calc_bias_ratio(c(1, 2, 3), c(2, 4, 6), na_rm = FALSE), 2)
})

# ---- calc_fit_diagnostics: zero / flat predictions (ensemble-results-06) ----

test_that("an all-zero prediction grades bias FAIL, not NA", {
  t <- seq_len(366)
  dts <- as.Date("2021-01-01") + t - 1
  obs <- 50 + 40 * sin(t / 20)
  d <- calc_fit_diagnostics(obs, rep(0, 366), dts)
  expect_equal(d$bias$total, 0)
  expect_identical(unname(d$scorecard["bias"]), "FAIL")
})

test_that("a flat prediction grades variance FAIL even with white residuals", {
  set.seed(11)
  n <- 400
  dts <- as.Date("2021-01-01") + seq_len(n) - 1
  obs <- 100 + stats::rnorm(n, 0, 10)
  d <- calc_fit_diagnostics(obs, rep(100, n), dts)
  expect_equal(d$variance$cv_ratio, 0)
  expect_lt(abs(d$variance$residual_autocorr_lag7), 0.2)   # would PASS on its own
  expect_identical(unname(d$scorecard["variance"]), "FAIL")
})

test_that(".fit_dev maps 0 to Inf and .fit_grade grades Inf as FAIL", {
  expect_identical(MOSAIC:::.fit_dev(0), Inf)
  expect_true(is.na(MOSAIC:::.fit_dev(-1)))
  expect_true(is.na(MOSAIC:::.fit_dev(NA_real_)))
  expect_equal(MOSAIC:::.fit_dev(0.5), 2)
  expect_identical(MOSAIC:::.fit_grade(Inf, 1.2, 2), "FAIL")
  expect_identical(MOSAIC:::.fit_grade(NA_real_, 1.2, 2), "NA")
})

# ---- calc_fit_diagnostics: paired variance metrics (ensemble-results-07) ----

test_that("cv_ratio is computed on the paired finite days only", {
  obs  <- c(10, 20, NA, NA, 30, 40)
  pred <- c(10, 20, 500, 900, 30, 40)
  # Paired days: identical series -> ratio exactly 1. Unpaired pred CV would not be.
  expect_equal(MOSAIC:::.fit_cv_ratio(obs, pred), 1, tolerance = 1e-12)
  obs2 <- c(10, 20, NA, 30); pred2 <- c(20, 40, 7, 60)
  expect_equal(MOSAIC:::.fit_cv_ratio(obs2, pred2), 1, tolerance = 1e-12)
})

test_that("residual autocorrelation keeps NA gaps so the lag stays in days", {
  # Residual r_t = sin(2*pi*t/14): at lag 7 (half period) r_{t+7} = -r_t, so the
  # true lag-7 autocorrelation is strongly negative. Removing a 5-day block and
  # concatenating would shift the lag onto the wrong days.
  n <- 200
  t <- seq_len(n)
  pred <- rep(0, n)
  obs <- sin(2 * pi * t / 14)
  full <- MOSAIC:::.fit_residual_autocorr(obs, pred, lag = 7L)
  obs_gap <- obs; obs_gap[101:105] <- NA
  gap <- MOSAIC:::.fit_residual_autocorr(obs_gap, pred, lag = 7L)
  expected <- stats::acf(obs_gap, lag.max = 7, plot = FALSE,
                         na.action = stats::na.pass)$acf[8]
  expect_equal(gap, expected, tolerance = 1e-12)
  expect_lt(gap, -0.9)
  expect_equal(gap, full, tolerance = 0.05)
})

# ---- run_fit_sandbox: paired aggregation + scored window (ensemble-results-01)

.sbx_config <- function(nT = 200L) {
  d0 <- as.Date("2021-01-01")
  list(date_start = as.character(d0), date_stop = as.character(d0 + nT - 1L),
       location_name = c("A", "B"),
       reported_cases  = rbind(rep(10, nT), rep(10, nT)),
       reported_deaths = rbind(rep(1, nT), rep(1, nT)),
       mu_jt = 0.02, rho = 0.2, rho_deaths = 0.6, chi_epidemic = 0.9)
}

test_that("sandbox aggregation never turns a missing observation into 0", {
  nT <- 200L
  cfg <- .sbx_config(nT)
  cfg$reported_cases[2, 51:150] <- NA          # B unobserved for 100 days
  cfg$reported_cases[, 181:190] <- NA          # nobody observed for 10 days
  runner <- function(config, seed, quiet) list(results = list(
    reported_cases  = rbind(rep(10, nT), rep(30, nT)),   # A perfect, B 3x over
    reported_deaths = rbind(rep(1, nT), rep(1, nT))))
  res <- run_fit_sandbox(cfg, full_metrics = FALSE, .sim_runner = runner)
  # Scored window = days 31..200 minus the 10 unobserved days (160 days):
  # 100 days with only A observed (days 51-150: obs 10, pred 10) and 60 with
  # both (days 31-50, 151-180, 191-200: obs 20, pred 40).
  # Paired bias = (100*10 + 60*40) / (100*10 + 60*20) = 3400/2200 = 17/11.
  # (A zero-filled observed sum against the full predicted aggregate would
  # give (160*40) / 2200 = 32/11.)
  expect_equal(res$metrics$bias_cases, 17 / 11, tolerance = 1e-12)

  # The table is paired too: on B's missing days observed and predicted are
  # both A-only, so the two columns stay comparable.
  pc <- res$predictions[res$predictions$metric == "Suspected Cases", ]
  expect_equal(pc$observed[51:150], rep(10, 100))
  expect_equal(pc$predicted_central[51:150], rep(10, 100))
  expect_identical(pc$n_locations_observed[51:150], rep(1L, 100))
  expect_equal(pc$observed[1:50], rep(20, 50))
  expect_equal(pc$predicted_central[1:50], rep(40, 50))
  expect_identical(pc$n_locations_observed[1:50], rep(2L, 50))
  # A day with no observation at all: observed NA, predicted = full aggregate.
  expect_true(all(is.na(pc$observed[181:190])))
  expect_equal(pc$predicted_central[181:190], rep(40, 10))
  expect_identical(pc$n_locations_observed[181:190], rep(0L, 10))
})

test_that("sandbox table keeps observed on realistic staggered coverage", {
  # Every day has some location unobserved (staggered blocks), as in real
  # configs; the aggregate observed column must still be populated.
  nT <- 120L; nL <- 4L
  obs <- matrix(5, nL, nT)
  for (j in seq_len(nL)) obs[j, ((j - 1L) * 30L + 1L):(j * 30L)] <- NA
  cfg <- .sbx_config(nT)
  cfg$location_name <- LETTERS[seq_len(nL)]
  cfg$reported_cases <- obs
  cfg$reported_deaths <- matrix(1, nL, nT)
  runner <- function(config, seed, quiet) list(results = list(
    reported_cases = matrix(5, nL, nT), reported_deaths = matrix(1, nL, nT)))
  res <- run_fit_sandbox(cfg, full_metrics = FALSE, .sim_runner = runner)
  pc <- res$predictions[res$predictions$metric == "Suspected Cases", ]
  expect_false(anyNA(pc$observed))
  expect_equal(pc$observed, rep(15, nT))
  expect_equal(pc$predicted_central, rep(15, nT))
  expect_equal(res$metrics$bias_cases, 1, tolerance = 1e-12)
})

test_that("sandbox drops the likelihood burn-in and cases warm-up before scoring", {
  nT <- 200L
  cfg <- .sbx_config(nT)
  cfg$location_name <- "A"
  cfg$reported_cases  <- matrix(rep(10, nT), nrow = 1)
  cfg$reported_deaths <- matrix(rep(1, nT), nrow = 1)
  pc <- rep(10, nT); pc[1:3] <- c(5000, 2000, 400)   # IC discharge spike
  pc[31] <- 40                                       # first scored day
  runner <- function(config, seed, quiet) list(results = list(
    reported_cases = matrix(pc, nrow = 1), reported_deaths = matrix(rep(1, nT), nrow = 1)))
  # Default control: burn_in_days = 30 -> scoring starts at day 31.
  res <- run_fit_sandbox(cfg, full_metrics = FALSE, .sim_runner = runner)
  expect_identical(res$metrics$score_idx_cases, 31L)
  expect_identical(res$metrics$score_idx_deaths, 31L)
  expect_equal(res$metrics$bias_cases, (40 + 10 * (nT - 31)) / (10 * (nT - 30)),
               tolerance = 1e-12)
  # The sandbox has no control argument: its signature is unchanged.
  expect_false("control" %in% names(formals(run_fit_sandbox)))
})

# ---- run_fit_sandbox: legacy CFR override (ensemble-results-02) -------------

test_that("on a legacy config the sandbox applies CFR_target and refuses mu_jt", {
  nT <- 60L
  cfg <- .sbx_config(nT)
  cfg$location_name <- "A"
  cfg$reported_cases  <- matrix(rep(10, nT), nrow = 1)
  cfg$reported_deaths <- matrix(rep(1, nT), nrow = 1)
  cfg$CFR_target <- 0.02
  cfg$mu_j_baseline <- 0.001
  # Consume the engine's once-per-session legacy-config warning up front.
  suppressWarnings(MOSAIC:::.mosaic_mu_jt_matrix(cfg, nticks = nT, npatches = 1L))
  seen <- NULL
  runner <- function(config, seed, quiet) {
    seen <<- config
    list(results = list(reported_cases = matrix(rep(10, nT), nrow = 1),
                        reported_deaths = matrix(rep(1, nT), nrow = 1)))
  }
  expect_warning(res <- run_fit_sandbox(cfg, params = list(mu_jt = 0.2),
                                        full_metrics = FALSE, .sim_runner = runner),
                 "override `CFR_target` instead")
  expect_false("mu_jt" %in% res$params_applied$parameter)
  expect_equal(seen$mu_jt, 0.02)

  res2 <- run_fit_sandbox(cfg, params = list(CFR_target = 0.2),
                          full_metrics = FALSE, .sim_runner = runner)
  expect_equal(seen$CFR_target, 0.2)
  expect_equal(res2$params_applied$new[res2$params_applied$parameter == "CFR_target"], 0.2)

  # Other retired fields are still skipped, pointing at CFR_target here.
  expect_warning(run_fit_sandbox(cfg, params = list(mu_j_baseline = 0.1),
                                 full_metrics = FALSE, .sim_runner = runner),
                 "`CFR_target`")
})

test_that("on a current config CFR_target is still refused in favour of mu_jt", {
  nT <- 60L
  cfg <- .sbx_config(nT)
  runner <- function(config, seed, quiet) list(results = list(
    reported_cases = rbind(rep(10, nT), rep(10, nT)),
    reported_deaths = rbind(rep(1, nT), rep(1, nT))))
  expect_warning(run_fit_sandbox(cfg, params = list(CFR_target = 0.2),
                                 full_metrics = FALSE, .sim_runner = runner),
                 "override `mu_jt`")
})
