# The auto-mode stopping rule of MOSAIC 1.3.0: the function-specific ESS of the
# posterior weights, a seed-batch stability criterion, a linear-or-sqrt growth
# model, and robust batch sizing when the ESS is not monotone.

gate_control <- function(...) {
  MOSAIC:::.mosaic_validate_and_merge_control(mosaic_control_defaults(...))
}

gate_pool <- function(n = 2000, shift_batch1 = 0, seed = 1) {
  set.seed(seed)
  r <- data.frame(sim = seq_len(n), likelihood = 0,
                  gamma_1 = runif(n), rho = runif(n))
  b1 <- (r$sim - 1L) %% 10L == 0L
  r$gamma_1[b1] <- r$gamma_1[b1] + shift_batch1
  r
}

auto_state <- function(control, pnames, batch_number) {
  st <- MOSAIC:::.mosaic_init_state(control, pnames, list(mode = "auto", fixed_target = NA_integer_))
  st$batch_number <- batch_number
  st
}

# ---- growth model ---------------------------------------------------------------

test_that("growth fit picks linear for a linear ESS and solves for the target exactly", {
  n <- c(1000, 2000, 3000, 4000, 5000)
  g <- MOSAIC:::.mosaic_ess_growth_fit(n, 5 + 0.05 * n, target = 1000)
  expect_identical(g$model, "linear")
  expect_equal(g$r2, 1)
  expect_equal(g$est_sims, (1000 - 5) / 0.05, tolerance = 1e-8)   # 19,900
  expect_lt(g$r2_sqrt, 1)
})

test_that("growth fit picks linear on a MOZ-like trajectory the old rule sent to sqrt", {
  # Coarsened Kish ESS replayed on MOZ was linear: R^2 1.000 vs sqrt 0.977. The
  # pre-1.3.0 rule took sqrt unless linear beat it by > 0.03.
  set.seed(4)
  n <- seq(1000, 8000, by = 1000)
  ess <- 0.0517 * n + rnorm(length(n), 0, 2)
  g <- MOSAIC:::.mosaic_ess_growth_fit(n, ess, target = 1000)
  expect_identical(g$model, "linear")
  expect_gt(g$r2_linear, 0.999)
  expect_lt(g$r2_linear - g$r2_sqrt, 0.03)        # the old margin would have kept sqrt
  expect_equal(g$est_sims, (1000 - g$intercept) / g$slope)
})

test_that("growth fit picks sqrt for a sqrt ESS; constant and too-short series give no model", {
  n <- c(1000, 4000, 9000, 16000)
  g <- MOSAIC:::.mosaic_ess_growth_fit(n, 20 + 2 * sqrt(n), target = 420)
  expect_identical(g$model, "sqrt")
  expect_equal(g$est_sims, 40000, tolerance = 1e-6)
  flat <- MOSAIC:::.mosaic_ess_growth_fit(n, rep(50, 4), target = 100)
  expect_identical(flat$r2, 0)
  expect_true(is.na(flat$est_sims))
  one <- MOSAIC:::.mosaic_ess_growth_fit(1000, 50, target = 100)
  expect_null(one$fit)
  expect_true(is.na(one$est_sims))
  down <- MOSAIC:::.mosaic_ess_growth_fit(n, c(300, 280, 260, 240), target = 400)
  expect_true(is.na(down$est_sims))             # slope <= 0: unreachable
})

# ---- calc_bookend_batch_size ----------------------------------------------------

test_that("bookend predicts from the linear model on a linear trajectory", {
  n <- c(1000, 2000, 3000, 4000, 5000)
  hist <- lapply(n, function(k) list(total_sims = k, threshold_ess = 0.05 * k))
  res <- suppressWarnings(calc_bookend_batch_size(hist, target_ess = 1000, max_total_sims = 1e6,
                                                  target_r_squared = 0.9))
  expect_identical(res$phase, "predictive")
  expect_identical(res$model, "linear")
  # 1000 / 0.05 = 20,000 total -> 15,000 more, x 1.05 safety
  expect_equal(res$batch_size, ceiling((20000 - 5000) * 1.05))
  # capped by max_total_sims
  res2 <- suppressWarnings(calc_bookend_batch_size(hist, 1000, max_total_sims = 9000, 0.9))
  expect_equal(res2$batch_size, 4000)
})

test_that("bookend returns batch_size 0 on a non-monotone (UGA-like) ESS", {
  hist <- lapply(list(c(1000, 40), c(2000, 70), c(3000, 30), c(4000, 45), c(5000, 25)),
                 function(v) list(total_sims = v[1], threshold_ess = v[2]))
  res <- suppressWarnings(calc_bookend_batch_size(hist, target_ess = 1000, max_total_sims = 1e6,
                                                  target_r_squared = 0.9))
  expect_true(res$phase %in% c("low_confidence", "no_progress"))
  expect_identical(res$batch_size, 0)
})

test_that("predictive batch falls back to batch_size_adaptive and respects the caps", {
  ctl <- gate_control(calibration = list(batch_size_adaptive = 700L, max_batch_predictive = 5000L,
                                         max_simulations_total = 1e6))
  st <- list(phase = "predictive", calibration_done = TRUE, predictive_done = FALSE)
  # non-monotone: no usable model -> the adaptive batch size, never 0 (no stall)
  bad <- lapply(list(c(1000, 40), c(2000, 70), c(3000, 30), c(4000, 45), c(5000, 25)),
                function(v) list(total_sims = v[1], threshold_ess = v[2]))
  d <- suppressWarnings(MOSAIC:::.mosaic_decide_next_batch(st, ctl, bad))
  expect_identical(d$batch_size, 700L)
  # a single measurement (e.g. after a resume re-derived the history)
  d1 <- MOSAIC:::.mosaic_decide_next_batch(st, ctl, bad[1])
  expect_identical(d1$batch_size, 700L)
  # a huge predicted batch is capped by max_batch_predictive
  lin <- lapply(c(1000, 2000, 3000, 4000, 5000),
                function(k) list(total_sims = k, threshold_ess = 0.001 * k))
  d2 <- suppressWarnings(MOSAIC:::.mosaic_decide_next_batch(st, ctl, lin))
  expect_identical(d2$batch_size, 5000L)
  # ESS already at target (batch stability still pending): adaptive batch, not 0
  done <- lapply(c(1000, 2000), function(k) list(total_sims = k, threshold_ess = 2000))
  d3 <- MOSAIC:::.mosaic_decide_next_batch(st, ctl, done)
  expect_identical(d3$batch_size, 700L)
})

# ---- the stopping rule ----------------------------------------------------------

test_that("convergence needs the ESS criterion, batch stability and min_batches", {
  pn <- c("gamma_1", "rho")
  ctl <- gate_control(targets = list(ESS_param = 500, ESS_param_prop = 0.95),
                      calibration = list(min_batches_adaptive = 2L, max_batches_adaptive = 8L))
  # flat likelihood: uniform posterior weights, per-parameter ESS = n = 2000
  ok <- MOSAIC:::.mosaic_ess_check_update_state(auto_state(ctl, pn, 3L), list(), pn, ctl,
                                                n_obs = 1000, results = gate_pool())
  tr <- ok$ess_tracking[[1]]
  expect_equal(tr$threshold_ess, 2000)
  expect_equal(tr$ess_kish, 2000)
  expect_lt(tr$batch_ratio_max, 0.2)
  expect_identical(tr$metric, MOSAIC:::.mosaic_gate_metric_id(ctl))
  expect_true(ok$converged)

  # same ESS, but batch 1 sits 3 units away: unstable -> not converged
  unstable <- MOSAIC:::.mosaic_ess_check_update_state(auto_state(ctl, pn, 3L), list(), pn, ctl,
                                                      n_obs = 1000,
                                                      results = gate_pool(shift_batch1 = 3))
  expect_equal(unstable$ess_tracking[[1]]$threshold_ess, 2000)
  expect_gt(unstable$ess_tracking[[1]]$batch_ratio_max, 0.2)
  expect_false(unstable$converged)

  # both criteria met but batch 1 < min_batches 2 -> continue
  early <- MOSAIC:::.mosaic_ess_check_update_state(auto_state(ctl, pn, 1L), list(), pn, ctl,
                                                   n_obs = 1000, results = gate_pool())
  expect_false(early$converged)

  # ESS short of target -> not converged however stable
  ctl_hi <- gate_control(targets = list(ESS_param = 5000, ESS_param_prop = 0.95),
                         calibration = list(min_batches_adaptive = 2L))
  short <- MOSAIC:::.mosaic_ess_check_update_state(auto_state(ctl_hi, pn, 3L), list(), pn, ctl_hi,
                                                   n_obs = 1000, results = gate_pool())
  expect_false(short$converged)
})

test_that("the stopping rule tracks the posterior: a point mass does not pass", {
  pn <- c("gamma_1", "rho")
  ctl <- gate_control(targets = list(ESS_param = 100, ESS_param_prop = 0.95),
                      calibration = list(min_batches_adaptive = 1L))
  r <- gate_pool()
  r$likelihood <- c(0, rep(-1e7, nrow(r) - 1L))     # one draw holds all the weight
  st <- MOSAIC:::.mosaic_ess_check_update_state(auto_state(ctl, pn, 3L), list(), pn, ctl,
                                                n_obs = 1000, results = r)
  expect_equal(st$ess_tracking[[1]]$threshold_ess, 1)
  expect_false(st$converged)
})

test_that("the calibration phase sizes from the better of linear and sqrt", {
  pn <- c("gamma_1", "rho")
  ctl <- gate_control(targets = list(ESS_param = 5000, ESS_param_prop = 0.95),
                      calibration = list(min_batches_adaptive = 5L, max_batches_adaptive = 8L,
                                         batch_size_adaptive = 400L, target_r2_adaptive = 0.9))
  st <- auto_state(ctl, pn, 5L)
  gid <- MOSAIC:::.mosaic_gate_metric_id(ctl)
  # four earlier measurements on ESS = n; the new one (n = 2000, uniform weights) is ESS 2000
  st$ess_tracking <- lapply(c(400, 800, 1200, 1600), function(k)
    list(batch = k / 400, total_sims = k, threshold_ess = k, min_ess = k, median_ess = k,
         max_ess = k, metric = gid))
  st2 <- MOSAIC:::.mosaic_ess_check_update_state(st, list(), pn, ctl, n_obs = 1000,
                                                 results = gate_pool())
  g <- MOSAIC:::.mosaic_ess_growth_fit(c(400, 800, 1200, 1600, 2000), c(400, 800, 1200, 1600, 2000), 5000)
  expect_identical(g$model, "linear")
  expect_equal(st2$r2_ess, 1)
  # R^2 met with 5 points; linear puts the target at n = 5000, 3000 away (> one batch)
  expect_true(st2$calibration_done)
  expect_false(st2$converged)
  src <- gsub("\\s+", " ", paste(deparse(MOSAIC:::.mosaic_ess_check_update_state), collapse = " "))
  expect_true(grepl(".mosaic_ess_growth_fit(sims_track, thresh_track, target_ess)", src, fixed = TRUE))
  expect_false(grepl("threshold_ess ~ sqrt_sims", src, fixed = TRUE))
})

test_that("the predictive phase is not closed by ESS alone while the posterior is unstable", {
  pn <- c("gamma_1", "rho")
  ctl <- gate_control(targets = list(ESS_param = 500, ESS_param_prop = 0.95),
                      calibration = list(min_batches_adaptive = 2L, max_batches_predictive = 5L))
  st <- auto_state(ctl, pn, 9L)
  st$phase <- "predictive"; st$calibration_done <- TRUE; st$phase_batch_count <- 1L
  st2 <- MOSAIC:::.mosaic_ess_check_update_state(st, list(), pn, ctl, n_obs = 1000,
                                                 results = gate_pool(shift_batch1 = 3))
  expect_false(st2$predictive_done)
  expect_false(st2$converged)
  st3 <- MOSAIC:::.mosaic_ess_check_update_state(st, list(), pn, ctl, n_obs = 1000,
                                                 results = gate_pool())
  expect_true(st3$predictive_done)
  expect_true(st3$converged)
})

test_that("saturated legacy: the rule scores the tier-subset weights, capped near the subset size", {
  pn <- c("gamma_1", "rho")
  ctl <- gate_control(targets = list(best_subset_weighting = "saturated", ESS_param = 100,
                                     ESS_param_prop = 0.95, min_best_subset = 30,
                                     max_best_subset = 300),
                      calibration = list(min_batches_adaptive = 1L))
  r <- gate_pool()
  r$likelihood <- -50 * (r$gamma_1 - 0.5)^2 * 100 + rnorm(nrow(r), 0, 1)
  w <- MOSAIC:::.mosaic_gate_weights(r, n_obs = 1000, control = ctl)
  expect_equal(sum(w), 1)
  expect_lte(sum(w > 0), 300)                              # the tier subset only
  st <- MOSAIC:::.mosaic_ess_check_update_state(auto_state(ctl, pn, 3L), list(), pn, ctl,
                                                n_obs = 1000, results = r)
  expect_lte(st$ess_tracking[[1]]$ess_kish, 300)
  expect_match(st$ess_tracking[[1]]$metric, "saturated$")
})

test_that("the ESS threshold is the order statistic the convergence count implies", {
  # 39 parameters, prop = 40/41: ceiling(0.9756 * 39) = 39 -> every parameter, so
  # the threshold is the minimum. An interpolated quantile (type 7) sat between
  # the two smallest and could clear 1000 while one parameter had not.
  ess <- c(990, 1010, seq(1100, 4800, length.out = 37))
  prop <- 0.975609756097561
  expect_equal(MOSAIC:::.mosaic_gate_threshold(ess, prop), 990)
  expect_gt(as.numeric(stats::quantile(ess, 1 - prop)), 1000)       # the old, inconsistent value
  # 20 parameters at prop 0.95: 19 must pass -> the 2nd smallest
  e20 <- c(5, 50, 120, seq(200, 900, length.out = 17))
  expect_equal(MOSAIC:::.mosaic_gate_threshold(e20, 0.95), 50)
  # parameters without an ESS (NA, or missing from the table) count as 0
  expect_equal(MOSAIC:::.mosaic_gate_threshold(c(NA, e20[-1]), 0.95), 50)
  expect_equal(MOSAIC:::.mosaic_gate_threshold(e20[-1], 0.95, n_total = 20), 50)
  # threshold >= target  <=>  the count rule passes, over random cases
  set.seed(8)
  for (i in 1:200) {
    n <- sample(5:60, 1); p <- runif(1, 0.5, 1); e <- rexp(n) * 1000; tgt <- 700
    expect_identical(MOSAIC:::.mosaic_gate_threshold(e, p) >= tgt,
                     sum(e >= tgt) >= ceiling(p * n - 1e-9))
  }
})

test_that("calibration honours max_batches_adaptive with fewer than min_batches measurements (after a resume reset)", {
  pn <- c("gamma_1", "rho")
  ctl <- gate_control(targets = list(ESS_param = 1e6, ESS_param_prop = 0.95),
                      calibration = list(min_batches_adaptive = 3L, max_batches_adaptive = 8L))
  # history discarded on resume: only this one measurement, at batch 8 = max
  st <- MOSAIC:::.mosaic_ess_check_update_state(auto_state(ctl, pn, 8L), list(), pn, ctl,
                                                n_obs = 1000, results = gate_pool())
  expect_length(st$ess_tracking, 1L)
  expect_true(st$calibration_done)
  # below max with one measurement: calibration continues
  st2 <- MOSAIC:::.mosaic_ess_check_update_state(auto_state(ctl, pn, 6L), list(), pn, ctl,
                                                 n_obs = 1000, results = gate_pool())
  expect_false(st2$calibration_done)
})
