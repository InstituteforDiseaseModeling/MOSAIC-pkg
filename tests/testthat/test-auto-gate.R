# The auto-mode stopping rule of MOSAIC 1.4.0: the convergence status
# (calc_convergence_status(), through calc_weight_health()) on every draw so
# far, the minimum run length, the ESS projection N x target / ESS_all guarded
# by the growth slope, the two-consecutive-greens stop, and the convergence history.

gate_control <- function(...) {
  suppressWarnings(MOSAIC:::.mosaic_validate_and_merge_control(mosaic_control_defaults(...)))
}

# A pool whose likelihood informs gamma_1 and rho (the untempered weights are
# concentrated, so it is not "flat" under the round-1 informativeness check);
# at n_obs = 1e5 (eta = 1e-4) the coarsened weights are mild: red at 300
# draws, yellow at 1,400, green from about 3,800.
gate_pool <- function(n = 4000, seed = 1) {
  set.seed(seed)
  r <- data.frame(sim = seq_len(n), gamma_1 = runif(n), rho = runif(n))
  r$likelihood <- -2e5 * (r$gamma_1 - 0.4)^2 - 5e3 * (r$rho - 0.5)^2
  r
}

auto_state <- function(control, pnames, batch_number, total = NA_integer_) {
  st <- MOSAIC:::.mosaic_init_state(control, pnames, list(mode = "auto", fixed_target = NA_integer_))
  st$batch_number <- batch_number
  st$total_sims_run <- total
  st
}

# ---- .mosaic_projected_batch_size: projection, else geometric growth -------------

pbs <- function(...) MOSAIC:::.mosaic_projected_batch_size(...)

test_that("the projection is N x target / ESS_all with a safety factor, floored and capped", {
  res <- pbs(n_draws = 5000, ess_all = 250, ess_all_target = 1000,
             slope = 1, batch_size_min = 500, batch_size_max = 1e6)
  expect_identical(res$method, "projection")
  expect_equal(res$projected_total, 20000)
  expect_identical(res$batch_size, as.integer(ceiling(1.1 * 20000) - 5000))   # 17,000
  # capped by max_batch_predictive, then by the budget left
  expect_identical(pbs(5000, 250, 1000, 1, 500, 10000)$batch_size, 10000L)
  expect_identical(pbs(5000, 250, 1000, 1, 500, 10000, remaining = 3000)$batch_size, 3000L)
  near <- pbs(9900, 990, 1000, 1, 500, 10000)
  expect_identical(near$method, "projection")
  expect_identical(near$batch_size, 1100L)          # ceiling(1.1 x 10,000) - 9,900
  expect_gte(pbs(9999, 999.9, 1000, 1, 500, 10000)$batch_size, 500L)   # floor
  # k-hat does not gate the projection (red team round 1, D3): the slope does
  expect_false("khat" %in% names(formals(MOSAIC:::.mosaic_projected_batch_size)))
  expect_identical(pbs(5000, 250, 1000, 1, 500)$method, "projection")
})

test_that("an invalid projection grows the batch geometrically (a quarter of N), clamped", {
  g <- function(n, ...) pbs(n_draws = n, ess_all = 0.02 * n, ess_all_target = 1000,
                            batch_size_min = 500, batch_size_max = 10000, ...)
  k <- g(8000, slope = 0.5)
  expect_identical(k$method, "geometric"); expect_identical(k$batch_size, 2000L)   # 0.25 x 8,000
  expect_match(k$reason, "growth slope 0.50 < 0.8")
  expect_identical(g(8000, slope = 0.79)$method, "geometric")
  expect_match(g(8000, slope = NA)$reason, "fewer than three checks")
  expect_identical(g(1000, slope = 0.1)$batch_size, 500L)                # floor
  expect_identical(g(100000, slope = 0.1)$batch_size, 10000L)            # cap
  expect_identical(g(100000, slope = 0.1, remaining = 1234)$batch_size, 1234L)
  # ESS_all already at its target: geometric growth while MCE / k-hat catch up
  t <- pbs(8000, 1200, 1000, 1, batch_size_min = 500, batch_size_max = 10000)
  expect_identical(t$method, "target_met"); expect_identical(t$batch_size, 2000L)
  expect_identical(g(8000, slope = 1, remaining = 0)$batch_size, 0L)
  expect_error(pbs(5000, 250, 1000, batch_size_min = 0), "batch_size_min")
  expect_error(pbs(5000, 250, 1000, batch_size_min = 500, batch_size_max = 100), "batch_size_max")
})

test_that("a heavy-tailed run reaches a 150k cap in about 20 checks, not 290", {
  ctl <- gate_control(calibration = list(batch_size_adaptive = 500L, min_batches_adaptive = 5L,
                                         min_draws = 5000L, max_batch_predictive = 10000L,
                                         max_simulations_total = 150000L))
  st <- auto_state(ctl, "gamma_1", 0L, 0L)
  checks <- 0L
  while (st$total_sims_run < 150000L) {
    d <- MOSAIC:::.mosaic_next_batch_size(st, ctl)
    st$total_sims_run <- st$total_sims_run + d$batch_size
    st$batch_number <- st$batch_number + 1L
    checks <- checks + 1L
    # ESS growth stalled for the whole run: the projection never applies
    st$convergence_history <- c(st$convergence_history, list(list(
      batch = st$batch_number, n = st$total_sims_run, ess_all = 0.002 * st$total_sims_run,
      ess_best = 0, mce_p90 = 0.3, khat = 0.8, slope = 0.5, status = "red")))
  }
  expect_identical(st$total_sims_run, 150000L)
  expect_gte(checks, 15L)
  expect_lte(checks, 35L)            # 10 minimum-run batches + ~20 geometric
})

test_that("calc_bookend_batch_size keeps its pre-1.4.0 behaviour, deprecated, and run_MOSAIC never calls it", {
  rm(list = ls(MOSAIC:::.mosaic_once), envir = MOSAIC:::.mosaic_once)
  n <- c(1000, 2000, 3000, 4000, 5000)
  hist <- lapply(n, function(k) list(total_sims = k, threshold_ess = 0.05 * k))
  expect_warning(res <- calc_bookend_batch_size(hist, target_ess = 1000, max_total_sims = 1e6,
                                                target_r_squared = 0.9), "deprecated since MOSAIC 1.4.0")
  expect_identical(res$phase, "predictive")
  expect_identical(res$model, "linear")
  expect_equal(res$batch_size, ceiling((20000 - 5000) * 1.05))
  expect_identical(names(formals(calc_bookend_batch_size)),
                   c("ess_history", "target_ess", "max_total_sims", "target_r_squared"))
  # no run_MOSAIC path uses the deprecated export
  for (f in list(MOSAIC::run_MOSAIC, MOSAIC:::.mosaic_next_batch_size,
                 MOSAIC:::.mosaic_convergence_check_update_state, MOSAIC:::.mosaic_reconstruct_state))
    expect_false(grepl("calc_bookend_batch_size", paste(deparse(f), collapse = " "), fixed = TRUE))
})

# ---- next-batch sizing in the run loop --------------------------------------------

test_that("batches are batch_size_adaptive until both minimum run lengths are met, then projected", {
  ctl <- gate_control(calibration = list(batch_size_adaptive = 500L, min_batches_adaptive = 5L,
                                         min_draws = 5000L, max_batch_predictive = 8000L,
                                         max_simulations_total = 150000L))
  h <- function(n, ess, slope = 1, khat = 0.1)
    list(batch = 1L, n = n, ess_all = ess, ess_best = 0.2 * ess, mce_p90 = 0.1, khat = khat,
         slope = slope, status = "red")
  st <- auto_state(ctl, "gamma_1", 0L, 0L)
  d0 <- MOSAIC:::.mosaic_next_batch_size(st, ctl)
  expect_identical(d0$method, "minimum_run"); expect_identical(d0$batch_size, 500L)
  st <- auto_state(ctl, "gamma_1", 6L, 3000L); st$convergence_history <- list(h(3000, 150))
  expect_identical(MOSAIC:::.mosaic_next_batch_size(st, ctl)$method, "minimum_run")   # < min_draws
  st <- auto_state(ctl, "gamma_1", 4L, 6000L); st$convergence_history <- list(h(6000, 300))
  expect_identical(MOSAIC:::.mosaic_next_batch_size(st, ctl)$method, "minimum_run")   # < min_batches
  st <- auto_state(ctl, "gamma_1", 10L, 5000L); st$convergence_history <- list(h(5000, 250))
  d <- MOSAIC:::.mosaic_next_batch_size(st, ctl)
  expect_identical(d$method, "projection"); expect_identical(d$batch_size, 8000L)    # capped
  st$total_sims_run <- 149000L
  expect_identical(MOSAIC:::.mosaic_next_batch_size(st, ctl)$batch_size, 1000L)       # budget left
  st$total_sims_run <- 5000L; st$convergence_history <- list(h(5000, 250, khat = 0.6, slope = 0.5))
  d6 <- MOSAIC:::.mosaic_next_batch_size(st, ctl)
  expect_identical(d6$method, "geometric"); expect_identical(d6$batch_size, 1250L)   # 0.25 x 5,000
  # k-hat >= 0.5 with a healthy slope still projects (D3)
  st$convergence_history <- list(h(5000, 250, khat = 0.6, slope = 1))
  expect_identical(MOSAIC:::.mosaic_next_batch_size(st, ctl)$method, "projection")
  # projected valid draws are converted to simulations at the success rate (D7):
  # 5,000 valid of 10,000 attempted doubles a 6,000-draw projection batch
  ctl2 <- ctl; ctl2$calibration$max_batch_predictive <- 1e5L
  st$convergence_history <- list(h(5000, 500, slope = 1)); st$total_sims_run <- 10000L
  d7 <- MOSAIC:::.mosaic_next_batch_size(st, ctl2)            # ceiling(1.1 x 10,000) - 5,000 = 6,000 draws
  expect_identical(d7$batch_size, 12000L)
})

test_that("in parallel an adaptive batch is rounded up to fill the cluster, never past the budget", {
  r <- MOSAIC:::.mosaic_round_batch_to_cluster
  # sequential: unchanged, the dispatcher resolves the shard
  expect_identical(r(1000L, 100L, 1L), list(batch_size = 1000L, shard_batch = NULL))
  # 168 workers, shards of up to 100 (clamped to 4 tasks per worker): 10,000 -> shard 14,
  # unit 2,352, rounded to 11,760 = 5 full waves
  x <- r(10000L, 100L, 168L)
  expect_identical(x$shard_batch, 14L)
  expect_identical(x$batch_size, 11760L)
  expect_identical(x$batch_size %% (168L * x$shard_batch), 0L)
  # a 500-draw minimum-run batch: shard 1, rounded to 504
  expect_identical(r(500L, 100L, 168L), list(batch_size = 504L, shard_batch = 1L))
  # never beyond the budget left (then unrounded)
  expect_identical(r(10000L, 100L, 168L, remaining = 11000)$batch_size, 10000L)
  expect_null(r(10000L, 100L, 168L, remaining = 11000)$shard_batch)
  expect_identical(r(10000L, 100L, 168L, remaining = 4000)$batch_size, 4000L)
  # never above max_batch_predictive (CN2): 11,760 > 10,000 rounds down to 4 waves of 2,352
  y <- r(10000L, 100L, 168L, batch_size_max = 10000)
  expect_identical(y$batch_size, 9408L); expect_identical(y$shard_batch, 14L)
  expect_identical(r(1500L, 100L, 168L, batch_size_max = 1000)$batch_size, 672L)   # 2 waves of 336
  # less than one wave under the cap: the unrounded size, capped
  expect_identical(r(1500L, 100L, 168L, batch_size_max = 300)$batch_size, 300L)
  # wired into .mosaic_next_batch_size()
  ctl <- gate_control(calibration = list(batch_size_adaptive = 500L, min_batches_adaptive = 5L,
                                         min_draws = 5000L, max_simulations_total = 150000L))
  st <- auto_state(ctl, "gamma_1", 0L, 0L)
  d <- MOSAIC:::.mosaic_next_batch_size(st, ctl, n_workers = 168L)
  expect_identical(d$batch_size, 504L); expect_identical(d$shard_batch, 1L)
  expect_match(d$reason, "rounded up from 500 to 504 to fill 168 workers")
  st$total_sims_run <- 149800L
  expect_identical(MOSAIC:::.mosaic_next_batch_size(st, ctl, n_workers = 168L)$batch_size, 200L)
})

# ---- the stopping rule ----------------------------------------------------------

test_that("the gate stops at GREEN on two consecutive checks once both minimum run lengths are met", {
  pn <- c("gamma_1", "rho")
  ctl <- gate_control(calibration = list(min_batches_adaptive = 3L, min_draws = 3000L))
  pool <- gate_pool()
  chk <- function(st, k, ctl, n) MOSAIC:::.mosaic_convergence_check_update_state(
    within(st, { batch_number <- k; total_sims_run <- n }), list(), pn, ctl, n_obs = 1e5,
    results = pool[seq_len(n), ], log = FALSE)
  # one green check is not enough (D5) ...
  one <- chk(auto_state(ctl, pn, 0L, 0L), 3L, ctl, 4000L)
  expect_identical(one$last_health$status, "green")
  expect_false(one$converged)
  # ... two consecutive green checks stop the run
  green <- chk(chk(auto_state(ctl, pn, 0L, 0L), 2L, ctl, 3800L), 3L, ctl, 4000L)
  expect_true(green$converged)
  expect_true(all(vapply(green$convergence_history, function(e) e$sampling_green, TRUE)))
  # a re-check at the same draw count counts once
  same <- chk(chk(auto_state(ctl, pn, 0L, 0L), 3L, ctl, 4000L), 3L, ctl, 4000L)
  expect_false(same$converged)
  early <- chk(chk(auto_state(ctl, pn, 0L, 0L), 1L, ctl, 3800L), 2L, ctl, 4000L)
  expect_identical(early$last_health$status, "green")
  expect_false(early$converged)                                  # batch 2 < 3
  ctl2 <- gate_control(calibration = list(min_batches_adaptive = 3L, min_draws = 5000L))
  short <- chk(chk(auto_state(ctl2, pn, 0L, 0L), 3L, ctl2, 3800L), 4L, ctl2, 4000L)
  expect_false(short$converged)                                  # 4,000 valid draws < 5,000
  # the status is calc_convergence_status() on the best-fit selection
  wh <- green$last_health
  expect_identical(wh$status, MOSAIC:::.mosaic_health_status(wh)$status)
})

test_that("a red pool never stops, and YELLOW does not stop either", {
  pn <- c("gamma_1", "rho")
  ctl <- gate_control(calibration = list(min_batches_adaptive = 1L, min_draws = 0L))
  small <- gate_pool(n = 300)
  red <- MOSAIC:::.mosaic_convergence_check_update_state(auto_state(ctl, pn, 5L, 300L), list(), pn, ctl,
                                                         n_obs = 1e5, results = small, log = FALSE)
  expect_identical(red$last_health$status, "red")
  expect_false(red$converged)
  ctl_y <- gate_control(calibration = list(min_batches_adaptive = 1L, min_draws = 0L),
                        targets = list(ESS_best = 300))
  pool <- gate_pool(n = 1400)
  y <- MOSAIC:::.mosaic_convergence_check_update_state(auto_state(ctl_y, pn, 5L, 1400L), list(), pn, ctl_y,
                                                       n_obs = 1e5, results = pool, log = FALSE)
  expect_identical(y$last_health$convergence$checks$colour[1], "yellow")
  expect_false(y$converged)
})

test_that("every check appends one convergence_history entry with the run_state.json fields", {
  pn <- c("gamma_1", "rho")
  ctl <- gate_control(calibration = list(min_batches_adaptive = 1L, min_draws = 0L))
  st <- auto_state(ctl, pn, 1L, 1000L)
  pool <- gate_pool()
  for (k in 1:4) {
    st$batch_number <- k
    st$total_sims_run <- 1000L * k
    st <- MOSAIC:::.mosaic_convergence_check_update_state(st, list(), pn, ctl, n_obs = 1e5,
                                                          results = pool[seq_len(1000 * k), ], log = FALSE)
  }
  expect_length(st$convergence_history, 4L)
  e <- st$convergence_history[[4]]
  expect_identical(names(e), c("batch", "n", "ess_all", "ess_best", "mce_p90", "khat", "slope", "status",
                               "sampling_green"))
  expect_identical(e$batch, 4L); expect_identical(e$n, 4000L)
  expect_true(is.na(st$convergence_history[[1]]$slope))          # fewer than three checks
  expect_true(is.finite(e$slope))
  n <- vapply(st$convergence_history, `[[`, numeric(1), "n")
  ess <- vapply(st$convergence_history, `[[`, numeric(1), "ess_all")
  expect_equal(e$slope, MOSAIC:::.mosaic_gate_slope(n, ess))
  # run_state.json persists it, NA as null
  f <- tempfile(fileext = ".json")
  MOSAIC:::.mosaic_save_state(st, f)
  rs <- jsonlite::read_json(f)
  expect_identical(rs$schema_version, 2L)
  expect_length(rs$convergence_history, 4L)
  expect_null(rs$convergence_history[[1]]$slope)
  expect_identical(rs$gate_metric, MOSAIC:::.mosaic_gate_metric_id(ctl))
  expect_false(any(c("ess_threshold", "ess_regression_r2", "batch_ratio_max") %in% names(rs)))
})

test_that("the budget-cap flag is raised at max_simulations_total", {
  pn <- c("gamma_1", "rho")
  ctl <- gate_control(calibration = list(min_batches_adaptive = 1L, min_draws = 0L,
                                         max_simulations_total = 300L, batch_size_adaptive = 100L))
  st <- MOSAIC:::.mosaic_convergence_check_update_state(auto_state(ctl, pn, 3L, 300L), list(), pn, ctl,
                                                        n_obs = 1e5, results = gate_pool(n = 300), log = FALSE)
  expect_true("budget_cap_reached" %in% st$last_health$convergence$flags)
})

# ---- helpers --------------------------------------------------------------------

test_that("the gate metric id changes with what the rule measures", {
  a <- MOSAIC:::.mosaic_gate_metric_id(gate_control())
  expect_match(a, "^convergence_status/v1.4.0/alpha=10/ratio=0.2/ess_best=200/members=30-1000/bands=")
  expect_false(identical(a, MOSAIC:::.mosaic_gate_metric_id(gate_control(targets = list(coarsening_alpha = 5)))))
  expect_false(identical(a, MOSAIC:::.mosaic_gate_metric_id(gate_control(targets = list(best_subset_ratio = 0.3)))))
  expect_false(identical(a, MOSAIC:::.mosaic_gate_metric_id(
    gate_control(targets = list(status_thresholds = list(mce_green = 0.08))))))
  st <- MOSAIC:::.mosaic_init_state(gate_control(), "x", list(mode = "auto", fixed_target = NA))
  expect_identical(st$gate_metric, a)
  expect_identical(st$phase, "adaptive")
})

test_that("the growth slope uses the latter part of the run (at least three points)", {
  n <- c(1000, 2000, 4000, 8000, 16000)
  expect_equal(MOSAIC:::.mosaic_gate_slope(n, 0.05 * n), 1)
  expect_equal(MOSAIC:::.mosaic_gate_slope(n, 3 * sqrt(n)), 0.5)
  expect_true(is.na(MOSAIC:::.mosaic_gate_slope(c(1000, 2000), c(50, 100))))
  # early growth does not mask a late stall
  ess <- c(50, 100, 200, 210, 215)
  expect_lt(MOSAIC:::.mosaic_gate_slope(n, ess), 0.2)
})

test_that("the gate pool appends only the shards it has not read", {
  skip_if_not_installed("arrow")
  d <- tempfile(); dir.create(d)
  w <- function(ids) arrow::write_parquet(
    data.frame(sim = ids, likelihood = -ids, gamma_1 = ids / 10, extra = 1),
    file.path(d, sprintf("sim_%07d-%07d.parquet", min(ids), max(ids))))
  w(1:10); w(11:20)
  p1 <- MOSAIC:::.mosaic_gate_pool_update(NULL, d, "gamma_1")
  expect_identical(nrow(p1$data), 20L)
  expect_identical(names(p1$data), c("sim", "likelihood", "gamma_1"))
  expect_length(p1$files, 2L)
  w(21:30)
  p2 <- MOSAIC:::.mosaic_gate_pool_update(p1, d, "gamma_1")
  expect_identical(p2$data$sim, 1:30)
  expect_length(p2$files, 3L)
  # an unchanged directory returns the pool as is
  expect_identical(MOSAIC:::.mosaic_gate_pool_update(p2, d, "gamma_1"), p2)
  # the check scores the pool exactly as a full re-read would
  full <- MOSAIC:::.mosaic_load_and_combine_results(d, method = "streaming", verbose = FALSE)
  expect_equal(p2$data$likelihood, full$likelihood[order(full$sim)])
})

test_that("the ESS replay is log-spaced, at most 40 points, and ends on the full pool", {
  pool <- gate_pool(n = 5000)
  eta <- 1e-3
  rp <- MOSAIC:::.mosaic_ess_replay(pool$likelihood, pool$sim, eta)
  expect_lte(nrow(rp), 40L)
  expect_identical(names(rp), c("n", "ess_all", "best_gap", "top100_gap"))
  expect_true(all(diff(rp$n) > 0))
  expect_identical(max(rp$n), 5000L)
  expect_equal(rp$ess_all[nrow(rp)], calc_model_ess(MOSAIC:::.mosaic_tempered_weights(pool$likelihood, eta), "kish"))
  expect_equal(rp$best_gap[nrow(rp)], 0)
  expect_true(all(rp$best_gap <= 0))
  # draw order is simulation order
  shuffled <- pool[sample(nrow(pool)), ]
  expect_equal(MOSAIC:::.mosaic_ess_replay(shuffled$likelihood, shuffled$sim, eta), rp)
  expect_identical(nrow(MOSAIC:::.mosaic_ess_replay(1, 1, eta)), 0L)
})
