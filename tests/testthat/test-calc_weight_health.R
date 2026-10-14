# calc_weight_health(): health of the declared (coarsened) posterior, the
# supported resolution eta*, and the weights actually used. Hand-computed
# fixtures; nothing here may change a posterior weight.

has_khat_name <- function(x) {
  nms <- names(unlist(x))
  any(grepl("khat", nms, ignore.case = TRUE))
}

test_that("coarsened ESS matches a hand-computed tempered fixture", {
  # eta_ref = alpha / n_obs = 5 / 10 = 0.5, so w ∝ exp(0.5 * (0, -1, -2, -3)).
  # Kish = (sum w)^2 / sum w^2 = (1 + e^-.5 + e^-1 + e^-1.5)^2 / (1 + e^-1 + e^-2 + e^-3).
  ll <- c(0, -1, -2, -3)
  wh <- calc_weight_health(ll, weights = rep(0.25, 4), params = data.frame(x = c(1, 2, 3, 4)),
                           n_obs = 10, coarsening_alpha = 5, ess_target = 2, n_batches = 2L)
  expect_equal(wh$coarsened$eta_ref, 0.5)
  expect_equal(wh$coarsened$ess_kish, 3.1095799253566, tolerance = 1e-12)
  expect_equal(wh$coarsened$ess_prop, 3.1095799253566 / 4, tolerance = 1e-12)
  expect_identical(wh$coarsened$n_obs, 10)
})

test_that("seed-batch ratio matches a hand-computed fixture", {
  # Uniform weights, x = 1..4, two batches by (sim - 1) mod 2: {1, 3} and {2, 4}.
  # Batch means 2 and 3 -> SD sqrt(0.5); posterior SD sqrt(1.25); ratio sqrt(0.4).
  wh <- calc_weight_health(rep(0, 4), weights = rep(1, 4), params = data.frame(x = 1:4),
                           n_obs = 10, ess_target = 2, n_batches = 2L)
  expect_equal(wh$weights_used$batch_ratio_max, 0.632455532033676, tolerance = 1e-12)
  expect_equal(wh$weights_used$batch_ratio_median, 0.632455532033676, tolerance = 1e-12)
  expect_equal(wh$weights_used$ess_kish, 4)
  expect_equal(wh$weights_used$ess_perplexity, 4)
})

test_that("a point mass in the weights used gives ESS 1, max share 1 and an infinite batch ratio", {
  set.seed(2)
  n <- 500
  wu <- numeric(n); wu[37] <- 1
  wh <- calc_weight_health(rnorm(n), weights = wu, params = data.frame(a = runif(n), b = rnorm(n)),
                           n_obs = 100)
  expect_equal(wh$weights_used$ess_kish, 1)
  expect_equal(wh$weights_used$ess_perplexity, 1)
  expect_equal(wh$weights_used$max_share, 1)
  expect_identical(wh$weights_used$n_nonzero, 1L)
  expect_identical(wh$weights_used$batch_ratio_max, Inf)
})

test_that("uniform weights used give ESS = N and a small batch ratio", {
  set.seed(3)
  n <- 4000
  wh <- calc_weight_health(rnorm(n), weights = rep(1, n), params = data.frame(a = runif(n)),
                           n_obs = 100)
  expect_equal(wh$weights_used$ess_kish, n)
  expect_equal(wh$weights_used$max_share, 1 / n)
  expect_lt(wh$weights_used$batch_ratio_max, 0.1)
})

test_that("status is ok only when the declared posterior meets the ESS and batch targets", {
  set.seed(4)
  n <- 20000
  th <- runif(n)
  ll <- -4000 * (th - 0.5)^2
  P <- data.frame(theta = th)
  # Flat declared posterior (small alpha): ESS large, batch ratio small -> ok.
  ok <- calc_weight_health(ll, rep(1, n), P, n_obs = 4000, coarsening_alpha = 4, ess_target = 1000)
  expect_identical(ok$status, "ok")
  expect_gte(ok$coarsened$ess_kish, 1000)
  expect_lt(ok$coarsened$batch_ratio_max, 0.2)
  # Sharp declared posterior (alpha = n_obs, i.e. eta_ref = 1): too few draws -> warn.
  bad <- calc_weight_health(ll, rep(1, n), P, n_obs = 4000, coarsening_alpha = 4000, ess_target = 1000)
  expect_identical(bad$status, "warn")
  expect_match(bad$reasons, "draws too few for the declared posterior")
  # A batch-only failure: the ESS target met, the batch ratio not.
  bb <- calc_weight_health(ll, rep(1, n), P, n_obs = 4000, coarsening_alpha = 4,
                           ess_target = 10, batch_max = 1e-4)
  expect_identical(bb$status, "warn")
  expect_match(bb$reasons, "seed batches")
  expect_false(grepl("draws too few", bb$reasons))
})

test_that("the eta = 1 concentration never enters the status", {
  set.seed(5)
  n <- 20000
  th <- runif(n)
  ll <- -1e9 * (th - 0.5)^2           # eta = 1 collapses onto one draw
  wh <- calc_weight_health(ll, rep(1, n), data.frame(theta = th), n_obs = 1e9,
                           coarsening_alpha = 2, ess_target = 1000)
  expect_lt(wh$likelihood_concentration$ess_is, 2)
  expect_identical(wh$status, "ok")
  expect_match(wh$likelihood_concentration$note, "not a fit or convergence diagnostic")
})

test_that("eta* is the largest eta meeting the ESS target, and n_eff = n_obs * eta*", {
  set.seed(6)
  n <- 20000
  th <- runif(n)
  ll <- -2e4 * (th - 0.3)^2
  wh <- calc_weight_health(ll, rep(1, n), data.frame(theta = th), n_obs = 2e4,
                           ess_target = 500, batch_max = 10)   # batch criterion slack
  es <- wh$eta_star
  expect_true(is.finite(es$eta) && es$eta < 1)
  expect_gte(es$ess_kish, 500)
  w_up <- MOSAIC:::.mosaic_tempered_weights(ll, es$eta * 1.01)
  expect_lt(calc_model_ess(w_up, method = "kish"), 500)
  expect_identical(es$binding, "ess")
  expect_equal(es$n_eff, 2e4 * es$eta)
  expect_match(es$note, "No Monte Carlo-noise ceiling")
})

test_that("eta* is 1 when the raw likelihood already meets the targets, NA when no eta can", {
  set.seed(7)
  n <- 5000
  flat <- calc_weight_health(rnorm(n, sd = 0.01), rep(1, n), data.frame(a = runif(n)),
                             n_obs = 100, ess_target = 100)
  expect_identical(flat$eta_star$eta, 1)
  few <- calc_weight_health(rnorm(50), rep(1, 50), data.frame(a = runif(50)),
                            n_obs = 100, ess_target = 1000)
  expect_true(is.na(few$eta_star$eta))
  expect_true(is.na(few$eta_star$n_eff))
})

test_that("no k-hat appears in the result or in the summary fields", {
  set.seed(8)
  n <- 3000
  wh <- calc_weight_health(-abs(rnorm(n, sd = 50)), rep(1, n), data.frame(a = runif(n)),
                           n_obs = 1000)
  expect_false(has_khat_name(wh))
  f <- MOSAIC:::.mosaic_weight_health_fields(wh)
  expect_false(any(grepl("khat|ess_is", names(f))))
  expect_true(all(c("ess_coarsened", "ess_coarsened_prop", "coarsening_alpha", "eta_ref",
                    "n_obs_scored", "batch_ratio_coarsened_max", "batch_ratio_coarsened_median",
                    "eta_star", "eta_star_n_eff", "eta_star_ess", "eta_star_batch_ratio_max",
                    "weights_used_ess_kish", "weights_used_ess_perplexity", "weights_used_max_share",
                    "weights_used_n", "weights_used_batch_ratio_max", "weights_used_batch_ratio_median",
                    "weight_health_status", "weight_health_reasons",
                    "likelihood_concentration_note") %in% names(f)))
  # A missing weight-health block still yields every field, as NA.
  f0 <- MOSAIC:::.mosaic_weight_health_fields(NULL)
  expect_identical(names(f0), names(f))
  expect_true(is.na(f0$ess_coarsened) && is.na(f0$weight_health_status))
})

test_that("calc_weight_health validates its inputs", {
  expect_error(calc_weight_health(1:3, rep(1, 2), data.frame(a = 1:3), n_obs = 10), "one entry per draw")
  expect_error(calc_weight_health(1:3, c(1, -1, 1), data.frame(a = 1:3), n_obs = 10), "non-negative")
  expect_error(calc_weight_health(1:3, rep(1, 3), data.frame(a = 1:3), n_obs = 10,
                                  coarsening_alpha = 0), "coarsening_alpha")
  expect_error(calc_weight_health(1:3, rep(0, 3), data.frame(a = 1:3), n_obs = 10), "sum to zero")
  expect_error(calc_weight_health(1:3, rep(1, 3), data.frame(a = 1:2), n_obs = 10), "one row per draw")
})

test_that("n_obs unknown leaves the declared posterior NA and warns", {
  wh <- calc_weight_health(c(0, -1, -2, -3), rep(1, 4), data.frame(a = 1:4), n_obs = NA,
                           ess_target = 2, n_batches = 2L)
  expect_true(is.na(wh$coarsened$ess_kish))
  expect_identical(wh$status, "warn")
  expect_match(wh$reasons, "n_obs unknown")
})

test_that(".mosaic_n_scored_obs counts finite observations inside the scored window", {
  cfg <- list(reported_cases  = matrix(c(1, NA, 3, 4, 5, NA), nrow = 1),
              reported_deaths = matrix(c(NA, NA, NA, NA, NA, 1), nrow = 1))
  expect_identical(MOSAIC:::.mosaic_n_scored_obs(cfg), 5)
  # cases from step 3, deaths from step 3: cases 3,4,5 + deaths at 6
  expect_identical(MOSAIC:::.mosaic_n_scored_obs(cfg, list(idx_cases = 3L, idx_deaths = 3L)), 4)
})

test_that("calc_run_weight_health backfills a legacy (saturated) run without touching its outputs", {
  skip_if_not_installed("arrow")
  d <- withr::local_tempdir()
  dir.create(file.path(d, "1_inputs")); dir.create(file.path(d, "2_calibration"))
  set.seed(9)
  n_t <- 400
  config <- list(location_name = "MOZ", date_start = "2020-01-01",
                 date_stop = as.character(as.Date("2020-01-01") + n_t - 1),
                 reported_cases = rpois(n_t, 20), reported_deaths = rpois(n_t, 1))
  jsonlite::write_json(config, file.path(d, "1_inputs", "config.json"), auto_unbox = TRUE, digits = NA)
  control <- mosaic_control_defaults(likelihood = list(burn_in_days = 10L),
                                     targets = list(ESS_param = 200,
                                                    best_subset_weighting = "saturated"))
  jsonlite::write_json(list(control = MOSAIC:::.mosaic_control_for_json(control)),
                       file.path(d, "1_inputs", "control.json"),
                       auto_unbox = TRUE, digits = NA, null = "null", na = "null")
  n <- 3000
  s <- data.frame(sim = seq_len(n), beta_j0_tot_MOZ = exp(rnorm(n)), gamma_1 = runif(n))
  s$likelihood <- -200 * log(s$beta_j0_tot_MOZ)^2
  top <- order(-s$likelihood)[1:100]
  s$weight_best <- 0; s$weight_best[top] <- 1 / 100
  s$weight_best_opt <- 0; s$weight_best_opt[top[1:40]] <- 1 / 40
  arrow::write_parquet(s, file.path(d, "2_calibration", "samples.parquet"))
  before <- tools::md5sum(file.path(d, "2_calibration", "samples.parquet"))

  r <- calc_run_weight_health(d, write = TRUE)
  expect_true(file.exists(file.path(d, "3_results", "weight_health.json")))
  expect_identical(unname(tools::md5sum(file.path(d, "2_calibration", "samples.parquet"))), unname(before))
  expect_identical(r$weights_used_column, "weight_best_opt")      # auto picks the optimised subset
  expect_identical(r$fields$weights_used_n, 40L)
  expect_identical(r$fields$n_obs_scored, as.numeric(n_t - 10))  # burn-in removed
  expect_equal(r$fields$coarsening_alpha, 10)                     # control default
  expect_equal(r$fields$eta_ref, 10 / (n_t - 10))
  expect_identical(r$fields$posterior_weighting, "saturated")
  expect_true(is.na(r$fields$resample_K))
  js <- jsonlite::read_json(file.path(d, "3_results", "weight_health.json"))
  expect_equal(js$fields$ess_coarsened, r$fields$ess_coarsened, tolerance = 1e-12)
  r2 <- calc_run_weight_health(d, write = FALSE, coarsening_alpha = 6, weight_col = "weight_best")
  expect_identical(r2$fields$weights_used_n, 100L)
  expect_gt(r2$fields$ess_coarsened, r$fields$ess_coarsened)    # smaller alpha, flatter
})

test_that("calc_run_weight_health: a coarsened run's posterior is its members, its verdict weight_all", {
  skip_if_not_installed("arrow")
  d <- withr::local_tempdir()
  dir.create(file.path(d, "1_inputs")); dir.create(file.path(d, "2_calibration"))
  set.seed(19)
  n_t <- 300
  config <- list(location_name = "MOZ", date_start = "2020-01-01",
                 date_stop = as.character(as.Date("2020-01-01") + n_t - 1),
                 reported_cases = rpois(n_t, 20), reported_deaths = rpois(n_t, 1))
  jsonlite::write_json(config, file.path(d, "1_inputs", "config.json"), auto_unbox = TRUE, digits = NA)
  control <- mosaic_control_defaults(likelihood = list(burn_in_days = 0L),
                                     targets = list(ESS_param = 100, min_best_subset = 30,
                                                    max_best_subset = 500))
  jsonlite::write_json(list(control = MOSAIC:::.mosaic_control_for_json(control)),
                       file.path(d, "1_inputs", "control.json"),
                       auto_unbox = TRUE, digits = NA, null = "null", na = "null")
  n <- 2000
  s <- data.frame(sim = seq_len(n), beta_j0_tot_MOZ = exp(rnorm(n)), gamma_1 = runif(n))
  s$likelihood <- -3000 * log(s$beta_j0_tot_MOZ)^2
  cp <- MOSAIC:::.mosaic_coarsened_posterior(s$likelihood, n_t, 10, 30, 500, 7L)
  s$weight_all <- cp$weights
  s$weight_best <- cp$member_weights
  s$is_best_subset <- cp$counts > 0
  arrow::write_parquet(s, file.path(d, "2_calibration", "samples.parquet"))
  r <- calc_run_weight_health(d, write = FALSE)
  expect_identical(r$weights_used_column, "weight_best")
  expect_identical(r$fields$posterior_weighting, "coarsened")
  expect_equal(r$fields$weights_used_ess_kish, 1 / sum(cp$member_weights^2), tolerance = 1e-10)
  expect_equal(r$fields$ess_coarsened, cp$ess_kish, tolerance = 1e-10)   # same alpha: same posterior
  expect_identical(r$fields$resample_K, cp$K)
  expect_identical(r$fields$resample_n_unique, cp$n_unique)
})

