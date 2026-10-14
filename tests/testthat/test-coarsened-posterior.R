# The coarsened (generalised-Bayes) posterior of MOSAIC 1.3.0, its systematic
# resample for the ensemble, and the control shims that go with it. Expected
# values are computed by hand in the comments.

# ---- systematic resampling ---------------------------------------------------

test_that("systematic resample: integer K*w gives exactly K*w copies, zero weights never drawn", {
  # w = (.5, 0, .3, .2), K = 10: every K * w_i is an integer, so floor = ceiling
  # and the counts are (5, 0, 3, 2) whatever the offset.
  for (seed in c(1L, 2L, 99L, 123456L))
    expect_identical(MOSAIC:::.mosaic_systematic_resample(c(.5, 0, .3, .2), 10L, seed),
                     c(5L, 0L, 3L, 2L))
})

test_that("systematic resample matches the hand-computed offset rule", {
  # w = (.55, .45), K = 4: positions (u + k) / 4, k = 0..3, against cw = (.55, 1).
  # Draw 1 is picked for every k with u + k <= 2.2: k = 0, 1 always, k = 2 iff
  # u <= 0.2. So counts = (2 + [u <= .2], 2 - [u <= .2]).
  for (seed in 1:25) {
    u <- MOSAIC:::.mosaic_with_fixed_seed(seed, function() stats::runif(1L))
    want <- if (u <= 0.2) c(3L, 1L) else c(2L, 2L)
    expect_identical(MOSAIC:::.mosaic_systematic_resample(c(.55, .45), 4L, seed), want,
                     info = sprintf("seed %d, u = %.3f", seed, u))
  }
})

test_that("systematic resample is deterministic, sums to K, keeps floor/ceiling and leaves the RNG alone", {
  set.seed(31)
  w <- rexp(500)^3
  K <- 137L
  a <- MOSAIC:::.mosaic_systematic_resample(w, K, 42L)
  expect_identical(a, MOSAIC:::.mosaic_systematic_resample(w, K, 42L))
  expect_identical(sum(a), K)
  kw <- K * w / sum(w)
  expect_true(all(a >= floor(kw) & a <= ceiling(kw)))
  # caller's stream untouched
  set.seed(7); r1 <- runif(3)
  set.seed(7); invisible(MOSAIC:::.mosaic_systematic_resample(w, K, 5L)); r2 <- runif(3)
  expect_identical(r1, r2)
  expect_error(MOSAIC:::.mosaic_systematic_resample(c(0, 0), 3L, 1L), "sum to zero")
})

test_that("systematic resample survives a cumulative sum that rounds past 1 early", {
  # Regression: cumsum(w / sum(w)) can exceed 1 by a few ulp before its last
  # element, which made findInterval() fail on non-decreasing input (found on
  # the national production draws). Search a seeded stream for such a vector.
  set.seed(1)
  w <- NULL
  for (i in 1:5000) {
    v <- rexp(sample(50:5000, 1))^4
    cw <- cumsum(v / sum(v))
    if (max(cw) > 1 && which.max(cw) < length(cw)) { w <- v; break }
  }
  skip_if(is.null(w), "no overshooting cumulative sum in the seeded stream")
  cnt <- MOSAIC:::.mosaic_systematic_resample(w, 997L, 11L)
  expect_identical(sum(cnt), 997L)
  kw <- 997 * w / sum(w)
  expect_true(all(cnt >= floor(kw) & cnt <= ceiling(kw)))
})

test_that("resample seed is a fixed function of the config seed", {
  expect_identical(MOSAIC:::.mosaic_resample_seed(list(seed = 123)), as.integer((123 * 7919 + 104729) %% 2147483647))
  expect_identical(MOSAIC:::.mosaic_resample_seed(list()), 104729L)
  expect_identical(MOSAIC:::.mosaic_resample_seed(list(seed = 5)),
                   MOSAIC:::.mosaic_resample_seed(list(seed = 5)))
})

# ---- the coarsened posterior -------------------------------------------------

test_that("coarsened posterior: eta = alpha / n_obs, tempered weights, K clamped Kish ESS, count weights", {
  # alpha = 10, n_obs = 100 -> eta = 0.1. ll = (0, -10 log 2, -10 log 4, -Inf)
  # -> unnormalised weights (1, 1/2, 1/4, 0) -> w = (4/7, 2/7, 1/7, 0),
  # Kish ESS = 1 / (16 + 4 + 1)/49 = 49/21 = 2.333 -> round 2 -> clamped to min_K = 3.
  ll <- c(0, -10 * log(2), -10 * log(4), -Inf)
  cp <- MOSAIC:::.mosaic_coarsened_posterior(ll, n_obs = 100, alpha = 10, min_K = 3, max_K = 50, seed = 1L)
  expect_equal(cp$eta, 0.1)
  expect_equal(cp$weights, c(4, 2, 1, 0) / 7, tolerance = 1e-14)
  expect_equal(cp$ess_kish, 49 / 21, tolerance = 1e-12)
  expect_identical(cp$K, 3L)
  expect_identical(sum(cp$counts), 3L)
  expect_identical(cp$counts[4], 0L)                      # -Inf never resampled
  expect_equal(cp$member_weights, cp$counts / 3)
  expect_identical(cp$n_unique, sum(cp$counts > 0L))
  # max_K binds from above
  flat <- MOSAIC:::.mosaic_coarsened_posterior(rep(0, 400), 100, 10, 3, 50, 1L)
  expect_identical(flat$K, 50L)
  expect_identical(flat$n_unique, 50L)                    # 50 positions over 400 equal weights: no duplicates
  expect_error(MOSAIC:::.mosaic_coarsened_posterior(ll, NA, 10, 3, 50, 1L), "n_obs")
})

test_that("duplicates collapse into one member weighted count / K", {
  # One dominant draw (w = .9) and nine light ones; K = 10 draws it 9 times.
  ll <- c(0, rep(log(0.1 / 9 / 0.9) / 0.1, 9))  # eta = .1: w = (.9, .1/9 x 9)
  cp <- MOSAIC:::.mosaic_coarsened_posterior(ll, 100, 10, 10, 10, 3L)
  expect_equal(cp$weights[1], 0.9, tolerance = 1e-12)
  expect_identical(cp$counts[1], 9L)
  expect_equal(cp$member_weights[1], 0.9)
  expect_identical(cp$n_unique, 2L)
})

test_that("posterior quantiles under coarsened come from the prediction members, not all draws", {
  # gamma_1 takes 1, 2, 3, 4 with all-draws coarsened weights .1, .2, .3, .4,
  # plus two failed draws. The resampled members are draws 2-4 at count / K =
  # 1/3 each, so the member posterior has midpoint positions (1/6, 1/2, 5/6):
  # median 3, q25 = 2 + (.25 - 1/6) / (1/3) = 2.25, mean 3. Over all draws it
  # would have been median 3.142857, q25 2.2: the posterior must not use them.
  w <- c(.1, .2, .3, .4)
  ll <- c(log(w / .4) / 0.1, -Inf, NA)
  res <- data.frame(sim = 1:6, likelihood = ll, gamma_1 = c(1, 2, 3, 4, 2.5, 2.5))
  res$is_finite <- is.finite(res$likelihood)
  res$is_valid <- res$is_finite; res$is_retained <- res$is_finite
  res$weight_all <- MOSAIC:::.mosaic_tempered_weights(res$likelihood, 0.1)
  res$is_best_subset <- c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
  res$weight_best <- c(0, 1, 1, 1, 0, 0) / 3
  ctl_off <- list(targets = list(best_subset_weighting = "coarsened"),
                  predictions = list(optimize_subset = FALSE))
  cols <- MOSAIC:::.mosaic_active_subset_cols(res, ctl_off)
  expect_identical(cols[c("subset_col", "weight_col", "sample_col", "source")],
                   list(subset_col = "is_best_subset", weight_col = "weight_best",
                        sample_col = "is_best_subset", source = "resample"))
  q <- calc_model_posterior_quantiles(res, probs = c(0.25, 0.5),
                                      output_dir = withr::local_tempdir(),
                                      subset_col = cols$subset_col, weight_col = cols$weight_col,
                                      verbose = FALSE)
  post <- q[q$parameter == "gamma_1" & q$type == "posterior", ]
  expect_equal(post$q0.5, 3, tolerance = 1e-12)
  expect_equal(post$q0.25, 2.25, tolerance = 1e-12)
  expect_equal(post$mean, 3, tolerance = 1e-12)

  # the optimiser ran: its subset (draws 3-4 at 1/2 each) is the posterior source
  res$is_best_subset_opt <- c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE)
  res$weight_best_opt <- c(0, 0, .5, .5, 0, 0)
  cols_opt <- MOSAIC:::.mosaic_active_subset_cols(
    res, list(targets = list(best_subset_weighting = "coarsened"),
              predictions = list(optimize_subset = TRUE)))
  expect_identical(cols_opt[c("subset_col", "weight_col", "sample_col", "source")],
                   list(subset_col = "is_best_subset_opt", weight_col = "weight_best_opt",
                        sample_col = "is_best_subset_opt", source = "optimized"))
  q2 <- calc_model_posterior_quantiles(res, probs = 0.5, output_dir = withr::local_tempdir(),
                                       subset_col = cols_opt$subset_col,
                                       weight_col = cols_opt$weight_col, verbose = FALSE)
  post2 <- q2[q2$parameter == "gamma_1" & q2$type == "posterior", ]
  expect_equal(post2$q0.5, 3.5, tolerance = 1e-12)
  expect_equal(post2$mean, 3.5, tolerance = 1e-12)
})

test_that("every scheme takes the posterior from the members, or the optimised subset", {
  r <- data.frame(is_best_subset = c(TRUE, TRUE), weight_best = c(.5, .5),
                  is_best_subset_opt = c(TRUE, FALSE), weight_best_opt = c(1, 0))
  for (sch in c("coarsened", "saturated", "tempered")) {
    a <- MOSAIC:::.mosaic_active_subset_cols(
      r, list(targets = list(best_subset_weighting = sch), predictions = list(optimize_subset = TRUE)))
    expect_identical(a$subset_col, "is_best_subset_opt")
    expect_identical(a$weight_col, "weight_best_opt")
    expect_identical(a$sample_col, "is_best_subset_opt")
    b <- MOSAIC:::.mosaic_posterior_cols(sch, FALSE)
    expect_identical(b[c("subset_col", "weight_col", "sample_col")],
                     list(subset_col = "is_best_subset", weight_col = "weight_best",
                          sample_col = "is_best_subset"))
  }
})

test_that("the subset optimiser renormalises count weights under coarsened", {
  # likelihood-sorted members with count weights (.4, .3, .2, .1): top 2 -> (.4, .3) / .7
  expect_equal(MOSAIC:::.optimize_subset_weights(c(-1, -2, -3, -4), c(.4, .3, .2, .1), 2L, "coarsened"),
               c(4, 3) / 7)
  expect_equal(MOSAIC:::.optimize_subset_weights(c(-1, -2, -3, -4), c(.4, .3, .2, .1), 4L, "coarsened"),
               c(.4, .3, .2, .1))
  # legacy schemes unchanged
  expect_identical(MOSAIC:::.optimize_subset_weights(c(-1, -2, -9), NULL, 3L, "saturated"),
                   MOSAIC:::.mosaic_best_subset_weights(c(-1, -2, -9), "saturated")$weights)
  ens <- structure(list(n_param_sets = 3L, n_simulations_per_config = 1L, n_locations = 1L,
                        n_time_points = 2L, cases_engine_array = array(1, c(1, 2, 3, 1)),
                        deaths_engine_array = array(1, c(1, 2, 3, 1))), class = "mosaic_ensemble")
  expect_error(optimize_ensemble_subset(ens, c(-1, -2, -3), weighting = "coarsened", verbose = FALSE),
               "needs base_weights")
})

# ---- control shims (raw user control; lesson #13) ------------------------------

test_that("ESS_marginal_method is retired: warned and dropped only when the user set it", {
  expect_null(mosaic_control_defaults()$targets$ESS_marginal_method)
  expect_silent(m <- MOSAIC:::.mosaic_validate_and_merge_control(mosaic_control_defaults()))
  expect_null(m$targets$ESS_marginal_method)
  expect_warning(m2 <- MOSAIC:::.mosaic_validate_and_merge_control(
    list(targets = list(ESS_marginal_method = "binned"))), "ESS_marginal_method is retired")
  expect_null(m2$targets$ESS_marginal_method)
  # the usual pattern: defaults, then one key changed
  ctl <- mosaic_control_defaults(); ctl$targets$ESS_marginal_method <- "kde"
  expect_warning(MOSAIC:::.mosaic_validate_and_merge_control(ctl), "retired")
})

test_that("tier targets warn once under coarsened only when set to a non-default value", {
  expect_silent(MOSAIC:::.mosaic_validate_and_merge_control(mosaic_control_defaults()))
  ctl <- mosaic_control_defaults(); ctl$targets$ESS_best <- 250; ctl$targets$CVw_best <- 2
  w <- testthat::capture_warnings(MOSAIC:::.mosaic_validate_and_merge_control(ctl))
  expect_length(w, 1L)
  expect_match(w, "ESS_best, \\$CVw_best are ignored under best_subset_weighting = \"coarsened\"")
  # a value equal to the default is not "set"
  expect_silent(MOSAIC:::.mosaic_validate_and_merge_control(list(targets = list(A_best = 0.70))))
  # legacy schemes use them: silent
  expect_silent(MOSAIC:::.mosaic_validate_and_merge_control(
    list(targets = list(best_subset_weighting = "saturated", ESS_best = 250))))
})

test_that("batch_ratio_max is a validated control default", {
  expect_identical(mosaic_control_defaults()$targets$batch_ratio_max, 0.2)
  for (bad in list(0, -0.1, Inf, NA_real_, "0.2", c(.1, .2)))
    expect_error(MOSAIC:::.mosaic_validate_and_merge_control(
      list(targets = list(batch_ratio_max = bad))), "batch_ratio_max")
})

# ---- convergence diagnostics under coarsened -----------------------------------

test_that("calc_convergence_diagnostics gates the coarsened posterior, not the tier metrics", {
  wh <- list(coarsened = list(ess_kish = 1500, batch_ratio_max = 0.12, alpha = 10),
             targets = list(ess_target = 1000, batch_max = 0.2), status = "ok")
  args <- list(n_total = 5000, n_successful = 5000, n_retained = 4900, n_best_subset = 950,
               ess_best = 20, A_best = 0.1, cvw_best = 9, percentile_used = 19,
               convergence_tier = "coarsened", target_ess_best = 100, target_A_best = 0.7,
               target_cvw_best = 1, target_max_best_subset = 1000, target_ess_param = 1000,
               weight_health = wh, weighting = "coarsened", verbose = FALSE)
  d <- do.call(calc_convergence_diagnostics, args)
  expect_identical(d$summary$convergence_status, "PASS")    # tier metrics would FAIL
  expect_identical(d$metrics$ess_best$status, "info")
  expect_identical(d$metrics$ess_coarsened$status, "pass")
  expect_identical(d$metrics$batch_ratio$status, "pass")
  args$weight_health$coarsened$batch_ratio_max <- 0.25      # 1.25 x target -> warn
  expect_identical(do.call(calc_convergence_diagnostics, args)$summary$convergence_status, "WARN")
  args$weight_health$coarsened$ess_kish <- 400             # < 0.5 x target -> fail
  expect_identical(do.call(calc_convergence_diagnostics, args)$summary$convergence_status, "FAIL")
  # the same inputs under "saturated" gate the tier metrics
  args$weighting <- "saturated"
  d_sat <- do.call(calc_convergence_diagnostics, args)
  expect_identical(d_sat$metrics$ess_best$status, "fail")
  expect_null(d_sat$metrics$ess_coarsened)
})

# ---- ensemble-size cap (max_ensemble_members) ----------------------------------

test_that("max_ensemble_members caps K under coarsened when it binds, and not otherwise", {
  # flat likelihood over 2,000 draws: Kish ESS = 2,000
  ll <- rep(-5, 2000)
  tg <- MOSAIC:::.mosaic_validate_and_merge_control(mosaic_control_defaults())$targets
  expect_identical(tg$max_ensemble_members, 300)
  expect_identical(MOSAIC:::.mosaic_ensemble_K_max(tg), 300L)        # min(1000, 300)
  cp <- MOSAIC:::.mosaic_coarsened_posterior(ll, 100, 10, tg$min_best_subset,
                                             MOSAIC:::.mosaic_ensemble_K_max(tg), 1L)
  expect_identical(cp$K, 300L)
  expect_identical(sum(cp$counts), 300L)
  expect_identical(MOSAIC:::.mosaic_ensemble_K_binding(cp$ess_kish, tg), "bound by max_ensemble_members")
  # cap above max_best_subset: max_best_subset binds
  tg2 <- utils::modifyList(tg, list(max_best_subset = 200, max_ensemble_members = 500))
  expect_identical(MOSAIC:::.mosaic_ensemble_K_max(tg2), 200L)
  expect_identical(MOSAIC:::.mosaic_ensemble_K_binding(2000, tg2), "bound by max_best_subset")
  # cap not binding: ESS below it, K = round(ESS)
  w_ll <- log(c(rep(1, 120), rep(1e-12, 1880)))
  cp3 <- MOSAIC:::.mosaic_coarsened_posterior(w_ll, 10, 10, 30, MOSAIC:::.mosaic_ensemble_K_max(tg), 1L)
  expect_identical(cp3$K, as.integer(round(cp3$ess_kish)))
  expect_lt(cp3$K, 300L)
  expect_identical(MOSAIC:::.mosaic_ensemble_K_binding(cp3$ess_kish, tg), "Kish ESS (no bound binds)")
  expect_identical(MOSAIC:::.mosaic_ensemble_K_binding(12, tg), "bound by min_best_subset")
  # legacy targets list without the key: bounded by max_best_subset only
  expect_identical(MOSAIC:::.mosaic_ensemble_K_max(list(max_best_subset = 1000)), 1000L)
})

test_that("max_ensemble_members is validated", {
  v <- function(x) MOSAIC:::.mosaic_validate_and_merge_control(
    mosaic_control_defaults(targets = list(max_ensemble_members = x)))
  expect_error(v(10), "max_ensemble_members")          # < min_best_subset (30)
  expect_error(v(150.5), "max_ensemble_members")
  expect_error(v(c(100, 200)), "max_ensemble_members")
  expect_error(v(Inf), "max_ensemble_members")
  expect_error(v("300"), "max_ensemble_members")
  expect_identical(v(150)$targets$max_ensemble_members, 150)
})
