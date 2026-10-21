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
  expect_warning(q <- calc_model_posterior_quantiles(res, probs = c(0.25, 0.5),
                                      output_dir = withr::local_tempdir(),
                                      subset_col = cols$subset_col, weight_col = cols$weight_col,
                                      verbose = FALSE), "percentile-scale KL divergence is NA")
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
  expect_warning(q2 <- calc_model_posterior_quantiles(res, probs = 0.5, output_dir = withr::local_tempdir(),
                                       subset_col = cols_opt$subset_col,
                                       weight_col = cols_opt$weight_col, verbose = FALSE),
                 "percentile-scale KL divergence is NA")
  post2 <- q2[q2$parameter == "gamma_1" & q2$type == "posterior", ]
  expect_equal(post2$q0.5, 3.5, tolerance = 1e-12)
  expect_equal(post2$mean, 3.5, tolerance = 1e-12)
})

test_that("every scheme takes the posterior from the members, or the optimised subset", {
  r <- data.frame(is_best_subset = c(TRUE, TRUE), weight_best = c(.5, .5),
                  is_best_subset_opt = c(TRUE, FALSE), weight_best_opt = c(1, 0))
  for (sch in c("bestfit", "coarsened", "saturated", "tempered")) {
    a <- MOSAIC:::.mosaic_active_subset_cols(
      r, list(targets = list(posterior_weighting = sch), predictions = list(optimize_subset = TRUE)))
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

test_that("tier settings warn once outside the legacy schemes, only when the user set them", {
  expect_silent(MOSAIC:::.mosaic_validate_and_merge_control(mosaic_control_defaults()))
  ctl <- mosaic_control_defaults(); ctl$targets$A_best <- 0.5; ctl$targets$max_best_subset <- 400
  w <- testthat::capture_warnings(m <- MOSAIC:::.mosaic_validate_and_merge_control(ctl))
  expect_length(w, 1L)
  expect_match(w, "A_best, \\$max_best_subset are ignored under posterior_weighting = \"bestfit\"")
  expect_null(m$targets$A_best)
  # a value equal to the default is not "set"
  expect_silent(MOSAIC:::.mosaic_validate_and_merge_control(list(targets = list(max_best_subset = 1000))))
  # ESS_best changed meaning in 1.4.0: an explicit value differing from the
  # scheme's default is honoured with a warning (D12); the default is silent
  expect_warning(m250 <- MOSAIC:::.mosaic_validate_and_merge_control(list(targets = list(ESS_best = 250))),
                 "ESS_best = 250 is honoured, but its meaning changed in MOSAIC 1.4.0")
  expect_identical(m250$targets$ESS_best, 250)
  expect_silent(MOSAIC:::.mosaic_validate_and_merge_control(list(targets = list(ESS_best = 200))))
  expect_silent(MOSAIC:::.mosaic_validate_and_merge_control(mosaic_control_defaults()))
  # a 1.3.x control.json (coarsened, ESS_best 100) warns and keeps 100
  expect_warning(m13 <- MOSAIC:::.mosaic_validate_and_merge_control(
    list(targets = list(posterior_weighting = "coarsened", ESS_best = 100))), "meaning changed")
  expect_identical(m13$targets$ESS_best, 100)
  # legacy schemes use the tier settings: silent, with their pre-1.4.0 defaults
  sat <- expect_silent(MOSAIC:::.mosaic_validate_and_merge_control(
    list(targets = list(posterior_weighting = "saturated", A_best = 0.6))))
  expect_identical(sat$targets[c("ESS_best", "A_best", "CVw_best")],
                   list(ESS_best = 100, A_best = 0.6, CVw_best = 1.0))
  expect_warning(sat2 <- MOSAIC:::.mosaic_validate_and_merge_control(
    mosaic_control_defaults(targets = list(posterior_weighting = "saturated", ESS_best = 50))),
    "and the tier-search target")
  expect_identical(sat2$targets$ESS_best, 50)                 # user-set tier target honoured
  # a raw 200 is the 1.4.0 default, i.e. unset (lesson #13, CN1): under "saturated"
  # the 1.2.1 tier target 100 applies, silently, in either usage pattern
  sat3 <- expect_silent(MOSAIC:::.mosaic_validate_and_merge_control(
    list(targets = list(posterior_weighting = "saturated", ESS_best = 200))))
  expect_identical(sat3$targets$ESS_best, 100)
  ctl_sat <- mosaic_control_defaults(); ctl_sat$targets$posterior_weighting <- "saturated"
  sat5 <- expect_silent(MOSAIC:::.mosaic_validate_and_merge_control(ctl_sat))
  expect_identical(sat5$targets$ESS_best, 100)
  ctl_bsw <- mosaic_control_defaults(); ctl_bsw$targets$best_subset_weighting <- "saturated"
  expect_warning(sat6 <- MOSAIC:::.mosaic_validate_and_merge_control(ctl_bsw), "best_subset_weighting is deprecated")
  expect_identical(sat6$targets$ESS_best, 100)
  # the legacy default 100 set explicitly under "saturated" is silent too
  expect_silent(MOSAIC:::.mosaic_validate_and_merge_control(
    list(targets = list(posterior_weighting = "saturated", ESS_best = 100))))
  # mosaic_control_defaults() gives a legacy scheme its tier default, silently
  sat4 <- expect_silent(MOSAIC:::.mosaic_validate_and_merge_control(
    mosaic_control_defaults(targets = list(posterior_weighting = "saturated"))))
  expect_identical(sat4$targets$ESS_best, 100)
  # min_draws above the budget is clamped with a warning (C6)
  expect_warning(mc <- MOSAIC:::.mosaic_validate_and_merge_control(
    list(calibration = list(min_draws = 20000L, max_simulations_total = 10000L))),
    "min_draws \\(20,000\\) exceeds calibration\\$max_simulations_total")
  expect_identical(as.numeric(mc$calibration$min_draws), 10000)
  # a fixed budget uses neither, so no warning and no clamp (PN1)
  mf <- expect_silent(MOSAIC:::.mosaic_validate_and_merge_control(
    list(calibration = list(n_simulations = 300L, min_draws = 20000L, max_simulations_total = 10000L))))
  expect_identical(as.numeric(mf$calibration$min_draws), 20000)
})

test_that("the 1.4.0 retired keys warn once and are dropped, judged from the raw control", {
  expect_null(mosaic_control_defaults()$targets$batch_ratio_max)
  expect_null(mosaic_control_defaults()$targets$ESS_param)
  expect_null(mosaic_control_defaults()$weights$floor)
  expect_null(mosaic_control_defaults()$io$verbose_weights)
  ctl <- mosaic_control_defaults()
  ctl$targets$ESS_param <- 1000; ctl$targets$batch_ratio_max <- 0.2
  ctl$weights$floor <- 1e-15; ctl$io$verbose_weights <- TRUE
  ctl$calibration$max_batches_adaptive <- 8L
  w <- testthat::capture_warnings(m <- MOSAIC:::.mosaic_validate_and_merge_control(ctl))
  expect_length(w, 4L)
  expect_true(any(grepl("ESS_param, control\\$targets\\$batch_ratio_max are deprecated", w)))
  expect_true(any(grepl("weights\\$floor is deprecated", w)))
  expect_true(any(grepl("verbose_weights is deprecated", w)))
  expect_true(any(grepl("max_batches_adaptive is deprecated", w)))
  expect_null(m$targets$ESS_param); expect_null(m$weights$floor); expect_null(m$calibration$max_batches_adaptive)
  # best_subset_weighting is the deprecated name of posterior_weighting
  expect_warning(m2 <- MOSAIC:::.mosaic_validate_and_merge_control(
    list(targets = list(best_subset_weighting = "coarsened"))), "best_subset_weighting is deprecated")
  expect_identical(m2$targets$posterior_weighting, "coarsened")
  expect_null(m2$targets$best_subset_weighting)
  expect_warning(m3 <- MOSAIC:::.mosaic_validate_and_merge_control(
    list(targets = list(best_subset_weighting = "coarsened", posterior_weighting = "saturated"))),
    "was ignored because")
  expect_identical(m3$targets$posterior_weighting, "saturated")
})

test_that("the 1.4.0 control keys have the SCHEMA defaults and are validated", {
  tg <- mosaic_control_defaults()$targets
  expect_identical(tg$posterior_weighting, "bestfit")
  expect_identical(tg$best_subset_ratio, 0.2)
  expect_identical(tg$ESS_best, 200)
  expect_identical(c(tg$min_ensemble_members, tg$max_ensemble_members), c(30L, 1000L))
  expect_identical(tg$status_thresholds, MOSAIC:::.mosaic_convergence_defaults()$thresholds)
  cal <- mosaic_control_defaults()$calibration
  expect_identical(cal$max_simulations_total, 150000L)
  expect_identical(cal$min_draws, 5000L)
  v <- function(...) MOSAIC:::.mosaic_validate_and_merge_control(mosaic_control_defaults(...))
  expect_error(v(targets = list(posterior_weighting = "best")), "posterior_weighting")
  expect_error(v(targets = list(best_subset_ratio = 0)), "best_subset_ratio")
  expect_error(v(targets = list(best_subset_ratio = 1.5)), "best_subset_ratio")
  expect_error(v(targets = list(ESS_best = -1)), "ESS_best")
  expect_error(v(targets = list(min_ensemble_members = 0)), "min_ensemble_members")
  expect_error(v(targets = list(min_ensemble_members = 50, max_ensemble_members = 40)), "max_ensemble_members")
  expect_error(v(targets = list(status_thresholds = list(mce_green = 0.3))), "mce_green <= mce_yellow")
  expect_error(v(targets = list(status_thresholds = list(foo = 1))), "unknown convergence threshold")
  expect_error(v(calibration = list(min_draws = -1)), "min_draws")
  expect_identical(v(targets = list(status_thresholds = list(mce_green = 0.08)))$targets$status_thresholds$mce_yellow,
                   0.15)
})

# ---- convergence diagnostics ---------------------------------------------------

test_that("calc_convergence_diagnostics carries the 1.4.0 status, not the tier metrics", {
  set.seed(3)
  ll <- -300 * (runif(3000) - 0.5)^2
  wh <- calc_weight_health(ll, params = data.frame(x = runif(3000)), n_obs = 1000, fixed_mode = TRUE)
  args <- list(n_total = 3000, n_successful = 3000, n_retained = 3000,
               n_best_subset = wh$bestfit$n_members, ess_best = NA_real_, A_best = NA_real_,
               cvw_best = NA_real_, percentile_used = 0, convergence_tier = "bestfit",
               weight_health = wh, weighting = "bestfit", verbose = FALSE)
  d <- do.call(calc_convergence_diagnostics, args)
  expect_identical(d$summary$convergence_status, wh$status)
  expect_identical(d$status$convergence_status, wh$status)
  expect_identical(d$metrics$ess_best$status, wh$convergence$checks$colour[1])
  expect_null(d$metrics$B_size)
  expect_null(d$metrics$batch_ratio)
  expect_error(do.call(calc_convergence_diagnostics, utils::modifyList(args, list(weight_health = NULL))),
               "needs weight_health")
  # under "saturated" the tier rows are reported and the tier verdict is tier_status
  sat <- do.call(calc_convergence_diagnostics, utils::modifyList(args, list(
    weighting = "saturated", ess_best = 20, A_best = 0.1, cvw_best = 9, target_ess_best = 100,
    n_best_subset = 100, percentile_used = 3)))
  expect_identical(sat$summary$convergence_status, wh$status)
  expect_identical(sat$summary$tier_status, "FAIL")
  expect_identical(sat$metrics$ess_B$status, "fail")
})

# ---- ensemble-size bounds under coarsened --------------------------------------

test_that("min/max_ensemble_members bound K under coarsened, and the binding is reported", {
  ll <- rep(-5, 2000)                       # flat: Kish ESS = 2,000
  cp <- MOSAIC:::.mosaic_coarsened_posterior(ll, 100, 10, 30, 300, 1L)
  expect_identical(cp$K, 300L)
  expect_identical(sum(cp$counts), 300L)
  expect_identical(MOSAIC:::.mosaic_resample_binding(cp$ess_kish, 30, 300), "max_members")
  w_ll <- log(c(rep(1, 120), rep(1e-12, 1880)))
  cp3 <- MOSAIC:::.mosaic_coarsened_posterior(w_ll, 10, 10, 30, 300, 1L)
  expect_identical(cp3$K, as.integer(round(cp3$ess_kish)))
  expect_identical(MOSAIC:::.mosaic_resample_binding(cp3$ess_kish, 30, 300), "ratio")
  expect_identical(MOSAIC:::.mosaic_resample_binding(12, 30, 300), "min_members")
})

test_that("max_ensemble_members is validated", {
  v <- function(x) MOSAIC:::.mosaic_validate_and_merge_control(
    mosaic_control_defaults(targets = list(max_ensemble_members = x)))
  expect_error(v(10), "max_ensemble_members")          # < min_ensemble_members (30)
  expect_error(v(150.5), "max_ensemble_members")
  expect_error(v(c(100, 200)), "max_ensemble_members")
  expect_error(v(Inf), "max_ensemble_members")
  expect_error(v("300"), "max_ensemble_members")
  expect_identical(v(150)$targets$max_ensemble_members, 150)
})
