# Regression tests for the weighting / posterior review findings
# (claude/deep_review/groups/weighting.json). Each block names its finding.

# ---- weighting-posterior-03: grid search scores the gated weights ------------

test_that("grid_search_best_subset weights are the saturated weight_best scheme (hand fixture)", {
  # delta = -2 * (ll - max ll) = (0, 2, 10, 10) -> saturated at 4 -> (0, 2, 4, 4)
  # w ~ (1, e^-1, e^-2, e^-2)
  r <- data.frame(sim = 1:4, likelihood = c(0, -1, -5, -5))
  res <- grid_search_best_subset(r, target_ESS = 100, target_A = 0.99, target_CVw = 0.01,
                                 min_size = 4, max_size = 4, ess_method = "kish")
  w <- c(1, exp(-1), exp(-2), exp(-2)); w <- w / sum(w)
  expect_false(res$converged)
  expect_equal(res$metrics$ESS, 1 / sum(w^2), tolerance = 1e-12)
  expect_equal(res$metrics$ESS, 2.290890, tolerance = 1e-6)
  expect_equal(res$metrics$CVw, calc_model_cvw(w * 4), tolerance = 1e-12)
  expect_equal(res$metrics$A, calc_model_agreement_index(w * 4)$A, tolerance = 1e-12)
})

test_that("grid-selected subset meets its tier on the weights the final gate recomputes", {
  set.seed(3)
  ll <- -cumsum(abs(rnorm(400, sd = 30)))
  r  <- data.frame(sim = seq_along(ll), likelihood = sample(ll))
  res <- grid_search_best_subset(r, target_ESS = 60, target_A = 0.7, target_CVw = 1.0,
                                 min_size = 30, max_size = 400, ess_method = "perplexity")
  expect_true(res$converged)
  # The final gate in run_MOSAIC(): pmin(delta, 4), eta = 0.5, on the same subset
  d <- -2 * res$subset$likelihood; d <- d - min(d)
  w_gate <- calc_model_weights_gibbs(pmin(d, 4), eta = 0.5)
  expect_equal(calc_model_ess(w_gate, method = "perplexity"), res$metrics$ESS, tolerance = 1e-12)
  expect_gte(calc_model_ess(w_gate, method = "perplexity"), 60)
})

test_that("grid-selected subset size responds to the likelihood scale", {
  # The old range-scaled weighting depended only on delta / range(delta), so the
  # selected n was identical at every scale. Saturation at a fixed 4 is not.
  set.seed(5)
  ll <- -sort(rexp(300, 1))
  pick <- function(scale) {
    grid_search_best_subset(data.frame(sim = 1:300, likelihood = ll * scale),
                            target_ESS = 40, target_A = 0.7, target_CVw = 1.0,
                            min_size = 10, max_size = 300, ess_method = "perplexity")$n
  }
  expect_false(pick(1) == pick(1000))
})

test_that("grid_search_best_subset honours weighting = 'tempered'", {
  r <- data.frame(sim = 1:50, likelihood = -cumsum(c(0, abs(sin(1:49)) * 3)))
  res <- grid_search_best_subset(r, target_ESS = 1e6, target_A = 0.7, target_CVw = 1,
                                 min_size = 50, max_size = 50, weighting = "tempered",
                                 ess_method = "kish")
  w <- MOSAIC:::.mosaic_calc_adaptive_gibbs_weights(res$subset$likelihood)$weights
  expect_equal(res$metrics$ESS, calc_model_ess(w, method = "kish"), tolerance = 1e-12)
})

# ---- weighting-posterior-04 / x-artifacts-13: optimize_ensemble_subset -------

mk_opt_ens <- function(n_params = 8L, n_stoch = 2L, n_locs = 2L, n_times = 6L) {
  set.seed(8)
  obs_c <- matrix(rpois(n_locs * n_times, 40), n_locs, n_times)
  obs_d <- matrix(rpois(n_locs * n_times, 4),  n_locs, n_times)
  ca <- array(rpois(n_locs * n_times * n_params * n_stoch, 40), c(n_locs, n_times, n_params, n_stoch))
  da <- array(rpois(n_locs * n_times * n_params * n_stoch, 4),  c(n_locs, n_times, n_params, n_stoch))
  structure(list(cases_array = ca, deaths_array = da, obs_cases = obs_c, obs_deaths = obs_d,
                 n_param_sets = n_params, n_simulations_per_config = n_stoch,
                 n_locations = n_locs, n_time_points = n_times,
                 location_names = paste0("L", seq_len(n_locs)),
                 envelope_quantiles = c(0.025, 0.25, 0.75, 0.975)),
            class = "mosaic_ensemble")
}

test_that("optimizer per-N weights reproduce weight_best at N = full ensemble", {
  ens <- mk_opt_ens()
  ll  <- c(-100, -100.5, -101, -103, -110, -140, -200, -500)  # delta 0,1,2,6,20,...
  res <- optimize_ensemble_subset(ens, ll, min_n = 8L, objective = "mae", verbose = FALSE)
  expect_identical(res$optimal_n, 8L)
  d <- -2 * ll; d <- d - min(d)
  w_best <- exp(-0.5 * pmin(d, 4)); w_best <- w_best / sum(w_best)
  expect_equal(res$optimal_weights, w_best, tolerance = 1e-14)
  # w = (1, e^-0.5, e^-1, e^-2, e^-2, ...) -> hand values of the first two shares
  z <- 1 + exp(-0.5) + exp(-1) + 5 * exp(-2)
  expect_equal(res$optimal_weights[1:2], c(1, exp(-0.5)) / z, tolerance = 1e-14)
  expect_equal(res$ensemble_optimized$parameter_weights, w_best, tolerance = 1e-14)
})

test_that("optimizer argmax selects a subset below the full ensemble when it scores better", {
  # Top-3 members reproduce the observations exactly; the other five are 10x off,
  # so WIS is best for N <= 3 and the optimizer must not return N = 8.
  ens <- mk_opt_ens()
  for (p in seq_len(8)) {
    f <- if (p <= 3) 1 else 10
    ens$cases_array[, , p, ]  <- ens$obs_cases * f
    ens$deaths_array[, , p, ] <- ens$obs_deaths * f
  }
  ll  <- c(-100, -100.5, -101, -103, -110, -140, -200, -500)
  res <- optimize_ensemble_subset(ens, ll, min_n = 2L, objective = "wis", stride = 1L,
                                  verbose = FALSE)
  expect_lte(res$optimal_n, 3L)
  d <- -2 * ll[seq_len(res$optimal_n)]; d <- d - min(d)
  w <- exp(-0.5 * pmin(d, 4))
  expect_equal(res$optimal_weights, w / sum(w), tolerance = 1e-14)
})

test_that("optimizer evaluation_table$ess honours ess_method", {
  ens <- mk_opt_ens()
  ll  <- c(-100, -100.5, -101, -103, -110, -140, -200, -500)
  k <- optimize_ensemble_subset(ens, ll, min_n = 8L, verbose = FALSE, ess_method = "kish")
  p <- optimize_ensemble_subset(ens, ll, min_n = 8L, verbose = FALSE, ess_method = "perplexity")
  w <- k$optimal_weights
  expect_equal(k$evaluation_table$ess, 1 / sum(w^2), tolerance = 1e-12)
  expect_equal(p$evaluation_table$ess, exp(-sum(w * log(w))), tolerance = 1e-12)
})

test_that("optimize_ensemble_subset explains an array-stripped saved ensemble", {
  ens <- mk_opt_ens()
  ens$cases_array <- NULL
  ens$deaths_array <- NULL
  expect_error(optimize_ensemble_subset(ens, seq(-1, -8), verbose = FALSE),
               "persist_ensemble_arrays")
})

# ---- weighting-posterior-05: percentile on the same denominator, not gated ----

test_that("percentile target uses n_total and its status is reported, not gated", {
  d <- calc_convergence_diagnostics(
    n_total = 10000, n_successful = 9000, n_retained = 5000, n_best_subset = 400,
    ess_best = 150, A_best = 0.9, cvw_best = 0.5,
    percentile_used = 6.0,            # inconsistent on purpose: exceeds 500/10000
    convergence_tier = "tier_1",
    target_ess_best = 100, target_A_best = 0.7, target_cvw_best = 1.0,
    target_max_best_subset = 500, verbose = FALSE)
  expect_equal(d$targets$percentile_max$value, 5.0)   # 500 / 10000, not 500 / 5000
  expect_identical(d$summary$percentile_status, "warn")
  expect_identical(d$summary$convergence_status, "PASS")
  expect_identical(d$metrics$B_size_upper$status, "pass")
})

test_that("B_size_upper still gates a subset above the cap", {
  d <- calc_convergence_diagnostics(
    n_total = 10000, n_successful = 9000, n_retained = 5000, n_best_subset = 700,
    ess_best = 150, A_best = 0.9, cvw_best = 0.5, percentile_used = 7.0,
    convergence_tier = "tier_1", target_ess_best = 100, target_A_best = 0.7,
    target_cvw_best = 1.0, target_max_best_subset = 500, verbose = FALSE)
  expect_identical(d$metrics$B_size_upper$status, "fail")   # 700 > 1.2 * 500
  expect_identical(d$summary$convergence_status, "FAIL")
})

# ---- weighting-posterior-07: status table shows every gated/reported row ------

test_that("convergence status table includes subset, cap, IS diagnostics and overall rows", {
  is_all  <- calc_is_diagnostics(c(0, -1e6 * (1:99)), method = "perplexity")
  is_best <- calc_is_diagnostics(-(0:19), method = "perplexity")
  d <- calc_convergence_diagnostics(
    n_total = 1000, n_successful = 1000, n_retained = 900, n_best_subset = 120,
    ess_best = 110, A_best = 0.9, cvw_best = 0.5, percentile_used = 12,
    convergence_tier = "tier_1", target_ess_best = 100, target_A_best = 0.7,
    target_cvw_best = 1.0, target_max_best_subset = 150,
    is_diagnostics = list(best = is_best, all = is_all), verbose = FALSE)
  res_dir <- withr::local_tempdir()
  jsonlite::write_json(d, file.path(res_dir, "convergence_diagnostics.json"),
                       auto_unbox = TRUE, pretty = TRUE, digits = NA)
  st <- calc_model_convergence_status(res_dir, verbose = FALSE)
  m  <- st$metrics_data
  expect_true(all(c("Subset Selection", "Best Subset (B)", "Best Subset (B) cap", "ESS_B",
                    "ESS_IS (all)", "ESS_IS (B)", "Pareto k-hat", "Overall") %in% m$Metric))
  expect_identical(length(st$metric_expressions), nrow(m))
  expect_identical(m$Value[m$Metric == "Subset Selection"], "12.0%")
  expect_identical(m$Target[m$Metric == "Subset Selection"], "<=15.0%")
  expect_identical(m$Status[m$Metric == "Best Subset (B) cap"], "pass")
  expect_identical(m$Value[m$Metric == "ESS_IS (all)"], "1 of 100")
  expect_identical(m$Status[m$Metric == "Overall"], tolower(d$summary$convergence_status))
  # khat_status is the tail-fit status, not a verdict: an "ok" fit must not
  # read as "Pareto k-hat: ok" next to an unreliable k-hat.
  khat_desc <- m$Description[m$Metric == "Pareto k-hat"]
  expect_false(grepl(": ok$", khat_desc))
  expect_match(khat_desc, "^Pareto k-hat, all draws \\(not gated\\)")
  if (identical(is_all$khat_status, "ok") && is.finite(is_all$khat) && is_all$khat >= 0.7)
    expect_match(khat_desc, "unreliable")
})

# ---- weighting-posterior-08: KL is KL(posterior || prior), uncapped ----------

test_that("posterior KL is the uncapped information gain and ranks identifiability", {
  set.seed(1)
  prior <- runif(20000)
  # Gaussian of sd s inside U(0,1): KL = -0.5 * log(2 * pi * e * s^2), evaluated
  # at the sample sd. The tight cases are narrower than one cell of a 1000-point
  # pooled grid, where a pooled-grid KDE KL levels off near 6.2.
  kl_u <- vapply(c(0.05, 1.2e-3, 1e-4, 1e-5), function(s) {
    post <- rnorm(115, 0.5, s)
    c(computed = MOSAIC:::.mosaic_posterior_kl(prior, post, rep(1, 115)),
      analytic = -0.5 * log(2 * pi * exp(1) * sd(post)^2))
  }, numeric(2))
  expect_true(all(abs(kl_u["computed", ] - kl_u["analytic", ]) < 0.1))
  expect_gt(kl_u["computed", 4], 10)
  expect_true(all(diff(kl_u["computed", ]) > 0))

  # Lognormal(0, 1.5) prior, lognormal(0, s) posterior:
  # KL = log(1.5 / s) + s^2 / (2 * 1.5^2) - 1/2 (1.13, 2.21, 3.41, 4.51)
  prior_ln <- rlnorm(20000, 0, 1.5)
  kl_ln <- vapply(c(0.3, 0.1, 0.03, 0.01), function(s) {
    post <- rlnorm(115, 0, s)
    s_hat <- sd(log(post))
    c(computed = MOSAIC:::.mosaic_posterior_kl(prior_ln, post),
      analytic = log(1.5 / s_hat) + (s_hat^2 + mean(log(post))^2) / (2 * 1.5^2) - 0.5)
  }, numeric(2))
  expect_true(all(abs(kl_ln["computed", ] - kl_ln["analytic", ]) < 0.15))
  expect_true(all(diff(kl_ln["computed", ]) > 0))

  # Weights are normalised; non-finite draws are dropped together with their weights
  post_wide <- rnorm(115, 0.5, 0.05)
  kl_wide <- MOSAIC:::.mosaic_posterior_kl(prior, post_wide, rep(1, 115))
  expect_equal(MOSAIC:::.mosaic_posterior_kl(prior, post_wide, rep(7, 115)), kl_wide,
               tolerance = 1e-12)
  expect_equal(MOSAIC:::.mosaic_posterior_kl(prior, c(post_wide, NA), c(rep(1, 115), 5)),
               kl_wide, tolerance = 1e-12)
  expect_true(is.na(MOSAIC:::.mosaic_posterior_kl(prior, post_wide[1:5])))
})

# ---- weighting-posterior-14 (+ -08 wiring): unknown rows are not failures ----

test_that("unknown-scale quantile rows are skipped silently and KL is not capped", {
  set.seed(42)
  n <- 2000
  results <- data.frame(decay_shape_1 = runif(n, 0.1, 10), not_a_model_param = rnorm(n),
                        is_finite = TRUE, is_retained = TRUE,
                        is_best_subset = c(rep(TRUE, 500), rep(FALSE, n - 500)),
                        weight_best = c(rep(1 / 500, 500), rep(0, n - 500)),
                        likelihood = rnorm(n, -100, 10))
  results$decay_shape_1[1:500] <- rnorm(500, 5, 0.01)
  priors <- list(metadata = list(version = "test"),
                 parameters_global = list(
                   decay_shape_1 = list(distribution = "uniform", parameters = list(min = 0.1, max = 10))),
                 parameters_location = list())
  out_dir <- withr::local_tempdir()
  q <- calc_model_posterior_quantiles(results = results, output_dir = out_dir,
                                      priors = priors, verbose = FALSE)
  expect_true("not_a_model_param" %in% q$parameter)
  kl <- q$kl[q$parameter == "decay_shape_1" & q$type == "posterior"]
  expect_true(is.finite(kl) && kl > 3 && kl < 20)
  priors_path <- file.path(out_dir, "priors.json")
  jsonlite::write_json(priors, priors_path, auto_unbox = TRUE, pretty = TRUE, digits = NA)
  out <- calc_model_posterior_distributions(
    quantiles_file = file.path(out_dir, "posterior_quantiles.csv"),
    priors_file = priors_path, output_dir = out_dir, verbose = FALSE)
  expect_identical(out$n_parameters_failed, 0)
  expect_false(is.null(out$posteriors$parameters_global$decay_shape_1))
})

# ---- weighting-posterior-11: location-parameter descriptions -----------------

test_that("sensitivity descriptions resolve location-scale parameters by base name", {
  priors <- list(
    parameters_global = list(gamma_1 = list(description = "Recovery rate")),
    parameters_location = list(beta_j0_tot = list(description = "Total transmission",
                                                  location = list(ETH = list()))))
  d <- MOSAIC:::.mosaic_sensitivity_descriptions(priors, c("gamma_1", "beta_j0_tot_ETH", "zzz"))
  expect_identical(unname(d), c("Recovery rate", "Total transmission", ""))
})

# ---- weighting-posterior-12: tied tail is not "underflow" --------------------

test_that("all-equal log-likelihoods report a tied tail, not underflow", {
  d <- calc_is_diagnostics(rep(-5, 100))
  expect_equal(d$ess_is, 100)
  expect_true(is.na(d$khat))
  expect_match(d$khat_status, "tied")
  expect_no_match(d$khat_status, "underflow")
})

# ---- weighting-posterior-13: no extrapolation from a non-increasing fit ------

test_that("bookend batch size refuses to extrapolate a declining ESS trajectory", {
  hist <- list(list(total_sims = 1000, threshold_ess = 300),
               list(total_sims = 4000, threshold_ess = 281),
               list(total_sims = 9000, threshold_ess = 260))
  res <- calc_bookend_batch_size(hist, target_ess = 400, max_total_sims = 1e5,
                                 target_r_squared = 0.9)
  expect_identical(res$phase, "no_progress")
  expect_identical(res$batch_size, 0)
})

test_that("bookend batch size still predicts from an increasing sqrt trajectory", {
  # ESS = 20 + 2 * sqrt(n) exactly -> target 420 needs n = 200^2 = 40000
  n <- c(1000, 4000, 9000, 16000)
  hist <- lapply(n, function(k) list(total_sims = k, threshold_ess = 20 + 2 * sqrt(k)))
  # lm() warns "essentially perfect fit" on this exact fixture; that is expected
  res <- suppressWarnings(calc_bookend_batch_size(hist, target_ess = 420, max_total_sims = 1e6,
                                                  target_r_squared = 0.9))
  expect_identical(res$phase, "predictive")
  expect_identical(res$model, "sqrt")
  expect_equal(res$batch_size, ceiling((40000 - 16000) * 1.05))
})

test_that("bookend batch size does not call an over-predicting fit 'complete'", {
  # sqrt fit puts target 280 at n ~ 8100 < current 9000, but ESS is still 250
  hist <- lapply(list(c(1000, 100), c(4000, 300), c(9000, 250)),
                 function(v) list(total_sims = v[1], threshold_ess = v[2]))
  res <- suppressWarnings(calc_bookend_batch_size(hist, target_ess = 280, max_total_sims = 1e5,
                                                  target_r_squared = 0.1))
  expect_identical(res$phase, "low_confidence")
  expect_identical(res$batch_size, 0)
  expect_match(res$message, "over-predicts current ESS")
})

# ---- weighting-posterior-01 (DEFERRED): pin the known defect -----------------
# The default kde marginal ESS does not detect importance-weight collapse. This
# test documents current behaviour; when the stop rule is fixed it should fail
# and be replaced by an assertion that a point mass gives a marginal ESS near 1.
test_that("KNOWN DEFECT: kde marginal ESS stays large for a point-mass weight vector", {
  set.seed(7)
  n <- 5000
  res <- data.frame(gamma_1 = runif(n, 0.05, 1), likelihood = c(0, rep(-1e6, n - 1)))
  expect_equal(calc_is_diagnostics(res$likelihood, method = "perplexity")$ess_is, 1,
               tolerance = 1e-8)
  ess <- calc_model_ess_parameter(res, param_names = "gamma_1", method = "perplexity",
                                  marginal_method = "kde")
  expect_gt(ess$ess_marginal[ess$parameter == "gamma_1"], 100)
})

test_that("a forced fallback under non-saturated best-subset weighting is logged", {
  body_txt <- paste(deparse(body(MOSAIC::run_MOSAIC)), collapse = "\n")
  expect_true(grepl("every tier failed, so the posterior is the top %d draws", body_txt,
                    fixed = TRUE))
})
