# The 1.3.0 weight-health diagnostic is reported only: it must leave the
# best-subset weights, the posterior quantiles and every gated decision exactly
# as they were. These tests pin that, plus the run_MOSAIC() / summary.json wiring.

# ---- the posterior is bit-identical to v1.2.1 --------------------------------

test_that("best-subset weights and posterior quantiles are bit-identical to v1.2.1 (ba71c4e7d)", {
  # The golden file was produced by this exact fixture with MOSAIC at
  # ba71c4e7d (v1.2.1), before the weight-health change, via
  # claude/weights_investigation/scripts/13_golden_posterior.R.
  golden <- readRDS(test_path("fixtures", "weights_posterior_golden_ba71c4e.rds"))
  set.seed(20261012)
  n <- 2000
  df <- data.frame(sim = seq_len(n), iter = 1L, seed_sim = seq_len(n), seed_iter = NA_real_)
  df$beta_j0_tot_MOZ <- exp(rnorm(n, log(3e-5), 1))
  df$psi_star_a_MOZ  <- runif(n, 0.1, 3)
  df$gamma_1         <- runif(n, 0.05, 0.5)
  df$rho             <- runif(n, 0.2, 0.7)
  df$likelihood <- -500 * (log(df$beta_j0_tot_MOZ / 3e-5))^2 - 300 * (df$psi_star_a_MOZ - 1)^2 +
    rnorm(n, 0, 30) - 10000
  df$likelihood[c(5, 17)] <- -Inf
  df$is_finite <- is.finite(df$likelihood); df$is_valid <- df$is_finite
  df$is_outlier <- FALSE; df$is_retained <- df$is_valid
  tr <- grid_search_best_subset(df[df$is_retained, ], target_ESS = 100, target_A = 0.7,
                                target_CVw = 1, min_size = 30, max_size = 1500,
                                ess_method = "perplexity", weighting = "saturated")
  df$is_best_subset <- df$sim %in% tr$subset$sim
  df$weight_best <- 0
  df$weight_best[df$is_best_subset] <-
    MOSAIC:::.mosaic_best_subset_weights(df$likelihood[df$is_best_subset], "saturated")$weights
  # weight health is computed on the same frame and must not touch it
  before <- df
  wh <- calc_weight_health(df$likelihood, df$weight_best,
                           df[, c("beta_j0_tot_MOZ", "psi_star_a_MOZ", "gamma_1", "rho")],
                           n_obs = 3000, sim = df$sim)
  expect_identical(df, before)
  df$weight_all <- MOSAIC:::.mosaic_tempered_weights(df$likelihood, wh$coarsened$eta_ref)
  od <- withr::local_tempdir()
  q <- calc_model_posterior_quantiles(df, output_dir = od, verbose = FALSE)
  expect_identical(tr$n, golden$n)
  expect_identical(df$is_best_subset, golden$is_best_subset)
  expect_identical(df$weight_best, golden$weight_best)
  expect_identical(q, golden$quantiles)
})

test_that("weight_all is metadata, never a posterior parameter", {
  set.seed(1)
  n <- 300
  df <- data.frame(sim = seq_len(n), gamma_1 = runif(n, 0.05, 0.5), likelihood = rnorm(n),
                   is_finite = TRUE, is_retained = TRUE, is_best_subset = FALSE,
                   weight_best = 0, weight_all = 1 / n)
  df$is_best_subset[1:60] <- TRUE; df$weight_best[1:60] <- 1 / 60
  q <- calc_model_posterior_quantiles(df, output_dir = withr::local_tempdir(), verbose = FALSE)
  expect_false("weight_all" %in% q$parameter)
  expect_true("gamma_1" %in% q$parameter)
})

# ---- B2: weight_all is the declared coarsened posterior ----------------------

test_that("tempered weights are exp(eta * (ll - max ll)), normalised, zero for non-finite ll", {
  w <- MOSAIC:::.mosaic_tempered_weights(c(0, -2, -Inf, -4, NA), 0.5)
  raw <- c(1, exp(-1), 0, exp(-2), 0)
  expect_equal(w, raw / sum(raw), tolerance = 1e-15)
})

test_that("run_MOSAIC sets weight_all to the coarsened posterior, not the worst-draw floor", {
  body_txt <- gsub("\\s+", " ", paste(deparse(body(MOSAIC::run_MOSAIC)), collapse = " "))
  # "coarsened": weight_all IS the posterior; legacy: weight_all from weight health's eta_ref
  expect_true(grepl("results$weight_all <- coarsened_post$weights", body_txt, fixed = TRUE))
  expect_true(grepl("results$weight_best <- coarsened_post$member_weights", body_txt, fixed = TRUE))
  expect_true(grepl("results$weight_all <- .mosaic_tempered_weights(results$likelihood",
                    body_txt, fixed = TRUE))
  expect_true(grepl("weight_health$coarsened$eta_ref", body_txt, fixed = TRUE))
  expect_false(grepl("all_result <- .mosaic_calc_adaptive_gibbs_weights", body_txt, fixed = TRUE))
  # The tempered best-subset scheme still uses the adaptive weights (posterior path untouched).
  helper_txt <- paste(deparse(MOSAIC:::.mosaic_best_subset_weights), collapse = "\n")
  expect_true(grepl(".mosaic_calc_adaptive_gibbs_weights", helper_txt, fixed = TRUE))
  # weight health is computed on the weights used and reaches the diagnostics
  expect_true(grepl("weights = results[[w_used_col]],", body_txt, fixed = TRUE))
  expect_true(grepl("weight_health = weight_health", body_txt, fixed = TRUE))
  expect_true(grepl("batch_max = control$targets$batch_ratio_max", body_txt, fixed = TRUE))
})

test_that("coarsening_alpha is a validated control default", {
  expect_identical(mosaic_control_defaults()$targets$coarsening_alpha, 10)
  expect_identical(eval(formals(calc_weight_health)$coarsening_alpha), 10)
  ok <- MOSAIC:::.mosaic_validate_and_merge_control(
    mosaic_control_defaults(targets = list(coarsening_alpha = 6)))
  expect_identical(ok$targets$coarsening_alpha, 6)
  for (bad in list(0, -1, Inf, NA_real_, "15", c(10, 20)))
    expect_error(MOSAIC:::.mosaic_validate_and_merge_control(
      mosaic_control_defaults(targets = list(coarsening_alpha = bad))), "coarsening_alpha")
})

# ---- summary.json and convergence_diagnostics.json ---------------------------

test_that("summary.json carries the weight-health fields and no eta = 1 / k-hat fields", {
  set.seed(3)
  n <- 4000
  th <- runif(n)
  ll <- -3000 * (th - 0.5)^2
  wu <- numeric(n); wu[order(-ll)[1:100]] <- 1 / 100
  wh <- calc_weight_health(ll, wu, data.frame(theta = th), n_obs = 3000)
  wh$weights_used_column <- "weight_best"
  isd <- calc_is_diagnostics(ll, method = "perplexity")
  d <- calc_convergence_diagnostics(
    n_total = n, n_successful = n, n_retained = n, n_best_subset = 100,
    ess_best = 95, A_best = 0.9, cvw_best = 0.5, percentile_used = 2.5,
    convergence_tier = "tier_1", target_ess_best = 100, target_A_best = 0.7,
    target_cvw_best = 1, target_max_best_subset = 1000,
    is_diagnostics = list(best = isd, all = isd), weight_health = wh, verbose = FALSE)
  expect_null(d$importance_sampling)
  expect_match(d$likelihood_concentration$note, "not a fit or convergence diagnostic")
  expect_false(any(grepl("khat", names(unlist(d)), ignore.case = TRUE)))
  expect_identical(d$weight_health$status, wh$status)

  dirs <- MOSAIC:::.mosaic_ensure_dir_tree(withr::local_tempdir(), clean_output = FALSE)
  jsonlite::write_json(d, file.path(dirs$cal_diag, "convergence_diagnostics.json"),
                       pretty = TRUE, auto_unbox = TRUE, digits = NA)
  state <- list(mode = "fixed", converged = FALSE, batch_number = 1L,
                total_sims_run = n, total_sims_successful = n)
  cfg <- list(location_name = "MOZ", date_start = "2023-01-01", date_stop = "2023-12-31")
  s <- MOSAIC:::.mosaic_write_summary_json(dirs, state, Sys.time(), cfg,
                                           posthoc_criteria_met = TRUE,
                                           io = mosaic_control_defaults()$io)
  want <- c("ess_coarsened", "ess_coarsened_prop", "coarsening_alpha", "eta_ref", "n_obs_scored",
            "batch_ratio_coarsened_max", "batch_ratio_coarsened_median",
            "eta_star", "eta_star_n_eff", "eta_star_ess", "eta_star_batch_ratio_max",
            "weights_used_ess_kish", "weights_used_ess_perplexity", "weights_used_max_share",
            "weights_used_n", "weights_used_batch_ratio_max", "weights_used_batch_ratio_median",
            "weights_used_column", "weight_health_status", "weight_health_reasons",
            "likelihood_concentration_note")
  expect_true(all(want %in% names(s)))
  for (gone in c("ess_is_all", "ess_is_all_prop", "khat_all", "khat_all_status",
                 "ess_is_best", "ess_is_optimized", "n_positive_ratios_all"))
    expect_false(gone %in% names(s), info = gone)
  expect_equal(s$ess_coarsened, wh$coarsened$ess_kish, tolerance = 1e-10)
  expect_identical(s$weights_used_n, 100L)
  expect_identical(s$weight_health_status, wh$status)
  js <- jsonlite::read_json(file.path(dirs$results, "summary.json"))
  expect_equal(js$eta_ref, 10 / 3000, tolerance = 1e-12)
})

test_that("a summary without weight health still carries the fields, as NA", {
  dirs <- MOSAIC:::.mosaic_ensure_dir_tree(withr::local_tempdir(), clean_output = FALSE)
  state <- list(mode = "fixed", converged = FALSE, batch_number = 1L,
                total_sims_run = 10L, total_sims_successful = 10L)
  cfg <- list(location_name = "MOZ", date_start = "2023-01-01", date_stop = "2023-12-31")
  s <- MOSAIC:::.mosaic_write_summary_json(dirs, state, Sys.time(), cfg,
                                           io = mosaic_control_defaults()$io)
  expect_true("ess_coarsened" %in% names(s))
  expect_true(is.na(s$ess_coarsened))
  expect_true(is.na(s$weight_health_status))
})
