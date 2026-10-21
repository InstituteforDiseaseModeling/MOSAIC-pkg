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
  wh <- calc_weight_health(df$likelihood,
                           params = df[, c("beta_j0_tot_MOZ", "psi_star_a_MOZ", "gamma_1", "rho")],
                           n_obs = 3000, sim = df$sim)
  expect_identical(df, before)
  df$weight_all <- MOSAIC:::.mosaic_tempered_weights(df$likelihood, wh$coarsened$eta)
  od <- withr::local_tempdir()
  q <- calc_model_posterior_quantiles(df, output_dir = od, verbose = FALSE)
  expect_identical(tr$n, golden$n)
  expect_identical(df$is_best_subset, golden$is_best_subset)
  expect_identical(df$weight_best, golden$weight_best)
  # The posterior itself (weights, quantiles, moments) is bit-identical. The KL
  # column is a descriptive whose estimator changed in 1.4.0 (percentile scale
  # replaced the weighted KDE), so it is checked against the new estimator.
  keep <- setdiff(names(golden$quantiles), "kl")
  expect_identical(q[keep], golden$quantiles[keep])
  post <- q$type == "posterior"
  expect_true(all(q$kl[post] >= 0 & q$kl[post] <= log(20), na.rm = TRUE))
  fin <- df$is_finite
  p1 <- q$parameter[post][1]
  expect_identical(q$kl[post][1],
                   MOSAIC:::.mosaic_posterior_kl(df[[p1]][fin], df[[p1]][df$is_best_subset],
                                                 df$weight_best[df$is_best_subset]))
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
  # every scheme: weight_all is the coarsened posterior at eta = alpha / n_obs
  expect_true(grepl("results$weight_all <- .mosaic_tempered_weights(results$likelihood, eta_run)",
                    body_txt, fixed = TRUE))
  expect_true(grepl("results$weight_all <- coarsened_post$weights", body_txt, fixed = TRUE))
  expect_true(grepl("results$weight_best <- coarsened_post$member_weights", body_txt, fixed = TRUE))
  # "bestfit" members carry the best-fit selection's weights
  expect_true(grepl("results$weight_best[member_idx] <- sel$weights", body_txt, fixed = TRUE))
  expect_false(grepl("eta_ref", body_txt, fixed = TRUE))
  expect_false(grepl("weights_used", body_txt, fixed = TRUE))
  expect_false(grepl("all_result <- .mosaic_calc_adaptive_gibbs_weights", body_txt, fixed = TRUE))
  # The tempered best-subset scheme still uses the adaptive weights (posterior path untouched).
  helper_txt <- paste(deparse(MOSAIC:::.mosaic_best_subset_weights), collapse = "\n")
  expect_true(grepl(".mosaic_calc_adaptive_gibbs_weights", helper_txt, fixed = TRUE))
  # the status of the members used reaches the diagnostics, and no deprecated
  # calc_weight_health() argument is passed
  expect_true(grepl("weight_health = conv_health", body_txt, fixed = TRUE))
  expect_false(grepl("batch_max =", body_txt, fixed = TRUE))
  expect_false(grepl("ess_target =", body_txt, fixed = TRUE))
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

test_that("summary.json carries the 1.4.0 convergence fields and no eta = 1 fields", {
  set.seed(3)
  n <- 4000
  th <- runif(n)
  ll <- -3000 * (th - 0.5)^2
  wu <- numeric(n); wu[order(-ll)[1:100]] <- 1 / 100
  wh <- calc_weight_health(ll, params = data.frame(theta = th), n_obs = 3000)
  isd <- calc_is_diagnostics(ll, method = "perplexity")
  d <- calc_convergence_diagnostics(
    n_total = n, n_successful = n, n_retained = n, n_best_subset = 100,
    ess_best = 95, A_best = 0.9, cvw_best = 0.5, percentile_used = 2.5,
    convergence_tier = "tier_1", target_ess_best = 100, target_A_best = 0.7,
    target_cvw_best = 1, target_max_best_subset = 1000,
    is_diagnostics = list(best = isd, all = isd), weight_health = wh, verbose = FALSE)
  expect_null(d$importance_sampling)
  expect_match(d$likelihood_concentration$note, "not a fit or convergence diagnostic")
  # the only k-hat is the tempered one (the validity guard), never the eta = 1 k-hat
  khat_names <- grep("khat", names(unlist(d)), ignore.case = TRUE, value = TRUE)
  expect_true(all(grepl(paste0("^(weight_health\\.(coarsened\\.khat|convergence\\.|targets\\.thresholds\\.)|",
                               "metrics\\.khat\\.|targets\\.status_thresholds\\.|status\\.checks)"),
                        khat_names)), info = paste(khat_names, collapse = ", "))
  expect_identical(d$weight_health$coarsened$khat, calc_is_diagnostics((10 / 3000) * ll)$khat)
  expect_identical(d$weight_health$status, wh$status)

  dirs <- MOSAIC:::.mosaic_ensure_dir_tree(withr::local_tempdir(), clean_output = FALSE)
  state <- list(mode = "fixed", converged = FALSE, batch_number = 1L,
                total_sims_run = n, total_sims_successful = n)
  cfg <- list(location_name = "MOZ", date_start = "2023-01-01", date_stop = "2023-12-31")
  s <- MOSAIC:::.mosaic_write_summary_json(dirs, state, Sys.time(), cfg, conv_health = wh,
                                           weighting = "saturated",
                                           io = mosaic_control_defaults()$io)
  want <- names(MOSAIC:::.mosaic_weight_health_fields(wh))
  expect_true(all(want %in% names(s)))
  for (gone in c("ess_is_all", "ess_is_all_prop", "khat_all", "khat_all_status",
                 "ess_is_best", "ess_is_optimized", "n_positive_ratios_all",
                 # removed in schema 2 (SCHEMA_v2.md section 1)
                 "weights_used_ess_kish", "weights_used_column", "batch_ratio_coarsened_max",
                 "batch_ratio_coarsened_median", "ess_coarsened_prop", "eta_ref",
                 "weight_health_reasons", "likelihood_concentration_note", "resample_K"))
    expect_false(gone %in% names(s), info = gone)
  expect_equal(s$ess_coarsened, wh$coarsened$ess_kish, tolerance = 1e-10)
  expect_identical(s$weight_health_status, if (wh$status == "green") "ok" else "warn")
  expect_identical(s$convergence_status, wh$status)
  expect_identical(s$posthoc_criteria_met, identical(wh$status, "green"))
  expect_identical(s$posterior_weighting, "saturated")
  # every SCHEMA_v2 section 1 field and alias is present
  schema2 <- c("schema_version", "posterior_weighting", "convergence_status", "convergence_label",
               "convergence_reasons", "convergence_reasons_text", "convergence_flags",
               "convergence_flags_text", "check_ess_status", "check_mce_status", "check_khat_status",
               "check_informativeness_status", "stopping_rule_met", "mode", "coarsening_alpha", "eta",
               "n_obs_scored", "ess_all", "ess_all_target", "ess_best", "ess_best_target",
               "best_subset_ratio", "best_subset_mass", "ensemble_members", "ensemble_members_unique",
               "ensemble_binding", "mce_median", "mce_p90", "mce_max", "khat_tempered", "khat_status",
               "eta_star", "eta_star_n_eff", "flat_likelihood", "ess_growth_slope", "sims_used",
               "sims_projected_for_target", "budget_exhausted", "param_ess_min", "param_ess_median",
               "converged", "convergence_evaluated", "posthoc_criteria_met", "weight_health_status",
               "ess_coarsened", "n_best_subset", "ess_min", "ess_median", "ess_target")
  expect_true(all(schema2 %in% names(s)), info = paste(setdiff(schema2, names(s)), collapse = ", "))
  expect_identical(s$ess_target, s$ess_best_target)
  expect_identical(s$n_best_subset, s$ensemble_members)
  js <- jsonlite::read_json(file.path(dirs$results, "summary.json"))
  expect_equal(js$eta, 10 / 3000, tolerance = 1e-12)
  expect_equal(js$ess_best, wh$bestfit$ess_best, tolerance = 1e-10)
  expect_true(is.list(js$convergence_reasons))
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
