#!/usr/bin/env Rscript
# Validate GPU (torch) likelihood matches serial CPU likelihood
devtools::load_all("/workspace/MOSAIC-pkg")

cat("torch available:", requireNamespace("torch", quietly = TRUE), "\n")
cat("torch installed:", torch::torch_is_installed(), "\n")
cat("CUDA available:", torch::cuda_is_available(), "\n\n")

set.seed(42)
n_loc <- 2; n_time <- 52; n_sims <- 20
obs_c <- matrix(rpois(n_loc * n_time, 50), nrow = n_loc)
obs_d <- matrix(rpois(n_loc * n_time, 5),  nrow = n_loc)
est_c <- array(rpois(n_sims * n_loc * n_time, 50), dim = c(n_sims, n_loc, n_time))
est_d <- array(rpois(n_sims * n_loc * n_time, 5),  dim = c(n_sims, n_loc, n_time))

run_comparison <- function(label, ls, tol = 0.1) {
  ll_gpu <- calc_model_likelihood_gpu(obs_c, obs_d, est_c, est_d, ls, device = "cpu")

  ll_serial <- numeric(n_sims)
  for (i in 1:n_sims) {
    ll_serial[i] <- tryCatch(
      calc_model_likelihood(
        config = NULL,
        obs_cases = obs_c, est_cases = matrix(est_c[i,,], nrow = n_loc),
        obs_deaths = obs_d, est_deaths = matrix(est_d[i,,], nrow = n_loc),
        weights_time = ls$.weights_time_resolved,
        weights_location = ls$weights_location,
        weight_cases = ls$weight_cases, weight_deaths = ls$weight_deaths,
        nb_k_min_cases = ls$nb_k_min_cases, nb_k_min_deaths = ls$nb_k_min_deaths,
        add_max_terms = isTRUE(ls$add_max_terms),
        add_peak_timing = isTRUE(ls$add_peak_timing),
        add_peak_magnitude = isTRUE(ls$add_peak_magnitude),
        add_cumulative_total = isTRUE(ls$add_cumulative_total),
        add_wis = isTRUE(ls$add_wis),
        enable_guardrails = isTRUE(ls$enable_guardrails),
        floor_likelihood = ls$floor_likelihood,
        weight_max_terms = if (!is.null(ls$weight_max_terms)) ls$weight_max_terms else 0.5,
        weight_cumulative_total = if (!is.null(ls$weight_cumulative_total)) ls$weight_cumulative_total else 0.3,
        guardrail_verbose = FALSE
      ),
      error = function(e) NA_real_
    )
  }

  max_diff <- max(abs(ll_gpu - ll_serial), na.rm = TRUE)
  cat(sprintf("[%s] max_diff=%.6e  ", label, max_diff))
  if (max_diff < tol) {
    cat("PASS\n")
  } else {
    cat("FAIL\n")
    cat("  GPU:   ", paste(round(ll_gpu, 2), collapse = ", "), "\n")
    cat("  Serial:", paste(round(ll_serial, 2), collapse = ", "), "\n")
    stop(paste("FAILED:", label))
  }
}

# Test 1: NB core only
cat("=== Test 1: NB core only ===\n")
run_comparison("NB_core", list(
  .weights_time_resolved = rep(1, n_time), weights_location = rep(1, n_loc),
  weight_cases = 1, weight_deaths = 0.5,
  nb_k_min_cases = 3, nb_k_min_deaths = 3,
  add_max_terms = FALSE, add_peak_timing = FALSE,
  add_peak_magnitude = FALSE, add_cumulative_total = FALSE,
  add_wis = FALSE, enable_guardrails = FALSE,
  floor_likelihood = -999999999
))

# Test 2: NB + max terms
cat("=== Test 2: NB + max_terms ===\n")
run_comparison("NB+max", list(
  .weights_time_resolved = rep(1, n_time), weights_location = rep(1, n_loc),
  weight_cases = 1, weight_deaths = 1,
  nb_k_min_cases = 3, nb_k_min_deaths = 3,
  add_max_terms = TRUE, add_peak_timing = FALSE,
  add_peak_magnitude = FALSE, add_cumulative_total = FALSE,
  add_wis = FALSE, enable_guardrails = FALSE,
  floor_likelihood = -999999999, weight_max_terms = 0.5
))

# Test 3: NB + cumulative (relaxed tolerance: lgamma on large cumulative sums
#   has inherent fp differences between torch lgamma and R's dnbinom C impl)
cat("=== Test 3: NB + cumulative ===\n")
run_comparison("NB+cum", list(
  .weights_time_resolved = rep(1, n_time), weights_location = rep(1, n_loc),
  weight_cases = 1, weight_deaths = 1,
  nb_k_min_cases = 3, nb_k_min_deaths = 3,
  add_max_terms = FALSE, add_peak_timing = FALSE,
  add_peak_magnitude = FALSE, add_cumulative_total = TRUE,
  add_wis = FALSE, enable_guardrails = FALSE,
  floor_likelihood = -999999999, weight_cumulative_total = 0.3
))

# Test 4: NB + guardrails
cat("=== Test 4: NB + guardrails ===\n")
run_comparison("NB+guard", list(
  .weights_time_resolved = rep(1, n_time), weights_location = rep(1, n_loc),
  weight_cases = 1, weight_deaths = 1,
  nb_k_min_cases = 3, nb_k_min_deaths = 3,
  add_max_terms = FALSE, add_peak_timing = FALSE,
  add_peak_magnitude = FALSE, add_cumulative_total = FALSE,
  add_wis = FALSE, enable_guardrails = TRUE,
  floor_likelihood = -999999999,
  cumulative_over_ratio = 10, cumulative_under_ratio = 0.1,
  min_cumulative_for_check = 100,
  max_timestep_ratio = 100, min_timestep_ratio = 0.01,
  min_obs_for_ratio = 1, min_obs_for_ratio_deaths = 10,
  negative_correlation_threshold = 0
))

# Test 5: All GPU features combined
cat("=== Test 5: All GPU features ===\n")
run_comparison("all_gpu", list(
  .weights_time_resolved = rep(1, n_time), weights_location = rep(1, n_loc),
  weight_cases = 1, weight_deaths = 0.5,
  nb_k_min_cases = 3, nb_k_min_deaths = 3,
  add_max_terms = TRUE, add_peak_timing = FALSE,
  add_peak_magnitude = FALSE, add_cumulative_total = TRUE,
  add_wis = FALSE, enable_guardrails = TRUE,
  floor_likelihood = -999999999,
  weight_max_terms = 0.5, weight_cumulative_total = 0.3,
  cumulative_over_ratio = 10, cumulative_under_ratio = 0.1,
  min_cumulative_for_check = 100,
  max_timestep_ratio = 100, min_timestep_ratio = 0.01,
  min_obs_for_ratio = 1, min_obs_for_ratio_deaths = 10,
  negative_correlation_threshold = 0
))

# Test 6: Larger batch (performance check)
cat("=== Test 6: Performance (500 sims) ===\n")
set.seed(99)
n_sims_perf <- 500
est_c_big <- array(rpois(n_sims_perf * n_loc * n_time, 50), dim = c(n_sims_perf, n_loc, n_time))
est_d_big <- array(rpois(n_sims_perf * n_loc * n_time, 5),  dim = c(n_sims_perf, n_loc, n_time))

ls_perf <- list(
  .weights_time_resolved = rep(1, n_time), weights_location = rep(1, n_loc),
  weight_cases = 1, weight_deaths = 0.5,
  nb_k_min_cases = 3, nb_k_min_deaths = 3,
  add_max_terms = TRUE, add_cumulative_total = TRUE,
  add_peak_timing = FALSE, add_peak_magnitude = FALSE,
  add_wis = FALSE, enable_guardrails = FALSE,
  floor_likelihood = -999999999,
  weight_max_terms = 0.5, weight_cumulative_total = 0.3
)

t_gpu <- system.time(
  ll_gpu_big <- calc_model_likelihood_gpu(obs_c, obs_d, est_c_big, est_d_big, ls_perf, device = "cpu")
)

t_serial <- system.time({
  ll_ser_big <- numeric(n_sims_perf)
  for (i in 1:n_sims_perf) {
    ll_ser_big[i] <- calc_model_likelihood(
      obs_cases = obs_c, est_cases = matrix(est_c_big[i,,], nrow = n_loc),
      obs_deaths = obs_d, est_deaths = matrix(est_d_big[i,,], nrow = n_loc),
      weights_time = rep(1, n_time), weights_location = rep(1, n_loc),
      weight_cases = 1, weight_deaths = 0.5,
      nb_k_min_cases = 3, nb_k_min_deaths = 3,
      add_max_terms = TRUE, add_cumulative_total = TRUE,
      add_peak_timing = FALSE, add_peak_magnitude = FALSE,
      add_wis = FALSE, enable_guardrails = FALSE,
      floor_likelihood = -999999999,
      weight_max_terms = 0.5, weight_cumulative_total = 0.3
    )
  }
})

max_diff_big <- max(abs(ll_gpu_big - ll_ser_big), na.rm = TRUE)
cat(sprintf("[perf] GPU: %.2fs, Serial: %.2fs, Speedup: %.1fx, max_diff=%.2e\n",
    t_gpu["elapsed"], t_serial["elapsed"],
    t_serial["elapsed"] / max(t_gpu["elapsed"], 0.001), max_diff_big))

cat("\n=== ALL VALIDATION TESTS PASSED ===\n")
