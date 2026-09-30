# =============================================================================
# test-review-runmosaic-score-window.R
#
# score_start_cases later than the deaths start must move the CASES scored
# window, not just the dispersion estimate. The worker slices every array to
# s = min(idx_cases, idx_deaths); the residual cases prefix s:(idx_cases-1)
# must not contribute to the likelihood.
# =============================================================================

.sw_fixture <- function(idx_cases, idx_deaths) {
  cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = "MOZ")
  for (f in c("reported_cases_weight", "reported_deaths_weight")) cfg[[f]] <- NULL
  oc <- cfg$reported_cases; od <- cfg$reported_deaths
  oc[!is.finite(oc)] <- 0; od[!is.finite(od)] <- 0
  cfg$reported_cases <- oc; cfg$reported_deaths <- od
  nT <- ncol(cfg$reported_cases)
  sw <- list(idx_cases = as.integer(idx_cases), idx_deaths = as.integer(idx_deaths), n_time = nT)
  ls <- list(weight_cases = 1, weight_deaths = 1, eps_rel_cases = 0.02, eps_rel_deaths = 0.25,
             .nb_k_cases_resolved = 0.5, .nb_k_deaths_resolved = 1, .score_window_resolved = sw,
             weights_location = 1, weight_peak_timing = 0, weight_peak_magnitude = 0,
             weight_cumulative_total = 1, weight_wis = 0, sigma_peak_time = 1, sigma_peak_log = 0.5)
  list(cfg = cfg, ls = ls, oc = oc, od = od, nT = nT)
}

.sw_worker_ll <- function(fx, est_cases) {
  stub <- function(config, seed = NULL, quiet = TRUE, ...) {
    list(results = list(reported_cases = est_cases, reported_deaths = round(fx$od * 0.8) + 1))
  }
  testthat::local_mocked_bindings(sample_parameters = function(...) fx$cfg,
                                  run_simulation = stub, .package = "MOSAIC")
  m <- MOSAIC:::.mosaic_run_simulation_worker(
    sim_id = 1L, n_iterations = 1L, priors = NULL, config = fx$cfg, PATHS = NULL,
    dir_cal_samples = tempdir(), param_names_all = "gamma_1",
    param_lookup = MOSAIC:::.mosaic_build_param_lookup("gamma_1", fx$cfg$location_name),
    sampling_args = list(), io = NULL, likelihood_settings = fx$ls, write_shard = FALSE)
  unname(m[1, "likelihood"])
}

test_that("cases cells before a later score_start_cases do not enter the likelihood", {
  fx <- .sw_fixture(idx_cases = 200L, idx_deaths = 31L)
  base <- round(fx$oc * 1.1) + 1
  perturbed <- base
  perturbed[, 31:199] <- perturbed[, 31:199] * 50 + 500   # only the excluded cases prefix

  ll_base <- .sw_worker_ll(fx, base)
  ll_pert <- .sw_worker_ll(fx, perturbed)
  expect_true(is.finite(ll_base))
  expect_identical(ll_base, ll_pert)

  # Control: the same perturbation INSIDE the scored cases window must matter.
  inside <- base
  inside[, 200:260] <- inside[, 200:260] * 50 + 500
  expect_false(isTRUE(all.equal(ll_base, .sw_worker_ll(fx, inside))))
})

test_that("equal cases/deaths starts are unaffected by the cases-prefix mask", {
  fx <- .sw_fixture(idx_cases = 31L, idx_deaths = 31L)
  base <- round(fx$oc * 1.1) + 1
  pert <- base
  pert[, 31:60] <- pert[, 31:60] * 50 + 500
  expect_false(isTRUE(all.equal(.sw_worker_ll(fx, base), .sw_worker_ll(fx, pert))))
})

test_that("the resolver lets score_start_cases move the cases start past the deaths start", {
  cfg <- list(date_start = "2023-01-01", reported_cases = matrix(0, 1, 365))
  sw <- MOSAIC:::.mosaic_resolve_score_window(
    cfg, list(likelihood = list(burn_in_days = 30, score_start_cases = "2023-04-11")))
  expect_identical(sw$idx_cases, 101L)
  expect_identical(sw$idx_deaths, 31L)
})
