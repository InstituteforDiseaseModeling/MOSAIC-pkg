# =============================================================================
# test-worker-deaths-integration.R
#
# The calibration worker must score deaths with the reported CFR integrated out
# whenever run_MOSAIC() resolved the integration (likelihood_settings
# $.deaths_integration): the worker's likelihood moves one-for-one with the
# integrated deaths log-likelihood, and eps_rel_deaths -- the floor of the
# retired daily NB deaths score -- no longer matters. Without the integration
# the worker falls back to that NB score, which does depend on eps_rel_deaths
# (so the test can tell the two apart).
# =============================================================================

.worker_fixture <- function() {
  cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = "MOZ")
  for (f in c("reported_cases_weight", "reported_deaths_weight")) cfg[[f]] <- NULL
  oc <- cfg$reported_cases; od <- cfg$reported_deaths
  oc[!is.finite(oc)] <- 0; od[!is.finite(od)] <- 0
  stub <- function(config, seed = NULL, quiet = TRUE, ...) {
    list(results = list(reported_cases = round(oc * 1.2), reported_deaths = round(od * 0.4),
                        new_symptomatic = round(oc * 1.2 * config$chi_epidemic / config$rho)))
  }
  nT <- ncol(cfg$reported_deaths)
  sw <- list(idx_cases = 31L, idx_deaths = 31L, n_time = nT)
  ls <- list(weight_cases = 1, weight_deaths = 1, eps_rel_cases = 0.02, eps_rel_deaths = 0.25,
             .nb_k_cases_resolved = 0.5, .nb_k_deaths_resolved = 1, .score_window_resolved = sw,
             weights_location = 1, weight_peak_timing = 0, weight_peak_magnitude = 0,
             weight_cumulative_total = 0, weight_wis = 0, sigma_peak_time = 1, sigma_peak_log = 0.5)
  pri <- MOSAIC::get_location_priors("MOZ", MOSAIC::priors_default)
  ls$.deaths_integration <- MOSAIC:::.mosaic_resolve_deaths_integration(
    cfg, list(likelihood = ls), pri, score_window = sw)
  list(cfg = cfg, stub = stub, ls = ls)
}

.worker_ll <- function(fx, ls) {
  m <- MOSAIC:::.mosaic_run_simulation_worker(
    sim_id = 1L, n_iterations = 1L, priors = NULL, config = fx$cfg, PATHS = NULL,
    dir_cal_samples = tempdir(), param_names_all = "gamma_1",
    param_lookup = MOSAIC:::.mosaic_build_param_lookup("gamma_1", fx$cfg$location_name),
    sampling_args = list(), io = NULL, likelihood_settings = ls, write_shard = FALSE)
  unname(m[1, "likelihood"])
}

test_that("the worker's likelihood moves one-for-one with the integrated deaths score", {
  fx <- .worker_fixture()
  core <- -100
  local_mocked_bindings(
    sample_parameters = function(...) fx$cfg,
    run_simulation = fx$stub,
    .mosaic_deaths_ll_integrated = function(di, results, params) list(ll = core),
    .package = "MOSAIC")
  ll_a <- .worker_ll(fx, fx$ls)
  core <- -90
  ll_b <- .worker_ll(fx, fx$ls)
  expect_true(is.finite(ll_a))
  expect_equal(ll_b - ll_a, 10, tolerance = 1e-8)
})

test_that("with the integration, eps_rel_deaths no longer changes the worker's likelihood", {
  fx <- .worker_fixture()
  local_mocked_bindings(sample_parameters = function(...) fx$cfg, run_simulation = fx$stub,
                        .package = "MOSAIC")
  ls_lo <- fx$ls; ls_lo$eps_rel_deaths <- 0.001
  expect_identical(.worker_ll(fx, fx$ls), .worker_ll(fx, ls_lo))
  # Without it, the retired NB deaths score is used and eps_rel_deaths matters.
  nd <- fx$ls; nd$.deaths_integration <- NULL
  nd_lo <- nd; nd_lo$eps_rel_deaths <- 0.001
  expect_false(isTRUE(all.equal(.worker_ll(fx, nd), .worker_ll(fx, nd_lo))))
  expect_false(isTRUE(all.equal(.worker_ll(fx, nd), .worker_ll(fx, fx$ls))))
})
