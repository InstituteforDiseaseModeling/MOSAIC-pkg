# Stubbed single-location run_MOSAIC() for the posterior regression tests
# (test-posterior-stub-runs.R). The engine is replaced by a deterministic stub
# whose output depends on the sampled beta_j0_tot and psi_star_a, so draws
# differ and the likelihood ranks them. The run is sequential: a mocked binding
# exists only in this process.
#
# claude/weights_investigation/scripts/19_golden_stub_fixture.R sources this
# file against the v1.2.1 source tree to produce
# fixtures/stub_posterior_golden_v121.rds, and
# claude/plan_convergence/ws2_golden_v130.R sources its WS1-era copy against the
# v1.3.0 tree for fixtures/stub_posterior_golden_v130.rds, so each fixture and
# its test run the same calibration.

.stub_run_mosaic <- function(dir_output, weighting = NULL, mode = c("fixed", "auto"),
                             n_sims = 150L, optimize_subset = NULL, targets_extra = list(),
                             plots = FALSE, ll_mode = c("normal", "onegood"),
                             calib_extra = list(), predictions_extra = list(), keep_tier = FALSE,
                             assign_targets = FALSE) {
  mode <- match.arg(mode)
  ll_mode <- match.arg(ll_mode)
  config <- get_location_config(iso = "MOZ", config = MOSAIC::config_default)
  drop <- c("metadata", "zeta_ratio", "decay_days_spread", "reported_cases_weight",
            "reported_deaths_weight", "reported_tier", "output_file_path")
  if (isTRUE(keep_tier)) drop <- setdiff(drop, "reported_tier")
  for (f in drop) config[[f]] <- NULL
  priors <- get_location_priors(iso = "MOZ", priors = MOSAIC::priors_default)
  n_loc <- length(config$location_name)
  n_t <- as.integer(as.Date(config$date_stop) - as.Date(config$date_start)) + 1L
  oc <- matrix(as.numeric(config$reported_cases), nrow = n_loc)
  od <- matrix(as.numeric(config$reported_deaths), nrow = n_loc)
  oc[!is.finite(oc)] <- 0; od[!is.finite(od)] <- 0
  fake_engine <- function(config, seed = NULL, quiet = FALSE, ...) {
    s <- if (!is.null(seed)) as.numeric(seed)[1] else 1
    b <- tryCatch(as.numeric(config$beta_j0_tot)[1], error = function(e) 0); if (!is.finite(b)) b <- 0
    g <- tryCatch(as.numeric(config$psi_star_a)[1], error = function(e) 1); if (!is.finite(g)) g <- 1
    mult <- max(1 + 0.4 * sin(s * 0.7 + b * 3e4) + 0.3 * (g - 1), 0.05)
    cases <- matrix(oc * mult, n_loc, n_t); deaths <- matrix(od * mult, n_loc, n_t)
    cases[] <- pmax(0, round(cases)); deaths[] <- pmax(0, round(deaths))
    # "onegood": every simulation but seed 7 fails (non-finite output), the
    # degenerate run of red team round 1 (C2).
    if (identical(ll_mode, "onegood") && s != 7) { cases[] <- NA_real_; deaths[] <- NA_real_ }
    list(params = config, results = list(reported_cases = cases, reported_deaths = deaths,
         new_symptomatic = round(cases * config$chi_epidemic / config$rho)), seed = as.integer(s))
  }
  # min/max_best_subset bound the legacy "saturated" tier search (the 1.2.1
  # golden run); the other schemes ignore them.
  targets <- list(min_best_subset = 10L, max_best_subset = 60L)
  if (!is.null(weighting)) targets$posterior_weighting <- weighting
  targets[names(targets_extra)] <- targets_extra
  calib <- if (identical(mode, "auto")) {
    list(n_simulations = NULL, n_iterations = 1L, batch_size_adaptive = 60L,
         min_batches_adaptive = 3L, min_draws = 180L, max_simulations_total = 420L,
         max_batch_predictive = 120L)
  } else list(n_simulations = as.integer(n_sims), n_iterations = 1L)
  calib[names(calib_extra)] <- calib_extra
  predictions <- list(n_iter_ensemble = 1L, n_iter_best = 1L, capture_trajectories = FALSE)
  if (!is.null(optimize_subset)) predictions$optimize_subset <- optimize_subset
  predictions[names(predictions_extra)] <- predictions_extra
  # assign_targets = TRUE builds the control the way most launchers do (the
  # lesson #13 pattern): the full defaults, then the targets assigned into them.
  control <- mosaic_control_defaults(
    calibration = calib, targets = if (isTRUE(assign_targets)) NULL else targets,
    predictions = predictions,
    parallel = list(enable = FALSE, n_cores = 1L, progress = FALSE),
    paths = list(plots = plots, clean_output = TRUE), logging = list(verbose = FALSE))
  if (isTRUE(assign_targets)) for (k in names(targets)) control$targets[[k]] <- targets[[k]]
  testthat::with_mocked_bindings(
    suppressWarnings(suppressMessages(run_MOSAIC(config = config, priors = priors,
                                                 dir_output = dir_output, control = control,
                                                 resume = FALSE))),
    run_simulation = fake_engine, .package = "MOSAIC")
  invisible(dir_output)
}

# The pieces of a finished run the posterior regression compares.
.stub_posterior_snapshot <- function(dir_output) {
  smp <- as.data.frame(arrow::read_parquet(file.path(dir_output, "2_calibration", "samples.parquet")))
  keep <- intersect(c("sim", "likelihood", "is_best_subset", "weight_best",
                      "is_best_subset_opt", "weight_best_opt", "weight_retained"), names(smp))
  q <- utils::read.csv(file.path(dir_output, "2_calibration", "posterior", "posterior_quantiles.csv"),
                       stringsAsFactors = FALSE)
  # v1.2.1 wrote a spurious weight_all row (fixed in 1.3.0); no real row changed.
  q <- q[q$parameter != "weight_all", ]
  rownames(q) <- NULL
  post <- jsonlite::read_json(file.path(dir_output, "2_calibration", "posterior", "posteriors.json"))
  post$metadata <- NULL   # carries the run directory path and a timestamp
  pick <- function(f) {
    e <- readRDS(file.path(dir_output, "2_calibration", f))
    e[intersect(c("cases_median", "cases_mean", "deaths_median", "deaths_mean", "ci_bounds",
                  "predictive_median", "parameter_weights", "seeds", "n_param_sets"), names(e))]
  }
  medoid <- file.path(dir_output, "2_calibration", "best_model", "config_medoid.json")
  list(samples = smp[, keep], quantiles = q, posteriors = post,
       ensemble_candidate = pick("ensemble_candidate.rds"),
       ensemble_optimized = pick("ensemble_optimized.rds"),
       medoid_seed = if (file.exists(medoid)) jsonlite::read_json(medoid)$seed else NULL)
}
