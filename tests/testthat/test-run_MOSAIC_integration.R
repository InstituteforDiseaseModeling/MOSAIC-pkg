# Integration test for run_MOSAIC()'s Bayesian (BFRS) calibration loop.
#
# Closes review gap B2-10: run_MOSAIC()'s end-to-end orchestration had no
# automated coverage. This test exercises the REAL R pipeline ---
#   parameter sampling -> batched simulation dispatch -> R-side likelihood
#   -> outlier/subset selection -> importance weights -> posterior ensemble
#   -> best/medoid reruns -> summary.json + return contract
# --- and stubs ONLY the transmission engine.
#
# The stub seam is run_simulation() in the MOSAIC namespace: the calibration
# worker and the post-calibration ensemble both reach the engine through it and
# nothing else, so one local_mocked_bindings() covers the whole pipeline.
#
# CRITICAL: a mocked binding exists only in this process, so the run MUST be
# SEQUENTIAL (control$parallel$enable = FALSE, n_cores = 1). PSOCK workers are
# separate processes that load MOSAIC afresh and would call the real engine.

# A handful of fixed simulations through the stub is fast (the synthetic engine
# call is a no-op matrix build), but parameter sampling for 40 locations x ~1278
# timesteps is not free. Gate behind an opt-in env var so the default test run
# stays quick; flip MOSAIC_RUN_INTEGRATION=1 to exercise it.
run_integration <- nzchar(Sys.getenv("MOSAIC_RUN_INTEGRATION"))

test_that("run_MOSAIC drives a full BFRS calibration on a stubbed simulation engine", {
  skip_on_cran()
  skip_if_not(run_integration,
              "set MOSAIC_RUN_INTEGRATION=1 to run the run_MOSAIC integration test")
  skip_if_not_installed("withr")
  skip_if_not_installed("arrow")
  skip_if_not_installed("jsonlite")

  # run_MOSAIC hard-requires options(root_directory), but everything this test
  # reads is packaged data, so a scoped root (the package dir when ~/MOSAIC is
  # absent, as on CI) is enough -- see local_test_root() in helper-skips.R.
  local_test_root()

  # ---- config / priors -----------------------------------------------------
  # config_default already validates through make_simulation_config; strip the
  # non-signature tracking fields (same shim as test-config_default.R).
  config <- MOSAIC::config_default
  config$metadata          <- NULL
  config$zeta_ratio        <- NULL
  config$decay_days_spread <- NULL
  config$reported_cases_weight  <- NULL
  config$reported_deaths_weight <- NULL
  config$reported_tier          <- NULL
  config$output_file_path  <- NULL

  priors <- MOSAIC::priors_default

  n_loc <- length(config$location_name)
  n_t   <- as.integer(as.Date(config$date_stop) - as.Date(config$date_start)) + 1L

  # The observed surveillance matrices are the comparison target for the R-side
  # likelihood. Replace NA (missing weeks) with 0 so the synthetic prediction is
  # a finite, non-negative function of the observed signal.
  obs_cases_base  <- config$reported_cases
  obs_deaths_base <- config$reported_deaths
  storage.mode(obs_cases_base)  <- "double"
  storage.mode(obs_deaths_base) <- "double"
  obs_cases_base[!is.finite(obs_cases_base)]   <- 0
  obs_deaths_base[!is.finite(obs_deaths_base)] <- 0

  # ---- stubbed transmission engine -----------------------------------------
  # The seam is run_simulation() in the MOSAIC namespace: both the calibration
  # worker and .mosaic_ensemble_sim_task() reach the engine through it and
  # nothing else, so one mocked binding covers the whole pipeline. (Before the
  # R engine this test parked a fake Python module in .GlobalEnv$lc, which
  # worked only because every call site duplicated the same
  # `exists("lc", .GlobalEnv)` lookup.)
  #
  # Closure-captured counter proves the loop actually dispatched sims here.
  call_env <- new.env(parent = emptyenv())
  call_env$n <- 0L
  call_env$seeds <- integer(0)

  fake_engine <- function(config, seed = NULL, quiet = FALSE, ...) {
    call_env$n <- call_env$n + 1L

    # The config carries the per-iteration seed and the once-per-sim sampled
    # transmission parameter beta_j0_tot. Derive a deterministic, bounded
    # multiplier so DIFFERENT sims/iterations produce DIFFERENT predictions
    # (and therefore different, non-degenerate likelihoods) while staying close
    # enough to the observed signal that the negative-binomial likelihood is
    # finite.
    seed_val <- if (!is.null(seed)) as.numeric(seed)[1] else
      tryCatch(as.numeric(config$seed)[1], error = function(e) 1)
    if (!is.finite(seed_val)) seed_val <- 1
    call_env$seeds <- c(call_env$seeds, as.integer(seed_val))

    beta_val <- tryCatch(as.numeric(config$beta_j0_tot)[1], error = function(e) NA_real_)
    if (!is.finite(beta_val)) beta_val <- 0

    # Multiplier in roughly [0.6, 1.4]; deterministic in (seed, beta).
    mult <- 1 + 0.4 * sin(seed_val * 0.7 + beta_val * 3.0)

    cases  <- matrix(obs_cases_base  * mult, nrow = n_loc, ncol = n_t)
    deaths <- matrix(obs_deaths_base * mult, nrow = n_loc, ncol = n_t)
    # Engine returns rounded reported counts; mimic non-negative integers.
    cases[]  <- pmax(0, round(cases))
    deaths[] <- pmax(0, round(deaths))

    # Symptomatic onsets consistent with the cases (reported = rho/chi * onsets):
    # the integrated deaths likelihood and the ensemble's post-hoc death redraw
    # both take the path's onsets as their exposure.
    onsets <- round(cases * config$chi_epidemic / config$rho)

    list(params = config,
         results = list(reported_cases = cases, reported_deaths = deaths,
                        new_symptomatic = onsets),
         seed = as.integer(seed_val))
  }

  local_mocked_bindings(run_simulation = fake_engine, .package = "MOSAIC")

  # ---- control: smallest meaningful fixed-mode calibration -----------------
  # Fixed mode (n_simulations = integer) runs exactly N sims in a single batch
  # through the in-process worker -- the most direct, fast exercise of the BFRS
  # dispatch path. Permissive subset targets + tiny ensemble reruns keep the
  # post-calibration phase cheap; n_iter_* = 1 minimises stubbed reruns.
  dir_output <- withr::local_tempdir()

  # 60 is the smallest fixed pool that clears the pipeline's hard floors: the
  # post-calibration parameter-ESS step (calc_model_ess_parameter) requires
  # >= 50 valid samples, and grid_search_best_subset needs min_best_subset (>= 10
  # by control validation) candidates. 60 leaves headroom above the 50 ESS floor.
  n_sims <- 60L
  control <- mosaic_control_defaults(
    calibration = list(
      n_simulations = n_sims,  # fixed mode: exactly this many sims, one batch
      n_iterations  = 1L       # 1 stochastic rerun per sim (keeps it fast)
    ),
    targets = list(
      # Cap the best-subset search at the ESS floor (50). Tukey outlier fencing
      # on the synthetic likelihoods retains ~50 of the 60 sims, so capping at 50
      # keeps the grid search inside the retained pool (and avoids a noisy
      # "max_size exceeds available simulations" warning per tier).
      min_best_subset = 10L,
      max_best_subset = 50L
    ),
    predictions = list(
      n_iter_ensemble = 1L,
      n_iter_best     = 1L
    ),
    parallel = list(enable = FALSE, n_cores = 1L, progress = FALSE),
    paths    = list(plots = FALSE, clean_output = FALSE),
    logging  = list(verbose = FALSE)
  )

  # ---- run ------------------------------------------------------------------
  # suppressWarnings: driving the REAL pipeline on synthetic simulation output emits
  # expected, data-driven warnings that are orthogonal to the orchestration flow
  # under test -- e.g. fit-quality warnings on the synthetic counts (they are
  # not epidemiologically calibrated) and arrow's "set_io_thread_count() with num_threads < 2" note from run_MOSAIC's
  # thread pinning. Errors still propagate and fail the test.
  result <- suppressWarnings(run_MOSAIC(
    config     = config,
    priors     = priors,
    dir_output = dir_output,
    control    = control,
    resume     = FALSE
  ))

  # ===========================================================================
  # ASSERTIONS
  # ===========================================================================

  # (1) The stub was actually invoked: the BFRS loop dispatched sims through it.
  # Calibration alone fires n_sims calls (n_sims sims x 1 iter). The
  # post-calibration ensemble + medoid reruns add more, so we expect
  # strictly more than the calibration count -- proving both phases dispatched
  # through the stub (calibration AND posterior-ensemble/medoid).
  expect_gt(call_env$n, 0L)
  expect_gte(call_env$n, n_sims)         # >= calibration sims
  expect_gt(call_env$n, n_sims)          # ensemble/medoid reruns dispatched too

  # (2) Return contract.
  expect_type(result, "list")
  expect_named(result, c("dirs", "files", "summary"))
  expect_true(isTRUE(result$summary$converged) || isFALSE(result$summary$converged))
  expect_equal(result$summary$sims_total, n_sims)   # fixed target met exactly
  expect_gt(result$summary$sims_success, 0L)        # stub yielded finite likelihoods
  expect_true(is.finite(result$summary$runtime_min))

  # (3) On-disk output structure: the three top-level run dirs exist.
  expect_true(dir.exists(file.path(dir_output, "1_inputs")))
  expect_true(dir.exists(file.path(dir_output, "2_calibration")))
  expect_true(dir.exists(file.path(dir_output, "3_results")))

  # (4) Key calibration artifacts written by the BFRS loop.
  samples_file <- file.path(dir_output, "2_calibration", "samples.parquet")
  expect_true(file.exists(samples_file))
  expect_true(file.exists(file.path(dir_output, "1_inputs", "config.json")))
  expect_true(file.exists(file.path(dir_output, "1_inputs", "priors.json")))
  expect_true(file.exists(
    file.path(dir_output, "2_calibration", "diagnostics", "parameter_ess.csv")))
  # The best model is no longer produced: config_best.json must NOT be written.
  # The medoid config IS the representative single-config model (written when a
  # medoid seed was identified, which it is for this fixture).
  expect_false(file.exists(
    file.path(dir_output, "2_calibration", "best_model", "config_best.json")))
  expect_true(file.exists(
    file.path(dir_output, "2_calibration", "best_model", "config_medoid.json")))

  # (5) samples.parquet holds exactly the fixed pool with the loop's derived
  # columns and a spread of FINITE likelihoods (degenerate likelihoods would
  # mean the stub did not vary across sims -> the loop was not meaningfully
  # exercised).
  samples <- arrow::read_parquet(samples_file)
  expect_equal(nrow(samples), n_sims)
  expect_true(all(c("sim", "likelihood", "is_valid", "is_best_model",
                    "weight_all", "weight_best") %in% names(samples)))
  finite_ll <- samples$likelihood[is.finite(samples$likelihood)]
  expect_gt(length(finite_ll), 1L)
  expect_gt(stats::sd(finite_ll), 0)                # likelihoods actually vary
  expect_equal(sum(samples$is_best_model), 1L)      # exactly one best model

  # Importance weights: at least one strictly positive, all finite & non-negative
  # (the posterior-weight machinery ran on the stubbed likelihoods).
  expect_true(all(is.finite(samples$weight_all)))
  expect_true(all(samples$weight_all >= 0))
  expect_gt(sum(samples$weight_all > 0), 0L)

  # (6) summary.json parses and reflects a completed calibration.
  summary_file <- file.path(dir_output, "3_results", "summary.json")
  expect_true(file.exists(summary_file))
  summ <- jsonlite::read_json(summary_file, simplifyVector = TRUE)
  for (k in c("location", "date_start", "date_stop", "converged",
              "n_simulations_total", "n_simulations_successful")) {
    expect_true(k %in% names(summ), info = sprintf("summary.json missing key: %s", k))
  }
  expect_equal(as.integer(summ$n_simulations_total), n_sims)
  expect_gt(as.integer(summ$n_simulations_successful), 0L)

  # (7) Ensemble-only fit metrics (v0.39 best-model removal) + central_method
  # provenance (v0.38). The single-model best fields must be GONE; the canonical
  # ensemble fields, the central_method provenance, and the dual cross-walk
  # fields must all be present (and carry the package default: the median for
  # cases and the mean for deaths since v0.101.0; both mean in v0.98.0-v0.100.x).
  expect_false(any(c("r2_cases", "r2_deaths", "bias_ratio_cases", "bias_ratio_deaths")
                   %in% names(summ)))
  expect_true(all(c("r2_cases_ensemble", "central_method_cases", "central_method_deaths",
                    "r2_cases_ensemble_mean", "r2_cases_ensemble_median")
                  %in% names(summ)))
  expect_equal(summ$central_method_cases,  "median")
  expect_equal(summ$central_method_deaths, "mean")

  # (8) Integrated deaths likelihood (v0.96.0): the posterior reported CFR by
  # location and year, from the ensemble members' post-hoc CFR draws.
  cfr_file <- file.path(dir_output, "3_results", "posterior", "cfr_posterior.csv")
  expect_true(file.exists(cfr_file))
  cfr <- utils::read.csv(cfr_file, stringsAsFactors = FALSE)
  expect_true(all(c("location", "year", "cfr_median", "cfr_lower", "cfr_upper", "prior_cfr")
                  %in% names(cfr)))
  win_years <- seq(as.integer(format(as.Date(config$date_start), "%Y")),
                   as.integer(format(as.Date(config$date_stop), "%Y")))
  expect_equal(nrow(cfr), n_loc * length(win_years))
  expect_setequal(unique(cfr$location), config$location_name)
  expect_true(all(is.finite(cfr$cfr_median) & cfr$cfr_median > 0 & cfr$cfr_median < 1))
  expect_true(all(cfr$cfr_lower <= cfr$cfr_median & cfr$cfr_median <= cfr$cfr_upper))

  # (9) config_medoid.json carries the MEDOID's posterior reported CFR (the
  # config's prior mu_jt shifted to the medoid ensemble's cfr_posterior), so
  # re-simulating it reproduces the medoid predictions' deaths level; the
  # integration setup is saved for post-hoc reruns.
  med_file <- file.path(dir_output, "2_calibration", "best_model", "config_medoid.json")
  expect_true(file.exists(med_file))
  med <- MOSAIC::read_json_to_list(med_file)
  med_ens <- readRDS(file.path(dir_output, "2_calibration", "medoid_ensemble.rds"))
  expect_false(is.null(med_ens$cfr_posterior))
  expect_equal(unname(as.matrix(med$mu_jt)),
               unname(MOSAIC:::.mosaic_apply_cfr_posterior(config, med_ens$cfr_posterior)$mu_jt),
               tolerance = 1e-8)
  expect_false(isTRUE(all.equal(unname(as.matrix(med$mu_jt)), unname(config$mu_jt))))
  di_file <- file.path(dir_output, "2_calibration", "deaths_integration.rds")
  expect_true(file.exists(di_file))
  di <- readRDS(di_file)
  expect_true(all(c("setup", "base_logit_full", "years") %in% names(di)))

  # (10) Forecast years (config_default runs past every location's data) are
  # centred on the members' latest-year CFR shift, estimated after calibration
  # and saved in the integration setup: finite wherever a location has forecast
  # years, and not all zero.
  has_fc <- vapply(di$setup$locs, function(L) length(L$forecast_years) > 0L, logical(1))
  expect_true(any(has_fc))
  shift <- vapply(di$setup$locs, function(L) L$forecast_shift, numeric(1))
  expect_true(all(is.finite(shift[has_fc])))
  expect_true(any(abs(shift[has_fc]) > 1e-6))
  expect_true(all(shift[!has_fc] == 0))

  # (11) Observation-level predictive (v0.101.0; release red team TA-02, OBS-1):
  # the candidate and medoid ensembles drew observation noise; the trajectory
  # central line is the engine-level cases median of the final ensemble; the
  # prediction CSV's predicted_median is that ensemble's predictive median and
  # predicted_central its engine median.
  cal  <- file.path(dir_output, "2_calibration")
  cand <- readRDS(file.path(cal, "ensemble_candidate.rds"))
  expect_true(isTRUE(cand$observation_model$cases))
  expect_true(isTRUE(med_ens$observation_model$cases))
  eo <- readRDS(file.path(cal, "ensemble_optimized.rds"))
  tr <- readRDS(file.path(cal, "trajectories_ensemble.rds"))
  expect_equal(as.numeric(tr$summary$reported_cases$median), as.numeric(eo$cases_median),
               tolerance = 1e-10)
  loc1 <- config$location_name[1]
  pc <- utils::read.csv(file.path(dir_output, "3_results", "predictions",
                                  sprintf("predictions_ensemble_%s.csv", loc1)), stringsAsFactors = FALSE)
  pc <- pc[pc$metric == "Suspected Cases", ]
  ok <- is.finite(pc$predicted_median)
  expect_gt(sum(ok), 0L)
  expect_equal(pc$predicted_median[ok], as.numeric(eo$predictive_median$cases[1, ok]), tolerance = 1e-8)
  expect_equal(pc$predicted_central[ok], as.numeric(eo$cases_median[1, ok]), tolerance = 1e-8)
})
