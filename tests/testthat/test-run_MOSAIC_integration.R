# Integration test for run_MOSAIC()'s Bayesian (BFRS) calibration loop.
#
# Closes review gap B2-10: run_MOSAIC()'s end-to-end orchestration had no
# automated coverage. This test exercises the REAL R pipeline ---
#   parameter sampling -> batched simulation dispatch -> R-side likelihood
#   -> outlier/subset selection -> importance weights -> posterior ensemble
#   -> best/medoid reruns -> summary.json + return contract
# --- and stubs ONLY the Python LASER engine.
#
# The stub seam (run_MOSAIC.R:247-252 and calc_model_ensemble.R:270-274): the
# in-process simulation worker resolves the laser-cholera module by first
# checking `.GlobalEnv` for an `lc` object and only importing the real Python
# module when none exists. By assigning a FAKE `lc` into .GlobalEnv before the
# call, the worker and the post-calibration ensemble both dispatch through it.
#
# CRITICAL: the fake lives in this process's .GlobalEnv, so the run MUST use the
# LOCAL SEQUENTIAL backend (dask_spec = NULL, control$parallel$enable = FALSE,
# n_cores = 1). PSOCK workers are separate processes that would import the real
# Python module and never see the fake. With parallel disabled, run_MOSAIC sets
# cl <- NULL (run_MOSAIC.R:1194-1196) and the worker runs in-process here.

# A handful of fixed simulations through the stub is fast (the synthetic LASER
# call is a no-op matrix build), but parameter sampling for 40 locations x ~1278
# timesteps is not free. Gate behind an opt-in env var so the default test run
# stays quick; flip MOSAIC_RUN_INTEGRATION=1 to exercise it.
run_integration <- nzchar(Sys.getenv("MOSAIC_RUN_INTEGRATION"))

test_that("run_MOSAIC drives a full BFRS calibration on a stubbed LASER engine", {
  skip_on_cran()
  skip_if_not(run_integration,
              "set MOSAIC_RUN_INTEGRATION=1 to run the run_MOSAIC integration test")
  skip_if_not_installed("withr")
  skip_if_not_installed("arrow")
  skip_if_not_installed("jsonlite")

  # The worker's sample_parameters() reads packaged default data via get_paths();
  # run_MOSAIC hard-requires options(root_directory). Set it best-effort.
  tryCatch(set_root_directory("~/MOSAIC"), error = function(e) NULL)
  skip_if(is.null(getOption("root_directory")), "MOSAIC root directory not set")

  # ---- config / priors -----------------------------------------------------
  # config_default already validates through make_LASER_config; strip the
  # non-signature tracking fields (same shim as test-config_default.R).
  config <- MOSAIC::config_default
  config$metadata          <- NULL
  config$zeta_ratio        <- NULL
  config$decay_days_spread <- NULL
  config$reported_cases_weight  <- NULL
  config$reported_deaths_weight <- NULL
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

  # ---- fake laser-cholera module -------------------------------------------
  # Closure-captured counter proves the loop actually dispatched sims here.
  call_env <- new.env(parent = emptyenv())
  call_env$n <- 0L
  call_env$seeds <- integer(0)

  fake_lc <- list(
    run_model = function(paramfile, quiet = TRUE, ...) {
      call_env$n <- call_env$n + 1L

      # paramfile carries the per-iteration seed (worker sets params_sim$seed)
      # and the once-per-sim sampled transmission parameter beta_j0_tot. Derive
      # a deterministic, bounded multiplier so DIFFERENT sims/iterations produce
      # DIFFERENT predictions (and therefore different, non-degenerate
      # likelihoods) while staying close enough to the observed signal that the
      # negative-binomial likelihood is finite.
      seed_val <- tryCatch(as.numeric(paramfile$seed)[1], error = function(e) 1)
      if (!is.finite(seed_val)) seed_val <- 1
      call_env$seeds <- c(call_env$seeds, as.integer(seed_val))

      beta_val <- tryCatch(as.numeric(paramfile$beta_j0_tot)[1], error = function(e) NA_real_)
      if (!is.finite(beta_val)) beta_val <- 0

      # Multiplier in roughly [0.6, 1.4]; deterministic in (seed, beta).
      mult <- 1 + 0.4 * sin(seed_val * 0.7 + beta_val * 3.0)

      cases  <- matrix(obs_cases_base  * mult, nrow = n_loc, ncol = n_t)
      deaths <- matrix(obs_deaths_base * mult, nrow = n_loc, ncol = n_t)
      # Engine returns rounded reported counts; mimic non-negative integers.
      cases[]  <- pmax(0, round(cases))
      deaths[] <- pmax(0, round(deaths))

      list(results = list(reported_cases = cases, reported_deaths = deaths))
    }
  )

  # Save/restore any pre-existing .GlobalEnv$lc so we don't poison other tests.
  had_lc  <- exists("lc", envir = .GlobalEnv, inherits = FALSE)
  prev_lc <- if (had_lc) get("lc", envir = .GlobalEnv) else NULL
  assign("lc", fake_lc, envir = .GlobalEnv)
  withr::defer({
    if (had_lc) assign("lc", prev_lc, envir = .GlobalEnv)
    else if (exists("lc", envir = .GlobalEnv, inherits = FALSE))
      rm("lc", envir = .GlobalEnv)
  })

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
  # suppressWarnings: driving the REAL pipeline on synthetic LASER output emits
  # expected, data-driven warnings that are orthogonal to the orchestration flow
  # under test -- e.g. "biologically extreme cfr_clinical_epidemic" from the
  # implied-CFR step (the fake counts are not epidemiologically calibrated) and
  # arrow's "set_io_thread_count() with num_threads < 2" note from run_MOSAIC's
  # thread pinning. Errors still propagate and fail the test.
  result <- suppressWarnings(run_MOSAIC(
    config     = config,
    priors     = priors,
    dir_output = dir_output,
    control    = control,
    resume     = FALSE,
    dask_spec  = NULL   # forces the local backend
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
  # fields must all be present (and default to the package "mean").
  expect_false(any(c("r2_cases", "r2_deaths", "bias_ratio_cases", "bias_ratio_deaths")
                   %in% names(summ)))
  expect_true(all(c("r2_cases_ensemble", "central_method_cases", "central_method_deaths",
                    "r2_cases_ensemble_mean", "r2_cases_ensemble_median")
                  %in% names(summ)))
  expect_equal(summ$central_method_cases,  "mean")
  expect_equal(summ$central_method_deaths, "mean")
})
