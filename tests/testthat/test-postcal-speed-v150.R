# MOSAIC 1.5.0 post-calibration robustness and speed (engine-free unit tests):
#   * HSIC sensitivity a chunk of parameters at a time, with results equal to
#     the single call (item 7);
#   * the sampling marker behind the post-calibration resume (item 6);
#   * the ensemble's captured R_eff inputs, which give the R_eff of a
#     re-simulation without re-simulating (item 8);
#   * seeds-mode ensemble members rebuilt from their seed on the worker.

# ---- HSIC chunks ---------------------------------------------------------------

test_that(".mosaic_hsic_chunks covers every parameter in order within the budget", {
  ch <- MOSAIC:::.mosaic_hsic_chunks(916L, 2000L)
  expect_identical(unlist(ch), seq_len(916L))
  expect_identical(max(lengths(ch)), 33L)           # 33 x 2000^2 doubles = 1007 MiB
  expect_lte(max(lengths(ch)) * 8 * 2000^2, 1024^3)
  expect_identical(lengths(MOSAIC:::.mosaic_hsic_chunks(5L, 2e5L)), rep(1L, 5L))   # at least one
  expect_identical(MOSAIC:::.mosaic_hsic_chunks(3L, 10L), list(1:3))
})

test_that("chunked HSIC gives exactly the single-call indices and p-values", {
  skip_if_not_installed("sensitivity")
  skip_if_not_installed("arrow")
  set.seed(5)
  n <- 300L; p <- 11L
  X <- matrix(stats::runif(n * p), n, p); colnames(X) <- sprintf("par_%02d", seq_len(p))
  ll <- -80 * (X[, 1] - 0.5)^2 + 4 * X[, 2] + stats::rnorm(n, sd = 0.2)
  smp <- data.frame(sim = seq_len(n), iter = 1L, seed_sim = seq_len(n), seed_iter = 1L,
                    likelihood = ll, is_finite = TRUE, is_valid = TRUE, X)
  smp$weight_all <- rep(1 / n, n)
  f <- withr::local_tempfile(fileext = ".parquet"); arrow::write_parquet(smp, f)

  set.seed(9); one <- calc_model_parameter_sensitivity(f, n_samples = 200, verbose = FALSE)
  # n = 200 fits every parameter in one chunk; force chunks of three.
  testthat::local_mocked_bindings(
    .mosaic_hsic_chunks = function(p, n, budget_bytes = 0) unname(split(seq_len(p), ceiling(seq_len(p) / 3))),
    .package = "MOSAIC")
  set.seed(9); chunked <- calc_model_parameter_sensitivity(f, n_samples = 200, verbose = FALSE)
  expect_identical(chunked$sens_df, one$sens_df)
  expect_identical(chunked$n_used, one$n_used)
  expect_identical(nrow(one$sens_df), p)
  expect_identical(one$sens_df$parameter[1], "par_01")
})

# ---- sampling marker -------------------------------------------------------------

.marker_dirs <- function() {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  MOSAIC:::.mosaic_ensure_dir_tree(d, clean_output = FALSE)
}

test_that("the sampling marker round-trips and verifies samples.parquet", {
  skip_if_not_installed("arrow")
  dirs <- .marker_dirs()
  res <- data.frame(sim = 1:5, iter = 1L, seed_sim = 1:5, likelihood = c(-3, -1, -2, -Inf, -4), a = 0.1 * 1:5)
  flagged <- MOSAIC:::.mosaic_flag_results(res, mosaic_control_defaults())
  arrow::write_parquet(cbind(flagged, weight_best = 0.2), file.path(dirs$calibration, "samples.parquet"))
  set.seed(42); state <- list(mode = "fixed", batch_number = 1L, total_sims_run = 5L, .gate_pool = list(x = 1))
  MOSAIC:::.mosaic_write_sampling_marker(dirs, flagged, state, names(res))
  rng <- .Random.seed

  m <- MOSAIC:::.mosaic_read_sampling_marker(dirs)
  expect_identical(m$n_rows, 5L)
  expect_null(m$state$.gate_pool)
  expect_identical(m$random_seed, rng)
  # Only the shard columns come back, in the combine's order, and flagging them
  # again reproduces the combine-time flags.
  back <- MOSAIC:::.mosaic_load_sampled_results(file.path(dirs$calibration, "samples.parquet"), m)
  expect_identical(names(back), names(res))
  expect_identical(MOSAIC:::.mosaic_flag_results(back, mosaic_control_defaults()), flagged)

  # A different draw set is refused.
  m2 <- m; m2$likelihood[1] <- 0
  expect_error(MOSAIC:::.mosaic_load_sampled_results(file.path(dirs$calibration, "samples.parquet"), m2),
               "does not hold the draws its sampling marker recorded")
})

test_that("an unusable sampling marker is ignored, never trusted", {
  skip_if_not_installed("arrow")
  dirs <- .marker_dirs()
  expect_null(MOSAIC:::.mosaic_read_sampling_marker(dirs))     # none
  res <- data.frame(sim = 1:3, likelihood = c(-1, -2, -3))
  MOSAIC:::.mosaic_write_sampling_marker(dirs, res, list(mode = "fixed"), names(res))
  expect_null(suppressMessages(MOSAIC:::.mosaic_read_sampling_marker(dirs)))   # no samples.parquet
  arrow::write_parquet(res[1:2, ], file.path(dirs$calibration, "samples.parquet"))
  expect_null(suppressMessages(MOSAIC:::.mosaic_read_sampling_marker(dirs)))   # row count differs
  arrow::write_parquet(res, file.path(dirs$calibration, "samples.parquet"))
  expect_false(is.null(MOSAIC:::.mosaic_read_sampling_marker(dirs)))
  saveRDS(list(schema_version = 99L), MOSAIC:::.mosaic_sampling_marker_file(dirs))
  expect_null(suppressMessages(MOSAIC:::.mosaic_read_sampling_marker(dirs)))   # other schema
})

test_that(".mosaic_restore_rng puts back the recorded stream, or none", {
  set.seed(3); s <- .Random.seed; a <- stats::runif(2)
  stats::runif(5)
  MOSAIC:::.mosaic_restore_rng(s)
  expect_identical(stats::runif(2), a)
  MOSAIC:::.mosaic_restore_rng(NULL)
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  set.seed(1)
})

# ---- captured R_eff inputs ---------------------------------------------------------

test_that("captured R_eff inputs give exactly the re-simulated R_eff", {
  testthat::local_mocked_bindings(
    run_simulation = .reff_fake_engine,
    sample_parameters = function(PATHS, priors, config, seed, ...) .reff_cfg(seed),
    .package = "MOSAIC")
  ens <- MOSAIC:::.mosaic_calc_model_ensemble(
    config = .reff_cfg(1), parameter_seeds = c(11, 12, 13), parameter_weights = c(0.5, 0.3, 0.2),
    n_simulations_per_config = 2L, PATHS = list(), priors = list(), capture_reff = TRUE, verbose = FALSE)
  cap <- ens$reff_capture
  expect_identical(dim(cap$inc_hum), c(2L, 60L, 3L, 2L))
  expect_identical(storage.mode(cap$inc_hum), "integer")      # counts, stored compactly
  expect_identical(dim(cap$delta), c(2L, 60L, 3L))            # once per parameter set
  expect_true(all(is.na(cap$error)))
  expect_identical(colnames(cap$kernel_params), c("iota", "gamma_1", "gamma_2", "sigma", "zeta_1", "zeta_2"))

  run <- function(e) MOSAIC:::.mosaic_reff_resim_ci(
    e, base_config = .reff_cfg(1), priors = list(), sampling_args = list(), PATHS = list(),
    burn_in_days = 5L, verbose = FALSE)
  captured <- run(ens)
  resim_ens <- ens; resim_ens$reff_capture <- NULL
  resim <- run(resim_ens)
  expect_true(captured$captured)
  expect_false(resim$captured)
  captured$captured <- resim$captured <- NULL
  expect_identical(captured, resim)

  # Inputs that are not this ensemble's are refused, not used.
  bad <- ens; bad$reff_capture$cases_checksum[1, 2, 1] <- -1
  expect_error(run(bad), "do not match its saved engine-level cases")
  # The arrays go with the dense arrays when they are stripped for persistence.
  expect_false("reff_capture" %in% names(MOSAIC:::.mosaic_ensemble_drop_arrays(ens)))
})

test_that("calc_model_ensemble() keeps the 1.4.2 signature and returns the 1.4.2 object", {
  expect_identical(names(formals(calc_model_ensemble)), c(
    "config", "parameter_seeds", "configs", "parameter_weights", "n_simulations_per_config",
    "envelope_quantiles", "PATHS", "priors", "sampling_args", "n_cases_warmup_mask",
    "mask_final_deaths_step", "score_idx_cases", "score_idx_deaths", "parallel", "n_cores",
    "root_dir", "capture_trajectories", "trajectory_channels", "trajectory_n_lines",
    "trajectory_scratch_dir", "reduce_trajectories", "deaths_integration", "observation_model",
    "verbose"))
  testthat::local_mocked_bindings(
    run_simulation = .reff_fake_engine,
    sample_parameters = function(PATHS, priors, config, seed, ...) .reff_cfg(seed),
    .package = "MOSAIC")
  # Made by MOSAIC 1.4.2 with the same call: claude/v150/fixture/make_ens_v142.R.
  golden <- readRDS(test_path("fixtures", "ensemble_v142_mock.rds"))
  ens <- calc_model_ensemble(config = .reff_cfg(1), parameter_seeds = c(11, 12, 13),
                             parameter_weights = c(0.5, 0.3, 0.2), n_simulations_per_config = 2L,
                             PATHS = list(), priors = list(), observation_model = list(k_cases = c(2, 5)),
                             verbose = FALSE)
  # Structure is exact (same elements in the same order, no reff_capture); values
  # agree to a relative 1e-10. The fixture was made on macOS arm64, and CI's
  # Ubuntu libm differs in the last bit of the weighted means (~2e-16).
  expect_identical(names(ens), names(golden))
  expect_identical(lapply(ens, class), lapply(golden, class))
  expect_equal(ens, golden, tolerance = 1e-10)
  expect_false("reff_capture" %in% names(ens))
})

test_that("a seeds-mode task rebuilt from its seed equals the task given its config", {
  testthat::local_mocked_bindings(
    run_simulation = .reff_fake_engine,
    sample_parameters = function(PATHS, priors, config, seed, ...) .reff_cfg(seed),
    .package = "MOSAIC")
  src <- list(config = .reff_cfg(1), priors = list(), PATHS = list(), sampling_args = list(),
              seeds = c(11, 12))
  cfgs <- lapply(1:2, function(p) MOSAIC:::.mosaic_ensemble_member_config(src, p))
  task <- data.frame(param_idx = 2L, stoch_idx = 1L)
  from_list <- MOSAIC:::.mosaic_ensemble_sim_task(task, cfgs, capture_reff = TRUE)
  from_seed <- MOSAIC:::.mosaic_ensemble_sim_task(task, NULL, capture_reff = TRUE, config_source = src)
  expect_true(from_list$success)
  expect_identical(from_seed, from_list)
})

# ---- real engine (integration tier) -------------------------------------------------

.real_ensemble_inputs <- function() {
  config <- get_location_config(iso = "MOZ", config = MOSAIC::config_default)
  priors <- get_location_priors(iso = "MOZ", priors = MOSAIC::priors_default)
  list(config = config, priors = priors, PATHS = get_paths(),
       sampling_args = mosaic_control_defaults()$sampling)
}

test_that("on the real engine the captured R_eff equals the re-simulated R_eff", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("MOSAIC_RUN_INTEGRATION")),
              "set MOSAIC_RUN_INTEGRATION=1 to run the real-engine R_eff capture check")
  local_test_root()
  inp <- .real_ensemble_inputs()
  ens <- MOSAIC:::.mosaic_calc_model_ensemble(
    config = inp$config, parameter_seeds = c(101, 202, 303), parameter_weights = c(0.5, 0.3, 0.2),
    n_simulations_per_config = 2L, PATHS = inp$PATHS, priors = inp$priors,
    sampling_args = inp$sampling_args, capture_reff = TRUE, verbose = FALSE)
  run <- function(e) MOSAIC:::.mosaic_reff_resim_ci(
    e, base_config = inp$config, priors = inp$priors, sampling_args = inp$sampling_args,
    PATHS = inp$PATHS, burn_in_days = 30L, verbose = FALSE)
  captured <- run(ens)
  resim_ens <- ens; resim_ens$reff_capture <- NULL
  resim <- run(resim_ens)
  expect_true(captured$captured)
  captured$captured <- resim$captured <- NULL
  expect_identical(captured, resim)
  expect_identical(resim$gate_max_abs_diff, 0)
})

test_that("a parallel seeds-mode ensemble (configs rebuilt on the workers) equals the sequential one", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("MOSAIC_RUN_INTEGRATION")),
              "set MOSAIC_RUN_INTEGRATION=1 to run the real-engine parallel ensemble check")
  # PSOCK workers load the INSTALLED MOSAIC; under devtools::load_all() that can
  # be another version (R CMD check installs this one).
  installed <- tryCatch(utils::packageDescription("MOSAIC", lib.loc = .libPaths())$Version,
                        error = function(e) NA_character_)
  skip_if(!identical(installed, as.character(utils::packageVersion("MOSAIC"))),
          "the installed MOSAIC is not the version under test")
  local_test_root()
  inp <- .real_ensemble_inputs()
  args <- list(config = inp$config, parameter_seeds = c(101, 202, 303), parameter_weights = c(0.5, 0.3, 0.2),
               n_simulations_per_config = 2L, PATHS = inp$PATHS, priors = inp$priors,
               sampling_args = inp$sampling_args, capture_reff = TRUE, verbose = FALSE)
  keys <- c("cases_engine_array", "deaths_engine_array", "cases_median", "deaths_mean", "seeds",
            "reff_capture", "spatial_hazard_ensemble")
  # Under the default RNG kind and under a non-default one: sample_parameters()
  # seeds with the session's kind, which the workers must share.
  for (kind in c("Mersenne-Twister", "L'Ecuyer-CMRG")) {
    old <- RNGkind(kind)
    on.exit(RNGkind(old[1], old[2], old[3]), add = TRUE)
    seq_e <- do.call(MOSAIC:::.mosaic_calc_model_ensemble, args)
    par_e <- do.call(MOSAIC:::.mosaic_calc_model_ensemble,
                     c(args, list(parallel = TRUE, n_cores = 2L, root_dir = getOption("root_directory"))))
    for (k in keys) expect_identical(par_e[[k]], seq_e[[k]], info = paste(kind, k))
    RNGkind(old[1], old[2], old[3])
  }
})
