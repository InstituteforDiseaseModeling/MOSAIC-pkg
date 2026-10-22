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

test_that("the test kill point escapes tryCatch(error = ) and is inert when unset", {
  withr::local_envvar(MOSAIC_TEST_KILL_AT = "")
  expect_null(MOSAIC:::.mosaic_test_kill_point("ensemble"))
  withr::local_envvar(MOSAIC_TEST_KILL_AT = "ensemble")
  expect_null(MOSAIC:::.mosaic_test_kill_point("batch_1"))
  caught <- tryCatch(
    tryCatch(MOSAIC:::.mosaic_test_kill_point("ensemble"), error = function(e) "absorbed"),
    mosaic_test_kill = function(c) "killed")
  expect_identical(caught, "killed")
})

# ---- captured R_eff inputs ---------------------------------------------------------

# A deterministic two-location engine whose route incidences, decay rates and
# stocks depend on the seed and on beta_j0_tot, so members differ.
.reff_fake_engine <- function(config, seed = NULL, quiet = TRUE, ...) {
  nL <- 2L; Tn <- 60L
  s <- as.numeric(seed); b <- as.numeric(config$beta_j0_tot)[1]
  tt <- matrix(rep(seq_len(Tn), each = nL), nL)
  inc_h <- round(20 * (1 + sin(tt / 9 + s / 7 + b)) + 5)
  inc_e <- round(10 * (1 + cos(tt / 11 + s / 5)) + 3)
  rc <- round(0.3 * (inc_h + inc_e))
  list(results = list(
    incidence_human = inc_h, incidence_env = inc_e, incidence = inc_h + inc_e,
    delta_jt = matrix(0.05 + 0.01 * (tt %% 7), nL), E = 2 * (inc_h + inc_e),
    Isym = inc_h + 1, Iasym = inc_e + 1, reported_cases = rc,
    reported_deaths = round(0.02 * rc), new_symptomatic = rc))
}
.reff_cfg <- function(seed) {
  list(location_name = c("AAA", "BBB"), date_start = "2020-01-01", date_stop = "2020-02-29",
       reported_cases = matrix(1, 2, 60), reported_deaths = matrix(0, 2, 60),
       beta_j0_tot = (seed %% 13) / 10, iota = 1.4, gamma_1 = 0.2, gamma_2 = 0.5,
       sigma = 0.25, zeta_1 = 1e6, zeta_2 = 1e5, seed = seed)
}

test_that("captured R_eff inputs give exactly the re-simulated R_eff", {
  testthat::local_mocked_bindings(
    run_simulation = .reff_fake_engine,
    sample_parameters = function(PATHS, priors, config, seed, ...) .reff_cfg(seed),
    .package = "MOSAIC")
  ens <- calc_model_ensemble(config = .reff_cfg(1), parameter_seeds = c(11, 12, 13),
                             parameter_weights = c(0.5, 0.3, 0.2), n_simulations_per_config = 2L,
                             PATHS = list(), priors = list(), capture_reff = TRUE, verbose = FALSE)
  cap <- ens$reff_capture
  expect_identical(dim(cap$inc_hum), c(2L, 60L, 3L, 2L))
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
  expect_null(MOSAIC:::.mosaic_ensemble_drop_arrays(ens)$reff_capture)
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
  ens <- calc_model_ensemble(config = inp$config, parameter_seeds = c(101, 202, 303),
                             parameter_weights = c(0.5, 0.3, 0.2), n_simulations_per_config = 2L,
                             PATHS = inp$PATHS, priors = inp$priors, sampling_args = inp$sampling_args,
                             capture_reff = TRUE, verbose = FALSE)
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
  seq_e <- do.call(calc_model_ensemble, args)
  par_e <- do.call(calc_model_ensemble, c(args, list(parallel = TRUE, n_cores = 2L,
                                                     root_dir = getOption("root_directory"))))
  for (k in c("cases_engine_array", "deaths_engine_array", "cases_median", "deaths_mean", "seeds",
              "reff_capture", "spatial_hazard_ensemble"))
    expect_identical(par_e[[k]], seq_e[[k]], info = k)
})
