# =============================================================================
# Functions sent to PSOCK workers must not carry their defining frame.
#
# Serialising a closure ships its enclosing environment. Before v1.1.1,
# calc_model_ensemble() passed an anonymous function to clusterCall() whose
# environment was the whole calc frame -- four all-NA ensemble arrays of ~1.2 GB
# each at 40 locations x 3,406 days x 108 x 10, plus param_configs, config and
# priors -- so every worker received ~5.7 GB before any task ran, and the
# continental ensemble reached the 1.47 TB watchdog on dugong. The subset
# optimiser had the same defect: its cell-block function reached the full
# cases/deaths arrays it had just exported once as .GLOBAL_*.
# =============================================================================

test_that("no anonymous function is passed straight to a PSOCK call", {
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "package sources not available (installed-package test run)")
  src <- unlist(lapply(list.files(r_dir, "[.]R$", full.names = TRUE), function(f) {
    x <- readLines(f, warn = FALSE)
    hit <- grep("(clusterCall|clusterApply|clusterApplyLB|parLapply|parSapply)\\(\\s*cl\\s*,([^,]*,)?\\s*function\\s*\\(", x)
    if (length(hit)) sprintf("%s:%d: %s", basename(f), hit, trimws(x[hit])) else character(0)
  }))
  expect_identical(src, character(0),
                   info = "define the worker function first and reset its environment (see calc_model_ensemble.R)")
})

test_that("the subset optimiser ships a small block function and gives the serial result", {
  skip_on_cran()
  skip_if(MOSAIC:::.psi_is_dev_namespace(), "PSOCK workers cannot load a load_all() namespace")

  # make_mock_ensemble() / make_mock_likelihoods() come from test-optimize_ensemble_subset.R;
  # build an ensemble whose arrays dwarf any legitimate block-function payload.
  set.seed(7)
  n_locs <- 2L; n_times <- 1500L; n_params <- 20L; n_stoch <- 3L
  obs <- matrix(rpois(n_locs * n_times, 40), n_locs)
  arr <- array(rpois(n_locs * n_times * n_params * n_stoch, 40), c(n_locs, n_times, n_params, n_stoch))
  ens <- structure(list(
    cases_array = arr, deaths_array = arr / 10, obs_cases = obs, obs_deaths = obs / 10,
    cases_mean = obs, cases_median = obs, deaths_mean = obs / 10, deaths_median = obs / 10,
    ci_bounds = list(cases = list(list(lower = obs, upper = obs)), deaths = list(list(lower = obs, upper = obs))),
    parameter_weights = rep(1 / n_params, n_params), n_param_sets = n_params,
    n_simulations_per_config = n_stoch, n_successful = n_params * n_stoch,
    location_names = c("LOC_A", "LOC_B"), n_locations = n_locs, n_time_points = n_times,
    date_start = "2020-01-01", date_stop = as.character(as.Date("2020-01-01") + n_times - 1),
    envelope_quantiles = c(0.025, 0.975)), class = "mosaic_ensemble")
  lik <- -100 - seq(0, by = 0.5, length.out = n_params)
  array_bytes <- length(serialize(arr, NULL))

  sizes <- new.env()
  sizes$fun <- numeric(0)
  trace("parLapply", where = asNamespace("parallel"), print = FALSE,
        tracer = bquote(assign("fun", c(get("fun", envir = .(sizes)), length(serialize(fun, NULL))),
                               envir = .(sizes))))
  on.exit(untrace("parLapply", where = asNamespace("parallel")), add = TRUE)

  cl <- parallel::makeCluster(2L, type = "PSOCK")
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, suppressPackageStartupMessages(library(MOSAIC)))

  par <- suppressWarnings(suppressMessages(
    MOSAIC::optimize_ensemble_subset(ens, lik, min_n = 5L, cl = cl, verbose = FALSE)))
  ser <- suppressWarnings(suppressMessages(
    MOSAIC::optimize_ensemble_subset(ens, lik, min_n = 5L, cl = NULL, verbose = FALSE)))

  expect_gt(length(sizes$fun), 0)
  expect_lt(max(sizes$fun), array_bytes / 20)   # was > 2 x array_bytes (both arrays rode along)
  expect_identical(par$optimal_n, ser$optimal_n)
  expect_equal(par$evaluation_table, ser$evaluation_table)
})
