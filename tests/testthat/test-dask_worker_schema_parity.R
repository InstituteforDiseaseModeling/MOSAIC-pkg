# =============================================================================
# test-dask_worker_schema_parity.R
#
# Engine-free regression test for the issue #101 worker-schema round trip.
#
# The Dask worker echoes back the sampled scalar/vector parameters (minus
# matrix fields) so the R orchestrator can flatten them into parquet rows
# without re-deserializing the JSON. .extract_sampled_params() strips
# `location_name` before the JSON ships out, so the gather adapter must
# re-inject it from `config$location_name` BEFORE calling
# convert_config_to_matrix() — otherwise location-specific vector
# parameters fall through to numeric suffixes (`beta_j0_tot_1`) instead of
# ISO suffixes (`beta_j0_tot_ETH`), silently breaking parquet schema
# parity with the local path.
#
# This test simulates the Dask round trip in pure R (no cluster, no
# Python) so it runs in every CI configuration.
# =============================================================================

skip_if_no_data <- function() {
  skip_if_not_installed("MOSAIC")
  env <- new.env()
  ok <- tryCatch({
    utils::data("config_default", package = "MOSAIC", envir = env)
    utils::data("priors_default", package = "MOSAIC", envir = env)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok || !exists("config_default", envir = env)) {
    skip("config_default / priors_default not available")
  }
  root <- if (dir.exists("/workspace/MOSAIC")) "/workspace/MOSAIC" else "~/MOSAIC"
  if (!dir.exists(root)) {
    skip(paste("MOSAIC root not found at", root))
  }
  MOSAIC::set_root_directory(root)
  # config_default is the global (multi-location) sub-Saharan Africa config.
  # The parity tests below are R-side flattening only — no LASER sims — so
  # the ~40-location config is cheap to exercise and gives broader coverage
  # of the ISO-suffix invariant than a single-country fixture would.
  list(config = env$config_default, priors = env$priors_default)
}

# Mirrors inst/python/mosaic_dask_worker.py::_MATRIX_FIELDS. Kept as a local
# constant rather than reaching into Python so this test works in pure R.
.matrix_fields <- c(
  "b_jt", "d_jt", "mu_jt", "psi_jt", "nu_1_jt", "nu_2_jt",
  "reported_cases", "reported_deaths"
)

# Simulates the Dask round trip in R: extract sampled, JSON ship out,
# JSON ship back, drop matrix fields (worker filter), re-inject the
# base_config fields stripped by .extract_sampled_params() (gather adapter).
.simulate_worker_round_trip <- function(params_sim, config) {
  sampled <- MOSAIC:::.extract_sampled_params(params_sim)
  sampled_json <- jsonlite::toJSON(sampled, auto_unbox = TRUE, digits = NA)
  # simplifyVector = TRUE matches what reticulate produces when py dicts of
  # py lists/scalars come back to R.
  echoed <- jsonlite::fromJSON(sampled_json, simplifyVector = TRUE)
  echoed[.matrix_fields] <- NULL          # worker-side filter
  # Gather-adapter re-injections — must mirror .mosaic_run_batch_dask().
  echoed$location_name <- config$location_name
  echoed$N_j_initial   <- config$N_j_initial
  echoed
}

# Mirrors run_MOSAIC.R param_names_all: convert_config_to_matrix(...) output
# minus `seed`. Used to compare both paths against the canonical parquet
# column schema rather than raw convert_config_to_matrix() output.
.parquet_columns <- function(config_like) {
  pv <- MOSAIC::convert_config_to_matrix(config_like)
  if ("seed" %in% names(pv)) pv <- pv[names(pv) != "seed"]
  names(pv)
}


# =============================================================================
# TEST 1: column-name parity between local path and Dask round trip
# =============================================================================
test_that("worker echo + base-field re-injection produces parquet column names matching the local path", {
  fx <- skip_if_no_data()
  cfg <- fx$config

  params_sim <- MOSAIC::sample_parameters(
    PATHS       = NULL,
    priors      = fx$priors,
    config      = cfg,
    seed        = 1L,
    sample_args = list(),
    verbose     = FALSE,
    validate    = FALSE
  )

  # Compare against the canonical parquet column schema (matches
  # run_MOSAIC.R param_names_all: convert_config_to_matrix() minus seed).
  local_cols <- .parquet_columns(params_sim)

  reconstituted <- .simulate_worker_round_trip(params_sim, cfg)
  dask_cols <- .parquet_columns(reconstituted)

  expect_setequal(local_cols, dask_cols)
})


# =============================================================================
# TEST 2: vector parameters carry ISO suffixes, not numeric fallbacks
#
# Regression for the silent _1 / _ETH break flagged in issue #101 step 2.
# Without location_name re-injection, convert_config_to_matrix() would emit
# `beta_j0_tot_1`, `beta_j0_tot_2`, ... instead of the ISO-suffixed columns.
# =============================================================================
test_that("vector parameters carry ISO suffixes after worker round trip", {
  fx <- skip_if_no_data()
  cfg <- fx$config

  params_sim <- MOSAIC::sample_parameters(
    PATHS = NULL, priors = fx$priors, config = cfg,
    seed = 1L, sample_args = list(),
    verbose = FALSE, validate = FALSE
  )

  reconstituted <- .simulate_worker_round_trip(params_sim, cfg)
  dask_cols <- names(MOSAIC::convert_config_to_matrix(reconstituted))

  iso_codes <- as.character(cfg$location_name)
  for (iso in iso_codes) {
    expect_true(
      any(grepl(paste0("_", iso, "$"), dask_cols, perl = FALSE)),
      info = sprintf("expected at least one column with ISO suffix _%s; got: %s",
                     iso, paste(head(dask_cols, 5), collapse = ","))
    )
  }
})


# =============================================================================
# TEST 3: dropping location_name re-injection silently breaks suffixes
#
# Locks in the failure mode the production code defends against, so a future
# refactor that removes the `params$location_name <- config$location_name`
# line in .mosaic_run_batch_dask() trips a clear test failure with a pointer
# to this property.
# =============================================================================
test_that("regression: missing location_name injection yields numeric suffixes", {
  fx <- skip_if_no_data()
  cfg <- fx$config
  if (length(cfg$location_name) < 2L) {
    skip("multi-location fixture required to detect numeric suffix fallback")
  }

  params_sim <- MOSAIC::sample_parameters(
    PATHS = NULL, priors = fx$priors, config = cfg,
    seed = 1L, sample_args = list(),
    verbose = FALSE, validate = FALSE
  )

  sampled <- MOSAIC:::.extract_sampled_params(params_sim)
  echoed <- jsonlite::fromJSON(
    jsonlite::toJSON(sampled, auto_unbox = TRUE, digits = NA),
    simplifyVector = TRUE
  )
  echoed[.matrix_fields] <- NULL
  # NOTE: location_name intentionally NOT re-injected — this is the silent
  # break we are guarding against.

  broken_cols <- names(MOSAIC::convert_config_to_matrix(echoed))
  iso_codes   <- as.character(cfg$location_name)
  has_iso     <- any(vapply(iso_codes, function(iso)
    any(grepl(paste0("_", iso, "$"), broken_cols, perl = FALSE)),
    logical(1)))

  expect_false(
    has_iso,
    info = "without location_name injection we expect numeric suffixes (_1, _2, ...) — the property test above guards against re-introducing this path"
  )
})


# =============================================================================
# TEST 4: .mosaic_write_one_shard_dask writes a valid parquet shard
#
# Engine-free test of the per-sim helper extracted in v0.32.17 so the Dask
# write loop can run in parallel via mclapply. Builds a mock worker result
# in pure R, calls the helper, and verifies the on-disk parquet matches
# the expected schema.
# =============================================================================
test_that(".mosaic_write_one_shard_dask produces a valid parquet shard", {
  fx <- skip_if_no_data()
  skip_if_not_installed("arrow")
  cfg <- fx$config

  params_sim <- MOSAIC::sample_parameters(
    PATHS = NULL, priors = fx$priors, config = cfg,
    seed = 1L, sample_args = list(),
    verbose = FALSE, validate = FALSE
  )

  # Build the params dict the worker would echo back: sampled minus matrix
  # fields (the worker filter), MINUS the base fields .extract_sampled_params
  # strips (location_name, N_j_initial, etc.). The helper re-injects those.
  worker_params <- MOSAIC:::.extract_sampled_params(params_sim)
  worker_params[.matrix_fields] <- NULL

  mock_res <- list(
    sim_id     = 42L,
    success    = TRUE,
    params     = worker_params,
    iterations = list(
      list(iter = 1L, seed_iter = 100L, likelihood = -1234.5),
      list(iter = 2L, seed_iter = 101L, likelihood = -1233.0),
      list(iter = 3L, seed_iter = 102L, likelihood = -1234.8)
    )
  )

  # Compute param_names_all the way run_MOSAIC does.
  pv <- MOSAIC::convert_config_to_matrix(params_sim)
  if ("seed" %in% names(pv)) pv <- pv[names(pv) != "seed"]
  param_names_all <- names(pv)

  temp_dir <- tempfile("shard_test_")
  dir.create(file.path(temp_dir, "samples"), recursive = TRUE)
  dirs <- list(cal_samples = file.path(temp_dir, "samples"))

  result <- MOSAIC:::.mosaic_write_one_shard_dask(
    sim_id          = 42L,
    res             = mock_res,
    n_iterations    = 3L,
    param_names_all = param_names_all,
    config          = cfg,
    dirs            = dirs,
    control         = list(io = MOSAIC::mosaic_io_presets("default"))
  )

  expect_true(result)

  out_file <- file.path(dirs$cal_samples, "sim_0000042.parquet")
  expect_true(file.exists(out_file))

  df <- arrow::read_parquet(out_file)
  expect_equal(nrow(df), 1L)
  expect_equal(df$sim, 42L)
  expect_equal(df$iter, 1L)
  expect_equal(df$seed_sim, 42L)
  expect_equal(df$seed_iter, as.integer((42L - 1L) * 3L + 1L))
  expect_true(is.finite(df$likelihood))
  # Likelihood is the log-mean-exp of the 3 iter likelihoods, which lies
  # between the min and max iter LL.
  expect_gte(df$likelihood, -1234.8)
  expect_lte(df$likelihood, -1233.0)
  expect_true(all(param_names_all %in% names(df)))

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})


# =============================================================================
# TEST 5: serial and parallel shard writes produce identical output
#
# Verifies the parallel-write refactor (v0.32.17) hasn't introduced any
# nondeterminism: writing the same 4 mock results via serial lapply and
# via mclapply must produce byte-identical parquet files.
# =============================================================================
test_that("parallel mclapply produces identical shards to serial lapply", {
  fx <- skip_if_no_data()
  skip_if_not_installed("arrow")
  skip_on_os("windows")  # mclapply is POSIX-only
  cfg <- fx$config

  params_sim <- MOSAIC::sample_parameters(
    PATHS = NULL, priors = fx$priors, config = cfg,
    seed = 1L, sample_args = list(),
    verbose = FALSE, validate = FALSE
  )
  worker_params <- MOSAIC:::.extract_sampled_params(params_sim)
  worker_params[.matrix_fields] <- NULL

  pv <- MOSAIC::convert_config_to_matrix(params_sim)
  if ("seed" %in% names(pv)) pv <- pv[names(pv) != "seed"]
  param_names_all <- names(pv)

  make_res <- function(sim_id) {
    list(
      sim_id     = as.integer(sim_id),
      success    = TRUE,
      params     = worker_params,
      iterations = list(
        list(iter = 1L, seed_iter = sim_id * 10L,     likelihood = -1000 - sim_id),
        list(iter = 2L, seed_iter = sim_id * 10L + 1, likelihood = -1001 - sim_id),
        list(iter = 3L, seed_iter = sim_id * 10L + 2, likelihood = -999  - sim_id)
      )
    )
  }
  sim_ids <- 1L:4L
  results_lookup <- setNames(lapply(sim_ids, make_res), as.character(sim_ids))

  run_one_path <- function(parallel) {
    tmp  <- tempfile("shard_parity_")
    dir.create(file.path(tmp, "samples"), recursive = TRUE)
    dirs <- list(cal_samples = file.path(tmp, "samples"))
    worker <- function(sim_id) {
      MOSAIC:::.mosaic_write_one_shard_dask(
        sim_id          = sim_id,
        res             = results_lookup[[as.character(sim_id)]],
        n_iterations    = 3L,
        param_names_all = param_names_all,
        config          = cfg,
        dirs            = dirs,
        control         = list(io = MOSAIC::mosaic_io_presets("default"))
      )
    }
    if (parallel) {
      parallel::mclapply(sim_ids, worker, mc.cores = 2L)
    } else {
      lapply(sim_ids, worker)
    }
    files <- sort(list.files(dirs$cal_samples, full.names = TRUE))
    out   <- lapply(files, function(f) arrow::read_parquet(f))
    unlink(tmp, recursive = TRUE)
    do.call(rbind, out)
  }

  serial_df   <- run_one_path(parallel = FALSE)
  parallel_df <- run_one_path(parallel = TRUE)

  expect_equal(parallel_df, serial_df)
})
