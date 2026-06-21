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

# skip_if_no_data() is centralized in helper-skips.R. It returns
# list(config = config_default, priors = priors_default) after setting the
# MOSAIC root; config_default is the global (multi-location) SSA config, which
# is cheap to exercise R-side (no LASER sims) and gives broad ISO-suffix
# coverage. Callers use it as `fx <- skip_if_no_data()`.

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
  param_lookup <- MOSAIC:::.mosaic_build_param_lookup(
    param_names_all, cfg$location_name)

  temp_dir <- tempfile("shard_test_")
  dir.create(file.path(temp_dir, "samples"), recursive = TRUE)
  dirs <- list(cal_samples = file.path(temp_dir, "samples"))

  result <- MOSAIC:::.mosaic_write_one_shard_dask(
    sim_id          = 42L,
    res             = mock_res,
    n_iterations    = 3L,
    param_names_all = param_names_all,
    param_lookup    = param_lookup,
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
# =============================================================================
# TEST: scored-window fields (score_idx_*) injected + broadcast on the Dask path
#
# Per-channel scoring window (burn-in + deaths-era start). The resolved
# score_idx_cases / score_idx_deaths must (1) be injected onto config by
# .mosaic_inject_likelihood_settings(), and (2) survive .extract_base_config()
# so they reach the worker (Lesson #12: Dask injection must stay in lockstep).
# Default (resolved idx = 1) => the Python worker takes the no-slice path.
# =============================================================================
test_that("score_idx_* are injected and broadcast in the Dask base config", {
  fx <- skip_if_no_data()
  cfg <- fx$config

  # Default (idx = 1) — bit-identical no-slice path.
  ls_default <- list(.score_window_resolved = list(idx_cases = 1L, idx_deaths = 1L))
  inj_default <- MOSAIC:::.mosaic_inject_likelihood_settings(cfg, ls_default)
  expect_identical(inj_default$score_idx_cases, 1L)
  expect_identical(inj_default$score_idx_deaths, 1L)

  base_default <- MOSAIC:::.extract_base_config(inj_default)
  expect_true(all(c("score_idx_cases", "score_idx_deaths") %in% names(base_default)))
  expect_identical(base_default$score_idx_cases, 1L)
  expect_identical(base_default$score_idx_deaths, 1L)

  # Non-default window — resolved indices propagate verbatim.
  ls_win <- list(.score_window_resolved = list(idx_cases = 15L, idx_deaths = 120L))
  inj_win <- MOSAIC:::.mosaic_inject_likelihood_settings(cfg, ls_win)
  expect_identical(inj_win$score_idx_cases, 15L)
  expect_identical(inj_win$score_idx_deaths, 120L)
  base_win <- MOSAIC:::.extract_base_config(inj_win)
  expect_identical(base_win$score_idx_cases, 15L)
  expect_identical(base_win$score_idx_deaths, 120L)

  # Absent .score_window_resolved (defensive) => 1L (no slice).
  inj_abs <- MOSAIC:::.mosaic_inject_likelihood_settings(cfg, list())
  expect_identical(inj_abs$score_idx_cases, 1L)
  expect_identical(inj_abs$score_idx_deaths, 1L)
})


test_that(".mosaic_sample_and_serialize captures errors instead of throwing", {
  fx <- skip_if_no_data()

  # Pass invalid priors to force an internal error inside the helper.
  # The full-body tryCatch (v0.32.20) must convert the error into a
  # structured return — NEVER let an exception escape, since under
  # mclapply that would corrupt the chunk_results positional contract
  # and crash the parent loop on the next $params/$json access.
  bad_result <- MOSAIC:::.mosaic_sample_and_serialize(
    sim_id        = 1L,
    PATHS         = NULL,
    priors        = "not a priors list",
    config        = fx$config,
    sampling_args = list()
  )

  expect_type(bad_result, "list")
  expect_named(bad_result, c("params", "json", "error"))
  expect_null(bad_result$params)
  expect_null(bad_result$json)
  expect_type(bad_result$error, "character")
  expect_true(nzchar(bad_result$error))
})


test_that(".mosaic_write_one_shard_dask returns a character error instead of throwing", {
  fx <- skip_if_no_data()
  cfg <- fx$config

  # res$success = FALSE → returns character error message, not TRUE
  bad_res <- list(sim_id = 1L, success = FALSE,
                  error = "synthetic worker failure")
  tmp <- tempfile("write_err_"); dir.create(file.path(tmp, "samples"), recursive = TRUE)
  out <- MOSAIC:::.mosaic_write_one_shard_dask(
    sim_id          = 1L,
    res             = bad_res,
    n_iterations    = 3L,
    param_names_all = c("a", "b"),
    param_lookup    = list(list(key = "a", idx = NULL),
                           list(key = "b", idx = NULL)),
    config          = cfg,
    dirs            = list(cal_samples = file.path(tmp, "samples")),
    control         = list(io = MOSAIC::mosaic_io_presets("default"))
  )
  expect_type(out, "character")
  expect_match(out, "synthetic worker failure")
  expect_false(isTRUE(out))

  # res = NULL also returns a character message
  out2 <- MOSAIC:::.mosaic_write_one_shard_dask(
    sim_id          = 2L,
    res             = NULL,
    n_iterations    = 3L,
    param_names_all = c("a", "b"),
    param_lookup    = list(list(key = "a", idx = NULL),
                           list(key = "b", idx = NULL)),
    config          = cfg,
    dirs            = list(cal_samples = file.path(tmp, "samples")),
    control         = list(io = MOSAIC::mosaic_io_presets("default"))
  )
  expect_type(out2, "character")
  expect_false(isTRUE(out2))

  # Missing iterations → returns character message
  partial_res <- list(sim_id = 3L, success = TRUE,
                      params = list(), iterations = list())
  out3 <- MOSAIC:::.mosaic_write_one_shard_dask(
    sim_id          = 3L,
    res             = partial_res,
    n_iterations    = 3L,
    param_names_all = c("a", "b"),
    param_lookup    = list(list(key = "a", idx = NULL),
                           list(key = "b", idx = NULL)),
    config          = cfg,
    dirs            = list(cal_samples = file.path(tmp, "samples")),
    control         = list(io = MOSAIC::mosaic_io_presets("default"))
  )
  expect_type(out3, "character")
  expect_match(out3, "no iterations|no params dict|failed on worker")

  unlink(tmp, recursive = TRUE)
})


test_that("parallel write loop survives mixed success/failure without aborting", {
  skip_if_testthat_parallel()  # inner mclapply fork is unsafe inside a testthat worker
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

  good_res <- function(sim_id) list(
    sim_id     = as.integer(sim_id),
    success    = TRUE,
    params     = worker_params,
    iterations = list(list(iter = 1L, seed_iter = sim_id * 10L, likelihood = -1234.5))
  )
  bad_res <- list(sim_id = 0L, success = FALSE,
                  error = "synthetic worker failure")

  # Mix: sim 1 ok, sim 2 bad, sim 3 ok, sim 4 bad
  results_lookup <- list(
    "1" = good_res(1), "2" = bad_res,
    "3" = good_res(3), "4" = bad_res
  )
  sim_ids <- 1L:4L
  tmp     <- tempfile("err_recovery_")
  dir.create(file.path(tmp, "samples"), recursive = TRUE)
  dirs    <- list(cal_samples = file.path(tmp, "samples"))

  param_lookup <- MOSAIC:::.mosaic_build_param_lookup(
    param_names_all, cfg$location_name)

  chunk_out <- parallel::mclapply(sim_ids, function(idx) {
    sim_id <- sim_ids[[idx]]
    MOSAIC:::.mosaic_write_one_shard_dask(
      sim_id          = sim_id,
      res             = results_lookup[[as.character(sim_id)]],
      n_iterations    = 1L,
      param_names_all = param_names_all,
      param_lookup    = param_lookup,
      config          = cfg,
      dirs            = dirs,
      control         = list(io = MOSAIC::mosaic_io_presets("default"))
    )
  }, mc.cores = 2L)

  # Parent must have survived all 4 — none caused a propagated error
  expect_length(chunk_out, 4L)

  # isTRUE correctly distinguishes TRUE (success) from character (failure)
  success <- vapply(chunk_out, isTRUE, logical(1))
  expect_equal(success, c(TRUE, FALSE, TRUE, FALSE))

  # Failed entries are character error messages — parent can surface them
  expect_type(chunk_out[[2]], "character")
  expect_match(chunk_out[[2]], "synthetic worker failure")
  expect_type(chunk_out[[4]], "character")

  unlink(tmp, recursive = TRUE)
})


test_that(".mosaic_sample_and_serialize is reproducible across parallel and serial paths", {
  skip_if_testthat_parallel()  # inner mclapply fork is unsafe inside a testthat worker
  fx <- skip_if_no_data()
  skip_on_os("windows")  # mclapply is POSIX-only
  cfg <- fx$config

  # sample_parameters() is internally seeded by sim_id, so the per-sim
  # output of .mosaic_sample_and_serialize() must depend only on sim_id —
  # NOT on which fork (or serial) processed it. This is the load-bearing
  # property for parallel sampling.
  sim_ids <- 1L:8L

  call_one <- function(sim_id) {
    MOSAIC:::.mosaic_sample_and_serialize(
      sim_id        = sim_id,
      PATHS         = NULL,
      priors        = fx$priors,
      config        = cfg,
      sampling_args = list()
    )
  }

  serial_out   <- lapply(sim_ids, call_one)
  parallel_out <- parallel::mclapply(sim_ids, call_one, mc.cores = 4L)

  # Each entry is list(params = <config-shaped>, json = "...")
  # Compare JSONs first (lighter check) then full params.
  for (k in seq_along(sim_ids)) {
    expect_equal(parallel_out[[k]]$json, serial_out[[k]]$json,
                 info = sprintf("sim_id=%d JSON differs", sim_ids[k]))
    expect_equal(parallel_out[[k]]$params, serial_out[[k]]$params,
                 info = sprintf("sim_id=%d params differ", sim_ids[k]))
  }
})


test_that("parallel mclapply produces identical shards to serial lapply", {
  skip_if_testthat_parallel()  # inner mclapply fork is unsafe inside a testthat worker
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
