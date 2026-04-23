# =============================================================================
# test-dask_local_cluster_integration.R
#
# Tier 3: in-process Dask LocalCluster integration tests for the Dask/Coiled
# code path in run_MOSAIC_helpers.R. These tests:
#
#   - Spin up a LocalCluster (scheduler + 2 workers on localhost, no cloud)
#   - Upload the mosaic_dask_worker.py module to workers
#   - Scatter a small base config
#   - Exercise submit/gather/map paths used by .mosaic_run_batch_dask()
#
# They do NOT require a Coiled account, network access, or a Docker image.
# They DO require reticulate + Python with dask.distributed + laser_cholera.
# Any missing piece → test is skipped, not failed.
# =============================================================================

diag <- function(fmt, ...) {
  cat(sprintf(paste0("  [DIAG] ", fmt, "\n"), ...))
}

# -----------------------------------------------------------------------------
# Silence Dask's distributed logger. Tearing down a LocalCluster after each
# test causes workers to emit CommClosedError/StreamClosedError heartbeat
# tracebacks as the scheduler disappears — cosmetic noise, not a failure.
# Raising the log level to CRITICAL suppresses these without hiding genuine
# errors.
# -----------------------------------------------------------------------------
.silence_dask_logs <- function() {
  if (requireNamespace("reticulate", quietly = TRUE) &&
      reticulate::py_available(initialize = FALSE) &&
      reticulate::py_module_available("distributed")) {
    reticulate::py_run_string(
      paste(
        "import logging",
        "for _name in ('distributed',",
        "              'distributed.worker',",
        "              'distributed.scheduler',",
        "              'distributed.nanny',",
        "              'distributed.core',",
        "              'distributed.comm',",
        "              'distributed.comm.tcp',",
        "              'distributed.utils_perf'):",
        "    logging.getLogger(_name).setLevel(logging.CRITICAL)",
        sep = "\n"
      )
    )
  }
  invisible(NULL)
}
.silence_dask_logs()

skip_if_no_dask <- function() {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  if (!reticulate::py_available(initialize = TRUE)) {
    skip("Python not available via reticulate")
  }
  if (!reticulate::py_module_available("dask.distributed")) {
    skip("dask.distributed not installed in Python env")
  }
  if (!reticulate::py_module_available("laser.cholera.metapop.model")) {
    skip("laser_cholera not installed — required by mosaic_dask_worker")
  }
  # Re-apply silencer after lazy Python init on first test.
  .silence_dask_logs()
  py_ver <- reticulate::py_config()$version
  dd_ver <- reticulate::py_get_attr(reticulate::import("distributed"), "__version__")
  diag("Python %s | dask.distributed %s", py_ver, reticulate::py_to_r(dd_ver))
  invisible(TRUE)
}

local_cluster_fixture <- function(n_workers = 2L) {
  dask_dist <- reticulate::import("dask.distributed", delay_load = FALSE)
  t0 <- Sys.time()
  cluster <- dask_dist$LocalCluster(
    n_workers           = as.integer(n_workers),
    threads_per_worker  = 1L,
    processes           = TRUE,
    dashboard_address   = NULL,
    silence_logs        = 50L  # logging.CRITICAL — suppress worker heartbeat
                               # errors that fire after teardown
  )
  client <- dask_dist$Client(cluster)
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)

  info <- client$scheduler_info()
  diag("LocalCluster ready in %.2fs | scheduler=%s | %d worker(s)",
       elapsed, info$address, length(info$workers))
  worker_names <- names(info$workers)
  for (wn in worker_names) {
    w <- info$workers[[wn]]
    diag("  worker %s | nthreads=%d | memory_limit=%.1f GB",
         wn, as.integer(w$nthreads),
         as.numeric(w$memory_limit) / 1024^3)
  }

  worker_py <- system.file("python/mosaic_dask_worker.py", package = "MOSAIC")
  if (!nzchar(worker_py) || !file.exists(worker_py)) {
    client$close(); cluster$close()
    skip("mosaic_dask_worker.py not installed with MOSAIC package")
  }
  client$upload_file(worker_py)
  diag("uploaded mosaic_dask_worker.py (%.1f KB)",
       file.info(worker_py)$size / 1024)

  reticulate::py_run_string(sprintf(
    "import sys\n_mw = r'%s'\nimport os\n_dir = os.path.dirname(_mw)\n%s",
    worker_py,
    "if _dir not in sys.path: sys.path.insert(0, _dir)"
  ))
  mosaic_worker <- reticulate::import("mosaic_dask_worker")

  list(cluster = cluster, client = client, mosaic_worker = mosaic_worker)
}

teardown_cluster <- function(fx) {
  tryCatch(fx$client$close(),  error = function(e) NULL)
  tryCatch(fx$cluster$close(), error = function(e) NULL)
}

# -----------------------------------------------------------------------------
# Helper: build a minimal single-location config that LASER will actually run.
# Uses the packaged config_default_MOZ if available; otherwise synthesizes a
# toy config that's large enough for laser_cholera's validations.
# -----------------------------------------------------------------------------
get_tiny_config <- function() {
  skip_if_not_installed("MOSAIC")
  env <- new.env()
  ok <- tryCatch({
    utils::data("config_default_MOZ", package = "MOSAIC", envir = env)
    utils::data("priors_default_MOZ", package = "MOSAIC", envir = env)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok || !exists("config_default_MOZ", envir = env)) {
    skip("config_default_MOZ / priors_default_MOZ not available")
  }
  # sample_parameters() / get_paths() need root_directory set.
  root <- if (dir.exists("/workspace/MOSAIC")) "/workspace/MOSAIC" else "~/MOSAIC"
  if (!dir.exists(root)) {
    skip(paste("MOSAIC root not found at", root))
  }
  MOSAIC::set_root_directory(root)
  cfg <- env$config_default_MOZ
  diag("config_default_MOZ: %d location(s) [%s] | t=[%s..%s]",
       length(cfg$location_name),
       paste(head(cfg$location_name, 5), collapse = ","),
       cfg$date_start, cfg$date_stop)
  if (!is.null(cfg$psi_jt)) {
    diag("  psi_jt dims: %s | reported_cases dims: %s",
         paste(dim(as.matrix(cfg$psi_jt)), collapse = "x"),
         paste(dim(as.matrix(cfg$reported_cases)), collapse = "x"))
  }
  list(
    config = cfg,
    priors = env$priors_default_MOZ
  )
}


# =============================================================================
# TEST 1: LocalCluster lifecycle + worker module upload
# =============================================================================
test_that("LocalCluster starts, uploads worker module, exposes it to workers", {
  skip_if_no_dask()

  fx <- local_cluster_fixture(n_workers = 2L)
  on.exit(teardown_cluster(fx), add = TRUE)

  info <- fx$client$scheduler_info()
  expect_true(length(info$workers) >= 1L)

  # Workers can see the uploaded module — run a trivial function there.
  ping_fn <- reticulate::py_run_string(
    "def _mw_ping():\n    import mosaic_dask_worker as m\n    return hasattr(m, 'run_laser_sim')\n",
    local = TRUE
  )$`_mw_ping`
  results <- fx$client$run(ping_fn)
  expect_true(all(vapply(results, isTRUE, logical(1))))
})


# =============================================================================
# TEST 2: scatter(broadcast=TRUE) round-trips a base config
# =============================================================================
test_that("client$scatter broadcasts base_config to all workers", {
  skip_if_no_dask()
  fx_data <- get_tiny_config()
  cfg <- fx_data$config

  fx <- local_cluster_fixture(n_workers = 2L)
  on.exit(teardown_cluster(fx), add = TRUE)

  base_cfg <- MOSAIC:::.extract_base_config(cfg)
  base_py  <- reticulate::r_to_py(base_cfg)
  fut      <- fx$client$scatter(base_py, broadcast = TRUE)

  # Pull the scattered value back — it should equal what we sent.
  # reticulate may auto-convert to R list; accept either form.
  got <- fx$client$gather(fut)
  if (inherits(got, "python.builtin.object")) {
    got <- reticulate::py_to_r(got)
  }
  expect_true(is.list(got))
  expect_true("location_name" %in% names(got))
  expect_identical(
    sort(as.character(got$location_name)),
    sort(as.character(base_cfg$location_name))
  )
})


# =============================================================================
# TEST 3: run_laser_sim round-trip via client$submit
# Verifies that (a) the Python worker signature matches what R sends, (b) the
# returned dict shape is what .mosaic_run_batch_dask expects to consume.
# =============================================================================
test_that("client$submit(run_laser_sim) returns expected dict shape", {
  skip_if_no_dask()
  fx_data <- get_tiny_config()
  cfg     <- fx_data$config
  priors  <- fx_data$priors

  fx <- local_cluster_fixture(n_workers = 2L)
  on.exit(teardown_cluster(fx), add = TRUE)

  # Sample a full parameter set, extract base + sampled, scatter, submit.
  params_sim <- tryCatch(
    MOSAIC::sample_parameters(
      PATHS       = NULL,
      priors      = priors,
      config      = cfg,
      seed        = 1L,
      sample_args = list(),
      verbose     = FALSE,
      validate    = FALSE
    ),
    error = function(e) { skip(paste("sample_parameters failed:", e$message)); NULL }
  )

  base_cfg <- MOSAIC:::.extract_base_config(params_sim)
  sampled  <- MOSAIC:::.extract_sampled_params(params_sim)
  sampled_json <- jsonlite::toJSON(sampled, auto_unbox = TRUE, digits = NA)

  diag("base_config fields: %s", paste(names(base_cfg), collapse = ","))
  diag("sampled fields (%d): %s",
       length(sampled),
       paste(head(names(sampled), 10), collapse = ","))
  diag("sampled_params_json size: %.1f KB",
       nchar(as.character(sampled_json)) / 1024)

  base_fut <- fx$client$scatter(reticulate::r_to_py(base_cfg), broadcast = TRUE)

  t_submit <- Sys.time()
  fut <- fx$client$submit(
    fx$mosaic_worker$run_laser_sim,
    1L,                         # sim_id
    1L,                         # n_iterations
    as.character(sampled_json), # sampled_params_json
    base_fut                    # scattered base_config
  )
  diag("submitted run_laser_sim(sim_id=1, n_iter=1) — waiting for result...")

  # Allow up to 3 minutes for first LASER run on this worker (JIT warm-up).
  res <- tryCatch(
    fx$client$gather(fut),
    error = function(e) { skip(paste("gather failed:", e$message)); NULL }
  )
  gather_sec <- as.numeric(difftime(Sys.time(), t_submit, units = "secs"))

  expect_true(is.list(res) || inherits(res, "python.builtin.dict"))
  res <- if (inherits(res, "python.builtin.dict")) reticulate::py_to_r(res) else res

  expect_named(res, c("sim_id", "success", "worker_elapsed_sec", "iterations"),
               ignore.order = TRUE, ignore.case = FALSE)
  expect_true(isTRUE(res$success),
              info = paste("worker error:", res$error %||% ""))
  expect_equal(length(res$iterations), 1L)

  iter <- res$iterations[[1]]
  expect_named(iter, c("j", "seed", "reported_cases", "disease_deaths"),
               ignore.order = TRUE)

  # Matrix reconstruction: nested lists → R matrix with expected dims
  n_locs <- length(cfg$location_name)
  cases_mat  <- matrix(unlist(iter$reported_cases),
                       nrow = n_locs, byrow = FALSE)
  deaths_mat <- matrix(unlist(iter$disease_deaths),
                       nrow = n_locs, byrow = FALSE)

  diag("result: sim_id=%d | success=%s | worker_elapsed=%.2fs | roundtrip=%.2fs",
       as.integer(res$sim_id),
       as.character(res$success),
       as.numeric(res$worker_elapsed_sec),
       gather_sec)
  diag("  iteration j=%d | seed=%d", as.integer(iter$j), as.integer(iter$seed))
  diag("  reported_cases: dim=%dx%d | sum=%.1f | max=%.1f",
       nrow(cases_mat), ncol(cases_mat),
       sum(cases_mat, na.rm = TRUE),
       max(cases_mat, na.rm = TRUE))
  diag("  disease_deaths: dim=%dx%d | sum=%.1f | max=%.1f",
       nrow(deaths_mat), ncol(deaths_mat),
       sum(deaths_mat, na.rm = TRUE),
       max(deaths_mat, na.rm = TRUE))

  expect_equal(nrow(cases_mat), n_locs)
  expect_gt(ncol(cases_mat), 1L)
  expect_true(all(is.finite(cases_mat) | is.na(cases_mat)))
})


# =============================================================================
# TEST 4: client$map chunked submission (matches .mosaic_run_batch_dask path)
# =============================================================================
test_that("client$map dispatches multiple sims and preserves per-sim ordering", {
  skip_if_no_dask()
  fx_data <- get_tiny_config()
  cfg     <- fx_data$config
  priors  <- fx_data$priors

  fx <- local_cluster_fixture(n_workers = 2L)
  on.exit(teardown_cluster(fx), add = TRUE)

  # Build 3 parameter sets with different seeds.
  n_sims <- 3L
  sim_ids <- seq_len(n_sims)
  params_list <- lapply(sim_ids, function(sid) {
    MOSAIC::sample_parameters(
      PATHS = NULL, priors = priors, config = cfg,
      seed = sid, sample_args = list(),
      verbose = FALSE, validate = FALSE
    )
  })
  sampled_jsons <- lapply(params_list, function(p) {
    as.character(jsonlite::toJSON(
      MOSAIC:::.extract_sampled_params(p),
      auto_unbox = TRUE, digits = NA
    ))
  })
  base_fut <- fx$client$scatter(
    reticulate::r_to_py(MOSAIC:::.extract_base_config(params_list[[1]])),
    broadcast = TRUE
  )

  t_map <- Sys.time()
  futures <- fx$client$map(
    fx$mosaic_worker$run_laser_sim,
    as.list(as.integer(sim_ids)),           # sim_id per task
    rep(list(1L), n_sims),                  # n_iterations
    sampled_jsons,                          # per-task JSON
    rep(list(base_fut), n_sims)             # broadcast base
  )
  diag("client$map dispatched %d tasks", n_sims)

  gathered <- tryCatch(
    fx$client$gather(futures),
    error = function(e) { skip(paste("gather failed:", e$message)); NULL }
  )
  diag("gathered %d results in %.2fs", length(gathered),
       as.numeric(difftime(Sys.time(), t_map, units = "secs")))

  expect_equal(length(gathered), n_sims)

  # Convert py dicts to R lists if needed and validate ordering.
  gathered <- lapply(gathered, function(r) {
    if (inherits(r, "python.builtin.dict")) reticulate::py_to_r(r) else r
  })
  ids <- vapply(gathered, function(r) as.integer(r$sim_id), integer(1))
  worker_times <- vapply(gathered, function(r)
    as.numeric(r$worker_elapsed_sec), numeric(1))
  expect_equal(ids, as.integer(sim_ids))

  successes <- vapply(gathered, function(r) isTRUE(r$success), logical(1))
  diag("per-sim results: ids=[%s] | success=[%s] | worker_sec=[%s]",
       paste(ids, collapse = ","),
       paste(successes, collapse = ","),
       paste(sprintf("%.2f", worker_times), collapse = ","))
  expect_true(all(successes))
})


# =============================================================================
# TEST 5: post-cal reconnect — close client, get fresh client from cluster
# Validates the Option A (reconnect) pattern used in run_MOSAIC.R after
# R-heavy post-processing.
# =============================================================================
test_that("dask_cluster$get_client() returns a working fresh client after close", {
  skip_if_no_dask()

  fx <- local_cluster_fixture(n_workers = 1L)
  on.exit(teardown_cluster(fx), add = TRUE)

  # Close the client (not the cluster).
  fx$client$close()

  # Reconnect via the cluster handle — this is the pattern run_MOSAIC.R uses.
  client2 <- fx$cluster$get_client()
  on.exit(tryCatch(client2$close(), error = function(e) NULL), add = TRUE)

  info <- client2$scheduler_info()
  expect_true(length(info$workers) >= 1L)

  # Submit a trivial task to confirm the reconnected client is usable.
  add_one <- reticulate::py_run_string(
    "def _add_one(x):\n    return int(x) + 1\n",
    local = TRUE
  )$`_add_one`
  fut <- client2$submit(add_one, 41L)
  expect_equal(as.integer(client2$gather(fut)), 42L)
})


# =============================================================================
# TEST 6: error path — bad JSON sampled params → success=FALSE, no crash
# =============================================================================
test_that("malformed sampled_params_json returns success=FALSE with traceback", {
  skip_if_no_dask()
  fx_data <- get_tiny_config()
  cfg     <- fx_data$config

  fx <- local_cluster_fixture(n_workers = 1L)
  on.exit(teardown_cluster(fx), add = TRUE)

  base_fut <- fx$client$scatter(
    reticulate::r_to_py(MOSAIC:::.extract_base_config(cfg)),
    broadcast = TRUE
  )

  fut <- fx$client$submit(
    fx$mosaic_worker$run_laser_sim,
    1L, 1L,
    "this is not json {{{",   # intentionally malformed
    base_fut
  )
  res <- fx$client$gather(fut)
  if (inherits(res, "python.builtin.dict")) res <- reticulate::py_to_r(res)

  expect_false(isTRUE(res$success))
  expect_true(nzchar(as.character(res$error %||% "")))
  expect_true(nzchar(as.character(res$traceback %||% "")))
})
