# Tests for run_MOSAIC() resume capability (resume = TRUE).
#
# These exercise the factored, engine-free internals so they run without the
# Python/LASER engine. The full end-to-end interrupt/resume reproducibility
# check lives in the smoke test (claude/smoke_test_resume.R) since it needs the
# simulation engine.

# ---- helpers ---------------------------------------------------------------

new_samples_dir <- function() {
  d <- file.path(tempfile("resume_"), "samples")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

make_shard <- function(dir, sim_id, rows = 1L) {
  df <- data.frame(
    sim       = rep(as.integer(sim_id), rows),
    iter      = 1L,
    seed_sim  = as.integer(sim_id),
    seed_iter = NA_real_,
    likelihood = -10
  )
  arrow::write_parquet(df, file.path(dir, sprintf("sim_%07d.parquet", sim_id)))
}

state_template <- function() {
  list(
    total_sims_run = 0L, total_sims_successful = 0L, batch_number = 0L,
    batch_success_rates = numeric(), batch_sizes_used = integer(),
    phase = "calibration", calib_batches = 0L, r2_ess = NA_real_,
    calibration_done = FALSE, ess_history = list(), ess_tracking = list(),
    param_names_est = c("a", "b"), converged = FALSE, predictive_done = FALSE,
    mode = "auto", fixed_target = NA_integer_, phase_batch_count = 0L,
    phase_last = NULL
  )
}

# ---- signature -------------------------------------------------------------

test_that("run_MOSAIC signature adds resume defaulting to FALSE", {
  fmls <- formals(run_MOSAIC)
  expect_true("resume" %in% names(fmls))
  expect_identical(eval(fmls$resume), FALSE)
  # Only addition; rest of the public signature is unchanged.
  expect_setequal(
    names(fmls),
    c("config", "priors", "dir_output", "control", "resume", "cluster", "dask_spec")
  )
})

# ---- .mosaic_resume_scan ---------------------------------------------------

test_that(".mosaic_resume_scan uses max id as watermark, not count", {
  d <- new_samples_dir()
  for (id in c(1L, 2L, 3L, 5L, 8L)) make_shard(d, id)
  scan <- MOSAIC:::.mosaic_resume_scan(d)
  expect_equal(scan$watermark, 8L)   # critical: max, never count (5)
  expect_equal(scan$n, 5L)
  expect_equal(scan$ids, c(1L, 2L, 3L, 5L, 8L))
})

test_that(".mosaic_resume_scan handles empty and missing directories", {
  d <- new_samples_dir()
  expect_equal(MOSAIC:::.mosaic_resume_scan(d)$n, 0L)
  expect_equal(MOSAIC:::.mosaic_resume_scan(d)$watermark, 0L)
  expect_equal(MOSAIC:::.mosaic_resume_scan(file.path(d, "does_not_exist"))$n, 0L)
})

test_that(".mosaic_resume_scan quarantines bad shards and sweeps temp files", {
  d <- new_samples_dir()
  make_shard(d, 1L); make_shard(d, 2L)
  writeLines("garbage", file.path(d, "sim_0000003.parquet"))  # corrupt
  file.create(file.path(d, "sim_0000004.parquet"))            # zero-byte
  file.create(file.path(d, ".mosaic_tmp_sim_0000009.parquet_abc"))  # orphaned temp

  scan <- MOSAIC:::.mosaic_resume_scan(d)

  expect_equal(scan$ids, c(1L, 2L))
  expect_equal(scan$watermark, 2L)
  expect_false(file.exists(file.path(d, ".mosaic_tmp_sim_0000009.parquet_abc")))
  expect_true(file.exists(file.path(d, ".quarantine", "sim_0000003.parquet")))
  expect_true(file.exists(file.path(d, ".quarantine", "sim_0000004.parquet")))
})

# ---- shard validation (read data + schema/type, not just num_rows) ---------

test_that(".mosaic_resume_scan quarantines schema/type-invalid shards", {
  d <- new_samples_dir()
  make_shard(d, 1L); make_shard(d, 2L)
  # likelihood written as a string → must be rejected (would coerce the whole
  # combined column to character and silently zero the posterior)
  arrow::write_parquet(
    data.frame(sim = 3L, iter = 1L, seed_sim = 3L, seed_iter = NA_real_,
               likelihood = "not_a_number"),
    file.path(d, "sim_0000003.parquet"))
  # missing the likelihood column entirely → must be rejected
  arrow::write_parquet(
    data.frame(sim = 4L, iter = 1L, seed_sim = 4L, foo = 1.0),
    file.path(d, "sim_0000004.parquet"))

  scan <- MOSAIC:::.mosaic_resume_scan(d)
  expect_equal(scan$ids, c(1L, 2L))        # only the well-formed shards survive
  expect_equal(scan$watermark, 2L)
  expect_true(file.exists(file.path(d, ".quarantine", "sim_0000003.parquet")))
  expect_true(file.exists(file.path(d, ".quarantine", "sim_0000004.parquet")))
})

# ---- control$likelihood drift guard ----------------------------------------

test_that(".mosaic_resume_check_inputs guards control$likelihood drift", {
  base <- tempfile("ctl_"); inp <- file.path(base, "1_inputs")
  dir.create(inp, recursive = TRUE)
  dirs <- list(inputs = inp)
  priors <- list(a = 1); config <- list(location_name = "ETH")
  wj <- function(x, f) jsonlite::write_json(x, f, pretty = TRUE, auto_unbox = TRUE, digits = NA)
  wj(priors, file.path(inp, "priors.json"))
  wj(config, file.path(inp, "config.json"))

  lik <- list(weight_cases = 1, weight_deaths = 1, weight_wis = 0)
  control <- list(likelihood = lik)
  # control.json is written as sim_params: list(control = control, ...)
  wj(list(control = control, timestamp = "t0"), file.path(inp, "control.json"))

  # identical likelihood → passes
  expect_true(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors, control))

  # changed weight → hard error
  control2 <- list(likelihood = list(weight_cases = 1, weight_deaths = 2, weight_wis = 0))
  expect_error(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors, control2),
               "control\\$likelihood")

  # control omitted (back-compat) → likelihood check skipped, no error
  expect_true(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors))
})

# ---- checkpoint round-trip -------------------------------------------------

test_that("resume checkpoint round-trips decision state", {
  d <- tempfile("ckpt_"); dir.create(d, recursive = TRUE)
  f <- file.path(d, "resume_checkpoint.rds")
  st <- state_template()
  st$total_sims_run <- 2000L; st$batch_number <- 4L; st$phase <- "predictive"
  st$phase_batch_count <- 2L; st$calibration_done <- TRUE; st$converged <- FALSE
  st$ess_tracking <- list(list(batch = 1L, total_sims = 500L, threshold_ess = 20,
                               min_ess = 10, median_ess = 30, max_ess = 50))
  st$batch_success_rates <- c(99, 100); st$batch_sizes_used <- c(500L, 500L)

  MOSAIC:::.mosaic_save_checkpoint(st, f)
  ck <- MOSAIC:::.mosaic_load_checkpoint(f)

  expect_equal(ck$batch_number, 4L)
  expect_equal(ck$phase, "predictive")
  expect_true(ck$calibration_done)
  expect_equal(ck$ess_tracking, st$ess_tracking)
  expect_equal(ck$batch_sizes_used, c(500L, 500L))
})

test_that(".mosaic_load_checkpoint returns NULL on missing/corrupt files", {
  expect_null(MOSAIC:::.mosaic_load_checkpoint(tempfile()))
  f <- tempfile(fileext = ".rds"); writeLines("not an rds file", f)
  expect_null(MOSAIC:::.mosaic_load_checkpoint(f))
})

# ---- .mosaic_reconstruct_state ---------------------------------------------

make_dirs <- function() {
  base <- tempfile("recon_")
  dirs <- list(
    cal_samples = file.path(base, "2_calibration/samples"),
    cal_state   = file.path(base, "2_calibration/state")
  )
  dir.create(dirs$cal_samples, recursive = TRUE)
  dir.create(dirs$cal_state, recursive = TRUE)
  dirs
}

test_that(".mosaic_reconstruct_state restores exactly from checkpoint", {
  dirs <- make_dirs()
  for (id in 1:5) make_shard(dirs$cal_samples, id)

  ck <- state_template()
  ck$batch_number <- 3L; ck$phase <- "predictive"; ck$phase_batch_count <- 1L
  ck$calib_batches <- 2L; ck$r2_ess <- 0.97; ck$calibration_done <- TRUE
  ck$converged <- TRUE   # already-done run → no ESS recompute needed
  ck$ess_tracking <- list(list(batch = 3L, total_sims = 5L, threshold_ess = 120,
                               min_ess = 110, median_ess = 130, max_ess = 150))
  MOSAIC:::.mosaic_save_checkpoint(ck, file.path(dirs$cal_state, "resume_checkpoint.rds"))

  control <- list(calibration = list(batch_size_adaptive = 2L))
  st <- MOSAIC:::.mosaic_reconstruct_state(state_template(), dirs, control, c("a", "b"))

  expect_equal(st$total_sims_run, 5L)   # disk watermark, NOT checkpoint's total
  expect_equal(st$total_sims_successful, 5L)
  expect_equal(st$batch_number, 3L)
  expect_equal(st$phase, "predictive")
  expect_true(st$calibration_done)
  expect_true(st$converged)
})

test_that(".mosaic_reconstruct_state bootstraps from shards without a checkpoint", {
  dirs <- make_dirs()
  for (id in 1:10) make_shard(dirs$cal_samples, id)  # < 50 → ESS check short-circuits

  control <- list(calibration = list(batch_size_adaptive = 5L),
                  io = list(), targets = list())
  st <- MOSAIC:::.mosaic_reconstruct_state(state_template(), dirs, control, c("a", "b"))

  expect_equal(st$total_sims_run, 10L)
  expect_equal(st$total_sims_successful, 10L)
  expect_equal(st$batch_number, 2L)     # ceiling(10 / 5)
  expect_false(st$converged)
})

test_that(".mosaic_reconstruct_state returns unchanged state when no shards (fresh)", {
  dirs <- make_dirs()
  control <- list(calibration = list(batch_size_adaptive = 5L))
  st <- MOSAIC:::.mosaic_reconstruct_state(state_template(), dirs, control, c("a", "b"))
  expect_equal(st$total_sims_run, 0L)   # untouched → behaves as a fresh run
  expect_equal(st$batch_number, 0L)
})

test_that(".mosaic_reconstruct_state bootstrap marks calibration done when budget met", {
  dirs <- make_dirs()
  for (id in 1:10) make_shard(dirs$cal_samples, id)  # < 50 → ESS check short-circuits
  # batch_size 2 → ceiling(10/2) = 5 batches; max_batches_adaptive 4 → budget met.
  control <- list(calibration = list(batch_size_adaptive = 2L, max_batches_adaptive = 4L),
                  io = list(), targets = list())
  st <- MOSAIC:::.mosaic_reconstruct_state(state_template(), dirs, control, c("a", "b"))
  expect_equal(st$batch_number, 5L)
  expect_true(st$calibration_done)   # avoids re-running the full adaptive allotment
})

test_that(".mosaic_reconstruct_state refreshes ESS over the full pool when shards exceed checkpoint", {
  # Auto-mode continuation: a partial batch wrote shards after the last
  # checkpoint, so reconstruct must re-run the ESS check over ALL shards and
  # append a fresh tracking point. Uses real (engine-free) ESS computation.
  skip_if_not_installed("arrow")
  dirs <- make_dirs()
  set.seed(42)
  # Use real estimated-parameter names so calc_model_ess_parameter accepts them.
  pnames <- head(MOSAIC::estimated_parameters$parameter_name, 2L)
  n <- 55L  # > 50 so .mosaic_ess_check_update_state does not short-circuit
  for (id in seq_len(n)) {
    df <- data.frame(sim = id, iter = 1L, seed_sim = id, seed_iter = NA_real_,
                     likelihood = -runif(1, 1, 100))
    df[[pnames[1]]] <- runif(1)
    df[[pnames[2]]] <- runif(1)
    arrow::write_parquet(df, file.path(dirs$cal_samples, sprintf("sim_%07d.parquet", id)))
  }

  # Checkpoint: not converged, last tracking point at total_sims = 30 (< 55 on disk).
  ck <- state_template()
  ck$batch_number <- 1L; ck$converged <- FALSE
  ck$ess_tracking <- list(list(batch = 1L, total_sims = 30L, threshold_ess = 10,
                               min_ess = 5, median_ess = 12, max_ess = 20))
  MOSAIC:::.mosaic_save_checkpoint(ck, file.path(dirs$cal_state, "resume_checkpoint.rds"))

  control <- mosaic_control_defaults()
  st <- MOSAIC:::.mosaic_reconstruct_state(state_template(), dirs, control, pnames)

  expect_equal(st$total_sims_run, n)                # disk watermark
  expect_gt(length(st$ess_tracking), 1L)            # refresh appended a point
  newest <- st$ess_tracking[[length(st$ess_tracking)]]
  expect_equal(newest$total_sims, n)                # computed over the FULL pool
})

# ---- input integrity check -------------------------------------------------

test_that(".mosaic_resume_check_inputs passes on match, errors on drift", {
  base <- tempfile("inputs_"); inp <- file.path(base, "1_inputs")
  dir.create(inp, recursive = TRUE)
  dirs <- list(inputs = inp)

  priors <- list(tau_i = list(shape = 2, rate = 1))
  config <- list(location_name = "ETH", value = 1)
  wj <- function(x, f) jsonlite::write_json(x, f, pretty = TRUE, auto_unbox = TRUE, digits = NA)
  wj(priors, file.path(inp, "priors.json"))
  wj(config, file.path(inp, "config.json"))

  expect_true(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors))

  expect_error(
    MOSAIC:::.mosaic_resume_check_inputs(dirs, config, list(tau_i = list(shape = 20, rate = 1))),
    "priors"
  )
  expect_error(
    MOSAIC:::.mosaic_resume_check_inputs(dirs, list(location_name = "ETH", value = 2), priors),
    "config"
  )
})

test_that(".mosaic_resume_check_inputs is a no-op when inputs not yet persisted", {
  base <- tempfile("inputs_"); inp <- file.path(base, "1_inputs")
  dir.create(inp, recursive = TRUE)
  dirs <- list(inputs = inp)
  # No priors.json/config.json on disk (fresh resume) → must not error.
  expect_true(MOSAIC:::.mosaic_resume_check_inputs(dirs, list(x = 1), list(y = 2)))
})

# ---- engine-version (deaths-scale) guard -----------------------------------

test_that(".mosaic_lc_pre013 classifies the v0.13 deaths-scale boundary", {
  f <- MOSAIC:::.mosaic_lc_pre013
  expect_true(f("0.12.5"))
  expect_true(f("0.12"))
  expect_true(f("0.0.9"))
  expect_false(f("0.13.0"))
  expect_false(f("0.13.5"))
  expect_false(f("1.0.0"))
  # PEP440 suffixes reduce to the leading integer of each component
  expect_true(f("0.12rc1"))
  expect_false(f("0.13.0.dev1"))
  expect_false(f("0.13.0+local"))
  # unparseable / degenerate → NA
  expect_true(is.na(f("garbage")))
  expect_true(is.na(f("")))
  expect_true(is.na(f(NA_character_)))
  expect_true(is.na(f("0")))   # major only, no minor
})

test_that(".mosaic_lc_deaths_scale flags only boundary-crossing version pairs", {
  g <- MOSAIC:::.mosaic_lc_deaths_scale
  # same side of the boundary → compatible (regression for the old || bug:
  # two different pre-0.13 versions must NOT be flagged incompatible)
  expect_equal(g("0.12.1", "0.12.5"), "compatible")
  expect_equal(g("0.13.0", "0.13.5"), "compatible")
  # straddles the boundary → incompatible (both directions)
  expect_equal(g("0.12.5", "0.13.0"), "incompatible")
  expect_equal(g("0.13.0", "0.12.5"), "incompatible")
  # unclassifiable either side → unknown
  expect_equal(g("garbage", "0.13.0"), "unknown")
  expect_equal(g("0.12.0", ""), "unknown")
})

test_that(".mosaic_resume_check_inputs hard-errors across the v0.13 engine boundary", {
  # Requires the installed laser-cholera to be classifiable as >= v0.13 so a
  # persisted 0.12.x crosses the boundary against the live version.
  current_lc <- tryCatch({
    if (reticulate::py_available(initialize = FALSE)) {
      as.character(reticulate::import("importlib.metadata", delay_load = FALSE)$version("laser-cholera"))
    } else NA_character_
  }, error = function(e) NA_character_)
  skip_if(is.na(current_lc) || isTRUE(MOSAIC:::.mosaic_lc_pre013(current_lc)),
          "installed laser-cholera not classifiable as >= v0.13")

  base <- tempfile("eng_"); inp <- file.path(base, "1_inputs"); dir.create(inp, recursive = TRUE)
  dirs <- list(inputs = inp)
  priors <- list(a = 1); config <- list(location_name = "ETH")
  wj <- function(x, f) jsonlite::write_json(x, f, pretty = TRUE, auto_unbox = TRUE, digits = NA)
  wj(priors, file.path(inp, "priors.json"))
  wj(config, file.path(inp, "config.json"))

  # persisted pre-0.13 vs current >= 0.13 → boundary crossed → hard error
  wj(list(python = list(pkg_laser_cholera = "0.12.4")), file.path(inp, "environment.json"))
  expect_error(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors),
               "engine version mismatch")

  # persisted == current → compatible → no error
  wj(list(python = list(pkg_laser_cholera = current_lc)), file.path(inp, "environment.json"))
  expect_true(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors))
})
