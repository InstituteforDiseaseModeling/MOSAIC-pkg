# Regression tests for the empty-validation-fold skip in .psi_fit_predict_rw_cv
# (commit 979f066e). A rolling-CV fold whose validation slice is empty (e.g. a
# terminal future fold, or a per-capita target that is NA across the window)
# must be SKIPPED rather than crashing the keras fit with "validation data
# required when n_epochs_fixed is NULL". These tests pin that behaviour with a
# STUB fit_predict_fn (no real keras) by mocking the two slice helpers so we can
# inject empty / non-empty validation slices deterministically and record which
# folds the arch is actually fitted on.

ns          <- "MOSAIC"
rw_cv       <- getFromNamespace(".psi_fit_predict_rw_cv", ns)
check_ram   <- getFromNamespace(".psi_check_parallel_seeds_ram", ns)
worker_gb   <- getFromNamespace(".PSI_PER_SEED_WORKER_GB", ns)

# A non-empty slice the stub arch can "fit": minimal train + val payload. n_val
# > 0 signals a fittable fold; the slice contents are never inspected by the
# fit because we stub fit_predict_fn.
.mk_slice <- function(n_val) {
  base <- list(
    X_train = matrix(0, 2, 1), y_train = c(0, 1),
    country_ids_train = c(1L, 1L), region_ids_train = c(1L, 1L),
    confidence_weight_train = NULL,
    X_val = NULL, y_val = NULL,
    country_ids_val = NULL, region_ids_val = NULL,
    confidence_weight_val = NULL,
    n_train = 2L, n_val = 0L)
  if (n_val > 0L) {
    base$X_val            <- matrix(0, n_val, 1)
    base$y_val            <- rep(0, n_val)
    base$country_ids_val  <- rep(1L, n_val)
    base$region_ids_val   <- rep(1L, n_val)
    base$n_val            <- as.integer(n_val)
  }
  base
}

# A minimal data_bundle: only the fields .psi_fit_predict_rw_cv touches directly.
# The slice helpers are mocked, so pool_data/seq_params/encoders are not read.
.mk_bundle <- function(n_steps) {
  steps <- lapply(seq_len(n_steps), function(k)
    list(step = k,
         train_end  = as.Date("2022-01-01") + 30 * k,
         test_start = as.Date("2022-02-01") + 30 * k,
         test_end   = as.Date("2022-06-01") + 30 * k))
  list(rw_steps = steps, cutoff_date = as.Date("2023-01-01"))
}

# Stub arch fit: records the seed/epochs it was called with and returns a fixed
# best_epoch. n_epochs_fixed present => this is the final full-IS refit.
.mk_stub_fit <- function(env) {
  function(data_bundle, seed, hyperparams) {
    is_final <- !is.null(hyperparams$n_epochs_fixed)
    if (is_final) {
      env$final_called   <- TRUE
      env$final_epochs   <- as.integer(hyperparams$n_epochs_fixed)
    } else {
      # Record each CV fold fit by its validation length (proxy for fold id).
      # .psi_fit_predict_rw_cv copies X_val (not n_val) into the sub-bundle, so
      # nrow(X_val) is the fittable-fold fingerprint that actually reaches here.
      env$cv_n_val <- c(env$cv_n_val, nrow(data_bundle$X_val))
    }
    list(pred = data.frame(iso = "MOZ", date = as.Date("2023-01-01"), psi = 0.5),
         n_epochs = 17L, val_loss = 0.1, val_metric = 0.2,
         loss_type = "bce")
  }
}

# ---------------------------------------------------------------------------
# (a)+(b)+(c) a middle fold with an empty validation slice is skipped without
# error; folds WITH validation are still fitted; best_epoch for the skipped
# fold is NA.
# ---------------------------------------------------------------------------
test_that("empty-val fold is skipped (no fit, NA best_epoch); other folds fitted; no error", {
  env <- new.env(); env$cv_n_val <- integer(0); env$final_called <- FALSE
  # 3 folds: fold 2 has an empty validation slice.
  n_vals <- c(20L, 0L, 25L)
  testthat::local_mocked_bindings(
    .psi_slice_rw_step = function(data_bundle, step) .mk_slice(n_vals[[step$step]]),
    .psi_slice_full_is = function(data_bundle) .mk_slice(0L),
    .package = ns)

  res <- expect_no_error(
    rw_cv(.mk_bundle(3L), fit_predict_fn = .mk_stub_fit(env),
          seed = 7L, hyperparams = list(epochs = 200L), verbose = FALSE))

  # (a) the empty-val fold (fold 2) was NOT fitted: only the two non-empty
  #     folds reached the stub (recorded by their n_val 20 and 25).
  expect_equal(sort(env$cv_n_val), c(20L, 25L))
  # (a) best_epoch for the skipped fold is NA; the others are recorded.
  be <- res$rw_diagnostics$best_epochs
  expect_length(be, 3L)
  expect_true(is.na(be[2L]))
  expect_equal(be[c(1L, 3L)], c(17L, 17L))
  # (d) final full-IS refit still proceeded, at median(best_epoch) = 17.
  expect_true(env$final_called)
  expect_equal(env$final_epochs, 17L)
  expect_equal(res$n_epochs, 17L)
  expect_false(isTRUE(res$rw_diagnostics$all_steps_failed))
})

# ---------------------------------------------------------------------------
# Terminal empty fold (the documented "terminal future fold" case): last fold
# empty, earlier folds fitted.
# ---------------------------------------------------------------------------
test_that("terminal empty-val fold is skipped and earlier folds still fit", {
  env <- new.env(); env$cv_n_val <- integer(0); env$final_called <- FALSE
  n_vals <- c(15L, 30L, 0L)
  testthat::local_mocked_bindings(
    .psi_slice_rw_step = function(data_bundle, step) .mk_slice(n_vals[[step$step]]),
    .psi_slice_full_is = function(data_bundle) .mk_slice(0L),
    .package = ns)

  res <- expect_no_error(
    rw_cv(.mk_bundle(3L), fit_predict_fn = .mk_stub_fit(env),
          seed = 1L, hyperparams = list(epochs = 200L), verbose = FALSE))

  expect_equal(sort(env$cv_n_val), c(15L, 30L))
  expect_true(is.na(res$rw_diagnostics$best_epochs[3L]))
  expect_true(env$final_called)
  expect_equal(res$n_epochs, 17L)
})

# ---------------------------------------------------------------------------
# All-folds-empty edge case: no fold informs best_epoch, so the final refit
# uses the min(50, epochs) fallback (the round-3 amplitude-blowup guard), emits
# a warning, sets all_steps_failed = TRUE, and still refits.
# ---------------------------------------------------------------------------
test_that("all folds empty -> min(50, epochs) fallback, warning, all_steps_failed", {
  env <- new.env(); env$cv_n_val <- integer(0); env$final_called <- FALSE
  testthat::local_mocked_bindings(
    .psi_slice_rw_step = function(data_bundle, step) .mk_slice(0L),  # every fold empty
    .psi_slice_full_is = function(data_bundle) .mk_slice(0L),
    .package = ns)

  # epochs = 200 -> floored to 50.
  expect_warning(
    res <- rw_cv(.mk_bundle(3L), fit_predict_fn = .mk_stub_fit(env),
                 seed = 1L, hyperparams = list(epochs = 200L), verbose = FALSE),
    "no valid best_epoch")

  # No CV fold was fitted at all.
  expect_length(env$cv_n_val, 0L)
  # All best_epochs NA.
  expect_true(all(is.na(res$rw_diagnostics$best_epochs)))
  expect_true(isTRUE(res$rw_diagnostics$all_steps_failed))
  # Final refit still ran, at the floored fallback (min(50, 200) = 50).
  expect_true(env$final_called)
  expect_equal(env$final_epochs, 50L)
  expect_equal(res$n_epochs, 50L)
})

test_that("all folds empty with epochs < 50 keeps the smaller epoch count", {
  env <- new.env(); env$cv_n_val <- integer(0); env$final_called <- FALSE
  testthat::local_mocked_bindings(
    .psi_slice_rw_step = function(data_bundle, step) .mk_slice(0L),
    .psi_slice_full_is = function(data_bundle) .mk_slice(0L),
    .package = ns)

  expect_warning(
    res <- rw_cv(.mk_bundle(2L), fit_predict_fn = .mk_stub_fit(env),
                 seed = 1L, hyperparams = list(epochs = 30L), verbose = FALSE),
    "no valid best_epoch")
  # min(50, 30) = 30.
  expect_equal(res$n_epochs, 30L)
  expect_equal(env$final_epochs, 30L)
})

# ===========================================================================
# B7: parallel_seeds OOM guard. .psi_check_parallel_seeds_ram() warns (does NOT
# cap) when the projected concurrent-worker RAM footprint exceeds ~85% of probed
# system memory. The serial default (parallel_seeds=1) must stay silent. The RAM
# probe is mocked so the test is deterministic across hosts.
# ===========================================================================

test_that("parallel_seeds=1 (serial default) never warns", {
  testthat::local_mocked_bindings(
    .psi_total_system_ram_gb = function() 32, .package = ns)
  expect_silent(check_ram(parallel_seeds = 1L, n_seeds = 5L))
  expect_null(check_ram(parallel_seeds = 1L, n_seeds = 5L))
})

# The OOM projection multiplies by n_workers = min(parallel_seeds, n_seeds,
# cores - 2); on a tiny CI box (<= 3 cores) the clamp drops concurrency to 1 and
# the guard is (correctly) silent, so the small-RAM warn assertions only hold
# with enough cores to actually spawn multiple workers.
skip_if_few_cores <- function(min_workers = 4L) {
  nc <- parallel::detectCores()
  if (is.na(nc)) nc <- 2L
  testthat::skip_if(nc - 2L < min_workers,
                    sprintf("needs >= %d spawnable cores (have %d)", min_workers + 2L, nc))
}

test_that("high parallel_seeds on a small box warns about OOM (does not error/cap)", {
  skip_if_few_cores(4L)
  # 8 workers * 6 GB = 48 GB projected vs a 16 GB box -> over 0.85*16 -> warn.
  testthat::local_mocked_bindings(
    .psi_total_system_ram_gb = function() 16, .package = ns)
  w <- capture_warnings(check_ram(parallel_seeds = 10L, n_seeds = 8L))
  expect_match(w, "risks OOM", all = FALSE)
  expect_match(w, "parallel_seeds=10", all = FALSE)
})

test_that("parallel_seeds within the RAM budget does not warn", {
  # 2 workers * 6 GB = 12 GB vs 64 GB box -> well under 0.85*64 -> silent.
  testthat::local_mocked_bindings(
    .psi_total_system_ram_gb = function() 64, .package = ns)
  expect_silent(check_ram(parallel_seeds = 2L, n_seeds = 2L))
})

test_that("the production parallel_seeds=10 warns on a 32 GB box but not a 448 GB host", {
  skip_if_few_cores(6L)   # need ~6 workers * 6 GB = 36 GB to exceed 0.85*32
  testthat::local_mocked_bindings(
    .psi_total_system_ram_gb = function() 32, .package = ns)
  # 10 seeds but clamped to cores-2; on a typical CI box that is still several
  # workers * 6 GB, comfortably over 0.85*32 -> warn.
  expect_warning(check_ram(parallel_seeds = 10L, n_seeds = 10L), "risks OOM")

  testthat::local_mocked_bindings(
    .psi_total_system_ram_gb = function() 448, .package = ns)
  # The documented production host: 448 GB absorbs the footprint -> silent.
  expect_silent(check_ram(parallel_seeds = 10L, n_seeds = 10L))
})

test_that("unprobeable RAM warns to ensure headroom (NA path)", {
  skip_if_few_cores(4L)
  testthat::local_mocked_bindings(
    .psi_total_system_ram_gb = function() NA_real_, .package = ns)
  expect_warning(check_ram(parallel_seeds = 8L, n_seeds = 8L),
                 "could not be probed")
})

test_that("per-worker GB budget constant is the documented conservative value", {
  expect_equal(worker_gb, 6)
})
