# Unit tests for the Phase-1 forecast-CV psi freeze/cache substrate:
#   - prefit_rolling_cv_psi() spec_hash / manifest round-trip
#   - run_rolling_cv(psi_cache=) cache hit, missing-cutoff, and spec_hash mismatch
#   - the `ess` column contract on compiled predictions
# These exercise pure logic only — no real TF fit, no run_MOSAIC.

# ---- spec_hash key behavior ------------------------------------------------

test_that(".rcv_psi_spec_hash is deterministic and keyed on date + spec (incl n_seeds)", {
  spec <- list(architecture = "lstm_v2_hierarchical_film",
               arch_control = list(n_seeds = 10L, parallel_seeds = 2L))
  h1 <- MOSAIC:::.rcv_psi_spec_hash(as.Date("2024-01-01"), spec)
  expect_identical(h1, MOSAIC:::.rcv_psi_spec_hash(as.Date("2024-01-01"), spec))
  expect_equal(nchar(h1), 64L)

  # n_seeds MUST be part of the key
  spec_seeds <- spec; spec_seeds$arch_control$n_seeds <- 5L
  expect_false(identical(h1, MOSAIC:::.rcv_psi_spec_hash(as.Date("2024-01-01"), spec_seeds)))

  # cutoff date is part of the key
  expect_false(identical(h1, MOSAIC:::.rcv_psi_spec_hash(as.Date("2024-02-01"), spec)))

  # a modeling knob is part of the key
  spec_arch <- spec; spec_arch$feature_set <- "v7.3"
  expect_false(identical(h1, MOSAIC:::.rcv_psi_spec_hash(as.Date("2024-01-01"), spec_arch)))
})

test_that(".rcv_strip_date_keys removes harness-owned date keys (with warning)", {
  spec <- list(feature_set = "v7.3", fit_date_stop = "2024-01-01",
               pred_date_start = "2023-01-01")
  expect_warning(s <- MOSAIC:::.rcv_strip_date_keys(spec), "harness-owned")
  expect_named(s, "feature_set")
  # spec without date keys is untouched & silent
  expect_silent(MOSAIC:::.rcv_strip_date_keys(list(feature_set = "v7.3")))
})

# ---- manifest write/read round-trip ----------------------------------------

test_that("psi_manifest write/read round-trips per-cutoff entries", {
  dir_cache <- tempfile("psi_cache_"); dir.create(dir_cache)
  mpath <- file.path(dir_cache, "psi_manifest.json")
  spec <- list(feature_set = "v7.3", arch_control = list(n_seeds = 10L))
  entry <- list(list(
    cutoff = "2024-01-01", csv = "psi_2024-01-01.csv",
    sha256 = "abc", spec_hash = "deadbeef", n_seeds = 10L,
    parallel_seeds = 2L, mosaic_version = "0.0.0", laser_version = "0.0.0"))
  MOSAIC:::.rcv_psi_write_manifest(mpath, entry, spec = spec,
    pred_start = as.Date("2023-01-01"), pred_stop = as.Date("2025-01-01"),
    mosaic_ver = "0.0.0", laser_ver = "0.0.0")
  expect_true(file.exists(mpath))

  man <- MOSAIC:::.rcv_psi_read_manifest(mpath)
  expect_length(man$cutoffs, 1L)
  e <- man$cutoffs[[1]]
  expect_equal(as.character(e$cutoff), "2024-01-01")
  expect_equal(as.character(e$spec_hash), "deadbeef")
  expect_equal(as.character(e$csv), "psi_2024-01-01.csv")

  # missing file -> empty cutoffs (not an error)
  expect_length(MOSAIC:::.rcv_psi_read_manifest(file.path(dir_cache, "nope.json"))$cutoffs, 0L)
})

# ---- cache lookup: hit / missing / mismatch --------------------------------

# Build a tiny frozen cache (CSV + manifest) for a single cutoff, with a
# spec_hash computed from the supplied modeling spec (date keys stripped).
.make_fixture_cache <- function(cutoff, spec) {
  dir_cache <- tempfile("psi_cache_"); dir.create(dir_cache)
  T_chr <- as.character(as.Date(cutoff))
  csv <- file.path(dir_cache, sprintf("psi_%s.csv", T_chr))
  utils::write.csv(data.frame(iso_code = "MOZ", date = T_chr, psi = 0.5),
                   csv, row.names = FALSE)
  spec_stripped <- MOSAIC:::.rcv_strip_date_keys(spec)
  spec_hash <- MOSAIC:::.rcv_psi_spec_hash(cutoff, spec_stripped)
  entry <- list(list(
    cutoff = T_chr, csv = basename(csv),
    sha256 = MOSAIC:::.rcv_file_hash(csv), spec_hash = spec_hash,
    n_seeds = 10L, parallel_seeds = 1L,
    mosaic_version = "0.0.0", laser_version = "0.0.0"))
  MOSAIC:::.rcv_psi_write_manifest(
    file.path(dir_cache, "psi_manifest.json"), entry, spec = spec_stripped,
    pred_start = as.Date("2023-01-01"), pred_stop = as.Date("2025-01-01"),
    mosaic_ver = "0.0.0", laser_ver = "0.0.0")
  list(dir = dir_cache, csv = csv)
}

test_that(".rcv_psi_cache_lookup returns the frozen CSV on a spec_hash hit", {
  cutoff <- as.Date("2024-06-01")
  spec <- list(feature_set = "v7.3", arch_control = list(n_seeds = 10L))
  fx <- .make_fixture_cache(cutoff, spec)
  man <- MOSAIC:::.rcv_psi_read_manifest(file.path(fx$dir, "psi_manifest.json"))

  got <- MOSAIC:::.rcv_psi_cache_lookup(fx$dir, man, cutoff, spec)
  expect_equal(normalizePath(got), normalizePath(fx$csv))
})

test_that(".rcv_psi_cache_lookup hard-errors on a missing cutoff", {
  cutoff <- as.Date("2024-06-01")
  spec <- list(feature_set = "v7.3", arch_control = list(n_seeds = 10L))
  fx <- .make_fixture_cache(cutoff, spec)
  man <- MOSAIC:::.rcv_psi_read_manifest(file.path(fx$dir, "psi_manifest.json"))

  expect_error(
    MOSAIC:::.rcv_psi_cache_lookup(fx$dir, man, as.Date("2024-07-01"), spec),
    "missing cutoff")
})

test_that(".rcv_psi_cache_lookup hard-errors on a spec_hash mismatch (not silent NA)", {
  cutoff <- as.Date("2024-06-01")
  spec <- list(feature_set = "v7.3", arch_control = list(n_seeds = 10L))
  fx <- .make_fixture_cache(cutoff, spec)
  man <- MOSAIC:::.rcv_psi_read_manifest(file.path(fx$dir, "psi_manifest.json"))

  # different n_seeds -> different hash -> hard error
  spec_bad <- spec; spec_bad$arch_control$n_seeds <- 5L
  expect_error(
    MOSAIC:::.rcv_psi_cache_lookup(fx$dir, man, cutoff, spec_bad),
    "spec_hash mismatch")

  # date keys in the run spec are stripped before hashing -> still a HIT
  spec_dates <- c(spec, list(fit_date_stop = "2024-06-01"))
  expect_silent(suppressWarnings(
    MOSAIC:::.rcv_psi_cache_lookup(fx$dir, man, cutoff, spec_dates)))
})

# ---- ess column contract ---------------------------------------------------

# Build a minimal run dir carrying an ensemble_candidate.rds + a
# convergence_diagnostics.json, and assert the compiled table gains `ess`.
.make_fixture_run_dir <- function(ess_value = 106.5, write_diag = TRUE) {
  run_dir <- tempfile("cutoff_"); dir.create(run_dir)
  cal <- file.path(run_dir, "2_calibration"); dir.create(cal, recursive = TRUE)
  n_t <- 30L
  ds <- as.Date("2024-01-01"); de <- ds + (n_t - 1L)
  mk <- function(v) matrix(v, nrow = 1)
  ci_pair <- function(lo, hi) list(lower = mk(lo), upper = mk(hi))
  ens <- list(
    n_time_points = n_t, date_start = ds, date_stop = de,
    location_names = "MOZ", envelope_quantiles = c(0.025, 0.25, 0.75, 0.975),
    cases_median = mk(seq_len(n_t)), deaths_median = mk(seq_len(n_t) / 10),
    ci_bounds = list(
      cases  = list(ci_pair(seq_len(n_t) - 1, seq_len(n_t) + 1),
                    ci_pair(seq_len(n_t) - 0.5, seq_len(n_t) + 0.5)),
      deaths = list(ci_pair(seq_len(n_t)/10 - 1, seq_len(n_t)/10 + 1),
                    ci_pair(seq_len(n_t)/10 - 0.5, seq_len(n_t)/10 + 0.5))))
  saveRDS(ens, file.path(cal, "ensemble_candidate.rds"))
  if (write_diag) {
    diag_dir <- file.path(cal, "diagnostics"); dir.create(diag_dir)
    jsonlite::write_json(
      list(metrics = list(ess_best = list(value = ess_value,
                                          description = "x", status = "pass"))),
      file.path(diag_dir, "convergence_diagnostics.json"),
      auto_unbox = TRUE)
  }
  list(run_dir = run_dir, n_t = n_t, ds = ds, de = de)
}

test_that(".rcv_read_ess_best extracts metrics$ess_best$value (NA when absent)", {
  fx <- .make_fixture_run_dir(ess_value = 88.25)
  expect_equal(MOSAIC:::.rcv_read_ess_best(fx$run_dir), 88.25)

  fx0 <- .make_fixture_run_dir(write_diag = FALSE)
  expect_identical(MOSAIC:::.rcv_read_ess_best(fx0$run_dir), NA_real_)
})

test_that(".rcv_compile_all_models attaches `ess` carrying ess_best$value on every row", {
  fx <- .make_fixture_run_dir(ess_value = 123.5)
  obs_dates <- seq(as.Date("2023-06-01"), as.Date("2025-06-01"), by = "day")
  oc <- matrix(NA_real_, 1, length(obs_dates)); od <- oc

  out <- MOSAIC:::.rcv_compile_all_models(
    run_dir = fx$run_dir, run_id = "cutoff_2024-01-15",
    cutoff = as.Date("2024-01-15"), anchor = as.Date("2023-06-01"),
    embargo_days = 7L, horizons_months = c(1, 3),
    obs_cases = oc, obs_deaths = od, obs_dates = obs_dates,
    location_names = "MOZ", models = "ensemble", n_reps = 1L,
    central_method = "median")

  expect_true("ess" %in% names(out))
  expect_true(all(out$ess == 123.5))
  expect_true(is.numeric(out$ess))
  # existing columns still present (contract preserved)
  expect_true(all(c("run_id","model","iso_code","date","metric","segment",
                    "observed","pred_central","pred_median") %in% names(out)))
})

# ---- run_rolling_cv(psi_cache=) end-to-end gating (no TF / no run_MOSAIC) ----

test_that("run_rolling_cv aborts up front on a psi_cache spec_hash mismatch", {
  # Real config_default + get_location_config (pure R); the up-front cache
  # validation must hard-error BEFORE any est_suitability()/run_MOSAIC() call.
  spec <- list(feature_set = "v7.3", arch_control = list(n_seeds = 10L))
  cutoff <- as.Date("2024-06-01")
  fx <- .make_fixture_cache(cutoff, spec)

  bad_spec <- spec; bad_spec$arch_control$n_seeds <- 5L   # invalidates the hash
  expect_error(
    run_rolling_cv(
      PATHS = list(MODEL_INPUT = tempdir()), iso = "MOZ",
      n_cutoffs = 1L, latest_cutoff = cutoff, step_months = 1L,
      horizons_months = 1, embargo_weeks = 1L,
      base_config = MOSAIC::config_default, priors = MOSAIC::priors_default,
      est_suitability_spec = bad_spec, psi_cache = fx$dir,
      dir_output = tempfile("rcv_out_"), verbose = FALSE),
    "spec_hash mismatch")
})

test_that("run_rolling_cv aborts up front when psi_cache lacks a requested cutoff", {
  spec <- list(feature_set = "v7.3", arch_control = list(n_seeds = 10L))
  fx <- .make_fixture_cache(as.Date("2024-06-01"), spec)
  expect_error(
    run_rolling_cv(
      PATHS = list(MODEL_INPUT = tempdir()), iso = "MOZ",
      n_cutoffs = 1L, latest_cutoff = as.Date("2024-07-01"), step_months = 1L,
      horizons_months = 1, embargo_weeks = 1L,
      base_config = MOSAIC::config_default, priors = MOSAIC::priors_default,
      est_suitability_spec = spec, psi_cache = fx$dir,
      dir_output = tempfile("rcv_out_"), verbose = FALSE),
    "missing cutoff")
})

test_that("run_rolling_cv errors when psi_cache has no manifest", {
  empty_dir <- tempfile("empty_cache_"); dir.create(empty_dir)
  expect_error(
    run_rolling_cv(
      PATHS = list(MODEL_INPUT = tempdir()), iso = "MOZ",
      n_cutoffs = 1L, latest_cutoff = as.Date("2024-06-01"),
      base_config = MOSAIC::config_default, priors = MOSAIC::priors_default,
      psi_cache = empty_dir, dir_output = tempfile("rcv_out_"), verbose = FALSE),
    "psi_manifest.json")
})

test_that(".rcv_compile_all_models sets ess = NA when diagnostics are absent", {
  fx <- .make_fixture_run_dir(write_diag = FALSE)
  obs_dates <- seq(as.Date("2023-06-01"), as.Date("2025-06-01"), by = "day")
  oc <- matrix(NA_real_, 1, length(obs_dates)); od <- oc

  out <- MOSAIC:::.rcv_compile_all_models(
    run_dir = fx$run_dir, run_id = "cutoff_2024-01-15",
    cutoff = as.Date("2024-01-15"), anchor = as.Date("2023-06-01"),
    embargo_days = 7L, horizons_months = c(1, 3),
    obs_cases = oc, obs_deaths = od, obs_dates = obs_dates,
    location_names = "MOZ", models = "ensemble", n_reps = 1L,
    central_method = "median")

  expect_true("ess" %in% names(out))
  expect_true(all(is.na(out$ess)))
})
