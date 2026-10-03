# Regression tests for the rolling-origin forecast-CV harness fixes from the
# production-readiness review (group cfr-cv): as-of epidemic peaks, psi
# coverage / cache-window validation, psi-fit isolation from MODEL_INPUT,
# per-cutoff output cleaning, incremental manifest, weeks_ahead / oos_range
# off-by-one, ensemble_opt gating, compile model validation, and the prefit
# cache manifest (union across calls, prediction window in the cache key,
# docs-figure isolation of the v7.4 panel build). No TF fit and no run_MOSAIC.

# ---- fixtures -------------------------------------------------------------

.rv_psi_csv <- function(path, iso = "MOZ", from = "2023-01-01", to = "2025-01-01",
                        psi = 0.5) {
  d <- seq(as.Date(from), as.Date(to), by = "day")
  utils::write.csv(do.call(rbind, lapply(iso, function(i)
    data.frame(iso_code = i, date = as.character(d), psi = psi))),
    path, row.names = FALSE)
  path
}

# A frozen psi cache holding `cutoffs`, each entry hashed from `spec`, predicted
# from config_default's start (run_rolling_cv() requires the frozen psi to cover
# the config start).
.rv_cache <- function(cutoffs, spec, pred_start = MOSAIC::config_default$date_start,
                      pred_stop = "2025-01-01") {
  dir_cache <- tempfile("rv_cache_"); dir.create(dir_cache)
  spec_s <- MOSAIC:::.rcv_strip_date_keys(spec)
  entries <- lapply(as.list(as.Date(cutoffs)), function(T_k) {
    T_chr <- as.character(T_k)
    csv <- .rv_psi_csv(file.path(dir_cache, sprintf("psi_%s.csv", T_chr)),
                       from = pred_start, to = pred_stop)
    list(cutoff = T_chr, csv = basename(csv), sha256 = MOSAIC:::.rcv_file_hash(csv),
         spec_hash = MOSAIC:::.rcv_psi_spec_hash(T_k, spec_s),
         n_seeds = 10L, parallel_seeds = 1L, mosaic_version = "0.0.0")
  })
  MOSAIC:::.rcv_psi_write_manifest(
    file.path(dir_cache, "psi_manifest.json"), entries, spec = spec_s,
    pred_start = as.Date(pred_start), pred_stop = as.Date(pred_stop),
    mosaic_ver = "0.0.0")
  dir_cache
}

.rv_who_dir <- function() {
  set.seed(11)
  isos <- c("MOZ", MOSAIC::iso_codes_mosaic[1:8])
  who <- expand.grid(iso_code = isos, year = 2000:2025, stringsAsFactors = FALSE)
  who$cases_total <- rpois(nrow(who), 3000)
  who$deaths_total <- rbinom(nrow(who), who$cases_total,
                             plogis(qlogis(0.02) + rnorm(nrow(who), 0, 0.4)))
  who$country <- who$iso_code
  d <- tempfile("rv_who_"); dir.create(d)
  utils::write.csv(who, file.path(d, "who_afro_annual.csv"), row.names = FALSE)
  d
}

# Minimal run dir: candidate ensemble (+ optional fallback optimized ensemble
# and subset_opt.rds marker).
.rv_run_dir <- function(with_opt_rds = TRUE, with_subset_opt = FALSE) {
  run_dir <- tempfile("rv_cutoff_"); cal <- file.path(run_dir, "2_calibration")
  dir.create(cal, recursive = TRUE)
  n_t <- 40L; ds <- as.Date("2024-01-01")
  mk <- function(v) matrix(v, nrow = 1)
  cp <- function(lo, hi) list(lower = mk(lo), upper = mk(hi))
  ens <- list(n_time_points = n_t, date_start = ds, date_stop = ds + n_t - 1L,
              location_names = "MOZ", envelope_quantiles = c(0.025, 0.25, 0.75, 0.975),
              cases_median = mk(seq_len(n_t)), deaths_median = mk(seq_len(n_t) / 10),
              ci_bounds = list(cases = list(cp(0, 100), cp(1, 50)),
                               deaths = list(cp(0, 10), cp(0.1, 5))))
  saveRDS(ens, file.path(cal, "ensemble_candidate.rds"))
  if (with_opt_rds) saveRDS(ens, file.path(cal, "ensemble_optimized.rds"))
  if (with_subset_opt) saveRDS(list(optimal_n = 10L), file.path(cal, "subset_opt.rds"))
  run_dir
}

.rv_compile <- function(run_dir, models) {
  obs_dates <- seq(as.Date("2023-06-01"), as.Date("2025-06-01"), by = "day")
  oc <- matrix(NA_real_, 1, length(obs_dates))
  MOSAIC:::.rcv_compile_all_models(
    run_dir = run_dir, run_id = "cutoff_2024-01-15", cutoff = as.Date("2024-01-15"),
    anchor = as.Date("2023-06-01"), embargo_days = 7L, horizons_months = 1,
    obs_cases = oc, obs_deaths = oc, obs_dates = obs_dates, location_names = "MOZ",
    models = models, n_reps = 1L, central_method = "median")
}

# ---- epidemic peaks ---------------------------------------------------------

test_that(".rcv_asof_epidemic_peaks drops peaks whose scoring window reaches past T", {
  T_k <- as.Date("2024-06-01")
  pk <- data.frame(iso_code = c("MOZ", "MOZ", "MOZ", "MOZ"),
                   peak_date = c("2024-01-10", "2024-05-18", "2024-05-19", "2025-03-20"),
                   stringsAsFactors = FALSE)
  out <- MOSAIC:::.rcv_asof_epidemic_peaks(pk, T_k)
  expect_equal(out$peak_date, c("2024-01-10", "2024-05-18"))   # 05-18 + 14 = T
  expect_true(all(as.Date(out$peak_date) + 14 <= T_k))

  # none left / NULL -> a 0-row frame with the columns, never NULL (NULL makes
  # calc_model_likelihood fall back to the full package dataset)
  none <- MOSAIC:::.rcv_asof_epidemic_peaks(pk, as.Date("2023-01-01"))
  expect_true(is.data.frame(none)); expect_equal(nrow(none), 0L)
  expect_true(all(c("iso_code", "peak_date") %in% names(none)))
  expect_equal(nrow(MOSAIC:::.rcv_asof_epidemic_peaks(NULL, T_k)), 0L)
})

# ---- run_rolling_cv end-to-end (psi cache, mocked run_MOSAIC) -----------------

test_that("run_rolling_cv: as-of peaks, clean_output forced, incremental manifest, oos_range", {
  skip_if_not_installed("mgcv")
  spec <- list(feature_set = "v7.3", arch_control = list(n_seeds = 10L))
  cuts <- as.Date(c("2024-05-01", "2024-06-01"))
  cache <- .rv_cache(cuts, spec)
  dir_out <- tempfile("rv_out_")

  seen <- new.env(); seen$calls <- list()
  local_mocked_bindings(
    run_MOSAIC = function(config, priors, dir_output, control, ...) {
      man_now <- if (file.exists(file.path(dir_out, "manifest.json")))
        jsonlite::read_json(file.path(dir_out, "manifest.json"), simplifyVector = TRUE) else NULL
      seen$calls[[length(seen$calls) + 1L]] <- list(
        peaks = config$epidemic_peaks, control = control, manifest = man_now)
      invisible(NULL)
    },
    .rcv_compile_all_models = function(...) NULL,
    .package = "MOSAIC")
  warns <- character(0)
  withCallingHandlers(
    run_rolling_cv(
      PATHS = list(MODEL_INPUT = tempdir(), DATA_WHO_ANNUAL = .rv_who_dir()), iso = "MOZ",
      n_cutoffs = 2L, latest_cutoff = max(cuts), step_months = 1L,
      horizons_months = 1, embargo_weeks = 1L,
      base_config = MOSAIC::config_default, priors = MOSAIC::priors_default,
      est_suitability_spec = spec, psi_cache = cache,
      dir_output = dir_out, verbose = FALSE),
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") })
  # a v7.3 cache is not built from per-cutoff leak-free panels
  expect_true(any(grepl("not strictly leak-free", warns)))

  expect_length(seen$calls, 2L)
  for (i in 1:2) {
    pk <- seen$calls[[i]]$peaks
    expect_true(is.data.frame(pk))                     # never NULL (no fallback)
    expect_true(all(as.Date(pk$peak_date) + 14 <= cuts[i]))
    expect_true(isTRUE(seen$calls[[i]]$control$paths$clean_output))
  }
  # MOSAIC::config_default does carry post-cutoff MOZ peaks, so the filter bit.
  full_pk <- MOSAIC::config_default$epidemic_peaks
  expect_true(any(full_pk$iso_code == "MOZ" & as.Date(full_pk$peak_date) > cuts[2]))

  # manifest written after cutoff 1, before cutoff 2 ran
  m1 <- seen$calls[[2]]$manifest
  expect_false(is.null(m1))
  expect_identical(m1$status, "running")
  expect_equal(nrow(m1$runs), 1L)
  expect_identical(m1$runs$cutoff_date[1], as.character(cuts[1]))

  man <- jsonlite::read_json(file.path(dir_out, "manifest.json"), simplifyVector = TRUE)
  expect_identical(man$status, "complete")
  expect_equal(nrow(man$runs), 2L)
  # OOS range starts at the first OOS date, T + embargo + 1
  expect_identical(man$runs$oos_range[[1]][1], as.character(cuts[1] + 8L))
})

test_that("run_rolling_cv hard-errors when the psi cache window ends before the OOS end", {
  spec <- list(feature_set = "v7.3", arch_control = list(n_seeds = 10L))
  cut <- as.Date("2024-06-01")
  cache <- .rv_cache(cut, spec, pred_stop = "2024-06-15")   # OOS runs to 2024-07-09
  expect_error(suppressWarnings(
    run_rolling_cv(
      PATHS = list(MODEL_INPUT = tempdir()), iso = "MOZ",
      n_cutoffs = 1L, latest_cutoff = cut, step_months = 1L,
      horizons_months = 1, embargo_weeks = 1L,
      base_config = MOSAIC::config_default, priors = MOSAIC::priors_default,
      est_suitability_spec = spec, psi_cache = cache,
      dir_output = tempfile("rv_out_"), verbose = FALSE)),
    "predicted only to 2024-06-15")
})

test_that("run_rolling_cv(psi_cache = NULL) fits psi in scratch and never touches MODEL_INPUT", {
  skip_if_not_installed("mgcv")
  model_input <- tempfile("rv_model_input_"); dir.create(model_input)
  canon <- file.path(model_input, "pred_psi_suitability_day.csv")
  writeLines("SENTINEL", canon)
  cut <- as.Date("2024-06-01")
  dir_out <- tempfile("rv_out_")
  seen <- new.env()
  local_mocked_bindings(
    est_suitability = function(PATHS, fit_date_stop, pred_date_start, pred_date_stop, ...) {
      seen$model_input <- PATHS$MODEL_INPUT
      .rv_psi_csv(file.path(PATHS$MODEL_INPUT, "pred_psi_suitability_day.csv"),
                  from = pred_date_start, to = pred_date_stop, psi = 0.3)
      invisible(NULL)
    },
    run_MOSAIC = function(config, ...) { seen$psi <- config$psi_jt; invisible(NULL) },
    .rcv_compile_all_models = function(...) NULL,
    .package = "MOSAIC")
  suppressWarnings(run_rolling_cv(
    PATHS = list(MODEL_INPUT = model_input, DATA_WHO_ANNUAL = .rv_who_dir()), iso = "MOZ",
    n_cutoffs = 1L, latest_cutoff = cut, step_months = 1L,
    horizons_months = 1, embargo_weeks = 1L,
    base_config = MOSAIC::config_default, priors = MOSAIC::priors_default,
    dir_output = dir_out, verbose = FALSE))
  expect_identical(readLines(canon), "SENTINEL")            # canonical file untouched
  expect_false(identical(seen$model_input, model_input))    # fit ran in scratch
  expect_false(dir.exists(seen$model_input))                # scratch cleaned up
  expect_true(file.exists(file.path(dir_out, "runs", "psi_cutoff_2024-06-01.csv")))
  expect_true(all(seen$psi == 0.3))
})

# ---- psi matrix coverage ------------------------------------------------------

test_that(".rolling_cv_psi_matrix refuses to carry psi flat into the scored window", {
  dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
  csv <- .rv_psi_csv(tempfile(fileext = ".csv"), iso = "AAA",
                     from = "2024-01-01", to = "2024-03-31", psi = 0.9)
  # ends before the scored window end -> error (was: silent flat LOCF)
  expect_error(MOSAIC:::.rolling_cv_psi_matrix(csv, "AAA", dates,
                                               required_stop = as.Date("2024-06-30")),
               "ends before the last scored OOS date")
  # a location absent from the CSV -> error (was: all-NA row)
  expect_error(MOSAIC:::.rolling_cv_psi_matrix(csv, c("AAA", "BBB"), dates,
                                               required_stop = as.Date("2024-03-01")),
               "no rows for location\\(s\\) BBB")
  # ends after the scored window but before the config stop -> flat, with warning
  expect_warning(m <- MOSAIC:::.rolling_cv_psi_matrix(csv, "AAA", dates,
                                                      required_stop = as.Date("2024-03-15")),
                 "held flat over those dates")
  expect_equal(unname(m["AAA", ncol(m)]), 0.9)
  # starts after the config start -> error (no back-fill of the leading edge)
  late <- .rv_psi_csv(tempfile(fileext = ".csv"), iso = "AAA",
                      from = "2024-02-01", to = "2024-12-31")
  expect_error(MOSAIC:::.rolling_cv_psi_matrix(late, "AAA", dates), "starts after the config start")
})

# ---- labels -------------------------------------------------------------------

test_that(".rolling_cv_label: week 1 is the first seven OOS days", {
  cutoff <- as.Date("2024-06-01")
  dates <- seq(cutoff - 5, cutoff + 60, by = "day")
  lab <- MOSAIC:::.rolling_cv_label(dates, cutoff, 7L, c(1, 3))
  oos <- dates[lab$segment == "OOS"]
  expect_equal(min(oos), cutoff + 8L)
  wk <- lab$weeks_ahead[lab$segment == "OOS"]
  tab <- table(wk)
  expect_equal(as.integer(tab[c("1", "2", "3")]), c(7L, 7L, 7L))
  expect_equal(wk[oos == cutoff + 14L], 1L)
  expect_equal(wk[oos == cutoff + 15L], 2L)
})

# ---- ensemble_opt gating + compile model validation -------------------------

test_that("ensemble_opt is emitted only when the optimizer selected a subset", {
  # fallback file only (optimize_subset off / empty subset) -> skipped, warned
  rd <- .rv_run_dir(with_opt_rds = TRUE, with_subset_opt = FALSE)
  expect_warning(out <- .rv_compile(rd, c("ensemble", "ensemble_opt")),
                 "did not select a subset")
  expect_setequal(unique(out$model), "ensemble")
  # optimizer ran -> emitted
  rd2 <- .rv_run_dir(with_opt_rds = TRUE, with_subset_opt = TRUE)
  out2 <- .rv_compile(rd2, c("ensemble", "ensemble_opt"))
  expect_setequal(unique(out2$model), c("ensemble", "ensemble_opt"))
})

test_that("ensemble_opt falls back to summary.json when subset_opt.rds is missing", {
  # subset_opt.rds is saved in a non-fatal tryCatch; the summary's tier count is
  # set on the same optimizer-selected branch.
  rd <- .rv_run_dir(with_opt_rds = TRUE, with_subset_opt = FALSE)
  dir.create(file.path(rd, "3_results"))
  jsonlite::write_json(list(n_ensemble_params_tier = 50L), file.path(rd, "3_results", "summary.json"),
                       auto_unbox = TRUE)
  out <- .rv_compile(rd, c("ensemble", "ensemble_opt"))
  expect_setequal(unique(out$model), c("ensemble", "ensemble_opt"))
  # optimizer off: the tier count is NA (written as null) -> skipped
  jsonlite::write_json(list(n_ensemble_params_tier = NA), file.path(rd, "3_results", "summary.json"),
                       auto_unbox = TRUE, null = "null", na = "null")
  expect_warning(out2 <- .rv_compile(rd, c("ensemble", "ensemble_opt")), "did not select a subset")
  expect_setequal(unique(out2$model), "ensemble")
})

test_that("run_rolling_cv(optimize_subset = FALSE) drops ensemble_opt up front", {
  skip_if_not_installed("mgcv")
  spec <- list(feature_set = "v7.3", arch_control = list(n_seeds = 10L))
  cuts <- as.Date("2024-06-01")
  cache <- .rv_cache(cuts, spec)
  seen <- new.env(); seen$models <- list()
  local_mocked_bindings(
    run_MOSAIC = function(...) invisible(NULL),
    .rcv_compile_all_models = function(..., models) { seen$models[[length(seen$models) + 1L]] <- models; NULL },
    .package = "MOSAIC")
  expect_message(suppressWarnings(run_rolling_cv(
    PATHS = list(MODEL_INPUT = tempdir(), DATA_WHO_ANNUAL = .rv_who_dir()), iso = "MOZ",
    n_cutoffs = 1L, latest_cutoff = max(cuts), step_months = 1L,
    horizons_months = 1, embargo_weeks = 1L, optimize_subset = FALSE,
    base_config = MOSAIC::config_default, priors = MOSAIC::priors_default,
    est_suitability_spec = spec, psi_cache = cache,
    dir_output = tempfile("rv_out_"), verbose = FALSE)), "'ensemble_opt' is dropped")
  expect_gt(length(seen$models), 0L)
  expect_false(any(vapply(seen$models, function(m) "ensemble_opt" %in% m, logical(1))))
})

test_that("compile_rolling_cv_predictions rejects an unknown model name", {
  d <- tempfile("rv_man_"); dir.create(d)
  jsonlite::write_json(list(spec = list(iso = "MOZ", anchor_date = "2023-01-01",
                                        horizons_months = 1, embargo_weeks = 1),
                            runs = list()),
                       file.path(d, "manifest.json"), auto_unbox = TRUE)
  expect_error(compile_rolling_cv_predictions(d, models = c("ensemble", "opt"), write = FALSE),
               "unknown: .opt.")
})

# ---- prefit cache manifest ---------------------------------------------------

.rv_fake_est <- function(env) {
  function(PATHS, fit_date_stop, pred_date_start, pred_date_stop, ...) {
    env$n_fits <- if (is.null(env$n_fits)) 1L else env$n_fits + 1L
    env$model_input <- c(env$model_input, PATHS$MODEL_INPUT)
    .rv_psi_csv(file.path(PATHS$MODEL_INPUT, "pred_psi_suitability_day.csv"),
                from = pred_date_start, to = pred_date_stop)
    invisible(NULL)
  }
}

test_that("prefit keeps earlier cutoffs in the manifest and keys cache hits on the window", {
  env <- new.env()
  local_mocked_bindings(est_suitability = .rv_fake_est(env), .package = "MOSAIC")
  model_input <- tempfile("rv_mi_"); dir.create(model_input)
  canon <- file.path(model_input, "pred_psi_suitability_day.csv")
  writeLines("SENTINEL", canon)
  P <- list(MODEL_INPUT = model_input)
  cache <- tempfile("rv_prefit_")
  spec <- list(feature_set = "v7.3")

  suppressWarnings(prefit_rolling_cv_psi(P, cutoffs = "2024-03-01", est_suitability_spec = spec,
                        pred_date_start = "2023-01-01", pred_date_stop = "2025-01-01",
                        dir_cache = cache, verbose = FALSE))
  suppressWarnings(prefit_rolling_cv_psi(P, cutoffs = "2025-03-01", est_suitability_spec = spec,
                        pred_date_start = "2023-01-01", pred_date_stop = "2025-06-01",
                        dir_cache = cache, verbose = FALSE))
  man <- MOSAIC:::.rcv_psi_read_manifest(file.path(cache, "psi_manifest.json"))
  cuts <- vapply(man$cutoffs, function(e) as.character(e$cutoff), "")
  expect_setequal(cuts, c("2024-03-01", "2025-03-01"))     # union, not overwrite
  e24 <- man$cutoffs[[match("2024-03-01", cuts)]]
  expect_identical(as.character(e24$pred_date_stop), "2025-01-01")   # per-entry window kept
  expect_equal(env$n_fits, 2L)

  # same cutoff + spec but a longer prediction window -> refit, not a cache hit
  suppressWarnings(prefit_rolling_cv_psi(P, cutoffs = "2024-03-01", est_suitability_spec = spec,
                        pred_date_start = "2023-01-01", pred_date_stop = "2025-06-01",
                        dir_cache = cache, verbose = FALSE))
  expect_equal(env$n_fits, 3L)
  # identical window -> cache hit
  suppressWarnings(prefit_rolling_cv_psi(P, cutoffs = "2024-03-01", est_suitability_spec = spec,
                        pred_date_start = "2023-01-01", pred_date_stop = "2025-06-01",
                        dir_cache = cache, verbose = FALSE))
  expect_equal(env$n_fits, 3L)

  # every fit ran in a private scratch MODEL_INPUT; the canonical file survives
  expect_false(any(env$model_input == model_input))
  expect_identical(readLines(canon), "SENTINEL")
})

test_that("prefit keeps the fit config and records the pooled seeds and anchor end", {
  # SCV-2: psi_suitability_config.json is the only record of which seeds were
  # pooled; the isolated fit deleted it with its scratch dir, so a cutoff
  # pooled over 4 of 5 seeds looked like a normal manifest entry.
  env <- new.env()
  fake <- .rv_fake_est(env)
  local_mocked_bindings(est_suitability = function(PATHS, ...) {
    fake(PATHS, ...)
    jsonlite::write_json(list(n_seeds = 5L, n_seeds_ok = 4L, seeds_ok = I(1:4),
                              seeds_failed = I(5L), target_anchor_end = "2024-01-07",
                              provenance = list(source_csv_md5 = "abc123")),
                         file.path(PATHS$MODEL_INPUT, "psi_suitability_config.json"),
                         auto_unbox = TRUE)
    invisible(NULL)
  }, .package = "MOSAIC")
  cache <- tempfile("rv_prefit_")
  suppressWarnings(prefit_rolling_cv_psi(list(MODEL_INPUT = tempdir()), cutoffs = "2024-03-01",
                        est_suitability_spec = list(feature_set = "v7.3"),
                        pred_date_start = "2023-01-01", pred_date_stop = "2025-01-01",
                        dir_cache = cache, verbose = FALSE))
  expect_true(file.exists(file.path(cache, "psi_2024-03-01_config.json")))
  man <- MOSAIC:::.rcv_psi_read_manifest(file.path(cache, "psi_manifest.json"))
  e <- man$cutoffs[[1]]
  expect_identical(e$psi_config, "psi_2024-03-01_config.json")
  expect_equal(as.integer(e$n_seeds), 5L)       # spec had no arch_control$n_seeds
  expect_equal(as.integer(e$n_seeds_ok), 4L)
  expect_equal(as.integer(unlist(e$seeds_failed)), 5L)
  expect_identical(e$target_anchor_end, "2024-01-07")
  expect_identical(e$source_csv_md5, "abc123")
})

test_that("an isolated psi fit without a config removes a stale copied config", {
  env <- new.env()
  local_mocked_bindings(est_suitability = .rv_fake_est(env), .package = "MOSAIC")
  d <- withr::local_tempdir()
  dest <- file.path(d, "psi_x.csv")
  writeLines("{}", file.path(d, "psi_x_config.json"))
  out <- MOSAIC:::.rcv_fit_psi_isolated(
    list(PATHS = list(MODEL_INPUT = d), fit_date_stop = "2024-03-01",
         pred_date_start = "2024-01-01", pred_date_stop = "2024-02-01"), dest)
  expect_true(file.exists(dest))
  expect_null(attr(out, "config_json"))
  expect_false(file.exists(file.path(d, "psi_x_config.json")))
})

test_that("prefit warns that the canonical-panel (non-v7.4) path is not leak-free", {
  env <- new.env()
  local_mocked_bindings(est_suitability = .rv_fake_est(env), .package = "MOSAIC")
  expect_warning(
    prefit_rolling_cv_psi(list(MODEL_INPUT = tempdir()), cutoffs = "2024-03-01",
                          pred_date_start = "2023-01-01", pred_date_stop = "2025-01-01",
                          dir_cache = tempfile("rv_prefit_"), verbose = FALSE),
    "not strictly leak-free")
})

test_that("the v7.4 panel build keeps hazard-GAM diagnostics out of DOCS_FIGURES", {
  root <- withr::local_tempdir()
  dchw <- file.path(root, "dchw"); dir.create(dchw)
  docs <- file.path(root, "docs_figures"); dir.create(docs)
  cache <- file.path(root, "cache"); dir.create(cache)
  writeLines("iso_code,year,week,cases", file.path(dchw, "cholera_surveillance_weekly_combined.csv"))
  seen <- new.env()
  local_mocked_bindings(
    compile_suitability_data = function(PATHS, ...) {
      seen$docs <- PATHS$DOCS_FIGURES
      utils::write.csv(data.frame(x = 1), file.path(PATHS$DATA_CHOLERA_WEEKLY,
                       "cholera_country_weekly_suitability_data.csv"), row.names = FALSE)
      invisible(NULL)
    }, .package = "MOSAIC")
  MOSAIC:::.rcv_build_leakfree_panel_v74(
    PATHS = list(DATA_CHOLERA_WEEKLY = dchw, DOCS_FIGURES = docs),
    cutoff = as.Date("2018-06-30"), compile_date_start = "2015-01-01",
    out_csv = file.path(cache, "panel.csv"), verbose = FALSE)
  expect_false(is.null(seen$docs))
  expect_false(identical(seen$docs, docs))
  expect_length(list.files(docs, recursive = TRUE), 0L)
})
