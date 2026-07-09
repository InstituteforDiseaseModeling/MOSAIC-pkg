#!/usr/bin/env Rscript
# =============================================================================
# forecast_cv_experiment.R  --  SOURCE OF TRUTH for the MOSAIC rolling-origin
# forecast cross-validation experiment.
#
# The whole experiment is defined by the SPEC block below: units (countries),
# explicit cutoff dates, horizons, embargo, psi pooling. Re-running with the same
# SPEC + package + laser version reproduces the experiment (psi is FROZEN to a
# cache so the stochastic LSTM fit is fixed once; see TWO-PHASE below).
#
# WHAT IT DOES (red-teamed dev plan: claude/plan_forecast_cv/PLAN.md)
#   TWO-PHASE, contention-free:
#     PREFIT   : fit psi ONCE per cutoff (global, all ISOs), SERIALLY, each fit
#                using the whole box; freeze each to a cache CSV (no TF race).
#     CALIBRATE: per (unit x cutoff), inject the FROZEN psi, mask obs > T,
#                calibrate weeks <= T, project forward. NO TensorFlow here, so
#                cells run fully parallel (no global-psi-CSV race).
#     SCORE    : per-unit out-of-sample scoring vs climatology + manifest + plots.
#
# LOCKED DECISIONS (PLAN.md):
#   D1 per-country INDEPENDENT (a unit may be a coupled ISO vector).
#   D2 psi pooled over >=10 seeds (logit-median). parallel_seeds is execution-
#      only and is NOT a determinism lever: the POOLED psi is statistically
#      equivalent across modes, but per-seed draws are not bitwise-identical
#      (thread count changes float reduction order + recurrent_dropout). The
#      FROZEN cache is the source of truth (everyone downstream reads one file).
#   D4 OOS psi uses REALIZED climate/ENSO (perfect foresight). Admissible because
#      the goal is testing LSTM+MOSAIC MODEL performance -> results are HINDCAST /
#      conditional model skill, NOT operational forecast skill. Label them so.
#   D5 cutoffs start >= 50% through the series, so frozen ICs are pure burn-in
#      upstream and cannot leak the future.
#
# RUN
#   # dry run (validate SPEC, print the matrix, no compute):
#   Rscript inst/examples/forecast_cv_experiment.R
#   # full run on one box (prefit serial -> calibrate -> score):
#   FORECAST_CV_DRYRUN=0 Rscript inst/examples/forecast_cv_experiment.R
#
# FULLY PARALLEL ON DUGONG (the right way -- no TF contention):
#   1) build the psi cache ONCE (serial, whole box):
#      FORECAST_CV_DRYRUN=0 FORECAST_CV_PHASE=prefit Rscript .../forecast_cv_experiment.R
#   2) launch the cells in parallel, one process per (unit x cutoff), each a slice:
#      FORECAST_CV_DRYRUN=0 FORECAST_CV_PHASE=calibrate \
#        FORECAST_CV_UNIT=NGA FORECAST_CV_CUTOFF=2024-06-01 FORECAST_CV_CORES=18 \
#        Rscript .../forecast_cv_experiment.R           # x (units x cutoffs)
#   3) compile + score once everything is done:
#      FORECAST_CV_DRYRUN=0 FORECAST_CV_PHASE=score Rscript .../forecast_cv_experiment.R
#   (PHASE defaults to "all" = prefit -> calibrate -> score in one process.)
# =============================================================================

suppressMessages({
     root <- Sys.getenv("MOSAIC_ROOT", unset = "/Users/johngiles/MOSAIC")
     MOSAIC::set_root_directory(root)
     library(MOSAIC)
})
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- per-process core budget + thread env -----------------------------------
CORES <- as.integer(Sys.getenv("FORECAST_CV_CORES",
                    unset = as.character(max(1L, parallel::detectCores() - 2L))))
# BLAS/numba pinned to 1 (calibration parallelizes ACROSS workers). OMP + TF
# intra-op get the per-process slice for the (sequential) psi fit; MOSAIC_PSI_
# CORE_BUDGET hands est_suitability this slice and the seed spawner focuses each
# TF worker to budget/n_workers via a real tf.config cap.
Sys.setenv(OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1",
           TBB_NUM_THREADS = "1", NUMBA_NUM_THREADS = "1", OMP_NUM_THREADS = as.character(CORES),
           MOSAIC_PSI_CORE_BUDGET = as.character(CORES),
           MOSAIC_PSI_TF_INTRAOP = as.character(CORES), MOSAIC_PSI_TF_INTEROP = "1")

# RAM-safe default for psi parallel_seeds (~6 GB per concurrent seed-worker):
# min(5, 0.6*RAM/6, n_seeds); NA RAM -> 1 (safe). Override via env.
.ram_gb <- function() {
     os <- Sys.info()[["sysname"]]
     v <- tryCatch({
          if (os == "Linux")
               as.numeric(gsub("[^0-9]", "", system("grep MemTotal /proc/meminfo", intern = TRUE)))/1024/1024
          else if (os == "Darwin") as.numeric(system("sysctl -n hw.memsize", intern = TRUE))/1024^3
          else NA_real_
     }, error = function(e) NA_real_)
     if (length(v) != 1L || !is.finite(v)) NA_real_ else v
}

# =============================================================================
# SPEC  --  the experiment definition (edit ONLY this block)
# =============================================================================
.ramgb <- .ram_gb()
.ps_default <- if (is.na(.ramgb)) 1L else max(1L, min(5L, floor(0.6 * .ramgb / 6)))
SPEC <- list(

     # --- units (per-country INDEPENDENT; a vector element = coupled metapop) ---
     # NGA is EXPLORATORY-ONLY (surveillance data-quality confound voids its
     # climatology baseline) -- never pooled / never the headline (PLAN D3).
     units = list("NGA", "MOZ", "ETH"),

     # --- in-sample start (= base_config$date_start; NULL uses config_default) --
     # To change it, supply a config built for that window via base_config_rds;
     # do NOT mutate date_start (misaligns the date-indexed arrays).
     start_date = NULL,

     # --- explicit cutoff dates (forecast origins) -----------------------------
     # 6-month spacing -> non-overlapping 6mo horizons; all in the recent 1-2yr;
     # all past ic_t0 and >50% through the series; all scoreable (obs coverage
     # verified: NGA->2026-05, MOZ->2026-06, ETH->2026-03).
     cutoff_dates = as.Date(c("2024-06-01", "2024-12-01", "2025-06-01")),

     # --- horizons (months) -- single PRIMARY horizon kills the nesting problem --
     horizons_months = c(2, 4, 6),
     primary_horizon = 6,

     # --- embargo (gap T -> OOS start), channel-specific ----------------------
     # cases 4wk; deaths >=6wk (onset->death dwell + death-event->report lag),
     # 8wk for ETH (long gamma_1 dwell). Applied per-metric in scoring.
     embargo_weeks_cases  = 4L,
     embargo_weeks_deaths = 6L,
     embargo_weeks_deaths_eth = 8L,

     # --- psi ensemble pooling (D2) -------------------------------------------
     psi_n_seeds        = as.integer(Sys.getenv("FORECAST_CV_N_SEEDS", "10")),
     psi_parallel_seeds = as.integer(Sys.getenv("FORECAST_CV_PSI_PARALLEL_SEEDS",
                                                as.character(.ps_default))),

     # --- scoring -------------------------------------------------------------
     metrics   = c("cases", "deaths"),
     baselines = c("seasonal", "persistence", "persistence_last"),  # seasonal=climatology=PRIMARY
     ess_min   = as.numeric(Sys.getenv("FORECAST_CV_ESS_MIN", "50")),  # weight-ESS floor
     min_cells_ci = 5L,                                                # suppress CI below this n

     # --- models + central tendency -------------------------------------------
     models         = c("ensemble", "ensemble_opt", "medoid"),
     central_method = "median",

     # --- output --------------------------------------------------------------
     out_dir = Sys.getenv("FORECAST_CV_OUT",
                          unset = file.path(root, "MOSAIC-pkg", "claude", "forecast_cv_experiment")),

     # --- calibration control (NULL = cheap experiment control; gate on ESS) --
     control = NULL,
     base_config_rds = NULL
)

PHASE   <- match.arg(Sys.getenv("FORECAST_CV_PHASE", "all"),
                     c("all", "prefit", "calibrate", "score"))
DRY_RUN <- !identical(Sys.getenv("FORECAST_CV_DRYRUN", unset = "1"), "0")
psi_cache_dir <- file.path(SPEC$out_dir, "psi_cache")

# Single-cell restriction (applies to the CALIBRATE cell list only; PREFIT always
# builds the full cutoff cache). FORECAST_CV_UNIT accepts "+" for a coupled unit.
.u_env <- Sys.getenv("FORECAST_CV_UNIT",   unset = "")
.c_env <- Sys.getenv("FORECAST_CV_CUTOFF", unset = "")
cell_units   <- if (nzchar(.u_env)) list(strsplit(.u_env, "[+]")[[1]]) else SPEC$units
cell_cutoffs <- if (nzchar(.c_env)) as.Date(strsplit(.c_env, ",")[[1]]) else as.Date(SPEC$cutoff_dates)

# =============================================================================
# resolve config + validate (D5 + window alignment)
# =============================================================================
base_config <- if (!is.null(SPEC$base_config_rds)) readRDS(SPEC$base_config_rds) else MOSAIC::config_default
cfg_start <- as.Date(base_config$date_start); cfg_stop <- as.Date(base_config$date_stop)
if (!is.null(SPEC$start_date) && as.Date(SPEC$start_date) != cfg_start)
     stop(sprintf("SPEC$start_date (%s) != base_config$date_start (%s); supply a config windowed to your start via base_config_rds (do not mutate date_start).",
                  SPEC$start_date, cfg_start))
cutoffs_all <- sort(as.Date(SPEC$cutoff_dates))
halfway <- cfg_start + as.integer(0.5 * as.numeric(cfg_stop - cfg_start))
if (any(cutoffs_all < halfway))
     warning(sprintf("D5: cutoff(s) before the 50%% mark (%s) risk IC leakage.", format(halfway)), call. = FALSE)
if (any(cutoffs_all <= cfg_start) || any(cutoffs_all >= cfg_stop))
     stop("All cutoffs must lie strictly inside [", cfg_start, ", ", cfg_stop, "].")

unit_ids <- vapply(SPEC$units, function(u) paste(u, collapse = "+"), character(1))
cat("=========== FORECAST-CV EXPERIMENT (HINDCAST / model skill) ===========\n")
cat(sprintf("phase        : %s   dry_run: %s\n", PHASE, DRY_RUN))
cat(sprintf("config window: %s -> %s   (50%% mark %s)\n", cfg_start, cfg_stop, format(halfway)))
cat(sprintf("units        : %s   (NGA = exploratory-only)\n", paste(unit_ids, collapse = ", ")))
cat(sprintf("cutoffs      : %s\n", paste(format(cutoffs_all), collapse = ", ")))
cat(sprintf("horizons     : %s mo (primary %d)   embargo: cases %dwk / deaths %dwk (ETH %dwk)\n",
            paste(SPEC$horizons_months, collapse = ","), SPEC$primary_horizon,
            SPEC$embargo_weeks_cases, SPEC$embargo_weeks_deaths, SPEC$embargo_weeks_deaths_eth))
cat(sprintf("psi          : n_seeds %d, parallel_seeds %d (~%d TF threads/seed)   RAM~%sGB\n",
            SPEC$psi_n_seeds, SPEC$psi_parallel_seeds,
            max(1L, CORES %/% max(1L, min(SPEC$psi_parallel_seeds, SPEC$psi_n_seeds))),
            ifelse(is.na(.ramgb), "?", round(.ramgb))))
cat(sprintf("ess_min      : %g   cores/process: %d   out: %s\n", SPEC$ess_min, CORES, SPEC$out_dir))
cat("======================================================================\n")
if (DRY_RUN) { cat("\nDRY RUN: SPEC validated, no compute. Set FORECAST_CV_DRYRUN=0 to execute.\n"); quit(save = "no", status = 0) }

# =============================================================================
# helpers
# =============================================================================
.read_preds <- function(dir) {
     pq <- file.path(dir, "predictions.parquet"); cs <- file.path(dir, "predictions.csv")
     if (file.exists(pq) && requireNamespace("arrow", quietly = TRUE)) return(as.data.frame(arrow::read_parquet(pq)))
     if (file.exists(cs)) return(utils::read.csv(cs, stringsAsFactors = FALSE))
     NULL
}
.write_tab <- function(df, path) {
     if (requireNamespace("arrow", quietly = TRUE)) arrow::write_parquet(df, path)
     else utils::write.csv(df, sub("\\.parquet$", ".csv", path), row.names = FALSE)
}
PATHS   <- MOSAIC::get_paths()
# Identical est_suitability_spec in prefit + (cache read) calibrate so the spec
# hash matches (parallel_seeds is excluded from the hash, so it may differ).
es_spec <- list(arch_control = list(n_seeds = SPEC$psi_n_seeds, parallel_seeds = SPEC$psi_parallel_seeds))
dir.create(SPEC$out_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# PHASE: PREFIT  --  freeze psi once per cutoff (serial, whole box)
# =============================================================================
if (PHASE %in% c("all", "prefit")) {
     cat("\n--- PREFIT: freezing psi per cutoff (serial) ---\n")
     # Honor a single-cell cutoff restriction (FORECAST_CV_CUTOFF): prefit only
     # the needed cutoff(s). Unrestricted => all SPEC cutoffs (the parallel path).
     MOSAIC::prefit_rolling_cv_psi(
          PATHS = PATHS, cutoffs = cell_cutoffs, est_suitability_spec = es_spec,
          pred_date_start = cfg_start, pred_date_stop = cfg_stop,
          dir_cache = psi_cache_dir, verbose = TRUE)
}

# =============================================================================
# PHASE: CALIBRATE  --  per (unit x cutoff), read frozen psi, calibrate, project
# =============================================================================
if (PHASE %in% c("all", "calibrate")) {
     ctrl <- SPEC$control
     if (is.null(ctrl)) {
          ctrl <- MOSAIC::mosaic_control_defaults()
          ctrl$calibration <- modifyList(ctrl$calibration %||% list(),
                                         list(n_simulations = as.integer(Sys.getenv("FORECAST_CV_N_SIM", "2000")),
                                              n_iterations   = as.integer(Sys.getenv("FORECAST_CV_N_ITER", "3"))))
          ctrl$targets     <- modifyList(ctrl$targets %||% list(), list(ESS_param = 100L))
     }
     ctrl$parallel <- modifyList(ctrl$parallel %||% list(), list(enable = CORES > 1L, n_cores = CORES))
     ctrl$paths    <- modifyList(ctrl$paths %||% list(), list(plots = FALSE))

     status <- list()
     for (u in cell_units) {
          uid <- paste(u, collapse = "+")
          # Index (not `for (T_k in cell_cutoffs)`): iterating a Date vector strips
          # the Date class to the underlying numeric, mangling format()/dir names.
          for (ci in seq_along(cell_cutoffs)) {
               T_k <- cell_cutoffs[ci]
               run_dir <- file.path(SPEC$out_dir, "per_unit", uid, sprintf("cutoff_%s", format(T_k)))
               key <- sprintf("%s @ %s", uid, format(T_k))
               if (file.exists(file.path(run_dir, "predictions.parquet")) ||
                   file.exists(file.path(run_dir, "predictions.csv"))) {
                    cat(sprintf("[skip] %s\n", key)); status[[key]] <- "skipped"; next }
               cat(sprintf("[run ] %s\n", key))
               status[[key]] <- tryCatch({
                    MOSAIC::run_rolling_cv(
                         PATHS = PATHS, iso = u, n_cutoffs = 1L, latest_cutoff = T_k, step_months = 1L,
                         horizons_months = SPEC$horizons_months,
                         embargo_weeks = SPEC$embargo_weeks_cases,   # scoring re-applies per-metric embargo
                         base_config = base_config, priors = MOSAIC::priors_default,
                         control = ctrl, models = SPEC$models, central_method = SPEC$central_method,
                         est_suitability_spec = es_spec, psi_cache = psi_cache_dir,
                         dir_output = run_dir, verbose = TRUE)
                    "success"
               }, error = function(e) { cat("   FAILED:", conditionMessage(e), "\n"); "failed" })
          }
     }
     cat(sprintf("calibrate: %d ok / %d failed / %d skipped\n",
                 sum(unlist(status) == "success"), sum(unlist(status) == "failed"), sum(unlist(status) == "skipped")))
}

# =============================================================================
# PHASE: SCORE  --  per-unit OOS scoring (climatology baseline), plots, manifest
# =============================================================================
if (PHASE %in% c("all", "score")) {
     cat("\n--- SCORE: per-unit OOS evaluation ---\n")
     unit_dirs <- list.dirs(file.path(SPEC$out_dir, "per_unit"), recursive = FALSE)
     all_preds <- list(); all_cells <- list(); all_summ <- list()
     for (ud in unit_dirs) {
          uid <- basename(ud)
          cdirs <- list.dirs(ud, recursive = FALSE); cdirs <- cdirs[grepl("cutoff_", basename(cdirs))]
          pu <- do.call(rbind, Filter(Negate(is.null), lapply(cdirs, .read_preds)))
          if (is.null(pu) || !nrow(pu)) next
          isos <- unique(pu$iso_code)
          dd   <- if ("ETH" %in% isos) SPEC$embargo_weeks_deaths_eth else SPEC$embargo_weeks_deaths
          expl <- "NGA" %in% isos
          ev_u <- MOSAIC::evaluate_rolling_cv(
               predictions = pu, horizons_months = SPEC$horizons_months, baselines = SPEC$baselines,
               metrics = SPEC$metrics, embargo_weeks = c(cases = SPEC$embargo_weeks_cases, deaths = dd),
               ess_min = SPEC$ess_min, min_cells_ci = SPEC$min_cells_ci)
          ev_u$cells$unit <- uid;   ev_u$cells$exploratory <- expl
          ev_u$summary$unit <- uid; ev_u$summary$exploratory <- expl
          all_preds[[uid]] <- pu; all_cells[[uid]] <- ev_u$cells; all_summ[[uid]] <- ev_u$summary
     }
     if (!length(all_preds)) stop("No predictions to score in ", SPEC$out_dir)
     preds <- do.call(rbind, all_preds)
     cells <- do.call(rbind, all_cells); summ <- do.call(rbind, all_summ)
     .write_tab(preds, file.path(SPEC$out_dir, "predictions_all.parquet"))
     .write_tab(cells, file.path(SPEC$out_dir, "scores_cells.parquet"))
     .write_tab(summ,  file.path(SPEC$out_dir, "leaderboard.parquet"))

     if (requireNamespace("ggplot2", quietly = TRUE)) {
          for (m in SPEC$metrics)
               tryCatch(MOSAIC::plot_rolling_cv(predictions = preds, metric = m, eval = list(cells = cells),
                                                dir_output = SPEC$out_dir, file_prefix = "rolling_cv"),
                        error = function(e) cat("   plot failed (", m, "): ", conditionMessage(e), "\n"))
     }

     # ESS gate readout (from the predictions `ess` column, per the scoring contract)
     ess_tab <- unique(cells[, c("unit", "iso_code", "cutoff_date", "metric", "ess", "ess_ok")])
     cat(sprintf("\nESS gate (floor %g): %d/%d cells pass\n",
                 SPEC$ess_min, sum(cells$ess_ok, na.rm = TRUE), nrow(cells)))
     print(utils::head(ess_tab[order(ess_tab$ess), ], 20), row.names = FALSE)

     manifest <- list(
          experiment = "forecast_cv (HINDCAST / conditional model skill, realized covariates -- NOT forecast skill)",
          created = as.character(Sys.time()), mosaic_version = as.character(utils::packageVersion("MOSAIC")),
          laser_version = tryCatch(as.character(reticulate::py_to_r(reticulate::import("laser.cholera")$`__version__`)),
                                   error = function(e) NA_character_),
          git_sha = tryCatch(system(paste("git -C", shQuote(file.path(root, "MOSAIC-pkg")), "rev-parse HEAD"), intern = TRUE),
                             error = function(e) NA_character_),
          spec = SPEC, config_window = c(as.character(cfg_start), as.character(cfg_stop)),
          primary = list(horizon_months = SPEC$primary_horizon, baseline = "seasonal",
                         verdict = "per-origin win/loss vs climatology + raw per-origin values (n=3 -> NO CI)",
                         exploratory_units = "NGA"),
          ic_provenance = list(note = "ICs frozen at build-time ic_t0; D5 places cutoffs downstream"))
     jsonlite::write_json(manifest, file.path(SPEC$out_dir, "spec.json"),
                          auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")

     cat("\n=========== DONE (HINDCAST / model skill) ===========\n")
     cat("artifacts:", SPEC$out_dir, "\n")
     cat("  predictions_all.parquet  scores_cells.parquet  leaderboard.parquet  spec.json\n")
     cat("  rolling_cv_{cases,deaths}_*.png/pdf\n")
     cat("REMINDER: realized covariates => conditional/hindcast MODEL skill, NOT forecast skill (D4).\n")
     cat("PRIMARY verdict = win/loss vs climatology at 6mo; n=3 per country => report raw per-origin values, NO CI. NGA exploratory-only.\n")
}
