# =============================================================================
# run_rolling_cv_suitability.R — lstm_v2 ("gauge_A") orchestrator for
# est_suitability(architecture = "lstm_v2_hierarchical_film"). The dispatcher in
# est_suitability.R routes here.
#
# Flow: load arch_control (fixture + production overrides + user overrides) ->
# resolve features + region map + response var + dates -> build the data bundle
# (.psi_build_data) -> seed ensemble (each seed runs its own expanding-window
# rolling-origin CV + full-IS refit, .psi_fit_predict_rw_cv over the gauge_A arch
# .psi_fit_predict_lstm) -> per-country logit-scale smoothing + per-country
# bias correction -> canonical `psi` column (Option A schema) -> writes the four
# output files:
#   pred_psi_suitability_day.csv, pred_psi_suitability_week.csv,
#   data_psi_suitability.csv, psi_suitability_config.json
#
# `%||%` is provided package-wide by R/aaa_utils.R.
# =============================================================================

# Curated full candidate feature set for feature_set = "default" (the lstm_v2
# toggle). Mirrors the legacy est_suitability covariates_all (excludes
# vaccination + raw timestamps by construction); _lag columns are appended from
# the CSV header at resolve time. (The legacy body is frozen, so its inline copy
# cannot drift this one.)
#' @keywords internal
#' @noRd
.PSI_DEFAULT_FEATURE_CANDIDATES <- c(
     "sin_annual", "cos_annual", "sin_biannual", "cos_biannual",
     "sin_quarterly", "cos_quarterly", "sin_monthly", "cos_monthly",
     "total_population", "population_density", "urban_population_pct",
     "GDP", "poverty_ratio",
     "Piped_Water", "Other_Improved_Water", "Septic_or_Sewer_Sanitation",
     "Other_Improved_Sanitation", "Unimproved_Water", "Surface_Water", "Open_Defecation",
     "temperature_2m_mean", "temperature_2m_max", "temperature_2m_min", "precipitation_sum",
     "relative_humidity_2m_mean", "wind_speed_10m_mean", "cloud_cover_mean",
     "et0_fao_evapotranspiration_sum", "soil_moisture_0_to_10cm_mean", "precip_sum_2w",
     "precip_sum_4w", "precip_sum_8w", "precip_sum_12w",
     "temp_mean_2w", "temp_mean_4w", "temp_mean_8w", "temp_mean_12w",
     "rh_mean_2w", "rh_mean_4w", "rh_mean_8w", "rh_mean_12w", "precip_anom",
     "temp_anom", "vpd_anom", "soil_moisture_anom",
     "precip_extreme_p90_count", "heatwave_days", "dry_spell_len",
     "temp_precip_interaction", "humidity_temp_interaction", "enso_precip_interaction",
     "iod_temp_interaction", "elevation_temp_interaction", "moisture_temp_interaction",
     "wash_precip_extreme_interaction", "urban_precip_anom_interaction", "precip_anom_sq",
     "temp_anom_sq", "vpd", "moisture_deficit", "aridity_index", "spei_approx",
     "gdd_cholera", "diurnal_temp_range", "heat_index", "dew_point_2m_max",
     "dew_point_2m_mean", "dew_point_2m_min", "pressure_msl_mean",
     "relative_humidity_2m_max", "relative_humidity_2m_min", "shortwave_radiation_sum",
     "wind_speed_10m_max", "IOD", "ENSO3", "ENSO34", "ENSO4", "elevation",
     "emdat_flood_prob", "emdat_flood_prob_4w_max", "emdat_flood_prob_12w_max",
     "emdat_flood_prob_12w_sum", "emdat_flood_prob_anom")

#' Resolve feature_set -> a candidate feature vector (the sole public selector
#' is get_feature_set; "v7.3" -> the 38-name set, "default" -> the curated full
#' set + CSV lag columns). Errors if a "v7.3" feature is absent (schema guard).
#' @keywords internal
#' @noRd
.psi_resolve_features <- function(feature_set, source_csv) {
     fs <- get_feature_set(feature_set)   # match.arg inside: errors on bad names
     cols <- names(utils::read.csv(source_csv, nrows = 1L, stringsAsFactors = FALSE))
     if (!is.null(fs)) {
          missing_fs <- setdiff(fs, cols)
          if (length(missing_fs) > 0)
               stop(sprintf("est_suitability: feature_set='%s' needs %d feature(s) absent from the suitability CSV: %s.\n  Recompile the suitability data, or use feature_set='default'.",
                            feature_set, length(missing_fs), paste(missing_fs, collapse = ", ")),
                    call. = FALSE)
          return(fs)
     }
     # "default": curated full candidate set + lag columns present in the CSV.
     lag_cols <- grep("_lag", cols, value = TRUE)
     unique(c(.PSI_DEFAULT_FEATURE_CANDIDATES, lag_cols))
}

# Per-PSOCK-worker RAM budget (GB) for a parallel suitability seed fit. Each
# worker is a separate process that imports TensorFlow/keras AND holds the full
# pooled data bundle; TF grabs memory per process and does not release it. ~6 GB
# is a deliberately conservative single-worker footprint (TF runtime + the
# all-MOSAIC pooled tensors + the K+1 fits' graphs); used only to decide whether
# parallel_seeds risks OOM, not to allocate anything.
.PSI_PER_SEED_WORKER_GB <- 6

#' Best-effort total system RAM in GB (NA if it cannot be probed portably).
#' macOS: sysctl hw.memsize; Linux: /proc/meminfo MemTotal. No hard dependency.
#' @keywords internal
#' @noRd
.psi_total_system_ram_gb <- function() {
     os <- Sys.info()[["sysname"]]
     bytes <- tryCatch({
          if (identical(os, "Darwin")) {
               as.numeric(system2("sysctl", c("-n", "hw.memsize"),
                                  stdout = TRUE, stderr = FALSE))
          } else if (identical(os, "Linux")) {
               mi <- readLines("/proc/meminfo", n = 1L)            # "MemTotal:  N kB"
               as.numeric(sub("\\D*(\\d+)\\s*kB.*", "\\1", mi)) * 1024
          } else NA_real_
     }, error = function(e) NA_real_, warning = function(e) NA_real_)
     if (length(bytes) != 1L || !is.finite(bytes) || bytes <= 0) return(NA_real_)
     bytes / 2^30
}

#' Warn (do NOT cap) when parallel_seeds risks OOM. The number of concurrent
#' PSOCK workers is min(parallel_seeds, n_seeds, cores - 2) (see
#' .psi_fit_seeds_parallel); each holds ~.PSI_PER_SEED_WORKER_GB of TF + bundle
#' RAM. If the projected footprint exceeds ~85% of probed system RAM we warn so
#' a user does not silently OOM a small box. We warn rather than cap because the
#' true per-worker footprint depends on the pool size and the box; the safe move
#' is to inform, not to second-guess a user who knows their RAM.
#' @keywords internal
#' @noRd
.psi_check_parallel_seeds_ram <- function(parallel_seeds, n_seeds) {
     ps <- as.integer(parallel_seeds %||% 1L)
     if (is.na(ps) || ps <= 1L) return(invisible(NULL))   # serial path: no risk
     # Match .psi_fit_seeds_parallel: honor a per-process core budget if set.
     nc <- suppressWarnings(as.integer(Sys.getenv("MOSAIC_PSI_CORE_BUDGET", "")))
     if (is.na(nc) || nc < 1L) nc <- parallel::detectCores()
     if (is.na(nc) || nc < 1L) nc <- 2L
     n_workers <- max(1L, min(ps, as.integer(n_seeds %||% ps), nc - 2L))
     if (n_workers <= 1L) return(invisible(NULL))         # clamps to serial anyway

     need_gb  <- n_workers * .PSI_PER_SEED_WORKER_GB
     total_gb <- .psi_total_system_ram_gb()
     if (is.na(total_gb)) {
          warning(sprintf(
               "parallel_seeds=%d will spawn ~%d concurrent TF/keras workers (~%.0f GB total at ~%g GB each). System RAM could not be probed on this platform; ensure the box has the headroom or set parallel_seeds=1 (serial).",
               ps, n_workers, need_gb, .PSI_PER_SEED_WORKER_GB), call. = FALSE)
          return(invisible(NULL))
     }
     if (need_gb > 0.85 * total_gb) {
          warning(sprintf(
               "parallel_seeds=%d projects ~%d concurrent TF/keras workers needing ~%.0f GB (~%g GB each) but the system has ~%.0f GB RAM -- this risks OOM. Lower parallel_seeds (each worker is one whole seed pipeline) or run serial (parallel_seeds=1). NOTE: the production runs used parallel_seeds=10, which only fit on a 448 GB host.",
               ps, n_workers, need_gb, .PSI_PER_SEED_WORKER_GB, total_gb),
               call. = FALSE)
     }
     invisible(NULL)
}

#' Load + resolve the lstm_v2 arch_control: read the B4 fixture, apply the two
#' production overrides (n_seeds=5, region_map="snf_k5") + the parallel_seeds
#' execution default (1L), then the user's arch_control via modifyList, then
#' coerce known-integer fields (a user override could inject a double).
#'
#' \strong{parallel_seeds / OOM:} \code{parallel_seeds} (default \code{1L} =
#' serial) fits seeds across PSOCK workers, each a separate process that imports
#' TensorFlow/keras and holds the full pooled data bundle (~6 GB/worker; RAM, not
#' cores, is the binding constraint). Concurrency is
#' \code{min(parallel_seeds, n_seeds, cores - 2)}. Safe range: keep
#' \code{parallel_seeds * ~6 GB} comfortably under system RAM (e.g. <= 4 on a
#' 32 GB box). The production runs used \code{parallel_seeds = 10}, which only
#' fit on the 448 GB compute host; do not copy that value onto a smaller machine.
#' A RAM-aware \code{warning()} fires here when the projected footprint exceeds
#' ~85\% of probed system memory.
#' @keywords internal
#' @noRd
.psi_load_arch_control <- function(arch_control = NULL) {
     f <- system.file("fixtures", "B4_rolling_cv_spec.yml", package = "MOSAIC")
     if (!nzchar(f) || !file.exists(f))
          stop(".psi_load_arch_control: B4 fixture not found (expected inst/fixtures/B4_rolling_cv_spec.yml)",
               call. = FALSE)
     fixture <- yaml::read_yaml(f)
     # Production defaults = fixture + the two sanctioned divergences + the
     # parallel_seeds execution knob (NOT in the fixture; does not change results).
     prod <- utils::modifyList(fixture, list(n_seeds = 5L, region_map = "snf_k5",
                                             parallel_seeds = 1L))
     ac <- if (is.null(arch_control)) prod else utils::modifyList(prod, arch_control)
     int_fields <- c("units_1", "units_2", "units_3", "batch_size", "epochs",
                     "patience", "country_dim", "timesteps", "n_seeds",
                     "seed_base", "seed_step", "max_gap_days", "rlr_patience",
                     "rw_step_months", "rw_test_months", "rw_subsample",
                     "rw_gap_weeks", "parallel_seeds")
     for (k in int_fields) if (!is.null(ac[[k]]) && !is.na(ac[[k]]))
          ac[[k]] <- as.integer(round(as.numeric(ac[[k]])))
     # Warn (once, before any worker spawns) if parallel_seeds risks OOM on this
     # box. Does not change ac; serial (default) is a no-op.
     .psi_check_parallel_seeds_ram(ac$parallel_seeds, ac$n_seeds)
     ac
}

#' lstm_v2 orchestrator (the est_suitability default path).
#' @keywords internal
#' @noRd
.est_suitability_lstm_v2 <- function(PATHS,
                                     fit_date_start = NULL,
                                     fit_date_stop = NULL,
                                     pred_date_start = NULL,
                                     pred_date_stop = NULL,
                                     feature_set = "v7.3",
                                     response_var = "transmission_intensity",
                                     bias_correct = TRUE,
                                     arch_control = NULL,
                                     plot_country_diagnostics = FALSE,
                                     verbose = TRUE) {

     if (!requireNamespace("keras3", quietly = TRUE) ||
         !requireNamespace("reticulate", quietly = TRUE))
          stop("est_suitability(architecture='lstm_v2_hierarchical_film') requires the 'keras3' + 'reticulate' Python stack. Run MOSAIC::install_dependencies() and MOSAIC::check_dependencies().",
               call. = FALSE)

     if (length(PATHS$DATA_CHOLERA_WEEKLY) != 1L || !nzchar(PATHS$DATA_CHOLERA_WEEKLY) ||
         length(PATHS$MODEL_INPUT) != 1L || !nzchar(PATHS$MODEL_INPUT))
          stop("est_suitability: PATHS$DATA_CHOLERA_WEEKLY and PATHS$MODEL_INPUT must be non-empty paths (use get_paths()).",
               call. = FALSE)
     source_csv <- file.path(PATHS$DATA_CHOLERA_WEEKLY,
                             "cholera_country_weekly_suitability_data.csv")
     if (!file.exists(source_csv))
          stop("est_suitability: suitability CSV not found at ", source_csv, call. = FALSE)

     ac <- .psi_load_arch_control(arch_control)
     features        <- .psi_resolve_features(feature_set, source_csv)
     region_map_vec  <- .psi_resolve_region_map(ac$region_map)
     # arch_control can carry an lstm_v2 research ablation override.
     exclude <- ac$exclude_covariates
     if (!is.null(exclude) && length(exclude) > 0) {
          n_dropped <- length(intersect(exclude, features))
          features <- setdiff(features, exclude)
          if (verbose) message(sprintf("arch_control$exclude_covariates: dropped %d feature(s)", n_dropped))
     }

     # ---- Date resolution --------------------------------------------------
     fit_date_start <- if (is.null(fit_date_start)) as.Date(ac$fit_date_start %||% "2015-01-01")
                       else as.Date(fit_date_start)
     if (is.null(fit_date_stop) || is.null(pred_date_stop)) {
          dd <- utils::read.csv(source_csv, stringsAsFactors = FALSE)
          dd$date <- as.Date(dd$date)
          dd <- dd[dd$iso_code %in% MOSAIC::iso_codes_mosaic, ]
          dd$cases[is.na(dd$cases)] <- 0
          enso_cols <- c("IOD", "ENSO3", "ENSO34", "ENSO4")
          enso_ok <- stats::complete.cases(dd[, enso_cols])
          if (is.null(fit_date_stop)) {
               fit_date_stop <- max(dd$date[enso_ok], na.rm = TRUE)
               if (verbose) message(glue::glue("Auto-detected fit_date_stop (cutoff): {fit_date_stop}"))
          } else fit_date_stop <- as.Date(fit_date_stop)
          if (is.null(pred_date_stop)) {
               pred_date_stop <- max(dd$date[enso_ok], na.rm = TRUE)
               if (verbose) message(glue::glue("Auto-detected pred_date_stop: {pred_date_stop}"))
          } else pred_date_stop <- as.Date(pred_date_stop)
          rm(dd)
     } else {
          fit_date_stop  <- as.Date(fit_date_stop)
          pred_date_stop <- as.Date(pred_date_stop)
     }
     cutoff_date     <- fit_date_stop
     pred_date_start <- if (is.null(pred_date_start)) fit_date_start else as.Date(pred_date_start)
     if (fit_date_start >= cutoff_date) stop("fit_date_start must be before fit_date_stop")
     if (pred_date_start >= pred_date_stop) stop("pred_date_start must be before pred_date_stop")
     if (verbose) {
          message(glue::glue("lstm_v2: fit_date_start={fit_date_start}  cutoff(fit_date_stop)={cutoff_date}"))
          message(glue::glue("lstm_v2: prediction window {pred_date_start} -> {pred_date_stop}"))
          message(glue::glue("lstm_v2: feature_set='{feature_set}' ({length(features)} candidates), region_map='{ac$region_map}', n_seeds={ac$n_seeds}"))
     }

     # ---- Build the data bundle -------------------------------------------
     bundle <- .psi_build_data(
          source_csv     = source_csv,
          cutoff_date    = cutoff_date,
          fit_date_start = fit_date_start,
          pred_date_stop = pred_date_stop,
          region_map     = region_map_vec,
          country_pool   = ac$country_pool %||% "all_mosaic",
          target_iso     = ac$target_iso %||% "MOZ",
          timesteps      = ac$timesteps,
          features       = features,
          split_params   = list(rw_step_months = ac$rw_step_months,
                                 rw_test_months = ac$rw_test_months,
                                 rw_subsample   = ac$rw_subsample,
                                 rw_gap_weeks   = ac$rw_gap_weeks),
          use_confidence_weight = isTRUE(ac$use_confidence_weight),
          response_var   = response_var,
          max_gap_days   = ac$max_gap_days,
          verbose        = verbose)

     # ---- Seed ensemble (each seed = whole RW-CV + refit + predict) --------
     seeds <- seq.int(ac$seed_base, by = ac$seed_step, length.out = ac$n_seeds)
     fit_fn <- function(data_bundle, seed, hyperparams)
          .psi_fit_predict_rw_cv(data_bundle = data_bundle,
                                 fit_predict_fn = .psi_fit_predict_lstm,
                                 seed = seed, hyperparams = hyperparams,
                                 verbose = verbose)
     arch_hp <- list(
          arch_kind = "hierarchical", hier_mode = "film",
          units_1 = ac$units_1, units_2 = ac$units_2, units_3 = ac$units_3,
          dropout = ac$dropout, rec_dropout = ac$rec_dropout, l2 = ac$l2,
          lr = ac$lr, batch_size = ac$batch_size, epochs = ac$epochs,
          patience = ac$patience, rlr_factor = ac$rlr_factor,
          rlr_patience = ac$rlr_patience, min_lr = ac$min_lr,
          restore_best_weights = ac$restore_best_weights %||% TRUE,
          country_dim = ac$country_dim, partial_pool_lambda = ac$partial_pool_lambda,
          region_l2 = ac$region_l2, sample_weights = ac$sample_weights,
          balance_R = ac$balance_R, loss_kind = ac$loss_kind,
          country_balance = isTRUE(ac$country_balance),
          # Loss internals — passive under bce + balanced_uniform (the B4/production
          # config), but threaded so an arch_control research override (mse_logit /
          # linear / quadratic) is honored rather than silently ignored. Values
          # default to the fixture pins, so production output is unchanged.
          logit_eps = ac$loss_logit_eps, logit_clip = ac$logit_clip,
          sw_offset = ac$sw_offset, sw_min = ac$sw_min,
          sw_offset_quad = ac$sw_offset_quad, sw_min_quad = ac$sw_min_quad)
     # arch_kind / hier_mode / gamma_activation are FIXED by the gauge_A-only port
     # (hierarchical + film + tanh); the fixture carries them for provenance, but
     # the exp/concat/flat/hypernet branches were dropped, so they are not live
     # arch_control overrides.

     ens <- .psi_run_seed_ensemble(
          fit_predict_fn = fit_fn, data_bundle = bundle, seeds = seeds,
          hyperparams = arch_hp, smooth_span = ac$smooth_span,
          logit_eps = ac$ensemble_logit_eps,
          loess_surface = ac$loess_surface %||% "direct",
          loess_degree  = as.integer(ac$loess_degree %||% 2L),
          parallel_seeds = ac$parallel_seeds, verbose = verbose)

     # ---- Bias correction -> canonical psi ---------------------------------
     el <- ens$ensemble_long
     el$iso_code <- el$iso
     if (isTRUE(bias_correct)) {
          obs_df <- el[, c("iso_code", "date", "intensity")]
          el <- calibrate_psi_predictions(
               el, obs_df, fit_date_stop = cutoff_date,
               pred_col = "pred_smooth", obs_col = "intensity",
               out_col = "pred_bias_corrected")
          el$psi <- el$pred_bias_corrected
          if (verbose) message("Applied per-country logit-scale bias correction -> psi.")
     } else {
          el$pred_bias_corrected <- el$pred_smooth
          el$psi <- el$pred_smooth
     }

     # ---- Assemble + trim daily output (Option A schema) -------------------
     out_daily <- el[, c("iso_code", "date", "cases", "psi", "pred_raw",
                         "pred_smooth", "pred_bias_corrected",
                         "q025", "q25", "q75", "q975")]
     out_daily$year        <- as.integer(format(out_daily$date, "%Y"))
     out_daily$week        <- as.integer(format(out_daily$date, "%V"))
     out_daily$cases_binary <- as.integer(out_daily$cases > 0)
     out_daily$pred        <- out_daily$pred_raw   # backward-compat alias for cosmetic plotters
     out_daily <- out_daily[out_daily$date >= pred_date_start &
                            out_daily$date <= pred_date_stop, ]
     out_daily <- out_daily[order(out_daily$iso_code, out_daily$date), ]

     # Weekly = daily sampled at the observed weekly dates (diagnostic only).
     wk_keys <- unique(bundle$obs_all[, c("iso_code", "date")])
     out_weekly <- merge(out_daily, wk_keys, by = c("iso_code", "date"))
     out_weekly <- out_weekly[order(out_weekly$iso_code, out_weekly$date), ]

     # ---- Write the four output files --------------------------------------
     p_week <- file.path(PATHS$MODEL_INPUT, "pred_psi_suitability_week.csv")
     utils::write.csv(out_weekly, p_week, row.names = FALSE)
     if (verbose) message("Predictions saved to: ", p_week)

     p_day <- file.path(PATHS$MODEL_INPUT, "pred_psi_suitability_day.csv")
     utils::write.csv(out_daily, p_day, row.names = FALSE)
     if (verbose) message("Predictions saved to: ", p_day)

     obs_out <- bundle$obs_all
     obs_out$cases_binary <- as.integer(obs_out$cases > 0)   # consumed by LAUNCH.R iso_with_data
     obs_out$data_type <- ifelse(obs_out$date <= cutoff_date, "training", "prediction")
     p_data <- file.path(PATHS$MODEL_INPUT, "data_psi_suitability.csv")
     utils::write.csv(obs_out, p_data, row.names = FALSE)
     if (verbose) message("Observed data and metadata saved to: ", p_data)

     config_info <- list(
          architecture     = "lstm_v2_hierarchical_film",
          fit_date_start   = as.character(fit_date_start),
          fit_date_stop    = as.character(cutoff_date),
          pred_date_start  = as.character(pred_date_start),
          pred_date_stop   = as.character(pred_date_stop),
          feature_set      = feature_set,
          response_var     = response_var,
          bias_correct     = isTRUE(bias_correct),
          region_map       = ac$region_map,
          n_seeds          = ac$n_seeds,
          seeds            = seeds,
          parallel_seeds   = ac$parallel_seeds,
          n_countries      = bundle$encoders$n_countries,
          n_regions        = bundle$encoders$n_regions,
          n_features       = length(bundle$features),
          features         = bundle$features,
          fit_info         = ens$fit_info,
          rw_diagnostics   = ens$rw_diagnostics)
     p_cfg <- file.path(PATHS$MODEL_INPUT, "psi_suitability_config.json")
     jsonlite::write_json(config_info, p_cfg, pretty = TRUE, auto_unbox = TRUE,
                          digits = NA, null = "null")
     if (verbose) message("Model configuration saved to: ", p_cfg)

     invisible(list(daily = out_daily, weekly = out_weekly,
                    fit_info = ens$fit_info, config = config_info))
}
