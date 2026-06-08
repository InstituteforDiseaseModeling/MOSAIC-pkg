# =============================================================================
# build_suitability_sequences.R — Data loading, target construction, train-only
# feature scaling, and country-blocked LSTM sequence building for the lstm_v2
# ("gauge_A") path of est_suitability(). Ported from the MOSAIC-Mozambique LSTM
# sandbox (io.R), gauge_A-only:
#
#   * ALL transforms use TRAINING-PERIOD statistics only (cases_99th, scaler
#     mean/sd) — the key leakage fix vs the legacy est_suitability().
#   * The duplicated MINFEAT_* feature-set constants are NOT ported; feature
#     selection reuses R/feature_sets.R::get_feature_set() (the single public
#     selector). Features are resolved by the orchestrator and passed in.
#   * The hypernet-only static-covariate matrix and the single-split
#     (make_split) branch are dropped — the lstm_v2 path is rolling-origin CV
#     only (split_type = "rolling_cv").
#   * No source()/here::here(): cross-file helpers resolve via the package
#     namespace (.psi_make_rw_cv_steps lives in rolling_cv_suitability.R).
#
# Public-to-the-package entry point: .psi_build_data()
# `%||%` is provided package-wide by R/aaa_utils.R.
# =============================================================================

# ---- Sequence builder (country-blocked, gap-aware) --------------------------
# Optionally tracks per-country / per-region integer IDs alongside X/y/dates,
# and a per-sequence confidence weight anchored to the last (target) timestep.
#' @keywords internal
#' @noRd
.psi_build_sequences <- function(X, y, countries, dates, timesteps = 13L,
                                 max_gap_days = 14L,
                                 country_id_lookup = NULL,
                                 region_for_country = NULL,
                                 cw = NULL) {
     dates <- as.Date(dates)
     uniq  <- unique(countries)
     seqs <- list(); ys <- numeric(0); cs <- character(0)
     ds <- as.Date(character(0))
     c_ids <- integer(0); r_ids <- integer(0)
     cw_track <- if (!is.null(cw)) numeric(0) else NULL
     for (iso in uniq) {
          m <- countries == iso
          Xi <- X[m, , drop = FALSE]; yi <- y[m]; di <- dates[m]
          cwi <- if (!is.null(cw)) cw[m] else NULL
          ord <- order(di)
          Xi <- Xi[ord, , drop = FALSE]; yi <- yi[ord]; di <- di[ord]
          if (!is.null(cwi)) cwi <- cwi[ord]
          n <- nrow(Xi)
          if (n < timesteps) next
          cid <- if (!is.null(country_id_lookup)) as.integer(country_id_lookup[[iso]]) else NA_integer_
          rid <- if (!is.null(region_for_country)) as.integer(region_for_country[[iso]]) else NA_integer_
          for (i in timesteps:n) {
               sub_d <- di[(i - timesteps + 1):i]
               if (any(as.numeric(diff(sub_d)) > max_gap_days)) next
               seqs[[length(seqs) + 1]] <- Xi[(i - timesteps + 1):i, , drop = FALSE]
               ys <- c(ys, yi[i]); cs <- c(cs, iso); ds <- c(ds, di[i])
               c_ids <- c(c_ids, cid); r_ids <- c(r_ids, rid)
               # Per-sequence confidence weight = CW at the last (target)
               # timestep, mirroring how y is anchored at the sequence end.
               if (!is.null(cwi)) cw_track <- c(cw_track, cwi[i])
          }
     }
     if (length(seqs) == 0) stop(".psi_build_sequences: no valid sequences built")
     X3 <- array(0, dim = c(length(seqs), timesteps, ncol(X)))
     for (i in seq_along(seqs)) X3[i, , ] <- seqs[[i]]
     list(X = X3, y = ys, countries = cs, dates = ds,
          country_ids = c_ids, region_ids = r_ids,
          cw = cw_track)
}

# ---- Derive eco-zone categorical features from existing CSV columns ---------
# Adds elevation_bin, urbanization_tier, wash_zone (derived from elevation,
# urban_population_pct, Piped_Water - Open_Defecation). Harmless for v7.3 (which
# does not use them); retained so alternate feature sets remain available via
# arch_control. Operates only on columns already present in d.
#' @keywords internal
#' @noRd
.psi_derive_eco_zone_features <- function(d) {
     if ("elevation" %in% names(d)) {
          d$elevation_bin <- as.integer(cut(d$elevation,
                                             breaks = c(-Inf, 100, 500, 1500, Inf),
                                             labels = FALSE, right = FALSE) - 1L)
     } else {
          d$elevation_bin <- NA_integer_
     }
     if ("urban_population_pct" %in% names(d)) {
          u <- d$urban_population_pct
          # Source data is percent (0-100); guard against proportion-form.
          if (max(u, na.rm = TRUE) <= 1.5) u <- u * 100
          d$urbanization_tier <- as.integer(cut(u,
                                                 breaks = c(-Inf, 25, 50, Inf),
                                                 labels = FALSE, right = FALSE) - 1L)
     } else {
          d$urbanization_tier <- NA_integer_
     }
     if (all(c("Piped_Water", "Open_Defecation") %in% names(d))) {
          wash_score <- d$Piped_Water - d$Open_Defecation
          d$wash_zone <- as.integer(cut(wash_score,
                                        breaks = c(-Inf, -20, 30, Inf),
                                        labels = FALSE, right = FALSE) - 1L)
     } else {
          d$wash_zone <- NA_integer_
     }
     d
}

# ---- Public entry point -----------------------------------------------------
#' Build the lstm_v2 data bundle for one cutoff over a country pool.
#'
#' Returns everything the gauge_A architecture + rolling-CV wrapper need: the
#' pre-sequence pool data (sliced per RW step), the full prediction sequences
#' (all pool countries, full date range), the categorical encoders, the RW step
#' grid, and aligned observation series. All scaling/anchoring uses TRAINING-
#' PERIOD statistics only.
#'
#' @param source_csv Path to the merged weekly suitability data CSV.
#' @param cutoff_date Date (or string); IS data is everything < cutoff_date.
#' @param fit_date_start Start of the data window (default "2015-01-01").
#' @param pred_date_stop End of the prediction window (default cutoff + ~5 mo).
#' @param region_map Named character vector iso -> region, or NULL to use the
#'   CSV's `region` column. Resolved by the orchestrator from arch_control.
#' @param country_pool "all_mosaic" (default), "regional", or "moz_only".
#' @param target_iso Headline country for backward-compat aliases (default "MOZ").
#' @param timesteps Sequence length in weeks (default 13).
#' @param features Character vector of feature column names (resolved upstream
#'   from feature_set via get_feature_set()).
#' @param split_params List of RW grid params (rw_step_months, rw_test_months,
#'   rw_subsample, rw_gap_weeks).
#' @param use_confidence_weight Logical; propagate the CSV `confidence_weight`
#'   column as a per-sequence loss overlay (default TRUE).
#' @param response_var Training target column. "transmission_intensity" (public
#'   default) and "intensity" both select the train-only
#'   log1p(cases)/log1p(cases_99th) recipe; a `target_*` name reads that
#'   pre-computed [0,1] column directly.
#' @param max_gap_days Max within-sequence date gap (default 14).
#' @param verbose Logical.
#' @return A data bundle list consumed by .psi_fit_predict_rw_cv() /
#'   .psi_run_seed_ensemble().
#' @keywords internal
#' @noRd
.psi_build_data <- function(source_csv,
                            cutoff_date,
                            fit_date_start = "2015-01-01",
                            pred_date_stop = NULL,
                            region_map     = NULL,
                            country_pool   = "all_mosaic",
                            target_iso     = "MOZ",
                            timesteps      = 13L,
                            features       = NULL,
                            split_params   = list(),
                            use_confidence_weight = TRUE,
                            response_var   = "transmission_intensity",
                            max_gap_days   = 14L,
                            verbose        = TRUE) {

     # The lstm_v2 path is rolling-origin CV only (the single-split make_split
     # branch is not ported). Reject other split types loudly.
     split_type <- split_params$split_type %||% "rolling_cv"
     if (!identical(split_type, "rolling_cv"))
          stop(sprintf(".psi_build_data: split_type='%s' is not supported; the lstm_v2 path is rolling_cv only.",
                       split_type), call. = FALSE)

     if (is.null(features) || length(features) < 5L)
          stop(".psi_build_data: `features` must be a character vector of >= 5 names (resolve via get_feature_set()).",
               call. = FALSE)

     # Public name maps to the sandbox "intensity" recipe (train-only cases_99th).
     if (identical(response_var, "transmission_intensity")) response_var <- "intensity"

     cutoff_date    <- as.Date(cutoff_date)
     fit_date_start <- as.Date(fit_date_start)
     if (is.null(pred_date_stop)) {
          pred_date_stop <- seq.Date(cutoff_date, by = "month", length.out = 6L)[6L]
     } else {
          pred_date_stop <- as.Date(pred_date_stop)
     }

     iso_pools <- list(
          moz_only = c("MOZ"),
          regional = c("MOZ", "MWI", "TZA", "ZMB", "ZWE")
          # "all_mosaic" is resolved after loading the CSV (MOSAIC::iso_codes_mosaic)
     )
     if (!country_pool %in% c(names(iso_pools), "all_mosaic"))
          stop("Unknown country_pool: ", country_pool,
               " (expected one of: ",
               paste(c(names(iso_pools), "all_mosaic"), collapse = ", "), ")")

     if (verbose) message(sprintf("Loading source CSV: %s", source_csv))
     d <- utils::read.csv(source_csv, stringsAsFactors = FALSE)
     d$date <- as.Date(d$date)

     if (country_pool == "all_mosaic") {
          pool <- intersect(unique(d$iso_code), MOSAIC::iso_codes_mosaic)
          if (verbose) message(sprintf("  all_mosaic pool resolved to %d countries", length(pool)))
     } else {
          pool <- iso_pools[[country_pool]]
     }
     d <- d[d$iso_code %in% pool, ]
     d <- d[d$date >= fit_date_start & d$date <= pred_date_stop, ]

     # Sanitize cases (mirrors production).
     d$cases[is.na(d$cases)] <- 0
     d$cases[d$cases < 0]    <- 0

     # ---- Confidence weight (AI-augmented CSV column) ----------------------
     if ("confidence_weight" %in% names(d)) {
          d$cw <- as.numeric(d$confidence_weight)
          d$cw[is.na(d$cw)] <- 1.0
          if (verbose) {
               n_lt_one <- sum(d$cw < 1.0)
               message(sprintf("  confidence_weight: %d rows < 1.0 (%s)",
                               n_lt_one,
                               if (use_confidence_weight) "WILL be applied"
                               else "NOT applied (use_confidence_weight=FALSE)"))
          }
     } else {
          d$cw <- rep(1.0, nrow(d))
          if (verbose)
               message("  confidence_weight column not in CSV; defaulting to 1.0 (no weighting)")
     }

     # ---- Target: response variable selection (TRAIN-ONLY anchor) ----------
     if (identical(response_var, "intensity")) {
          d_train_period <- d[d$date <= cutoff_date, ]
          cases_99th <- stats::quantile(d_train_period$cases, 0.99, na.rm = TRUE,
                                        names = FALSE)
          if (is.na(cases_99th) || cases_99th < 1) {
               if (verbose) message(sprintf("  cases_99th was %s, flooring at 1.0",
                                             format(cases_99th %||% NA_real_)))
               cases_99th <- 1
          }
          if (verbose) message(sprintf("  cases_99th (train-only): %.1f", cases_99th))
          d$intensity <- pmin(1.0, log1p(d$cases) / log1p(cases_99th))
     } else {
          valid_targets <- c("target_A_count_global",
                              "target_B_count_per_country",
                              "target_C_rate_global",
                              "target_D_rate_per_country_floored",
                              "target_F_rank_per_country")
          if (!response_var %in% valid_targets) {
               stop(sprintf("response_var '%s' not recognized. Use 'transmission_intensity'/'intensity' or one of: %s",
                            response_var, paste(valid_targets, collapse = ", ")))
          }
          if (!response_var %in% colnames(d)) {
               stop(sprintf("response_var '%s' not present in suitability CSV columns",
                            response_var))
          }
          if (verbose) message(sprintf("  Using response_var='%s' (pre-computed [0,1] target)",
                                       response_var))
          raw <- as.numeric(d[[response_var]])
          if (any(raw < 0 | raw > 1, na.rm = TRUE)) {
               warning(sprintf("  response_var '%s' had values outside [0,1]; clamping",
                               response_var))
               raw <- pmax(0, pmin(1, raw))
          }
          d$intensity <- raw
          cases_99th  <- NA_real_
     }

     # ---- Preserve observed target series BEFORE feature filtering ----------
     obs_all <- d[, c("date", "iso_code", "cases", "intensity")]
     obs_all <- obs_all[order(obs_all$iso_code, obs_all$date), ]
     obs_target_full <- obs_all[obs_all$iso_code == target_iso,
                                c("date", "cases", "intensity")]

     # ---- Eco-zone derived features (harmless if unused by feature set) -----
     d <- .psi_derive_eco_zone_features(d)

     # ---- Feature selection ------------------------------------------------
     feats <- intersect(features, colnames(d))
     missing <- setdiff(features, feats)
     if (length(missing) > 0 && verbose)
          message(sprintf("  Missing features (will skip): %s",
                          paste(missing, collapse = ", ")))
     if (length(feats) < 5)
          stop("Too few features available: ", length(feats))

     # Keep only rows with complete feature data (affects model tensors only).
     ok <- stats::complete.cases(d[, feats, drop = FALSE])
     if (verbose) {
          dropped <- sum(!ok)
          if (dropped > 0)
               message(sprintf("  Dropped %d rows due to incomplete features", dropped))
     }
     d <- d[ok, ]

     # ---- Train / pred partition (date-based, before scaling) --------------
     is_train_row <- d$date <= cutoff_date & !is.na(d$intensity)

     # ---- Feature scaling: fit on TRAINING rows ONLY -----------------------
     Xtr_raw <- as.matrix(d[is_train_row, feats, drop = FALSE])
     if (nrow(Xtr_raw) < 1L)
          stop(".psi_build_data: no training rows (date <= cutoff with complete features and a non-NA target). Check the cutoff/fit_date_start window and feature availability.",
               call. = FALSE)
     scaler_center <- colMeans(Xtr_raw)
     scaler_scale  <- apply(Xtr_raw, 2, stats::sd)
     # complete.cases() above keys on `feats`, so a finite center/scale is
     # expected; guard the degenerate all-NA-column case so we fail loudly here
     # rather than silently propagate NaN into every prediction.
     if (anyNA(scaler_center) || anyNA(scaler_scale))
          stop(".psi_build_data: feature scaler has NA center/scale (a feature column is all-NA over the training rows). Drop the offending feature or fix the suitability CSV.",
               call. = FALSE)
     scaler_scale[scaler_scale == 0] <- 1
     X_all <- scale(as.matrix(d[, feats, drop = FALSE]),
                    center = scaler_center, scale = scaler_scale)

     # ---- Categorical encoders for the hierarchical arch -------------------
     # 0-indexed integer IDs (keras embedding expects 0..N-1). If region_map is
     # supplied it overrides the CSV's `region` column. Countries not in
     # region_map fall back to "_unmapped_" (the production maps are exhaustive
     # over the MOSAIC-40, so this never fires there).
     if (!is.null(region_map)) {
          iso_uniq <- d$iso_code[!duplicated(d$iso_code)]
          mapped   <- ifelse(iso_uniq %in% names(region_map),
                             region_map[iso_uniq], "_unmapped_")
          iso_to_region <- stats::setNames(mapped, iso_uniq)
          if (verbose) {
               message(sprintf("  region_map: %d countries mapped to %d zones%s",
                               sum(iso_uniq %in% names(region_map)),
                               length(unique(mapped)),
                               if (any(mapped == "_unmapped_"))
                                    sprintf(" (+%d unmapped)", sum(mapped == "_unmapped_")) else ""))
          }
     } else {
          iso_to_region <- stats::setNames(d$region[!duplicated(d$iso_code)],
                                           d$iso_code[!duplicated(d$iso_code)])
     }
     iso_list      <- sort(unique(d$iso_code))
     region_list   <- sort(unique(iso_to_region[iso_list]))
     country_to_id <- stats::setNames(seq_along(iso_list) - 1L, iso_list)
     region_to_id  <- stats::setNames(seq_along(region_list) - 1L, region_list)
     region_for_country <- stats::setNames(region_to_id[iso_to_region[iso_list]], iso_list)
     country_to_region_vec <- as.integer(region_for_country)

     # region FiLM degrades to identity when only one region in the pool — warn
     # (not a hard error: single-region / single-country runs are legitimate).
     if (length(region_list) <= 1L) {
          warning(sprintf(".psi_build_data: only %d region in the pool; region FiLM will degrade to identity (z_r = z).",
                          length(region_list)), call. = FALSE)
     }

     encoders <- list(
          n_countries           = length(iso_list),
          n_regions             = length(region_list),
          iso_list              = iso_list,
          region_list           = region_list,
          country_to_id         = country_to_id,
          region_to_id          = region_to_id,
          iso_to_region         = iso_to_region,
          region_for_country    = region_for_country,
          country_to_region_vec = country_to_region_vec
     )

     # ---- Pool data sliced per RW step / final refit -----------------------
     pool_data <- list(
          X           = X_all,
          intensity   = d$intensity,
          dates       = d$date,
          countries   = d$iso_code,
          country_ids = as.integer(country_to_id[d$iso_code]),
          region_ids  = as.integer(region_for_country[d$iso_code]),
          cw          = d$cw
     )
     seq_params <- list(timesteps = timesteps, max_gap_days = max_gap_days)

     # Prediction sequences: ALL pool countries, full date range.
     seqs_pred <- .psi_build_sequences(
          X         = X_all,
          y         = rep(0, nrow(X_all)),
          countries = d$iso_code,
          dates     = d$date,
          timesteps = timesteps,
          max_gap_days = max_gap_days,
          country_id_lookup  = country_to_id,
          region_for_country = region_for_country)

     # ---- Rolling-CV step grid ---------------------------------------------
     gap_weeks <- split_params$rw_gap_weeks %||% 4L
     rw_steps  <- .psi_make_rw_cv_steps(
          fit_date_start = fit_date_start,
          cutoff_date    = cutoff_date,
          step_months    = split_params$rw_step_months %||% 1L,
          test_months    = split_params$rw_test_months %||% 5L,
          gap_weeks      = gap_weeks,
          subsample      = split_params$rw_subsample   %||% 1L,
          timesteps      = timesteps)
     if (length(rw_steps) == 0L)
          stop(".psi_build_data: 0 RW steps generated. Check step/test/gap params vs the cutoff/fit_date_start window.")
     if (verbose) {
          message(sprintf("  [rolling_cv] %d RW steps (gap=%dw, step=%dmo, test=%dmo, subsample=%d)",
                          length(rw_steps), gap_weeks,
                          split_params$rw_step_months %||% 1L,
                          split_params$rw_test_months %||% 5L,
                          split_params$rw_subsample   %||% 1L))
          message(sprintf("    first: train_end=%s test=[%s, %s]",
                          rw_steps[[1L]]$train_end, rw_steps[[1L]]$test_start, rw_steps[[1L]]$test_end))
          message(sprintf("    last : train_end=%s test=[%s, %s]",
                          rw_steps[[length(rw_steps)]]$train_end,
                          rw_steps[[length(rw_steps)]]$test_start,
                          rw_steps[[length(rw_steps)]]$test_end))
     }

     # Placeholder full-IS train sequences (RW wrapper overrides per step; this
     # is a sensible fallback for any code reading these slots).
     is_train_row_full <- d$date < cutoff_date & !is.na(d$intensity)
     seqs_full <- .psi_build_sequences(
          X         = X_all[is_train_row_full, , drop = FALSE],
          y         = d$intensity[is_train_row_full],
          countries = d$iso_code[is_train_row_full],
          dates     = d$date[is_train_row_full],
          timesteps = timesteps,
          max_gap_days = max_gap_days,
          country_id_lookup  = country_to_id,
          region_for_country = region_for_country,
          cw        = if (use_confidence_weight) d$cw[is_train_row_full] else NULL)

     list(
          X_train          = seqs_full$X, y_train = seqs_full$y,
          country_ids_train = seqs_full$country_ids,
          region_ids_train  = seqs_full$region_ids,
          confidence_weight_train = seqs_full$cw,
          X_val            = NULL, y_val = NULL,
          country_ids_val  = NULL, region_ids_val = NULL,
          confidence_weight_val = NULL,
          use_confidence_weight = isTRUE(use_confidence_weight),
          X_pred           = seqs_pred$X,
          dates_pred       = seqs_pred$dates,
          countries_pred   = seqs_pred$countries,
          country_ids_pred = seqs_pred$country_ids,
          region_ids_pred  = seqs_pred$region_ids,
          encoders         = encoders,
          obs_all          = obs_all,
          obs_target       = obs_target_full,
          cutoff_date      = cutoff_date,
          pred_date_stop   = pred_date_stop,
          fit_date_start   = fit_date_start,
          cases_99th       = cases_99th,
          scaler_center    = scaler_center,
          scaler_scale     = scaler_scale,
          features         = feats,
          split_method     = "rolling_cv",
          country_pool     = country_pool,
          target_iso       = target_iso,
          timesteps        = timesteps,
          pool_data        = pool_data,
          seq_params       = seq_params,
          rw_steps         = rw_steps
     )
}
