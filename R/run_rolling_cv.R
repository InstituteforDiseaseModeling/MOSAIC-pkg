#' Rolling-Window Forecast Validation for the MOSAIC Transmission Model
#'
#' Runs an expanding-window (fixed-anchor) rolling-origin backtest of the MOSAIC
#' transmission model. For each cutoff date \code{T} it (1) trains the
#' environmental-suitability (psi) LSTM on data up to \code{T}, (2) injects that
#' psi into the config, (3) calibrates the transmission model on observed cases
#' up to \code{T}, and (4) projects forward over the out-of-sample (OOS) window.
#'
#' This function is a \strong{fit-and-forecast engine only}: it produces
#' calibrations, projections, and one organized predictions artifact. It does
#' \strong{not} compute evaluation metrics, baselines, or skill scores — those are
#' done post-hoc by reading \code{predictions.parquet}.
#'
#' @details
#' \strong{Window.} The in-sample (IS) start is fixed at \code{config$date_start}
#' (the anchor); the simulation runs over the full \code{config} window
#' (\code{config$date_start} .. \code{config$date_stop}). For each cutoff \code{T}
#' the calibration likelihood only scores weeks \eqn{\le T} (observations after
#' \code{T} are masked to \code{NA}); the post-\code{T} portion of the simulation
#' is the forecast. A \code{embargo_weeks} gap separates the IS stop (\code{T})
#' from the OOS start (\code{T + embargo}); the embargo week is neither trained
#' nor labeled OOS.
#'
#' \strong{Leakage discipline.} psi is re-fit per cutoff with
#' \code{fit_date_stop = T}; the harness \emph{owns} the leakage-critical date
#' arguments to \code{\link{est_suitability}} and overrides any date keys passed
#' via \code{est_suitability_spec} (with a warning). \code{est_suitability_spec}
#' therefore controls only modeling choices (target, features, architecture), not
#' the cutoff window.
#'
#' \strong{Coupled metapopulation.} \code{iso} may be a single country or a vector;
#' a vector runs as the coupled metapopulation (one calibration per cutoff covering
#' all listed locations). Thus there is one \code{run_MOSAIC} calibration per cutoff
#' (not per country).
#'
#' \strong{Outputs.} Under \code{dir_output}: \code{manifest.json} (settings +
#' per-run index with status), \code{predictions.parquet} (the compiled long
#' table), and \code{runs/cutoff_<T>/} (the native \code{run_MOSAIC} directory for
#' each cutoff). \code{predictions.parquet} is a derived view — it can be rebuilt
#' from the run directories with \code{\link{compile_rolling_cv_predictions}}.
#'
#' The predictions table has one row per (cutoff x location x date x metric) with
#' columns: \code{run_id, iso_code, anchor_date, cutoff_date, date, metric,
#' segment} (IS/embargo/OOS), \code{weeks_ahead, horizon_bucket, observed,
#' observed_source, pred_median}, and CI columns (\code{pi*_lo}/\code{pi*_hi}).
#' \code{observed} is the held-out (unmasked) trusted surveillance value, so OOS
#' rows carry the real target for post-hoc scoring.
#'
#' @param PATHS Path list from \code{\link{get_paths}}.
#' @param iso Character; one ISO3 code or a vector (coupled metapopulation).
#' @param n_cutoffs Integer; number of monthly cutoffs (default 12).
#' @param latest_cutoff Date/character or NULL; most-recent cutoff. If NULL,
#'   computed as \code{(last scorable observed date) - embargo - max(horizons)}.
#' @param step_months Integer; months between cutoffs (default 1).
#' @param horizons_months Numeric vector of forecast horizons in months
#'   (default \code{c(1,3,5)}); used to set the projection length and to label OOS
#'   points. The largest horizon bounds the latest cutoff.
#' @param embargo_weeks Integer; gap between IS stop (T) and OOS start (default 1).
#' @param base_config MOSAIC config (default \code{MOSAIC::config_default}); its
#'   window defines the anchor and projection span.
#' @param priors Priors list (default \code{MOSAIC::priors_default}).
#' @param control \code{run_MOSAIC} control list, or NULL for an experiment-grade
#'   cheap default (fixed \code{n_simulations}, plots off). See
#'   \code{\link{mosaic_control_defaults}}.
#' @param optimize_subset Logical (default \code{TRUE}); enable
#'   \code{run_MOSAIC}'s post-ensemble best-subset optimizer
#'   (\code{control$predictions$optimize_subset}). When \code{TRUE} the harness
#'   sets this on the resolved control for every cutoff, so the ensemble is
#'   re-scored against the training-window observed series and the posterior is
#'   driven by the optimizer-selected subset. Set \code{FALSE} to use the raw
#'   candidate ensemble.
#' @param est_suitability_spec Named list of \emph{modeling} arguments passed
#'   through to \code{\link{est_suitability}} (e.g. \code{n_splits},
#'   \code{exclude_covariates}). Date arguments are ignored (harness-owned).
#' @param dask_spec Optional Dask/Coiled spec passed to \code{run_MOSAIC}.
#' @param dir_output Directory for the experiment artifact (created if needed).
#' @param verbose Logical (default TRUE).
#'
#' @return Invisibly, the manifest list. Side effects: writes
#'   \code{manifest.json}, \code{predictions.parquet}, and \code{runs/} under
#'   \code{dir_output}.
#'
#' @seealso \code{\link{run_MOSAIC}}, \code{\link{est_suitability}},
#'   \code{\link{compile_rolling_cv_predictions}}
#'
#' @importFrom utils read.csv
#' @export
run_rolling_cv <- function(PATHS,
                           iso,
                           n_cutoffs            = 12L,
                           latest_cutoff        = NULL,
                           step_months          = 1L,
                           horizons_months      = c(1, 3, 5),
                           embargo_weeks        = 1L,
                           base_config          = MOSAIC::config_default,
                           priors               = MOSAIC::priors_default,
                           control              = NULL,
                           optimize_subset      = TRUE,
                           est_suitability_spec = list(),
                           dask_spec            = NULL,
                           dir_output,
                           verbose              = TRUE) {

     stopifnot(is.character(iso), length(iso) >= 1L)
     if (missing(dir_output) || is.null(dir_output)) stop("dir_output is required.")
     horizons_months <- sort(unique(as.numeric(horizons_months)))
     max_h_days      <- ceiling(max(horizons_months) * 30.4375)
     embargo_days    <- as.integer(embargo_weeks) * 7L

     cfg_start <- as.Date(base_config$date_start)
     cfg_stop  <- as.Date(base_config$date_stop)
     cfg_dates <- seq.Date(cfg_start, cfg_stop, by = "day")

     # ---- latest scorable observed date (from the unmasked trusted config) ----
     loc_cfg_full <- MOSAIC::get_location_config(iso = iso, config = base_config)
     obs_cases_full  <- .rcv_as_matrix(loc_cfg_full$reported_cases,  length(loc_cfg_full$location_name), length(cfg_dates))
     obs_deaths_full <- .rcv_as_matrix(loc_cfg_full$reported_deaths, length(loc_cfg_full$location_name), length(cfg_dates))
     has_obs    <- colSums(!is.na(obs_cases_full)) > 0
     L_obs      <- if (any(has_obs)) max(cfg_dates[has_obs]) else cfg_stop

     # ---- cutoff schedule ----
     if (is.null(latest_cutoff)) {
          latest_cutoff <- L_obs - embargo_days - max_h_days
     }
     cutoffs <- .rolling_cv_cutoffs(as.Date(latest_cutoff), n_cutoffs, step_months)
     cutoffs <- cutoffs[cutoffs > cfg_start]                  # must have burn-in
     if (length(cutoffs) == 0L) stop("No valid cutoffs (check window vs anchor).")

     if (is.null(control)) control <- .rcv_cheap_control()
     # Harness owns the best-subset optimizer toggle: rescore the ensemble against
     # the (training-window) observed series and drive the posterior from the
     # optimizer-selected subset. Forced onto the resolved control so it applies
     # whether `control` is the cheap default or user-supplied.
     if (is.null(control$predictions)) control$predictions <- list()
     control$predictions$optimize_subset <- isTRUE(optimize_subset)

     dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)
     runs_dir <- file.path(dir_output, "runs")
     dir.create(runs_dir, showWarnings = FALSE)

     run_records  <- vector("list", length(cutoffs))
     pred_tables  <- vector("list", length(cutoffs))

     for (k in seq_along(cutoffs)) {
          T_k    <- cutoffs[k]
          run_id <- sprintf("cutoff_%s", format(T_k, "%Y-%m-%d"))
          dir_k  <- file.path(runs_dir, run_id)
          t0     <- Sys.time()
          if (verbose) message(sprintf("[%d/%d] %s  (IS %s -> %s | OOS to %s)",
                    k, length(cutoffs), run_id, format(cfg_start), format(T_k),
                    format(T_k + embargo_days + max_h_days)))

          rec <- list(run_id = run_id, iso = iso, cutoff_date = as.character(T_k),
                      is_range = c(as.character(cfg_start), as.character(T_k)),
                      oos_range = c(as.character(T_k + embargo_days),
                                    as.character(min(T_k + embargo_days + max_h_days, cfg_stop))),
                      dir = file.path("runs", run_id), status = "pending")

          res <- tryCatch({
               # 1. Re-fit psi on data <= T (leakage-clean) — done EVERY cutoff;
               #    this per-cutoff refit is the point of the rolling-CV test. The
               #    harness owns the date args; est_suitability_spec controls only
               #    modeling knobs (e.g. n_splits, feature set) and run speed.
               es_args <- .rcv_merge_est_args(est_suitability_spec, list(
                    PATHS          = PATHS,
                    fit_date_stop  = T_k,
                    pred_date_start= cfg_start,
                    pred_date_stop = cfg_stop))
               do.call(MOSAIC::est_suitability, es_args)
               psi_csv <- file.path(PATHS$MODEL_INPUT, "pred_psi_suitability_day.csv")

               # 2. build cutoff config: subset loc, swap psi, mask obs > T
               cfg <- MOSAIC::get_location_config(iso = iso, config = base_config)
               cfg$psi_jt <- .rolling_cv_psi_matrix(psi_csv, cfg$location_name, cfg_dates)
               nloc <- length(cfg$location_name)
               rc <- .rcv_as_matrix(cfg$reported_cases,  nloc, length(cfg_dates))
               rd <- .rcv_as_matrix(cfg$reported_deaths, nloc, length(cfg_dates))
               mask <- cfg_dates > T_k
               rc[, mask] <- NA; rd[, mask] <- NA
               cfg$reported_cases <- rc; cfg$reported_deaths <- rd

               # 3. calibrate <= T + project full window
               MOSAIC::run_MOSAIC(config = cfg, priors = priors, dir_output = dir_k,
                                  control = control, dask_spec = dask_spec)

               # 4. compile predictions from the ensemble (predicted) + held-out obs
               ens_path <- file.path(dir_k, "2_calibration", "ensemble_candidate.rds")
               if (!file.exists(ens_path)) stop("ensemble_candidate.rds not found at ", ens_path)
               ens <- readRDS(ens_path)
               pred_tables[[k]] <- .rolling_cv_compile_run(
                    ensemble       = ens,
                    run_id         = run_id,
                    cutoff         = T_k,
                    anchor         = cfg_start,
                    embargo_days   = embargo_days,
                    horizons_months= horizons_months,
                    obs_cases      = obs_cases_full,     # unmasked, trusted
                    obs_deaths     = obs_deaths_full,
                    obs_dates      = cfg_dates,
                    location_names = loc_cfg_full$location_name)
               "success"
          }, error = function(e) {
               if (verbose) message("  FAILED: ", conditionMessage(e))
               structure("failed", message = conditionMessage(e))
          })

          rec$status      <- if (identical(res, "success")) "success" else "failed"
          if (!identical(res, "success")) rec$error <- attr(res, "message")
          rec$runtime_min <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2)
          run_records[[k]] <- rec
     }

     # ---- compile unified predictions.parquet ----
     pred_tables <- Filter(Negate(is.null), pred_tables)
     predictions <- if (length(pred_tables)) do.call(rbind, pred_tables) else NULL
     if (!is.null(predictions)) {
          .rcv_write_parquet(predictions, file.path(dir_output, "predictions.parquet"))
     }

     # ---- manifest + README ----
     manifest <- list(
          experiment            = "rolling_cv",
          created               = as.character(Sys.time()),
          mosaic_pkg_version     = as.character(utils::packageVersion("MOSAIC")),
          spec = list(
               anchor_date      = as.character(cfg_start),
               window_stop      = as.character(cfg_stop),
               horizons_months  = horizons_months,
               embargo_weeks    = as.integer(embargo_weeks),
               step_months      = as.integer(step_months),
               n_cutoffs        = length(cutoffs),
               latest_cutoff    = as.character(max(cutoffs)),
               iso              = iso,
               optimize_subset  = isTRUE(optimize_subset),
               est_suitability_spec = est_suitability_spec),
          runs = run_records)
     .rcv_write_json(manifest, file.path(dir_output, "manifest.json"))
     .rcv_write_readme(dir_output)

     n_ok <- sum(vapply(run_records, function(r) identical(r$status, "success"), logical(1)))
     if (verbose) message(sprintf("Done: %d/%d cutoffs succeeded. Artifact: %s",
                                  n_ok, length(cutoffs), dir_output))
     invisible(manifest)
}


# ============================ internal helpers ============================

#' Generate a monthly rolling-origin cutoff schedule (back from the latest)
#' @keywords internal
#' @noRd
.rolling_cv_cutoffs <- function(latest_cutoff, n_cutoffs, step_months) {
     latest_cutoff <- as.Date(latest_cutoff)
     offs <- seq.int(0L, by = as.integer(step_months), length.out = as.integer(n_cutoffs))
     dts  <- vapply(offs, function(m) as.character(.rcv_add_months(latest_cutoff, -m)),
                    character(1))
     sort(as.Date(dts))
}

#' Add (possibly negative) whole months to a Date, clamping day-of-month
#' @keywords internal
#' @noRd
.rcv_add_months <- function(d, n) {
     lt <- as.POSIXlt(as.Date(d))
     tm   <- (lt$year + 1900L) * 12L + lt$mon + as.integer(n)   # absolute month index
     newY <- tm %/% 12L
     newM <- tm %% 12L + 1L                                     # 1-12
     # last day of target month = day before the 1st of the following month
     nxtY <- newY + (newM %/% 12L)
     nxtM <- (newM %% 12L) + 1L
     last_day <- as.integer(format(
          as.Date(sprintf("%04d-%02d-01", nxtY, nxtM)) - 1L, "%d"))
     day  <- min(lt$mday, last_day)                             # clamp (e.g. Jan 31 - 1mo -> Feb 28)
     as.Date(sprintf("%04d-%02d-%02d", newY, newM, day))
}

#' Per-date IS / embargo / OOS labels + weeks-ahead + horizon bucket
#' @keywords internal
#' @noRd
.rolling_cv_label <- function(dates, cutoff, embargo_days, horizons_months) {
     dates  <- as.Date(dates); cutoff <- as.Date(cutoff)
     oos0   <- cutoff + embargo_days
     segment <- ifelse(dates <= cutoff, "IS",
                ifelse(dates <= oos0, "embargo", "OOS"))
     weeks_ahead <- ifelse(segment == "OOS",
                           as.integer(floor(as.numeric(dates - oos0) / 7)) + 1L, NA_integer_)
     hb <- sort(unique(horizons_months))
     horizon_bucket <- rep(NA_character_, length(dates))
     is_oos <- segment == "OOS"
     for (h in hb) {
          hend <- oos0 + ceiling(h * 30.4375)
          fill <- is_oos & is.na(horizon_bucket) & dates <= hend
          horizon_bucket[fill] <- sprintf("h%gmo", h)
     }
     data.frame(segment = segment, weeks_ahead = weeks_ahead,
                horizon_bucket = horizon_bucket, stringsAsFactors = FALSE)
}

#' Build psi_jt (locations x config-dates) from the est_suitability daily CSV
#' (mirrors data-raw/make_config_default.R)
#' @keywords internal
#' @noRd
.rolling_cv_psi_matrix <- function(psi_csv, location_names, dates) {
     if (!file.exists(psi_csv)) stop("psi prediction file not found: ", psi_csv)
     tmp <- utils::read.csv(psi_csv, stringsAsFactors = FALSE)
     tmp$date <- as.Date(tmp$date)
     tmp <- tmp[tmp$iso_code %in% location_names &
                tmp$date >= min(dates) & tmp$date <= max(dates), ]
     if (nrow(tmp) == 0L) stop("no psi rows overlap the config window")
     # Build (locations x config-dates) matrix with base R (no reshape2 dep).
     date_chr <- as.character(dates)
     full <- matrix(NA_real_, nrow = length(location_names), ncol = length(dates),
                    dimnames = list(location_names, date_chr))
     tmp$dchr <- as.character(tmp$date)
     agg <- stats::aggregate(pred_smooth ~ iso_code + dchr, data = tmp, FUN = mean)
     ir <- match(agg$iso_code, location_names)
     ic <- match(agg$dchr, date_chr)
     ok <- !is.na(ir) & !is.na(ic)
     full[cbind(ir[ok], ic[ok])] <- agg$pred_smooth[ok]
     # carry forward/back to fill any interpolation gaps (psi is smooth)
     for (i in seq_len(nrow(full))) {
          full[i, ] <- zoo::na.locf(zoo::na.locf(full[i, ], na.rm = FALSE),
                                    fromLast = TRUE, na.rm = FALSE)
     }
     full[location_names, , drop = FALSE]
}

#' Rebuild the rolling-CV predictions table from run directories
#'
#' Regenerates \code{predictions.parquet} from the per-cutoff \code{run_MOSAIC}
#' directories under a \code{run_rolling_cv()} artifact (e.g. after adding a
#' cutoff or to add quantile columns), without recalibrating.
#'
#' @param dir_output A \code{run_rolling_cv()} output directory (must contain
#'   \code{manifest.json} and \code{runs/}).
#' @param base_config Config used to recover the held-out (unmasked) observed
#'   series (default \code{MOSAIC::config_default}); must match the run config.
#' @param write Logical; write \code{predictions.parquet} (default TRUE).
#' @return The compiled long predictions data frame (invisibly if written).
#' @export
compile_rolling_cv_predictions <- function(dir_output,
                                           base_config = MOSAIC::config_default,
                                           write = TRUE) {
     mpath <- file.path(dir_output, "manifest.json")
     if (!file.exists(mpath)) stop("manifest.json not found in ", dir_output)
     man <- jsonlite::read_json(mpath, simplifyVector = TRUE)
     iso <- unlist(man$spec$iso)
     anchor <- as.Date(man$spec$anchor_date)
     horizons <- as.numeric(unlist(man$spec$horizons_months))
     embargo_days <- as.integer(man$spec$embargo_weeks) * 7L

     cfg_dates <- seq.Date(as.Date(base_config$date_start),
                           as.Date(base_config$date_stop), by = "day")
     loc <- MOSAIC::get_location_config(iso = iso, config = base_config)
     nloc <- length(loc$location_name)
     oc <- .rcv_as_matrix(loc$reported_cases,  nloc, length(cfg_dates))
     od <- .rcv_as_matrix(loc$reported_deaths, nloc, length(cfg_dates))

     runs <- man$runs
     tabs <- list()
     for (r in seq_len(nrow(runs))) {
          if (!identical(runs$status[r], "success")) next
          ens_path <- file.path(dir_output, runs$dir[r], "2_calibration", "ensemble_candidate.rds")
          if (!file.exists(ens_path)) next
          tabs[[length(tabs) + 1L]] <- .rolling_cv_compile_run(
               ensemble = readRDS(ens_path), run_id = runs$run_id[r],
               cutoff = as.Date(runs$cutoff_date[r]), anchor = anchor,
               embargo_days = embargo_days, horizons_months = horizons,
               obs_cases = oc, obs_deaths = od, obs_dates = cfg_dates,
               location_names = loc$location_name)
     }
     predictions <- if (length(tabs)) do.call(rbind, tabs) else NULL
     if (write && !is.null(predictions))
          .rcv_write_parquet(predictions, file.path(dir_output, "predictions.parquet"))
     invisible(predictions)
}

#' Compile one run's ensemble into the long predictions table
#' @keywords internal
#' @noRd
.rolling_cv_compile_run <- function(ensemble, run_id, cutoff, anchor, embargo_days,
                                    horizons_months, obs_cases, obs_deaths, obs_dates,
                                    location_names) {
     n_t   <- ensemble$n_time_points
     ds    <- as.Date(ensemble$date_start); de <- as.Date(ensemble$date_stop)
     edates <- seq(ds, de, length.out = n_t)
     locs  <- ensemble$location_names %||% location_names
     eq    <- ensemble$envelope_quantiles
     n_pair<- length(eq) / 2L
     lab   <- .rolling_cv_label(edates, cutoff, embargo_days, horizons_months)
     getrow <- function(mat, i) if (is.matrix(mat)) mat[i, ] else mat

     # map ensemble dates -> nearest observed (config daily) index for held-out obs
     obs_idx <- match(as.character(as.Date(edates)), as.character(as.Date(obs_dates)))

     out <- list()
     for (i in seq_along(locs)) {
          oi <- match(locs[i], location_names)
          for (metric in c("cases", "deaths")) {
               med <- if (metric == "cases") getrow(ensemble$cases_median, i)
                      else                    getrow(ensemble$deaths_median, i)
               obs_src <- if (metric == "cases") obs_cases else obs_deaths
               observed <- if (!is.na(oi)) getrow(obs_src, oi)[obs_idx] else rep(NA_real_, n_t)
               df <- data.frame(
                    run_id = run_id, iso_code = locs[i],
                    anchor_date = as.character(anchor), cutoff_date = as.character(cutoff),
                    date = as.Date(edates), metric = metric,
                    segment = lab$segment, weeks_ahead = lab$weeks_ahead,
                    horizon_bucket = lab$horizon_bucket,
                    observed = as.numeric(observed),
                    observed_source = "config_reported",
                    pred_median = as.numeric(med),
                    stringsAsFactors = FALSE)
               ci_c <- if (metric == "cases") ensemble$ci_bounds$cases else ensemble$ci_bounds$deaths
               for (p in seq_len(n_pair)) {
                    lo_q <- eq[p]; hi_q <- eq[length(eq) - p + 1L]
                    tag  <- sprintf("pi%g", round((hi_q - lo_q) * 100))
                    df[[paste0(tag, "_lo")]] <- as.numeric(getrow(ci_c[[p]]$lower, i))
                    df[[paste0(tag, "_hi")]] <- as.numeric(getrow(ci_c[[p]]$upper, i))
               }
               out[[length(out) + 1L]] <- df
          }
     }
     do.call(rbind, out)
}

#' Coerce a config reported_* field to an (n_loc x n_t) matrix
#' @keywords internal
#' @noRd
.rcv_as_matrix <- function(x, n_loc, n_t) {
     if (is.matrix(x)) return(x)
     matrix(x, nrow = n_loc, ncol = n_t, byrow = (n_loc == 1L))
}

#' Merge user est_suitability_spec with harness-owned date args (harness wins)
#' @keywords internal
#' @noRd
.rcv_merge_est_args <- function(spec, owned) {
     date_keys <- c("fit_date_start", "fit_date_stop", "pred_date_start", "pred_date_stop")
     bad <- intersect(names(spec), date_keys)
     if (length(bad)) {
          warning("est_suitability_spec date args ignored (harness-owned): ",
                  paste(bad, collapse = ", "), call. = FALSE)
          spec <- spec[setdiff(names(spec), date_keys)]
     }
     utils::modifyList(spec, owned)
}

#' Experiment-grade cheap calibration control
#' @keywords internal
#' @noRd
.rcv_cheap_control <- function() {
     ctrl <- tryCatch(MOSAIC::mosaic_control_defaults(), error = function(e) list())
     ctrl$calibration <- utils::modifyList(ctrl$calibration %||% list(),
                                           list(n_simulations = 2000L, n_iterations = 3L))
     ctrl$targets <- utils::modifyList(ctrl$targets %||% list(), list(ESS_param = 100L))
     ctrl$paths   <- utils::modifyList(ctrl$paths   %||% list(), list(plots = FALSE))
     ctrl$predictions <- utils::modifyList(ctrl$predictions %||% list(),
                                           list(optimize_subset = TRUE))
     ctrl
}

#' @keywords internal
#' @noRd
.rcv_write_parquet <- function(df, path) {
     ok <- requireNamespace("arrow", quietly = TRUE)
     if (ok) arrow::write_parquet(df, path)
     else    utils::write.csv(df, sub("\\.parquet$", ".csv", path), row.names = FALSE)
}

#' @keywords internal
#' @noRd
.rcv_write_json <- function(obj, path) {
     jsonlite::write_json(obj, path, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
}

#' @keywords internal
#' @noRd
.rcv_write_readme <- function(dir_output) {
     lines <- c(
          "# Rolling-window forecast-validation artifact",
          "",
          "| item | description |",
          "|---|---|",
          "| `manifest.json` | settings + per-run index (status, ranges, runtime) |",
          "| `predictions.parquet` | compiled long table: 1 row per cutoff x location x date x metric (IS/embargo/OOS labeled; observed + predicted median + CIs) |",
          "| `runs/cutoff_<T>/` | native run_MOSAIC output per cutoff |",
          "",
          "`predictions.parquet` is a derived view (rebuildable from `runs/`).",
          "Evaluation/baselines/skill scores are computed post-hoc from the predictions table.")
     writeLines(lines, file.path(dir_output, "README.md"))
}
