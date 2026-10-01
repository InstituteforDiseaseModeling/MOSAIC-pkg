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
#' \strong{not} compute evaluation metrics, baselines, or skill scores -- those are
#' done post-hoc by reading \code{predictions.parquet}.
#'
#' @details
#' \strong{Window.} The in-sample (IS) start is fixed at \code{config$date_start}
#' (the anchor); the simulation runs over the full \code{config} window
#' (\code{config$date_start} .. \code{config$date_stop}). For each cutoff \code{T}
#' the calibration likelihood only scores weeks \eqn{\le T} (observations after
#' \code{T} are masked to \code{NA}); the post-\code{T} portion of the simulation
#' is the forecast. A \code{embargo_weeks} gap separates the IS stop (\code{T})
#' from the OOS start: dates in \code{(T, T + embargo]} are labeled
#' \code{"embargo"} (neither trained nor scored) and the first OOS date is
#' \code{T + embargo + 1}. \code{weeks_ahead} counts whole weeks from that first
#' OOS date (week 1 = its first seven days).
#'
#' \strong{Leakage discipline.} What is rebuilt as of each cutoff:
#' \itemize{
#'   \item psi is re-fit per cutoff with \code{fit_date_stop = T}; the harness
#'     \emph{owns} the leakage-critical date arguments to
#'     \code{\link{est_suitability}} and overrides any date keys passed via
#'     \code{est_suitability_spec} (with a warning), so the spec controls only
#'     modeling choices (target, features, architecture).
#'   \item The reported CFR \code{mu_jt} and its prior are rebuilt from a
#'     WHO-annual GAM fitted only to years up to \code{year(T) - 1}, carried flat
#'     past that year's 1 July. The annual totals used are the final revised
#'     ones, not the vintage that had been published at \code{T}.
#'   \item Observed cases and deaths after \code{T} are masked, and epidemic peaks
#'     whose 14-day peak-shape scoring window reaches past \code{T} are dropped
#'     from the cutoff config (an empty set is kept as a 0-row table so the
#'     likelihood never falls back to the full \code{\link{epidemic_peaks}}
#'     dataset). The remaining peaks were still \emph{detected} on the full record.
#' }
#' What is \strong{not} as of the cutoff, so the hindcast is only approximately
#' leak-free:
#' \itemize{
#'   \item psi fitted in place (\code{psi_cache = NULL}), or from a cache built
#'     with any feature set other than \code{"v7.4"}, is trained on the canonical
#'     suitability panel. Its per-country target anchors and its flood-probability
#'     GAM were fitted on the whole panel, including rows after \code{T}. The run
#'     warns when this is the case; build the cache with
#'     \code{\link{prefit_rolling_cv_psi}(est_suitability_spec =
#'     list(feature_set = "v7.4", ...))} for per-cutoff leak-free panels.
#'   \item Every prior other than \code{mu_jt} comes from \code{priors} unchanged.
#'     \code{priors_default} carries centres derived from surveillance through its
#'     build date (e.g. the per-country \code{beta_j0_tot} centres, recentred on
#'     posterior medians of fits to the full record); supply as-of priors for a
#'     strictly leak-free hindcast.
#' }
#'
#' \strong{Coupled metapopulation.} \code{iso} may be a single country or a vector;
#' a vector runs as the coupled metapopulation (one calibration per cutoff covering
#' all listed locations). Thus there is one \code{run_MOSAIC} calibration per cutoff
#' (not per country).
#'
#' \strong{Outputs.} Under \code{dir_output}: \code{manifest.json} (settings +
#' per-run index with status), \code{predictions.parquet} (the compiled long
#' table), and \code{runs/cutoff_<T>/} (the native \code{run_MOSAIC} directory for
#' each cutoff). \code{predictions.parquet} is a derived view -- it can be rebuilt
#' from the run directories with \code{\link{compile_rolling_cv_predictions}}.
#'
#' The predictions table has one row per (cutoff x location x date x metric) with
#' columns: \code{run_id, iso_code, anchor_date, cutoff_date, date, metric,
#' segment} (IS/embargo/OOS), \code{weeks_ahead, horizon_bucket, observed,
#' observed_source, pred_central} (the scored series), \code{pred_mean,
#' pred_median, central_method}, CI columns (\code{pi*_lo}/\code{pi*_hi}) and
#' \code{pred_median_obs}, the median of the same draws as the CI columns (the
#' observation-level predictive median since v0.101.0), which WIS pairs with them.
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
#'   \code{\link{mosaic_control_defaults}}. The harness forces
#'   \code{paths$clean_output = TRUE}, so each cutoff's \code{runs/cutoff_<T>/}
#'   directory is wiped before its calibration and simulation shards left by an
#'   earlier interrupted attempt cannot enter the new posterior.
#' @param optimize_subset Logical (default \code{TRUE}); enable
#'   \code{run_MOSAIC}'s post-ensemble best-subset optimizer
#'   (\code{control$predictions$optimize_subset}). When \code{TRUE} the harness
#'   sets this on the resolved control for every cutoff, so the ensemble is
#'   re-scored against the training-window observed series and the posterior is
#'   driven by the optimizer-selected subset. Set \code{FALSE} to use the raw
#'   candidate ensemble.
#' @param models Character vector of model types to score and carry in
#'   \code{predictions.parquet} (default
#'   \code{c("ensemble","ensemble_opt","medoid")}). \code{"ensemble"}
#'   (posterior-weighted candidate) is always included. \code{"ensemble_opt"} is
#'   the optimizer-selected subset, emitted only for cutoffs where the optimizer
#'   actually ran and selected a subset (\code{subset_opt.rds} present, or a
#'   finite \code{n_ensemble_params_tier} in \code{summary.json}); with
#'   \code{optimize_subset = FALSE} it is dropped from \code{models} up front,
#'   and a cutoff whose optimizer selected nothing is skipped with a warning
#'   rather than duplicating the candidate ensemble, which
#'   \code{run_MOSAIC()} saves as a fallback \code{ensemble_optimized.rds}. \code{"medoid"} is
#'   re-simulated from its saved config (see \code{n_reps_best_medoid}).
#'   \code{"best"} is accepted for back-compat but is no longer produced by
#'   \code{run_MOSAIC()} (no \code{config_best.json}); it is skipped with a
#'   warning unless an older run dir still carries that file. Each model appears
#'   as a value of the \code{model} column.
#' @param n_reps_best_medoid Integer (default 50); number of stochastic
#'   reruns used to build the predictive median + intervals for the \code{best}
#'   and \code{medoid} configs. These reruns execute locally in the calling R
#'   process, so cost scales with this value times the number of
#'   cutoffs and locations.
#' @param central_method Ensemble central tendency used for the compiled
#'   predictions and the in-sample calibration metrics/medoid: \code{"mean"}
#'   (the expected count, which never collapses to zero on sparse deaths) or
#'   \code{"median"} (the typical trajectory). Scalar or per-channel
#'   \code{c(cases=, deaths=)}; default \code{c(cases = "median", deaths =
#'   "mean")} (both mean from v0.98.0 to v0.100.x, both median from v0.46.1 to
#'   v0.97.x). The
#'   predictions table carries \code{pred_central} (this choice) plus
#'   \code{pred_mean}/\code{pred_median} for cross-walk; WIS/coverage remain
#'   quantile-based and are unaffected.
#' @param est_suitability_spec Named list of \emph{modeling} arguments passed
#'   through to \code{\link{est_suitability}} (e.g. \code{architecture},
#'   \code{feature_set}, \code{response_var}, \code{bias_correct}, and the
#'   lstm_v2 \code{arch_control} list). Date arguments are ignored (harness-owned).
#'   Deprecated v0.33 keys (\code{n_splits}, \code{exclude_covariates}) are
#'   accepted but ignored with a per-cutoff deprecation message -- prefer
#'   \code{arch_control} for lstm_v2 knobs. When \code{psi_cache} is supplied this
#'   spec is used \emph{only} to recompute the cache spec-hash for validation; the
#'   per-cutoff \code{est_suitability()} fit is skipped entirely.
#' @param psi_cache NULL (default) or a directory produced by
#'   \code{\link{prefit_rolling_cv_psi}}. When NULL the per-cutoff psi is re-fit
#'   in-place (original behavior). When set, the per-cutoff \code{est_suitability()}
#'   call is skipped and the frozen \code{psi_<T>.csv} is loaded from this cache
#'   directory instead. The run \strong{hard-errors} if a requested cutoff is
#'   absent from the cache manifest, if the run's \code{est_suitability_spec}
#'   hash does not match the manifest \code{spec_hash} recorded for that cutoff,
#'   or if the cache's prediction window does not cover \code{base_config}'s
#'   start through the cutoff's last OOS date. When \code{psi_cache} is NULL the
#'   per-cutoff psi is fitted into a scratch \code{MODEL_INPUT} (the canonical
#'   \code{pred_psi_suitability_day.csv} is never overwritten) and kept as
#'   \code{runs/psi_cutoff_<T>.csv}.
#' @param dir_output Directory for the experiment artifact (created if needed).
#' @param verbose Logical (default TRUE).
#'
#' @return Invisibly, the manifest list. Side effects: writes
#'   \code{manifest.json}, \code{predictions.parquet}, and \code{runs/} under
#'   \code{dir_output}. \code{manifest.json} is rewritten atomically after every
#'   cutoff (\code{status = "running"}, then \code{"complete"}), so the cutoffs
#'   finished before an interruption can still be compiled with
#'   \code{\link{compile_rolling_cv_predictions}}.
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
                           models               = c("ensemble", "ensemble_opt", "medoid"),
                           n_reps_best_medoid  = 50L,
                           central_method       = c(cases = "median", deaths = "mean"),
                           est_suitability_spec = list(),
                           psi_cache            = NULL,
                           dir_output,
                           verbose              = TRUE) {

     stopifnot(is.character(iso), length(iso) >= 1L)
     if (missing(dir_output) || is.null(dir_output)) stop("dir_output is required.")
     models <- .rcv_validate_models(models)
     models <- union("ensemble", models)              # candidate ensemble always emitted
     if (!isTRUE(optimize_subset) && "ensemble_opt" %in% models) {
          message("run_rolling_cv: optimize_subset = FALSE, so 'ensemble_opt' is dropped from models.")
          models <- setdiff(models, "ensemble_opt")
     }
     n_reps_best_medoid <- as.integer(n_reps_best_medoid)
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
     # Central tendency is harness-owned too: forced onto the inner control so the
     # per-cutoff calibration's medoid + ensemble metrics use the SAME summary
     # the compiled predictions table is scored on (in-sample == out-of-sample).
     central_method <- .mosaic_resolve_central_method(central_method)
     control$predictions$central_method <- central_method
     # Each cutoff owns runs/cutoff_<T>/ outright. run_MOSAIC() combines every
     # sim_* shard it finds in 2_calibration/samples, so a directory left by an
     # interrupted earlier attempt (possibly under a different psi, prior or
     # simulation budget) would silently pool its stale shards into this
     # cutoff's posterior. Wiping the per-cutoff directory first prevents that.
     if (is.null(control$paths)) control$paths <- list()
     control$paths$clean_output <- TRUE

     # ---- frozen-psi cache (optional) ----
     # When psi_cache is set, the per-cutoff est_suitability() fit is skipped and
     # the frozen psi_<T>.csv is loaded from the cache. The cache manifest is read
     # once and the run's modeling spec hash is validated per cutoff below.
     use_psi_cache <- !is.null(psi_cache)
     psi_cache_man <- NULL
     oos_end_of <- function(T_k) min(T_k + embargo_days + max_h_days, cfg_stop)
     if (use_psi_cache) {
          if (!is.character(psi_cache) || length(psi_cache) != 1L || !nzchar(psi_cache))
               stop("psi_cache must be a single cache-directory path or NULL.")
          man_path <- file.path(psi_cache, "psi_manifest.json")
          if (!file.exists(man_path))
               stop("psi_cache has no psi_manifest.json: ", psi_cache,
                    " (build it with prefit_rolling_cv_psi()).")
          psi_cache_man <- .rcv_psi_read_manifest(man_path)
          if (!length(psi_cache_man$cutoffs))
               stop("psi_cache manifest records no cutoffs: ", man_path)
          # Validate the WHOLE requested schedule against the cache up front so a
          # missing cutoff, spec_hash mismatch or too-short prediction window is a
          # fatal configuration error (aborts the run) rather than a per-cutoff
          # "failed" record swallowed by the loop's tryCatch.
          for (T_chk in as.list(cutoffs))
               invisible(.rcv_psi_cache_lookup(psi_cache, psi_cache_man, T_chk,
                                               est_suitability_spec,
                                               need_start = cfg_start,
                                               need_stop  = oos_end_of(T_chk)))
          not_leakfree <- vapply(psi_cache_man$cutoffs, function(e)
               !identical(as.character(e$hazard_panel %||% ""), "v7.4_leakfree"),
               logical(1))
          cache_cuts <- vapply(psi_cache_man$cutoffs, function(e) as.character(e$cutoff),
                               character(1))
          if (any(not_leakfree[cache_cuts %in% as.character(cutoffs)]))
               warning(.rcv_psi_leak_warning("psi_cache was not built from per-cutoff leak-free panels"),
                       call. = FALSE)
     } else {
          warning(.rcv_psi_leak_warning("psi_cache = NULL fits psi from the canonical panel"),
                  call. = FALSE)
     }

     # WHO annual data for the per-cutoff reported-CFR refit (one GAM per
     # distinct last data year, shared by the cutoffs in the same calendar year).
     # Checked before anything is written: without it no cutoff can be built.
     if (!requireNamespace("mgcv", quietly = TRUE))
          stop("run_rolling_cv() refits the reported CFR per cutoff with mgcv; install it.")
     who_annual_path <- file.path(PATHS$DATA_WHO_ANNUAL, "who_afro_annual.csv")
     if (is.null(PATHS$DATA_WHO_ANNUAL) || !file.exists(who_annual_path))
          stop("WHO annual data not found at ", who_annual_path,
               "; run_rolling_cv() refits the reported CFR per cutoff from it.")
     who_annual <- utils::read.csv(who_annual_path, stringsAsFactors = FALSE)
     cfr_asof <- list()

     dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)
     runs_dir <- file.path(dir_output, "runs")
     dir.create(runs_dir, showWarnings = FALSE)

     run_records  <- vector("list", length(cutoffs))
     pred_tables  <- vector("list", length(cutoffs))

     # manifest.json is (re)written atomically after every cutoff so that an
     # interrupted run (OOM, preemption, SIGKILL) still leaves a manifest that
     # compile_rolling_cv_predictions() can rebuild predictions from.
     spec_out <- list(
          anchor_date      = as.character(cfg_start),
          window_stop      = as.character(cfg_stop),
          horizons_months  = horizons_months,
          embargo_weeks    = as.integer(embargo_weeks),
          step_months      = as.integer(step_months),
          n_cutoffs        = length(cutoffs),
          latest_cutoff    = as.character(max(cutoffs)),
          iso              = iso,
          optimize_subset  = isTRUE(optimize_subset),
          models           = models,
          n_reps_best_medoid = n_reps_best_medoid,
          # as.list() so the per-channel names survive JSON round-trip:
          # auto_unbox drops the names of a length-2 named vector (-> a bare
          # array), which would break compile_rolling_cv_predictions()'s
          # read-back. A named list serializes as a JSON object.
          central_method   = as.list(central_method),
          est_suitability_spec = est_suitability_spec,
          psi_cache        = if (use_psi_cache) psi_cache else NULL)
     created <- as.character(Sys.time())
     write_manifest <- function(status) {
          manifest <- list(
               experiment         = "rolling_cv",
               created            = created,
               status             = status,
               mosaic_pkg_version = as.character(utils::packageVersion("MOSAIC")),
               spec               = spec_out,
               runs               = Filter(Negate(is.null), run_records))
          .rcv_write_json_atomic(manifest, file.path(dir_output, "manifest.json"))
          manifest
     }

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
                      oos_range = c(as.character(T_k + embargo_days + 1L),
                                    as.character(oos_end_of(T_k))),
                      dir = file.path("runs", run_id), status = "pending")

          res <- tryCatch({
               # 1. Obtain psi for this cutoff. Two modes:
               #    (a) psi_cache=NULL  -> re-fit psi on data <= T (the per-cutoff
               #        refit is the point of the rolling-CV test) into a scratch
               #        MODEL_INPUT, keeping a copy under runs/. The harness owns
               #        the date args; est_suitability_spec controls only modeling
               #        knobs (architecture, feature_set, arch_control).
               #    (b) psi_cache=<dir> -> skip the fit and load the FROZEN
               #        psi_<T>.csv from the cache, after asserting the cutoff is
               #        present, the modeling-spec hash matches the manifest and
               #        the cache's prediction window covers this cutoff.
               if (use_psi_cache) {
                    psi_csv <- .rcv_psi_cache_lookup(
                         psi_cache, psi_cache_man, T_k, est_suitability_spec,
                         need_start = cfg_start, need_stop = oos_end_of(T_k))
               } else {
                    es_args <- .rcv_merge_est_args(est_suitability_spec, list(
                         PATHS          = PATHS,
                         fit_date_stop  = T_k,
                         pred_date_start= cfg_start,
                         pred_date_stop = cfg_stop))
                    psi_csv <- .rcv_fit_psi_isolated(
                         es_args, file.path(runs_dir, sprintf("psi_%s.csv", run_id)))
               }

               # 2. build cutoff config: subset loc, swap psi, as-of mu_jt,
               #    as-of epidemic peaks, mask obs > T
               cfg <- MOSAIC::get_location_config(iso = iso, config = base_config)
               cfg$psi_jt <- .rolling_cv_psi_matrix(psi_csv, cfg$location_name, cfg_dates,
                                                    required_stop = oos_end_of(T_k))
               # The reported CFR and its prior, from WHO annual years <= year(T) - 1
               # only. Calibration's CFR offset, estimated on deaths <= T, then
               # carries into the forecast through the post-hoc death redraw.
               last_cfr_year <- as.integer(format(T_k, "%Y")) - 1L
               key <- as.character(last_cfr_year)
               if (is.null(cfr_asof[[key]]))
                    cfr_asof[[key]] <- .rcv_cfr_asof(who_annual, last_cfr_year, cfg_stop)
               cfg <- .rcv_apply_cfr_asof(cfg, cfr_asof[[key]], cfg_dates)
               priors_k <- priors
               priors_k$mu_jt <- .mosaic_mu_jt_prior(
                    cfr_asof[[key]]$predictions, location_name = cfg$location_name,
                    sd_year = cfr_asof[[key]]$sigma, tau = cfr_asof[[key]]$tau,
                    sd_product = priors$mu_jt$sd_product %||% 0.3)
               cfg$epidemic_peaks <- .rcv_asof_epidemic_peaks(cfg$epidemic_peaks, T_k)
               nloc <- length(cfg$location_name)
               rc <- .rcv_as_matrix(cfg$reported_cases,  nloc, length(cfg_dates))
               rd <- .rcv_as_matrix(cfg$reported_deaths, nloc, length(cfg_dates))
               mask <- cfg_dates > T_k
               rc[, mask] <- NA; rd[, mask] <- NA
               cfg$reported_cases <- rc; cfg$reported_deaths <- rd

               # 3. calibrate <= T + project full window
               MOSAIC::run_MOSAIC(config = cfg, priors = priors_k, dir_output = dir_k,
                                  control = control)

               # 4. compile predictions for every requested model type
               #    (ensemble candidate / optimizer subset / best / medoid)
               #    against the held-out trusted observed series.
               pred_tables[[k]] <- .rcv_compile_all_models(
                    run_dir        = dir_k,
                    run_id         = run_id,
                    cutoff         = T_k,
                    anchor         = cfg_start,
                    embargo_days   = embargo_days,
                    horizons_months= horizons_months,
                    obs_cases      = obs_cases_full,     # unmasked, trusted
                    obs_deaths     = obs_deaths_full,
                    obs_dates      = cfg_dates,
                    location_names = loc_cfg_full$location_name,
                    models         = models,
                    n_reps         = n_reps_best_medoid,
                    central_method = central_method)
               "success"
          }, error = function(e) {
               if (verbose) message("  FAILED: ", conditionMessage(e))
               structure("failed", message = conditionMessage(e))
          })

          rec$status      <- if (identical(res, "success")) "success" else "failed"
          if (!identical(res, "success")) rec$error <- attr(res, "message")
          rec$runtime_min <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2)
          run_records[[k]] <- rec
          write_manifest("running")
     }

     # ---- compile unified predictions.parquet ----
     pred_tables <- Filter(Negate(is.null), pred_tables)
     predictions <- if (length(pred_tables)) do.call(rbind, pred_tables) else NULL
     if (!is.null(predictions)) {
          .rcv_write_parquet(predictions, file.path(dir_output, "predictions.parquet"))
     }

     # ---- final manifest + README ----
     manifest <- write_manifest("complete")
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
     # First OOS date is oos0 + 1, so week 1 is oos0+1 .. oos0+7.
     weeks_ahead <- ifelse(segment == "OOS",
                           as.integer(floor(as.numeric(dates - oos0 - 1) / 7)) + 1L, NA_integer_)
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
#'
#' Interior gaps are filled by carrying the neighbouring value (psi is smooth),
#' but the CSV must cover every location from \code{min(dates)} through
#' \code{required_stop} (the end of the scored OOS window): a location missing
#' from the CSV, a series that starts after \code{min(dates)}, or one that ends
#' before \code{required_stop} is an error, because carrying the last value
#' forward would score a flat psi as if it were a forecast (the trailing-fill
#' artefact removed from est_suitability() in v0.44.14). A series that ends
#' between \code{required_stop} and \code{max(dates)} is filled flat with a
#' warning, since those dates are simulated but never scored.
#' @keywords internal
#' @noRd
.rolling_cv_psi_matrix <- function(psi_csv, location_names, dates,
                                   required_stop = max(dates)) {
     if (!file.exists(psi_csv)) stop("psi prediction file not found: ", psi_csv)
     tmp <- utils::read.csv(psi_csv, stringsAsFactors = FALSE)
     # Option A (v0.34): consume the canonical `psi` column (smoothed +
     # bias-corrected). Fail loudly on a stale pre-v0.34 CSV rather than silently
     # falling back to the pre-bias-correction `pred_smooth` (the latent no-op bug).
     if (!"psi" %in% names(tmp))
          stop("psi prediction file lacks the canonical `psi` column (", psi_csv,
               "); regenerate it with est_suitability() v0.34+.")
     tmp$date <- as.Date(tmp$date)
     tmp <- tmp[tmp$iso_code %in% location_names &
                tmp$date >= min(dates) & tmp$date <= max(dates), ]
     if (nrow(tmp) == 0L) stop("no psi rows overlap the config window")
     # Build (locations x config-dates) matrix with base R (no reshape2 dep).
     date_chr <- as.character(dates)
     full <- matrix(NA_real_, nrow = length(location_names), ncol = length(dates),
                    dimnames = list(location_names, date_chr))
     tmp$dchr <- as.character(tmp$date)
     agg <- stats::aggregate(psi ~ iso_code + dchr, data = tmp, FUN = mean)
     ir <- match(agg$iso_code, location_names)
     ic <- match(agg$dchr, date_chr)
     ok <- !is.na(ir) & !is.na(ic)
     full[cbind(ir[ok], ic[ok])] <- agg$psi[ok]

     # Coverage: every location, from the first config date through the end of
     # the scored window.
     has_val <- !is.na(full)
     absent <- location_names[rowSums(has_val) == 0L]
     if (length(absent))
          stop("psi file has no rows for location(s) ", paste(absent, collapse = ", "),
               " in the config window: ", psi_csv, call. = FALSE)
     first_d <- dates[apply(has_val, 1L, function(v) min(which(v)))]
     last_d  <- dates[apply(has_val, 1L, function(v) max(which(v)))]
     late <- first_d > min(dates)
     if (any(late))
          stop(sprintf("psi file starts after the config start (%s) for %s: %s",
                       format(min(dates)),
                       paste(sprintf("%s (%s)", location_names[late], format(first_d[late])),
                             collapse = ", "), psi_csv), call. = FALSE)
     required_stop <- min(as.Date(required_stop), max(dates))
     short <- last_d < required_stop
     if (any(short))
          stop(sprintf(paste0("psi file ends before the last scored OOS date (%s) for %s; ",
                              "carrying psi forward would score a flat psi as a forecast: %s"),
                       format(required_stop),
                       paste(sprintf("%s (%s)", location_names[short], format(last_d[short])),
                             collapse = ", "), psi_csv), call. = FALSE)
     tail_fill <- last_d < max(dates)
     if (any(tail_fill))
          warning(sprintf(paste0("psi file ends before the config stop (%s) for %s; psi is held ",
                                 "flat over those dates. They lie past the scored window only while ",
                                 "evaluate_rolling_cv() uses the harness embargo; a larger ",
                                 "embargo_weeks there shifts the scored window into them."),
                          format(max(dates)),
                          paste(sprintf("%s (%s)", location_names[tail_fill],
                                        format(last_d[tail_fill])), collapse = ", ")),
                  call. = FALSE)

     # carry forward/back to fill interior interpolation gaps (psi is smooth)
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
#'   \code{manifest.json} and \code{runs/}); a manifest from an interrupted run
#'   (\code{status = "running"}) compiles the cutoffs it records.
#' @param base_config Config used to recover the held-out (unmasked) observed
#'   series (default \code{MOSAIC::config_default}); must match the run config.
#' @param models Character vector of model types to compile, from
#'   \code{"ensemble"}, \code{"ensemble_opt"}, \code{"best"}, \code{"medoid"};
#'   NULL (default) uses the set recorded in the run manifest.
#' @param n_reps_best_medoid Integer or NULL (default); number of stochastic
#'   replicates to draw for the single-config \code{best}/\code{medoid}
#'   models. NULL reuses the value stored in the run manifest.
#' @param central_method Central tendency for \code{pred_central}: \code{NULL}
#'   (default) reuses the value recorded in the run manifest (or \code{"mean"},
#'   the default those runs were made under, for manifests that predate the
#'   field); otherwise a scalar or per-channel \code{c(cases=, deaths=)}
#'   override.
#' @param write Logical; write \code{predictions.parquet} (default TRUE).
#' @return The compiled long predictions data frame (invisibly if written).
#' @export
compile_rolling_cv_predictions <- function(dir_output,
                                           base_config = MOSAIC::config_default,
                                           models = NULL,
                                           n_reps_best_medoid = NULL,
                                           central_method = NULL,
                                           write = TRUE) {
     mpath <- file.path(dir_output, "manifest.json")
     if (!file.exists(mpath)) stop("manifest.json not found in ", dir_output)
     man <- jsonlite::read_json(mpath, simplifyVector = TRUE)
     iso <- unlist(man$spec$iso)
     anchor <- as.Date(man$spec$anchor_date)
     horizons <- as.numeric(unlist(man$spec$horizons_months))
     embargo_days <- as.integer(man$spec$embargo_weeks) * 7L
     # default model set / rep count from the manifest (fall back to the default set / 50)
     if (is.null(models))
          models <- unlist(man$spec$models) %||% c("ensemble", "ensemble_opt", "medoid")
     models <- .rcv_validate_models(models)
     if (is.null(n_reps_best_medoid))
          n_reps_best_medoid <- as.integer(man$spec$n_reps_best_medoid %||% 50L)
     # Reuse the run's recorded central tendency unless the caller overrides it.
     # A manifest without the field was written under the then-default mean
     # (deliberately not the current package default).
     if (is.null(central_method))
          central_method <- man$spec$central_method %||% "mean"
     central_method <- .mosaic_resolve_central_method(central_method)

     cfg_dates <- seq.Date(as.Date(base_config$date_start),
                           as.Date(base_config$date_stop), by = "day")
     loc <- MOSAIC::get_location_config(iso = iso, config = base_config)
     nloc <- length(loc$location_name)
     oc <- .rcv_as_matrix(loc$reported_cases,  nloc, length(cfg_dates))
     od <- .rcv_as_matrix(loc$reported_deaths, nloc, length(cfg_dates))

     runs <- man$runs
     tabs <- list()
     if (!is.data.frame(runs) || !nrow(runs)) {
          warning("manifest.json records no cutoff runs in ", dir_output, call. = FALSE)
          return(invisible(NULL))
     }
     for (r in seq_len(nrow(runs))) {
          if (!identical(runs$status[r], "success")) next
          run_dir <- file.path(dir_output, runs$dir[r])
          if (!file.exists(file.path(run_dir, "2_calibration", "ensemble_candidate.rds"))) next
          tabs[[length(tabs) + 1L]] <- .rcv_compile_all_models(
               run_dir = run_dir, run_id = runs$run_id[r],
               cutoff = as.Date(runs$cutoff_date[r]), anchor = anchor,
               embargo_days = embargo_days, horizons_months = horizons,
               obs_cases = oc, obs_deaths = od, obs_dates = cfg_dates,
               location_names = loc$location_name,
               models = models, n_reps = n_reps_best_medoid,
               central_method = central_method)
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
                                    location_names, model = "ensemble",
                                    central_method = c(cases = "median", deaths = "mean")) {
     central_method <- .mosaic_resolve_central_method(central_method)
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
               cm       <- central_method[[metric]]
               med_mat  <- if (metric == "cases") ensemble$cases_median else ensemble$deaths_median
               mean_mat <- if (metric == "cases") ensemble$cases_mean   else ensemble$deaths_mean
               med <- getrow(med_mat, i)
               # Fall back to median if an older ensemble lacks the *_mean field.
               mn  <- if (!is.null(mean_mat)) getrow(mean_mat, i) else med
               central <- if (cm == "mean") mn else med
               obs_src <- if (metric == "cases") obs_cases else obs_deaths
               observed <- if (!is.na(oi)) getrow(obs_src, oi)[obs_idx] else rep(NA_real_, n_t)
               df <- data.frame(
                    run_id = run_id, model = model, iso_code = locs[i],
                    anchor_date = as.character(anchor), cutoff_date = as.character(cutoff),
                    date = as.Date(edates), metric = metric,
                    segment = lab$segment, weeks_ahead = lab$weeks_ahead,
                    horizon_bucket = lab$horizon_bucket,
                    observed = as.numeric(observed),
                    observed_source = "config_reported",
                    # pred_central is the scored series (mean or median per channel);
                    # pred_median/pred_mean are both retained for cross-walk.
                    pred_central = as.numeric(central),
                    pred_mean    = as.numeric(mn),
                    pred_median  = as.numeric(med),
                    central_method = cm,
                    stringsAsFactors = FALSE)
               ci_c <- if (metric == "cases") ensemble$ci_bounds$cases else ensemble$ci_bounds$deaths
               for (p in seq_len(n_pair)) {
                    lo_q <- eq[p]; hi_q <- eq[length(eq) - p + 1L]
                    tag  <- sprintf("pi%g", round((hi_q - lo_q) * 100))
                    df[[paste0(tag, "_lo")]] <- as.numeric(getrow(ci_c[[p]]$lower, i))
                    df[[paste0(tag, "_hi")]] <- as.numeric(getrow(ci_c[[p]]$upper, i))
               }
               # The median of the draws the CI columns come from: the
               # observation-level predictive median when the ensemble drew
               # observation noise (v0.101.0), else the ensemble median. A
               # proper interval score pairs it with those intervals.
               pm_obs <- ensemble$predictive_median[[metric]]
               df$pred_median_obs <- if (is.null(pm_obs)) as.numeric(med)
                                     else as.numeric(getrow(pm_obs, i))
               out[[length(out) + 1L]] <- df
          }
     }
     do.call(rbind, out)
}

#' Validate rolling-CV model names (exact match; match.arg(several.ok = TRUE)
#' would silently drop an unknown name such as "opt")
#' @keywords internal
#' @noRd
.rcv_validate_models <- function(models) {
     choices <- c("ensemble", "ensemble_opt", "best", "medoid")
     models <- as.character(unlist(models))
     bad <- setdiff(models, choices)
     if (!length(models) || length(bad))
          stop("models must be drawn from ", paste(sprintf("'%s'", choices), collapse = ", "),
               if (length(bad)) paste0("; unknown: ", paste(sprintf("'%s'", bad), collapse = ", ")),
               call. = FALSE)
     unique(models)
}

#' Coerce a config reported_* field to an (n_loc x n_t) matrix
#' @keywords internal
#' @noRd
.rcv_as_matrix <- function(x, n_loc, n_t) {
     if (is.matrix(x)) return(x)
     matrix(x, nrow = n_loc, ncol = n_t, byrow = (n_loc == 1L))
}

#' Resolve and validate one cutoff's frozen psi CSV from a prefit cache.
#'
#' Hard-errors when the cutoff is absent from the manifest, when the recorded
#' \code{psi_<T>.csv} is missing on disk, when the run's modeling-spec hash
#' (date keys stripped, mirroring \code{prefit_rolling_cv_psi}) does not match the
#' \code{spec_hash} the cache recorded for that cutoff, or (when
#' \code{need_start}/\code{need_stop} are given) when the prediction window the
#' cutoff was fitted over does not cover \code{[need_start, need_stop]}. The
#' window is read from the cutoff's own entry, else from the manifest top level;
#' a cache that records neither is left to the coverage check in
#' \code{.rolling_cv_psi_matrix()}. Returns the CSV path.
#' @keywords internal
#' @noRd
.rcv_psi_cache_lookup <- function(psi_cache, manifest, cutoff, est_suitability_spec,
                                  need_start = NULL, need_stop = NULL) {
     T_chr <- as.character(as.Date(cutoff))
     cuts  <- manifest$cutoffs
     keys  <- vapply(cuts, function(e) as.character(e$cutoff), character(1))
     idx   <- match(T_chr, keys)
     if (is.na(idx))
          stop("psi_cache is missing cutoff ", T_chr,
               " \u2014 present cutoffs: ", paste(keys, collapse = ", "),
               ". Re-run prefit_rolling_cv_psi() for this cutoff.", call. = FALSE)
     entry <- cuts[[idx]]
     csv   <- file.path(psi_cache, entry$csv %||%
                        sprintf("psi_%s.csv", T_chr))
     if (!file.exists(csv))
          stop("psi_cache entry for ", T_chr, " points to a missing file: ", csv,
               call. = FALSE)
     # Recompute the spec hash with the SAME contract prefit used (strip date keys
     # first) and require an exact match -- a mismatch means the run's modeling spec
     # differs from what produced the frozen psi; that is an error, not a silent NA.
     run_spec  <- .rcv_strip_date_keys(est_suitability_spec)
     run_hash  <- .rcv_psi_spec_hash(cutoff, run_spec)
     cache_hash <- entry$spec_hash
     if (is.null(cache_hash) || !identical(run_hash, as.character(cache_hash)))
          stop("psi_cache spec_hash mismatch for cutoff ", T_chr,
               ": run est_suitability_spec hashes to ", run_hash,
               " but the cache recorded ", cache_hash %||% "<none>",
               ". The frozen psi was produced with a different modeling spec.",
               call. = FALSE)
     pw_start <- entry$pred_date_start %||% manifest$pred_date_start
     pw_stop  <- entry$pred_date_stop  %||% manifest$pred_date_stop
     if (!is.null(need_start) && !is.null(pw_start) &&
         as.Date(pw_start) > as.Date(need_start))
          stop("psi_cache entry for ", T_chr, " was predicted from ", pw_start,
               ", after the config start ", as.character(as.Date(need_start)),
               ". Re-run prefit_rolling_cv_psi() with an earlier pred_date_start.",
               call. = FALSE)
     if (!is.null(need_stop) && !is.null(pw_stop) &&
         as.Date(pw_stop) < as.Date(need_stop))
          stop("psi_cache entry for ", T_chr, " was predicted only to ", pw_stop,
               ", before the cutoff's last scored OOS date ",
               as.character(as.Date(need_stop)),
               ". Re-run prefit_rolling_cv_psi() with a later pred_date_stop.",
               call. = FALSE)
     csv
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

#' Compile every requested model type for one cutoff into one long table
#'
#' Reads the candidate ensemble (always), the optimizer-selected ensemble (when
#' present), and re-simulates the best / medoid configs, emitting one
#' \code{model}-tagged block per type and row-binding them.
#' @keywords internal
#' @noRd
.rcv_compile_all_models <- function(run_dir, run_id, cutoff, anchor, embargo_days,
                                    horizons_months, obs_cases, obs_deaths, obs_dates,
                                    location_names, models, n_reps,
                                    central_method = c(cases = "median", deaths = "mean")) {
     central_method <- .mosaic_resolve_central_method(central_method)
     cal      <- file.path(run_dir, "2_calibration")
     ens_path <- file.path(cal, "ensemble_candidate.rds")
     if (!file.exists(ens_path)) stop("ensemble_candidate.rds not found at ", ens_path)
     ens <- readRDS(ens_path)

     emit <- function(predobj, model) {
          if (is.null(predobj)) return(NULL)
          .rolling_cv_compile_run(
               ensemble = predobj, run_id = run_id, cutoff = cutoff, anchor = anchor,
               embargo_days = embargo_days, horizons_months = horizons_months,
               obs_cases = obs_cases, obs_deaths = obs_deaths, obs_dates = obs_dates,
               location_names = location_names, model = model,
               central_method = central_method)
     }

     parts <- list(emit(ens, "ensemble"))
     if ("ensemble_opt" %in% models) {
          # run_MOSAIC() writes ensemble_optimized.rds as a copy of the candidate
          # ensemble whenever the optimizer is off or selects nothing, so the
          # file alone does not mean an optimizer arm exists.
          opt_path <- file.path(cal, "ensemble_optimized.rds")
          if (file.exists(opt_path) && .rcv_optimizer_selected(run_dir)) {
               parts[[length(parts) + 1L]] <- emit(readRDS(opt_path), "ensemble_opt")
          } else {
               warning("models includes 'ensemble_opt' but the subset optimizer did not ",
                       "select a subset in ", run_dir, " (optimize_subset off or empty ",
                       "subset); skipping ensemble_opt rather than duplicating the ",
                       "candidate ensemble.", call. = FALSE)
          }
     }
     bm <- file.path(cal, "best_model")
     # "best" is no longer produced by run_MOSAIC() (only ensemble + medoid).
     # Still honored for back-compat when an older run dir carries config_best.json;
     # skip with a warning otherwise.
     if ("best" %in% models) {
          best_cfg <- file.path(bm, "config_best.json")
          if (file.exists(best_cfg)) {
               parts[[length(parts) + 1L]] <- emit(
                    .rcv_simulate_config(best_cfg, ens, n_reps), "best")
          } else {
               warning("models includes 'best' but config_best.json not found in ",
                       bm, " (best model is no longer produced); skipping.",
                       call. = FALSE)
          }
     }
     if ("medoid" %in% models) {
          medoid_cfg <- file.path(bm, "config_medoid.json")
          if (file.exists(medoid_cfg)) {
               parts[[length(parts) + 1L]] <- emit(
                    .rcv_simulate_config(medoid_cfg, ens, n_reps), "medoid")
          } else {
               warning("config_medoid.json not found in ", bm,
                       " (expected for pre-v0.39 run dirs); skipping medoid.",
                       call. = FALSE)
          }
     }

     parts <- Filter(Negate(is.null), parts)
     if (!length(parts)) return(NULL)
     out <- do.call(rbind, parts)
     # Attach the per-cutoff importance-weight ESS (metrics$ess_best$value from the
     # calibration convergence diagnostics) as a single numeric column carried on
     # EVERY row of this cutoff. NA_real_ when the file/key is absent. This is the
     # weight-ESS gating contract for the downstream scoring layer.
     out$ess <- .rcv_read_ess_best(run_dir)
     out
}

#' Read metrics$ess_best$value from a run's convergence diagnostics.
#'
#' Returns the importance-weight effective sample size for the calibration, or
#' \code{NA_real_} if the diagnostics file or the key is missing. Never errors --
#' a missing diagnostic is a benign NA on the row, not a fatal condition.
#' @keywords internal
#' @noRd
.rcv_read_ess_best <- function(run_dir) {
     diag_path <- file.path(run_dir, "2_calibration", "diagnostics",
                            "convergence_diagnostics.json")
     if (!file.exists(diag_path)) return(NA_real_)
     val <- tryCatch({
          d <- jsonlite::read_json(diag_path, simplifyVector = TRUE)
          d$metrics$ess_best$value
     }, error = function(e) NULL)
     if (is.null(val) || length(val) != 1L || !is.numeric(val)) return(NA_real_)
     as.numeric(val)
}

#' Reported-CFR estimates as of a cutoff
#'
#' Fits the WHO-annual GAM to years up to \code{last_year} only, with carry-forward
#' to the end of the simulation window.
#' @keywords internal
#' @noRd
.rcv_cfr_asof <- function(who_annual, last_year, cfg_stop) {
     fy <- max(0L, as.integer(format(as.Date(cfg_stop), "%Y")) - as.integer(last_year))
     est <- .cfr_estimate(who_annual, forecast_years = fy, forecast_method = "carry_forward",
                          last_year = last_year)
     if (est$last_data_year != last_year)
          warning(sprintf("WHO annual data end in %d, before the cutoff's last usable year %d.",
                          est$last_data_year, last_year), call. = FALSE)
     list(predictions = est$predictions, sigma = est$fit$sigma, tau = est$fit$tau,
          last_data_year = est$last_data_year)
}

#' Put an as-of reported CFR into a cutoff config
#'
#' Replaces \code{mu_jt} (and any legacy mortality fields) with the daily matrix
#' built from \code{.rcv_cfr_asof()} estimates.
#' @keywords internal
#' @noRd
.rcv_apply_cfr_asof <- function(cfg, asof, cfg_dates) {
     for (f in c(.MOSAIC_LEGACY_MORTALITY_FIELDS, "delta_reporting_deaths")) cfg[[f]] <- NULL
     cfg$mu_jt <- make_mu_jt(asof$predictions, location_name = cfg$location_name,
                             date_start = min(cfg_dates), date_stop = max(cfg_dates))
     cfg
}

#' Re-simulate a single config to a prediction object matching the ensemble shape
#'
#' Runs \code{n_reps} stochastic reruns of the saved config and reduces them
#' to a predictive median + interval bounds on the template ensemble's date grid,
#' so the result can be emitted by \code{.rolling_cv_compile_run}. Returns NULL if
#' the config is absent.
#' @keywords internal
#' @noRd
.rcv_simulate_config <- function(config_path, template, n_reps) {
     if (!file.exists(config_path)) return(NULL)
     cfg       <- .mosaic_read_json_cached(config_path)
     sim_dates <- seq.Date(as.Date(cfg$date_start), as.Date(cfg$date_stop), by = "day")
     edates    <- seq(as.Date(template$date_start), as.Date(template$date_stop),
                      length.out = template$n_time_points)
     col_idx   <- match(as.character(as.Date(edates)), as.character(sim_dates))
     if (anyNA(col_idx))
          stop("config window does not cover the ensemble date grid in ", config_path)
     locs  <- template$location_names
     nloc  <- length(locs); nt <- length(edates)
     eq    <- template$envelope_quantiles
     seeds <- seq_len(max(1L, as.integer(n_reps)))

     cas <- array(NA_real_, c(length(seeds), nloc, nt))
     dea <- array(NA_real_, c(length(seeds), nloc, nt))
     for (s in seq_along(seeds)) {
          r  <- MOSAIC::run_simulation(cfg, seed = seeds[s], quiet = TRUE)
          rc <- r$results$reported_cases
          rd <- r$results$reported_deaths
          if (!is.matrix(rc)) rc <- matrix(rc, nrow = 1L)
          if (!is.matrix(rd)) rd <- matrix(rd, nrow = 1L)
          cas[s, , ] <- rc[, col_idx, drop = FALSE]
          dea[s, , ] <- rd[, col_idx, drop = FALSE]
     }

     reduce <- function(arr) {
          med    <- apply(arr, c(2L, 3L), stats::median, na.rm = TRUE)
          mn     <- apply(arr, c(2L, 3L), mean, na.rm = TRUE)
          n_pair <- length(eq) / 2L
          ci <- vector("list", n_pair)
          for (p in seq_len(n_pair)) {
               lo_q <- eq[p]; hi_q <- eq[length(eq) - p + 1L]
               ci[[p]] <- list(
                    lower = apply(arr, c(2L, 3L), stats::quantile, probs = lo_q, na.rm = TRUE),
                    upper = apply(arr, c(2L, 3L), stats::quantile, probs = hi_q, na.rm = TRUE))
          }
          list(median = med, mean = mn, ci = ci)
     }
     qc <- reduce(cas); qd <- reduce(dea)
     list(
          n_time_points      = nt,
          date_start         = as.character(template$date_start),
          date_stop          = as.character(template$date_stop),
          location_names     = locs,
          envelope_quantiles = eq,
          cases_median       = qc$median,
          cases_mean         = qc$mean,
          deaths_median      = qd$median,
          deaths_mean        = qd$mean,
          ci_bounds          = list(cases = qc$ci, deaths = qd$ci))
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

#' Write JSON via a tempfile in the destination directory + rename, so readers
#' never see a half-written file
#' @keywords internal
#' @noRd
.rcv_write_json_atomic <- function(obj, path) {
     tmp <- tempfile(pattern = paste0(basename(path), "_"), tmpdir = dirname(path),
                     fileext = ".tmp")
     on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
     .rcv_write_json(obj, tmp)
     if (!file.rename(tmp, path)) stop("failed to atomically place ", path)
     invisible(path)
}

#' Whether run_MOSAIC's subset optimizer selected a subset in a run directory
#'
#' TRUE when \code{2_calibration/subset_opt.rds} exists (written only when the
#' optimizer selected a subset), or, because that save sits in a non-fatal
#' \code{tryCatch}, when \code{3_results/summary.json} records a finite
#' \code{n_ensemble_params_tier} (set only on the same branch).
#' @keywords internal
#' @noRd
.rcv_optimizer_selected <- function(run_dir) {
     if (file.exists(file.path(run_dir, "2_calibration", "subset_opt.rds"))) return(TRUE)
     sj <- file.path(run_dir, "3_results", "summary.json")
     if (!file.exists(sj)) return(FALSE)
     smry <- tryCatch(jsonlite::read_json(sj, simplifyVector = TRUE), error = function(e) NULL)
     n_tier <- suppressWarnings(as.numeric(smry$n_ensemble_params_tier))
     length(n_tier) == 1L && is.finite(n_tier) && n_tier > 0
}

#' Epidemic peaks usable at a cutoff
#'
#' Keeps peaks whose peak-shape scoring window (\code{half_window} days either
#' side of the peak, the window \code{calc_model_likelihood()} uses) ends on or
#' before the cutoff. Returns a 0-row \code{iso_code}/\code{peak_date} frame
#' when none remain: the likelihood falls back to the full package dataset when
#' the field is NULL, which would reintroduce every post-cutoff peak.
#' @keywords internal
#' @noRd
.rcv_asof_epidemic_peaks <- function(peaks, cutoff, half_window = 14L) {
     empty <- data.frame(iso_code = character(0), peak_date = character(0),
                         stringsAsFactors = FALSE)
     if (is.null(peaks) || !NROW(peaks)) return(empty)
     keep <- !is.na(as.Date(peaks$peak_date)) &
          as.Date(peaks$peak_date) + half_window <= as.Date(cutoff)
     out <- peaks[keep, , drop = FALSE]
     if (!nrow(out)) return(empty)
     rownames(out) <- NULL
     out
}

#' Warning text for psi that is not trained on a per-cutoff leak-free panel
#' @keywords internal
#' @noRd
.rcv_psi_leak_warning <- function(what) {
     paste0(what, ": the per-country target anchors and the flood-probability ",
            "GAM behind the psi features were fitted on the whole suitability ",
            "panel, including rows after each cutoff, so OOS skill is not strictly ",
            "leak-free. Build the cache with prefit_rolling_cv_psi(est_suitability_spec ",
            "= list(feature_set = \"v7.4\", ...)) for per-cutoff leak-free panels.")
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
          "| `predictions.parquet` | compiled long table: 1 row per cutoff x model x location x date x metric. `model` in {ensemble, ensemble_opt, best, medoid}. IS/embargo/OOS labeled; observed + predicted median + CIs |",
          "| `runs/cutoff_<T>/` | native run_MOSAIC output per cutoff |",
          "",
          "`predictions.parquet` is a derived view (rebuildable from `runs/`).",
          "Evaluation/baselines/skill scores are computed post-hoc from the predictions table.")
     writeLines(lines, file.path(dir_output, "README.md"))
}
