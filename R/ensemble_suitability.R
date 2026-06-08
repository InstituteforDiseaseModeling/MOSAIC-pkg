# =============================================================================
# ensemble_suitability.R — Multi-seed runner + logit-scale smoothing for the
# lstm_v2 suitability path. Ported from the MOSAIC-Mozambique sandbox
# (ensemble.R + smooth.R), merged, no file-scope source().
#
# Structure: the expensive per-seed FIT (keras) is separated from the cheap
# per-country daily SMOOTHING + cross-seed aggregation (pure R, in the parent).
# Seeds are independent, so the fit can run serially (default) or across
# thread-pinned PSOCK workers (arch_control$parallel_seeds > 1; see §7 of the
# plan). Parallel and serial give identical per-seed results (each seed is its
# own seeded pipeline), so determinism is unaffected.
#
# Option-A output diagnostics emitted in the long table (per iso x date):
#   pred_raw    — cross-seed logit-MEDIAN of the daily forward-filled,
#                 UN-loess'd prediction (raw inverse-logit LSTM output -> days).
#   pred_smooth — cross-seed logit-MEDIAN of the LOESS-smoothed series
#                 (pre-bias-correction; the 0.5 seed quantile).
#   q025/q25/q75/q975 — seed-dispersion quantiles of the smoothed series.
#                 DIAGNOSTIC dispersion, NOT predictive intervals, and
#                 PRE-bias-correction (a different scale than the canonical psi).
#
# `%||%` is provided package-wide by R/aaa_utils.R.
# =============================================================================

#' LOESS smoothing on the logit scale, returning probability-scale predictions.
#' @keywords internal
#' @noRd
.psi_smooth_logit_loess <- function(dates, probs, span = 0.025,
                                    surface = "direct", degree = 2L) {
     if (length(dates) != length(probs))
          stop(".psi_smooth_logit_loess: dates and probs must be same length")
     eps <- 1e-6
     pr_b  <- pmax(eps, pmin(1 - eps, probs))
     logit <- stats::qlogis(pr_b)
     df    <- data.frame(t = as.numeric(dates), y = logit)
     df    <- df[!is.na(df$y), ]
     if (nrow(df) < 5) return(probs)
     fit <- tryCatch(
          stats::loess(y ~ t, data = df, span = span, degree = degree,
                       control = stats::loess.control(surface = surface)),
          error = function(e) NULL)
     if (is.null(fit)) return(probs)
     hat_logit <- suppressWarnings(
          stats::predict(fit, newdata = data.frame(t = as.numeric(dates))))
     # loess local quadratic fits can be non-finite on short/sparse series
     # (too few points in a span neighbourhood). Backfill those positions with
     # the clamped input logit so the smoothed series is finite everywhere.
     if (any(!is.finite(hat_logit))) {
          bad <- !is.finite(hat_logit)
          hat_logit[bad] <- stats::qlogis(pr_b)[bad]
     }
     stats::plogis(hat_logit)
}

#' Interpolate weekly predictions onto a daily grid (forward-fill then smooth).
#' Returns data.frame(date, pred [forward-filled, un-smoothed], pred_smooth).
#' @keywords internal
#' @noRd
.psi_weekly_to_daily_smooth <- function(weekly_dates, weekly_probs,
                                        day_start, day_stop, span = 0.025,
                                        surface = "direct", degree = 2L) {
     tmp <- data.frame(date = as.Date(weekly_dates), pred = weekly_probs)
     tmp <- tmp[order(tmp$date), ]
     grid <- data.frame(date = seq(as.Date(day_start), as.Date(day_stop), by = "day"))
     out <- merge(grid, tmp, by = "date", all.x = TRUE)
     out <- out[order(out$date), ]
     out$pred <- zoo::na.locf(out$pred, na.rm = FALSE)
     out$pred <- zoo::na.locf(out$pred, fromLast = TRUE, na.rm = FALSE)
     out$pred_smooth <- .psi_smooth_logit_loess(out$date, out$pred, span = span,
                                                surface = surface, degree = degree)
     out
}

# ---- Parallel per-seed fitting over thread-pinned PSOCK workers -------------
# One worker = one whole seed pipeline (its RW-CV fits + refit + predict), so the
# ~10 s TF import amortizes over the seed's K+1 fits (plan §7). Each worker pins
# the 6 BLAS/Numba thread vars and RETICULATE_PYTHON BEFORE loading keras.
# Requires an INSTALLED MOSAIC (workers library(MOSAIC)); under devtools the
# caller falls back to serial. RAM (not cores) is the real cap — TF grabs memory
# per process — so workers are clamped to cores-2 and the user's value.
#' @keywords internal
#' @noRd
.psi_fit_seeds_parallel <- function(seeds, parallel_seeds, fit_predict_fn,
                                    data_bundle, hyperparams, verbose = TRUE) {
     nc <- parallel::detectCores()
     if (is.na(nc) || nc < 1L) nc <- 2L   # detectCores() may return NA on some platforms
     n_workers <- max(1L, min(as.integer(parallel_seeds), length(seeds), nc - 2L))
     if (n_workers <= 1L) return(NULL)   # nothing to gain; caller runs serial
     if (verbose) message(sprintf("  [ensemble] parallel seed fitting: %d PSOCK worker(s)", n_workers))
     retpy <- Sys.getenv("RETICULATE_PYTHON")
     cl <- parallel::makeCluster(n_workers, type = "PSOCK")
     on.exit(parallel::stopCluster(cl), add = TRUE)
     parallel::clusterExport(cl, "retpy", envir = environment())
     parallel::clusterEvalQ(cl, {
          Sys.setenv(OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
                     OPENBLAS_NUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1",
                     TBB_NUM_THREADS = "1", NUMBA_NUM_THREADS = "1")
          if (nzchar(retpy)) Sys.setenv(RETICULATE_PYTHON = retpy)
          suppressMessages(library(MOSAIC))
          suppressMessages(library(keras3))
          NULL
     })
     parallel::clusterExport(cl, c("fit_predict_fn", "data_bundle", "hyperparams"),
                             envir = environment())
     parallel::parLapply(cl, seeds, function(seed) {
          t0 <- proc.time()
          out <- tryCatch(
               fit_predict_fn(data_bundle = data_bundle, seed = seed,
                              hyperparams = hyperparams),
               error = function(e) NULL)
          list(seed = seed, out = out,
               elapsed = round((proc.time() - t0)["elapsed"] / 60, 2))
     })
}

#' Multi-seed ensemble runner with per-country logit-scale aggregation.
#'
#' @param fit_predict_fn The arch fit_predict (here the RW-CV-wrapped gauge_A).
#' @param data_bundle From .psi_build_data().
#' @param seeds Integer seed vector (derived seq.int(seed_base,by=seed_step,...)).
#' @param hyperparams Passed to fit_predict_fn.
#' @param smooth_span LOESS span (default 0.025).
#' @param logit_eps Prediction clamp before logit smoothing/quantiles
#'   (default 0.01 — the ENSEMBLE eps, DISTINCT from the loss logit_eps 1e-6).
#' @param parallel_seeds Integer; >1 fits seeds across PSOCK workers (default 1L
#'   serial). Falls back to serial if the cluster cannot be set up.
#' @return list(ensemble_long, by_country, seeds_by_country, fit_info,
#'   rw_diagnostics, ensemble [target headline], seeds [target headline]).
#' @keywords internal
#' @noRd
.psi_run_seed_ensemble <- function(fit_predict_fn, data_bundle,
                                   seeds        = c(11L, 22L, 33L, 44L, 55L),
                                   hyperparams  = list(),
                                   smooth_span  = 0.025,
                                   logit_eps    = 0.01,
                                   loess_surface = "direct",
                                   loess_degree  = 2L,
                                   parallel_seeds = 1L,
                                   verbose      = TRUE) {

     if (length(data_bundle$dates_pred) == 0L)
          stop(".psi_run_seed_ensemble: data_bundle$dates_pred is empty")
     if (length(seeds) == 0L)
          stop(".psi_run_seed_ensemble: seeds vector is empty")

     pred_end   <- data_bundle$pred_date_stop
     isos_pred  <- sort(unique(data_bundle$countries_pred))
     target_iso <- data_bundle$target_iso

     # Per-country day grids (each country starts at its first predicted date).
     day_grids <- lapply(stats::setNames(isos_pred, isos_pred), function(iso) {
          idx   <- data_bundle$countries_pred == iso
          start <- min(data_bundle$dates_pred[idx])
          seq.Date(start, pred_end, by = "day")
     })

     # ---- Phase A: fit every seed (serial or parallel) ---------------------
     fit_one_seed <- function(seed) {
          if (verbose) message(sprintf("\n--- fitting seed %d ---", seed))
          t0  <- proc.time()
          out <- tryCatch(
               fit_predict_fn(data_bundle = data_bundle, seed = seed,
                              hyperparams = hyperparams),
               error = function(e) {
                    message(sprintf("  seed %d FAILED: %s", seed, conditionMessage(e)))
                    NULL
               })
          list(seed = seed, out = out,
               elapsed = round((proc.time() - t0)["elapsed"] / 60, 2))
     }

     seed_fits <- NULL
     if (as.integer(parallel_seeds) > 1L && length(seeds) > 1L) {
          seed_fits <- tryCatch(
               .psi_fit_seeds_parallel(seeds, parallel_seeds, fit_predict_fn,
                                       data_bundle, hyperparams, verbose = verbose),
               error = function(e) {
                    warning(sprintf(".psi_run_seed_ensemble: parallel seed fitting failed (%s); falling back to serial.",
                                    conditionMessage(e)), call. = FALSE)
                    NULL
               })
          # A broken worker environment (e.g. namespace/closure resolution under
          # an uninstalled package) yields all-NULL outs. Treat "no usable
          # results" or a short/mismatched return as a parallel failure and re-run
          # serial — identical per-seed results, just slower.
          if (!is.null(seed_fits)) {
               n_ok <- sum(vapply(seed_fits, function(x) !is.null(x$out), logical(1)))
               if (length(seed_fits) != length(seeds) || n_ok == 0L) {
                    warning(".psi_run_seed_ensemble: parallel seed fitting returned no usable results; falling back to serial.",
                            call. = FALSE)
                    seed_fits <- NULL
               }
          }
     }
     if (is.null(seed_fits)) seed_fits <- lapply(seeds, fit_one_seed)

     # ---- Phase B: per-country daily smoothing + bookkeeping (parent) ------
     per_seed_daily_by_country <- list()
     fit_rows        <- list()
     rw_diag_by_seed <- list()

     for (i in seq_along(seed_fits)) {
          sf      <- seed_fits[[i]]
          seed    <- sf$seed
          out     <- sf$out
          elapsed <- sf$elapsed

          if (is.null(out)) {
               fit_rows[[i]] <- data.frame(
                    seed = seed, val_loss = NA_real_, val_metric = NA_real_,
                    train_minutes = unname(elapsed), n_epochs = NA_integer_,
                    loss_type = NA_character_, status = "failed",
                    stringsAsFactors = FALSE)
               next
          }

          pred_prob <- out$pred %||% if (!is.null(out$pred_logit))
               stats::plogis(out$pred_logit) else
                    stop("fit_predict must return either 'pred' (probability) or 'pred_logit'")
          pred_prob <- pmax(logit_eps, pmin(1 - logit_eps, pred_prob))

          daily_by_country <- list()
          for (iso in isos_pred) {
               idx <- data_bundle$countries_pred == iso
               wk_dates <- data_bundle$dates_pred[idx]
               wk_pred  <- pred_prob[idx]
               ord      <- order(wk_dates)
               wk_dates <- wk_dates[ord]; wk_pred <- wk_pred[ord]

               grid <- day_grids[[iso]]
               daily <- .psi_weekly_to_daily_smooth(
                    weekly_dates = wk_dates, weekly_probs = wk_pred,
                    day_start = min(grid), day_stop = max(grid),
                    span = smooth_span, surface = loess_surface, degree = loess_degree)
               daily$pred_smooth_logit <- stats::qlogis(
                    pmax(logit_eps, pmin(1 - logit_eps, daily$pred_smooth)))
               daily$pred_logit <- stats::qlogis(
                    pmax(logit_eps, pmin(1 - logit_eps, daily$pred)))
               daily$seed <- seed
               daily$iso  <- iso
               daily_by_country[[iso]] <- daily
          }
          per_seed_daily_by_country[[as.character(seed)]] <- daily_by_country
          if (!is.null(out$rw_diagnostics))
               rw_diag_by_seed[[as.character(seed)]] <- out$rw_diagnostics

          fit_rows[[i]] <- data.frame(
               seed          = seed,
               val_loss      = out$val_loss      %||% NA_real_,
               val_metric    = out$val_metric    %||% out$val_mae %||% NA_real_,
               train_minutes = out$train_minutes %||% elapsed,
               n_epochs      = out$n_epochs      %||% NA_integer_,
               loss_type     = out$loss_type     %||% NA_character_,
               status        = "ok",
               stringsAsFactors = FALSE)
          if (verbose) {
               message(sprintf("  seed %d ok: val_loss=%.4f val_metric=%.4f epochs=%s",
                               seed, fit_rows[[i]]$val_loss, fit_rows[[i]]$val_metric,
                               ifelse(is.na(fit_rows[[i]]$n_epochs), "?", fit_rows[[i]]$n_epochs)))
          }
     }

     if (length(per_seed_daily_by_country) == 0L)
          stop(".psi_run_seed_ensemble: ALL seeds failed")

     # ---- Per-country cross-seed aggregation (on the LOGIT scale) ----------
     ensembles_by_country <- list()
     seeds_by_country     <- list()
     for (iso in isos_pred) {
          seed_dfs <- lapply(names(per_seed_daily_by_country), function(s) {
               per_seed_daily_by_country[[s]][[iso]]
          })
          seeds_df <- do.call(rbind, seed_dfs)
          rownames(seeds_df) <- NULL
          seeds_by_country[[iso]] <- seeds_df

          dates_u <- sort(unique(seeds_df$date))
          q_logit <- vapply(dates_u, function(d) {
               v <- seeds_df$pred_smooth_logit[seeds_df$date == d]
               stats::quantile(v, probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                               na.rm = TRUE, names = FALSE)
          }, numeric(5))
          q_logit <- t(q_logit)
          q_prob  <- stats::plogis(q_logit)
          raw_logit_med <- vapply(dates_u, function(d) {
               stats::median(seeds_df$pred_logit[seeds_df$date == d], na.rm = TRUE)
          }, numeric(1))
          pred_raw <- stats::plogis(raw_logit_med)

          ens <- data.frame(
               date         = dates_u,
               pred_raw     = pred_raw,
               pred_smooth  = q_prob[, 3],
               q025         = q_prob[, 1], q25  = q_prob[, 2],
               q75          = q_prob[, 4], q975 = q_prob[, 5],
               median_logit = q_logit[, 3])

          obs_iso <- data_bundle$obs_all[
               data_bundle$obs_all$iso_code == iso,
               c("date", "cases", "intensity"), drop = FALSE]
          obs_iso <- unique(obs_iso)   # defensive: one row per (iso, date) before the merge
          ens <- merge(ens, obs_iso, by = "date", all.x = TRUE)
          ens <- ens[order(ens$date), ]
          ens$iso <- iso
          ensembles_by_country[[iso]] <- ens
     }

     ensemble_long <- do.call(rbind, lapply(ensembles_by_country, function(df) {
          df[, c("iso", "date", "pred_raw", "pred_smooth",
                 "q025", "q25", "q75", "q975", "cases", "intensity")]
     }))
     rownames(ensemble_long) <- NULL

     fit_info <- do.call(rbind, fit_rows)

     list(
          ensemble          = ensembles_by_country[[target_iso]],
          seeds             = seeds_by_country[[target_iso]],
          by_country        = ensembles_by_country,
          seeds_by_country  = seeds_by_country,
          ensemble_long     = ensemble_long,
          fit_info          = fit_info,
          rw_diagnostics    = rw_diag_by_seed
     )
}
