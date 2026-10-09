# =============================================================================
# ensemble_suitability.R — Multi-seed runner + logit-scale smoothing for the
# lstm_v2 suitability path. Ported from the MOSAIC-Mozambique sandbox
# (ensemble.R + smooth.R), merged, no file-scope source().
#
# Structure: the expensive per-seed FIT (keras) is separated from the cheap
# per-country daily SMOOTHING + cross-seed aggregation (pure R, in the parent).
# Seeds are independent, so the fit can run serially (default) or across
# thread-pinned PSOCK workers (arch_control$parallel_seeds > 1; see §7 of the
# plan). Reproducibility is at the POOLED-ensemble level, NOT per seed: the
# pooled psi (logit-median over seeds) is statistically equivalent across
# execution modes, but the PER-SEED draws are NOT bitwise-identical between
# serial and parallel (or between different worker/thread counts). Changing the
# TF thread count changes the float reduction order, which compounds the
# recurrent_dropout non-determinism — so two seeds with the "same" integer seed
# can diverge across modes. Pool >=10 seeds (D2) and rely on the aggregate.
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
#' Returns data.frame(date, pred \[forward-filled, un-smoothed\], pred_smooth).
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
# Workers run library(MOSAIC), i.e. the INSTALLED package, and a closure shipped
# to them resolves its namespace by name to that installed copy. So under
# devtools::load_all() the seeds would silently run the installed (possibly
# older) fitting code while the parent smooths/writes with the dev code. This
# function therefore refuses a load_all() namespace (returns NULL -> serial) and
# errors if a worker's MOSAIC version differs from the parent's (the caller
# catches that and falls back to serial). RAM (not cores) is the real cap — TF
# grabs memory per process — so workers are clamped to cores-2 and the user's
# value. A worker's fit error is returned (not discarded) so the parent can
# report which seeds failed and why.
#' @keywords internal
#' @noRd
.psi_fit_seeds_parallel <- function(seeds, parallel_seeds, fit_predict_fn,
                                    data_bundle, hyperparams, verbose = TRUE) {
     # Core pool for THIS process: a caller-supplied per-process budget
     # (MOSAIC_PSI_CORE_BUDGET) when set -- so parallel callers can hand each
     # process a slice -- else the whole box. Unset => unchanged legacy behavior.
     budget_env <- Sys.getenv("MOSAIC_PSI_CORE_BUDGET", "")
     nc <- suppressWarnings(as.integer(budget_env))
     if (is.na(nc) || nc < 1L) nc <- parallel::detectCores()
     if (is.na(nc) || nc < 1L) nc <- 2L   # detectCores() may return NA on some platforms
     # Cross-process oversubscription guard: when this is a parallel-seed run on a
     # big box and the per-process budget env is UNSET, every cell-process here
     # sizes its TF intra-op pool to the whole box (nc = detectCores()), so K
     # concurrent processes request K*nc threads -> loadavg >> cores. Warn so the
     # caller sets MOSAIC_PSI_CORE_BUDGET to a per-process slice (see plan §7).
     if (as.integer(parallel_seeds) > 1L && !nzchar(budget_env) &&
         isTRUE(parallel::detectCores() > 32L)) {
          warning("est_suitability parallel_seeds>1 with MOSAIC_PSI_CORE_BUDGET unset on a ",
                  parallel::detectCores(), "-core host: each cell-process will size TF ",
                  "intra-op to the whole box (cross-process thread oversubscription risk). ",
                  "Set MOSAIC_PSI_CORE_BUDGET to cores/(parallel_seeds x runs) per process.",
                  call. = FALSE)
     }
     n_workers <- max(1L, min(as.integer(parallel_seeds), length(seeds), nc - 2L))
     # Same connection clamp as every other PSOCK site: parallel_seeds is
     # normally small enough not to reach it, but an unclamped makeCluster()
     # here would throw rather than fit fewer seeds in parallel.
     n_workers <- .mosaic_clamp_psock_workers(n_workers, reserve = 2L,
                                              what = "seed-fit workers")
     if (n_workers <= 1L) return(NULL)   # nothing to gain; caller runs serial
     if (.psi_is_dev_namespace()) {
          warning("parallel_seeds > 1 ignored: MOSAIC is loaded with devtools::load_all(), and ",
                  "PSOCK workers would run the INSTALLED MOSAIC's fitting code instead of this ",
                  "session's. Fitting seeds serially. Install the package to fit seeds in parallel.",
                  call. = FALSE)
          return(NULL)
     }
     parent_version <- as.character(getNamespaceVersion("MOSAIC"))
     # Focus each worker's TF intra-op pool to its core slice so n_workers x
     # tf_intra ~ nc (saturate the box, no oversubscription). The fit
     # (.psi_fit_predict_lstm) reads MOSAIC_PSI_TF_INTRAOP and applies the cap.
     tf_intra <- max(1L, nc %/% n_workers)
     if (verbose) message(sprintf("  [ensemble] parallel seed fitting: %d PSOCK worker(s), %d TF intra-op threads each",
                                  n_workers, tf_intra))
     retpy <- Sys.getenv("RETICULATE_PYTHON")
     cl <- parallel::makeCluster(n_workers, type = "PSOCK")
     # See ?.mosaic_stop_cluster: a stalled seed fit would otherwise leave a
     # worker holding this process's stdout after stopCluster() returns.
     .worker_pids <- .mosaic_cluster_worker_pids(cl)
     on.exit(.mosaic_stop_cluster(cl, .worker_pids), add = TRUE)
     parallel::clusterExport(cl, c("retpy", "tf_intra"), envir = environment())
     parallel::clusterEvalQ(cl, {
          if (nzchar(retpy)) Sys.setenv(RETICULATE_PYTHON = retpy)
          suppressMessages(library(MOSAIC))
          # Canonical per-worker thread pin (all 7 vars incl ARROW + the BLAS
          # clamp), set BEFORE keras3/TF import so TF reads the pinned env.
          # Matches every other PSOCK worker in the package.
          MOSAIC:::.mosaic_set_blas_threads(1L)
          # Focus TF's intra-op pool to this worker's slice (BLAS pin above does
          # NOT govern TF's Eigen pool); read by .psi_fit_predict_lstm.
          # GUARD: no TF op may precede this threading cap. tf.config.threading
          # set_intra/inter_op (applied downstream from these env vars) are SILENT
          # no-ops once the TF runtime has initialised its thread pools at the
          # first op. We only set env vars here and defer library(keras3)/TF import
          # until after — so the cap is established before any op runs.
          Sys.setenv(MOSAIC_PSI_TF_INTRAOP = as.character(tf_intra),
                     MOSAIC_PSI_TF_INTEROP = "1")
          suppressMessages(library(keras3))
          NULL
     })
     worker_versions <- unlist(parallel::clusterEvalQ(
          cl, as.character(getNamespaceVersion("MOSAIC"))))
     if (any(worker_versions != parent_version))
          stop(sprintf("PSOCK workers loaded MOSAIC %s but this session runs %s; the seeds would be fit with different code.",
                       paste(unique(worker_versions), collapse = "/"), parent_version),
               call. = FALSE)
     parallel::clusterExport(cl, c("fit_predict_fn", "data_bundle", "hyperparams"),
                             envir = environment())
     # Detached from this frame so it does not ship a second copy of data_bundle
     # to every worker: the names it reads are the clusterExport()ed ones above.
     .seed_fun <- function(seed) {
          t0  <- proc.time()
          err <- NA_character_
          out <- tryCatch(
               fit_predict_fn(data_bundle = data_bundle, seed = seed,
                              hyperparams = hyperparams),
               error = function(e) {
                    err <<- conditionMessage(e)
                    NULL
               })
          list(seed = seed, out = out, error = err,
               elapsed = round((proc.time() - t0)["elapsed"] / 60, 2))
     }
     environment(.seed_fun) <- globalenv()
     parallel::parLapply(cl, seeds, .seed_fun)
}

#' TRUE when the MOSAIC namespace was created by devtools/pkgload::load_all()
#' (pkgload marks it with `.__DEVTOOLS__`, which is what pkgload::is_dev_package()
#' checks). PSOCK workers cannot see such a namespace.
#' @keywords internal
#' @noRd
.psi_is_dev_namespace <- function() {
     isNamespaceLoaded("MOSAIC") &&
          exists(".__DEVTOOLS__", envir = asNamespace("MOSAIC"), inherits = FALSE)
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
#' @return list(ensemble_long, by_country, seeds_by_country, fit_info
#'   \[one row per seed, with status and the error text of failed seeds\],
#'   rw_diagnostics, genuine_last_pred \[per-iso last covariate-supported weekly
#'   prediction date, pre-fill\], ensemble \[target headline\],
#'   seeds \[target headline\], seeds_ok, seeds_failed).
#'   A warning is raised whenever some (but not all) seeds fail.
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

     # Per-country LAST GENUINE (covariate-supported) weekly prediction date,
     # capped at pred_date_stop. Seed-independent. Mirrors the legacy path's
     # genuine_last_pred (R/est_suitability.R).
     genuine_last_pred <- data.frame(
          iso_code = isos_pred,
          last_genuine_date = as.Date(vapply(isos_pred, function(iso) {
               as.character(min(pred_end,
                                max(data_bundle$dates_pred[data_bundle$countries_pred == iso])))
          }, character(1))),
          stringsAsFactors = FALSE)

     # Per-country day grids: first predicted date -> last GENUINE prediction
     # date. The grid used to run to pred_date_stop, so the carry-forward fill
     # past a country's covariate coverage entered the LOESS fit (pulling the
     # retained end-of-series days toward the flat constant) and the per-country
     # amplitude reference of calibrate_psi_predictions() before the writer
     # dropped it. Ending the grid here keeps the fill out of both; the writer's
     # .drop_filled_prediction_tail() is then a no-op safety net.
     day_grids <- lapply(stats::setNames(isos_pred, isos_pred), function(iso) {
          idx   <- data_bundle$countries_pred == iso
          start <- min(data_bundle$dates_pred[idx])
          stop_ <- genuine_last_pred$last_genuine_date[genuine_last_pred$iso_code == iso]
          seq.Date(start, max(start, stop_), by = "day")
     })

     # ---- Phase A: fit every seed (serial or parallel) ---------------------
     fit_one_seed <- function(seed) {
          if (verbose) message(sprintf("\n--- fitting seed %d ---", seed))
          t0  <- proc.time()
          err <- NA_character_
          out <- tryCatch(
               fit_predict_fn(data_bundle = data_bundle, seed = seed,
                              hyperparams = hyperparams),
               error = function(e) {
                    err <<- conditionMessage(e)
                    message(sprintf("  seed %d FAILED: %s", seed, err))
                    NULL
               })
          list(seed = seed, out = out, error = err,
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
          # serial. The serial re-run is statistically equivalent at the POOLED
          # level (not bitwise-identical per seed: the thread count differs, so
          # the float reduction order — and hence the recurrent_dropout draws —
          # differ); reproducibility is the pooled psi, just produced more slowly.
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
                    error = as.character(sf$error %||% NA_character_),
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
               train_minutes = unname(out$train_minutes %||% elapsed),
               n_epochs      = out$n_epochs      %||% NA_integer_,
               loss_type     = out$loss_type     %||% NA_character_,
               status        = "ok",
               error         = NA_character_,
               stringsAsFactors = FALSE)
          if (verbose) {
               message(sprintf("  seed %d ok: val_loss=%.4f val_metric=%.4f epochs=%s",
                               seed, fit_rows[[i]]$val_loss, fit_rows[[i]]$val_metric,
                               ifelse(is.na(fit_rows[[i]]$n_epochs), "?", fit_rows[[i]]$n_epochs)))
          }
     }

     if (length(per_seed_daily_by_country) == 0L)
          stop(".psi_run_seed_ensemble: ALL seeds failed")

     # A partial failure silently shrinks the ensemble the manifest describes;
     # say so, with each failed seed's error.
     fit_info  <- do.call(rbind, fit_rows)
     rownames(fit_info) <- NULL
     failed    <- fit_info$status != "ok"
     seeds_ok     <- fit_info$seed[!failed]
     seeds_failed <- fit_info$seed[failed]
     if (length(seeds_failed)) {
          warning(sprintf(".psi_run_seed_ensemble: %d of %d seed(s) failed; psi is pooled over the %d that succeeded (%s). Failed: %s",
                          length(seeds_failed), length(seed_fits), length(seeds_ok),
                          paste(seeds_ok, collapse = ", "),
                          paste(sprintf("seed %s (%s)", seeds_failed,
                                        ifelse(is.na(fit_info$error[failed]), "no error captured",
                                               fit_info$error[failed])),
                                collapse = "; ")),
                  call. = FALSE)
     }

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

     list(
          ensemble          = ensembles_by_country[[target_iso]],
          seeds             = seeds_by_country[[target_iso]],
          by_country        = ensembles_by_country,
          seeds_by_country  = seeds_by_country,
          ensemble_long     = ensemble_long,
          fit_info          = fit_info,
          rw_diagnostics    = rw_diag_by_seed,
          genuine_last_pred = genuine_last_pred,
          seeds_ok          = seeds_ok,
          seeds_failed      = seeds_failed
     )
}
