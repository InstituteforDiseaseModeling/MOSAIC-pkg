# =============================================================================
# rolling_cv_suitability.R — Expanding-window rolling-origin cross-validation
# for the lstm_v2 ("gauge_A") suitability path. Ported from the MOSAIC-Mozambique
# sandbox (rolling_cv.R), with two corrections vs the sandbox:
#
#   (1) NO in-function source()/here::here(): slice_* call .psi_build_sequences
#       via the package namespace (the sandbox had a CIRCULAR io.R <-> rolling_cv.R
#       source() pair).
#   (2) The all-steps-failed epoch fallback is FLOORED at min(50, epochs) instead
#       of resolving to the fixture's epochs=200 — a 200-epoch refit with NO early
#       stopping would over-train and blow up amplitude (plan §8 gate 6b, round-3).
#
# Design:
#   - IS data = everything strictly before cutoff_date; OOS = cutoff..forecast_end
#     (never touched during fitting).
#   - Inside IS: start at midpoint = fit_date_start + (cutoff - fit_date_start)/2;
#     at step k, train_end = midpoint + k*step_months, test_start = train_end +
#     gap_weeks (4-week embargo), test_end = min(test_start + test_months, cutoff).
#   - Refit on FULL IS data at round(median(best_epoch)) with no early stopping.
#
# `%||%` is provided package-wide by R/aaa_utils.R.
# =============================================================================

#' Generate the expanding-window RW step grid.
#' @keywords internal
#' @noRd
.psi_make_rw_cv_steps <- function(fit_date_start, cutoff_date,
                                  step_months   = 1L,
                                  test_months   = 5L,
                                  gap_weeks     = 4L,
                                  subsample     = 1L,
                                  timesteps     = 13L,
                                  min_test_days = NULL) {
     # Test window must hold >= 1 buildable sequence per country: with weekly
     # data and `timesteps` weeks/sequence, need >= timesteps*7 + ~7 days slack.
     if (is.null(min_test_days)) {
          min_test_days <- as.integer(timesteps * 7L + 7L)
     }
     fit_date_start <- as.Date(fit_date_start)
     cutoff_date    <- as.Date(cutoff_date)
     midpoint       <- fit_date_start +
          as.numeric(cutoff_date - fit_date_start) / 2

     train_ends <- seq.Date(midpoint, cutoff_date,
                            by = sprintf("%d months", step_months))
     # Last train_end must allow at least one valid test day before cutoff.
     train_ends <- train_ends[train_ends + 7L * gap_weeks < cutoff_date]

     steps <- lapply(seq_along(train_ends), function(k) {
          train_end  <- train_ends[k]
          test_start <- train_end + 7L * gap_weeks
          raw_end <- seq.Date(test_start, by = sprintf("%d months", test_months),
                              length.out = 2L)[2L] - 1L
          test_end <- min(raw_end, cutoff_date - 1L)
          if (test_start >= cutoff_date) return(NULL)
          if (as.integer(test_end - test_start) < min_test_days) return(NULL)
          list(step       = k,
               train_end  = train_end,
               test_start = test_start,
               test_end   = test_end)
     })
     steps <- Filter(Negate(is.null), steps)

     if (subsample > 1L && length(steps) > 0L) {
          idx   <- seq.int(1L, length(steps), by = subsample)
          steps <- steps[idx]
          for (i in seq_along(steps)) steps[[i]]$step <- i
     }
     steps
}

#' Slice the pool data for one RW step (returns train + val sequences).
#' @keywords internal
#' @noRd
.psi_slice_rw_step <- function(data_bundle, step) {
     pd  <- data_bundle$pool_data
     sp  <- data_bundle$seq_params
     enc <- data_bundle$encoders
     use_cw <- isTRUE(data_bundle$use_confidence_weight)

     is_train <- pd$dates <= step$train_end & !is.na(pd$intensity)
     if (sum(is_train) < sp$timesteps * 2L)
          stop(sprintf("RW step %d: too few training rows (%d)",
                       step$step, sum(is_train)))
     seqs_tr <- .psi_build_sequences(
          X         = pd$X[is_train, , drop = FALSE],
          y         = pd$intensity[is_train],
          countries = pd$countries[is_train],
          dates     = pd$dates[is_train],
          timesteps = sp$timesteps,
          max_gap_days = sp$max_gap_days,
          country_id_lookup  = enc$country_to_id,
          region_for_country = enc$region_for_country,
          cw        = if (use_cw) pd$cw[is_train] else NULL)

     is_val <- pd$dates >= step$test_start & pd$dates <= step$test_end &
          !is.na(pd$intensity)
     if (sum(is_val) < sp$timesteps) {
          # Allow training but skip validation (reports NA val_loss).
          return(list(X_train = seqs_tr$X, y_train = seqs_tr$y,
                      country_ids_train = seqs_tr$country_ids,
                      region_ids_train  = seqs_tr$region_ids,
                      confidence_weight_train = seqs_tr$cw,
                      X_val = NULL, y_val = NULL,
                      country_ids_val = NULL, region_ids_val = NULL,
                      confidence_weight_val = NULL,
                      n_train = length(seqs_tr$y), n_val = 0L))
     }
     seqs_val <- .psi_build_sequences(
          X         = pd$X[is_val, , drop = FALSE],
          y         = pd$intensity[is_val],
          countries = pd$countries[is_val],
          dates     = pd$dates[is_val],
          timesteps = sp$timesteps,
          max_gap_days = sp$max_gap_days,
          country_id_lookup  = enc$country_to_id,
          region_for_country = enc$region_for_country,
          cw        = if (use_cw) pd$cw[is_val] else NULL)

     list(X_train = seqs_tr$X, y_train = seqs_tr$y,
          country_ids_train = seqs_tr$country_ids,
          region_ids_train  = seqs_tr$region_ids,
          confidence_weight_train = seqs_tr$cw,
          X_val   = seqs_val$X, y_val = seqs_val$y,
          country_ids_val = seqs_val$country_ids,
          region_ids_val  = seqs_val$region_ids,
          confidence_weight_val = seqs_val$cw,
          n_train = length(seqs_tr$y), n_val = length(seqs_val$y))
}

#' Slice the pool data to FULL IS (everything strictly before cutoff_date), no
#' validation set. Used for the final refit.
#' @keywords internal
#' @noRd
.psi_slice_full_is <- function(data_bundle) {
     pd  <- data_bundle$pool_data
     sp  <- data_bundle$seq_params
     enc <- data_bundle$encoders
     use_cw <- isTRUE(data_bundle$use_confidence_weight)

     is_train <- pd$dates < data_bundle$cutoff_date & !is.na(pd$intensity)
     seqs_tr <- .psi_build_sequences(
          X         = pd$X[is_train, , drop = FALSE],
          y         = pd$intensity[is_train],
          countries = pd$countries[is_train],
          dates     = pd$dates[is_train],
          timesteps = sp$timesteps,
          max_gap_days = sp$max_gap_days,
          country_id_lookup  = enc$country_to_id,
          region_for_country = enc$region_for_country,
          cw        = if (use_cw) pd$cw[is_train] else NULL)
     list(X_train = seqs_tr$X, y_train = seqs_tr$y,
          country_ids_train = seqs_tr$country_ids,
          region_ids_train  = seqs_tr$region_ids,
          confidence_weight_train = seqs_tr$cw,
          X_val = NULL, y_val = NULL,
          country_ids_val = NULL, region_ids_val = NULL,
          confidence_weight_val = NULL,
          n_train = length(seqs_tr$y), n_val = 0L)
}

#' RW-CV wrapper around an architecture's fit_predict function.
#'
#' Iterates RW steps, fits the arch on each, records best_epoch; then refits once
#' on full IS data at round(median(best_epoch)) with no early stopping and
#' forecasts over data_bundle$X_pred. Return value matches the arch fit_predict
#' contract plus $rw_diagnostics.
#' @keywords internal
#' @noRd
.psi_fit_predict_rw_cv <- function(data_bundle, fit_predict_fn,
                                   seed = 11L, hyperparams = list(),
                                   verbose = TRUE) {
     rw_steps <- data_bundle$rw_steps
     if (is.null(rw_steps) || length(rw_steps) == 0L)
          stop(".psi_fit_predict_rw_cv: data_bundle$rw_steps is empty")

     if (verbose) message(sprintf("  [RW CV] %d steps", length(rw_steps)))

     n_steps      <- length(rw_steps)
     best_epochs  <- integer(n_steps)
     val_losses   <- numeric(n_steps)
     val_metrics  <- numeric(n_steps)
     step_minutes <- numeric(n_steps)

     for (k in seq_along(rw_steps)) {
          step <- rw_steps[[k]]
          if (verbose)
               message(sprintf("    step %2d/%d  train_end=%s  test=[%s, %s]",
                               step$step, n_steps, step$train_end,
                               step$test_start, step$test_end))

          sub <- .psi_slice_rw_step(data_bundle, step)

          # Skip folds with no held-out validation slice (e.g. a terminal RW
          # step whose test window extends past observed data, or a country
          # data gap that leaves the response NA across the window). The arch
          # fit uses early stopping in CV mode (n_epochs_fixed = NULL), which
          # requires validation; without it the fit errors ("validation data
          # required when n_epochs_fixed is NULL"). Such a fold cannot inform
          # best_epoch anyway, so record NA and skip rather than crash. (No-op
          # for folds that have validation, so targets with full coverage are
          # unaffected; only sparse targets/windows trigger this.)
          if (is.null(sub$X_val) || isTRUE(sub$n_val == 0L)) {
               best_epochs[k]  <- NA_integer_
               val_losses[k]   <- NA_real_
               val_metrics[k]  <- NA_real_
               step_minutes[k] <- 0
               if (verbose)
                    message("       (no validation slice; fold skipped for epoch selection)")
               next
          }

          # Sub-bundle that masquerades as a full data_bundle for the arch.
          sub_bundle <- utils::modifyList(data_bundle, list(
               X_train = sub$X_train, y_train = sub$y_train,
               country_ids_train = sub$country_ids_train,
               region_ids_train  = sub$region_ids_train,
               confidence_weight_train = sub$confidence_weight_train,
               X_val   = sub$X_val,   y_val   = sub$y_val,
               country_ids_val = sub$country_ids_val,
               region_ids_val  = sub$region_ids_val,
               confidence_weight_val = sub$confidence_weight_val))

          t0 <- proc.time()
          out <- fit_predict_fn(data_bundle = sub_bundle,
                                seed        = seed,
                                hyperparams = hyperparams)
          step_minutes[k] <- round((proc.time() - t0)["elapsed"] / 60, 2)

          best_epochs[k] <- as.integer(out$n_epochs %||% NA_integer_)
          val_losses[k]  <- as.numeric(out$val_loss   %||% NA_real_)
          val_metrics[k] <- as.numeric(out$val_metric %||% NA_real_)

          if (verbose)
               message(sprintf("       val_loss=%.4f  best_epoch=%s  elapsed=%.2f min",
                               val_losses[k],
                               ifelse(is.na(best_epochs[k]), "?", best_epochs[k]),
                               step_minutes[k]))
     }

     # Aggregate: how many epochs for the final fit?
     good <- !is.na(best_epochs) & best_epochs > 0L
     if (!any(good)) {
          # CORRECTION vs sandbox (plan §8 gate 6b): cap the all-steps-failed
          # fallback at min(50, epochs). The sandbox used `epochs %||% 50` which
          # resolves to the fixture's epochs=200 -> a 200-epoch refit with NO
          # early stopping (amplitude blow-up). 50 is the documented sane floor.
          n_epochs_final <- as.integer(min(50L, as.integer(hyperparams$epochs %||% 50L)))
          warning(sprintf(".psi_fit_predict_rw_cv: no valid best_epoch from any of %d RW steps; ",
                          n_steps),
                  sprintf("falling back to %d epochs (floored at 50 to avoid an over-trained refit).",
                          n_epochs_final), call. = FALSE)
          if (verbose)
               message(sprintf("  [RW CV] no valid best_epoch; defaulting to %d epochs (floored)",
                               n_epochs_final))
     } else {
          n_epochs_final <- as.integer(round(stats::median(best_epochs[good])))
          if (verbose)
               message(sprintf("  [RW CV] best_epochs: min=%d med=%d max=%d -> final fit at %d epochs",
                               min(best_epochs[good]), n_epochs_final,
                               max(best_epochs[good]), n_epochs_final))
     }

     # ---- Final fit on full IS data, fixed epoch count ---------------------
     if (verbose) message("  [RW CV] final fit on full IS data")
     full <- .psi_slice_full_is(data_bundle)
     final_bundle <- utils::modifyList(data_bundle, list(
          X_train = full$X_train, y_train = full$y_train,
          country_ids_train = full$country_ids_train,
          region_ids_train  = full$region_ids_train,
          confidence_weight_train = full$confidence_weight_train,
          X_val   = NULL,         y_val   = NULL,
          country_ids_val = NULL, region_ids_val = NULL,
          confidence_weight_val = NULL))
     hp_final <- utils::modifyList(hyperparams, list(n_epochs_fixed = n_epochs_final))

     t0 <- proc.time()
     final_out <- fit_predict_fn(data_bundle = final_bundle,
                                 seed        = seed,
                                 hyperparams = hp_final)
     final_minutes <- round((proc.time() - t0)["elapsed"] / 60, 2)

     list(
          pred           = final_out$pred,
          val_loss       = mean(val_losses,  na.rm = TRUE),
          val_metric     = mean(val_metrics, na.rm = TRUE),
          train_minutes  = sum(step_minutes) + final_minutes,
          n_epochs       = n_epochs_final,
          loss_type      = final_out$loss_type,
          rw_diagnostics = list(
               n_rw_steps     = n_steps,
               best_epochs    = best_epochs,
               val_losses     = val_losses,
               val_metrics    = val_metrics,
               step_minutes   = step_minutes,
               final_minutes  = final_minutes,
               n_epochs_final = n_epochs_final,
               all_steps_failed = !any(good))
     )
}
