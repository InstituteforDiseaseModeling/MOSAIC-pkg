#' Post-hoc evaluation of a rolling-window forecast-validation artifact
#'
#' Reads a \code{\link{run_rolling_cv}} predictions artifact and computes
#' out-of-sample forecast skill: per-cell metrics (R2 correlation & SSE, bias
#' ratio, 50/95% PI coverage, WIS) for the in-sample fit and for cumulative OOS
#' horizons, plus MAE-skill and WIS-skill versus seasonal-climatology and
#' persistence baselines, aggregated across cutoffs and countries with bootstrap
#' CIs (suppressed at small cell counts).
#'
#' @details
#' This is the post-hoc companion to \code{run_rolling_cv()} — the harness emits
#' predictions only; all judgement (metrics, baselines, skill) lives here.
#'
#' Horizons are \strong{cumulative} (\eqn{\le h} months): the \eqn{\le h} window
#' is every scored OOS date \eqn{d} with \code{origin < d <= origin +
#' ceiling(h * 30.4375)} days, where \code{origin} is the per-metric OOS-scoring
#' origin below. The origin is fixed by the cutoff and embargo, never by where
#' observations happen to exist, so a reporting gap just after the cutoff shrinks
#' a window's \code{n} instead of shifting its lead times (windows with fewer
#' than 3 scorable dates are dropped). The \eqn{\le}max-horizon window is the
#' largest scored set; predictions further out are not scored in any window. An
#' \code{IS} window (training fit, \eqn{\le} cutoff) is always reported.
#'
#' Scoring uses \strong{trusted observed only}: rows whose \code{observed_source}
#' is missing or not \code{"AI"} (when \code{trusted_only = TRUE}).
#'
#' \strong{Surveillance tier.} When the predictions carry \code{observed_tier}
#' (the config's \code{reported_tier}: 1 observed, 2 reconstructed, 3 imputed),
#' out-of-sample days whose tier is not in \code{observed_tiers} are not scored.
#' The in-sample rows, which the baselines are fitted on, keep every tier, as
#' the calibration did. Predictions without the column are scored unchanged.
#'
#' \strong{Embargo and scoring origin.} The harness labels the dates in
#' \code{(cutoff, cutoff + embargo]} as \code{segment == "embargo"}; those are
#' never scored. For each metric the scoring origin is
#' \code{max(last embargo-labelled date (or the cutoff), cutoff_date +
#' embargo_weeks[metric] * 7)} and the scored dates are those strictly after it.
#' \code{embargo_weeks} can therefore only lengthen the harness embargo, per
#' channel: it may be a scalar (all metrics) or a named vector such as
#' \code{c(cases = 4, deaths = 6)}; metrics it omits get 0. With the default
#' (\code{0}) exactly the \code{segment == "OOS"} block is scored, from the
#' harness's own OOS origin.
#'
#' \strong{Baselines}, fit per (cutoff, country, metric) on the IS observed
#' history. \code{"seasonal"} (week-of-year climatology) is the \strong{primary}
#' baseline and \strong{requires \eqn{\ge 2} years of in-sample history}; with
#' less, the seasonal baseline (and its skill) is \code{NA} (no grand-mean
#' fallback). \code{"persistence"} (trailing 4-point mean carried forward) and
#' \code{"persistence_last"} (the single last observed value) are secondary
#' nulls. Each baseline is also given 50/95% predictive intervals from empirical
#' quantiles of its in-sample residuals, so a genuine \strong{WIS-skill} exists:
#' \eqn{1 - WIS_{model}/WIS_{baseline}}. \code{mae_skill} is the analogous
#' MAE ratio. Positive = model beats the baseline; \code{NA} when the baseline
#' (or its interval) cannot be formed.
#'
#' \strong{Convergence gate (opt-in).} With \code{ess_min = 0} (the default)
#' there is no gate: every cell has \code{ess_ok = TRUE} and enters
#' \code{$summary}. With \code{ess_min > 0} the gate is on: a cell whose
#' predictions carry a \code{convergence_status} (MOSAIC 1.4.0: the cutoff
#' calibration's simulation-convergence status, constant per cutoff) passes when
#' the status is not \code{"red"}; a cell without one (a cutoff run from before
#' 1.4.0) falls back to the weight-ESS rule \code{ess_ok = (!is.na(ess) & ess >=
#' ess_min)}; a cell with neither passes. Cells failing the gate remain in
#' \code{$cells} (flagged) but are \strong{excluded} from the \code{$summary}
#' aggregation. \code{$cells$convergence_status} is carried as information
#' whatever \code{ess_min} is. A warning is issued when more than half of the
#' cells that carry a status are red (cheap or short cutoff calibrations), and
#' when the gate removes every cell.
#'
#' @param predictions A \code{run_rolling_cv} output directory, a path to a
#'   \code{predictions.parquet}, or the predictions data frame itself.
#' @param horizons_months Cumulative OOS horizons in months (default \code{c(1,3,5)}).
#' @param baselines Baselines for skill scores (default
#'   \code{c("seasonal","persistence","persistence_last")}).
#' @param metrics Metrics to evaluate (default \code{c("cases","deaths")}).
#' @param trusted_only Drop rows with \code{observed_source == "AI"} (default TRUE).
#' @param embargo_weeks Channel-specific OOS embargo, in weeks, after the cutoff
#'   before scoring begins, applied on top of the harness embargo (see Details).
#'   A scalar applies to all metrics; a named vector (e.g.
#'   \code{c(cases = 4, deaths = 6)}) applies per metric, and omitted metrics get
#'   0. Default \code{0}.
#' @param ess_min Turns the convergence gate on when > 0: cells whose cutoff
#'   run is red (or, before MOSAIC 1.4.0, whose ESS is below \code{ess_min}) are
#'   left out of the summary. Default \code{0} (no gate). See Details.
#' @param min_cells_ci Minimum number of finite cells in a (model, metric,
#'   window) group required to emit a bootstrap CI; below this the CI is
#'   suppressed (lo = hi = NA) and \code{ci_suppressed = TRUE}. Default \code{5}.
#' @param n_boot Bootstrap resamples over cells for the skill CI (default 1000).
#' @param seed RNG seed for the bootstrap (default 1).
#' @param observed_tiers Surveillance tiers scored out of sample when the
#'   predictions carry \code{observed_tier} (default \code{c(1, 2)}: observed and
#'   reconstructed weeks); \code{NULL} scores every tier. See Details.
#'
#' When the predictions carry a \code{model} column (\code{ensemble},
#' \code{ensemble_opt}, \code{best}, \code{medoid}), every metric is computed and
#' reported \strong{per model}; absent that column all rows are treated as a
#' single \code{"ensemble"} model (back-compatible).
#'
#' @return A list with:
#' \describe{
#'   \item{cells}{Per (cutoff x model x iso x metric x window) metrics + per-baseline
#'     mae_skill and wis_skill, the cell \code{ess}, \code{convergence_status} and \code{ess_ok} flag (plus
#'     \code{anchor_date} when the predictions carry it).}
#'   \item{summary}{Aggregated across \code{ess_ok} cells per (model x metric x
#'     window): \code{n_cells_used} / \code{n_cells_total}, mean/median of each
#'     metric, and mean baseline skills with bootstrap CI (suppressed when fewer
#'     than \code{min_cells_ci} finite cells), plus \code{ess_gated} and
#'     \code{ci_suppressed} flags.}
#' }
#' @seealso \code{\link{run_rolling_cv}}, \code{\link{calc_model_R2}}, \code{\link{calc_bias_ratio}}
#' @importFrom stats median quantile
#' @export
evaluate_rolling_cv <- function(predictions,
                                horizons_months = c(1, 3, 5),
                                baselines       = c("seasonal", "persistence", "persistence_last"),
                                metrics         = c("cases", "deaths"),
                                trusted_only    = TRUE,
                                embargo_weeks   = 0,
                                ess_min         = 0,
                                min_cells_ci    = 5L,
                                n_boot          = 1000L,
                                seed            = 1L,
                                observed_tiers  = c(1, 2)) {

     d <- .rcv_eval_input(predictions)
     req <- c("iso_code", "cutoff_date", "date", "metric", "segment",
              "observed", "pred_median")
     miss <- setdiff(req, names(d))
     if (length(miss)) stop("predictions missing column(s): ", paste(miss, collapse = ", "))
     # pred_central is the scored point series (mean or median per channel);
     # default to the median for back-compat with pre-central_method parquets.
     if (!"pred_central" %in% names(d)) d$pred_central <- d$pred_median
     d$date        <- as.Date(d$date)
     d$cutoff_date <- as.Date(d$cutoff_date)
     if (!"run_id" %in% names(d)) d$run_id <- paste0("cutoff_", d$cutoff_date)
     if (!"model"  %in% names(d)) d$model  <- "ensemble"   # back-compat: single series
     # Harness embargo end per cell, taken BEFORE the trusted_only filter: that
     # filter drops whole rows, and if every embargo date in a cell were
     # AI-sourced the origin would otherwise fall back to the cutoff.
     emb_rows <- d$segment == "embargo"
     emb_key  <- paste(d$run_id, d$model, d$iso_code, d$metric, sep = "\r")
     emb_end  <- if (any(emb_rows)) tapply(d$date[emb_rows], emb_key[emb_rows], max) else numeric(0)
     if (trusted_only && "observed_source" %in% names(d))
          d <- d[is.na(d$observed_source) | d$observed_source != "AI", ]
     if (!is.null(observed_tiers) && "observed_tier" %in% names(d)) {
          untrusted <- d$segment != "IS" & !(d$observed_tier %in% observed_tiers)
          d$observed[untrusted] <- NA_real_
     }
     horizons_months <- sort(unique(as.numeric(horizons_months)))
     baselines <- match.arg(baselines, several.ok = TRUE)
     has_ess <- "ess" %in% names(d)
     has_status <- "convergence_status" %in% names(d)

     # Resolve per-metric embargo (weeks). Scalar -> recycle; named -> lookup.
     emb_of <- .rcv_embargo_lookup(embargo_weeks, metrics)

     cell_rows <- list()
     cells <- unique(d[, c("run_id", "model", "iso_code", "metric")])
     cells <- cells[cells$metric %in% metrics, , drop = FALSE]

     for (r in seq_len(nrow(cells))) {
          this_metric <- cells$metric[r]
          cd <- d[d$run_id == cells$run_id[r] & d$model == cells$model[r] &
                  d$iso_code == cells$iso_code[r] & d$metric == this_metric, ]
          cutoff <- cd$cutoff_date[1]
          # cell-level ESS (constant per cutoff); first finite value if present
          ess_val <- if (has_ess) {
               ev <- cd$ess[is.finite(cd$ess)]
               if (length(ev)) ev[1] else NA_real_
          } else NA_real_
          # cell-level convergence status (constant per cutoff; NA before 1.4.0)
          status_val <- if (has_status) {
               sv <- as.character(cd$convergence_status)
               sv <- sv[!is.na(sv) & sv %in% c("green", "yellow", "red")]
               if (length(sv)) sv[1] else NA_character_
          } else NA_character_
          ess_ok <- .rcv_ess_ok(status_val, ess_val, ess_min, has_ess)

          # Per-metric scoring origin: the later of the harness embargo end (the
          # last "embargo"-labelled date, else the cutoff) and the caller's
          # per-metric embargo. Scored dates are strictly after it, and horizon
          # windows are measured from it -- not from the first date that happens
          # to carry an observation.
          ck <- paste(cells$run_id[r], cells$model[r], cells$iso_code[r], this_metric, sep = "\r")
          harness_origin <- if (ck %in% names(emb_end))
               max(cutoff, as.Date(emb_end[[ck]], origin = "1970-01-01")) else cutoff
          oos0 <- max(harness_origin, cutoff + emb_of[[this_metric]] * 7)

          is_df  <- cd[cd$segment == "IS"  & is.finite(cd$observed) & is.finite(cd$pred_central), ]
          oos_df <- cd[cd$date > oos0 & cd$segment != "IS" &
                       is.finite(cd$observed) & is.finite(cd$pred_central), ]

          base_meta <- list(run_id = cells$run_id[r], model = cells$model[r],
                            iso_code = cells$iso_code[r], metric = this_metric,
                            cutoff_date = as.character(cutoff))
          if ("anchor_date" %in% names(cd))
               base_meta$anchor_date <- as.character(cd$anchor_date[1])

          # IS window (fit) — no baselines/skill
          if (nrow(is_df) >= 3L)
               cell_rows[[length(cell_rows) + 1L]] <- cbind(
                    as.data.frame(base_meta), window = "IS",
                    .rcv_window_metrics(is_df), .rcv_empty_skill(baselines),
                    ess = ess_val, convergence_status = status_val, ess_ok = ess_ok)

          # OOS cumulative horizons
          for (h in horizons_months) {
               wend <- oos0 + ceiling(h * 30.4375)
               sl   <- oos_df[oos_df$date <= wend, ]
               if (nrow(sl) < 3L) next
               sk <- .rcv_skill(sl, is_df, baselines)
               cell_rows[[length(cell_rows) + 1L]] <- cbind(
                    as.data.frame(base_meta), window = sprintf("OOS<=%gmo", h),
                    .rcv_window_metrics(sl), sk,
                    ess = ess_val, convergence_status = status_val, ess_ok = ess_ok)
          }
     }
     cells_df <- do.call(rbind, cell_rows)
     .rcv_gate_warnings(cells_df, gate_on = ess_min > 0, where = "evaluate_rolling_cv()")

     # ---- aggregate across ESS-passing cells per (model, metric, window) ----
     agg_keys <- unique(cells_df[, c("model", "metric", "window")])
     summ <- list()
     skill_cols <- grep("^(mae_skill|wis_skill)_", names(cells_df), value = TRUE)
     for (i in seq_len(nrow(agg_keys))) {
          all_sub <- cells_df[cells_df$model == agg_keys$model[i] &
                              cells_df$metric == agg_keys$metric[i] &
                              cells_df$window == agg_keys$window[i], ]
          n_total <- nrow(all_sub)
          sub <- all_sub[all_sub$ess_ok, , drop = FALSE]   # gated aggregation
          n_used <- nrow(sub)
          row <- data.frame(
               model = agg_keys$model[i], metric = agg_keys$metric[i],
               window = agg_keys$window[i],
               n_cells_used  = n_used,
               n_cells_total = n_total,
               ess_gated     = ess_min > 0 && (has_ess || has_status),
               R2_corr_med = round(stats::median(sub$R2_corr, na.rm = TRUE), 3),
               R2_sse_med  = round(stats::median(sub$R2_sse,  na.rm = TRUE), 3),
               bias_med    = round(stats::median(sub$bias_ratio, na.rm = TRUE), 3),
               cov50       = round(mean(sub$cov50, na.rm = TRUE), 3),
               cov95       = round(mean(sub$cov95, na.rm = TRUE), 3),
               cov50_calib_err = round(abs(mean(sub$cov50, na.rm = TRUE) - 0.50), 3),
               cov95_calib_err = round(abs(mean(sub$cov95, na.rm = TRUE) - 0.95), 3),
               wis_mean    = round(mean(sub$wis, na.rm = TRUE), 3),
               stringsAsFactors = FALSE)
          for (sc in skill_cols) {
               vals <- sub[[sc]]
               n_fin <- sum(is.finite(vals))
               suppress <- n_fin < min_cells_ci
               bs <- .rcv_boot_mean(vals, n_boot, seed, suppress)
               row[[paste0(sc, "_mean")]] <- round(bs[["mean"]], 3)
               row[[paste0(sc, "_lo")]]   <- round(bs[["lo"]], 3)
               row[[paste0(sc, "_hi")]]   <- round(bs[["hi"]], 3)
          }
          # CI suppression flag: TRUE if any skill column had < min_cells_ci finite cells
          row$ci_suppressed <- any(vapply(skill_cols,
               function(sc) sum(is.finite(sub[[sc]])) < min_cells_ci, logical(1)))
          summ[[length(summ) + 1L]] <- row
     }
     summary_df <- do.call(rbind, summ)
     summary_df <- summary_df[order(summary_df$model, summary_df$metric, summary_df$window), ]

     list(cells = cells_df, summary = summary_df)
}


# ============================ internal helpers ============================

#' Whether a cell passes the opt-in convergence gate
#'
#' No gate when \code{ess_min <= 0}. Otherwise a cell with a convergence status
#' passes unless it is red; a cell without one (pre-1.4.0 cutoff) passes when
#' its ESS is at least \code{ess_min}; a cell with neither passes. Shared by
#' \code{evaluate_rolling_cv()} so mixed old/new cutoffs are scored under one rule.
#' @param status Cell convergence status (\code{NA} before 1.4.0).
#' @param ess Cell ESS (\code{NA} when absent).
#' @param ess_min Gate threshold; \code{<= 0} disables the gate.
#' @param has_ess Whether the predictions carry an \code{ess} column.
#' @return Logical scalar.
#' @keywords internal
#' @noRd
.rcv_ess_ok <- function(status, ess, ess_min, has_ess) {
     if (!is.finite(ess_min) || ess_min <= 0) return(TRUE)
     if (!is.na(status)) return(status != "red")
     if (!isTRUE(has_ess)) return(TRUE)
     !is.na(ess) && ess >= ess_min
}

#' Warn when the convergence status would empty a forecast-CV summary
#'
#' Called by \code{evaluate_rolling_cv()}, \code{make_forecast_cv_table()} and
#' \code{plot_forecast_cv_skill()}: warns when more than half of the cells that
#' carry a convergence status are red (the cutoff calibrations are too short for
#' the 1.4.0 status; the gate, if switched on, would drop them), and when the
#' gate (\code{ess_ok}) removes every cell.
#' @param cells Scored cells (\code{convergence_status}, \code{ess_ok} optional).
#' @param gate_on Whether a gate is applied.
#' @param where Caller name for the message.
#' @return Invisible NULL.
#' @keywords internal
#' @noRd
.rcv_gate_warnings <- function(cells, gate_on, where) {
     if (is.null(cells) || !nrow(cells)) return(invisible(NULL))
     if ("convergence_status" %in% names(cells)) {
          st <- as.character(cells$convergence_status)
          st <- st[!is.na(st)]
          if (length(st) && mean(st == "red") > 0.5)
               warning(sprintf(paste0("%s: %d of %d cells come from cutoff calibrations whose convergence ",
                                      "status is red (too few simulations for the MOSAIC 1.4.0 status)%s."),
                               where, sum(st == "red"), length(st),
                               if (isTRUE(gate_on)) "; the gate leaves them out of the summary"
                               else "; they are scored (no gate: ess_min = 0)"),
                       call. = FALSE)
     }
     if (isTRUE(gate_on) && "ess_ok" %in% names(cells) && !any(as.logical(cells$ess_ok) %in% TRUE))
          warning(sprintf(paste0("%s: the convergence gate removed every cell, so the summary has no ",
                                 "cells; set ess_min = 0 to score them without the gate."), where),
                  call. = FALSE)
     invisible(NULL)
}

#' @keywords internal
#' @noRd
.rcv_eval_input <- function(predictions) {
     if (is.data.frame(predictions)) return(predictions)
     if (is.character(predictions) && length(predictions) == 1L) {
          p <- predictions
          if (dir.exists(p)) p <- file.path(p, "predictions.parquet")
          if (!file.exists(p)) stop("predictions file not found: ", p)
          if (!requireNamespace("arrow", quietly = TRUE))
               stop("the 'arrow' package is required to read parquet predictions")
          return(as.data.frame(arrow::read_parquet(p)))
     }
     stop("`predictions` must be a data.frame, a parquet path, or a run_rolling_cv dir")
}

#' Resolve a scalar-or-named embargo into a per-metric lookup (weeks)
#' @keywords internal
#' @noRd
.rcv_embargo_lookup <- function(embargo_weeks, metrics) {
     ew <- embargo_weeks
     if (length(ew) == 1L && is.null(names(ew))) {
          out <- as.list(rep(as.numeric(ew), length(metrics)))
          names(out) <- metrics
          return(out)
     }
     if (is.null(names(ew)))
          stop("`embargo_weeks` must be a scalar or a named vector (e.g. c(cases=4, deaths=6))")
     out <- setNames(as.list(rep(0, length(metrics))), metrics)
     # `ew[[m]]` on an atomic vector errors for an absent name, so test
     # membership first; omitted metrics keep 0.
     for (m in metrics) if (m %in% names(ew) && !is.na(ew[[m]])) out[[m]] <- as.numeric(ew[[m]])
     # any name in ew not in metrics is harmlessly ignored
     out
}

#' Per-window model metrics (R2 corr/sse, bias, coverage, WIS, MAE/RMSE)
#' @keywords internal
#' @noRd
.rcv_window_metrics <- function(df) {
     o <- df$observed
     # Point metrics use the chosen central series; WIS uses the median of the
     # same predictive draws as the intervals (quantile-based, central-method-
     # invariant): pred_median_obs since v0.101.0, the ensemble median before.
     p     <- if ("pred_central" %in% names(df)) df$pred_central else df$pred_median
     p_med <- if ("pred_median_obs" %in% names(df)) df$pred_median_obs else df$pred_median
     cov50 <- cov95 <- wis <- NA_real_
     if (all(c("pi50_lo","pi50_hi","pi95_lo","pi95_hi") %in% names(df))) {
          cov50 <- mean(o >= df$pi50_lo & o <= df$pi50_hi, na.rm = TRUE)
          cov95 <- mean(o >= df$pi95_lo & o <= df$pi95_hi, na.rm = TRUE)
          wis   <- mean(.rcv_wis(o, p_med, df$pi50_lo, df$pi50_hi, df$pi95_lo, df$pi95_hi), na.rm = TRUE)
     }
     data.frame(
          n        = nrow(df),
          R2_corr  = round(MOSAIC::calc_model_R2(o, p, method = "corr"), 3),
          R2_sse   = round(MOSAIC::calc_model_R2(o, p, method = "sse"), 3),
          bias_ratio = round(MOSAIC::calc_bias_ratio(o, p), 3),
          cov50    = round(cov50, 3),
          cov95    = round(cov95, 3),
          wis      = round(wis, 3),
          mae      = round(mean(abs(o - p), na.rm = TRUE), 3),
          rmse     = round(sqrt(mean((o - p)^2, na.rm = TRUE)), 3),
          stringsAsFactors = FALSE)
}

#' Weighted Interval Score from median + 50% + 95% intervals (Bracher et al. 2021)
#' @keywords internal
#' @noRd
.rcv_wis <- function(y, m, l50, u50, l95, u95) {
     is_score <- function(l, u, y, a) (u - l) + (2/a)*(l - y)*(y < l) + (2/a)*(y - u)*(y > u)
     (0.5*abs(y - m) + 0.25*is_score(l50, u50, y, 0.5) + 0.025*is_score(l95, u95, y, 0.05)) / 2.5
}

#' Baseline point + interval forecasts for OOS dates from IS observed history
#'
#' Returns a list with \code{point} (length(oos_dates)) and the four interval
#' bounds (\code{pi50_lo/hi}, \code{pi95_lo/hi}). The intervals are the baseline
#' point plus empirical quantiles of the in-sample residuals (obs - baseline_fit
#' on the IS history). When the baseline cannot be formed, every element is
#' \code{NA}. The seasonal baseline returns all-\code{NA} unless the IS history
#' spans >= 2 years (no grand-mean fallback).
#' @keywords internal
#' @noRd
.rcv_baseline <- function(is_df, oos_dates, type) {
     na_out <- function() list(point = rep(NA_real_, length(oos_dates)),
                               pi50_lo = rep(NA_real_, length(oos_dates)),
                               pi50_hi = rep(NA_real_, length(oos_dates)),
                               pi95_lo = rep(NA_real_, length(oos_dates)),
                               pi95_hi = rep(NA_real_, length(oos_dates)))
     if (nrow(is_df) == 0L) return(na_out())
     io <- is_df[order(is_df$date), ]

     # add residual-quantile intervals around a point vector
     add_intervals <- function(point, resid) {
          resid <- resid[is.finite(resid)]
          if (length(resid) < 2L) {
               # cannot form an empirical interval -> NA bounds (WIS undefined)
               return(list(point = point,
                           pi50_lo = rep(NA_real_, length(point)),
                           pi50_hi = rep(NA_real_, length(point)),
                           pi95_lo = rep(NA_real_, length(point)),
                           pi95_hi = rep(NA_real_, length(point))))
          }
          q <- stats::quantile(resid, c(0.025, 0.25, 0.75, 0.975),
                               na.rm = TRUE, names = FALSE, type = 7)
          list(point   = point,
               pi50_lo = point + q[2], pi50_hi = point + q[3],
               pi95_lo = point + q[1], pi95_hi = point + q[4])
     }

     if (type == "persistence") {
          mu    <- mean(utils::tail(io$observed, 4), na.rm = TRUE)
          point <- rep(mu, length(oos_dates))
          add_intervals(point, io$observed - mu)

     } else if (type == "persistence_last") {
          last  <- utils::tail(io$observed, 1)
          point <- rep(as.numeric(last), length(oos_dates))
          add_intervals(point, io$observed - as.numeric(last))

     } else if (type == "seasonal") {
          # require >= 2 years of in-sample history; else NA (no grand-mean)
          span_days <- as.numeric(max(io$date) - min(io$date))
          if (span_days < 730) return(na_out())
          woy_is  <- as.integer(format(io$date, "%V"))
          clim    <- tapply(io$observed, woy_is, mean, na.rm = TRUE)
          woy_oos <- as.integer(format(oos_dates, "%V"))
          point   <- as.numeric(clim[as.character(woy_oos)])   # NA where WoY unseen
          # IS residual vs its own WoY climatology
          fit_is  <- as.numeric(clim[as.character(woy_is)])
          add_intervals(point, io$observed - fit_is)

     } else stop("unknown baseline: ", type)
}

#' MAE-ratio and WIS-ratio skill vs each baseline for an OOS slice
#' @keywords internal
#' @noRd
.rcv_skill <- function(sl, is_df, baselines) {
     sl_p   <- if ("pred_central" %in% names(sl)) sl$pred_central else sl$pred_median
     # Median of the same draws as the intervals (see .rcv_window_metrics()).
     sl_med <- if ("pred_median_obs" %in% names(sl)) sl$pred_median_obs else sl$pred_median
     mae_m  <- mean(abs(sl$observed - sl_p), na.rm = TRUE)
     wis_m  <- if (all(c("pi50_lo","pi50_hi","pi95_lo","pi95_hi") %in% names(sl)))
          mean(.rcv_wis(sl$observed, sl_med, sl$pi50_lo, sl$pi50_hi,
                        sl$pi95_lo, sl$pi95_hi), na.rm = TRUE) else NA_real_
     out <- list()
     for (b in baselines) {
          bl    <- .rcv_baseline(is_df, sl$date, b)
          mae_b <- mean(abs(sl$observed - bl$point), na.rm = TRUE)
          out[[paste0("mae_skill_", b)]] <- if (is.finite(mae_b) && mae_b > 0)
               round(1 - mae_m / mae_b, 3) else NA_real_
          wis_b <- if (any(is.finite(bl$pi50_lo)))
               mean(.rcv_wis(sl$observed, bl$point, bl$pi50_lo, bl$pi50_hi,
                             bl$pi95_lo, bl$pi95_hi), na.rm = TRUE) else NA_real_
          out[[paste0("wis_skill_", b)]] <- if (is.finite(wis_m) && is.finite(wis_b) && wis_b > 0)
               round(1 - wis_m / wis_b, 3) else NA_real_
     }
     as.data.frame(out)
}

#' @keywords internal
#' @noRd
.rcv_empty_skill <- function(baselines) {
     nm <- c(paste0("mae_skill_", baselines), paste0("wis_skill_", baselines))
     out <- as.list(rep(NA_real_, length(nm)))
     names(out) <- nm
     as.data.frame(out)
}

#' Bootstrap mean + 95% CI over cells (CI suppressed when \code{suppress=TRUE})
#' @keywords internal
#' @noRd
.rcv_boot_mean <- function(x, n_boot, seed, suppress = FALSE) {
     x <- x[is.finite(x)]
     if (length(x) == 0L) return(c(mean = NA_real_, lo = NA_real_, hi = NA_real_))
     m <- mean(x)
     if (suppress || length(x) == 1L) return(c(mean = m, lo = NA_real_, hi = NA_real_))
     # local RNG that does not disturb the global stream
     old <- if (exists(".Random.seed", envir = .GlobalEnv))
          get(".Random.seed", envir = .GlobalEnv) else NULL
     set.seed(seed)
     bs <- vapply(seq_len(n_boot), function(i) mean(sample(x, length(x), replace = TRUE)),
                  numeric(1))
     if (!is.null(old)) assign(".Random.seed", old, envir = .GlobalEnv)
     c(mean = m, lo = unname(stats::quantile(bs, 0.025)), hi = unname(stats::quantile(bs, 0.975)))
}
