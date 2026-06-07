#' Post-hoc evaluation of a rolling-window forecast-validation artifact
#'
#' Reads a \code{\link{run_rolling_cv}} predictions artifact and computes
#' out-of-sample forecast skill: per-cell metrics (R2 correlation & SSE, bias
#' ratio, 50/95\% PI coverage, WIS) for the in-sample fit and for cumulative OOS
#' horizons, plus skill (MAE ratio) versus seasonal-climatology and persistence
#' baselines, aggregated across cutoffs and countries with bootstrap CIs.
#'
#' @details
#' This is the post-hoc companion to \code{run_rolling_cv()} — the harness emits
#' predictions only; all judgement (metrics, baselines, skill) lives here.
#'
#' Horizons are \strong{cumulative} (\eqn{\le h} months): the \eqn{\le h} window
#' is every OOS point within \code{h} months of the forecast origin, computed from
#' \code{date} and \code{cutoff_date} (not the disjoint \code{horizon_bucket}
#' label), so the \eqn{\le}max-horizon window equals the full scored OOS set.
#' An \code{IS} window (training fit, \eqn{\le} cutoff) is always reported.
#'
#' Scoring uses \strong{trusted observed only}: rows whose \code{observed_source}
#' is missing or not \code{"AI"} (when \code{trusted_only = TRUE}).
#'
#' Baselines, fit per (cutoff, country, metric) on the IS observed history:
#' \code{"persistence"} (trailing 4-point mean carried forward) and
#' \code{"seasonal"} (week-of-year mean of IS observations). Skill is the MAE
#' ratio \eqn{1 - MAE_{model}/MAE_{baseline}} (positive = model beats baseline).
#'
#' @param predictions A \code{run_rolling_cv} output directory, a path to a
#'   \code{predictions.parquet}, or the predictions data frame itself.
#' @param horizons_months Cumulative OOS horizons in months (default \code{c(1,3,5)}).
#' @param baselines Baselines for skill scores (default \code{c("seasonal","persistence")}).
#' @param metrics Metrics to evaluate (default \code{c("cases","deaths")}).
#' @param trusted_only Drop rows with \code{observed_source == "AI"} (default TRUE).
#' @param n_boot Bootstrap resamples over cells for the skill CI (default 1000).
#' @param seed RNG seed for the bootstrap (default 1).
#'
#' @return A list with:
#' \describe{
#'   \item{cells}{Per (cutoff x iso x metric x window) metrics + per-baseline skill.}
#'   \item{summary}{Aggregated across cells per (metric x window): cell count,
#'     mean/median of each metric, and mean baseline skill with bootstrap CI.}
#' }
#' @seealso \code{\link{run_rolling_cv}}, \code{\link{calc_model_R2}}, \code{\link{calc_bias_ratio}}
#' @importFrom stats median quantile
#' @export
evaluate_rolling_cv <- function(predictions,
                                horizons_months = c(1, 3, 5),
                                baselines       = c("seasonal", "persistence"),
                                metrics         = c("cases", "deaths"),
                                trusted_only    = TRUE,
                                n_boot          = 1000L,
                                seed            = 1L) {

     d <- .rcv_eval_input(predictions)
     req <- c("iso_code", "cutoff_date", "date", "metric", "segment",
              "observed", "pred_median")
     miss <- setdiff(req, names(d))
     if (length(miss)) stop("predictions missing column(s): ", paste(miss, collapse = ", "))
     d$date        <- as.Date(d$date)
     d$cutoff_date <- as.Date(d$cutoff_date)
     if (!"run_id" %in% names(d)) d$run_id <- paste0("cutoff_", d$cutoff_date)
     if (trusted_only && "observed_source" %in% names(d))
          d <- d[is.na(d$observed_source) | d$observed_source != "AI", ]
     horizons_months <- sort(unique(as.numeric(horizons_months)))
     baselines <- match.arg(baselines, several.ok = TRUE)

     cell_rows <- list()
     cells <- unique(d[, c("run_id", "iso_code", "metric")])
     cells <- cells[cells$metric %in% metrics, , drop = FALSE]

     for (r in seq_len(nrow(cells))) {
          cd <- d[d$run_id == cells$run_id[r] & d$iso_code == cells$iso_code[r] &
                  d$metric == cells$metric[r], ]
          cutoff <- cd$cutoff_date[1]
          is_df  <- cd[cd$segment == "IS"  & is.finite(cd$observed) & is.finite(cd$pred_median), ]
          oos_df <- cd[cd$segment == "OOS" & is.finite(cd$observed) & is.finite(cd$pred_median), ]
          oos0   <- if (nrow(oos_df)) min(oos_df$date) else cutoff

          base_meta <- list(run_id = cells$run_id[r], iso_code = cells$iso_code[r],
                            metric = cells$metric[r], cutoff_date = as.character(cutoff))

          # IS window (fit) — no baselines/skill
          if (nrow(is_df) >= 3L)
               cell_rows[[length(cell_rows) + 1L]] <- cbind(
                    as.data.frame(base_meta), window = "IS",
                    .rcv_window_metrics(is_df), .rcv_empty_skill(baselines))

          # OOS cumulative horizons
          for (h in horizons_months) {
               wend <- oos0 + ceiling(h * 30.4375)
               sl   <- oos_df[oos_df$date <= wend, ]
               if (nrow(sl) < 3L) next
               sk <- .rcv_skill(sl, is_df, baselines)
               cell_rows[[length(cell_rows) + 1L]] <- cbind(
                    as.data.frame(base_meta), window = sprintf("OOS<=%gmo", h),
                    .rcv_window_metrics(sl), sk)
          }
     }
     cells_df <- do.call(rbind, cell_rows)

     # ---- aggregate across cells per (metric, window) ----
     agg_keys <- unique(cells_df[, c("metric", "window")])
     summ <- list()
     skill_cols <- grep("^skill_", names(cells_df), value = TRUE)
     for (i in seq_len(nrow(agg_keys))) {
          sub <- cells_df[cells_df$metric == agg_keys$metric[i] &
                          cells_df$window == agg_keys$window[i], ]
          row <- data.frame(
               metric = agg_keys$metric[i], window = agg_keys$window[i],
               n_cells = nrow(sub),
               R2_corr_med = round(stats::median(sub$R2_corr, na.rm = TRUE), 3),
               R2_sse_med  = round(stats::median(sub$R2_sse,  na.rm = TRUE), 3),
               bias_med    = round(stats::median(sub$bias_ratio, na.rm = TRUE), 3),
               cov50       = round(mean(sub$cov50, na.rm = TRUE), 3),
               cov95       = round(mean(sub$cov95, na.rm = TRUE), 3),
               wis_mean    = round(mean(sub$wis, na.rm = TRUE), 3),
               stringsAsFactors = FALSE)
          for (sc in skill_cols) {
               bs <- .rcv_boot_mean(sub[[sc]], n_boot, seed)
               row[[paste0(sc, "_mean")]] <- round(bs[["mean"]], 3)
               row[[paste0(sc, "_lo")]]   <- round(bs[["lo"]], 3)
               row[[paste0(sc, "_hi")]]   <- round(bs[["hi"]], 3)
          }
          summ[[length(summ) + 1L]] <- row
     }
     summary_df <- do.call(rbind, summ)
     summary_df <- summary_df[order(summary_df$metric, summary_df$window), ]

     list(cells = cells_df, summary = summary_df)
}


# ============================ internal helpers ============================

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

#' Per-window model metrics (R2 corr/sse, bias, coverage, WIS, MAE/RMSE)
#' @keywords internal
#' @noRd
.rcv_window_metrics <- function(df) {
     o <- df$observed; p <- df$pred_median
     cov50 <- cov95 <- wis <- NA_real_
     if (all(c("pi50_lo","pi50_hi","pi95_lo","pi95_hi") %in% names(df))) {
          cov50 <- mean(o >= df$pi50_lo & o <= df$pi50_hi, na.rm = TRUE)
          cov95 <- mean(o >= df$pi95_lo & o <= df$pi95_hi, na.rm = TRUE)
          wis   <- mean(.rcv_wis(o, p, df$pi50_lo, df$pi50_hi, df$pi95_lo, df$pi95_hi), na.rm = TRUE)
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

#' Point baseline forecasts for OOS dates from IS observed history
#' @keywords internal
#' @noRd
.rcv_baseline_point <- function(is_df, oos_dates, type) {
     if (nrow(is_df) == 0L) return(rep(NA_real_, length(oos_dates)))
     io <- is_df[order(is_df$date), ]
     if (type == "persistence") {
          tail_n <- utils::tail(io$observed, 4)
          rep(mean(tail_n, na.rm = TRUE), length(oos_dates))
     } else if (type == "seasonal") {
          woy_is  <- as.integer(format(io$date, "%V"))
          clim    <- tapply(io$observed, woy_is, mean, na.rm = TRUE)
          woy_oos <- as.integer(format(oos_dates, "%V"))
          out     <- as.numeric(clim[as.character(woy_oos)])
          out[is.na(out)] <- mean(io$observed, na.rm = TRUE)   # fallback
          out
     } else stop("unknown baseline: ", type)
}

#' MAE-ratio skill vs each baseline for an OOS slice
#' @keywords internal
#' @noRd
.rcv_skill <- function(sl, is_df, baselines) {
     mae_m <- mean(abs(sl$observed - sl$pred_median), na.rm = TRUE)
     out <- list()
     for (b in baselines) {
          bp <- .rcv_baseline_point(is_df, sl$date, b)
          mae_b <- mean(abs(sl$observed - bp), na.rm = TRUE)
          out[[paste0("skill_", b)]] <- if (is.finite(mae_b) && mae_b > 0)
               round(1 - mae_m / mae_b, 3) else NA_real_
     }
     as.data.frame(out)
}

#' @keywords internal
#' @noRd
.rcv_empty_skill <- function(baselines) {
     out <- as.list(rep(NA_real_, length(baselines)))
     names(out) <- paste0("skill_", baselines)
     as.data.frame(out)
}

#' Bootstrap mean + 95% CI over cells
#' @keywords internal
#' @noRd
.rcv_boot_mean <- function(x, n_boot, seed) {
     x <- x[is.finite(x)]
     if (length(x) == 0L) return(c(mean = NA_real_, lo = NA_real_, hi = NA_real_))
     m <- mean(x)
     if (length(x) == 1L) return(c(mean = m, lo = NA_real_, hi = NA_real_))
     # local RNG that does not disturb the global stream
     old <- if (exists(".Random.seed", envir = .GlobalEnv))
          get(".Random.seed", envir = .GlobalEnv) else NULL
     set.seed(seed)
     bs <- vapply(seq_len(n_boot), function(i) mean(sample(x, length(x), replace = TRUE)),
                  numeric(1))
     if (!is.null(old)) assign(".Random.seed", old, envir = .GlobalEnv)
     c(mean = m, lo = unname(stats::quantile(bs, 0.025)), hi = unname(stats::quantile(bs, 0.975)))
}
