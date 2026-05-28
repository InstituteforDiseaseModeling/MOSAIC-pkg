#' Impute Country-Week Flood Probability from EM-DAT and Climate Covariates
#'
#' Fits a binomial GAM on observed EM-DAT flood events (`emdat_flood_active`)
#' as a function of climate anomalies, cyclic seasonality, ENSO/IOD long
#' lags, and country random effects, then predicts a continuous flood
#' probability for every (iso_code, year, week) row in the suitability
#' data — including forecast-window rows where EM-DAT carries no
#' observation. The imputed column lets the downstream LSTM consume the
#' same flood feature definition in training and inference, eliminating
#' the distribution shift the raw 0/1 indicator would otherwise introduce.
#'
#' @param d A data.frame with one row per (iso_code, year, week). Must
#'   contain: \code{iso_code}, \code{year}, \code{week}, \code{date},
#'   \code{emdat_flood_active} (0/1; NA in forecast rows is fine), the
#'   precipitation channels \code{precipitation_sum}, \code{precip_anom},
#'   \code{precip_sum_2w}, \code{precip_sum_4w}, \code{precip_sum_8w},
#'   \code{precip_sum_12w}, \code{precip_extreme_p90_count}; soil moisture
#'   \code{soil_moisture_0_to_10cm_mean}, \code{soil_moisture_anom};
#'   \code{spei_approx}; atmospheric humidity
#'   \code{relative_humidity_2m_mean}, \code{rh_mean_12w}; storm proxy
#'   \code{wind_speed_10m_max}; and the teleconnection indices
#'   \code{ENSO3}, \code{ENSO34}, \code{ENSO4}, \code{IOD} (lags computed
#'   inline). Standard output shape from \code{compile_suitability_data()}
#'   after its enhanced-covariate block.
#' @param output_col Character. Name of the new probability column.
#'   Default \code{"emdat_flood_prob"}.
#' @param diagnostics Logical. If \code{TRUE}, fits a rolling-year
#'   cross-validation and writes four artefacts to \code{diag_dir}:
#'   \code{flood_gam_smooths.png}, \code{flood_gam_calibration.png},
#'   \code{flood_gam_cv_metrics.csv}, \code{flood_gam_summary.txt}.
#' @param diag_dir Character. Directory for diagnostic outputs (created
#'   if missing). Ignored when \code{diagnostics = FALSE}.
#' @param verbose Logical. Echo progress messages.
#'
#' @return The input data.frame with one added column named
#'   \code{output_col} carrying a non-NA numeric in \[0, 1\] for every row.
#'
#' @details
#' The GAM is fit with \code{mgcv::bam()} (mgcv's big-data variant — uses
#' block updates, discrete covariate binning, and parallel-friendly
#' linear algebra; on ~30k rows it returns in seconds where \code{gam()}
#' would take ~15 min with essentially identical fitted values).
#' \code{family = binomial(link = "logit")}, \code{method = "fREML"}, and
#' \code{select = FALSE} (the smooth-shrinkage penalty was previously
#' suppressing the ENSO/IOD teleconnection terms; without it those terms
#' keep their unpenalized REML weight). Lagged ENSO34/IOD predictors
#' (lags 8, 16, and 24 weeks; lag-0 is the raw current index) are
#' computed inside the function via grouped \code{dplyr::lag()}, so the
#' caller does not need to pass \code{include_lags = TRUE} to
#' \code{compile_suitability_data()}.
#'
#' Sentinel handling: any row whose prediction is NA (e.g. a lag-startup
#' edge before week 20 of the earliest year, or a row with a missing
#' predictor that survived upstream imputation) is filled with that
#' country's mean predicted probability. Rows where even the country mean
#' is NA fall back to the global mean. The function asserts the final
#' column is in \[0, 1\] and contains no NAs.
#'
#' If \code{diagnostics = TRUE}, the function additionally runs a
#' rolling-year cross-validation: for each of the 3 most recent
#' fully-observed years Y, the GAM is refit on rows with \code{year < Y}
#' and used to predict rows with \code{year == Y}. Per-fold AUC, Brier
#' score, and log-loss are written to \code{flood_gam_cv_metrics.csv}.
#' If the mean CV AUC is below 0.65, a warning is emitted but the
#' function still returns the column.
#'
#' @seealso \code{\link{compile_suitability_data}},
#'   \code{\link{est_suitability}}, \code{\link{process_EMDAT_data}}
#'
#' @importFrom mgcv bam s
#' @importFrom dplyr group_by arrange mutate lag ungroup
#' @importFrom stats binomial predict
#' @importFrom utils write.csv capture.output
#' @importFrom grDevices png dev.off
#' @importFrom graphics plot abline points
#' @export
impute_flood_probability <- function(d,
                                     output_col  = "emdat_flood_prob",
                                     diagnostics = TRUE,
                                     diag_dir    = NULL,
                                     verbose     = TRUE) {

     required <- .impute_flood_probability_required()
     missing_cols <- setdiff(required, names(d))
     if (length(missing_cols) > 0) {
          stop("impute_flood_probability: missing required column(s): ",
               paste(missing_cols, collapse = ", "))
     }

     if (verbose) message("Imputing flood probability via binomial GAM...")

     # Locally compute ENSO34 and IOD at multiple lag horizons (current,
     # 2 months, 4 months, 6 months prior) inside a grouped pipeline.
     # Lag-0 of each index is simply the raw current value.
     d_aug <- d %>%
          dplyr::group_by(iso_code) %>%
          dplyr::arrange(date, .by_group = TRUE) %>%
          dplyr::mutate(
               ENSO34_lag8  = dplyr::lag(ENSO34, n = 8),
               ENSO34_lag16 = dplyr::lag(ENSO34, n = 16),
               ENSO34_lag24 = dplyr::lag(ENSO34, n = 24),
               IOD_lag8     = dplyr::lag(IOD,    n = 8),
               IOD_lag16    = dplyr::lag(IOD,    n = 16),
               IOD_lag24    = dplyr::lag(IOD,    n = 24)
          ) %>%
          dplyr::ungroup()

     # Factorize iso_code for the random-effect smooth (mgcv requires factor)
     d_aug$iso_code_f <- factor(d_aug$iso_code)

     # Flood-physics-driven covariate set:
     #   * No cyclic-week smooth: lets the climate predictors carry the
     #     within-year cycle directly rather than imposing it as a global
     #     parametric shape.
     #   * No static elevation / urban_population_pct: country-level
     #     terrain and vulnerability differences are absorbed by the
     #     iso_code random effect.
     #   * All available precipitation / soil-moisture / humidity channels
     #     are included -- raw, anomaly, multiple rolling windows -- so
     #     the GAM can pick out the rainfall horizon that matters per
     #     country.
     #   * Heavy ENSO/IOD bench: ENSO34 at current + 2/4/6-month lags,
     #     plus current ENSO3 and ENSO4 (different Pacific regions), and
     #     IOD at the same lag horizons. This is the multi-month
     #     forecast backbone for the East African flood regimes that
     #     carry the strongest interannual signal.
     #   * select = FALSE: the extra smooth-shrinkage penalty was
     #     suppressing exactly the teleconnection signal we want to
     #     emphasise. Without it, smooths keep their unpenalized REML
     #     weight. CV AUC will tell us if any term needs trimming.
     gam_formula <- emdat_flood_active ~
          # Country baseline
          s(iso_code_f, bs = "re") +
          # All precipitation channels
          s(precipitation_sum) +
          s(precip_anom) +
          s(precip_sum_2w) +
          s(precip_sum_4w) +
          s(precip_sum_8w) +
          s(precip_sum_12w) +
          # Extreme-rain trigger (binary -> linear)
          precip_extreme_p90_count +
          # Soil-moisture state
          s(soil_moisture_0_to_10cm_mean) +
          s(soil_moisture_anom) +
          # Drought/wet bidirectional index
          s(spei_approx) +
          # Atmospheric humidity
          s(relative_humidity_2m_mean) +
          s(rh_mean_12w) +
          # Storm / cyclone proxy
          s(wind_speed_10m_max) +
          # ENSO heavy: multi-region + multi-horizon lags
          s(ENSO34) +
          s(ENSO34_lag8) +
          s(ENSO34_lag16) +
          s(ENSO34_lag24) +
          s(ENSO3) +
          s(ENSO4) +
          # IOD heavy: current + multi-horizon lags
          s(IOD) +
          s(IOD_lag8) +
          s(IOD_lag16) +
          s(IOD_lag24)

     train_idx <- !is.na(d_aug$emdat_flood_active)
     train <- d_aug[train_idx, , drop = FALSE]
     if (nrow(train) < 100) {
          stop("impute_flood_probability: only ", nrow(train),
               " training rows with non-NA emdat_flood_active; need >= 100.")
     }
     if (verbose) {
          message(sprintf("  Training rows: %d (active fraction: %.3f)",
                          nrow(train), mean(train$emdat_flood_active)))
     }

     # bam() is mgcv's big-data fitter: block updates, single-precision
     # linear algebra, optional discrete covariate binning. With ~22 smooths
     # on ~30k rows it runs in well under a minute.
     # select = FALSE: see the formula block above; the shrinkage penalty
     # was suppressing the teleconnection signal we now want to lean on.
     gam_model <- mgcv::bam(
          formula  = gam_formula,
          family   = stats::binomial(link = "logit"),
          data     = train,
          method   = "fREML",
          select   = FALSE,
          discrete = TRUE
     )
     if (verbose) {
          message(sprintf("  Deviance explained: %.1f%%",
                          summary(gam_model)$dev.expl * 100))
     }

     # Predict on every row (incl. forecast). Missing-predictor rows return NA.
     preds <- as.numeric(stats::predict(gam_model, newdata = d_aug,
                                         type = "response"))

     # Sentinel: country mean prob for residual NAs; global mean as last resort.
     country_mean <- tapply(preds, d_aug$iso_code, mean, na.rm = TRUE)
     na_rows <- is.na(preds)
     if (any(na_rows)) {
          preds[na_rows] <- country_mean[as.character(d_aug$iso_code[na_rows])]
     }
     still_na <- is.na(preds)
     if (any(still_na)) {
          preds[still_na] <- mean(preds, na.rm = TRUE)
     }

     # Guard the contract: numeric, no NA, in [0, 1]
     stopifnot(!any(is.na(preds)), all(preds >= 0), all(preds <= 1))

     d[[output_col]] <- preds

     if (isTRUE(diagnostics)) {
          if (is.null(diag_dir)) {
               diag_dir <- file.path(tempdir(), "flood_imputation")
          }
          dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
          .write_flood_gam_diagnostics(gam_model, train, d_aug, preds,
                                        diag_dir, verbose)
     }

     d
}


# Internal: produce the four diagnostic artefacts
.write_flood_gam_diagnostics <- function(gam_model, train, d_aug, preds,
                                          diag_dir, verbose) {

     # 1. Smooth-term diagnostics
     png(file.path(diag_dir, "flood_gam_smooths.png"),
         width = 1400, height = 1000, res = 120)
     tryCatch({
          plot(gam_model, pages = 1)
     }, error = function(e) {
          message("    diagnostic: plot(gam) failed: ", conditionMessage(e))
     })
     dev.off()

     # 2. Per-country time-series: imputed probability vs observed binary.
     # The 0/1 observed events are rendered as red tick marks just above the
     # probability line; the imputed continuous probability is the dark line.
     # A vertical dashed line marks the last week with any observed
     # emdat_flood_active so the forecast tail (imputed-only) is visually
     # distinct from the historical training window.
     ts_df <- data.frame(
          iso_code  = d_aug$iso_code,
          date      = as.Date(d_aug$date),
          observed  = d_aug$emdat_flood_active,
          predicted = preds,
          stringsAsFactors = FALSE
     )
     panel_end <- max(ts_df$date[!is.na(ts_df$observed)], na.rm = TRUE)
     events <- ts_df[!is.na(ts_df$observed) & ts_df$observed == 1, ]
     if (requireNamespace("ggplot2", quietly = TRUE)) {
          p_ts <- ggplot2::ggplot(ts_df, ggplot2::aes(x = date, y = predicted)) +
               ggplot2::geom_vline(xintercept = panel_end,
                                    linetype = "dashed", colour = "grey50") +
               ggplot2::geom_line(linewidth = 0.3, colour = "grey20") +
               ggplot2::geom_rug(data = events, sides = "t",
                                  colour = "firebrick", alpha = 0.6,
                                  length = grid::unit(0.04, "npc")) +
               ggplot2::facet_wrap(~ iso_code, ncol = 5, scales = "free_x") +
               ggplot2::scale_y_continuous(limits = c(0, 1)) +
               ggplot2::labs(
                    title    = "Imputed flood probability vs observed EM-DAT events",
                    subtitle = "Black line = emdat_flood_prob; red ticks = weeks with emdat_flood_active = 1; dashed line = end of EM-DAT panel",
                    x = NULL, y = "P(flood)"
               ) +
               ggplot2::theme_minimal(base_size = 9) +
               ggplot2::theme(
                    panel.grid.minor = ggplot2::element_blank(),
                    strip.text       = ggplot2::element_text(face = "bold")
               )
          ggplot2::ggsave(
               filename = file.path(diag_dir, "flood_gam_predictions_timeseries.png"),
               plot     = p_ts,
               width    = 14, height = 18, dpi = 120, limitsize = FALSE
          )
     } else {
          message("    diagnostic: ggplot2 unavailable; skipping per-country time-series plot")
     }

     # 3a. Calibration: decile-bin predicted vs observed
     train_pred <- as.numeric(stats::predict(gam_model, newdata = train,
                                              type = "response"))
     deciles <- cut(train_pred,
                     breaks = stats::quantile(train_pred,
                                              probs = seq(0, 1, 0.1),
                                              na.rm = TRUE),
                     include.lowest = TRUE)
     calib <- data.frame(
          predicted = tapply(train_pred,                  deciles, mean, na.rm = TRUE),
          observed  = tapply(train$emdat_flood_active,    deciles, mean, na.rm = TRUE)
     )
     png(file.path(diag_dir, "flood_gam_calibration.png"),
         width = 800, height = 800, res = 120)
     plot(calib$predicted, calib$observed,
          xlim = c(0, 1), ylim = c(0, 1),
          xlab = "Mean predicted flood probability (decile)",
          ylab = "Observed flood rate (decile)",
          main = "Flood-prob GAM calibration",
          pch = 19)
     abline(0, 1, lty = 2)
     dev.off()

     # 3b. Rolling-year CV (cap at the 3 most recent fully-observed years to
     # keep diagnostics under a few minutes; 3 folds is plenty for an
     # out-of-time AUC point estimate)
     years <- sort(unique(d_aug$year[!is.na(d_aug$emdat_flood_active)]))
     candidate_years <- years[years >= 2018 & years <= max(years) - 1]
     cv_years <- utils::tail(candidate_years, 3)
     cv_rows <- list()
     for (yr in cv_years) {
          tr <- train[train$year <  yr, , drop = FALSE]
          te <- train[train$year == yr, , drop = FALSE]
          if (nrow(tr) < 100 || nrow(te) < 20 ||
              length(unique(te$emdat_flood_active)) < 2) next
          fit <- tryCatch(
               mgcv::bam(formula  = gam_model$formula,
                         family   = stats::binomial(link = "logit"),
                         data     = tr, method = "fREML",
                         select   = FALSE, discrete = TRUE),
               error = function(e) NULL
          )
          if (is.null(fit)) next
          p <- as.numeric(stats::predict(fit, newdata = te, type = "response"))
          p <- pmin(pmax(p, 1e-9), 1 - 1e-9)
          y <- te$emdat_flood_active
          # AUC via Mann-Whitney-U
          r <- rank(p)
          n_pos <- sum(y == 1); n_neg <- sum(y == 0)
          auc <- (sum(r[y == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
          brier <- mean((p - y)^2)
          ll    <- -mean(y * log(p) + (1 - y) * log(1 - p))
          cv_rows[[length(cv_rows) + 1]] <- data.frame(
               year_val = yr, n_train = nrow(tr), n_val = nrow(te),
               auc = auc, brier = brier, log_loss = ll
          )
     }
     if (length(cv_rows) > 0) {
          cv_df <- do.call(rbind, cv_rows)
          utils::write.csv(cv_df,
                            file.path(diag_dir, "flood_gam_cv_metrics.csv"),
                            row.names = FALSE)
          mean_auc <- mean(cv_df$auc, na.rm = TRUE)
          if (verbose) {
               message(sprintf("  Rolling-year CV mean AUC: %.3f (n=%d folds)",
                               mean_auc, nrow(cv_df)))
          }
          if (is.finite(mean_auc) && mean_auc < 0.65) {
               warning(sprintf(
                    "Flood-prob GAM mean CV AUC = %.3f < 0.65: imputation may add limited signal beyond climatology.",
                    mean_auc))
          }
     }

     # 4. Summary text
     writeLines(utils::capture.output(summary(gam_model)),
                 con = file.path(diag_dir, "flood_gam_summary.txt"))

     if (verbose) {
          message("  Diagnostics written to: ", diag_dir)
     }
     invisible(NULL)
}


# Internal: canonical list of columns the flood-prob GAM consumes. Both
# `impute_flood_probability()` and the upstream gate in
# `compile_suitability_data()` source the list from here so the two cannot
# drift out of sync as the GAM formula evolves.
.impute_flood_probability_required <- function() {
     c("iso_code", "year", "week", "date", "emdat_flood_active",
       # Precipitation channels: raw + anomaly + rolling windows
       "precipitation_sum", "precip_anom",
       "precip_sum_2w", "precip_sum_4w", "precip_sum_8w", "precip_sum_12w",
       "precip_extreme_p90_count",
       # Soil moisture (raw + anomaly)
       "soil_moisture_0_to_10cm_mean", "soil_moisture_anom",
       # Drought/wet bidirectional index
       "spei_approx",
       # Atmospheric humidity (raw + rolling)
       "relative_humidity_2m_mean", "rh_mean_12w",
       # Storm/cyclone proxy
       "wind_speed_10m_max",
       # Teleconnections (lags computed inline by the imputer)
       "ENSO3", "ENSO34", "ENSO4", "IOD")
}
