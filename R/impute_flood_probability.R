#' Impute Country-Week Flood Probability from EM-DAT and Climate Covariates
#'
#' Fits an enriched binomial GAM on observed EM-DAT flood events
#' (\code{emdat_flood_active}, 0/1) as a function of climate anomalies,
#' multi-month antecedent precipitation, region-conditional
#' precipitation, joint precip/soil-moisture interactions, ENSO/IOD long
#' lags, and country random effects. The fitted model is then used to
#' predict a continuous flood probability for every (iso_code, year,
#' week) row -- historical AND forecast. The imputed column lets the
#' downstream LSTM consume the same feature definition in training and
#' inference, eliminating the distribution shift the raw 0/1 indicator
#' would otherwise introduce.
#'
#' The four "enriched" predictors (added in v0.30.23 over the prior
#' v0.30.20-21 baseline) -- long-memory antecedent precipitation at
#' 24-week and 52-week horizons, a region-conditional medium-window
#' precip smooth, and a precip-anomaly x soil-moisture-anomaly joint
#' interaction -- improve detection of major cyclone-driven flood events
#' (Idai 2019, Kenneth 2019, Eloise 2021, Ana 2022, Gombe 2022, Freddy
#' 2023). On a 10-cyclone benchmark this formulation lands every event
#' in the top 21\% of its country's historical predicted-probability
#' distribution (median 93rd percentile), the most reliable cyclone
#' detector across the rebuild strategies tested.
#'
#' @param d A data.frame with one row per (iso_code, year, week). Must
#'   contain the columns returned by
#'   \code{.impute_flood_probability_required()}: \code{iso_code},
#'   \code{year}, \code{week}, \code{date}; the binary target
#'   \code{emdat_flood_active} (0/1; NA in forecast rows is fine); the
#'   WHO subregion \code{region}; the precipitation channels
#'   \code{precipitation_sum}, \code{precip_anom},
#'   \code{precip_sum_2w/4w/8w/12w}, \code{precip_extreme_p90_count};
#'   soil moisture \code{soil_moisture_0_to_10cm_mean},
#'   \code{soil_moisture_anom}; \code{spei_approx}; atmospheric humidity
#'   \code{relative_humidity_2m_mean}, \code{rh_mean_12w}; storm proxy
#'   \code{wind_speed_10m_max}; and the teleconnection indices
#'   \code{ENSO3}, \code{ENSO34}, \code{ENSO4}, \code{IOD} (lags and
#'   long-window precip sums computed inline by the function).
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
#' The GAM is fit with \code{mgcv::bam()} (mgcv's big-data variant -- uses
#' block updates, discrete covariate binning, and parallel-friendly
#' linear algebra; on ~30k rows it returns in ~30-40 sec).
#' \code{family = stats::binomial(link = "logit")} produces predictions
#' naturally in \[0, 1\] without rescaling. \code{method = "fREML"},
#' \code{select = TRUE} (smoothness null-space shrinkage, the principled
#' mgcv mechanism for driving uninformative smooths toward zero -- replaces
#' the iterative in-sample-p-value pruning loop that v0.30.24 used).
#' Predictors computed inline (so the caller does not need to provide them):
#' \itemize{
#'   \item ENSO34 and IOD at lags 8, 16, 24 weeks (current value is the
#'     raw column).
#'   \item Long-memory antecedent precipitation:
#'     \code{precip_sum_24w} (~6-month basin saturation memory) and
#'     \code{precip_sum_52w} (annual).
#'   \item Joint climate index: \code{precip_anom * soil_moisture_anom}
#'     (very-wet AND already-saturated regime).
#' }
#' Region-conditional precip-sum-4w smooth requires a \code{region}
#' column on the input dataframe (4-region WHO classification:
#' Central / East / Southern / West Africa).
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
#' @importFrom slider slide_dbl
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

     if (verbose) message("Imputing flood probability via enriched binomial GAM...")

     # Inline-compute additional predictors the GAM needs but the saved
     # suitability CSV doesn't carry:
     #   * ENSO34 / IOD lags (current + 2/4/6 months)
     #   * Long-memory antecedent precipitation (24-week ~ 6 months;
     #     52-week annual) -- captures basin saturation memory.
     #   * Joint climate index: precip_anom * soil_moisture_anom captures
     #     the "very wet AND already saturated" regime.
     d_aug <- d %>%
          dplyr::group_by(iso_code) %>%
          dplyr::arrange(date, .by_group = TRUE) %>%
          dplyr::mutate(
               ENSO34_lag8        = dplyr::lag(ENSO34, n = 8),
               ENSO34_lag16       = dplyr::lag(ENSO34, n = 16),
               ENSO34_lag24       = dplyr::lag(ENSO34, n = 24),
               IOD_lag8           = dplyr::lag(IOD,    n = 8),
               IOD_lag16          = dplyr::lag(IOD,    n = 16),
               IOD_lag24          = dplyr::lag(IOD,    n = 24),
               precip_sum_24w     = slider::slide_dbl(precipitation_sum, sum,
                                                       .before = 23L, .complete = TRUE),
               precip_sum_52w     = slider::slide_dbl(precipitation_sum, sum,
                                                       .before = 51L, .complete = TRUE),
               precip_x_soil_anom = precip_anom * soil_moisture_anom
          ) %>%
          dplyr::ungroup()

     # Factor versions for the GAM's random effect + by-factor smooths
     d_aug$iso_code_f <- factor(d_aug$iso_code)
     d_aug$region_f   <- factor(d_aug$region)

     # Enriched binomial GAM with backward-elimination pruning loop
     # (v0.30.24). Same family + target as v0.30.23 (binomial / logit on
     # emdat_flood_active), same enriched predictors, but the formula
     # is constructed from term-string lists so a pruning loop can drop
     # non-significant predictors one at a time.
     #
     # History note (v0.30.23 -- reverted from the v0.30.22 Tweedie
     # severity formulation because cyclone-benchmark scoring showed the
     # binomial-on-active target gives more reliable detection across
     # Idai/Kenneth/Eloise/Ana/Gombe/Freddy. The Tweedie chase EM-DAT's
     # logged Total Affected, which under-counts events like Idai
     # relative to their real impact.):
     #   * Same family (binomial / logit) and same target
     #     (emdat_flood_active) as the v0.30.20-21 baseline.
     #   * Adds four physics-driven predictors found by the v0.30.22
     #     S2 exploration to be load-bearing for the cyclone benchmark:
     #       - precip_sum_24w (basin saturation memory ~ 6 mo)
     #       - precip_sum_52w (annual antecedent precipitation)
     #       - s(precip_sum_4w, by = region_f) (region-conditional rain
     #         -> flood mapping; East / West / Central / Southern Africa)
     #       - s(precip_x_soil_anom) (very-wet AND saturated regime)
     #   * No cyclic-week smooth: lets the climate predictors carry the
     #     within-year cycle directly.
     #   * No static elevation / urban_population_pct: country-level
     #     terrain/vulnerability differences are absorbed by the iso
     #     random effect.
     #   * Heavy ENSO/IOD bench: ENSO34 at current + 2/4/6-month lags,
     #     plus current ENSO3 / ENSO4, and IOD at the same lag horizons.
     #   * select = FALSE: the shrinkage penalty was suppressing the
     #     teleconnection terms; without it they keep their REML weight.
     # Full GAM formula. Two changes vs v0.30.24 (validated by independent
     # modeler + software-engineer agent experiments):
     #
     #   * te(precip_sum_4w, precip_sum_24w) tensor product replaces the
     #     two univariate smooths. The interaction "heavy 4-week rain
     #     ON an already-saturated catchment (6-month antecedent)" is
     #     genuinely nonlinear; the tensor captures it where the
     #     univariates plus precip_x_soil_anom only approximated it.
     #     Modeler-agent experiment: cyclone median 91.6 -> 93.8.
     #
     #   * select = TRUE (smoothness null-space shrinkage) replaces the
     #     iterative p-value pruning loop that v0.30.24 added. Both
     #     agents converged on this independently. select=TRUE is the
     #     principled mgcv mechanism for "drop terms that don't carry
     #     weight" -- in-sample p-values are explicitly
     #     multicollinearity-sensitive (documented caveat) and produced
     #     fragile pruning behavior. Net effect: identical CV AUC
     #     (~0.85), +2-4 percentile points on the worst-detected
     #     cyclone (Kenneth 2019), single 10-second fit replaces
     #     1+N_drops iterative fits.
     gam_formula <- emdat_flood_active ~
          # Country baseline
          s(iso_code_f, bs = "re") +
          # Precipitation channels (24w/4w jointly via tensor product)
          s(precipitation_sum) +
          s(precip_anom) +
          s(precip_sum_2w) +
          s(precip_sum_8w) +
          s(precip_sum_12w) +
          s(precip_sum_52w) +
          te(precip_sum_4w, precip_sum_24w, k = c(10, 10)) +
          precip_extreme_p90_count +
          # Region-conditional medium-window precip
          s(precip_sum_4w, by = region_f) +
          # Joint precip x soil_moisture anomaly (saturation interaction)
          s(precip_x_soil_anom) +
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

     # Training rows: where emdat_flood_active was observed (historical
     # EM-DAT panel coverage). The 24w / 52w smooths require lookback, so
     # drop the leading-edge rows where those columns are NA.
     train_idx <- !is.na(d_aug$emdat_flood_active) &
                  !is.na(d_aug$precip_sum_24w) &
                  !is.na(d_aug$precip_sum_52w) &
                  !is.na(d_aug$ENSO34_lag24) &
                  !is.na(d_aug$IOD_lag24)
     train <- d_aug[train_idx, , drop = FALSE]
     if (nrow(train) < 100) {
          stop("impute_flood_probability: only ", nrow(train),
               " training rows after lag/window warm-up; need >= 100.")
     }
     if (verbose) {
          message(sprintf("  Training rows: %d (active fraction: %.3f)",
                          nrow(train), mean(train$emdat_flood_active)))
     }

     # Single bam() fit with smoothness shrinkage (select=TRUE).
     # No iterative pruning loop -- the null-space penalty drives
     # uninformative smooths' coefficients toward zero in one shot.
     gam_model <- mgcv::bam(
          formula  = gam_formula,
          family   = stats::binomial(link = "logit"),
          data     = train,
          method   = "fREML",
          select   = TRUE,
          discrete = TRUE
     )
     if (verbose) {
          message(sprintf("  Deviance explained: %.1f%%",
                          summary(gam_model)$dev.expl * 100))
     }

     # Predict on every row (incl. forecast). Missing-predictor rows
     # (leading-edge warm-up etc.) return NA and fall through to the
     # sentinel below.
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

     # Binomial-logit predictions are naturally in [0, 1] -- no scaling
     # needed. Guard the contract: numeric, no NA, in [0, 1].
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
                         select   = TRUE, discrete = TRUE),
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
     c("iso_code", "year", "week", "date",
       # Binary target (0/1 flood active per country-week)
       "emdat_flood_active",
       # WHO subregion factor (for region-conditional smooths)
       "region",
       # Precipitation channels: raw + anomaly + rolling windows
       # (24w/52w/precip_x_soil_anom are computed inline by the imputer)
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
