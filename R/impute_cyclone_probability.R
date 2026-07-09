#' Impute Country-Week Tropical-Cyclone Probability from EM-DAT and Climate
#'
#' Fits a binomial GAM on observed EM-DAT tropical-cyclone / storm-surge
#' events (\code{emdat_cyclone_active}, 0/1) as a function of a
#' \strong{wind/coastal} mechanistic predictor set that is deliberately
#' distinct from the precipitation-driven flood GAM. The key proxy is the
#' weekly maximum 10m wind speed (\code{wind_speed_10m_max}); precipitation
#' channels capture the co-arriving surge/rain, and ENSO/IOD teleconnection
#' lags capture the seasonal steering of cyclone tracks into the Southwest
#' Indian Ocean / Mozambique Channel. The fitted model predicts a
#' continuous cyclone probability for every (iso_code, year, week) row --
#' historical AND forecast -- so the downstream psi LSTM consumes the same
#' feature definition in training and inference.
#'
#' \strong{Why a separate GAM (not folded into the flood label).} Tropical
#' cyclones carry the opposite climate signature from ordinary
#' precipitation floods: a sharp wind spike with a short, crisply-dated
#' footprint. The flood-only EM-DAT filter historically dropped all 49
#' tropical-cyclone / surge events, which neutered the flood GAM's wind
#' smooth. Modeling cyclones with their own wind-led GAM restores that
#' signal as a clean input channel; the psi LSTM recombines the flood and
#' cyclone probabilities downstream (the shared cholera-contamination
#' mechanism lives in the LSTM, not in the label).
#'
#' @param d A data.frame with one row per (iso_code, year, week). Must
#'   contain the columns returned by
#'   \code{.impute_cyclone_probability_required()}: \code{iso_code},
#'   \code{year}, \code{week}, \code{date}; the binary target
#'   \code{emdat_cyclone_active} (0/1; NA in forecast rows is fine); the
#'   WHO subregion \code{region}; the storm proxy \code{wind_speed_10m_max};
#'   precipitation channels \code{precipitation_sum}, \code{precip_sum_2w},
#'   \code{precip_sum_4w}; and the teleconnection indices \code{ENSO34},
#'   \code{IOD} (lags computed inline by the function).
#' @param output_col Character. Name of the new probability column.
#'   Default \code{"emdat_cyclone_prob"}.
#' @param gam_train_stop Date or character (\code{"YYYY-MM-DD"}) or
#'   \code{NULL}. When non-\code{NULL}, the binomial GAM is FIT only on rows
#'   with \code{date <= gam_train_stop}; the fitted model then PREDICTS every
#'   row (rows \code{<= gam_train_stop} in-sample, rows \code{> gam_train_stop}
#'   leak-free extrapolation). The leakage-hygiene hook for rolling-origin
#'   forecast CV. Default \code{NULL} = current full-data fit (back-compatible).
#'   Only the fit-row subset changes; \code{select=TRUE}/fREML/the formula are
#'   identical.
#' @param diagnostics Logical. If \code{TRUE}, fits a rolling-year
#'   cross-validation and writes diagnostic artefacts to \code{diag_dir}.
#' @param diag_dir Character. Directory for diagnostic outputs (created if
#'   missing). Ignored when \code{diagnostics = FALSE}.
#' @param verbose Logical. Echo progress messages.
#'
#' @return The input data.frame with one added column named
#'   \code{output_col} carrying a non-NA numeric in \[0, 1\] for every row.
#'
#' @details
#' The GAM is fit with \code{mgcv::bam()},
#' \code{family = stats::binomial(link = "logit")} (predictions naturally in
#' \[0, 1\]), \code{method = "fREML"}, \code{select = TRUE} (smoothness
#' null-space shrinkage -- drives uninformative smooths toward zero, the same
#' principled mechanism used by \code{\link{impute_flood_probability}}).
#' Predictors computed inline (the caller need not supply them):
#' \itemize{
#'   \item ENSO34 and IOD at lags 8, 16, 24 weeks (seasonal cyclone-season
#'     steering; SWIO cyclone activity co-varies with ENSO/IOD phase).
#'   \item Nothing else -- the wind and short-window precip channels are
#'     the acute proxy; the country random effect absorbs baseline
#'     coastal-exposure differences.
#' }
#'
#' The active fraction is very low (~74 weeks out of ~35k country-weeks,
#' ~0.2\%). The country random effect \code{s(iso_code_f, bs = "re")}
#' concentrates predicted probability on the historically cyclone-exposed
#' coastal countries (MOZ, MWI, ZWE, MDG-adjacent belt) and keeps landlocked
#' / non-cyclone countries near zero. A region-conditional wind smooth
#' (\code{s(wind_speed_10m_max, by = region_f)}) lets Southern Africa (the
#' cyclone belt) map wind to risk more steeply than the other subregions.
#'
#' Sentinel handling mirrors the flood imputer: any NA prediction (lag
#' warm-up edge, residual missing predictor) is filled with that country's
#' mean predicted probability; global mean is the last resort. The function
#' asserts the final column is numeric, in \[0, 1\], and NA-free.
#'
#' If \code{diagnostics = TRUE}, a rolling-year CV (3 most-recent
#' fully-observed years) writes per-fold AUC / Brier / log-loss to
#' \code{cyclone_gam_cv_metrics.csv}. A mean CV AUC below 0.65 emits a
#' warning but the column is still returned.
#'
#' @seealso \code{\link{impute_flood_probability}},
#'   \code{\link{compile_suitability_data}}, \code{\link{process_EMDAT_data}}
#'
#' @importFrom mgcv bam s
#' @importFrom dplyr group_by arrange mutate lag ungroup
#' @importFrom stats binomial predict
#' @importFrom utils write.csv capture.output
#' @importFrom grDevices png dev.off
#' @importFrom graphics plot abline
#' @export
impute_cyclone_probability <- function(d,
                                        output_col     = "emdat_cyclone_prob",
                                        gam_train_stop = NULL,
                                        diagnostics    = TRUE,
                                        diag_dir       = NULL,
                                        verbose        = TRUE) {

     required <- .impute_cyclone_probability_required()
     missing_cols <- setdiff(required, names(d))
     if (length(missing_cols) > 0) {
          stop("impute_cyclone_probability: missing required column(s): ",
               paste(missing_cols, collapse = ", "))
     }

     if (verbose) message("Imputing cyclone probability via wind/coastal binomial GAM...")

     # Inline-compute teleconnection lags the GAM needs but the saved
     # suitability CSV doesn't necessarily carry at these horizons.
     d_aug <- d %>%
          dplyr::group_by(iso_code) %>%
          dplyr::arrange(date, .by_group = TRUE) %>%
          dplyr::mutate(
               ENSO34_lag8  = dplyr::lag(ENSO34, n = 8),
               ENSO34_lag16 = dplyr::lag(ENSO34, n = 16),
               ENSO34_lag24 = dplyr::lag(ENSO34, n = 24),
               IOD_lag8     = dplyr::lag(IOD,    n = 8),
               IOD_lag16    = dplyr::lag(IOD,    n = 16)
          ) %>%
          dplyr::ungroup()

     d_aug$iso_code_f <- factor(d_aug$iso_code)
     d_aug$region_f   <- factor(d_aug$region)

     # Wind/coastal binomial GAM. Distinct from the flood GAM: wind is the
     # lead proxy (region-conditional so the Southern-Africa cyclone belt
     # maps wind->risk more steeply), short-window precip captures the
     # co-arriving surge/rain, ENSO/IOD lags capture cyclone-season steering,
     # and the iso random effect absorbs coastal-exposure baseline.
     gam_formula <- emdat_cyclone_active ~
          # Country baseline (concentrates risk on cyclone-exposed coasts)
          s(iso_code_f, bs = "re") +
          # Wind proxy: overall + region-conditional (Southern-Africa belt)
          s(wind_speed_10m_max) +
          s(wind_speed_10m_max, by = region_f) +
          # Surge / co-arriving rain (short windows)
          s(precipitation_sum) +
          s(precip_sum_2w) +
          s(precip_sum_4w) +
          # Cyclone-season teleconnection steering
          s(ENSO34) +
          s(ENSO34_lag8) +
          s(ENSO34_lag16) +
          s(ENSO34_lag24) +
          s(IOD) +
          s(IOD_lag8) +
          s(IOD_lag16)

     # Training rows: where emdat_cyclone_active was observed and the
     # longest lag (24w) has warmed up.
     train_idx <- !is.na(d_aug$emdat_cyclone_active) &
                  !is.na(d_aug$ENSO34_lag24) &
                  !is.na(d_aug$IOD_lag16) &
                  !is.na(d_aug$wind_speed_10m_max)
     # Leakage gate: restrict the FIT to rows on/before the cutoff when
     # gam_train_stop is supplied; predict still covers every row.
     if (!is.null(gam_train_stop)) {
          train_idx <- train_idx &
               (as.Date(d_aug$date) <= as.Date(gam_train_stop))
     }
     train <- d_aug[train_idx, , drop = FALSE]
     if (nrow(train) < 100) {
          stop("impute_cyclone_probability: only ", nrow(train),
               " training rows after lag warm-up; need >= 100.")
     }
     if (verbose) {
          message(sprintf("  Training rows: %d (active fraction: %.4f, %d active weeks)",
                          nrow(train), mean(train$emdat_cyclone_active),
                          sum(train$emdat_cyclone_active)))
     }

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

     preds <- as.numeric(stats::predict(gam_model, newdata = d_aug,
                                         type = "response"))

     # Sentinel: country mean prob for residual NAs; global mean as last resort.
     # Leakage control: when gam_train_stop is set, the sentinel means are
     # computed over <=stop predictions ONLY (see impute_flood_probability).
     sentinel_mask <- if (is.null(gam_train_stop)) {
          rep(TRUE, length(preds))
     } else {
          as.Date(d_aug$date) <= as.Date(gam_train_stop)
     }
     country_mean <- tapply(preds[sentinel_mask],
                            d_aug$iso_code[sentinel_mask], mean, na.rm = TRUE)
     na_rows <- is.na(preds)
     if (any(na_rows)) {
          preds[na_rows] <- country_mean[as.character(d_aug$iso_code[na_rows])]
     }
     still_na <- is.na(preds)
     if (any(still_na)) {
          preds[still_na] <- mean(preds[sentinel_mask], na.rm = TRUE)
     }

     stopifnot(!any(is.na(preds)), all(preds >= 0), all(preds <= 1))

     d[[output_col]] <- preds

     if (isTRUE(diagnostics)) {
          if (is.null(diag_dir)) {
               diag_dir <- file.path(tempdir(), "cyclone_imputation")
          }
          dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
          .write_cyclone_gam_diagnostics(gam_model, train, d_aug, preds,
                                         diag_dir, verbose)
     }

     d
}


# Internal: diagnostic artefacts for the cyclone GAM (parallel to the flood
# imputer's diagnostics, minus the ggplot time-series which is not needed
# for the sparse cyclone label).
.write_cyclone_gam_diagnostics <- function(gam_model, train, d_aug, preds,
                                           diag_dir, verbose) {

     # 1. Smooth-term diagnostics
     png(file.path(diag_dir, "cyclone_gam_smooths.png"),
         width = 1400, height = 1000, res = 120)
     tryCatch({
          plot(gam_model, pages = 1)
     }, error = function(e) {
          message("    diagnostic: plot(gam) failed: ", conditionMessage(e))
     })
     dev.off()

     # 2. Calibration: decile-bin predicted vs observed
     train_pred <- as.numeric(stats::predict(gam_model, newdata = train,
                                              type = "response"))
     brks <- stats::quantile(train_pred, probs = seq(0, 1, 0.1), na.rm = TRUE)
     brks <- unique(brks)
     if (length(brks) >= 3) {
          deciles <- cut(train_pred, breaks = brks, include.lowest = TRUE)
          calib <- data.frame(
               predicted = tapply(train_pred,                   deciles, mean, na.rm = TRUE),
               observed  = tapply(train$emdat_cyclone_active,    deciles, mean, na.rm = TRUE)
          )
          png(file.path(diag_dir, "cyclone_gam_calibration.png"),
              width = 800, height = 800, res = 120)
          plot(calib$predicted, calib$observed,
               xlim = c(0, max(calib$predicted, na.rm = TRUE)),
               ylim = c(0, max(calib$observed,  na.rm = TRUE)),
               xlab = "Mean predicted cyclone probability (decile)",
               ylab = "Observed cyclone rate (decile)",
               main = "Cyclone-prob GAM calibration",
               pch = 19)
          abline(0, 1, lty = 2)
          dev.off()
     }

     # 3. Rolling-year CV (3 most-recent fully-observed years)
     years <- sort(unique(d_aug$year[!is.na(d_aug$emdat_cyclone_active)]))
     candidate_years <- years[years >= 2018 & years <= max(years) - 1]
     cv_years <- utils::tail(candidate_years, 3)
     cv_rows <- list()
     for (yr in cv_years) {
          tr <- train[train$year <  yr, , drop = FALSE]
          te <- train[train$year == yr, , drop = FALSE]
          if (nrow(tr) < 100 || nrow(te) < 20 ||
              length(unique(te$emdat_cyclone_active)) < 2) next
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
          y <- te$emdat_cyclone_active
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
                           file.path(diag_dir, "cyclone_gam_cv_metrics.csv"),
                           row.names = FALSE)
          mean_auc <- mean(cv_df$auc, na.rm = TRUE)
          if (verbose) {
               message(sprintf("  Rolling-year CV mean AUC: %.3f (n=%d folds)",
                               mean_auc, nrow(cv_df)))
          }
          if (is.finite(mean_auc) && mean_auc < 0.65) {
               warning(sprintf(
                    "Cyclone-prob GAM mean CV AUC = %.3f < 0.65: imputation may add limited signal beyond climatology.",
                    mean_auc))
          }
     }

     # 4. Summary text
     writeLines(utils::capture.output(summary(gam_model)),
                con = file.path(diag_dir, "cyclone_gam_summary.txt"))

     if (verbose) {
          message("  Diagnostics written to: ", diag_dir)
     }
     invisible(NULL)
}


# Internal: canonical list of columns the cyclone-prob GAM consumes. Both
# `impute_cyclone_probability()` and the upstream gate in
# `compile_suitability_data()` source the list from here so the two cannot
# drift out of sync as the GAM formula evolves.
.impute_cyclone_probability_required <- function() {
     c("iso_code", "year", "week", "date",
       # Binary target (0/1 cyclone active per country-week)
       "emdat_cyclone_active",
       # WHO subregion factor (region-conditional wind smooth)
       "region",
       # Wind proxy (weekly max 10m wind)
       "wind_speed_10m_max",
       # Surge / co-arriving rain (short windows)
       "precipitation_sum", "precip_sum_2w", "precip_sum_4w",
       # Teleconnections (lags computed inline by the imputer)
       "ENSO34", "IOD")
}
