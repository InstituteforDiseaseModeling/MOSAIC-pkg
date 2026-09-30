#' Impute Country-Week Drought Probability from a SPEI-Deficit Label
#'
#' Fits a binomial GAM that maps ENSO/IOD teleconnections and antecedent
#' temperature / precipitation-deficit onto a \strong{sustained-SPEI-deficit}
#' drought label, then predicts a continuous drought probability for every
#' (iso_code, year, week) row -- historical AND forecast. The value of the
#' GAM is the \emph{teleconnection-to-drought lead-time mapping}: it lets the
#' downstream psi LSTM see drought risk building from the slow ocean-state
#' drivers, months before the local moisture deficit fully develops.
#'
#' \strong{Label (derived here, NOT from EM-DAT events).} EM-DAT drought
#' records are unusable as a weekly label (median ~344-day duration, dozens of
#' multi-year events, ~18/40 countries "active" in a single week -- a constant
#' background wash). Instead the label is built from the panel's own
#' \code{spei_approx} (per-country standardized precipitation minus
#' evapotranspiration): a country-week is \code{drought_active = 1} when the
#' \strong{12-week rolling mean of \code{spei_approx} is at or below
#' \code{spei_threshold}} (default -0.8). The 12-week window enforces the
#' "sustained" requirement (a single dry week does not qualify) and the
#' rolling mean is the standard meteorological way to distinguish a drought
#' \emph{spell} from short-term dryness. On the production panel this yields
#' ~15% positive weeks across 38 countries and recovers the known
#' meteorological droughts (Southern Africa 2015-16 El Nino & 2018-19, Horn
#' of Africa 2016-17, the 2023-24 El Nino) -- NOT the smeared multi-year mess
#' the EM-DAT drought type produces.
#'
#' \strong{CRITICAL leakage control.} The concurrent \code{spei_approx} is the
#' label source, so it (and its rolling mean) is \strong{excluded} from the
#' predictor set -- predicting the label from its own generator would be
#' trivially circular and would defeat the entire purpose (the lead-time
#' mapping). The same applies to any local-climate series that overlaps the
#' label's \code{sustain_weeks} window (precipitation and temperature are the
#' ingredients of \code{spei_approx}), so every local-climate predictor is
#' lagged by \code{sustain_weeks} weeks: it describes conditions strictly
#' \emph{before} the label window. The predictors are exogenous ocean-state
#' teleconnections and genuinely antecedent temperature / precipitation only.
#'
#' @param d A data.frame with one row per (iso_code, year, week). Must
#'   contain the columns returned by
#'   \code{.impute_drought_probability_required()}: \code{iso_code},
#'   \code{year}, \code{week}, \code{date}; \code{spei_approx} (used ONLY to
#'   build the label, never as a predictor); antecedent
#'   \code{temp_anom}, \code{precip_anom}, \code{precip_sum_12w},
#'   \code{precipitation_sum}; the WHO subregion \code{region}; and the
#'   teleconnection indices \code{ENSO34}, \code{IOD} (lags computed inline).
#' @param spei_threshold Numeric. The 12-week rolling-mean \code{spei_approx}
#'   at or below which a country-week is labeled drought-active. Default
#'   \code{-0.8} (justified above; see \code{sustain_weeks}).
#' @param sustain_weeks Integer. Rolling-mean window (weeks) enforcing the
#'   "sustained" requirement. Default \code{12L}.
#' @param output_col Character. Name of the new probability column.
#'   Default \code{"drought_prob"}.
#' @param gam_train_stop Date or character (\code{"YYYY-MM-DD"}) or
#'   \code{NULL}. When non-\code{NULL}, the binomial GAM is FIT only on rows
#'   with \code{date <= gam_train_stop}; the fitted model then PREDICTS every
#'   row (rows \code{<= gam_train_stop} in-sample, rows \code{> gam_train_stop}
#'   leak-free extrapolation). The leakage-hygiene hook for rolling-origin
#'   forecast CV. The SPEI-deficit LABEL's rolling mean (an input transform)
#'   is unaffected -- only the GAM fit-row subset is capped. Default
#'   \code{NULL} = current full-data fit (back-compatible). Only the fit-row
#'   subset changes; \code{select=TRUE}/fREML/the formula are identical.
#' @param climate_obs_stop Date/character, or a Date vector named by \code{iso_code}. Last observed-climate date; rows after it are never fit. \code{NULL} (default) = no cap.
#' @param integrator_col Character. Name of the slow long-memory integrator
#'   column. Default \code{"drought_prob_26w_mean"}.
#' @param integrator_weeks Integer. Trailing window (weeks) for the slow
#'   integrator. Default \code{26L} (~half a year; see Details).
#' @param diagnostics Logical. If \code{TRUE}, fits a rolling-year
#'   cross-validation and writes diagnostic artefacts to \code{diag_dir}.
#' @param diag_dir Character. Directory for diagnostic outputs (created if
#'   missing). Ignored when \code{diagnostics = FALSE}.
#' @param verbose Logical. Echo progress messages.
#'
#' @return The input data.frame with TWO added columns: \code{output_col}
#'   (per-week probability, non-NA numeric in \[0, 1\]) and
#'   \code{integrator_col} (trailing-window mean of the probability, the slow
#'   long-memory drought-state channel, also non-NA in \[0, 1\]).
#'
#' @details
#' The GAM is fit with \code{mgcv::bam()},
#' \code{family = stats::binomial(link = "logit")}, \code{method = "fREML"},
#' \code{select = TRUE} (smoothness null-space shrinkage). Predictors:
#' \itemize{
#'   \item ENSO34 and IOD at lags 8, 16, 24 weeks (the slow ocean-state
#'     drivers that LEAD drought by a season or two -- this lead time is the
#'     product the GAM sells to the LSTM).
#'   \item Antecedent \code{temp_anom} (heat amplifies evaporative demand),
#'     \code{precip_anom}, and long-window antecedent precipitation
#'     \code{precip_sum_12w} / \code{precip_sum_24w} (accumulated rainfall
#'     deficit; 24w computed inline), each lagged by \code{sustain_weeks} so it
#'     ends the week before the label window begins.
#'   \item \code{s(iso_code_f, bs = "re")} country random effect (baseline
#'     aridity / drought propensity).
#' }
#' The concurrent \code{spei_approx} and its rolling mean are NOT predictors
#' (label leakage; see the leakage-control note above). Fit rows are limited to
#' \code{date <= climate_obs_stop} (and \code{<= gam_train_stop} when set). A
#' warning is raised if \code{bam()} reports non-convergence. The output is
#' returned in the input's row order, whatever that order is.
#'
#' \strong{Slow integrator.} Drought is a persistent state whose cholera
#' relevance accumulates (WASH strain, water-source concentration, migration).
#' A single per-week probability under-represents that memory, so a trailing
#' \code{integrator_weeks}-week mean of the probability is emitted alongside.
#' 26 weeks (~half a year) is chosen because it spans a full dry-season build
#' without bleeding a prior year's drought into the next -- long enough to be
#' a genuine slow channel, short enough to remain seasonally resolved. It is a
#' backward-looking rolling mean (no future leakage) and warm-up NAs are
#' filled with the country mean.
#'
#' Sentinel handling mirrors the flood/cyclone imputers: NA predictions are
#' filled with the country mean, then the global mean. Both output columns are
#' asserted numeric, in \[0, 1\], NA-free.
#'
#' @seealso \code{\link{impute_flood_probability}},
#'   \code{\link{impute_cyclone_probability}},
#'   \code{\link{compile_suitability_data}}
#'
#' @importFrom mgcv bam s
#' @importFrom dplyr group_by arrange mutate lag ungroup
#' @importFrom slider slide_dbl
#' @importFrom stats binomial predict
#' @importFrom utils write.csv capture.output
#' @importFrom grDevices png dev.off
#' @importFrom graphics plot abline
#' @export
impute_drought_probability <- function(d,
                                        spei_threshold   = -0.8,
                                        sustain_weeks    = 12L,
                                        output_col       = "drought_prob",
                                        integrator_col   = "drought_prob_26w_mean",
                                        integrator_weeks = 26L,
                                        gam_train_stop   = NULL,
                                        climate_obs_stop = NULL,
                                        diagnostics      = TRUE,
                                        diag_dir         = NULL,
                                        verbose          = TRUE) {

     required <- .impute_drought_probability_required()
     missing_cols <- setdiff(required, names(d))
     if (length(missing_cols) > 0) {
          stop("impute_drought_probability: missing required column(s): ",
               paste(missing_cols, collapse = ", "))
     }

     if (verbose) message("Imputing drought probability via teleconnection binomial GAM...")

     # ---- Build the sustained-SPEI-deficit label + inline predictors ----
     # spei_approx is used ONLY to build the label; it and its rolling mean
     # are deliberately kept OUT of the predictor set (leakage control).
     # .orig_row carries each row's position in `d` through the per-country
     # sort, so predictions are written back to the rows they belong to.
     d_aug <- d
     d_aug$.orig_row <- seq_len(nrow(d))
     d_aug <- d_aug %>%
          dplyr::group_by(iso_code) %>%
          dplyr::arrange(date, .by_group = TRUE) %>%
          dplyr::mutate(
               spei_roll = slider::slide_dbl(spei_approx, mean,
                                             .before = sustain_weeks - 1L,
                                             .complete = TRUE),
               ENSO34_lag8    = dplyr::lag(ENSO34, n = 8),
               ENSO34_lag16   = dplyr::lag(ENSO34, n = 16),
               ENSO34_lag24   = dplyr::lag(ENSO34, n = 24),
               IOD_lag8       = dplyr::lag(IOD,    n = 8),
               IOD_lag16      = dplyr::lag(IOD,    n = 16),
               precip_sum_24w = slider::slide_dbl(precipitation_sum, sum,
                                                  .before = 23L, .complete = TRUE),
               # Local climate lagged past the label window (see leakage control)
               temp_anom_ante      = dplyr::lag(temp_anom,      n = sustain_weeks),
               precip_anom_ante    = dplyr::lag(precip_anom,    n = sustain_weeks),
               precip_sum_12w_ante = dplyr::lag(precip_sum_12w, n = sustain_weeks),
               precip_sum_24w_ante = dplyr::lag(precip_sum_24w, n = sustain_weeks)
          ) %>%
          dplyr::ungroup()

     d_aug$drought_active <- as.integer(!is.na(d_aug$spei_roll) &
                                        d_aug$spei_roll <= spei_threshold)
     # Where the rolling SPEI is NA (warm-up edge) the label is undefined, not 0.
     d_aug$drought_active[is.na(d_aug$spei_roll)] <- NA_integer_

     d_aug$iso_code_f <- factor(d_aug$iso_code)
     d_aug$region_f   <- factor(d_aug$region)

     if (verbose) {
          n_pos <- sum(d_aug$drought_active, na.rm = TRUE)
          n_lab <- sum(!is.na(d_aug$drought_active))
          message(sprintf(
               "  Label: 12w-rollmean(spei_approx) <= %.2f -> %d positive / %d labeled weeks (%.1f%%), %d countries",
               spei_threshold, n_pos, n_lab, 100 * n_pos / n_lab,
               length(unique(d_aug$iso_code[!is.na(d_aug$drought_active) &
                                            d_aug$drought_active == 1]))))
     }

     # Teleconnection + antecedent-deficit GAM. spei_approx EXCLUDED.
     gam_formula <- drought_active ~
          # Country baseline aridity / drought propensity
          s(iso_code_f, bs = "re") +
          # Slow ocean-state teleconnections that LEAD drought (the product)
          s(ENSO34) +
          s(ENSO34_lag8) +
          s(ENSO34_lag16) +
          s(ENSO34_lag24) +
          s(IOD) +
          s(IOD_lag8) +
          s(IOD_lag16) +
          # Antecedent temperature / precip, lagged past the label window
          s(temp_anom_ante) +
          s(precip_anom_ante) +
          s(precip_sum_12w_ante) +
          s(precip_sum_24w_ante)

     train_idx <- !is.na(d_aug$drought_active) &
                  !is.na(d_aug$ENSO34_lag24) &
                  !is.na(d_aug$IOD_lag16) &
                  !is.na(d_aug$precip_sum_24w_ante) &
                  !is.na(d_aug$temp_anom_ante) &
                  !is.na(d_aug$precip_anom_ante) &
                  !is.na(d_aug$precip_sum_12w_ante) &
                  !is.na(d_aug$date)
     row_date <- as.Date(d_aug$date)
     # Never fit on projected climate / forecast teleconnections. The horizon
     # is data-derived by the caller (see .drought_climate_obs_stop), so the
     # fit depends on the inputs, not on the day the code runs. A country
     # absent from a per-country horizon has no observed climate: not fit.
     if (!is.null(climate_obs_stop)) {
          obs_stop <- .drought_row_horizon(climate_obs_stop, d_aug$iso_code)
          train_idx <- train_idx & ((row_date <= obs_stop) %in% TRUE)
     }
     # Leakage gate: restrict the FIT to rows on/before the cutoff when
     # gam_train_stop is supplied. The SPEI-deficit label (built above) and
     # the predict step below are unaffected -- only the fit rows are capped.
     if (!is.null(gam_train_stop)) {
          train_idx <- train_idx & ((row_date <= as.Date(gam_train_stop)) %in% TRUE)
     }
     train <- d_aug[train_idx, , drop = FALSE]
     if (nrow(train) < 100) {
          stop("impute_drought_probability: only ", nrow(train),
               " training rows after lag/window warm-up; need >= 100.")
     }
     if (verbose) {
          message(sprintf("  Training rows: %d (active fraction: %.3f)",
                          nrow(train), mean(train$drought_active)))
     }

     gam_model <- mgcv::bam(
          formula  = gam_formula,
          family   = stats::binomial(link = "logit"),
          data     = train,
          method   = "fREML",
          select   = TRUE,
          discrete = TRUE
     )
     if (!isTRUE(gam_model$converged)) {
          warning("impute_drought_probability: mgcv::bam() did not converge; ",
                  "drought probabilities may be unreliable.")
     }
     if (verbose) {
          message(sprintf("  Deviance explained: %.1f%%",
                          summary(gam_model)$dev.expl * 100))
     }

     preds <- as.numeric(stats::predict(gam_model, newdata = d_aug,
                                         type = "response"))

     # Sentinel: country mean for residual NAs; global mean as last resort.
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

     d[[output_col]] <- NA_real_
     d[[output_col]][d_aug$.orig_row] <- preds

     # ---- Slow long-memory integrator: trailing-window mean of the prob ----
     # Carry a stable original-row index through the group/arrange so the
     # rolling result maps back to d's ORIGINAL row order unambiguously (dplyr
     # re-sorts by iso/date internally for the trailing window).
     d_tmp <- data.frame(.orig_row = seq_len(nrow(d)),
                         iso_code  = d$iso_code,
                         date      = d$date,
                         .dp       = d[[output_col]],
                         stringsAsFactors = FALSE)
     d_tmp <- d_tmp %>%
          dplyr::group_by(iso_code) %>%
          dplyr::arrange(date, .by_group = TRUE) %>%
          dplyr::mutate(
               .integ = slider::slide_dbl(.dp, mean,
                                          .before = integrator_weeks - 1L,
                                          .complete = TRUE)
          ) %>%
          dplyr::ungroup()
     integ <- d_tmp$.integ
     # Warm-up NAs -> country mean of the integrator, then global mean. The
     # trailing integrator itself is backward-looking (no future leakage), but
     # its warm-up sentinel means are masked to <=stop when gam_train_stop is
     # set so a <=stop warm-up row is never filled from post-cutoff rows.
     integ_mask <- if (is.null(gam_train_stop)) {
          rep(TRUE, length(integ))
     } else {
          as.Date(d_tmp$date) <= as.Date(gam_train_stop)
     }
     imean <- tapply(integ[integ_mask], d_tmp$iso_code[integ_mask], mean, na.rm = TRUE)
     ina <- is.na(integ)
     if (any(ina)) integ[ina] <- imean[as.character(d_tmp$iso_code[ina])]
     ina2 <- is.na(integ)
     if (any(ina2)) integ[ina2] <- mean(integ[integ_mask], na.rm = TRUE)
     stopifnot(!any(is.na(integ)), all(integ >= 0), all(integ <= 1))
     # Scatter back to original row positions via the carried index.
     d[[integrator_col]] <- NA_real_
     d[[integrator_col]][d_tmp$.orig_row] <- integ
     stopifnot(!any(is.na(d[[integrator_col]])))

     if (isTRUE(diagnostics)) {
          if (is.null(diag_dir)) {
               diag_dir <- file.path(tempdir(), "drought_imputation")
          }
          dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
          .write_drought_gam_diagnostics(gam_model, train, d_aug, preds,
                                         diag_dir, verbose)
     }

     d
}


# Internal: diagnostic artefacts for the drought GAM.
.write_drought_gam_diagnostics <- function(gam_model, train, d_aug, preds,
                                           diag_dir, verbose) {

     png(file.path(diag_dir, "drought_gam_smooths.png"),
         width = 1400, height = 1000, res = 120)
     tryCatch({
          plot(gam_model, pages = 1)
     }, error = function(e) {
          message("    diagnostic: plot(gam) failed: ", conditionMessage(e))
     })
     dev.off()

     train_pred <- as.numeric(stats::predict(gam_model, newdata = train,
                                              type = "response"))
     brks <- unique(stats::quantile(train_pred, probs = seq(0, 1, 0.1), na.rm = TRUE))
     if (length(brks) >= 3) {
          deciles <- cut(train_pred, breaks = brks, include.lowest = TRUE)
          calib <- data.frame(
               predicted = tapply(train_pred,             deciles, mean, na.rm = TRUE),
               observed  = tapply(train$drought_active,    deciles, mean, na.rm = TRUE)
          )
          png(file.path(diag_dir, "drought_gam_calibration.png"),
              width = 800, height = 800, res = 120)
          plot(calib$predicted, calib$observed,
               xlim = c(0, 1), ylim = c(0, 1),
               xlab = "Mean predicted drought probability (decile)",
               ylab = "Observed drought rate (decile)",
               main = "Drought-prob GAM calibration",
               pch = 19)
          abline(0, 1, lty = 2)
          dev.off()
     }

     years <- sort(unique(d_aug$year[!is.na(d_aug$drought_active)]))
     candidate_years <- years[years >= 2018 & years <= max(years) - 1]
     cv_years <- utils::tail(candidate_years, 3)
     cv_rows <- list()
     for (yr in cv_years) {
          tr <- train[train$year <  yr, , drop = FALSE]
          te <- train[train$year == yr, , drop = FALSE]
          if (nrow(tr) < 100 || nrow(te) < 20 ||
              length(unique(te$drought_active)) < 2) next
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
          y <- te$drought_active
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
                           file.path(diag_dir, "drought_gam_cv_metrics.csv"),
                           row.names = FALSE)
          mean_auc <- mean(cv_df$auc, na.rm = TRUE)
          if (verbose) {
               message(sprintf("  Rolling-year CV mean AUC: %.3f (n=%d folds)",
                               mean_auc, nrow(cv_df)))
          }
          if (is.finite(mean_auc) && mean_auc < 0.65) {
               warning(sprintf(
                    "Drought-prob GAM mean CV AUC = %.3f < 0.65: teleconnection lead-time signal is weak.",
                    mean_auc))
          }
     }

     writeLines(utils::capture.output(summary(gam_model)),
                con = file.path(diag_dir, "drought_gam_summary.txt"))

     if (verbose) message("  Diagnostics written to: ", diag_dir)
     invisible(NULL)
}


# Internal: canonical list of columns the drought-prob GAM consumes (as
# LABEL source + predictors). Sourced by both the imputer and the upstream
# gate in compile_suitability_data() to prevent drift.
.impute_drought_probability_required <- function() {
     c("iso_code", "year", "week", "date",
       # Label source (used to build drought_active; NOT a predictor)
       "spei_approx",
       # WHO subregion factor
       "region",
       # Antecedent deficit predictors
       "temp_anom", "precip_anom", "precip_sum_12w", "precipitation_sum",
       # Teleconnections (lags computed inline by the imputer)
       "ENSO34", "IOD")
}


#' Per-row observed-climate horizon for impute_drought_probability()
#'
#' @param climate_obs_stop Scalar date, or a date vector named by iso_code.
#' @param iso Character vector of row iso codes.
#' @return Date vector, one per row (NA where a named horizon lacks the iso).
#' @keywords internal
#' @noRd
.drought_row_horizon <- function(climate_obs_stop, iso) {
     h <- as.Date(climate_obs_stop)
     if (is.null(names(climate_obs_stop))) {
          if (length(h) != 1L || is.na(h)) {
               stop("climate_obs_stop must be one non-NA date or a date vector named by iso_code.",
                    call. = FALSE)
          }
          return(rep(h, length(iso)))
     }
     names(h) <- names(climate_obs_stop)
     unname(h[as.character(iso)])
}


#' Data-derived observed-climate horizon for the drought GAM fit
#'
#' The weekly suitability panel carries ERA5 observations up to each
#' country's last ERA5 pull and climate-model projections after it
#' (process_open_meteo_data splices at era5_max_date), plus ENSO/IOD that turn
#' from observed into forecast values. This returns, per country, the earlier
#' of (a) the country's last ERA5 date, read from the newest raw historical
#' parquet under \code{PATHS$OPEN_METEO_REPO/data/historical/<ISO>/}, and
#' (b) the last week in which every teleconnection index is observed
#' (\code{data_source} "historical" or "observed" in
#' \code{PATHS$DATA_ENSO/enso_weekly.csv}). Both inputs are read, never written.
#'
#' @param PATHS List from get_paths().
#' @return Date vector named by iso_code; a scalar Date if the ERA5 archive is
#'   not available (teleconnection horizon only, with a warning); NULL (with a
#'   warning) if neither horizon can be determined.
#' @keywords internal
#' @noRd
.drought_climate_obs_stop <- function(PATHS) {
     tele <- NA
     enso_file <- file.path(PATHS$DATA_ENSO %||% "", "enso_weekly.csv")
     if (file.exists(enso_file)) {
          e <- utils::read.csv(enso_file, stringsAsFactors = FALSE)
          if (all(c("variable", "data_source", "date_stop") %in% names(e))) {
               obs <- e[e$data_source %in% c("historical", "observed"), , drop = FALSE]
               if (nrow(obs)) {
                    tele <- min(tapply(as.Date(obs$date_stop), obs$variable, max, na.rm = TRUE))
                    tele <- as.Date(tele, origin = "1970-01-01")
               }
          }
     }

     era5 <- NULL
     hist_dir <- file.path(PATHS$OPEN_METEO_REPO %||% "", "data", "historical")
     if (dir.exists(hist_dir)) {
          isos <- sort(basename(list.dirs(hist_dir, recursive = FALSE)))
          era5 <- vapply(isos, function(iso) {
               f <- sort(list.files(file.path(hist_dir, iso), pattern = "\\.parquet$",
                                    full.names = TRUE), decreasing = TRUE)
               if (!length(f)) return(NA_real_)
               dts <- arrow::read_parquet(f[1L], col_select = "date")$date
               as.numeric(max(as.Date(dts), na.rm = TRUE))
          }, numeric(1))
          era5 <- as.Date(era5[!is.na(era5)], origin = "1970-01-01")
          if (!length(era5)) era5 <- NULL
     }

     if (is.null(era5)) {
          if (is.na(tele)) {
               warning("Observed-climate horizon unknown (no ERA5 archive, no observed ENSO/IOD); ",
                       "the drought GAM is fit on every labelled row, projections included.",
                       call. = FALSE)
               return(NULL)
          }
          warning("ERA5 archive not found at ", hist_dir, "; drought GAM fit capped at the ",
                  "teleconnection horizon ", format(tele), " only.", call. = FALSE)
          return(tele)
     }
     if (!is.na(tele)) era5[] <- pmin(era5, tele)
     era5
}
