#' Per-country logit-scale bias-correction of suitability (psi) predictions
#'
#' Fits a per-country affine recalibration on the \strong{logit scale},
#' \eqn{logit(obs) \approx a \cdot logit(pred) + b}, using \strong{outbreak
#' (non-zero observed) weeks within the training window only}, and applies it to
#' all predictions via the inverse logit. Addresses the systematic
#' negative/midrange bias (right shape, wrong scale) of the suitability LSTM
#' without altering its training, and feeds the canonical \code{psi} column.
#'
#' @details
#' The fit is on \code{logit(obs) ~ logit(pred)} (v0.34; the v0.33 method was a
#' raw-scale \code{lm(obs ~ pred)} with a hard \eqn{[0,1]} clip). Two changes
#' matter:
#' \itemize{
#'   \item \strong{Outbreak-weeks-only fit.} Zero observed weeks carry no
#'     \emph{level} information for a scale/offset correction and would make
#'     \code{logit(obs = 0)} degenerate for the zero-week majority, so the fit
#'     uses only weeks with \code{date <= fit_date_stop} and \code{obs > 0}.
#'   \item \strong{Logit-scale, monotone, no hard clip.} The fit is applied to
#'     all weeks and mapped back with \code{plogis}, which is monotone and
#'     bounded in \eqn{(0,1)} by construction — so low predictions map near 0
#'     without the hard-\eqn{[0,1]}-clip truncation that distorted the
#'     \code{psi_bar}-relative deviation feeding LASER.
#' }
#' A country with fewer than \code{min_train} outbreak weeks (this includes
#' zero-history countries, which have none) or no logit-pred variance falls back
#' to the \strong{identity} transform (output = input prediction) with a warning.
#' This is the defined behavior for low-incidence / zero-history countries: their
#' psi is the uncorrected (region-FiLM-modulated) model output.
#'
#' @section Robustness (B1):
#' An \emph{unregularized}, \emph{unbounded} per-country \code{lm} corrupts
#' degenerate / low-signal countries: a near-flat logit-pred predictor yields a
#' wild slope that either collapses the country's psi to a constant or blows its
#' amplitude up many-fold (the \code{prediction from rank-deficient fit}
#' warning). To prevent that, each per-country fit is screened and the affine is
#' guarded:
#' \itemize{
#'   \item \strong{Degeneracy screen.} Fits that are rank-deficient, have a
#'     too-low logit-pred standard deviation (\code{min_pred_sd}), too few
#'     distinct logit-pred values, or a non-finite / explosive slope fall back to
#'     the identity transform.
#'   \item \strong{Bounded affine.} The estimated slope is clamped to
#'     \code{slope_range} and the offset to \code{offset_range} (logit scale)
#'     before being applied, so no country's correction can be steeper / more
#'     offset than is plausible for a scale/level fix.
#'   \item \strong{Amplitude clamp.} The corrected series' logit-scale standard
#'     deviation is constrained to within \code{amp_range} times the input
#'     prediction's logit-scale standard deviation (shrinking the affine toward
#'     identity if it would over-collapse or over-inflate the amplitude), so a
#'     country's psi cannot be flattened to a constant or inflated beyond a sane
#'     range.
#' }
#' Well-behaved countries (ample outbreak weeks, real logit-pred variance, a
#' slope/offset/amplitude inside the guard ranges) are \strong{unaffected} — the
#' guard is a no-op for them.
#'
#' @param pred_df Data frame of predictions with at least \code{iso_code},
#'   \code{date}, and \code{pred_col}.
#' @param obs_df Data frame of observed targets with \code{iso_code},
#'   \code{date}, and \code{obs_col} (in \eqn{[0,1]}).
#' @param fit_date_stop Date (or coercible); the fit uses \code{date <= fit_date_stop}.
#' @param pred_col Prediction column to correct (default \code{"pred_smooth"}).
#' @param obs_col Observed-target column (default \code{"transmission_intensity"}).
#' @param out_col Name of the corrected output column (default
#'   \code{"pred_bias_corrected"}; renamed from the v0.33 \code{"pred_calibrated"}).
#' @param min_train Minimum outbreak (non-zero observed) weeks per country to fit;
#'   otherwise identity (default 8).
#' @param eps Clamp applied to predictions and observations before \code{qlogis}
#'   to keep the logit transform finite (default 1e-6).
#' @param slope_range Length-2 numeric \code{c(lo, hi)}; the per-country logit
#'   slope is clamped to this range before being applied (default
#'   \code{c(0.25, 4)}).
#' @param offset_range Length-2 numeric \code{c(lo, hi)}; the per-country logit
#'   offset is clamped to this range (default \code{c(-4, 4)}).
#' @param amp_range Length-2 numeric \code{c(lo, hi)}; the corrected series'
#'   logit-scale sd is constrained to within this multiple of the input
#'   prediction's logit-scale sd (default \code{c(0.5, 2)}), shrinking the affine
#'   toward identity when violated.
#' @param min_pred_sd Minimum logit-pred standard deviation (over outbreak weeks)
#'   for a country to be eligible for a fit; below it, identity (default 0.05).
#'
#' @return \code{pred_df} with an added \code{out_col} in \eqn{(0,1)}. A
#'   per-country diagnostic data frame is attached as
#'   \code{attr(., "calibration_diagnostics")} (see
#'   \code{\link{check_psi_amplitude}}).
#'
#' @seealso \code{\link{est_suitability}}, \code{\link{check_psi_amplitude}}
#' @importFrom stats lm predict sd qlogis plogis coef
#' @export
calibrate_psi_predictions <- function(pred_df, obs_df, fit_date_stop,
                                      pred_col = "pred_smooth",
                                      obs_col  = "transmission_intensity",
                                      out_col  = "pred_bias_corrected",
                                      min_train = 8L,
                                      eps = 1e-6,
                                      slope_range  = c(0.25, 4),
                                      offset_range = c(-4, 4),
                                      amp_range    = c(0.5, 2),
                                      min_pred_sd  = 0.05) {

     stopifnot(all(c("iso_code", "date") %in% names(pred_df)),
               pred_col %in% names(pred_df),
               all(c("iso_code", "date") %in% names(obs_df)),
               obs_col %in% names(obs_df),
               length(slope_range)  == 2L, slope_range[1]  > 0,
               length(offset_range) == 2L,
               length(amp_range)    == 2L, amp_range[1]     > 0)

     pred_df$date  <- as.Date(pred_df$date)
     obs_df$date   <- as.Date(obs_df$date)
     fit_date_stop <- as.Date(fit_date_stop)

     clamp_logit <- function(p) stats::qlogis(pmax(eps, pmin(1 - eps, p)))
     clamp_to    <- function(x, rng) pmax(rng[1], pmin(rng[2], x))

     m <- merge(pred_df[, c("iso_code", "date", pred_col)],
                obs_df[,  c("iso_code", "date", obs_col)],
                by = c("iso_code", "date"))
     # Fit window: training period AND outbreak (non-zero observed) weeks only.
     m <- m[m$date <= fit_date_stop &
            is.finite(m[[pred_col]]) & is.finite(m[[obs_col]]) &
            m[[obs_col]] > 0, , drop = FALSE]

     pred_df[[out_col]] <- NA_real_
     isos       <- unique(pred_df$iso_code)
     n_identity <- 0L
     n_guarded  <- 0L
     diag_rows  <- vector("list", length(isos))

     for (i in seq_along(isos)) {
          iso  <- isos[i]
          sel  <- pred_df$iso_code == iso
          tr   <- m[m$iso_code == iso, , drop = FALSE]
          xall <- clamp_logit(pred_df[[pred_col]][sel])
          sd_in <- stats::sd(xall, na.rm = TRUE)

          status <- "identity"
          a_used <- NA_real_; b_used <- NA_real_

          eligible <- nrow(tr) >= min_train
          if (eligible) {
               xl <- clamp_logit(tr[[pred_col]])
               yl <- clamp_logit(tr[[obs_col]])
               sd_xl <- stats::sd(xl)
               # Degeneracy screen: real, non-trivial logit-pred variance and
               # enough distinct predictor values to identify a slope.
               if (is.finite(sd_xl) && sd_xl >= min_pred_sd &&
                   length(unique(round(xl, 8))) >= 2L) {
                    fit  <- stats::lm(yl ~ xl)
                    cf   <- stats::coef(fit)
                    a    <- cf[["xl"]]; b <- cf[["(Intercept)"]]
                    # Rank-deficient / non-finite fit -> identity.
                    if (is.finite(a) && is.finite(b) &&
                        !any(is.na(stats::coef(fit)))) {
                         a_c <- clamp_to(a, slope_range)
                         b_c <- clamp_to(b, offset_range)
                         status <- if (a_c != a || b_c != b) "guarded" else "fit"
                         yhat <- a_c * xall + b_c
                         # Amplitude clamp: keep corrected logit-sd within
                         # amp_range * input logit-sd by shrinking toward identity.
                         sd_out <- stats::sd(yhat, na.rm = TRUE)
                         if (is.finite(sd_in) && sd_in > 0 && is.finite(sd_out) &&
                             sd_out > 0) {
                              lo <- amp_range[1] * sd_in
                              hi <- amp_range[2] * sd_in
                              if (sd_out < lo || sd_out > hi) {
                                   target <- min(hi, max(lo, sd_out))
                                   # Blend toward identity (xall) to hit target sd.
                                   # sd(w*yhat + (1-w)*xall) is monotone in w when
                                   # the two series are positively related; solve
                                   # on a small grid for robustness.
                                   ws <- seq(0, 1, by = 0.02)
                                   sds <- vapply(ws, function(w)
                                        stats::sd(w * yhat + (1 - w) * xall,
                                                  na.rm = TRUE), numeric(1))
                                   w_best <- ws[which.min(abs(sds - target))]
                                   yhat <- w_best * yhat + (1 - w_best) * xall
                                   status <- "guarded"
                              }
                         }
                         pred_df[[out_col]][sel] <- stats::plogis(yhat)
                         a_used <- a_c; b_used <- b_c
                         if (status == "guarded") n_guarded <- n_guarded + 1L
                    } else {
                         pred_df[[out_col]][sel] <- pred_df[[pred_col]][sel]
                         n_identity <- n_identity + 1L
                    }
               } else {
                    pred_df[[out_col]][sel] <- pred_df[[pred_col]][sel]
                    n_identity <- n_identity + 1L
               }
          } else {
               pred_df[[out_col]][sel] <- pred_df[[pred_col]][sel]
               n_identity <- n_identity + 1L
          }

          sd_out_final <- stats::sd(clamp_logit(pred_df[[out_col]][sel]),
                                    na.rm = TRUE)
          diag_rows[[i]] <- data.frame(
               iso_code      = iso,
               n_outbreak    = nrow(tr),
               status        = status,
               slope         = a_used,
               offset        = b_used,
               sd_pred_logit = sd_in,
               sd_out_logit  = sd_out_final,
               amp_ratio     = if (is.finite(sd_in) && sd_in > 0)
                                    sd_out_final / sd_in else NA_real_,
               stringsAsFactors = FALSE)
     }

     if (n_identity > 0L) {
          warning(sprintf(
               "calibrate_psi_predictions: %d country/countries had < %d outbreak (non-zero observed) week(s) within the training window, no/low logit-pred variance, or a rank-deficient fit; used identity correction for those.",
               n_identity, min_train), call. = FALSE)
     }
     if (n_guarded > 0L) {
          warning(sprintf(
               "calibrate_psi_predictions: %d country/countries had their affine correction guarded (slope/offset/amplitude clamped toward identity) to prevent psi collapse or blow-up.",
               n_guarded), call. = FALSE)
     }

     diags <- do.call(rbind, diag_rows)
     attr(pred_df, "calibration_diagnostics") <- diags

     # Amplitude guard (B5): flag any country whose corrected series collapsed or
     # inflated relative to the input prediction. Cheap, non-fatal (warn + diag).
     amp_diag <- tryCatch(
          check_psi_amplitude(pred_df, psi_col = out_col, pred_col = pred_col,
                              amp_range = amp_range),
          error = function(e) NULL)
     attr(pred_df, "amplitude_diagnostics") <- amp_diag

     pred_df
}
