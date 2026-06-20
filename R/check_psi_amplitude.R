#' Amplitude guard for post-correction suitability (psi)
#'
#' Lightweight, non-fatal sanity check on the per-country amplitude of the
#' produced suitability series \code{psi}. Implements the long-referenced (but
#' previously unwritten) "amplitude guard": it flags countries whose
#' \emph{post-correction} psi amplitude has \strong{collapsed} toward a constant
#' or has been \strong{inflated} implausibly relative to the input model
#' prediction. Such distortions are introduced by a degenerate per-country
#' bias-correction fit (see \code{\link{calibrate_psi_predictions}}) or by
#' weighting that down-weights the outbreak (non-zero) class.
#'
#' The check is intentionally cheap and never errors: it emits a single
#' \code{warning()} listing the flagged countries (if any) and returns a
#' per-country diagnostic data frame so callers can log / inspect it.
#'
#' @details
#' Amplitude is measured as the standard deviation of the series on the
#' \strong{logit scale} (the scale on which the bias correction operates), using
#' an \code{eps} clamp to keep \code{qlogis} finite. For each country two flags
#' are computed:
#' \itemize{
#'   \item \strong{collapsed} — the corrected logit-sd is below
#'     \code{collapse_abs} (near-constant in absolute terms) \emph{or} below
#'     \code{amp_range[1]} times the input prediction's logit-sd (lost most of
#'     its shape relative to the model).
#'   \item \strong{inflated} — the corrected logit-sd exceeds
#'     \code{amp_range[2]} times the input prediction's logit-sd (amplitude blown
#'     up relative to the model).
#' }
#' When \code{pred_col} is supplied and present, the relative checks use the
#' input prediction as the reference amplitude; otherwise only the absolute
#' collapse check applies.
#'
#' @param psi_df Data frame with at least \code{iso_code} and \code{psi_col}.
#' @param psi_col Name of the produced suitability column (default \code{"psi"}).
#' @param pred_col Optional name of the pre-correction prediction column used as
#'   the reference amplitude (default \code{"pred_smooth"}); ignored if absent.
#' @param amp_range Length-2 numeric \code{c(lo, hi)}; a country is flagged
#'   \code{collapsed} when its logit-sd ratio to the reference is below
#'   \code{lo}, and \code{inflated} when above \code{hi} (default
#'   \code{c(0.5, 2)}).
#' @param collapse_abs Absolute logit-sd floor; a country below it is flagged
#'   \code{collapsed} regardless of the reference (default 0.02).
#' @param eps Clamp applied before \code{qlogis} (default 1e-6).
#' @param warn Logical; emit a \code{warning()} when any country is flagged
#'   (default \code{TRUE}).
#'
#' @return Invisibly, a per-country data frame with columns \code{iso_code},
#'   \code{n}, \code{sd_psi_logit}, \code{sd_ref_logit}, \code{amp_ratio},
#'   \code{collapsed}, \code{inflated}, and \code{flag} (\code{"ok"},
#'   \code{"collapsed"}, or \code{"inflated"}).
#'
#' @seealso \code{\link{calibrate_psi_predictions}}, \code{\link{est_suitability}}
#' @importFrom stats sd qlogis aggregate
#' @export
check_psi_amplitude <- function(psi_df,
                                psi_col      = "psi",
                                pred_col     = "pred_smooth",
                                amp_range    = c(0.5, 2),
                                collapse_abs = 0.02,
                                eps          = 1e-6,
                                warn         = TRUE) {

     stopifnot(is.data.frame(psi_df),
               "iso_code" %in% names(psi_df),
               psi_col %in% names(psi_df),
               length(amp_range) == 2L, amp_range[1] > 0)

     clamp_logit <- function(p) {
          p <- p[is.finite(p)]
          if (!length(p)) return(NA_real_)
          stats::sd(stats::qlogis(pmax(eps, pmin(1 - eps, p))))
     }

     have_ref <- !is.null(pred_col) && length(pred_col) == 1L &&
          pred_col %in% names(psi_df)

     isos  <- unique(psi_df$iso_code)
     rows  <- vector("list", length(isos))
     for (i in seq_along(isos)) {
          iso <- isos[i]
          sub <- psi_df[psi_df$iso_code == iso, , drop = FALSE]
          sd_psi <- clamp_logit(sub[[psi_col]])
          sd_ref <- if (have_ref) clamp_logit(sub[[pred_col]]) else NA_real_
          ratio  <- if (is.finite(sd_ref) && sd_ref > 0) sd_psi / sd_ref else NA_real_

          collapsed <- (is.finite(sd_psi) && sd_psi < collapse_abs) ||
               (is.finite(ratio) && ratio < amp_range[1])
          inflated  <- is.finite(ratio) && ratio > amp_range[2]
          flag <- if (isTRUE(collapsed)) "collapsed" else
                  if (isTRUE(inflated))  "inflated"  else "ok"

          rows[[i]] <- data.frame(
               iso_code     = iso,
               n            = nrow(sub),
               sd_psi_logit = sd_psi,
               sd_ref_logit = sd_ref,
               amp_ratio    = ratio,
               collapsed    = isTRUE(collapsed),
               inflated     = isTRUE(inflated),
               flag         = flag,
               stringsAsFactors = FALSE)
     }
     diag <- do.call(rbind, rows)

     flagged <- diag[diag$flag != "ok", , drop = FALSE]
     if (isTRUE(warn) && nrow(flagged) > 0L) {
          parts <- sprintf("%s (%s, amp_ratio=%.2f)",
                           flagged$iso_code, flagged$flag,
                           flagged$amp_ratio)
          warning(sprintf(
               "check_psi_amplitude: %d country/countries have distorted psi amplitude (collapsed or inflated): %s. Inspect the bias correction (see calibrate_psi_predictions diagnostics).",
               nrow(flagged), paste(parts, collapse = "; ")), call. = FALSE)
     }
     invisible(diag)
}
