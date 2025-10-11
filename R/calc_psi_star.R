#' Calibrate Daily Suitability on the Logit Scale
#' (vector; optional causal EWMA + time offset + NA filling)
#'
#' @description
#' Transforms a **daily** suitability series \eqn{\psi_t \in [0,1]} with a
#' **logit-scale affine calibration**, optional **causal EWMA smoothing**, and an
#' optional **time offset** \eqn{k}. Any missing values introduced by the shift
#' (or pre-existing) are filled using either **LOCF** (default) or **linear interpolation**.
#'
#' @details
#' **Offset (index shift, in days):** \eqn{\psi^{(k)}_t = \psi_{t-k}}.
#' Positive \eqn{k} shifts the series forward/right (delay); negative \eqn{k} shifts backward/left.
#' Non-integer \eqn{k} is rounded to the nearest integer.
#'
#' **Calibration (odds-space power & scale):**
#' \deqn{\psi_t^{\star}=\sigma\!\big(a\cdot \mathrm{logit}(\tilde\psi_t)+b\big),\quad
#' \tilde\psi_t=\min\{\max(\psi^{(k)}_t,\varepsilon),\,1-\varepsilon\}}
#' with \eqn{\text{odds}^\star_t = e^{b}\,(\text{odds}_t)^{a}}, \;
#' \eqn{\text{odds}_t=\tilde\psi_t/(1-\tilde\psi_t)}.
#'
#' **Optional causal smoothing (no look-ahead):**
#' \deqn{\tilde\psi^{\star}_t = z\,\psi^{\star}_t + (1-z)\,\tilde\psi^{\star}_{t-1},\quad
#' z\in(0,1],\ \tilde\psi^{\star}_1=\psi^{\star}_1.}
#'
#' **NA filling (after calibration, before smoothing):**
#' Choose via \code{fill_method}:
#' \itemize{
#'   \item \code{"locf"} — forward fill (LOCF) then backward pass for leading gaps.
#'   \item \code{"linear"} — linear interpolation for interior gaps with constant edge extension.
#' }
#' If all values are NA after shifting, the function falls back to a constant baseline \eqn{\sigma(b)}.
#'
#' @param psi Numeric vector in \eqn{[0,1]} (daily suitability \eqn{\psi_t}).
#' @param a   Numeric (>0). Shape/gain on the logit scale (\eqn{a>1} sharpens; \eqn{a<1} flattens).
#' @param b   Numeric. Scale/offset on the logit scale (baseline up/down).
#' @param z   Numeric in \eqn{(0,1]} controlling causal EWMA smoothing; \code{1} = no smoothing.
#' @param k   Numeric or integer; time offset in **days**. Positive = forward/right; negative = backward/left.
#'            Non-integers are rounded to nearest integer.
#' @param eps Small positive number to clip \code{psi} away from \{0,1\} before \code{qlogis}.
#' @param fill_method Character; one of \code{c("locf","linear")} controlling how NA gaps are filled.
#'
#' @return Numeric vector \eqn{\tilde\psi^{\star}_t} (same length as \code{psi}):
#'         calibrated, offset, NA-filled, and smoothed if \code{z<1}.
#'
#' @seealso \code{\link[stats]{qlogis}}, \code{\link[stats]{plogis}}, \code{\link[stats]{approx}}
#'
#' @export
calc_psi_star <- function(psi, a = 1, b = 0, z = 1, k = 0, eps = 1e-6,
                          fill_method = c("locf","linear")) {

     fill_method <- match.arg(fill_method)

     # ---- Checks ----
     if (!is.numeric(psi)) stop("`psi` must be numeric.")
     if (!is.numeric(a) || length(a) != 1 || a <= 0) stop("`a` must be a positive scalar.")
     if (!is.numeric(b) || length(b) != 1) stop("`b` must be a numeric scalar.")
     if (!is.numeric(z) || length(z) != 1 || z <= 0 || z > 1) stop("`z` must be in (0,1].")
     if (!is.numeric(k) || length(k) != 1 || is.na(k)) stop("`k` must be a single numeric or integer.")
     if (!is.numeric(eps) || eps <= 0) stop("`eps` must be a small positive number.")

     n <- length(psi)
     if (n == 0L) return(psi)

     # Round k to nearest integer
     k_int <- as.integer(round(k))
     if (!isTRUE(all.equal(k, k_int))) {
          warning(sprintf("`k` (%.3f) is not an integer; rounding to %d.", k, k_int))
     }
     if (abs(k_int) >= n) {
          warning("abs(k) >= length(psi); fill will reduce to a constant baseline at edges.")
     }

     # Clean psi
     if (any(!is.finite(psi), na.rm = TRUE)) {
          warning("Non-finite values detected in `psi` (Inf/-Inf). Treating as NA.")
          psi[!is.finite(psi)] <- NA_real_
     }
     if (any(psi < 0 | psi > 1, na.rm = TRUE)) {
          warning("Some psi values are outside [0,1] and will be clipped.")
     }

     # ---- (0) Shift by k days: psi^(k)_t = psi_{t-k} (pad with NA) --------------
     if (k_int == 0L) {
          psi_shift <- psi
     } else if (k_int > 0L) {              # forward/right (delay)
          psi_shift <- rep(NA_real_, n)
          if (k_int < n) psi_shift[(k_int + 1L):n] <- psi[1L:(n - k_int)]
     } else {                               # backward/left (advance)
          kk <- -k_int
          psi_shift <- rep(NA_real_, n)
          if (kk < n) psi_shift[1L:(n - kk)] <- psi[(1L + kk):n]
     }

     # ---- (1) Clip for stable logit --------------------------------------------
     psi_clipped <- pmin(pmax(psi_shift, eps), 1 - eps)  # NAs stay NA

     # ---- (2) Logit affine transform then logistic back-transform --------------
     psi_star <- stats::plogis(a * stats::qlogis(psi_clipped) + b)

     # ---- (3) Fill NA's: "locf" or "linear" ------------------------------------
     if (all(is.na(psi_star))) {
          # total gap: fall back to baseline σ(b)
          psi_star[] <- stats::plogis(b)
     } else if (fill_method == "linear") {
          idx <- which(!is.na(psi_star))
          psi_star <- stats::approx(x = idx, y = psi_star[idx],
                                    xout = seq_len(n),
                                    method = "linear", rule = 2)$y
     } else { # "locf": forward fill then backward pass
          # forward pass
          for (i in 2L:n) if (is.na(psi_star[i])) psi_star[i] <- psi_star[i - 1]
          # backward pass for any leading NAs
          for (i in (n - 1L):1L) if (is.na(psi_star[i])) psi_star[i] <- psi_star[i + 1L]
          # if still all NA (paranoid fallback)
          if (all(is.na(psi_star))) psi_star[] <- stats::plogis(b)
     }

     # ---- (4) Causal EWMA smoothing --------------------------------------------
     if (z >= 1) {
          return(psi_star)
     } else {
          out <- psi_star
          if (n >= 2L) {
               for (i in 2L:n) out[i] <- z * psi_star[i] + (1 - z) * out[i - 1]
          }
          return(out)
     }
}
