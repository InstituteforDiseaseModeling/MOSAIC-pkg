#' Calibrate Suitability on the Logit Scale (returns vector; optional causal EWMA)
#'
#' @description
#' Transforms a weekly suitability series \eqn{\psi_t \in [0,1]} with a
#' **logit-scale affine calibration** followed by optional **causal exponential
#' smoothing (EWMA)**. The result is a single calibrated series \eqn{\psi_t^{\star}}
#' of the same length as the input.
#'
#' @details
#' Let \eqn{\tilde\psi_t=\min\{\max(\psi_t,\varepsilon),\,1-\varepsilon\}} (clipping for stability),
#' \eqn{\mathrm{logit}(x)=\log\{x/(1-x)\}}, and \eqn{\sigma(u)=1/(1+e^{-u})}.
#'
#' **Calibration (odds-space power & scale):**
#' \deqn{\psi_t^{\star}=\sigma\!\big(a\cdot \mathrm{logit}(\tilde\psi_t)+b\big)
#' \quad\Longleftrightarrow\quad \text{odds}^{\star}_t = e^{b}\,(\text{odds}_t)^{a},}
#' with \eqn{\text{odds}_t=\tilde\psi_t/(1-\tilde\psi_t)}.
#'
#' **Optional causal smoothing (no look-ahead):**
#' \deqn{\tilde\psi^{\star}_t = z\,\psi^{\star}_t + (1-z)\,\tilde\psi^{\star}_{t-1},
#' \quad z\in(0,1],\; \tilde\psi^{\star}_1=\psi^{\star}_1.}
#'
#' @param psi Numeric vector in \eqn{[0,1]}; weekly suitability \eqn{\psi_t}.
#' @param a   Numeric scalar (>0). \strong{Shape/gain} on the logit scale.
#'            \eqn{a>1} sharpens peaks; \eqn{a<1} flattens peaks. Default \code{1}.
#' @param b   Numeric scalar. \strong{Scale/offset} on the logit scale (baseline up/down).
#'            Default \code{0}.
#' @param z   Numeric scalar in \eqn{(0,1]} controlling \strong{causal EWMA smoothing}.
#'            \code{1} = no smoothing (just calibrated). Default \code{1}.
#' @param eps Small positive numeric used to clip \code{psi} away from \code{0} and \code{1}
#'            before applying \code{qlogis}. Default \code{1e-6}.
#'
#' @return Numeric vector \eqn{\tilde\psi^{\star}_t} of the same length as \code{psi}
#'         (calibrated, and smoothed if \code{z<1}).
#'
#' @section Interpretation:
#' \itemize{
#'   \item \strong{a} — shape/gain (peaks sharper for \eqn{a>1}, flatter for \eqn{a<1}).
#'   \item \strong{b} — scale/offset (shifts baseline up/down on logit scale).
#'   \item \strong{z} — smoothing weight; smaller values increase causal damping.
#' }
#'
#' @references
#' Logit / logistic function: \url{https://en.wikipedia.org/wiki/Logit} \\
#' Logistic (sigmoid): \url{https://en.wikipedia.org/wiki/Logistic_function} \\
#' Odds and log-odds: \url{https://en.wikipedia.org/wiki/Odds} \\
#' Platt scaling (logistic calibration): \url{https://en.wikipedia.org/wiki/Platt_scaling} \\
#' Exponential smoothing / EWMA: \url{https://en.wikipedia.org/wiki/Exponential_smoothing}
#'
#' @seealso \code{\link[stats]{qlogis}}, \code{\link[stats]{plogis}}
#'
#' @examples
#' set.seed(1)
#' t  <- 1:100
#' psi <- plogis(-2 + 2.5 * exp(-0.5*((t-60)/6)^2)) + rnorm(100, sd = 0.01)
#' psi <- pmin(pmax(psi, 0), 1)
#' psi_star <- calc_psi_star(psi, a = 1.8, b = -0.4, z = 0.85)
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   df <- data.frame(week = t, original = psi, psi_star = psi_star)
#'   ggplot(df, aes(week)) +
#'     geom_line(aes(y = original, colour = "Original ψ"), linewidth = 0.7) +
#'     geom_line(aes(y = psi_star, colour = "Calibrated ψ* (EWMA if z<1)"), linewidth = 0.9) +
#'     scale_colour_manual(values = c("grey40","firebrick")) +
#'     labs(x = "Week", y = "Suitability",
#'          title = "Logit calibration with optional causal EWMA",
#'          subtitle = "a: shape/gain | b: scale/offset | z: smoothing weight") +
#'     theme_bw() + theme(legend.title = element_blank())
#' }
#'
#' @export
#'

calc_psi_star <- function(psi, a = 1, b = 0, z = 1, eps = 1e-6) {

     # Checks
     if (!is.numeric(psi)) stop("`psi` must be numeric.")
     if (!is.numeric(a) || length(a) != 1 || a <= 0) stop("`a` must be a positive scalar.")
     if (!is.numeric(b) || length(b) != 1) stop("`b` must be a numeric scalar.")
     if (!is.numeric(z) || length(z) != 1 || z <= 0 || z > 1) {
          stop("`z` must be in (0, 1]. Use z = 1 for no smoothing.")
     }
     if (!is.numeric(eps) || eps <= 0) stop("`eps` must be a small positive number.")

     # Clip psi values to be in bounds
     psi_clipped <- pmin(pmax(psi, eps), 1 - eps)

     # Apply shape and scale adjustments in logit space
     psi_star <- stats::plogis(a * stats::qlogis(psi_clipped) + b)

     # Smooth psi_star using causal EWMA smoothing
     if (z >= 1) {
          return(psi_star)
     } else {
          n <- length(psi_star)
          out <- psi_star
          if (n >= 2) {
               for (t in 2:n) {
                    out[t] <- z * psi_star[t] + (1 - z) * out[t - 1]
               }
          }
          return(out)
     }
}
