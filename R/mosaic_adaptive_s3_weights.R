#' Compute Adaptive Likelihood Weights for Joint Calibration Stage
#'
#' @description
#' Computes data-driven \code{weight_cases} and \code{weight_deaths} values
#' for a joint calibration stage (S3) based on how well cases and deaths were
#' fit in the preceding single-outcome stages (S1 cases, S2 deaths). The
#' weights are computed once before S3 begins and set as scalars in
#' \code{\link{mosaic_control_defaults}}.
#'
#' The underlying principle is **complementary deficiency weighting**: outcomes
#' that are more poorly fit receive more weight in S3 so that S3 can improve
#' them. Two deficiency signals are combined for each outcome:
#'
#' \enumerate{
#'   \item \strong{R² deficiency}: \code{1 - R²}. Measures how poorly the
#'     model tracks the temporal pattern of each outcome.
#'   \item \strong{Bias deficiency}: \code{1 - exp(-|log(bias_ratio)|)}.
#'     Measures how far the total predicted magnitude is from observed.
#'     Symmetric on log scale (2x over-prediction = 2x under-prediction).
#'     Maps bias_ratio = 1 → 0, ratio = 2 → 0.5, ratio = 5 → 0.8.
#' }
#'
#' The two signals are combined as a weighted mean (\code{r2_weight} on R²,
#' remainder on bias) because R² is more statistically stable at small
#' simulation counts. The resulting weights always sum to 1 and are clamped
#' to \code{[w_min, w_max]}.
#'
#' @param r2_cases Numeric. R² for cases from the S1 (cases) calibration
#'   stage. Read from \code{3_results/summary.json$r2_cases}.
#' @param r2_deaths Numeric. R² for deaths from the S2 (deaths) calibration
#'   stage. Read from \code{3_results/summary.json$r2_deaths}.
#' @param bias_cases Numeric. Bias ratio for cases from S1:
#'   \code{sum(predicted) / sum(observed)}. From \code{summary.json$bias_ratio_cases}.
#' @param bias_deaths Numeric. Bias ratio for deaths from S2.
#'   From \code{summary.json$bias_ratio_deaths}.
#' @param frac_floored_cases Optional numeric. Fraction of S1 simulations that
#'   hit the likelihood floor (guardrail). When > \code{degenerate_floor},
#'   the stage is considered degenerate and fallback weights are used.
#' @param frac_floored_deaths Optional numeric. Same for S2 deaths stage.
#' @param r2_weight Numeric in (0,1). Weight given to R²-deficiency vs
#'   bias-deficiency when computing combined deficiency. Default 0.6 (R²
#'   more reliable than bias at typical sim counts).
#' @param w_min Numeric. Hard lower bound for either weight. Default 0.2
#'   (neither outcome can receive less than 20% of total weight).
#' @param w_max Numeric. Hard upper bound for either weight. Default 0.8.
#' @param degenerate_floor Numeric. Fraction-floored threshold above which a
#'   stage is considered degenerate. Default 0.90.
#' @param fallback_w_cases Numeric. weight_cases to use when a degenerate
#'   stage is detected. Default 0.5 (equal weights).
#' @param verbose Logical. Print computed weights and diagnostics. Default TRUE.
#'
#' @return Named list with:
#' \describe{
#'   \item{weight_cases}{Scalar for \code{mosaic_control_defaults(likelihood=list(weight_cases=...))}.}
#'   \item{weight_deaths}{Scalar for \code{weight_deaths}.}
#'   \item{degenerate}{Logical. TRUE if a degenerate stage was detected.}
#'   \item{diagnostics}{List of intermediate values for inspection.}
#' }
#'
#' @examples
#' \dontrun{
#' # After S1 and S2 complete, load their summaries and compute adaptive weights
#' summ_s1 <- jsonlite::read_json("stage_1/3_results/summary.json")
#' summ_s2 <- jsonlite::read_json("stage_2/3_results/summary.json")
#'
#' weights <- mosaic_adaptive_s3_weights(
#'   r2_cases    = summ_s1$r2_cases,
#'   r2_deaths   = summ_s2$r2_deaths,
#'   bias_cases  = summ_s1$bias_ratio_cases,
#'   bias_deaths = summ_s2$bias_ratio_deaths
#' )
#'
#' ctrl_s3 <- mosaic_control_defaults(
#'   likelihood = list(
#'     weight_cases  = weights$weight_cases,
#'     weight_deaths = weights$weight_deaths
#'   )
#' )
#' }
#'
#' @seealso \code{\link{inflate_priors}}, \code{\link{mosaic_control_defaults}}
#'
#' @export
mosaic_adaptive_s3_weights <- function(r2_cases,
                                       r2_deaths,
                                       bias_cases,
                                       bias_deaths,
                                       frac_floored_cases  = NULL,
                                       frac_floored_deaths = NULL,
                                       r2_weight           = 0.6,
                                       w_min               = 0.2,
                                       w_max               = 0.8,
                                       degenerate_floor    = 0.90,
                                       fallback_w_cases    = 0.5,
                                       verbose             = TRUE) {

  # ---------------------------------------------------------------------------
  # Input validation
  # ---------------------------------------------------------------------------
  for (arg in list(r2_cases, r2_deaths, bias_cases, bias_deaths)) {
    if (!is.numeric(arg) || length(arg) != 1L || !is.finite(arg))
      stop("r2_cases, r2_deaths, bias_cases, bias_deaths must all be finite scalars",
           call. = FALSE)
  }
  if (bias_cases <= 0 || bias_deaths <= 0)
    stop("bias_cases and bias_deaths must be positive", call. = FALSE)

  # ---------------------------------------------------------------------------
  # Degenerate stage guard
  # ---------------------------------------------------------------------------
  cases_degen  <- !is.null(frac_floored_cases)  && isTRUE(frac_floored_cases  > degenerate_floor)
  deaths_degen <- !is.null(frac_floored_deaths) && isTRUE(frac_floored_deaths > degenerate_floor)

  if (cases_degen || deaths_degen) {
    w_cases  <- fallback_w_cases
    w_deaths <- 1 - fallback_w_cases
    msg <- sprintf(
      "mosaic_adaptive_s3_weights: degenerate stage detected (frac_floored cases=%.2f deaths=%.2f). Using fallback w_cases=%.2f w_deaths=%.2f.",
      frac_floored_cases  %||% NA_real_,
      frac_floored_deaths %||% NA_real_,
      w_cases, w_deaths
    )
    warning(msg, call. = FALSE)
    return(list(
      weight_cases  = w_cases,
      weight_deaths = w_deaths,
      degenerate    = TRUE,
      diagnostics   = NULL
    ))
  }

  # ---------------------------------------------------------------------------
  # Bias deficiency transform: f(b) = 1 - exp(-|log(b)|)
  # Maps [0,inf) -> [0,1): 0 at b=1, ~0.5 at 2x, ~0.8 at 5x
  # Symmetric on log scale (over- and under-prediction equally penalised)
  # ---------------------------------------------------------------------------
  .bias_def <- function(b) {
    b <- max(b, 1e-9)
    1 - exp(-abs(log(b)))
  }

  # ---------------------------------------------------------------------------
  # R² deficiency (clamp input to [0,1])
  # ---------------------------------------------------------------------------
  r2_cases  <- max(0, min(1, r2_cases))
  r2_deaths <- max(0, min(1, r2_deaths))

  def_r2_cases  <- 1 - r2_cases
  def_r2_deaths <- 1 - r2_deaths

  # ---------------------------------------------------------------------------
  # Bias deficiency
  # ---------------------------------------------------------------------------
  def_bias_cases  <- .bias_def(bias_cases)
  def_bias_deaths <- .bias_def(bias_deaths)

  # ---------------------------------------------------------------------------
  # Combined deficiency: weighted mean (R² more reliable than bias at small N)
  # ---------------------------------------------------------------------------
  bias_weight <- 1 - r2_weight

  def_cases  <- r2_weight * def_r2_cases  + bias_weight * def_bias_cases
  def_deaths <- r2_weight * def_r2_deaths + bias_weight * def_bias_deaths

  # ---------------------------------------------------------------------------
  # Weight ratio: more deficient outcome receives more weight in S3
  # ---------------------------------------------------------------------------
  denom <- def_cases + def_deaths

  if (denom < 1e-10) {
    w_cases  <- 0.5
    w_deaths <- 0.5
  } else {
    w_cases  <- def_cases  / denom
    w_deaths <- def_deaths / denom
  }

  # ---------------------------------------------------------------------------
  # Hard bounds — enforce symmetrically, preserving sum-to-1
  # ---------------------------------------------------------------------------
  w_cases  <- max(w_min, min(w_max, w_cases))
  w_deaths <- 1 - w_cases
  # Re-enforce lower bound on deaths after cases adjustment
  if (w_deaths < w_min) {
    w_deaths <- w_min
    w_cases  <- 1 - w_deaths
  }

  # ---------------------------------------------------------------------------
  # Diagnostics and logging
  # ---------------------------------------------------------------------------
  diagnostics <- list(
    def_r2_cases    = def_r2_cases,
    def_r2_deaths   = def_r2_deaths,
    def_bias_cases  = def_bias_cases,
    def_bias_deaths = def_bias_deaths,
    def_cases       = def_cases,
    def_deaths      = def_deaths,
    r2_weight       = r2_weight,
    bias_weight     = bias_weight
  )

  if (verbose) {
    message(sprintf(
      "mosaic_adaptive_s3_weights: w_cases=%.3f  w_deaths=%.3f",
      w_cases, w_deaths))
    message(sprintf(
      "  R2:   cases=%.3f (def=%.3f)  deaths=%.3f (def=%.3f)",
      r2_cases, def_r2_cases, r2_deaths, def_r2_deaths))
    message(sprintf(
      "  Bias: cases=%.3f (def=%.3f)  deaths=%.3f (def=%.3f)",
      bias_cases, def_bias_cases, bias_deaths, def_bias_deaths))
    message(sprintf(
      "  Combined deficiency: cases=%.3f  deaths=%.3f",
      def_cases, def_deaths))
  }

  list(
    weight_cases  = w_cases,
    weight_deaths = w_deaths,
    degenerate    = FALSE,
    diagnostics   = diagnostics
  )
}
