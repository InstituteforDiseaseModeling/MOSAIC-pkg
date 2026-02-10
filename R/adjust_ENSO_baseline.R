###############################################################################
## adjust_ENSO_baseline.R - Bias correction for multi-source ENSO data
###############################################################################

#' Simple Bias Correction for ENSO Data with Different Climatological Baselines
#'
#' Harmonizes ENSO forecast data from different sources (BOM, IRI) to NOAA's
#' 1991-2020 baseline by estimating and removing systematic bias during an
#' overlap period where both historical observations and archived forecasts exist.
#'
#' @param historical Numeric vector of historical observations (degrees Celsius).
#'   Use NA for future periods without observations.
#' @param forecast Numeric vector of forecast values (degrees Celsius), same
#'   length as historical. Use NA for periods without forecasts.
#' @param n_overlap Integer number of timesteps from end of overlap period to
#'   use for bias calculation. If NULL or Inf, uses all available overlap points.
#'   Default: NULL (all overlap).
#'
#' @return A list containing:
#' \item{historical}{Original historical observations vector.}
#' \item{forecast}{Original forecast vector.}
#' \item{forecast_corrected}{Bias-corrected forecast vector (forecast - bias).}
#' \item{combined}{Combined time series: historical takes priority, then corrected forecast.}
#' \item{bias}{Estimated mean bias (degrees Celsius) calculated as mean(forecast - historical).}
#' \item{se}{Standard error of the bias estimate.}
#' \item{n_overlap}{Number of overlap points used for bias calculation.}
#' \item{differences}{Raw differences (forecast - historical) during overlap period used for bias estimation.}
#'
#' @details
#' Three ENSO data sources use different climatological baselines, creating
#' systematic offsets of 0.1-0.3°C:
#' \itemize{
#'   \item NOAA Historical: 1991-2020 baseline (reference)
#'   \item BOM Forecasts: 1981-2010 baseline (typically +0.15 to +0.25°C warmer)
#'   \item IRI Ensemble: Mixed baselines (typically +0.20 to +0.30°C warmer)
#' }
#'
#' The function estimates bias as the mean difference during the overlap period
#' and subtracts it from forecast values to harmonize them to NOAA's baseline.
#' A minimum of 12 months overlap is recommended to capture seasonal cycles.
#'
#' For IRI data, ensure values are converted to degrees Celsius (divide by 100)
#' before passing to this function, as IRI stores values in hundredths.
#'
#' @examples
#' \dontrun{
#' # Example 1: BOM forecast with 3-month overlap
#' # Historical NOAA observations (months 1-12, then NA for future)
#' historical <- c(-0.5, -0.6, -0.7, -0.8, -0.7, -0.6,
#'                 -0.5, -0.4, -0.3, -0.2, -0.1, 0.0,
#'                 rep(NA, 12))
#'
#' # BOM forecasts (NA for months 1-9, overlap 10-12, future 13-24)
#' forecast <- c(rep(NA, 9),
#'               -0.2, 0.0, 0.2,  # Overlap period
#'               0.3, 0.4, 0.5, 0.6, 0.7, 0.8,
#'               0.9, 1.0, 1.1, 1.2, 1.3, 1.4)  # Future
#'
#' # Apply bias correction using all overlap points
#' result <- adjust_ENSO_baseline(historical, forecast, n_overlap = NULL)
#'
#' print(result$bias)           # Expected: ~0.2°C
#' print(result$n_overlap)      # 3 overlap points used
#' print(result$combined[13:15]) # Corrected future forecasts
#'
#'
#' # Example 2: IRI forecast with unit conversion and 12-month overlap
#' # Historical NOAA observations
#' historical <- c(-0.5, -0.6, -0.7, -0.8, -0.7, -0.6,
#'                 -0.5, -0.4, -0.3, -0.2, -0.1, 0.0,
#'                 rep(NA, 12))
#'
#' # IRI forecasts - RAW values in hundredths (must convert!)
#' forecast_raw <- c(-28, -38, -47, -58, -48, -37,
#'                   -26, -16, -8, 3, 14, 23,    # Overlap period (months 1-12)
#'                   67, 72, 68, 55, 38, 22,
#'                   10, 5, 2, -5, -8, -10)      # Future (months 13-24)
#'
#' # Convert IRI to degrees Celsius
#' forecast <- forecast_raw / 100
#'
#' # Apply bias correction using last 6 months only
#' result <- adjust_ENSO_baseline(historical, forecast, n_overlap = 6)
#'
#' print(result$bias)            # Expected: ~0.23°C
#' print(result$se)              # Standard error
#' print(result$n_overlap)       # 6 (months 7-12)
#'
#' # Examine bias distribution
#' hist(result$differences,
#'      main = "Bias Distribution",
#'      xlab = "Difference (°C)",
#'      col = "lightblue")
#' abline(v = result$bias, col = "red", lwd = 2, lty = 2)
#' }
#'
#' @export
adjust_ENSO_baseline <- function(historical, forecast, n_overlap = NULL) {

  # Input validation
  if (!is.numeric(historical) || !is.numeric(forecast)) {
    stop("historical and forecast must be numeric vectors")
  }

  if (length(historical) != length(forecast)) {
    stop("historical and forecast must have equal length")
  }

  # Find overlap period (where both are non-NA)
  valid <- !is.na(historical) & !is.na(forecast)

  if (sum(valid) == 0) {
    stop("No overlapping non-NA values found between historical and forecast")
  }

  # Get indices of valid overlap
  overlap_indices <- which(valid)

  # Determine how many overlap points to use for bias calculation
  if (is.null(n_overlap) || is.infinite(n_overlap)) {
    # Use all available overlap
    use_indices <- overlap_indices
    n_used <- length(overlap_indices)
  } else {
    # Validate n_overlap is positive integer
    if (!is.numeric(n_overlap) || length(n_overlap) != 1 || n_overlap <= 0) {
      stop("n_overlap must be a positive integer or NULL")
    }
    n_overlap <- as.integer(n_overlap)

    # Use last n_overlap points from the end of overlap period
    n_available <- length(overlap_indices)
    n_use <- min(n_overlap, n_available)
    use_indices <- tail(overlap_indices, n_use)
    n_used <- n_use
  }

  if (n_used < 6) {
    warning("Only ", n_used, " overlap points used. Bias estimate may be unreliable (recommend >= 12)")
  }

  # Calculate bias from selected overlap period
  differences <- forecast[use_indices] - historical[use_indices]
  bias <- mean(differences)
  se <- sd(differences) / sqrt(n_used)

  # Apply bias correction to entire forecast
  forecast_corrected <- forecast - bias

  # Create combined vector: historical takes priority, then corrected forecast
  combined <- ifelse(!is.na(historical), historical, forecast_corrected)

  # Return results
  list(
    historical = historical,
    forecast = forecast,
    forecast_corrected = forecast_corrected,
    combined = combined,
    bias = bias,
    se = se,
    n_overlap = n_used,
    differences = differences
  )
}
