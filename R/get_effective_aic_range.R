#' Get Effective AIC Range for Gibbs Temperature Scaling
#'
#' Calculates the expected delta AIC range for a given top percentile
#' using a continuous relationship based on log odds ratio.
#'
#' @param top_percentile Numeric percentage (0-100) of top simulations
#'
#' @return Numeric effective range for delta AIC normalization
#'
#' @details
#' The effective range represents the expected delta AIC span for the
#' top X% of models under typical (non-dispersed) conditions.
#'
#' Uses a heuristic that maps percentiles to Burnham & Anderson's AIC thresholds.
#' Based on their interpretation:
#' - 1% → Δ AIC < 2 (substantial support, ~37% relative likelihood)
#' - 5% → Δ AIC ≈ 4 (boundary of substantial/less support)
#' - 10% → Δ AIC ≈ 7 (upper end of "considerably less support")
#' - 20% → Δ AIC ≈ 10 (boundary to "essentially no support")
#'
#' @examples
#' get_effective_aic_range(1)   # 2.0
#' get_effective_aic_range(5)   # 4.0
#' get_effective_aic_range(10)  # 7.0
#'
#' @export
get_effective_aic_range <- function(top_percentile) {

    # Input validation
    if (!is.numeric(top_percentile) || top_percentile <= 0 || top_percentile > 100) {
        stop("top_percentile must be between 0 and 100")
    }

    # Convert percentage to proportion
    p <- top_percentile / 100

    # Heuristic mapping based on Burnham & Anderson thresholds:
    # 1% → Δ AIC = 2 (substantial support threshold)
    # 5% → Δ AIC = 4 (transition to less support)
    # 10% → Δ AIC = 7 (upper end of less support)
    # 20% → Δ AIC = 10 (essentially no support threshold)

    # Use smooth interpolation between these anchor points
    if (top_percentile <= 1) {
        # Linear from 0 to 1%
        effective_range <- 2 * top_percentile
    } else if (top_percentile <= 5) {
        # Linear interpolation from 1% to 5%
        effective_range <- 2 + (4 - 2) * (top_percentile - 1) / (5 - 1)
    } else if (top_percentile <= 10) {
        # Linear interpolation from 5% to 10%
        effective_range <- 4 + (7 - 4) * (top_percentile - 5) / (10 - 5)
    } else if (top_percentile <= 20) {
        # Linear interpolation from 10% to 20%
        effective_range <- 7 + (10 - 7) * (top_percentile - 10) / (20 - 10)
    } else {
        # Logarithmic growth beyond 20%
        effective_range <- 10 + 2 * log10(top_percentile / 20)
    }

    # Ensure reasonable bounds
    effective_range <- pmax(0.5, pmin(15.0, effective_range))

    return(effective_range)
}

#' Plot Effective Range Function
#'
#' Visualizes the relationship between top percentile and effective AIC range
#'
#' @param percentiles Numeric vector of percentiles to evaluate (default: 0.5 to 30)
#'
#' @return A plot showing the effective AIC range as a function of percentile
#'
#' @examples
#' \dontrun{
#' plot_effective_range()
#' plot_effective_range(percentiles = seq(1, 20, by = 1))
#' }
#'
#' @export
plot_effective_range <- function(percentiles = seq(0.5, 30, by = 0.5)) {

    # Calculate ranges (note: current implementation uses a single heuristic method)
    results <- data.frame(percentile = percentiles)
    results$range <- sapply(percentiles, get_effective_aic_range)

    # Create plot
    plot(results$percentile, results$range,
         type = "l", lwd = 2, col = "blue",
         xlab = "Top Percentile (%)",
         ylab = "Effective Delta AIC Range",
         main = "Effective AIC Range Function")

    # Add reference lines for common percentiles
    abline(v = c(1, 5, 10), lty = 2, col = "gray")

    # Add horizontal reference lines for AIC interpretation
    abline(h = c(2, 4, 6, 8), lty = 3, col = "gray")
    text(max(percentiles) * 0.95, c(2, 4, 6, 8) + 0.2,
         c("Strong", "Considerable", "Less", "Minimal"),
         adj = 1, col = "gray", cex = 0.8)

    # Add specific points for reference
    for (p in c(1, 5, 10)) {
        points(p, get_effective_aic_range(p),
               col = "blue", pch = 16, cex = 1.5)
    }
}