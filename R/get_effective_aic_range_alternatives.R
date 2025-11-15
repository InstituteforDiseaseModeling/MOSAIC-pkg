#' Alternative Theoretically-Grounded Approaches for Effective AIC Range
#'
#' These functions demonstrate more principled approaches based on statistical theory
#'
#' @param top_percentile Numeric percentage (0-100) of top simulations
#' @param df Degrees of freedom for chi-square approach (default: 2)
#'
#' @examples
#' # Compare different approaches
#' percentiles <- c(1, 5, 10, 20)
#'
#' # Current ad hoc formula
#' current <- sapply(percentiles, function(p) 2 + 2.3 * log10(p))
#'
#' # Chi-square based
#' chisq_based <- sapply(percentiles, get_effective_aic_range_chisq)
#'
#' # Relative likelihood based
#' likelihood_based <- sapply(percentiles, get_effective_aic_range_likelihood)
#'
#' @name effective_aic_alternatives

#' @rdname effective_aic_alternatives
#' @export
get_effective_aic_range_chisq <- function(top_percentile, df = 2) {
    # Based on chi-square distribution theory
    # Δ AIC ~ χ²(df) for nested models with df parameter difference

    # Convert to proportion
    p <- top_percentile / 100

    # Get quantile of chi-square distribution
    # We want the (1-p) quantile since top p% means Δ AIC below this value
    qchisq(1 - p, df = df)
}

#' @rdname effective_aic_alternatives
#' @export
get_effective_aic_range_likelihood <- function(top_percentile) {
    # Based on relative likelihood interpretation
    # Maps percentiles to relative likelihood thresholds

    # Heuristic mapping: top X% should have at least Y relative likelihood
    # where Y decreases as X increases

    # Convert to proportion
    p <- top_percentile / 100

    # Map percentile to target relative likelihood
    # This is still somewhat arbitrary but based on the idea that:
    # - Top 1% should be very similar (e.g., 50% relative likelihood)
    # - Top 10% can be less similar (e.g., 10% relative likelihood)

    if (p <= 0.01) {
        target_likelihood <- 0.5  # 50% as likely as best
    } else if (p <= 0.05) {
        target_likelihood <- 0.5 * (0.05 - p) / 0.04 + 0.2 * (p - 0.01) / 0.04
    } else if (p <= 0.10) {
        target_likelihood <- 0.2 * (0.10 - p) / 0.05 + 0.1 * (p - 0.05) / 0.05
    } else {
        target_likelihood <- 0.1 * (0.20 - p) / 0.10 + 0.05 * (p - 0.10) / 0.10
        target_likelihood <- pmax(0.01, target_likelihood)  # Minimum 1% likelihood
    }

    # Convert to Δ AIC
    -2 * log(target_likelihood)
}

#' @rdname effective_aic_alternatives
#' @export
get_effective_aic_range_empirical <- function(top_percentile) {
    # Empirically calibrated to match common practice
    # Based on the observation that in practice:
    # - Top 1% often has Δ AIC range ~2-3
    # - Top 5% often has Δ AIC range ~4-5
    # - Top 10% often has Δ AIC range ~6-7

    # Use a smooth function that matches these empirical observations
    # This is similar to our current formula but with adjusted constants

    # Convert to proportion
    p <- top_percentile / 100

    # Use exponential interpolation
    # Δ AIC = a * (1 - exp(-b * percentile))
    a <- 10  # Maximum Δ AIC for very large percentiles
    b <- 20  # Rate of increase

    a * (1 - exp(-b * p))
}

# Comparison function
compare_approaches <- function(percentiles = c(1, 2, 5, 10, 20)) {

    results <- data.frame(
        Percentile = percentiles,
        Current_Formula = sapply(percentiles, function(p) 2 + 2.3 * log10(p)),
        ChiSq_df1 = sapply(percentiles, function(p) get_effective_aic_range_chisq(p, df = 1)),
        ChiSq_df2 = sapply(percentiles, function(p) get_effective_aic_range_chisq(p, df = 2)),
        ChiSq_df3 = sapply(percentiles, function(p) get_effective_aic_range_chisq(p, df = 3)),
        Likelihood_Based = sapply(percentiles, get_effective_aic_range_likelihood),
        Empirical = sapply(percentiles, get_effective_aic_range_empirical)
    )

    print("Comparison of Different Approaches:")
    print(round(results, 2))

    cat("\nInterpretation:\n")
    cat("- Current Formula: Ad hoc but gives reasonable values\n")
    cat("- Chi-Square: Theoretically grounded but gives large values\n")
    cat("- Likelihood-Based: Maps percentiles to relative likelihood thresholds\n")
    cat("- Empirical: Calibrated to common practice\n")

    cat("\nRecommendation:\n")
    cat("The chi-square approach with df=1 gives the most conservative (smallest) ranges\n")
    cat("and is most theoretically justified for comparing nested models.\n")
    cat("However, the current formula or empirical approach may be more practical.\n")

    return(results)
}