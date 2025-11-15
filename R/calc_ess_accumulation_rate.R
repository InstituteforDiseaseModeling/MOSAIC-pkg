#' Calculate ESS Accumulation Rate
#'
#' Tracks the rate at which ESS accumulates per simulation and predicts
#' the number of additional simulations needed to reach target ESS.
#'
#' @param ess_history List of ESS measurements at different simulation counts
#' @param target_ess Target ESS value for convergence
#' @param min_points Minimum number of data points needed for prediction (default: 3)
#' @return List containing accumulation metrics and predictions
#' @export
calc_ess_accumulation_rate <- function(ess_history, target_ess, min_points = 3) {

    if (length(ess_history) < min_points) {
        return(list(
            sufficient_data = FALSE,
            message = sprintf("Need at least %d measurements for rate calculation", min_points)
        ))
    }

    # Extract data points
    n_sims <- sapply(ess_history, function(x) x$total_sims)
    min_ess <- sapply(ess_history, function(x) x$min_ess)
    median_ess <- sapply(ess_history, function(x) x$median_ess)

    # Calculate rates (ESS per 1000 simulations)
    recent_points <- tail(seq_along(ess_history), min_points)

    # Linear regression on recent points for rate estimation
    fit_min <- lm(min_ess[recent_points] ~ n_sims[recent_points])
    fit_median <- lm(median_ess[recent_points] ~ n_sims[recent_points])

    rate_min <- coef(fit_min)[2] * 1000  # ESS per 1000 sims
    rate_median <- coef(fit_median)[2] * 1000

    # Current values
    current_n <- tail(n_sims, 1)
    current_min_ess <- tail(min_ess, 1)
    current_median_ess <- tail(median_ess, 1)

    # Predict simulations needed
    gap_to_target <- target_ess - current_min_ess

    if (rate_min <= 0) {
        # ESS not improving or decreasing
        sims_needed <- Inf
        predicted_convergence_n <- Inf
        efficiency_status <- "stagnant"
    } else {
        sims_needed <- ceiling(gap_to_target / (rate_min / 1000))
        predicted_convergence_n <- current_n + sims_needed

        # Classify efficiency
        if (rate_min > 50) {
            efficiency_status <- "high"
        } else if (rate_min > 20) {
            efficiency_status <- "moderate"
        } else if (rate_min > 5) {
            efficiency_status <- "low"
        } else {
            efficiency_status <- "very_low"
        }
    }

    # Calculate acceleration (is rate improving?)
    if (length(ess_history) >= 4) {
        # Compare recent rate to earlier rate
        early_points <- head(recent_points, 2)
        late_points <- tail(recent_points, 2)

        early_rate <- (min_ess[late_points[1]] - min_ess[early_points[1]]) /
                     (n_sims[late_points[1]] - n_sims[early_points[1]]) * 1000
        recent_rate <- (min_ess[late_points[2]] - min_ess[late_points[1]]) /
                      (n_sims[late_points[2]] - n_sims[late_points[1]]) * 1000

        acceleration <- recent_rate - early_rate
        trend <- ifelse(acceleration > 0, "improving",
                       ifelse(acceleration < -0.1, "degrading", "stable"))
    } else {
        acceleration <- NA
        trend <- "unknown"
    }

    # Recommend next batch size based on efficiency
    recommended_batch_size <- switch(efficiency_status,
        "high" = min(2000, max(500, sims_needed)),  # Large batches
        "moderate" = min(1000, max(500, sims_needed / 2)),  # Medium batches
        "low" = 500,  # Small batches to reassess frequently
        "very_low" = 250,  # Very small batches, may need intervention
        "stagnant" = 100  # Minimal batches, likely need parameter adjustment
    )

    # Round to nearest 100
    recommended_batch_size <- round(recommended_batch_size / 100) * 100
    recommended_batch_size <- max(100, recommended_batch_size)  # At least 100

    return(list(
        sufficient_data = TRUE,
        current_n = current_n,
        current_min_ess = current_min_ess,
        current_median_ess = current_median_ess,
        target_ess = target_ess,
        gap_to_target = gap_to_target,
        rate_min_per_1000 = rate_min,
        rate_median_per_1000 = rate_median,
        sims_needed = sims_needed,
        predicted_convergence_n = predicted_convergence_n,
        efficiency_status = efficiency_status,
        trend = trend,
        acceleration = acceleration,
        recommended_batch_size = recommended_batch_size,
        confidence = summary(fit_min)$r.squared  # R-squared as confidence metric
    ))
}

#' Calculate Optimal Batch Schedule
#'
#' Plans an efficient sequence of batch sizes to reach convergence
#' based on current ESS accumulation patterns.
#'
#' @param rate_analysis Output from calc_ess_accumulation_rate
#' @param max_batch_size Maximum allowed batch size
#' @param min_batch_size Minimum allowed batch size
#' @return Vector of recommended batch sizes
#' @export
calc_optimal_batch_schedule <- function(rate_analysis,
                                       max_batch_size = 5000,
                                       min_batch_size = 100) {

    if (!rate_analysis$sufficient_data) {
        # Not enough data, return conservative schedule
        return(c(500, 500, 500))  # Calibration batches
    }

    if (is.infinite(rate_analysis$sims_needed)) {
        # Stagnant, return minimal batches
        return(rep(min_batch_size, 3))
    }

    sims_remaining <- rate_analysis$sims_needed
    batch_schedule <- numeric()

    # Build schedule based on efficiency
    if (rate_analysis$efficiency_status == "high") {
        # Efficient: use larger batches
        while (sims_remaining > 0) {
            batch <- min(max_batch_size, sims_remaining)
            batch_schedule <- c(batch_schedule, batch)
            sims_remaining <- sims_remaining - batch
        }
    } else if (rate_analysis$efficiency_status == "moderate") {
        # Moderate: balanced approach
        batch_size <- min(2000, max_batch_size)
        while (sims_remaining > 0) {
            batch <- min(batch_size, sims_remaining)
            batch_schedule <- c(batch_schedule, batch)
            sims_remaining <- sims_remaining - batch
        }
    } else {
        # Low efficiency: frequent reassessment
        batch_size <- rate_analysis$recommended_batch_size
        # Only schedule next 2-3 batches for reassessment
        for (i in 1:3) {
            if (sims_remaining > 0) {
                batch <- min(batch_size, sims_remaining)
                batch_schedule <- c(batch_schedule, batch)
                sims_remaining <- sims_remaining - batch
            }
        }
    }

    # Ensure minimum batch size
    batch_schedule <- pmax(batch_schedule, min_batch_size)

    # Round to nearest 100
    batch_schedule <- round(batch_schedule / 100) * 100

    return(batch_schedule)
}

#' Format ESS Rate Analysis for Logging
#'
#' Creates formatted output for ESS accumulation analysis
#'
#' @param rate_analysis Output from calc_ess_accumulation_rate
#' @return Character vector of formatted messages
#' @export
format_ess_rate_analysis <- function(rate_analysis) {

    if (!rate_analysis$sufficient_data) {
        return(rate_analysis$message)
    }

    msgs <- character()

    msgs <- c(msgs, "\n=== ESS ACCUMULATION ANALYSIS ===")
    msgs <- c(msgs, sprintf("Current state: %d simulations", rate_analysis$current_n))
    msgs <- c(msgs, sprintf("  Min ESS:    %.1f / %.1f (%.1f%% of target)",
                           rate_analysis$current_min_ess,
                           rate_analysis$target_ess,
                           100 * rate_analysis$current_min_ess / rate_analysis$target_ess))
    msgs <- c(msgs, sprintf("  Median ESS: %.1f", rate_analysis$current_median_ess))

    msgs <- c(msgs, "\nAccumulation rate:")
    msgs <- c(msgs, sprintf("  Min ESS rate:    %.2f per 1000 sims",
                           rate_analysis$rate_min_per_1000))
    msgs <- c(msgs, sprintf("  Median ESS rate: %.2f per 1000 sims",
                           rate_analysis$rate_median_per_1000))
    msgs <- c(msgs, sprintf("  Efficiency:      %s", toupper(rate_analysis$efficiency_status)))
    msgs <- c(msgs, sprintf("  Trend:           %s", rate_analysis$trend))

    if (!is.na(rate_analysis$acceleration)) {
        msgs <- c(msgs, sprintf("  Acceleration:    %+.2f ESS/1000²",
                               rate_analysis$acceleration))
    }

    msgs <- c(msgs, "\nPredictions:")
    if (is.finite(rate_analysis$sims_needed)) {
        msgs <- c(msgs, sprintf("  Simulations needed:   %d", rate_analysis$sims_needed))
        msgs <- c(msgs, sprintf("  Predicted total:      %d",
                               rate_analysis$predicted_convergence_n))
        msgs <- c(msgs, sprintf("  Confidence (R²):      %.2f", rate_analysis$confidence))
    } else {
        msgs <- c(msgs, "  ⚠ ESS accumulation stagnant - intervention needed")
    }

    msgs <- c(msgs, sprintf("\nRecommended next batch: %d simulations",
                           rate_analysis$recommended_batch_size))

    return(paste(msgs, collapse = "\n"))
}