#' Calculate Batch Size for Bookend Strategy
#'
#' Implements a three-phase strategy:
#' 1. Initial calibration (n_batches × batch_size)
#' 2. One large predictive batch (calculated from ESS rate)
#' 3. Final fine-tuning (adaptive batch sizing based on gap)
#'
#' @param ess_history ESS measurements from calibration phase
#' @param target_ess Target ESS value
#' @param reserved_sims Number of simulations reserved for fine-tuning
#' @param max_total_sims Maximum total simulations allowed
#' @param target_r_squared Target R-squared for ESS regression (default: 0.95)
#' @return List with batch size recommendation
#' @export
calc_bookend_batch_size <- function(ess_history,
                                   target_ess,
                                   reserved_sims,
                                   max_total_sims,
                                   target_r_squared = 0.95) {

    # Extract current state - use threshold_ess if available (percentile-based)
    n_sims <- sapply(ess_history, function(x) x$total_sims)
    threshold_ess <- sapply(ess_history, function(x) {
        if ("threshold_ess" %in% names(x)) {
            x$threshold_ess
        } else {
            x$min_ess  # Fallback for compatibility with old data
        }
    })

    current_n <- tail(n_sims, 1)
    current_ess <- tail(threshold_ess, 1)
    gap_to_target <- target_ess - current_ess

    if (gap_to_target <= 0) {
        return(list(
            phase = "complete",
            batch_size = 0,
            message = "Already at target"
        ))
    }

    # Fit models (linear, sqrt, log)
    fit_linear <- lm(threshold_ess ~ n_sims)
    fit_sqrt <- lm(threshold_ess ~ sqrt(n_sims))
    fit_log <- NULL
    if (all(n_sims > 0)) {
        tryCatch({
            fit_log <- lm(threshold_ess ~ log(n_sims))
        }, error = function(e) {})
    }

    # Calculate R² values
    r2_linear <- summary(fit_linear)$r.squared
    r2_sqrt <- summary(fit_sqrt)$r.squared
    r2_log <- ifelse(!is.null(fit_log), summary(fit_log)$r.squared, 0)

    # DEFAULT TO SQRT (conservative, theoretically justified for importance sampling)
    best_model <- "sqrt"
    best_r2 <- r2_sqrt
    best_fit <- fit_sqrt

    # Only switch to linear with STRONG evidence
    # Require: High R² AND meaningful improvement over sqrt
    if (r2_linear >= 0.95 && r2_linear - r2_sqrt > 0.03) {
        best_model <- "linear"
        best_r2 <- r2_linear
        best_fit <- fit_linear
    }

    # Diagnostic: Warn if log fits best (NEVER use for prediction)
    if (r2_log > r2_linear && r2_log > r2_sqrt) {
        warning(sprintf(
            "Log model fits best (R²=%.4f) - suggests prior-posterior mismatch. Consider extending calibration.",
            r2_log
        ))
    }

    # Check confidence against target R²
    if (best_r2 < target_r_squared) {
        return(list(
            phase = "low_confidence",
            batch_size = 500,  # Default batch
            r_squared = best_r2,
            message = sprintf("Low confidence (R²=%.3f < %.2f), continuing calibration",
                            best_r2, target_r_squared)
        ))
    }

    # Predict total simulations needed
    if (best_model == "linear") {
        a <- coef(best_fit)[1]
        b <- coef(best_fit)[2]
        total_needed <- (target_ess - a) / b

    } else if (best_model == "sqrt") {
        a <- coef(best_fit)[1]
        b <- coef(best_fit)[2]
        total_needed <- ((target_ess - a) / b)^2

    } else if (best_model == "log") {
        a <- coef(best_fit)[1]
        b <- coef(best_fit)[2]
        total_needed <- exp((target_ess - a) / b)
    }

    total_needed <- ceiling(total_needed)

    # BUG FIX #5: Check if we're already past the predicted requirement
    if (total_needed <= current_n) {
        return(list(
            phase = "complete",
            batch_size = 0,
            message = sprintf(
                "Calibration already at/exceeded predicted requirement (current=%d, predicted=%d)",
                current_n, total_needed
            )
        ))
    }

    # Calculate predictive batch size
    # Total = current + predictive_batch + reserved_fine_tuning
    predictive_batch_size <- total_needed - current_n - reserved_sims

    # Apply safety margins based on R² confidence
    # Adjusted for better prediction accuracy with high-confidence models
    if (best_r2 >= 0.95) {
        safety_factor <- 1.05  # Slight overshoot OK when very confident
    } else if (best_r2 >= 0.92) {
        safety_factor <- 1.00  # Use full prediction
    } else if (best_r2 >= 0.88) {
        safety_factor <- 0.95  # Slight undershoot
    } else if (best_r2 >= 0.85) {
        safety_factor <- 0.90  # More conservative
    } else {
        safety_factor <- 0.85  # Very conservative for low confidence
    }

    predictive_batch_size <- ceiling(predictive_batch_size * safety_factor)

    # Apply bounds
    predictive_batch_size <- max(0, predictive_batch_size)

    # BUG FIX #5: Additional safety check for negative/zero batch size
    if (predictive_batch_size <= 0) {
        return(list(
            phase = "complete",
            batch_size = 0,
            message = sprintf(
                "Predicted batch size <= 0 after safety adjustments (gap_to_target=%.1f)",
                gap_to_target
            )
        ))
    }

    # Check against maximum
    if (current_n + predictive_batch_size + reserved_sims > max_total_sims) {
        predictive_batch_size <- max_total_sims - current_n - reserved_sims
    }

    # Predict ESS after the batch
    # BUG FIX #2: Models expect raw n_sims, not pre-transformed values
    # The model formula already applies the transformation (sqrt or log)
    predicted_ess <- predict(best_fit,
        newdata = data.frame(n_sims = current_n + predictive_batch_size))

    return(list(
        phase = "predictive",
        batch_size = predictive_batch_size,
        model = best_model,
        r_squared = best_r2,
        safety_factor = safety_factor,
        current_ess = current_ess,
        target_ess = target_ess,
        predicted_ess = as.numeric(predicted_ess),
        total_predicted = current_n + predictive_batch_size + reserved_sims,
        message = sprintf("Predictive batch: %.0f sims (model=%s, R²=%.3f)",
                         predictive_batch_size, best_model, best_r2)
    ))
}