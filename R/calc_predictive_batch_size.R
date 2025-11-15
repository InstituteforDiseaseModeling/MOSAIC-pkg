#' Calculate Predictive Batch Size for Near-Complete Convergence
#'
#' After calibration phase, predicts the number of simulations needed to
#' reach near-convergence (e.g., 90-95% of target) in a single large batch.
#'
#' @param ess_history List of ESS measurements from calibration phase
#' @param target_ess Target ESS value for convergence
#' @param safety_factor Factor to apply (e.g., 0.9 = aim for 90% of needed sims)
#' @param max_batch_size Maximum allowed batch size
#' @param confidence_threshold Minimum R-squared for prediction (default: 0.7)
#' @return List with recommended batch size and prediction details
#' @export
calc_predictive_batch_size <- function(ess_history,
                                      target_ess,
                                      safety_factor = 0.9,
                                      max_batch_size = 10000,
                                      confidence_threshold = 0.7) {

    # Need at least 3 points for reliable prediction
    if (length(ess_history) < 3) {
        return(list(
            success = FALSE,
            message = "Insufficient calibration data",
            recommended_batch = 1000  # Fallback to standard batch
        ))
    }

    # Extract data
    n_sims <- sapply(ess_history, function(x) x$total_sims)
    min_ess <- sapply(ess_history, function(x) x$min_ess)

    # Current state
    current_n <- tail(n_sims, 1)
    current_ess <- tail(min_ess, 1)
    gap_to_target <- target_ess - current_ess

    if (gap_to_target <= 0) {
        return(list(
            success = TRUE,
            message = "Already at target",
            recommended_batch = 0
        ))
    }

    # Fit models for prediction
    # 1. Linear model (assumes constant rate)
    fit_linear <- lm(min_ess ~ n_sims)
    r2_linear <- summary(fit_linear)$r.squared
    rate_linear <- coef(fit_linear)[2]

    # 2. Log model (assumes diminishing returns)
    # ESS = a + b*log(n_sims)
    fit_log <- NULL
    r2_log <- 0
    if (all(n_sims > 0)) {
        tryCatch({
            fit_log <- lm(min_ess ~ log(n_sims))
            r2_log <- summary(fit_log)$r.squared
        }, error = function(e) {})
    }

    # 3. Square root model (common in Monte Carlo convergence)
    # ESS = a + b*sqrt(n_sims)
    fit_sqrt <- lm(min_ess ~ sqrt(n_sims))
    r2_sqrt <- summary(fit_sqrt)$r.squared

    # Choose best model
    best_model <- "linear"
    best_r2 <- r2_linear
    best_fit <- fit_linear

    if (r2_sqrt > best_r2) {
        best_model <- "sqrt"
        best_r2 <- r2_sqrt
        best_fit <- fit_sqrt
    }

    if (!is.null(fit_log) && r2_log > best_r2) {
        best_model <- "log"
        best_r2 <- r2_log
        best_fit <- fit_log
    }

    # Check confidence
    if (best_r2 < confidence_threshold) {
        # Low confidence - use conservative linear projection
        if (rate_linear > 0) {
            sims_needed <- ceiling(gap_to_target / rate_linear)
        } else {
            sims_needed <- 5000  # Fallback
        }

        return(list(
            success = FALSE,
            message = sprintf("Low prediction confidence (R²=%.2f)", best_r2),
            recommended_batch = min(max_batch_size, sims_needed),
            model_used = "linear_fallback",
            r_squared = best_r2
        ))
    }

    # Predict simulations needed based on best model
    if (best_model == "linear") {
        # Linear: ESS = a + b*n
        sims_needed <- ceiling((target_ess - coef(best_fit)[1]) / coef(best_fit)[2])
        sims_needed <- sims_needed - current_n

    } else if (best_model == "sqrt") {
        # Sqrt: ESS = a + b*sqrt(n)
        # Solve: target_ess = a + b*sqrt(n_total)
        a <- coef(best_fit)[1]
        b <- coef(best_fit)[2]
        n_total_needed <- ((target_ess - a) / b)^2
        sims_needed <- ceiling(n_total_needed - current_n)

    } else if (best_model == "log") {
        # Log: ESS = a + b*log(n)
        # Solve: target_ess = a + b*log(n_total)
        a <- coef(best_fit)[1]
        b <- coef(best_fit)[2]
        n_total_needed <- exp((target_ess - a) / b)
        sims_needed <- ceiling(n_total_needed - current_n)
    }

    # Apply safety factor
    sims_recommended <- ceiling(sims_needed * safety_factor)

    # Apply bounds
    sims_recommended <- max(100, min(max_batch_size, sims_recommended))

    # Calculate expected ESS after this batch
    if (best_model == "linear") {
        expected_ess <- predict(best_fit,
                               newdata = data.frame(n_sims = current_n + sims_recommended))
    } else if (best_model == "sqrt") {
        expected_ess <- predict(best_fit,
                               newdata = data.frame(n_sims = sqrt(current_n + sims_recommended)))
    } else {
        expected_ess <- predict(best_fit,
                               newdata = data.frame(n_sims = log(current_n + sims_recommended)))
    }

    return(list(
        success = TRUE,
        recommended_batch = sims_recommended,
        predicted_total_sims = current_n + sims_recommended,
        current_ess = current_ess,
        expected_ess = as.numeric(expected_ess),
        gap_to_target = gap_to_target,
        model_used = best_model,
        r_squared = best_r2,
        safety_factor = safety_factor,
        message = sprintf("Recommending %d simulations to reach ~%.0f%% of target",
                         sims_recommended, safety_factor * 100)
    ))
}

#' Plan Multi-Stage Batch Strategy
#'
#' Plans a multi-stage strategy: calibration -> large predictive batch -> fine-tuning
#'
#' @param current_ess Current minimum ESS
#' @param target_ess Target ESS
#' @param ess_history ESS history from calibration
#' @param stage Current stage ("calibration", "prediction", "fine_tuning")
#' @return Recommended batch size and stage info
#' @export
plan_batch_strategy <- function(current_ess, target_ess, ess_history = NULL,
                               stage = "calibration",
                               calibration_size = 2000,
                               calibration_batch = 500) {

    # Stage 1: Calibration
    if (stage == "calibration" || length(ess_history) < 4) {
        n_done <- ifelse(is.null(ess_history), 0,
                        tail(sapply(ess_history, function(x) x$total_sims), 1))

        if (n_done < calibration_size) {
            return(list(
                stage = "calibration",
                batch_size = calibration_batch,
                message = sprintf("Calibration batch %d of %d",
                                floor(n_done/calibration_batch) + 1,
                                calibration_size/calibration_batch)
            ))
        }
    }

    # Stage 2: Large predictive batch
    gap_percent <- (target_ess - current_ess) / target_ess

    if (gap_percent > 0.15) {  # More than 15% gap
        # Calculate large predictive batch
        pred_result <- calc_predictive_batch_size(
            ess_history = ess_history,
            target_ess = target_ess,
            safety_factor = 0.85,  # Aim for 85% of remaining gap
            max_batch_size = 10000
        )

        if (pred_result$success) {
            return(list(
                stage = "prediction",
                batch_size = pred_result$recommended_batch,
                expected_ess = pred_result$expected_ess,
                model = pred_result$model_used,
                confidence = pred_result$r_squared,
                message = sprintf("Large predictive batch: %d sims (%.0f%% confidence)",
                                pred_result$recommended_batch,
                                pred_result$r_squared * 100)
            ))
        }
    }

    # Stage 3: Fine-tuning (close to convergence)
    if (gap_percent <= 0.15 && gap_percent > 0.05) {
        # Medium batches for fine-tuning
        return(list(
            stage = "fine_tuning",
            batch_size = 500,
            message = sprintf("Fine-tuning: %.1f%% gap remaining", gap_percent * 100)
        ))
    }

    # Stage 4: Final convergence
    if (gap_percent <= 0.05) {
        # Small batches for precise convergence
        return(list(
            stage = "final",
            batch_size = 200,
            message = sprintf("Final convergence: %.1f%% gap remaining", gap_percent * 100)
        ))
    }

    # Fallback
    return(list(
        stage = "standard",
        batch_size = 1000,
        message = "Standard batch"
    ))
}