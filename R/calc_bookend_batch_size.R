#' Calculate Batch Size for Bookend Strategy
#'
#' Predicts the number of simulations needed to reach a target ESS based on
#' the observed ESS trajectory. Fits linear and sqrt models to the cumulative
#' (n_sims, threshold_ESS) data and predicts from whichever has the higher
#' R-squared (linear on a tie), through the same helper as the auto-mode
#' calibration phase. Linear is the expected shape: the ESS the stopping rule
#' tracks is the function-specific ESS of a fixed posterior (bounded by its
#' Kish ESS), and for \eqn{n} independent prior draws the Kish ESS of fixed
#' importance weights grows as \eqn{n \, (E w)^2 / E w^2}, linearly in
#' \eqn{n}. Replaying the 2026-10 national runs, the coarsened-posterior Kish
#' ESS was linear in the number of draws (R-squared 0.995-1.000 against
#' 0.955-0.978 for sqrt). Before MOSAIC 1.3.0 sqrt was the default and linear
#' had to beat it by 0.03, which picked sqrt even at a linear R-squared of 1.000.
#' A log model is also fitted, but only as a diagnostic (a warning when it fits
#' best); it is never used for prediction.
#'
#' The extrapolation is only defined when the chosen model's ESS increases with
#' the number of simulations. When its slope is zero or negative the target is
#' unreachable on the fitted trajectory, and the function returns
#' \code{phase = "no_progress"} with \code{batch_size = 0} instead of squaring a
#' negative root into a positive requirement. The ESS can fall as draws are
#' added: a much better draw concentrates the weights. A fit below
#' \code{target_r_squared} returns \code{phase = "low_confidence"} with
#' \code{batch_size = 0} as well. In both cases \code{run_MOSAIC()} runs a batch
#' of \code{control$calibration$batch_size_adaptive} instead, capped by
#' \code{max_batch_predictive} and \code{max_simulations_total}, so a
#' non-monotone trajectory neither stalls the run nor sizes a batch from a
#' meaningless extrapolation.
#'
#' @param ess_history ESS measurements from calibration phase
#' @param target_ess Target ESS value
#' @param max_total_sims Maximum total simulations allowed
#' @param target_r_squared Target R-squared for ESS regression (default: 0.95)
#' @return List with batch size recommendation. \code{phase} is one of
#'   \code{"complete"}, \code{"low_confidence"}, \code{"no_progress"} or
#'   \code{"predictive"}; \code{batch_size} is positive only for
#'   \code{"predictive"}. \code{model} is \code{"linear"} or \code{"sqrt"} and
#'   \code{r_squared_linear} / \code{r_squared_sqrt} report both fits when a fit
#'   was made.
#' @export
calc_bookend_batch_size <- function(ess_history,
                                   target_ess,
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

    # Linear and sqrt growth models; the better R^2 predicts (linear on a tie).
    gf <- .mosaic_ess_growth_fit(n_sims, threshold_ess, target_ess)
    best_model <- gf$model
    best_r2 <- gf$r2
    best_fit <- gf$fit

    # Diagnostic: Warn if log fits best (NEVER use for prediction)
    if (all(n_sims > 0) && length(unique(n_sims)) >= 2L) {
        r2_log <- tryCatch(suppressWarnings(summary(lm(threshold_ess ~ log(n_sims)))$r.squared),
                           error = function(e) NA_real_)
        if (is.finite(r2_log) && r2_log > gf$r2_linear && r2_log > gf$r2_sqrt) {
            warning(sprintf(
                "Log model fits best (R\u00B2=%.4f) - suggests prior-posterior mismatch. Consider extending calibration.",
                r2_log
            ))
        }
    }

    # Check confidence against target R^2. Batch size 0: the caller runs its
    # adaptive batch size instead of a size from an unreliable fit.
    if (is.null(best_fit) || best_r2 < target_r_squared) {
        return(list(
            phase = "low_confidence",
            batch_size = 0,
            model = best_model,
            r_squared = best_r2,
            r_squared_linear = gf$r2_linear,
            r_squared_sqrt = gf$r2_sqrt,
            message = sprintf("Low confidence (R\u00B2=%.3f < %.2f), continuing calibration",
                            best_r2, target_r_squared)
        ))
    }

    # Predict total simulations needed. Both models are ESS = a + b * f(n) with
    # f increasing, so the target is reachable only when b > 0.
    a <- gf$intercept
    b <- gf$slope

    if (!is.finite(b) || b <= 0) {
        return(list(
            phase = "no_progress",
            batch_size = 0,
            model = best_model,
            r_squared = best_r2,
            r_squared_linear = gf$r2_linear,
            r_squared_sqrt = gf$r2_sqrt,
            message = sprintf(
                "ESS is not increasing with simulations (%s-model slope = %.4g); target %.1f is unreachable on the fitted trajectory",
                best_model, b, target_ess)
        ))
    }

    # With b > 0, a negative root means the fit places the target below its
    # intercept (reached at n <= 0); clamp it rather than squaring it positive.
    total_needed <- ceiling(gf$est_sims)

    # The fit places the target at or below the current size, yet the observed
    # ESS is still short of it (gap_to_target > 0 above): the fitted curve
    # over-predicts the current ESS, so it cannot size the next batch.
    if (total_needed <= current_n) {
        return(list(
            phase = "low_confidence",
            batch_size = 0,
            model = best_model,
            r_squared = best_r2,
            message = sprintf(
                paste0("Fitted %s model predicts target %.1f by n=%d, but ESS at current n=%d ",
                       "is still %.1f; fitted trajectory over-predicts current ESS"),
                best_model, target_ess, total_needed, current_n, current_ess
            )
        ))
    }

    # Calculate predictive batch size
    # Predictive batch uses full remaining budget
    predictive_batch_size <- total_needed - current_n

    # Apply safety margins based on R^2 confidence
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

    # Additional safety check for negative/zero batch size
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
    if (current_n + predictive_batch_size > max_total_sims) {
        predictive_batch_size <- max_total_sims - current_n
    }

    # Predict ESS after the batch
    # Models expect raw n_sims -- the formula already applies the transformation
    # The model formula already applies the transformation (sqrt or log)
    predicted_ess <- predict(best_fit,
        newdata = data.frame(n_sims = current_n + predictive_batch_size))

    return(list(
        phase = "predictive",
        batch_size = predictive_batch_size,
        model = best_model,
        r_squared = best_r2,
        r_squared_linear = gf$r2_linear,
        r_squared_sqrt = gf$r2_sqrt,
        safety_factor = safety_factor,
        current_ess = current_ess,
        target_ess = target_ess,
        predicted_ess = as.numeric(predicted_ess),
        total_predicted = current_n + predictive_batch_size,
        message = sprintf("Predictive batch: %.0f sims (model=%s, R\u00B2=%.3f)",
                         predictive_batch_size, best_model, best_r2)
    ))
}
