#' Plot Posterior Predictive Checks for LASER Model
#'
#' Creates professional posterior predictive check (PPC) plots to assess model fit.
#' Generates three separate PDF files with density overlays, Q-Q plots, and residual analysis.
#'
#' @param model A laser-cholera Model object containing results and parameters
#' @param output_dir Directory where PPC plots will be saved
#' @param verbose Logical indicating whether to print progress messages (default: TRUE)
#'
#' @return NULL (invisibly). Creates PDF files as side effects.
#'
#' @details
#' This function creates three professional-quality PPC plots:
#' \itemize{
#'   \item ppc_density.pdf - Density overlays and observed vs predicted scatter plots
#'   \item ppc_qqplots.pdf - Q-Q plots comparing observed and predicted distributions
#'   \item ppc_residuals.pdf - Residual analysis including temporal patterns
#' }
#'
#' @export
#' @importFrom grDevices pdf dev.off rgb
#' @importFrom graphics plot lines abline legend mtext par grid polygon text points
#' @importFrom stats density loess predict quantile sd cor qqplot ppoints
#'
plot_model_ppc <- function(model,
                          output_dir,
                          verbose = TRUE) {

    # ============================================================================
    # Input validation
    # ============================================================================

    if (missing(model) || is.null(model)) {
         stop("model is required")
    }

    if (missing(output_dir) || is.null(output_dir)) {
         stop("output_dir is required")
    }

    # Check model structure
    if (!inherits(model, "laser_cholera.metapop.model.Model") && !is.list(model)) {
         stop("model must be a laser-cholera Model object (class 'laser_cholera.metapop.model.Model') or a list")
    }

    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    location_names <- model$params$location_name
    date_start <- model$params$date_start
    date_stop <- model$params$date_stop

    obs_cases <- model$params$reported_cases
    obs_deaths <- model$params$reported_deaths

    pred_cases <- model$results$expected_cases
    pred_deaths <- model$results$disease_deaths

    # ============================================================================
    # Define professional color scheme
    # ============================================================================

    col_observed <- "#2E86AB"      # Professional blue
    col_predicted <- "#A23B72"      # Professional purple-red
    col_reference <- "#F18F01"      # Orange for reference lines
    col_smooth <- "#C73E1D"         # Red-orange for smoothing
    col_grid <- "#F5F5F5"          # Light gray for grid
    col_points <- rgb(46/255, 134/255, 171/255, alpha = 0.4)  # Semi-transparent blue

    # ============================================================================
    # Prepare data
    # ============================================================================

    # Flatten matrices for analysis
    obs_cases_flat <- as.vector(obs_cases)
    obs_deaths_flat <- as.vector(obs_deaths)
    pred_cases_flat <- as.vector(pred_cases)
    pred_deaths_flat <- as.vector(pred_deaths)

    # Remove NAs and zeros for log transformation
    valid_cases <- !is.na(obs_cases_flat) & !is.na(pred_cases_flat) &
                   obs_cases_flat > 0 & pred_cases_flat > 0
    valid_deaths <- !is.na(obs_deaths_flat) & !is.na(pred_deaths_flat) &
                    obs_deaths_flat > 0 & pred_deaths_flat > 0

    obs_cases_valid <- obs_cases_flat[valid_cases]
    pred_cases_valid <- pred_cases_flat[valid_cases]
    obs_deaths_valid <- obs_deaths_flat[valid_deaths]
    pred_deaths_valid <- pred_deaths_flat[valid_deaths]

    # Calculate residuals
    residuals_cases <- obs_cases_flat - pred_cases_flat
    residuals_deaths <- obs_deaths_flat - pred_deaths_flat

    # Create time vector
    n_times <- nrow(obs_cases)
    n_locations <- ncol(obs_cases)
    time_vec <- rep(1:n_times, n_locations)

    if (verbose) {
        message(sprintf("Creating PPC plots for %d locations over %d time points",
                       n_locations, n_times))
        message(sprintf("Valid cases: %d, Valid deaths: %d",
                       length(obs_cases_valid), length(obs_deaths_valid)))
    }

    # ============================================================================
    # Plot 1: Density overlays and observed vs predicted scatter plots
    # ============================================================================

    pdf(file.path(output_dir, "ppc_density.pdf"), width = 14, height = 10)

    # Set up layout with professional margins
    par(mfrow = c(2, 2),
        mar = c(4.5, 4.5, 3, 2),
        oma = c(2, 2, 3, 1),
        mgp = c(2.5, 0.7, 0),
        las = 1,           # Horizontal axis labels
        cex.axis = 0.9,    # Axis text size
        cex.lab = 1.0,     # Label text size
        cex.main = 1.1,    # Title text size
        font.lab = 1,      # Plain font for labels
        font.main = 2)     # Bold font for titles

    # Add subtle background grid function
    add_grid <- function() {
        grid(nx = NULL, ny = NULL, col = col_grid, lty = 1, lwd = 0.5)
    }

    # Cases density overlay (need at least 2 points for density estimation)
    if (length(obs_cases_valid) >= 2 && !any(is.na(obs_cases_valid))) {
        # Calculate densities with robust fallback bandwidth selection
        dens_obs <- tryCatch(
            density(log(obs_cases_valid + 1), bw = "SJ"),
            error = function(e) tryCatch(
                density(log(obs_cases_valid + 1), bw = "nrd0"),
                error = function(e2) density(log(obs_cases_valid + 1), bw = 0.5)  # Fixed bandwidth
            )
        )
        dens_pred <- tryCatch(
            density(log(pred_cases_valid + 1), bw = "SJ"),
            error = function(e) tryCatch(
                density(log(pred_cases_valid + 1), bw = "nrd0"),
                error = function(e2) density(log(pred_cases_valid + 1), bw = 0.5)  # Fixed bandwidth
            )
        )

        # Set up plot with better limits
        xlim_range <- range(c(dens_obs$x, dens_pred$x))
        ylim_range <- c(0, max(c(dens_obs$y, dens_pred$y)) * 1.1)

        plot(dens_obs, col = col_observed, lwd = 2.5,
             main = "Distribution of Log-Transformed Cases",
             xlab = "log(Cases + 1)", ylab = "Density",
             xlim = xlim_range, ylim = ylim_range,
             type = "n")
        add_grid()

        # Add shaded areas under curves
        polygon(c(dens_obs$x, rev(dens_obs$x)),
                c(dens_obs$y, rep(0, length(dens_obs$y))),
                col = rgb(46/255, 134/255, 171/255, alpha = 0.2),
                border = NA)
        polygon(c(dens_pred$x, rev(dens_pred$x)),
                c(dens_pred$y, rep(0, length(dens_pred$y))),
                col = rgb(162/255, 59/255, 114/255, alpha = 0.2),
                border = NA)

        lines(dens_obs, col = col_observed, lwd = 2.5)
        lines(dens_pred, col = col_predicted, lwd = 2.5)

        legend("topright",
               legend = c("Observed", "Predicted"),
               col = c(col_observed, col_predicted),
               lwd = 2.5, bty = "n", cex = 0.9)
    } else {
        plot.new()
        text(0.5, 0.5, "Insufficient valid case data for density plot", cex = 1.2)
    }

    # Deaths density overlay (need at least 2 points for density estimation)
    if (length(obs_deaths_valid) >= 2 && !any(is.na(obs_deaths_valid))) {
        dens_obs <- tryCatch(
            density(log(obs_deaths_valid + 1), bw = "SJ"),
            error = function(e) tryCatch(
                density(log(obs_deaths_valid + 1), bw = "nrd0"),
                error = function(e2) density(log(obs_deaths_valid + 1), bw = 0.5)  # Fixed bandwidth
            )
        )
        dens_pred <- tryCatch(
            density(log(pred_deaths_valid + 1), bw = "SJ"),
            error = function(e) tryCatch(
                density(log(pred_deaths_valid + 1), bw = "nrd0"),
                error = function(e2) density(log(pred_deaths_valid + 1), bw = 0.5)  # Fixed bandwidth
            )
        )

        xlim_range <- range(c(dens_obs$x, dens_pred$x))
        ylim_range <- c(0, max(c(dens_obs$y, dens_pred$y)) * 1.1)

        plot(dens_obs, col = col_observed, lwd = 2.5,
             main = "Distribution of Log-Transformed Deaths",
             xlab = "log(Deaths + 1)", ylab = "Density",
             xlim = xlim_range, ylim = ylim_range,
             type = "n")
        add_grid()

        polygon(c(dens_obs$x, rev(dens_obs$x)),
                c(dens_obs$y, rep(0, length(dens_obs$y))),
                col = rgb(46/255, 134/255, 171/255, alpha = 0.2),
                border = NA)
        polygon(c(dens_pred$x, rev(dens_pred$x)),
                c(dens_pred$y, rep(0, length(dens_pred$y))),
                col = rgb(162/255, 59/255, 114/255, alpha = 0.2),
                border = NA)

        lines(dens_obs, col = col_observed, lwd = 2.5)
        lines(dens_pred, col = col_predicted, lwd = 2.5)

        legend("topright",
               legend = c("Observed", "Predicted"),
               col = c(col_observed, col_predicted),
               lwd = 2.5, bty = "n", cex = 0.9)
    } else {
        plot.new()
        text(0.5, 0.5, "Insufficient valid death data for density plot", cex = 1.2)
    }

    # Observed vs Predicted Cases (log scale)
    if (length(obs_cases_valid) > 0) {
        plot(log(pred_cases_valid + 1), log(obs_cases_valid + 1),
             pch = 19, col = col_points, cex = 0.6,
             xlab = "log(Predicted Cases + 1)",
             ylab = "log(Observed Cases + 1)",
             main = "Observed vs Predicted Cases",
             panel.first = add_grid())

        # Add 1:1 reference line
        abline(0, 1, col = col_reference, lwd = 2, lty = 2)

        # Add loess smooth
        if (length(obs_cases_valid) > 10) {
            tryCatch({
                lo <- loess(log(obs_cases_valid + 1) ~ log(pred_cases_valid + 1), span = 0.75)
                x_seq <- seq(min(log(pred_cases_valid + 1)),
                            max(log(pred_cases_valid + 1)),
                            length.out = 100)
                lo_pred <- predict(lo, x_seq)

                if (!any(is.na(lo_pred))) {
                    lines(x_seq, lo_pred, col = col_smooth, lwd = 2.5)
                }
            }, error = function(e) {
                if (verbose) message("Could not fit loess smooth for cases")
            })
        }

        # Add correlation text
        cor_val <- cor(log(pred_cases_valid + 1), log(obs_cases_valid + 1))
        text(min(log(pred_cases_valid + 1)), max(log(obs_cases_valid + 1)),
             paste0("r = ", round(cor_val, 3)),
             pos = 4, cex = 0.9, font = 2)

        legend("bottomright",
               legend = c("1:1 Line", "LOESS Smooth"),
               col = c(col_reference, col_smooth),
               lwd = c(2, 2.5), lty = c(2, 1),
               bty = "n", cex = 0.9)
    } else {
        plot.new()
        text(0.5, 0.5, "Insufficient valid case data for scatter plot", cex = 1.2)
    }

    # Observed vs Predicted Deaths (log scale)
    if (length(obs_deaths_valid) > 0) {
        plot(log(pred_deaths_valid + 1), log(obs_deaths_valid + 1),
             pch = 19, col = col_points, cex = 0.6,
             xlab = "log(Predicted Deaths + 1)",
             ylab = "log(Observed Deaths + 1)",
             main = "Observed vs Predicted Deaths",
             panel.first = add_grid())

        abline(0, 1, col = col_reference, lwd = 2, lty = 2)

        if (length(obs_deaths_valid) > 10) {
            tryCatch({
                lo <- loess(log(obs_deaths_valid + 1) ~ log(pred_deaths_valid + 1), span = 0.75)
                x_seq <- seq(min(log(pred_deaths_valid + 1)),
                            max(log(pred_deaths_valid + 1)),
                            length.out = 100)
                lo_pred <- predict(lo, x_seq)

                if (!any(is.na(lo_pred))) {
                    lines(x_seq, lo_pred, col = col_smooth, lwd = 2.5)
                }
            }, error = function(e) {
                if (verbose) message("Could not fit loess smooth for deaths")
            })
        }

        cor_val <- cor(log(pred_deaths_valid + 1), log(obs_deaths_valid + 1))
        text(min(log(pred_deaths_valid + 1)), max(log(obs_deaths_valid + 1)),
             paste0("r = ", round(cor_val, 3)),
             pos = 4, cex = 0.9, font = 2)

        legend("bottomright",
               legend = c("1:1 Line", "LOESS Smooth"),
               col = c(col_reference, col_smooth),
               lwd = c(2, 2.5), lty = c(2, 1),
               bty = "n", cex = 0.9)
    } else {
        plot.new()
        text(0.5, 0.5, "Insufficient valid death data for scatter plot", cex = 1.2)
    }

    mtext("Posterior Predictive Checks: Density and Calibration Analysis",
          outer = TRUE, cex = 1.3, font = 2, line = 1)
    dev.off()

    # ============================================================================
    # Plot 2: Q-Q plots
    # ============================================================================

    pdf(file.path(output_dir, "ppc_qqplots.pdf"), width = 12, height = 6)
    par(mfrow = c(1, 2),
        mar = c(4.5, 4.5, 3, 2),
        oma = c(2, 2, 3, 1),
        mgp = c(2.5, 0.7, 0),
        las = 1,
        cex.axis = 0.9,
        cex.lab = 1.0,
        cex.main = 1.1,
        font.lab = 1,
        font.main = 2)

    # Q-Q plot for cases
    if (length(obs_cases_flat) > 0 && length(pred_cases_flat) > 0) {
        # Remove NAs for Q-Q plot
        valid_qq <- !is.na(obs_cases_flat) & !is.na(pred_cases_flat)
        obs_qq <- obs_cases_flat[valid_qq]
        pred_qq <- pred_cases_flat[valid_qq]

        if (length(obs_qq) > 0) {
            qqplot(pred_qq, obs_qq,
                   pch = 19, col = col_points, cex = 0.6,
                   xlab = "Predicted Cases (Quantiles)",
                   ylab = "Observed Cases (Quantiles)",
                   main = "Q-Q Plot: Cases",
                   panel.first = add_grid())
            abline(0, 1, col = col_reference, lwd = 2.5, lty = 2)

            # Add confidence bands
            n <- length(obs_qq)
            probs <- ppoints(n)
            q_pred <- quantile(pred_qq, probs = probs)
            q_obs <- quantile(obs_qq, probs = probs)

            # Highlight deviations
            if (sd(obs_qq) > 0) {
                large_dev <- abs(q_obs - q_pred) > 2 * sd(obs_qq)
                if (any(large_dev)) {
                    points(q_pred[large_dev], q_obs[large_dev],
                          col = col_smooth, pch = 1, cex = 1.2)
                }
            }
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient valid case data for Q-Q plot", cex = 1.2)
        }
    } else {
        plot.new()
        text(0.5, 0.5, "Insufficient data for Q-Q plot", cex = 1.2)
    }

    # Q-Q plot for deaths
    if (length(obs_deaths_flat) > 0 && length(pred_deaths_flat) > 0) {
        valid_qq <- !is.na(obs_deaths_flat) & !is.na(pred_deaths_flat)
        obs_qq <- obs_deaths_flat[valid_qq]
        pred_qq <- pred_deaths_flat[valid_qq]

        if (length(obs_qq) > 0) {
            qqplot(pred_qq, obs_qq,
                   pch = 19, col = col_points, cex = 0.6,
                   xlab = "Predicted Deaths (Quantiles)",
                   ylab = "Observed Deaths (Quantiles)",
                   main = "Q-Q Plot: Deaths",
                   panel.first = add_grid())
            abline(0, 1, col = col_reference, lwd = 2.5, lty = 2)

            n <- length(obs_qq)
            probs <- ppoints(n)
            q_pred <- quantile(pred_qq, probs = probs)
            q_obs <- quantile(obs_qq, probs = probs)

            if (sd(obs_qq) > 0) {
                large_dev <- abs(q_obs - q_pred) > 2 * sd(obs_qq)
                if (any(large_dev)) {
                    points(q_pred[large_dev], q_obs[large_dev],
                          col = col_smooth, pch = 1, cex = 1.2)
                }
            }
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient valid death data for Q-Q plot", cex = 1.2)
        }
    } else {
        plot.new()
        text(0.5, 0.5, "Insufficient data for Q-Q plot", cex = 1.2)
    }

    mtext("Posterior Predictive Checks: Quantile-Quantile Analysis",
          outer = TRUE, cex = 1.3, font = 2, line = 1)
    dev.off()

    # ============================================================================
    # Plot 3: Residual analysis
    # ============================================================================

    pdf(file.path(output_dir, "ppc_residuals.pdf"), width = 14, height = 10)
    par(mfrow = c(2, 2),
        mar = c(4.5, 4.5, 3, 2),
        oma = c(2, 2, 3, 1),
        mgp = c(2.5, 0.7, 0),
        las = 1,
        cex.axis = 0.9,
        cex.lab = 1.0,
        cex.main = 1.1,
        font.lab = 1,
        font.main = 2)

    # Residuals vs observed values - Cases
    if (length(obs_cases_flat) > 0) {
        valid_resid <- !is.na(residuals_cases) & !is.na(obs_cases_flat)
        if (sum(valid_resid) > 0) {
            plot(obs_cases_flat[valid_resid], residuals_cases[valid_resid],
                 pch = 19, col = col_points, cex = 0.4,
                 xlab = "Observed Cases",
                 ylab = "Residuals (Observed - Predicted)",
                 main = "Residuals vs Observed: Cases",
                 panel.first = add_grid())
            abline(h = 0, col = col_reference, lwd = 2.5, lty = 2)

            # Add loess smooth
            if (sum(valid_resid) > 10) {
                tryCatch({
                    lo <- loess(residuals_cases[valid_resid] ~ obs_cases_flat[valid_resid], span = 0.75)
                    x_seq <- seq(min(obs_cases_flat[valid_resid]),
                                max(obs_cases_flat[valid_resid]),
                                length.out = 100)
                    lo_pred <- predict(lo, x_seq)
                    if (!any(is.na(lo_pred))) {
                        lines(x_seq, lo_pred, col = col_smooth, lwd = 2.5)
                    }
                }, error = function(e) {
                    if (verbose) message("Could not fit loess smooth for case residuals")
                })
            }

            # Add ±2 SD lines
            sd_resid <- sd(residuals_cases[valid_resid])
            if (!is.na(sd_resid) && sd_resid > 0) {
                abline(h = c(-2*sd_resid, 2*sd_resid), col = col_grid, lty = 3, lwd = 1)
            }
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient valid data for case residuals", cex = 1.2)
        }
    } else {
        plot.new()
        text(0.5, 0.5, "No case data available", cex = 1.2)
    }

    # Residuals vs observed values - Deaths
    if (length(obs_deaths_flat) > 0) {
        valid_resid <- !is.na(residuals_deaths) & !is.na(obs_deaths_flat)
        if (sum(valid_resid) > 0) {
            plot(obs_deaths_flat[valid_resid], residuals_deaths[valid_resid],
                 pch = 19, col = col_points, cex = 0.4,
                 xlab = "Observed Deaths",
                 ylab = "Residuals (Observed - Predicted)",
                 main = "Residuals vs Observed: Deaths",
                 panel.first = add_grid())
            abline(h = 0, col = col_reference, lwd = 2.5, lty = 2)

            if (sum(valid_resid) > 10) {
                tryCatch({
                    lo <- loess(residuals_deaths[valid_resid] ~ obs_deaths_flat[valid_resid], span = 0.75)
                    x_seq <- seq(min(obs_deaths_flat[valid_resid]),
                                max(obs_deaths_flat[valid_resid]),
                                length.out = 100)
                    lo_pred <- predict(lo, x_seq)
                    if (!any(is.na(lo_pred))) {
                        lines(x_seq, lo_pred, col = col_smooth, lwd = 2.5)
                    }
                }, error = function(e) {
                    if (verbose) message("Could not fit loess smooth for death residuals")
                })
            }

            sd_resid <- sd(residuals_deaths[valid_resid])
            if (!is.na(sd_resid) && sd_resid > 0) {
                abline(h = c(-2*sd_resid, 2*sd_resid), col = col_grid, lty = 3, lwd = 1)
            }
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient valid data for death residuals", cex = 1.2)
        }
    } else {
        plot.new()
        text(0.5, 0.5, "No death data available", cex = 1.2)
    }

    # Residuals over time - Cases
    if (length(time_vec) > 0 && length(residuals_cases) > 0) {
        valid_time <- !is.na(residuals_cases) & !is.na(time_vec)
        if (sum(valid_time) > 0) {
            plot(time_vec[valid_time], residuals_cases[valid_time],
                 pch = 19, col = col_points, cex = 0.3,
                 xlab = "Time Index",
                 ylab = "Residuals (Observed - Predicted)",
                 main = "Temporal Pattern of Case Residuals",
                 panel.first = add_grid())
            abline(h = 0, col = col_reference, lwd = 2.5, lty = 2)

            if (sum(valid_time) > 10) {
                tryCatch({
                    lo <- loess(residuals_cases[valid_time] ~ time_vec[valid_time], span = 0.3)
                    x_seq <- seq(min(time_vec[valid_time]),
                                max(time_vec[valid_time]),
                                length.out = 200)
                    lo_pred <- predict(lo, x_seq)
                    if (!any(is.na(lo_pred))) {
                        lines(x_seq, lo_pred, col = col_smooth, lwd = 2.5)
                    }
                }, error = function(e) {
                    if (verbose) message("Could not fit loess smooth for temporal case residuals")
                })
            }

            sd_resid <- sd(residuals_cases[valid_time])
            if (!is.na(sd_resid) && sd_resid > 0) {
                abline(h = c(-2*sd_resid, 2*sd_resid), col = col_grid, lty = 3, lwd = 1)
            }
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient valid temporal data for cases", cex = 1.2)
        }
    } else {
        plot.new()
        text(0.5, 0.5, "No temporal case data available", cex = 1.2)
    }

    # Residuals over time - Deaths
    if (length(time_vec) > 0 && length(residuals_deaths) > 0) {
        valid_time <- !is.na(residuals_deaths) & !is.na(time_vec)
        if (sum(valid_time) > 0) {
            plot(time_vec[valid_time], residuals_deaths[valid_time],
                 pch = 19, col = col_points, cex = 0.3,
                 xlab = "Time Index",
                 ylab = "Residuals (Observed - Predicted)",
                 main = "Temporal Pattern of Death Residuals",
                 panel.first = add_grid())
            abline(h = 0, col = col_reference, lwd = 2.5, lty = 2)

            if (sum(valid_time) > 10) {
                tryCatch({
                    lo <- loess(residuals_deaths[valid_time] ~ time_vec[valid_time], span = 0.3)
                    x_seq <- seq(min(time_vec[valid_time]),
                                max(time_vec[valid_time]),
                                length.out = 200)
                    lo_pred <- predict(lo, x_seq)
                    if (!any(is.na(lo_pred))) {
                        lines(x_seq, lo_pred, col = col_smooth, lwd = 2.5)
                    }
                }, error = function(e) {
                    if (verbose) message("Could not fit loess smooth for temporal death residuals")
                })
            }

            sd_resid <- sd(residuals_deaths[valid_time])
            if (!is.na(sd_resid) && sd_resid > 0) {
                abline(h = c(-2*sd_resid, 2*sd_resid), col = col_grid, lty = 3, lwd = 1)
            }
        } else {
            plot.new()
            text(0.5, 0.5, "Insufficient valid temporal data for deaths", cex = 1.2)
        }
    } else {
        plot.new()
        text(0.5, 0.5, "No temporal death data available", cex = 1.2)
    }

    mtext("Posterior Predictive Checks: Residual Diagnostics",
          outer = TRUE, cex = 1.3, font = 2, line = 1)
    dev.off()

    if (verbose) {
        message("PPC plots successfully saved to ", output_dir)
        message("  - ppc_density.pdf: Density overlays and calibration plots")
        message("  - ppc_qqplots.pdf: Q-Q plots for distributional assessment")
        message("  - ppc_residuals.pdf: Residual analysis and temporal patterns")
    }

    invisible(NULL)
}
