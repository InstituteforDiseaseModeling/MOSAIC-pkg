#' Plot Posterior Predictive Checks from Ensemble Predictions
#'
#' Creates professional posterior predictive check (PPC) plots to assess model fit
#' using CSV files from stochastic/ensemble predictions. Generates diagnostic plots
#' across six pages: density overlays, credible interval coverage, calibration
#' scatter plots, Q-Q plots, residuals vs observed, and temporal residual patterns.
#'
#' @param predictions_dir Character string specifying directory containing prediction CSV files.
#'   Function will auto-discover predictions_ensemble_*.csv or predictions_stochastic_*.csv files.
#' @param predictions_files Character vector of specific CSV file paths to use.
#'   Overrides predictions_dir if provided.
#' @param locations Character vector of specific locations to plot. NULL (default) uses all locations.
#' @param model Legacy: A laser-cholera Model object (for backward compatibility).
#'   Not recommended - use CSV-based inputs instead.
#' @param output_dir Directory where PPC plots will be saved. Creates "ppc" subdirectory.
#' @param verbose Logical indicating whether to print progress messages (default: TRUE)
#'
#' @return NULL (invisibly). Creates PDF files as side effects.
#'
#' @details
#' Creates a 6-page multi-page PDF per output file:
#' \enumerate{
#'   \item Density overlays of observed vs predicted median distributions
#'   \item Credible interval coverage analysis (50\% and 95\% CIs vs nominal)
#'   \item Observed vs predicted calibration scatter plots
#'   \item Quantile-quantile plots
#'   \item Residuals vs observed
#'   \item Temporal residual patterns
#' }
#'
#' Output files:
#' \itemize{
#'   \item ppc.pdf - Aggregate diagnostics (all locations combined)
#'   \item ppc_ISO.pdf - Per-location diagnostics (e.g., ppc_ETH.pdf, ppc_KEN.pdf)
#' }
#'
#' @examples
#' \dontrun{
#' # From predictions directory (auto-discovers CSV files)
#' plot_model_ppc(
#'   predictions_dir = "output/1_bfrs/plots/predictions",
#'   output_dir = "output/1_bfrs/plots"
#' )
#'
#' # Specific locations only
#' plot_model_ppc(
#'   predictions_dir = "output/plots/predictions",
#'   output_dir = "output/plots",
#'   locations = c("ETH", "KEN")
#' )
#' }
#'
#' @export
#' @importFrom grDevices pdf dev.off rgb
#' @importFrom graphics plot lines abline legend mtext par grid polygon text points rect axis
#' @importFrom stats density loess predict quantile sd cor qqplot ppoints
#' @importFrom utils read.csv
#'
plot_model_ppc <- function(predictions_dir = NULL,
                           predictions_files = NULL,
                           locations = NULL,
                           model = NULL,
                           output_dir,
                           verbose = TRUE) {

    # ============================================================================
    # Input validation
    # ============================================================================

    if (missing(output_dir) || is.null(output_dir)) {
        stop("output_dir is required")
    }

    use_csv   <- !is.null(predictions_dir) || !is.null(predictions_files)
    use_model <- !is.null(model)

    if (!use_csv && !use_model) {
        stop("Must provide predictions_dir, predictions_files, or model")
    }

    # ============================================================================
    # Color palette
    # ============================================================================

    palette <- list(
        observed    = "black",
        cases_pred  = "steelblue",
        deaths_pred = "darkred",
        reference   = "gray50",
        smooth      = "darkorange",
        alert       = "red",
        grid        = "gray85"
    )

    col_shade_obs    <- rgb(0,           0,           0,           alpha = 0.2)
    col_shade_cases  <- rgb(70/255,  130/255,  180/255,  alpha = 0.2)
    col_shade_deaths <- rgb(139/255,   0,           0,           alpha = 0.2)
    col_points_cases  <- rgb(70/255,  130/255,  180/255,  alpha = 0.4)
    col_points_deaths <- rgb(139/255,   0,           0,           alpha = 0.4)

    # ============================================================================
    # Load data
    # ============================================================================

    if (use_csv) {

        if (verbose) message("=== Loading predictions from CSV files ===")

        if (!is.null(predictions_dir)) {
            ensemble_files   <- list.files(predictions_dir,
                                           pattern = "^predictions_ensemble_.*\\.csv$",
                                           full.names = TRUE)
            stochastic_files <- list.files(predictions_dir,
                                           pattern = "^predictions_stochastic_.*\\.csv$",
                                           full.names = TRUE)
            csv_files <- if (length(ensemble_files) > 0) {
                if (verbose) message("Using ensemble predictions (", length(ensemble_files), " files)")
                ensemble_files
            } else if (length(stochastic_files) > 0) {
                if (verbose) message("Using stochastic predictions (", length(stochastic_files), " files)")
                stochastic_files
            } else {
                stop("No prediction CSV files found in: ", predictions_dir)
            }
        } else {
            csv_files <- predictions_files
            if (verbose) message("Using ", length(csv_files), " specified prediction files")
        }

        all_data <- do.call(rbind, lapply(csv_files, function(f) {
            tryCatch(read.csv(f, stringsAsFactors = FALSE),
                     error = function(e) { warning("Failed to read: ", f, " - ", e$message); NULL })
        }))

        if (is.null(all_data) || nrow(all_data) == 0) {
            stop("Failed to load any valid prediction data from CSV files")
        }

        if (!is.null(locations)) {
            all_data <- all_data[all_data$location %in% locations, ]
            if (nrow(all_data) == 0) {
                stop("No data found for specified locations: ", paste(locations, collapse = ", "))
            }
        }

        available_locations <- unique(all_data$location)
        n_locations         <- length(available_locations)

        if (verbose) {
            message("Loaded predictions for ", n_locations, " location(s): ",
                    paste(available_locations, collapse = ", "))
            message("Total rows: ", format(nrow(all_data), big.mark = ","))
        }

        pred_col <- if ("predicted_median" %in% names(all_data)) {
            "predicted_median"
        } else if ("predicted_mean" %in% names(all_data)) {
            "predicted_mean"
        } else {
            stop("CSV must contain 'predicted_median' or 'predicted_mean' column")
        }

        if (verbose) message("Using prediction column: ", pred_col)

        # Parse dates if present
        has_dates <- "date" %in% names(all_data)
        if (has_dates) all_data$date <- as.Date(all_data$date)

        # Detect CI columns (ci_1 = 95%, ci_2 = 50% by default envelope_quantiles)
        has_ci <- all(c("ci_1_lower", "ci_1_upper", "ci_2_lower", "ci_2_upper") %in% names(all_data))
        if (verbose && has_ci) message("Credible interval columns detected (ci_1 = 95%, ci_2 = 50%)")

        data_source <- "ensemble predictions"

    } else {

        if (verbose) message("=== Extracting data from model object (legacy mode) ===")

        if (!inherits(model, "laser.cholera.metapop.model.Model") && !is.list(model)) {
            stop("model must be a laser-cholera Model object or a list")
        }

        obs_cases   <- model$params$reported_cases
        obs_deaths  <- model$params$reported_deaths
        pred_cases  <- model$results$reported_cases
        pred_deaths <- model$results$disease_deaths

        obs_cases_flat   <- as.vector(obs_cases)
        obs_deaths_flat  <- as.vector(obs_deaths)
        pred_cases_flat  <- as.vector(pred_cases)
        pred_deaths_flat <- as.vector(pred_deaths)

        n_times     <- if (is.matrix(obs_cases)) nrow(obs_cases) else length(obs_cases)
        n_locations <- if (is.matrix(obs_cases)) ncol(obs_cases) else 1

        time_vec_legacy     <- rep(1:n_times, n_locations)
        location_names      <- model$params$location_name
        available_locations <- if (!is.null(location_names)) location_names else "Location"
        has_dates           <- FALSE
        has_ci              <- FALSE
        data_source         <- "single best model"

        if (verbose) message("Extracted data for ", n_locations, " location(s)")
    }

    # ============================================================================
    # Output directory
    # ============================================================================

    ppc_dir <- file.path(output_dir, "ppc")
    if (!dir.exists(ppc_dir)) {
        dir.create(ppc_dir, recursive = TRUE)
        if (verbose) message("Created PPC directory: ", ppc_dir)
    }

    # ============================================================================
    # Core plotting function
    # ============================================================================

    create_ppc_plots <- function(obs_cases_flat,  pred_cases_flat,
                                 obs_deaths_flat, pred_deaths_flat,
                                 dates_vec  = NULL,
                                 ci_cases   = NULL,   # list(lo50, hi50, lo95, hi95)
                                 ci_deaths  = NULL,
                                 location_label = "") {

        suffix <- if (nchar(location_label) > 0) paste0("_", location_label) else ""

        # Valid pairs for density/scatter: remove NAs only (log(x+1) handles zeros)
        valid_cases  <- !is.na(obs_cases_flat)  & !is.na(pred_cases_flat)
        valid_deaths <- !is.na(obs_deaths_flat) & !is.na(pred_deaths_flat)

        obs_cases_valid  <- obs_cases_flat[valid_cases]
        pred_cases_valid <- pred_cases_flat[valid_cases]
        obs_deaths_valid <- obs_deaths_flat[valid_deaths]
        pred_deaths_valid <- pred_deaths_flat[valid_deaths]

        n_cases_valid  <- length(obs_cases_valid)
        n_deaths_valid <- length(obs_deaths_valid)

        # Residuals (raw scale, all non-NA pairs)
        residuals_cases  <- obs_cases_flat  - pred_cases_flat
        residuals_deaths <- obs_deaths_flat - pred_deaths_flat

        if (verbose && suffix == "") {
            message("Valid data points (non-NA):")
            message("  Cases:  ", n_cases_valid,  " / ", length(obs_cases_flat))
            message("  Deaths: ", n_deaths_valid, " / ", length(obs_deaths_flat))
        }

        add_grid <- function() {
            grid(nx = NULL, ny = NULL, col = palette$grid, lty = 1, lwd = 0.5)
        }

        par_base <- list(
            mar    = c(4.5, 4.5, 3, 2),
            oma    = c(2, 2, 3, 1),
            mgp    = c(2.5, 0.7, 0),
            las    = 1,
            cex.axis = 0.9,
            cex.lab  = 1.0,
            cex.main = 1.1,
            font.lab  = 1,
            font.main = 2
        )

        pdf(file.path(ppc_dir, paste0("ppc", suffix, ".pdf")), width = 14, height = 7)

        # ======================================================================
        # PAGE 1: Density overlays
        # Compares the marginal distribution of observations to the distribution
        # of posterior predictive medians. Note: this is not a full PPC density
        # (which would require all ensemble draws); it assesses whether the
        # central tendency of predictions matches the data distribution.
        # ======================================================================

        do.call(par, c(list(mfrow = c(1, 2)), par_base))

        .plot_density_panel <- function(obs_v, pred_v, metric, col_pred, col_shade_pred, n_total) {
            if (length(obs_v) >= 2) {
                .safe_density <- function(x) {
                    tryCatch(density(x, bw = "SJ",   adjust = 2), error = function(e)
                    tryCatch(density(x, bw = "nrd0", adjust = 2), error = function(e2)
                             density(x, bw = 0.5,   adjust = 2)))
                }
                d_obs  <- .safe_density(log(obs_v  + 1))
                d_pred <- .safe_density(log(pred_v + 1))

                xl <- range(c(d_obs$x, d_pred$x))
                yl <- c(0, max(c(d_obs$y, d_pred$y)) * 1.1)

                plot(d_obs, col = palette$observed, lwd = 2.5,
                     main = paste0("Observed vs Predicted Median: ", metric),
                     sub  = paste0("n = ", length(obs_v), " non-NA time points"),
                     xlab = paste0("log(", metric, " + 1)"), ylab = "Density",
                     xlim = xl, ylim = yl, type = "n")
                add_grid()
                polygon(c(d_obs$x,  rev(d_obs$x)),  c(d_obs$y,  rep(0, length(d_obs$y))),
                        col = col_shade_obs,   border = NA)
                polygon(c(d_pred$x, rev(d_pred$x)), c(d_pred$y, rep(0, length(d_pred$y))),
                        col = col_shade_pred,  border = NA)
                lines(d_obs,  col = palette$observed, lwd = 2.5)
                lines(d_pred, col = col_pred,          lwd = 2.5)
                legend("topright",
                       legend = c("Observed", "Predicted Median"),
                       col    = c(palette$observed, col_pred),
                       lwd    = 2.5, bty = "n", cex = 0.9)
            } else {
                plot.new()
                text(0.5, 0.5, paste0("Insufficient data for density plot\n",
                                      "(", length(obs_v), " of ", n_total, " non-NA)"),
                     cex = 1.1, adj = 0.5)
            }
        }

        .plot_density_panel(obs_cases_valid,  pred_cases_valid,
                            "Suspected Cases", palette$cases_pred,  col_shade_cases,
                            length(obs_cases_flat))
        .plot_density_panel(obs_deaths_valid, pred_deaths_valid,
                            "Deaths",          palette$deaths_pred, col_shade_deaths,
                            length(obs_deaths_flat))

        mtext(if (nchar(location_label) > 0)
                  paste0("PPC (", location_label, "): Marginal Distribution Comparison")
              else "PPC: Marginal Distribution Comparison",
              outer = TRUE, cex = 1.3, font = 2, line = 1)

        # ======================================================================
        # PAGE 2: Credible interval coverage analysis
        # Key Bayesian PPC diagnostic: a well-calibrated posterior predictive
        # distribution should contain the observed data at its nominal rate
        # (i.e., ~50% of observations should fall in the 50% CI, ~95% in the
        # 95% CI). Systematic under-coverage indicates overconfident intervals;
        # over-coverage indicates conservative intervals.
        # ======================================================================

        do.call(par, c(list(mfrow = c(1, 2)), par_base))

        .plot_coverage_panel <- function(obs_v, pred_v, lo50, hi50, lo95, hi95,
                                         metric, col_pred) {
            valid <- !is.na(obs_v) & !is.na(pred_v)
            obs_v_valid <- obs_v[valid]

            if (!is.null(lo50) && !is.null(hi50) && !is.null(lo95) && !is.null(hi95) &&
                length(obs_v_valid) > 0) {

                lo50_v <- lo50[valid]; hi50_v <- hi50[valid]
                lo95_v <- lo95[valid]; hi95_v <- hi95[valid]

                cov50 <- mean(obs_v_valid >= lo50_v & obs_v_valid <= hi50_v, na.rm = TRUE) * 100
                cov95 <- mean(obs_v_valid >= lo95_v & obs_v_valid <= hi95_v, na.rm = TRUE) * 100

                # Bayesian p-value: P(pred > obs)
                bp <- mean(pred_v[valid] > obs_v_valid, na.rm = TRUE)

                bar_vals <- c(cov50, cov95)
                nom_vals <- c(50, 95)
                bar_cols <- sapply(seq_along(bar_vals), function(k) {
                    diff <- abs(bar_vals[k] - nom_vals[k])
                    if (diff <= 5)  "#4CAF50"   # green: within 5pp
                    else if (diff <= 15) "#FF9800" # orange: 5-15pp
                    else "#F44336"                  # red: >15pp
                })

                plot(0, 0, type = "n",
                     xlim = c(0, 3), ylim = c(0, 110),
                     xaxt = "n", yaxt = "n",
                     xlab = "", ylab = "Coverage (%)",
                     main = paste0("CI Coverage: ", metric))
                add_grid()
                axis(2, at = seq(0, 100, 10))
                axis(1, at = c(1, 2), labels = c("50% CI", "95% CI"), tick = FALSE, cex.axis = 1.1)

                # Draw bars
                rect(0.55, 0, 1.45, bar_vals[1], col = bar_cols[1], border = "white", lwd = 1.5)
                rect(1.55, 0, 2.45, bar_vals[2], col = bar_cols[2], border = "white", lwd = 1.5)

                # Nominal reference lines
                segments(0.55, nom_vals[1], 1.45, nom_vals[1],
                         col = "black", lwd = 2.5, lty = 2)
                segments(1.55, nom_vals[2], 2.45, nom_vals[2],
                         col = "black", lwd = 2.5, lty = 2)

                # Coverage labels on bars
                text(1.0, max(bar_vals[1] + 4, 8),
                     paste0(round(cov50, 1), "%"), cex = 1.0, font = 2)
                text(2.0, max(bar_vals[2] + 4, 8),
                     paste0(round(cov95, 1), "%"), cex = 1.0, font = 2)

                # Bayesian p-value annotation
                mtext(paste0("Bayesian p = ", round(bp, 3),
                             "  (n = ", length(obs_v_valid), ")"),
                      side = 1, line = 2.5, cex = 0.85, font = 3)

                legend("topright",
                       legend = c("Nominal coverage", "Color: |obs - nominal|",
                                  "  \u22645pp (good)", "  5-15pp (warn)", "  >15pp (poor)"),
                       col    = c("black", NA, "#4CAF50", "#FF9800", "#F44336"),
                       lty    = c(2, NA, 1, 1, 1),
                       lwd    = c(2.5, NA, 6, 6, 6),
                       bty    = "n", cex = 0.8)

            } else {
                plot.new()
                if (is.null(lo50)) {
                    text(0.5, 0.5, paste0("No CI data available for\n", metric),
                         cex = 1.1, adj = 0.5)
                } else {
                    text(0.5, 0.5, paste0("Insufficient data for coverage\nanalysis: ", metric),
                         cex = 1.1, adj = 0.5)
                }
            }
        }

        .plot_coverage_panel(
            obs_cases_flat, pred_cases_flat,
            lo50 = if (!is.null(ci_cases)) ci_cases$lo50 else NULL,
            hi50 = if (!is.null(ci_cases)) ci_cases$hi50 else NULL,
            lo95 = if (!is.null(ci_cases)) ci_cases$lo95 else NULL,
            hi95 = if (!is.null(ci_cases)) ci_cases$hi95 else NULL,
            metric = "Suspected Cases", col_pred = palette$cases_pred
        )
        .plot_coverage_panel(
            obs_deaths_flat, pred_deaths_flat,
            lo50 = if (!is.null(ci_deaths)) ci_deaths$lo50 else NULL,
            hi50 = if (!is.null(ci_deaths)) ci_deaths$hi50 else NULL,
            lo95 = if (!is.null(ci_deaths)) ci_deaths$lo95 else NULL,
            hi95 = if (!is.null(ci_deaths)) ci_deaths$hi95 else NULL,
            metric = "Deaths", col_pred = palette$deaths_pred
        )

        mtext(if (nchar(location_label) > 0)
                  paste0("PPC (", location_label, "): Credible Interval Coverage")
              else "PPC: Credible Interval Coverage",
              outer = TRUE, cex = 1.3, font = 2, line = 1)

        # ======================================================================
        # PAGE 3: Observed vs predicted scatter (calibration plot)
        # ======================================================================

        do.call(par, c(list(mfrow = c(1, 2)), par_base))

        .plot_scatter_panel <- function(obs_v, pred_v, metric, col_pts, col_line) {
            n_valid <- length(obs_v)
            if (n_valid > 0) {

                lx_obs  <- log(obs_v  + 1)
                lx_pred <- log(pred_v + 1)

                plot(lx_pred, lx_obs,
                     pch = 19, col = col_pts, cex = 0.6,
                     xlab = paste0("log(Predicted ", metric, " + 1)"),
                     ylab = paste0("log(Observed ",  metric, " + 1)"),
                     main = paste0("Observed vs Predicted: ", metric),
                     panel.first = add_grid())

                abline(0, 1, col = palette$reference, lwd = 2, lty = 2)

                drew_loess <- FALSE
                if (n_valid > 10 && sd(lx_pred) > 0) {
                    tryCatch({
                        lo     <- suppressWarnings(loess(lx_obs ~ lx_pred, span = 0.75))
                        x_seq  <- seq(min(lx_pred), max(lx_pred), length.out = 100)
                        lo_fit <- suppressWarnings(predict(lo, x_seq))
                        if (!any(is.na(lo_fit))) {
                            lines(x_seq, lo_fit, col = col_line, lwd = 2.5)
                            drew_loess <- TRUE
                        }
                    }, error = function(e) {})
                }

                # Correlation — only meaningful when both variables have variance
                if (sd(lx_pred) > 0 && sd(lx_obs) > 0) {
                    r_val <- round(cor(lx_pred, lx_obs), 3)
                    text(min(lx_pred), max(lx_obs),
                         paste0("r = ", r_val), pos = 4, cex = 0.9, font = 2)
                }

                leg_labels <- "1:1 Line"
                leg_cols   <- palette$reference
                leg_lwds   <- 2
                leg_ltys   <- 2
                if (drew_loess) {
                    leg_labels <- c(leg_labels, "LOESS Smooth")
                    leg_cols   <- c(leg_cols,   col_line)
                    leg_lwds   <- c(leg_lwds,   2.5)
                    leg_ltys   <- c(leg_ltys,   1)
                }
                legend("bottomright",
                       legend = leg_labels, col = leg_cols,
                       lwd = leg_lwds, lty = leg_ltys,
                       bty = "n", cex = 0.9)

            } else {
                plot.new()
                text(0.5, 0.5, paste0("No valid data for scatter plot\n(", metric, ")"),
                     cex = 1.1, adj = 0.5)
            }
        }

        .plot_scatter_panel(obs_cases_valid,  pred_cases_valid,
                            "Suspected Cases", col_points_cases,  palette$cases_pred)
        .plot_scatter_panel(obs_deaths_valid, pred_deaths_valid,
                            "Deaths",          col_points_deaths, palette$deaths_pred)

        mtext(if (nchar(location_label) > 0)
                  paste0("PPC (", location_label, "): Calibration Scatter")
              else "PPC: Calibration Scatter",
              outer = TRUE, cex = 1.3, font = 2, line = 1)

        # ======================================================================
        # PAGE 4: Q-Q plots
        # Compares the empirical quantile distributions of observed and predicted
        # values. Systematic departure from the 1:1 line indicates distributional
        # misfit (e.g., underdispersion, tail behaviour).
        # ======================================================================

        do.call(par, c(list(mfrow = c(1, 2)), par_base))

        .plot_qq_panel <- function(obs_flat, pred_flat, metric, col_pts) {
            valid_qq <- !is.na(obs_flat) & !is.na(pred_flat)
            n_valid  <- sum(valid_qq)
            if (n_valid > 0) {
                obs_qq  <- obs_flat[valid_qq]
                pred_qq <- pred_flat[valid_qq]

                qqplot(pred_qq, obs_qq,
                       pch = 19, col = col_pts, cex = 0.6,
                       xlab = paste0("Predicted ", metric, " (Quantiles)"),
                       ylab = paste0("Observed ",  metric, " (Quantiles)"),
                       main = paste0("Q-Q Plot: ", metric),
                       sub  = paste0("n = ", n_valid),
                       panel.first = add_grid())
                abline(0, 1, col = palette$reference, lwd = 2.5, lty = 2)

                if (sd(obs_qq) > 0 && n_valid > 5) {
                    probs  <- ppoints(n_valid)
                    q_pred <- quantile(pred_qq, probs = probs)
                    q_obs  <- quantile(obs_qq,  probs = probs)
                    large_dev <- abs(q_obs - q_pred) > 2 * sd(obs_qq)
                    if (any(large_dev)) {
                        points(q_pred[large_dev], q_obs[large_dev],
                               col = palette$alert, pch = 1, cex = 1.2, lwd = 2)
                    }
                }
            } else {
                plot.new()
                text(0.5, 0.5, paste0("No valid data for Q-Q plot\n(", metric, ")"),
                     cex = 1.1, adj = 0.5)
            }
        }

        .plot_qq_panel(obs_cases_flat,  pred_cases_flat,  "Suspected Cases", col_points_cases)
        .plot_qq_panel(obs_deaths_flat, pred_deaths_flat, "Deaths",          col_points_deaths)

        mtext(if (nchar(location_label) > 0)
                  paste0("PPC (", location_label, "): Quantile-Quantile Analysis")
              else "PPC: Quantile-Quantile Analysis",
              outer = TRUE, cex = 1.3, font = 2, line = 1)

        # ======================================================================
        # PAGE 5: Residuals vs observed
        # ======================================================================

        do.call(par, c(list(mfrow = c(1, 2)), par_base))

        .plot_resid_panel <- function(obs_flat, resid_vec, metric, col_pts, col_line) {
            valid_r <- !is.na(resid_vec) & !is.na(obs_flat)
            n_valid <- sum(valid_r)
            if (n_valid > 0) {
                plot(obs_flat[valid_r], resid_vec[valid_r],
                     pch = 19, col = col_pts, cex = 0.4,
                     xlab = paste0("Observed ", metric),
                     ylab = "Residuals (Observed - Predicted)",
                     main = paste0("Residuals vs Observed: ", metric),
                     sub  = paste0("n = ", n_valid),
                     panel.first = add_grid())
                abline(h = 0, col = palette$reference, lwd = 2.5, lty = 2)

                if (n_valid > 10) {
                    tryCatch({
                        lo     <- suppressWarnings(
                            loess(resid_vec[valid_r] ~ obs_flat[valid_r], span = 0.75))
                        x_seq  <- seq(min(obs_flat[valid_r]), max(obs_flat[valid_r]),
                                      length.out = 100)
                        lo_fit <- suppressWarnings(predict(lo, x_seq))
                        if (!any(is.na(lo_fit))) lines(x_seq, lo_fit, col = col_line, lwd = 2.5)
                    }, error = function(e) {})
                }

                sd_r <- sd(resid_vec[valid_r])
                if (!is.na(sd_r) && sd_r > 0) {
                    abline(h = c(-2 * sd_r, 2 * sd_r), col = palette$grid, lty = 3, lwd = 1.5)
                }
            } else {
                plot.new()
                text(0.5, 0.5, paste0("No valid data for residual plot\n(", metric, ")"),
                     cex = 1.1, adj = 0.5)
            }
        }

        .plot_resid_panel(obs_cases_flat,  residuals_cases,  "Suspected Cases",
                          col_points_cases,  palette$cases_pred)
        .plot_resid_panel(obs_deaths_flat, residuals_deaths, "Deaths",
                          col_points_deaths, palette$deaths_pred)

        mtext(if (nchar(location_label) > 0)
                  paste0("PPC (", location_label, "): Residuals vs Observed")
              else "PPC: Residuals vs Observed",
              outer = TRUE, cex = 1.3, font = 2, line = 1)

        # ======================================================================
        # PAGE 6: Temporal residual patterns
        # Uses actual dates on x-axis when available (single-location mode).
        # Helps detect systematic temporal biases (e.g., seasonal miscalibration).
        # ======================================================================

        do.call(par, c(list(mfrow = c(1, 2)), par_base))

        use_dates  <- !is.null(dates_vec) && inherits(dates_vec, "Date") && length(dates_vec) > 0
        x_vec      <- if (use_dates) dates_vec else seq_along(obs_cases_flat)
        x_lab      <- if (use_dates) "Date" else "Time Index"

        .plot_temporal_panel <- function(x_v, resid_vec, metric, col_pts, col_line, use_dates) {
            valid_t <- !is.na(resid_vec) & !is.na(x_v)
            n_valid <- sum(valid_t)
            if (n_valid > 0) {
                x_num <- if (use_dates) as.numeric(x_v[valid_t]) else x_v[valid_t]

                plot(x_v[valid_t], resid_vec[valid_t],
                     pch  = 19, col = col_pts, cex = 0.3,
                     xlab = x_lab,
                     ylab = "Residuals (Observed - Predicted)",
                     main = paste0("Temporal Residuals: ", metric),
                     xaxt = if (use_dates) "n" else "s",
                     panel.first = add_grid())

                if (use_dates) {
                    axis.Date(1, at = pretty(x_v[valid_t]), format = "%b %Y", las = 2)
                }

                abline(h = 0, col = palette$reference, lwd = 2.5, lty = 2)

                if (n_valid > 10 && sd(x_num) > 0) {
                    tryCatch({
                        lo     <- suppressWarnings(loess(resid_vec[valid_t] ~ x_num, span = 0.3))
                        x_seq  <- seq(min(x_num), max(x_num), length.out = 200)
                        lo_fit <- suppressWarnings(predict(lo, x_seq))
                        if (!any(is.na(lo_fit))) {
                            x_plot <- if (use_dates) as.Date(x_seq, origin = "1970-01-01") else x_seq
                            lines(x_plot, lo_fit, col = col_line, lwd = 2.5)
                        }
                    }, error = function(e) {})
                }

                sd_r <- sd(resid_vec[valid_t])
                if (!is.na(sd_r) && sd_r > 0) {
                    abline(h = c(-2 * sd_r, 2 * sd_r), col = palette$grid, lty = 3, lwd = 1.5)
                }
            } else {
                plot.new()
                text(0.5, 0.5, paste0("No valid temporal data\n(", metric, ")"),
                     cex = 1.1, adj = 0.5)
            }
        }

        # Align x_vec to cases and deaths vectors (same length as obs_cases_flat)
        x_cases  <- if (length(x_vec) == length(obs_cases_flat))  x_vec else seq_along(obs_cases_flat)
        x_deaths <- if (length(x_vec) == length(obs_deaths_flat)) x_vec else seq_along(obs_deaths_flat)

        .plot_temporal_panel(x_cases,  residuals_cases,  "Suspected Cases",
                             col_points_cases,  palette$cases_pred,  use_dates)
        .plot_temporal_panel(x_deaths, residuals_deaths, "Deaths",
                             col_points_deaths, palette$deaths_pred, use_dates)

        mtext(if (nchar(location_label) > 0)
                  paste0("PPC (", location_label, "): Temporal Residual Patterns")
              else "PPC: Temporal Residual Patterns",
              outer = TRUE, cex = 1.3, font = 2, line = 1)

        dev.off()

        if (verbose) {
            suffix_text <- if (nchar(suffix) > 0) paste0(" (", location_label, ")") else ""
            message("  PPC plot", suffix_text, " saved: ", paste0("ppc", suffix, ".pdf"))
        }
    }

    # ============================================================================
    # Dispatch
    # ============================================================================

    if (use_csv) {

        # --- Helper to extract vectors for one metric -------------------------
        .get_metric <- function(df, met, col) df[[col]][df$metric == met]
        .get_ci     <- function(df, met, lo_col, hi_col) {
            list(vals = df[[lo_col]][df$metric == met],
                 hivals = df[[hi_col]][df$metric == met])
        }

        # --- Aggregate plot (all locations combined) --------------------------
        if (verbose) message("\n=== Creating Aggregate PPC Plots ===")

        obs_cases_flat   <- .get_metric(all_data, "Suspected Cases", "observed")
        pred_cases_flat  <- .get_metric(all_data, "Suspected Cases", pred_col)
        obs_deaths_flat  <- .get_metric(all_data, "Deaths",          "observed")
        pred_deaths_flat <- .get_metric(all_data, "Deaths",          pred_col)

        ci_cases  <- if (has_ci) list(
            lo50 = .get_metric(all_data, "Suspected Cases", "ci_2_lower"),
            hi50 = .get_metric(all_data, "Suspected Cases", "ci_2_upper"),
            lo95 = .get_metric(all_data, "Suspected Cases", "ci_1_lower"),
            hi95 = .get_metric(all_data, "Suspected Cases", "ci_1_upper")
        ) else NULL

        ci_deaths <- if (has_ci) list(
            lo50 = .get_metric(all_data, "Deaths", "ci_2_lower"),
            hi50 = .get_metric(all_data, "Deaths", "ci_2_upper"),
            lo95 = .get_metric(all_data, "Deaths", "ci_1_lower"),
            hi95 = .get_metric(all_data, "Deaths", "ci_1_upper")
        ) else NULL

        # Aggregate uses sequential index (locations have different date ranges)
        create_ppc_plots(obs_cases_flat, pred_cases_flat,
                         obs_deaths_flat, pred_deaths_flat,
                         dates_vec    = NULL,
                         ci_cases     = ci_cases,
                         ci_deaths    = ci_deaths,
                         location_label = "")

        # --- Per-location plots -----------------------------------------------
        if (n_locations > 1) {
            if (verbose) message("\n=== Creating Per-Location PPC Plots ===")
        }

        for (loc in available_locations) {
            if (n_locations > 1 && verbose) message("  Processing location: ", loc)

            loc_df <- all_data[all_data$location == loc, ]

            obs_c_loc  <- .get_metric(loc_df, "Suspected Cases", "observed")
            pred_c_loc <- .get_metric(loc_df, "Suspected Cases", pred_col)
            obs_d_loc  <- .get_metric(loc_df, "Deaths",          "observed")
            pred_d_loc <- .get_metric(loc_df, "Deaths",          pred_col)

            ci_c_loc <- if (has_ci) list(
                lo50 = .get_metric(loc_df, "Suspected Cases", "ci_2_lower"),
                hi50 = .get_metric(loc_df, "Suspected Cases", "ci_2_upper"),
                lo95 = .get_metric(loc_df, "Suspected Cases", "ci_1_lower"),
                hi95 = .get_metric(loc_df, "Suspected Cases", "ci_1_upper")
            ) else NULL

            ci_d_loc <- if (has_ci) list(
                lo50 = .get_metric(loc_df, "Deaths", "ci_2_lower"),
                hi50 = .get_metric(loc_df, "Deaths", "ci_2_upper"),
                lo95 = .get_metric(loc_df, "Deaths", "ci_1_lower"),
                hi95 = .get_metric(loc_df, "Deaths", "ci_1_upper")
            ) else NULL

            # Use actual dates for per-location temporal plot
            dates_loc <- if (has_dates) {
                .get_metric(loc_df, "Suspected Cases", "date")
            } else NULL

            # Skip aggregate-only when single location (avoids duplicate file)
            label <- if (n_locations > 1) loc else ""

            create_ppc_plots(obs_c_loc, pred_c_loc,
                             obs_d_loc, pred_d_loc,
                             dates_vec      = dates_loc,
                             ci_cases       = ci_c_loc,
                             ci_deaths      = ci_d_loc,
                             location_label = label)
        }

    } else {
        # Legacy model mode
        if (verbose) message("\n=== Creating PPC Plots (Legacy Mode) ===")
        create_ppc_plots(obs_cases_flat, pred_cases_flat,
                         obs_deaths_flat, pred_deaths_flat,
                         dates_vec      = NULL,
                         ci_cases       = NULL,
                         ci_deaths      = NULL,
                         location_label = "")
    }

    # ============================================================================
    # Summary
    # ============================================================================

    if (verbose) {
        message("\n=== PPC Plots Complete ===")
        message("Output directory: ", ppc_dir)
        message("Data source: ", data_source)
        if (use_csv) {
            message("Locations processed: ", paste(available_locations, collapse = ", "))
        }
    }

    invisible(NULL)
}
