#' Plot Best Model Replications
#'
#' Creates diagnostic plots for best model replication results, comparing
#' observed data with multiple replications of the **single best-fitting model**
#' to assess model adequacy and show stochastic variability.
#'
#' @param ppc_results List returned by [calc_model_ppc()]. Must contain
#'   `observed`, `predictions`, `summary_stats`, and `best_model`.
#' @param output_dir Character string specifying directory where plots should be saved.
#'   Directory will be created if it doesn't exist.
#' @param location_names Optional character vector of location names for labeling.
#'   If NULL, uses generic location indices.
#' @param location_codes Optional character vector of location codes (e.g., "ETH")
#'   for file naming. If NULL, uses `location_names` or indices (sanitized).
#' @param time_labels Optional character/Date vector for time axis labels.
#'   If NULL, uses sequential time indices.
#' @param credible_intervals Numeric vector of credible interval levels to display
#'   (default c(0.5, 0.8, 0.95)). Values must be in (0,1).
#' @param max_predictions Integer. Max number of individual replication traces
#'   to show in time series (default 20) to reduce clutter.
#' @param plot_width Numeric. Width of saved plots in inches (default 16).
#' @param plot_height Numeric. Height of saved plots in inches (default 14).
#' @param verbose Logical. Whether to print progress messages (default TRUE).
#'
#' @return Invisibly returns a list of ggplot objects for each location:
#'   \itemize{
#'     \item \code{[location]_timeseries}: Time series plots (cases and deaths)
#'     \item \code{[location]_diagnostics}: Diagnostic plots (distributions, scatter, Q-Q, residuals)
#'   }
#'
#' @details
#' **Time series** show observed points, mean prediction, and layered credible-interval ribbons
#' computed **only from replications of the best model**. Individual replicate traces (up to
#' `max_predictions`) visualize stochastic variability.
#'
#' **Diagnostics** compare empirical distributions and residual patterns for the best model.
#'
#' @examples
#' \dontrun{
#' best_model_plots <- plot_model_ppc(
#'   ppc_results = ppc_results,
#'   output_dir = "best_model_validation",
#'   location_names = c("Region A", "Region B"),
#'   credible_intervals = c(0.5, 0.8, 0.95)
#' )
#' print(best_model_plots$ETH_timeseries)
#' print(best_model_plots$ETH_diagnostics)
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_ribbon geom_density geom_smooth
#' @importFrom ggplot2 geom_abline geom_qq geom_hline stat_qq scale_fill_manual theme_minimal
#' @importFrom ggplot2 theme element_text element_blank labs ggsave xlim ylim annotate theme_void
#' @importFrom dplyr mutate filter bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom patchwork plot_layout
#' @seealso [calc_model_ppc()], [plot_model_fit()]
#' @family model-validation
plot_model_ppc <- function(ppc_results,
                           output_dir,
                           location_names = NULL,
                           location_codes = NULL,
                           time_labels = NULL,
                           credible_intervals = c(0.5, 0.8, 0.95),
                           max_predictions = 20,
                           plot_width = 16,
                           plot_height = 14,
                           verbose = TRUE) {

     # ============================================================================
     # Input validation
     # ============================================================================
     if (!is.list(ppc_results)) stop("ppc_results must be a list from calc_model_ppc()")
     required_elements <- c("observed", "predictions", "summary_stats", "best_model")
     missing_elements <- setdiff(required_elements, names(ppc_results))
     if (length(missing_elements) > 0) {
          stop("ppc_results missing required elements: ", paste(missing_elements, collapse = ", "))
     }
     if (missing(output_dir) || is.null(output_dir)) stop("output_dir is required")

     if (!is.numeric(credible_intervals) ||
         any(credible_intervals <= 0) || any(credible_intervals >= 1)) {
          stop("credible_intervals must be numeric and strictly between 0 and 1")
     }
     credible_intervals <- sort(unique(credible_intervals))

     # Create output directory
     if (!dir.exists(output_dir)) {
          dir.create(output_dir, recursive = TRUE)
          if (verbose) message("Created output directory: ", output_dir)
     }

     if (verbose) cat("Preparing data for best model replication plots...\n")

     observed_cases <- as.matrix(ppc_results$observed$cases)
     observed_deaths <- as.matrix(ppc_results$observed$deaths)
     predictions <- ppc_results$predictions
     best_model <- ppc_results$best_model

     # Basic dims
     n_locations <- nrow(observed_cases)
     n_time <- ncol(observed_cases)
     n_predictions <- length(predictions)

     # Labels
     if (is.null(location_names)) location_names <- paste("Location", seq_len(n_locations))
     sanitize_code <- function(x) gsub("[^A-Za-z0-9]+", "_", x)
     if (is.null(location_codes)) {
          location_codes <- sanitize_code(location_names)
     } else {
          location_codes <- sanitize_code(location_codes)
     }
     if (length(location_codes) != n_locations) {
          stop("length(location_codes) must equal number of locations (rows of observed)")
     }
     if (is.null(time_labels)) time_labels <- seq_len(n_time)
     if (length(time_labels) != n_time) {
          stop("length(time_labels) must equal number of time points (cols of observed)")
     }

     # ============================================================================
     # Guardrail: ensure predictions belong to the best model (filter if needed)
     # ============================================================================
     if (!is.null(best_model$parameters) && ncol(best_model$parameters) > 0) {
          target_params <- as.numeric(best_model$parameters[1, ])
          names(target_params) <- colnames(best_model$parameters)
          is_from_best <- vapply(
               predictions,
               FUN = function(p) {
                    if (is.null(p$parameters)) return(NA)
                    pv <- as.numeric(p$parameters)
                    if (length(pv) != length(target_params)) return(FALSE)
                    all(abs(pv - target_params) <= 1e-12)
               },
               logical(1)
          )
          if (all(is.na(is_from_best))) {
               if (verbose) warning("Predictions lack parameter metadata; assuming all are from the best model.")
          } else {
               if (!all(is_from_best, na.rm = TRUE)) {
                    if (verbose) warning("Detected predictions not from the best model; filtering to best-model replications only.")
                    predictions <- predictions[which(is_from_best %in% TRUE)]
                    n_predictions <- length(predictions)
                    if (n_predictions == 0) stop("No predictions remain after filtering to the best model.")
               }
          }
     }

     # ============================================================================
     # Build arrays from predictions (best model only)
     # ============================================================================
     n_valid <- length(predictions)
     if (n_valid == 0) stop("No predictions available.")
     cases_dims  <- dim(predictions[[1]]$cases)
     deaths_dims <- dim(predictions[[1]]$deaths)
     if (cases_dims[1] != n_locations || cases_dims[2] != n_time) {
          stop("Prediction dimensions (cases) do not match observed (locations x time).")
     }
     if (deaths_dims[1] != n_locations || deaths_dims[2] != n_time) {
          stop("Prediction dimensions (deaths) do not match observed (locations x time).")
     }
     pred_cases_array  <- array(NA_real_, dim = c(n_valid, n_locations, n_time))
     pred_deaths_array <- array(NA_real_, dim = c(n_valid, n_locations, n_time))
     for (i in seq_len(n_valid)) {
          pred_cases_array[i, , ]  <- predictions[[i]]$cases
          pred_deaths_array[i, , ] <- predictions[[i]]$deaths
     }

     # Helper to compute mean and arbitrary CIs for a [rep, loc, time] array
     ci_levels <- credible_intervals
     q_lower <- (1 - ci_levels) / 2
     q_upper <- 1 - q_lower
     make_summary <- function(arr3) {
          mean_mat <- apply(arr3, c(2, 3), mean, na.rm = TRUE)
          out <- list(mean = mean_mat)
          for (k in seq_along(ci_levels)) {
               lo <- apply(arr3, c(2, 3), quantile, probs = q_lower[k], na.rm = TRUE)
               hi <- apply(arr3, c(2, 3), quantile, probs = q_upper[k], na.rm = TRUE)
               out[[paste0("lo_", gsub("\\.", "", sprintf("%.2f", ci_levels[k])))] ] <- lo
               out[[paste0("hi_", gsub("\\.", "", sprintf("%.2f", ci_levels[k])))] ] <- hi
          }
          out
     }
     sum_cases  <- make_summary(pred_cases_array)
     sum_deaths <- make_summary(pred_deaths_array)

     # Indices for up to max_predictions traces (deterministic, evenly spaced)
     if (n_valid <= max_predictions) {
          trace_idx <- seq_len(n_valid)
     } else {
          trace_idx <- unique(round(seq(1, n_valid, length.out = max_predictions)))
     }

     if (verbose) {
          cat("Best model replication data:\n")
          cat("- Best model: sim ", best_model$sim_id, " (LL = ", sprintf("%.3f", best_model$likelihood), ")\n", sep = "")
          cat("- Locations: ", n_locations, "\n", sep = "")
          cat("- Time points: ", n_time, "\n", sep = "")
          cat("- Replications: ", n_valid, " (", length(trace_idx), " trace lines shown)\n", sep = "")
     }

     # ============================================================================
     # Plotting per location
     # ============================================================================
     if (verbose) cat("Creating best model replication plots for each location...\n")
     location_plots <- list()
     patchwork_available <- requireNamespace("patchwork", quietly = TRUE)

     # Layer helper for ribbons across multiple CIs (inner-most first)
     add_ci_ribbons <- function(p, df_base, ci_levels, what = c("cases","deaths")) {
          what <- match.arg(what)
          for (k in seq_along(ci_levels)) {
               tag <- gsub("\\.", "", sprintf("%.2f", ci_levels[k]))
               lo_col <- paste0("lo_", tag)
               hi_col <- paste0("hi_", tag)
               p <- p + ggplot2::geom_ribbon(
                    data = df_base,
                    ggplot2::aes(ymin = .data[[lo_col]], ymax = .data[[hi_col]]),
                    alpha = 0.10 + 0.05 * (k - 1),
                    fill  = if (what == "cases") "steelblue" else "darkred"
               )
          }
          p
     }

     for (loc in seq_len(n_locations)) {
          if (verbose) cat("Processing location ", loc, ": ", location_names[loc], "\n", sep = "")

          # --- Build long data for this location ---
          # Means + CI bands
          mk_df <- function(sum_list) {
               df <- data.frame(
                    time         = time_labels,
                    time_numeric = seq_len(n_time),
                    mean         = as.numeric(sum_list$mean[loc, ]),
                    stringsAsFactors = FALSE
               )
               for (k in seq_along(ci_levels)) {
                    tag <- gsub("\\.", "", sprintf("%.2f", ci_levels[k]))
                    df[[paste0("lo_", tag)]] <- as.numeric(sum_list[[paste0("lo_", tag)]][loc, ])
                    df[[paste0("hi_", tag)]] <- as.numeric(sum_list[[paste0("hi_", tag)]][loc, ])
               }
               df
          }
          cases_df  <- mk_df(sum_cases)
          deaths_df <- mk_df(sum_deaths)

          # Observed
          obs_cases_loc  <- as.numeric(observed_cases[loc, ])
          obs_deaths_loc <- as.numeric(observed_deaths[loc, ])

          # Trace data
          mk_traces <- function(arr3) {
               # arr3: [rep, loc, time]
               lst <- vector("list", length(trace_idx))
               for (j in seq_along(trace_idx)) {
                    r <- trace_idx[j]
                    lst[[j]] <- data.frame(
                         time         = time_labels,
                         time_numeric = seq_len(n_time),
                         predicted    = as.numeric(arr3[r, loc, ]),
                         trace_id     = j,
                         stringsAsFactors = FALSE
                    )
               }
               do.call(rbind, lst)
          }
          cases_traces  <- mk_traces(pred_cases_array)
          deaths_traces <- mk_traces(pred_deaths_array)

          # --- Cases time series ---
          p_cases_ts <- ggplot2::ggplot(cases_df, ggplot2::aes(x = time_numeric))
          p_cases_ts <- p_cases_ts + add_ci_ribbons(p_cases_ts, cases_df, ci_levels, "cases")
          if (nrow(cases_traces) > 0) {
               p_cases_ts <- p_cases_ts +
                    ggplot2::geom_line(
                         data = cases_traces,
                         ggplot2::aes(y = predicted, group = trace_id),
                         alpha = 0.12, color = "steelblue", linewidth = 0.3
                    )
          }
          p_cases_ts <- p_cases_ts +
               ggplot2::geom_line(ggplot2::aes(y = mean), color = "steelblue", linewidth = 1, linetype = "dashed") +
               ggplot2::geom_point(ggplot2::aes(y = obs_cases_loc), color = "#2a2a2a", size = 1.8, alpha = 0.85) +
               ggplot2::theme_minimal(base_size = 13) +
               ggplot2::theme(
                    panel.grid.minor = ggplot2::element_blank(),
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                    plot.title = ggplot2::element_text(size = 16, face = "bold")
               ) +
               ggplot2::labs(
                    x = "",
                    y = "Cases",
                    title = paste0("Best Model Fit: ", location_names[loc]),
                    subtitle = paste0(
                         "Sim ", best_model$sim_id,
                         " (LL = ", sprintf("%.2f", best_model$likelihood), "); ",
                         length(trace_idx), " replicate traces; CI levels: ",
                         paste0(format(credible_intervals, trim = TRUE), collapse = ", ")
                    )
               )

          # --- Deaths time series ---
          p_deaths_ts <- ggplot2::ggplot(deaths_df, ggplot2::aes(x = time_numeric))
          p_deaths_ts <- p_deaths_ts + add_ci_ribbons(p_deaths_ts, deaths_df, ci_levels, "deaths")
          if (nrow(deaths_traces) > 0) {
               p_deaths_ts <- p_deaths_ts +
                    ggplot2::geom_line(
                         data = deaths_traces,
                         ggplot2::aes(y = predicted, group = trace_id),
                         alpha = 0.12, color = "darkred", linewidth = 0.3
                    )
          }
          p_deaths_ts <- p_deaths_ts +
               ggplot2::geom_line(ggplot2::aes(y = mean), color = "darkred", linewidth = 1, linetype = "dashed") +
               ggplot2::geom_point(ggplot2::aes(y = obs_deaths_loc), color = "#2a2a2a", size = 1.8, alpha = 0.85) +
               ggplot2::theme_minimal(base_size = 13) +
               ggplot2::theme(
                    panel.grid.minor = ggplot2::element_blank(),
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
               ) +
               ggplot2::labs(x = "Time", y = "Deaths", title = "", subtitle = "")

          # --- Diagnostics (distributions, scatter, QQ, residuals) ---
          # Flatten and filter positive counts for log-scale visuals
          obs_cases_flat  <- obs_cases_loc[is.finite(obs_cases_loc) & obs_cases_loc > 0]
          obs_deaths_flat <- obs_deaths_loc[is.finite(obs_deaths_loc) & obs_deaths_loc > 0]
          pred_cases_flat  <- as.numeric(pred_cases_array[, loc, , drop = FALSE])
          pred_deaths_flat <- as.numeric(pred_deaths_array[, loc, , drop = FALSE])
          pred_cases_flat  <- pred_cases_flat[is.finite(pred_cases_flat) & pred_cases_flat > 0]
          pred_deaths_flat <- pred_deaths_flat[is.finite(pred_deaths_flat) & pred_deaths_flat > 0]

          # Distributions
          mk_density <- function(obs_v, pred_v, what = c("cases","deaths")) {
               what <- match.arg(what)
               if (length(obs_v) > 1 && length(pred_v) > 1) {
                    df <- rbind(
                         data.frame(type = "Observed", value = obs_v),
                         data.frame(type = "Predicted", value = pred_v)
                    )
                    ggplot2::ggplot(df, ggplot2::aes(x = log(value + 1), fill = type)) +
                         ggplot2::geom_density(alpha = 0.6, position = "identity") +
                         ggplot2::scale_fill_manual(values = c("Observed" = "#2a2a2a",
                                                               "Predicted" = if (what == "cases") "steelblue" else "darkred")) +
                         ggplot2::theme_minimal(base_size = 11) +
                         ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank(), aspect.ratio = 1) +
                         ggplot2::labs(x = paste0("Log(", tools::toTitleCase(what), " + 1)"),
                                       y = "Density",
                                       title = paste(tools::toTitleCase(what), "Distribution"))
               } else {
                    ggplot2::ggplot() +
                         ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Insufficient\ndata", size = 3) +
                         ggplot2::theme_void() + ggplot2::labs(title = paste(tools::toTitleCase(what), "Distribution")) +
                         ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
               }
          }
          p_dist_cases  <- mk_density(obs_cases_flat,  pred_cases_flat,  "cases")
          p_dist_deaths <- mk_density(obs_deaths_flat, pred_deaths_flat, "deaths")

          # Scatter + residuals vs mean prediction
          pred_cases_mean  <- as.numeric(sum_cases$mean[loc, ])
          pred_deaths_mean <- as.numeric(sum_deaths$mean[loc, ])

          mk_scatter <- function(obs, pred, col) {
               df <- data.frame(observed = obs, predicted = pred)
               df <- df[is.finite(df$observed) & is.finite(df$predicted) & df$observed > 0 & df$predicted > 0, , drop = FALSE]
               if (nrow(df) >= 3) {
                    ggplot2::ggplot(df, ggplot2::aes(x = log(observed), y = log(predicted))) +
                         ggplot2::geom_point(alpha = 0.7, size = 1.5, color = col) +
                         ggplot2::geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed", linewidth = 0.5) +
                         ggplot2::geom_abline(intercept = 0, slope = 1, color = "black", linetype = "solid", linewidth = 0.5) +
                         ggplot2::theme_minimal(base_size = 11) +
                         ggplot2::theme(aspect.ratio = 1) +
                         ggplot2::labs(x = "Log(Observed)", y = "Log(Predicted)", title = "Obs vs Pred")
               } else {
                    ggplot2::ggplot() +
                         ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Insufficient\nvalid pairs", size = 3) +
                         ggplot2::theme_void() + ggplot2::labs(title = "Obs vs Pred") +
                         ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
               }
          }
          p_scatter_cases  <- mk_scatter(obs_cases_loc,  pred_cases_mean,  "steelblue")
          p_scatter_deaths <- mk_scatter(obs_deaths_loc, pred_deaths_mean, "darkred")

          # QQ
          mk_qq <- function(obs_v, pred_v, col, ttl) {
               obs_v <- obs_v[is.finite(obs_v)]
               pred_v <- pred_v[is.finite(pred_v)]
               if (length(obs_v) > 3 && length(pred_v) > 3) {
                    n_sample <- min(length(obs_v), length(pred_v), 200)
                    obs_s  <- sort(sample(obs_v,  n_sample))
                    pred_s <- sort(sample(pred_v, n_sample))
                    df <- data.frame(theoretical = pred_s, sample = obs_s)
                    ggplot2::ggplot(df, ggplot2::aes(x = theoretical, y = sample)) +
                         ggplot2::geom_point(alpha = 0.7, size = 1, color = col) +
                         ggplot2::geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
                         ggplot2::theme_minimal(base_size = 9) +
                         ggplot2::theme(aspect.ratio = 1) +
                         ggplot2::labs(x = "Predicted Quantiles", y = "Observed Quantiles", title = ttl)
               } else {
                    ggplot2::ggplot() +
                         ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Insufficient\ndata for\nQ-Q plot", size = 3) +
                         ggplot2::theme_void() + ggplot2::labs(title = ttl) +
                         ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
               }
          }
          p_qq_cases  <- mk_qq(obs_cases_flat,  pred_cases_flat,  "steelblue", "Cases: Q-Q Plot")
          p_qq_deaths <- mk_qq(obs_deaths_flat, pred_deaths_flat, "darkred",  "Deaths: Q-Q Plot")

          # Residuals
          mk_resid <- function(pred, obs, col, ttl) {
               df <- data.frame(predicted = pred, residual = obs - pred)
               df <- df[is.finite(df$predicted) & is.finite(df$residual), , drop = FALSE]
               if (nrow(df) >= 3) {
                    ggplot2::ggplot(df, ggplot2::aes(x = predicted, y = residual)) +
                         ggplot2::geom_point(alpha = 0.7, size = 1, color = col) +
                         ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
                         ggplot2::geom_smooth(method = "loess", se = FALSE, color = col, linewidth = 0.5) +
                         ggplot2::theme_minimal(base_size = 9) +
                         ggplot2::theme(aspect.ratio = 1) +
                         ggplot2::labs(x = "Predicted", y = "Residual", title = ttl)
               } else {
                    ggplot2::ggplot() +
                         ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Insufficient\ndata for\nresiduals", size = 3) +
                         ggplot2::theme_void() + ggplot2::labs(title = ttl) +
                         ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
               }
          }
          p_resid_cases  <- mk_resid(pred_cases_mean,  obs_cases_loc,  "steelblue", "Cases: Residuals")
          p_resid_deaths <- mk_resid(pred_deaths_mean, obs_deaths_loc, "darkred",  "Deaths: Residuals")

          # Combine and save
          if (patchwork_available) {
               timeseries_plot <- p_cases_ts / p_deaths_ts + patchwork::plot_layout(heights = c(1, 1))
               diagnostics_plot <- patchwork::wrap_plots(
                    p_dist_cases, p_scatter_cases, p_qq_cases, p_resid_cases,
                    p_dist_deaths, p_scatter_deaths, p_qq_deaths, p_resid_deaths,
                    nrow = 2, ncol = 4
               )
          } else if (requireNamespace("gridExtra", quietly = TRUE)) {
               timeseries_plot <- gridExtra::grid.arrange(p_cases_ts, p_deaths_ts, nrow = 2)
               diagnostics_plot <- gridExtra::grid.arrange(
                    p_dist_cases, p_scatter_cases, p_qq_cases, p_resid_cases,
                    p_dist_deaths, p_scatter_deaths, p_qq_deaths, p_resid_deaths,
                    nrow = 2, ncol = 4
               )
          } else {
               timeseries_plot <- p_cases_ts
               diagnostics_plot <- p_dist_cases
               if (verbose) warning("Neither patchwork nor gridExtra available. Using simplified plots.")
          }

          location_plots[[paste0(location_codes[loc], "_timeseries")]]  <- timeseries_plot
          location_plots[[paste0(location_codes[loc], "_diagnostics")]] <- diagnostics_plot

          timeseries_file  <- file.path(output_dir, paste0("ppc_timeseries_",  location_codes[loc], ".pdf"))
          diagnostics_file <- file.path(output_dir, paste0("ppc_diagnostics_", location_codes[loc], ".pdf"))

          ggplot2::ggsave(timeseries_file,  plot = timeseries_plot,  width = plot_width * 0.75, height = plot_height * 0.5, dpi = 300)
          ggplot2::ggsave(diagnostics_file, plot = diagnostics_plot, width = plot_width,        height = plot_height * 0.6, dpi = 300)

          if (verbose) {
               cat("Saved timeseries:  ", basename(timeseries_file),  "\n", sep = "")
               cat("Saved diagnostics: ", basename(diagnostics_file), "\n", sep = "")
          }
     }

     if (verbose) {
          cat("Posterior predictive check plots completed!\n")
          cat("Created ", length(location_plots), " plots for ", n_locations, " locations.\n", sep = "")
          cat("Files saved in: ", output_dir, "\n", sep = "")
     }

     invisible(location_plots)
}
