#' Plot Stochastic Ensemble Predictions
#'
#' @description
#' Renders timeseries plots from a \code{mosaic_ensemble} object produced by
#' \code{\link{calc_model_ensemble}}. Optionally saves per-location prediction
#' CSVs for downstream use.
#'
#' This is the plotting half of what was previously bundled inside
#' \code{\link{plot_model_fit_stochastic}}. Accepting a pre-computed ensemble
#' object means simulations are never run twice when both metrics and plots are
#' needed.
#'
#' @param ensemble A \code{mosaic_ensemble} object returned by
#'   \code{\link{calc_model_ensemble}}.
#' @param output_dir Character. Directory where plots and CSVs are saved.
#'   Created if it does not exist.
#' @param save_predictions Logical. Save per-location prediction CSVs
#'   (\code{predictions_stochastic_<location>.csv}). Default \code{FALSE}.
#' @param verbose Logical. Print progress messages. Default \code{TRUE}.
#'
#' @return Invisibly returns a list with:
#' \describe{
#'   \item{individual}{Named list of ggplot objects, one per location.}
#'   \item{cases_faceted}{Faceted cases plot (multi-location only).}
#'   \item{deaths_faceted}{Faceted deaths plot (multi-location only).}
#'   \item{simulation_stats}{Simulation metadata from the ensemble object.}
#' }
#'
#' @seealso \code{\link{calc_model_ensemble}} to compute the ensemble,
#'   \code{\link{plot_model_fit_stochastic}} for the combined convenience wrapper.
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_point geom_line facet_grid
#'   facet_wrap scale_color_manual scale_fill_manual scale_y_continuous
#'   scale_x_date theme_minimal theme element_text element_blank labs ggsave
#' @importFrom dplyr filter mutate
#' @importFrom scales comma
plot_model_ensemble <- function(ensemble,
                                output_dir,
                                save_predictions = FALSE,
                                verbose          = TRUE) {

  # ---------------------------------------------------------------------------
  # Validate inputs
  # ---------------------------------------------------------------------------

  if (!inherits(ensemble, "mosaic_ensemble"))
    stop("ensemble must be a mosaic_ensemble object from calc_model_ensemble()")

  if (missing(output_dir) || is.null(output_dir))
    stop("output_dir is required")

  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Unpack ensemble fields
  cases_stats        <- ensemble$cases_stats
  deaths_stats       <- ensemble$deaths_stats
  obs_cases          <- ensemble$obs_cases
  obs_deaths         <- ensemble$obs_deaths
  location_names     <- ensemble$location_names
  n_locations        <- ensemble$n_locations
  n_time_points      <- ensemble$n_time_points
  n_successful       <- ensemble$n_successful
  n_simulations      <- ensemble$n_simulations
  envelope_quantiles <- ensemble$envelope_quantiles
  date_start         <- ensemble$date_start
  date_stop          <- ensemble$date_stop

  # ---------------------------------------------------------------------------
  # Handle dates
  # ---------------------------------------------------------------------------

  if (!is.null(date_start) && !is.null(date_stop)) {
    dates <- seq(as.Date(date_start), as.Date(date_stop), length.out = n_time_points)
  } else if (!is.null(date_start)) {
    dates <- seq(as.Date(date_start), length.out = n_time_points, by = "week")
  } else {
    dates <- seq_len(n_time_points)
    if (verbose) message("Warning: no date info in ensemble. Using numeric time points.")
  }

  use_date_axis <- inherits(dates, "Date")

  # ---------------------------------------------------------------------------
  # Helper: extract data for a single location (handles matrix or vector)
  # ---------------------------------------------------------------------------

  .extract_loc <- function(data, i) if (is.matrix(data)) data[i, ] else data

  # ---------------------------------------------------------------------------
  # Build tidy plot_data frame
  # ---------------------------------------------------------------------------

  if (verbose) message("Building plot data...")

  plot_data <- do.call(rbind, lapply(seq_len(n_locations), function(i) {
    data.frame(
      location        = location_names[i],
      date            = rep(dates, 2L),
      metric          = c(rep("Suspected Cases", n_time_points),
                          rep("Deaths",          n_time_points)),
      observed        = c(.extract_loc(obs_cases,  i),
                          .extract_loc(obs_deaths, i)),
      predicted_mean  = c(cases_stats$mean[i, ],   deaths_stats$mean[i, ]),
      predicted_lower = c(cases_stats$lower[i, ],  deaths_stats$lower[i, ]),
      predicted_upper = c(cases_stats$upper[i, ],  deaths_stats$upper[i, ]),
      stringsAsFactors = FALSE
    )
  }))

  plot_data$metric <- factor(plot_data$metric,
                              levels = c("Suspected Cases", "Deaths"))

  # ---------------------------------------------------------------------------
  # Save per-location prediction CSVs
  # ---------------------------------------------------------------------------

  if (save_predictions) {
    if (verbose) message("Saving prediction CSVs...")
    for (i in seq_len(n_locations)) {
      loc     <- location_names[i]
      loc_df  <- plot_data[plot_data$location == loc, ]
      csv_out <- file.path(output_dir, paste0("predictions_stochastic_", loc, ".csv"))
      utils::write.csv(loc_df, csv_out, row.names = FALSE)
      if (verbose) message("  Saved: ", csv_out)
    }
  }

  # ---------------------------------------------------------------------------
  # Plotting helpers
  # ---------------------------------------------------------------------------

  envelope_pct <- round((envelope_quantiles[2L] - envelope_quantiles[1L]) * 100)

  .add_date_scale <- function(p) {
    if (use_date_axis)
      p + ggplot2::scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")
    else p
  }

  plot_list <- list(individual = list())

  # ---------------------------------------------------------------------------
  # 1. Individual location plots
  # ---------------------------------------------------------------------------

  if (verbose) message("Generating individual location plots...")

  for (i in seq_len(n_locations)) {

    loc <- location_names[i]
    loc_data <- plot_data[plot_data$location == loc, ]

    obs_c   <- .extract_loc(obs_cases,  i)
    obs_d   <- .extract_loc(obs_deaths, i)
    pred_c  <- cases_stats$mean[i, ]
    pred_d  <- deaths_stats$mean[i, ]

    r2_c    <- tryCatch(round(stats::cor(obs_c, pred_c, use = "complete.obs")^2, 3L),
                        error = function(e) NA)
    r2_d    <- tryCatch(round(stats::cor(obs_d, pred_d, use = "complete.obs")^2, 3L),
                        error = function(e) NA)
    bias_c  <- tryCatch(round(calc_bias_ratio(obs_c, pred_c), 2L), error = function(e) NA)
    bias_d  <- tryCatch(round(calc_bias_ratio(obs_d, pred_d), 2L), error = function(e) NA)

    p <- ggplot2::ggplot(loc_data, ggplot2::aes(x = date)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = predicted_lower,
                                        ymax = predicted_upper,
                                        fill = metric),
                           alpha = 0.3) +
      ggplot2::geom_point(ggplot2::aes(y = observed),
                          color = mosaic_colors("data"), size = 1.5, alpha = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = predicted_mean, color = metric),
                         linewidth = 0.8) +
      ggplot2::facet_grid(metric ~ ., scales = "free_y", switch = "y") +
      ggplot2::scale_color_manual(
        values = c("Suspected Cases" = unname(mosaic_colors("cases")),
                   "Deaths"          = unname(mosaic_colors("deaths"))),
        guide = "none"
      ) +
      ggplot2::scale_fill_manual(
        values = c("Suspected Cases" = unname(mosaic_colors("cases")),
                   "Deaths"          = unname(mosaic_colors("deaths"))),
        guide = "none"
      ) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      theme_mosaic(base_size = 10) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                     strip.placement = "outside") +
      ggplot2::labs(
        x = if (use_date_axis) "Date" else "Time",
        y = NULL,
        title    = paste0("Stochastic Model Fit: ", loc),
        subtitle = paste0("Mean prediction (line) with ", envelope_pct,
                          "% range (ribbon) | ", n_successful, " simulations"),
        caption  = paste0(
          "Cases: Obs = ", format(round(sum(obs_c, na.rm = TRUE)), big.mark = ","),
          ", Pred = ",     format(round(sum(pred_c, na.rm = TRUE)), big.mark = ","),
          ", R\u00b2 = ", ifelse(is.na(r2_c), "NA", r2_c),
          ", Bias = ",    ifelse(is.na(bias_c), "NA", bias_c),
          " | Deaths: Obs = ", format(round(sum(obs_d, na.rm = TRUE)), big.mark = ","),
          ", Pred = ",         format(round(sum(pred_d, na.rm = TRUE)), big.mark = ","),
          ", R\u00b2 = ", ifelse(is.na(r2_d), "NA", r2_d),
          ", Bias = ",    ifelse(is.na(bias_d), "NA", bias_d),
          "\nGenerated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      )

    p <- .add_date_scale(p)

    plot_list$individual[[loc]] <- p
    if (verbose) print(p)

    out_file <- file.path(output_dir, paste0("model_stochastic_", loc, ".pdf"))
    ggplot2::ggsave(out_file, plot = p, width = 10, height = 6, dpi = 300)
    if (verbose) message("  Saved: ", out_file)
  }

  # ---------------------------------------------------------------------------
  # 2. Faceted cases plot (multi-location only)
  # ---------------------------------------------------------------------------

  if (n_locations > 1L) {

    if (verbose) message("Generating faceted cases plot...")

    cases_data    <- plot_data[plot_data$metric == "Suspected Cases", ]
    all_obs_c     <- as.numeric(obs_cases)
    all_pred_c    <- as.numeric(cases_stats$mean)
    r2_c_all      <- tryCatch(round(stats::cor(all_obs_c, all_pred_c, use = "complete.obs")^2, 3L),
                              error = function(e) NA)
    bias_c_all    <- tryCatch(round(calc_bias_ratio(all_obs_c, all_pred_c), 2L),
                              error = function(e) NA)

    p_cases <- ggplot2::ggplot(cases_data, ggplot2::aes(x = date)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = predicted_lower, ymax = predicted_upper),
                           fill  = mosaic_color_variant(unname(mosaic_colors("cases")), "lighten", 0.3),
                           alpha = 0.3) +
      ggplot2::geom_point(ggplot2::aes(y = observed),
                          color = mosaic_colors("data"), size = 1.5, alpha = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = predicted_mean),
                         color = mosaic_colors("cases"), linewidth = 0.8) +
      ggplot2::facet_wrap(~ location, scales = "free_y",
                          ncol = min(3L, n_locations)) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      theme_mosaic(base_size = 10) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8)) +
      ggplot2::labs(
        x = if (use_date_axis) "Date" else "Time", y = "Suspected Cases",
        title    = "Stochastic Model Fit: Suspected Cases by Location",
        subtitle = paste0("Mean prediction with ", envelope_pct, "% range | ",
                          n_successful, " simulations"),
        caption  = paste0(
          "Total: Obs = ",    format(round(sum(obs_cases, na.rm = TRUE)), big.mark = ","),
          ", Pred = ",         format(round(sum(cases_stats$mean, na.rm = TRUE)), big.mark = ","),
          ", R\u00b2 = ", ifelse(is.na(r2_c_all), "NA", r2_c_all),
          ", Bias = ",    ifelse(is.na(bias_c_all), "NA", bias_c_all),
          "\nGenerated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      )

    p_cases <- .add_date_scale(p_cases)
    plot_list$cases_faceted <- p_cases
    if (verbose) print(p_cases)

    plot_w <- if (n_locations <= 3L) 12 else if (n_locations <= 6L) 14 else 16
    plot_h <- if (n_locations <= 3L) 5  else if (n_locations <= 6L) 8  else
              max(10, ceiling(n_locations / 3L) * 3L)

    out_file <- file.path(output_dir, "model_stochastic_cases_all.pdf")
    ggplot2::ggsave(out_file, plot = p_cases,
                    width = plot_w, height = plot_h, dpi = 300, limitsize = FALSE)
    if (verbose) message("  Saved: ", out_file)

    # ----- Faceted deaths plot -----------------------------------------------

    if (verbose) message("Generating faceted deaths plot...")

    deaths_data   <- plot_data[plot_data$metric == "Deaths", ]
    all_obs_d     <- as.numeric(obs_deaths)
    all_pred_d    <- as.numeric(deaths_stats$mean)
    r2_d_all      <- tryCatch(round(stats::cor(all_obs_d, all_pred_d, use = "complete.obs")^2, 3L),
                              error = function(e) NA)
    bias_d_all    <- tryCatch(round(calc_bias_ratio(all_obs_d, all_pred_d), 2L),
                              error = function(e) NA)

    p_deaths <- ggplot2::ggplot(deaths_data, ggplot2::aes(x = date)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = predicted_lower, ymax = predicted_upper),
                           fill  = mosaic_color_variant(unname(mosaic_colors("deaths")), "lighten", 0.3),
                           alpha = 0.3) +
      ggplot2::geom_point(ggplot2::aes(y = observed),
                          color = mosaic_colors("data"), size = 1.5, alpha = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = predicted_mean),
                         color = mosaic_colors("deaths"), linewidth = 0.8) +
      ggplot2::facet_wrap(~ location, scales = "free_y",
                          ncol = min(3L, n_locations)) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      theme_mosaic(base_size = 10) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8)) +
      ggplot2::labs(
        x = if (use_date_axis) "Date" else "Time", y = "Deaths",
        title    = "Stochastic Model Fit: Deaths by Location",
        subtitle = paste0("Mean prediction with ", envelope_pct, "% range | ",
                          n_successful, " simulations"),
        caption  = paste0(
          "Total: Obs = ",    format(round(sum(obs_deaths, na.rm = TRUE)), big.mark = ","),
          ", Pred = ",         format(round(sum(deaths_stats$mean, na.rm = TRUE)), big.mark = ","),
          ", R\u00b2 = ", ifelse(is.na(r2_d_all), "NA", r2_d_all),
          ", Bias = ",    ifelse(is.na(bias_d_all), "NA", bias_d_all),
          "\nGenerated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      )

    p_deaths <- .add_date_scale(p_deaths)
    plot_list$deaths_faceted <- p_deaths
    if (verbose) print(p_deaths)

    out_file <- file.path(output_dir, "model_stochastic_deaths_all.pdf")
    ggplot2::ggsave(out_file, plot = p_deaths,
                    width = plot_w, height = plot_h, dpi = 300, limitsize = FALSE)
    if (verbose) message("  Saved: ", out_file)
  }

  # ---------------------------------------------------------------------------
  # Attach simulation stats and return
  # ---------------------------------------------------------------------------

  plot_list$simulation_stats <- list(
    n_simulations      = n_simulations,
    n_successful       = n_successful,
    success_rate       = n_successful / n_simulations,
    envelope_quantiles = envelope_quantiles,
    seeds              = ensemble$seeds,
    cases_stats        = cases_stats,
    deaths_stats       = deaths_stats
  )

  if (verbose) message("plot_model_ensemble complete.")
  invisible(plot_list)
}
