#' Plot Ensemble Predictions from a mosaic_ensemble Object
#'
#' @description
#' Renders time-series plots from a \code{mosaic_ensemble} object produced by
#' \code{\link{calc_model_ensemble}}. Shows the weighted median prediction line
#' with confidence interval ribbons and observed data points. Optionally saves
#' per-location prediction CSVs for downstream use.
#'
#' @param ensemble A \code{mosaic_ensemble} object returned by
#'   \code{\link{calc_model_ensemble}}.
#' @param output_dir Character. Directory where plots and CSVs are saved.
#'   Created if it does not exist.
#' @param save_predictions Logical. Save per-location prediction CSVs. Default
#'   \code{FALSE}.
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
#' @seealso \code{\link{calc_model_ensemble}} to compute the ensemble.
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
  cases_median       <- ensemble$cases_median
  deaths_median      <- ensemble$deaths_median
  obs_cases          <- ensemble$obs_cases
  obs_deaths         <- ensemble$obs_deaths
  location_names     <- ensemble$location_names
  n_locations        <- ensemble$n_locations
  n_time_points      <- ensemble$n_time_points
  n_successful       <- ensemble$n_successful
  n_param_sets       <- ensemble$n_param_sets
  n_stoch_per        <- ensemble$n_simulations_per_config
  envelope_quantiles <- ensemble$envelope_quantiles
  date_start         <- ensemble$date_start
  date_stop          <- ensemble$date_stop
  ci_bounds_cases    <- ensemble$ci_bounds$cases
  ci_bounds_deaths   <- ensemble$ci_bounds$deaths

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
  # Helper: extract data for a single location
  # ---------------------------------------------------------------------------

  .extract_loc <- function(data, i) if (is.matrix(data)) data[i, ] else data

  # ---------------------------------------------------------------------------
  # Build tidy plot_data frame
  # ---------------------------------------------------------------------------

  if (verbose) message("Building plot data...")

  n_ci_pairs <- length(envelope_quantiles) / 2L

  plot_data <- do.call(rbind, lapply(seq_len(n_locations), function(i) {
    loc_df <- data.frame(
      location         = location_names[i],
      date             = rep(dates, 2L),
      metric           = c(rep("Suspected Cases", n_time_points),
                           rep("Deaths",          n_time_points)),
      observed         = c(.extract_loc(obs_cases,    i),
                           .extract_loc(obs_deaths,   i)),
      predicted_median = c(.extract_loc(cases_median, i),
                           .extract_loc(deaths_median, i)),
      stringsAsFactors = FALSE
    )

    # Add CI bounds dynamically
    for (ci_idx in seq_len(n_ci_pairs)) {
      lower_col <- paste0("ci_", ci_idx, "_lower")
      upper_col <- paste0("ci_", ci_idx, "_upper")
      loc_df[[lower_col]] <- c(ci_bounds_cases[[ci_idx]]$lower[i, ],
                                ci_bounds_deaths[[ci_idx]]$lower[i, ])
      loc_df[[upper_col]] <- c(ci_bounds_cases[[ci_idx]]$upper[i, ],
                                ci_bounds_deaths[[ci_idx]]$upper[i, ])
    }
    loc_df
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
      csv_out <- file.path(output_dir, paste0("predictions_ensemble_", loc, ".csv"))
      utils::write.csv(loc_df, csv_out, row.names = FALSE)
      if (verbose) message("  Saved: ", csv_out)
    }
  }

  # ---------------------------------------------------------------------------
  # Plotting helpers
  # ---------------------------------------------------------------------------

  .add_date_scale <- function(p) {
    if (use_date_axis)
      p + ggplot2::scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")
    else p
  }

  ribbon_alphas <- seq(0.2, 0.5, length.out = n_ci_pairs)
  total_sims    <- n_param_sets * n_stoch_per

  plot_list <- list(individual = list())

  # ---------------------------------------------------------------------------
  # 1. Individual location plots
  # ---------------------------------------------------------------------------

  if (verbose) message("Generating individual location plots...")

  for (i in seq_len(n_locations)) {

    loc      <- location_names[i]
    loc_data <- plot_data[plot_data$location == loc, ]

    obs_c  <- .extract_loc(obs_cases,    i)
    obs_d  <- .extract_loc(obs_deaths,   i)
    pred_c <- .extract_loc(cases_median, i)
    pred_d <- .extract_loc(deaths_median, i)

    r2_c   <- tryCatch(round(calc_model_R2(obs_c, pred_c), 3L), error = function(e) NA)
    r2_d   <- tryCatch(round(calc_model_R2(obs_d, pred_d), 3L), error = function(e) NA)
    bias_c <- tryCatch(round(calc_bias_ratio(obs_c, pred_c), 2L), error = function(e) NA)
    bias_d <- tryCatch(round(calc_bias_ratio(obs_d, pred_d), 2L), error = function(e) NA)

    loc_data_points <- loc_data[!is.na(loc_data$observed), ]

    p <- ggplot2::ggplot(loc_data, ggplot2::aes(x = date))

    # Add CI ribbons from widest to narrowest
    for (ci_idx in seq_len(n_ci_pairs)) {
      lower_col <- paste0("ci_", ci_idx, "_lower")
      upper_col <- paste0("ci_", ci_idx, "_upper")
      p <- p +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[[lower_col]],
                                           ymax = .data[[upper_col]],
                                           fill = metric),
                             alpha = ribbon_alphas[ci_idx])
    }

    p <- p +
      ggplot2::geom_point(data = loc_data_points, ggplot2::aes(y = observed),
                          color = mosaic_colors("data"), size = 1.5, alpha = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = predicted_median, color = metric),
                         linewidth = 0.75) +
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
        title = paste0("Posterior Ensemble: ", loc),
        subtitle = paste0(
          n_param_sets, " parameter sets \u00d7 ",
          n_stoch_per, " stochastic = ",
          total_sims, " total simulations"
        ),
        caption = paste0(
          "Ribbons show ", paste(
            paste0(round(envelope_quantiles[seq(1, length(envelope_quantiles), by = 2)] * 100), "-",
                   round(envelope_quantiles[seq(2, length(envelope_quantiles), by = 2)] * 100), "%"),
            collapse = " and "
          ), " confidence intervals\n",
          "Cases: Obs = ", format(round(sum(obs_c, na.rm = TRUE)), big.mark = ","),
          ", Pred = ",     format(round(sum(pred_c, na.rm = TRUE)), big.mark = ","),
          ", R\u00b2 = ", ifelse(is.na(r2_c), "NA", r2_c),
          ", Bias = ",    ifelse(is.na(bias_c), "NA", bias_c),
          " | Deaths: Obs = ", format(round(sum(obs_d, na.rm = TRUE)), big.mark = ","),
          ", Pred = ",         format(round(sum(pred_d, na.rm = TRUE)), big.mark = ","),
          ", R\u00b2 = ", ifelse(is.na(r2_d), "NA", r2_d),
          ", Bias = ",    ifelse(is.na(bias_d), "NA", bias_d)
        )
      )

    p <- .add_date_scale(p)

    plot_list$individual[[loc]] <- p
    if (verbose) print(p)

    out_file <- file.path(output_dir, paste0("model_ensemble_", loc, ".pdf"))
    ggplot2::ggsave(out_file, plot = p, width = 10, height = 6, dpi = 300)
    if (verbose) message("  Saved: ", out_file)
  }

  # ---------------------------------------------------------------------------
  # 2. Faceted plots (multi-location only)
  # ---------------------------------------------------------------------------

  if (n_locations > 1L) {

    # ----- Faceted cases plot -------------------------------------------------

    if (verbose) message("Generating faceted cases plot...")

    cases_data <- plot_data[plot_data$metric == "Suspected Cases", ]
    all_obs_c  <- as.numeric(obs_cases)
    all_pred_c <- as.numeric(cases_median)
    r2_c_all   <- tryCatch(round(calc_model_R2(all_obs_c, all_pred_c), 3L),
                            error = function(e) NA)
    bias_c_all <- tryCatch(round(calc_bias_ratio(all_obs_c, all_pred_c), 2L),
                            error = function(e) NA)

    p_cases <- ggplot2::ggplot(cases_data, ggplot2::aes(x = date))

    for (ci_idx in seq_len(n_ci_pairs)) {
      lower_col <- paste0("ci_", ci_idx, "_lower")
      upper_col <- paste0("ci_", ci_idx, "_upper")
      p_cases <- p_cases +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[[lower_col]],
                                           ymax = .data[[upper_col]]),
                             fill  = mosaic_color_variant(unname(mosaic_colors("cases")), "lighten", 0.3),
                             alpha = ribbon_alphas[ci_idx])
    }

    cases_data_points <- cases_data[!is.na(cases_data$observed), ]

    p_cases <- p_cases +
      ggplot2::geom_point(data = cases_data_points, ggplot2::aes(y = observed),
                          color = mosaic_colors("data"), size = 1.5, alpha = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = predicted_median),
                         color = mosaic_colors("cases"), linewidth = 0.8) +
      ggplot2::facet_wrap(~ location, scales = "free_y",
                          ncol = min(3L, n_locations)) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      theme_mosaic(base_size = 10) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8)) +
      ggplot2::labs(
        x = if (use_date_axis) "Date" else "Time", y = "Suspected Cases",
        title    = "Posterior Ensemble: Suspected Cases by Location",
        subtitle = paste0(n_param_sets, " parameter sets \u00d7 ", n_stoch_per,
                          " stochastic | ", n_successful, " successful sims"),
        caption  = paste0(
          "Total: Obs = ",    format(round(sum(obs_cases, na.rm = TRUE)), big.mark = ","),
          ", Pred = ",         format(round(sum(cases_median, na.rm = TRUE)), big.mark = ","),
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

    out_file <- file.path(output_dir, "model_ensemble_cases_all.pdf")
    ggplot2::ggsave(out_file, plot = p_cases,
                    width = plot_w, height = plot_h, dpi = 300, limitsize = FALSE)
    if (verbose) message("  Saved: ", out_file)

    # ----- Faceted deaths plot ------------------------------------------------

    if (verbose) message("Generating faceted deaths plot...")

    deaths_data <- plot_data[plot_data$metric == "Deaths", ]
    all_obs_d   <- as.numeric(obs_deaths)
    all_pred_d  <- as.numeric(deaths_median)
    r2_d_all    <- tryCatch(round(calc_model_R2(all_obs_d, all_pred_d), 3L),
                            error = function(e) NA)
    bias_d_all  <- tryCatch(round(calc_bias_ratio(all_obs_d, all_pred_d), 2L),
                            error = function(e) NA)

    p_deaths <- ggplot2::ggplot(deaths_data, ggplot2::aes(x = date))

    for (ci_idx in seq_len(n_ci_pairs)) {
      lower_col <- paste0("ci_", ci_idx, "_lower")
      upper_col <- paste0("ci_", ci_idx, "_upper")
      p_deaths <- p_deaths +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[[lower_col]],
                                           ymax = .data[[upper_col]]),
                             fill  = mosaic_color_variant(unname(mosaic_colors("deaths")), "lighten", 0.3),
                             alpha = ribbon_alphas[ci_idx])
    }

    deaths_data_points <- deaths_data[!is.na(deaths_data$observed), ]

    p_deaths <- p_deaths +
      ggplot2::geom_point(data = deaths_data_points, ggplot2::aes(y = observed),
                          color = mosaic_colors("data"), size = 1.5, alpha = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = predicted_median),
                         color = mosaic_colors("deaths"), linewidth = 0.8) +
      ggplot2::facet_wrap(~ location, scales = "free_y",
                          ncol = min(3L, n_locations)) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      theme_mosaic(base_size = 10) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8)) +
      ggplot2::labs(
        x = if (use_date_axis) "Date" else "Time", y = "Deaths",
        title    = "Posterior Ensemble: Deaths by Location",
        subtitle = paste0(n_param_sets, " parameter sets \u00d7 ", n_stoch_per,
                          " stochastic | ", n_successful, " successful sims"),
        caption  = paste0(
          "Total: Obs = ",    format(round(sum(obs_deaths, na.rm = TRUE)), big.mark = ","),
          ", Pred = ",         format(round(sum(deaths_median, na.rm = TRUE)), big.mark = ","),
          ", R\u00b2 = ", ifelse(is.na(r2_d_all), "NA", r2_d_all),
          ", Bias = ",    ifelse(is.na(bias_d_all), "NA", bias_d_all),
          "\nGenerated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      )

    p_deaths <- .add_date_scale(p_deaths)
    plot_list$deaths_faceted <- p_deaths
    if (verbose) print(p_deaths)

    out_file <- file.path(output_dir, "model_ensemble_deaths_all.pdf")
    ggplot2::ggsave(out_file, plot = p_deaths,
                    width = plot_w, height = plot_h, dpi = 300, limitsize = FALSE)
    if (verbose) message("  Saved: ", out_file)
  }

  # ---------------------------------------------------------------------------
  # Return
  # ---------------------------------------------------------------------------

  plot_list$simulation_stats <- list(
    n_param_sets              = n_param_sets,
    n_simulations_per_config  = n_stoch_per,
    n_successful              = n_successful,
    envelope_quantiles        = envelope_quantiles
  )

  if (verbose) message("plot_model_ensemble complete.")
  invisible(plot_list)
}
