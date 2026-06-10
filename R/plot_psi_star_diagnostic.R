#' Plot Raw vs Calibrated Environmental Suitability (psi vs psi*)
#'
#' Creates a time-series plot comparing the raw LSTM-predicted environmental
#' suitability psi with the calibrated psi* after applying the posterior psi_star
#' parameters (\code{psi_star_a}, \code{psi_star_b}, \code{psi_star_z},
#' \code{psi_star_k}) via \code{\link{calc_psi_star}}.
#'
#' @description
#' The psi_star parameters recalibrate the LSTM suitability signal on the logit
#' scale before it enters the transmission model. Without this plot there is no
#' routine way to see how much the calibration suppresses or reshapes the LSTM
#' output. The subtitle reports the mean suppression percentage, making it
#' immediately clear whether psi is acting as a meaningful seasonal driver or
#' being effectively disabled.
#'
#' @param dirs Named list of output directory paths as returned by the internal
#'   \code{.mosaic_ensure_dir_tree()} helper inside \code{run_MOSAIC()}. Required
#'   entries: \code{dirs$res_fig_diag} (output), \code{dirs$inputs} (for
#'   \code{config.json}), \code{dirs$res_post} (for \code{parameter_estimates.csv}).
#' @param PATHS Named list of project paths as returned by \code{get_paths()}.
#'   Used to locate \code{pred_psi_suitability_day.csv} at
#'   \code{PATHS$MODEL_INPUT}.
#' @param location_names Character vector of ISO3 location codes (e.g.
#'   \code{"MOZ"}). One plot is generated per location.
#' @param verbose Logical; if \code{TRUE} (default) emits progress messages.
#'
#' @return Invisibly returns a named list of \code{ggplot} objects, one per
#'   location. Saves PNG files to \code{dirs$res_fig_diag} with filenames
#'   \code{psi_raw_vs_psi_star_{j}.png}.
#'
#' @details
#' The plot is skipped gracefully (with a warning) for any location where:
#' \itemize{
#'   \item the psi_star posterior parameters are absent from
#'     \code{parameter_estimates.csv} (e.g. parameters were frozen or not sampled),
#'   \item \code{pred_psi_suitability_day.csv} cannot be found at
#'     \code{PATHS$MODEL_INPUT}, or
#'   \item the suitability data contains no rows for the location or calibration
#'     window.
#' }
#'
#' @seealso \code{\link{calc_psi_star}} for the transformation applied.
#'
#' @export
plot_psi_star_diagnostic <- function(dirs,
                                     PATHS,
                                     location_names,
                                     verbose = TRUE) {

  # --- Input checks -----------------------------------------------------------
  stopifnot(is.list(dirs), is.list(PATHS), is.character(location_names))

  psi_csv <- file.path(PATHS$MODEL_INPUT, "pred_psi_suitability_day.csv")
  param_csv <- file.path(dirs$res_posterior, "parameter_estimates.csv")
  config_json <- file.path(dirs$inputs, "config.json")

  if (!file.exists(psi_csv)) {
    if (verbose) message("plot_psi_star_diagnostic: psi CSV not found at ", psi_csv,
                         " \u2014 skipping.")
    return(invisible(NULL))
  }
  if (!file.exists(param_csv)) {
    if (verbose) message("plot_psi_star_diagnostic: parameter_estimates.csv not found \u2014 skipping.")
    return(invisible(NULL))
  }
  if (!file.exists(config_json)) {
    if (verbose) message("plot_psi_star_diagnostic: config.json not found \u2014 skipping.")
    return(invisible(NULL))
  }

  # --- Load shared data -------------------------------------------------------
  params     <- read.csv(param_csv, stringsAsFactors = FALSE)
  config     <- jsonlite::fromJSON(config_json, simplifyVector = TRUE)
  psi_raw_all <- read.csv(psi_csv, stringsAsFactors = FALSE)

  # Visualize the SAME series that feeds psi_jt -> LASER: the canonical `psi`
  # column (smoothed + bias-corrected). Fall back to `pred_smooth` only for a
  # stale pre-v0.34 CSV so the diagnostic degrades gracefully rather than erroring.
  psi_col <- if ("psi" %in% names(psi_raw_all)) "psi" else "pred_smooth"
  if (psi_col != "psi" && verbose)
    message("plot_psi_star_diagnostic: `psi` column absent; using pre-correction `pred_smooth` (regenerate psi CSV with est_suitability v0.34+).")

  date_start <- as.Date(config$date_start)
  date_stop  <- as.Date(config$date_stop)

  out_dir <- dirs$res_fig_diag
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # --- Iterate over locations -------------------------------------------------
  plots_out <- list()

  for (j in location_names) {

    tryCatch({

      # Extract psi_star posteriors for this location
      get_param <- function(nm) {
        v <- params$median[params$parameter == nm]
        if (length(v) == 0L) return(NULL)
        v
      }

      psi_a <- get_param(paste0("psi_star_a_", j))
      psi_b <- get_param(paste0("psi_star_b_", j))
      psi_z <- get_param(paste0("psi_star_z_", j))
      psi_k <- get_param(paste0("psi_star_k_", j))

      if (any(sapply(list(psi_a, psi_b, psi_z, psi_k), is.null))) {
        if (verbose) message(sprintf(
          "plot_psi_star_diagnostic: psi_star params not found for %s \u2014 skipping.", j))
        next
      }

      # Filter psi data to this location and calibration window
      psi_loc <- psi_raw_all[psi_raw_all$iso_code == j, ]
      psi_loc$date <- as.Date(psi_loc$date)
      psi_cal <- psi_loc[psi_loc$date >= date_start & psi_loc$date <= date_stop, ]

      if (nrow(psi_cal) == 0L) {
        if (verbose) message(sprintf(
          "plot_psi_star_diagnostic: no psi data in calibration window for %s \u2014 skipping.", j))
        next
      }

      # Apply full psi_star transformation via package function
      psi_star_series <- calc_psi_star(
        psi             = psi_cal[[psi_col]],
        a               = psi_a,
        b               = psi_b,
        z               = psi_z,
        k               = psi_k,
        warn_k_rounding = FALSE
      )

      # Build data frame and summary stats
      df <- data.frame(
        date     = psi_cal$date,
        raw      = psi_cal[[psi_col]],
        psi_star = psi_star_series
      )

      raw_mean  <- mean(df$raw,      na.rm = TRUE)
      star_mean <- mean(df$psi_star, na.rm = TRUE)
      supp_pct  <- if (isTRUE(raw_mean > 0)) 100 * (1 - star_mean / raw_mean) else NA_real_

      supp_str <- if (!is.na(supp_pct)) sprintf("%.1f%% suppression", supp_pct) else "suppression N/A"
      subtitle <- sprintf(
        "a=%.3f  b=%.2f  z=%.3f  k=%.0f  |  Raw mean=%.3f  \u2192  \u03c8* mean=%.4f  (%s)",
        psi_a, psi_b, psi_z, psi_k,
        raw_mean, star_mean, supp_str
      )

      # Build plot
      p <- ggplot2::ggplot(df, ggplot2::aes(x = date)) +
        ggplot2::geom_area(ggplot2::aes(y = raw),
                           fill = "#2196F3", alpha = 0.20) +
        ggplot2::geom_line(ggplot2::aes(y = raw,
                                        colour = "Raw LSTM \u03c8"),
                           linewidth = 0.6, alpha = 0.85) +
        ggplot2::geom_area(ggplot2::aes(y = psi_star),
                           fill = "#E63946", alpha = 0.40) +
        ggplot2::geom_line(ggplot2::aes(y = psi_star,
                                        colour = "Calibrated \u03c8* (psi_star)"),
                           linewidth = 0.7) +
        ggplot2::scale_colour_manual(
          name   = NULL,
          values = c("Raw LSTM \u03c8"               = "#2196F3",
                     "Calibrated \u03c8* (psi_star)" = "#E63946"),
          guide  = ggplot2::guide_legend(
            override.aes = list(linewidth = 1.2))
        ) +
        ggplot2::scale_x_date(
          date_breaks = "1 year",
          date_labels = "%Y",
          expand      = ggplot2::expansion(mult = c(0.01, 0.01))
        ) +
        ggplot2::scale_y_continuous(
          limits = c(0, 1),
          breaks = seq(0, 1, 0.2),
          labels = scales::number_format(accuracy = 0.1)
        ) +
        ggplot2::labs(
          title   = sprintf(
            "Environmental Suitability: Raw LSTM \u03c8 vs Calibrated \u03c8* (%s)", j),
          subtitle = subtitle,
          x        = NULL,
          y        = "\u03c8 (suitability)"
        ) +
        theme_mosaic() +
        ggplot2::theme(
          legend.position  = "top",
          legend.direction = "horizontal",
          plot.subtitle    = ggplot2::element_text(size = 8, colour = "grey40")
        )

      # Save
      out_path <- file.path(out_dir,
                            sprintf("psi_raw_vs_psi_star_%s.png", j))
      ggplot2::ggsave(out_path, p, width = 12, height = 4.5, dpi = 180)
      if (verbose) message(sprintf("  Saved %s",
                                   file.path("3_results/figures/diagnostics",
                                             basename(out_path))))

      plots_out[[j]] <- p

    }, error = function(e) {
      if (verbose) message(sprintf(
        "plot_psi_star_diagnostic: failed for %s \u2014 %s", j, e$message))
    })
  }

  invisible(plots_out)
}
