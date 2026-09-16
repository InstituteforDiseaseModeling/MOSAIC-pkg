#' Publication timeseries grid for rolling-origin forecast cross-validation
#'
#' Renders a country (column) \eqn{\times} cutoff (row) grid of observed-vs-model
#' timeseries for one metric, the intuitive companion to the scalar skill table
#' (\code{\link{make_forecast_cv_table}}). One figure per metric (cases in MOSAIC
#' blue, deaths in MOSAIC red).
#'
#' Encodings (locked for the OCV-4 experiment):
#' \itemize{
#'   \item Observed points by \code{segment}: filled circle = in-sample (training,
#'     \eqn{\le} cutoff), \code{x} = embargo-gap weeks, open circle = out-of-sample
#'     (validation, shown up to \code{forecast_display_months} past the cutoff).
#'   \item Model \code{pred_median}: solid line + full-opacity CI ribbon for
#'     dates \eqn{\le} cutoff; solid line + lighter CI ribbon after the cutoff (distinguished by the dashed cutoff rule)
#'     (clipped to \code{forecast_display_months}).
#'   \item Dashed vertical line at the cutoff; a faint dotted line at
#'     \code{cutoff + scored_horizon_months} marks the formally-scored boundary
#'     (the displayed window is longer than the scored window on purpose).
#' }
#'
#' Per-country fixed y-axis (free across countries) is achieved by assembling one
#' \code{\link[patchwork]{patchwork}} column per country (vanilla \code{facet_grid}
#' cannot free y by column). The canvas is sized \emph{width} \eqn{\propto} number
#' of countries and \emph{height} \eqn{\propto} number of cutoffs, so adding
#' countries widens the figure rather than squishing panels.
#'
#' @param predictions Predictions to plot: a data.frame, a path to a
#'   \code{predictions*.parquet}/\code{.csv}, or a directory holding per-cell
#'   \code{*/cutoff_*/predictions.parquet}. Must carry \code{iso_code,
#'   cutoff_date, date, metric, segment, observed, pred_median, model} and the CI
#'   columns for \code{ci} (e.g. \code{pi95_lo}/\code{pi95_hi}).
#' @param metric Single metric to render: \code{"cases"} or \code{"deaths"}.
#' @param model Model series to plot (default \code{"ensemble_opt"}, the
#'   pre-registered headline).
#' @param isos Country column order (character); default sorted unique isos.
#' @param x_range Optional \code{c(min,max)} Date for the shared x-axis; default
#'   spans \code{min(cutoff) - 6 months} to the earlier of
#'   \code{max(cutoff) + forecast_display_months} and the last observed date.
#' @param forecast_display_months Months of forecast shown past each cutoff
#'   (default 9). Beyond this, model line and OOS points are dropped.
#' @param scored_horizon_months Formally-scored horizon marker (default 3).
#' @param ci CI band to shade: \code{"pi95"} (default) or \code{"pi50"}.
#' @param colors Optional \code{c(line=, ci=)} hex overrides; default derives
#'   from \code{\link{mosaic_colors}(metric)} + a lightened CI variant.
#' @param dir_output If non-NULL, save the figure there.
#' @param file_prefix Output file stem (default \code{"forecast_cv_grid"}); the
#'   metric and model are appended.
#' @param width_per_country,height_per_cutoff Panel sizing (inches) for the saved
#'   canvas (default 4.2 and 1.55).
#' @param save_pdf,save_png Save a (cairo) PDF and/or PNG (default PDF only).
#' @param verbose Emit progress messages (default TRUE).
#'
#' @return (Invisibly) the assembled \code{patchwork} object.
#' @seealso \code{\link{make_forecast_cv_table}}, \code{\link{run_rolling_cv}},
#'   \code{\link{evaluate_rolling_cv}}
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_vline geom_point
#'   scale_shape_manual scale_x_date scale_y_continuous facet_wrap labs
#'   theme_bw theme element_blank element_rect element_text expansion ggsave
#'   unit
#' @importFrom rlang .data
#' @export
plot_forecast_cv_grid <- function(predictions,
                                  metric = c("cases", "deaths"),
                                  model = "ensemble_opt",
                                  isos = NULL,
                                  x_range = NULL,
                                  forecast_display_months = 9,
                                  scored_horizon_months = 3,
                                  ci = c("pi95", "pi50"),
                                  colors = NULL,
                                  dir_output = NULL,
                                  file_prefix = "forecast_cv_grid",
                                  width_per_country = 4.2,
                                  height_per_cutoff = 1.55,
                                  save_pdf = TRUE,
                                  save_png = FALSE,
                                  verbose = TRUE) {

     if (!requireNamespace("patchwork", quietly = TRUE))
          stop("plot_forecast_cv_grid requires the 'patchwork' package.")
     metric <- match.arg(metric)
     ci     <- match.arg(ci)
     lo <- paste0(ci, "_lo"); hi <- paste0(ci, "_hi")

     d <- .fcv_read_predictions(predictions)
     req <- c("iso_code", "cutoff_date", "date", "metric", "segment",
              "observed", "pred_median", "model", lo, hi)
     miss <- setdiff(req, names(d))
     if (length(miss)) stop("predictions missing column(s): ", paste(miss, collapse = ", "))

     d <- d[d$metric == metric & d$model == model, , drop = FALSE]
     if (!nrow(d)) stop("No rows for metric='", metric, "', model='", model, "'.")
     d$date        <- as.Date(d$date)
     d$cutoff_date <- as.Date(d$cutoff_date)

     if (is.null(isos)) isos <- sort(unique(d$iso_code)) else d <- d[d$iso_code %in% isos, , drop = FALSE]
     cutoffs <- sort(unique(d$cutoff_date))
     cut_lab <- format(cutoffs, "%Y-%m")

     # Shared x-window
     if (is.null(x_range)) {
          xmin <- min(cutoffs) - 183L
          obs_max <- suppressWarnings(max(d$date[is.finite(d$observed)], na.rm = TRUE))
          xmax <- min(max(cutoffs) + ceiling(forecast_display_months * 30.4375),
                      if (is.finite(obs_max)) obs_max else max(d$date))
          x_range <- c(xmin, xmax)
     }
     x_range <- as.Date(x_range)

     if (is.null(colors)) {
          line_col <- unname(MOSAIC::mosaic_colors(metric))
          ci_col   <- MOSAIC::mosaic_color_variant(line_col, "lighten", 0.3)
          colors <- c(line = line_col, ci = ci_col)
     }
     ylab <- sprintf("Reported %s per week", metric)

     # Global cutoff factor so every country column shows the SAME rows (empty
     # panels for absent (iso,cutoff), keeping rows aligned across columns).
     mk_col <- function(iso) {
          di <- d[d$iso_code == iso, , drop = FALSE]
          di$fc_end <- di$cutoff_date + ceiling(forecast_display_months * 30.4375)
          di$cut_lab <- factor(format(di$cutoff_date, "%Y-%m"), levels = cut_lab)
          di <- di[di$date >= x_range[1] & di$date <= x_range[2], , drop = FALSE]
          pre  <- di[di$date <= di$cutoff_date, , drop = FALSE]
          post <- di[di$date >= di$cutoff_date & di$date <= di$fc_end, , drop = FALSE]
          oi   <- di[is.finite(di$observed) &
                          (di$segment == "IS" | di$date <= di$fc_end), , drop = FALSE]
          oi$cls <- factor(ifelse(oi$segment == "IS", "IS (train)",
                            ifelse(oi$segment == "embargo", "gap", "OOS (validation)")),
                           levels = c("IS (train)", "gap", "OOS (validation)"))
          vlines <- di[!duplicated(di$cutoff_date), , drop = FALSE]
          vlines$scored_end <- vlines$cutoff_date + ceiling(scored_horizon_months * 30.4375)

          # Layer order (bottom -> top): CI ribbon, then cutoff/scored rules,
          # then observed points, then the model median line ON TOP of everything.
          p <- ggplot2::ggplot() +
               ggplot2::geom_ribbon(data = pre,  ggplot2::aes(.data$date, ymin = .data[[lo]], ymax = .data[[hi]]),
                                    fill = colors[["ci"]], alpha = 0.40) +
               ggplot2::geom_ribbon(data = post, ggplot2::aes(.data$date, ymin = .data[[lo]], ymax = .data[[hi]]),
                                    fill = colors[["ci"]], alpha = 0.17) +
               ggplot2::geom_vline(data = vlines, ggplot2::aes(xintercept = .data$scored_end),
                                   linetype = "dotted", color = "grey65", linewidth = 0.3) +
               ggplot2::geom_vline(data = vlines, ggplot2::aes(xintercept = .data$cutoff_date),
                                   linetype = "dashed", color = "grey35", linewidth = 0.35)
          # observed points (behind the model line); only when present
          if (nrow(oi) > 0)
               p <- p +
                    ggplot2::geom_point(data = oi, ggplot2::aes(.data$date, .data$observed, shape = .data$cls),
                                        color = "grey15", fill = "white", size = 1.15, stroke = 0.5,
                                        alpha = 0.30) +
                    ggplot2::scale_shape_manual(values = c("IS (train)" = 16, "gap" = 4, "OOS (validation)" = 21),
                                                drop = FALSE, name = NULL)
          # model median line LAST -> drawn on top of ribbon + points
          p <- p +
               ggplot2::geom_line(data = pre,  ggplot2::aes(.data$date, .data$pred_median),
                                  color = colors[["line"]], linewidth = 1.0) +
               ggplot2::geom_line(data = post, ggplot2::aes(.data$date, .data$pred_median),
                                  color = colors[["line"]], linewidth = 1.0)
          p +
               ggplot2::scale_x_date(limits = x_range, date_breaks = "6 months", date_labels = "%y-%m",
                                     expand = ggplot2::expansion(mult = 0.01)) +
               ggplot2::scale_y_continuous(labels = scales::label_comma()) +
               ggplot2::facet_wrap(~ cut_lab, ncol = 1, strip.position = "right", drop = FALSE) +
               ggplot2::labs(title = iso, x = NULL, y = NULL) +
               ggplot2::theme_bw(base_size = 8) +
               ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                              strip.background = ggplot2::element_rect(fill = "grey92", color = NA),
                              strip.text.y.right = ggplot2::element_text(angle = 0, size = 7),
                              axis.text = ggplot2::element_text(size = 6),
                              plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 11))
     }

     if (verbose) message(sprintf("plot_forecast_cv_grid: %s / %s | %d countries x %d cutoffs",
                                  metric, model, length(isos), length(cutoffs)))
     cols <- lapply(isos, mk_col)
     # ASCII-only annotation text so the plain pdf device renders correctly where
     # cairo is unavailable (point *shapes* carry the filled/x/open encoding).
     fig <- patchwork::wrap_plots(cols, nrow = 1) +
          patchwork::plot_annotation(
               title = sprintf("MOSAIC rolling-origin forecast CV -- reported %s (%s)", metric, model),
               subtitle = paste0("filled circle = in-sample train | x = embargo gap | open circle = OOS validation   ||   ",
                                 "solid + 95% CI up to cutoff (dashed line); solid + lighter CI after ",
                                 "(shown to ", forecast_display_months, " mo; dotted line = ",
                                 scored_horizon_months, "-mo scored horizon)"),
               caption = ylab) &
          ggplot2::theme(legend.position = "bottom",
                         plot.title = ggplot2::element_text(face = "bold"))
     fig <- fig + patchwork::plot_layout(guides = "collect")

     if (!is.null(dir_output)) {
          dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)
          w <- max(6, width_per_country * length(isos))
          h <- max(6, height_per_cutoff * length(cutoffs) + 1.2)
          stem <- file.path(dir_output, sprintf("%s_%s_%s", file_prefix, metric, model))
          if (save_pdf) {
               # Prefer cairo_pdf (nicer text) but fall back to the base pdf device
               # if cairo cannot actually be loaded (e.g. no X11); ASCII text above
               # keeps the base device correct.
               dev <- grDevices::pdf
               if (capabilities("cairo")) {
                    ok <- tryCatch({ tf <- tempfile(fileext = ".pdf"); grDevices::cairo_pdf(tf)
                                     grDevices::dev.off(); unlink(tf); TRUE },
                                   error = function(e) FALSE, warning = function(e) FALSE)
                    if (ok) dev <- grDevices::cairo_pdf
               }
               ggplot2::ggsave(paste0(stem, ".pdf"), fig, width = w, height = h,
                               device = dev, limitsize = FALSE)
               if (verbose) message("  saved ", paste0(stem, ".pdf"), sprintf("  (%.0f x %.0f in)", w, h))
          }
          if (save_png) {
               ggplot2::ggsave(paste0(stem, ".png"), fig, width = w, height = h, dpi = 130, limitsize = FALSE)
               if (verbose) message("  saved ", paste0(stem, ".png"))
          }
     }
     invisible(fig)
}

#' Normalize a predictions argument (data.frame / file / dir) to a data.frame
#' @keywords internal
#' @noRd
.fcv_read_predictions <- function(predictions) {
     if (is.data.frame(predictions)) return(as.data.frame(predictions))
     if (!is.character(predictions) || length(predictions) != 1L)
          stop("predictions must be a data.frame, a file path, or a directory path.")
     rd <- function(f) {
          if (grepl("\\.parquet$", f)) {
               if (!requireNamespace("arrow", quietly = TRUE)) stop("Reading parquet needs 'arrow'.")
               as.data.frame(arrow::read_parquet(f))
          } else utils::read.csv(f, stringsAsFactors = FALSE)
     }
     if (dir.exists(predictions)) {
          fs <- Sys.glob(file.path(predictions, "*", "cutoff_*", "predictions.parquet"))
          if (!length(fs)) fs <- Sys.glob(file.path(predictions, "predictions*.parquet"))
          if (!length(fs)) stop("No predictions parquet found under: ", predictions)
          return(do.call(rbind, lapply(fs, rd)))
     }
     if (file.exists(predictions)) return(rd(predictions))
     stop("predictions path not found: ", predictions)
}
