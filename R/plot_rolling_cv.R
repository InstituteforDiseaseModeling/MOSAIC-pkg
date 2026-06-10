#' Presentation-quality plot of a rolling-origin forecast-validation artifact
#'
#' Renders the out-of-sample forecast from a \code{\link{run_rolling_cv}}
#' predictions artifact for one metric, one panel per cutoff (forecast origin),
#' restricted to the assessed horizon (default \eqn{\le 5} months). The two
#' posterior ensembles (\code{ensemble}, \code{ensemble_opt}) are drawn as
#' "hero" series with 50/95\% prediction-interval ribbons; the two point
#' configurations (\code{best}, \code{medioid}) are drawn as thin reference
#' lines. Held-out observations are overlaid as points and the forecast origin
#' is marked with a labelled dashed rule. Each panel is annotated with the
#' cumulative \eqn{\le}max-horizon skill (R\eqn{^2} correlation + WIS) of the
#' ensembles, computed via \code{\link{evaluate_rolling_cv}}.
#'
#' @details
#' \strong{Horizon truncation.} The compiled artifact stores the full OOS path
#' to \code{window_stop} regardless of cutoff, so an early cutoff appears to
#' forecast years ahead. This function clips each panel to
#' \code{[cutoff - context_months, cutoff + horizon_max_months]} so the plotted
#' window equals the scored window. Scoring in \code{evaluate_rolling_cv()} is
#' already restricted to cumulative \eqn{\le h}-month windows, so this is a
#' display change only.
#'
#' \strong{Styling.} Uses \code{\link{theme_mosaic}} and the MOSAIC semantic
#' palette. Output is sized for 16:9 slides and written as both PNG (raster,
#' \code{dpi}) and PDF (vector) when \code{dir_output} is supplied.
#'
#' @param predictions A \code{run_rolling_cv} output directory, a path to a
#'   \code{predictions.parquet}, or the predictions data frame itself.
#' @param metric Metric to plot: \code{"cases"} (default) or \code{"deaths"}.
#' @param horizon_max_months Max assessed horizon in months; right edge of each
#'   panel relative to its cutoff (default 5).
#' @param context_months Months of in-sample context shown before the cutoff
#'   (left edge of each panel, default 2).
#' @param models_ribbon Models drawn with 50/95\% PI ribbons + median line
#'   (default \code{c("ensemble", "ensemble_opt")}).
#' @param models_line Models drawn as thin reference median lines
#'   (default \code{c("best", "medioid")}).
#' @param annotate Annotate each panel with ensemble \eqn{\le}max-horizon skill
#'   (R\eqn{^2}_corr + WIS). Default \code{TRUE}.
#' @param eval Optional precomputed \code{\link{evaluate_rolling_cv}} result
#'   (its \code{$cells}) to source annotations from; recomputed if \code{NULL}.
#' @param title,subtitle,caption Optional plot labels; sensible defaults are
#'   derived from the artifact when \code{NULL}.
#' @param dir_output Directory to write figures to. If \code{NULL} (default)
#'   nothing is written and the ggplot objects are returned for further use.
#' @param file_prefix Filename stem for written figures
#'   (default \code{"rolling_cv_forecast"}).
#' @param width Figure width in inches (default 11).
#' @param height_panel Height in inches allotted per cutoff panel (default 3.1).
#' @param dpi Raster resolution for the PNG (default 300).
#' @param base_size Base font size for \code{theme_mosaic()} (default 14).
#' @param save_pdf Also write a vector PDF alongside each PNG. Default \code{TRUE}.
#' @param verbose Print progress messages. Default \code{TRUE}.
#'
#' @return Invisibly, a list with:
#' \describe{
#'   \item{overview}{A faceted ggplot (one panel per cutoff).}
#'   \item{per_cutoff}{Named list of single-cutoff ggplots.}
#'   \item{data}{The truncated long data frame that was plotted.}
#'   \item{skill}{The \eqn{\le}max-horizon skill cells used for annotation.}
#'   \item{files}{Character vector of any files written.}
#' }
#' @seealso \code{\link{run_rolling_cv}}, \code{\link{evaluate_rolling_cv}},
#'   \code{\link{theme_mosaic}}, \code{\link{mosaic_colors}}
#' @importFrom rlang .data
#' @export
plot_rolling_cv <- function(predictions,
                            metric             = c("cases", "deaths"),
                            horizon_max_months = 5,
                            context_months     = 2,
                            models_ribbon      = c("ensemble", "ensemble_opt"),
                            models_line        = c("best", "medioid"),
                            annotate           = TRUE,
                            eval               = NULL,
                            title              = NULL,
                            subtitle           = NULL,
                            caption            = NULL,
                            dir_output         = NULL,
                            file_prefix        = "rolling_cv_forecast",
                            width              = 11,
                            height_panel       = 3.1,
                            dpi                = 300,
                            base_size          = 14,
                            save_pdf           = TRUE,
                            verbose            = TRUE) {

     metric <- match.arg(metric)

     # ---- load + validate -----------------------------------------------------
     d <- .rcv_eval_input(predictions)
     req <- c("model", "cutoff_date", "date", "metric", "segment",
              "observed", "pred_median", "pi50_lo", "pi50_hi", "pi95_lo", "pi95_hi")
     miss <- setdiff(req, names(d))
     if (length(miss))
          stop("predictions missing column(s): ", paste(miss, collapse = ", "))
     d <- d[d$metric == metric, , drop = FALSE]
     if (!nrow(d)) stop("no rows for metric '", metric, "'")
     d$date        <- as.Date(d$date)
     d$cutoff_date <- as.Date(d$cutoff_date)

     present <- unique(d$model)
     models_ribbon <- intersect(models_ribbon, present)
     models_line   <- intersect(setdiff(models_line, models_ribbon), present)
     keep_models   <- c(models_ribbon, models_line)
     if (!length(keep_models)) stop("none of the requested models are present")
     d <- d[d$model %in% keep_models, , drop = FALSE]

     # ---- truncate each cutoff to [cutoff - context, cutoff + horizon] ---------
     lo_off <- ceiling(context_months     * 30.4375)
     hi_off <- ceiling(horizon_max_months * 30.4375)
     d <- d[d$date >= (d$cutoff_date - lo_off) & d$date <= (d$cutoff_date + hi_off), ,
            drop = FALSE]
     if (!nrow(d)) stop("no rows remain after horizon truncation")

     cutoffs <- sort(unique(d$cutoff_date))
     iso     <- if ("iso_code" %in% names(d)) d$iso_code[1] else NA_character_
     d$panel <- factor(paste0("Forecast origin: ", format(d$cutoff_date, "%Y-%m-%d")),
                        levels = paste0("Forecast origin: ", format(cutoffs, "%Y-%m-%d")))

     # canonical model order + presentation styling
     order_all <- c("ensemble", "ensemble_opt", "best", "medioid")
     lvl       <- order_all[order_all %in% keep_models]
     d$model   <- factor(d$model, levels = lvl)
     model_cols <- c(ensemble = unname(mosaic_colors("cases")), ensemble_opt = "#EE7733",
                     best = "#555555", medioid = "#999999")[lvl]
     model_labs <- c(ensemble = "Ensemble", ensemble_opt = "Optimized ensemble",
                     best = "Best", medioid = "Medioid")[lvl]
     model_lwd  <- c(ensemble = 1.1, ensemble_opt = 1.1, best = 0.55, medioid = 0.55)[lvl]
     model_lty  <- c(ensemble = "solid", ensemble_opt = "solid",
                     best = "longdash", medioid = "dotted")[lvl]

     # ---- forecast-region band + cutoff rules (one row per cutoff) ------------
     band_df <- data.frame(
          cutoff_date = cutoffs,
          xmin        = cutoffs,
          xmax        = cutoffs + hi_off,
          panel       = factor(paste0("Forecast origin: ", format(cutoffs, "%Y-%m-%d")),
                               levels = levels(d$panel)),
          stringsAsFactors = FALSE)

     # ---- per-cutoff skill annotation (ensembles, cumulative <= max horizon) --
     anno_df <- NULL
     skill_cells <- NULL
     if (annotate) {
          cells <- if (!is.null(eval) && is.list(eval) && !is.null(eval$cells)) eval$cells
                   else evaluate_rolling_cv(d, horizons_months = horizon_max_months,
                                            metrics = metric, n_boot = 1L)$cells
          win <- sprintf("OOS<=%gmo", horizon_max_months)
          skill_cells <- cells[cells$metric == metric & cells$window == win &
                               cells$model %in% models_ribbon, , drop = FALSE]
          if (nrow(skill_cells)) {
               skill_cells$cutoff_date <- as.Date(skill_cells$cutoff_date)
               lab_by_cut <- tapply(seq_len(nrow(skill_cells)), skill_cells$cutoff_date,
                                    function(ix) {
                                         s <- skill_cells[ix, , drop = FALSE]
                                         s <- s[match(intersect(lvl, s$model), s$model), , drop = FALSE]
                                         paste(sprintf("%s: R2=%.2f  WIS=%.1f",
                                                       model_labs[as.character(s$model)],
                                                       s$R2_corr, s$wis), collapse = "\n")
                                    })
               anno_df <- data.frame(
                    cutoff_date = as.Date(names(lab_by_cut)),
                    label       = unname(lab_by_cut),
                    stringsAsFactors = FALSE)
               anno_df$x     <- anno_df$cutoff_date - lo_off
               anno_df$panel <- factor(paste0("Forecast origin: ",
                                              format(anno_df$cutoff_date, "%Y-%m-%d")),
                                       levels = levels(d$panel))
          }
     }

     # ---- default labels (ASCII-only so any graphics device renders cleanly) --
     iso_lab <- if (!is.na(iso)) paste0(iso, ": ") else ""
     metric_lab <- if (metric == "cases") "reported cases" else "reported deaths"
     user_subtitle <- !is.null(subtitle)
     base_sub <- sprintf("Out-of-sample %s vs ensemble forecast | %g-month horizon",
                         metric_lab, horizon_max_months)
     if (is.null(title))
          title <- paste0(iso_lab, "rolling-origin forecast validation")
     if (is.null(subtitle))
          subtitle <- paste0(base_sub, " | ", length(cutoffs), " origins")
     if (is.null(caption))
          caption <- paste0("Bands = 50/95% prediction intervals (ensembles); ",
                            "points = held-out observations; dashed line = forecast origin.")

     # ---- the plot builder ----------------------------------------------------
     build <- function(dd, bb, aa, faceted) {
          obs_col <- unname(mosaic_colors("data"))
          dr <- dd[dd$model %in% models_ribbon, , drop = FALSE]
          p <- ggplot2::ggplot(dd, ggplot2::aes(x = .data[["date"]])) +
               ggplot2::geom_rect(data = bb, inherit.aes = FALSE,
                                  ggplot2::aes(xmin = .data[["xmin"]], xmax = .data[["xmax"]],
                                               ymin = -Inf, ymax = Inf),
                                  fill = "#000000", alpha = 0.035)
          # ribbons (95 then 50) for the two ensembles
          if (nrow(dr)) {
               p <- p +
                    ggplot2::geom_ribbon(data = dr,
                         ggplot2::aes(ymin = .data[["pi95_lo"]], ymax = .data[["pi95_hi"]],
                                      fill = .data[["model"]]), alpha = 0.16) +
                    ggplot2::geom_ribbon(data = dr,
                         ggplot2::aes(ymin = .data[["pi50_lo"]], ymax = .data[["pi50_hi"]],
                                      fill = .data[["model"]]), alpha = 0.30)
          }
          p <- p +
               ggplot2::geom_vline(data = bb, inherit.aes = FALSE,
                                   ggplot2::aes(xintercept = .data[["cutoff_date"]]),
                                   linetype = 2, linewidth = 0.4, colour = "#B5123B") +
               ggplot2::geom_line(ggplot2::aes(y = .data[["pred_median"]], colour = .data[["model"]],
                                               linewidth = .data[["model"]], linetype = .data[["model"]]),
                                  na.rm = TRUE) +
               ggplot2::geom_point(ggplot2::aes(y = .data[["observed"]]),
                                   colour = obs_col, size = 0.7, na.rm = TRUE) +
               ggplot2::scale_colour_manual(values = model_cols, labels = model_labs,
                                            name = NULL, drop = FALSE) +
               ggplot2::scale_fill_manual(values = model_cols, guide = "none") +
               ggplot2::scale_linewidth_manual(values = model_lwd, guide = "none") +
               ggplot2::scale_linetype_manual(values = model_lty, guide = "none") +
               ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.05))) +
               ggplot2::labs(title = title, subtitle = subtitle, caption = caption,
                             x = NULL, y = metric_lab) +
               theme_mosaic(base_size = base_size) +
               ggplot2::theme(legend.position = "top",
                              legend.key.width = ggplot2::unit(1.4, "lines"))
          if (!is.null(aa) && nrow(aa))
               p <- p + ggplot2::geom_text(data = aa, inherit.aes = FALSE,
                              ggplot2::aes(x = .data[["x"]], y = Inf, label = .data[["label"]]),
                              hjust = 0, vjust = 1.25, size = base_size / 4.2,
                              colour = "#333333", lineheight = 0.95)
          if (faceted)
               p <- p + ggplot2::facet_wrap(~ panel, ncol = 1, scales = "free")
          p + ggplot2::guides(colour = ggplot2::guide_legend(
                    override.aes = list(linewidth = 1.1, linetype = model_lty)))
     }

     overview <- build(d, band_df, anno_df, faceted = TRUE)

     per_cutoff <- stats::setNames(lapply(cutoffs, function(cd) {
          pl  <- levels(d$panel)[match(cd, cutoffs)]
          ddc <- d[d$panel == pl, , drop = FALSE]
          bbc <- band_df[band_df$cutoff_date == cd, , drop = FALSE]
          aac <- if (!is.null(anno_df)) anno_df[anno_df$cutoff_date == cd, , drop = FALSE] else NULL
          sub_cd <- if (user_subtitle) subtitle
                    else paste0(base_sub, " | origin ", format(cd, "%Y-%m-%d"))
          build(ddc, bbc, aac, faceted = FALSE) +
               ggplot2::ggtitle(title, subtitle = sub_cd)
     }), format(cutoffs, "%Y-%m-%d"))

     # ---- write -------------------------------------------------------------
     files <- character(0)
     if (!is.null(dir_output)) {
          dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)
          ov_h <- height_panel * length(cutoffs) + 1.6
          save_one <- function(p, stem, h) {
               png <- file.path(dir_output, paste0(stem, ".png"))
               ggplot2::ggsave(png, p, width = width, height = h, dpi = dpi, limitsize = FALSE)
               out <- png
               if (save_pdf) {
                    pdf <- file.path(dir_output, paste0(stem, ".pdf"))
                    # base pdf device: portable (no cairo/X11), warning-free with ASCII labels
                    ok <- tryCatch({
                         ggplot2::ggsave(pdf, p, width = width, height = h,
                                         device = grDevices::pdf, limitsize = FALSE)
                         file.exists(pdf)
                    }, error = function(e) FALSE)
                    if (isTRUE(ok)) out <- c(out, pdf)
               }
               out
          }
          files <- c(files, save_one(overview, paste0(file_prefix, "_", metric, "_overview"), ov_h))
          for (cd in names(per_cutoff))
               files <- c(files, save_one(per_cutoff[[cd]],
                            paste0(file_prefix, "_", metric, "_", cd), height_panel + 2.6))
          if (verbose) message("plot_rolling_cv: wrote ", length(files), " file(s) to ", dir_output)
     }

     invisible(list(overview = overview, per_cutoff = per_cutoff,
                    data = d, skill = skill_cells, files = files))
}
