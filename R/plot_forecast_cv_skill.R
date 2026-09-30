# Bare column names used inside ggplot2::aes() (evaluated in the plotted data
# frame); declared here so R CMD check does not flag them as undefined globals.
utils::globalVariables(c("val", "unit_lab", "origin", "ess_ok", "med_val", "lab"))

#' Headline forecast-CV plot: per-country out-of-sample metric across origins
#'
#' Draws the headline view of a \code{\link{run_rolling_cv}} / forecast-CV
#' experiment that \code{\link{plot_rolling_cv}} does not: a per-country
#' out-of-sample summary at a single primary horizon, \strong{one point per
#' forecast origin} (cutoff), faceted by metric. With only a few origins per
#' country there is no meaningful interval, so it shows the \strong{raw per-origin
#' values} plus the per-country median (diamond) and a tally annotation -- never a
#' confidence interval -- matching the small-\eqn{n} discipline of
#' \code{\link{evaluate_rolling_cv}}.
#'
#' \code{value} selects what to plot:
#' \itemize{
#'   \item \code{"R2_corr"} / \code{"R2_sse"} -- out-of-sample R^2 (shape vs
#'     scale-aware); tally = median.
#'   \item \code{"bias_ratio"} -- mean(pred)/mean(obs); reference line at 1
#'     (perfect); tally = origins within \[0.5, 2\].
#'   \item \code{"wis_skill"} / \code{"mae_skill"} -- skill vs \code{baseline}
#'     (\code{1 - score_model/score_baseline}); reference line at 0; tally =
#'     origins with skill > 0 (i.e. beating the baseline).
#' }
#' All are \strong{conditional/hindcast} values when the experiment used realized
#' covariates (the forecast-CV default) -- label them so.
#'
#' @param x A forecast-CV output directory (with \code{scores_cells.parquet}), a
#'   path to that parquet/csv, or the \code{$cells} data frame from
#'   \code{\link{evaluate_rolling_cv}}.
#' @param value What to plot (see Details). Default \code{"R2_corr"}.
#' @param horizon_months Primary OOS horizon; mapped to the \code{OOS<=\{h\}mo}
#'   window (default 6).
#' @param baseline Baseline for the skill values (default \code{"seasonal"});
#'   ignored for R2/bias.
#' @param model Model type to plot (default \code{"ensemble"}).
#' @param metrics Channels to facet (default \code{c("cases","deaths")}).
#' @param show_gated Logical; draw ESS-gated-out origins as hollow points instead
#'   of dropping them (default TRUE). Gated origins are excluded from the median +
#'   tally regardless.
#' @param title,subtitle,caption Plot labels (sensible defaults from \code{value}).
#' @param dir_output Directory to write the figure (PNG + PDF); NULL returns the
#'   ggplot only.
#' @param file_prefix Filename stem (default \code{"forecast_cv"}).
#' @param width,height,dpi,base_size Figure geometry.
#' @param verbose Logical (default TRUE).
#'
#' @return Invisibly, a list with \code{plot} (ggplot), \code{data} (plotted long
#'   table), \code{summary} (per unit x metric: median, n origins, tally,
#'   n_gated), and \code{files} (written paths).
#'
#' @seealso \code{\link{plot_rolling_cv}}, \code{\link{evaluate_rolling_cv}},
#'   \code{\link{run_rolling_cv}}
#' @export
plot_forecast_cv_skill <- function(x,
                                   value          = c("R2_corr", "bias_ratio",
                                                      "R2_sse", "wis_skill", "mae_skill"),
                                   horizon_months = 6,
                                   baseline       = "seasonal",
                                   model          = "ensemble",
                                   metrics        = c("cases", "deaths"),
                                   show_gated     = TRUE,
                                   title          = NULL,
                                   subtitle       = NULL,
                                   caption        = NULL,
                                   dir_output     = NULL,
                                   file_prefix    = "forecast_cv",
                                   width          = 9,
                                   height         = NULL,
                                   dpi            = 300,
                                   base_size      = 14,
                                   verbose        = TRUE) {
     if (!requireNamespace("ggplot2", quietly = TRUE))
          stop("plot_forecast_cv_skill requires the 'ggplot2' package.")
     value <- match.arg(value)

     cells <- .fcs_read_cells(x)
     req <- c("model", "iso_code", "metric", "cutoff_date", "window")
     miss <- setdiff(req, names(cells))
     if (length(miss)) stop("cells is missing required column(s): ", paste(miss, collapse = ", "))
     if (is.null(cells$unit))        cells$unit <- cells$iso_code
     if (is.null(cells$exploratory)) cells$exploratory <- FALSE
     if (is.null(cells$ess_ok))      cells$ess_ok <- TRUE

     # resolve column, reference line, label, and "good" predicate from `value`
     is_skill <- value %in% c("wis_skill", "mae_skill")
     col   <- if (is_skill) paste0(value, "_", baseline) else value
     ref   <- if (is_skill) 0 else if (value == "bias_ratio") 1 else if (value == "R2_sse") 0 else NA_real_
     good  <- if (is_skill) function(v) v > 0
              else if (value == "bias_ratio") function(v) v >= 0.5 & v <= 2
              else function(v) rep(NA, length(v))   # R2 -> show median, no win/loss tally
     xlab  <- if (value == "R2_corr") "OOS R\u00b2 (corr, shape)"
              else if (value == "R2_sse") "OOS R\u00b2 (SSE, scale-aware)"
              else if (value == "bias_ratio") "OOS bias ratio (pred/obs)"
              else sprintf("OOS %s vs %s", toupper(sub("_skill", "-skill", value)),
                           if (baseline == "seasonal") "climatology" else baseline)
     if (!col %in% names(cells))
          stop("column '", col, "' not found. Available: ",
               paste(grep("_skill_|^R2_|^bias", names(cells), value = TRUE), collapse = ", "))
     win <- sprintf("OOS<=%gmo", horizon_months)
     if (!win %in% cells$window)
          stop("window '", win, "' not in data. Available: ",
               paste(grep("^OOS", unique(cells$window), value = TRUE), collapse = ", "))

     d <- cells[cells$model == model & cells$window == win & cells$metric %in% metrics, , drop = FALSE]
     d$val <- suppressWarnings(as.numeric(d[[col]]))
     d <- d[is.finite(d$val), , drop = FALSE]
     if (!nrow(d)) stop("no finite '", col, "' for model='", model, "', window='", win, "'.")
     if (!isTRUE(show_gated)) d <- d[d$ess_ok %in% TRUE, , drop = FALSE]
     d$metric <- factor(d$metric, levels = metrics)
     d$origin <- as.character(d$cutoff_date)

     # per unit x metric summary on ESS-passing origins (no CI at small n)
     dg <- d[d$ess_ok %in% TRUE, , drop = FALSE]
     parts <- split(dg, list(dg$unit, dg$metric), drop = TRUE)
     summ <- do.call(rbind, lapply(parts, function(g) {
          gd <- good(g$val)
          data.frame(unit = g$unit[1], metric = g$metric[1],
                     med_val = stats::median(g$val), n_origins = nrow(g),
                     n_good = if (all(is.na(gd))) NA_integer_ else sum(gd),
                     stringsAsFactors = FALSE)
     }))
     ng <- stats::aggregate(ess_ok ~ unit + metric, data = d, FUN = function(v) sum(!(v %in% TRUE)))
     summ <- merge(summ, data.frame(unit = ng$unit, metric = ng$metric, n_gated = ng$ess_ok),
                   by = c("unit", "metric"), all.x = TRUE)
     summ$metric <- factor(summ$metric, levels = metrics)
     summ$lab <- if (all(is.na(summ$n_good))) sprintf("med %.2f", summ$med_val)
                 else sprintf("%d/%d", summ$n_good, summ$n_origins)

     # unit labels: exploratory flagged + ordered last
     expl_units <- unique(d$unit[d$exploratory %in% TRUE])
     ulab <- function(u) ifelse(u %in% expl_units, paste0(u, " *"), u)
     unit_order <- c(sort(setdiff(unique(d$unit), expl_units)), sort(expl_units))
     d$unit_lab    <- factor(ulab(d$unit),    levels = ulab(unit_order))
     summ$unit_lab <- factor(ulab(summ$unit), levels = ulab(unit_order))

     if (is.null(title))    title <- sprintf("%s (%g-mo horizon)", xlab, horizon_months)
     if (is.null(subtitle)) subtitle <- "HINDCAST / conditional model performance (realized covariates) -- NOT operational forecast skill; points = forecast origins, no CI"
     if (is.null(caption))  caption <- sprintf("model=%s | diamond = per-country median over ESS-passing origins | annotation = %s | * exploratory",
                                               model, if (all(is.na(summ$n_good))) "median" else if (value=="bias_ratio") "origins in [0.5,2]" else "origins better than ref")

     gg <- ggplot2::ggplot(d, ggplot2::aes(x = val, y = unit_lab))
     if (!is.na(ref))
          gg <- gg + ggplot2::geom_vline(xintercept = ref, linetype = 2, linewidth = 0.4, colour = "#B5123B")
     gg <- gg +
          ggplot2::geom_point(ggplot2::aes(colour = origin, shape = ess_ok), size = 2.6, alpha = 0.9) +
          ggplot2::geom_point(data = summ, ggplot2::aes(x = med_val, y = unit_lab),
                              shape = 18, size = 4.2, colour = "black", inherit.aes = FALSE) +
          ggplot2::geom_text(data = summ, ggplot2::aes(x = -Inf, y = unit_lab, label = lab),
                             hjust = -0.2, vjust = 0.5, size = 3.0, colour = "grey30", inherit.aes = FALSE) +
          ggplot2::scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), name = "ESS pass", drop = FALSE) +
          ggplot2::facet_wrap(~ metric, ncol = 1, scales = "free_x") +
          ggplot2::labs(title = title, subtitle = subtitle, caption = caption,
                        x = xlab, y = NULL, colour = "Origin (cutoff)") +
          .fcs_theme(base_size)

     files <- character(0)
     if (!is.null(dir_output)) {
          dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)
          if (is.null(height)) height <- max(3.5, 1.1 * length(unit_order) * length(metrics) + 1.5)
          png <- file.path(dir_output, sprintf("%s_%s_%gmo.png", file_prefix, value, horizon_months))
          ggplot2::ggsave(png, gg, width = width, height = height, dpi = dpi)
          files <- png
          pdf <- sub("\\.png$", ".pdf", png)
          ok <- tryCatch({ ggplot2::ggsave(pdf, gg, width = width, height = height, device = grDevices::pdf); TRUE },
                         error = function(e) FALSE)
          if (ok) files <- c(files, pdf)
          if (verbose) message("plot_forecast_cv_skill: wrote ", length(files), " file(s) to ", dir_output)
     }
     invisible(list(plot = gg, data = d, summary = summ, files = files))
}

#' @keywords internal
#' @noRd
.fcs_read_cells <- function(x) {
     if (is.data.frame(x)) return(x)
     if (!is.character(x) || length(x) != 1L) stop("x must be a dir, a parquet/csv path, or a data frame.")
     path <- if (dir.exists(x)) file.path(x, "scores_cells.parquet") else x
     if (grepl("\\.parquet$", path)) {
          if (!file.exists(path)) path <- sub("\\.parquet$", ".csv", path)
     }
     if (!file.exists(path)) stop("scored cells not found at: ", path)
     if (grepl("\\.parquet$", path)) {
          if (!requireNamespace("arrow", quietly = TRUE))
               stop("reading a .parquet requires the 'arrow' package (or pass the .csv / data frame).")
          as.data.frame(arrow::read_parquet(path))
     } else utils::read.csv(path, stringsAsFactors = FALSE)
}

#' @keywords internal
#' @noRd
.fcs_theme <- function(base_size) {
     if (exists("theme_mosaic", where = asNamespace("MOSAIC"), inherits = FALSE))
          return(get("theme_mosaic", asNamespace("MOSAIC"))(base_size = base_size))
     ggplot2::theme_minimal(base_size = base_size) +
          ggplot2::theme(legend.position = "top",
                         panel.grid.minor = ggplot2::element_blank())
}
