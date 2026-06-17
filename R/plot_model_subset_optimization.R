# =============================================================================
# plot_model_subset_optimization.R
#
# Diagnostic plot for the post-ensemble best-subset optimization step. The
# optimizer (optimize_ensemble_subset) evaluates top-N subsets ranked by
# likelihood and selects the N that maximizes prediction quality; run_MOSAIC()
# writes its evaluation table to optimization_diagnostics.csv but renders no
# figure. This function fills that gap, drawing the objective score, R^2, bias
# ratio, and ESS as functions of subset size N, with the preliminary (tier)
# size and the optimized (argmax-score) size marked.
# =============================================================================

#' Plot Best-Subset Optimization Diagnostic from a mosaic_subset_optimization Object
#'
#' @description
#' Renders a four-panel diagnostic from a \code{mosaic_subset_optimization}
#' object produced by \code{\link{optimize_ensemble_subset}}. Each panel plots a
#' diagnostic against the candidate subset size \code{N} (top-N members by
#' likelihood): (1) the objective score, (2) \eqn{R^2} for cases and deaths,
#' (3) the bias ratio for cases and deaths (with a dotted reference line at
#' bias = 1), and (4) the effective sample size (ESS). The preliminary
#' (tier/diagnostics) subset size is marked with a dashed grey vertical line and
#' the optimizer-selected size with a solid black vertical line plus a point on
#' the score panel.
#'
#' @param subset_opt A \code{mosaic_subset_optimization} object returned by
#'   \code{\link{optimize_ensemble_subset}}. Its \code{$evaluation_table} carries
#'   the per-N diagnostics; the scalar fields \code{$optimal_n},
#'   \code{$diagnostics_n}, \code{$objective}, and \code{$central_method} drive
#'   the markers and labels. The object is consumed directly; the diagnostics are
#'   \strong{not} re-derived from the on-disk CSV.
#' @param output_dir Character. Directory where plots are saved. Created if it
#'   does not exist.
#' @param file_prefix Character. Prefix used in output filenames:
#'   \code{<file_prefix>_diagnostic.pdf} and \code{<file_prefix>_diagnostic.png}.
#'   Default \code{"subset_optimization"}.
#' @param title_label Character or \code{NULL}. Leading label used in the plot
#'   title. When \code{NULL} (default) a title derived from the objective and
#'   central method is used.
#' @param verbose Logical. Print progress messages. Default \code{TRUE}.
#'
#' @return Invisibly returns the \code{ggplot} object.
#'
#' @details
#' The PDF is rendered with \code{grDevices::cairo_pdf} when the cairo device is
#' available (\code{capabilities("cairo")}), so the proper glyphs \eqn{R^2},
#' \eqn{\rightarrow}, and \eqn{\Delta} are encoded correctly. When cairo is
#' unavailable the function falls back to the default PDF device with
#' ASCII-safe labels (\code{R2}, \code{->}, \code{delta}). The PNG is always
#' rendered with the standard raster device.
#'
#' Semantic colors for the cases and deaths series come from
#' \code{\link{mosaic_colors}} and the panel theming from
#' \code{\link{theme_mosaic}}, both with graceful fallbacks if unavailable.
#'
#' @seealso \code{\link{optimize_ensemble_subset}} to compute the optimization;
#'   \code{\link{plot_model_ensemble}} for the ensemble prediction plots.
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_vline geom_hline geom_line geom_point
#'   facet_wrap scale_color_manual labs theme element_text theme_minimal ggsave
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' subset_opt <- optimize_ensemble_subset(ensemble, likelihoods)
#' plot_model_subset_optimization(subset_opt, output_dir = "diagnostics")
#' }
plot_model_subset_optimization <- function(subset_opt,
                                           output_dir,
                                           file_prefix = "subset_optimization",
                                           title_label = NULL,
                                           verbose     = TRUE) {

  # ---------------------------------------------------------------------------
  # Validate inputs
  # ---------------------------------------------------------------------------

  if (!inherits(subset_opt, "mosaic_subset_optimization"))
    stop("subset_opt must be a mosaic_subset_optimization object from optimize_ensemble_subset()",
         call. = FALSE)

  if (missing(output_dir) || is.null(output_dir))
    stop("output_dir is required", call. = FALSE)

  ev <- subset_opt$evaluation_table
  if (!is.data.frame(ev) || nrow(ev) == 0L)
    stop("subset_opt$evaluation_table must be a non-empty data frame", call. = FALSE)

  required_cols <- c("n", "r2_cases", "r2_deaths", "bias_cases", "bias_deaths",
                     "ess", "score")
  missing_cols <- setdiff(required_cols, names(ev))
  if (length(missing_cols) > 0L)
    stop(sprintf("subset_opt$evaluation_table is missing required columns: %s",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)

  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # ---------------------------------------------------------------------------
  # Resolve markers and labels
  # ---------------------------------------------------------------------------

  optimal_n     <- as.integer(subset_opt$optimal_n)
  diagnostics_n <- as.integer(subset_opt$diagnostics_n)
  objective     <- subset_opt$objective %||% "mae"
  cm            <- subset_opt$central_method
  central_lab   <- if (is.null(cm)) {
    "median"
  } else if (length(cm) > 1L) {
    paste0("cases=", cm[["cases"]], "/deaths=", cm[["deaths"]])
  } else {
    as.character(cm)
  }

  # Whether cairo can ACTUALLY render. capabilities("cairo") is necessary but
  # not sufficient -- on some macOS installs the cairo DLL fails to load at draw
  # time (missing XQuartz libXrender), so probe by opening a throwaway device.
  use_cairo <- .mosaic_cairo_pdf_works()

  # Semantic colors with graceful fallback.
  series_cols <- tryCatch({
    sc <- mosaic_colors("cases", "deaths")
    c(cases = unname(sc[["cases"]]), deaths = unname(sc[["deaths"]]),
      score = "#238b45", ess = "#756bb1")
  }, error = function(e) {
    c(cases = "#0167AF", deaths = "#B5123B", score = "#238b45", ess = "#756bb1")
  })

  theme_obj <- tryCatch(theme_mosaic(), error = function(e) theme_minimal())

  base_title <- if (is.null(title_label)) {
    "Best-subset optimization diagnostic"
  } else {
    title_label
  }

  # Build the ggplot for a given glyph mode. Proper glyphs (R^2, ->, delta) need
  # cairo in the PDF; ASCII-safe labels are used otherwise (and the default PDF
  # device cannot encode the Unicode glyphs at all). The PNG raster device
  # handles Unicode regardless, so we always pass it the glyph build.
  .build_plot <- function(glyphs = TRUE) {
    glyph_r2    <- if (glyphs) "R²"    else "R2"
    glyph_arrow <- if (glyphs) "→"     else "->"
    glyph_delta <- if (glyphs) "Δ"     else "delta"

    panel_levels <- c(
      sprintf("Objective score (%s)", objective),
      glyph_r2, "Bias ratio", "ESS"
    )

    long <- rbind(
      data.frame(n = ev$n, panel = panel_levels[1], series = "score",  value = ev$score),
      data.frame(n = ev$n, panel = panel_levels[2], series = "cases",  value = ev$r2_cases),
      data.frame(n = ev$n, panel = panel_levels[2], series = "deaths", value = ev$r2_deaths),
      data.frame(n = ev$n, panel = panel_levels[3], series = "cases",  value = ev$bias_cases),
      data.frame(n = ev$n, panel = panel_levels[3], series = "deaths", value = ev$bias_deaths),
      data.frame(n = ev$n, panel = panel_levels[4], series = "ess",    value = ev$ess),
      stringsAsFactors = FALSE
    )
    long$panel <- factor(long$panel, levels = panel_levels)

    # bias = 1 reference line on the Bias panel only.
    hl_bias <- data.frame(panel = factor("Bias ratio", levels = panel_levels), yint = 1)
    # Optimal marker point on the score panel.
    pt_opt  <- data.frame(panel = factor(panel_levels[1], levels = panel_levels),
                          n = optimal_n,
                          value = ev$score[which.max(ev$score)])

    subtitle <- sprintf(
      "central = %s   |   preliminary (tier) N = %d [dashed]  %s  optimized argmax-score N = %d [solid]   (%s = %d members)",
      central_lab, diagnostics_n, glyph_arrow, optimal_n, glyph_delta,
      diagnostics_n - optimal_n
    )

    ggplot(long, aes(x = .data$n, y = .data$value, color = .data$series)) +
      geom_vline(xintercept = diagnostics_n, linetype = "dashed",
                 color = "grey50", linewidth = 0.4) +
      geom_vline(xintercept = optimal_n, linetype = "solid",
                 color = "black", linewidth = 0.4) +
      geom_hline(data = hl_bias, aes(yintercept = .data$yint),
                 linetype = "dotted", color = "grey40", inherit.aes = FALSE) +
      geom_line(linewidth = 0.7) +
      geom_point(data = pt_opt, aes(x = .data$n, y = .data$value),
                 inherit.aes = FALSE, color = "black", size = 2) +
      facet_wrap(~ panel, scales = "free_y", ncol = 2) +
      scale_color_manual(values = series_cols, name = NULL) +
      labs(
        title    = base_title,
        subtitle = subtitle,
        x        = "Subset size N (top-N by likelihood)",
        y        = NULL
      ) +
      theme_obj +
      theme(legend.position = "bottom")
  }

  # Glyph build is returned and used for the PNG; the PDF uses it only when cairo
  # works, else an ASCII build on the default device.
  p_glyph <- .build_plot(glyphs = TRUE)

  # ---------------------------------------------------------------------------
  # Save outputs
  # ---------------------------------------------------------------------------

  out_pdf <- file.path(output_dir, paste0(file_prefix, "_diagnostic.pdf"))
  out_png <- file.path(output_dir, paste0(file_prefix, "_diagnostic.png"))

  pdf_ok <- FALSE
  if (use_cairo) {
    pdf_ok <- tryCatch({
      ggsave(out_pdf, p_glyph, width = 10, height = 7, device = grDevices::cairo_pdf)
      TRUE
    }, error = function(e) FALSE)
  }
  if (!pdf_ok) {
    # Fall back to the default PDF device with ASCII-safe labels.
    ggsave(out_pdf, .build_plot(glyphs = FALSE), width = 10, height = 7)
  }

  ggsave(out_png, p_glyph, width = 10, height = 7, dpi = 150)

  if (verbose) {
    message(sprintf("Saved subset optimization diagnostic:\n  %s\n  %s",
                    out_pdf, out_png))
  }

  invisible(p_glyph)
}


# ── Internal: probe whether cairo_pdf can actually open a device ────────────
# capabilities("cairo") only reports build-time support; on some macOS installs
# the cairo DLL fails to dlopen at draw time (missing XQuartz libXrender). Open a
# throwaway cairo_pdf to a tempfile and report whether it succeeds.
# @keywords internal
.mosaic_cairo_pdf_works <- function() {
  if (!isTRUE(tryCatch(capabilities("cairo"), error = function(e) FALSE)))
    return(FALSE)
  tf <- tempfile(fileext = ".pdf")
  on.exit(unlink(tf), add = TRUE)
  ok <- tryCatch({
    grDevices::cairo_pdf(tf, width = 1, height = 1)
    grDevices::dev.off()
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  isTRUE(ok)
}
