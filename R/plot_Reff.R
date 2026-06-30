# -----------------------------------------------------------------------------
# Plot the Cori effective reproductive number (R_eff) over time
# -----------------------------------------------------------------------------
# Renders the per-location, phase-coherent R_eff(t) estimand produced by
# calc_Reff(): the headline line is the MEDOID trajectory R_t (`central`), which
# preserves the timing/height of the epidemic's R_t peak, with a reference line
# at R_eff = 1. A FAINT 95% band (q2.5-q97.5) shows the per-calendar-date
# posterior range ACROSS members; because member peaks are phase-misaligned this
# band does NOT represent the epidemic's peak R_t (a per-calendar-day median of
# offset peaks flattens toward 1). The explosivity of the epidemic is instead
# summarized by the per-member peak R_t annotation (attr `peak_Rt`): the
# posterior-weighted quantiles of each member's TIME-MAX R_t. Burn-in is excluded
# upstream as leading NA `central`; those rows are trimmed cleanly here.
# -----------------------------------------------------------------------------

#' Plot Cori effective reproductive number (R_eff) over time
#'
#' Renders the per-location, time-varying Cori (2013) instantaneous
#' \strong{infection} effective reproductive number \eqn{R_{jt}} produced by
#' \code{\link{calc_Reff}}. The headline line is the \strong{medoid trajectory}
#' R_t (\code{central}) drawn as a bold purple line over date, with a horizontal
#' reference line at \eqn{R_{\mathrm{eff}} = 1}. Because the medoid is a single
#' coherent member trajectory it preserves the timing and height of the
#' epidemic's R_t peak (typically 2-3.3), unlike a per-calendar-day cross-member
#' median which flattens phase-misaligned peaks toward 1.
#'
#' \strong{Faint posterior band.} The 95\% ribbon (\code{q2.5}-\code{q97.5}) is
#' the per-calendar-date posterior range \emph{across members} and is rendered
#' faintly. The caption makes explicit that this band does \emph{not} represent
#' the epidemic's peak R_t (member peaks are phase-misaligned in calendar time);
#' that explosivity statistic is shown by the medoid line and the per-member peak
#' R_t annotation. The inner 50\% band (\code{q25}-\code{q75}) is drawn only when
#' \code{show_iqr = TRUE} and those columns are populated.
#'
#' \strong{Per-member peak R_t annotation.} When the input carries an
#' \code{attr(reff, "peak_Rt")} data.frame (per-location posterior-weighted
#' \code{q2.5}/\code{q50}/\code{q97.5} of each member's time-max R_t), the plot
#' annotates it: for a single location in the subtitle, for multi-location input
#' as a per-facet in-panel label. Older artifacts that lack the attribute are
#' handled gracefully (the annotation is simply omitted).
#'
#' \strong{Graceful CI handling.} If the quantile columns are absent or all
#' \code{NA} (e.g. an older artifact with strided trajectory lines, attr
#' \code{ci_source = "unavailable_strided_lines"}), the band is omitted (medoid
#' line only) and the caption notes the missing CI. The function never errors
#' when the band or peak annotation is missing.
#'
#' @param reff A \code{reproductive_numbers} \code{data.frame} from
#'   \code{\link{calc_Reff}} with columns \code{location}, \code{date}, \code{t},
#'   \code{estimand}, \code{central} (the medoid trajectory R_t), and the
#'   per-calendar-date quantile columns (\code{q2.5}, \code{q25}, \code{q50},
#'   \code{q75}, \code{q97.5}). The \code{peak_Rt}, \code{central_definition},
#'   \code{band_definition} and \code{ci_source} attributes (when present) drive
#'   the annotation and caption. Leading warm-up rows with a non-finite
#'   \code{central} are dropped per location so no gap artifact is plotted.
#' @param show_iqr Logical. Draw the inner 50\% (\code{q25}-\code{q75}) band in
#'   addition to the faint 95\% band. Default \code{FALSE} (the plot shows the
#'   medoid line and the faint 95\% cross-member band only). Retained for
#'   back-compatibility.
#' @param title Character or \code{NULL}. Plot title. \code{NULL} (default) uses
#'   \code{"Effective reproductive number"} for multi-location input and
#'   \code{"Effective reproductive number: <LOC>"} for a single location.
#' @param ncol Integer. Number of facet columns for multi-location input.
#'   \code{NULL} (default) uses \code{min(3, n_locations)}.
#' @param base_size Numeric. Base font size passed to \code{\link{theme_mosaic}}.
#'   Default \code{12}.
#'
#' @return A \code{ggplot} object (not printed or saved). The driver
#'   \code{\link{add_reproductive_numbers}} is responsible for saving it.
#'
#' @seealso \code{\link{calc_Reff}} to compute the series;
#'   \code{\link{add_reproductive_numbers}} to apply both to an output directory.
#'
#' @references Cori A, Ferguson NM, Fraser C, Cauchemez S (2013). A new framework
#'   and software to estimate time-varying reproduction numbers during epidemics.
#'   American Journal of Epidemiology 178(9):1505-1512.
#'
#' @examples
#' \dontrun{
#' tr  <- readRDS("2_calibration/trajectories_ensemble.rds")
#' cfg <- jsonlite::fromJSON("1_inputs/config.json")
#' reff <- calc_Reff(tr, cfg)
#' p <- plot_Reff(reff)
#' print(p)
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_hline geom_line geom_text
#'   facet_wrap scale_x_date scale_y_continuous labs
plot_Reff <- function(reff,
                      show_iqr  = FALSE,
                      title     = NULL,
                      ncol      = NULL,
                      base_size = 12) {

  # ---------------------------------------------------------------------------
  # Validate input
  # ---------------------------------------------------------------------------
  if (!is.data.frame(reff))
    stop("plot_Reff: `reff` must be a data.frame from calc_Reff().")
  req <- c("location", "date", "central")
  miss <- setdiff(req, names(reff))
  if (length(miss))
    stop("plot_Reff: `reff` is missing required column(s): ",
         paste(miss, collapse = ", "), ".")
  if (nrow(reff) == 0L)
    stop("plot_Reff: `reff` has zero rows.")

  ci_source <- attr(reff, "ci_source")
  peak_Rt   <- attr(reff, "peak_Rt")

  # ---------------------------------------------------------------------------
  # Drop leading warm-up rows (non-finite central) per location so the plotted
  # line starts where R_eff first becomes defined (no gap artifact). Interior
  # non-finite values (deep-trough floor gating) are retained as NA so ggplot
  # breaks the line there rather than interpolating across a real gap.
  # ---------------------------------------------------------------------------
  pd <- reff
  pd$date <- as.Date(pd$date)
  pd <- do.call(rbind, lapply(split(pd, pd$location), function(d) {
    d <- d[order(d$date), , drop = FALSE]
    first_ok <- which(is.finite(d$central))
    if (length(first_ok) == 0L) return(d[0, , drop = FALSE])
    d[seq.int(first_ok[1L], nrow(d)), , drop = FALSE]
  }))
  if (is.null(pd) || nrow(pd) == 0L)
    stop("plot_Reff: no finite `central` values to plot (all warm-up/NA).")
  rownames(pd) <- NULL

  use_date_axis <- inherits(pd$date, "Date") && any(is.finite(pd$date))
  locs <- unique(as.character(pd$location))
  n_loc <- length(locs)

  # ---------------------------------------------------------------------------
  # Decide which bands are drawable (columns present AND not all-NA)
  # ---------------------------------------------------------------------------
  .has_ci <- function(lo, hi) {
    all(c(lo, hi) %in% names(pd)) &&
      any(is.finite(pd[[lo]])) && any(is.finite(pd[[hi]]))
  }
  draw_outer <- .has_ci("q2.5", "q97.5")
  draw_inner <- isTRUE(show_iqr) && .has_ci("q25", "q75")

  reff_color  <- "#762A83"   # purple (ColorBrewer PRGn dark purple)
  ref_color   <- unname(mosaic_colors("reference"))

  # ---------------------------------------------------------------------------
  # Build plot. Headline = the coherent medoid trajectory R_t (`central`).
  # ---------------------------------------------------------------------------
  p <- ggplot2::ggplot(pd, ggplot2::aes(x = .data$date))

  if (draw_outer) {
    # FAINT: this is the per-calendar-date cross-member range, NOT the peak.
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$q2.5, ymax = .data$q97.5),
      fill = mosaic_color_variant(reff_color, "lighten", 0.65), alpha = 0.22)
  }
  if (draw_inner) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$q25, ymax = .data$q75),
      fill = mosaic_color_variant(reff_color, "lighten", 0.45), alpha = 0.25)
  }

  p <- p +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed",
                        color = ref_color, linewidth = 0.6) +
    ggplot2::geom_line(ggplot2::aes(y = .data$central),
                       color = reff_color, linewidth = 1.0, na.rm = TRUE)

  if (n_loc > 1L) {
    nc <- if (is.null(ncol)) min(3L, n_loc) else as.integer(ncol)
    p <- p + ggplot2::facet_wrap(~ location, scales = "free_y", ncol = nc)
  }

  # ---------------------------------------------------------------------------
  # Per-member peak R_t annotation (attr `peak_Rt`). Format:
  #   "Peak R_t (per-member): q50 [q2.5, q97.5]"
  # Single location -> subtitle; multi-location -> per-facet in-panel label.
  # Gracefully omitted when peak_Rt is absent or has no usable rows.
  # ---------------------------------------------------------------------------
  subtitle <- NULL
  pk <- .reff_peak_table(peak_Rt, locs)
  if (!is.null(pk) && nrow(pk) > 0L) {
    pk$label <- sprintf("Peak R_t (per-member): %.2f [%.2f, %.2f]",
                        pk$q50, pk$q2.5, pk$q97.5)
    if (n_loc == 1L) {
      subtitle <- pk$label[match(locs[1L], pk$location)]
    } else {
      # In-panel label, top-left of each facet (free_y), at the first date.
      pk_panel <- pk[pk$location %in% locs, , drop = FALSE]
      if (nrow(pk_panel) > 0L) {
        x0 <- min(pd$date, na.rm = TRUE)
        ann <- do.call(rbind, lapply(split(pd, pd$location), function(d) {
          loc <- as.character(d$location[1L])
          row <- pk_panel[match(loc, pk_panel$location), , drop = FALSE]
          if (nrow(row) == 0L || is.na(row$label)) return(NULL)
          yv <- suppressWarnings(max(c(d$central, d$q97.5), na.rm = TRUE))
          if (!is.finite(yv)) yv <- suppressWarnings(max(d$central, na.rm = TRUE))
          data.frame(location = loc, date = x0, y = yv, label = row$label,
                     stringsAsFactors = FALSE)
        }))
        if (!is.null(ann) && nrow(ann) > 0L) {
          ann$date <- as.Date(ann$date, origin = "1970-01-01")
          p <- p + ggplot2::geom_text(
            data = ann,
            ggplot2::aes(x = .data$date, y = .data$y, label = .data$label),
            hjust = 0, vjust = 1.2, size = base_size * 0.22,
            color = reff_color, inherit.aes = FALSE, na.rm = TRUE)
        }
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Caption / title
  # ---------------------------------------------------------------------------
  band_note <- if (draw_outer) {
    paste0("Faint band: 95%", if (draw_inner) " (and 50%)" else "",
           " posterior range ACROSS members at each calendar date — it does",
           " NOT show the epidemic's peak R_t (member peaks are phase-",
           "misaligned, so a per-date range flattens them toward 1). The peak",
           " R_t is shown by the medoid line and the per-member peak annotation.")
  } else if (identical(ci_source, "unavailable_strided_lines")) {
    paste0("Posterior band unavailable for this artifact (per-member trajectory ",
           "lines are time-strided); medoid series shown.")
  } else if (identical(ci_source, "unavailable_no_incidence_lines")) {
    paste0("Posterior band unavailable for this artifact (no per-member ",
           "incidence lines); medoid series shown.")
  } else {
    "Medoid series shown; posterior band unavailable."
  }
  caption <- paste0(
    "Headline line: medoid-trajectory Cori R_eff on simulated infection ",
    "incidence (a coherent member trajectory that preserves peak timing/height;",
    " a descriptor of the model trajectory, not a first-principles R0). Dashed",
    " line: R_eff = 1.\n", band_note)

  if (is.null(title)) {
    title <- if (n_loc == 1L)
      paste0("Effective reproductive number: ", locs[1L]) else
        "Effective reproductive number"
  }

  p <- p +
    ggplot2::scale_y_continuous() +
    theme_mosaic(base_size = base_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      x = if (use_date_axis) "Date" else "Time",
      y = expression(R[eff](t)),
      title = title,
      subtitle = subtitle,
      caption = caption)

  if (use_date_axis)
    p <- p + ggplot2::scale_x_date()

  p
}

# -----------------------------------------------------------------------------
# Internal: normalize the `peak_Rt` attribute into a per-location data.frame
# with finite q2.5/q50/q97.5 rows for the locations being plotted. Returns NULL
# when the attribute is absent, malformed, or has no usable rows (so the caller
# omits the annotation rather than erroring).
# -----------------------------------------------------------------------------
.reff_peak_table <- function(peak_Rt, locs) {
  if (is.null(peak_Rt) || !is.data.frame(peak_Rt) || nrow(peak_Rt) == 0L)
    return(NULL)
  need <- c("location", "q2.5", "q50", "q97.5")
  if (!all(need %in% names(peak_Rt))) return(NULL)
  pk <- peak_Rt[, need, drop = FALSE]
  pk$location <- as.character(pk$location)
  pk <- pk[pk$location %in% locs, , drop = FALSE]
  pk <- pk[is.finite(pk$q50) & is.finite(pk$q2.5) & is.finite(pk$q97.5), ,
           drop = FALSE]
  if (nrow(pk) == 0L) return(NULL)
  rownames(pk) <- NULL
  pk
}
