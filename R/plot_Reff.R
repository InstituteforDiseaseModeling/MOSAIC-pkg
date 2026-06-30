# -----------------------------------------------------------------------------
# Plot the Cori effective reproductive number (R_eff) over time
# -----------------------------------------------------------------------------
# Renders the per-location R_eff(t) series produced by calc_Reff(): a central
# (medoid) line over date with a reference line at R_eff = 1, and a posterior
# credible-interval ribbon WHEN it is available. On production-default artifacts
# the per-member trajectory lines are time-strided, so calc_Reff() returns the
# q* columns as all-NA with attr ci_source = "unavailable_strided_lines"; in
# that case this function draws just the central line and notes the missing CI
# in the caption (it never errors on an all-NA CI).
# -----------------------------------------------------------------------------

#' Plot Cori effective reproductive number (R_eff) over time
#'
#' Renders the per-location, time-varying Cori (2013) instantaneous
#' \strong{infection} effective reproductive number \eqn{R_{jt}} produced by
#' \code{\link{calc_Reff}}. Draws the \code{central} (medoid) series as a line
#' over date with a horizontal reference line at \eqn{R_{\mathrm{eff}} = 1}; when
#' the posterior credible-interval columns are populated it overlays the
#' 95\% (\code{q2.5}-\code{q97.5}) and optionally the 50\% (\code{q25}-\code{q75})
#' ribbons. Multi-location input is faceted by location; a single (national)
#' location is rendered as one panel titled with the location.
#'
#' \strong{Graceful CI handling.} On production-default trajectory artifacts the
#' per-member \code{lines} are time-strided, so \code{calc_Reff()} cannot compute
#' a daily renewal CI and returns the \code{q*} columns as all-\code{NA} with
#' \code{attr(reff, "ci_source") = "unavailable_strided_lines"}. This function
#' detects an all-\code{NA} CI per ribbon level and simply omits that ribbon
#' (drawing the central line only), adding a caption noting the CI is unavailable
#' for the artifact. It never errors when the CI is missing.
#'
#' @param reff A \code{reproductive_numbers} \code{data.frame} from
#'   \code{\link{calc_Reff}} with columns \code{location}, \code{date}, \code{t},
#'   \code{estimand}, \code{central}, and the quantile columns (\code{q2.5},
#'   \code{q25}, \code{q50}, \code{q75}, \code{q97.5}). The \code{ci_source}
#'   attribute (if present) is used for the caption. Leading warm-up rows with a
#'   non-finite \code{central} are dropped per location so no gap artifact is
#'   plotted.
#' @param show_iqr Logical. Draw the inner 50\% (\code{q25}-\code{q75}) ribbon in
#'   addition to the 95\% ribbon when those columns are populated. Default
#'   \code{TRUE}.
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
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_hline geom_line facet_wrap
#'   scale_x_date scale_y_continuous labs
plot_Reff <- function(reff,
                      show_iqr  = TRUE,
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
  # Decide which ribbons are drawable (columns present AND not all-NA)
  # ---------------------------------------------------------------------------
  .has_ci <- function(lo, hi) {
    all(c(lo, hi) %in% names(pd)) &&
      any(is.finite(pd[[lo]])) && any(is.finite(pd[[hi]]))
  }
  draw_outer <- .has_ci("q2.5", "q97.5")
  draw_inner <- isTRUE(show_iqr) && .has_ci("q25", "q75")

  reff_color  <- unname(mosaic_colors("cases"))
  ref_color   <- unname(mosaic_colors("reference"))

  # ---------------------------------------------------------------------------
  # Build plot
  # ---------------------------------------------------------------------------
  p <- ggplot2::ggplot(pd, ggplot2::aes(x = .data$date))

  if (draw_outer) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$q2.5, ymax = .data$q97.5),
      fill = mosaic_color_variant(reff_color, "lighten", 0.55), alpha = 0.5)
  }
  if (draw_inner) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$q25, ymax = .data$q75),
      fill = mosaic_color_variant(reff_color, "lighten", 0.30), alpha = 0.5)
  }

  p <- p +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed",
                        color = ref_color, linewidth = 0.6) +
    ggplot2::geom_line(ggplot2::aes(y = .data$central),
                       color = reff_color, linewidth = 0.7, na.rm = TRUE)

  if (n_loc > 1L) {
    nc <- if (is.null(ncol)) min(3L, n_loc) else as.integer(ncol)
    p <- p + ggplot2::facet_wrap(~ location, scales = "free_y", ncol = nc)
  }

  # ---------------------------------------------------------------------------
  # Caption / title
  # ---------------------------------------------------------------------------
  ci_note <- if (draw_outer) {
    paste0("Ribbon: 95%",
           if (draw_inner) " and 50%" else "",
           " posterior credible interval")
  } else if (identical(ci_source, "unavailable_strided_lines")) {
    paste0("Posterior CI unavailable for this artifact (per-member trajectory ",
           "lines are time-strided); central (medoid) series shown.")
  } else if (identical(ci_source, "unavailable_no_incidence_lines")) {
    paste0("Posterior CI unavailable for this artifact (no per-member incidence ",
           "lines); central (medoid) series shown.")
  } else {
    "Central (medoid) series shown; posterior CI unavailable."
  }
  caption <- paste0(
    "Cori R_eff on simulated infection incidence (descriptor of the model ",
    "trajectory, not a first-principles R0). Dashed line: R_eff = 1.\n", ci_note)

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
      caption = caption)

  if (use_date_axis)
    p <- p + ggplot2::scale_x_date()

  p
}
