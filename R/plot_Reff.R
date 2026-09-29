# -----------------------------------------------------------------------------
# Plot the route-decomposed effective reproductive number over time
# -----------------------------------------------------------------------------
# Environmental R as a filled area from zero, human-to-human R stacked on top,
# total R_eff (their sum) as the purple upper edge, reference line at R = 1.
# The faint per-date cross-member band (when present) is not the peak R_t;
# the per-member peak statistic (attr `peak_Rt`) is annotated instead.
# -----------------------------------------------------------------------------

#' Plot the route-decomposed effective reproductive number over time
#'
#' Renders the per-location R_eff(t) produced by \code{\link{calc_Reff}} or
#' \code{\link{add_reproductive_numbers}}. When the input carries the route
#' components (\code{estimand} \code{"R_hum"} and \code{"R_env"}) and
#' \code{routes = TRUE}, the environmental reproductive number is drawn as a
#' filled area from zero and the human-to-human contribution is stacked on top of
#' it, so the upper edge is the total \eqn{R_{eff} = R_{env} + R_{hum}} (purple
#' line) and the height of the orange band is how much human transmission adds.
#' A dashed reference line marks \eqn{R_{eff} = 1}. Without route rows (older
#' artifacts) the total alone is drawn.
#'
#' \strong{Headline series.} On the re-simulation path \code{central} is the
#' MEDOID trajectory's R_t (a coherent member, preserving peak timing and
#' height); on the direct path it is the renewal on weighted-median incidence.
#' The daily series is noisy, so each component is shown as a centered
#' \code{smooth_days} rolling mean, taken over the days on which the total is
#' defined (a silent route counts as 0 there, as in \code{calc_Reff()}) so the
#' smoothed stack still sums to the smoothed total, with the raw daily total as
#' a faint background line.
#'
#' \strong{Faint band.} When populated, the \code{q2.5}-\code{q97.5} total-R band
#' is the per-calendar-date range across members. Member peaks are
#' phase-misaligned, so it does not show the epidemic's peak R_t; the per-member
#' peak statistic (attr \code{peak_Rt}) is annotated instead.
#'
#' @param reff A \code{reproductive_numbers} data.frame with columns
#'   \code{location}, \code{date}, \code{central}, optionally \code{estimand}
#'   and the quantile columns. Leading rows with a non-finite total are dropped
#'   per location.
#' @param show_iqr Logical. Also draw the inner 50% (\code{q25}-\code{q75})
#'   total-R band. Default \code{FALSE}.
#' @param smooth_days Integer. Centered rolling-mean window (days) for the
#'   displayed series; \code{1} plots the raw daily values. Default \code{14}.
#' @param title Character or \code{NULL} for the default title.
#' @param ncol Integer facet columns for multi-location input (\code{NULL}:
#'   \code{min(3, n_locations)}).
#' @param base_size Numeric base font size for \code{\link{theme_mosaic}}.
#' @param routes Logical. Stack \code{R_env} and \code{R_hum} under the total
#'   when present. Default \code{TRUE}.
#'
#' @return A \code{ggplot} object (not printed or saved).
#'
#' @seealso \code{\link{calc_Reff}}, \code{\link{add_reproductive_numbers}}.
#'
#' @examples
#' \dontrun{
#' tr  <- readRDS("2_calibration/trajectories_ensemble.rds")
#' cfg <- jsonlite::fromJSON("1_inputs/config.json")
#' print(plot_Reff(calc_Reff(tr, cfg)))
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_hline geom_line geom_text
#'   facet_wrap scale_x_date scale_y_continuous labs scale_fill_manual
#'   scale_colour_manual guides guide_legend
plot_Reff <- function(reff,
                      show_iqr    = FALSE,
                      smooth_days = 14L,
                      title       = NULL,
                      ncol        = NULL,
                      base_size   = 12,
                      routes      = TRUE) {

  if (!is.data.frame(reff))
    stop("plot_Reff: `reff` must be a data.frame from calc_Reff().")
  miss <- setdiff(c("location", "date", "central"), names(reff))
  if (length(miss))
    stop("plot_Reff: `reff` is missing required column(s): ",
         paste(miss, collapse = ", "), ".")
  if (nrow(reff) == 0L)
    stop("plot_Reff: `reff` has zero rows.")

  ci_source <- attr(reff, "ci_source")
  peak_Rt   <- attr(reff, "peak_Rt")

  est <- if ("estimand" %in% names(reff)) as.character(reff$estimand) else
    rep("R_eff", nrow(reff))
  tot <- reff[est == "R_eff", , drop = FALSE]
  if (nrow(tot) == 0L)
    stop("plot_Reff: `reff` has no R_eff rows.")
  has_routes <- isTRUE(routes) && all(c("R_hum", "R_env") %in% est)
  comp <- function(e) {
    d <- reff[est == e, c("location", "t", "central"), drop = FALSE]
    stats::setNames(d, c("location", "t", e))
  }

  sd_k <- suppressWarnings(as.integer(smooth_days))
  if (length(sd_k) != 1L || is.na(sd_k) || sd_k < 1L) sd_k <- 1L

  pd <- tot
  pd$date <- as.Date(pd$date)
  if (!"t" %in% names(pd))
    pd$t <- stats::ave(as.numeric(pd$date), pd$location, FUN = seq_along)
  if (has_routes)
    pd <- merge(merge(pd, comp("R_hum"), by = c("location", "t"), all.x = TRUE),
                comp("R_env"), by = c("location", "t"), all.x = TRUE)

  # Trim leading warm-up (non-finite total) per location; interior NA stays NA
  # so lines and areas break at real gaps.
  pd <- do.call(rbind, lapply(split(pd, pd$location), function(d) {
    d <- d[order(d$date), , drop = FALSE]
    first_ok <- which(is.finite(d$central))
    if (length(first_ok) == 0L) return(d[0, , drop = FALSE])
    d <- d[seq.int(first_ok[1L], nrow(d)), , drop = FALSE]
    d$central_smooth <- .reff_roll_mean(d$central, sd_k)
    if (has_routes) {
      # Stack on every day the total is defined. calc_Reff() defines the total
      # when a route below the floor has no infections of its own, and counts
      # that route as 0, so the same 0 is used here and the stack still sums to
      # the total. Both components are smoothed over the SAME days, or the
      # NA-skipping means use different day sets and the stack stops summing.
      raw_tot <- is.finite(d$central)
      env_s <- .reff_roll_mean(ifelse(raw_tot,
                                      ifelse(is.finite(d$R_env), d$R_env, 0),
                                      NA_real_), sd_k)
      hum_s <- .reff_roll_mean(ifelse(raw_tot,
                                      ifelse(is.finite(d$R_hum), d$R_hum, 0),
                                      NA_real_), sd_k)
      both  <- is.finite(env_s) & is.finite(hum_s)
      d$env_top <- ifelse(both, env_s, NA_real_)
      d$tot_top <- ifelse(both, env_s + hum_s, NA_real_)
      d$zero    <- ifelse(both, 0, NA_real_)
    }
    d
  }))
  if (is.null(pd) || nrow(pd) == 0L)
    stop("plot_Reff: no finite `central` values to plot (all warm-up/NA).")
  rownames(pd) <- NULL

  use_date_axis <- inherits(pd$date, "Date") && any(is.finite(pd$date))
  locs  <- unique(as.character(pd$location))
  n_loc <- length(locs)

  .has_ci <- function(lo, hi) all(c(lo, hi) %in% names(pd)) &&
    any(is.finite(pd[[lo]])) && any(is.finite(pd[[hi]]))
  draw_outer <- .has_ci("q2.5", "q97.5")
  draw_inner <- isTRUE(show_iqr) && .has_ci("q25", "q75")

  reff_color <- "#762A83"                              # total R_eff (purple)
  env_color  <- unname(mosaic_colors("environmental")) # teal
  hum_color  <- "#EE7733"                              # human-to-human (orange)
  ref_color  <- unname(mosaic_colors("reference"))
  lab_env <- "Environmental (R_env)"
  lab_hum <- "Human-to-human added on top (R_hum)"
  lab_tot <- "Total R_eff = R_env + R_hum"

  p <- ggplot2::ggplot(pd, ggplot2::aes(x = .data$date))
  if (draw_outer)
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$q2.5, ymax = .data$q97.5),
      fill = mosaic_color_variant(reff_color, "lighten", 0.6), alpha = 0.3)
  if (draw_inner)
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$q25, ymax = .data$q75),
      fill = mosaic_color_variant(reff_color, "lighten", 0.4), alpha = 0.35)

  if (has_routes) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$zero, ymax = .data$env_top, fill = lab_env),
        alpha = 0.55, na.rm = TRUE) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$env_top, ymax = .data$tot_top, fill = lab_hum),
        alpha = 0.75, na.rm = TRUE) +
      ggplot2::scale_fill_manual(
        values = stats::setNames(c(env_color, hum_color), c(lab_env, lab_hum)),
        breaks = c(lab_env, lab_hum), name = NULL)
  }

  p <- p + ggplot2::geom_hline(yintercept = 1, linetype = "dashed",
                               color = ref_color, linewidth = 0.6)
  if (sd_k > 1L)
    p <- p + ggplot2::geom_line(ggplot2::aes(y = .data$central),
                                color = reff_color, linewidth = 0.2,
                                alpha = 0.2, na.rm = TRUE)
  yvar <- if (has_routes) "tot_top" else if (sd_k > 1L) "central_smooth" else "central"
  p <- p + ggplot2::geom_line(ggplot2::aes(y = .data[[yvar]], colour = lab_tot),
                              linewidth = 0.5, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = stats::setNames(reff_color, lab_tot),
                                 name = NULL) +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1),
                    fill = ggplot2::guide_legend(order = 2))

  if (n_loc > 1L) {
    nc <- if (is.null(ncol)) min(3L, n_loc) else as.integer(ncol)
    p <- p + ggplot2::facet_wrap(~ location, scales = "free_y", ncol = nc)
  }

  # Per-member peak R_t annotation (total R_eff rows of attr peak_Rt).
  subtitle <- NULL
  pk <- .reff_peak_table(peak_Rt, locs)
  if (!is.null(pk) && nrow(pk) > 0L) {
    pw <- attr(reff, "peak_Rt_window")
    pk$label <- sprintf("Peak %sR_eff (per-member): %.2f [%.2f, %.2f]",
                        if (is.numeric(pw) && length(pw) == 1L && pw > 1)
                          paste0(pw, "-day ") else "",
                        pk$q50, pk$q2.5, pk$q97.5)
    if (n_loc == 1L) {
      subtitle <- pk$label[match(locs[1L], pk$location)]
    } else {
      x0 <- min(pd$date, na.rm = TRUE)
      ann <- do.call(rbind, lapply(split(pd, pd$location), function(d) {
        loc <- as.character(d$location[1L])
        row <- pk[match(loc, pk$location), , drop = FALSE]
        if (nrow(row) == 0L || is.na(row$label)) return(NULL)
        yv <- suppressWarnings(max(c(d[[yvar]], d$q97.5), na.rm = TRUE))
        if (!is.finite(yv)) return(NULL)
        data.frame(location = loc, date = x0, y = yv, label = row$label,
                   stringsAsFactors = FALSE)
      }))
      if (!is.null(ann) && nrow(ann) > 0L)
        p <- p + ggplot2::geom_text(
          data = ann, ggplot2::aes(x = .data$date, y = .data$y, label = .data$label),
          hjust = 0, vjust = 1.2, size = base_size * 0.22,
          color = "#2D2D2D", inherit.aes = FALSE, na.rm = TRUE)
    }
  }

  band_note <- if (draw_outer) {
    paste0("Faint band: 95%", if (draw_inner) " (and 50%)" else "",
           " range of total R_eff ACROSS members at each date (not the peak R_t;",
           " member peaks are phase-misaligned).")
  } else if (identical(ci_source, "unavailable_strided_lines")) {
    "Posterior band unavailable (per-member trajectory lines are time-strided)."
  } else if (identical(ci_source, "unavailable_no_incidence_lines")) {
    "Posterior band unavailable (no per-member route incidence lines)."
  } else {
    "Posterior band unavailable."
  }
  route_note <- if (has_routes)
    paste0(" Teal area: environmental R; orange band: human-to-human R stacked",
           " on top; purple line: their sum.") else ""
  smooth_note <- if (sd_k > 1L) sprintf(" %d-day centered mean.", sd_k) else ""
  caption <- paste0(
    "Cori R_eff on simulated infection incidence (a descriptor of the model ",
    "trajectory, not a first-principles R0).", route_note, smooth_note,
    " Dashed line: R = 1.\n", band_note)

  if (is.null(title))
    title <- if (n_loc == 1L) paste0("Effective reproductive number: ", locs[1L]) else
      "Effective reproductive number"

  p <- p +
    ggplot2::scale_y_continuous() +
    theme_mosaic(base_size = base_size) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   legend.position = "top") +
    ggplot2::labs(x = if (use_date_axis) "Date" else "Time",
                  y = expression(R[eff](t)), title = title,
                  subtitle = subtitle, caption = caption)
  if (use_date_axis) p <- p + ggplot2::scale_x_date()
  p
}

# -----------------------------------------------------------------------------
# Centered, NA-aware rolling mean (window k days). Each point averages the finite
# values in [i - (k-1)/2, i + (k-1)/2]; positions with no finite neighbour stay
# NA so genuine gaps are preserved. k <= 1 returns x unchanged.
# -----------------------------------------------------------------------------
.reff_roll_mean <- function(x, k) {
  k <- as.integer(k)
  n <- length(x)
  if (is.na(k) || k <= 1L || n == 0L) return(x)
  half <- (k - 1L) %/% 2L
  fin  <- is.finite(x)
  xf   <- ifelse(fin, x, 0)
  cs   <- c(0, cumsum(xf))
  cw   <- c(0, cumsum(as.numeric(fin)))
  out  <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    lo <- max(1L, i - half); hi <- min(n, i + half)
    w  <- cw[hi + 1L] - cw[lo]
    if (w > 0) out[i] <- (cs[hi + 1L] - cs[lo]) / w
  }
  out
}

# -----------------------------------------------------------------------------
# Internal: normalize the `peak_Rt` attribute into a per-location data.frame
# with finite q2.5/q50/q97.5 rows (total R_eff) for the locations being
# plotted. Returns NULL when the attribute is absent, malformed, or has no
# usable rows (so the caller omits the annotation rather than erroring).
# -----------------------------------------------------------------------------
.reff_peak_table <- function(peak_Rt, locs) {
  if (is.null(peak_Rt) || !is.data.frame(peak_Rt) || nrow(peak_Rt) == 0L)
    return(NULL)
  need <- c("location", "q2.5", "q50", "q97.5")
  if (!all(need %in% names(peak_Rt))) return(NULL)
  if ("estimand" %in% names(peak_Rt))
    peak_Rt <- peak_Rt[peak_Rt$estimand == "R_eff", , drop = FALSE]
  pk <- peak_Rt[, need, drop = FALSE]
  pk$location <- as.character(pk$location)
  pk <- pk[pk$location %in% locs, , drop = FALSE]
  pk <- pk[is.finite(pk$q50) & is.finite(pk$q2.5) & is.finite(pk$q97.5), ,
           drop = FALSE]
  if (nrow(pk) == 0L) return(NULL)
  rownames(pk) <- NULL
  pk
}
