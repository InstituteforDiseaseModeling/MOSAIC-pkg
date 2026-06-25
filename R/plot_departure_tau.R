#' Plot the model-implied departure probability (\eqn{\tau_i}) and daily travelers
#'
#' Two-panel figure for the \code{"spatial"} group of
#' \code{\link{render_MOSAIC_figures}}: (left, A) a forest plot of the daily
#' departure probability \eqn{\tau_i} per location, with optional 95\% credible
#' intervals; (right, B) the implied total daily travelers leaving each origin,
#' \eqn{N_i \tau_i}.
#'
#' The aesthetic matches the published \eqn{\tau_j} panel of
#' \code{\link{plot_mobility}} exactly (\code{theme_minimal()}, red dashed mean
#' line, black \code{shape = 21} points, \code{dodgerblue} bars on a
#' \code{scale_x_sqrt} axis, \code{cowplot::plot_grid} with A/B labels). Both
#' panels are ordered by \eqn{\tau_i} via \code{reorder(iso3, mean)} — a keyed
#' reorder over the data frame, not a positional sort (F3-safe).
#'
#' \eqn{\tau_i} is a \strong{daily} probability (the engine consumes it per-day
#' in the spatial-hazard step), so \eqn{N_i \tau_i} is the expected number of
#' travelers \emph{per day}. CI bars are drawn only when \code{ci} is supplied
#' (from the optional \code{mobility_tau_ci.csv} run artifact); otherwise the
#' left panel shows point estimates only — matching \code{\link{plot_mobility}}
#' minus the error bars.
#'
#' @param tau Numeric vector of daily departure probabilities (length J),
#'   in config order. From \code{\link{calc_mobility_flux}} element \code{$tau}.
#' @param N Numeric vector of origin populations (length J), config order.
#' @param location_name Character vector of length J giving the labels, config
#'   order. Defaults to \code{names(tau)}.
#' @param ci Optional data frame of credible intervals with columns
#'   \code{location} (matching \code{location_name}), \code{lower}, and
#'   \code{upper} (on the \eqn{\tau} scale). When \code{NULL} (default), no
#'   interval bars are drawn.
#'
#' @return Invisibly, the combined \pkg{cowplot} object (or the single forest
#'   \pkg{ggplot2} object if \pkg{cowplot} is unavailable).
#'
#' @seealso \code{\link{calc_mobility_flux}}, \code{\link{plot_mobility}}.
#' @importFrom ggplot2 ggplot aes geom_vline geom_point geom_errorbarh geom_bar xlab ylab theme_minimal theme element_text element_blank margin scale_x_sqrt
#' @importFrom cowplot plot_grid
#' @import ggplot2
#' @export
plot_departure_tau <- function(tau, N, location_name = names(tau), ci = NULL) {

  if (!is.numeric(tau)) stop("`tau` must be a numeric vector.")
  if (!is.numeric(N))   stop("`N` must be a numeric vector.")
  J <- length(tau)
  if (length(N) != J) stop("`N` and `tau` must have the same length.")
  if (is.null(location_name)) location_name <- paste0("loc_", seq_len(J))
  if (length(location_name) != J)
    stop("`location_name` length must match length(tau).")

  loc <- as.character(location_name)

  # Mirror plot_mobility's tau_df columns: `iso3`, `mean`, `population`.
  tau_df <- data.frame(
    iso3       = loc,
    mean       = as.numeric(tau),
    population = as.numeric(N),
    stringsAsFactors = FALSE
  )

  has_ci <- !is.null(ci)
  if (has_ci) {
    if (!is.data.frame(ci) ||
        !all(c("location", "lower", "upper") %in% names(ci))) {
      warning("plot_departure_tau: `ci` must have columns location/lower/upper; ",
              "drawing point estimates only.", call. = FALSE)
      has_ci <- FALSE
    } else {
      m <- match(tau_df$iso3, ci$location)
      tau_df$Q2.5  <- ci$lower[m]
      tau_df$Q97.5 <- ci$upper[m]
      if (all(is.na(tau_df$Q2.5)) || all(is.na(tau_df$Q97.5))) has_ci <- FALSE
    }
  }

  # main forest plot: tau_i with optional CI (plot_mobility.R:223-235)
  main_plot <-
    ggplot2::ggplot(tau_df, ggplot2::aes(x = .data$mean,
                                         y = stats::reorder(.data$iso3, .data$mean))) +
    ggplot2::geom_vline(xintercept = mean(tau_df$mean), linetype = 2, color = 'red')
  if (has_ci) {
    main_plot <- main_plot +
      ggplot2::geom_errorbarh(
        ggplot2::aes(xmin = .data$Q2.5, xmax = .data$Q97.5),
        height = 0, linewidth = 0.75, na.rm = TRUE)
  }
  main_plot <- main_plot +
    ggplot2::geom_point(size = 2.5, shape = 21, fill = "black") +
    ggplot2::xlab("Mean daily probability of travel") +
    ggplot2::ylab(NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_text(size = 10),
      axis.text.y  = ggplot2::element_text(size = 10),
      axis.title.x = ggplot2::element_text(size = 12, margin = ggplot2::margin(t = 15)),
      axis.title.y = ggplot2::element_text(size = 12, margin = ggplot2::margin(r = 15)),
      plot.margin  = ggplot2::margin(20, 5, 5, 0))

  # bar plot: total daily travelers = mean * population (plot_mobility.R:238-250)
  bar_plot <-
    ggplot2::ggplot(tau_df, ggplot2::aes(x = .data$mean * .data$population,
                                         y = stats::reorder(.data$iso3, .data$mean))) +
    ggplot2::geom_bar(stat = "identity", fill = "dodgerblue") +
    ggplot2::scale_x_sqrt(breaks = c(0, 500, 2000, 5000),
                          label = c("0", "500", "2,000", "5,000")) +
    ggplot2::theme_minimal() +
    ggplot2::xlab("Estimated total\ndaily travelers") +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 12, margin = ggplot2::margin(t = 15)),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x  = ggplot2::element_text(size = 10),
      axis.text.y  = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      plot.margin  = ggplot2::margin(20, 5, 5, 5))

  if (requireNamespace("cowplot", quietly = TRUE)) {
    combined <- cowplot::plot_grid(main_plot, bar_plot, rel_widths = c(2, 1),
                                   align = 'h', axis = 'tb', labels = c("A", "B"))
    print(combined)
    return(invisible(combined))
  }

  print(main_plot)
  invisible(main_plot)
}
