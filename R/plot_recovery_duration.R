#' Plot recovery durations and rates for symptomatic and asymptomatic infections
#'
#' This function visualizes the assumed shedding durations and corresponding recovery rates
#' (1/duration) for symptomatic and asymptomatic cholera infections using transparent bars
#' and solid mean lines. Recovery duration is shown on the x-axis.
#'
#' If a list `PATHS` is provided with a valid `DOCS_FIGURES` element, the plot will also be
#' saved as a PNG in that directory as "recovery_rates.png".
#'
#' @param symp_range A numeric vector of length 2 indicating the lower and upper shedding-duration bound (in days) for symptomatic infections. Default is the 95% credible interval of the canonical lognormal prior gamma_1 ~ Lognormal(-2.303, 0.5), i.e. c(3.8, 26.6) days.
#' @param asymp_range A numeric vector of length 2 indicating the lower and upper shedding-duration bound (in days) for asymptomatic infections. Default is the 95% credible interval of the canonical lognormal prior gamma_2 ~ Lognormal(-0.693, 0.4), i.e. c(0.9, 4.4) days.
#' @param symp_central A single numeric giving the central duration (typically the median) for symptomatic infections. Default 10 days (median of gamma_1 prior). The vertical solid line in the plot is drawn here. If `NA`, falls back to the arithmetic midpoint of `symp_range`.
#' @param asymp_central A single numeric giving the central duration (typically the median) for asymptomatic infections. Default 2 days (median of gamma_2 prior).
#' @param PATHS Optional list containing output paths. If provided, must include `DOCS_FIGURES` where plot will be saved.
#'
#' @return Invisibly returns the ggplot object. If `PATHS` is supplied, also saves a PNG.
#' @export
#'
#' @examples
#' plot_recovery_duration()
#' plot_recovery_duration(symp_range = c(4, 6), asymp_range = c(8, 12),
#'                        symp_central = 5, asymp_central = 10)
#'

plot_recovery_duration <- function(PATHS = NULL,
                                   symp_range = c(3.8, 26.6),
                                   asymp_range = c(0.9, 4.4),
                                   symp_central = 10,
                                   asymp_central = 2) {

     requireNamespace("ggplot2")

     # Define data
     data <- data.frame(
          type = factor(c("Symptomatic", "Asymptomatic"), levels = c("Asymptomatic", "Symptomatic")),
          min_days = c(symp_range[1], asymp_range[1]),
          max_days = c(symp_range[2], asymp_range[2]),
          central_days = c(
               if (is.na(symp_central))  (symp_range[1]  + symp_range[2])  / 2 else symp_central,
               if (is.na(asymp_central)) (asymp_range[1] + asymp_range[2]) / 2 else asymp_central
          )
     )

     # Compute additional values
     data$y <- as.numeric(data$type)
     data$ymin <- data$y - 0.3
     data$ymax <- data$y + 0.3

     # Dynamic x-axis: pad either side of the widest range by 5%
     x_lo <- min(data$min_days, na.rm = TRUE) * 0.95
     x_hi <- max(data$max_days, na.rm = TRUE) * 1.05
     # Pick a break step that gives roughly 7-8 ticks
     break_step <- pretty(c(x_lo, x_hi), n = 7)
     break_step <- diff(break_step)[1]

     # Define manual colors
     color_values <- c("Symptomatic" = "#E41A1C", "Asymptomatic" = "#377EB8")

     # Build plot
     p <- ggplot(data) +
          geom_rect(aes(xmin = min_days, xmax = max_days,
                        ymin = ymin, ymax = ymax,
                        fill = type),
                    alpha = 0.4, color = NA) +
          geom_segment(aes(x = central_days, xend = central_days,
                           y = ymin, yend = ymax, color = type),
                       linewidth = 2) +
          scale_fill_manual(values = color_values) +
          scale_color_manual(values = color_values) +
          scale_y_continuous(
               breaks = data$y,
               labels = rev(levels(data$type))
          ) +
          scale_x_continuous(
               name = "Shedding Duration (days)",
               limits = c(max(0, x_lo), x_hi),
               breaks = seq(0, ceiling(x_hi / break_step) * break_step, by = break_step)
          ) +
          theme_minimal(base_size = 14) +
          theme(
               legend.position = "none",
               axis.title.y = element_blank(),
               plot.title = element_blank(),
               plot.subtitle = element_blank(),
               axis.title.x = element_text(margin = margin(t = 15)),
               panel.grid.minor = element_blank()
          )

     # Save plot if PATHS is provided
     if (!is.null(PATHS)) {
          if (!"DOCS_FIGURES" %in% names(PATHS)) {
               stop("PATHS must include a 'DOCS_FIGURES' element")
          }
          output_file <- file.path(PATHS$DOCS_FIGURES, "recovery_rates.png")
          png(filename = output_file, width = 7, height = 3, units = "in", res = 300)
          print(p)
          dev.off()
          message("Shedding rate plot saved to: ", output_file)
     }

     invisible(p)
}
