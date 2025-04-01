#' Plot recovery durations and rates for symptomatic and asymptomatic infections
#'
#' This function visualizes the assumed shedding durations and corresponding recovery rates
#' (1/duration) for symptomatic and asymptomatic cholera infections using transparent bars
#' and solid mean lines. Recovery duration is shown on the x-axis.
#'
#' If a list `PATHS` is provided with a valid `DOCS_FIGURES` element, the plot will also be
#' saved as a PNG in that directory as "recovery_rates.png".
#'
#' @param symp_range A numeric vector of length 2 indicating the minimum and maximum shedding duration (in days) for symptomatic infections. Default is c(3, 7).
#' @param asymp_range A numeric vector of length 2 indicating the minimum and maximum shedding duration (in days) for asymptomatic infections. Default is c(7, 14).
#' @param PATHS Optional list containing output paths. If provided, must include `DOCS_FIGURES` where plot will be saved.
#'
#' @return Invisibly returns the ggplot object. If `PATHS` is supplied, also saves a PNG.
#' @export
#'
#' @examples
#' plot_recovery_duration()
#' plot_recovery_duration(symp_range = c(4, 6), asymp_range = c(8, 12))
plot_recovery_duration <- function(PATHS = NULL, symp_range = c(3, 7), asymp_range = c(7, 14)) {

     requireNamespace("ggplot2")

     # Define data
     data <- data.frame(
          type = factor(c("Symptomatic", "Asymptomatic"), levels = c("Asymptomatic", "Symptomatic")),
          min_days = c(symp_range[1], asymp_range[1]),
          max_days = c(symp_range[2], asymp_range[2])
     )

     # Compute additional values
     data$mean_days <- (data$min_days + data$max_days) / 2
     data$y <- as.numeric(data$type)
     data$ymin <- data$y - 0.3
     data$ymax <- data$y + 0.3

     # Define manual colors
     color_values <- c("Symptomatic" = "#E41A1C", "Asymptomatic" = "#377EB8")

     # Build plot
     p <- ggplot(data) +
          geom_rect(aes(xmin = min_days, xmax = max_days,
                        ymin = ymin, ymax = ymax,
                        fill = type),
                    alpha = 0.4, color = NA) +
          geom_segment(aes(x = mean_days, xend = mean_days,
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
               limits = c(2, 15),
               breaks = seq(2, 15, 2)
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
