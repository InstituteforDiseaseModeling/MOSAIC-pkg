#' Plot Predicted Suitability and Reported Cholera Cases for a Specific Country
#'
#' This function plots the reported cholera cases and predicted environmental suitability
#' for a specific country over time, with shaded regions indicating cholera outbreaks.
#'
#' @param PATHS A list of paths to the directories where data is stored and plots will be saved.
#' @param plot_iso_code The ISO code of the country to plot.
#'
#' @return A combined plot of reported cholera cases and predicted environmental suitability.
#' @export

plot_suitability_and_cases <- function(PATHS, plot_iso_code) {

     # Load necessary libraries
     requireNamespace('ggplot2')
     requireNamespace('patchwork')
     requireNamespace('glue')

     # Define the professional color palette
     color_cases <- "#152F45"  # Navy Blue for bars (cases)
     color_shaded <- "#B0E0E6"  # Light Blue for shaded regions
     color_pred <- "#ff7f0e"    # Orange for predicted values
     color_actual <- "black"    # Black for actual predictions

     # Load prediction data
     d_all <- read.csv(file.path(PATHS$MODEL_INPUT, "data_psi_suitability.csv"), stringsAsFactors = FALSE)

     # Convert date columns to Date format
     d_all$date_start <- as.Date(d_all$date_start)
     d_all$date_stop <- as.Date(d_all$date_stop)

     # Filter data for the specific country and time range (>= 2021)
     year_start <- 2021
     plot_data <- d_all[d_all$iso_code == plot_iso_code & d_all$year >= year_start, ]
     plot_country_name <- convert_iso_to_country(plot_iso_code)
     date_present <- max(plot_data$date_stop[plot_data$date_stop < Sys.Date()], na.rm = TRUE)

     # Extract the country name and date limits
     country_name <- unique(plot_data$country_name)
     date_min <- min(plot_data$date_start, na.rm = TRUE)
     date_max <- max(plot_data$date_start, na.rm = TRUE)

     # Create the subset for shaded regions where cases_binary == 1
     shaded_data <- plot_data[plot_data$cases_binary == 1 & !is.na(plot_data$cases_binary), ]

     # Bar plot for cholera cases with shaded regions
     bar_plot <- ggplot2::ggplot() +
          ggplot2::geom_rect(data = shaded_data,
                             ggplot2::aes(xmin = date_start, xmax = date_stop, ymin = 0, ymax = Inf),
                             fill = color_shaded, color = color_shaded) +  # Shaded regions with transparency
          ggplot2::geom_bar(data = plot_data,
                            ggplot2::aes(x = date_start, y = cases), stat = "identity", fill = color_cases, color = color_cases) +
          ggplot2::labs(x = NULL, y = "Number of Cases") +
          ggplot2::scale_y_continuous(expand = c(0, 0)) +
          ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", expand = c(0, 0),
                                limits = c(date_min, date_max)) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
               axis.text.x = ggplot2::element_blank(),
               axis.title.y = ggplot2::element_text(size = 12),
               panel.grid.major.x = ggplot2::element_blank(),
               panel.grid.minor = ggplot2::element_blank(),
               axis.ticks.x = ggplot2::element_line(),
               axis.line.x = ggplot2::element_line()
          )

     # Line plot for predicted suitability with actual data
     line_plot <- ggplot2::ggplot() +
          ggplot2::geom_rect(data = shaded_data,
                             ggplot2::aes(xmin = date_start, xmax = date_stop, ymin = 0, ymax = Inf),
                             fill = color_shaded, color = color_shaded) +  # Shaded regions

          ggplot2::geom_line(data = plot_data,
                             ggplot2::aes(x = date_start, y = pred), linewidth = 0.8, color = color_pred, alpha = 0.3) +

          ggplot2::geom_line(data = plot_data,
                             ggplot2::aes(x = date_start, y = pred_smooth), linewidth = 1.2, color = color_pred) +

          ggplot2::geom_line(data = plot_data[plot_data$date_stop <= date_present, ],
                             ggplot2::aes(x = date_start, y = pred), linewidth = 0.8, color = color_actual, alpha=0.3) +

          ggplot2::geom_line(data = plot_data[plot_data$date_stop <= date_present, ],
                             ggplot2::aes(x = date_start, y = pred_smooth), linewidth = 1.2, color = color_actual) +

          ggplot2::geom_vline(xintercept = as.Date(date_present) - 7, linetype = "dashed", color = "black", linewidth = 0.5) +
          ggplot2::annotate("text", x = as.Date(date_present) - 7, y = Inf, label = as.character(date_present),
                            angle = 90, hjust = 1, vjust = -0.5, size = 3, color = "black") +
          ggplot2::labs(x = NULL, y = 'Predicted Suitability') +
          ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0.0025, 0.0025)) +
          ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", expand = c(0, 0),
                                limits = c(date_min, date_max)) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
               panel.grid.major.x = ggplot2::element_blank(),
               panel.grid.major.y = ggplot2::element_line(),
               panel.grid.minor = ggplot2::element_blank(),
               axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1, size = 9),
               axis.title.y = ggplot2::element_text(size = 12, margin = ggplot2::margin(r = 10)),
               axis.ticks.x = ggplot2::element_line(),
               axis.line.x = ggplot2::element_line()
          )

     # Combine the bar plot and line plot using patchwork
     p_combined <- (bar_plot / line_plot) + plot_layout(heights = c(1, 2)) +
          plot_annotation(title = glue::glue("Reported Cholera Cases and Predicted Environmental Suitability (\u03A8) for {plot_country_name} ({plot_iso_code})")) &
          theme(plot.title = element_text(hjust = 0.5))

     print(p_combined)


     # Save the combined plot to the specified path
     plot_file <- file.path(PATHS$DOCS_FIGURES, glue::glue("suitability_cases_{plot_iso_code}.png"))
     ggplot2::ggsave(filename = plot_file, plot = p_combined, width = 8, height = 5, units = "in", dpi = 300)

     # Print a message
     message(glue::glue("Plot saved to: {plot_file}"))
}
