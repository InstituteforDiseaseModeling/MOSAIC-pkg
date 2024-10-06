#' Plot Predicted Suitability for All Countries
#'
#' This function generates a facet plot of predicted environmental suitability for all countries
#' over time, with shaded regions indicating cholera outbreaks.
#'
#' @param PATHS A list of paths to the directories where data is stored and plots will be saved.
#'
#' @return A facet plot of predicted environmental suitability for all countries.
#' @export

plot_suitability_by_country <- function(PATHS) {

     # Load necessary libraries
     requireNamespace('ggplot2')
     requireNamespace('glue')

     # Load prediction data (generated from est_suitability function)
     d_all <- read.csv(file.path(PATHS$MODEL_INPUT, "data_psi_suitability.csv"), stringsAsFactors = FALSE)


     # Define the professional color palette
     color_shaded <- "#B0E0E6"  # Light Blue for shaded regions
     color_pred <- "#ff7f0e"    # Orange for predicted values
     color_actual <- "black"    # Black for actual predictions

     # Convert date columns to Date format
     d_all$date_start <- as.Date(d_all$date_start)
     d_all$date_stop <- as.Date(d_all$date_stop)

     # Filter data for years >= 2021 (if applicable)
     year_start <- 2021
     plot_data <- d_all[d_all$year >= year_start, ]

     # Extract date limits for consistency across all plots
     date_min <- min(plot_data$date_start, na.rm = TRUE)
     date_max <- max(plot_data$date_start, na.rm = TRUE)

     # Create the subset for shaded regions where cases_binary == 1
     shaded_data <- plot_data[plot_data$cases_binary == 1 & !is.na(plot_data$cases_binary), ]

     # Combine the shaded areas and line plot for predicted values
     combined_plot <- ggplot() +
          geom_rect(data = shaded_data,
                    aes(xmin = date_start, xmax = date_stop, ymin = 0, ymax = Inf),
                    fill = color_shaded, color = color_shaded, alpha = 0.5) +  # Shaded regions in Light Blue
          geom_line(data = plot_data, aes(x = date_start, y = pred_smooth), linewidth = 1, color = color_pred) +
          geom_line(data = plot_data[plot_data$date_stop <= Sys.Date(), ],
                    aes(x = date_start, y = pred_smooth), linewidth = 1, color = color_actual) +
          facet_wrap(~ country, ncol = 3) +  # Facet by country
          labs(x = NULL, y = 'Predicted Suitability') +
          scale_y_continuous(limits = c(-0.001, 1), breaks = c(0, 0.5, 1), expand = c(0.1, 0.1)) +
          scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", expand = c(0, 0),
                       limits = c(date_min, date_max)) +
          theme_minimal() +
          theme(
               axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
               axis.title.y = element_text(size = 12, margin = margin(r = 10)),
               panel.grid.major.x = element_blank(),
               panel.grid.major.y = element_line(),
               panel.grid.minor = element_blank(),
               axis.ticks.x = element_line(),
               axis.line.x = element_blank()
          )

     print(combined_plot)

     # Save the facet plot to the specified path
     plot_file <- file.path(PATHS$DOCS_FIGURES, "suitability_by_country.png")
     ggplot2::ggsave(filename = plot_file, plot = combined_plot, width = 8, height = 16, units = "in", dpi = 300)

     # Print a message
     message(glue::glue("Facet plot saved to: {plot_file}"))
}
