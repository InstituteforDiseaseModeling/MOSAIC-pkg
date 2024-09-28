library(dplyr)
library(ggplot2)

plot_cases_binary <- function(PATHS, cutoff) {

     # Load cholera case data
     case_data <- utils::read.csv(file.path(PATHS$DATA_WHO_WEEKLY, "cholera_country_weekly_processed.csv"), stringsAsFactors = FALSE)

     case_data$date_start <- as.Date(case_data$date_start)
     case_data$date_stop <- as.Date(case_data$date_stop)

     # Filter data to include only rows where cases is not NA
     case_data <- case_data[!is.na(case_data$cases), ]

     # Group by country and create a new grouping for consecutive cases_binary == 1
     case_data <- case_data %>%
          group_by(country) %>%
          mutate(group = cumsum(c(1, diff(cases_binary) != 0))) %>%
          ungroup()

     # Filter only periods where cases_binary == 1
     shaded_data <- case_data %>%
          filter(cases_binary == 1) %>%
          group_by(country, group) %>%
          summarize(date_start = min(date_start), date_stop = max(date_stop), .groups = 'drop')

     # Define colors for cases and shaded regions
     #color_cases <- "#1f77b4"   # Navy Blue for bars (cases)
     #color_shaded <- "#d3d3d3"  # Light Gray for shaded regions

     color_cases <- "#152F45"  # Navy Blue for bars (cases)
     color_shaded <- "#B0E0E6"  # Navy Blue for shaded regions

     # Define date limits for x-axis (across all data)
     date_min <- min(case_data$date_start, na.rm = TRUE)
     date_max <- max(case_data$date_start, na.rm = TRUE)

     # Create the plot using case_data
     p <- ggplot2::ggplot() +

          # Shaded regions for binary cases (continuous regions)
          ggplot2::geom_rect(data = shaded_data,
                             ggplot2::aes(xmin = date_start, xmax = date_stop, ymin = 0, ymax = Inf),
                             fill = color_shaded, color = color_shaded) +  # Shaded regions with transparency

          # Bar plot for cases
          ggplot2::geom_bar(data = case_data,
                            ggplot2::aes(x = date_start, y = cases), stat = "identity", fill = color_cases, color = color_cases) +

          # Facet wrap by country
          ggplot2::facet_wrap(~ country, ncol = 2) +

          # Labels and axis formatting
          ggplot2::labs(x = NULL, y = "Number of Cases") +
          ggplot2::scale_y_continuous(expand = c(0, 0)) +  # No extra space around bars
          ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", expand = c(0, 0),
                                limits = c(date_min-14, date_max+14)) +  # Set date limits
          ggplot2::theme_minimal() +
          ggplot2::theme(
               axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1, size = 9),  # Rotate x-axis labels
               axis.title.y = ggplot2::element_text(size = 12, margin = margin(r = 10)),
               panel.grid.major.x = ggplot2::element_blank(),
               panel.grid.minor = ggplot2::element_blank(),
               axis.ticks.x = ggplot2::element_line(),   # Add x-axis ticks
               axis.line.x = ggplot2::element_line()
          )

     print(p)

     # Save the plot to the specified path
     plot_file <- file.path(PATHS$DOCS_FIGURES, glue::glue("cases_binary.png"))
     ggplot2::ggsave(filename = plot_file, plot = p, width = 6, height = 8, units = "in", dpi = 300)
     message(glue::glue("Plot saved to: {plot_file}"))
}
