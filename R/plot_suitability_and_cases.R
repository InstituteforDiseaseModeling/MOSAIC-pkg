#' Plot Predicted Suitability and Reported Cholera Cases for a Specific Country
#'
#' This function plots the reported cholera cases and predicted environmental suitability
#' for a specific country over time, with shaded regions indicating cholera outbreaks.
#' Requires that est_suitability() has been run first to generate prediction files.
#'
#' @param PATHS A list of paths to the directories where data is stored and plots will be saved.
#'   Must include MODEL_INPUT (for reading prediction files) and DOCS_FIGURES (for saving plots).
#' @param plot_iso_code The ISO code of the country to plot (e.g., "AGO", "CMR").
#'
#' @return A combined plot showing both cholera case bars and smoothed suitability predictions
#'   for the specified country.
#'
#' @details The function reads pred_psi_suitability_day.csv created by est_suitability(),
#'   which contains: date, cases, pred, pred_smooth, and iso_code columns.
#'
#' @export

plot_suitability_and_cases <- function(PATHS, plot_iso_code) {

     # Load necessary libraries
     requireNamespace('ggplot2')
     requireNamespace('patchwork')
     requireNamespace('glue')

     # Load prediction data
     pred_daily_file <- file.path(PATHS$MODEL_INPUT, "pred_psi_suitability_day.csv")

     if (!file.exists(pred_daily_file)) {
          stop(glue::glue("Prediction file not found: {pred_daily_file}. Please run est_suitability(PATHS) first."))
     }

     # Load and process prediction data
     d_all <- read.csv(pred_daily_file, stringsAsFactors = FALSE)
     d_all$date <- as.Date(d_all$date)
     d_all$pred <- as.numeric(d_all$pred)
     d_all$pred_smooth <- as.numeric(d_all$pred_smooth)
     d_all$cases <- as.numeric(d_all$cases)
     d_all$cases[is.na(d_all$cases)] <- 0

     # Create additional required columns
     d_all$cases_binary <- as.numeric(d_all$cases > 0)
     d_all$year <- as.numeric(format(d_all$date, "%Y"))
     d_all$country <- convert_iso_to_country(d_all$iso_code)

     # Filter data for the specific country
     plot_data <- d_all[d_all$iso_code == plot_iso_code, ]

     if (nrow(plot_data) == 0) {
          stop(glue::glue("No data found for country: {plot_iso_code}"))
     }

     plot_country_name <- convert_iso_to_country(plot_iso_code)

     # Find the latest date with reported cases (not just current date)
     date_last_case <- max(plot_data$date[plot_data$cases > 0 & !is.na(plot_data$cases)], na.rm = TRUE)

     # If no cases found, fall back to current date
     if (is.infinite(date_last_case) || is.na(date_last_case)) {
          date_last_case <- max(plot_data$date[plot_data$date <= Sys.Date()], na.rm = TRUE)
     }

     # Extract date limits
     date_min <- min(plot_data$date, na.rm = TRUE)
     date_max <- max(plot_data$date, na.rm = TRUE)

     # Define the professional color palette
     color_cases <- "#5B9BD5"   # Light blue for bars (cases)
     color_pred <- "#f54725"    # Red-orange for future predictions
     color_actual <- "#4a4a4a"  # Gray for historical predictions

     # Bar plot for cholera cases
     bar_plot <- ggplot2::ggplot() +
          ggplot2::geom_bar(data = plot_data,
                            ggplot2::aes(x = date, y = cases),
                            stat = "identity", fill = color_cases, color = color_cases) +
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

     # Line plot for predicted suitability
     line_plot <- ggplot2::ggplot() +
          # Future predictions in red-orange (after last case date)
          ggplot2::geom_line(data = plot_data[plot_data$date > date_last_case, ],
                             ggplot2::aes(x = date, y = pred),
                             linewidth = 0.8, color = color_pred, alpha = 0.3) +
          ggplot2::geom_line(data = plot_data[plot_data$date > date_last_case, ],
                             ggplot2::aes(x = date, y = pred_smooth),
                             linewidth = 1.2, color = color_pred) +
          # Historical predictions in gray (up to last case date)
          ggplot2::geom_line(data = plot_data[plot_data$date <= date_last_case, ],
                             ggplot2::aes(x = date, y = pred),
                             linewidth = 0.8, color = color_actual, alpha = 0.3) +
          ggplot2::geom_line(data = plot_data[plot_data$date <= date_last_case, ],
                             ggplot2::aes(x = date, y = pred_smooth),
                             linewidth = 1.2, color = color_actual) +
          # Vertical line marking the last case date
          ggplot2::geom_vline(xintercept = as.Date(date_last_case),
                              linetype = "dashed", color = "black", linewidth = 0.5) +
          ggplot2::annotate("text", x = as.Date(date_last_case), y = Inf,
                            label = as.character(date_last_case),
                            angle = 90, hjust = 1, vjust = -0.5, size = 3, color = "black") +
          ggplot2::labs(x = NULL, y = 'Predicted Suitability') +
          ggplot2::scale_y_continuous(limits = c(-0.001, 1), expand = c(0.0025, 0.0025)) +
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
     p_combined <- (bar_plot / line_plot) + patchwork::plot_layout(heights = c(1, 2)) +
          patchwork::plot_annotation(title = glue::glue("Reported Cholera Cases and Predicted Environmental Suitability (\u03A8) for {plot_country_name} ({plot_iso_code})")) &
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

     print(p_combined)

     # Save the combined plot to the specified path
     plot_file <- file.path(PATHS$DOCS_FIGURES, glue::glue("suitability_cases_{plot_iso_code}.png"))
     ggplot2::ggsave(filename = plot_file, plot = p_combined, width = 8, height = 5, units = "in", dpi = 300)

     # Print a message
     message(glue::glue("Plot saved to: {plot_file}"))
}
