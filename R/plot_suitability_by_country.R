#' Plot Predicted Suitability for All Countries
#'
#' This function generates a facet plot of predicted environmental suitability for all countries
#' over time. Requires that est_suitability() has been run first to generate the necessary
#' prediction files.
#'
#' @param PATHS A list of paths to the directories where data is stored and plots will be saved.
#'   Must include MODEL_INPUT (for reading prediction files) and DOCS_FIGURES (for saving plots).
#'
#' @return A facet plot of predicted environmental suitability for all countries, filtered to
#'   years >= 2021. The plot includes suitability predictions with separate colors for
#'   historical (black) and future (orange) predictions.
#'
#' @details The function reads pred_psi_suitability_week.csv created by est_suitability(),
#'   which contains the columns: iso_code, year, week, date, cases, cases_binary, and pred.
#'
#' @export

plot_suitability_by_country <- function(PATHS) {

     # Load necessary libraries
     requireNamespace('ggplot2')
     requireNamespace('glue')

     # Load prediction data (generated from est_suitability function)
     pred_weekly_file <- file.path(PATHS$MODEL_INPUT, "pred_psi_suitability_week.csv")

     if (!file.exists(pred_weekly_file)) {
          stop(glue::glue("Prediction file not found: {pred_weekly_file}. Please run est_suitability(PATHS) first."))
     }

     # Load prediction data
     d_all <- read.csv(pred_weekly_file, stringsAsFactors = FALSE)
     d_all$date <- as.Date(d_all$date)
     d_all$country <- convert_iso_to_country(d_all$iso_code)

     # Fill in country names for missing entries using first available country name per iso_code
     country_lookup <- d_all[!is.na(d_all$country), c("iso_code", "country")]
     country_lookup <- country_lookup[!duplicated(country_lookup$iso_code), ]

     for (iso in unique(d_all$iso_code[is.na(d_all$country)])) {
          if (iso %in% country_lookup$iso_code) {
               d_all$country[d_all$iso_code == iso & is.na(d_all$country)] <- country_lookup$country[country_lookup$iso_code == iso]
          } else {
               # Fallback to iso_code if no country name available
               d_all$country[d_all$iso_code == iso & is.na(d_all$country)] <- iso
          }
     }

     # Define the professional color palette
     color_pred <- "#f54725"    # Orange for future predictions
     color_actual <- "#4a4a4a"    # Black for historical predictions

     # Filter data for years >= 2021
     year_start <- 2021
     plot_data <- d_all[d_all$year >= year_start, ]

     # Extract date limits for consistency across all plots
     date_min <- min(plot_data$date, na.rm = TRUE)
     date_max <- max(plot_data$date, na.rm = TRUE)

     # Create line plot for predicted values
     combined_plot <- ggplot() +
          geom_line(data = plot_data, aes(x = date, y = pred), linewidth = 1.75, color = color_pred) +
          geom_line(data = plot_data[plot_data$date <= Sys.Date(), ],
                    aes(x = date, y = pred), linewidth = 1.75, color = color_actual) +
          facet_wrap(~ country, ncol = 2) +  # Facet by country
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
     ggplot2::ggsave(filename = plot_file, plot = combined_plot, width = 10, height = 25, units = "in", dpi = 600)

     # Print a message
     message(glue::glue("Facet plot saved to: {plot_file}"))
}
