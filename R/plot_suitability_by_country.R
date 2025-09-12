#' Plot Predicted Suitability for All Countries
#'
#' This function generates a facet plot of predicted environmental suitability for all countries
#' over time, with shaded regions indicating cholera outbreaks. Requires that est_suitability() 
#' has been run first to generate the necessary prediction files.
#'
#' @param PATHS A list of paths to the directories where data is stored and plots will be saved.
#'   Must include MODEL_INPUT (for reading prediction files) and DOCS_FIGURES (for saving plots).
#'
#' @return A facet plot of predicted environmental suitability for all countries, filtered to 
#'   years >= 2021. The plot includes smoothed suitability predictions and shaded regions 
#'   indicating periods with cholera cases.
#'   
#' @details The function merges data from two files created by est_suitability():
#'   \itemize{
#'     \item pred_psi_suitability_week.csv - Contains smoothed predictions (pred_smooth)
#'     \item data_psi_suitability.csv - Contains country names and outbreak indicators
#'   }
#'   
#' @export

plot_suitability_by_country <- function(PATHS) {

     # Load necessary libraries
     requireNamespace('ggplot2')
     requireNamespace('glue')

     # Load prediction data with smoothed predictions (generated from est_suitability function)
     pred_weekly_file <- file.path(PATHS$MODEL_INPUT, "pred_psi_suitability_week.csv")
     data_main_file <- file.path(PATHS$MODEL_INPUT, "data_psi_suitability.csv")
     
     if (!file.exists(pred_weekly_file)) {
          stop(glue::glue("Prediction file not found: {pred_weekly_file}. Please run est_suitability(PATHS) first."))
     }
     
     if (!file.exists(data_main_file)) {
          stop(glue::glue("Main data file not found: {data_main_file}. Please run est_suitability(PATHS) first."))
     }
     
     # Load prediction data with smoothed values
     d_pred <- read.csv(pred_weekly_file, stringsAsFactors = FALSE)
     
     # Load main data file to get additional columns (country, cases_binary, year, etc.)
     d_main <- read.csv(data_main_file, stringsAsFactors = FALSE)
     
     # Merge to get all needed columns
     # First, create matching keys
     d_pred$date_start <- as.Date(d_pred$date_start)
     d_main$date_start <- as.Date(d_main$date_start)
     
     # Merge on iso_code, week, and date_start for exact matches
     d_all <- merge(d_pred, 
                    d_main[c("iso_code", "week", "date_start", "country", "cases_binary", "year")], 
                    by = c("iso_code", "week", "date_start"), 
                    all.x = TRUE)
     
     # For prediction periods that don't have main data, fill in missing columns
     # Extract year from date_start if missing
     d_all$year[is.na(d_all$year)] <- as.numeric(format(d_all$date_start[is.na(d_all$year)], "%Y"))
     
     # Set cases_binary to 0 for prediction-only periods (assumes no outbreaks in future predictions)
     d_all$cases_binary[is.na(d_all$cases_binary)] <- 0
     
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
