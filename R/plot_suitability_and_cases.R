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
#'   for the specified country, filtered to years >= 2021.
#'   
#' @details The function merges data from two files created by est_suitability():
#'   \itemize{
#'     \item pred_psi_suitability_week.csv - Contains smoothed predictions (pred_smooth)
#'     \item data_psi_suitability.csv - Contains case counts and outbreak indicators
#'   }
#'   
#' @export

plot_suitability_and_cases <- function(PATHS, plot_iso_code) {

     # Load necessary libraries
     requireNamespace('ggplot2')
     requireNamespace('patchwork')
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
     
     # Check what columns are available in the main data file
     required_cols <- c("iso_code", "week", "date_start", "cases_binary", "year")
     optional_cols <- c("country", "cases_weekly")
     
     available_cols <- colnames(d_main)
     cols_to_merge <- intersect(c(required_cols, optional_cols), available_cols)
     missing_required <- setdiff(required_cols, available_cols)
     
     if (length(missing_required) > 0) {
          stop(glue::glue("Required columns missing from main data file: {paste(missing_required, collapse=', ')}"))
     }
     
     # Merge on iso_code, week, and date_start for exact matches
     d_all <- merge(d_pred, 
                    d_main[cols_to_merge], 
                    by = c("iso_code", "week", "date_start"), 
                    all.x = TRUE)
     
     # For prediction periods that don't have main data, fill in missing columns
     # Extract year from date_start if missing
     d_all$year[is.na(d_all$year)] <- as.numeric(format(d_all$date_start[is.na(d_all$year)], "%Y"))
     
     # Set cases_binary to 0 for prediction-only periods
     d_all$cases_binary[is.na(d_all$cases_binary)] <- 0
     
     # Handle cases_weekly column (may not exist in all datasets)
     if ("cases_weekly" %in% colnames(d_all)) {
          d_all$cases_weekly[is.na(d_all$cases_weekly)] <- 0
     } else {
          # Create cases_weekly column if it doesn't exist (set to 0 for all)
          d_all$cases_weekly <- 0
     }
     
     # Handle country column (may not exist in all datasets)
     if ("country" %in% colnames(d_all)) {
          # Fill in country names for missing entries using existing data
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
     } else {
          # Create country column using iso_code as fallback
          d_all$country <- d_all$iso_code
     }


     # Define the professional color palette
     color_cases <- "#152F45"  # Navy Blue for bars (cases)
     color_shaded <- "#B0E0E6"  # Light Blue for shaded regions
     color_pred <- "#ff7f0e"    # Orange for predicted values
     color_actual <- "black"    # Black for actual predictions

     # Convert date_stop to Date format (date_start already converted during merge)
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
