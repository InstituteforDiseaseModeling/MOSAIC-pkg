#' Plot Precipitation and Cholera Case Data with Fourier Series Fits
#'
#' This function visualizes the seasonal precipitation and cholera cases data, fitted with a double Fourier series model. It generates a plot of the Z-scored data for each country, indicating inferred countries where cholera cases data was imputed from neighbors.
#'
#' @param PATHS A list containing paths where processed climate, cholera, and fitted data are stored. Typically generated by the get_paths() function and should include:
#' \itemize{
#'   \item \strong{DOCS_FIGURES}: Path to save the output plot.
#' }
#'
#' @return The function generates and saves a PNG file showing the seasonal dynamics of precipitation and cholera cases across multiple countries.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_text scale_x_continuous scale_color_manual labs theme_minimal theme element_text element_blank
#' @importFrom dplyr filter distinct
#' @importFrom glue glue
#' @importFrom cowplot get_legend
#' @export

plot_seasonal_transmission <- function(PATHS) {

     # Load the combined precipitation data and fitted values
     combined_precip_data <- utils::read.csv(file.path(PATHS$MODEL_INPUT, "data_seasonal_precipitation.csv"), stringsAsFactors = FALSE)
     combined_fitted_values <- utils::read.csv(file.path(PATHS$MODEL_INPUT, "pred_seasonal_dynamics_day.csv"), stringsAsFactors = FALSE)
     combined_fitted_values$inferred_from_neighbor[combined_fitted_values$inferred_from_neighbor == "Democratic Republic of Congo"] <- "DRC"

     # Add day_week_mid to both datasets to align weekly values on the daily Fourier scale
     combined_fitted_values$day <- as.numeric(combined_fitted_values$day)
     combined_fitted_values$day_week_mid <- (((combined_fitted_values$day - 1) %/% 7) + 1) * 7 + 4
     combined_precip_data$day_week_mid <- ((as.numeric(format(as.Date(combined_precip_data$date), "%j")) - 1) %/% 7) * 7 + 4

     tmp_precip <- combined_precip_data[, c("iso_code", "day_week_mid", "precip_scaled")]
     tmp_precip <- distinct(tmp_precip)

     tmp_cases <- combined_precip_data[, c("iso_code", "day_week_mid", "cases_scaled")]
     tmp_cases <- distinct(tmp_cases)

     iso_codes <- sort(unique(combined_fitted_values$iso_code))
     iso_codes_with_data <- sort(unique(combined_fitted_values$iso_code[is.na(combined_fitted_values$inferred_from_neighbor)]))

     # Add a column to hold the label for plotting
     inferred_map <- combined_fitted_values[, c("iso_code", "inferred_from_neighbor")]
     inferred_map <- dplyr::distinct(inferred_map)
     inferred_map <- cbind(data.frame(country_name = MOSAIC::convert_iso_to_country(inferred_map$iso_code)), inferred_map)


     inferred_map$plot_label <- glue::glue("(Inferred from {inferred_map$inferred_from_neighbor})")
     inferred_map$plot_label[is.na(inferred_map$inferred_from_neighbor)] <- " "
     inferred_map$plot_label <- glue::glue("{inferred_map$country_name}\n{inferred_map$plot_label}")


     combined_fitted_values <- merge(combined_fitted_values, inferred_map[, c("iso_code", "plot_label")], by = "iso_code")
     tmp_precip <- merge(tmp_precip, inferred_map[, c("iso_code", "plot_label")], by = "iso_code")
     tmp_cases <- merge(tmp_cases, inferred_map[, c("iso_code", "plot_label")], by = "iso_code")

     # Create the full facet plot using daily Fourier fits and weekly case/precipitation data aligned to day_week_mid
     p_facet <-
          ggplot() +
          geom_hline(yintercept = 0, color = "black") +

          geom_point(data = tmp_precip, aes(x = day_week_mid, y = precip_scaled, color = "Precipitation (1994-2024)"),
                     size = 1.5, alpha = 0.15) +

          geom_point(data = tmp_cases, aes(x = day_week_mid, y = cases_scaled, color = "Cholera Cases (2023-2024)"),
                     size = 1.5, alpha = 0.4) +

          geom_line(data = combined_fitted_values, aes(x = day, y = fitted_values_fourier_precip, color = "Fourier Series (Precip)"),
                    linewidth = 1.2) +

          geom_line(data = combined_fitted_values, aes(x = day, y = fitted_values_fourier_cases, color = "Fourier Series (Cases)", linetype = !is.na(inferred_from_neighbor)), linewidth = 1.2) +



          scale_x_continuous(breaks = c(1, 70, 140, 210, 280, 365), name = "Day of Year") +
          scale_y_continuous(name = "Scaled Precipitation and Cholera Cases (Z-Score)") +

          scale_color_manual(
               values = c(
                    "Precipitation (1994-2024)" = "dodgerblue",
                    "Cholera Cases (2023-2024)" = "#d7191c",
                    "Fourier Series (Precip)" = "#1873CC",
                    "Fourier Series (Cases)" = "#AC1416"
               ),
               breaks = c(
                    "Precipitation (1994-2024)",
                    "Cholera Cases (2023-2024)",
                    "Fourier Series (Precip)",
                    "Fourier Series (Cases)"
               )
          ) +

          theme_minimal() +
          theme(
               plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
               axis.title.x = element_text(size = 14, margin = margin(t = 20)),
               axis.title.y = element_text(size = 14, margin = margin(r = 20)),
               axis.text = element_text(size = 12),
               legend.text = element_text(size = 12),
               legend.title = element_blank(),
               legend.position = "bottom",
               legend.box = "horizontal",
               legend.box.just = "left",
               panel.grid.major.x = element_blank(),
               panel.grid.major.y = element_line(color = "grey90", linewidth = 0.25),
               panel.grid.minor = element_blank(),
               strip.text = element_text(size = 9)
          ) +

          guides(
               color = guide_legend(nrow = 2, byrow = TRUE),
               linetype = "none"
          ) +

          facet_wrap(~ plot_label, scales = "free_y", ncol = 3)

     print(p_facet)

     # Save the plot to the figures directory
     png_filename <- file.path(PATHS$DOCS_FIGURES, "seasonal_transmission_all.png")
     png(filename = png_filename, width = 9, height = 20, units = "in", res = 600)
     print(p_facet)
     dev.off()

     message(glue::glue("Seasonal transmission plot saved to {png_filename}."))
}
