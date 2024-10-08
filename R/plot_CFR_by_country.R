#' Plot Case Fatality Ratios, Total Cases, and Beta Distributions by Country
#'
#' This function creates two separate plots: one for case fatality ratios (CFR) with confidence intervals and total cases by country, and another for Beta distributions for selected countries. The plots are generated based on cholera data stored in the `PATHS$DATA_WHO_ANNUAL` directory.
#'
#' @param PATHS A list containing the paths where the cholera data and figures will be saved. Typically generated by the `get_paths()` function and should include:
#' \itemize{
#'   \item \strong{DATA_WHO_ANNUAL}: Path to the directory where processed WHO annual cholera data is located.
#'   \item \strong{DOCS_FIGURES}: Path to the directory where the plots will be saved.
#' }
#'
#' @return A list containing the two ggplot objects: one for CFR and total cases, and one for Beta distributions.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar geom_bar scale_fill_manual scale_color_manual coord_flip labs theme_minimal scale_y_sqrt element_text margin theme geom_line scale_x_continuous scale_y_continuous
#' @importFrom cowplot plot_grid
#' @importFrom utils read.csv
#' @importFrom dplyr filter group_by do arrange
#' @examples
#' \dontrun{
#' PATHS <- get_paths()
#' plot_CFR_by_country(PATHS)
#' }
#' @export

plot_CFR_by_country <- function(PATHS) {
     # Load cholera data
     cholera_data <- utils::read.csv(file.path(PATHS$DATA_WHO_ANNUAL, "case_fatality_ratio_2014_2024.csv"), stringsAsFactors = FALSE)

     # Ensure AFRO Region is at the top, followed by other countries in alphabetical order
     cholera_data <- cholera_data %>%
          dplyr::arrange(dplyr::desc(country == "AFRO Region"), country)
     cholera_data$country <- factor(cholera_data$country, levels = c("AFRO Region", sort(unique(cholera_data$country[cholera_data$country != "AFRO Region"]))))

     # Get the number of unique countries in the data
     num_countries <- length(unique(cholera_data$country))

     # Define a color palette that can handle an arbitrary number of countries
     pal <- rev(colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(num_countries))
     names(pal) <- levels(cholera_data$country)
     pal["AFRO Region"] <- "black"  # Set AFRO Region to black

     # Plot CFR with CIs
     p1 <- ggplot2::ggplot(cholera_data, ggplot2::aes(x = country, y = cfr, color = country)) +
          ggplot2::geom_hline(yintercept = cholera_data$cfr[cholera_data$country == "AFRO Region"], linetype = "solid", color = "black", size = 0.25) +
          ggplot2::geom_point(size = 3.5) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin = cfr_lo, ymax = cfr_hi), width = 0, size = 1) +
          ggplot2::scale_color_manual(values = pal) +  # Use the dynamically generated color palette
          ggplot2::scale_x_discrete(limits=rev) +
          ggplot2::coord_flip() +
          ggplot2::labs(title = "A", y = "Case Fatality Ratio (CFR)") +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
                         axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20)),
                         legend.position = 'none')

     # Plot total cases
     p2 <- ggplot2::ggplot(cholera_data, ggplot2::aes(x = country, y = cases_total, fill = country)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::scale_fill_manual(values = pal) +  # Use the dynamically generated color palette
          ggplot2::scale_y_sqrt() +
          ggplot2::scale_x_discrete(limits=rev) +
          ggplot2::coord_flip() +
          ggplot2::labs(title = "B", y = "Total Cases") +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, angle = 45, hjust = 1),
                         legend.position = 'none')

     # Combine the two plots for CFR and total cases
     combined_plot <- cowplot::plot_grid(p1, p2, ncol = 2, align = "h", rel_widths = c(2, 1))

     print(combined_plot)

     # Save the combined CFR and total cases plot
     file_out_combined <- file.path(PATHS$DOCS_FIGURES, "case_fatality_ratio_and_cases_total_by_country.png")
     ggplot2::ggsave(file_out_combined, plot = combined_plot, width = 10, height = 7, dpi = 300)
     message(paste("CFR and total cases plot saved to:", file_out_combined))


     filtered_data <- cholera_data %>%
          dplyr::filter(!is.na(shape1), !is.na(shape2), shape1 > 0, shape2 > 0, country != "AFRO Region")

     # Find the country with the minimum CFR
     cfr_country_min <- as.character(filtered_data$country[which.min(filtered_data$cfr)])

     # Find the country with the maximum CFR
     cfr_country_max <- as.character(filtered_data$country[which.max(filtered_data$cfr)])

     selected_countries <- c("AFRO Region", cfr_country_min, cfr_country_max)  # Customize this list as needed
     x_vals <- seq(0, 1, length.out = 1000)

     # Filter out rows with missing or invalid shape1 and shape2 parameters
     beta_density <- cholera_data %>%
          dplyr::filter(country %in% selected_countries, !is.na(shape1), !is.na(shape2), shape1 > 0, shape2 > 0) %>%
          dplyr::group_by(country) %>%
          dplyr::do(data.frame(x = x_vals, density = dbeta(x_vals, .$shape1, .$shape2), country = .$country))

     if (nrow(beta_density) > 0) {
          p3 <- ggplot2::ggplot(beta_density, ggplot2::aes(x = x, y = density, color = country)) +
               ggplot2::geom_line(size = 1.25) +
               ggplot2::geom_line(data=beta_density[beta_density$country == "AFRO Region",], size = 1.25) +
               ggplot2::scale_color_manual(values = pal[selected_countries]) +
               ggplot2::scale_x_continuous(expand = c(0.005, 0.005)) +
               ggplot2::scale_y_continuous(expand = c(0.005, 0.005)) +
               ggplot2::labs(x = "Case Fatality Ratio (CFR)", y = "Density") +
               ggplot2::theme_minimal() +
               ggplot2::theme(legend.position = "right", legend.title = ggplot2::element_blank())

          print(p3)

          # Save the Beta distributions plot
          file_out_beta <- file.path(PATHS$DOCS_FIGURES, "case_fatality_ratio_beta_distributions.png")
          ggplot2::ggsave(file_out_beta, plot = p3, width = 10, height = 7, dpi = 300)
          message(paste("Beta distributions plot saved to:", file_out_beta))
     } else {
          message("Beta distributions could not be generated: insufficient data for min and max CFR countries.")
     }


}
