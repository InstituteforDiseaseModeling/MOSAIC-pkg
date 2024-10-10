#' Plot WHO Vaccination Data
#'
#' This function creates plots based on WHO vaccination request data, displaying both raw and redistributed doses, and highlighting the proportion of the population vaccinated in each country.
#' It produces two types of plots: (1) a faceted plot showing the cumulative vaccination data for all countries, and (2) a detailed plot zooming in on a specific country for a selected date range.
#'
#' @param PATHS A list of file paths, which should include the following elements:
#' \describe{
#'   \item{MODEL_INPUT}{The path to the folder containing the processed vaccination data (`data_vaccinations.csv`).}
#'   \item{DOCS_FIGURES}{The path to the folder where the plot images will be saved.}
#' }
#'
#' @return This function returns no R object but saves two PNG files:
#' \itemize{
#'   \item A faceted plot for vaccination data across all countries saved as \code{"vaccination_by_country.png"}.
#'   \item A zoomed-in plot for a selected country saved as \code{"vaccination_example_<iso_code>.png"}.
#' }
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Loads the processed vaccination data from a CSV file.
#'   \item Extracts the last cumulative value for each country, displaying the maximum proportion of the population vaccinated.
#'   \item Creates a faceted plot showing raw doses shipped and redistributed doses over time, for each country, with independent y-axes.
#'   \item Saves the faceted plot as a PNG file in the specified folder.
#'   \item Creates a zoomed-in plot for a specific country and date range, adding text labels to highlight important milestones.
#'   \item Saves the zoomed-in plot as a PNG file.
#' }
#'
#' @examples
#' \dontrun{
#' PATHS <- list(
#'   MODEL_INPUT = "path/to/input/data",
#'   DOCS_FIGURES = "path/to/save/figures"
#' )
#' plot_WHO_vaccination_data(PATHS)
#' }
#'
#' @export

plot_WHO_vaccination_data <- function(PATHS) {

     # Load the vaccination data
     redistributed_data <- read.csv(file.path(PATHS$MODEL_INPUT, "data_vaccinations.csv"), stringsAsFactors = FALSE)
     redistributed_data$date <- as.Date(redistributed_data$date)

     # Extract the last cumulative value and its corresponding proportion for each country

     split_data <- split(redistributed_data, redistributed_data$country)

     get_last_cumsum <- function(df) {
          df_max <- df[df$prop_vaccinated == max(df$prop_vaccinated, na.rm = TRUE), c('country', 'iso_code', 'date', 'doses_distributed_cumulative', 'prop_vaccinated')]
          return(df_max[1, , drop = FALSE])
     }

     last_cumsum_list <- lapply(split_data, get_last_cumsum)
     last_cumsum <- do.call(rbind, last_cumsum_list)
     row.names(last_cumsum) <- NULL




     # Plot: Combine raw and redistributed data in the same plot
     p1 <-
          ggplot(data = redistributed_data) +
          # Add shaded area under cumulative doses (geom_area)
          geom_area(aes(x = date, y = doses_distributed_cumulative, group = iso_code),
                    fill = "dodgerblue", alpha = 0.1) +  # Light shading under the line

          # Cumulative doses as blue line for redistributed doses
          geom_line(aes(x = date, y = doses_distributed_cumulative, group = iso_code),
                    color = "dodgerblue", size = 0.5) +

          # Add a horizontal line at y = 0
          geom_hline(yintercept = 0, color = "black", size = 0.5) +

          # Raw doses as red bars
          geom_bar(aes(x = date, y = doses_shipped, fill = "Raw Doses"),
                   stat = "identity", position = 'dodge', color = "black", fill = "#e74c3c", width = 20) +

          # Redistributed doses as blue bars
          geom_bar(aes(x = date, y = doses_distributed, fill = "Redistributed Doses"),
                   stat = "identity", position = 'dodge', color = "black", fill = "dodgerblue", width = 20) +

          # Add text labels for proportion vaccinated as "subtitle" for each individual panel
          geom_text(data = last_cumsum,
                    aes(x = min(redistributed_data$date),
                        y = max(doses_distributed_cumulative, na.rm = TRUE),
                        label = paste("Proportion Vaccinated: ",
                                      scales::percent(prop_vaccinated, accuracy = 0.1))),
                    hjust = 0, vjust = 2, size = 3, color = "dodgerblue") +

          labs(x = "", y = "Number of OCV Doses Distributed (square root transform)") +

          # Make the y-axes independent for each country facet
          facet_wrap(~ country, ncol = 2) +  # Facet by country with independent y-axes
          theme_minimal() +  # Minimal theme with horizontal grid lines
          theme(strip.text = element_text(size = 10),
                panel.grid.minor = element_blank(),
                legend.position = "none",
                axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 20, b = 0, l = 0)),
                axis.text = element_text(size=10.5)) +  # Adjust plot margin to the right

          scale_fill_manual(values = c("Raw Doses" = "#e74c3c", "Redistributed Doses" = "dodgerblue")) +
          guides(fill = guide_legend(title = "Dose Type")) +

          # Apply square root scale with manual breaks and remove gap to zero
          scale_y_sqrt(expand = c(0, 0))

     print(p1)

     # Save the facet plot to the specified path
     plot_file <- file.path(PATHS$DOCS_FIGURES, "vaccination_by_country.png")
     ggplot2::ggsave(filename = plot_file, plot = p1, width = 7, height = 10, units = "in", dpi = 300)
     message(glue::glue("Vaccination by country plot saved to: {plot_file}"))




     # Country zoomed in plot

     iso <- 'ZMB'
     tmp <- redistributed_data[redistributed_data$iso_code == iso &
                                    redistributed_data$date >= as.Date('2024-06-01') &
                                    redistributed_data$date <= as.Date('2024-08-01'),]

     p2 <-
          ggplot(data = tmp) +
          # Add shaded area under cumulative doses (geom_area)
          geom_area(aes(x = date, y = doses_distributed_cumulative, group = country),
                    fill = "dodgerblue", alpha = 0.1) +  # Light shading under the line

          # Cumulative doses as blue line for redistributed doses
          geom_line(aes(x = date, y = doses_distributed_cumulative, group = country),
                    color = "dodgerblue", size = 0.75) +

          # Add a horizontal line at y = 0
          geom_hline(yintercept = 0, color = "black", size = 0.75) +
          geom_hline(yintercept = max(tmp$doses_distributed), color = "black", size = 0.5, linetype = 'dashed') +

          # Raw doses as red bars
          geom_bar(aes(x = date, y = doses_shipped, fill = "Raw Doses"),
                   stat = "identity", color = "black", fill = "#e74c3c", width = 1) +

          # Redistributed doses as blue bars
          geom_bar(aes(x = date, y = doses_distributed, fill = "Redistributed Doses"),
                   stat = "identity", color = "black", fill = "dodgerblue", width = 1) +

          # Add red text label for the red bars (Shipped doses on campaign start date)
          geom_text(aes(x = tmp$date[tmp$doses_shipped > 0], y = max(tmp$doses_shipped),
                        label = "Shipped doses on\ncampaign start date"),
                    hjust = 1.15, vjust = 1, size = 4, color = '#e74c3c') +  # Red text for red bars

          # Add blue text label for the blue bars (Estimated dose distributed)
          geom_text(aes(x = max(tmp$date[tmp$doses_distributed > 0]), y = max(tmp$doses_distributed),
                        label = "Estimated doses  \ndistributed per day"),
                    hjust = 1, vjust = -0.5, size = 4, color = 'dodgerblue') +  # Blue text for blue bars

          # Add text labels for proportion vaccinated
          geom_text(data = tmp[tmp$date == max(tmp$date),],
                    aes(x = max(tmp$date), y = doses_distributed_cumulative,
                        label = scales::percent(prop_vaccinated, accuracy = 0.1)),
                    hjust = 1, vjust = -0.5,
                    size = 3.5, color = 'dodgerblue') +

          labs(x = "", y = "Number of OCV Doses") +

          scale_fill_manual(
               values = c("Raw Doses" = "#e74c3c", "Redistributed Doses" = "dodgerblue"),  # Red for raw, blue for redistributed
               labels = c("Raw Doses", "Redistributed Doses")  # Labels for legend
          ) +

          guides(fill = guide_legend(title = "Dose Type")) +

          theme_minimal() +  # Minimal theme with horizontal grid lines
          theme(strip.text = element_text(size = 10),
                panel.grid.minor = element_blank(),
                legend.position = "bottom",
                axis.title.y = element_text(size=12, margin = margin(t = 0, r = 20, b = 0, l = 0)),
                axis.title.x = element_text(size=11.5)) +  # Increase gap to y-axis title

          scale_y_sqrt(
               breaks = c(0, 100000, 500000, 1000000, 2000000, 4000000),  # Add gridline at 50,000
               limits = c(0, max(tmp$doses_distributed_cumulative) * 1.1),  # Set upper limit as 1.1 times max doses_distributed
               expand = c(0, 0)
          ) +

          scale_x_date(date_labels = "%d %b\n%Y")


     print(p2)

     # Save the facet plot to the specified path
     plot_file <- file.path(PATHS$DOCS_FIGURES, glue::glue("vaccination_example_{iso}.png"))
     ggplot2::ggsave(filename = plot_file, plot = p2, width = 7, height = 4, units = "in", dpi = 300)
     message(glue::glue("Vaccination example plot for {iso} saved to: {plot_file}"))




}
