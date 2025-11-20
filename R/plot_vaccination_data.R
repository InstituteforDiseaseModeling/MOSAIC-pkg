#' Plot Vaccination Data
#'
#' This function creates plots based on vaccination request data from WHO, GTFCC, or combined sources, displaying both raw and redistributed doses, and highlighting the proportion of the population vaccinated in each country.
#' It produces two types of plots: (1) a faceted plot showing the cumulative vaccination data for all countries, and (2) a detailed plot zooming in on a specific country for a selected date range.
#'
#' @param PATHS A list of file paths, which should include the following elements:
#' \describe{
#'   \item{MODEL_INPUT}{The path to the folder containing the processed vaccination data.}
#'   \item{DOCS_FIGURES}{The path to the folder where the plot images will be saved.}
#' }
#' @param data_source The source of the vaccination data. Must be one of \code{"WHO"}, \code{"GTFCC"}, or \code{"BOTH"}. Default is \code{"WHO"}.
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
#' plot_vaccination_data(PATHS)
#' plot_vaccination_data(PATHS, data_source = "GTFCC")
#' plot_vaccination_data(PATHS, data_source = "BOTH")
#' }
#'
#' @export

plot_vaccination_data <- function(PATHS, data_source = "WHO") {

     # Validate data_source
     if (!data_source %in% c("WHO", "GTFCC", "BOTH")) {
          stop("data_source must be one of: 'WHO', 'GTFCC', or 'BOTH'")
     }

     # Determine the file suffix based on data_source
     suffix <- ifelse(data_source == "BOTH", "GTFCC_WHO", data_source)

     # Load the vaccination data
     data_file <- file.path(PATHS$MODEL_INPUT, glue::glue("data_vaccinations_{suffix}_redistributed.csv"))

     if (!file.exists(data_file)) {
          stop(glue::glue("Vaccination data file not found: {data_file}. Please run est_vaccination_rate() with data_source='{data_source}' first."))
     }

     message(glue::glue("Loading vaccination data from {data_source} source"))
     redistributed_data <- read.csv(data_file, stringsAsFactors = FALSE)
     redistributed_data$date <- as.Date(redistributed_data$date)

     # Also load the raw vaccination data to get doses_shipped
     raw_data_file <- file.path(PATHS$MODEL_INPUT, glue::glue("data_vaccinations_{suffix}.csv"))
     if (file.exists(raw_data_file)) {
          raw_vaccination_data <- read.csv(raw_data_file, stringsAsFactors = FALSE)
          raw_vaccination_data$campaign_date <- as.Date(raw_vaccination_data$campaign_date)
          # Create a simplified dataset with just the doses_shipped info we need for plotting
          doses_shipped_data <- raw_vaccination_data[!is.na(raw_vaccination_data$doses_shipped) & raw_vaccination_data$doses_shipped > 0,
                                                     c('iso_code', 'campaign_date', 'doses_shipped')]
          names(doses_shipped_data)[2] <- 'date'  # Rename campaign_date to date for merging
     } else {
          # If raw file doesn't exist, create empty doses_shipped_data
          doses_shipped_data <- data.frame(iso_code = character(), date = as.Date(character()), doses_shipped = numeric())
     }

     # Extract the last cumulative value and its corresponding proportion for each country

     split_data <- split(redistributed_data, redistributed_data$country)






     # Merge doses_shipped data with redistributed data for plotting
     plot_data <- merge(redistributed_data, doses_shipped_data,
                        by = c('iso_code', 'date'), all.x = TRUE)
     plot_data$doses_shipped[is.na(plot_data$doses_shipped)] <- 0

     get_last_cumsum <- function(df) {
          df_max <- df[df$prop_vaccinated == max(df$prop_vaccinated, na.rm = TRUE), c('country', 'iso_code', 'date', 'doses_distributed_cumulative', 'prop_vaccinated')]
          return(df_max[1, , drop = FALSE])
     }

     last_cumsum_list <- lapply(split_data, get_last_cumsum)
     last_cumsum <- do.call(rbind, last_cumsum_list)
     last_cumsum$date <- max(plot_data$date)
     row.names(last_cumsum) <- NULL

     vax_color <- "#800080"

     # Plot: Simplified plot showing only cumulative doses distributed
     p1 <-
          ggplot2::ggplot(data = plot_data) +
          # Add shaded area under cumulative doses (geom_area)
          ggplot2::geom_area(ggplot2::aes(x = date, y = doses_distributed_cumulative),
                    fill = vax_color, alpha = 0.3) +  # Light shading under the line

          # Cumulative doses as blue line
          ggplot2::geom_line(ggplot2::aes(x = date, y = doses_distributed_cumulative),
                    color = vax_color, linewidth = 0.75) +

          # Add text labels for proportion vaccinated in top left of each facet (grey)
          ggplot2::geom_text(data = last_cumsum,
                    ggplot2::aes(x = min(plot_data$date),
                        y = Inf,
                        label = paste0(scales::percent(prop_vaccinated, accuracy = 0.1), " vaccinated")),
                    hjust = 0, vjust = 1, size = 2.5, color = vax_color) +

          # Add total doses label centered before last date and above the max cumulative line
          ggplot2::geom_text(data = last_cumsum,
                    ggplot2::aes(x = max(plot_data$date),  # Position 180 days before the last date
                        y = Inf,  # Position 10% above the max
                        label = paste0(round(doses_distributed_cumulative/1000000, 1), "M")),
                    hjust = 1, vjust = 1, size = 2.5, color = vax_color) +

          ggplot2::labs(x = "", y = "Cumulative OCV Doses Distributed (millions)") +

          # Make the y-axes independent for each country facet
          ggplot2::facet_wrap(~ country, ncol = 4) +  # Free y-scales for each facet

          ggplot2::theme_minimal() +  # Minimal theme
          ggplot2::theme(strip.text = ggplot2::element_text(size = 10),
                panel.grid.minor = ggplot2::element_blank(),
                axis.title.y = ggplot2::element_text(size = 11),
                axis.text = ggplot2::element_text(size = 9)) +

          # Square root scale with specific breaks in millions
          ggplot2::scale_y_sqrt(
               expand = c(0.02, 0.25),  # Small expansion at bottom (2%), larger at top (15%) for text
               breaks = c(0, 1000000, 5000000, 10000000, 20000000, 40000000),
               labels = c("0", "1M", "5M", "10M", "20M", "40M"))

     print(p1)

     # Save the facet plot to the specified path
     plot_file <- file.path(PATHS$DOCS_FIGURES, "vaccination_by_country.png")
     ggplot2::ggsave(filename = plot_file, plot = p1, width = 8, height = 11, units = "in", dpi = 600)
     message(glue::glue("Vaccination by country plot saved to: {plot_file}"))




     # Country zoomed in plot

     iso <- 'ZMB'
     tmp <- plot_data[plot_data$iso_code == iso &
                       plot_data$date >= as.Date('2024-06-01') &
                       plot_data$date <= as.Date('2024-08-01'),]


     doses_shipped_color <- "#2E7D32"
     doses_distributed_color <- "#A3D9A1"

     p2 <-
          ggplot2::ggplot(data = tmp) +
          # Add shaded area under cumulative doses (geom_area)
          ggplot2::geom_area(ggplot2::aes(x = date, y = doses_distributed_cumulative, group = country),
                    fill = vax_color, alpha = 0.1) +  # Light shading under the line

          # Cumulative doses as blue line for redistributed doses
          ggplot2::geom_line(ggplot2::aes(x = date, y = doses_distributed_cumulative, group = country),
                    color = vax_color, linewidth = 0.75) +

          # Add a horizontal line at y = 0
          ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.75) +
          ggplot2::geom_hline(yintercept = max(tmp$doses_distributed, na.rm = TRUE), color = "black", linewidth = 0.5, linetype = 'dashed') +

          # Raw doses as green bars (only show where doses_shipped > 0)
          ggplot2::geom_bar(data = tmp[tmp$doses_shipped > 0, ],
                   ggplot2::aes(x = date, y = doses_shipped, fill = "Raw Doses"),
                   stat = "identity", color = "black", width = 1) +

          # Redistributed doses as purple bars (only show where doses_distributed > 0)
          ggplot2::geom_bar(data = tmp[tmp$doses_distributed > 0, ],
                   ggplot2::aes(x = date, y = doses_distributed, fill = "Redistributed Doses"),
                   stat = "identity", color = "black", width = 1) +

          # Add red text label for the red bars (Shipped doses on campaign start date)
          # Only add if there are shipped doses
          {if(any(tmp$doses_shipped > 0, na.rm = TRUE))
               ggplot2::geom_text(data = tmp[which.max(tmp$doses_shipped),],
                    ggplot2::aes(x = date, y = doses_shipped,
                        label = "Shipped doses on\ncampaign start date"),
                    hjust = 1.15, vjust = 1, size = 4, color = doses_shipped_color)
          else
               list()} +  # Empty list if no shipped doses

          # Add blue text label for the blue bars (Estimated dose distributed)
          # Only add if there are distributed doses
          {if(any(tmp$doses_distributed > 0, na.rm = TRUE))
               ggplot2::geom_text(data = tmp[which.max(tmp$doses_distributed),],
                    ggplot2::aes(x = date + 19, y = doses_distributed,
                        label = "Estimated doses  \ndistributed per day"),
                    hjust = 1, vjust = -0.5, size = 4, color = 'grey20')
          else
               list()} +  # Empty list if no distributed doses

          # Add text labels for proportion vaccinated
          ggplot2::geom_text(data = tmp[tmp$date == max(tmp$date),],
                    ggplot2::aes(x = max(tmp$date), y = doses_distributed_cumulative,
                        label = scales::percent(prop_vaccinated, accuracy = 0.1)),
                    hjust = 1, vjust = -0.5,
                    size = 3.5, color = vax_color) +

          ggplot2::labs(x = "", y = "Number of OCV Doses") +

          ggplot2::scale_fill_manual(
               values = c("Raw Doses" = doses_shipped_color, "Redistributed Doses" = doses_distributed_color),  # Green for shipped, light green for distributed
               labels = c("Raw Doses", "Redistributed Doses")  # Labels for legend
          ) +

          ggplot2::guides(fill = ggplot2::guide_legend(title = "Dose Type")) +

          ggplot2::theme_minimal() +  # Minimal theme with horizontal grid lines
          ggplot2::theme(strip.text = ggplot2::element_text(size = 10),
                panel.grid.minor = ggplot2::element_blank(),
                legend.position = "bottom",
                axis.title.y = ggplot2::element_text(size=12, margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)),
                axis.title.x = ggplot2::element_text(size=11.5)) +  # Increase gap to y-axis title

          ggplot2::scale_y_sqrt(
               breaks = c(0, 100000, 500000, 1000000, 2000000, 4000000),  # Add gridline at 50,000
               limits = c(0, max(c(tmp$doses_distributed_cumulative, 100000), na.rm = TRUE) * 1.1),  # Set upper limit as 1.1 times max (or min 100000)
               expand = c(0, 0)
          ) +

          ggplot2::scale_x_date(date_labels = "%d %b\n%Y")


     print(p2)

     # Save the facet plot to the specified path
     plot_file <- file.path(PATHS$DOCS_FIGURES, glue::glue("vaccination_example_{iso}.png"))
     ggplot2::ggsave(filename = plot_file, plot = p2, width = 7, height = 4, units = "in", dpi = 600)
     message(glue::glue("Vaccination example plot for {iso} saved to: {plot_file}"))




}
