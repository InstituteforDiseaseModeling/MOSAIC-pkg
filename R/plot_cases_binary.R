library(dplyr)
library(ggplot2)

#' Plot Binary Environmental Suitability Indicator with Case Data
#'
#' This function creates a visualization showing cholera case counts over time with shaded regions
#' indicating periods identified as environmentally suitable for cholera transmission. The environmental
#' suitability is determined using sophisticated temporal logic that considers not only outbreak periods
#' but also lead-up weeks when conditions become favorable.
#'
#' @param PATHS A list containing paths where the data is stored. Must include:
#' \itemize{
#'   \item \strong{DATA_CHOLERA_WEEKLY}: Path to combined weekly cholera surveillance data (WHO+JHU+SUPP)
#'   \item \strong{DOCS_FIGURES}: Path where the generated plot will be saved
#' }
#'
#' @return Invisibly returns the ggplot object after displaying and saving it.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Loads processed suitability data containing pre-computed \code{cases_binary} indicators
#'   \item Uses the existing binary environmental suitability indicators from the main pipeline
#'   \item Generates a multi-panel plot showing case counts as bars with shaded regions
#'         indicating environmentally suitable periods
#'   \item Saves the plot as a PNG file to the figures directory
#' }
#'
#' The shaded regions represent periods identified as environmentally suitable by the
#' \code{process_suitability_data()} pipeline, including both outbreak periods and
#' lead-up weeks when conditions become favorable.
#'
#' @examples
#' \dontrun{
#' PATHS <- get_paths()
#' plot_cases_binary(PATHS)
#' }
#'
#' @seealso
#' \code{\link{process_suitability_data}} for the main pipeline that creates the processed data with \code{cases_binary}.
#' \code{\link{get_cases_binary}} for details on how the environmental suitability indicator is created.
#'
#' @export
plot_cases_binary <- function(PATHS) {

     require(dplyr)

     # Load processed suitability data with pre-computed cases_binary indicators
     message("Loading processed suitability data...")
     case_data <- utils::read.csv(file.path(PATHS$DATA_CHOLERA_WEEKLY, "cholera_country_weekly_suitability_data.csv"), stringsAsFactors = FALSE)

     case_data$date <- as.Date(case_data$date)

     # Verify that cases_binary column exists
     if (!"cases_binary" %in% names(case_data)) {
          stop("cases_binary column not found in processed suitability data. Please run process_suitability_data() first.")
     }

     # Report data summary
     message(sprintf("Loaded processed data: %d total observations, %d with case data, %d with missing cases",
                     nrow(case_data), sum(!is.na(case_data$cases)), sum(is.na(case_data$cases))))
     message(sprintf("Binary indicators: %d suitable periods, %d with NA binary indicator",
                     sum(case_data$cases_binary == 1, na.rm = TRUE), sum(is.na(case_data$cases_binary))))
     message(sprintf("Full dataset date range: %s to %s",
                     min(case_data$date, na.rm = TRUE), max(case_data$date, na.rm = TRUE)))
     message(sprintf("Case data available from: %s to %s",
                     min(case_data$date[!is.na(case_data$cases)], na.rm = TRUE),
                     max(case_data$date[!is.na(case_data$cases)], na.rm = TRUE)))

     # Filter out NA cases AFTER loading for plotting purposes only
     case_data_for_plotting <- case_data[!is.na(case_data$cases), ]

     # Remove countries with all zero or missing case data
     countries_with_data <- case_data_for_plotting %>%
          group_by(iso_code) %>%
          summarize(
               total_cases = sum(cases, na.rm = TRUE),
               max_cases = max(cases, na.rm = TRUE),
               n_nonzero = sum(cases > 0, na.rm = TRUE),
               .groups = 'drop'
          ) %>%
          filter(total_cases > 0 & n_nonzero > 0) %>%
          pull(iso_code)

     message(sprintf("Countries with case data: %d out of %d total countries",
                     length(countries_with_data), length(unique(case_data$iso_code))))
     message(sprintf("Excluded countries (no cases): %s",
                     paste(setdiff(unique(case_data$iso_code), countries_with_data), collapse = ", ")))

     # Filter both datasets to only include countries with actual case data
     case_data <- case_data[case_data$iso_code %in% countries_with_data, ]
     case_data_for_plotting <- case_data_for_plotting[case_data_for_plotting$iso_code %in% countries_with_data, ]

     # Group by country and create a new grouping for consecutive cases_binary == 1
     # Use the FULL dataset (including NAs) for shaded region calculation
     case_data_full <- case_data %>%
          group_by(iso_code) %>%
          mutate(
               # Handle NAs properly by treating them as a distinct state (neither 0 nor 1)
               cases_binary_clean = ifelse(is.na(cases_binary), -1, cases_binary),
               # Create groups for consecutive periods with same binary status
               group = cumsum(c(1, diff(cases_binary_clean) != 0))
          ) %>%
          ungroup()

     # Filter only periods where cases_binary == 1 from the FULL dataset
     shaded_data <- case_data_full %>%
          filter(cases_binary == 1 & !is.na(cases_binary)) %>%
          group_by(iso_code, group) %>%
          summarize(date_start = min(date), date_stop = max(date), .groups = 'drop') %>%
          mutate(
               # Add 3.5 days to start and end for weekly data visualization
               date_start = date_start - 3.5,
               date_stop = date_stop + 3.5
          )

     # Define colors for cases and shaded regions
     color_cases <- "#152F45"  # Navy Blue for bars (cases)
     color_shaded <- "#B0E0E6"  # Light Blue for shaded regions

     # Define date limits for x-axis based on actual case data availability
     # Use only periods with case data (not the full climate dataset range)
     date_min <- min(case_data_for_plotting$date, na.rm = TRUE)
     date_max <- max(case_data_for_plotting$date, na.rm = TRUE)

     # Calculate appropriate date breaks to have maximum 30 labels
     date_range_months <- as.numeric(difftime(date_max, date_min, units = "days")) / 30.44
     target_breaks <- min(30, ceiling(date_range_months / 2))  # Max 30 labels
     break_interval <- ceiling(date_range_months / target_breaks)
     date_break_string <- paste0(break_interval, " months")

     message(sprintf("Plotting date range: %s to %s", date_min, date_max))
     message(sprintf("Using date breaks: %s (approximately %d labels)", date_break_string, target_breaks))
     message(sprintf("Shaded regions created: %d environmentally suitable periods", nrow(shaded_data)))

     # Check if epidemic peaks data was used for cases_binary creation
     if (any(grepl("epidemic", names(case_data), ignore.case = TRUE))) {
          message("Environmental suitability based on epidemic peak analysis from est_epidemic_peaks()")
     } else {
          message("Environmental suitability based on case threshold method")
     }

     # Create the plot using case_data
     p <- ggplot2::ggplot() +

          # Shaded regions for binary cases (continuous regions)
          ggplot2::geom_rect(data = shaded_data,
                             ggplot2::aes(xmin = date_start, xmax = date_stop, ymin = 0, ymax = Inf),
                             fill = color_shaded, color = color_shaded) +  # Shaded regions with transparency

          # Bar plot for cases (use filtered data that excludes NAs for visual clarity)
          ggplot2::geom_bar(data = case_data_for_plotting,
                            ggplot2::aes(x = date, y = cases), stat = "identity", fill = color_cases, color = color_cases) +

          # Facet wrap by country with free y-axis scales
          ggplot2::facet_wrap(~ iso_code, ncol = 2, scales = "free_y") +

          # Labels and axis formatting
          ggplot2::labs(x = NULL, y = "Number of Cases") +
          ggplot2::scale_y_continuous(expand = c(0, 0)) +  # No extra space around bars
          ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = date_break_string, expand = c(0, 0),
                                limits = c(date_min-14, date_max+14)) +  # Set date limits
          ggplot2::theme_minimal() +
          ggplot2::theme(
               axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1, size = 9),  # Rotate x-axis labels
               axis.title.y = ggplot2::element_text(size = 12, margin = margin(r = 10)),
               panel.grid.major.x = ggplot2::element_blank(),
               panel.grid.minor = ggplot2::element_blank(),
               axis.ticks.x = ggplot2::element_line(),   # Add x-axis ticks
               axis.line.x = ggplot2::element_line(),
               plot.background = ggplot2::element_rect(fill = "white", color = NA),  # White background
               panel.background = ggplot2::element_rect(fill = "white", color = NA)  # White panel background
          )

     print(p)

     # Save the plot to the specified path
     plot_file <- file.path(PATHS$DOCS_FIGURES, glue::glue("cases_binary.png"))
     ggplot2::ggsave(filename = plot_file, plot = p, width = 12, height = 16, units = "in", dpi = 600)
     message(glue::glue("Plot saved to: {plot_file}"))

     # Return the plot object invisibly
     return(invisible(p))
}
