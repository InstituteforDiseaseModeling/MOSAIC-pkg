#' Plot Downscaled Cholera Data
#'
#' This function generates a two-panel plot that visualizes daily cholera cases and deaths
#' for a specific country, based on downscaled weekly and daily data. The top panel shows
#' reported cholera cases and the bottom panel shows reported cholera deaths. Both panels
#' use a log-transformation (with a log(cases + 1) and log(deaths + 1) scale) while the y-axis
#' is labeled with raw counts. Data for the specified country are filtered by its ISO code,
#' and the function converts the ISO code to the country name using \code{MOSAIC::convert_iso_to_country}.
#'
#' The function reads input CSV files from the directories specified in \code{PATHS} and saves
#' the resulting combined plot as a PNG file in the directory specified by \code{PATHS$DOCS_FIGURES}.
#'
#' @param PATHS A named list of file paths. Required elements include:
#'   \itemize{
#'     \item \code{DATA_WHO_WEEKLY} - Directory containing the weekly data file
#'           \code{"cholera_country_weekly_processed.csv"}.
#'     \item \code{DATA_WHO_DAILY} - Directory containing the daily data file
#'           \code{"cholera_country_daily_processed.csv"}.
#'     \item \code{DOCS_FIGURES} - Directory in which the output PNG file will be saved.
#'   }
#' @param iso A character string representing the ISO code of the target country (e.g., "ETH").
#'
#' @return Invisibly returns \code{NULL}. The function produces a combined two-panel plot, which is printed
#'   to the active graphics device and saved as a PNG file.
#'
#' @details The function performs the following steps:
#'   \enumerate{
#'     \item Reads in weekly and daily CSV files and converts date strings to \code{Date} objects.
#'     \item Filters the data for the target country using its ISO code.
#'     \item Separates the valid and missing daily entries for both cases and deaths.
#'     \item Constructs two ggplot2 objects: one for cases (upper panel) and one for deaths (lower panel),
#'           applying a log-transformation with custom y-axis breaks and labels.
#'     \item Combines the two panels vertically using the \code{patchwork} package and saves the plot as a PNG file.
#'   }
#'
#' @examples
#' \dontrun{
#'   PATHS <- list(
#'     DATA_WHO_WEEKLY = "path/to/weekly/data",
#'     DATA_WHO_DAILY = "path/to/daily/data",
#'     DOCS_FIGURES = "path/to/output/figures"
#'   )
#'   # Plot data for Ethiopia (ISO code "ETH")
#'   plot_downscaled_cholera_data(PATHS, iso = "ETH")
#' }
#'
#' @import ggplot2 dplyr patchwork glue
#' @importFrom grDevices png dev.off
#' @importFrom MOSAIC convert_iso_to_country
#' @export

plot_downscaled_cholera_data <- function(PATHS, iso) {

     # Load required namespaces
     if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
     if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
     if (!requireNamespace("patchwork", quietly = TRUE)) stop("Package 'patchwork' is required for arranging plots with a shared legend.")

     library(ggplot2)
     library(dplyr)

     # Read in the weekly and daily data
     df_weekly <- read.csv(file.path(PATHS$DATA_WHO_WEEKLY, "cholera_country_weekly_processed.csv"))
     df_weekly$date_start <- as.Date(df_weekly$date_start)

     df_daily <- read.csv(file.path(PATHS$DATA_WHO_DAILY, "cholera_country_daily_processed.csv"))
     df_daily$date <- as.Date(df_daily$date)

     # Choose the country for plotting (ISO code)
     target_iso <- iso
     target_country_name <- MOSAIC::convert_iso_to_country(iso)

     # Filter data for the target country
     df_daily_iso <- df_daily %>% filter(iso_code == target_iso)
     df_weekly_iso <- df_weekly %>%
          filter(iso_code == target_iso) %>%
          mutate(data_type = "Weekly")

     # For daily data, separate valid (non-NA) entries and missing entries for cases and deaths.
     # Cases:
     df_daily_cases_valid <- df_daily_iso %>% filter(!is.na(cases)) %>% mutate(data_type = "Daily")
     df_daily_cases_missing <- df_daily_iso %>% filter(is.na(cases)) %>% mutate(data_type = "Missing")

     # Deaths:
     df_daily_deaths_valid <- df_daily_iso %>% filter(!is.na(deaths)) %>% mutate(data_type = "Daily")
     df_daily_deaths_missing <- df_daily_iso %>% filter(is.na(deaths)) %>% mutate(data_type = "Missing")

     # Ensure the x-axis date range is consistent across panels
     common_x <- scale_x_date(
          date_breaks = "1 month",
          date_labels = "%b %Y"
     )

     # Y-axis ticks (log scale)
     log_y_breaks_cases <- log_y_breaks_deaths <- log(c(0, 1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000) + 1)
     y_labels_cases <- round(exp(log_y_breaks_cases) - 1)
     y_labels_deaths <- round(exp(log_y_breaks_deaths) - 1)

     # Define common scales for both panels so that the legend is shared
     common_fill <- scale_fill_manual(values = c("Daily" = "dodgerblue", "Missing" = "grey85"),
                                      guide = guide_legend(title = NULL))
     common_color <- scale_color_manual(values = c("Weekly" = "black"),
                                        guide = guide_legend(title = NULL))
     common_shape <- scale_shape_manual(values = c("Weekly" = 19),
                                        guide = guide_legend(title = NULL))

     # Create the cases plot (upper panel)
     p_cases <- ggplot() +
          # Missing data as background tiles for cases
          geom_tile(data = df_daily_cases_missing,
                    aes(x = date, y = 0, fill = data_type),
                    height = Inf, width = 1) +
          # Daily cases bars (log-transformed)
          geom_col(data = df_daily_cases_valid,
                   aes(x = date, y = log(cases + 1), fill = data_type),
                   color = NA, alpha = 0.9) +
          # Weekly overlay for cases (line and points)
          geom_line(data = df_weekly_iso,
                    aes(x = date_start, y = log(cases + 1), color = data_type),
                    linewidth = 0.5) +
          geom_point(data = df_weekly_iso,
                     aes(x = date_start, y = log(cases + 1), color = data_type, shape = data_type),
                     size = 2) +
          common_fill +
          common_color +
          common_shape +
          scale_y_continuous(
               breaks = log_y_breaks_cases,
               labels = y_labels_cases,
               expand = c(0, 0)
          ) +
          common_x +
          labs(
               title = paste("Reported Cholera Cases in", target_country_name),
               x = NULL,
               y = "Case Count"
          ) +
          theme_minimal(base_size = 11) +
          theme(
               panel.grid.minor.x = element_blank(),
               panel.grid.major.x = element_blank(),
               panel.grid.minor.y = element_blank(),
               axis.line = element_line(color = "black", linewidth = 0.6),
               axis.ticks = element_line(color = "black"),
               axis.text.x = element_text(angle = 45, hjust = 1)
          )

     # Create the deaths plot (lower panel)
     p_deaths <- ggplot() +
          # Missing data as background tiles for deaths
          geom_tile(data = df_daily_deaths_missing,
                    aes(x = date, y = 0, fill = data_type),
                    height = Inf, width = 1) +
          # Daily deaths bars (log-transformed)
          geom_col(data = df_daily_deaths_valid,
                   aes(x = date, y = log(deaths + 1), fill = data_type),
                   color = NA, alpha = 0.9) +
          # Weekly overlay for deaths (line and points)
          geom_line(data = df_weekly_iso,
                    aes(x = date_start, y = log(deaths + 1), color = data_type),
                    linewidth = 0.5) +
          geom_point(data = df_weekly_iso,
                     aes(x = date_start, y = log(deaths + 1), color = data_type, shape = data_type),
                     size = 2) +
          common_fill +
          common_color +
          common_shape +
          scale_y_continuous(
               breaks = log_y_breaks_deaths,
               labels = y_labels_deaths,
               expand = c(0, 0)
          ) +
          common_x +
          labs(
               title = paste("Reported Cholera Deaths in", target_country_name),
               x = NULL,
               y = "Death Count"
          ) +
          theme_minimal(base_size = 11) +
          theme(
               panel.grid.minor.x = element_blank(),
               panel.grid.major.x = element_blank(),
               panel.grid.minor.y = element_blank(),
               axis.line = element_line(color = "black", linewidth = 0.6),
               axis.ticks = element_line(color = "black"),
               axis.text.x = element_text(angle = 45, hjust = 1)
          )

     # Combine the two plots vertically with a shared legend.
     # The guides = "collect" option gathers the legends from both plots and
     # the patchwork theme sets the combined legend at the bottom.
     combined_plot <- p_cases + p_deaths +
          patchwork::plot_layout(ncol = 1, guides = "collect") &
          theme(legend.position = "bottom")

     # Print the combined plot
     print(combined_plot)

     output_file <- file.path(PATHS$DOCS_FIGURES, glue::glue("cholera_data_downscaled_{iso}.png"))
     grDevices::png(filename = output_file, width = 9, height = 6, units = "in", res = 600)
     print(combined_plot)
     grDevices::dev.off()

     message(glue::glue("Downscaled cholera data plot saved to: {output_file}"))


}
