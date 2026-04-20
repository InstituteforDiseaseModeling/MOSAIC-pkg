#' Plot Combined Cholera Surveillance Data
#'
#' This function generates a two-panel plot that visualizes daily cholera cases and deaths
#' for a specific country, based on the combined downscaled surveillance data from WHO, JHU, and SUPP.
#' The top panel shows reported cholera cases and the bottom panel shows reported cholera deaths.
#' Both panels use a log-transformation (with log(count + 1)) while labeling the y-axis with raw counts.
#' Daily data bars are colored by their original source (WHO, JHU, or SUPP), and weekly observations are shown
#' as discrete points without connecting lines.
#'
#' The function reads input CSV files from the directories specified in \code{PATHS}
#' and saves the resulting combined plot as a PNG file in the directory specified by \code{PATHS$DOCS_FIGURES}.
#'
#' @param PATHS A named list of file paths. Required elements include:
#'   \itemize{
#'     \item \code{DATA_CHOLERA_WEEKLY} - Directory containing the combined weekly data file
#'           \code{"cholera_surveillance_weekly_combined.csv"}.
#'     \item \code{DATA_CHOLERA_DAILY} - Directory containing the combined daily data file
#'           \code{"cholera_surveillance_daily_combined.csv"}.
#'     \item \code{DOCS_FIGURES} - Directory in which the output PNG file will be saved.
#'   }
#' @param iso A character string representing the ISO code of the target country (e.g., "ETH").
#'
#' @return Invisibly returns \code{NULL}. The function produces a combined two-panel plot,
#'   which is printed to the active graphics device and saved as a PNG file.
#'
#' @import ggplot2 dplyr patchwork glue
#' @importFrom grDevices png dev.off
#' @importFrom MOSAIC convert_iso_to_country
#' @importFrom lubridate floor_date
#' @export
#'

plot_cholera_surveillance_data <- function(PATHS, iso) {

     # Ensure required packages
     if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' required.")
     if (!requireNamespace("dplyr", quietly = TRUE))   stop("Package 'dplyr' required.")
     if (!requireNamespace("patchwork", quietly = TRUE)) stop("Package 'patchwork' required.")

     # All required packages loaded via NAMESPACE

     # Read combined weekly and daily data
     weekly_file <- file.path(PATHS$DATA_CHOLERA_WEEKLY, "cholera_surveillance_weekly_combined.csv")
     daily_file  <- file.path(PATHS$DATA_CHOLERA_DAILY,  "cholera_surveillance_daily_combined.csv")

     df_w <- utils::read.csv(weekly_file, stringsAsFactors = FALSE)
     df_w$date_start <- as.Date(df_w$date_start)
     df_d <- utils::read.csv(daily_file, stringsAsFactors = FALSE)
     df_d$date <- as.Date(df_d$date)

     # Filter for target ISO
     df_w <- df_w[df_w$iso_code == iso, ]
     df_d <- df_d[df_d$iso_code == iso, ]
     country_name <- MOSAIC::convert_iso_to_country(iso)

     # Match source for daily rows
     df_d$week_start <- lubridate::floor_date(df_d$date, unit = "week", week_start = 1)
     df_d$source     <- df_w$source[match(df_d$week_start, df_w$date_start)]

     # Common scales
     common_x <- ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
     y_vals  <- c(0, 1, 10, 100, 1000, 10000)
     log_brks <- log(y_vals + 1)
     names(log_brks) <- y_vals

     fill_scale <- ggplot2::scale_fill_manual(
          name   = "Source",
          breaks = c("JHU", "WHO", "SUPP"),
          values = c("JHU"  = "#1f78b4",
                     "WHO"  = "#33a02c",
                     "SUPP" = "#ff7f00")
     )

     # Cases plot
     df_d_cases <- df_d[!is.na(df_d$cases), ]
     p1 <- ggplot2::ggplot() +
          ggplot2::geom_col(data = df_d_cases,
                   ggplot2::aes(x = date, y = log(cases + 1), fill = source), color = NA) +
          ggplot2::geom_point(data = df_w,
                     ggplot2::aes(x = date_start, y = log(cases + 1)), color = "black", size = 2) +
          fill_scale +
          ggplot2::scale_y_continuous(breaks = log_brks, labels = names(log_brks), expand = c(0,0)) +
          common_x +
          ggplot2::labs(title = paste("Cholera Cases -", country_name), y = "Cases", x = NULL) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
               panel.grid.minor    = ggplot2::element_blank(),
               panel.grid.major.x  = ggplot2::element_blank(),
               panel.grid.major.y  = ggplot2::element_line(color = "grey80"),
               axis.text.x         = ggplot2::element_text(angle = 90, vjust = 0.5, size = 7)
          )

     # Deaths plot
     df_d_deaths <- df_d[!is.na(df_d$deaths), ]
     p2 <- ggplot2::ggplot() +
          ggplot2::geom_col(data = df_d_deaths,
                   ggplot2::aes(x = date, y = log(deaths + 1), fill = source), color = NA) +
          ggplot2::geom_point(data = df_w,
                     ggplot2::aes(x = date_start, y = log(deaths + 1)), color = "black", size = 2) +
          fill_scale +
          ggplot2::scale_y_continuous(breaks = log_brks, labels = names(log_brks), expand = c(0,0)) +
          common_x +
          ggplot2::labs(title = paste("Cholera Deaths -", country_name), y = "Deaths", x = NULL) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
               panel.grid.minor    = ggplot2::element_blank(),
               panel.grid.major.x  = ggplot2::element_blank(),
               panel.grid.major.y  = ggplot2::element_line(color = "grey80"),
               axis.text.x         = ggplot2::element_text(angle = 90, vjust = 0.5, size = 7)
          )

     # Combine
     combined <- p1 + p2 + patchwork::plot_layout(ncol = 1, guides = "collect") &
          ggplot2::theme(legend.position = "bottom")

     print(combined)
     outfile <- file.path(PATHS$DOCS_FIGURES,
                          glue::glue("cholera_data_downscaled_{iso}.png"))
     grDevices::png(outfile, width = 12, height = 7, units = "in", res = 600)
     print(combined)
     grDevices::dev.off()
     message("Plot saved to: ", outfile)
}
