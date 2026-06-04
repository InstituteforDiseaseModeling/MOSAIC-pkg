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

     if (!requireNamespace("ggplot2",   quietly = TRUE)) stop("Package 'ggplot2' required.")
     if (!requireNamespace("dplyr",     quietly = TRUE)) stop("Package 'dplyr' required.")
     if (!requireNamespace("patchwork", quietly = TRUE)) stop("Package 'patchwork' required.")

     weekly_file <- file.path(PATHS$DATA_CHOLERA_WEEKLY, "cholera_surveillance_weekly_combined.csv")
     daily_file  <- file.path(PATHS$DATA_CHOLERA_DAILY,  "cholera_surveillance_daily_combined.csv")

     df_w <- utils::read.csv(weekly_file, stringsAsFactors = FALSE)
     df_w$date_start <- as.Date(df_w$date_start)
     df_d <- utils::read.csv(daily_file, stringsAsFactors = FALSE)
     df_d$date <- as.Date(df_d$date)

     df_w <- df_w[df_w$iso_code == iso, ]
     df_d <- df_d[df_d$iso_code == iso, ]
     country_name <- MOSAIC::convert_iso_to_country(iso)

     df_d$week_start <- lubridate::floor_date(df_d$date, unit = "week", week_start = 1)
     df_d$source     <- df_w$source[match(df_d$week_start, df_w$date_start)]

     # ---- Shared scales -------------------------------------------------------

     y_vals   <- c(0, 1, 10, 100, 1000, 10000)
     log_brks <- setNames(log(y_vals + 1), as.character(y_vals))

     src_colors <- c(JHU = "#2166ac", WHO = "#4dac26", SUPP = "#d6604d")

     fill_scale <- ggplot2::scale_fill_manual(
          name   = "Daily (downscaled)",
          breaks = c("JHU", "WHO", "SUPP"),
          values = src_colors
     )
     # Adds "Weekly observed" to the legend as a point symbol
     color_scale <- ggplot2::scale_color_manual(
          name   = NULL,
          values = c("Weekly observed" = "grey15"),
          guide  = ggplot2::guide_legend(
               override.aes = list(shape = 19, size = 2.5, linetype = 0)
          )
     )

     common_x <- ggplot2::scale_x_date(
          date_breaks       = "1 year",
          date_minor_breaks = "3 months",
          date_labels       = "%Y",
          expand            = ggplot2::expansion(mult = 0.01)
     )

     panel_theme <- MOSAIC::theme_mosaic() +
          ggplot2::theme(
               axis.text.x        = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
               axis.title.y       = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
               panel.grid.minor   = ggplot2::element_blank(),
               panel.grid.major.x = ggplot2::element_blank(),
               panel.grid.major.y = ggplot2::element_line(color = "grey85", linewidth = 0.35)
          )

     # ---- Cases panel ---------------------------------------------------------

     df_d_cases <- df_d[!is.na(df_d$cases), ]
     df_w_cases <- df_w[!is.na(df_w$cases), ]

     p1 <- ggplot2::ggplot() +
          ggplot2::geom_col(
               data  = df_d_cases,
               ggplot2::aes(x = date, y = log(cases + 1), fill = source),
               color = NA, width = 1
          ) +
          ggplot2::geom_point(
               data  = df_w_cases,
               ggplot2::aes(x = date_start, y = log(cases + 1), color = "Weekly observed"),
               size = 1.8, shape = 19, alpha = 0.85
          ) +
          fill_scale + color_scale +
          ggplot2::scale_y_continuous(
               name   = "Cases",
               breaks = log_brks,
               labels = names(log_brks),
               expand = ggplot2::expansion(mult = c(0, 0.05))
          ) +
          common_x +
          ggplot2::labs(title = "Cases", x = NULL) +
          panel_theme

     # ---- Deaths panel --------------------------------------------------------

     df_d_deaths <- df_d[!is.na(df_d$deaths), ]
     df_w_deaths <- df_w[!is.na(df_w$deaths), ]

     p2 <- ggplot2::ggplot() +
          ggplot2::geom_col(
               data  = df_d_deaths,
               ggplot2::aes(x = date, y = log(deaths + 1), fill = source),
               color = NA, width = 1
          ) +
          ggplot2::geom_point(
               data  = df_w_deaths,
               ggplot2::aes(x = date_start, y = log(deaths + 1), color = "Weekly observed"),
               size = 1.8, shape = 19, alpha = 0.85
          ) +
          fill_scale + color_scale +
          ggplot2::scale_y_continuous(
               name   = "Deaths",
               breaks = log_brks,
               labels = names(log_brks),
               expand = ggplot2::expansion(mult = c(0, 0.05))
          ) +
          common_x +
          ggplot2::labs(title = "Deaths", x = NULL) +
          panel_theme

     # ---- Combine -------------------------------------------------------------

     annot_theme <- MOSAIC::theme_mosaic() +
          ggplot2::theme(
               plot.title    = ggplot2::element_text(size = 14, face = "bold"),
               plot.subtitle = ggplot2::element_text(size = 9,  colour = "grey40"),
               plot.margin   = ggplot2::margin(8, 8, 4, 8)
          )

     combined <- (p1 / p2) +
          patchwork::plot_layout(guides = "collect") +
          patchwork::plot_annotation(
               title    = paste0(country_name, " — Cholera Surveillance"),
               subtitle = "Bars: daily downscaled counts by source  ·  Points: weekly observed counts (log₁₀ scale)",
               theme    = annot_theme
          ) &
          ggplot2::theme(legend.position = "bottom")

     outfile <- file.path(PATHS$DOCS_FIGURES,
                          glue::glue("cholera_data_downscaled_{iso}.png"))
     ggplot2::ggsave(outfile, combined, width = 12, height = 7, dpi = 300)
     message("Plot saved to: ", outfile)
}
