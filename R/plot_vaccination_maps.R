#' Plot Vaccination Maps
#'
#' Generates choropleth maps and corresponding bar plots to visualize OCV vaccination data from WHO, GTFCC, or combined sources across African countries.
#'
#' @param PATHS A named list containing file paths required for input and output operations. It should include:
#' \describe{
#'   \item{`MODEL_INPUT`}{Directory path where the processed vaccination data files are located.}
#'   \item{`DOCS_FIGURES`}{Directory path where the generated plots will be saved.}
#' }
#' @param data_source The source of the vaccination data. Must be one of \code{"WHO"}, \code{"GTFCC"}, or \code{"BOTH"}. Default is \code{"WHO"}.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item **Data Loading and Processing**: Reads the vaccination data from a CSV file, converts the `date` column to `Date` objects, and extracts the latest cumulative doses distributed and corresponding vaccination proportions for each country.
#'   \item **Map Data Retrieval**: Obtains map data for African countries using the `rnaturalearth` package.
#'   \item **Data Merging**: Merges the vaccination data with the map data, identifying Sub-Saharan African countries based on ISO codes provided by the `MOSAIC::iso_codes_ssa` dataset.
#'   \item **Color Scale Definition**: Defines distinct color scales for cumulative doses distributed (green gradient) and proportion vaccinated (purple gradient), including appropriate breaks and labels.
#'   \item **Choropleth Map Creation**: Creates two choropleth maps—one for cumulative doses distributed and another for the proportion vaccinated—using `ggplot2` and layering map data accordingly.
#'   \item **Barplot Generation**: Constructs barplots for each metric, ordering countries by descending values and applying the corresponding color scales.
#'   \item **Plot Arrangement**: Utilizes the `patchwork` and `cowplot` packages to arrange the maps and barplots into a structured grid with panel labels "A" and "B" and adds a footnote.
#'   \item **Saving the Plot**: Saves the final combined plot as a PNG file in the specified `DOCS_FIGURES` directory.
#' }
#'
#' @return
#' The function does not return any value. It generates and saves a PNG file containing the vaccination maps and barplots with annotations.
#'
#' @import ggplot2
#' @import dplyr
#' @import sf
#' @import rnaturalearth
#' @import rnaturalearthdata
#' @import patchwork
#' @import viridis
#' @import grid
#' @import gtable
#' @import cowplot
#' @import glue
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Define the PATHS list with appropriate directories
#' PATHS <- list(
#'   MODEL_INPUT = "path/to/model/input",
#'   DOCS_FIGURES = "path/to/docs/figures"
#' )
#'
#' # Call the function to generate and save the vaccination maps
#' plot_vaccination_maps(PATHS)
#' plot_vaccination_maps(PATHS, data_source = "GTFCC")
#' plot_vaccination_maps(PATHS, data_source = "BOTH")
#' }
#'

plot_vaccination_maps <- function(PATHS, data_source = "WHO") {

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

     # Extract the last cumulative value and its corresponding proportion for each country

     split_data <- split(redistributed_data, redistributed_data$country)

     get_last_cumsum <- function(df) {
          df_max <- df[df$prop_vaccinated == max(df$prop_vaccinated, na.rm = TRUE), c('country', 'iso_code', 'date', 'doses_distributed_cumulative', 'prop_vaccinated')]
          return(df_max[1, , drop = FALSE])
     }

     last_cumsum_list <- lapply(split_data, get_last_cumsum)
     vaccination_data <- do.call(rbind, last_cumsum_list)
     row.names(vaccination_data) <- NULL


     # Step 2: Retrieve Map Data for Africa
     africa_map <- rnaturalearth::ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

     # Step 3: Merge Vaccination Data with Map Data
     # Define SubSaharan Africa based on 'subregion'
     africa_vax_map <- africa_map %>%
          dplyr::mutate(
               iso_a3 = toupper(iso_a3),
               is_subsaharan = iso_a3 %in% MOSAIC::iso_codes_ssa
          ) %>%
          dplyr::left_join(vaccination_data, by = c("iso_a3" = "iso_code"))

     # Step 4: Define Scale Breaks and Labels

     # Dynamic Scale Breaks and Labels for doses_distributed_cumulative
     # Use breaks every 10M for cleaner x-axis
     doses_range <- range(vaccination_data$doses_distributed_cumulative, na.rm = TRUE)
     max_doses <- max(vaccination_data$doses_distributed_cumulative, na.rm = TRUE)
     # Create breaks at 0, 10M, 20M, 30M, etc up to ceiling of max
     scale_breaks_doses <- seq(0, ceiling(max_doses/10000000) * 10000000, by = 10000000)
     # Format labels as millions
     scale_labels_doses <- ifelse(scale_breaks_doses == 0, "0",
                                  paste0(round(scale_breaks_doses/1000000), "M"))

     # Dynamic Scale Breaks and Labels for prop_vaccinated
     # Use breaks every 25% and extend max to avoid cutoff
     prop_range <- range(vaccination_data$prop_vaccinated, na.rm = TRUE)
     max_prop <- max(vaccination_data$prop_vaccinated, na.rm = TRUE)
     # Create breaks at 0%, 25%, 50%, 75%, etc up to max * 1.1
     scale_breaks_prop <- seq(0, ceiling(max_prop * 1.1 / 0.25) * 0.25, by = 0.25)
     scale_labels_prop <- paste0(round(scale_breaks_prop * 100), "%")

     # Step 5: Define Color Scales

     # Color Scale for doses_distributed_cumulative (Green Gradient)
     color_scale_doses <- ggplot2::scale_fill_gradient(
          name = "Cumulative Doses\nDistributed",  # Legend title with line break
          low = "#ECFFDC",                            # Light tan
          high = "#097969",                           # Dark green
          na.value = "grey95",                        # Non-SubSaharan Africa countries filled with grey95
          breaks = scale_breaks_doses,                # Breaks matching barplot's x-axis
          labels = scale_labels_doses,                # Labels for the breaks
          limits = c(0, max(scale_breaks_doses))  # Set limits to match scale breaks
     )

     # Color Scale for prop_vaccinated (Purple Gradient)
     color_scale_prop <- ggplot2::scale_fill_gradient(
          name = "Proportion\nVaccinated",            # Legend title with line break
          low = "#E6E6FA",                            # Lavender (light purple)
          high = "#800080",                           # Purple (dark purple)
          na.value = "grey95",                        # Non-SubSaharan Africa countries filled with grey95
          breaks = scale_breaks_prop,                  # Breaks matching barplot's x-axis
          labels = scale_labels_prop,                  # Labels for the breaks
          limits = c(0, max(scale_breaks_prop))  # Set limits to match scale breaks
     )

     # Step 6: Create the Choropleth Maps

     # Choropleth Map for doses_distributed_cumulative
     map_plot_doses <-
          ggplot2::ggplot() +
          # Layer 1: Non-SubSaharan Africa countries filled with white
          ggplot2::geom_sf(data = africa_vax_map %>% dplyr::filter(!is_subsaharan), fill = "white", color = "grey50") +

          # Layer 2: SubSaharan Africa countries without vaccination data filled with grey95
          ggplot2::geom_sf(data = africa_vax_map %>% dplyr::filter(is_subsaharan & is.na(doses_distributed_cumulative)),
                  fill = "grey95", color = "black") +

          # Layer 3: SubSaharan Africa countries with vaccination data filled using the color scale
          ggplot2::geom_sf(data = africa_vax_map %>% dplyr::filter(is_subsaharan & !is.na(doses_distributed_cumulative)),
                  ggplot2::aes(fill = doses_distributed_cumulative), color = "black") +

          # Apply the custom color scale
          color_scale_doses +

          # Adjust the coordinate system to prevent cutting off the top of Africa
          ggplot2::coord_sf(
               xlim = c(-20, 55),   # Longitude limits
               ylim = c(-35, 40),    # Latitude limits to fully display Africa's top
               expand = FALSE
          ) +

          # Minimalist theme with customized legend
          ggplot2::theme_minimal() +
          ggplot2::theme(
               legend.position = "left",             # Position legend on the left
               legend.direction = "vertical",
               legend.key.height = grid::unit(6, "cm"),    # Adjust legend key height
               legend.key.width = grid::unit(0.25, "cm"),  # Adjust legend key width
               legend.title = ggplot2::element_text(size = 10),
               legend.text = ggplot2::element_text(size = 8),
               axis.text = ggplot2::element_blank(),
               axis.title = ggplot2::element_blank(),
               panel.grid = ggplot2::element_blank()
          ) +

          # Customize the colorbar guide
          ggplot2::guides(
               fill = ggplot2::guide_colorbar(
                    barwidth = grid::unit(0.25, "cm"),   # Narrow width
                    barheight = grid::unit(6, "cm"),     # Tall height
                    title.position = "top",
                    title.hjust = 0.5,
                    label.position = "left"         # Labels on the left side of the colorbar
               )
          )

     # Choropleth Map for prop_vaccinated
     map_plot_prop <-
          ggplot2::ggplot() +
          # Layer 1: Non-SubSaharan Africa countries filled with white
          ggplot2::geom_sf(data = africa_vax_map %>% dplyr::filter(!is_subsaharan), fill = "white", color = "grey50") +

          # Layer 2: SubSaharan Africa countries without vaccination data filled with grey95
          ggplot2::geom_sf(data = africa_vax_map %>% dplyr::filter(is_subsaharan & is.na(prop_vaccinated)),
                  fill = "grey95", color = "black") +

          # Layer 3: SubSaharan Africa countries with vaccination data filled using the color scale
          ggplot2::geom_sf(data = africa_vax_map %>% dplyr::filter(is_subsaharan & !is.na(prop_vaccinated)),
                  ggplot2::aes(fill = prop_vaccinated), color = "black") +

          # Apply the custom color scale
          color_scale_prop +

          # Adjust the coordinate system to prevent cutting off the top of Africa
          ggplot2::coord_sf(
               xlim = c(-20, 55),   # Longitude limits
               ylim = c(-35, 40),    # Latitude limits to fully display Africa's top
               expand = FALSE
          ) +

          # Minimalist theme with customized legend
          ggplot2::theme_minimal() +
          ggplot2::theme(
               legend.position = "left",             # Position legend on the left
               legend.direction = "vertical",
               legend.key.height = grid::unit(6, "cm"),    # Adjust legend key height
               legend.key.width = grid::unit(0.25, "cm"),  # Adjust legend key width
               legend.title = ggplot2::element_text(size = 10),
               legend.text = ggplot2::element_text(size = 8),
               axis.text = ggplot2::element_blank(),
               axis.title = ggplot2::element_blank(),
               panel.grid = ggplot2::element_blank()
          ) +

          # Customize the colorbar guide
          ggplot2::guides(
               fill = ggplot2::guide_colorbar(
                    barwidth = grid::unit(0.25, "cm"),   # Narrow width
                    barheight = grid::unit(6, "cm"),     # Tall height
                    title.position = "top",
                    title.hjust = 0.5,
                    label.position = "left"         # Labels on the left side of the colorbar
               )
          )

     # Step 7: Prepare Data for Barplots

     # Barplot for doses_distributed_cumulative
     # Replace NAs with 0 and sort properly
     bar_data_doses <- vaccination_data
     bar_data_doses$doses_distributed_cumulative[is.na(bar_data_doses$doses_distributed_cumulative)] <- 0
     bar_data_doses <- bar_data_doses %>%
          dplyr::arrange(doses_distributed_cumulative) %>%  # Arrange ascending first
          dplyr::mutate(country = factor(country, levels = country))  # Set factor levels in ascending order

     # Barplot for prop_vaccinated
     # Replace NAs with 0 and sort properly
     bar_data_prop <- vaccination_data
     bar_data_prop$prop_vaccinated[is.na(bar_data_prop$prop_vaccinated)] <- 0
     bar_data_prop <- bar_data_prop %>%
          dplyr::arrange(prop_vaccinated) %>%  # Arrange ascending first
          dplyr::mutate(country = factor(country, levels = country))  # Set factor levels in ascending order

     # Step 8: Create the Barplots

     # Barplot for doses_distributed_cumulative
     bar_plot_doses <-
          ggplot2::ggplot(bar_data_doses, ggplot2::aes(x = doses_distributed_cumulative, y = country)) +
          ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = doses_distributed_cumulative)) +
          color_scale_doses +  # Use the same color scale for consistency
          ggplot2::scale_x_continuous(
               breaks = scale_breaks_doses,    # Define where ticks appear
               labels = scale_labels_doses,    # Define labels for the ticks
               limits = c(0, max(scale_breaks_doses)),  # Set limits to match scale breaks
               expand = c(0, 0)                                # Remove extra space
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
               legend.position = "none",  # Remove legend from barplot
               axis.title.y = ggplot2::element_blank(),
               axis.text.y = ggplot2::element_text(size = 8),
               axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),  # Angle x-axis labels
               panel.grid.major.y = ggplot2::element_blank(),
               panel.grid.minor = ggplot2::element_blank(),
               axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 15))  # Expanded margin above x-axis title
          ) +
          xlab("Cumulative Doses Distributed") +  # Updated X-axis title
          ylab(NULL)

     # Barplot for prop_vaccinated
     bar_plot_prop <-
          ggplot2::ggplot(bar_data_prop, ggplot2::aes(x = prop_vaccinated, y = country)) +
          ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = prop_vaccinated)) +
          color_scale_prop +  # Use the same color scale for consistency
          ggplot2::scale_x_continuous(
               breaks = scale_breaks_prop,    # Define where ticks appear
               labels = scale_labels_prop,    # Define labels for the ticks
               limits = c(0, max(scale_breaks_prop)),  # Set limits to match scale breaks
               expand = c(0, 0)                # Remove extra space
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
               legend.position = "none",  # Remove legend from barplot
               axis.title.y = ggplot2::element_blank(),
               axis.text.y = ggplot2::element_text(size = 8),
               axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),  # Angle x-axis labels
               panel.grid.major.y = ggplot2::element_blank(),
               panel.grid.minor = ggplot2::element_blank(),
               axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 15))  # Expanded margin above x-axis title
          ) +
          xlab("Proportion Vaccinated *") +  # Updated X-axis title
          ylab(NULL)


     # Combine map and barplot for doses_distributed_cumulative
     combined_doses <- patchwork::wrap_plots(
          map_plot_doses,
          bar_plot_doses,
          widths = c(3, 2)
     ) +
          patchwork::plot_annotation(title = "A")

     # Combine map and barplot for prop_vaccinated
     combined_prop <- patchwork::wrap_plots(
          map_plot_prop,
          bar_plot_prop,
          widths = c(3, 2)
     ) +
          patchwork::plot_annotation(title = "B",
                          theme = ggplot2::theme(
                               plot.title = ggplot2::element_text(size = 12, face = 'bold', hjust = 0)
                          ))

     # Arrange both combined plots vertically
     final_combined_plot <- patchwork::wrap_plots(
          combined_doses,
          combined_prop,
          ncol = 1
     ) +
          patchwork::plot_annotation(
               title = glue::glue("Reported OCV doses distributed as of {format(Sys.Date(), '%B %d, %Y')}"),
               subtitle = "",
               theme = ggplot2::theme(
                    plot.title = ggplot2::element_text(size = 13, face = 'bold', hjust = 0.5)
               )
          )



     footnote <- cowplot::ggdraw() +
          cowplot::draw_label(
               "* Assumes single-dose OCV strategy",
               x = 0,
               hjust = -0.125,
               fontface = "italic",
               size = 10
          )

     # Combine the main plot with the footnote
     final_combined_plot <- cowplot::plot_grid(
          final_combined_plot,
          footnote,
          ncol = 1,
          rel_heights = c(1, 0.05)
     )

     # Display the final plot
     print(final_combined_plot)


     # Save the facet plot to the specified path
     plot_file <- file.path(PATHS$DOCS_FIGURES, glue::glue("vaccination_maps.png"))
     ggplot2::ggsave(filename = plot_file, plot = print(final_combined_plot), width = 7.75, height = 7, units = "in", dpi = 300)
     message(glue::glue("Vaccination maps saved to: {plot_file}"))


}
