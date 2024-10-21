#' Plot WHO Vaccination Maps
#'
#' Generates choropleth maps and corresponding bar plots to visualize WHO-ICG OCV vaccination data across African countries.
#'
#' @param PATHS A named list containing file paths required for input and output operations. It should include:
#' \describe{
#'   \item{`MODEL_INPUT`}{Directory path where the input CSV file `data_vaccinations.csv` is located.}
#'   \item{`DOCS_FIGURES`}{Directory path where the generated plots will be saved.}
#' }
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
#' plot_WHO_vaccination_maps(PATHS)
#' }
#'

plot_WHO_vaccination_maps <- function(PATHS) {

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
     vaccination_data <- do.call(rbind, last_cumsum_list)
     row.names(vaccination_data) <- NULL


     # Step 2: Retrieve Map Data for Africa
     africa_map <- rnaturalearth::ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

     # Step 3: Merge Vaccination Data with Map Data
     # Define SubSaharan Africa based on 'subregion'
     africa_vax_map <- africa_map %>%
          mutate(
               iso_a3 = toupper(iso_a3),
               is_subsaharan = iso_a3 %in% MOSAIC::iso_codes_ssa
          ) %>%
          left_join(vaccination_data, by = c("iso_a3" = "iso_code"))

     # Step 4: Define Scale Breaks and Labels

     # Scale Breaks and Labels for doses_distributed_cumulative
     scale_breaks_doses <- c(1000000, 5000000, 10000000, 15000000, 20000000, 25000000)
     scale_labels_doses <- c("1M", "5M", "10M", "15M", "20M", "25M")

     # Scale Breaks and Labels for prop_vaccinated
     scale_breaks_prop <- c(0.1, 0.2, 0.3, 0.4, 0.5)
     scale_labels_prop <- c("10%", "20%", "30%", "40%", "50%")

     # Step 5: Define Color Scales

     # Color Scale for doses_distributed_cumulative (Green Gradient)
     color_scale_doses <- scale_fill_gradient(
          name = "Cumulative Doses\nDistributed",  # Legend title with line break
          low = "#ECFFDC",                            # Light tan
          high = "#097969",                           # Dark green
          na.value = "grey95",                        # Non-SubSaharan Africa countries filled with grey95
          breaks = scale_breaks_doses,                # Breaks matching barplot's x-axis
          labels = scale_labels_doses,                # Labels for the breaks
          limits = c(0, max(scale_breaks_doses) + 1000000)  # Set limits to accommodate data and breaks
     )

     # Color Scale for prop_vaccinated (Purple Gradient)
     color_scale_prop <- scale_fill_gradient(
          name = "Proportion\nVaccinated",            # Legend title with line break
          low = "#E6E6FA",                            # Lavender (light purple)
          high = "#800080",                           # Purple (dark purple)
          na.value = "grey95",                        # Non-SubSaharan Africa countries filled with grey95
          breaks = scale_breaks_prop,                  # Breaks matching barplot's x-axis
          labels = scale_labels_prop,                  # Labels for the breaks
          limits = c(0, max(scale_breaks_prop)+0.01)                           # Set limits to accommodate data and breaks (0% to 50%)
     )

     # Step 6: Create the Choropleth Maps

     # Choropleth Map for doses_distributed_cumulative
     map_plot_doses <-
          ggplot() +
          # Layer 1: Non-SubSaharan Africa countries filled with white
          geom_sf(data = africa_vax_map %>% filter(!is_subsaharan), fill = "white", color = "grey50") +

          # Layer 2: SubSaharan Africa countries without vaccination data filled with grey95
          geom_sf(data = africa_vax_map %>% filter(is_subsaharan & is.na(doses_distributed_cumulative)),
                  fill = "grey95", color = "black") +

          # Layer 3: SubSaharan Africa countries with vaccination data filled using the color scale
          geom_sf(data = africa_vax_map %>% filter(is_subsaharan & !is.na(doses_distributed_cumulative)),
                  aes(fill = doses_distributed_cumulative), color = "black") +

          # Apply the custom color scale
          color_scale_doses +

          # Adjust the coordinate system to prevent cutting off the top of Africa
          coord_sf(
               xlim = c(-20, 55),   # Longitude limits
               ylim = c(-35, 40),    # Latitude limits to fully display Africa's top
               expand = FALSE
          ) +

          # Minimalist theme with customized legend
          theme_minimal() +
          theme(
               legend.position = "left",             # Position legend on the left
               legend.direction = "vertical",
               legend.key.height = unit(6, "cm"),    # Adjust legend key height
               legend.key.width = unit(0.25, "cm"),  # Adjust legend key width
               legend.title = element_text(size = 10),
               legend.text = element_text(size = 8),
               axis.text = element_blank(),
               axis.title = element_blank(),
               panel.grid = element_blank()
          ) +

          # Customize the colorbar guide
          guides(
               fill = guide_colorbar(
                    barwidth = unit(0.25, "cm"),   # Narrow width
                    barheight = unit(6, "cm"),     # Tall height
                    title.position = "top",
                    title.hjust = 0.5,
                    label.position = "left"         # Labels on the left side of the colorbar
               )
          )

     # Choropleth Map for prop_vaccinated
     map_plot_prop <-
          ggplot() +
          # Layer 1: Non-SubSaharan Africa countries filled with white
          geom_sf(data = africa_vax_map %>% filter(!is_subsaharan), fill = "white", color = "grey50") +

          # Layer 2: SubSaharan Africa countries without vaccination data filled with grey95
          geom_sf(data = africa_vax_map %>% filter(is_subsaharan & is.na(prop_vaccinated)),
                  fill = "grey95", color = "black") +

          # Layer 3: SubSaharan Africa countries with vaccination data filled using the color scale
          geom_sf(data = africa_vax_map %>% filter(is_subsaharan & !is.na(prop_vaccinated)),
                  aes(fill = prop_vaccinated), color = "black") +

          # Apply the custom color scale
          color_scale_prop +

          # Adjust the coordinate system to prevent cutting off the top of Africa
          coord_sf(
               xlim = c(-20, 55),   # Longitude limits
               ylim = c(-35, 40),    # Latitude limits to fully display Africa's top
               expand = FALSE
          ) +

          # Minimalist theme with customized legend
          theme_minimal() +
          theme(
               legend.position = "left",             # Position legend on the left
               legend.direction = "vertical",
               legend.key.height = unit(6, "cm"),    # Adjust legend key height
               legend.key.width = unit(0.25, "cm"),  # Adjust legend key width
               legend.title = element_text(size = 10),
               legend.text = element_text(size = 8),
               axis.text = element_blank(),
               axis.title = element_blank(),
               panel.grid = element_blank()
          ) +

          # Customize the colorbar guide
          guides(
               fill = guide_colorbar(
                    barwidth = unit(0.25, "cm"),   # Narrow width
                    barheight = unit(6, "cm"),     # Tall height
                    title.position = "top",
                    title.hjust = 0.5,
                    label.position = "left"         # Labels on the left side of the colorbar
               )
          )

     # Step 7: Prepare Data for Barplots

     # Barplot for doses_distributed_cumulative
     bar_data_doses <- vaccination_data %>%
          arrange(desc(doses_distributed_cumulative)) %>%
          mutate(country = factor(country, levels = rev(country)))  # For descending order

     # Barplot for prop_vaccinated
     bar_data_prop <- vaccination_data %>%
          arrange(desc(prop_vaccinated)) %>%
          mutate(country = factor(country, levels = rev(country)))  # For descending order

     # Step 8: Create the Barplots

     # Barplot for doses_distributed_cumulative
     bar_plot_doses <-
          ggplot(bar_data_doses, aes(x = doses_distributed_cumulative, y = country)) +
          geom_bar(stat = "identity", aes(fill = doses_distributed_cumulative)) +
          color_scale_doses +  # Use the same color scale for consistency
          scale_x_continuous(
               breaks = scale_breaks_doses,    # Define where ticks appear
               labels = scale_labels_doses,    # Define labels for the ticks
               limits = c(0, max(scale_breaks_doses) + 1000000),  # Set limits to accommodate data and labels
               expand = c(0, 0)                                # Remove extra space
          ) +
          theme_minimal() +
          theme(
               legend.position = "none",  # Remove legend from barplot
               axis.title.y = element_blank(),
               axis.text.y = element_text(size = 8),
               panel.grid.major.y = element_blank(),
               panel.grid.minor = element_blank(),
               axis.title.x = element_text(margin = margin(t = 15))  # Expanded margin above x-axis title
          ) +
          xlab("Cumulative Doses Distributed") +  # Updated X-axis title
          ylab(NULL)

     # Barplot for prop_vaccinated
     bar_plot_prop <-
          ggplot(bar_data_prop, aes(x = prop_vaccinated, y = country)) +
          geom_bar(stat = "identity", aes(fill = prop_vaccinated)) +
          color_scale_prop +  # Use the same color scale for consistency
          scale_x_continuous(
               breaks = scale_breaks_prop,    # Define where ticks appear
               labels = scale_labels_prop,    # Define labels for the ticks
               limits = c(0, 0.5),             # Set limits to accommodate data and labels (0% to 50%)
               expand = c(0, 0)                # Remove extra space
          ) +
          theme_minimal() +
          theme(
               legend.position = "none",  # Remove legend from barplot
               axis.title.y = element_blank(),
               axis.text.y = element_text(size = 8),
               panel.grid.major.y = element_blank(),
               panel.grid.minor = element_blank(),
               axis.title.x = element_text(margin = margin(t = 15))  # Expanded margin above x-axis title
          ) +
          xlab("Proportion Vaccinated *") +  # Updated X-axis title
          ylab(NULL)


     # Combine map and barplot for doses_distributed_cumulative
     combined_doses <- map_plot_doses +
          bar_plot_doses +
          plot_layout(widths = c(3, 2)) +  # Adjust widths as needed
          plot_annotation(title = "A")

     # Combine map and barplot for prop_vaccinated
     combined_prop <- map_plot_prop +
          bar_plot_prop +
          plot_layout(widths = c(3, 2)) +  # Adjust widths as needed
          plot_annotation(title = "B",
                          theme = theme(
                               plot.title = element_text(size = 12, face = 'bold', hjust = 0)
                          ))

     # Arrange both combined plots vertically
     final_combined_plot <- combined_doses / combined_prop +
          plot_layout(nrow = 2) +
          plot_annotation(
               title = glue::glue("Reported OCV doses distributed by WHO-ICG as of {format(max(redistributed_data$date), '%B %d, %Y')}"),
               subtitle = "",
               theme = theme(
                    plot.title = element_text(size = 13, face = 'bold', hjust = 0.5)
               )
          )



     footnote <- ggdraw() +
          draw_label(
               "* Assumes single-dose OCV strategy",
               x = 0,
               hjust = -0.125,
               fontface = "italic",
               size = 10
          )

     # Combine the main plot with the footnote
     final_combined_plot <- plot_grid(
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
