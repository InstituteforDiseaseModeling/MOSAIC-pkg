#' Estimate and Visualize Mobility Model for MOSAIC Data
#'
#' This function processes flight data, population data, and distance data to fit a mobility model using the `mobility` package. The model estimates both the probability of travel from each origin country (departure) and the probability of travel from each origin to a destination (diffusion). It then saves the model matrices and results to CSV files and generates plots for visualization.
#'
#' @param PATHS A list containing paths to directories for raw and processed data. Typically, `PATHS` is the output of `get_paths()` and should include:
#' \itemize{
#'   \item \strong{DATA_OAG}: Path to the directory containing flight data.
#'   \item \strong{DATA_SHAPEFILES}: Path to the directory containing country shapefiles.
#'   \item \strong{DATA_DEMOGRAPHICS}: Path to the directory containing demographic data.
#'   \item \strong{MODEL_INPUT}: Path to the directory where model matrices will be saved.
#'   \item \strong{DOCS_FIGURES}: Path to the directory where figures will be saved.
#' }
#'
#' @return The function does not return a value. It processes data, fits the mobility model, saves the results as CSV files, and creates several visualizations that are saved as PNG files.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Loads flight data and constructs a mobility matrix (`M`).
#'   \item Constructs a distance matrix (`D`) from country centroids based on shapefiles.
#'   \item Retrieves population sizes (`N`) from demographic data.
#'   \item Ensures that dimensions of `M`, `D`, and `N` match.
#'   \item Fits the probability of travel (departure) and diffusion (origin-destination) using the `mobility` package.
#'   \item Saves the results, including the mobility matrix (`M`), distance matrix (`D`), and population vector (`N`) as CSV files.
#'   \item Saves several plots related to the mobility model, including a heatmap of the flight data, a network plot of flight data, and plots of travel probabilities and diffusion probabilities.
#' }
#'
#' @importFrom ggplot2 ggplot geom_tile geom_segment geom_sf geom_point geom_text_repel aes scale_fill_gradient scale_size_continuous theme_bw theme_void
#' @importFrom dplyr filter mutate arrange select
#' @importFrom tidyr pivot_longer
#' @importFrom reshape2 melt
#' @importFrom sf st_read st_coordinates
#' @importFrom glue glue
#' @importFrom viridis scale_fill_viridis
#' @importFrom gridExtra grid.arrange
#' @importFrom utils read.csv write.csv
#' @importFrom propvacc get_beta_params
#' @importFrom mobility get_mob_matrix fit_prob_travel mobility summary check predict
#' @importFrom cowplot plot_grid
#' @importFrom ggrepel geom_text_repel
#'
#' @examples
#' \dontrun{
#' # Assuming PATHS is generated from the get_paths function
#' PATHS <- get_paths()
#'
#' # Fit the mobility model and generate outputs
#' est_mobility(PATHS)
#' }
#'
#' @export

est_mobility <- function(PATHS) {

     requireNamespace('ggplot2')
     requireNamespace('reshape2')
     requireNamespace('sf')
     requireNamespace('dplyr')
     requireNamespace('tidyr')
     requireNamespace('glue')

     #--------------------------------------------------------------------------
     # Load flight data and build mobility matrix (M)
     #--------------------------------------------------------------------------
     message("Getting flight data")
     data_flights <- utils::read.csv(file.path(PATHS$DATA_OAG, "oag_mosaic_2017_mean_weekly.csv"), stringsAsFactors = FALSE)

     if (!(all(MOSAIC::iso_codes_mosaic %in% data_flights$origin_iso3))) {
          sel <- !(MOSAIC::iso_codes_mosaic %in% unique(data_flights$origin_iso3))
          missing <- unique(data_flights$origin_iso3)[sel]
          stop(glue::glue("Some iso3 codes are missing in data_centriods: {missing}"))
     }

     data_flights <- dplyr::filter(data_flights, origin_iso3 %in% MOSAIC::iso_codes_mosaic)

     M <- mobility::get_mob_matrix(orig = data_flights$origin_iso3,
                                   dest = data_flights$destination_iso3,
                                   value = data_flights$count)

     #--------------------------------------------------------------------------
     # Get distance matrix (D)
     #--------------------------------------------------------------------------
     message("Making distance matrix")
     shapefiles <- list.files(PATHS$DATA_SHAPEFILES, pattern = "\\.shp$", full.names = TRUE)
     data_centroids <- do.call(rbind, lapply(shapefiles, MOSAIC::get_centroid))

     if (!(all(data_flights$origin_iso3 %in% data_centroids$iso3))) stop("Some iso3 codes are missing in data_centriods")
     data_centroids <- dplyr::filter(data_centroids, iso3 %in% MOSAIC::iso_codes_mosaic)
     row.names(data_centroids) <- NULL
     head(data_centroids)

     D <- get_distance_matrix(x = data_centroids$lon, y = data_centroids$lat, id = data_centroids$iso3)
     D <- D * 111.35  # Convert decimal degrees to kilometers

     #--------------------------------------------------------------------------
     # Get Population size vector (N)
     #--------------------------------------------------------------------------
     message("Getting population sizes")
     data_demographics <- utils::read.csv(file.path(PATHS$DATA_DEMOGRAPHICS, "demographics_africa_2000_2023.csv"), stringsAsFactors = FALSE)
     data_demographics <- dplyr::filter(data_demographics, year == 2017 & iso_code %in% MOSAIC::iso_codes_mosaic)
     data_demographics <- dplyr::arrange(data_demographics, iso_code)

     N <- base::as.vector(data_demographics$population)
     names(N) <- data_demographics$iso_code

     #--------------------------------------------------------------------------
     # Check that dimensions match
     #--------------------------------------------------------------------------
     check <- c(
          dim(M)[1] == dim(M)[2],
          dim(M)[1] == length(N),
          dim(M)[2] == length(N),
          all(dimnames(M)$origin == names(N)),
          all(dimnames(M)$destination == names(N)),
          dim(M)[1] == dim(D)[1],
          dim(M)[2] == dim(D)[2],
          all(dimnames(M)$origin == dimnames(D)$origin),
          all(dimnames(M)$destination == dimnames(D)$destination)
     )

     if (!all(check)) {
          stop("Dimension mismatch in M, D, or N.")
     } else {
          message("Mobility matrices dimensions match")
     }

     #--------------------------------------------------------------------------
     # Fit probability of travel, departure (tau_i)
     #--------------------------------------------------------------------------
     message("Fitting departure process: travel probability (tau_j)")
     diagonal <- diag(M)
     diagonal[is.na(diagonal)] <- 0

     trips_total <- apply(M, 1, function(x) sum(x, na.rm = TRUE))
     y <- as.integer(trips_total - diagonal)
     names(y) <- names(diag(M))

     mod_travel_prob <- mobility::fit_prob_travel(travel = y,
                                                  total = N,
                                                  n_chain = 4,
                                                  n_burn = 1000,
                                                  n_samp = 1000,
                                                  n_thin = 10)

     mod_travel_prob <- mobility::summary(mod_travel_prob)

     beta_params <- propvacc::get_beta_params(mu = mod_travel_prob$mean, mod_travel_prob$sd^2)

     df_travel_prob <- data.frame(
          country = convert_iso_to_country(names(y)),
          iso_code = names(y),
          mean = mod_travel_prob$mean,
          sd = mod_travel_prob$sd,
          ci_lo = mod_travel_prob$Q2.5,
          ci_hi = mod_travel_prob$Q97.5,
          shape1 = beta_params$shape1,
          shape2 = beta_params$shape2
     )

     df_travel_prob <- merge(df_travel_prob, data.frame(iso_code = names(N), population = N), by = 'iso_code')

     # Save matrices and plots to files and directories specified in PATHS
     # ...
}
