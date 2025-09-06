#' Plot Model Parameters vs Likelihood
#'
#' Creates faceted plots showing the relationship between model parameters and
#' likelihood values from calibration results. Generates separate plots for
#' global and location-specific parameters.
#'
#' @param results A data frame containing calibration results with columns for
#'   parameters, likelihood values, and simulation identifiers (sim, iter, seed).
#'   This is typically the output from a model calibration run. The function
#'   will automatically infer the number of simulations and iterations from
#'   the data.
#' @param output_dir Character string specifying the directory where plots should
#'   be saved. Directory will be created if it doesn't exist.
#' @param verbose Logical indicating whether to print messages. Default is TRUE.
#'
#' @return Invisibly returns a list containing the plot objects:
#'   \itemize{
#'     \item \code{global}: ggplot object for global parameters
#'     \item \code{location}: Named list of ggplot objects for each location
#'   }
#'
#' @details
#' This function creates publication-quality faceted plots showing how each
#' parameter relates to the model likelihood. Points are colored by density,
#' with a LOESS smooth curve showing the trend. The best parameter value
#' (highest likelihood) is highlighted with a red point.
#'
#' The function automatically:
#' \itemize{
#'   \item Separates global and location-specific parameters
#'   \item Categorizes parameters by type (epidemiological, environmental, etc.)
#'   \item Creates separate plots for each location found in the data
#'   \item Saves plots as PDF files in the specified output directory
#' }
#'
#' @examples
#' \dontrun{
#' # Run calibration and collect results
#' results <- run_calibration()
#'
#' # Create plots (automatically infers n_sim and n_iter from results)
#' plots <- plot_model_parameters(results, output_dir = "calibration_output")
#'
#' # Access individual plots
#' print(plots$global)
#' print(plots$location[["ETH"]])
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth facet_wrap theme_minimal theme element_text element_rect element_blank element_line labs ggsave
#' @importFrom dplyr select filter mutate group_by slice_max ungroup case_when everything contains select_if
#' @importFrom tidyr pivot_longer
plot_model_parameters <- function(results,
                                  output_dir,
                                  verbose = TRUE) {

     # ============================================================================
     # Input validation
     # ============================================================================

     if (!is.data.frame(results)) {
          stop("results must be a data frame")
     }

     if (!"likelihood" %in% names(results)) {
          stop("results must contain a 'likelihood' column")
     }

     if (missing(output_dir) || is.null(output_dir)) {
          stop("output_dir is required")
     }

     # Create output directory if it doesn't exist
     if (!dir.exists(output_dir)) {
          dir.create(output_dir, recursive = TRUE)
          if (verbose) message("Created output directory: ", output_dir)
     }

     # Infer n_sim and n_iter from the results dataframe
     n_sim <- NULL
     n_iter <- NULL

     if ("sim" %in% names(results)) {
          n_sim <- max(results$sim, na.rm = TRUE)
     }
     if ("iter" %in% names(results)) {
          n_iter <- max(results$iter, na.rm = TRUE)
     }

     # If sim and iter columns don't exist, try to infer from number of rows
     if (is.null(n_sim) && is.null(n_iter)) {
          n_total <- nrow(results)
          if (verbose) message("Note: 'sim' and 'iter' columns not found. Total rows: ", n_total)
     }

     # ============================================================================
     # Define parameter categories
     # ============================================================================

     # Define global parameters explicitly
     global_params <- c(
          "phi_1", "phi_2",                          # Vaccine effectiveness
          "omega_1", "omega_2",                      # Vaccine waning
          "gamma_1", "gamma_2",                      # Recovery rates
          "epsilon",                                 # Incubation rate
          "rho",                                     # Reporting fraction
          "sigma",                                   # Progression rate
          "iota",                                    # Importation rate
          "alpha_1", "alpha_2",                      # Environmental transmission
          "zeta_1", "zeta_2",                        # Environmental parameters
          "kappa",                                   # Environmental capacity
          "decay_days_short", "decay_days_long",    # Immunity decay
          "decay_shape_1", "decay_shape_2",         # Decay shape parameters
          "mobility_omega", "mobility_gamma"         # Mobility parameters
     )

     # Define location-specific parameters (base names without location suffixes)
     location_params_base <- c(
          "S_j_initial", "E_j_initial", "I_j_initial",
          "R_j_initial", "V1_j_initial", "V2_j_initial",  # Initial conditions
          "beta_j0_env", "beta_j0_hum",                   # Transmission rates
          "tau_i", "theta_j",                             # Mobility rate
          "a_1_j", "a_2_j", "b_1_j", "b_2_j"             # Seasonal parameters
     )

     # ============================================================================
     # Prepare data for plotting
     # ============================================================================

     # Select only numeric parameters (exclude metadata columns)
     params_to_plot <- results %>%
          dplyr::select(-dplyr::any_of(c("sim", "iter", "seed")),
                        -dplyr::contains("location_name"),
                        -dplyr::contains("date"),
                        -dplyr::contains("N_j_initial"),
                        -dplyr::contains("longitude"),
                        -dplyr::contains("latitude")) %>%
          dplyr::select_if(is.numeric) %>%
          dplyr::select(likelihood, dplyr::everything())

     # Pivot longer for faceting
     results_long <- params_to_plot %>%
          tidyr::pivot_longer(cols = -likelihood,
                              names_to = "parameter",
                              values_to = "value") %>%
          dplyr::filter(!is.na(likelihood)) %>%  # Remove failed simulations
          # Add parameter categories and type (global vs location-specific)
          dplyr::mutate(
               # Clean parameter names (remove ANY 3-letter ISO code suffix)
               param_base = gsub("_[A-Z]{3}$", "", parameter),
               # Determine if global or location-specific
               param_type = dplyr::case_when(
                    parameter %in% global_params ~ "Global",
                    param_base %in% location_params_base ~ "Location-Specific",
                    grepl("_[A-Z]{3}$", parameter) ~ "Location-Specific",
                    TRUE ~ "Global"  # Default to global for unmatched
               ),
               category = dplyr::case_when(
                    grepl("_initial", parameter) ~ "1_Initial Conditions",
                    grepl("^(phi_|omega_|gamma_|epsilon|rho|sigma|iota)", parameter) ~ "2_Epidemiological",
                    grepl("^(alpha_|zeta_|kappa)", parameter) ~ "3_Environmental",
                    grepl("^(beta_j0_|theta_j)", parameter) ~ "4_Transmission",
                    grepl("^(a_|b_|a1_|a2_|b1_|b2_)", parameter) ~ "5_Seasonality",
                    grepl("^(decay_|vaccine)", parameter) ~ "6_Immunity & Vaccine",
                    grepl("^(mobility_|tau_)", parameter) ~ "7_Mobility",
                    TRUE ~ "8_Other"
               ),
               # Create ordered factor for proper facet ordering
               parameter = factor(parameter,
                                  levels = unique(parameter[order(category, parameter)]))
          ) %>%
          dplyr::select(-param_base)  # Remove temporary column

     # Split into global and location-specific
     results_global <- results_long %>% dplyr::filter(param_type == "Global")
     results_location <- results_long %>% dplyr::filter(param_type == "Location-Specific")

     # ============================================================================
     # Helper function to extract ISO codes
     # ============================================================================

     extract_iso <- function(param_names) {
          iso_codes <- unique(gsub(".*_([A-Z]{3})$", "\\1",
                                   param_names[grepl("_[A-Z]{3}$", param_names)]))
          return(iso_codes)
     }

     unique_locations <- extract_iso(results_location$parameter)

     # Debug messages
     if (verbose) {
          if (length(unique_locations) == 0) {
               message("Warning: No location-specific parameters found.")
               if (nrow(results_location) > 0) {
                    message("Sample location-specific parameters: ",
                            paste(head(unique(results_location$parameter), 10), collapse = ", "))
               }
          } else {
               message("Unique locations found: ", paste(unique_locations, collapse = ", "))
          }
     }

     # ============================================================================
     # Create subtitle for plots
     # ============================================================================

     n_successful <- sum(!is.na(results$likelihood))
     n_total <- nrow(results)

     # Build subtitle based on available information
     if (!is.null(n_sim) && !is.null(n_iter)) {
          subtitle_text <- paste0("N = ", n_successful, " successful / ",
                                  n_sim * n_iter, " total (",
                                  n_sim, " simulations × ", n_iter, " iterations)")
     } else if (!is.null(n_sim)) {
          subtitle_text <- paste0("N = ", n_successful, " successful / ",
                                  n_total, " total simulations")
     } else {
          # Fallback when we don't have sim/iter columns
          subtitle_text <- paste0("N = ", n_successful, " successful / ",
                                  n_total, " total parameter sets")
     }

     # ============================================================================
     # Create GLOBAL parameters plot
     # ============================================================================

     plot_list <- list()

     if (nrow(results_global) > 0) {
          # Find the best parameter value for each parameter
          best_points_global <- results_global %>%
               dplyr::group_by(parameter) %>%
               dplyr::slice_max(likelihood, n = 1) %>%
               dplyr::ungroup()

          # Create the plot
          p_global <- ggplot2::ggplot(results_global, ggplot2::aes(x = value, y = likelihood)) +
               ggplot2::geom_point(alpha = 0.5, size = 0.8, color = "gray30") +
               ggplot2::geom_smooth(method = "loess",
                                    span = 0.3,
                                    se = TRUE,
                                    color = "steelblue",
                                    fill = "steelblue",
                                    alpha = 0.2,
                                    linewidth = 0.8) +
               # Add best points in dark red
               ggplot2::geom_point(data = best_points_global,
                                   ggplot2::aes(x = value, y = likelihood),
                                   color = "red3",
                                   size = 2.5,
                                   shape = 10) +
               ggplot2::facet_wrap(~ parameter,
                                   scales = "free_x",
                                   ncol = 5) +
               ggplot2::theme_minimal(base_size = 10) +
               ggplot2::theme(
                    strip.text = ggplot2::element_text(size = 8, face = "bold"),
                    strip.background = ggplot2::element_rect(fill = "gray95", color = NA),
                    panel.grid.minor = ggplot2::element_blank(),
                    panel.grid.major = ggplot2::element_line(linewidth = 0.25, color = "gray85"),
                    axis.text = ggplot2::element_text(size = 7),
                    axis.title = ggplot2::element_text(size = 9),
                    plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                    plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5),
                    plot.caption = ggplot2::element_text(size = 8, hjust = 1, face = "italic")
               ) +
               ggplot2::labs(
                    x = "Parameter Value",
                    y = "Log Likelihood",
                    title = "Model Calibration: Global Parameters",
                    subtitle = subtitle_text,
                    caption = paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
               )

          # Display plot
          if (verbose) print(p_global)

          # Save plot
          output_file <- file.path(output_dir, "calibration_global_parameters.pdf")
          ggplot2::ggsave(output_file,
                          plot = p_global,
                          width = 14,
                          height = 10,
                          dpi = 300)
          if (verbose) message("Global parameters plot saved as '", output_file, "'")

          plot_list$global <- p_global
     }

     # ============================================================================
     # Create LOCATION-SPECIFIC parameters plots
     # ============================================================================

     location_plots <- list()

     if (length(unique_locations) > 0) {
          if (verbose) message("Creating plots for locations: ", paste(unique_locations, collapse = ", "))

          for (loc in unique_locations) {
               # Filter data for this location
               results_loc <- results_location %>%
                    dplyr::filter(grepl(paste0("_", loc, "$"), parameter))

               # Remove the location suffix for cleaner facet labels
               results_loc <- results_loc %>%
                    dplyr::mutate(parameter_clean = gsub(paste0("_", loc, "$"), "", parameter))

               # Find best points for this location
               best_points_loc <- results_loc %>%
                    dplyr::group_by(parameter_clean) %>%
                    dplyr::slice_max(likelihood, n = 1) %>%
                    dplyr::ungroup()

               # Create plot for this location
               p_loc <- ggplot2::ggplot(results_loc, ggplot2::aes(x = value, y = likelihood)) +
                    ggplot2::geom_point(alpha = 0.5, size = 0.8, color = "gray30") +
                    ggplot2::geom_smooth(method = "loess",
                                         se = TRUE,
                                         color = "steelblue",
                                         fill = "steelblue",
                                         alpha = 0.2,
                                         linewidth = 0.8) +
                    # Add best points in dark red
                    ggplot2::geom_point(data = best_points_loc,
                                        ggplot2::aes(x = value, y = likelihood),
                                        color = "red3",
                                        size = 2.5,
                                        shape = 10) +
                    ggplot2::facet_wrap(~ parameter_clean,
                                        scales = "free_x",
                                        ncol = 4) +
                    ggplot2::theme_minimal(base_size = 10) +
                    ggplot2::theme(
                         strip.text = ggplot2::element_text(size = 8, face = "bold"),
                         strip.background = ggplot2::element_rect(fill = "gray95", color = NA),
                         panel.grid.minor = ggplot2::element_blank(),
                         panel.grid.major = ggplot2::element_line(linewidth = 0.25, color = "gray85"),
                         axis.text = ggplot2::element_text(size = 7),
                         axis.title = ggplot2::element_text(size = 9),
                         plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                         plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5),
                         plot.caption = ggplot2::element_text(size = 8, hjust = 1, face = "italic")
                    ) +
                    ggplot2::labs(
                         x = "Parameter Value",
                         y = "Log Likelihood",
                         title = paste0("Model Calibration: Location-Specific Parameters - ", loc),
                         subtitle = subtitle_text,
                         caption = paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
                    )

               location_plots[[loc]] <- p_loc

               # Display plot
               if (verbose && nrow(results_loc) > 0) {
                    print(p_loc)
               }

               # Save plot
               if (nrow(results_loc) > 0) {
                    output_file <- file.path(output_dir, paste0("calibration_parameters_", loc, ".pdf"))
                    ggplot2::ggsave(output_file,
                                    plot = p_loc,
                                    width = 12,
                                    height = 10,
                                    dpi = 300)
                    if (verbose) message("Location parameters plot saved as '", output_file, "'")
               }
          }
     } else {
          if (verbose) message("No location-specific plots created (no locations found)")
     }

     plot_list$location <- location_plots

     # Return plot objects invisibly
     invisible(plot_list)
}
