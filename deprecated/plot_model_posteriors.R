#' Plot Model Posterior Distributions
#'
#' Creates faceted plots showing the posterior distributions of model parameters
#' from calibration results. Parameters are weighted by likelihood values to
#' compute empirical posterior distributions. Generates separate plots for
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
#' This function creates publication-quality faceted plots showing posterior
#' distributions for each parameter. The posteriors are computed by weighting
#' parameter values by their likelihood (transformed from log-likelihood).
#'
#' Color scheme and visual elements (blue palette with red accent):
#' \itemize{
#'   \item \strong{Prior (Light Blue #A8DADC / #457B9D)}:
#'     \itemize{
#'       \item Light blue histogram bars (30\% opacity)
#'       \item Thin solid medium blue density curve
#'       \item Thin dashed medium blue vertical line at mode
#'     }
#'   \item \strong{Posterior (Dark Blue #1D3557)}:
#'     \itemize{
#'       \item Dark blue histogram bars (60\% opacity)
#'       \item Thick solid dark blue density curve
#'       \item Dashed dark blue vertical line at mode
#'     }
#'   \item \strong{Maximum Likelihood (Red #E63946)}:
#'     \itemize{
#'       \item Dotted red vertical line at ML estimate (accent color)
#'     }
#' }
#'
#' The function uses temperature scaling when likelihood ranges are extreme (>100 log units)
#' to prevent the posterior from collapsing to a single point. Professional formatting
#' includes clean facet headers without grey backgrounds, subtle grid lines, and
#' space-efficient layout.
#'
#' The function automatically:
#' \itemize{
#'   \item Transforms log-likelihoods to weights (higher likelihood = higher weight)
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
#' # Create posterior plots
#' plots <- plot_model_posteriors(results, output_dir = "calibration_output")
#'
#' # Access individual plots
#' print(plots$global)
#' print(plots$location[["ETH"]])
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density geom_vline facet_wrap theme_minimal theme element_text element_rect element_blank element_line labs ggsave after_stat
#' @importFrom dplyr select filter mutate group_by ungroup case_when everything contains select_if summarise any_of
#' @importFrom tidyr pivot_longer
#' @importFrom stats density weighted.mean
plot_model_posteriors <- function(results,
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

     # ============================================================================
     # Transform likelihoods to weights
     # ============================================================================

     # Remove rows with NA likelihood
     results_valid <- results %>% dplyr::filter(!is.na(likelihood))

     if (nrow(results_valid) == 0) {
          warning("No simulations with valid likelihood values found")
          return(invisible(NULL))
     }

     # Transform log-likelihood to weights
     # Higher likelihood (less negative) should have higher weight
     # Use a temperature parameter to control concentration
     max_ll <- max(results_valid$likelihood, na.rm = TRUE)
     min_ll <- min(results_valid$likelihood, na.rm = TRUE)
     ll_range <- max_ll - min_ll

     # Add temperature scaling to prevent extreme concentration
     # Temperature > 1 makes distribution more diffuse
     temperature <- 1.0

     # Handle edge cases
     if (ll_range == 0) {
          # All likelihoods are identical - uniform weights
          if (verbose) message("All likelihoods are identical - using uniform weights")
          results_valid <- results_valid %>%
               dplyr::mutate(weight = 1 / n())
     } else {
          # Apply temperature scaling for large ranges
          if (ll_range > 100) {
               temperature <- ll_range / 100
               if (verbose) message("Using temperature scaling: ", round(temperature, 2),
                                   " due to large likelihood range: ", round(ll_range, 1))
          }

          results_valid <- results_valid %>%
               dplyr::mutate(
                    # Convert log-likelihood to relative likelihood with temperature scaling
                    weight = exp((likelihood - max_ll) / temperature)
               )

          # Check for underflow
          weight_sum <- sum(results_valid$weight, na.rm = TRUE)
          if (weight_sum == 0 || is.na(weight_sum)) {
               warning("All weights underflowed to zero - using uniform weights")
               results_valid <- results_valid %>%
                    dplyr::mutate(weight = 1 / n())
          } else {
               # Normalize weights to sum to 1
               results_valid <- results_valid %>%
                    dplyr::mutate(weight = weight / weight_sum)
          }
     }

     if (verbose) {
          message("Transformed ", nrow(results_valid), " log-likelihood values to weights")
          message("Likelihood range: [", round(min_ll, 1), ", ", round(max_ll, 1), "]")
          message("Weight range: [", format(min(results_valid$weight, na.rm = TRUE), scientific = TRUE),
                  ", ", format(max(results_valid$weight, na.rm = TRUE), scientific = TRUE), "]")

          # Check weight concentration (only if weights vary)
          if (ll_range > 0) {
               sorted_weights <- sort(results_valid$weight, decreasing = TRUE)
               n_weights <- length(sorted_weights)
               top_weight <- sorted_weights[1]
               top5_weight <- sum(sorted_weights[1:min(5, n_weights)])
               message("Top weight: ", format(top_weight, scientific = TRUE),
                       " (", round(top_weight * 100, 2), "%)")
               message("Top 5 weights sum: ", format(top5_weight, scientific = TRUE),
                       " (", round(top5_weight * 100, 2), "%)")
          }
     }

     # ============================================================================
     # Define parameter categories (same as plot_model_parameters)
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
     params_to_plot <- results_valid %>%
          dplyr::select(-dplyr::any_of(c("sim", "iter", "seed")),
                        -dplyr::contains("location_name"),
                        -dplyr::contains("date"),
                        -dplyr::contains("N_j_initial"),
                        -dplyr::contains("longitude"),
                        -dplyr::contains("latitude")) %>%
          dplyr::select_if(is.numeric) %>%
          dplyr::select(likelihood, weight, dplyr::everything())

     # Pivot longer for faceting
     results_long <- params_to_plot %>%
          tidyr::pivot_longer(cols = -c(likelihood, weight),
                              names_to = "parameter",
                              values_to = "value") %>%
          dplyr::mutate(
               # Clean parameter names
               param_base = gsub("_[A-Z]{3}$", "", parameter),
               # Determine if global or location-specific
               param_type = dplyr::case_when(
                    parameter %in% global_params ~ "Global",
                    param_base %in% location_params_base ~ "Location-Specific",
                    grepl("_[A-Z]{3}$", parameter) ~ "Location-Specific",
                    TRUE ~ "Global"
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
          dplyr::select(-param_base)

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

     # ============================================================================
     # Helper function to create posterior plot
     # ============================================================================

     create_posterior_plot <- function(data, title, subtitle, output_file) {

          if (nrow(data) == 0) {
               return(NULL)
          }

          # Define color palette from coolors.co/palette/e63946-f1faee-a8dadc-457b9d-1d3557
          col_prior <- "#457b9d"       # Light blue for prior histogram
          col_posterior <- "#1d3557"   # Dark blue for posterior histogram
          col_ml <- "#e63946"          # Red for ML estimate
          col_map_prior <- "#457b9d"   # Medium blue for prior mode
          col_map_post <- "#1d3557"    # Dark blue for posterior mode

          # Calculate statistics for each parameter
          map_estimates <- data %>%
               dplyr::group_by(parameter) %>%
               dplyr::summarise(
                    # Maximum likelihood value
                    ml_value = value[which.max(likelihood)],
                    # Prior mode (unweighted)
                    prior_mode = {
                         n_unique <- length(unique(value))
                         if (n_unique > 1) {
                              tryCatch({
                                   dens <- density(value, adjust = 1.2, n = 512)
                                   dens$x[which.max(dens$y)]
                              }, error = function(e) {
                                   median(value, na.rm = TRUE)
                              })
                         } else {
                              value[1]
                         }
                    },
                    # Posterior mode (weighted)
                    posterior_mode = {
                         n_unique <- length(unique(value))
                         weight_sum <- sum(weight, na.rm = TRUE)

                         if (n_unique > 1 && weight_sum > 0 && !all(weight == weight[1])) {
                              tryCatch({
                                   # Ensure weights are normalized and valid
                                   w_norm <- weight / weight_sum
                                   w_norm[is.na(w_norm)] <- 0
                                   if (sum(w_norm) > 0) {
                                        dens <- density(value, weights = w_norm,
                                                      adjust = 1.2, n = 512)
                                        dens$x[which.max(dens$y)]
                                   } else {
                                        value[which.max(likelihood)]
                                   }
                              }, error = function(e) {
                                   # Fallback to ML estimate
                                   value[which.max(likelihood)]
                              })
                         } else {
                              # Uniform weights or single value - use ML estimate
                              value[which.max(likelihood)]
                         }
                    },
                    .groups = "drop"
               )

          # Create the plot
          p <- ggplot2::ggplot(data, ggplot2::aes(x = value)) +
               # Prior histogram - light blue bars
               ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                      bins = 30,
                                      fill = col_prior,
                                      alpha = 0.4,
                                      color = "white") +
               # Posterior histogram - dark blue bars
               ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density),
                                                    weight = weight),
                                      bins = 30,
                                      fill = col_posterior,
                                      alpha = 0.65,
                                      color = "white",
                                      linewidth = 0.15) +
               # Prior density - thin solid medium blue line
               ggplot2::geom_density(color = col_map_prior,
                                    linewidth = 0.7,
                                    alpha = 0.9,
                                    linetype = "solid",
                                    adjust = 1.2) +
               # Posterior density - thicker solid dark blue line
               ggplot2::geom_density(ggplot2::aes(weight = weight),
                                    color = col_map_post,
                                    linewidth = 1,
                                    linetype = "solid",
                                    adjust = 1.2) +
               # Prior mode - thin dashed medium blue line
               ggplot2::geom_vline(data = map_estimates,
                                   ggplot2::aes(xintercept = prior_mode),
                                   color = col_map_prior,
                                   linetype = "dashed",
                                   linewidth = 0.4,
                                   alpha = 1.0) +
               # Posterior mode - dashed dark blue line
               ggplot2::geom_vline(data = map_estimates,
                                   ggplot2::aes(xintercept = posterior_mode),
                                   color = col_map_post,
                                   linetype = "dashed",
                                   linewidth = 0.6,
                                   alpha = 1.0) +
               # Maximum likelihood - dotted red line (accent color)
               ggplot2::geom_vline(data = map_estimates,
                                   ggplot2::aes(xintercept = ml_value),
                                   color = col_ml,
                                   linetype = "dotted",
                                   linewidth = 0.5,
                                   alpha = 1.0) +
               # Facet by parameter
               ggplot2::facet_wrap(~ parameter,
                                   scales = "free",
                                   ncol = 5) +
               ggplot2::theme_minimal(base_size = 9) +
               ggplot2::theme(
                    strip.text = ggplot2::element_text(size = 7, face = "bold", color = "black"),
                    strip.background = ggplot2::element_blank(),  # Remove grey fill
                    panel.spacing = ggplot2::unit(0.8, "lines"),  # Compact spacing
                    panel.grid.minor = ggplot2::element_blank(),
                    panel.grid.major = ggplot2::element_line(linewidth = 0.15, color = "grey92"),
                    panel.border = ggplot2::element_rect(color = "grey85", fill = NA, linewidth = 0.3),
                    axis.text = ggplot2::element_text(size = 6, color = "black"),
                    axis.title = ggplot2::element_text(size = 8, face = "bold"),
                    axis.ticks = ggplot2::element_line(linewidth = 0.2, color = "grey70"),
                    plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5),
                    plot.subtitle = ggplot2::element_text(size = 9, hjust = 0.5, color = "grey30"),
                    plot.caption = ggplot2::element_text(size = 7, hjust = 1, face = "italic", color = "grey50"),
                    plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
               ) +
               ggplot2::labs(
                    x = "Parameter Value",
                    y = "Posterior Density",
                    title = title,
                    subtitle = subtitle,
                    caption = paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
               )

          # Save plot
          if (!is.null(output_file)) {
               ggplot2::ggsave(output_file,
                               plot = p,
                               width = 14,
                               height = 10,
                               dpi = 300)
               if (verbose) message("Posterior plot saved as '", output_file, "'")
          }

          return(p)
     }

     # ============================================================================
     # Create subtitle for plots
     # ============================================================================

     n_successful <- nrow(results_valid)
     n_total <- nrow(results)

     # Infer n_sim and n_iter from the results dataframe
     if ("sim" %in% names(results)) {
          n_sim <- max(results$sim, na.rm = TRUE)
          if ("iter" %in% names(results)) {
               n_iter <- max(results$iter, na.rm = TRUE)
               subtitle_text <- paste0("N = ", n_successful, " successful / ",
                                       n_sim * n_iter, " total (",
                                       n_sim, " simulations × ", n_iter, " iterations)")
          } else {
               subtitle_text <- paste0("N = ", n_successful, " successful / ",
                                       n_total, " total simulations")
          }
     } else {
          subtitle_text <- paste0("N = ", n_successful, " successful / ",
                                  n_total, " total parameter sets")
     }

     # ============================================================================
     # Create plots
     # ============================================================================

     plot_list <- list()

     # Create GLOBAL parameters plot
     if (nrow(results_global) > 0) {
          output_file <- file.path(output_dir, "posterior_global_parameters.pdf")
          p_global <- create_posterior_plot(
               results_global,
               title = "Model Posterior Distributions: Global Parameters",
               subtitle = subtitle_text,
               output_file = output_file
          )
          if (verbose && !is.null(p_global)) print(p_global)
          plot_list$global <- p_global
     }

     # Create LOCATION-SPECIFIC parameters plots
     location_plots <- list()

     if (length(unique_locations) > 0) {
          if (verbose) message("Creating posterior plots for locations: ", paste(unique_locations, collapse = ", "))

          for (loc in unique_locations) {
               # Filter data for this location
               results_loc <- results_location %>%
                    dplyr::filter(grepl(paste0("_", loc, "$"), parameter))

               # Remove the location suffix for cleaner facet labels
               results_loc <- results_loc %>%
                    dplyr::mutate(
                         # Keep original parameter, create clean version for display
                         parameter_display = gsub(paste0("_", loc, "$"), "", parameter),
                         parameter = factor(parameter_display, levels = unique(parameter_display))
                    ) %>%
                    dplyr::select(-parameter_display)

               # Create plot for this location
               output_file <- file.path(output_dir, paste0("posterior_parameters_", loc, ".pdf"))
               p_loc <- create_posterior_plot(
                    results_loc,
                    title = paste0("Model Posterior Distributions: Location-Specific Parameters - ", loc),
                    subtitle = subtitle_text,
                    output_file = output_file
               )

               location_plots[[loc]] <- p_loc

               # Display plot
               if (verbose && !is.null(p_loc) && nrow(results_loc) > 0) {
                    print(p_loc)
               }
          }
     } else {
          if (verbose) message("No location-specific posterior plots created (no locations found)")
     }

     plot_list$location <- location_plots

     # Return plot objects invisibly
     invisible(plot_list)
}
