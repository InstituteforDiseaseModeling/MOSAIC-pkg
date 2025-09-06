#' Plot Model Posterior Parameter Distributions
#'
#' Creates publication-quality plots showing prior vs posterior parameter
#' distributions from calibration results. Uses importance sampling weights
#' to display unbiased posterior distributions. Generates separate plots for
#' global and location-specific parameters.
#'
#' @param results Data frame containing simulation results
#' @param col_ll Column index or name containing log-likelihood values
#' @param col_params Vector of parameter column indices or names to plot
#' @param posterior_weights Optional pre-calculated weights (if NULL, calculated internally)
#' @param output_dir Directory to save plots (optional)
#' @param ncol Number of columns in facet layout (default: 4 for global, 3 for location)
#' @param show_modes Logical, whether to show mode lines (default: TRUE)
#' @param show_ml Logical, whether to show ML estimate line (default: TRUE)
#' @param filter_weights Logical, whether to filter out very small weights (default: FALSE)
#' @param verbose Logical, whether to print messages (default: TRUE)
#'
#' @return Invisibly returns a list containing the plot objects:
#'   \itemize{
#'     \item \code{global}: ggplot object for global parameters
#'     \item \code{location}: Named list of ggplot objects for each location
#'   }
#'
#' @details
#' This function creates faceted plots comparing prior (unweighted) and posterior
#' (weighted) parameter distributions. The visual design includes:
#' 
#' \itemize{
#'   \item \strong{Prior (Light Blue #457B9D)}: Histogram, density curve, and mode line
#'   \item \strong{Posterior (Dark Blue #1D3557)}: Histogram, density curve, and mode line
#'   \item \strong{Maximum Likelihood (Red #E63946)}: ML estimate line (optional)
#'   \item Automatic separation of global and location-specific parameters
#'   \item Location plots structured with initial conditions section and other parameters section
#'   \item Clean styling with minimal theme
#' }
#'
#' If \code{posterior_weights} is provided, those weights are used directly.
#' Otherwise, weights are calculated internally using [calc_model_posterior_distributions()].
#' This allows for efficient reuse of weights when creating multiple plots.
#' 
#' By default, \code{filter_weights = FALSE}, meaning all samples with valid (non-NA, 
#' finite, non-negative) weights are used. This preserves the full posterior distribution. 
#' Setting \code{filter_weights = TRUE} applies adaptive thresholds to remove very small 
#' weights, which may be useful for computational efficiency but can distort the posterior.
#'
#' @examples
#' \dontrun{
#' # Basic usage - no weight filtering (default)
#' plot_model_posteriors(
#'   results = calibration_results,
#'   col_ll = "likelihood",
#'   col_params = 4:ncol(calibration_results),
#'   output_dir = "plots"
#' )
#'
#' # With weight filtering for computational efficiency
#' plot_model_posteriors(
#'   results = calibration_results,
#'   col_ll = "likelihood", 
#'   col_params = 4:8,
#'   filter_weights = TRUE,
#'   output_dir = "plots"
#' )
#'
#' # Efficient usage - reuse pre-calculated weights
#' posterior <- calc_model_posterior_distributions(results, "likelihood", 4:8)
#' plot_model_posteriors(
#'   results = results,
#'   col_ll = "likelihood",
#'   col_params = 4:8,
#'   posterior_weights = posterior$posterior_weights,
#'   show_ml = TRUE,
#'   show_modes = TRUE
#' )
#' }
#'
#' @seealso [calc_model_posterior_distributions()], [plot_model_priors()]
#' @family posterior-analysis
#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density geom_vline facet_wrap theme_minimal theme element_text element_rect element_blank element_line labs ggsave after_stat scale_x_continuous
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate filter group_by summarise case_when select any_of everything
#' @importFrom stats density weighted.mean
#' @importFrom cowplot plot_grid ggdraw draw_label add_sub
plot_model_posteriors <- function(results,
                                              col_ll,
                                              col_params,
                                              posterior_weights = NULL,
                                              output_dir = NULL,
                                              ncol = NULL,
                                              show_modes = TRUE,
                                              show_ml = TRUE,
                                              filter_weights = FALSE,
                                              verbose = TRUE) {
  
  # Input validation
  stopifnot(is.data.frame(results))
  
  # Handle column specification (index or name)
  if (is.character(col_ll)) {
    if (!col_ll %in% names(results)) {
      stop("Column '", col_ll, "' not found in results")
    }
    col_ll_idx <- which(names(results) == col_ll)
  } else {
    stopifnot(is.numeric(col_ll) && length(col_ll) == 1)
    stopifnot(col_ll > 0 && col_ll <= ncol(results))
    col_ll_idx <- col_ll
  }
  
  # Handle parameter column specification
  if (is.character(col_params)) {
    param_indices <- which(names(results) %in% col_params)
    if (length(param_indices) == 0) {
      stop("None of the specified parameter columns found in results")
    }
  } else {
    stopifnot(is.numeric(col_params) && all(col_params > 0 & col_params <= ncol(results)))
    param_indices <- col_params
  }
  
  # Calculate or use provided weights
  if (is.null(posterior_weights)) {
    if (verbose) message("Calculating posterior weights using AIC weights...")
    
    # Calculate weights using calc_model_posterior_distributions
    posterior_result <- calc_model_posterior_distributions(
      results, col_ll_idx, param_indices
    )
    posterior_weights <- posterior_result$posterior_weights
  }
  
  # Validate weights
  if (length(posterior_weights) != nrow(results)) {
    stop("Length of posterior_weights must match number of rows in results")
  }
  
  # Filter to valid samples (optional weight filtering)
  if (filter_weights) {
    # Use adaptive threshold to avoid filtering out very small but valid weights
    weight_threshold <- max(1e-15, max(posterior_weights, na.rm = TRUE) * 1e-8)
    valid_idx <- !is.na(posterior_weights) & posterior_weights > weight_threshold
    
    # Ensure minimum number of samples for meaningful distributions
    min_samples <- 10
    if (sum(valid_idx) < min_samples) {
      # If too few samples with strict threshold, use top N samples
      if (verbose) warning("Only ", sum(valid_idx), " samples pass weight threshold. Using top ", 
                          min_samples, " samples instead.")
      top_indices <- order(posterior_weights, decreasing = TRUE, na.last = NA)[1:min(min_samples, length(posterior_weights))]
      valid_idx <- seq_along(posterior_weights) %in% top_indices
    }
    
    if (verbose) {
      message("Weight filtering enabled: Using ", sum(valid_idx), " samples for posterior distributions (", 
              round(100 * sum(valid_idx) / length(valid_idx), 1), "% of total)")
    }
  } else {
    # No weight filtering - only remove invalid weights (NA, infinite, negative)
    valid_idx <- !is.na(posterior_weights) & is.finite(posterior_weights) & posterior_weights >= 0
    
    if (verbose) {
      message("Weight filtering disabled: Using ", sum(valid_idx), " samples for posterior distributions (", 
              round(100 * sum(valid_idx) / length(valid_idx), 1), "% of total)")
    }
  }
  
  results_valid <- results_plot <- results[valid_idx, ]
  weights_plot <- posterior_weights[valid_idx]
  
  # Define parameter categories
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
  
  # Define columns that should be excluded (not model parameters)
  exclude_cols <- c(
    "importance_weight", "importance_weights", "weights",
    "N_j_initial", "retained", "converged", 
    "sim", "iter", "seed", "time", "timestamp",
    "likelihood", "log_likelihood", "ll", "loglik",
    "acceptance", "accept", "rejected"
  )
  
  # Define location-specific parameters (base names without location suffixes)
  location_params_base <- c(
    "S_j_initial", "E_j_initial", "I_j_initial",
    "R_j_initial", "V1_j_initial", "V2_j_initial",  # Initial conditions
    "beta_j0_env", "beta_j0_hum",                   # Transmission rates (derived)
    "beta_j0_tot", "p_beta",                        # Transmission rates (primary)
    "tau_i", "theta_j",                             # Mobility rate and WASH
    "a_1_j", "a_2_j", "b_1_j", "b_2_j",            # Seasonal parameters
    "a1", "a2", "b1", "b2"                         # Alternative seasonal names
  )
  
  # Prepare data for plotting
  plot_data <- results_plot[, param_indices, drop = FALSE]
  plot_data$weights <- weights_plot
  
  # Get parameter names for labeling
  param_names <- names(results)[param_indices]
  names(plot_data)[1:length(param_indices)] <- param_names
  
  # Debug: Check for transmission parameters
  if (verbose) {
    transmission_check <- c("beta_j0_tot", "p_beta", "beta_j0_hum", "beta_j0_env")
    cat("Checking for transmission parameters:\n")
    missing_params <- character()
    for (param_base in transmission_check) {
      matching <- param_names[grepl(paste0("^", param_base, "_[A-Z]{3}$"), param_names)]
      if (length(matching) > 0) {
        cat("  ✓", param_base, "found:", paste(matching, collapse = ", "), "\n")
      } else {
        cat("  ✗", param_base, "missing\n")
        missing_params <- c(missing_params, param_base)
      }
    }
    
    # Warn about missing key parameters
    if ("beta_j0_tot" %in% missing_params || "p_beta" %in% missing_params) {
      warning("Missing key transmission parameters (beta_j0_tot, p_beta). ",
              "This may be because calibration results were generated before ",
              "these parameters were added to convert_config_to_matrix. ",
              "Consider re-running calibration to include all transmission parameters.")
    }
  }
  
  # Handle new beta parameterization: derive beta_j0_hum and beta_j0_env if needed
  # Check for each location
  iso_pattern <- "_[A-Z]{3}$"
  location_suffixes <- unique(gsub(".*(_[A-Z]{3})$", "\1", 
                                   param_names[grepl(iso_pattern, param_names)]))
  
  for (suffix in location_suffixes) {
    beta_tot_col <- paste0("beta_j0_tot", suffix)
    p_beta_col <- paste0("p_beta", suffix)
    beta_hum_col <- paste0("beta_j0_hum", suffix)
    beta_env_col <- paste0("beta_j0_env", suffix)
    
    # If we have the new parameters but not the old ones, derive them
    if (beta_tot_col %in% param_names && p_beta_col %in% param_names) {
      if (!(beta_hum_col %in% param_names)) {
        # Derive beta_j0_hum
        plot_data[[beta_hum_col]] <- plot_data[[p_beta_col]] * plot_data[[beta_tot_col]]
        param_names <- c(param_names, beta_hum_col)
      }
      if (!(beta_env_col %in% param_names)) {
        # Derive beta_j0_env
        plot_data[[beta_env_col]] <- (1 - plot_data[[p_beta_col]]) * plot_data[[beta_tot_col]]
        param_names <- c(param_names, beta_env_col)
      }
    }
  }
  
  # Reshape to long format (exclude weights column)
  plot_long <- tidyr::pivot_longer(
    plot_data, 
    cols = -weights,
    names_to = "parameter",
    values_to = "value"
  ) %>%
    dplyr::filter(!is.na(value)) %>%
    # Additional validation: ensure we have sufficient data per parameter
    dplyr::group_by(parameter) %>%
    dplyr::filter(dplyr::n() >= 3) %>%  # Need at least 3 observations per parameter
    dplyr::ungroup() %>%
    dplyr::mutate(
      param_base = gsub("_[A-Z]{3}$", "", parameter),
      param_type = dplyr::case_when(
        parameter %in% exclude_cols ~ "Exclude",  # Explicitly exclude non-parameters first
        parameter %in% global_params ~ "Global",
        param_base %in% location_params_base ~ "Location-Specific",
        grepl("_[A-Z]{3}$", parameter) ~ "Location-Specific",
        TRUE ~ "Exclude"  # Default to exclude anything not recognized
      ),
      category = dplyr::case_when(
        grepl("_initial", parameter) ~ "1_Initial Conditions",
        grepl("^(phi_|omega_|gamma_|epsilon|rho|sigma|iota)", parameter) ~ "2_Epidemiological",
        grepl("^(alpha_|zeta_|kappa)", parameter) ~ "3_Environmental",
        grepl("^(beta_j0_|beta_j0|p_beta)", parameter) ~ "4_Transmission",
        grepl("^(a_|b_|a1|a2|b1|b2)", parameter) ~ "5_Seasonality",
        grepl("^(decay_|vaccine)", parameter) ~ "6_Immunity & Vaccine",
        grepl("^(mobility_|tau_)", parameter) ~ "7_Mobility",
        grepl("^theta", parameter) ~ "8_WASH",
        TRUE ~ "9_Other"
      )
    )
  
  # Filter out non-parameter columns and split into global and location-specific
  plot_long_filtered <- plot_long %>% dplyr::filter(param_type != "Exclude")
  results_global <- plot_long_filtered %>% dplyr::filter(param_type == "Global")
  results_location <- plot_long_filtered %>% dplyr::filter(param_type == "Location-Specific")
  
  # Extract ISO codes from location parameters
  extract_iso <- function(param_names) {
    iso_codes <- unique(gsub(".*_([A-Z]{3})$", "\\1",
                           param_names[grepl("_[A-Z]{3}$", param_names)]))
    return(iso_codes)
  }
  
  unique_locations <- extract_iso(results_location$parameter)
  
  # Create helper function for calculating modes with improved edge case handling
  calc_mode <- function(values, weights = NULL) {
    # Remove NAs
    valid_idx <- !is.na(values)
    values <- values[valid_idx]
    if (!is.null(weights)) weights <- weights[valid_idx]
    
    if (length(values) == 0) return(NA_real_)
    if (length(values) == 1) return(values[1])
    
    n_unique <- length(unique(values))
    
    # Handle case where all values are identical
    if (n_unique == 1) return(values[1])
    
    # Handle case with very few unique values
    if (n_unique <= 3 && length(values) < 10) {
      # Use weighted mean for sparse data instead of mode
      if (!is.null(weights) && sum(weights) > 0) {
        return(weighted.mean(values, weights, na.rm = TRUE))
      } else {
        return(mean(values, na.rm = TRUE))
      }
    }
    
    tryCatch({
      if (!is.null(weights) && sum(weights) > 0 && !all(weights == weights[1]) && length(values) >= 5) {
        # Weighted mode using density estimation
        w_norm <- weights / sum(weights)
        # Adjust bandwidth based on data spread and sample size
        bw_adjust <- max(0.5, min(2.0, 1.2 * sqrt(length(values) / 50)))
        dens <- density(values, weights = w_norm, adjust = bw_adjust, n = min(512, length(values) * 10))
        return(dens$x[which.max(dens$y)])
      } else {
        # Unweighted mode using density estimation
        if (length(values) >= 5) {
          bw_adjust <- max(0.5, min(2.0, 1.2 * sqrt(length(values) / 50)))
          dens <- density(values, adjust = bw_adjust, n = min(512, length(values) * 10))
          return(dens$x[which.max(dens$y)])
        } else {
          # For very small samples, use median
          return(median(values, na.rm = TRUE))
        }
      }
    }, error = function(e) {
      # Robust fallback
      if (!is.null(weights) && sum(weights) > 0) {
        weighted.mean(values, weights, na.rm = TRUE)
      } else {
        median(values, na.rm = TRUE)
      }
    })
  }
  
  # Helper function for adaptive binning
  calc_optimal_bins <- function(n_obs, n_unique) {
    if (n_obs <= 5) return(max(2, n_unique))
    if (n_obs <= 10) return(max(3, min(5, n_unique)))
    if (n_obs <= 30) return(max(5, min(15, ceiling(n_unique / 2))))
    # Standard case - use Freedman-Diaconis or Scott's rule approximation
    return(max(8, min(30, ceiling(sqrt(n_obs) * 2))))
  }
  
  # Helper function to sample from posterior using importance weights
  sample_posterior <- function(values, weights, n_samples = 1000) {
    if (length(values) == 0 || sum(weights) == 0) return(numeric(0))
    
    # Normalize weights
    weights_norm <- weights / sum(weights)
    
    # Sample with replacement using weights as probabilities
    sample_indices <- sample(
      x = seq_along(values),
      size = min(n_samples, length(values)),
      prob = weights_norm,
      replace = TRUE
    )
    
    return(values[sample_indices])
  }
  
  # Create the plot
  # Helper function to create posterior plot
  create_posterior_plot <- function(data, title, subtitle, output_file = NULL, 
                                   ncol_plot = NULL, separate_ic = FALSE) {
    
    if (nrow(data) == 0) return(NULL)
    
    # Define color palette - uniform light/dark blue theme
    col_prior <- "#457b9d"       # Light blue for prior (same as plot_model_priors)
    col_posterior <- "#1d3557"   # Dark blue for posterior
    col_ml <- "#e63946"          # Red for ML estimate
    
    # Calculate statistics for each parameter if showing modes/ML
    stats_df <- NULL
    if (show_modes || show_ml) {
      stats_df <- data %>%
        dplyr::group_by(parameter) %>%
        dplyr::summarise(
          ml_value = if(show_ml) value[which.max(weights)] else NA,
          prior_mode = if(show_modes) calc_mode(value, NULL) else NA,
          posterior_mode = if(show_modes) calc_mode(value, weights) else NA,
          .groups = "drop"
        )
    }
    
    # For location-specific plots with separate IC section
    if (separate_ic) {
      # Separate initial conditions from other parameters
      data_ic <- data %>% dplyr::filter(grepl("_initial", parameter))
      data_other <- data %>% dplyr::filter(!grepl("_initial", parameter))
      
      plot_list <- list()
      
      # Create initial conditions plots
      if (nrow(data_ic) > 0) {
        ic_plots <- list()
        
        for (param in unique(data_ic$parameter)) {
          param_data <- data_ic %>% dplyr::filter(parameter == param)
          param_stats <- if(!is.null(stats_df)) stats_df %>% dplyr::filter(parameter == param) else NULL
          
          # Clean parameter name for display
          param_clean <- gsub("_[A-Z]{3}$", "", param)
          param_label <- switch(param_clean,
            "S_j_initial" = "Susceptible (S)",
            "E_j_initial" = "Exposed (E)",
            "I_j_initial" = "Infected (I)",
            "R_j_initial" = "Recovered (R)",
            "V1_j_initial" = "Vaccinated 1 dose (V1)",
            "V2_j_initial" = "Vaccinated 2 doses (V2)",
            param_clean
          )
          
          # Calculate adaptive bins for this parameter
          n_obs <- nrow(param_data)
          n_unique <- length(unique(param_data$value))
          n_bins <- calc_optimal_bins(n_obs, n_unique)
          
          # Sample from posterior using importance weights
          posterior_samples <- sample_posterior(param_data$value, param_data$weights, n_samples = 1000)
          
          # Create base plot with prior (unweighted) histogram
          p <- ggplot2::ggplot(param_data, ggplot2::aes(x = value)) +
            ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                  bins = n_bins, fill = col_prior, alpha = 0.4, color = "white") +
            ggplot2::theme_minimal()
          
          # Add posterior histogram if we have samples
          if (length(posterior_samples) > 0) {
            posterior_df <- data.frame(value = posterior_samples)
            post_bins <- calc_optimal_bins(length(posterior_samples), length(unique(posterior_samples)))
            p <- p + ggplot2::geom_histogram(data = posterior_df, ggplot2::aes(x = value, y = ggplot2::after_stat(density)),
                                           bins = post_bins, fill = col_posterior, alpha = 0.7, color = "white")
          }
          
          # Add density layers with error handling
          tryCatch({
            if (n_obs >= 3 && n_unique >= 2) {
              p <- p + ggplot2::geom_density(color = col_prior, linewidth = 0.7, linetype = "solid")
            }
          }, error = function(e) {
            if (verbose) message("Skipping prior density curve for ", param, ": ", e$message)
          })
          
          tryCatch({
            if (n_obs >= 3 && n_unique >= 2 && sum(param_data$weights) > 0) {
              p <- p + ggplot2::geom_density(ggplot2::aes(weight = weights), color = col_posterior, 
                                            linewidth = 1, linetype = "solid")
            }
          }, error = function(e) {
            if (verbose) message("Skipping posterior density curve for ", param, ": ", e$message)
          })
          
          if (!is.null(param_stats)) {
            if (show_modes && !is.na(param_stats$prior_mode)) {
              p <- p + ggplot2::geom_vline(xintercept = param_stats$prior_mode,
                                          color = col_prior, linetype = "dashed", linewidth = 0.4)
            }
            if (show_modes && !is.na(param_stats$posterior_mode)) {
              p <- p + ggplot2::geom_vline(xintercept = param_stats$posterior_mode,
                                          color = col_posterior, linetype = "dashed", linewidth = 0.6)
            }
            if (show_ml && !is.na(param_stats$ml_value)) {
              p <- p + ggplot2::geom_vline(xintercept = param_stats$ml_value,
                                          color = col_ml, linetype = "dotted", linewidth = 0.5)
            }
          }
          
          p <- p + ggplot2::theme_minimal() +
            ggplot2::theme(
              plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5),
              axis.title = ggplot2::element_blank(),
              panel.grid.minor = ggplot2::element_blank()
            ) +
            ggplot2::labs(title = param_label)
          
          ic_plots[[param_clean]] <- p
        }
        
        # Combine IC plots
        if (length(ic_plots) > 0) {
          ic_combined <- cowplot::plot_grid(plotlist = ic_plots, ncol = 3, nrow = 2)
          ic_heading <- cowplot::ggdraw() +
            cowplot::draw_label("Initial Conditions", fontface = 'bold', size = 14, x = 0.5, y = 0.5)
          ic_combined <- cowplot::add_sub(ic_combined, "Proportion of Population", size = 10, vjust = 0)
          ic_combined <- cowplot::ggdraw(ic_combined) +
            cowplot::draw_label("Density", x = 0.02, y = 0.5, angle = 90, size = 10)
          
          plot_list$ic_heading <- ic_heading
          plot_list$ic_plots <- ic_combined
        }
      }
      
      # Create other parameter plots organized by category
      if (nrow(data_other) > 0) {
        # Separate transmission parameters from other parameters
        data_transmission <- data_other %>% 
          dplyr::filter(category == "4_Transmission" | grepl("^(beta_j0_|p_beta)", parameter))
        data_other_non_transmission <- data_other %>% 
          dplyr::filter(category != "4_Transmission" & !grepl("^(beta_j0_|p_beta)", parameter))
        
        # Create Transmission Parameters section (matching priors layout)
        if (nrow(data_transmission) > 0) {
          transmission_plots <- list()
          
          if (verbose) {
            cat("Found transmission parameters for plotting:\n")
            print(unique(data_transmission$parameter))
          }
          
          for (param in unique(data_transmission$parameter)) {
            param_data <- data_transmission %>% dplyr::filter(parameter == param)
            param_stats <- if(!is.null(stats_df)) stats_df %>% dplyr::filter(parameter == param) else NULL
            
            # Clean parameter name
            param_clean <- gsub("_[A-Z]{3}$", "", param)
            param_label <- switch(param_clean,
              "beta_j0_tot" = "Total Transmission Rate (β₀)",
              "p_beta" = "Proportion Human Transmission (p_β)",
              "beta_j0_hum" = "Human Transmission (Derived)",
              "beta_j0_env" = "Environmental Transmission (Derived)",
              param_clean
            )
            
            # Calculate adaptive bins for this parameter
            n_obs <- nrow(param_data)
            n_unique <- length(unique(param_data$value))
            n_bins <- calc_optimal_bins(n_obs, n_unique)
            
            # Sample from posterior using importance weights
            posterior_samples <- sample_posterior(param_data$value, param_data$weights, n_samples = 1000)
            
            # Create base plot with prior (unweighted) histogram
            p <- ggplot2::ggplot(param_data, ggplot2::aes(x = value)) +
              ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                    bins = n_bins, fill = col_prior, alpha = 0.4, color = "white") +
              ggplot2::theme_minimal()
            
            # Add posterior histogram if we have samples
            if (length(posterior_samples) > 0) {
              posterior_df <- data.frame(value = posterior_samples)
              post_bins <- calc_optimal_bins(length(posterior_samples), length(unique(posterior_samples)))
              p <- p + ggplot2::geom_histogram(data = posterior_df, ggplot2::aes(x = value, y = ggplot2::after_stat(density)),
                                             bins = post_bins, fill = col_posterior, alpha = 0.7, color = "white")
            }
            
            # Add density layers with error handling
            tryCatch({
              if (n_obs >= 3 && n_unique >= 2) {
                p <- p + ggplot2::geom_density(color = col_prior, linewidth = 0.7, linetype = "solid")
              }
            }, error = function(e) {
              if (verbose) message("Skipping prior density curve for ", param, ": ", e$message)
            })
            
            tryCatch({
              if (n_obs >= 3 && n_unique >= 2 && sum(param_data$weights) > 0) {
                p <- p + ggplot2::geom_density(ggplot2::aes(weight = weights), color = col_posterior,
                                              linewidth = 1, linetype = "solid")
              }
            }, error = function(e) {
              if (verbose) message("Skipping posterior density curve for ", param, ": ", e$message)
            })
            
            if (!is.null(param_stats)) {
              if (show_modes && !is.na(param_stats$prior_mode)) {
                p <- p + ggplot2::geom_vline(xintercept = param_stats$prior_mode,
                                            color = col_prior, linetype = "dashed", linewidth = 0.4)
              }
              if (show_modes && !is.na(param_stats$posterior_mode)) {
                p <- p + ggplot2::geom_vline(xintercept = param_stats$posterior_mode,
                                            color = col_posterior, linetype = "dashed", linewidth = 0.6)
              }
              if (show_ml && !is.na(param_stats$ml_value)) {
                p <- p + ggplot2::geom_vline(xintercept = param_stats$ml_value,
                                            color = col_ml, linetype = "dotted", linewidth = 0.5)
              }
            }
            
            p <- p + ggplot2::theme_minimal() +
              ggplot2::theme(
                plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5),
                axis.title = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank()
              ) +
              ggplot2::labs(title = param_label)
            
            transmission_plots[[param_clean]] <- p
          }
          
          # Combine transmission plots in 2x2 grid (matching priors layout)
          if (length(transmission_plots) > 0) {
            transmission_combined <- cowplot::plot_grid(plotlist = transmission_plots, ncol = 2, nrow = 2)
            transmission_heading <- cowplot::ggdraw() +
              cowplot::draw_label("Transmission Parameters", fontface = 'bold', size = 14, x = 0.5, y = 0.5)
            
            plot_list$transmission_heading <- transmission_heading
            plot_list$transmission_plots <- transmission_combined
          }
        }
        
        # Create Other Parameters section
        if (nrow(data_other_non_transmission) > 0) {
          other_plots <- list()
          
          for (param in unique(data_other_non_transmission$parameter)) {
            param_data <- data_other_non_transmission %>% dplyr::filter(parameter == param)
            param_stats <- if(!is.null(stats_df)) stats_df %>% dplyr::filter(parameter == param) else NULL
            
            # Clean parameter name
            param_clean <- gsub("_[A-Z]{3}$", "", param)
            param_label <- switch(param_clean,
              "theta_j" = "WASH Coverage (θ_j)",
              "tau_i" = "Travel Probability",
              "a1" = "Seasonality: Cosine 1",
              "a2" = "Seasonality: Cosine 2", 
              "b1" = "Seasonality: Sine 1",
              "b2" = "Seasonality: Sine 2",
              param_clean
            )
            
            # Calculate adaptive bins for this parameter
            n_obs <- nrow(param_data)
            n_unique <- length(unique(param_data$value))
            n_bins <- calc_optimal_bins(n_obs, n_unique)
            
            # Sample from posterior using importance weights
            posterior_samples <- sample_posterior(param_data$value, param_data$weights, n_samples = 1000)
            
            # Create base plot with prior (unweighted) histogram
            p <- ggplot2::ggplot(param_data, ggplot2::aes(x = value)) +
              ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                    bins = n_bins, fill = col_prior, alpha = 0.4, color = "white") +
              ggplot2::theme_minimal()
            
            # Add posterior histogram if we have samples
            if (length(posterior_samples) > 0) {
              posterior_df <- data.frame(value = posterior_samples)
              post_bins <- calc_optimal_bins(length(posterior_samples), length(unique(posterior_samples)))
              p <- p + ggplot2::geom_histogram(data = posterior_df, ggplot2::aes(x = value, y = ggplot2::after_stat(density)),
                                             bins = post_bins, fill = col_posterior, alpha = 0.7, color = "white")
            }
            
            # Add density layers with error handling
            tryCatch({
              if (n_obs >= 3 && n_unique >= 2) {
                p <- p + ggplot2::geom_density(color = col_prior, linewidth = 0.7, linetype = "solid")
              }
            }, error = function(e) {
              if (verbose) message("Skipping prior density curve for ", param, ": ", e$message)
            })
            
            tryCatch({
              if (n_obs >= 3 && n_unique >= 2 && sum(param_data$weights) > 0) {
                p <- p + ggplot2::geom_density(ggplot2::aes(weight = weights), color = col_posterior,
                                              linewidth = 1, linetype = "solid")
              }
            }, error = function(e) {
              if (verbose) message("Skipping posterior density curve for ", param, ": ", e$message)
            })
            
            if (!is.null(param_stats)) {
              if (show_modes && !is.na(param_stats$prior_mode)) {
                p <- p + ggplot2::geom_vline(xintercept = param_stats$prior_mode,
                                            color = col_prior, linetype = "dashed", linewidth = 0.4)
              }
              if (show_modes && !is.na(param_stats$posterior_mode)) {
                p <- p + ggplot2::geom_vline(xintercept = param_stats$posterior_mode,
                                            color = col_posterior, linetype = "dashed", linewidth = 0.6)
              }
              if (show_ml && !is.na(param_stats$ml_value)) {
                p <- p + ggplot2::geom_vline(xintercept = param_stats$ml_value,
                                            color = col_ml, linetype = "dotted", linewidth = 0.5)
              }
            }
            
            p <- p + ggplot2::theme_minimal() +
              ggplot2::theme(
                plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5),
                panel.grid.minor = ggplot2::element_blank()
              ) +
              ggplot2::labs(title = param_label, x = "Value", y = "Density")
            
            other_plots[[param_clean]] <- p
          }
          
          # Combine other plots (keep existing 3-column layout)
          if (length(other_plots) > 0) {
            n_plots <- length(other_plots)
            n_cols <- 3
            n_rows <- ceiling(n_plots / n_cols)
            
            other_combined <- cowplot::plot_grid(plotlist = other_plots, ncol = n_cols, nrow = n_rows)
            other_heading <- cowplot::ggdraw() +
              cowplot::draw_label("Other Location-Specific Parameters", fontface = 'bold', size = 14, x = 0.5, y = 0.5)
            
            plot_list$other_heading <- other_heading
            plot_list$other_plots <- other_combined
          }
        }
      }
      
      # Combine all elements
      if (length(plot_list) > 0) {
        final_plot_list <- list()
        
        # Add title
        main_title <- cowplot::ggdraw() +
          cowplot::draw_label(title, fontface = 'bold', size = 16, x = 0.5, y = 0.7, hjust = 0.5) +
          cowplot::draw_label(subtitle, fontface = 'plain', size = 12, x = 0.5, y = 0.3, hjust = 0.5, color = "gray30")
        
        final_plot_list[[1]] <- main_title
        
        # Add sections in the correct order (matching priors layout)
        if (!is.null(plot_list$ic_heading)) {
          final_plot_list[[length(final_plot_list) + 1]] <- plot_list$ic_heading
          final_plot_list[[length(final_plot_list) + 1]] <- plot_list$ic_plots
        }
        if (!is.null(plot_list$transmission_heading)) {
          final_plot_list[[length(final_plot_list) + 1]] <- plot_list$transmission_heading
          final_plot_list[[length(final_plot_list) + 1]] <- plot_list$transmission_plots
        }
        if (!is.null(plot_list$other_heading)) {
          final_plot_list[[length(final_plot_list) + 1]] <- plot_list$other_heading
          final_plot_list[[length(final_plot_list) + 1]] <- plot_list$other_plots
        }
        
        # Calculate heights
        heights <- c(0.08)  # Title
        if (!is.null(plot_list$ic_heading)) heights <- c(heights, 0.04, 0.35)
        if (!is.null(plot_list$transmission_heading)) heights <- c(heights, 0.04, 0.35)
        if (!is.null(plot_list$other_heading)) {
          # Calculate number of rows based on parameters that actually exist
          n_other_params <- if (exists("other_plots")) length(other_plots) else 0
          n_rows <- ceiling(n_other_params / 3)
          heights <- c(heights, 0.04, 0.35 * (n_rows / 2))
        }
        
        p <- cowplot::plot_grid(plotlist = final_plot_list, ncol = 1, rel_heights = heights)
      }
      
    } else {
      # Standard faceted plot for global parameters
      # Calculate adaptive bins based on data characteristics
      data_summary <- data %>%
        dplyr::group_by(parameter) %>%
        dplyr::summarise(
          n_obs = dplyr::n(),
          n_unique = length(unique(value)),
          optimal_bins = calc_optimal_bins(dplyr::n(), length(unique(value))),
          .groups = "drop"
        )
      
      # Use median number of bins across all parameters
      median_bins <- max(5, median(data_summary$optimal_bins, na.rm = TRUE))
      
      # Sample from posterior for each parameter
      posterior_data <- data %>%
        dplyr::group_by(parameter) %>%
        dplyr::do({
          posterior_samples <- sample_posterior(.$value, .$weights, n_samples = 1000)
          if (length(posterior_samples) > 0) {
            data.frame(
              parameter = .$parameter[1],
              value = posterior_samples,
              stringsAsFactors = FALSE
            )
          } else {
            data.frame(parameter = character(0), value = numeric(0), stringsAsFactors = FALSE)
          }
        }) %>%
        dplyr::ungroup()
      
      # Create base plot with prior (unweighted) histogram
      p <- ggplot2::ggplot(data, ggplot2::aes(x = value)) +
        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                              bins = median_bins, fill = col_prior, alpha = 0.4, color = "white", linewidth = 0.2) +
        ggplot2::theme_minimal(base_size = 9)
      
      # Add posterior histogram if we have posterior samples
      if (nrow(posterior_data) > 0) {
        p <- p + ggplot2::geom_histogram(data = posterior_data, ggplot2::aes(x = value, y = ggplot2::after_stat(density)),
                                       bins = median_bins, fill = col_posterior, alpha = 0.7, color = "white", linewidth = 0.2)
      }
      
      # Add density curves with error handling for faceted plot
      tryCatch({
        # Check if we have sufficient data for density estimation
        sufficient_data <- data %>%
          dplyr::group_by(parameter) %>%
          dplyr::summarise(
            n_obs = dplyr::n(),
            n_unique = length(unique(value)),
            .groups = "drop"
          ) %>%
          dplyr::filter(n_obs >= 3, n_unique >= 2)
        
        if (nrow(sufficient_data) > 0) {
          p <- p + ggplot2::geom_density(color = col_prior, linewidth = 0.7, linetype = "solid")
        }
      }, error = function(e) {
        if (verbose) message("Skipping prior density curves: ", e$message)
      })
      
      tryCatch({
        # Check weighted data availability
        if (any(data$weights > 0, na.rm = TRUE)) {
          p <- p + ggplot2::geom_density(ggplot2::aes(weight = weights), color = col_posterior,
                                        linewidth = 1, linetype = "solid")
        }
      }, error = function(e) {
        if (verbose) message("Skipping posterior density curves: ", e$message)
      })
      
      # Add mode and ML lines if requested
      if (!is.null(stats_df)) {
        if (show_modes) {
          p <- p + ggplot2::geom_vline(data = stats_df, ggplot2::aes(xintercept = prior_mode),
                                      color = col_prior, linetype = "dashed", linewidth = 0.4, alpha = 1.0) +
                   ggplot2::geom_vline(data = stats_df, ggplot2::aes(xintercept = posterior_mode),
                                      color = col_posterior, linetype = "dashed", linewidth = 0.6, alpha = 1.0)
        }
        if (show_ml) {
          p <- p + ggplot2::geom_vline(data = stats_df, ggplot2::aes(xintercept = ml_value),
                                      color = col_ml, linetype = "dotted", linewidth = 0.5, alpha = 1.0)
        }
      }
      
      p <- p + ggplot2::facet_wrap(~ parameter, scales = "free", ncol = ncol_plot) +
        ggplot2::theme_minimal(base_size = 9) +
        ggplot2::theme(
          strip.text = ggplot2::element_text(size = 7, face = "bold", color = "black"),
          strip.background = ggplot2::element_blank(),
          panel.spacing = ggplot2::unit(0.8, "lines"),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_line(linewidth = 0.15, color = "grey92"),
          panel.border = ggplot2::element_rect(color = "grey85", fill = NA, linewidth = 0.3),
          axis.text = ggplot2::element_text(size = 6, color = "black"),
          axis.title = ggplot2::element_text(size = 8, face = "bold"),
          plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5),
          plot.subtitle = ggplot2::element_text(size = 9, hjust = 0.5, color = "grey30"),
          plot.caption = ggplot2::element_text(size = 7, hjust = 1, face = "italic", color = "grey50")
        ) +
        ggplot2::labs(x = "Parameter Value", y = "Density", title = title, subtitle = subtitle,
                     caption = paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    }
    
    # Save plot if output file specified
    if (!is.null(output_file)) {
      # Increase height for better readability
      plot_height <- if (separate_ic) 16 else 14  # Taller for location plots with IC section
      ggplot2::ggsave(output_file, plot = p, width = 14, height = plot_height, dpi = 300)
      if (verbose) message("Saved: ", output_file)
    }
    
    return(p)
  }
  
  # Create subtitle for plots
  n_successful <- nrow(results_valid)
  n_total <- nrow(results)
  
  # Infer n_sim and n_iter from results if available
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
  
  # Create output directory if specified
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
      if (verbose) message("Created output directory: ", output_dir)
    }
  }
  
  # Initialize plot list
  plot_list <- list()
  
  # Create GLOBAL parameters plot
  if (nrow(results_global) > 0) {
    output_file <- if (!is.null(output_dir)) file.path(output_dir, "posteriors_global.pdf") else NULL
    
    # Set default ncol for global plot if not specified
    ncol_global <- if (is.null(ncol)) 4 else ncol
    
    p_global <- create_posterior_plot(
      results_global,
      title = "Model Posterior Distributions: Global Parameters",
      subtitle = subtitle_text,
      output_file = output_file,
      ncol_plot = ncol_global,
      separate_ic = FALSE
    )
    
    if (!is.null(p_global)) {
      plot_list$global <- p_global
      if (verbose) message("Created global parameters plot")
    }
  } else {
    if (verbose) message("No global parameters found to plot")
  }
  
  # Create LOCATION-SPECIFIC parameters plots
  location_plots <- list()
  
  if (length(unique_locations) > 0) {
    if (verbose) message("Creating posterior plots for locations: ", paste(unique_locations, collapse = ", "))
    
    for (loc in unique_locations) {
      # Filter data for this location
      results_loc <- results_location %>%
        dplyr::filter(grepl(paste0("_", loc, "$"), parameter))
      
      if (nrow(results_loc) == 0) next
      
      # Remove location suffix for cleaner display
      results_loc <- results_loc %>%
        dplyr::mutate(
          parameter_display = gsub(paste0("_", loc, "$"), "", parameter),
          parameter = factor(parameter_display, levels = unique(parameter_display[order(category, parameter_display)]))
        ) %>%
        dplyr::select(-parameter_display, -param_base)
      
      # Create plot for this location
      output_file <- if (!is.null(output_dir)) file.path(output_dir, paste0("posteriors_", loc, ".pdf")) else NULL
      
      p_loc <- create_posterior_plot(
        results_loc,
        title = paste0("Location-Specific Posterior Distributions: ", loc),
        subtitle = subtitle_text,
        output_file = output_file,
        ncol_plot = 3,
        separate_ic = TRUE  # Use structured layout for location plots
      )
      
      if (!is.null(p_loc)) {
        location_plots[[loc]] <- p_loc
        if (verbose) message("Created plot for location: ", loc)
      }
    }
    
    plot_list$location <- location_plots
  } else {
    if (verbose) message("No location-specific parameters found to plot")
  }
  
  # Return plot objects invisibly
  invisible(plot_list)
}