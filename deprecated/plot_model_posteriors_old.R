#' Plot Model Posterior Parameter Distributions
#'
#' Creates publication-quality plots showing prior vs posterior parameter
#' distributions from calibration results. Uses importance sampling weights
#' to display unbiased posterior distributions. Generates separate plots for
#' global and location-specific parameters.
#'
#' @param results Data frame containing simulation results
#' @param posterior_weights Column name containing importance weights (required)
#' @param col_params Vector of parameter column indices or names to plot
#' @param priors_base List containing prior distributions (default: priors_default)
#' @param n_prior_samples Number of samples to draw from prior distributions (default: 10000)
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
#'   \item \strong{Prior (Light Blue #457B9D)}: Samples from actual prior distributions
#'   \item \strong{Posterior (Dark Blue #1D3557)}: Weighted histogram and density curve from results
#'   \item \strong{Maximum Likelihood (Red #E63946)}: ML estimate line (optional)
#'   \item Automatic separation of global and location-specific parameters
#'   \item Location plots structured with initial conditions section and other parameters section
#'   \item Clean styling with minimal theme
#' }
#'
#' The function uses the importance weights directly as provided in the posterior_weights column.
#' Prior distributions are sampled from the priors_base object (default: priors_default).
#'
#' By default, \code{filter_weights = FALSE}, meaning all samples with valid (non-NA,
#' finite, non-negative) weights are used. This preserves the full posterior distribution.
#' Setting \code{filter_weights = TRUE} applies adaptive thresholds to remove very small
#' weights, which may be useful for computational efficiency but can distort the posterior.
#'
#' @examples
#' \dontrun{
#' # Basic usage with importance weights
#' plot_model_posteriors(
#'   results = calibration_results,
#'   posterior_weights = "importance_weight",
#'   col_params = 4:ncol(calibration_results),
#'   output_dir = "plots"
#' )
#'
#' # With custom priors
#' plot_model_posteriors(
#'   results = calibration_results,
#'   posterior_weights = "importance_weight",
#'   col_params = 4:8,
#'   priors_base = my_priors,
#'   n_prior_samples = 5000,
#'   output_dir = "plots"
#' )
#' }
#'
#' @seealso [plot_model_priors()], [sample_from_prior()]
#' @family posterior-analysis
#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density geom_vline facet_wrap theme_minimal theme element_text element_rect element_blank element_line labs ggsave after_stat scale_x_continuous
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate filter group_by summarise case_when select any_of everything
#' @importFrom stats density weighted.mean
#' @importFrom cowplot plot_grid ggdraw draw_label add_sub
plot_model_posteriors <- function(results,
                                 posterior_weights,
                                 col_params,
                                 priors_base = NULL,
                                 n_prior_samples = 10000,
                                 output_dir = NULL,
                                 ncol = NULL,
                                 show_modes = TRUE,
                                 show_ml = TRUE,
                                 filter_weights = FALSE,
                                 verbose = TRUE) {

  # Load default priors if not provided
  if (is.null(priors_base)) {
    if (!exists("priors_default")) {
      data("priors_default", package = "MOSAIC", envir = environment())
    }
    priors_base <- priors_default
  }

  # Input validation
  stopifnot(is.data.frame(results))

  # Handle weight column specification
  if (!posterior_weights %in% names(results)) {
    stop("Column '", posterior_weights, "' not found in results")
  }
  weights <- results[[posterior_weights]]

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

  # Validate weights
  if (length(weights) != nrow(results)) {
    stop("Length of weights must match number of rows in results")
  }

  # Filter to valid samples (optional weight filtering)
  if (filter_weights) {
    # Use adaptive threshold to avoid filtering out very small but valid weights
    weight_threshold <- max(1e-15, max(weights, na.rm = TRUE) * 1e-8)
    valid_idx <- !is.na(weights) & weights > weight_threshold

    # Ensure minimum number of samples for meaningful distributions
    min_samples <- 10
    if (sum(valid_idx) < min_samples) {
      # If too few samples with strict threshold, use top N samples
      if (verbose) warning("Only ", sum(valid_idx), " samples pass weight threshold. Using top ",
                          min_samples, " samples instead.")
      top_indices <- order(weights, decreasing = TRUE, na.last = NA)[1:min(min_samples, length(weights))]
      valid_idx <- seq_along(weights) %in% top_indices
    }

    if (verbose) {
      message("Weight filtering enabled: Using ", sum(valid_idx), " samples for posterior distributions (",
              round(100 * sum(valid_idx) / length(valid_idx), 1), "% of total)")
    }
  } else {
    # No weight filtering - only remove invalid weights (NA, infinite, negative)
    valid_idx <- !is.na(weights) & is.finite(weights) & weights >= 0

    if (verbose) {
      message("Weight filtering disabled: Using ", sum(valid_idx), " samples for posterior distributions (",
              round(100 * sum(valid_idx) / length(valid_idx), 1), "% of total)")
    }
  }

  results_valid <- results_plot <- results[valid_idx, ]
  weights_plot <- weights[valid_idx]

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
    "a1", "a2", "b1", "b2",                        # Alternative seasonal names
    "mu_j",                                         # Case fatality ratio
    "psi_star_a", "psi_star_b", "psi_star_z", "psi_star_k"  # psi_star calibration parameters
  )

  # Prepare data for plotting
  plot_data <- results_plot[, param_indices, drop = FALSE]
  plot_data$weights <- weights_plot

  # Get parameter names for labeling
  param_names <- names(results)[param_indices]
  names(plot_data)[1:length(param_indices)] <- param_names

  # Handle new beta parameterization: derive beta_j0_hum and beta_j0_env if needed
  # Check for each location
  iso_pattern <- "_[A-Z]{3}$"
  location_suffixes <- unique(gsub(".*(_[A-Z]{3})$", "\\1",
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
    dplyr::group_by(parameter) %>%
    dplyr::filter(dplyr::n() >= 3) %>%  # Need at least 3 observations per parameter
    dplyr::ungroup() %>%
    dplyr::mutate(
      param_base = gsub("_[A-Z]{3}$", "", parameter),
      param_type = dplyr::case_when(
        parameter %in% exclude_cols ~ "Exclude",
        param_base %in% exclude_cols ~ "Exclude",
        parameter %in% global_params ~ "Global",
        param_base %in% location_params_base ~ "Location-Specific",
        grepl("_[A-Z]{3}$", parameter) ~ "Location-Specific",
        TRUE ~ "Exclude"
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
        grepl("^mu_j", parameter) ~ "2_Epidemiological",
        grepl("^psi_star_", parameter) ~ "9_Psi_Star_Calibration",
        TRUE ~ "10_Other"
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

  # Helper function to get prior samples for a parameter
  get_prior_samples <- function(param_name, location = NULL) {
    # Clean parameter name
    param_clean <- gsub("_[A-Z]{3}$", "", param_name)

    # Map initial condition parameter names to prior names
    ic_param_map <- list(
      "S_j_initial" = "S_prop",
      "E_j_initial" = "E_prop",
      "I_j_initial" = "I_prop",
      "R_j_initial" = "R_prop",
      "V1_j_initial" = "V1_prop",
      "V2_j_initial" = "V2_prop"
    )

    # Check if it's an initial condition parameter
    if (param_clean %in% names(ic_param_map)) {
      prior_name <- ic_param_map[[param_clean]]
      if (!is.null(priors_base$parameters_initial) && prior_name %in% names(priors_base$parameters_initial)) {
        prior <- priors_base$parameters_initial[[prior_name]]
        return(sample_from_prior(n = n_prior_samples, prior = prior))
      }
    }

    # Try global parameters first
    if (!is.null(priors_base$parameters_global) && param_name %in% names(priors_base$parameters_global)) {
      prior <- priors_base$parameters_global[[param_name]]
      return(sample_from_prior(n = n_prior_samples, prior = prior))
    }

    # Try location-specific parameters
    if (!is.null(location) && !is.null(priors_base$parameters_location)) {
      # Try with location suffix
      param_with_loc <- paste0(param_clean, "_", location)
      if (param_with_loc %in% names(priors_base$parameters_location)) {
        prior <- priors_base$parameters_location[[param_with_loc]]
        return(sample_from_prior(n = n_prior_samples, prior = prior))
      }

      # Try base parameter name for location-specific parameters
      if (param_clean %in% names(priors_base$parameters_location)) {
        prior <- priors_base$parameters_location[[param_clean]]
        return(sample_from_prior(n = n_prior_samples, prior = prior))
      }
    }

    # If no prior found, return NULL
    return(NULL)
  }

  # Create helper function for calculating modes
  calc_mode <- function(values, weights = NULL) {
    valid_idx <- !is.na(values)
    values <- values[valid_idx]
    if (!is.null(weights)) weights <- weights[valid_idx]

    if (length(values) == 0) return(NA_real_)
    if (length(values) == 1) return(values[1])

    n_unique <- length(unique(values))
    if (n_unique == 1) return(values[1])

    if (n_unique <= 3 && length(values) < 10) {
      if (!is.null(weights) && sum(weights) > 0) {
        return(weighted.mean(values, weights, na.rm = TRUE))
      } else {
        return(mean(values, na.rm = TRUE))
      }
    }

    tryCatch({
      if (!is.null(weights) && sum(weights) > 0 && !all(weights == weights[1]) && length(values) >= 5) {
        w_norm <- weights / sum(weights)
        bw_adjust <- max(0.5, min(2.0, 1.2 * sqrt(length(values) / 50)))
        dens <- density(values, weights = w_norm, adjust = bw_adjust, n = min(512, length(values) * 10))
        return(dens$x[which.max(dens$y)])
      } else {
        if (length(values) >= 5) {
          bw_adjust <- max(0.5, min(2.0, 1.2 * sqrt(length(values) / 50)))
          dens <- density(values, adjust = bw_adjust, n = min(512, length(values) * 10))
          return(dens$x[which.max(dens$y)])
        } else {
          return(median(values, na.rm = TRUE))
        }
      }
    }, error = function(e) {
      if (!is.null(weights) && sum(weights) > 0) {
        weighted.mean(values, weights, na.rm = TRUE)
      } else {
        median(values, na.rm = TRUE)
      }
    })
  }

  # Define color palette - matching priors
  col_prior <- "#457b9d"       # Light blue for prior
  col_posterior <- "#1d3557"   # Dark blue for posterior
  col_ml <- "#e63946"          # Red for ML estimate

  # Helper function to create individual parameter plot
  create_param_plot <- function(param_data, param_name, param_label = NULL, location = NULL) {
    if (is.null(param_label)) param_label <- param_name

    n_obs <- nrow(param_data)
    n_unique <- length(unique(param_data$value))
    is_fixed <- n_unique == 1 || (n_obs >= 3 && var(param_data$value, na.rm = TRUE) < 1e-10)

    if (is_fixed) {
      # Fixed parameter visualization
      fixed_val <- param_data$value[1]

      p <- ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5),
          plot.subtitle = ggplot2::element_text(size = 9, hjust = 0.5, color = "gray50"),
          panel.background = ggplot2::element_rect(fill = "white", color = NA)
        ) +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = paste0("Fixed Value\n", format(fixed_val, scientific = FALSE, digits = 4)),
                        size = 4, hjust = 0.5, vjust = 0.5, color = "gray30") +
        ggplot2::labs(title = param_label, subtitle = "(Not sampled)")
    } else {
      # Get prior samples
      prior_samples <- get_prior_samples(param_name, location)

      # Create data frame for posterior
      df_posterior <- data.frame(x = param_data$value, w = param_data$weights)

      # Create base plot
      p <- ggplot2::ggplot()

      # Add prior distribution if available
      if (!is.null(prior_samples) && length(prior_samples) > 0 && !all(is.na(prior_samples))) {
        df_prior <- data.frame(x = prior_samples[!is.na(prior_samples)])

        # Prior: histogram from actual prior samples
        p <- p +
          ggplot2::geom_histogram(data = df_prior, ggplot2::aes(x = x, y = ggplot2::after_stat(density)),
                                bins = 30, fill = col_prior, alpha = 0.4, color = "white") +
          # Prior: density curve from prior samples
          ggplot2::geom_density(data = df_prior, ggplot2::aes(x = x),
                              color = col_prior, linewidth = 0.7, alpha = 0.8)
      } else {
        # If no prior available, show message
        if (verbose) message("No prior distribution found for parameter: ", param_name)
      }

      # Add posterior distributions
      if (sum(df_posterior$w) > 0) {
        # Posterior: weighted histogram
        p <- p +
          ggplot2::geom_histogram(data = df_posterior,
                                ggplot2::aes(x = x, y = ggplot2::after_stat(density), weight = w),
                                bins = 30, fill = col_posterior, alpha = 0.7, color = "white") +
          # Posterior: weighted density
          ggplot2::geom_density(data = df_posterior, ggplot2::aes(x = x, weight = w),
                              color = col_posterior, linewidth = 1.2, alpha = 0.8)
      }

      # Add mode and ML lines if requested
      if (show_modes || show_ml) {
        if (show_modes) {
          if (!is.null(prior_samples) && length(prior_samples) > 0 && !all(is.na(prior_samples))) {
            prior_mode <- calc_mode(prior_samples[!is.na(prior_samples)], NULL)
            if (!is.na(prior_mode)) {
              p <- p + ggplot2::geom_vline(xintercept = prior_mode,
                                          color = col_prior, linetype = "dashed", linewidth = 0.4)
            }
          }
          posterior_mode <- calc_mode(df_posterior$x, df_posterior$w)
          if (!is.na(posterior_mode)) {
            p <- p + ggplot2::geom_vline(xintercept = posterior_mode,
                                        color = col_posterior, linetype = "dashed", linewidth = 0.6)
          }
        }
        if (show_ml) {
          ml_value <- df_posterior$x[which.max(df_posterior$w)]
          p <- p + ggplot2::geom_vline(xintercept = ml_value,
                                      color = col_ml, linetype = "dotted", linewidth = 0.5)
        }
      }

      # Determine x-axis title
      x_axis_title <- if (grepl("prop|proportion|initial", tolower(param_name))) {
        "Proportion"
      } else if (grepl("rate|beta", tolower(param_name))) {
        "Rate"
      } else if (grepl("coef|coefficient|a1|a2|b1|b2", tolower(param_name))) {
        "Coefficient"
      } else {
        "Value"
      }

      p <- p +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5),
          axis.title.y = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_text(size = 9),
          panel.grid.minor = ggplot2::element_blank()
        ) +
        ggplot2::labs(title = param_label, x = x_axis_title, y = "Density")
    }

    return(p)
  }

  # Create global parameter plots
  create_global_posterior_plot <- function(data, subtitle_text = NULL, output_file = NULL) {
    if (nrow(data) == 0) return(NULL)

    unique_params <- unique(data$parameter)
    plot_list <- list()

    for (param_name in unique_params) {
      param_data <- data[data$parameter == param_name, ]
      if (nrow(param_data) == 0) next

      plot_list[[param_name]] <- create_param_plot(param_data, param_name)
    }

    if (length(plot_list) == 0) return(NULL)

    # Create grid layout (3 columns max)
    n_plots <- length(plot_list)
    ncol <- min(3, n_plots)
    nrow <- ceiling(n_plots / ncol)

    p_global <- cowplot::plot_grid(plotlist = plot_list, ncol = ncol, nrow = nrow)

    # Create title
    title_text <- "Global Parameter Posterior Distributions"
    if (!is.null(subtitle_text) && subtitle_text != "") {
      title <- cowplot::ggdraw() +
        cowplot::draw_label(title_text,
                           fontface = 'bold', size = 16, x = 0.5, y = 0.7, hjust = 0.5) +
        cowplot::draw_label(subtitle_text,
                           fontface = 'plain', size = 12, x = 0.5, y = 0.3, hjust = 0.5, color = "gray30")
    } else {
      title <- cowplot::ggdraw() +
        cowplot::draw_label(title_text,
                           fontface = 'bold', size = 16, x = 0.5, hjust = 0.5)
    }

    p_global <- cowplot::plot_grid(title, p_global,
                                   ncol = 1, rel_heights = c(0.08, 0.92))

    # Add common y-axis label
    p_global <- cowplot::ggdraw(p_global) +
      cowplot::draw_label("Density", x = 0.02, y = 0.5, angle = 90, size = 12)

    if (!is.null(output_file)) {
      ggplot2::ggsave(output_file, p_global, width = 13, height = 14, dpi = 300)
      if (verbose) message("Saved: ", output_file)
    }

    return(p_global)
  }

  # Create location-specific plots with structured layout
  create_location_posterior_plot <- function(data, title, subtitle, output_file = NULL, location = NULL) {
    if (nrow(data) == 0) return(NULL)

    # Separate by category
    data_ic <- data %>% dplyr::filter(grepl("_initial", parameter))
    data_transmission <- data %>% dplyr::filter(category == "4_Transmission" | grepl("^(beta_j0_|p_beta)", parameter))
    data_seasonality <- data %>% dplyr::filter(category == "5_Seasonality" | grepl("^(a1|a2|b1|b2|a_|b_)", parameter))
    data_psi_star <- data %>% dplyr::filter(category == "9_Psi_Star_Calibration" | grepl("^psi_star_", parameter))
    data_other <- data %>% dplyr::filter(!parameter %in% c(data_ic$parameter, data_transmission$parameter,
                                                          data_seasonality$parameter, data_psi_star$parameter))

    plot_list <- list()

    # Initial Conditions section
    if (nrow(data_ic) > 0) {
      ic_plots <- list()
      for (param in unique(data_ic$parameter)) {
        param_data <- data_ic %>% dplyr::filter(parameter == param)
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
        ic_plots[[param_clean]] <- create_param_plot(param_data, param, param_label, location)
      }

      if (length(ic_plots) > 0) {
        ic_combined <- cowplot::plot_grid(plotlist = ic_plots, ncol = 3, nrow = 2)
        ic_heading <- cowplot::ggdraw() +
          cowplot::draw_label("Initial Conditions", fontface = 'bold', size = 14, x = 0.5, y = 0.5)
        ic_combined <- cowplot::ggdraw(ic_combined) +
          cowplot::draw_label("Density", x = 0.02, y = 0.5, angle = 90, size = 10)

        plot_list$ic_heading <- ic_heading
        plot_list$ic_plots <- ic_combined
      }
    }

    # Transmission Parameters section
    if (nrow(data_transmission) > 0) {
      transmission_plots <- list()
      for (param in unique(data_transmission$parameter)) {
        param_data <- data_transmission %>% dplyr::filter(parameter == param)
        param_clean <- gsub("_[A-Z]{3}$", "", param)
        param_label <- switch(param_clean,
          "beta_j0_tot" = "Total Transmission Rate (β₀)",
          "p_beta" = "Proportion Human Transmission (p_β)",
          "beta_j0_hum" = "Human Transmission (Derived)",
          "beta_j0_env" = "Environmental Transmission (Derived)",
          param_clean
        )
        transmission_plots[[param_clean]] <- create_param_plot(param_data, param, param_label, location)
      }

      if (length(transmission_plots) > 0) {
        ordered_param_names <- c("beta_j0_tot", "p_beta", "beta_j0_hum", "beta_j0_env")
        ordered_plots <- list()
        for (param_name in ordered_param_names) {
          if (!is.null(transmission_plots[[param_name]])) {
            ordered_plots[[param_name]] <- transmission_plots[[param_name]]
          }
        }

        transmission_combined <- cowplot::plot_grid(plotlist = ordered_plots, ncol = 2, nrow = 2)
        transmission_heading <- cowplot::ggdraw() +
          cowplot::draw_label("Transmission Parameters", fontface = 'bold', size = 14, x = 0.5, y = 0.5)

        plot_list$transmission_heading <- transmission_heading
        plot_list$transmission_plots <- transmission_combined
      }
    }

    # Combine all sections
    if (length(plot_list) > 0) {
      final_plot_list <- list()

      # Add title
      main_title <- cowplot::ggdraw() +
        cowplot::draw_label(title, fontface = 'bold', size = 16, x = 0.5, y = 0.7, hjust = 0.5) +
        cowplot::draw_label(subtitle, fontface = 'plain', size = 12, x = 0.5, y = 0.3, hjust = 0.5, color = "gray30")

      final_plot_list[[1]] <- main_title

      # Add sections
      if (!is.null(plot_list$ic_heading)) {
        final_plot_list[[length(final_plot_list) + 1]] <- plot_list$ic_heading
        final_plot_list[[length(final_plot_list) + 1]] <- plot_list$ic_plots
      }
      if (!is.null(plot_list$transmission_heading)) {
        final_plot_list[[length(final_plot_list) + 1]] <- plot_list$transmission_heading
        final_plot_list[[length(final_plot_list) + 1]] <- plot_list$transmission_plots
      }

      # Calculate heights
      heights <- c(0.08)  # Title
      if (!is.null(plot_list$ic_heading)) heights <- c(heights, 0.04, 0.35)
      if (!is.null(plot_list$transmission_heading)) heights <- c(heights, 0.04, 0.35)

      p <- cowplot::plot_grid(plotlist = final_plot_list, ncol = 1, rel_heights = heights)

      # Add common y-axis label
      p <- cowplot::ggdraw(p) +
        cowplot::draw_label("Density", x = 0.02, y = 0.5, angle = 90, size = 12)
    } else {
      p <- NULL
    }

    if (!is.null(output_file)) {
      plot_height <- 24
      plot_width <- 15
      ggplot2::ggsave(output_file, plot = p, width = plot_width, height = plot_height, dpi = 300)
      if (verbose) message("Saved: ", output_file)
    }

    return(p)
  }

  # Create subtitle for plots
  n_successful <- nrow(results_valid)
  n_total <- nrow(results)

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

    p_global <- create_global_posterior_plot(
      results_global,
      subtitle_text = subtitle_text,
      output_file = output_file
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

      p_loc <- create_location_posterior_plot(
        results_loc,
        title = paste0("Location-Specific Posterior Distributions: ", loc),
        subtitle = subtitle_text,
        output_file = output_file,
        location = loc
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