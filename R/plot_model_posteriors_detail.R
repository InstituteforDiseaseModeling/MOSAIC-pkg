#' Plot Detailed Prior to Posterior Distributions
#'
#' Creates detailed multi-panel visualizations showing the progression from prior
#' to posterior distributions through retained and best subsets, with ESS and KL metrics.
#'
#' @param quantiles_file Path to the posterior_quantiles.csv file from calc_model_posterior_quantiles
#' @param results_file Path to simulations.parquet file containing all simulation results
#' @param priors_file Path to priors.json file
#' @param posteriors_file Path to posteriors.json file (optional, for theoretical distributions)
#' @param diagnostics_file Path to convergence_diagnostics.json file (optional, for ESS values)
#' @param output_dir Directory to save plots (default: "./results/plots")
#' @param verbose Logical; print progress messages (default: TRUE)
#'
#' @return List of plot filenames created (invisible)
#'
#' @examples
#' \dontrun{
#' plot_model_posteriors(
#'   quantiles_file = "./results/posterior_quantiles.csv",
#'   results_file = "./results/simulations.parquet",
#'   priors_file = "./priors.json",
#'   posteriors_file = "./results/posteriors.json",
#'   output_dir = "./results/plots"
#' )
#' }
#'
#' @export
plot_model_posteriors_detail <- function(quantiles_file,
                                 results_file,
                                 priors_file,
                                 posteriors_file = NULL,
                                 diagnostics_file = NULL,
                                 output_dir = "./results/plots",
                                 verbose = TRUE) {

  # Load required libraries
  library(ggplot2)
  library(dplyr)
  library(arrow)
  library(patchwork)
  library(cowplot)
  library(jsonlite)

  # Color scheme from plot_model_distributions
  prior_color <- "#457b9d"      # Blue
  posterior_color <- "#e63946"  # Red

  if (verbose) cat("Loading data files...\n")

  # Load quantiles data
  if (!file.exists(quantiles_file)) {
    stop("Quantiles file not found: ", quantiles_file)
  }
  quantiles_df <- read.csv(quantiles_file, stringsAsFactors = FALSE)

  # Load results data
  if (!file.exists(results_file)) {
    stop("Results file not found: ", results_file)
  }
  results_full <- read_parquet(results_file)

  # Access different subsets
  results_all <- results_full  # All simulations
  results_retained <- results_full[results_full$is_retained, ]  # Non-outlier models
  results_best <- results_full[results_full$is_best_subset, ]  # Best subset

  # Load priors
  if (!file.exists(priors_file)) {
    stop("Priors file not found: ", priors_file)
  }
  priors_object <- jsonlite::read_json(priors_file)

  # Load posteriors.json if provided (for theoretical distributions)
  posteriors_object <- NULL
  if (!is.null(posteriors_file)) {
    if (file.exists(posteriors_file)) {
      posteriors_object <- jsonlite::read_json(posteriors_file)
      if (verbose) cat("Loaded posteriors.json for theoretical distributions\n")
    } else {
      warning("Posteriors file specified but not found: ", posteriors_file)
    }
  } else {
    # Try to find posteriors.json in the same directory as quantiles
    posteriors_file_auto <- file.path(dirname(quantiles_file), "posteriors.json")
    if (file.exists(posteriors_file_auto)) {
      posteriors_object <- jsonlite::read_json(posteriors_file_auto)
      if (verbose) cat("Auto-detected posteriors.json for theoretical distributions\n")
    } else {
      if (verbose) cat("Note: posteriors.json not provided - theoretical distributions will not be shown\n")
    }
  }

  # Load convergence diagnostics if provided (for ESS values)
  diagnostics <- NULL
  ess_retained_global <- NULL
  ess_best_global <- NULL
  if (!is.null(diagnostics_file)) {
    if (file.exists(diagnostics_file)) {
      diagnostics <- jsonlite::read_json(diagnostics_file)
      # Extract ESS values from diagnostics
      if (!is.null(diagnostics$metrics$ess_retained$value)) {
        ess_retained_global <- diagnostics$metrics$ess_retained$value
      }
      if (!is.null(diagnostics$metrics$ess_best$value)) {
        ess_best_global <- diagnostics$metrics$ess_best$value
      } else if (!is.null(diagnostics$metrics$ess$value)) {
        # Fallback to 'ess' if 'ess_best' not available
        ess_best_global <- diagnostics$metrics$ess$value
      }
      if (verbose) cat("Loaded ESS values from convergence diagnostics\n")
    }
  } else {
    # Try to find diagnostics in the same directory as results
    diagnostics_file_auto <- file.path(dirname(results_file), "convergence_diagnostics.json")
    if (file.exists(diagnostics_file_auto)) {
      diagnostics <- jsonlite::read_json(diagnostics_file_auto)
      if (!is.null(diagnostics$metrics$ess_retained$value)) {
        ess_retained_global <- diagnostics$metrics$ess_retained$value
      }
      if (!is.null(diagnostics$metrics$ess_best$value)) {
        ess_best_global <- diagnostics$metrics$ess_best$value
      } else if (!is.null(diagnostics$metrics$ess$value)) {
        ess_best_global <- diagnostics$metrics$ess$value
      }
      if (verbose) cat("Auto-detected ESS values from convergence diagnostics\n")
    }
  }

  # Load estimated_parameters for ordering
  data(estimated_parameters, package = "MOSAIC", envir = environment())

  # Create output subdirectory for all plots
  output_subdir <- file.path(output_dir, "posterior_distributions_detail")
  if (!dir.exists(output_subdir)) {
    dir.create(output_subdir, recursive = TRUE, showWarnings = FALSE)
  }

  # Get unique parameters from quantiles (only posteriors have KL values)
  posterior_types <- c("posterior", "npe", "bfrs")
  params_with_posteriors <- quantiles_df %>%
    filter(type %in% posterior_types, !is.na(kl)) %>%
    pull(parameter) %>%
    unique()

  if (verbose) cat("Found", length(params_with_posteriors), "parameters with posteriors\n")

  # Store all created files
  created_files <- c()

  # =========================================================================
  # HELPER FUNCTIONS
  # =========================================================================

  # Unwrap values from arrays (handles priors.json array wrapping)
  unwrap_value <- function(x) {
    if (is.list(x) && length(x) == 1) {
      return(unwrap_value(x[[1]]))  # Recursive unwrapping
    }
    return(x)
  }

  # Get distribution object with proper unwrapping
  extract_distribution <- function(obj) {
    if (is.null(obj)) return(NULL)

    result <- list()
    result$distribution <- unwrap_value(obj$distribution)

    if (!is.null(obj$parameters)) {
      result$parameters <- list()
      for (param_name in names(obj$parameters)) {
        result$parameters[[param_name]] <- unwrap_value(obj$parameters[[param_name]])
      }
    }

    # Preserve other fields like description
    if (!is.null(obj$description)) {
      result$description <- unwrap_value(obj$description)
    }

    return(result)
  }

  # Calculate density for a distribution object
  calc_density_safe <- function(x_vals, dist_obj) {
    if (is.null(dist_obj) || is.null(dist_obj$distribution)) {
      return(rep(0, length(x_vals)))
    }

    dist_type <- tolower(dist_obj$distribution)
    params <- dist_obj$parameters

    if (is.null(params)) {
      return(rep(0, length(x_vals)))
    }

    tryCatch({
      switch(dist_type,
        "normal" = dnorm(x_vals,
                       mean = params$mean,
                       sd = params$sd),
        "gamma" = dgamma(x_vals,
                       shape = params$shape,
                       rate = params$rate),
        "beta" = dbeta(x_vals,
                     shape1 = params$shape1,
                     shape2 = params$shape2),
        "uniform" = dunif(x_vals,
                        min = params$min,
                        max = params$max),
        "lognormal" = dlnorm(x_vals,
                           meanlog = params$meanlog,
                           sdlog = params$sdlog),
        "gompertz" = {
          b <- params$b
          eta <- params$eta
          b * eta * exp(b * x_vals) * exp(eta) * exp(-eta * exp(b * x_vals))
        },
        rep(0, length(x_vals)) # Default to zero if distribution not supported
      )
    }, error = function(e) {
      rep(0, length(x_vals))
    })
  }

  # =========================================================================
  # HELPER FUNCTION: Create panels for a single parameter
  # =========================================================================
  create_parameter_panels <- function(param_name, param_info, posteriors_obj = NULL) {

    # Check if parameter exists in results
    if (!param_name %in% names(results_best)) {
      return(NULL)
    }

    # Get prior object with proper unwrapping
    prior_obj_raw <- NULL
    if (param_name %in% names(priors_object$parameters_global)) {
      prior_obj_raw <- priors_object$parameters_global[[param_name]]
    } else {
      # Check with and without location suffix
      base_param <- gsub("_[A-Z]{3}$", "", param_name)
      if (!is.null(priors_object$parameters_location)) {
        if (base_param %in% names(priors_object$parameters_location)) {
          prior_obj_raw <- priors_object$parameters_location[[base_param]]
        }
      }
    }

    if (is.null(prior_obj_raw)) {
      return(NULL)
    }

    # Get samples from different stages
    prior_samples <- results_all[[param_name]]
    retained_samples <- results_retained[[param_name]]
    best_samples <- results_best[[param_name]]

    # Use the weight_best column from results (should already be calculated)
    # For best subset, we use weight_best; for retained, we'd use weight_retained
    if (!"weight_best" %in% names(results_best)) {
      stop("weight_best column not found in results. Check that convergence analysis was run.")
    }

    posterior_weights <- results_best$weight_best

    # Check if retained weights exist
    retained_weights <- NULL
    if ("weight_retained" %in% names(results_retained)) {
      retained_weights <- results_retained$weight_retained
    }

    # Basic validation only - weights should already be proper
    if (!is.numeric(posterior_weights)) {
      stop("weight_best column is not numeric. Check the results data.")
    }

    # Use ESS values from convergence diagnostics if available
    # Otherwise fall back to simple estimates
    n_retained <- length(retained_samples)
    n_best <- length(best_samples)

    # Default to sample sizes if ESS not available from diagnostics
    ess_retained <- if (!is.null(ess_retained_global)) ess_retained_global else n_retained
    ess_best <- if (!is.null(ess_best_global)) ess_best_global else n_best

    # Get quantiles and KL from the quantiles_df
    param_data <- quantiles_df %>% filter(parameter == param_name)
    posterior_types <- c("posterior", "npe", "bfrs")
    posterior_data <- param_data %>% filter(type %in% posterior_types)
    prior_data <- param_data %>% filter(type == "prior")

    if (nrow(posterior_data) == 0 || nrow(prior_data) == 0) {
      return(NULL)
    }

    kl_divergence <- posterior_data$kl[1]
    param_description <- param_info$description

    # Determine x-axis limits
    x_range <- range(c(prior_samples, retained_samples, best_samples), na.rm = TRUE)
    x_padding <- diff(x_range) * 0.05
    x_limits <- c(x_range[1] - x_padding, x_range[2] + x_padding)

    # Create data frames
    df_prior <- data.frame(x = prior_samples)
    df_retained <- data.frame(x = retained_samples)
    df_best <- data.frame(x = best_samples)
    df_best_weighted <- data.frame(x = best_samples, w = posterior_weights)

    # Create retained weighted df if weights exist
    df_retained_weighted <- if (!is.null(retained_weights)) {
      data.frame(x = retained_samples, w = retained_weights)
    } else {
      NULL
    }

    # Panel 1: Unweighted Prior Samples
    p_prior <- ggplot() +
      geom_histogram(data = df_prior,
                    aes(x = x, y = after_stat(density)),
                    bins = 50, fill = prior_color, alpha = 0.6, color = "white") +
      labs(title = "Prior Samples",
           subtitle = paste("n =", length(prior_samples)),
           x = NULL, y = "Density") +
      scale_x_continuous(limits = x_limits) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 11, hjust = 0),
        plot.subtitle = element_text(size = 9, hjust = 0, color = "gray50"),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.02, 0.1, 0.1, 0.1), "cm"),  # Reduced top margin
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10)
      )

    # Panel 2: Unweighted Retained Samples
    p_retained <- ggplot() +
      geom_histogram(data = df_retained,
                    aes(x = x, y = after_stat(density)),
                    bins = 50, fill = posterior_color, alpha = 0.4, color = "white") +
      labs(title = "Retained Samples (Unweighted)",
           subtitle = paste("n =", length(retained_samples)),
           x = NULL, y = "Density") +
      scale_x_continuous(limits = x_limits) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 11, hjust = 0),
        plot.subtitle = element_text(size = 9, hjust = 0, color = "gray50"),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10)
      )

    # Panel 3: Weighted Retained Samples (if weights exist)
    if (!is.null(df_retained_weighted)) {
      p_retained_weighted <- ggplot() +
        geom_histogram(data = df_retained_weighted,
                      aes(x = x, y = after_stat(density), weight = w),
                      bins = 50, fill = posterior_color, alpha = 0.5, color = "white") +
        geom_density(data = df_retained_weighted,
                    aes(x = x, weight = w),
                    color = posterior_color, linewidth = 1.0, alpha = 0.6) +
        labs(title = "Retained Samples (Weighted)",
             subtitle = paste("ESS =", round(ess_retained, 1)),
             x = NULL, y = "Density") +
        scale_x_continuous(limits = x_limits) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 11, hjust = 0),
          plot.subtitle = element_text(size = 9, hjust = 0, color = "gray50"),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
          axis.text.x = element_blank(),
          axis.title.y = element_text(size = 10)
        )
    } else {
      # Create empty placeholder if no weights
      p_retained_weighted <- NULL
    }

    # Panel 4: Unweighted Best Subset Samples
    p_best_unweighted <- ggplot() +
      geom_histogram(data = df_best,
                    aes(x = x, y = after_stat(density)),
                    bins = 50, fill = posterior_color, alpha = 0.6, color = "white") +
      labs(title = "Best Subset (Unweighted)",
           subtitle = paste("n =", length(best_samples)),
           x = NULL, y = "Density") +
      scale_x_continuous(limits = x_limits) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 11, hjust = 0),
        plot.subtitle = element_text(size = 9, hjust = 0, color = "gray50"),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10)
      )

    # Panel 5: Weighted Best Subset Samples
    p_best_weighted <- ggplot() +
      geom_histogram(data = df_best_weighted,
                    aes(x = x, y = after_stat(density), weight = w),
                    bins = 50, fill = posterior_color, alpha = 0.8, color = "white") +
      geom_density(data = df_best_weighted,
                  aes(x = x, weight = w),
                  color = posterior_color, linewidth = 1.2, alpha = 0.8) +
      labs(title = "Best Subset (Weighted)",
           subtitle = paste("Effective Sample Size =", round(ess_best, 1)),
           x = NULL, y = "Density") +
      scale_x_continuous(limits = x_limits) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 11, hjust = 0),
        plot.subtitle = element_text(size = 9, hjust = 0, color = "gray50"),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10)
      )

    # Panel 6: Caterpillar plot
    # Handle missing columns gracefully
    get_quantile_safe <- function(data, col_name, default_val = NA) {
      if (nrow(data) == 0 || !col_name %in% names(data)) {
        return(default_val)
      }
      return(data[[col_name]][1])
    }

    caterpillar_data <- data.frame(
      distribution = c("Prior", "Posterior"),
      median = c(get_quantile_safe(prior_data, "q0.5"),
                 get_quantile_safe(posterior_data, "q0.5")),
      q25 = c(get_quantile_safe(prior_data, "q0.25"),
              get_quantile_safe(posterior_data, "q0.25")),
      q75 = c(get_quantile_safe(prior_data, "q0.75"),
              get_quantile_safe(posterior_data, "q0.75")),
      q2.75 = c(get_quantile_safe(prior_data, "q0.025"),
                get_quantile_safe(posterior_data, "q0.025")),
      q97.5 = c(get_quantile_safe(prior_data, "q0.975"),
                get_quantile_safe(posterior_data, "q0.975"))
    )

    p_caterpillar <- ggplot(caterpillar_data, aes(x = distribution, y = median)) +
      geom_errorbar(aes(ymin = q2.75, ymax = q97.5),
                    width = 0.15, linewidth = 0.6,
                    color = c(prior_color, posterior_color)) +
      geom_errorbar(aes(ymin = q25, ymax = q75),
                    width = 0.08, linewidth = 1.2,
                    color = c(prior_color, posterior_color)) +
      geom_point(size = 3, color = c(prior_color, posterior_color)) +
      labs(title = "Quantiles",
           x = NULL, y = NULL) +
      scale_y_continuous(limits = x_limits) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 11, hjust = 0),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_blank()
      ) +
      coord_flip()

    # Panel 7: Probability distributions (empirical)
    p_distributions <- ggplot() +
      geom_density(data = df_prior,
                  aes(x = x),
                  fill = prior_color, color = prior_color,
                  alpha = 0.3, linewidth = 0.8) +
      geom_density(data = df_best_weighted,
                  aes(x = x, weight = w),
                  fill = posterior_color, color = posterior_color,
                  alpha = 0.3, linewidth = 1.0) +
      labs(title = "Prior vs Posterior (Empirical)",
           subtitle = paste("Kullback-Leibler divergence =", round(kl_divergence, 3)),
           x = "Value", y = "Density") +
      scale_x_continuous(limits = x_limits) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 11, hjust = 0),
        plot.subtitle = element_text(size = 9, hjust = 0, color = "gray50"),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.2, 0.1), "cm"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9)
      )

    # Panel 8: Theoretical distributions from posteriors.json
    # Extract distributions with proper handling of nested structures
    posterior_obj_raw <- NULL
    if (!is.null(posteriors_obj)) {
      if (param_name %in% names(posteriors_obj$parameters_global)) {
        posterior_obj_raw <- posteriors_obj$parameters_global[[param_name]]
      } else {
        # For location parameters, check base name without suffix
        base_param <- gsub("_[A-Z]{3}$", "", param_name)
        if (!is.null(posteriors_obj$parameters_location) && base_param %in% names(posteriors_obj$parameters_location)) {
          posterior_obj_raw <- posteriors_obj$parameters_location[[base_param]]
        }
      }
    }

    # Extract location-specific distributions for theoretical plotting
    prior_dist_obj <- NULL
    posterior_dist_obj <- NULL

    # For location parameters, extract the specific location's distribution
    if (!is.null(prior_obj_raw)) {
      if (!is.null(prior_obj_raw$location)) {
        # Extract location code from parameter name
        location_match <- regexec("_([A-Z]{3})$", param_name)
        if (location_match[[1]][1] != -1) {
          location_code <- regmatches(param_name, location_match)[[1]][2]
          if (location_code %in% names(prior_obj_raw$location)) {
            prior_dist_obj <- extract_distribution(prior_obj_raw$location[[location_code]])
          }
        }
      } else {
        # Global parameter - use directly
        prior_dist_obj <- extract_distribution(prior_obj_raw)
      }
    }

    if (!is.null(posterior_obj_raw)) {
      if (!is.null(posterior_obj_raw$location)) {
        # Extract location code from parameter name
        location_match <- regexec("_([A-Z]{3})$", param_name)
        if (location_match[[1]][1] != -1) {
          location_code <- regmatches(param_name, location_match)[[1]][2]
          if (location_code %in% names(posterior_obj_raw$location)) {
            # Posteriors don't need unwrapping
            posterior_dist_obj <- posterior_obj_raw$location[[location_code]]
          }
        }
      } else {
        # Global parameter - posteriors don't need unwrapping
        posterior_dist_obj <- posterior_obj_raw
      }
    }

    # Create theoretical plot if both prior and posterior distributions available
    if (!is.null(posterior_dist_obj) && !is.null(prior_dist_obj)) {
      # Calculate densities using safe function
      x_seq <- seq(x_limits[1], x_limits[2], length.out = 400)
      prior_dens <- calc_density_safe(x_seq, prior_dist_obj)
      post_dens <- calc_density_safe(x_seq, posterior_dist_obj)

      # Create separate data frames for cleaner plotting
      prior_data <- data.frame(x = x_seq, density = prior_dens)
      post_data <- data.frame(x = x_seq, density = post_dens)

      p_theoretical <- ggplot() +
        # Prior distribution
        geom_area(data = prior_data, aes(x = x, y = density),
                  fill = prior_color, alpha = 0.3) +
        geom_line(data = prior_data, aes(x = x, y = density),
                  color = prior_color, linewidth = 0.8) +
        # Posterior distribution
        geom_area(data = post_data, aes(x = x, y = density),
                  fill = posterior_color, alpha = 0.3) +
        geom_line(data = post_data, aes(x = x, y = density),
                  color = posterior_color, linewidth = 1.0) +
        labs(title = "Prior vs Posterior (Theoretical)",
             subtitle = paste("Distribution:",
                              if (!is.null(posterior_dist_obj$distribution)) {
                                tools::toTitleCase(as.character(posterior_dist_obj$distribution))
                              } else {
                                "Unknown"
                              }),
             x = "Value", y = "Density") +
        scale_x_continuous(limits = x_limits) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 11, hjust = 0),
          plot.subtitle = element_text(size = 9, hjust = 0, color = "gray50"),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.2, 0.1), "cm"),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 9),
          legend.position = "none"
        )
    } else {
      # Placeholder when no theoretical distributions available
      p_theoretical <- ggplot() +
        theme_void() +
        labs(title = "Prior vs Posterior (Theoretical)",
             subtitle = "Not available") +
        theme(
          plot.title = element_text(size = 11, hjust = 0.5, color = "gray50"),
          plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray50"),
          plot.margin = unit(c(0.1, 0.1, 0.2, 0.1), "cm")
        )
    }

    # Create column title with wrapped subtitle
    # Wrap long descriptions
    wrapped_description <- if (!is.null(param_description) && nchar(param_description) > 50) {
      paste(strwrap(param_description, width = 50), collapse = "\n")
    } else {
      param_description
    }

    col_title <- ggplot() +
      theme_void() +
      labs(title = param_name,
           subtitle = wrapped_description) +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray50", lineheight = 0.9),
        plot.margin = unit(c(0.05, 0.2, -0.05, 0.2), "cm")  # Reduced top/bottom margins
      )

    # Combine panels vertically
    # Include retained weighted panel if it exists
    if (!is.null(p_retained_weighted)) {
      panels_only <- p_prior / p_retained / p_retained_weighted / p_best_unweighted /
                     p_best_weighted / p_caterpillar / p_distributions / p_theoretical +
        plot_layout(heights = c(1, 1, 1, 1, 1, 0.5, 1.2, 1.2))
    } else {
      # Original layout without retained weighted
      panels_only <- p_prior / p_retained / p_best_unweighted / p_best_weighted /
                     p_caterpillar / p_distributions / p_theoretical +
        plot_layout(heights = c(1, 1, 1, 1, 0.5, 1.2, 1.2))
    }

    # Adjust title height based on description length
    title_height <- if (!is.null(param_description) && nchar(param_description) > 50) {
      0.07  # More space for wrapped text (reduced from 0.08)
    } else {
      0.05  # Standard height (reduced from 0.06)
    }

    combined_panels <- col_title / panels_only +
      plot_layout(heights = c(title_height, 1 - title_height))

    return(combined_panels)
  }

  # =========================================================================
  # PLOT 1: Global Parameters
  # =========================================================================
  if (verbose) cat("\nCreating global parameter plots...\n")

  # Get global parameters ordered by estimated_parameters
  global_params_info <- estimated_parameters %>%
    filter(scale == "global") %>%
    arrange(order)

  # Filter for global parameters with posteriors
  global_params <- intersect(global_params_info$parameter_name, params_with_posteriors)

  if (length(global_params) > 0) {
    # Group by category
    for (cat in unique(global_params_info$category)) {
      cat_params_info <- global_params_info %>%
        filter(category == cat, parameter_name %in% global_params)

      if (nrow(cat_params_info) > 0) {
        if (verbose) cat("  Processing category:", cat, "\n")

        # Create panels for each parameter in category
        cat_panels <- list()
        for (i in seq_len(nrow(cat_params_info))) {
          param_name <- cat_params_info$parameter_name[i]
          panels <- create_parameter_panels(param_name, cat_params_info[i,], posteriors_object)
          if (!is.null(panels)) {
            cat_panels[[param_name]] <- panels
          }
        }

        if (length(cat_panels) > 0) {
          # Combine panels horizontally
          n_params <- length(cat_panels)
          combined_panels <- wrap_plots(cat_panels, ncol = n_params)

          # Add overall title
          title_plot <- ggplot() +
            theme_void() +
            labs(title = paste("Global Parameters -", tools::toTitleCase(gsub("_", " ", cat)))) +
            theme(
              plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              plot.margin = unit(c(0.2, 0, 0.1, 0), "cm")
            )

          # Add legend
          legend_data <- data.frame(
            x = c(1, 2),
            y = c(1, 1),
            dist = factor(c("Prior", "Posterior"), levels = c("Prior", "Posterior"))
          )

          p_legend <- ggplot(legend_data, aes(x = x, y = y, fill = dist)) +
            geom_point(size = 5, shape = 22, stroke = 0.7) +
            scale_fill_manual(values = c("Prior" = prior_color, "Posterior" = posterior_color),
                              name = NULL) +
            theme_void() +
            theme(
              legend.position = "top",
              legend.direction = "horizontal",
              legend.text = element_text(size = 11),
              legend.spacing.x = unit(0.5, "cm")
            )

          legend_grob <- get_legend(p_legend)
          legend_wrapper <- ggplot() +
            theme_void() +
            annotation_custom(legend_grob, xmin = 0.4, xmax = 0.6, ymin = 0.5, ymax = 1)

          # Final combination
          final_plot <- plot_grid(
            title_plot,
            combined_panels,
            legend_wrapper,
            ncol = 1,
            rel_heights = c(0.04, 0.91, 0.05)
          )

          # Save plot
          filename <- paste0("posterior_distributions_detail_global_", cat, ".pdf")
          plot_width <- 4 * n_params + 2
          plot_height <- 18  # Increased for additional retained weighted panel

          ggsave(file.path(output_subdir, filename), final_plot,
                 width = min(plot_width, 20), height = plot_height, limitsize = FALSE)
          created_files <- c(created_files, filename)
          if (verbose) cat("    Saved:", filename, "\n")
        }
      }
    }
  }

  # =========================================================================
  # PLOT 2: Location-Specific Parameters
  # =========================================================================

  # Get locations
  locations <- unique(quantiles_df$location[!is.na(quantiles_df$location) & quantiles_df$location != ""])
  location_pattern <- "_([A-Z]{3})$"
  locations_from_params <- unique(gsub(".*_([A-Z]{3})$", "\\1",
                                       params_with_posteriors[grepl(location_pattern, params_with_posteriors)]))
  locations <- unique(c(locations, locations_from_params))

  for (iso in locations) {
    if (verbose) cat("\nCreating plots for location:", iso, "\n")

    # Get location-specific parameters
    location_params_info <- estimated_parameters %>%
      filter(scale == "location") %>%
      arrange(order)

    # Find parameters for this location
    location_params <- params_with_posteriors[
      grepl(paste0("_", iso, "$"), params_with_posteriors) |
      (params_with_posteriors %in% location_params_info$parameter_name &
       paste0(params_with_posteriors, "_", iso) %in% names(results_best))
    ]

    if (length(location_params) > 0) {
      # Group by category
      for (cat in unique(location_params_info$category)) {
        # Get base parameter names for this category
        cat_params_base <- location_params_info %>%
          filter(category == cat) %>%
          pull(parameter_name)

        # Find matching parameters for this location
        cat_params <- c()
        for (param_base in cat_params_base) {
          # Check with suffix
          param_with_suffix <- paste0(param_base, "_", iso)
          if (param_with_suffix %in% location_params) {
            cat_params <- c(cat_params, param_with_suffix)
          } else if (param_base %in% location_params) {
            cat_params <- c(cat_params, param_base)
          }
        }

        if (length(cat_params) > 0) {
          if (verbose) cat("  Processing category:", cat, "\n")

          # Create panels for each parameter
          cat_panels <- list()
          for (param_name in cat_params) {
            # Get parameter info
            base_name <- gsub(paste0("_", iso, "$"), "", param_name)
            param_info <- location_params_info %>%
              filter(parameter_name == base_name) %>%
              slice(1)

            if (nrow(param_info) == 0) {
              param_info <- data.frame(
                parameter_name = param_name,
                description = paste(tools::toTitleCase(gsub("_", " ", base_name)), iso),
                stringsAsFactors = FALSE
              )
            } else {
              param_info$description <- paste(param_info$description, iso)
            }

            panels <- create_parameter_panels(param_name, param_info, posteriors_object)
            if (!is.null(panels)) {
              cat_panels[[param_name]] <- panels
            }
          }

          if (length(cat_panels) > 0) {
            # Combine panels
            n_params <- length(cat_panels)
            combined_panels <- wrap_plots(cat_panels, ncol = n_params)

            # Add title
            title_plot <- ggplot() +
              theme_void() +
              labs(title = paste(iso, "-", tools::toTitleCase(gsub("_", " ", cat)))) +
              theme(
                plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                plot.margin = unit(c(0.2, 0, 0.1, 0), "cm")
              )

            # Add legend
            legend_data <- data.frame(
              x = c(1, 2),
              y = c(1, 1),
              dist = factor(c("Prior", "Posterior"), levels = c("Prior", "Posterior"))
            )

            p_legend <- ggplot(legend_data, aes(x = x, y = y, fill = dist)) +
              geom_point(size = 5, shape = 22, stroke = 0.7) +
              scale_fill_manual(values = c("Prior" = prior_color, "Posterior" = posterior_color),
                                name = NULL) +
              theme_void() +
              theme(
                legend.position = "top",
                legend.direction = "horizontal",
                legend.text = element_text(size = 11),
                legend.spacing.x = unit(0.5, "cm")
              )

            legend_grob <- get_legend(p_legend)
            legend_wrapper <- ggplot() +
              theme_void() +
              annotation_custom(legend_grob, xmin = 0.4, xmax = 0.6, ymin = 0.5, ymax = 1)

            # Final combination
            final_plot <- plot_grid(
              title_plot,
              combined_panels,
              legend_wrapper,
              ncol = 1,
              rel_heights = c(0.04, 0.91, 0.05)
            )

            # Save plot
            filename <- paste0("posterior_distributions_detail_", iso, "_", cat, ".pdf")
            plot_width <- 4 * n_params + 2
            plot_height <- 18  # Increased for additional retained weighted panel

            ggsave(file.path(output_subdir, filename), final_plot,
                   width = min(plot_width, 20), height = plot_height, limitsize = FALSE)
            created_files <- c(created_files, filename)
            if (verbose) cat("    Saved:", filename, "\n")
          }
        }
      }
    }
  }

  if (verbose) {
    cat("\n✓ Successfully created detailed posterior distribution plots\n")
    cat("  Output directory:", output_subdir, "\n")
  }

  invisible(created_files)
}
