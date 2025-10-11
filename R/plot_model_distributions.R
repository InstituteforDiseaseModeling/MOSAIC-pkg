#' Plot Prior and Posterior Distributions for MOSAIC Model Parameters
#'
#' Creates visualizations of global and location-specific parameter priors and posteriors using
#' metadata from the estimated_parameters inventory. Parameters are organized by
#' biological category and ordered consistently across all plots. When posteriors are provided,
#' they are overlaid on the prior distributions for direct comparison.
#'
#' @param PATHS A list containing paths for the MOSAIC project, from \code{get_paths()}
#' @param priors Path to priors JSON file. If NULL, uses default priors file.
#' @param posteriors Path to posteriors JSON file (optional). If provided, posterior distributions are overlaid on priors.
#' @param config A configuration object containing date_start and other settings. If NULL, uses default date.
#' @param output_dir Output directory for plots. If NULL, uses PATHS$DOCS_FIGURES
#' @param selected_locations Character vector of ISO codes for location-specific plots. If NULL, plots all available locations.
#'
#' @return Invisibly returns a list of generated plot objects
#'
#' @export
plot_model_distributions <- function(PATHS = NULL, priors_file = NULL, posteriors_file = NULL, config = NULL,
                                    output_dir = NULL, selected_locations = NULL) {

  library(ggplot2)
  library(dplyr)
  library(cowplot)

  # Store original warning setting for later restoration
  old_warn <- getOption("warn")

  # Load estimated parameters inventory
  data("estimated_parameters", package = "MOSAIC")

  # Helper function to unnest single-element lists from JSON
  unnest_json <- function(x) {
    if (is.list(x) && length(x) == 1 && !is.null(names(x))) {
      return(x)  # Keep named lists as-is
    }
    if (is.list(x) && length(x) == 1) {
      return(x[[1]])  # Unnest single-element unnamed lists
    }
    if (is.list(x)) {
      return(lapply(x, unnest_json))  # Recurse for nested lists
    }
    return(x)
  }

  # Load priors from file
  if (is.null(priors_file)) {
    stop("Please provide path to priors JSON file")
  }
  if (!file.exists(priors_file)) {
    stop("Priors file not found: ", priors_file)
  }
  priors <- jsonlite::read_json(priors_file, simplifyVector = FALSE)
  priors <- unnest_json(priors)

  # Load posteriors from file if provided
  posteriors <- NULL
  if (!is.null(posteriors_file)) {
    if (!file.exists(posteriors_file)) {
      warning("Posteriors file not found: ", posteriors_file, ". Plotting priors only.")
    } else {
      posteriors <- jsonlite::read_json(posteriors_file, simplifyVector = FALSE)
      posteriors <- unnest_json(posteriors)
    }
  }

  # Setup output directory
  if (is.null(output_dir)) {
    if (!is.null(PATHS$DOCS_FIGURES)) {
      output_dir <- PATHS$DOCS_FIGURES
    } else if (dir.exists("../MOSAIC-docs/figures")) {
      output_dir <- "../MOSAIC-docs/figures"
    } else {
      output_dir <- "figures"
    }
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  plots <- list()

  # Extract location codes
  location_codes <- NULL
  if (!is.null(priors$parameters_location)) {
    for (param in names(priors$parameters_location)) {
      if (!is.null(priors$parameters_location[[param]]$location)) {
        location_codes <- names(priors$parameters_location[[param]]$location)
        break
      }
    }
  }

  # Filter locations if specified
  if (!is.null(selected_locations)) {
    location_codes <- intersect(location_codes, selected_locations)
  }

  # Helper function to calculate KL divergence analytically for known distributions
  calc_kl_analytical <- function(dist1_type, dist1_params, dist2_type, dist2_params) {
    if (dist1_type != dist2_type) return(NA)  # Different types, would need numerical

    kl <- NA
    tryCatch({
      if (dist1_type == "beta") {
        a1 <- as.numeric(dist1_params$shape1)
        b1 <- as.numeric(dist1_params$shape2)
        a2 <- as.numeric(dist2_params$shape1)
        b2 <- as.numeric(dist2_params$shape2)
        kl <- lgamma(a1 + b1) - lgamma(a1) - lgamma(b1) -
              lgamma(a2 + b2) + lgamma(a2) + lgamma(b2) +
              (a1 - a2) * (digamma(a1) - digamma(a1 + b1)) +
              (b1 - b2) * (digamma(b1) - digamma(a1 + b1))
      } else if (dist1_type == "normal") {
        mu1 <- as.numeric(dist1_params$mean)
        sigma1 <- as.numeric(dist1_params$sd)
        mu2 <- as.numeric(dist2_params$mean)
        sigma2 <- as.numeric(dist2_params$sd)
        kl <- log(sigma2/sigma1) + (sigma1^2 + (mu1 - mu2)^2) / (2 * sigma2^2) - 0.5
      } else if (dist1_type == "gamma") {
        k1 <- as.numeric(dist1_params$shape)
        theta1 <- 1/as.numeric(dist1_params$rate)
        k2 <- as.numeric(dist2_params$shape)
        theta2 <- 1/as.numeric(dist2_params$rate)
        kl <- (k1 - k2) * digamma(k1) - lgamma(k1) + lgamma(k2) +
              k2 * (log(theta2) - log(theta1)) + k1 * (theta1 - theta2) / theta2
      } else if (dist1_type == "uniform") {
        a1 <- as.numeric(dist1_params$min)
        b1 <- as.numeric(dist1_params$max)
        a2 <- as.numeric(dist2_params$min)
        b2 <- as.numeric(dist2_params$max)

        # Calculate KL using numerical integration approach
        # Create a grid over the prior's support
        n_points <- 1000
        x <- seq(a1, b1, length.out = n_points)
        dx <- (b1 - a1) / n_points

        # Prior density (uniform)
        p_prior <- rep(1 / (b1 - a1), n_points)

        # Posterior density (0 outside [a2, b2])
        p_post <- ifelse(x >= a2 & x <= b2, 1 / (b2 - a2), 1e-10)

        # Calculate KL divergence using numerical integration
        # KL(P||Q) = sum(P(x) * log(P(x)/Q(x)) * dx)
        kl <- sum(p_prior * log(p_prior / p_post) * dx)

        # Cap at a reasonable maximum to avoid numerical issues
        if (kl > 20) kl <- 20  # This is still a very large KL divergence
      } else if (dist1_type == "lognormal") {
        mu1 <- as.numeric(dist1_params$meanlog)
        sigma1 <- as.numeric(dist1_params$sdlog)
        mu2 <- as.numeric(dist2_params$meanlog)
        sigma2 <- as.numeric(dist2_params$sdlog)
        kl <- log(sigma2/sigma1) + (sigma1^2 + (mu1 - mu2)^2) / (2 * sigma2^2) - 0.5
      } else if (dist1_type == "gompertz") {
        # Use numerical integration for Gompertz
        b1 <- as.numeric(dist1_params$b)
        eta1 <- as.numeric(dist1_params$eta)
        b2 <- as.numeric(dist2_params$b)
        eta2 <- as.numeric(dist2_params$eta)

        # Define range for integration
        x_max <- max(qgompertz(0.999, b = b1, eta = eta1),
                    qgompertz(0.999, b = b2, eta = eta2))
        n_points <- 1000
        x <- seq(0, x_max, length.out = n_points)
        dx <- x_max / n_points

        # Calculate densities
        p1 <- dgompertz(x, b = b1, eta = eta1)
        p2 <- dgompertz(x, b = b2, eta = eta2)

        # Add small epsilon to avoid log(0)
        eps <- 1e-10
        p1[p1 < eps] <- eps
        p2[p2 < eps] <- eps

        # Calculate KL divergence
        kl <- sum(p1 * log(p1 / p2) * dx)
        if (kl > 20) kl <- 20
      } else if (dist1_type == "truncnorm") {
        # Use numerical integration for truncated normal
        mu1 <- as.numeric(dist1_params$mean)
        sigma1 <- as.numeric(dist1_params$sd)
        a1 <- as.numeric(dist1_params$a %||% -45)
        b1 <- as.numeric(dist1_params$b %||% 45)

        mu2 <- as.numeric(dist2_params$mean)
        sigma2 <- as.numeric(dist2_params$sd)
        a2 <- as.numeric(dist2_params$a %||% -45)
        b2 <- as.numeric(dist2_params$b %||% 45)

        # Define range for integration
        x_min <- min(a1, a2)
        x_max <- max(b1, b2)
        n_points <- 1000
        x <- seq(x_min, x_max, length.out = n_points)
        dx <- (x_max - x_min) / n_points

        # Calculate truncated normal densities
        p1 <- dtruncnorm(x, a = a1, b = b1, mean = mu1, sd = sigma1)
        p2 <- dtruncnorm(x, a = a2, b = b2, mean = mu2, sd = sigma2)

        # Add small epsilon to avoid log(0)
        eps <- 1e-10
        p1[p1 < eps] <- eps
        p2[p2 < eps] <- eps

        # Calculate KL divergence
        kl <- sum(p1 * log(p1 / p2) * dx)
        if (kl > 20) kl <- 20
      }
    }, error = function(e) { kl <- NA })
    return(kl)
  }

  # =========================================================================
  # HELPER FUNCTION: Create Individual Parameter Plot
  # =========================================================================
  create_parameter_plot <- function(param_name, param_data, param_info, location = NULL,
                                   posterior_data = NULL) {

    if (is.null(param_data$distribution)) return(NULL)

    # Helper function to calculate distribution density
    calc_distribution_density <- function(dist_data, param_name, param_info) {
      x <- NULL; y <- NULL; dist_str <- NULL; mean_val <- NULL

      distribution <- dist_data$distribution
      parameters <- dist_data$parameters

      # Handle different distribution types
      if (distribution == "gamma" && !is.null(parameters)) {
        # Ensure parameters are numeric (handle potential character input from JSON)
        shape <- as.numeric(parameters$shape)
        rate <- as.numeric(parameters$rate)
        if (!is.null(shape) && !is.null(rate) && !is.na(shape) && !is.na(rate)) {
          x_max <- qgamma(0.99, shape, rate)
          x <- seq(0, x_max, length.out = 1000)
          y <- dgamma(x, shape, rate)
          dist_str <- sprintf("Gamma(%.2f, %.2f)", shape, rate)
          # Use fitted_mean if available, otherwise calculate
          if (!is.null(parameters$fitted_mean)) {
            mean_val <- as.numeric(parameters$fitted_mean)
          } else {
            mean_val <- shape / rate
          }
        }
      } else if (distribution == "beta" && !is.null(parameters)) {
        # Ensure parameters are numeric (handle potential character input from JSON)
        shape1 <- as.numeric(parameters$shape1)
        shape2 <- as.numeric(parameters$shape2)
        if (!is.null(shape1) && !is.null(shape2) && !is.na(shape1) && !is.na(shape2)) {
          x <- seq(0, 1, length.out = 1000)
          y <- dbeta(x, shape1, shape2)

          # Special handling for highly skewed distributions
          if (param_info$category == "initial_conditions" && param_name %in% c("prop_E_initial", "prop_I_initial")) {
            x_max <- qbeta(0.9999, shape1, shape2)
            mean_val <- shape1 / (shape1 + shape2)
            x_max <- max(x_max, mean_val * 5)
            x <- seq(0, x_max, length.out = 1000)
            y <- dbeta(x, shape1, shape2)
          }

          if (shape1 < 1) {
            dist_str <- sprintf("Beta(%.2g, %.0f)", shape1, shape2)
          } else {
            dist_str <- sprintf("Beta(%.1f, %.1f)", shape1, shape2)
          }
          # Use fitted_mean if available, otherwise calculate
          if (!is.null(parameters$fitted_mean)) {
            mean_val <- as.numeric(parameters$fitted_mean)
          } else {
            mean_val <- shape1 / (shape1 + shape2)
          }
        }
      } else if (distribution == "lognormal" && !is.null(parameters)) {
        # Handle both parameter naming conventions
        meanlog_val <- parameters$meanlog %||%
                       (if(!is.null(parameters$mean) && !is.null(parameters$sd)) {
                         mu <- parameters$mean
                         sigma <- parameters$sd
                         log(mu^2 / sqrt(mu^2 + sigma^2))
                       } else NULL)

        sdlog_val <- parameters$sdlog %||%
                     (if(!is.null(parameters$mean) && !is.null(parameters$sd)) {
                       mu <- parameters$mean
                       sigma <- parameters$sd
                       sqrt(log(1 + sigma^2 / mu^2))
                     } else NULL)

        if (!is.null(meanlog_val) && !is.null(sdlog_val) &&
            is.finite(meanlog_val) && is.finite(sdlog_val) && sdlog_val > 0) {
          x_max <- qlnorm(0.99, meanlog_val, sdlog_val)
          if (is.finite(x_max) && x_max > 0) {
            x <- seq(0, x_max, length.out = 1000)
            y <- dlnorm(x, meanlog_val, sdlog_val)
            dist_str <- sprintf("LogNormal(%.2g, %.2g)", meanlog_val, sdlog_val)
            # Use fitted_mean if available, otherwise calculate
            if (!is.null(parameters$fitted_mean) || !is.null(parameters$mean)) {
              mean_val <- as.numeric(parameters$fitted_mean %||% parameters$mean)
            } else {
              mean_val <- exp(meanlog_val + sdlog_val^2/2)
            }
          }
        }
      } else if (distribution == "uniform" && !is.null(parameters)) {
        # Ensure parameters are numeric
        min_val <- as.numeric(parameters$min)
        max_val <- as.numeric(parameters$max)
        if (!is.null(min_val) && !is.null(max_val) && !is.na(min_val) && !is.na(max_val) &&
            is.finite(min_val) && is.finite(max_val) && max_val > min_val) {
          buffer <- (max_val - min_val) * 0.1
          x <- seq(min_val - buffer, max_val + buffer, length.out = 1000)
          y <- dunif(x, min_val, max_val)
          dist_str <- sprintf("Uniform(%.2f, %.2f)", min_val, max_val)
          mean_val <- (min_val + max_val) / 2
        }
      } else if (distribution == "normal" && !is.null(parameters)) {
        # Ensure parameters are numeric
        mean_param <- as.numeric(parameters$mean)
        sd_param <- as.numeric(parameters$sd)
        if (!is.null(mean_param) && !is.null(sd_param) && !is.na(mean_param) && !is.na(sd_param) &&
            is.finite(mean_param) && is.finite(sd_param) && sd_param > 0) {
          x <- seq(mean_param - 4*sd_param, mean_param + 4*sd_param, length.out = 1000)
          y <- dnorm(x, mean_param, sd_param)
          dist_str <- sprintf("Normal(%.3f, %.3f)", mean_param, sd_param)
          mean_val <- mean_param
        }
      } else if (distribution == "gompertz" && !is.null(parameters)) {
        # Ensure parameters are numeric
        b_param <- as.numeric(parameters$b)
        eta_param <- as.numeric(parameters$eta)
        if (!is.null(b_param) && !is.null(eta_param) && !is.na(b_param) && !is.na(eta_param) &&
            is.finite(b_param) && is.finite(eta_param) &&
            b_param > 0 && eta_param > 0) {
          tryCatch({
            x_max <- qgompertz(0.99, b = b_param, eta = eta_param)
            if (is.finite(x_max) && x_max > 0) {
              x <- seq(0, x_max, length.out = 1000)
              y <- dgompertz(x, b = b_param, eta = eta_param)
              dist_str <- sprintf("Gompertz(b=%.1f, eta=%.1f)", b_param, eta_param)
              # Use fitted_mean if available, otherwise calculate mode
              if (!is.null(parameters$fitted_mean)) {
                mean_val <- as.numeric(parameters$fitted_mean)
              } else {
                # Mode formula for Gompertz
                mean_val <- (1 / b_param) * log(eta_param / b_param)
              }
              if (!is.finite(mean_val)) mean_val <- NULL
            }
          }, error = function(e) {
            # Skip problematic Gompertz parameters
            return(NULL)
          })
        }
      } else if (distribution == "truncnorm" && !is.null(parameters)) {
        # Ensure parameters are numeric
        mean_param <- as.numeric(parameters$mean)
        sd_param <- as.numeric(parameters$sd)
        a_bound <- as.numeric(parameters$a %||% -45)
        b_bound <- as.numeric(parameters$b %||% 45)

        if (!is.null(mean_param) && !is.null(sd_param) && !is.na(mean_param) && !is.na(sd_param) &&
            is.finite(mean_param) && is.finite(sd_param) && sd_param > 0 &&
            is.finite(a_bound) && is.finite(b_bound) && b_bound > a_bound) {
          x <- seq(a_bound, b_bound, length.out = 1000)
          y <- truncnorm::dtruncnorm(x, a = a_bound, b = b_bound, mean = mean_param, sd = sd_param)
          dist_str <- sprintf("TruncNorm(%.1f, %.1f, [%.0f, %.0f])", mean_param, sd_param, a_bound, b_bound)
          mean_val <- mean_param
        }
      }

      return(list(x = x, y = y, dist_str = dist_str, mean_val = mean_val))
    }

    # Calculate prior distribution
    prior_result <- calc_distribution_density(param_data, param_name, param_info)
    if (is.null(prior_result$x) || is.null(prior_result$y)) return(NULL)

    # Calculate posterior distribution if provided
    posterior_result <- NULL
    if (!is.null(posterior_data) && !is.null(posterior_data$distribution)) {
      posterior_result <- calc_distribution_density(posterior_data, param_name, param_info)
    }

    # Create combined plot data
    plot_data <- data.frame(
      x = prior_result$x,
      y = prior_result$y,
      type = "Prior",
      stringsAsFactors = FALSE
    )

    # Add posterior data if available
    if (!is.null(posterior_result) && !is.null(posterior_result$x) && !is.null(posterior_result$y)) {
      # Remove non-finite values from posterior
      posterior_finite_mask <- is.finite(posterior_result$x) & is.finite(posterior_result$y) & posterior_result$y > 0
      if (sum(posterior_finite_mask) >= 10) {
        posterior_data_df <- data.frame(
          x = posterior_result$x[posterior_finite_mask],
          y = posterior_result$y[posterior_finite_mask],
          type = "Posterior",
          stringsAsFactors = FALSE
        )
        plot_data <- rbind(plot_data, posterior_data_df)
      }
    }

    # Remove non-finite values from prior
    finite_mask <- is.finite(plot_data$x) & is.finite(plot_data$y) & plot_data$y > 0
    if (sum(finite_mask) < 10) return(NULL)
    plot_data <- plot_data[finite_mask, ]

    # Create subtitle with KL divergence if applicable
    subtitle_str <- prior_result$dist_str
    kl_text <- ""

    if (!is.null(posterior_result) && !is.null(posterior_result$dist_str)) {
      # Calculate KL divergence (Prior || Posterior)
      kl_value <- calc_kl_analytical(
        param_data$distribution, param_data$parameters,
        posterior_data$distribution, posterior_data$parameters
      )

      # Format KL for display (check for NULL and length)
      if (!is.null(kl_value) && length(kl_value) > 0 && !is.na(kl_value)) {
        if (is.infinite(kl_value)) {
          kl_text <- " | KL: Inf"
        } else if (kl_value < 0.01) {
          kl_text <- sprintf(" | KL: %.3f", kl_value)
        } else if (kl_value < 1) {
          kl_text <- sprintf(" | KL: %.2f", kl_value)
        } else {
          kl_text <- sprintf(" | KL: %.1f", kl_value)
        }
      }

      subtitle_str <- paste0("Prior: ", prior_result$dist_str,
                            "\nPosterior: ", posterior_result$dist_str, kl_text)
    }

    if (!is.null(param_data$derived_note)) {
      subtitle_str <- paste0(subtitle_str, "\n", param_data$derived_note)
    }

    # Determine x-axis formatting
    x_scale <- if (param_info$category == "initial_conditions" && param_name %in% c("prop_E_initial", "prop_I_initial")) {
      scale_x_continuous(labels = scales::scientific)
    } else if (grepl("beta.*transmission", param_name)) {
      scale_x_continuous(labels = scales::scientific)
    } else {
      scale_x_continuous()
    }

    # Create plot
    if ("Posterior" %in% plot_data$type) {
      # Plot with both prior and posterior
      p <- ggplot(plot_data, aes(x = x, y = y, color = type, fill = type)) +
        geom_area(alpha = 0.3, position = "identity") +
        geom_line(linewidth = 0.7) +
        scale_color_manual(values = c("Prior" = "#457b9d", "Posterior" = "#e63946"),
                          name = "") +
        scale_fill_manual(values = c("Prior" = "#457b9d", "Posterior" = "#e63946"),
                         name = "") +
        x_scale +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray50", face = "italic"),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 9),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray90"),
          legend.position = "none"  # Remove legend from individual plots
        )
    } else {
      # Plot with prior only (original style)
      p <- ggplot(plot_data, aes(x = x, y = y)) +
        geom_area(fill = "#457b9d", alpha = 0.4) +
        geom_line(color = "#457b9d", linewidth = 0.7) +
        x_scale +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray50", face = "italic"),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 9),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray90")
        )
    }

    # Add labels
    p <- p + labs(
      title = paste0(param_info$display_name, " (", param_name, ")"),
      subtitle = subtitle_str,
      x = param_info$units
    )

    # Add mean lines if applicable
    if (!is.null(prior_result$mean_val) && is.finite(prior_result$mean_val) &&
        !param_data$distribution %in% c("uniform")) {
      x_range <- range(plot_data$x, na.rm = TRUE)
      if (prior_result$mean_val >= x_range[1] && prior_result$mean_val <= x_range[2]) {
        p <- p + geom_vline(xintercept = prior_result$mean_val, linetype = "dashed",
                           color = "#457b9d", alpha = 0.5)
      }
    }

    if (!is.null(posterior_result) && !is.null(posterior_result$mean_val) &&
        is.finite(posterior_result$mean_val) && !posterior_data$distribution %in% c("uniform")) {
      x_range <- range(plot_data$x, na.rm = TRUE)
      if (posterior_result$mean_val >= x_range[1] && posterior_result$mean_val <= x_range[2]) {
        p <- p + geom_vline(xintercept = posterior_result$mean_val, linetype = "dashed",
                           color = "#e63946", alpha = 0.5)
      }
    }

    return(p)
  }

  # =========================================================================
  # PLOT 1: Global Parameters (Category-Based Organization)
  # =========================================================================
  cat("Plotting global parameter distributions organized by category...\n")

  # Get global parameters from estimated_parameters, ordered appropriately
  global_params_info <- estimated_parameters %>%
    filter(scale == "global") %>%
    arrange(order)

  if (!is.null(priors$parameters_global) && nrow(global_params_info) > 0) {
    # Organize global parameters by category
    global_categories <- unique(global_params_info$category)
    global_category_plots <- list()

    for (category in global_categories) {
      category_info <- global_params_info %>%
        filter(category == !!category) %>%
        arrange(order)

      category_plot_list <- list()

      for (i in 1:nrow(category_info)) {
        param_name <- category_info$parameter_name[i]
        param_info <- category_info[i, ]

        if (param_name %in% names(priors$parameters_global)) {
          param_data <- priors$parameters_global[[param_name]]

          # Get posterior data if available
          posterior_data <- NULL
          if (!is.null(posteriors) && !is.null(posteriors$parameters_global[[param_name]])) {
            posterior_data <- posteriors$parameters_global[[param_name]]
          }

          p <- create_parameter_plot(param_name, param_data, param_info,
                                    location = NULL, posterior_data = posterior_data)
          if (!is.null(p)) {
            category_plot_list[[param_name]] <- p
          }
        }
      }

      if (length(category_plot_list) > 0) {
        global_category_plots[[category]] <- category_plot_list
      }
    }

    # Create combined global plot with category sections
    if (length(global_category_plots) > 0) {
      final_plot_list <- list()

      # Main title with legend
      subtitle_text <- ""
      if (!is.null(priors$metadata$version)) {
        subtitle_text <- paste0("Priors version: ", priors$metadata$version)
      }

      # Update title based on whether posteriors are included
      title_text <- if (!is.null(posteriors)) {
        "Global Parameter Prior and Posterior Distributions"
      } else {
        "Global Parameter Prior Distributions"
      }

      # Create legend if posteriors are included
      if (!is.null(posteriors)) {
        # Create a dummy plot just for the legend
        legend_data <- data.frame(
          x = c(0, 1, 0, 1),
          y = c(0, 0, 1, 1),
          type = factor(c("Prior", "Prior", "Posterior", "Posterior"),
                       levels = c("Prior", "Posterior"))
        )
        legend_plot <- ggplot(legend_data, aes(x = x, y = y, color = type, fill = type)) +
          geom_area(alpha = 0.3) +
          scale_color_manual(values = c("Prior" = "#457b9d", "Posterior" = "#e63946"),
                           name = "") +
          scale_fill_manual(values = c("Prior" = "#457b9d", "Posterior" = "#e63946"),
                          name = "") +
          theme_void() +
          theme(legend.position = "top",
                legend.direction = "horizontal",
                legend.justification = "center",
                legend.text = element_text(size = 12),
                legend.key.size = unit(1.2, "cm"))

        # Extract just the legend
        legend_grob <- cowplot::get_legend(legend_plot)

        # Combine title and legend
        if (subtitle_text != "") {
          title_section <- cowplot::plot_grid(
            cowplot::ggdraw() + cowplot::draw_label(title_text, fontface = 'bold', size = 16),
            cowplot::ggdraw() + cowplot::draw_label(subtitle_text, size = 12, color = "gray30"),
            legend_grob,
            ncol = 1, rel_heights = c(0.35, 0.25, 0.4)
          )
        } else {
          title_section <- cowplot::plot_grid(
            cowplot::ggdraw() + cowplot::draw_label(title_text, fontface = 'bold', size = 16),
            legend_grob,
            ncol = 1, rel_heights = c(0.5, 0.5)
          )
        }
        main_title <- title_section
      } else {
        # No posteriors, so no legend needed
        if (subtitle_text != "") {
          main_title <- cowplot::ggdraw() +
            cowplot::draw_label(title_text,
                               fontface = 'bold', size = 16, x = 0.5, y = 0.7, hjust = 0.5) +
            cowplot::draw_label(subtitle_text,
                               fontface = 'plain', size = 12, x = 0.5, y = 0.3, hjust = 0.5, color = "gray30")
        } else {
          main_title <- cowplot::ggdraw() +
            cowplot::draw_label(title_text,
                               fontface = 'bold', size = 16, x = 0.5, hjust = 0.5)
        }
      }

      final_plot_list[[1]] <- main_title
      heights <- c(0.08)  # Slightly more space for title + legend

      # Add each category section
      for (category in names(global_category_plots)) {
        category_title <- switch(category,
                               "transmission" = "Transmission Parameters",
                               "environmental" = "Environmental Parameters",
                               "disease" = "Disease Parameters",
                               "immunity" = "Immunity Parameters",
                               "surveillance" = "Surveillance Parameters",
                               "mobility" = "Mobility Parameters",
                               tools::toTitleCase(gsub("_", " ", category)))

        # Category heading
        category_heading <- cowplot::ggdraw() +
          cowplot::draw_label(category_title, fontface = 'bold', size = 14, x = 0.5, y = 0.5, hjust = 0.5)

        # Category plots grid - use 2 columns with equal subplot dimensions
        n_plots <- length(global_category_plots[[category]])
        ncol <- 2
        nrow <- ceiling(n_plots / ncol)

        # Create grid with equal subplot dimensions (always use 2-column layout)
        category_combined <- cowplot::plot_grid(plotlist = global_category_plots[[category]],
                                              ncol = ncol, nrow = nrow,
                                              align = "hv", axis = "tblr")

        final_plot_list[[length(final_plot_list) + 1]] <- category_heading
        final_plot_list[[length(final_plot_list) + 1]] <- category_combined

        # Adjust height based on number of rows - provide more space for readability
        # Give extra height to key categories
        if (category %in% c("environmental", "disease", "transmission", "immunity")) {
          section_height <- max(0.2, nrow * 0.15)  # More space for important categories
        } else {
          section_height <- max(0.15, nrow * 0.12)  # Standard space for other categories
        }
        heights <- c(heights, 0.03, section_height)
      }

      # Add bottom legend if posteriors are included
      if (!is.null(posteriors)) {
        final_plot_list[[length(final_plot_list) + 1]] <- legend_grob
        heights <- c(heights, 0.04)
      }

      # Normalize heights
      heights <- heights / sum(heights)

      # Combine all sections
      p_global <- cowplot::plot_grid(plotlist = final_plot_list,
                                    ncol = 1, rel_heights = heights)

      # Add common y-axis label
      p_global <- cowplot::ggdraw(p_global) +
        cowplot::draw_label("Density", x = 0.02, y = 0.5, angle = 90, size = 12)

      # Calculate appropriate height with extra space for disease and immunity
      base_height <- 4  # Base height for title and margins
      category_height <- sum(sapply(names(global_category_plots), function(cat_name) {
        cat <- global_category_plots[[cat_name]]
        n_plots <- length(cat)
        nrow <- ceiling(n_plots / 2)  # 2 columns
        # Give extra height to disease and immunity categories
        if (cat_name %in% c("disease", "immunity")) {
          if (nrow == 1) {
            return(3.2)  # Disease/immunity single row: 3.2 units (increased)
          } else {
            return(nrow * 3.2)  # Disease/immunity multi-row: 3.2 units per row (increased)
          }
        } else {
          if (nrow == 1) {
            return(2.8)  # Other single row: 2.8 units
          } else {
            return(nrow * 2.8)  # Other multi-row: 2.8 units per row
          }
        }
      }))
      plot_height <- base_height + category_height

      # Save plot with increased dimensions and limitsize override
      filename <- if (!is.null(posteriors)) "posteriors_global.pdf" else "priors_global.pdf"
      ggsave(file.path(output_dir, filename), p_global,
             width = 12, height = plot_height, limitsize = FALSE)
      plots$global <- p_global
      cat(paste0("  Saved: ", filename, "\n"))
    }
  }

  # =========================================================================
  # PLOT 2: Location-Specific Parameters (Category-Based Organization)
  # =========================================================================
  if (!is.null(location_codes) && length(location_codes) > 0) {
    cat("Plotting location-specific priors organized by category...\n")

    # Get location-specific parameters organized by category
    location_params_info <- estimated_parameters %>%
      filter(scale == "location") %>%
      arrange(order)

    # Get target date from config
    target_date <- "2023-01-01"
    if (!is.null(config$date_start)) {
      target_date <- as.character(config$date_start)
    }

    # Load population data for context (if available)
    pop_data <- NULL
    if (!is.null(PATHS) && !is.null(PATHS$DATA_PROCESSED)) {
      un_pop_file <- file.path(PATHS$DATA_PROCESSED, "demographics", "UN_world_population_prospects_daily.csv")
      if (file.exists(un_pop_file)) {
        pop_data <- read.csv(un_pop_file, stringsAsFactors = FALSE)
      }
    }

    for (iso in location_codes) {
      # Get population size for context
      pop_size <- NULL
      if (!is.null(pop_data)) {
        country_data <- pop_data[pop_data$iso_code == iso, ]
        if (nrow(country_data) > 0) {
          exact_match <- country_data[country_data$date == target_date, ]
          if (nrow(exact_match) > 0) {
            pop_size <- exact_match$total_population[1]
          } else {
            pop_size <- tail(country_data$total_population, 1)
          }
        }
      }

      # Organize plots by category
      category_plots <- list()
      categories_available <- unique(location_params_info$category)

      for (category in categories_available) {
        category_info <- location_params_info %>%
          filter(category == !!category) %>%
          arrange(order)

        category_plot_list <- list()

        for (i in 1:nrow(category_info)) {
          param_name <- category_info$parameter_name[i]
          param_info <- category_info[i, ]

          # Handle parameter name mapping for seasonality and derived parameters
          prior_param_name <- switch(param_name,
                                   "a_1_j" = "a1",
                                   "a_2_j" = "a2",
                                   "b_1_j" = "b1",
                                   "b_2_j" = "b2",
                                   "beta_j0_hum" = "beta_j0_tot",  # Use beta_j0_tot as base for human component
                                   "beta_j0_env" = "beta_j0_tot",  # Use beta_j0_tot as base for environmental component
                                   param_name)

          if (prior_param_name %in% names(priors$parameters_location)) {
            param_data_raw <- priors$parameters_location[[prior_param_name]]$location[[iso]]

            if (!is.null(param_data_raw)) {
              # Handle nested parameter structure
              param_data <- if (!is.null(param_data_raw$parameters)) {
                list(distribution = param_data_raw$distribution %||%
                                  priors$parameters_location[[prior_param_name]]$distribution,
                     parameters = param_data_raw$parameters)
              } else {
                list(distribution = priors$parameters_location[[prior_param_name]]$distribution,
                     parameters = param_data_raw)
              }

              # Special handling for derived beta parameters
              if (param_name %in% c("beta_j0_hum", "beta_j0_env")) {
                # For derived parameters, we need both beta_j0_tot and p_beta
                p_beta_data <- priors$parameters_location[["p_beta"]]$location[[iso]]
                if (!is.null(p_beta_data)) {
                  # Create a note that this is derived from beta_j0_tot * p_beta or beta_j0_tot * (1-p_beta)
                  if (param_name == "beta_j0_hum") {
                    param_data$derived_note <- "Derived from beta_j0_tot × p_beta"
                  } else {
                    param_data$derived_note <- "Derived from beta_j0_tot × (1-p_beta)"
                  }
                }
              }

              # Get posterior data if available (use same parameter mapping)
              posterior_data <- NULL
              if (!is.null(posteriors) && !is.null(posteriors$parameters_location[[prior_param_name]])) {
                posterior_data_raw <- posteriors$parameters_location[[prior_param_name]]$location[[iso]]
                if (!is.null(posterior_data_raw)) {
                  posterior_data <- if (!is.null(posterior_data_raw$parameters)) {
                    list(distribution = posterior_data_raw$distribution %||%
                                      posteriors$parameters_location[[prior_param_name]]$distribution,
                         parameters = posterior_data_raw$parameters)
                  } else {
                    list(distribution = posteriors$parameters_location[[prior_param_name]]$distribution,
                         parameters = posterior_data_raw)
                  }
                }
              }

              p <- create_parameter_plot(param_name, param_data, param_info, location = iso,
                                        posterior_data = posterior_data)
              if (!is.null(p)) {
                # Add population context for initial conditions
                if (param_info$category == "initial_conditions" && !is.null(pop_size)) {
                  mean_val <- if (param_data$distribution == "beta") {
                    param_data$parameters$shape1 / (param_data$parameters$shape1 + param_data$parameters$shape2)
                  } else NULL

                  if (!is.null(mean_val)) {
                    pop_count <- formatC(round(mean_val * pop_size), format = "d", big.mark = ",")
                    current_subtitle <- p$labels$subtitle
                    p$labels$subtitle <- paste0(current_subtitle, " (", pop_count, ")")
                  }
                }

                category_plot_list[[param_name]] <- p
              }
            }
          }
        }

        if (length(category_plot_list) > 0) {
          category_plots[[category]] <- category_plot_list
        }
      }

      # Create combined plot for this location
      if (length(category_plots) > 0) {
        final_plot_list <- list()

        # Main title with legend
        title_text <- if (!is.null(posteriors)) {
          paste0("Location-Specific Prior and Posterior Distributions: ", iso)
        } else {
          paste0("Location-Specific Prior Distributions: ", iso)
        }
        subtitle_parts <- paste0("Start date: ", target_date)
        if (!is.null(priors$metadata$version)) {
          subtitle_parts <- paste0(subtitle_parts, " | Priors version: ", priors$metadata$version)
        }

        # Create legend if posteriors are included (reuse from global)
        if (!is.null(posteriors)) {
          # Create a dummy plot just for the legend
          legend_data <- data.frame(
            x = c(0, 1, 0, 1),
            y = c(0, 0, 1, 1),
            type = factor(c("Prior", "Prior", "Posterior", "Posterior"),
                         levels = c("Prior", "Posterior"))
          )
          legend_plot <- ggplot(legend_data, aes(x = x, y = y, color = type, fill = type)) +
            geom_area(alpha = 0.3) +
            scale_color_manual(values = c("Prior" = "#457b9d", "Posterior" = "#e63946"),
                             name = "") +
            scale_fill_manual(values = c("Prior" = "#457b9d", "Posterior" = "#e63946"),
                            name = "") +
            theme_void() +
            theme(legend.position = "top",
                  legend.direction = "horizontal",
                  legend.justification = "center",
                  legend.text = element_text(size = 12),
                  legend.key.size = unit(1.2, "cm"))

          # Extract just the legend
          location_legend_grob <- cowplot::get_legend(legend_plot)

          # Combine title and legend
          title_section <- cowplot::plot_grid(
            cowplot::ggdraw() + cowplot::draw_label(title_text, fontface = 'bold', size = 16),
            cowplot::ggdraw() + cowplot::draw_label(subtitle_parts, size = 12, color = "gray30"),
            location_legend_grob,
            ncol = 1, rel_heights = c(0.35, 0.25, 0.4)
          )
          main_title <- title_section
        } else {
          main_title <- cowplot::ggdraw() +
            cowplot::draw_label(title_text, fontface = 'bold', size = 16, x = 0.5, y = 0.7, hjust = 0.5) +
            cowplot::draw_label(subtitle_parts, fontface = 'plain', size = 12, x = 0.5, y = 0.3, hjust = 0.5, color = "gray30")
        }

        final_plot_list[[1]] <- main_title
        heights <- c(0.08)  # More space for title + legend

        # Add each category section
        for (category in names(category_plots)) {
          category_title <- switch(category,
                                 "initial_conditions" = "Initial Conditions",
                                 "transmission" = "Transmission Parameters",
                                 "seasonality" = "Seasonality Parameters",
                                 "environmental" = "Environmental Calibration (psi*)",
                                 "disease" = "Disease Parameters",
                                 "mobility" = "Mobility Parameters",
                                 "spatial" = "Spatial Parameters",
                                 tools::toTitleCase(gsub("_", " ", category)))

          # Category heading
          category_heading <- cowplot::ggdraw() +
            cowplot::draw_label(category_title, fontface = 'bold', size = 14, x = 0.5, y = 0.5, hjust = 0.5)

          # Category plots grid - use 2 columns with equal subplot dimensions
          n_plots <- length(category_plots[[category]])
          ncol <- 2
          nrow <- ceiling(n_plots / ncol)

          # Create grid with equal subplot dimensions (always use 2-column layout)
          category_combined <- cowplot::plot_grid(plotlist = category_plots[[category]],
                                                ncol = ncol, nrow = nrow,
                                                align = "hv", axis = "tblr")

          final_plot_list[[length(final_plot_list) + 1]] <- category_heading
          final_plot_list[[length(final_plot_list) + 1]] <- category_combined

          # Adjust height based on number of rows for better readability
          # Give extra height to key categories
          if (category %in% c("environmental", "disease", "transmission", "immunity")) {
            section_height <- max(0.2, nrow * 0.15)  # More space for important categories
          } else {
            section_height <- max(0.15, nrow * 0.12)  # Standard space for other categories
          }
          heights <- c(heights, 0.03, section_height)  # Heading + plots
        }

        # Footer
        footer <- cowplot::ggdraw() +
          cowplot::draw_label("Initial condition proportions are normalized to sum to 1.0 during sampling",
                             fontface = 'italic', size = 10, x = 0.5, hjust = 0.5, color = "gray50")
        final_plot_list[[length(final_plot_list) + 1]] <- footer
        heights <- c(heights, 0.02)

        # Add bottom legend if posteriors are included
        if (!is.null(posteriors)) {
          final_plot_list[[length(final_plot_list) + 1]] <- location_legend_grob
          heights <- c(heights, 0.04)
        }

        # Normalize heights
        heights <- heights / sum(heights)

        # Combine all sections
        p_combined_final <- cowplot::plot_grid(plotlist = final_plot_list,
                                              ncol = 1, rel_heights = heights)

        # Add common y-axis label
        p_combined_final <- cowplot::ggdraw(p_combined_final) +
          cowplot::draw_label("Density", x = 0.02, y = 0.5, angle = 90, size = 12)

        # Save plot
        base_filename <- if (!is.null(posteriors)) "posteriors_" else "priors_"
        filename <- file.path(output_dir, paste0(base_filename, iso, ".pdf"))

        # Calculate appropriate height based on categories and plots - increased for readability
        base_height <- 4  # Base height for title, footer, and margins
        category_height <- sum(sapply(names(category_plots), function(cat_name) {
          n_plots <- length(category_plots[[cat_name]])
          nrow <- ceiling(n_plots / 2)  # 2 columns
          # Proportional height: single-row categories get adequate space, multi-row get more
          if (nrow == 1) {
            return(2.8)  # Single row: 2.8 units (increased for better visibility)
          } else {
            return(nrow * 2.8)  # Multi-row: 2.8 units per row (same as before)
          }
        }))
        plot_height <- base_height + category_height

        tryCatch({
          # Save with increased dimensions and limitsize override
          ggsave(filename, p_combined_final, width = 12, height = plot_height, limitsize = FALSE)
          if (!exists("plots$location_specific")) plots$location_specific <- list()
          plots$location_specific[[iso]] <- p_combined_final
          cat("  Saved:", basename(filename), "\n")
        }, error = function(e) {
          cat("  Warning: Could not save", basename(filename), "-", e$message, "\n")
        })
      }
    }
  }

  # Update todo status
  status_msg <- if (!is.null(posteriors)) {
    "✓ Successfully plotted prior and posterior distributions\n"
  } else {
    "✓ Successfully plotted prior distributions\n"
  }
  cat(status_msg)

  # Restore original warning setting
  options(warn = old_warn)

  return(invisible(plots))
}

# Define null-coalescing operator if not available
`%||%` <- function(x, y) if (is.null(x)) y else x
