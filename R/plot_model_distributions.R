#' Plot Multi-Method Parameter Distributions
#'
#' Creates visualizations comparing parameter distributions across multiple methods
#' (e.g., priors, BFRS posteriors, NPE posteriors) using a flexible vector-based interface.
#' Parameters are organized by biological category and ordered consistently across all plots.
#'
#' @param json_files Vector of paths to JSON distribution files
#' @param method_names Vector of method names corresponding to each JSON file (for legends and colors)
#' @param output_dir Directory to save generated plots
#' @param custom_colors Optional named vector of colors for each method (e.g., c("Prior" = "#4a4a4a", "BFRS" = "#1f77b4", "NPE" = "#d00000"))
#'
#' @return Invisibly returns a list of generated plot objects
#'
#' @examples
#' \dontrun{
#' # Compare three methods
#' plot_model_distributions(
#'   json_files = c("priors.json", "posteriors_bfrs.json", "posterior/posteriors.json"),
#'   method_names = c("Prior", "BFRS", "NPE"),
#'   output_dir = "plots"
#' )
#'
#' # Compare just prior and NPE
#' plot_model_distributions(
#'   json_files = c("priors.json", "posterior/posteriors.json"),
#'   method_names = c("Prior", "NPE"),
#'   output_dir = "plots"
#' )
#'
#' # Use custom colors (MOSAIC defaults shown)
#' plot_model_distributions(
#'   json_files = c("priors.json", "posteriors_bfrs.json", "posterior/posteriors.json"),
#'   method_names = c("Prior", "BFRS", "NPE"),
#'   output_dir = "plots",
#'   custom_colors = c(
#'     "Prior" = "#4a4a4a",  # Dark gray
#'     "BFRS" = "#1f77b4",   # Blue
#'     "NPE" = "#d00000"     # Red
#'   )
#' )
#' }
#'
#' @export
plot_model_distributions <- function(json_files, method_names, output_dir, custom_colors = NULL, verbose = FALSE) {

  # All required packages loaded via NAMESPACE

  # Save current warning setting
  old_warn <- getOption("warn")

  # =========================================================================
  # INPUT VALIDATION
  # =========================================================================

  # Validate basic inputs
  if (length(json_files) != length(method_names)) {
    stop("json_files and method_names must have the same length")
  }

  if (length(json_files) == 0) {
    stop("At least one JSON file must be provided")
  }

  # Check file existence
  file_exists <- file.exists(json_files)
  if (!any(file_exists)) {
    stop("No valid JSON files found")
  }

  # Filter to existing files only and warn about missing
  if (!all(file_exists)) {
    missing_methods <- method_names[!file_exists]
    warning("Missing files for methods: ", paste(missing_methods, collapse = ", "))
    json_files <- json_files[file_exists]
    method_names <- method_names[file_exists]
  }

  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # =========================================================================
  # LOAD DATA AND SETUP
  # =========================================================================

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

  # Load all JSON files into a named list
  methods_data <- setNames(vector("list", length(json_files)), method_names)

  for (i in seq_along(json_files)) {
    method_name <- method_names[i]
    file_path <- json_files[i]

    tryCatch({
      methods_data[[method_name]] <- jsonlite::read_json(file_path, simplifyVector = FALSE)
      methods_data[[method_name]] <- unnest_json(methods_data[[method_name]])
      message("Loaded ", method_name, " from ", basename(file_path))
    }, error = function(e) {
      warning("Failed to load ", method_name, ": ", e$message)
      methods_data[[method_name]] <- NULL
    })
  }

  # Remove failed loads
  methods_data <- methods_data[!sapply(methods_data, is.null)]

  if (length(methods_data) == 0) {
    stop("No JSON files could be loaded successfully")
  }

  # =========================================================================
  # COLOR SCHEME SETUP
  # =========================================================================

  # Generate color scheme using standardized method names
  method_names <- names(methods_data)

  # MOSAIC color scheme for calibration methods
  # Consistent colors across all distribution and quantile plots
  standard_colors <- list(
    "Prior" = "#4a4a4a",    # Dark gray (neutral baseline)
    "Priors" = "#4a4a4a",   # Alternative naming
    "BFRS" = "#1f77b4",     # Blue (Bayesian retrospective)
    "Posterior" = "#1f77b4", # Alternative naming for BFRS
    "NPE" = "#d00000",      # Red (neural posterior estimation)
    "SMC" = "#d00000",      # Red (sequential Monte Carlo - deprecated)
    "MCMC" = "#CC79A7",     # Reddish purple (other methods)
    "ABC" = "#009E73",      # Bluish green (other methods)
    "SBC" = "#F0E442"       # Yellow (diagnostics)
  )

  # Fallback palette for methods not in standard list
  # Uses Paul Tol's bright qualitative palette (colorblind-friendly)
  fallback_palette <- c(
    "#4477AA",  # Blue
    "#EE6677",  # Red
    "#228833",  # Green
    "#CCBB44",  # Yellow
    "#66CCEE",  # Cyan
    "#AA3377",  # Purple
    "#BBBBBB"   # Grey
  )

  # Assign colors
  if (!is.null(custom_colors)) {
    # Use custom colors if provided
    method_colors <- custom_colors[method_names]

    # Fill in any missing colors with defaults
    missing_colors <- is.na(method_colors)
    if (any(missing_colors)) {
      missing_names <- method_names[missing_colors]
      for (name in missing_names) {
        # Try standard colors first
        name_upper <- toupper(name)
        color_found <- FALSE
        for (std_name in names(standard_colors)) {
          if (toupper(std_name) == name_upper) {
            method_colors[name] <- standard_colors[[std_name]]
            color_found <- TRUE
            break
          }
        }
        # If not found, use fallback palette
        if (!color_found) {
          idx <- which(method_names == name)
          method_colors[name] <- fallback_palette[((idx - 1) %% length(fallback_palette)) + 1]
        }
      }
    }
  } else {
    # Use standard color assignment
    method_colors <- sapply(method_names, function(name) {
      # Check if method name (case-insensitive) is in standard colors
      name_upper <- toupper(name)
      for (std_name in names(standard_colors)) {
        if (toupper(std_name) == name_upper) {
          return(standard_colors[[std_name]])
        }
      }
      # If not found, use fallback palette
      idx <- which(method_names == name)
      return(fallback_palette[((idx - 1) %% length(fallback_palette)) + 1])
    })
  }
  names(method_colors) <- method_names

  # =========================================================================
  # EXTRACT LOCATIONS FROM FIRST AVAILABLE METHOD
  # =========================================================================

  location_codes <- NULL
  for (method_name in names(methods_data)) {
    method_data <- methods_data[[method_name]]
    if (!is.null(method_data$parameters_location)) {
      for (param in names(method_data$parameters_location)) {
        if (!is.null(method_data$parameters_location[[param]]$location)) {
          location_codes <- names(method_data$parameters_location[[param]]$location)
          break
        }
      }
      if (!is.null(location_codes)) break
    }
  }

  plots <- list()

  # =========================================================================
  # HELPER FUNCTIONS
  # =========================================================================

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
        kl <- sum(p_prior * log(p_prior / p_post) * dx)

        # Cap at a reasonable maximum to avoid numerical issues
        if (kl > 20) kl <- 20
      } else if (dist1_type == "lognormal") {
        mu1 <- as.numeric(dist1_params$meanlog)
        sigma1 <- as.numeric(dist1_params$sdlog)
        mu2 <- as.numeric(dist2_params$meanlog)
        sigma2 <- as.numeric(dist2_params$sdlog)
        kl <- log(sigma2/sigma1) + (sigma1^2 + (mu1 - mu2)^2) / (2 * sigma2^2) - 0.5
      }
    }, error = function(e) { kl <- NA })
    return(kl)
  }

  # Helper function to calculate distribution density
  calc_distribution_density <- function(dist_data, param_name, param_info) {
    x <- NULL; y <- NULL; dist_str <- NULL; mean_val <- NULL
    failure_reason <- NULL  # Track why density calculation failed

    distribution <- dist_data$distribution
    parameters <- dist_data$parameters

    # Handle different distribution types
    if (distribution == "gamma" && !is.null(parameters)) {
      shape <- as.numeric(parameters$shape)
      rate <- as.numeric(parameters$rate)
      if (!is.null(shape) && !is.null(rate) && !is.na(shape) && !is.na(rate)) {
        x_max <- qgamma(0.99, shape, rate)
        x <- seq(0, x_max, length.out = 1000)
        y <- dgamma(x, shape, rate)
        dist_str <- sprintf("Gamma(%.2f, %.2f)", shape, rate)
        mean_val <- if (!is.null(parameters$fitted_mean)) as.numeric(parameters$fitted_mean) else shape / rate
      }
    } else if (distribution == "beta" && !is.null(parameters)) {
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
        mean_val <- if (!is.null(parameters$fitted_mean)) as.numeric(parameters$fitted_mean) else shape1 / (shape1 + shape2)
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

      if (is.null(meanlog_val) || is.null(sdlog_val)) {
        failure_reason <- "lognormal: meanlog or sdlog is NULL"
      } else if (!is.finite(meanlog_val) || !is.finite(sdlog_val)) {
        failure_reason <- sprintf("lognormal: non-finite params (meanlog=%.4g, sdlog=%.4g)", meanlog_val, sdlog_val)
      } else if (sdlog_val <= 0) {
        failure_reason <- sprintf("lognormal: sdlog <= 0 (%.4g)", sdlog_val)
      } else {
        # Use quantile-based x-axis range for better coverage of extreme distributions
        x_min <- qlnorm(0.001, meanlog_val, sdlog_val)  # 0.1st percentile
        x_max <- qlnorm(0.999, meanlog_val, sdlog_val)  # 99.9th percentile

        if (!is.finite(x_min) || !is.finite(x_max)) {
          failure_reason <- sprintf("lognormal: non-finite quantiles (x_min=%.4g, x_max=%.4g)", x_min, x_max)
        } else if (x_max <= x_min) {
          failure_reason <- sprintf("lognormal: x_max <= x_min (%.4g <= %.4g)", x_max, x_min)
        } else {
          x <- seq(x_min, x_max, length.out = 1000)
          y <- dlnorm(x, meanlog_val, sdlog_val)

          # Check if density is non-negligible (avoid flat lines from numerical precision issues)
          max_density <- max(y, na.rm = TRUE)
          if (!is.finite(max_density)) {
            failure_reason <- "lognormal: max_density is not finite"
            x <- NULL
            y <- NULL
          } else if (max_density <= 0) {
            failure_reason <- sprintf("lognormal: max_density is zero or negative (%.4g)", max_density)
            x <- NULL
            y <- NULL
          } else {
            # Very lenient filtering: keep points with density > 1e-12 × max
            # This allows plotting of very diffuse/wide distributions with low peak density
            # while still filtering out numerical noise near zero
            keep_idx <- y > max_density * 1e-12
            if (sum(keep_idx) == 0) {
              failure_reason <- "lognormal: all points filtered out (y < threshold)"
              x <- NULL
              y <- NULL
            } else {
              x <- x[keep_idx]
              y <- y[keep_idx]

              dist_str <- sprintf("LogNormal(%.2g, %.2g)", meanlog_val, sdlog_val)
              mean_val <- if (!is.null(parameters$fitted_mean) || !is.null(parameters$mean)) {
                as.numeric(parameters$fitted_mean %||% parameters$mean)
              } else {
                exp(meanlog_val + sdlog_val^2/2)
              }
            }
          }
        }
      }
    } else if (distribution == "uniform" && !is.null(parameters)) {
      min_val <- if (!is.null(parameters$min)) as.numeric(parameters$min) else NA_real_
      max_val <- if (!is.null(parameters$max)) as.numeric(parameters$max) else NA_real_
      if (!is.na(min_val) && !is.na(max_val) &&
          is.finite(min_val) && is.finite(max_val) && max_val > min_val) {
        buffer <- (max_val - min_val) * 0.1
        x <- seq(min_val - buffer, max_val + buffer, length.out = 1000)
        y <- dunif(x, min_val, max_val)
        dist_str <- sprintf("Uniform(%.2f, %.2f)", min_val, max_val)
        mean_val <- (min_val + max_val) / 2
      }
    } else if (distribution == "normal" && !is.null(parameters)) {
      mean_param <- if (!is.null(parameters$mean)) as.numeric(parameters$mean) else NA_real_
      sd_param <- if (!is.null(parameters$sd)) as.numeric(parameters$sd) else NA_real_
      if (!is.na(mean_param) && !is.na(sd_param) &&
          is.finite(mean_param) && is.finite(sd_param) && sd_param > 0) {
        x <- seq(mean_param - 4*sd_param, mean_param + 4*sd_param, length.out = 1000)
        y <- dnorm(x, mean_param, sd_param)
        dist_str <- sprintf("Normal(%.3f, %.3f)", mean_param, sd_param)
        mean_val <- mean_param
      }
    } else if (distribution == "gompertz" && !is.null(parameters)) {
      b_param <- if (!is.null(parameters$b)) as.numeric(parameters$b) else NA_real_
      eta_param <- if (!is.null(parameters$eta)) as.numeric(parameters$eta) else NA_real_
      if (!is.na(b_param) && !is.na(eta_param) &&
          is.finite(b_param) && is.finite(eta_param) &&
          b_param > 0 && eta_param > 0) {
        tryCatch({
          x_max <- qgompertz(0.99, b = b_param, eta = eta_param)
          if (is.finite(x_max) && x_max > 0) {
            x <- seq(0, x_max, length.out = 1000)
            y <- dgompertz(x, b = b_param, eta = eta_param)
            dist_str <- sprintf("Gompertz(b=%.1f, eta=%.1f)", b_param, eta_param)
            mean_val <- if (!is.null(parameters$fitted_mean)) {
              as.numeric(parameters$fitted_mean)
            } else {
              (1 / b_param) * log(eta_param / b_param)
            }
            if (!is.finite(mean_val)) mean_val <- NULL
          }
        }, error = function(e) {
          return(NULL)
        })
      }
    } else if (distribution == "truncnorm" && !is.null(parameters)) {
      mean_param <- if (!is.null(parameters$mean)) as.numeric(parameters$mean) else NA_real_
      sd_param <- if (!is.null(parameters$sd)) as.numeric(parameters$sd) else NA_real_
      # Use infinite bounds as defaults (matches fit_truncnorm_from_ci behavior)
      a_bound <- if (!is.null(parameters$a)) as.numeric(parameters$a) else -Inf
      b_bound <- if (!is.null(parameters$b)) as.numeric(parameters$b) else Inf

      if (!is.na(mean_param) && !is.na(sd_param) &&
          is.finite(mean_param) && is.finite(sd_param) && sd_param > 0) {

        # Check bounds are valid (allow infinite bounds)
        valid_bounds <- (!is.na(a_bound) && !is.na(b_bound) &&
                        (is.infinite(a_bound) || is.infinite(b_bound) || b_bound > a_bound))

        if (valid_bounds) {
          # For plotting, use finite range based on distribution
          if (is.infinite(a_bound) || is.infinite(b_bound)) {
            # No truncation or one-sided: use mean ± 4sd (covers ~99.99% of normal dist)
            x_min <- if (is.infinite(a_bound)) mean_param - 4*sd_param else a_bound
            x_max <- if (is.infinite(b_bound)) mean_param + 4*sd_param else b_bound
          } else {
            # Both bounds finite: use them
            x_min <- a_bound
            x_max <- b_bound
          }

          x <- seq(x_min, x_max, length.out = 1000)
          y <- truncnorm::dtruncnorm(x, a = a_bound, b = b_bound, mean = mean_param, sd = sd_param)

          # Format bounds for display
          a_str <- if (is.infinite(a_bound)) "-Inf" else sprintf("%.0f", a_bound)
          b_str <- if (is.infinite(b_bound)) "Inf" else sprintf("%.0f", b_bound)
          dist_str <- sprintf("TruncNorm(%.1f, %.1f, [%s, %s])", mean_param, sd_param, a_str, b_str)
          mean_val <- mean_param
        }
      }
    }

    return(list(x = x, y = y, dist_str = dist_str, mean_val = mean_val, failure_reason = failure_reason))
  }

  # =========================================================================
  # MAIN PLOTTING FUNCTION: Create Multi-Method Parameter Plot
  # =========================================================================

  create_multi_method_plot <- function(param_name, param_info, location = NULL) {

    # Collect distributions for this parameter across all methods
    param_distributions <- list()

    for (method_name in names(methods_data)) {
      method_data <- methods_data[[method_name]]
      param_data <- NULL

      # Generate parameter name variants to handle naming inconsistencies
      # between priors, BFRS, and NPE (especially seasonality parameters)
      param_variants <- c(param_name)

      # Add underscore variants for seasonality params (a1 → a_1_j)
      if (param_name %in% c("a1", "a2", "b1", "b2")) {
        base <- substr(param_name, 1, 1)  # "a" or "b"
        num <- substr(param_name, 2, 2)   # "1" or "2"
        param_variants <- c(param_variants, paste0(base, "_", num, "_j"))
      }

      # Add non-underscore variants (a_1_j → a1)
      if (grepl("^[ab]_\\d_j$", param_name)) {
        param_variants <- c(param_variants,
                           gsub("_j$", "", gsub("_", "", param_name)))
      }

      # Try to get parameter from global parameters first (try all variants)
      for (variant in param_variants) {
        if (!is.null(method_data$parameters_global) &&
            variant %in% names(method_data$parameters_global)) {
          param_data <- method_data$parameters_global[[variant]]
          break
        }
      }

      # Try to get parameter from location-specific parameters (try all variants)
      if (is.null(param_data) && !is.null(location)) {
        for (variant in param_variants) {
          if (!is.null(method_data$parameters_location) &&
              variant %in% names(method_data$parameters_location) &&
              !is.null(method_data$parameters_location[[variant]]$location) &&
              location %in% names(method_data$parameters_location[[variant]]$location)) {
            param_data <- method_data$parameters_location[[variant]]$location[[location]]
            break
          }
        }
      }

      if (!is.null(param_data) && !is.null(param_data$distribution)) {
        # Skip failed distributions (marked by calc_model_posterior_distributions)
        if (param_data$distribution == "failed") {
          if (verbose) {
            cat(sprintf("  [SKIP] %s (%s): Distribution fitting failed\n",
                       param_name, method_name))
          }
          next
        }
        param_distributions[[method_name]] <- param_data
      } else {
        if (verbose && is.null(param_data)) {
          cat(sprintf("  [MISSING] %s (%s): Parameter not found in JSON\n",
                     param_name, method_name))
        } else if (verbose && is.null(param_data$distribution)) {
          cat(sprintf("  [MISSING] %s (%s): Distribution field is NULL\n",
                     param_name, method_name))
        }
      }
    }

    # Skip if no method has this parameter
    if (length(param_distributions) == 0) return(NULL)

    # Calculate density for each method
    method_results <- list()
    for (method_name in names(param_distributions)) {
      result <- calc_distribution_density(param_distributions[[method_name]], param_name, param_info)
      if (!is.null(result$x) && !is.null(result$y)) {
        method_results[[method_name]] <- result
      } else {
        if (verbose) {
          dist_type <- param_distributions[[method_name]]$distribution
          if (!is.null(result$failure_reason)) {
            cat(sprintf("  [DENSITY FAIL] %s (%s): %s\n",
                       param_name, method_name, result$failure_reason))
          } else {
            cat(sprintf("  [DENSITY FAIL] %s (%s): calc_distribution_density returned NULL for %s distribution\n",
                       param_name, method_name, dist_type))
          }
        }
      }
    }

    if (length(method_results) == 0) {
      if (verbose) {
        cat(sprintf("  [NO PLOT] %s: No valid densities calculated for any method\n", param_name))
      }
      return(NULL)
    }

    # Create combined plot data
    plot_data <- data.frame()

    for (method_name in names(method_results)) {
      result <- method_results[[method_name]]

      # Remove non-finite values
      finite_mask <- is.finite(result$x) & is.finite(result$y) & result$y > 0
      n_finite <- sum(finite_mask)

      # Lenient threshold: need at least 5 points for a meaningful plot
      # (reduced from 10 to allow plotting of distributions with extreme tails)
      if (n_finite >= 5) {
        method_data_df <- data.frame(
          x = result$x[finite_mask],
          y = result$y[finite_mask],
          method = method_name,
          stringsAsFactors = FALSE
        )
        plot_data <- rbind(plot_data, method_data_df)
      } else {
        if (verbose) {
          cat(sprintf("  [FILTERED] %s (%s): Only %d finite points (need ≥5)\n",
                     param_name, method_name, n_finite))
        }
      }
    }

    if (nrow(plot_data) == 0) {
      if (verbose) {
        cat(sprintf("  [NO PLOT] %s: No data survived finite value filtering\n", param_name))
      }
      return(NULL)
    }

    # Ensure method factor levels are in the order they appear in method_names
    available_methods <- intersect(names(methods_data), unique(plot_data$method))
    plot_data$method <- factor(plot_data$method, levels = available_methods)

    # Create subtitle with distribution information
    subtitle_lines <- c()
    for (method_name in available_methods) {
      if (method_name %in% names(method_results)) {
        result <- method_results[[method_name]]
        subtitle_lines <- c(subtitle_lines, paste0(method_name, ": ", result$dist_str))
      }
    }
    subtitle_str <- paste(subtitle_lines, collapse = "\n")

    # Determine x-axis formatting
    x_scale <- if (param_info$category == "initial_conditions" && param_name %in% c("prop_E_initial", "prop_I_initial")) {
      ggplot2::scale_x_continuous(labels = scales::scientific)
    } else if (grepl("beta.*transmission", param_name)) {
      ggplot2::scale_x_continuous(labels = scales::scientific)
    } else {
      ggplot2::scale_x_continuous()
    }

    # Create plot
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y, color = method, fill = method)) +
      ggplot2::geom_area(alpha = 0.25, position = "identity") +  # Slightly lower alpha for better overlap visibility
      ggplot2::geom_line(linewidth = 1.2) +  # Thicker lines for better distinguishability
      ggplot2::scale_color_manual(values = method_colors[available_methods], name = "Method") +
      ggplot2::scale_fill_manual(values = method_colors[available_methods], name = "Method") +
      x_scale +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5),
        plot.subtitle = ggplot2::element_text(size = 9, hjust = 0.5, color = "gray50", face = "italic"),
        axis.title.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_text(size = 9),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "gray90"),
        legend.position = "none"  # Remove legend from individual plots
      ) +
      ggplot2::labs(
        title = paste0(param_info$display_name, " (", param_name, ")"),
        subtitle = subtitle_str,
        x = param_info$units
      )

    # Add mean lines for each method
    for (method_name in names(method_results)) {
      result <- method_results[[method_name]]
      if (!is.null(result$mean_val) && is.finite(result$mean_val) &&
          !param_distributions[[method_name]]$distribution %in% c("uniform")) {
        x_range <- range(plot_data$x, na.rm = TRUE)
        if (result$mean_val >= x_range[1] && result$mean_val <= x_range[2]) {
          p <- p + ggplot2::geom_vline(xintercept = result$mean_val, linetype = "dashed",
                             color = method_colors[method_name], alpha = 0.5)
        }
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
    dplyr::filter(scale == "global") %>%
    dplyr::arrange(order)

  # Check if any method has global parameters
  has_global <- any(sapply(methods_data, function(x) !is.null(x$parameters_global)))

  if (has_global && nrow(global_params_info) > 0) {
    # Organize global parameters by category
    global_categories <- unique(global_params_info$category)
    global_category_plots <- list()

    for (category in global_categories) {
      category_info <- global_params_info %>%
        dplyr::filter(category == .env$category) %>%
        dplyr::arrange(order)

      category_plot_list <- list()

      for (i in 1:nrow(category_info)) {
        param_name <- category_info$parameter_name[i]
        param_info <- category_info[i, ]

        p <- create_multi_method_plot(param_name, param_info, location = NULL)
        if (!is.null(p)) {
          category_plot_list[[param_name]] <- p
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
      first_method_data <- methods_data[[1]]
      if (!is.null(first_method_data$metadata$version)) {
        subtitle_text <- paste0("Version: ", first_method_data$metadata$version)
      }

      # Update title based on number of methods
      title_text <- if (length(methods_data) > 1) {
        paste("Parameter Distributions:", paste(names(methods_data), collapse = " vs "))
      } else {
        paste("Parameter Distributions:", names(methods_data)[1])
      }

      # Create legend if multiple methods
      if (length(methods_data) > 1) {
        # Create a dummy plot just for the legend
        legend_data <- data.frame(
          x = rep(c(0, 1), length(method_colors)),
          y = rep(c(0, 1), length(method_colors)),
          method = factor(rep(names(method_colors), each = 2), levels = names(method_colors))
        )
        legend_plot <- ggplot2::ggplot(legend_data, ggplot2::aes(x = x, y = y, color = method, fill = method)) +
          ggplot2::geom_area(alpha = 0.3) +
          ggplot2::scale_color_manual(values = method_colors, name = "Method") +
          ggplot2::scale_fill_manual(values = method_colors, name = "Method") +
          ggplot2::theme_void() +
          ggplot2::theme(legend.position = "top",
                legend.direction = "horizontal",
                legend.justification = "center",
                legend.text = ggplot2::element_text(size = 12),
                legend.key.size = grid::unit(1.2, "cm"))

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
        # Single method, so no legend needed
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
      heights <- c(0.08)  # Space for title + legend

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

        # Category plots grid - use 2 columns
        n_plots <- length(global_category_plots[[category]])
        ncol <- 2
        nrow <- ceiling(n_plots / ncol)

        category_combined <- cowplot::plot_grid(plotlist = global_category_plots[[category]],
                                              ncol = ncol, nrow = nrow,
                                              align = "hv", axis = "tblr")

        final_plot_list[[length(final_plot_list) + 1]] <- category_heading
        final_plot_list[[length(final_plot_list) + 1]] <- category_combined

        # Adjust height based on number of rows
        section_height <- max(0.15, nrow * 0.12)
        heights <- c(heights, 0.03, section_height)
      }

      # Normalize heights
      heights <- heights / sum(heights)

      # Combine all sections
      p_global <- cowplot::plot_grid(plotlist = final_plot_list,
                                    ncol = 1, rel_heights = heights)

      # Add common y-axis label
      p_global <- cowplot::ggdraw(p_global) +
        cowplot::draw_label("Density", x = 0.02, y = 0.5, angle = 90, size = 12)

      # Calculate appropriate height
      base_height <- 4  # Base height for title and margins
      category_height <- sum(sapply(names(global_category_plots), function(cat_name) {
        cat <- global_category_plots[[cat_name]]
        n_plots <- length(cat)
        nrow <- ceiling(n_plots / 2)  # 2 columns
        return(max(2.8, nrow * 2.8))
      }))
      plot_height <- base_height + category_height

      # Save plot
      filename <- paste0("distributions_global_", paste(names(methods_data), collapse = "_"), ".pdf")
      ggplot2::ggsave(file.path(output_dir, filename), p_global,
             width = 12, height = plot_height, limitsize = FALSE)
      plots$global <- p_global
      cat(paste0("  Saved: ", filename, "\n"))
    }
  }

  # =========================================================================
  # PLOT 2: Location-Specific Parameters (if any locations exist)
  # =========================================================================

  if (!is.null(location_codes) && length(location_codes) > 0) {
    cat("Plotting location-specific distributions organized by category...\n")

    # Get location-specific parameters organized by category
    location_params_info <- estimated_parameters %>%
      dplyr::filter(scale == "location") %>%
      dplyr::arrange(order)

    for (iso in location_codes) {
      # Organize plots by category
      category_plots <- list()
      categories_available <- unique(location_params_info$category)

      for (category in categories_available) {
        category_info <- location_params_info %>%
          dplyr::filter(category == .env$category) %>%
          dplyr::arrange(order)

        category_plot_list <- list()

        for (i in 1:nrow(category_info)) {
          param_name <- category_info$parameter_name[i]
          param_info <- category_info[i, ]

          # Parameter name variants now handled within create_multi_method_plot()
          # No need for external mapping
          p <- create_multi_method_plot(param_name, param_info, location = iso)
          if (!is.null(p)) {
            category_plot_list[[param_name]] <- p
          }
        }

        if (length(category_plot_list) > 0) {
          category_plots[[category]] <- category_plot_list
        }
      }

      # Create combined location plot
      if (length(category_plots) > 0) {
        final_plot_list <- list()

        # Location title
        title_text <- paste0("Location-Specific Parameters: ", iso)
        if (length(methods_data) > 1) {
          title_text <- paste0(title_text, " (", paste(names(methods_data), collapse = " vs "), ")")
        }

        # Create legend if multiple methods
        if (length(methods_data) > 1) {
          # Reuse legend from global plot
          legend_data <- data.frame(
            x = rep(c(0, 1), length(method_colors)),
            y = rep(c(0, 1), length(method_colors)),
            method = factor(rep(names(method_colors), each = 2), levels = names(method_colors))
          )
          legend_plot <- ggplot2::ggplot(legend_data, ggplot2::aes(x = x, y = y, color = method, fill = method)) +
            ggplot2::geom_area(alpha = 0.3) +
            ggplot2::scale_color_manual(values = method_colors, name = "Method") +
            ggplot2::scale_fill_manual(values = method_colors, name = "Method") +
            ggplot2::theme_void() +
            ggplot2::theme(legend.position = "top",
                  legend.direction = "horizontal",
                  legend.justification = "center",
                  legend.text = ggplot2::element_text(size = 12),
                  legend.key.size = grid::unit(1.2, "cm"))

          legend_grob <- cowplot::get_legend(legend_plot)

          title_section <- cowplot::plot_grid(
            cowplot::ggdraw() + cowplot::draw_label(title_text, fontface = 'bold', size = 16),
            legend_grob,
            ncol = 1, rel_heights = c(0.5, 0.5)
          )
          main_title <- title_section
        } else {
          main_title <- cowplot::ggdraw() +
            cowplot::draw_label(title_text, fontface = 'bold', size = 16, x = 0.5, hjust = 0.5)
        }

        final_plot_list[[1]] <- main_title
        heights <- c(0.08)

        # Add each category section
        for (category in names(category_plots)) {
          category_title <- switch(category,
                                 "transmission" = "Transmission Parameters",
                                 "environmental" = "Environmental Parameters",
                                 "disease" = "Disease Parameters",
                                 "immunity" = "Immunity Parameters",
                                 "surveillance" = "Surveillance Parameters",
                                 "mobility" = "Mobility Parameters",
                                 tools::toTitleCase(gsub("_", " ", category)))

          category_heading <- cowplot::ggdraw() +
            cowplot::draw_label(category_title, fontface = 'bold', size = 14, x = 0.5, y = 0.5, hjust = 0.5)

          # Category plots grid
          n_plots <- length(category_plots[[category]])
          ncol <- 2
          nrow <- ceiling(n_plots / ncol)

          category_combined <- cowplot::plot_grid(plotlist = category_plots[[category]],
                                                ncol = ncol, nrow = nrow,
                                                align = "hv", axis = "tblr")

          final_plot_list[[length(final_plot_list) + 1]] <- category_heading
          final_plot_list[[length(final_plot_list) + 1]] <- category_combined

          section_height <- max(0.15, nrow * 0.12)
          heights <- c(heights, 0.03, section_height)
        }

        # Normalize heights
        heights <- heights / sum(heights)

        # Combine all sections
        p_location <- cowplot::plot_grid(plotlist = final_plot_list,
                                        ncol = 1, rel_heights = heights)

        # Add common y-axis label
        p_location <- cowplot::ggdraw(p_location) +
          cowplot::draw_label("Density", x = 0.02, y = 0.5, angle = 90, size = 12)

        # Calculate height
        base_height <- 4
        category_height <- sum(sapply(names(category_plots), function(cat_name) {
          cat <- category_plots[[cat_name]]
          n_plots <- length(cat)
          nrow <- ceiling(n_plots / 2)
          return(max(2.8, nrow * 2.8))
        }))
        plot_height <- base_height + category_height

        # Save plot
        filename <- paste0("distributions_", iso, "_", paste(names(methods_data), collapse = "_"), ".pdf")
        ggplot2::ggsave(file.path(output_dir, filename), p_location,
               width = 12, height = plot_height, limitsize = FALSE)
        plots[[paste0("location_", iso)]] <- p_location
        cat(paste0("  Saved: ", filename, "\n"))
      }
    }
  }

  cat("Plot generation completed!\n")

  # Restore original warning setting
  options(warn = old_warn)

  # Return plot objects invisibly
  invisible(plots)
}