#' Calculate Parameter-Specific ESS using Kernel Density Estimation
#'
#' Computes the effective sample size (ESS) for individual parameters using
#' kernel density estimation to approximate marginal posteriors. This method
#' integrates out other parameters to provide a true measure of information
#' content for each parameter individually.
#'
#' @param results Data frame containing simulation results
#' @param param_names Character vector of parameter names to analyze (required)
#' @param likelihood_col Character name of the column containing log-likelihood values (default: "likelihood")
#' @param n_grid Integer number of grid points for KDE evaluation (default: 100)
#' @param method Character string specifying ESS calculation method: "kish" (default) or "perplexity"
#' @param verbose Logical whether to print progress messages (default: FALSE)
#'
#' @return Data frame with columns:
#'   - parameter: Parameter name
#'   - type: Parameter type ('global' or 'location')
#'   - iso_code: ISO code for location parameters (NA for global)
#'   - ess_marginal: Marginal ESS for this parameter
#'
#' @examples
#' \dontrun{
#' # Basic usage with simulation results
#' ess_results <- calc_model_ess_parameter(
#'   results = simulation_results,
#'   param_names = c("tau_i", "mu_j", "gamma_2")
#' )
#'
#' # With custom likelihood column name
#' ess_results <- calc_model_ess_parameter(
#'   results = simulation_results,
#'   param_names = c("tau_i", "mu_j", "gamma_2"),
#'   likelihood_col = "log_lik"
#' )
#'
#' }
#'
#' @export
calc_model_ess_parameter <- function(
    results,
    param_names,
    likelihood_col = "likelihood",
    n_grid = 100,
    method = c("kish", "perplexity"),
    verbose = FALSE
) {

    # Validate method parameter
    method <- match.arg(method)
    
    # ==========================================================================
    # Input validation
    # ==========================================================================
    
    if (!is.data.frame(results)) {
        stop("results must be a data frame")
    }

    if (!is.character(likelihood_col) || length(likelihood_col) != 1) {
        stop("likelihood_col must be a single character string")
    }

    if (!likelihood_col %in% colnames(results)) {
        stop(sprintf("Column '%s' not found in results", likelihood_col))
    }
    
    # Validate input parameters
    if (missing(param_names) || is.null(param_names)) {
        stop("param_names is required. Please specify which parameters to analyze.")
    }

    if (!is.character(param_names)) {
        stop("param_names must be a character vector")
    }

    if (length(param_names) == 0) {
        stop("param_names cannot be empty")
    }

    if (!is.numeric(n_grid) || n_grid < 10) {
        stop("n_grid must be a numeric value >= 10")
    }

    if (!is.logical(verbose)) {
        stop("verbose must be logical (TRUE/FALSE)")
    }

    # Filter to valid rows
    valid_rows <- !is.na(results[[likelihood_col]]) & is.finite(results[[likelihood_col]])
    n_valid <- sum(valid_rows)

    # Require minimum of 50 samples for reasonable KDE
    if (n_valid < 50) {
        stop(sprintf("Insufficient valid samples: %d (need at least 50 for ESS calculation)",
                     n_valid))
    }

    # Adapt n_grid to sample size if necessary
    # Rule of thumb: n_grid should be at most n_samples / 2
    n_grid_adaptive <- min(n_grid, floor(n_valid / 2))
    if (n_grid_adaptive < n_grid && verbose) {
        log_msg("Adapting n_grid from %.0f to %.0f based on sample size (%d samples)",
                n_grid, n_grid_adaptive, n_valid)
    }
    n_grid <- n_grid_adaptive

    results_valid <- results[valid_rows, ]
    n_samples <- nrow(results_valid)
    
    if (verbose) {
        log_msg("Calculating parameter-specific ESS for %d valid samples", n_samples)
    }
    
    # ==========================================================================
    # Validate parameters exist
    # ==========================================================================

    # Validate parameter names exist
    missing_params <- setdiff(param_names, colnames(results_valid))
    if (length(missing_params) > 0) {
        stop(sprintf("Parameters not found in results: %s", paste(missing_params, collapse = ", ")))
    }

    # Check that parameters are numeric
    non_numeric_params <- param_names[!sapply(results_valid[param_names], is.numeric)]
    if (length(non_numeric_params) > 0) {
        stop(sprintf("Parameters must be numeric. Non-numeric parameters found: %s",
                     paste(non_numeric_params, collapse = ", ")))
    }

    # Filter to only include estimated parameters from MOSAIC framework
    est_params <- get("estimated_parameters", envir = asNamespace("MOSAIC"))

    # Get base parameter names (without ISO suffixes) from param_names
    base_params <- unique(gsub("_[A-Z]{3}$", "", param_names))

    # Keep only parameters that are in estimated_parameters
    valid_base_params <- base_params[base_params %in% est_params$parameter_name]

    # Filter param_names to only those with valid base parameters
    param_names_filtered <- param_names[gsub("_[A-Z]{3}$", "", param_names) %in% valid_base_params]

    # Exclude initial count variables (N_j_initial, S_j_initial, etc.) but keep proportions
    # These are compartment counts, not estimated parameters
    param_names_filtered <- param_names_filtered[!grepl("^[NSEIRV][12]?_j_initial", param_names_filtered)]

    if (length(param_names_filtered) == 0) {
        stop("No valid estimated parameters found in param_names. Check that parameters match those in MOSAIC::estimated_parameters")
    }

    if (length(param_names_filtered) < length(param_names)) {
        excluded <- setdiff(param_names, param_names_filtered)
        if (verbose) {
            log_msg("Excluded %d non-estimated parameters: %s",
                    length(excluded),
                    if(length(excluded) <= 5) paste(excluded, collapse = ", ")
                    else paste(c(head(excluded, 5), "..."), collapse = ", "))
        }
    }

    # Use filtered parameter names
    param_names <- param_names_filtered

    
    # ==========================================================================
    # Calculate global ESS for reference
    # ==========================================================================

    log_lik <- results_valid[[likelihood_col]]
    log_lik_centered <- log_lik - max(log_lik)  # For numerical stability
    
    # Global importance weights
    w_global <- exp(log_lik_centered)
    w_global <- w_global / sum(w_global)

    # Global ESS using specified method
    ess_global <- calc_model_ess(w_global, method = method)
    
    if (verbose) {
        log_msg("Global ESS: %.1f (%.1f%% of samples)",
                ess_global, 100 * ess_global / n_samples)
    }
    
    # ==========================================================================
    # Helper function for log-sum-exp
    # ==========================================================================
    
    log_sum_exp <- function(x) {
        if (length(x) == 0) return(-Inf)
        max_x <- max(x)
        if (is.infinite(max_x)) return(max_x)
        return(max_x + log(sum(exp(x - max_x))))
    }
    
    # ==========================================================================
    # Helper function for bandwidth selection
    # ==========================================================================

    select_bandwidth <- function(x) {
        n <- length(x)
        sd_x <- sd(x)
        # Silverman's rule of thumb
        iqr_x <- IQR(x)
        return(0.9 * min(sd_x, iqr_x/1.34) * n^(-1/5))
    }
    
    
    # ==========================================================================
    # Function to calculate marginal ESS for one parameter
    # ==========================================================================
    
    calc_marginal_ess <- function(param_name) {

        # Determine parameter type and ISO code
        # Extract base parameter name (without ISO suffix)
        base_param <- gsub("_[A-Z]{3}$", "", param_name)

        # Check if this is a location-specific parameter
        param_type <- "global"  # default
        iso_code <- NA_character_

        # Check if parameter has ISO suffix
        if (grepl("_[A-Z]{3}$", param_name)) {
            # Extract ISO code
            iso_code <- gsub(".*_([A-Z]{3})$", "\\1", param_name)

            # Load estimated_parameters from MOSAIC package
            est_params <- get("estimated_parameters", envir = asNamespace("MOSAIC"))

            # Check if base parameter is in location scale
            if (base_param %in% est_params$parameter_name) {
                param_scale <- est_params$scale[est_params$parameter_name == base_param]
                if (length(param_scale) > 0 && param_scale == "location") {
                    param_type <- "location"
                }
            }
        }

        # Extract parameter values
        param_vals <- results_valid[[param_name]]
        
        # Remove any NA or infinite values
        valid_param <- is.finite(param_vals)
        min_param_samples <- max(50, n_grid)  # Need at least as many samples as grid points

        if (sum(valid_param) < min_param_samples) {
            warning(sprintf("Parameter '%s' has only %d valid samples (need >= %d). Returning NA.",
                          param_name, sum(valid_param), min_param_samples))
            return(data.frame(
                parameter = param_name,
                type = param_type,
                iso_code = iso_code,
                ess_marginal = NA_real_,
                stringsAsFactors = FALSE
            ))
        }
        
        param_vals_clean <- param_vals[valid_param]
        log_lik_clean <- log_lik[valid_param]
        weights_clean <- w_global[valid_param]
        weights_clean <- weights_clean / sum(weights_clean)  # Renormalize

        # Check if parameter has near-zero variance (constant)
        param_range <- range(param_vals_clean)
        if (diff(param_range) < .Machine$double.eps * 100) {
            # Parameter is essentially constant, ESS = n_obs
            return(data.frame(
                parameter = param_name,
                type = param_type,
                iso_code = iso_code,
                ess_marginal = length(param_vals_clean),
                stringsAsFactors = FALSE
            ))
        }

        # Select bandwidth
        h <- select_bandwidth(param_vals_clean)

        # Avoid too small bandwidth
        h <- max(h, diff(param_range) / 100)

        # Create evaluation grid
        param_grid <- seq(param_range[1], param_range[2], length.out = n_grid)
        
        # =======================================================================
        # Compute conditional likelihood at each grid point
        # =======================================================================
        
        cond_log_lik <- sapply(param_grid, function(x) {
            # Gaussian kernel weights
            kern_weights <- dnorm((param_vals_clean - x) / h)

            # Check for NA or all zeros
            if (any(is.na(kern_weights)) || sum(kern_weights) < .Machine$double.eps) {
                return(-Inf)
            }
            
            # Normalize kernel weights
            kern_weights <- kern_weights / sum(kern_weights)
            
            # Weighted log-likelihood using log-sum-exp for stability
            log_weights <- log(kern_weights + .Machine$double.xmin)
            return(log_sum_exp(log_lik_clean + log_weights))
        })
        
        # =======================================================================
        # Approximate marginal posterior via weighted KDE
        # =======================================================================
        
        # Use R's density function with likelihood weights
        posterior_kde <- tryCatch({
            density(
                x = param_vals_clean,
                weights = weights_clean,
                from = param_range[1],
                to = param_range[2],
                n = n_grid,
                bw = h
            )
        }, error = function(e) {
            # Fallback to unweighted KDE if weighted fails
            density(
                x = param_vals_clean,
                from = param_range[1],
                to = param_range[2],
                n = n_grid,
                bw = h
            )
        })
        
        # =======================================================================
        # Calculate prior density on grid (uniform)
        # =======================================================================

        prior_density <- rep(1 / (max(param_grid) - min(param_grid)), length(param_grid))
        
        # =======================================================================
        # Compute importance weights and marginal ESS
        # =======================================================================
        
        # Marginal importance weights
        posterior_density <- posterior_kde$y
        min_density <- .Machine$double.eps * 100  # Small but stable minimum
        posterior_density[posterior_density < min_density] <- min_density

        # Weight ratio
        weight_ratio <- posterior_density / (prior_density + min_density)
        
        # Normalize
        w_marginal <- weight_ratio / sum(weight_ratio)

        # Calculate marginal ESS using specified method
        ess_marginal_raw <- calc_model_ess(w_marginal, method = method)

        # Scale back to sample size
        # The grid-based ESS needs to be scaled by the ratio of samples to grid points
        ess_marginal <- ess_marginal_raw * (length(param_vals_clean) / n_grid)
        
        # Ensure ESS doesn't exceed number of samples
        ess_marginal <- min(ess_marginal, length(param_vals_clean))
        
        return(data.frame(
            parameter = param_name,
            type = param_type,
            iso_code = iso_code,
            ess_marginal = ess_marginal,
            stringsAsFactors = FALSE
        ))
    }
    
    # ==========================================================================
    # Process all parameters
    # ==========================================================================

    if (verbose) {
        log_msg("Processing %d parameters", length(param_names))
    }

    ess_list <- lapply(param_names, function(param) {
        if (verbose && length(param_names) > 10) {
            log_msg("  Processing: %s", param)
        }
        calc_marginal_ess(param)
    })
    
    # Combine results
    ess_results <- do.call(rbind, ess_list)

    # Ensure data frame consistency
    ess_results <- as.data.frame(ess_results, stringsAsFactors = FALSE)
    
    # ==========================================================================
    # Summary statistics
    # ==========================================================================
    
    if (verbose) {
        mean_ess <- mean(ess_results$ess_marginal, na.rm = TRUE)
        median_ess <- median(ess_results$ess_marginal, na.rm = TRUE)

        log_msg(paste(rep("=", 60), collapse = ""))
        log_msg("PARAMETER-SPECIFIC ESS SUMMARY")
        log_msg(paste(rep("=", 60), collapse = ""))
        log_msg("Parameters analyzed: %d", nrow(ess_results))
        log_msg("Mean marginal ESS: %.1f", mean_ess)
        log_msg("Median marginal ESS: %.1f", median_ess)
        log_msg("Global ESS: %.1f", ess_global)
        
        # Show worst parameters (excluding NA values)
        valid_ess <- !is.na(ess_results$ess_marginal)
        if (sum(valid_ess) > 0) {
            worst_params <- head(ess_results[valid_ess, ][order(ess_results$ess_marginal[valid_ess]), ],
                               min(5, sum(valid_ess)))
            if (nrow(worst_params) > 0) {
                log_msg("Lowest ESS parameters:")
                for (i in 1:nrow(worst_params)) {
                    log_msg("  %s: ESS = %.1f",
                            worst_params$parameter[i],
                            worst_params$ess_marginal[i])
                }
            }
        }

        # Report any failed parameters
        failed_params <- ess_results$parameter[!valid_ess]
        if (length(failed_params) > 0) {
            log_msg("Warning: %d parameter(s) failed ESS calculation: %s",
                    length(failed_params),
                    paste(failed_params, collapse = ", "))
        }
        
        log_msg(paste(rep("=", 60), collapse = ""))
    }
    
    return(ess_results)
}