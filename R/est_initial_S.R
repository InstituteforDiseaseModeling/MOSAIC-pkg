#' Estimate Initial S Compartment from Other Compartment Priors
#'
#' @description
#' Estimates the initial proportion of the population in the Susceptible (S) compartment
#' using the constraint S + V1 + V2 + E + I + R = 1. This function samples from the
#' estimated priors of other compartments and calculates S as the constrained residual,
#' ensuring mathematical consistency while propagating uncertainty from all compartments.
#'
#' @param PATHS List containing paths to data directories (for compatibility)
#' @param priors Prior distributions object containing estimated priors for V1, V2, E, I, R
#' @param config Configuration object containing location codes
#' @param n_samples Integer, number of Monte Carlo samples for uncertainty quantification (default 1000)
#' @param t0 Date object, target date for estimation (default NULL, used for metadata)
#' @param variance_inflation Numeric factor to inflate variance of fitted Beta distributions (default 1 = no inflation).
#'   Values > 1 increase uncertainty while preserving the mean. For example, 2 doubles the variance.
#' @param verbose Logical, whether to print progress messages (default TRUE)
#' @param min_S_proportion Numeric, minimum allowed S proportion to prevent negative values (default 0.01 = 1%)
#'
#' @return List with priors_default-compatible structure containing:
#' \describe{
#'   \item{metadata}{Information about the estimation run including summary statistics}
#'   \item{parameters_location}{Parameter hierarchy matching priors_default:
#'     \itemize{
#'       \item prop_S_initial: Beta distribution parameters for S/N by location
#'     }
#'     Each parameter contains $parameters$location$[ISO_CODE] with shape1, shape2, and metadata
#'   }
#' }
#'
#' @details
#' The function implements a constrained residual approach:
#'
#' **Method Overview:**
#' 1. Sample from estimated Beta priors for V1, V2, E, I, R compartments
#' 2. Calculate S as constrained residual: S = 1 - (V1 + V2 + E + I + R)
#' 3. Apply constraints: ensure S ≥ min_S_proportion and handle over-allocation
#' 4. Fit Beta distribution to S samples using CI expansion method
#' 5. Return priors-compatible structure with metadata
#'
#' **Constraint Handling:**
#' - If other compartments sum > (1 - min_S_proportion), proportionally scale them down
#' - Ensures S is always ≥ min_S_proportion for biological realism
#' - Maintains mathematical consistency: all compartments sum to 1
#'
#' **Fallback Behavior:**
#' - If estimated priors unavailable for any compartment, uses default prior parameters
#' - Locations without any estimated compartments use independent S prior (Beta(30, 7.5))
#' - Graceful degradation ensures function always returns valid results
#'
#' **Uncertainty Propagation:**
#' - S uncertainty incorporates uncertainty from all other compartments
#' - Variance inflation applied using CI expansion method consistent with other est_initial_* functions
#' - Countries with high vaccination/immunity naturally get lower and more certain S estimates
#'
#' @examples
#' \dontrun{
#' # After running est_initial_V1_V2, est_initial_E_I, est_initial_R
#' PATHS <- get_paths()
#' priors_updated <- priors_default  # With updated V1,V2,E,I,R compartments
#'
#' initial_S <- est_initial_S(
#'   PATHS = PATHS,
#'   priors = priors_updated,
#'   config = config_default,
#'   n_samples = 1000,
#'   variance_inflation = 2  # Double the variance for less constrained priors
#' )
#'
#' # Access results for a location
#' eth_s_params <- initial_S$parameters_location$prop_S_initial$parameters$location$ETH
#' eth_s_mean <- eth_s_params$metadata$mean
#' }
#'
#' @seealso
#' \code{\link{est_initial_V1_V2}} for vaccination compartment estimation
#' \code{\link{est_initial_E_I}} for infection compartment estimation
#' \code{\link{est_initial_R}} for recovered compartment estimation
#' \code{\link{fit_beta_from_ci}} for Beta distribution fitting with CI constraints
#'
#' @export

est_initial_S <- function(PATHS, priors, config, n_samples = 1000,
                         t0 = NULL, variance_inflation = 0, verbose = TRUE,
                         min_S_proportion = 0.01) {

    # Input validation
    if (!is.list(PATHS)) {
        stop("PATHS must be a list from get_paths()")
    }

    if (!is.list(priors)) {
        stop("priors must be a priors object")
    }

    if (!is.list(config)) {
        stop("config must be a configuration object")
    }

    if (n_samples <= 0) {
        stop("n_samples must be positive")
    }

    if (min_S_proportion <= 0 || min_S_proportion >= 1) {
        stop("min_S_proportion must be in (0, 1)")
    }

    # Extract t0 from config if not provided (for metadata)
    if (is.null(t0)) {
        if (!is.null(config$date_start)) {
            t0 <- as.Date(config$date_start)
        } else {
            t0 <- Sys.Date()
        }
    } else {
        t0 <- as.Date(t0)
    }

    # Get location codes from config
    if (!is.null(config$location_name)) {
        location_codes <- config$location_name
    } else if (!is.null(config$location_codes)) {
        location_codes <- config$location_codes
    } else {
        stop("No location codes found in config (checked location_name and location_codes)")
    }

    if (length(location_codes) == 0) {
        stop("Location list is empty in config")
    }

    if (verbose) {
        cat("=== Estimating Initial S Compartment ===\n")
        cat("Method: Constrained residual from other compartments\n")
        cat("Target date (t0):", as.character(t0), "\n")
        cat("Processing", length(location_codes), "locations with", n_samples, "samples each\n")
        cat("Minimum S proportion:", min_S_proportion, "\n")
        if (variance_inflation != 1) {
            cat("Variance inflation factor:", variance_inflation, "\n")
        }
        cat("\n")
    }


    # Helper function to fit Beta with variance inflation for S compartment
    fit_beta_with_variance_inflation_S <- function(samples, variance_inflation, label = "") {
        # Remove invalid samples
        valid_samples <- samples[!is.na(samples) & samples > 0 & samples < 1]

        if (length(valid_samples) < 2) {
            if (verbose) cat("  Insufficient S samples for", label, "- using default\n")
            return(list(shape1 = 30, shape2 = 7.5, method = "insufficient_data"))
        }

        # Calculate sample statistics
        sample_mean <- mean(valid_samples)
        sample_quantiles <- quantile(valid_samples, c(0.025, 0.975))
        ci_lower <- sample_quantiles[1]
        ci_upper <- sample_quantiles[2]

        # Apply variance inflation using the new pattern
        ci_lower <- pmax(0.001, ci_lower*(1-variance_inflation))
        ci_upper <- pmin(0.999, ci_upper*(1+variance_inflation))

        # Use fit_beta_from_ci with sample_mean as mode_val
        tryCatch({
            beta_fit <- fit_beta_from_ci(
                mode_val = sample_mean,
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                method = "moment_matching"
            )

            return(list(
                shape1 = beta_fit$shape1,
                shape2 = beta_fit$shape2,
                method = "ci_expansion_constrained_residual"
            ))

        }, error = function(e) {
            # Simple fallback using method of moments
            sample_var <- var(valid_samples)
            precision <- (sample_mean * (1 - sample_mean) / sample_var) - 1
            precision <- max(2.1, precision)

            return(list(
                shape1 = max(1.01, sample_mean * precision),
                shape2 = max(1.01, (1 - sample_mean) * precision),
                method = "fallback_method_of_moments"
            ))
        })
    }

    # Default parameters for each compartment (fallbacks)
    defaults <- list(
        V1 = list(shape1 = 0.5, shape2 = 49.5),    # ~1% mean
        V2 = list(shape1 = 0.5, shape2 = 99.5),    # ~0.5% mean
        E = list(shape1 = 0.01, shape2 = 9999.99), # ~0.0001% mean
        I = list(shape1 = 0.01, shape2 = 9999.99), # ~0.0001% mean
        R = list(shape1 = 3.5, shape2 = 14)        # ~20% mean
    )

    # Initialize results storage
    results_list <- list()

    # Process each location
    if (verbose) {
        cat("Processing locations:\n")
        pb <- txtProgressBar(min = 0, max = length(location_codes), style = 3)
    }

    for (i in seq_along(location_codes)) {
        loc <- location_codes[i]

        if (verbose) {
            setTxtProgressBar(pb, i)
        }

        # Get prior parameters for this location (with fallbacks)
        V1_params <- priors$parameters_location$prop_V1_initial$parameters$location[[loc]]
        V2_params <- priors$parameters_location$prop_V2_initial$parameters$location[[loc]]
        E_params <- priors$parameters_location$prop_E_initial$parameters$location[[loc]]
        I_params <- priors$parameters_location$prop_I_initial$parameters$location[[loc]]
        R_params <- priors$parameters_location$prop_R_initial$parameters$location[[loc]]

        # Check if we have any estimated compartments for this location
        has_estimates <- (!is.null(V1_params) && !is.null(V1_params$shape1) && !is.na(V1_params$shape1)) ||
                        (!is.null(V2_params) && !is.null(V2_params$shape1) && !is.na(V2_params$shape1)) ||
                        (!is.null(E_params) && !is.null(E_params$shape1) && !is.na(E_params$shape1)) ||
                        (!is.null(I_params) && !is.null(I_params$shape1) && !is.na(I_params$shape1)) ||
                        (!is.null(R_params) && !is.null(R_params$shape1) && !is.na(R_params$shape1))

        if (!has_estimates) {
            # No estimates available - use independent S prior
            if (verbose && i == 1) {  # Only warn once
                cat("\n  Note: Using independent S priors for locations without compartment estimates\n")
            }

            results_list[[loc]] <- list(
                shape1 = 30,
                shape2 = 7.5,
                method = "independent_prior_no_estimates",
                metadata = list(
                    estimated_from_constraints = FALSE,
                    mean = 30 / (30 + 7.5),
                    message = "No other compartment estimates available"
                )
            )
            next
        }

        # Create proper prior structures (using estimates or defaults)
        # Helper to build beta prior with fallback
        build_prior <- function(params, default) {
            if (!is.null(params) && !is.null(params$shape1) && !is.null(params$shape2) && 
                !is.na(params$shape1) && !is.na(params$shape2)) {
                list(distribution = "beta", parameters = list(shape1 = params$shape1, shape2 = params$shape2))
            } else {
                list(distribution = "beta", parameters = list(shape1 = default$shape1, shape2 = default$shape2))
            }
        }
        
        # Build priors once before the loop
        V1_prior <- build_prior(V1_params, defaults$V1)
        V2_prior <- build_prior(V2_params, defaults$V2)
        E_prior <- build_prior(E_params, defaults$E)
        I_prior <- build_prior(I_params, defaults$I)
        R_prior <- build_prior(R_params, defaults$R)
        
        # Monte Carlo sampling with constraint enforcement
        S_samples <- numeric(n_samples)
        constraint_violations <- 0

        for (j in 1:n_samples) {
            # Sample from each compartment using unified sample_from_prior
            V1_j <- sample_from_prior(n = 1, prior = V1_prior, verbose = FALSE)
            V2_j <- sample_from_prior(n = 1, prior = V2_prior, verbose = FALSE)
            E_j <- sample_from_prior(n = 1, prior = E_prior, verbose = FALSE)
            I_j <- sample_from_prior(n = 1, prior = I_prior, verbose = FALSE)
            R_j <- sample_from_prior(n = 1, prior = R_prior, verbose = FALSE)

            # Calculate sum of other compartments
            other_sum <- V1_j + V2_j + E_j + I_j + R_j

            # Handle constraint violations
            if (other_sum > (1 - min_S_proportion)) {
                constraint_violations <- constraint_violations + 1

                # Proportionally scale down other compartments
                scale_factor <- (1 - min_S_proportion) / other_sum
                V1_j <- V1_j * scale_factor
                V2_j <- V2_j * scale_factor
                E_j <- E_j * scale_factor
                I_j <- I_j * scale_factor
                R_j <- R_j * scale_factor

                # Recalculate S
                S_samples[j] <- min_S_proportion
            } else {
                # Normal case: S as residual
                S_samples[j] <- 1 - other_sum
            }

            # Ensure S is within bounds
            S_samples[j] <- max(min_S_proportion, min(0.99, S_samples[j]))
        }

        # Fit Beta distribution to S samples
        S_beta <- fit_beta_with_variance_inflation_S(
            samples = S_samples,
            variance_inflation = variance_inflation,
            label = paste0("S_", loc)
        )

        # Calculate summary statistics
        S_mean <- mean(S_samples)
        S_ci <- quantile(S_samples, c(0.025, 0.975))

        # Store results
        results_list[[loc]] <- list(
            shape1 = S_beta$shape1,
            shape2 = S_beta$shape2,
            method = S_beta$method,
            metadata = list(
                estimated_from_constraints = TRUE,
                mean = S_mean,
                ci_lower = S_ci[1],
                ci_upper = S_ci[2],
                constraint_violations = constraint_violations,
                constraint_violation_rate = constraint_violations / n_samples,
                t0 = as.character(t0),
                n_samples = n_samples,
                min_S_proportion = min_S_proportion,
                variance_inflation = variance_inflation,
                message = sprintf("Estimated from %d samples with %.1f%% constraint violations",
                                n_samples, 100 * constraint_violations / n_samples)
            )
        )
    }

    if (verbose) {
        close(pb)
        cat("\n")
    }

    # Format output to match priors_default structure
    output <- list(
        metadata = list(
            description = "Initial S susceptible compartment estimates from constrained residual",
            version = "1.0.0",
            date = as.character(Sys.Date()),
            initial_conditions_S = list(
                estimated_date = as.character(Sys.Date()),
                method = "constrained_residual",
                constraint = "S + V1 + V2 + E + I + R = 1",
                n_samples = n_samples,
                t0 = as.character(t0),
                min_S_proportion = min_S_proportion,
                n_locations_processed = length(results_list),
                locations_processed = names(results_list),
                compartments_used = c("V1", "V2", "E", "I", "R")
            )
        ),

        parameters_location = list(
            prop_S_initial = list(
                parameter_name = "prop_S_initial",
                description = "Initial proportion in susceptible (S) compartment from constrained residual",
                distribution = "beta",
                parameters = list(
                    location = results_list
                )
            )
        )
    )

    class(output) <- c("mosaic_initial_conditions", "list")

    if (verbose) {
        cat("=== S Compartment Estimation Complete ===\n")
        cat(sprintf("Processed %d locations\n", length(results_list)))

        # Summary statistics
        constrained_locs <- sum(sapply(results_list, function(x) x$metadata$estimated_from_constraints))
        independent_locs <- length(results_list) - constrained_locs

        cat(sprintf("Constrained estimates: %d locations\n", constrained_locs))
        cat(sprintf("Independent priors: %d locations\n", independent_locs))

        # Show detailed results for all locations
        cat("\n=== Compartment Proportions Summary ===\n")
        cat(sprintf("%-4s %-8s %-8s %-8s %-8s %-8s %-8s %-8s %-10s\n",
                    "LOC", "S(%)", "V1(%)", "V2(%)", "E(%)", "I(%)", "R(%)", "Total(%)", "Violations(%)"))
        cat(paste(rep("-", 85), collapse=""), "\n")

        for (loc in names(results_list)) {
            # Get S estimate
            S_result <- results_list[[loc]]
            S_mean <- if (!is.null(S_result$metadata$mean)) S_result$metadata$mean else 0

            # Get other compartment means (from priors used in calculation)
            V1_mean <- 0
            V2_mean <- 0
            E_mean <- 0
            I_mean <- 0
            R_mean <- 0

            # Extract means from priors if available
            if (!is.null(priors$parameters_location$prop_V1_initial$parameters$location[[loc]]$metadata$mean)) {
                V1_mean <- priors$parameters_location$prop_V1_initial$parameters$location[[loc]]$metadata$mean
            } else if (!is.null(priors$parameters_location$prop_V1_initial$parameters$location[[loc]])) {
                # Calculate from Beta parameters if metadata not available
                V1_params <- priors$parameters_location$prop_V1_initial$parameters$location[[loc]]
                if (!is.na(V1_params$shape1)) {
                    V1_mean <- V1_params$shape1 / (V1_params$shape1 + V1_params$shape2)
                }
            }

            if (!is.null(priors$parameters_location$prop_V2_initial$parameters$location[[loc]]$metadata$mean)) {
                V2_mean <- priors$parameters_location$prop_V2_initial$parameters$location[[loc]]$metadata$mean
            } else if (!is.null(priors$parameters_location$prop_V2_initial$parameters$location[[loc]])) {
                V2_params <- priors$parameters_location$prop_V2_initial$parameters$location[[loc]]
                if (!is.na(V2_params$shape1)) {
                    V2_mean <- V2_params$shape1 / (V2_params$shape1 + V2_params$shape2)
                }
            }

            if (!is.null(priors$parameters_location$prop_E_initial$parameters$location[[loc]]$metadata$mean)) {
                E_mean <- priors$parameters_location$prop_E_initial$parameters$location[[loc]]$metadata$mean
            } else if (!is.null(priors$parameters_location$prop_E_initial$parameters$location[[loc]])) {
                E_params <- priors$parameters_location$prop_E_initial$parameters$location[[loc]]
                if (!is.na(E_params$shape1)) {
                    E_mean <- E_params$shape1 / (E_params$shape1 + E_params$shape2)
                }
            }

            if (!is.null(priors$parameters_location$prop_I_initial$parameters$location[[loc]]$metadata$mean)) {
                I_mean <- priors$parameters_location$prop_I_initial$parameters$location[[loc]]$metadata$mean
            } else if (!is.null(priors$parameters_location$prop_I_initial$parameters$location[[loc]])) {
                I_params <- priors$parameters_location$prop_I_initial$parameters$location[[loc]]
                if (!is.na(I_params$shape1)) {
                    I_mean <- I_params$shape1 / (I_params$shape1 + I_params$shape2)
                }
            }

            if (!is.null(priors$parameters_location$prop_R_initial$parameters$location[[loc]]$metadata$mean)) {
                R_mean <- priors$parameters_location$prop_R_initial$parameters$location[[loc]]$metadata$mean
            } else if (!is.null(priors$parameters_location$prop_R_initial$parameters$location[[loc]])) {
                R_params <- priors$parameters_location$prop_R_initial$parameters$location[[loc]]
                if (!is.na(R_params$shape1)) {
                    R_mean <- R_params$shape1 / (R_params$shape1 + R_params$shape2)
                }
            }

            # Calculate total
            total_mean <- S_mean + V1_mean + V2_mean + E_mean + I_mean + R_mean

            # Get violation rate
            violation_rate <- if (!is.null(S_result$metadata$constraint_violation_rate)) {
                S_result$metadata$constraint_violation_rate * 100
            } else { 0 }

            # Format output - use different precision for very small values
            S_str <- sprintf("%.1f", S_mean * 100)
            V1_str <- sprintf("%.1f", V1_mean * 100)
            V2_str <- sprintf("%.1f", V2_mean * 100)
            E_str <- if (E_mean < 0.001) sprintf("%.3f", E_mean * 100) else sprintf("%.1f", E_mean * 100)
            I_str <- if (I_mean < 0.001) sprintf("%.3f", I_mean * 100) else sprintf("%.1f", I_mean * 100)
            R_str <- sprintf("%.1f", R_mean * 100)
            Total_str <- sprintf("%.1f", total_mean * 100)
            Viol_str <- sprintf("%.1f", violation_rate)

            cat(sprintf("%-4s %-8s %-8s %-8s %-8s %-8s %-8s %-8s %-10s\n",
                       loc, S_str, V1_str, V2_str, E_str, I_str, R_str, Total_str, Viol_str))
        }

        cat("\nNote: E and I shown with 3 decimal places if < 0.1%\n")
        cat("Violations = % of samples requiring constraint enforcement\n")
        cat("\n")

        # Additional summary statistics
        if (constrained_locs > 0) {
            # Calculate overall statistics
            all_S_means <- sapply(results_list[sapply(results_list, function(x) x$metadata$estimated_from_constraints)],
                                 function(x) x$metadata$mean)
            all_violations <- sapply(results_list[sapply(results_list, function(x) x$metadata$estimated_from_constraints)],
                                   function(x) x$metadata$constraint_violation_rate)

            cat("=== Summary Statistics ===\n")
            cat(sprintf("S compartment: mean = %.1f%%, range = %.1f%% - %.1f%%\n",
                       mean(all_S_means) * 100, min(all_S_means) * 100, max(all_S_means) * 100))
            cat(sprintf("Constraint violations: mean = %.1f%%, max = %.1f%%\n",
                       mean(all_violations) * 100, max(all_violations) * 100))
        }
        cat("\n")
    }

    return(output)
}

#' Print method for S compartment initial conditions
#' @export
print.mosaic_initial_conditions_S <- function(x, ...) {
    cat("MOSAIC Initial S Conditions (Constrained Residual Method)\n")
    cat("=========================================================\n")
    cat("Method:", x$metadata$initial_conditions_S$method, "\n")
    cat("Constraint:", x$metadata$initial_conditions_S$constraint, "\n")
    cat("t0:", x$metadata$initial_conditions_S$t0, "\n")
    cat("Locations processed:", x$metadata$initial_conditions_S$n_locations_processed, "\n")
    cat("Samples per location:", x$metadata$initial_conditions_S$n_samples, "\n\n")

    # Summary table for locations with constrained estimates
    locs <- names(x$parameters_location$prop_S_initial$parameters$location)
    constrained_locs <- locs[sapply(x$parameters_location$prop_S_initial$parameters$location,
                                  function(y) y$metadata$estimated_from_constraints)]

    if (length(constrained_locs) > 0) {
        cat("Constrained S estimates (showing first 10):\n")
        show_locs <- head(constrained_locs, 10)

        summary_data <- lapply(show_locs, function(loc) {
            loc_data <- x$parameters_location$prop_S_initial$parameters$location[[loc]]
            data.frame(
                Location = loc,
                S_mean_pct = round(loc_data$metadata$mean * 100, 1),
                CI_lower_pct = round(loc_data$metadata$ci_lower * 100, 1),
                CI_upper_pct = round(loc_data$metadata$ci_upper * 100, 1),
                Violations_pct = round(loc_data$metadata$constraint_violation_rate * 100, 1)
            )
        })

        summary_df <- do.call(rbind, summary_data)
        print(summary_df, row.names = FALSE)
    }
}
