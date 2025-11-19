#' Get NPE Parameter Bounds
#'
#' Extracts mathematical bounds for NPE training from prior distributions,
#' applying universal constraints based on parameter type.
#'
#' @param param_names Character vector of parameter names
#' @param priors_file Path to priors.json file (optional)
#' @param safety_buffer Numeric buffer for numerical stability (default: 1e-10)
#' @param verbose Logical whether to print diagnostics
#' @return Data frame with columns: parameter, min, max
#' @export
get_npe_parameter_bounds <- function(
    param_names,
    priors_file = NULL,
    safety_buffer = 1e-10,
    verbose = FALSE
) {

    if (verbose) {
        log_msg("Extracting NPE parameter bounds for %d parameters", length(param_names))
    }

    # Initialize with infinite bounds
    bounds <- data.frame(
        parameter = param_names,
        min = -Inf,
        max = Inf,
        stringsAsFactors = FALSE
    )

    # Load priors if provided
    priors <- NULL
    if (!is.null(priors_file) && file.exists(priors_file)) {
        priors <- jsonlite::read_json(priors_file)
        if (verbose) {
            log_msg("  Loaded priors from: %s", basename(priors_file))
        }
    } else if (verbose) {
        log_msg("  No priors file, using parameter-based rules only")
    }

    for (i in seq_along(param_names)) {
        param <- param_names[i]
        base_param <- gsub("_[A-Z]{3}$", "", param)

        # Get distribution info from priors if available
        dist_info <- NULL
        if (!is.null(priors)) {
            dist_info <- .get_prior_distribution_info(param, priors)
        }

        # Apply mathematical bounds based on distribution type
        if (!is.null(dist_info)) {
            bounds[i, c("min", "max")] <- switch(dist_info$distribution,
                "beta" = c(0, 1),
                "gamma" = c(0, Inf),
                "uniform" = c(dist_info$parameters$min, dist_info$parameters$max),
                "normal" = c(-Inf, Inf),
                "truncnorm" = c(dist_info$parameters$a, dist_info$parameters$b),
                "lognormal" = c(safety_buffer, Inf),
                c(-Inf, Inf)  # Default
            )
        }

        # Override with parameter-specific mathematical constraints
        # These take precedence over distribution bounds

        # Alpha parameters: [0.05, 0.99] for numerical stability
        if (base_param %in% c("alpha_1", "alpha_2")) {
            bounds$min[i] <- 0.05
            bounds$max[i] <- 0.99
        }

        # Proportions and probabilities: [0, 1]
        else if (grepl("^(prop_|phi_|rho|sigma|p_)", base_param)) {
            bounds$min[i] <- 0
            bounds$max[i] <- 1
        }

        # Rates must be non-negative
        else if (grepl("(rate|tau_i|gamma|omega|epsilon|mu_j|decay)", base_param)) {
            bounds$min[i] <- max(0, bounds$min[i])
        }

        # Counts must be non-negative
        else if (grepl("_j_initial_", base_param)) {
            bounds$min[i] <- 0
        }

        # Environmental parameters must be positive
        else if (base_param %in% c("zeta_1", "zeta_2", "kappa")) {
            bounds$min[i] <- max(safety_buffer, bounds$min[i])
        }

        # Apply safety buffer to infinite bounds
        if (is.infinite(bounds$min[i]) && bounds$min[i] < 0) {
            bounds$min[i] <- -1e6  # Large but finite
        }
        if (is.infinite(bounds$max[i]) && bounds$max[i] > 0) {
            bounds$max[i] <- 1e6   # Large but finite
        }
    }

    # Validate bounds
    invalid <- bounds$min >= bounds$max
    if (any(invalid)) {
        warning(sprintf("Invalid bounds for parameters: %s",
                       paste(param_names[invalid], collapse = ", ")))
        # Fix by adding small range
        bounds$max[invalid] <- bounds$min[invalid] + 1
    }

    if (verbose) {
        log_msg("  Bounds set for %d parameters", nrow(bounds))

        # Report bound types
        n_finite <- sum(is.finite(bounds$min) & is.finite(bounds$max))
        n_semi <- sum(is.finite(bounds$min) | is.finite(bounds$max)) - n_finite
        n_inf <- nrow(bounds) - n_finite - n_semi

        log_msg("  Finite bounds: %d, Semi-infinite: %d, Infinite: %d",
                n_finite, n_semi, n_inf)
    }

    return(bounds)
}

#' Helper function to extract distribution info from priors
#'
#' @param param_name Parameter name
#' @param priors Priors list from JSON
#' @return Distribution information or NULL
#' @keywords internal
.get_prior_distribution_info <- function(param_name, priors) {
    # Check if location-specific
    if (grepl("_[A-Z]{3}$", param_name)) {
        base_name <- gsub("_[A-Z]{3}$", "", param_name)
        location <- regmatches(param_name, regexpr("[A-Z]{3}$", param_name))

        # Handle seasonality parameter naming mismatch
        # Calibration uses: a_1_j, a_2_j, b_1_j, b_2_j
        # Priors use: a1, a2, b1, b2
        if (base_name == "a_1_j") {
            base_name <- "a1"
        } else if (base_name == "a_2_j") {
            base_name <- "a2"
        } else if (base_name == "b_1_j") {
            base_name <- "b1"
        } else if (base_name == "b_2_j") {
            base_name <- "b2"
        }

        # Try location-specific
        if (!is.null(priors$parameters_location[[base_name]]$location[[location]])) {
            return(priors$parameters_location[[base_name]]$location[[location]])
        }
    }

    # Try global
    base_name <- gsub("_[A-Z]{3}$", "", param_name)
    if (!is.null(priors$parameters_global[[base_name]])) {
        return(priors$parameters_global[[base_name]])
    }

    return(NULL)
}