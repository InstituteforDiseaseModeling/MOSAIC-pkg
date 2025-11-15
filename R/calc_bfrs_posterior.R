#' Calculate BFRS Posterior via Importance Sampling
#'
#' Creates a posterior approximation from BFRS simulation results using
#' importance sampling with likelihood weights.
#'
#' @param results Data frame containing simulation results with parameters and likelihoods
#' @param param_names Character vector of parameter names to include in posterior
#' @param n_samples Integer number of posterior samples to generate (default 10000)
#' @param method Character string specifying method: "importance_sampling" or "rejection_sampling"
#' @param log_likelihood Logical whether likelihood column is in log scale (default TRUE)
#' @param temperature Numeric temperature parameter for tempering (default 1.0)
#' @param verbose Logical whether to print progress messages
#'
#' @return List containing:
#'   \describe{
#'     \item{samples}{Matrix of posterior samples (n_samples × n_params)}
#'     \item{weights}{Vector of importance weights for original simulations}
#'     \item{log_weights}{Log-scale importance weights}
#'     \item{ess}{Effective sample size}
#'     \item{mean}{Posterior mean estimates}
#'     \item{cov}{Posterior covariance matrix}
#'     \item{quantiles}{Posterior quantiles at standard levels}
#'     \item{log_evidence}{Log marginal likelihood estimate}
#'   }
#'
#' @export
#' @importFrom matrixStats logSumExp
#'
#' @examples
#' \dontrun{
#' # After running BFRS simulations
#' bfrs_posterior <- calc_bfrs_posterior(
#'     results = results,
#'     param_names = c("tau_i_ETH", "mobility_gamma_ETH", "mu_j_ETH"),
#'     n_samples = 10000,
#'     verbose = TRUE
#' )
#' }
calc_bfrs_posterior <- function(results,
                               param_names,
                               n_samples = 10000,
                               method = "importance_sampling",
                               log_likelihood = TRUE,
                               temperature = 1.0,
                               verbose = FALSE) {

    if (verbose) {
        log_msg("Creating BFRS posterior approximation via %s", method)
        log_msg("  Input simulations: %d", nrow(results))
        log_msg("  Parameters: %d", length(param_names))
        log_msg("  Target samples: %d", n_samples)
    }

    # Extract parameters and likelihoods
    theta <- as.matrix(results[, param_names])

    if (log_likelihood) {
        log_lik <- results$likelihood
    } else {
        log_lik <- log(results$likelihood)
    }

    # Remove invalid entries
    valid <- !is.na(log_lik) & is.finite(log_lik) & (log_lik > -Inf)
    n_valid <- sum(valid)

    if (n_valid < 100) {
        warning(sprintf("Only %d valid simulations out of %d. Results may be unreliable.",
                       n_valid, nrow(results)))
    }

    theta <- theta[valid, , drop = FALSE]
    log_lik <- log_lik[valid]

    if (verbose) {
        log_msg("  Valid simulations: %d (%.1f%%)", n_valid, 100 * n_valid / nrow(results))
    }

    # Apply temperature for tempering if needed
    if (temperature != 1.0) {
        if (verbose) log_msg("  Applying temperature: %.2f", temperature)
        log_lik <- log_lik * temperature
    }

    # Compute importance weights
    max_log_lik <- max(log_lik)
    log_weights <- log_lik - max_log_lik  # Shift for numerical stability
    log_normalizer <- matrixStats::logSumExp(log_weights)
    log_weights_normalized <- log_weights - log_normalizer
    weights <- exp(log_weights_normalized)

    # Compute effective sample size
    ess <- 1 / sum(weights^2)

    if (verbose) {
        log_msg("  Effective sample size: %.0f (%.1f%% of valid)",
                ess, 100 * ess / n_valid)
        log_msg("  Max weight: %.3f", max(weights))
    }

    # Generate posterior samples
    if (method == "importance_sampling") {
        # Resample with replacement using weights
        indices <- sample(1:nrow(theta),
                         size = n_samples,
                         replace = TRUE,
                         prob = weights)
        posterior_samples <- theta[indices, , drop = FALSE]

    } else if (method == "rejection_sampling") {
        # Rejection sampling (slower but sometimes more stable)
        posterior_samples <- matrix(NA, nrow = n_samples, ncol = ncol(theta))
        max_weight <- max(weights)
        n_accepted <- 0
        n_attempts <- 0

        while (n_accepted < n_samples && n_attempts < n_samples * 100) {
            idx <- sample(1:nrow(theta), 1)
            if (runif(1) < weights[idx] / max_weight) {
                n_accepted <- n_accepted + 1
                posterior_samples[n_accepted, ] <- theta[idx, ]
            }
            n_attempts <- n_attempts + 1
        }

        if (n_accepted < n_samples) {
            warning(sprintf("Rejection sampling only produced %d/%d samples",
                          n_accepted, n_samples))
            posterior_samples <- posterior_samples[1:n_accepted, , drop = FALSE]
        }

        if (verbose) {
            log_msg("  Rejection rate: %.1f%%",
                    100 * (1 - n_accepted / n_attempts))
        }

    } else {
        stop("Method must be 'importance_sampling' or 'rejection_sampling'")
    }

    # Ensure column names are preserved
    colnames(posterior_samples) <- param_names

    # Compute posterior statistics
    posterior_mean <- colMeans(posterior_samples, na.rm = TRUE)
    posterior_cov <- cov(posterior_samples, use = "pairwise.complete.obs")

    # Compute quantiles
    quantile_levels <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
    posterior_quantiles <- apply(posterior_samples, 2, quantile,
                                 probs = quantile_levels,
                                 na.rm = TRUE)

    # Estimate log marginal likelihood (log evidence)
    log_evidence <- log_normalizer + max_log_lik - log(nrow(theta))

    if (verbose) {
        log_msg("  Log evidence estimate: %.2f", log_evidence)
        log_msg("  Posterior samples generated: %d", nrow(posterior_samples))
    }

    # Create output object
    posterior <- list(
        samples = posterior_samples,
        weights = weights,
        log_weights = log_weights_normalized,
        ess = ess,
        mean = posterior_mean,
        cov = posterior_cov,
        quantiles = posterior_quantiles,
        log_evidence = log_evidence,
        n_simulations = n_valid,
        method = method,
        temperature = temperature
    )

    class(posterior) <- c("bfrs_posterior", "list")

    return(posterior)
}


#' Convert BFRS Posterior to Prior Format
#'
#' Converts a BFRS posterior object into a prior specification that can be
#' used with sample_parameters() for sequential Bayesian updating.
#'
#' @param bfrs_posterior BFRS posterior object from calc_bfrs_posterior()
#' @param method Character string: "gaussian", "kde", or "empirical"
#'
#' @return List in prior specification format
#' @export
bfrs_posterior_to_prior <- function(bfrs_posterior, method = "gaussian") {

    priors <- list()
    param_names <- colnames(bfrs_posterior$samples)

    if (method == "gaussian") {
        # Approximate with multivariate Gaussian
        for (i in seq_along(param_names)) {
            param <- param_names[i]
            priors[[param]] <- list(
                dist = "normal",
                params = list(
                    mean = bfrs_posterior$mean[i],
                    sd = sqrt(bfrs_posterior$cov[i, i])
                ),
                description = "BFRS posterior approximation"
            )
        }

    } else if (method == "empirical") {
        # Store empirical samples
        for (param in param_names) {
            priors[[param]] <- list(
                dist = "empirical",
                samples = bfrs_posterior$samples[, param],
                description = "BFRS posterior samples"
            )
        }

    } else if (method == "kde") {
        # Kernel density estimation (requires additional implementation)
        stop("KDE method not yet implemented")
    }

    priors$method <- paste("bfrs_posterior", method, sep = "_")
    return(priors)
}