# ==============================================================================
# NPE Diagnostic Functions
# ==============================================================================
# Functions for validating NPE calibration using SBC, coverage, and PPC
# ==============================================================================

#' Run NPE Diagnostics
#'
#' @description
#' Performs comprehensive diagnostics on a trained NPE model including
#' coverage tests and simulation-based calibration (SBC).
#'
#' @param model Trained NPE model object
#' @param test_data Test data (if NULL, uses subset of training data)
#' @param n_test_samples Number of test samples for diagnostics
#' @param diagnostics Which diagnostics to run
#' @param output_dir Directory to save diagnostic results
#' @param verbose Print progress messages
#'
#' @return List containing:
#' \itemize{
#'   \item coverage_50 - Coverage at 50% credible interval
#'   \item coverage_80 - Coverage at 80% credible interval
#'   \item sbc_pass_rate - Proportion of parameters passing SBC
#'   \item needs_retraining - Whether model should be retrained
#'   \item detailed_results - Detailed diagnostic metrics
#' }
#' @export
run_npe_diagnostics <- function(
    model,
    test_data = NULL,
    n_test_samples = 200,
    diagnostics = c("coverage", "sbc"),
    output_dir = NULL,
    verbose = TRUE
) {

    if (verbose) message("Running NPE diagnostics...")

    results <- list()

    # Extract parameter names from model if available
    param_names <- NULL
    if (!is.null(model$architecture$param_names)) {
        param_names <- model$architecture$param_names
    } else if (!is.null(test_data) && !is.null(test_data$param_names)) {
        param_names <- test_data$param_names
    }

    # Store param names in results
    results$param_names <- param_names

    # Run coverage diagnostic
    if ("coverage" %in% diagnostics) {
        if (verbose) message("  Computing coverage...")
        coverage_results <- .calculate_coverage(
            model = model,
            test_data = test_data,
            n_samples = n_test_samples,
            verbose = FALSE
        )
        results$coverage <- coverage_results
        results$coverage_50 <- coverage_results$coverage_50
        results$coverage_80 <- coverage_results$coverage_80
    }

    # Run SBC diagnostic
    if ("sbc" %in% diagnostics) {
        if (verbose) message("  Running SBC...")
        sbc_results <- .calculate_sbc(
            model = model,
            test_data = test_data,
            n_samples = min(n_test_samples, 100),  # SBC is expensive
            verbose = FALSE
        )
        # Set column names on ranks if we have param_names
        if (!is.null(param_names) && !is.null(sbc_results$ranks)) {
            colnames(sbc_results$ranks) <- param_names
        }
        results$sbc <- sbc_results
        results$sbc_pass_rate <- sbc_results$pass_rate
    }

    # Determine if retraining is needed
    results$needs_retraining <- .check_needs_retraining(results)

    # Save diagnostics if requested
    if (!is.null(output_dir)) {
        .save_diagnostics(results, output_dir, verbose)
    }

    if (verbose) {
        message("  Coverage at 50% CI: ", sprintf("%.1f%%", results$coverage_50 * 100))
        message("  Coverage at 80% CI: ", sprintf("%.1f%%", results$coverage_80 * 100))
        if (!is.null(results$sbc_pass_rate)) {
            message("  SBC pass rate: ", sprintf("%.1f%%", results$sbc_pass_rate * 100))
        }
        message("  Needs retraining: ", if (results$needs_retraining) "Yes" else "No")
    }

    return(results)
}

# ==============================================================================
# Internal Diagnostic Functions
# ==============================================================================

#' @keywords internal
.calculate_coverage <- function(model, test_data, n_samples, verbose) {

    if (is.null(test_data)) {
        # Use synthetic test data
        test_data <- .generate_test_data(model, n_samples)
    }

    # Ensure test_data has the correct structure
    if (!all(c("parameters", "observations") %in% names(test_data))) {
        stop("test_data must have 'parameters' and 'observations' fields")
    }

    # Ensure parameters and observations are matrices
    if (!is.matrix(test_data$parameters)) {
        test_data$parameters <- as.matrix(test_data$parameters)
    }
    if (!is.matrix(test_data$observations)) {
        test_data$observations <- as.matrix(test_data$observations)
    }

    n_test <- min(n_samples, nrow(test_data$parameters))
    coverage_50 <- numeric(n_test)
    coverage_80 <- numeric(n_test)

    for (i in 1:n_test) {
        # Get true parameters
        true_params <- test_data$parameters[i, ]

        # Get observations
        obs <- test_data$observations[i, ]

        # Estimate posterior
        obs_df <- data.frame(
            j = 1,
            t = 1:length(obs),
            cases = obs
        )

        posterior_result <- estimate_npe_posterior(
            model = model,
            observed_data = obs_df,
            n_samples = 500,  # Smaller for speed
            return_log_probs = FALSE,
            verbose = FALSE
        )

        # Check coverage
        n_params_to_check <- length(true_params)
        for (j in 1:n_params_to_check) {
            param_samples <- posterior_result$samples[, j]
            true_val <- true_params[j]

            # 50% CI
            ci_50 <- quantile(param_samples, c(0.25, 0.75))
            coverage_50[i] <- coverage_50[i] + (true_val >= ci_50[1] && true_val <= ci_50[2])

            # 80% CI
            ci_80 <- quantile(param_samples, c(0.1, 0.9))
            coverage_80[i] <- coverage_80[i] + (true_val >= ci_80[1] && true_val <= ci_80[2])
        }
    }

    # Average across parameters and test samples
    n_params <- ncol(test_data$parameters)
    coverage_50 <- mean(coverage_50) / n_params
    coverage_80 <- mean(coverage_80) / n_params

    return(list(
        coverage_50 = coverage_50,
        coverage_80 = coverage_80,
        n_test = n_test
    ))
}

#' @keywords internal
.calculate_sbc <- function(model, test_data, n_samples, verbose) {

    if (is.null(test_data)) {
        test_data <- .generate_test_data(model, n_samples)
    }

    n_test <- min(n_samples, nrow(test_data$parameters))
    n_params <- ncol(test_data$parameters)

    ranks <- matrix(NA, nrow = n_test, ncol = n_params)

    # Preserve parameter names if available
    if (!is.null(colnames(test_data$parameters))) {
        param_names <- colnames(test_data$parameters)
    } else if (!is.null(test_data$param_names)) {
        param_names <- test_data$param_names
    } else {
        param_names <- paste0("param_", 1:n_params)
    }

    for (i in 1:n_test) {
        true_params <- test_data$parameters[i, ]
        obs <- test_data$observations[i, ]

        # Format observations
        obs_df <- data.frame(
            j = 1,
            t = 1:length(obs),
            cases = obs
        )

        # Get posterior samples
        posterior_result <- estimate_npe_posterior(
            model = model,
            observed_data = obs_df,
            n_samples = 100,  # Small for speed
            return_log_probs = FALSE,
            verbose = FALSE
        )

        # Calculate ranks
        for (j in 1:n_params) {
            true_val <- true_params[j]
            param_samples <- posterior_result$samples[, j]
            ranks[i, j] <- mean(param_samples < true_val)
        }
    }

    # Test for uniformity
    ks_pvalues <- apply(ranks, 2, function(r) {
        r_clean <- r[!is.na(r)]
        if (length(r_clean) > 5) {
            ks.test(r_clean, punif)$p.value
        } else {
            NA
        }
    })

    pass_rate <- mean(ks_pvalues > 0.05, na.rm = TRUE)

    # Set column names on ranks matrix
    colnames(ranks) <- param_names

    return(list(
        ranks = ranks,
        ks_pvalues = ks_pvalues,
        pass_rate = pass_rate
    ))
}

#' @keywords internal
.check_needs_retraining <- function(diagnostic_results) {
    # Determine if model needs retraining based on diagnostics

    needs_retraining <- FALSE

    # Check coverage
    if (!is.null(diagnostic_results$coverage_50)) {
        # Expected coverage is 50%, allow 20% relative error
        if (diagnostic_results$coverage_50 < 0.4 || diagnostic_results$coverage_50 > 0.6) {
            needs_retraining <- TRUE
        }
    }

    if (!is.null(diagnostic_results$coverage_80)) {
        # Expected coverage is 80%, allow 20% relative error
        if (diagnostic_results$coverage_80 < 0.64 || diagnostic_results$coverage_80 > 0.96) {
            needs_retraining <- TRUE
        }
    }

    # Check SBC
    if (!is.null(diagnostic_results$sbc_pass_rate)) {
        # Expect at least 80% of parameters to pass SBC
        if (diagnostic_results$sbc_pass_rate < 0.8) {
            needs_retraining <- TRUE
        }
    }

    return(needs_retraining)
}

#' @keywords internal
.generate_test_data <- function(model, n_samples) {
    # Generate synthetic test data by sampling from training distribution

    # This is a simplified version - in practice would sample from
    # the training data distribution
    n_params <- model$architecture$n_params
    n_obs <- model$architecture$n_timesteps * model$architecture$n_locations

    parameters <- matrix(
        runif(n_samples * n_params),
        nrow = n_samples,
        ncol = n_params
    )

    observations <- matrix(
        rpois(n_samples * n_obs, lambda = 10),
        nrow = n_samples,
        ncol = n_obs
    )

    return(list(
        parameters = parameters,
        observations = observations
    ))
}

#' @keywords internal
.save_diagnostics <- function(results, output_dir, verbose) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    # Save main results
    saveRDS(results, file.path(output_dir, "diagnostic_results.rds"))

    # Save summary
    summary <- data.frame(
        metric = c("coverage_50", "coverage_80", "sbc_pass_rate", "needs_retraining"),
        value = c(
            results$coverage_50,
            results$coverage_80,
            results$sbc_pass_rate %||% NA,
            as.numeric(results$needs_retraining)
        )
    )

    write.csv(summary, file.path(output_dir, "diagnostic_summary.csv"), row.names = FALSE)

    if (verbose) message("  Diagnostics saved to: ", output_dir)
}

#' @keywords internal
.sample_from_priors <- function(priors, param_names) {
    # Sample parameter values from prior distributions

    samples <- list()

    for (param in param_names) {
        if (!(param %in% names(priors))) {
            warning("No prior found for: ", param, ", using uniform [0,1]")
            samples[[param]] <- runif(1)
            next
        }

        prior <- priors[[param]]

        if (prior$distribution == "beta") {
            samples[[param]] <- rbeta(1, prior$alpha, prior$beta)
        } else if (prior$distribution == "gamma") {
            samples[[param]] <- rgamma(1, shape = prior$shape, rate = prior$rate)
        } else if (prior$distribution == "lognormal") {
            samples[[param]] <- rlnorm(1, meanlog = prior$meanlog, sdlog = prior$sdlog)
        } else if (prior$distribution == "normal") {
            samples[[param]] <- rnorm(1, mean = prior$mean, sd = prior$sd)
        } else if (prior$distribution == "uniform") {
            samples[[param]] <- runif(1, min = prior$min, max = prior$max)
        } else {
            warning("Unknown distribution for ", param, ": ", prior$distribution)
            samples[[param]] <- runif(1)
        }
    }

    return(samples)
}

#' @keywords internal
.calculate_coverage_from_sbc <- function(sbc_results, param_names) {
    # Calculate coverage from SBC results

    n_sims <- length(sbc_results)
    n_params <- length(param_names)

    coverage_50 <- matrix(0, nrow = n_sims, ncol = n_params)
    coverage_80 <- matrix(0, nrow = n_sims, ncol = n_params)

    for (i in 1:n_sims) {
        true_params <- sbc_results[[i]]$true_params
        posterior_samples <- sbc_results[[i]]$posterior_samples

        for (j in 1:n_params) {
            param <- param_names[j]
            true_val <- true_params[[param]]
            samples <- posterior_samples[, param]

            # 50% CI
            ci_50 <- quantile(samples, c(0.25, 0.75))
            coverage_50[i, j] <- (true_val >= ci_50[1] && true_val <= ci_50[2])

            # 80% CI
            ci_80 <- quantile(samples, c(0.1, 0.9))
            coverage_80[i, j] <- (true_val >= ci_80[1] && true_val <= ci_80[2])
        }
    }

    return(list(
        coverage_50 = mean(coverage_50),
        coverage_80 = mean(coverage_80),
        coverage_50_by_param = colMeans(coverage_50),
        coverage_80_by_param = colMeans(coverage_80)
    ))
}

#' @keywords internal
.load_npe_model <- function(model_dir) {
    # Load saved NPE model

    torch <- reticulate::import("torch")

    model_file <- file.path(model_dir, "npe_model.pt")
    if (!file.exists(model_file)) {
        stop("Model file not found: ", model_file)
    }

    # Load model state
    checkpoint <- torch$load(model_file, map_location = "cpu")

    # Reconstruct model
    # This would need to rebuild the architecture - simplified here
    model <- list(
        state_dict = checkpoint$model_state_dict,
        architecture = checkpoint$architecture,
        normalization = checkpoint$normalization
    )

    return(model)
}

#' @keywords internal
`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}