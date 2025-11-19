# ==============================================================================
# Sequential Monte Carlo (SMC) Exact Posterior Functions
# ==============================================================================
# Functions for computing exact Bayesian posterior via importance sampling
# using NPE as proposal distribution
# ==============================================================================

#' Compute SMC Exact Posterior
#'
#' @description
#' Computes the exact Bayesian posterior using Sequential Monte Carlo (SMC)
#' importance sampling. Uses NPE posterior samples as proposal distribution
#' and corrects to exact posterior via importance weights.
#'
#' Formula: w̃ᵢ ∝ p(θᵢ) · p(x|θᵢ) / q_φ(θᵢ|x)
#'
#' @param npe_dir Directory containing NPE outputs (used if npe_samples not provided)
#' @param setup_dir Directory containing setup files (priors.json, config_base.json)
#' @param output_dir Directory to save SMC results
#' @param config_base Base MOSAIC configuration for simulations
#' @param npe_samples Matrix of NPE posterior samples (if NULL, loaded from npe_dir)
#' @param npe_log_probs Vector of NPE log probabilities (if NULL, loaded from npe_dir)
#' @param PATHS MOSAIC paths object
#' @param priors_base Prior specifications (if NULL, loaded from setup_dir)
#' @param sampling_args Sampling arguments for parameter configuration
#' @param n_cores Number of parallel cores for likelihood evaluation
#' @param sample_subset Number of NPE samples to use (NULL = all, recommend 500-1000)
#' @param n_iter Number of iterations per simulation for averaging
#' @param resample Logical, whether to resample weighted samples to create unweighted posterior
#' @param verbose Logical, print progress messages
#'
#' @return List containing:
#'   \describe{
#'     \item{smc_posterior}{Resampled exact posterior samples (if resample=TRUE)}
#'     \item{weights}{List with weights, log_weights, ess}
#'     \item{log_components}{List with log_priors, log_likelihoods, log_q_npe}
#'     \item{n_samples}{Total NPE samples available}
#'     \item{n_subset}{Number of samples actually used}
#'     \item{resampled}{Whether resampling was performed}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' smc_result <- compute_smc_posterior(
#'     npe_dir = "output/2_npe",
#'     setup_dir = "output/0_setup",
#'     output_dir = "output/3_smc",
#'     config_base = config_base,
#'     n_cores = 9,
#'     sample_subset = 500,
#'     verbose = TRUE
#' )
#' }
compute_smc_posterior <- function(
    npe_dir = NULL,
    setup_dir,
    output_dir,
    config_base,
    npe_samples = NULL,
    npe_log_probs = NULL,
    PATHS = NULL,
    priors_base = NULL,
    sampling_args = NULL,
    n_cores = 4,
    sample_subset = 500,
    n_iter = 3,
    resample = TRUE,
    verbose = TRUE
) {

    if (verbose) {
        log_msg("Starting SMC exact posterior computation...")
        log_msg("  Output directory: %s", output_dir)
    }

    # -------------------------------------------------------------------------
    # STEP 1: Load or Use Provided NPE Outputs
    # -------------------------------------------------------------------------

    if (verbose) log_msg("\n=== STEP 1: Loading NPE Outputs ===")

    # Prefer in-memory objects (more efficient when running full workflow)
    if (!is.null(npe_samples) && !is.null(npe_log_probs)) {
        if (verbose) log_msg("  Using in-memory NPE samples and log probabilities")

        # Convert to matrix if data frame
        if (is.data.frame(npe_samples)) {
            param_names <- colnames(npe_samples)
            npe_samples <- as.matrix(npe_samples)
            colnames(npe_samples) <- param_names
        }

        if (verbose) log_msg("  In-memory samples: %d × %d", nrow(npe_samples), ncol(npe_samples))
        if (verbose) log_msg("  In-memory log probs: %d values", length(npe_log_probs))

    } else {
        # Fall back to loading from files (for standalone usage)
        if (is.null(npe_dir)) {
            stop("Either provide npe_samples + npe_log_probs OR npe_dir for loading from files")
        }

        if (verbose) log_msg("  Loading from files: %s", npe_dir)

        # Load posterior samples
        samples_file <- file.path(npe_dir, "posterior", "posterior_samples.parquet")
        if (!file.exists(samples_file)) {
            stop("NPE posterior samples not found: ", samples_file)
        }
        npe_samples <- arrow::read_parquet(samples_file)
        if (verbose) log_msg("  Loaded NPE samples: %d × %d", nrow(npe_samples), ncol(npe_samples))

        # Load NPE log probabilities (CRITICAL for SMC)
        log_probs_file <- file.path(npe_dir, "posterior", "posterior_log_probs.parquet")
        if (!file.exists(log_probs_file)) {
            stop("NPE log probabilities not found: ", log_probs_file,
                 "\nSMC requires log q_φ(θ|x) from NPE model.")
        }
        npe_log_probs_df <- arrow::read_parquet(log_probs_file)
        npe_log_probs <- npe_log_probs_df$log_q_theta
        if (verbose) log_msg("  Loaded NPE log probabilities: %d values", length(npe_log_probs))
    }

    # Validate dimensions match
    if (nrow(npe_samples) != length(npe_log_probs)) {
        stop("Dimension mismatch: ", nrow(npe_samples), " samples but ",
             length(npe_log_probs), " log probabilities")
    }

    n_samples_total <- nrow(npe_samples)

    # -------------------------------------------------------------------------
    # STEP 2: Subset Selection (for computational efficiency)
    # -------------------------------------------------------------------------

    if (!is.null(sample_subset) && sample_subset < n_samples_total) {
        if (verbose) {
            log_msg("\n=== STEP 2: Sample Subset Selection ===")
            log_msg("  Using %d of %d available NPE samples (%.1f%%)",
                   sample_subset, n_samples_total, 100 * sample_subset / n_samples_total)
        }

        set.seed(42)  # Reproducibility
        subset_indices <- sample(1:n_samples_total, sample_subset)

        npe_samples <- npe_samples[subset_indices, ]
        npe_log_probs <- npe_log_probs[subset_indices]

        n_samples_used <- sample_subset
    } else {
        if (verbose) {
            log_msg("\n=== STEP 2: Using All NPE Samples ===")
            log_msg("  Processing all %d samples", n_samples_total)
        }
        n_samples_used <- n_samples_total
    }

    # -------------------------------------------------------------------------
    # STEP 3: Load and Parse Priors
    # -------------------------------------------------------------------------

    if (verbose) log_msg("\n=== STEP 3: Loading and Parsing Priors ===")

    if (is.null(priors_base)) {
        priors_file <- file.path(setup_dir, "priors.json")
        if (!file.exists(priors_file)) {
            stop("Priors file not found: ", priors_file)
        }
        priors_raw <- jsonlite::read_json(priors_file)
        if (verbose) log_msg("  Loaded priors from: %s", basename(priors_file))
    } else {
        priors_raw <- priors_base
        if (verbose) log_msg("  Using provided priors object")
    }

    # Parse priors to flat structure
    param_names <- colnames(npe_samples)
    priors_flat <- parse_priors_for_smc(
        priors_json = priors_raw,
        param_names = param_names,
        verbose = verbose
    )

    # -------------------------------------------------------------------------
    # STEP 4: Evaluate Prior Densities
    # -------------------------------------------------------------------------

    if (verbose) log_msg("\n=== STEP 4: Evaluating Prior Densities ===")

    log_priors <- evaluate_prior_densities(
        samples = as.matrix(npe_samples),
        priors = priors_flat,
        verbose = verbose
    )

    if (verbose) {
        log_msg("  Prior density range: [%.2f, %.2f]",
               min(log_priors, na.rm = TRUE), max(log_priors, na.rm = TRUE))
        n_invalid_priors <- sum(!is.finite(log_priors))
        if (n_invalid_priors > 0) {
            warning("  ", n_invalid_priors, " samples have invalid prior densities")
        }
    }

    # -------------------------------------------------------------------------
    # STEP 5: Compute Likelihoods (EXPENSIVE STEP)
    # -------------------------------------------------------------------------

    if (verbose) {
        log_msg("\n=== STEP 5: Computing Likelihoods ===")
        log_msg("  Running %d MOSAIC simulations (%d iterations each)",
               n_samples_used, n_iter)
        log_msg("  Using %d parallel cores", n_cores)
        estimated_time <- n_samples_used * n_iter * 30 / n_cores / 60
        log_msg("  Estimated time: %.1f minutes", estimated_time)
    }

    log_likelihoods <- compute_smc_likelihoods(
        npe_samples = npe_samples,
        config_base = config_base,
        PATHS = PATHS,
        priors_base = priors_raw,
        sampling_args = sampling_args,
        n_cores = n_cores,
        n_iter = n_iter,
        verbose = verbose
    )

    if (verbose) {
        log_msg("  Likelihood range: [%.2f, %.2f]",
               min(log_likelihoods, na.rm = TRUE), max(log_likelihoods, na.rm = TRUE))
        n_failed <- sum(!is.finite(log_likelihoods))
        if (n_failed > 0) {
            warning("  ", n_failed, " simulations failed (%.1f%%)",
                   100 * n_failed / n_samples_used)
        }
    }

    # -------------------------------------------------------------------------
    # STEP 6: Calculate Importance Weights
    # -------------------------------------------------------------------------

    if (verbose) log_msg("\n=== STEP 6: Calculating Importance Weights ===")

    weights_result <- compute_smc_weights(
        npe_log_probs = npe_log_probs,
        log_priors = log_priors,
        log_likelihoods = log_likelihoods,
        verbose = verbose
    )

    if (verbose) {
        log_msg("  ESS: %.1f (%.1f%% of samples)",
               weights_result$ess, 100 * weights_result$ess / n_samples_used)
        log_msg("  Max weight: %.4f", max(weights_result$weights))
        log_msg("  Min weight: %.4f", min(weights_result$weights))

        # Check for weight collapse
        ess_percent <- 100 * weights_result$ess / n_samples_used

        if (ess_percent < 0.5) {
            # CRITICAL: ESS < 0.5% means extreme weight collapse
            warning("CRITICAL: ESS < 0.5% (", round(weights_result$ess), " / ", n_samples_used, ")",
                   "\nSMC correction is UNRELIABLE. NPE approximation is extremely poor.",
                   "\nThe posterior is likely collapsed to 1-2 samples.",
                   "\nRECOMMENDED ACTIONS:",
                   "\n  1. Review NPE training diagnostics (coverage, calibration)",
                   "\n  2. Consider using BFRS posterior instead of SMC",
                   "\n  3. Increase sample_subset to 2000-5000",
                   "\n  4. Retrain NPE with better architecture/more data",
                   call. = FALSE)
        } else if (ess_percent < 10) {
            # WARNING: ESS < 10% suggests poor NPE approximation
            warning("Low ESS detected (", round(weights_result$ess), " / ", n_samples_used, ")",
                   "\nNPE approximation may be poor. Consider:",
                   "\n  - Using more NPE samples (increase sample_subset)",
                   "\n  - Retraining NPE with better diagnostics",
                   "\n  - Checking prior-posterior consistency",
                   call. = FALSE)
        }
    }

    # -------------------------------------------------------------------------
    # STEP 7: Resample to Create Unweighted Posterior (Optional)
    # -------------------------------------------------------------------------

    smc_posterior <- NULL

    if (resample) {
        if (verbose) log_msg("\n=== STEP 7: Resampling Weighted Posterior ===")

        smc_posterior <- resample_smc_posterior(
            npe_samples = as.matrix(npe_samples),
            smc_weights = weights_result$weights,
            n_resample = n_samples_used,
            verbose = verbose
        )

        if (verbose) log_msg("  Created %d unweighted exact posterior samples", nrow(smc_posterior))
    } else {
        if (verbose) log_msg("\n=== STEP 7: Skipping Resampling ===")
        if (verbose) log_msg("  Using weighted samples (call resample_smc_posterior() later if needed)")
    }

    # -------------------------------------------------------------------------
    # STEP 8: Save Results
    # -------------------------------------------------------------------------

    if (verbose) log_msg("\n=== STEP 8: Saving Results ===")

    smc_result <- list(
        smc_posterior = smc_posterior,
        npe_samples = npe_samples,
        weights = weights_result,
        log_components = list(
            log_priors = log_priors,
            log_likelihoods = log_likelihoods,
            log_q_npe = npe_log_probs
        ),
        n_samples = n_samples_total,
        n_subset = n_samples_used,
        resampled = resample
    )

    .save_smc_results(smc_result, output_dir, verbose)

    if (verbose) {
        log_msg("\n=== SMC EXACT POSTERIOR COMPLETE ===")
        log_msg("Results saved to: %s", output_dir)
    }

    return(smc_result)
}


#' Compute SMC Likelihoods via Parallel Simulation
#'
#' @description
#' Runs MOSAIC forward simulations for each NPE sample to compute log p(x|θ).
#' Uses parallel cluster for efficiency.
#'
#' @param npe_samples Matrix or data frame of NPE posterior samples
#' @param config_base Base MOSAIC configuration
#' @param PATHS MOSAIC paths object
#' @param priors_base Prior specifications
#' @param sampling_args Sampling arguments
#' @param n_cores Number of parallel cores
#' @param n_iter Number of iterations per simulation
#' @param verbose Logical, print progress
#'
#' @return Vector of log likelihoods (length = nrow(npe_samples))
#' @export
compute_smc_likelihoods <- function(
    npe_samples,
    config_base,
    PATHS = NULL,
    priors_base = NULL,
    sampling_args = NULL,
    n_cores = 4,
    n_iter = 3,
    verbose = TRUE
) {

    if (verbose) message("Computing likelihoods for ", nrow(npe_samples), " samples...")

    # Convert to matrix if data frame
    if (is.data.frame(npe_samples)) {
        param_names <- colnames(npe_samples)
        npe_samples <- as.matrix(npe_samples)
        colnames(npe_samples) <- param_names
    }

    # Set up parallel cluster
    if (verbose) message("  Setting up cluster with ", n_cores, " cores...")

    cl <- parallel::makeCluster(n_cores, type = "PSOCK")

    # Initialize workers
    parallel::clusterEvalQ(cl, {
        library(MOSAIC)
        library(reticulate)

        # Suppress warnings
        Sys.setenv(OMP_DISPLAY_ENV = "FALSE")
        Sys.setenv(PYTHONWARNINGS = "ignore")

        # Set up MOSAIC environment
        if (exists("PATHS") && !is.null(PATHS)) {
            set_root_directory(PATHS$ROOT)
        }

        # Import laser-cholera
        lc <- tryCatch({
            reticulate::import("laser_cholera.metapop.model")
        }, error = function(e) NULL)

        if (is.null(lc)) {
            warning("Failed to import laser-cholera in worker")
        }

        return(TRUE)
    })

    # Export variables to cluster
    parallel::clusterExport(cl, c("npe_samples", "config_base", "n_iter", "PATHS",
                                   "priors_base", "sampling_args"),
                           envir = environment())

    # Define worker function
    smc_worker <- function(i) {

        # Get NPE sample parameters
        theta_i <- npe_samples[i, ]

        # Create configuration with NPE parameters
        config_i <- config_base

        for (param_name in names(theta_i)) {
            if (param_name %in% names(config_i)) {
                config_i[[param_name]] <- as.numeric(theta_i[param_name])
            }
        }

        # Run iterations and collect likelihoods
        iter_likelihoods <- numeric(n_iter)

        for (j in 1:n_iter) {
            # Set iteration-specific seed
            config_i$seed <- (i - 1) * n_iter + j

            # Run MOSAIC simulation
            model <- tryCatch({
                lc$run_model(paramfile = config_i, quiet = TRUE)
            }, error = function(e) NULL)

            # Calculate likelihood
            if (!is.null(model)) {
                obs_cases <- config_base$reported_cases
                est_cases <- model$results$expected_cases
                obs_deaths <- config_base$reported_deaths
                est_deaths <- model$results$disease_deaths

                if (!is.null(obs_cases) && !is.null(est_cases) &&
                    !is.null(obs_deaths) && !is.null(est_deaths)) {

                    iter_likelihoods[j] <- tryCatch({
                        calc_model_likelihood(
                            config = config_base,
                            obs_cases = obs_cases,
                            est_cases = est_cases,
                            obs_deaths = obs_deaths,
                            est_deaths = est_deaths,
                            add_max_terms = FALSE,
                            add_peak_timing = FALSE,
                            add_peak_magnitude = FALSE,
                            add_cumulative_total = FALSE,
                            add_wis = FALSE,
                            weight_cases = 1.0,
                            weight_deaths = 0,
                            enable_guardrails = FALSE
                        )
                    }, error = function(e) -Inf)
                } else {
                    iter_likelihoods[j] <- -Inf
                }
            } else {
                iter_likelihoods[j] <- -Inf
            }
        }

        # Average likelihoods in log space
        valid_ll <- iter_likelihoods[is.finite(iter_likelihoods)]

        if (length(valid_ll) > 0) {
            return(calc_log_mean_exp(valid_ll))
        } else {
            return(-Inf)
        }
    }

    # Run parallel simulations with progress bar
    if (verbose) message("  Running parallel simulations...")

    log_likelihoods <- pbapply::pblapply(
        1:nrow(npe_samples),
        smc_worker,
        cl = cl
    )

    # Clean up cluster
    parallel::stopCluster(cl)

    # Convert to vector
    log_likelihoods <- unlist(log_likelihoods)

    if (verbose) {
        n_success <- sum(is.finite(log_likelihoods))
        message("  Completed: ", n_success, "/", nrow(npe_samples), " successful (",
               sprintf("%.1f%%", 100 * n_success / nrow(npe_samples)), ")")
    }

    return(log_likelihoods)
}


#' Calculate SMC Importance Weights
#'
#' @description
#' Computes importance sampling weights using formula:
#' w̃ᵢ ∝ p(θᵢ) · p(x|θᵢ) / q_φ(θᵢ|x)
#'
#' @param npe_log_probs Vector of log q_φ(θᵢ|x) from NPE
#' @param log_priors Vector of log p(θᵢ) from original priors
#' @param log_likelihoods Vector of log p(x|θᵢ) from simulations
#' @param verbose Logical, print diagnostics
#'
#' @return List with weights, log_weights, log_weights_unnorm, ess
#' @export
compute_smc_weights <- function(
    npe_log_probs,
    log_priors,
    log_likelihoods,
    verbose = FALSE
) {

    if (verbose) message("Calculating importance weights...")

    # Validate inputs
    n <- length(npe_log_probs)
    if (length(log_priors) != n || length(log_likelihoods) != n) {
        stop("Input vectors must have same length: ",
             "npe_log_probs=", length(npe_log_probs),
             ", log_priors=", length(log_priors),
             ", log_likelihoods=", length(log_likelihoods))
    }

    # Calculate unnormalized log weights
    # log w̃ᵢ = log p(θᵢ) + log p(x|θᵢ) - log q_φ(θᵢ|x)
    log_weights_unnorm <- log_priors + log_likelihoods - npe_log_probs

    # Check for invalid values
    n_invalid <- sum(!is.finite(log_weights_unnorm))
    if (n_invalid > 0) {
        if (verbose) {
            warning("  ", n_invalid, " samples have invalid weights (setting to -Inf)")
        }
        log_weights_unnorm[!is.finite(log_weights_unnorm)] <- -Inf
    }

    # Normalize using log-sum-exp
    log_normalizer <- matrixStats::logSumExp(log_weights_unnorm[is.finite(log_weights_unnorm)])
    log_weights_norm <- log_weights_unnorm - log_normalizer

    # Convert to linear space
    weights <- exp(log_weights_norm)

    # Ensure weights sum to 1 (numerical stability)
    weights <- weights / sum(weights)

    # Calculate effective sample size
    ess <- 1 / sum(weights^2)

    if (verbose) {
        message("  ESS: ", sprintf("%.1f", ess), " (", sprintf("%.1f%%", 100 * ess / n), ")")
        message("  Weight range: [", sprintf("%.6f", min(weights)), ", ",
               sprintf("%.6f", max(weights)), "]")
    }

    return(list(
        weights = weights,
        log_weights = log_weights_norm,
        log_weights_unnorm = log_weights_unnorm,
        ess = ess
    ))
}


#' Resample SMC Posterior
#'
#' @description
#' Creates unweighted exact posterior samples by resampling with replacement
#' based on importance weights.
#'
#' @param npe_samples Matrix of NPE samples
#' @param smc_weights Vector of importance weights (must sum to 1)
#' @param n_resample Number of samples to generate (NULL = same as input)
#' @param verbose Logical, print progress
#'
#' @return Matrix of resampled posterior samples
#' @export
resample_smc_posterior <- function(
    npe_samples,
    smc_weights,
    n_resample = NULL,
    verbose = FALSE
) {

    if (is.null(n_resample)) {
        n_resample <- nrow(npe_samples)
    }

    if (verbose) {
        message("Resampling ", n_resample, " exact posterior samples from ",
               nrow(npe_samples), " weighted samples...")
    }

    # Validate weights
    if (length(smc_weights) != nrow(npe_samples)) {
        stop("Length of smc_weights (", length(smc_weights),
             ") must match nrow(npe_samples) (", nrow(npe_samples), ")")
    }

    # Check weights sum to 1
    weight_sum <- sum(smc_weights)
    if (abs(weight_sum - 1.0) > 1e-6) {
        warning("Weights don't sum to 1 (sum = ", weight_sum, "), normalizing...")
        smc_weights <- smc_weights / weight_sum
    }

    # Resample with replacement based on weights
    resampled_indices <- sample(
        x = 1:nrow(npe_samples),
        size = n_resample,
        replace = TRUE,
        prob = smc_weights
    )

    # Extract resampled rows
    smc_posterior <- npe_samples[resampled_indices, , drop = FALSE]

    # Preserve column names
    colnames(smc_posterior) <- colnames(npe_samples)

    if (verbose) {
        message("  Resampling complete")
        # Report how many unique samples were selected
        n_unique <- length(unique(resampled_indices))
        message("  Unique samples selected: ", n_unique, " of ", nrow(npe_samples))
    }

    return(smc_posterior)
}
