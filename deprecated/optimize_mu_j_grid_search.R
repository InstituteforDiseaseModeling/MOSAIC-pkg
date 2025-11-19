#' Optimize Mortality Parameters via Grid Search
#'
#' After BFRS calibration to cases data, this function estimates mortality parameters
#' (mu_j) using grid search over actual mu_j_baseline values. It runs simulations
#' for each grid point, calculates death likelihood, estimates the posterior
#' distribution, and optionally applies the optimal mu_j to all best subset configs.
#'
#' @param config_best_path Path to config_best.json file (e.g., "1_bfrs/config/config_best.json").
#'   The config must contain `reported_deaths` matrix with observed death data.
#' @param n_iter Number of stochastic iterations per grid point (default: 5)
#' @param verbose Logical; print progress messages (default: TRUE)
#'
#' @return List containing:
#'   - grid_results: Data frame with likelihood for each mu_j value
#'   - posterior: List with posterior statistics (mean, median, CI, ESS, MAP)
#'   - optimal_mu_j: MAP estimate of mu_j_baseline
#'   - configs_updated: Character vector of paths to updated config files
#'   - summary_file: Path to summary JSON file
#'
#' @details
#' The function performs the following steps:
#' 1. Load best model config and extract death data from `reported_deaths`
#' 2. Define grid over mu_j_baseline values (50 points, log-spaced from 0.0001 to 0.05)
#' 3. Run stochastic simulations for each grid point (n_iter replicates)
#' 4. Calculate negative binomial death likelihood for each simulation
#' 5. Estimate posterior distribution from likelihood surface
#' 6. Apply optimal mu_j to top 50 configs by weight
#' 7. Generate posterior predictive check plots
#' 8. Save all results and summary
#'
#' Computational requirements:
#' - Grid size: 50 points
#' - Iterations per point: n_iter (default 5)
#' - Total simulations: 50 × n_iter = 250 (default)
#' - Estimated runtime: 5-10 minutes
#'
#' @examples
#' \dontrun{
#' # After running BFRS calibration, optimize mu_j using grid search
#' results <- optimize_mu_j_grid_search(
#'   config_best_path = "local/calibration/ETH/1_bfrs/config/config_best.json",
#'   n_iter = 5,
#'   verbose = TRUE
#' )
#'
#' # View optimal mu_j
#' print(results$optimal_mu_j)
#'
#' # View posterior statistics
#' print(results$posterior)
#'
#' # Access updated configs
#' print(results$configs_updated)
#' }
#'
#' @export
optimize_mu_j_grid_search <- function(
  config_best_path,
  n_iter = 5,
  verbose = TRUE
) {

  # ============================================================================
  # Define internal parameters
  # ============================================================================
  # Grid search parameters
  mu_j_grid <- 10^seq(log10(0.0001), log10(0.05), length.out = 50)

  # Likelihood parameters
  overdispersion <- 10

  # Best subset parameters
  n_best <- 50  # Number of top configs to update

  # ============================================================================
  # Validate inputs and derive paths
  # ============================================================================
  if (!file.exists(config_best_path)) {
    stop("config_best.json not found at: ", config_best_path)
  }

  # Derive dir_bfrs from config path: config/config_best.json -> 1_bfrs/
  dir_bfrs <- dirname(dirname(config_best_path))

  if (!dir.exists(dir_bfrs)) {
    stop("BFRS directory does not exist: ", dir_bfrs)
  }

  if (n_iter < 1) {
    stop("n_iter must be >= 1")
  }

  # Import laser-cholera
  if (verbose) cat("Loading laser-cholera Python module...\n")
  lc <- reticulate::import("laser_cholera")

  # ============================================================================
  # Step 1: Load Best Model Config and Extract Deaths Data
  # ============================================================================
  if (verbose) cat("\n=== Step 1: Loading best model config ===\n")

  config_best <- jsonlite::read_json(config_best_path)

  # Extract observed deaths from config
  reported_deaths <- config_best$reported_deaths
  if (is.null(reported_deaths)) {
    stop("config_best.json does not contain 'reported_deaths' field")
  }

  # Convert to matrix if needed and extract as vector
  if (is.list(reported_deaths)) {
    reported_deaths <- do.call(rbind, reported_deaths)
  }

  # Sum across locations (row sums) to get total deaths per time point
  if (is.matrix(reported_deaths)) {
    obs_deaths <- colSums(reported_deaths, na.rm = TRUE)
  } else {
    obs_deaths <- as.numeric(reported_deaths)
  }

  if (verbose) {
    cat("Loaded config_best.json\n")
    cat("Current mu_j_baseline:", config_best$mu_j_baseline, "\n")
    cat("Current mu_j_slope:", config_best$mu_j_slope, "\n")
    cat("Current mu_j_epidemic_factor:", config_best$mu_j_epidemic_factor, "\n")
    cat("Extracted", length(obs_deaths), "death observations from config\n")
    cat("Total observed deaths:", sum(obs_deaths, na.rm = TRUE), "\n")
  }

  # ============================================================================
  # Step 2: Grid Search Simulation Loop
  # ============================================================================
  if (verbose) {
    cat("\n=== Step 2: Running grid search simulations ===\n")
    cat("Grid size:", length(mu_j_grid), "\n")
    cat("Grid range: [", min(mu_j_grid), ",", max(mu_j_grid), "]\n")
    cat("Iterations per point:", n_iter, "\n")
    cat("Total simulations:", length(mu_j_grid) * n_iter, "\n\n")
  }

  # Initialize results storage
  results <- data.frame(
    mu_j_baseline = numeric(),
    death_lik_mean = numeric(),
    death_lik_sd = numeric(),
    death_lik_se = numeric(),
    n_iter = integer()
  )

  # Grid search loop
  for (i in seq_along(mu_j_grid)) {
    mu_j_test <- mu_j_grid[i]

    if (verbose) {
      cat(sprintf("Grid point %d/%d: mu_j_baseline = %.6f\n",
                  i, length(mu_j_grid), mu_j_test))
    }

    # Update config with test value
    config_test <- config_best
    config_test$mu_j_baseline <- mu_j_test

    # Regenerate mu_jt matrix from updated mu_j_baseline
    n_days <- ncol(config_test$mu_jt)
    time_factor <- (seq_len(n_days) - 1) / max(1, n_days - 1)
    config_test$mu_jt[1, ] <- mu_j_test * (1 + config_test$mu_j_slope * time_factor)
    config_test$mu_jt <- pmax(0, pmin(1, config_test$mu_jt))

    # Run stochastic simulations
    death_liks <- numeric(n_iter)

    for (iter in 1:n_iter) {
      # Update seed for stochastic variation
      config_test$seed <- config_best$seed + i * 1000 + iter

      # Run LASER simulation
      tryCatch({
        model <- lc$run_model(paramfile = config_test, quiet = TRUE)

        # Extract predicted deaths (assuming weekly aggregation matches obs_deaths)
        pred_deaths <- as.vector(model$C_jt_deaths_obs)

        # Match length of predictions to observations
        n_obs <- length(obs_deaths)
        if (length(pred_deaths) > n_obs) {
          pred_deaths <- pred_deaths[1:n_obs]
        } else if (length(pred_deaths) < n_obs) {
          # Pad with zeros if needed
          pred_deaths <- c(pred_deaths, rep(0, n_obs - length(pred_deaths)))
        }

        # Calculate death likelihood
        death_liks[iter] <- calc_log_likelihood_negbin(
          observed = obs_deaths,
          estimated = pred_deaths,
          k = overdispersion,
          verbose = FALSE
        )

      }, error = function(e) {
        if (verbose) {
          cat(sprintf("  Iteration %d failed: %s\n", iter, e$message))
        }
        death_liks[iter] <- -Inf
      })
    }

    # Store results for this grid point
    results <- rbind(results, data.frame(
      mu_j_baseline = mu_j_test,
      death_lik_mean = mean(death_liks[is.finite(death_liks)]),
      death_lik_sd = sd(death_liks[is.finite(death_liks)]),
      death_lik_se = sd(death_liks[is.finite(death_liks)]) / sqrt(sum(is.finite(death_liks))),
      n_iter = sum(is.finite(death_liks))
    ))

    if (verbose) {
      cat(sprintf("  Mean log-likelihood: %.2f (SD: %.2f)\n",
                  results$death_lik_mean[i], results$death_lik_sd[i]))
    }
  }

  # Save grid search results
  results_file <- file.path(dir_bfrs, "mu_j_grid_search_results.csv")
  write.csv(results, results_file, row.names = FALSE)
  if (verbose) cat("\nSaved grid search results to:", results_file, "\n")

  # ============================================================================
  # Step 3: Estimate Posterior Distribution
  # ============================================================================
  if (verbose) cat("\n=== Step 3: Estimating posterior distribution ===\n")

  # Convert log-likelihoods to weights (assuming uniform prior)
  log_lik <- results$death_lik_mean
  log_lik_centered <- log_lik - max(log_lik, na.rm = TRUE)
  weights <- exp(log_lik_centered)
  weights <- weights / sum(weights, na.rm = TRUE)

  # Calculate effective sample size
  ess <- 1 / sum(weights^2, na.rm = TRUE)

  if (verbose) {
    cat("Effective sample size (ESS):", round(ess, 1), "\n")
  }

  # Calculate posterior statistics
  mu_j_posterior_mean <- sum(mu_j_grid * weights, na.rm = TRUE)
  mu_j_posterior_var <- sum((mu_j_grid - mu_j_posterior_mean)^2 * weights, na.rm = TRUE)

  # Posterior quantiles
  posterior_cdf <- cumsum(weights)
  mu_j_q025 <- mu_j_grid[which(posterior_cdf >= 0.025)[1]]
  mu_j_q50 <- mu_j_grid[which(posterior_cdf >= 0.5)[1]]
  mu_j_q975 <- mu_j_grid[which(posterior_cdf >= 0.975)[1]]

  # Maximum a posteriori (MAP) estimate
  mu_j_map <- mu_j_grid[which.max(weights)]

  if (verbose) {
    cat("Posterior mean:", round(mu_j_posterior_mean, 6), "\n")
    cat("Posterior median:", round(mu_j_q50, 6), "\n")
    cat("Posterior 95% CI: [", round(mu_j_q025, 6), ",", round(mu_j_q975, 6), "]\n")
    cat("MAP estimate:", round(mu_j_map, 6), "\n")
  }

  # Store posterior statistics
  posterior <- list(
    mean = mu_j_posterior_mean,
    median = mu_j_q50,
    variance = mu_j_posterior_var,
    sd = sqrt(mu_j_posterior_var),
    ci_lower = mu_j_q025,
    ci_upper = mu_j_q975,
    map = mu_j_map,
    ess = ess
  )

  # Try to fit parametric posterior (Beta distribution)
  tryCatch({
    posterior_fit <- fit_beta_from_ci(
      mode_val = mu_j_map,
      ci_lower = mu_j_q025,
      ci_upper = mu_j_q975,
      verbose = FALSE
    )
    posterior$beta_shape1 <- posterior_fit$shape1
    posterior$beta_shape2 <- posterior_fit$shape2

    if (verbose) {
      cat("Fitted Beta distribution: shape1 =", round(posterior_fit$shape1, 3),
          ", shape2 =", round(posterior_fit$shape2, 3), "\n")
    }
  }, error = function(e) {
    if (verbose) {
      cat("Could not fit Beta distribution:", e$message, "\n")
    }
  })

  # Save posterior analysis
  posterior_file <- file.path(dir_bfrs, "mu_j_posterior_analysis.json")
  jsonlite::write_json(posterior, posterior_file, auto_unbox = TRUE, pretty = TRUE)
  if (verbose) cat("Saved posterior analysis to:", posterior_file, "\n")

  # ============================================================================
  # Step 4: Apply Optimal mu_j to Best Subset Configs
  # ============================================================================
  if (verbose) cat("\n=== Step 4: Applying optimal mu_j to best subset ===\n")

  configs_updated <- character()

  # Load simulations metadata
  simulations_file <- file.path(dir_bfrs, "simulations.parquet")
  if (!file.exists(simulations_file)) {
    warning("simulations.parquet not found - skipping best subset update")
  } else {
    simulations <- arrow::read_parquet(simulations_file)

    # Identify best subset
    best_subset <- simulations %>%
      dplyr::arrange(desc(weight)) %>%
      dplyr::slice_head(n = n_best)

    if (verbose) {
      cat("Updating", nrow(best_subset), "configs from best subset\n")
    }

    # Create output directory for updated configs
    dir_updated <- file.path(dir_bfrs, "outputs", "parameters_updated")
    dir.create(dir_updated, recursive = TRUE, showWarnings = FALSE)

    # Load priors and sampling args for reconstruction
    priors_file <- file.path(dir_bfrs, "priors.json")
    if (!file.exists(priors_file)) {
      stop("priors.json not found at: ", priors_file)
    }
    priors <- jsonlite::read_json(priors_file)

    # Get original sampling args from config_best
    sampling_args <- config_best$sampling_args
    if (is.null(sampling_args)) {
      warning("sampling_args not found in config_best - using defaults")
      sampling_args <- list()  # Will use defaults from sample_parameters
    }

    # Update each config
    for (i in seq_len(nrow(best_subset))) {
      if (verbose && i %% 10 == 0) {
        cat(sprintf("  Processing config %d/%d\n", i, nrow(best_subset)))
      }

      # Reconstruct config from seed
      config_updated <- tryCatch({
        sample_parameters(
          priors = priors,
          config = config_best,  # Use config_best as template
          seed = best_subset$seed_sim[i],
          sample_args = sampling_args,
          verbose = FALSE
        )
      }, error = function(e) {
        warning(sprintf("Failed to reconstruct config for sim %d: %s",
                       best_subset$sim_id[i], e$message))
        return(NULL)
      })

      if (is.null(config_updated)) next

      # Override mu_j_baseline with MAP estimate
      config_updated$mu_j_baseline <- mu_j_map

      # Regenerate mu_jt matrix
      n_days <- ncol(config_updated$mu_jt)
      time_factor <- (seq_len(n_days) - 1) / max(1, n_days - 1)
      config_updated$mu_jt[1, ] <- mu_j_map * (1 + config_updated$mu_j_slope * time_factor)
      config_updated$mu_jt <- pmax(0, pmin(1, config_updated$mu_jt))

      # Save updated config
      config_file <- file.path(
        dir_updated,
        sprintf("sim_%07d.json", best_subset$sim_id[i])
      )
      jsonlite::write_json(config_updated, config_file,
                          auto_unbox = TRUE, pretty = TRUE)
      configs_updated <- c(configs_updated, config_file)
    }

    if (verbose) {
      cat("Updated", length(configs_updated), "config files\n")
      cat("Saved to:", dir_updated, "\n")
    }

    # Update simulations.parquet with new mu_j values
    simulations_updated <- simulations
    simulations_updated$mu_j_baseline_original <- simulations$mu_j_baseline
    simulations_updated$mu_j_baseline_updated <- ifelse(
      simulations$sim_id %in% best_subset$sim_id,
      mu_j_map,
      simulations$mu_j_baseline
    )

    simulations_updated_file <- file.path(dir_bfrs, "simulations_mu_j_updated.parquet")
    arrow::write_parquet(simulations_updated, simulations_updated_file)

    if (verbose) {
      cat("Updated simulations file:", simulations_updated_file, "\n")
    }
  }

  # ============================================================================
  # Step 5: Generate Posterior Predictive Checks
  # ============================================================================
  if (verbose) cat("\n=== Step 5: Generating posterior predictive checks ===\n")

  # Create plots directory
  dir_plots <- file.path(dir_bfrs, "plots")
  dir.create(dir_plots, recursive = TRUE, showWarnings = FALSE)

  # Run best model with optimal mu_j
  config_best_updated <- config_best
  config_best_updated$mu_j_baseline <- mu_j_map

  # Regenerate mu_jt
  n_days <- ncol(config_best_updated$mu_jt)
  time_factor <- (seq_len(n_days) - 1) / max(1, n_days - 1)
  config_best_updated$mu_jt[1, ] <- mu_j_map * (1 + config_best_updated$mu_j_slope * time_factor)
  config_best_updated$mu_jt <- pmax(0, pmin(1, config_best_updated$mu_jt))

  # Run multiple stochastic iterations for PPC
  n_ppc_iterations <- 50
  if (verbose) cat("Running", n_ppc_iterations, "stochastic iterations for PPC...\n")

  death_predictions <- matrix(NA, nrow = n_ppc_iterations, ncol = length(obs_deaths))

  for (i in 1:n_ppc_iterations) {
    config_best_updated$seed <- config_best$seed + 10000 + i

    tryCatch({
      model <- lc$run_model(paramfile = config_best_updated, quiet = TRUE)
      pred_deaths <- as.vector(model$C_jt_deaths_obs)

      # Match length
      if (length(pred_deaths) >= length(obs_deaths)) {
        death_predictions[i, ] <- pred_deaths[1:length(obs_deaths)]
      } else {
        death_predictions[i, 1:length(pred_deaths)] <- pred_deaths
      }
    }, error = function(e) {
      if (verbose) cat("PPC iteration", i, "failed:", e$message, "\n")
    })
  }

  # Calculate prediction quantiles
  pred_median <- apply(death_predictions, 2, median, na.rm = TRUE)
  pred_q025 <- apply(death_predictions, 2, quantile, probs = 0.025, na.rm = TRUE)
  pred_q975 <- apply(death_predictions, 2, quantile, probs = 0.975, na.rm = TRUE)

  # Create PPC plot
  ppc_file <- file.path(dir_plots, "mu_j_ppc_deaths.pdf")
  pdf(ppc_file, width = 10, height = 6)

  # Create time index for x-axis
  time_index <- seq_len(length(obs_deaths))

  plot(time_index, obs_deaths, type = "p", pch = 16, col = "black",
       xlab = "Time (days)", ylab = "Deaths",
       main = "Posterior Predictive Check: Deaths (Optimized mu_j)",
       ylim = c(0, max(c(obs_deaths, pred_q975), na.rm = TRUE)))

  polygon(c(time_index, rev(time_index)),
          c(pred_q025, rev(pred_q975)),
          col = rgb(0, 0, 1, 0.2), border = NA)

  lines(time_index, pred_median, col = "blue", lwd = 2)

  legend("topright",
         legend = c("Observed", "Predicted median", "95% CI"),
         col = c("black", "blue", rgb(0, 0, 1, 0.2)),
         lty = c(NA, 1, 1), lwd = c(NA, 2, 10), pch = c(16, NA, NA))

  dev.off()

  if (verbose) cat("Saved PPC plot to:", ppc_file, "\n")

  # ============================================================================
  # Step 6: Save Final Summary
  # ============================================================================
  if (verbose) cat("\n=== Step 6: Saving final summary ===\n")

  # Calculate original and optimized death likelihoods
  original_death_lik <- results$death_lik_mean[which.min(abs(mu_j_grid - config_best$mu_j_baseline))]
  optimized_death_lik <- max(results$death_lik_mean, na.rm = TRUE)

  summary <- list(
    optimization = list(
      method = "grid_search",
      parameter = "mu_j_baseline",
      grid_size = length(mu_j_grid),
      grid_range = c(min(mu_j_grid), max(mu_j_grid)),
      iterations_per_point = n_iter,
      overdispersion = overdispersion
    ),
    results = list(
      original_mu_j = config_best$mu_j_baseline,
      optimal_mu_j = mu_j_map,
      posterior_mean = mu_j_posterior_mean,
      posterior_median = mu_j_q50,
      posterior_ci = c(mu_j_q025, mu_j_q975),
      posterior_ess = ess
    ),
    model_comparison = list(
      original_death_lik = original_death_lik,
      optimized_death_lik = optimized_death_lik,
      improvement = optimized_death_lik - original_death_lik
    ),
    configs_updated = list(
      n_updated = length(configs_updated),
      output_dir = file.path(dir_bfrs, "outputs", "parameters_updated")
    )
  )

  summary_file <- file.path(dir_bfrs, "mu_j_optimization_summary.json")
  jsonlite::write_json(summary, summary_file, auto_unbox = TRUE, pretty = TRUE)

  if (verbose) {
    cat("Saved optimization summary to:", summary_file, "\n")
    cat("\n=== Optimization Complete ===\n")
    cat("Original mu_j_baseline:", round(config_best$mu_j_baseline, 6), "\n")
    cat("Optimal mu_j_baseline:", round(mu_j_map, 6), "\n")
    cat("Death log-likelihood improvement:", round(optimized_death_lik - original_death_lik, 2), "\n")
  }

  # Return results
  invisible(list(
    grid_results = results,
    posterior = posterior,
    optimal_mu_j = mu_j_map,
    configs_updated = configs_updated,
    summary_file = summary_file
  ))
}
