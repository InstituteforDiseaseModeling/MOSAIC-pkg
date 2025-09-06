#' Calculate Best Model Predictive Check Samples
#'
#' Generates predictive samples by identifying the single best-fitting model
#' (highest log-likelihood) and re-running it multiple times with different
#' random seeds to capture stochastic variability. This enables model validation
#' through comparison of observed data with predictions from the best model.
#'
#' @param results Data frame containing calibration results with parameter columns,
#'   likelihood values, and importance weights from posterior analysis.
#' @param col_ll Integer. Column index for log-likelihood values.
#' @param col_params Integer vector. Column indices for model parameters.
#' @param col_seed Integer. Column index for random seeds (for reproducibility).
#' @param param_names Optional character vector of parameter names corresponding to col_params.
#'   If NULL, will use column names from results or generate generic names.
#' @param posterior_weights Numeric vector of posterior importance weights.
#'   Currently unused but kept for backward compatibility.
#' @param n_posterior_samples Integer. DEPRECATED: Ignored (only the single best model is used).
#' @param n_replications Integer. Number of replications of the best model
#'   to capture stochastic variability (default 50).
#' @param config_base Base LASER configuration list (from get_location_config).
#' @param priors_base Prior distributions list (from get_location_priors).
#' @param PATHS List of file paths (from get_paths).
#' @param parallel Logical. Whether to run simulations in parallel (default TRUE).
#' @param n_cores Integer. Number of cores for parallel execution (default 4).
#' @param verbose Logical. Whether to print progress messages (default TRUE).
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{observed}: List with \code{cases} and \code{deaths} matrices
#'     \item \code{predictions}: List of predicted \code{cases}/\code{deaths} for each replication
#'     \item \code{summary_stats}: Aggregated prediction statistics (mean, median, 2.5\%/97.5\% quantiles, sd)
#'     \item \code{best_model}: Information about the best model used (index, sim_id, seed, likelihood, parameters)
#'     \item \code{diagnostics}: Runtime and replication information
#'   }
#'
#' @details
#' This function implements best model predictive checking:
#' \enumerate{
#'   \item Identifies the single model with highest log-likelihood
#'   \item Re-runs that model multiple times with different random seeds
#'   \item Captures stochastic variability of the best-fitting model
#'   \item Returns structured results for visualization and diagnostic checking
#' }
#'
#' The predictive distribution represents what the best model predicts
#' we should observe, accounting for stochastic noise. Systematic discrepancies
#' between observed and predicted data indicate model misspecification.
#'
#' @examples
#' \dontrun{
#' ppc_results <- calc_model_ppc(
#'   results = calibration_results,
#'   col_ll = which(names(calibration_results) == "likelihood"),
#'   col_params = param_cols,
#'   col_seed = which(names(calibration_results) == "seed"),
#'   param_names = names(calibration_results)[param_cols],
#'   n_replications = 50,
#'   config_base = config_base,
#'   priors_base = priors_base,
#'   PATHS = PATHS
#' )
#' }
#'
#' @export
#' @importFrom parallel makeCluster stopCluster clusterEvalQ clusterExport
#' @importFrom pbapply pblapply
#' @seealso [plot_model_ppc()], [calc_model_posterior_distributions()]
#' @family model-validation
calc_model_ppc <- function(results,
                           col_ll,
                           col_params,
                           col_seed,
                           param_names = NULL,
                           posterior_weights = NULL,
                           n_posterior_samples = 100,  # deprecated, ignored
                           n_replications = 50,
                           config_base,
                           priors_base,
                           PATHS,
                           parallel = TRUE,
                           n_cores = 4,
                           verbose = TRUE) {

     # ============================================================================
     # Input validation
     # ============================================================================
     if (!is.data.frame(results)) stop("results must be a data.frame")
     if (!is.numeric(col_ll) || length(col_ll) != 1 || col_ll > ncol(results)) {
          stop("col_ll must be a single valid column index")
     }
     if (!is.numeric(col_params) || any(col_params > ncol(results))) {
          stop("col_params must be valid column indices")
     }
     if (!is.numeric(col_seed) || length(col_seed) != 1 || col_seed > ncol(results)) {
          stop("col_seed must be a single valid column index")
     }
     if (n_replications < 1) stop("n_replications must be a positive integer")

     # Parameter names
     if (is.null(param_names)) {
          if (!is.null(colnames(results))) {
               param_names <- colnames(results)[col_params]
          } else {
               param_names <- paste0("param_", col_params)
          }
     } else {
          if (length(param_names) != length(col_params)) {
               stop("param_names length must match col_params length")
          }
     }

     # Keep rows with valid likelihoods
     valid_rows <- !is.na(results[[col_ll]]) & is.finite(results[[col_ll]])
     if (!any(valid_rows)) stop("No valid likelihood values found")
     results <- results[valid_rows, , drop = FALSE]

     if (verbose) {
          cat("Best Model Predictive Check Setup:\n")
          cat("- Valid calibration results:", nrow(results), "\n")
          cat("- Parameters:", length(col_params), "\n")
          cat("- Model replications:", n_replications, "\n")
          if (!missing(n_posterior_samples) && n_posterior_samples != 100) {
               cat("- NOTE: n_posterior_samples is deprecated and ignored (single best model used)\n")
          }
     }

     # ============================================================================
     # Identify best model
     # ============================================================================
     if (verbose) cat("Identifying best model (highest log-likelihood)...\n")
     best_idx <- which.max(results[[col_ll]])
     best_model_row <- results[best_idx, , drop = FALSE]
     best_likelihood <- results[[col_ll]][best_idx]
     best_sim_id <- if ("sim" %in% names(results)) results$sim[best_idx] else best_idx
     best_seed <- results[[col_seed]][best_idx]

     if (verbose) {
          cat("Best model identified:\n")
          cat("- Row index:", best_idx, "\n")
          cat("- Simulation ID:", best_sim_id, "\n")
          cat("- Original seed:", best_seed, "\n")
          cat("- Log-likelihood:", sprintf("%.3f", best_likelihood), "\n")
     }

     # ============================================================================
     # Helper: robust observed matrices from config_base
     # ============================================================================
     get_first_nonnull <- function(x, candidates) {
          for (nm in candidates) {
               if (!is.null(x[[nm]])) return(x[[nm]])
          }
          NULL
     }

     observed_cases <- get_first_nonnull(
          config_base,
          c("reported_cases", "observed_cases", "obs_cases", "cases_obs", "cases_reported")
     )
     observed_deaths <- get_first_nonnull(
          config_base,
          c("reported_deaths", "observed_deaths", "obs_deaths", "deaths_obs", "deaths_reported")
     )

     if (is.null(observed_cases) || is.null(observed_deaths)) {
          stop("Observed data not found in config_base. Expected fields like ",
               "'reported_cases'/'reported_deaths' (or common variants).")
     }
     observed_cases <- as.matrix(observed_cases)
     observed_deaths <- as.matrix(observed_deaths)

     # ============================================================================
     # Worker: run one replicate of the best model
     # ============================================================================
     run_best_model_replication <- function(replication_idx,
                                            best_model_row,
                                            config_base, priors_base, PATHS,
                                            col_params, col_seed, param_names) {

          # Base seed
          base_seed <- best_model_row[[col_seed]]
          if (is.na(base_seed) || base_seed <= 0 || base_seed > 2147483646) {
               base_seed <- sample.int(100000L, 1L)
               warning(paste("Invalid best-model seed detected; using random seed:", base_seed))
          }
          # Create a distinct, in-range seed for this replication
          rep_seed <- (as.integer(base_seed) + as.integer(replication_idx) * 1000L) %% 2147483647L
          if (rep_seed <= 0L) rep_seed <- rep_seed + 1L

          # Parameters of best model
          param_values <- unlist(best_model_row[, col_params, drop = TRUE])
          names(param_values) <- param_names

          # Build config for this replicate
          # (Relies on MOSAIC helpers available when library(MOSAIC) is loaded)
          tryCatch({
               sampling_flags <- extract_sampling_metadata(config_base)
               current_config <- convert_matrix_to_config(
                    param_vector   = param_values,
                    config_base    = config_base,
                    sampling_flags = sampling_flags
               )
               # Set replicate seed
               current_config$seed <- rep_seed

               # Python call
               lc <- reticulate::import("laser_cholera.metapop.model")
               model <- lc$run_model(paramfile = current_config, quiet = TRUE)

               if (!is.null(model) && !is.null(model$results)) {
                    out_cases  <- as.matrix(model$results$expected_cases)
                    out_deaths <- as.matrix(model$results$disease_deaths)
                    return(list(
                         cases      = out_cases,
                         deaths     = out_deaths,
                         seed       = rep_seed,
                         replication= replication_idx,
                         parameters = param_values
                    ))
               } else {
                    return(NULL)
               }
          }, error = function(e) {
               message("Failed replication ", replication_idx, ": ", e$message)
               NULL
          })
     }

     # ============================================================================
     # Execute replications
     # ============================================================================
     if (verbose) cat("Running best model replications...\n")
     start_time <- Sys.time()

     if (parallel && n_cores > 1) {
          if (verbose) cat("Setting up parallel cluster with", n_cores, "cores...\n")
          cl <- parallel::makeCluster(n_cores, type = "PSOCK")
          parallel::clusterEvalQ(cl, { library(MOSAIC); library(reticulate) })
          parallel::clusterExport(
               cl,
               c("best_model_row", "config_base", "priors_base", "PATHS",
                 "col_params", "col_seed", "param_names",
                 "run_best_model_replication"),
               envir = environment()
          )
          all_predictions <- pbapply::pblapply(
               X = seq_len(n_replications),
               FUN = function(i) run_best_model_replication(
                    i, best_model_row, config_base, priors_base, PATHS,
                    col_params, col_seed, param_names
               ),
               cl = cl
          )
          parallel::stopCluster(cl)
     } else {
          all_predictions <- pbapply::pblapply(
               X = seq_len(n_replications),
               FUN = function(i) run_best_model_replication(
                    i, best_model_row, config_base, priors_base, PATHS,
                    col_params, col_seed, param_names
               )
          )
     }

     runtime <- difftime(Sys.time(), start_time, units = "mins")
     if (verbose) cat("Completed in", round(as.numeric(runtime), 2), "minutes\n")

     # Keep valid replications
     valid_predictions <- all_predictions[!vapply(all_predictions, is.null, logical(1))]
     if (length(valid_predictions) == 0) stop("No valid best model replications generated")
     if (verbose) {
          cat("Generated", length(valid_predictions), "valid replications (",
              round(100 * length(valid_predictions) / n_replications, 1), "%)\n", sep = "")
     }

     # Summaries at 95% for convenience (plots can recompute other CIs on the fly)
     n_valid <- length(valid_predictions)
     cases_dims  <- dim(valid_predictions[[1]]$cases)
     deaths_dims <- dim(valid_predictions[[1]]$deaths)
     pred_cases_array  <- array(NA_real_, dim = c(n_valid, cases_dims[1],  cases_dims[2]))
     pred_deaths_array <- array(NA_real_, dim = c(n_valid, deaths_dims[1], deaths_dims[2]))
     for (i in seq_len(n_valid)) {
          pred_cases_array[i, , ]  <- valid_predictions[[i]]$cases
          pred_deaths_array[i, , ] <- valid_predictions[[i]]$deaths
     }

     summary_stats <- list(
          cases = list(
               mean  = apply(pred_cases_array,  c(2, 3), mean,    na.rm = TRUE),
               median= apply(pred_cases_array,  c(2, 3), median,  na.rm = TRUE),
               q025  = apply(pred_cases_array,  c(2, 3), quantile, 0.025, na.rm = TRUE),
               q975  = apply(pred_cases_array,  c(2, 3), quantile, 0.975, na.rm = TRUE),
               sd    = apply(pred_cases_array,  c(2, 3), sd,      na.rm = TRUE)
          ),
          deaths = list(
               mean  = apply(pred_deaths_array, c(2, 3), mean,    na.rm = TRUE),
               median= apply(pred_deaths_array, c(2, 3), median,  na.rm = TRUE),
               q025  = apply(pred_deaths_array, c(2, 3), quantile, 0.025, na.rm = TRUE),
               q975  = apply(pred_deaths_array, c(2, 3), quantile, 0.975, na.rm = TRUE),
               sd    = apply(pred_deaths_array, c(2, 3), sd,      na.rm = TRUE)
          )
     )

     result <- list(
          observed = list(
               cases  = observed_cases,
               deaths = observed_deaths
          ),
          predictions = valid_predictions,
          summary_stats = summary_stats,
          best_model = list(
               index      = best_idx,
               sim_id     = best_sim_id,
               seed       = best_seed,
               likelihood = best_likelihood,
               parameters = best_model_row[, col_params, drop = FALSE]
          ),
          diagnostics = list(
               n_requested_replications = n_replications,
               n_valid_predictions      = length(valid_predictions),
               success_rate             = length(valid_predictions) / n_replications,
               runtime_minutes          = as.numeric(runtime),
               parallel_used            = parallel && n_cores > 1,
               n_cores_used             = if (parallel && n_cores > 1) n_cores else 1,
               replication_seeds        = vapply(valid_predictions, `[[`, integer(1), "seed")
          )
     )

     if (verbose) {
          cat("Best model predictive check completed!\n")
          cat("- Best model: sim ", best_sim_id, " with likelihood ", sprintf("%.3f", best_likelihood), "\n", sep = "")
          cat("- Valid replications: ", result$diagnostics$n_valid_predictions, "\n", sep = "")
          cat("- Success rate: ", round(100 * result$diagnostics$success_rate, 1), "%\n", sep = "")
          cat("- Runtime: ", round(result$diagnostics$runtime_minutes, 2), " minutes\n", sep = "")
     }

     return(result)
}
