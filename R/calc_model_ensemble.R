#' Compute Weighted Ensemble Predictions from Multiple Parameter Sets
#'
#' @description
#' Runs LASER simulations for multiple parameter sets (with stochastic reruns
#' per set) and aggregates results using importance weights. Returns a
#' \code{mosaic_ensemble} object containing weighted mean, median, and quantile
#' envelopes for cases and deaths.
#'
#' This is the computation half of the ensemble workflow. Use
#' \code{\link{plot_model_ensemble}} to render plots from the returned object.
#'
#' @param config Base configuration object (provides observed data and template).
#' @param parameter_seeds Numeric vector of seeds for \code{\link{sample_parameters}}.
#'   Each seed generates a different parameter set.
#' @param configs List of pre-sampled configuration objects (direct mode).
#'   Mutually exclusive with \code{parameter_seeds}.
#' @param parameter_weights Numeric vector of importance weights, same length as
#'   \code{parameter_seeds} or \code{configs}. Normalized internally to sum to 1.
#'   If \code{NULL}, all parameter sets are weighted equally.
#' @param n_simulations_per_config Integer. Stochastic LASER reruns per parameter
#'   set. Default \code{10L}.
#' @param envelope_quantiles Numeric vector of quantiles for confidence intervals.
#'   Must be even length to form lower/upper pairs. Default
#'   \code{c(0.025, 0.25, 0.75, 0.975)} for 50\% and 95\% CIs.
#' @param PATHS List of paths from \code{\link{get_paths}}. Required for sampling mode.
#' @param priors Priors object for parameter sampling. Required for sampling mode.
#' @param sampling_args Named list of additional arguments for
#'   \code{\link{sample_parameters}}.
#' @param parallel Logical. Use parallel cluster for simulations. Default \code{FALSE}.
#' @param n_cores Integer or \code{NULL}. Number of cores when \code{parallel = TRUE}.
#' @param root_dir Character. MOSAIC root directory. Required when \code{parallel = TRUE}.
#' @param precomputed_results Optional list of pre-gathered LASER results (e.g. from Dask).
#'   Each element must have \code{$param_idx}, \code{$stoch_idx},
#'   \code{$reported_cases}, \code{$disease_deaths}, and \code{$success}.
#' @param verbose Logical. Print progress messages. Default \code{TRUE}.
#'
#' @return S3 object of class \code{"mosaic_ensemble"} containing:
#' \describe{
#'   \item{cases_mean}{Matrix (n_locations x n_time_points) of weighted mean cases.}
#'   \item{cases_median}{Matrix of weighted median cases.}
#'   \item{deaths_mean}{Matrix of weighted mean deaths.}
#'   \item{deaths_median}{Matrix of weighted median deaths.}
#'   \item{ci_bounds}{List of CI pairs, each with \code{$lower} and \code{$upper} matrices.}
#'   \item{obs_cases}{Observed cases matrix from config.}
#'   \item{obs_deaths}{Observed deaths matrix from config.}
#'   \item{cases_array}{4-D array (n_locations x n_time_points x n_param_sets x n_stoch).}
#'   \item{deaths_array}{4-D array matching cases_array dimensions.}
#'   \item{parameter_weights}{Normalized weight vector.}
#'   \item{n_param_sets}{Number of parameter sets.}
#'   \item{n_simulations_per_config}{Stochastic runs per parameter set.}
#'   \item{n_successful}{Number of successful simulations.}
#'   \item{location_names}{Character vector of location names.}
#'   \item{date_start}{Simulation start date.}
#'   \item{date_stop}{Simulation end date.}
#'   \item{envelope_quantiles}{Quantiles used for CI envelopes.}
#' }
#'
#' @seealso \code{\link{plot_model_ensemble}} to render plots from this object.
#'
#' @export
calc_model_ensemble <- function(config,
                                parameter_seeds = NULL,
                                configs = NULL,
                                parameter_weights = NULL,
                                n_simulations_per_config = 10L,
                                envelope_quantiles = c(0.025, 0.25, 0.75, 0.975),
                                PATHS = NULL,
                                priors = NULL,
                                sampling_args = list(),
                                parallel = FALSE,
                                n_cores = NULL,
                                root_dir = NULL,
                                precomputed_results = NULL,
                                verbose = TRUE) {

  # ===========================================================================
  # Input validation and mode determination
  # ===========================================================================

  if (missing(config) || is.null(config)) stop("config is required")

  if (!is.null(configs)) {
    # Direct mode: pre-sampled configs provided
    if (verbose) message("Using ", length(configs), " provided configurations")
    param_configs <- configs
    n_param_sets <- length(configs)

  } else if (!is.null(parameter_seeds)) {
    # Sampling mode: generate configs from seeds
    if (is.null(priors)) stop("priors required when using parameter_seeds")
    if (is.null(PATHS)) stop("PATHS required when using parameter_seeds")

    # Filter to non-zero weights if provided
    if (!is.null(parameter_weights)) {
      if (length(parameter_weights) != length(parameter_seeds))
        stop("parameter_weights must have same length as parameter_seeds")

      nonzero_idx <- parameter_weights > 0
      n_zero <- sum(!nonzero_idx)
      if (verbose && n_zero > 0) {
        message("  Filtering: ", sum(nonzero_idx), " non-zero weighted seeds (",
                n_zero, " zero-weight skipped)")
      }
      parameter_seeds <- parameter_seeds[nonzero_idx]
      parameter_weights <- parameter_weights[nonzero_idx]
      if (length(parameter_seeds) == 0) stop("No parameter sets with non-zero weights")
    }

    if (verbose) {
      message("Sampling ", length(parameter_seeds), " parameter sets (",
              length(parameter_seeds) * n_simulations_per_config, " total sims)...")
    }

    param_configs <- vector("list", length(parameter_seeds))
    if (verbose) {
      pb <- utils::txtProgressBar(min = 0, max = length(parameter_seeds), style = 1,
                                  char = "\u2588")
    }
    for (i in seq_along(parameter_seeds)) {
      if (verbose) utils::setTxtProgressBar(pb, i)
      param_configs[[i]] <- tryCatch(
        sample_parameters(PATHS = PATHS, priors = priors, config = config,
                          seed = parameter_seeds[i], sample_args = sampling_args,
                          verbose = FALSE),
        error = function(e) {
          warning("Failed to sample parameters with seed ", parameter_seeds[i],
                  ": ", e$message)
          NULL
        }
      )
    }
    if (verbose) close(pb)

    param_configs <- Filter(Negate(is.null), param_configs)
    n_param_sets <- length(param_configs)
    if (n_param_sets == 0) stop("All parameter sampling attempts failed")

    if (n_param_sets < length(parameter_seeds) && verbose) {
      message("Warning: ", length(parameter_seeds) - n_param_sets,
              " parameter sets failed to sample")
    }

  } else {
    stop("Must provide either 'configs' or 'parameter_seeds' (with config + priors + PATHS)")
  }

  # Validate and normalize parameter weights
  if (!is.null(parameter_weights)) {
    if (length(parameter_weights) != n_param_sets)
      stop("parameter_weights must have the same length as configs or parameter_seeds")
    if (any(!is.finite(parameter_weights)) || any(parameter_weights < 0))
      stop("parameter_weights must be non-negative finite values")
    parameter_weights <- parameter_weights / sum(parameter_weights)
  } else {
    parameter_weights <- rep(1 / n_param_sets, n_param_sets)
  }

  # Validate envelope quantiles
  if (length(envelope_quantiles) %% 2 != 0)
    stop("envelope_quantiles must have an even number of elements to form CI pairs")
  if (any(!is.finite(envelope_quantiles)) || any(envelope_quantiles < 0) ||
      any(envelope_quantiles > 1))
    stop("envelope_quantiles must be finite values between 0 and 1")
  if (any(diff(envelope_quantiles) <= 0))
    stop("envelope_quantiles must be in ascending order")

  if (parallel && is.null(root_dir))
    stop("root_dir is required when parallel = TRUE")

  # ===========================================================================
  # Extract metadata from first config
  # ===========================================================================

  first_config   <- param_configs[[1]]
  obs_cases      <- first_config$reported_cases
  obs_deaths     <- first_config$reported_deaths
  location_names <- first_config$location_name
  date_start     <- first_config$date_start
  date_stop      <- first_config$date_stop

  if (is.null(obs_cases) || is.null(obs_deaths))
    stop("configs must contain reported_cases and reported_deaths")

  if (is.null(location_names)) {
    n_locations    <- if (is.matrix(obs_cases)) nrow(obs_cases) else 1L
    location_names <- if (n_locations == 1L) "Location" else paste0("Location_", seq_len(n_locations))
  }

  n_locations   <- length(location_names)
  n_time_points <- if (is.matrix(obs_cases)) ncol(obs_cases) else length(obs_cases)
  total_sims    <- n_param_sets * n_simulations_per_config

  if (verbose) {
    message(sprintf("calc_model_ensemble: %d param sets x %d stochastic = %d total | %d location(s) | %d time steps",
                    n_param_sets, n_simulations_per_config, total_sims, n_locations, n_time_points))
  }

  # ===========================================================================
  # Run simulations (precomputed, parallel, or sequential)
  # ===========================================================================

  cases_array  <- array(NA_real_, dim = c(n_locations, n_time_points, n_param_sets, n_simulations_per_config))
  deaths_array <- array(NA_real_, dim = c(n_locations, n_time_points, n_param_sets, n_simulations_per_config))

  if (!is.null(precomputed_results)) {
    if (verbose) message("Using ", length(precomputed_results), " precomputed LASER results")
    for (result in precomputed_results) {
      if (isTRUE(result$success)) {
        p <- result$param_idx
        s <- result$stoch_idx
        if (is.matrix(result$reported_cases)) {
          cases_array[, , p, s]  <- result$reported_cases
          deaths_array[, , p, s] <- result$disease_deaths
        } else {
          cases_array[1L, , p, s]  <- result$reported_cases
          deaths_array[1L, , p, s] <- result$disease_deaths
        }
      }
    }
  } else {
    # Worker function (self-contained for parallel execution)
    run_param_stoch_simulation <- function(task_info, param_configs_list) {
      param_idx <- task_info$param_idx
      stoch_idx <- task_info$stoch_idx
      tryCatch({
        if (!exists("lc", where = .GlobalEnv, inherits = FALSE)) {
          lc <- reticulate::import("laser.cholera.metapop.model")
        } else {
          lc <- get("lc", envir = .GlobalEnv)
        }
        param_config <- param_configs_list[[param_idx]]
        param_config$seed <- (param_idx * 1000L) + stoch_idx
        model <- lc$run_model(
          paramfile = MOSAIC:::.mosaic_prepare_config_for_python(param_config),
          quiet = TRUE
        )
        result <- list(param_idx = param_idx, stoch_idx = stoch_idx,
                       reported_cases = model$results$reported_cases,
                       disease_deaths = model$results$disease_deaths,
                       success = TRUE)
        gc(verbose = FALSE)
        reticulate::import("gc")$collect()
        result
      }, error = function(e) {
        list(param_idx = param_idx, stoch_idx = stoch_idx,
             success = FALSE, error = as.character(e))
      })
    }

    task_list <- expand.grid(param_idx = seq_len(n_param_sets),
                             stoch_idx = seq_len(n_simulations_per_config))

    if (parallel) {
      n_cores_use <- if (is.null(n_cores)) max(1L, parallel::detectCores() - 1L) else n_cores

      Sys.setenv(TBB_NUM_THREADS = "1", NUMBA_NUM_THREADS = "1",
                 OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
                 OPENBLAS_NUM_THREADS = "1")

      cl <- parallel::makeCluster(n_cores_use, type = "PSOCK")
      on.exit(parallel::stopCluster(cl), add = TRUE)

      parallel::clusterEvalQ(cl, {
        .libPaths(c("~/R/library", .libPaths()))
        library(MOSAIC)
        library(reticulate)
        MOSAIC:::.mosaic_set_blas_threads(1L)
        Sys.setenv(TBB_NUM_THREADS = "1", NUMBA_NUM_THREADS = "1",
                   OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
                   OPENBLAS_NUM_THREADS = "1")
        lc <- reticulate::import("laser.cholera.metapop.model")
        assign("lc", lc, envir = .GlobalEnv)
        NULL
      })

      if (!is.null(root_dir)) {
        parallel::clusterCall(cl, function(rd) {
          MOSAIC::set_root_directory(rd)
          MOSAIC::get_paths()
        }, root_dir)
      }

      parallel::clusterExport(cl, c("param_configs"), envir = environment())

      if (verbose) message("Parallel execution on ", n_cores_use, " cores...")
      pbo <- pbapply::pboptions(type = "timer", char = "\u2588", style = 1)
      on.exit(pbapply::pboptions(pbo), add = TRUE)

      results_list <- pbapply::pblapply(
        split(task_list, seq_len(nrow(task_list))),
        function(row) run_param_stoch_simulation(row, param_configs),
        cl = cl
      )
    } else {
      if (verbose) message("Running ", total_sims, " simulations sequentially...")
      pbo <- pbapply::pboptions(type = "timer", char = "\u2588", style = 1)
      on.exit(pbapply::pboptions(pbo), add = TRUE)

      results_list <- pbapply::pblapply(
        split(task_list, seq_len(nrow(task_list))),
        function(row) run_param_stoch_simulation(row, param_configs)
      )
    }

    # Fill arrays from results
    for (result in results_list) {
      if (isTRUE(result$success)) {
        p <- result$param_idx
        s <- result$stoch_idx
        if (is.matrix(result$reported_cases)) {
          cases_array[, , p, s]  <- result$reported_cases
          deaths_array[, , p, s] <- result$disease_deaths
        } else {
          cases_array[1L, , p, s]  <- result$reported_cases
          deaths_array[1L, , p, s] <- result$disease_deaths
        }
      }
    }
  }

  # ===========================================================================
  # Count successful simulations
  # ===========================================================================

  n_successful <- sum(!is.na(cases_array[1L, 1L, , ]))
  if (n_successful == 0L) stop("All ensemble simulations failed")

  if (verbose) {
    message(sprintf("Ensemble complete: %d/%d successful (%.0f%%)",
                    n_successful, total_sims, 100 * n_successful / total_sims))
  }

  # ===========================================================================
  # Weighted statistics aggregation
  # ===========================================================================

  # Weighted quantile helper
  weighted_quantile <- function(x, weights, probs) {
    na_idx <- is.na(x) | is.na(weights)
    x <- x[!na_idx]; weights <- weights[!na_idx]
    if (length(x) == 0L || all(weights == 0)) return(rep(NA_real_, length(probs)))
    ord <- order(x); x <- x[ord]; weights <- weights[ord]
    weights <- weights / sum(weights)
    cum_weights <- cumsum(weights)
    vapply(probs, function(p) {
      if (p == 0) return(min(x))
      if (p == 1) return(max(x))
      idx <- which(cum_weights >= p)[1L]
      if (is.na(idx)) max(x) else x[idx]
    }, numeric(1L))
  }

  calculate_overall_stats <- function(data_array) {
    dims <- dim(data_array)
    n_locs <- dims[1L]; n_times <- dims[2L]
    n_params <- dims[3L]; n_stoch <- dims[4L]

    # Each param set's weight is divided equally among its stochastic runs
    sim_weights <- rep(parameter_weights, each = n_stoch) / n_stoch

    n_ci_pairs   <- length(envelope_quantiles) / 2L
    stats_mean   <- matrix(NA_real_, nrow = n_locs, ncol = n_times)
    stats_median <- matrix(NA_real_, nrow = n_locs, ncol = n_times)

    ci_bounds <- lapply(seq_len(n_ci_pairs), function(ci_idx) {
      list(lower = matrix(NA_real_, nrow = n_locs, ncol = n_times),
           upper = matrix(NA_real_, nrow = n_locs, ncol = n_times))
    })

    for (i in seq_len(n_locs)) {
      for (j in seq_len(n_times)) {
        values <- as.vector(data_array[i, j, , ])
        stats_mean[i, j]   <- sum(values * sim_weights, na.rm = TRUE)
        stats_median[i, j] <- weighted_quantile(values, sim_weights, 0.5)

        all_q <- weighted_quantile(values, sim_weights, envelope_quantiles)
        for (ci_idx in seq_len(n_ci_pairs)) {
          lower_idx <- ci_idx
          upper_idx <- length(envelope_quantiles) - ci_idx + 1L
          ci_bounds[[ci_idx]]$lower[i, j] <- all_q[lower_idx]
          ci_bounds[[ci_idx]]$upper[i, j] <- all_q[upper_idx]
        }
      }
    }

    list(mean = stats_mean, median = stats_median, ci_bounds = ci_bounds)
  }

  cases_stats  <- calculate_overall_stats(cases_array)
  deaths_stats <- calculate_overall_stats(deaths_array)

  # ===========================================================================
  # Return mosaic_ensemble object
  # ===========================================================================

  structure(
    list(
      cases_mean                = cases_stats$mean,
      cases_median              = cases_stats$median,
      deaths_mean               = deaths_stats$mean,
      deaths_median             = deaths_stats$median,
      ci_bounds                 = list(cases = cases_stats$ci_bounds,
                                       deaths = deaths_stats$ci_bounds),
      obs_cases                 = obs_cases,
      obs_deaths                = obs_deaths,
      cases_array               = cases_array,
      deaths_array              = deaths_array,
      parameter_weights         = parameter_weights,
      n_param_sets              = n_param_sets,
      n_simulations_per_config  = as.integer(n_simulations_per_config),
      n_successful              = n_successful,
      location_names            = location_names,
      n_locations               = n_locations,
      n_time_points             = n_time_points,
      date_start                = date_start,
      date_stop                 = date_stop,
      envelope_quantiles        = envelope_quantiles
    ),
    class = "mosaic_ensemble"
  )
}
