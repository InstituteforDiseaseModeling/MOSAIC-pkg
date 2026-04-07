#' Run Stochastic Ensemble Simulations for Best Model
#'
#' @description
#' Runs \code{n_simulations} stochastic LASER simulations of a single model
#' configuration (typically \code{config_best}) with different random seeds and
#' aggregates the results into mean predictions and uncertainty envelopes.
#'
#' Separating computation from plotting allows ensemble results to be used for
#' R², bias ratios, windowed fit metrics, and summary statistics without
#' necessarily generating plots, and avoids running simulations twice when both
#' metrics and plots are needed.
#'
#' @param config Named list. Model configuration as returned by
#'   \code{\link{sample_parameters}} (typically \code{config_best}).
#' @param n_simulations Integer. Number of stochastic LASER simulations to run.
#'   Default \code{100L}.
#' @param envelope_quantiles Numeric vector of length 2. Lower and upper quantiles
#'   for uncertainty envelopes. Stored in return object for use by
#'   \code{\link{plot_model_ensemble}}. Default \code{c(0.025, 0.975)}.
#' @param parallel Logical. Use R parallel cluster for simulations. Default
#'   \code{FALSE}.
#' @param n_cores Integer or \code{NULL}. Number of cores when
#'   \code{parallel = TRUE}. \code{NULL} uses \code{detectCores() - 1}.
#' @param root_dir Character. MOSAIC root directory. Required when
#'   \code{parallel = TRUE} so workers can call \code{set_root_directory()}.
#' @param verbose Logical. Print progress messages. Default \code{TRUE}.
#' @param precomputed_results Optional list of pre-gathered LASER results (e.g. from Dask).
#'   Each element must have \code{$reported_cases}, \code{$disease_deaths}, and \code{$success}.
#'   When provided, local LASER execution is skipped entirely.
#'
#' @return A named list (S3 class \code{"mosaic_ensemble"}) containing:
#' \describe{
#'   \item{cases_stats}{List with \code{mean}, \code{median}, \code{lower},
#'     \code{upper} matrices (n_locations × n_time_points).}
#'   \item{deaths_stats}{Same structure as \code{cases_stats} for deaths.}
#'   \item{cases_array}{3-D array (n_locations × n_time_points × n_successful)
#'     of raw per-simulation case counts.}
#'   \item{deaths_array}{Same for deaths.}
#'   \item{obs_cases}{Observed cases matrix from \code{config$reported_cases}.}
#'   \item{obs_deaths}{Observed deaths matrix from \code{config$reported_deaths}.}
#'   \item{n_simulations}{Requested number of simulations.}
#'   \item{n_successful}{Number that completed without error.}
#'   \item{seeds}{Integer vector of seeds used.}
#'   \item{location_names}{Character vector of location names.}
#'   \item{n_locations}{Number of locations.}
#'   \item{n_time_points}{Number of time steps.}
#'   \item{envelope_quantiles}{The quantile pair passed in (for plotting).}
#'   \item{date_start}{Character. Simulation start date from config.}
#'   \item{date_stop}{Character. Simulation end date from config.}
#' }
#'
#' @seealso \code{\link{plot_model_ensemble}} to render plots from this object.
#'
#' @export
calc_model_ensemble <- function(config,
                                n_simulations      = 100L,
                                envelope_quantiles = c(0.025, 0.975),
                                parallel           = FALSE,
                                n_cores            = NULL,
                                root_dir           = NULL,
                                verbose            = TRUE,
                                precomputed_results = NULL) {

  # ---------------------------------------------------------------------------
  # Input validation
  # ---------------------------------------------------------------------------

  if (missing(config) || is.null(config)) stop("config is required")

  if (!is.numeric(n_simulations) || length(n_simulations) != 1L || n_simulations < 2L)
    stop("n_simulations must be an integer >= 2")

  if (length(envelope_quantiles) != 2L || envelope_quantiles[1L] >= envelope_quantiles[2L])
    stop("envelope_quantiles must be a length-2 vector with first value < second value")

  if (parallel && is.null(root_dir))
    stop("root_dir is required when parallel = TRUE")

  # ---------------------------------------------------------------------------
  # Extract observed data and metadata from config
  # ---------------------------------------------------------------------------

  obs_cases    <- config$reported_cases
  obs_deaths   <- config$reported_deaths
  location_names <- config$location_name

  if (is.null(obs_cases) || is.null(obs_deaths))
    stop("config must contain reported_cases and reported_deaths")

  if (is.null(location_names)) {
    n_locations    <- if (is.matrix(obs_cases)) nrow(obs_cases) else 1L
    location_names <- if (n_locations == 1L) "Location" else paste0("Location_", seq_len(n_locations))
    if (verbose) message("Warning: no location names in config. Using defaults.")
  }

  n_locations   <- length(location_names)
  n_time_points <- if (is.matrix(obs_cases)) ncol(obs_cases) else length(obs_cases)
  seeds         <- seq_len(n_simulations)

  if (verbose) {
    message(sprintf("calc_model_ensemble: %d sims | %d location(s) | %d time steps",
                    n_simulations, n_locations, n_time_points))
  }

  # ---------------------------------------------------------------------------
  # Worker function (self-contained for parallel execution)
  # ---------------------------------------------------------------------------

  run_single_simulation <- function(seed_i, config_template) {
    tryCatch({
      lc       <- reticulate::import("laser.cholera.metapop.model")
      config_i <- config_template
      config_i$seed <- seed_i
      model <- lc$run_model(
        paramfile = MOSAIC:::.mosaic_prepare_config_for_python(config_i),
        quiet = TRUE
      )
      result <- list(
        reported_cases = model$results$reported_cases,
        disease_deaths = model$results$disease_deaths,
        success        = TRUE,
        seed           = seed_i
      )
      gc(verbose = FALSE)
      reticulate::import("gc")$collect()
      result
    }, error = function(e) {
      list(success = FALSE, seed = seed_i, error = as.character(e))
    })
  }

  # ---------------------------------------------------------------------------
  # Run simulations
  # ---------------------------------------------------------------------------

  if (!is.null(precomputed_results)) {
    # Use pre-gathered LASER results (e.g. from Dask) instead of running locally.
    # Expected format: list of lists, each with $reported_cases, $disease_deaths, $success
    if (verbose) message("Using ", length(precomputed_results), " precomputed LASER results (skipping local execution)")
    simulation_results <- precomputed_results
    n_simulations <- length(simulation_results)
    seeds <- seq_len(n_simulations)

  } else {

  if (verbose) message("Running ", n_simulations, " stochastic LASER simulations...")

  pbo <- pbapply::pboptions(type = "timer", char = "\u2588", style = 1)
  on.exit(pbapply::pboptions(pbo), add = TRUE)

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
    })

    if (!is.null(root_dir)) {
      parallel::clusterCall(cl, function(rd) {
        MOSAIC::set_root_directory(rd)
        MOSAIC::get_paths()
      }, root_dir)
    }

    parallel::clusterExport(cl, c("config", "run_single_simulation"), envir = environment())

    if (verbose) message("Parallel execution on ", n_cores_use, " cores...")
    simulation_results <- pbapply::pblapply(
      seeds, function(s) run_single_simulation(s, config), cl = cl
    )

  } else {

    simulation_results <- pbapply::pblapply(
      seeds, function(s) run_single_simulation(s, config)
    )
  }

  } # end precomputed_results else

  # ---------------------------------------------------------------------------
  # Filter successful results
  # ---------------------------------------------------------------------------

  successful_results <- simulation_results[vapply(simulation_results,
                                                   function(x) isTRUE(x$success),
                                                   logical(1L))]
  n_successful <- length(successful_results)

  if (n_successful == 0L) {
    failed <- simulation_results[vapply(simulation_results,
                                        function(x) !is.null(x$error), logical(1L))]
    sample_errors <- unique(vapply(failed[seq_len(min(3L, length(failed)))],
                                   function(x) x$error, character(1L)))
    stop("All ensemble simulations failed.\n  ", paste(sample_errors, collapse = "\n  "))
  }

  if (verbose) {
    message(sprintf("Ensemble complete: %d/%d successful (%.0f%%)",
                    n_successful, n_simulations,
                    100 * n_successful / n_simulations))
  }

  # ---------------------------------------------------------------------------
  # Aggregate into arrays
  # ---------------------------------------------------------------------------

  cases_array  <- array(NA_real_, dim = c(n_locations, n_time_points, n_successful))
  deaths_array <- array(NA_real_, dim = c(n_locations, n_time_points, n_successful))

  for (i in seq_len(n_successful)) {
    r <- successful_results[[i]]
    if (is.matrix(r$reported_cases)) {
      cases_array[, , i]  <- r$reported_cases
      deaths_array[, , i] <- r$disease_deaths
    } else {
      cases_array[1L, , i]  <- r$reported_cases
      deaths_array[1L, , i] <- r$disease_deaths
    }
  }

  # ---------------------------------------------------------------------------
  # Compute summary statistics across simulations (3rd dimension)
  # ---------------------------------------------------------------------------

  .calc_stats <- function(arr) {
    m <- apply(arr, c(1L, 2L), mean,   na.rm = TRUE)
    d <- apply(arr, c(1L, 2L), median, na.rm = TRUE)
    lo <- apply(arr, c(1L, 2L), quantile,
                probs = envelope_quantiles[1L], na.rm = TRUE)
    hi <- apply(arr, c(1L, 2L), quantile,
                probs = envelope_quantiles[2L], na.rm = TRUE)
    # Always return matrices even for single-location configs
    if (!is.matrix(m)) { m <- matrix(m, nrow = 1L); d <- matrix(d, nrow = 1L)
                          lo <- matrix(lo, nrow = 1L); hi <- matrix(hi, nrow = 1L) }
    list(mean = m, median = d, lower = lo, upper = hi)
  }

  cases_stats  <- .calc_stats(cases_array)
  deaths_stats <- .calc_stats(deaths_array)

  # ---------------------------------------------------------------------------
  # Return ensemble object
  # ---------------------------------------------------------------------------

  structure(
    list(
      cases_stats        = cases_stats,
      deaths_stats       = deaths_stats,
      cases_array        = cases_array,
      deaths_array       = deaths_array,
      obs_cases          = obs_cases,
      obs_deaths         = obs_deaths,
      n_simulations      = n_simulations,
      n_successful       = n_successful,
      seeds              = seeds,
      location_names     = location_names,
      n_locations        = n_locations,
      n_time_points      = n_time_points,
      envelope_quantiles = envelope_quantiles,
      date_start         = config$date_start,
      date_stop          = config$date_stop
    ),
    class = "mosaic_ensemble"
  )
}
