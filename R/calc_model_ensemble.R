# -----------------------------------------------------------------------------
# Dense-array OOM guard
# -----------------------------------------------------------------------------
# calc_model_ensemble() allocates TWO dense double arrays of shape
# [n_loc x n_time x n_param x n_stoch] on the single orchestrator node, then also
# holds the full gathered results list (each successful element carries its own
# [n_loc x n_time] cases + deaths matrices) at the same time. At a long (e.g.
# 2015) calibration window (~4320 time cols) with production defaults
# (max_best_subset = 1000, n_iter_ensemble = 10) this peaks ~40 GB and silently
# OOMs a 32 GB box, whereas a 2023 window (~1398 cols, ~13 GB) is fine.
#
# This is a WARN-ONLY guard (immediate.): it never changes behavior, perf, or
# allocation for any width — it merely projects the footprint and, when it would
# exceed ~80% of probed system RAM, emits a loud warning naming the projected GB
# and the knobs to dial down. When RAM cannot be probed portably (non-mac/Linux)
# the projection is skipped. Reuses the package RAM probe .psi_total_system_ram_gb().
#
# Projection: two dense arrays (cases + deaths) at 8 bytes/double, PLUS the
# gathered results list which holds the same payload a second time as per-result
# matrices (cases + deaths) before the arrays are filled — so the concurrent
# peak is ~2x the dense-array footprint. We use a factor of 2 for the list term
# (conservative; the arrays and list co-exist during the fill loop).
.MOSAIC_ENSEMBLE_RAM_FRACTION <- 0.80
.MOSAIC_ENSEMBLE_GATHERED_LIST_FACTOR <- 2

.mosaic_ensemble_ram_projection_gb <- function(n_locations, n_time_points,
                                               n_param_sets, n_stoch) {
  # One dense double array = n_loc * n_time * n_param * n_stoch * 8 bytes.
  one_array_gb <- (as.numeric(n_locations) * as.numeric(n_time_points) *
                     as.numeric(n_param_sets) * as.numeric(n_stoch) * 8) / 2^30
  # Two dense arrays (cases + deaths) + the gathered results list (which holds an
  # equivalent cases+deaths payload concurrently during the fill loop).
  dense_gb <- 2 * one_array_gb
  list_gb  <- .MOSAIC_ENSEMBLE_GATHERED_LIST_FACTOR * one_array_gb
  dense_gb + list_gb
}

# Warn (do NOT cap) when the dense-array + gathered-list footprint risks OOM.
# Returns the projected GB invisibly (for testability). total_ram_gb is injectable
# so the projection+warn logic can be unit-tested with a mocked probe.
.mosaic_ensemble_check_ram <- function(n_locations, n_time_points, n_param_sets,
                                       n_stoch, total_ram_gb = NULL) {
  proj_gb <- .mosaic_ensemble_ram_projection_gb(n_locations, n_time_points,
                                                n_param_sets, n_stoch)
  if (is.null(total_ram_gb)) total_ram_gb <- .psi_total_system_ram_gb()
  if (is.na(total_ram_gb)) return(invisible(proj_gb))  # un-probed platform: skip
  if (proj_gb > .MOSAIC_ENSEMBLE_RAM_FRACTION * total_ram_gb) {
    warning(sprintf(paste0(
      "calc_model_ensemble: the dense prediction arrays + gathered results list ",
      "project to ~%.1f GB on this orchestrator node (2 dense [%d x %d x %d x %d] ",
      "double arrays + the concurrent gathered list), but the system has ~%.0f GB ",
      "RAM -- this risks an out-of-memory failure. Reduce max_best_subset (fewer ",
      "parameter sets) and/or n_iter_ensemble (fewer stochastic reruns per set), or ",
      "run on a larger-memory host. This is the dominant memory cliff at long ",
      "(e.g. 2015) calibration windows."),
      proj_gb, n_locations, n_time_points, n_param_sets, n_stoch, total_ram_gb),
      immediate. = TRUE, call. = FALSE)
  }
  invisible(proj_gb)
}

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
#'   \code{c(0.025, 0.25, 0.75, 0.975)} for 50 and 95 percent CIs.
#' @param PATHS List of paths from \code{\link{get_paths}}. Required for sampling mode.
#' @param priors Priors object for parameter sampling. Required for sampling mode.
#' @param sampling_args Named list of additional arguments for
#'   \code{\link{sample_parameters}}.
#' @param n_cases_warmup_mask Integer. Number of LEADING cases timesteps that are
#'   an initial-condition warm-up transient (seeded E flushing into
#'   new_symptomatic before the SEIR dynamics settle). Default \code{2L}, matching
#'   \code{\link{plot_model_ensemble}}. This value is NOT applied to any of the
#'   returned series here; it is recorded in the returned \code{artifact_mask}
#'   element so downstream scoring (R2/bias) can exclude these positions. Set to
#'   \code{0L} to record "no cases warm-up mask".
#' @param mask_final_deaths_step Logical. If \code{TRUE} (default, matching
#'   \code{\link{plot_model_ensemble}}), record that the FINAL deaths timestep is
#'   a laser-cholera structural zero (\code{reported_deaths} written at tick
#'   then leading-trimmed, so the last slot is never written; laser issue #82).
#'   This value is NOT applied to any returned series here; it
#'   is recorded in the returned \code{artifact_mask} element for downstream
#'   scoring.
#' @param score_idx_cases,score_idx_deaths Integer (1-based). Per-channel scored
#'   time-window START index (burn-in / deaths-era start). Columns strictly
#'   BEFORE these indices are unscored and recorded in \code{artifact_mask} so
#'   R2/bias scoring drops them. Default \code{1L} (no-op). NOT applied to the
#'   returned series here.
#' @param parallel Logical. Use parallel cluster for simulations. Default \code{FALSE}.
#' @param n_cores Integer or \code{NULL}. Number of cores when \code{parallel = TRUE}.
#' @param root_dir Character. MOSAIC root directory. Required when \code{parallel = TRUE}.
#' @param precomputed_results Optional list of pre-gathered LASER results (e.g. from Dask).
#'   Each element must have \code{$param_idx}, \code{$stoch_idx},
#'   \code{$reported_cases}, \code{$reported_deaths}, and \code{$success}.
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
#'   \item{seeds}{Integer vector of per-member simulation seeds, aligned with the
#'     parameter dimension of \code{cases_array} (member \code{i} <-> \code{seeds[i]}). Bound to
#'     the parameter set that produced each member so consumers (e.g. medoid
#'     selection) need not rely on positional alignment with an external vector.}
#'   \item{n_param_sets}{Number of parameter sets.}
#'   \item{n_simulations_per_config}{Stochastic runs per parameter set.}
#'   \item{n_successful}{Number of successful simulations.}
#'   \item{location_names}{Character vector of location names.}
#'   \item{date_start}{Simulation start date.}
#'   \item{date_stop}{Simulation end date.}
#'   \item{envelope_quantiles}{Quantiles used for CI envelopes.}
#'   \item{artifact_mask}{List recording the engine-artifact masking spec for
#'     downstream scoring: \code{$cases_warmup} (integer, leading cases timesteps
#'     to exclude), \code{$deaths_final} (logical, exclude the final deaths
#'     timestep), and \code{$score_idx_cases}/\code{$score_idx_deaths} (integer,
#'     1-based per-channel scored-window start; columns before are dropped). The
#'     central/quantile/array fields above are RAW (unmasked); this spec is the
#'     contract scoring sites use to drop artifact positions.}
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
                                n_cases_warmup_mask = 2L,
                                mask_final_deaths_step = TRUE,
                                score_idx_cases = 1L,
                                score_idx_deaths = 1L,
                                parallel = FALSE,
                                n_cores = NULL,
                                root_dir = NULL,
                                precomputed_results = NULL,
                                verbose = TRUE) {

  # ===========================================================================
  # Input validation and mode determination
  # ===========================================================================

  if (missing(config) || is.null(config)) stop("config is required")

  # Engine-artifact mask spec (carried, not applied to returned series). Validate
  # identically to plot_model_ensemble() for consistency.
  n_cases_warmup_mask <- as.integer(n_cases_warmup_mask)
  if (length(n_cases_warmup_mask) != 1L || is.na(n_cases_warmup_mask) ||
      n_cases_warmup_mask < 0L)
    stop("n_cases_warmup_mask must be a single non-negative integer")
  if (length(mask_final_deaths_step) != 1L || is.na(mask_final_deaths_step) ||
      !is.logical(mask_final_deaths_step))
    stop("mask_final_deaths_step must be a single logical value")

  # Per-channel scored-window start indices (1-based). Default 1L => no-op
  # (existing ensembles/tests unchanged). Recorded in artifact_mask so the R2/
  # bias scoring sites drop the unscored head via .mosaic_mask_central_for_scoring().
  score_idx_cases  <- as.integer(score_idx_cases)
  score_idx_deaths <- as.integer(score_idx_deaths)
  if (length(score_idx_cases) != 1L || is.na(score_idx_cases) || score_idx_cases < 1L)
    stop("score_idx_cases must be a single integer >= 1")
  if (length(score_idx_deaths) != 1L || is.na(score_idx_deaths) || score_idx_deaths < 1L)
    stop("score_idx_deaths must be a single integer >= 1")

  if (!is.null(configs)) {
    # Direct mode: pre-sampled configs provided
    if (verbose) message("Using ", length(configs), " provided configurations")
    param_configs <- configs
    n_param_sets <- length(configs)

  } else if (!is.null(parameter_seeds)) {
    # Sampling mode: generate configs from seeds

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

    n_param_sets <- length(parameter_seeds)

    if (!is.null(precomputed_results)) {
      # Precomputed mode: skip sampling, metadata comes from base config
      if (verbose) {
        message("Precomputed results provided -- skipping parameter sampling for ",
                n_param_sets, " seeds")
      }
      param_configs <- list(config)

      # Defensive guard: each precomputed result is tagged with the param_idx
      # slice it fills; those slices must densely cover 1:n_param_sets so every
      # prediction pairs with its OWN parameter weight. A gap means configs were
      # dropped upstream while weights were not (the v0.35.1 misalignment class)
      # — warn loudly. Overflow is an outright contract violation (error).
      pidx <- vapply(precomputed_results, function(r) {
        if (is.null(r$param_idx)) NA_integer_ else as.integer(r$param_idx)
      }, integer(1))
      pidx <- pidx[is.finite(pidx)]
      if (length(pidx) > 0L) {
        if (max(pidx) > n_param_sets)
          stop(sprintf(paste0("precomputed_results param_idx max (%d) exceeds n_param_sets (%d): ",
                              "predictions and parameter weights are misaligned"),
                       max(pidx), n_param_sets))
        missing_p <- setdiff(seq_len(n_param_sets), unique(pidx))
        if (length(missing_p) > 0L)
          warning(sprintf(paste0("calc_model_ensemble: %d of %d parameter slices have no precomputed ",
                                "predictions (param_idx %s); their weights are redistributed"),
                          length(missing_p), n_param_sets, paste(missing_p, collapse = ",")))
      }

    } else {
      # Full sampling mode: generate configs from seeds for simulation
      if (is.null(priors)) stop("priors required when using parameter_seeds")
      if (is.null(PATHS)) stop("PATHS required when using parameter_seeds")

      if (verbose) {
        message("Sampling ", n_param_sets, " parameter sets (",
                n_param_sets * n_simulations_per_config, " total sims)...")
      }

      param_configs <- vector("list", n_param_sets)
      if (verbose) {
        pb <- utils::txtProgressBar(min = 0, max = n_param_sets, style = 1,
                                    char = "\u2588")
      }
      for (i in seq_along(parameter_seeds)) {
        if (verbose) utils::setTxtProgressBar(pb, i)
        param_configs[[i]] <- tryCatch(
          .mosaic_clamp_transmission_params(
            sample_parameters(PATHS = PATHS, priors = priors, config = config,
                              seed = parameter_seeds[i], sample_args = sampling_args,
                              verbose = FALSE)),
          error = function(e) {
            warning("Failed to sample parameters with seed ", parameter_seeds[i],
                    ": ", e$message)
            NULL
          }
        )
      }
      if (verbose) close(pb)

      # Drop failed re-samples AND their seeds/weights in lockstep, then let the
      # validation block below renormalize. Degrades gracefully to the survivors
      # instead of erroring on the length mismatch — one transient sampling
      # failure must not discard the entire posterior ensemble.
      keep <- !vapply(param_configs, is.null, logical(1))
      param_configs <- param_configs[keep]
      n_param_sets <- length(param_configs)
      if (n_param_sets == 0) stop("All parameter sampling attempts failed")

      if (any(!keep)) {
        parameter_seeds <- parameter_seeds[keep]
        if (!is.null(parameter_weights)) parameter_weights <- parameter_weights[keep]
        warning(sprintf(
          "calc_model_ensemble: %d of %d parameter sets failed to sample; proceeding on the %d that succeeded",
          sum(!keep), length(keep), n_param_sets))
      }
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
  # OOM guard: project the dense-array + gathered-list footprint and warn loudly
  # (warn-only, no behavior change) before the large allocation below.
  # ===========================================================================

  .mosaic_ensemble_check_ram(n_locations, n_time_points, n_param_sets,
                             n_simulations_per_config)

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
          deaths_array[, , p, s] <- result$reported_deaths
        } else {
          cases_array[1L, , p, s]  <- result$reported_cases
          deaths_array[1L, , p, s] <- result$reported_deaths
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
          MOSAIC:::.mosaic_strip_laser_file_handler()
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
                       reported_deaths = model$results$reported_deaths,
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

      MOSAIC:::.mosaic_set_all_thread_env(1L)

      cl <- parallel::makeCluster(n_cores_use, type = "PSOCK")
      on.exit(parallel::stopCluster(cl), add = TRUE)

      # Workers load the same MOSAIC build as this parent (no hardcoded path).
      .parent_libs <- .libPaths()
      parallel::clusterExport(cl, ".parent_libs", envir = environment())
      parallel::clusterEvalQ(cl, {
        .libPaths(unique(c(.parent_libs, .libPaths())))
        library(MOSAIC)
        library(reticulate)
        MOSAIC:::.mosaic_set_blas_threads(1L)
        lc <- reticulate::import("laser.cholera.metapop.model")
        MOSAIC:::.mosaic_strip_laser_file_handler()
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
      # In-process LASER: pin threads so numba/MKL/OpenBLAS don't oversubscribe.
      # The parallel branch pins its workers; run_MOSAIC pins the orchestrator;
      # pin here too so standalone calc_model_ensemble(parallel=FALSE) is
      # self-sufficient. Idempotent.
      MOSAIC:::.mosaic_set_blas_threads(1L)
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
          deaths_array[, , p, s] <- result$reported_deaths
        } else {
          cases_array[1L, , p, s]  <- result$reported_cases
          deaths_array[1L, , p, s] <- result$reported_deaths
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

  calculate_overall_stats <- function(data_array) {
    dims <- dim(data_array)
    n_locs <- dims[1L]; n_times <- dims[2L]
    n_params <- dims[3L]; n_stoch <- dims[4L]

    # Each param set's weight is divided equally among its stochastic runs.
    # `times` (NOT `each`): as.vector(data_array[i, j, , ]) flattens the
    # [param, stoch] slice param-fastest, so the weight vector must repeat
    # param-fastest to pair every (param, stoch) prediction with its own
    # parameter's weight. Using `each` mis-pairs them when n_stoch > 1.
    sim_weights <- rep(parameter_weights, times = n_stoch) / n_stoch

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
        # Failed sims show up as NA. Drop them from the mean and
        # renormalize the surviving weights so a 10% failure rate
        # doesn't bias the ensemble mean toward zero. (The median
        # path already filters and renormalizes inside
        # weighted_quantiles, so the two stats stay consistent.)
        valid <- is.finite(values) & is.finite(sim_weights) & sim_weights > 0
        w_sum <- if (any(valid)) sum(sim_weights[valid]) else 0
        stats_mean[i, j]   <- if (w_sum > 0) sum(values[valid] * sim_weights[valid]) / w_sum else NA_real_
        # One weighted_quantiles call for median + envelope (single sort, not two)
        qv <- weighted_quantiles(values, sim_weights, c(0.5, envelope_quantiles))
        stats_median[i, j] <- qv[1]
        all_q <- qv[-1]
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
  # Per-member seeds, aligned with cases_array's param dimension (member i <->
  # seeds[i]). Bound to the parameter set that PRODUCED each member so downstream
  # consumers (medoid selection, optimize_ensemble_subset) never have to rely on
  # positional alignment with a separately-passed seed vector -- the failure mode
  # that mis-mapped the medoid to a collapsed member. Precedence:
  #   (a) precomputed (Dask) results carry their own config seed (param_seed)
  #       tagged with the param_idx they fill -- ground truth for that member;
  #   (b) the per-member config seed (local-sampling and direct-config paths build
  #       cases_array member i from param_configs[[i]], whose $seed is authoritative);
  #   (c) last-resort positional fallback to the supplied parameter_seeds.
  # ===========================================================================

  member_seeds <- rep(NA_integer_, n_param_sets)
  if (!is.null(precomputed_results)) {
    for (r in precomputed_results) {
      p  <- suppressWarnings(as.integer(r$param_idx)[1])
      ps <- suppressWarnings(as.integer(r$param_seed)[1])
      if (length(p) && !is.na(p) && p >= 1L && p <= n_param_sets &&
          length(ps) && !is.na(ps)) {
        member_seeds[p] <- ps
      }
    }
  }
  if (anyNA(member_seeds) && length(param_configs) == n_param_sets) {
    for (i in seq_len(n_param_sets)) {
      if (is.na(member_seeds[i])) {
        s <- param_configs[[i]]$seed
        if (!is.null(s) && length(s)) member_seeds[i] <- suppressWarnings(as.integer(s)[1])
      }
    }
  }
  if (anyNA(member_seeds) && !is.null(parameter_seeds) &&
      length(parameter_seeds) == n_param_sets) {
    na_i <- is.na(member_seeds)
    member_seeds[na_i] <- suppressWarnings(as.integer(parameter_seeds))[na_i]
  }

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
      seeds                     = member_seeds,
      n_param_sets              = n_param_sets,
      n_simulations_per_config  = as.integer(n_simulations_per_config),
      n_successful              = n_successful,
      location_names            = location_names,
      n_locations               = n_locations,
      n_time_points             = n_time_points,
      date_start                = date_start,
      date_stop                 = date_stop,
      envelope_quantiles        = envelope_quantiles,
      artifact_mask             = list(
        cases_warmup     = as.integer(n_cases_warmup_mask),
        deaths_final     = isTRUE(mask_final_deaths_step),
        score_idx_cases  = score_idx_cases,
        score_idx_deaths = score_idx_deaths
      )
    ),
    class = "mosaic_ensemble"
  )
}
