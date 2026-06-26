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

# Comprehensive default set of internal LASER `model$results` channels captured
# for the trajectory figures (the "related quantities" beyond reported cases/
# deaths). reported_cases/reported_deaths are NOT here -- they are sourced from
# the ensemble cases_array/deaths_array (no re-capture). Each is harvested
# defensively per worker (tryCatch -> omit if the engine build lacks it), so an
# absent channel degrades gracefully. RAM/payload is LINEAR in this set's length
# -- it is the documented `trajectory_channels` lever (PLAN sec 14.G/14.H).
.MOSAIC_TRAJECTORY_CHANNELS_DEFAULT <- c(
  # compartments (people-level state stocks)
  "S", "E", "Isym", "Iasym", "R", "V1", "V2", "W", "N",
  # force of infection & per-time transmission rates (hazards)
  "Lambda", "Psi", "beta_jt_human", "beta_jt_env",
  # incidence & infection flows
  "incidence", "incidence_human", "incidence_env", "new_symptomatic",
  # burden channels (expected_cases = new_symptomatic/rho; disease_deaths = true burden)
  "expected_cases", "disease_deaths"
)

.mosaic_ensemble_ram_projection_gb <- function(n_locations, n_time_points,
                                               n_param_sets, n_stoch,
                                               n_capture_channels = 0L,
                                               capture_in_gather = FALSE) {
  # One dense double array = n_loc * n_time * n_param * n_stoch * 8 bytes.
  one_array_gb <- (as.numeric(n_locations) * as.numeric(n_time_points) *
                     as.numeric(n_param_sets) * as.numeric(n_stoch) * 8) / 2^30
  # Two dense arrays (cases + deaths) + the gathered results list (which holds an
  # equivalent cases+deaths payload concurrently during the fill loop).
  dense_gb <- 2 * one_array_gb
  list_gb  <- .MOSAIC_ENSEMBLE_GATHERED_LIST_FACTOR * one_array_gb
  # Trajectory capture is STREAM-TO-DISK (PLAN 14.G): on the LOCAL PSOCK/sequential
  # path channels are spilled to a per-sim scratch file at sim time and NEVER
  # accumulated on the gathered list, so peak capture RAM is ONE transient
  # [n_loc x n_time x n_param x n_stoch] array the channel-by-channel reduce
  # builds-and-frees one channel at a time -- FLAT in the channel count (the
  # scratch DISK footprint, not RAM, scales with n_capture_channels). On the DASK
  # path, however, the gathered `precomputed_results` list transiently holds ALL
  # n_capture_channels per record on the client BEFORE they are streamed to
  # scratch (capture_in_gather = TRUE), so the honest client-RAM term adds
  # n_capture_channels * one_array (this is why the OOM warning must see it).
  capture_gb <- if (n_capture_channels > 0L) one_array_gb else 0
  if (isTRUE(capture_in_gather))
    list_gb <- list_gb + as.numeric(n_capture_channels) * one_array_gb
  dense_gb + list_gb + capture_gb
}

# Warn (do NOT cap) when the dense-array + gathered-list footprint risks OOM.
# Returns the projected GB invisibly (for testability). total_ram_gb is injectable
# so the projection+warn logic can be unit-tested with a mocked probe.
.mosaic_ensemble_check_ram <- function(n_locations, n_time_points, n_param_sets,
                                       n_stoch, total_ram_gb = NULL,
                                       n_capture_channels = 0L,
                                       capture_in_gather = FALSE) {
  proj_gb <- .mosaic_ensemble_ram_projection_gb(n_locations, n_time_points,
                                                n_param_sets, n_stoch,
                                                n_capture_channels,
                                                capture_in_gather)
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

# -----------------------------------------------------------------------------
# Worker-death-robust parallel gather (PSOCK)
# -----------------------------------------------------------------------------
# `parallel::parLapply()` / `pbapply::pblapply(cl=)` gather results with a
# BLOCKING `unserialize(node$con)`. If a PSOCK worker PROCESS dies mid-task --
# a segfault / fatal C-level abort in the embedded Python (numba/laser) runtime,
# an OOM kill, etc. (NOT an R-level error, which the worker's tryCatch already
# turns into a `success = FALSE` record) -- the behaviour is platform-dependent:
#   * macOS surfaces the half-closed socket as "error reading from connection";
#   * Linux can BLOCK FOREVER on the dead peer's socket.
# The Linux block is the observed "0 CPU on master AND all workers, frozen"
# deadlock: a worker crashed, every other worker finished, and the master is
# parked in unserialize() on the dead socket with no timeout.
#
# This helper replaces the blocking gather with a load-balanced dispatch that
# waits on the worker sockets with `socketSelect(timeout=)`. If NO worker
# produces a result within `idle_timeout_sec` of continuous silence (every
# in-flight worker has gone unresponsive) it tears the cluster down and
# `stop()`s with a diagnostic naming the stalled tasks -- surfacing the failure
# instead of hanging the master indefinitely. A worker that dies but whose
# socket reports readable (EOF / reset) is caught per-task and recorded as a
# failed result so the run degrades gracefully on the survivors.
#
# `socketSelect` is base R. The PSOCK send/recv primitives are accessed via
# `getFromNamespace()` (R CMD check clean; they are stable internal entry
# points used by parallel's own clusterApplyLB).
.mosaic_cluster_lapply_robust <- function(cl, X, fun, idle_timeout_sec = 1800,
                                          progress = TRUE,
                                          label = "calc_model_ensemble") {
  n <- length(X)
  if (n == 0L) return(list())

  sendCall <- utils::getFromNamespace("sendCall", "parallel")
  recvData <- utils::getFromNamespace("recvData", "parallel")

  n_workers <- length(cl)
  results <- vector("list", n)
  # node_task[w] = index into X currently in flight on worker w (NA if idle).
  node_task <- rep(NA_integer_, n_workers)
  next_task <- 1L
  n_done <- 0L
  pb <- NULL
  if (isTRUE(progress)) {
    pb <- utils::txtProgressBar(min = 0, max = n, style = 3, char = "█")
    on.exit(close(pb), add = TRUE)
  }

  # dead[w] = TRUE once worker w's process is known to have crashed; such a
  # worker is never sent another task.
  dead <- logical(n_workers)

  dispatch_to_idle <- function() {
    # Send queued tasks to any worker that is alive AND idle.
    idle <- which(is.na(node_task) & !dead)
    for (w in idle) {
      if (next_task > n) break
      sendCall(cl[[w]], fun, list(X[[next_task]]), tag = next_task)
      node_task[w] <<- next_task
      next_task <<- next_task + 1L
    }
  }

  # Prime: one task per worker (load-balanced).
  dispatch_to_idle()

  while (n_done < n) {
    busy_w <- which(!is.na(node_task))
    if (length(busy_w) == 0L) {
      # No worker holds a task but work remains -> every worker has died.
      stop(sprintf(paste0(
        "%s parallel gather: all %d worker(s) crashed with %d task(s) ",
        "unfinished. A worker process died (fatal abort in the embedded Python/numba engine ",
        "or an OOM kill), not an R-level error. Re-run with parallel = FALSE to surface the ",
        "underlying engine error, or reduce n_cores to lower memory pressure."),
        label, n_workers, n - n_done), call. = FALSE)
    }
    cons <- lapply(cl[busy_w], function(node) node$con)
    # Wait for any busy worker's socket to become readable, with a finite
    # timeout so a dead/silent peer cannot block the master forever. This is the
    # core deadlock guard: a blocking unserialize() on a crashed worker's socket
    # is what hangs the master indefinitely on Linux.
    ready <- socketSelect(cons, write = FALSE, timeout = idle_timeout_sec)
    if (!any(ready)) {
      stalled <- node_task[busy_w]
      stop(sprintf(paste0(
        "%s parallel gather stalled: no worker produced a result ",
        "within %d s. %d task(s) still in flight (indices: %s) on worker(s) that have gone ",
        "unresponsive -- a worker process most likely crashed (a fatal abort in the embedded ",
        "Python/numba engine or an OOM kill). The cluster is being torn down. Re-run with ",
        "parallel = FALSE to surface the underlying engine error, or reduce n_cores to lower ",
        "memory pressure."),
        label, as.integer(idle_timeout_sec), length(stalled),
        paste(stalled, collapse = ", ")),
        call. = FALSE)
    }
    # Drain every ready worker this round. Read from each ready node SPECIFICALLY
    # (recvData(node), not recvOneResult(cl)) so a failed read is attributed to
    # the correct worker -- the in-flight task on THAT node.
    for (k in which(ready)) {
      w <- busy_w[k]
      task_w <- node_task[w]
      res <- tryCatch(recvData(cl[[w]]), error = function(e) e)
      if (inherits(res, "error")) {
        # Socket reported readable but the read failed (EOF / reset): the worker
        # process died. Record the in-flight task as failed and retire the worker.
        results[[task_w]] <- list(.mosaic_worker_died = TRUE,
                                  success = FALSE,
                                  error = conditionMessage(res))
        node_task[w] <- NA_integer_
        dead[w] <- TRUE
        n_done <- n_done + 1L
        if (!is.null(pb)) utils::setTxtProgressBar(pb, n_done)
        next
      }
      results[[task_w]] <- res$value
      node_task[w] <- NA_integer_
      n_done <- n_done + 1L
      if (!is.null(pb)) utils::setTxtProgressBar(pb, n_done)
    }
    # Feed all freed (alive) workers their next queued tasks.
    dispatch_to_idle()
  }
  results
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
#' @param capture_trajectories Logical. When \code{TRUE}, harvest the
#'   comprehensive internal-state channels (\code{trajectory_channels}) from each
#'   member and attach a compact \code{$trajectories} (\code{mosaic_trajectories})
#'   object -- per-channel weighted median + a uniform-thinned set of actual
#'   member trajectories + derived series (I_total, mass_balance, CFR, epidemic
#'   frac). Default \code{FALSE} (\code{run_MOSAIC()} enables it for the posterior
#'   ensemble; never for the medoid). RAM/payload is linear in
#'   \code{length(trajectory_channels)}.
#' @param trajectory_channels Character vector of \code{model$results} channels to
#'   capture when \code{capture_trajectories = TRUE}. Default
#'   \code{.MOSAIC_TRAJECTORY_CHANNELS_DEFAULT} (the comprehensive set). The
#'   documented RAM lever -- shorten it to reduce capture cost.
#' @param trajectory_n_lines Integer. Number of uniform-thinned member
#'   trajectories retained per location for the spaghetti display. Default 150.
#' @param trajectory_scratch_dir Character or \code{NULL}. Directory for the
#'   per-sim trajectory-channel scratch spill (stream-to-disk capture). When
#'   \code{NULL} and capturing, a temporary directory is created. When provided,
#'   the CALLER owns its lifecycle (it is not auto-deleted) -- used by
#'   \code{run_MOSAIC()} to reduce over the optimized subset post-optimization.
#' @param reduce_trajectories Logical. When \code{TRUE} (default) the trajectory
#'   reduction runs here over all members and is attached as \code{$trajectories}.
#'   When \code{FALSE}, the channels are spilled to scratch but NOT reduced; the
#'   scratch handle is returned in \code{$trajectory_scratch} so the caller can
#'   reduce over a final (e.g. optimized) subset without re-simulating.
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
                                capture_trajectories = FALSE,
                                trajectory_channels = .MOSAIC_TRAJECTORY_CHANNELS_DEFAULT,
                                trajectory_n_lines = 150L,
                                trajectory_scratch_dir = NULL,
                                reduce_trajectories = TRUE,
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
                             n_simulations_per_config,
                             n_capture_channels = if (isTRUE(capture_trajectories))
                               length(trajectory_channels) else 0L,
                             # Dask path: the gathered precomputed_results list holds
                             # all captured channels on the client before spill (NOT
                             # flat); local PSOCK spills at sim time (flat).
                             capture_in_gather = isTRUE(capture_trajectories) &&
                               !is.null(precomputed_results))

  # ===========================================================================
  # Run simulations (precomputed, parallel, or sequential)
  # ===========================================================================

  cases_array  <- array(NA_real_, dim = c(n_locations, n_time_points, n_param_sets, n_simulations_per_config))
  deaths_array <- array(NA_real_, dim = c(n_locations, n_time_points, n_param_sets, n_simulations_per_config))

  # ---------------------------------------------------------------------------
  # Trajectory capture is STREAM-TO-DISK (capture-don't-replay, PLAN 14.G/14.H):
  # each sim's comprehensive internal-state channels are spilled to a per-sim
  # scratch RDS keyed by (param_idx, stoch_idx) AT SIM TIME -- never accumulated
  # in RAM on the gathered list, never re-simulated. The local PSOCK/sequential
  # workers write scratch directly; the Dask precomputed path streams each
  # record's channels to scratch as it is consumed and drops them. The reducer
  # (.mosaic_build_trajectories) reads the scratch back channel-by-channel,
  # bounding peak RAM to ONE dense array regardless of n_param (scales to 40-loc
  # on local + VM). reported_cases/reported_deaths are NOT captured -- they are
  # taken from cases_array/deaths_array (exact, no re-sim).
  .capture <- isTRUE(capture_trajectories)
  .scratch_owned <- FALSE
  traj_scratch <- NULL
  if (.capture) {
    traj_scratch <- trajectory_scratch_dir
    if (is.null(traj_scratch) || !nzchar(traj_scratch)) {
      traj_scratch <- tempfile("mosaic_traj_")
      .scratch_owned <- TRUE   # we created it -> we clean it up
    }
    dir.create(traj_scratch, recursive = TRUE, showWarnings = FALSE)
    # Clean up an auto-created scratch dir when we also reduce here (standalone
    # use). When reduce_trajectories = FALSE the CALLER (run_MOSAIC) owns the
    # scratch lifecycle and reduces post-optimize, so do NOT unlink here.
    if (.scratch_owned && isTRUE(reduce_trajectories))
      on.exit(unlink(traj_scratch, recursive = TRUE, force = TRUE), add = TRUE)
  }
  # Spill one sim's captured channels (+ epidemic-flag inputs) to scratch.
  .spill_traj <- function(param_idx, stoch_idx, traj, traj_epi) {
    if (!.capture || is.null(traj) || !length(traj)) return(invisible(NULL))
    saveRDS(list(traj = traj, traj_epi = traj_epi),
            file.path(traj_scratch, sprintf("sim_%d_%d.rds",
                                            as.integer(param_idx), as.integer(stoch_idx))))
    invisible(NULL)
  }

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
        # Dask path: the remote worker has no shared FS, so it returns the
        # channels in the record; spill them to local scratch HERE as each record
        # is consumed (the gathered list is freed on the big-RAM Dask client).
        .spill_traj(p, s, result$traj, result$traj_epi)
      }
    }
  } else {
    # Capture flags resolved once; threaded explicitly into the worker (NOT via
    # closure capture, which is fragile when the task fn's environment is reset
    # to .GlobalEnv for PSOCK export).
    .capture_traj  <- isTRUE(capture_trajectories)
    .traj_channels <- as.character(trajectory_channels)
    .traj_scratch  <- traj_scratch   # NULL unless capturing (set above)

    # Worker function (self-contained for parallel execution)
    run_param_stoch_simulation <- function(task_info, param_configs_list,
                                           capture_traj = FALSE,
                                           traj_channels = character(0),
                                           traj_scratch = NULL) {
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
        # Extract the engine's spatial-structure arrays (J x T hazard, J x J
        # coupling, J x J pi_ij) BEFORE the model is discarded below (F1). These
        # are computed by the engine's DerivedValues component at the final tick
        # and otherwise lost when the model object is gc'd. tryCatch each so an
        # engine that drops DerivedValues simply yields NULL (warn+skip downstream).
        sh  <- tryCatch(model$results$spatial_hazard, error = function(e) NULL)
        cpl <- tryCatch(model$results$coupling,       error = function(e) NULL)
        pij <- tryCatch(model$results$pi_ij,          error = function(e) NULL)
        result <- list(param_idx = param_idx, stoch_idx = stoch_idx,
                       reported_cases = model$results$reported_cases,
                       reported_deaths = model$results$reported_deaths,
                       spatial_hazard = sh,
                       coupling       = cpl,
                       pi_ij          = pij,
                       success = TRUE)
        # Trajectory channels (comprehensive internal-state capture). Harvested
        # here, where model$results is in hand, at zero marginal sim cost --
        # NEVER re-simulated downstream (PLAN sec 3, capture-don't-replay). Each
        # channel tryCatch'd -> omitted if the engine build lacks it. Also carry
        # the per-member epidemic-flag inputs (sampled per set) so epidemic_frac
        # is reconstructable on the master regardless of backend (DM F5).
        if (isTRUE(capture_traj) && length(traj_channels)) {
          traj <- list()
          for (ch in traj_channels) {
            v <- tryCatch(model$results[[ch]], error = function(e) NULL)
            if (!is.null(v)) traj[[ch]] <- v
          }
          traj_epi <- list(
            delta_reporting_cases = tryCatch(param_config$delta_reporting_cases,
                                             error = function(e) NULL),
            epidemic_threshold    = tryCatch(param_config$epidemic_threshold,
                                             error = function(e) NULL)
          )
          # STREAM-TO-DISK: write channels to local scratch (PSOCK workers share
          # the master's filesystem) and do NOT carry them back on the record --
          # this is what keeps the gather (and peak RAM) flat regardless of
          # n_param. If no scratch dir was provided (direct call without spill),
          # fall back to attaching them to the record for the in-process reduce.
          if (length(traj) && !is.null(traj_scratch) && nzchar(traj_scratch)) {
            tryCatch(
              saveRDS(list(traj = traj, traj_epi = traj_epi),
                      file.path(traj_scratch, sprintf("sim_%d_%d.rds",
                                                      param_idx, stoch_idx))),
              error = function(e) NULL)
          } else if (length(traj)) {
            result$traj <- traj
            result$traj_epi <- traj_epi
          }
        }
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

      # Export the configs AND the worker function to each worker's global env
      # so the per-task dispatch closure (below) carries no payload -- it resolves
      # both from the worker side. This keeps the robust load-balanced dispatcher
      # from re-serializing all param_configs on every task.
      parallel::clusterExport(cl, c("param_configs", "run_param_stoch_simulation",
                                     ".capture_traj", ".traj_channels", ".traj_scratch"),
                              envir = environment())

      if (verbose) message("Parallel execution on ", n_cores_use, " cores...")

      # Worker-death-robust gather (see .mosaic_cluster_lapply_robust): a PSOCK
      # worker that crashes its process (fatal Python/numba abort, OOM kill)
      # would otherwise block the master forever in unserialize() on Linux.
      # This dispatcher waits on the worker sockets with a finite timeout and
      # surfaces a diagnostic stop() instead of hanging.
      .ens_idle_timeout <- as.numeric(
        getOption("MOSAIC.ensemble_worker_timeout_sec", 1800))
      .ens_task_fun <- function(row) run_param_stoch_simulation(
        row, param_configs, .capture_traj, .traj_channels, .traj_scratch)
      environment(.ens_task_fun) <- .GlobalEnv  # resolve names worker-side
      results_list <- .mosaic_cluster_lapply_robust(
        cl = cl,
        X  = split(task_list, seq_len(nrow(task_list))),
        fun = .ens_task_fun,
        idle_timeout_sec = .ens_idle_timeout,
        progress = isTRUE(verbose)
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
        function(row) run_param_stoch_simulation(row, param_configs,
                                                 .capture_traj, .traj_channels,
                                                 .traj_scratch)
      )
    }

    # Surface any worker-process deaths recorded by the robust dispatcher (these
    # carry no param_idx and are skipped by the success check below, but the user
    # must be told a worker crashed rather than silently losing those slices).
    n_worker_deaths <- sum(vapply(results_list,
      function(r) isTRUE(r$.mosaic_worker_died), logical(1)))
    if (n_worker_deaths > 0L) {
      warning(sprintf(paste0(
        "calc_model_ensemble: %d task(s) lost to worker-process crashes (fatal engine ",
        "abort or OOM in a PSOCK worker); their predictions are dropped and the ensemble ",
        "proceeds on the survivors. Re-run with parallel = FALSE to surface the underlying ",
        "engine error."), n_worker_deaths), call. = FALSE)
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
  # Spatial-structure aggregation (figs 5-6): element-wise MEDIAN across the
  # posterior-ensemble members (DM#5). The engine arrays spatial_hazard (J x T),
  # coupling (J x J), and pi_ij (J x J) are extracted inside each worker before
  # the model is discarded (F1) and carried on every result record on BOTH the
  # local PSOCK/sequential path (results_list) and the Dask path
  # (precomputed_results). The member set is exactly the set of successful
  # records, identical across paths, so Dask and local produce the same
  # aggregate. pi_ij is deterministic across members (function of N/omega/gamma),
  # so its median equals any member's; persisting it lets the renderer prefer the
  # engine pi_ij over an R recompute (F4).
  # ===========================================================================

  spatial_source <- if (!is.null(precomputed_results)) precomputed_results else
    if (exists("results_list", inherits = FALSE)) results_list else NULL

  spatial_hazard_ensemble <- NULL
  coupling_ensemble       <- NULL
  pi_ij_ensemble          <- NULL

  if (!is.null(spatial_source)) {
    .as_mat <- function(x) {
      if (is.null(x)) return(NULL)
      m <- suppressWarnings(as.matrix(x))
      if (!is.numeric(m) || any(dim(m) == 0L)) return(NULL)
      m
    }
    .elementwise_median <- function(mats) {
      mats <- Filter(Negate(is.null), mats)
      if (length(mats) == 0L) return(NULL)
      d0 <- dim(mats[[1L]])
      mats <- Filter(function(m) identical(dim(m), d0), mats)
      if (length(mats) == 0L) return(NULL)
      stk <- array(unlist(mats, use.names = FALSE),
                   dim = c(d0[1L], d0[2L], length(mats)))
      # Element-wise median over the member dimension; preserve NaN/NA cells
      # that are NaN/NA across all members (DM#4 coupling masking downstream).
      out <- apply(stk, c(1L, 2L), function(v) {
        v <- v[is.finite(v)]
        if (length(v) == 0L) NaN else stats::median(v)
      })
      dim(out) <- d0
      out
    }

    sh_list  <- lapply(spatial_source, function(r) if (isTRUE(r$success)) .as_mat(r$spatial_hazard) else NULL)
    cpl_list <- lapply(spatial_source, function(r) if (isTRUE(r$success)) .as_mat(r$coupling)       else NULL)
    pij_list <- lapply(spatial_source, function(r) if (isTRUE(r$success)) .as_mat(r$pi_ij)          else NULL)

    spatial_hazard_ensemble <- .elementwise_median(sh_list)
    coupling_ensemble       <- .elementwise_median(cpl_list)
    pi_ij_ensemble          <- .elementwise_median(pij_list)

    # Attach config-order labels (F3). Hazard is J x T: rows = locations.
    if (!is.null(spatial_hazard_ensemble) &&
        nrow(spatial_hazard_ensemble) == n_locations) {
      rownames(spatial_hazard_ensemble) <- location_names
      # Column (time) labels from the run's date span when shapes line up.
      if (!is.null(date_start) && !is.null(date_stop)) {
        dseq <- tryCatch(
          seq(as.Date(date_start), as.Date(date_stop), by = "day"),
          error = function(e) NULL)
        if (!is.null(dseq) && length(dseq) == ncol(spatial_hazard_ensemble))
          colnames(spatial_hazard_ensemble) <- as.character(dseq)
      }
    }
    for (nm in c("coupling_ensemble", "pi_ij_ensemble")) {
      m <- get(nm)
      if (!is.null(m) && nrow(m) == n_locations && ncol(m) == n_locations) {
        dimnames(m) <- list(location_names, location_names)
        assign(nm, m)
      }
    }
  }

  # ===========================================================================
  # Trajectory capture reduction (comprehensive internal-state channels). Read
  # back from the per-sim SCRATCH spill (capture-don't-replay), channel-by-channel
  # to bound peak RAM to one dense array (PLAN sec 14.G). When reduce_trajectories
  # is TRUE (standalone use) the reduction runs here over ALL members and is
  # attached as $trajectories. When FALSE (run_MOSAIC), the scratch handle is
  # returned instead so the caller can reduce over the FINAL displayed subset
  # (optimized weights) AFTER optimize_ensemble_subset(), with NO re-simulation
  # (PLAN sec 14.B B-PERSIST, deviation-#1). The medoid passes
  # capture_trajectories = FALSE (PLAN 14.H B-MEDOID).
  # ===========================================================================

  trajectories       <- NULL
  trajectory_scratch <- NULL
  if (.capture) {
    if (isTRUE(reduce_trajectories)) {
      trajectories <- tryCatch(
        .mosaic_build_trajectories(
          scratch_dir       = traj_scratch,
          subset_orig_pidx  = seq_len(n_param_sets),
          subset_weights    = parameter_weights,
          cases_array       = cases_array,
          deaths_array      = deaths_array,
          n_stoch           = n_simulations_per_config,
          n_locations       = n_locations,
          n_time_points     = n_time_points,
          location_names    = location_names,
          date_start        = date_start,
          date_stop         = date_stop,
          n_successful      = n_successful,
          obs_cases         = obs_cases,
          obs_deaths        = obs_deaths,
          trajectory_channels = trajectory_channels,
          n_lines           = trajectory_n_lines,
          verbose           = verbose
        ),
        error = function(e) {
          warning("calc_model_ensemble: trajectory reduction failed: ",
                  conditionMessage(e), call. = FALSE)
          NULL
        })
    } else {
      # Caller-owned scratch: hand back everything needed to reduce later over the
      # optimized subset. member_seeds aligns scratch param_idx -> seed so the
      # caller can map optimized members (by seed) back to their scratch files.
      trajectory_scratch <- list(
        dir                 = traj_scratch,
        trajectory_channels = as.character(trajectory_channels),
        n_param_sets        = n_param_sets,
        n_stoch             = n_simulations_per_config,
        n_lines             = trajectory_n_lines
      )
    }
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
      spatial_hazard_ensemble   = spatial_hazard_ensemble,
      coupling_ensemble         = coupling_ensemble,
      pi_ij_ensemble            = pi_ij_ensemble,
      trajectories              = trajectories,
      trajectory_scratch        = trajectory_scratch,
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

#' Build the compact trajectory summary from the per-sim SCRATCH spill
#'
#' Capture-don't-replay (PLAN sec 14.G design B), STREAM-TO-DISK: the per-sim
#' channels were spilled to \code{scratch_dir} at sim time (keyed by the ORIGINAL
#' candidate \code{(param_idx, stoch_idx)}). This reducer reads them back over the
#' FINAL displayed subset (\code{subset_orig_pidx} -> scratch files,
#' \code{subset_weights} aligned) in two bounded passes: (1) a TRANSPOSE pass that
#' reads each present sim file ONCE and appends each channel's \code{[n_loc x
#' n_time]} matrix to a per-channel serialize connection (and streams the
#' \code{epidemic_frac} accumulators); (2) a per-channel reduce that reads ONE
#' channel back into a single \code{[n_loc x n_time x n_disp x n_stoch]} array,
#' computes the exact weighted median per (loc, t) with \code{weighted_quantiles()}
#' (weight applied ONCE; DM F6), and a UNIFORM-thinned set of actual member
#' trajectories for the spaghetti, then frees it. Peak RAM is ONE dense array
#' regardless of n_param (scales to 40-loc on local + VM). No re-simulation.
#'
#' \code{reported_cases}/\code{reported_deaths} are taken from the supplied
#' \code{cases_array}/\code{deaths_array} (already in display order) -- exact
#' match to the prediction plots, no re-capture (PLAN sec 5.5). Derived series
#' (\code{I_total}, \code{mass_balance}, \code{CFR}) are formed from the channel
#' medians; \code{epidemic_frac} is the streaming weighted-MEAN of the
#' reconstructed engine epidemic flag over ALL members (DM F5).
#'
#' Returns a \code{mosaic_trajectories} object, or \code{NULL} (with a loud
#' \code{warning()}) when no scratch files exist -- e.g. an outdated Dask/Coiled
#' worker image (PLAN sec 14.H capability check). Never silently partial.
#' @noRd
.mosaic_build_trajectories <- function(scratch_dir, subset_orig_pidx,
                                       subset_weights, cases_array, deaths_array,
                                       n_stoch, n_locations, n_time_points,
                                       location_names, date_start, date_stop,
                                       n_successful, obs_cases, obs_deaths,
                                       trajectory_channels,
                                       central_method = c(cases = "median",
                                                          deaths = "median"),
                                       n_lines = 150L,
                                       line_stride = 7L, verbose = TRUE) {
  if (is.null(scratch_dir) || !dir.exists(scratch_dir)) return(NULL)
  # Resolve central tendency to a c(cases=, deaths=) pair so the reported_*
  # central line matches the prediction plots under EITHER central_method (the
  # prediction line is predicted_central = mean or median per channel). All other
  # channels have no prediction-plot counterpart -> conventional weighted median.
  central_method <- .mosaic_resolve_central_method(central_method)
  n_disp <- length(subset_orig_pidx)
  if (n_disp == 0L) return(NULL)
  subset_orig_pidx <- as.integer(subset_orig_pidx)
  subset_weights   <- as.numeric(subset_weights)

  sim_file <- function(j, s)
    file.path(scratch_dir, sprintf("sim_%d_%d.rds", subset_orig_pidx[j], s))

  present_files <- matrix(FALSE, n_disp, n_stoch)
  for (s in seq_len(n_stoch)) for (j in seq_len(n_disp))
    present_files[j, s] <- file.exists(sim_file(j, s))

  # Capability check (PLAN 14.H): no scratch at all -> capture produced nothing
  # (e.g. an outdated Dask/Coiled worker image). Warn loudly, skip -- never
  # silently emit a one-backend-only artifact.
  if (!any(present_files)) {
    warning("calc_model_ensemble: capture_trajectories = TRUE but no trajectory ",
            "scratch was produced (likely an outdated Dask/Coiled worker image ",
            "predating this feature). Skipping the trajectory artifact; rebuild/",
            "republish the worker image to enable it.", call. = FALSE)
    return(NULL)
  }

  pf    <- which(present_files, arr.ind = TRUE)[1L, ]
  probe <- tryCatch(readRDS(sim_file(pf[1L], pf[2L])), error = function(e) NULL)
  present_ch <- if (!is.null(probe))
    intersect(trajectory_channels, names(probe$traj)) else character(0)

  # per-member weights, display-index-fastest (matches array fill [.,.,j,s]).
  sim_weights <- rep(subset_weights, times = n_stoch) / n_stoch

  # Orientation-robust coercion of a channel to [n_loc x n_time]; trim a trailing
  # tick+1 column (flow/FOI channels) or pad short -- mirrors enrich to_vec, so a
  # one-tick offset never silently empties a panel (DM Finding 2).
  .rec_mat <- function(val) {
    if (is.null(val)) return(NULL)
    m <- suppressWarnings(as.numeric(unlist(val, use.names = FALSE)))
    L <- length(m); target <- n_locations * n_time_points
    if (L == target) return(matrix(m, nrow = n_locations, ncol = n_time_points))
    if (L %% n_locations == 0L) {
      t_eff <- L %/% n_locations
      mm <- matrix(m, nrow = n_locations, ncol = t_eff)
      if (t_eff >= n_time_points) return(mm[, seq_len(n_time_points), drop = FALSE])
      out <- matrix(NA_real_, n_locations, n_time_points)
      out[, seq_len(t_eff)] <- mm
      return(out)
    }
    NULL
  }

  # UNIFORM thinning over present (j,s) (never prob = w; DM F6); LOCAL seed so the
  # reproducible draw never mutates the caller's global RNG (RNG-purity).
  ps_ok <- which(present_files, arr.ind = TRUE)   # col1 = j, col2 = s
  if (nrow(ps_ok) > n_lines) {
    keep <- withr::with_seed(
      20260625L, sort(sample(seq_len(nrow(ps_ok)), n_lines, replace = FALSE)))
    ps_thin <- ps_ok[keep, , drop = FALSE]
  } else ps_thin <- ps_ok
  n_thin     <- nrow(ps_thin)
  t_idx      <- which(seq_len(n_time_points) %% line_stride == 1L)
  member_ids <- (ps_thin[, 2L] - 1L) * n_disp + ps_thin[, 1L]
  member_w   <- subset_weights[ps_thin[, 1L]] / n_stoch

  # ---- TRANSPOSE pass: read each present sim file ONCE, append each channel's
  # matrix to a per-channel serialize connection (display-fastest then stoch),
  # and stream the epidemic_frac accumulators. RAM = one sim file at a time. -----
  chan_dir <- file.path(scratch_dir, "_bychan")
  dir.create(chan_dir, showWarnings = FALSE)
  cons <- lapply(present_ch, function(ch)
    file(file.path(chan_dir, paste0("c_", ch)), "wb"))
  names(cons) <- present_ch
  # Close every write connection on ANY exit from here on (error or normal) so a
  # failure mid-transpose cannot leak the N open connections (R's 128-connection
  # limit). The explicit close below makes this a harmless idempotent re-close.
  on.exit(for (cc in cons) try(close(cc), silent = TRUE), add = TRUE)

  ef_comp <- c("S", "E", "Isym", "Iasym", "R", "V1", "V2")
  ef_ok   <- all(ef_comp %in% present_ch)
  wflag   <- matrix(0, n_locations, n_time_points)
  wsum    <- matrix(0, n_locations, n_time_points)
  any_ef  <- FALSE
  na_mat  <- matrix(NA_real_, n_locations, n_time_points)

  for (s in seq_len(n_stoch)) for (j in seq_len(n_disp)) {
    rec  <- if (present_files[j, s])
      tryCatch(readRDS(sim_file(j, s)), error = function(e) NULL) else NULL
    traj <- if (!is.null(rec)) rec$traj else NULL
    for (ch in present_ch) {
      m <- if (!is.null(traj)) .rec_mat(traj[[ch]]) else NULL
      if (is.null(m)) m <- na_mat
      serialize(m, cons[[ch]])
    }
    if (ef_ok && !is.null(traj)) {
      epi   <- rec$traj_epi
      delta <- suppressWarnings(as.numeric(unlist(epi$delta_reporting_cases)))
      thr   <- suppressWarnings(as.numeric(unlist(epi$epidemic_threshold)))
      Isym  <- .rec_mat(traj[["Isym"]])
      Neff  <- tryCatch(Reduce(`+`, lapply(ef_comp, function(c) .rec_mat(traj[[c]]))),
                        error = function(e) NULL)
      if (length(delta) && length(thr) && !is.null(Isym) && !is.null(Neff) &&
          !all(is.na(delta)) && !all(is.na(thr))) {
        delta <- rep(delta, length.out = n_locations)
        thr   <- rep(thr,   length.out = n_locations)
        w_m   <- subset_weights[j] / n_stoch
        for (i in seq_len(n_locations)) {
          d <- as.integer(round(delta[i]))
          lagged <- rep(NA_real_, n_time_points)
          if (!is.na(d) && d >= 0L && d < n_time_points) {
            if (d == 0L) lagged <- Isym[i, ]
            else lagged[(d + 1L):n_time_points] <- Isym[i, 1L:(n_time_points - d)]
          }
          flag  <- as.numeric(lagged > thr[i] * Neff[i, ])
          valid <- is.finite(flag)
          wflag[i, valid] <- wflag[i, valid] + w_m * flag[valid]
          wsum[i, valid]  <- wsum[i, valid] + w_m
        }
        any_ef <- TRUE
      }
    }
  }
  for (cc in cons) close(cc)

  # ---- per-channel reduce (one dense array at a time) ------------------------
  summary_list <- list()
  thin_store   <- list()

  # `method` selects the central line: "median" (weighted median) or "mean"
  # (weighted mean), matching the prediction path's predicted_central. The list
  # field is named `median` for schema stability -- it holds the central series.
  reduce_arr <- function(arr, method = "median") {
    cen      <- matrix(NA_real_, n_locations, n_time_points)
    use_mean <- identical(method, "mean")
    for (i in seq_len(n_locations)) for (t in seq_len(n_time_points)) {
      v <- as.vector(arr[i, t, , ])
      if (use_mean) {
        ok <- is.finite(v) & is.finite(sim_weights)
        cen[i, t] <- if (any(ok)) stats::weighted.mean(v[ok], sim_weights[ok]) else NA_real_
      } else {
        cen[i, t] <- weighted_quantiles(v, sim_weights, 0.5)
      }
    }
    ts <- array(NA_real_, dim = c(n_locations, length(t_idx), n_thin))
    for (m in seq_len(n_thin))
      ts[, , m] <- arr[, t_idx, ps_thin[m, 1L], ps_thin[m, 2L]]
    list(median = cen, thin = ts)
  }

  # reported_* from the prediction arrays (exact match to the prediction plots;
  # no re-capture, no re-sim). cases_array/deaths_array are in display order. The
  # central line follows central_method PER CHANNEL so it is BIT-IDENTICAL to the
  # prediction plots' predicted_central under both "median" and "mean" modes.
  rc <- reduce_arr(cases_array,  central_method[["cases"]])
  summary_list[["reported_cases"]]  <- list(median = rc$median)
  thin_store[["reported_cases"]]    <- rc$thin
  rd <- reduce_arr(deaths_array, central_method[["deaths"]])
  summary_list[["reported_deaths"]] <- list(median = rd$median)
  thin_store[["reported_deaths"]]   <- rd$thin

  for (ch in present_ch) {
    con <- file(file.path(chan_dir, paste0("c_", ch)), "rb")
    arr <- array(NA_real_, dim = c(n_locations, n_time_points, n_disp, n_stoch))
    for (s in seq_len(n_stoch)) for (j in seq_len(n_disp)) {
      m <- tryCatch(unserialize(con), error = function(e) NULL)
      if (!is.null(m) && is.matrix(m)) arr[, , j, s] <- m
    }
    close(con)
    r <- reduce_arr(arr)
    summary_list[[ch]] <- list(median = r$median)
    thin_store[[ch]]   <- r$thin
    rm(arr)
  }
  unlink(chan_dir, recursive = TRUE, force = TRUE)

  .med <- function(ch) if (!is.null(summary_list[[ch]])) summary_list[[ch]]$median else NULL

  # I_total = Isym + Iasym (missing Iasym -> 0; DM F7)
  if (!is.null(.med("Isym"))) {
    ia_med <- .med("Iasym"); if (is.null(ia_med)) ia_med <- 0
    summary_list[["I_total"]] <- list(median = .med("Isym") + ia_med)
    ia_thn <- thin_store[["Iasym"]]; if (is.null(ia_thn)) ia_thn <- 0
    thin_store[["I_total"]] <- thin_store[["Isym"]] + ia_thn
  }

  # mass_balance = (S+E+Isym+Iasym+R+V1+V2)/N -- POINT series, ratio of medians
  mb_comp <- c("S", "E", "Isym", "Iasym", "R", "V1", "V2")
  if (all(vapply(mb_comp, function(c) !is.null(.med(c)), logical(1))) &&
      !is.null(.med("N"))) {
    num <- Reduce(`+`, lapply(mb_comp, .med))
    den <- .med("N"); den[den == 0] <- NA_real_
    summary_list[["mass_balance"]] <- list(median = num / den)
  }

  # CFR(t) = rolling(reported_deaths,W)/rolling(reported_cases,W) from medians;
  # W = min(28, n_time) so short series don't error.
  if (!is.null(.med("reported_cases")) && !is.null(.med("reported_deaths"))) {
    roll_w <- min(28L, n_time_points)
    roll28 <- function(M) {
      out <- matrix(NA_real_, n_locations, n_time_points)
      for (i in seq_len(n_locations))
        out[i, ] <- as.numeric(stats::filter(M[i, ], rep(1, roll_w), sides = 1))
      out
    }
    rc28 <- roll28(.med("reported_cases")); rd28 <- roll28(.med("reported_deaths"))
    cfr <- rd28 / rc28
    cfr[!is.finite(cfr) | rc28 <= 5] <- NA_real_
    summary_list[["CFR"]] <- list(median = cfr)
  }

  # epidemic_frac: weighted MEAN of the streamed engine flag over ALL members.
  if (any_ef) {
    ef <- wflag / wsum; ef[!is.finite(ef)] <- NA_real_
    summary_list[["epidemic_frac"]] <- list(median = ef)
  }

  # ---- assemble long-form display lines from the thinned strided slices -------
  n_t <- length(t_idx)
  lines_parts <- list()
  for (ch in names(thin_store)) {
    ts <- thin_store[[ch]]; if (is.null(ts)) next
    for (i in seq_len(n_locations)) {
      lines_parts[[length(lines_parts) + 1L]] <- data.frame(
        member_id = rep(member_ids, each = n_t),
        weight    = rep(member_w, each = n_t),
        location  = location_names[i],
        channel   = ch,
        t         = rep(t_idx, times = n_thin),
        value     = as.vector(ts[i, , ]),
        stringsAsFactors = FALSE)
    }
  }
  lines <- if (length(lines_parts)) do.call(rbind, lines_parts) else
    data.frame(member_id = integer(0), weight = numeric(0), location = character(0),
               channel = character(0), t = integer(0), value = numeric(0))
  lines <- lines[is.finite(lines$value), , drop = FALSE]

  # NA transparency (DM F3): captured channels are reduced only over members whose
  # scratch spilled; reported_* use ALL members (from the prediction arrays). If a
  # sim filled cases_array but failed to spill, its channel cells are NA, so a
  # compartment-panel central can rest on fewer members than the reported_* panel
  # beside it -- surface that count rather than let it read as a model inconsistency.
  if (verbose)
    message(sprintf(paste0("  trajectories: %d channels, %d display lines/location ",
                           "reduced; captured-channel members present %d/%d (reported_* ",
                           "use all %d)."),
                    length(summary_list), n_thin, sum(present_files),
                    n_disp * n_stoch, n_disp * n_stoch))

  structure(list(
    schema                   = "mosaic_trajectories",
    channels                 = names(summary_list),
    location_names           = location_names,
    n_locations              = n_locations,
    n_time_points            = n_time_points,
    date_start               = date_start,
    date_stop                = date_stop,
    n_param_sets             = n_disp,
    n_simulations_per_config = as.integer(n_stoch),
    n_successful             = n_successful,
    summary                  = summary_list,
    lines                    = lines,
    obs_cases                = obs_cases,
    obs_deaths               = obs_deaths
  ), class = "mosaic_trajectories")
}
