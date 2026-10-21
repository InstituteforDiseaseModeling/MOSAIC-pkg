# =============================================================================
# MOSAIC: run_mosaic.R
# Main calibration workflow function
# =============================================================================

# =============================================================================
# PER-COUNTRY (CROSS-LOCATION) INFLUENCE WEIGHTS
# =============================================================================

#' Derive data-driven per-location influence weights from observed signal
#'
#' Computes a default \code{weights_location} vector that down-weights
#' low-signal / absence countries so they inform the fit proportionally to
#' their information content WITHOUT dominating it. This is the cross-location
#' complement to the per-cell \code{weights_obs_*} confidence weighting (which,
#' by mass-preserving renormalization, cannot move influence ACROSS locations).
#'
#' \strong{Why this is needed.} An all-zero "absence" series yields a
#' method-of-moments NB dispersion \code{k = Inf} (a Poisson kernel), so any
#' over-prediction accrues an unbounded \code{~ -lambda} penalty in EVERY cell.
#' Summed over a multi-year window an absence country can contribute a larger
#' magnitude log-likelihood than a real outbreak country (empirically BFA scored
#' more negative than COD under a constant-outbreak prediction), letting the 16
#' all-zero countries in the default window dominate the importance weights.
#'
#' \strong{Proxy for information content.} Each location's positive-signal mass
#' is \eqn{s_j = \sum_{t: y^{cases}_{jt} > 0} w^{cases}_{jt} +
#' \sum_{t: y^{deaths}_{jt} > 0} w^{deaths}_{jt}}, i.e. the confidence-weighted
#' count of positive observations (cases plus deaths). This is purely
#' data-derived (no \code{est}/LL dependence -> no circularity) and rewards both
#' the number AND the surveillance trust of positive reports.
#'
#' \strong{Weight map.} A diminishing-returns log compression with a strictly
#' positive floor:
#' \deqn{w_j = f + (1 - f)\,\min\!\Big(1,\ \frac{\log(1 + s_j)}{\log(1 + s_{ref})}\Big)}
#' where \eqn{f} is the floor and \eqn{s_{ref}} is the median positive-signal
#' mass among locations that have any signal. Absence countries (\eqn{s_j = 0})
#' map to the floor \eqn{f}; a country at the median signal maps to \eqn{w = 1};
#' high-signal countries saturate at \eqn{w = 1}. The log compression keeps a
#' country's influence growing with its information but with diminishing returns,
#' so a single very large outbreak cannot buy unbounded cross-location weight.
#'
#' \strong{Floor.} \code{floor = 0.05} keeps every absence country in the fit at
#' 5 percent influence (requirement: no location is dropped). At this floor the
#' worst absence weighted contribution falls well below a typical active
#' country's, while active countries retain their relative influence.
#'
#' \strong{Backward compatibility.} The derivation is gated on the per-cell
#' weight matrices being present (the v4.1+ trust-aware regime). When
#' \code{reported_cases_weight}/\code{reported_deaths_weight} are absent (older
#' config), or when no location has any positive signal, or when all signals are
#' equal (uniform data), the function returns \code{rep(1, n_locations)} -- i.e.
#' byte-identical to the current uniform behavior. This is a DEFAULT only; a
#' \code{weights_location} explicitly supplied by the caller is honored upstream
#' and never overwritten.
#'
#' @param config A simulation config list with \code{reported_cases} (and optionally
#'   \code{reported_deaths}) observation matrices and the v4.1+ per-cell
#'   \code{reported_cases_weight} / \code{reported_deaths_weight} matrices.
#' @param floor Strictly-positive minimum weight for absence/low-signal
#'   locations. Default \code{0.05}.
#' @return Length-\code{n_locations} numeric vector of weights in \code{(0, 1]},
#'   or \code{NULL} when the inputs are absent/degenerate (signalling the caller
#'   to fall back to uniform \code{rep(1, n_locations)}).
#' @noRd
.mosaic_derive_weights_location <- function(config, floor = 0.05) {
  rc  <- config$reported_cases
  if (is.null(rc)) return(NULL)
  if (!is.matrix(rc)) rc <- matrix(rc, nrow = 1L)
  n_loc <- nrow(rc)

  rcw <- config$reported_cases_weight
  rdw <- config$reported_deaths_weight
  rd  <- config$reported_deaths

  # Gate on the per-cell weight matrices (v4.1+). Absent -> uniform (back-compat).
  if (is.null(rcw)) return(NULL)
  if (!is.matrix(rcw)) rcw <- matrix(rcw, nrow = 1L)
  if (any(dim(rcw) != dim(rc))) return(NULL)

  # Confidence-weighted count of positive observations per location.
  pos_mass_row <- function(obs_row, w_row) {
    sel <- is.finite(obs_row) & (obs_row > 0)
    if (!any(sel)) return(0)
    wv <- w_row[sel]
    wv[!is.finite(wv)] <- 0
    sum(wv)
  }

  s <- numeric(n_loc)
  for (j in seq_len(n_loc)) s[j] <- pos_mass_row(rc[j, ], rcw[j, ])

  # Add deaths-signal mass when the deaths obs + weight matrices are present and
  # conformable; otherwise cases-only (a deaths matrix without its weight matrix
  # contributes nothing rather than silently using implicit weight 1).
  if (!is.null(rd) && !is.null(rdw)) {
    if (!is.matrix(rd))  rd  <- matrix(rd,  nrow = 1L)
    if (!is.matrix(rdw)) rdw <- matrix(rdw, nrow = 1L)
    if (all(dim(rd) == c(n_loc, ncol(rc))) && all(dim(rdw) == dim(rd))) {
      for (j in seq_len(n_loc)) s[j] <- s[j] + pos_mass_row(rd[j, ], rdw[j, ])
    }
  }

  s_pos <- s[is.finite(s) & s > 0]
  if (length(s_pos) == 0) return(NULL)            # no signal anywhere -> uniform
  s_ref <- stats::median(s_pos)
  if (!is.finite(s_ref) || s_ref <= 0) return(NULL)

  # Uniform data (all positive-signal masses equal) -> all weights 1.
  rel <- log1p(pmax(s, 0)) / log1p(s_ref)
  w <- floor + (1 - floor) * pmin(1, rel)
  if (all(abs(w - 1) < .Machine$double.eps^0.5)) return(NULL)

  w
}

# =============================================================================
# FAST PARAMETER EXTRACTION (replaces convert_config_to_matrix in hot loop)
# =============================================================================

#' Build Parameter Lookup Table
#'
#' Precomputes a mapping from param_names_all to config keys and location
#' indices. Built once at setup, passed to each worker to avoid per-sim
#' overhead of convert_config_to_matrix().
#'
#' @param param_names_all Character vector of parameter names (from setup)
#' @param location_names Character vector of location ISO codes
#' @return A list of lists, each with \code{key} (config key) and \code{idx}
#'   (integer location index, or NULL for global params)
#' @noRd
.mosaic_build_param_lookup <- function(param_names_all, location_names) {
  n <- length(param_names_all)
  lookup <- vector("list", n)
  for (i in seq_len(n)) {
    nm <- param_names_all[i]
    # Check if name ends with _ISO (3-letter uppercase suffix matching a location)
    suffix <- sub("^.*_([A-Z]{3})$", "\\1", nm)
    if (nchar(suffix) == 3L && suffix %in% location_names) {
      base_key <- sub("_[A-Z]{3}$", "", nm)
      loc_idx <- match(suffix, location_names)
      lookup[[i]] <- list(key = base_key, idx = loc_idx)
    } else {
      lookup[[i]] <- list(key = nm, idx = NULL)
    }
  }
  lookup
}

#' Extract Parameter Row from Config Using Precomputed Lookup
#'
#' Lightweight replacement for convert_config_to_matrix() in the worker hot
#' loop. Uses direct indexed access instead of iterating all config keys.
#'
#' @param config A sampled config list
#' @param param_lookup Precomputed lookup from .mosaic_build_param_lookup()
#' @param n_params Length of param_names_all
#' @return Numeric vector of length n_params
#' @noRd
.mosaic_extract_param_row <- function(config, param_lookup, n_params) {
  row <- numeric(n_params)
  for (i in seq_len(n_params)) {
    entry <- param_lookup[[i]]
    val <- config[[entry$key]]
    if (is.null(val)) {
      row[i] <- NA_real_
    } else if (!is.null(entry$idx)) {
      row[i] <- as.numeric(val[entry$idx])
    } else {
      row[i] <- as.numeric(val[1L])
    }
  }
  row
}

# =============================================================================
# SIMULATION WORKER FUNCTION
# =============================================================================

#' Run several simulations and write them as one shard
#'
#' Runs \code{sim_ids} one at a time through
#' \code{.mosaic_run_simulation_worker(write_shard = FALSE)} and writes the
#' surviving rows as a SINGLE parquet named for the id range it covers.
#'
#' Why: a one-row shard at production width is almost all framing. At 1,352
#' columns it costs ~444 KB on disk for ~8 KB of data, so a 100,000-simulation
#' run wrote 52.9 GB of shards for 1.03 GB of data, and both the combine and the
#' resume scan then pay to re-read all of it. Measured at 1,352 columns,
#' 1,000 simulations: 434 MB and 20.62 s to read back at one row per file,
#' against 15.2 MB and 0.17 s at 100 rows per file.
#'
#' The cost is crash granularity: a worker that dies mid-chunk loses the whole
#' chunk instead of one simulation, because nothing is on disk until the chunk
#' finishes. An R-level error in ONE simulation does not -- each is wrapped, so
#' its siblings still land. Resume re-draws whatever is missing either way.
#'
#' @param sim_ids Integer vector of simulation ids for this chunk.
#' @param dir_cal_samples Directory to write the shard into.
#' @param io Control \code{io} list, passed to \code{.mosaic_write_parquet()}.
#' @param ... Passed through to \code{.mosaic_run_simulation_worker()}.
#' @return Logical vector, one element per element of \code{sim_ids}.
#' @noRd
.mosaic_run_simulation_chunk <- function(sim_ids, dir_cal_samples, io, ...) {

  sim_ids <- as.integer(sim_ids)
  if (!length(sim_ids)) return(logical(0))

  mats <- lapply(sim_ids, function(id) {
    tryCatch(
      .mosaic_run_simulation_worker(sim_id = id,
                                    dir_cal_samples = dir_cal_samples,
                                    io = io, ...,
                                    write_shard = FALSE),
      error = function(e) NULL
    )
  })

  ok <- !vapply(mats, is.null, logical(1))
  if (!any(ok)) return(rep(FALSE, length(sim_ids)))

  shard <- as.data.frame(do.call(rbind, mats[ok]))
  # A one-id chunk keeps the historical single-simulation name, so a run at the
  # default shard_batch_size = 1 writes byte-for-byte what it always did and an
  # existing output directory stays resumable.
  out <- file.path(
    dir_cal_samples,
    if (min(sim_ids) == max(sim_ids)) sprintf("sim_%07d.parquet", sim_ids[1])
    else sprintf("sim_%07d-%07d.parquet", min(sim_ids), max(sim_ids))
  )
  written <- tryCatch({
    .mosaic_write_parquet(shard, out, io)
    file.exists(out)
  }, error = function(e) {
    # Say why. Without this a malformed `io`, a full disk or a permissions
    # problem all present as "every simulation in this chunk failed", with no
    # trace of the real cause anywhere in the run log.
    warning(sprintf("shard write failed for sims %d-%d: %s",
                    min(sim_ids), max(sim_ids), conditionMessage(e)),
            call. = FALSE)
    FALSE
  })

  # Nothing is successful unless the shard actually landed: a row that was
  # computed but not persisted is not a completed simulation, and claiming it
  # would let the batch tally drift from what resume can see on disk.
  ok & written
}

#' Simulation Worker Function
#'
#' Runs n_iterations iterations per simulation, samples parameters once per sim_id,
#' collapses likelihoods via log-mean-exp, and writes parquet files.
#'
#' @section Seed Scheme:
#' \itemize{
#'   \item \code{sim_id}: Unique simulation ID (1-based integer)
#'   \item \code{seed_sim}: Parameter sampling seed (equals sim_id)
#'   \item \code{seed_iter}: engine seed for iteration j
#'                           = (sim_id - 1) * n_iterations + j
#' }
#'
#' This ensures:
#' - Same sim_id always gets same parameters
#' - Different iterations have different stochastic realizations
#' - Seeds don't overlap between simulations
#'
#' @noRd
.mosaic_run_simulation_worker <- function(sim_id, n_iterations, priors, config, PATHS,
                                          dir_cal_samples,
                                          dir_cal_simresults = NULL,
                                          param_names_all, param_lookup, sampling_args, io,
                                          likelihood_settings,
                                          write_shard = TRUE) {

  # Pre-allocate result matrix (FIXED: proper pre-allocation)
  n_params <- length(param_names_all)
  result_matrix <- matrix(NA_real_, nrow = n_iterations, ncol = 5 + n_params)
  colnames(result_matrix) <- c('sim', 'iter', 'seed_sim', 'seed_iter', 'likelihood', param_names_all)

  # Sample parameters ONCE per simulation
  params_sim <- tryCatch({
    sample_parameters(
      PATHS = PATHS,
      priors = priors,
      config = config,
      seed = sim_id,
      sample_args = sampling_args,
      verbose = FALSE,
      validate = FALSE  # Skip per-sim validation (priors guarantee valid ranges)
    )
  }, error = function(e) {
    # Log error details for debugging
    warning("Simulation ", sim_id, " failed during parameter sampling: ",
            e$message, call. = FALSE, immediate. = FALSE)
    NULL
  })

  # A failed draw returns the caller's failure sentinel: FALSE when this worker
  # owns the write, NULL when a chunk wrapper is collecting rows (which then has
  # nothing to contribute for this sim_id).
  if (is.null(params_sim)) return(if (write_shard) FALSE else NULL)

  # Guardrails: clamp transmission parameters to prevent the engine's ValueError
  # (GitHub #24: p = -np.expm1(-rate) produces p > 1 when rate < 0). Single
  # source of truth so re-sampled medoid/ensemble configs clamp identically.
  params_sim <- .mosaic_clamp_transmission_params(params_sim)

  # (A `params_sim$calc_likelihood <- FALSE` stood here, stopping the Python
  # engine from scoring internally alongside MOSAIC's own R likelihood. The
  # R engine has no internal scoring and sim_params() does not read the
  # field, so the assignment was a no-op from v0.68.0 onward.)

  # Pre-allocate simresults collector (validation mode only)
  simresults_raw <- if (!is.null(dir_cal_simresults)) vector("list", n_iterations) else NULL

  # ---------------------------------------------------------------------------
  # Per-channel scored-window slice (precomputed ONCE outside the hot loop).
  #
  # The shape terms in calc_model_likelihood() (peak timing/magnitude,
  # cumulative) do NOT honor weights_time -- they scan/accumulate the raw
  # series -- so the IC-transient head MUST be removed by SLICING the obs/est
  # arrays, not just down-weighting it. We:
  #   1. slice every array to s:n_time where s = min(idx_cases, idx_deaths),
  #      removing the IC spike in [1, burn_in] < s for ALL shape terms and BOTH
  #      channels with a single shared slice;
  #   2. zero weights_obs_deaths on the residual prefix s:(idx_deaths-1) so the
  #      later-starting deaths channel is not scored there (the region is
  #      already spike-free, so peak/cumulative are safe);
  #   2b. mask (NA) the obs/est CASES on the residual prefix s:(idx_cases-1)
  #      when cases start later than deaths (score_start_cases after the deaths
  #      start). NA cells drop out of the NB core, its effective-sample gate and
  #      every shape term, which matches the cases head that the ensemble R2/
  #      bias mask and the cases dispersion estimate already exclude;
  #   3. pass the SLICED start date (date_start + (s-1)) as config$date_start so
  #      the peak-index date_seq length matches the sliced n_time_steps (else the
  #      length check fails over to weekly and mis-snaps peaks).
  # All-default knobs => idx_cases = idx_deaths = 1 => s = 1 => NO slicing =>
  # bit-identical to pre-feature scoring.
  .sw <- likelihood_settings$.score_window_resolved
  .slice_lik <- !is.null(.sw) && (.sw$idx_cases > 1L || .sw$idx_deaths > 1L)
  .s_start   <- if (.slice_lik) min(.sw$idx_cases, .sw$idx_deaths) else 1L
  .config_lik <- config
  .wt_lik     <- likelihood_settings$.weights_time_resolved
  if (.slice_lik) {
    .n_time_sw <- .sw$n_time
    .keep      <- .s_start:.n_time_sw
    # Sliced peak date_seq start: keep config$date_start as a Date offset by s-1.
    ds0 <- tryCatch(as.Date(config$date_start), error = function(e) NA)
    if (!is.na(ds0)) .config_lik$date_start <- ds0 + (.s_start - 1L)
    # Slice weights_time to the kept columns (length must match sliced arrays).
    if (!is.null(.wt_lik)) .wt_lik <- .wt_lik[.keep]
  }

  # Per-cell confidence-weight matrices, sliced + deaths-prefix-zeroed once.
  .wobs_cases_lik  <- config$reported_cases_weight
  .wobs_deaths_lik <- config$reported_deaths_weight
  if (.slice_lik) {
    if (!is.null(.wobs_cases_lik))  .wobs_cases_lik  <- .wobs_cases_lik[,  .keep, drop = FALSE]
    if (!is.null(.wobs_deaths_lik)) .wobs_deaths_lik <- .wobs_deaths_lik[, .keep, drop = FALSE]
    # Zero deaths weights on the residual prefix s:(idx_deaths-1) (post-slice
    # columns 1:(idx_deaths - s)). If no per-cell matrix exists, synthesize a
    # ones matrix so the prefix can be zeroed (belt-and-suspenders; the deaths
    # min_obs gate also protects this region).
    .deaths_prefix <- .sw$idx_deaths - .s_start
    if (.deaths_prefix > 0L) {
      n_loc_sw <- if (is.matrix(config$reported_deaths)) nrow(config$reported_deaths) else 1L
      if (is.null(.wobs_deaths_lik)) {
        .wobs_deaths_lik <- matrix(1, nrow = n_loc_sw, ncol = length(.keep))
      }
      .wobs_deaths_lik[, seq_len(.deaths_prefix)] <- 0
    }
  }
  # Residual cases prefix (post-slice columns 1:(idx_cases - s)); 0 unless
  # cases start later than deaths.
  .cases_prefix <- if (.slice_lik) .sw$idx_cases - .s_start else 0L

  # Run iterations
  for (j in 1:n_iterations) {
    # Use iteration-specific seed (see seed scheme documentation above)
    seed_ij <- as.integer((sim_id - 1L) * n_iterations + j)
    params_sim$seed <- seed_ij

    # Initialize row directly in pre-allocated matrix (FIXED: no rbind!)
    result_matrix[j, 1:4] <- c(sim_id, j, sim_id, seed_ij)

    # Extract parameters directly using precomputed lookup (fast path)
    param_row <- tryCatch({
      .mosaic_extract_param_row(params_sim, param_lookup, n_params)
    }, error = function(e) NULL)

    if (!is.null(param_row)) {
      result_matrix[j, 6:(5 + n_params)] <- param_row
    }

    # Run model. The engine is reached through run_simulation() and nowhere else:
    # until the R port this worker imported and called the Python module
    # directly, so run_simulation() was not in fact a chokepoint. It is now.
    model <- tryCatch({
      run_simulation(config = params_sim, seed = seed_ij, quiet = TRUE)
    }, error = function(e) {
      # Log model run failure (but don't fail entire simulation)
      warning("Simulation ", sim_id, " iteration ", j, " model run failed: ",
              e$message, call. = FALSE, immediate. = FALSE)
      NULL
    })

    # Calculate likelihood
    if (!is.null(model)) {
      likelihood <- tryCatch({
        obs_cases <- params_sim$reported_cases
        est_cases <- model$results$reported_cases    # rho/chi-thinned onsets, the surveillance scale
        obs_deaths <- params_sim$reported_deaths
        est_deaths <- model$results$reported_deaths  # engine deaths at the PRIOR mu_jt; the deaths core below integrates the CFR out

        if (!is.null(obs_cases) && !is.null(est_cases) &&
            !is.null(obs_deaths) && !is.null(est_deaths)) {

          # Slice obs/est to the scored window (s:n_time) so the spike-laden IC
          # head is removed for every term (NB + non-weight-honoring shape
          # terms). No-op when .slice_lik is FALSE (default knobs).
          if (.slice_lik) {
            if (!is.matrix(obs_cases))  obs_cases  <- matrix(obs_cases,  nrow = 1L)
            if (!is.matrix(est_cases))  est_cases  <- matrix(est_cases,  nrow = 1L)
            if (!is.matrix(obs_deaths)) obs_deaths <- matrix(obs_deaths, nrow = 1L)
            if (!is.matrix(est_deaths)) est_deaths <- matrix(est_deaths, nrow = 1L)
            obs_cases  <- obs_cases[,  .keep, drop = FALSE]
            est_cases  <- est_cases[,  .keep, drop = FALSE]
            obs_deaths <- obs_deaths[, .keep, drop = FALSE]
            est_deaths <- est_deaths[, .keep, drop = FALSE]
            if (.cases_prefix > 0L) {
              obs_cases[, seq_len(.cases_prefix)] <- NA_real_
              est_cases[, seq_len(.cases_prefix)] <- NA_real_
            }
          }

          # Deaths core: the reported CFR integrated out of this path. Computed on
          # the full (unsliced) results; the resolver applied the same slice.
          # Skipped (zeros) when deaths carry no weight.
          .ll_d_core <- if (!is.null(likelihood_settings$.deaths_integration)) {
            if (isTRUE(likelihood_settings$weight_deaths == 0)) {
              rep(0, likelihood_settings$.deaths_integration$setup$nL)
            } else {
              .mosaic_deaths_ll_integrated(likelihood_settings$.deaths_integration,
                                           model$results, params_sim)$ll
            }
          } else NULL

          calc_model_likelihood(
            config = .config_lik,
            obs_cases = obs_cases,
            est_cases = est_cases,
            obs_deaths = obs_deaths,
            est_deaths = est_deaths,
            weights_time = .wt_lik,
            weights_location = likelihood_settings$weights_location,
            # Per-observation confidence-weight matrices (config_default v4.1+).
            # Sliced + deaths-prefix-zeroed above. NULL on older configs -> no
            # behavior change.
            weights_obs_cases  = .wobs_cases_lik,
            weights_obs_deaths = .wobs_deaths_lik,
            nb_k_cases  = likelihood_settings$.nb_k_cases_resolved,
            nb_k_deaths = likelihood_settings$.nb_k_deaths_resolved,
            eps_rel_cases  = likelihood_settings$eps_rel_cases,
            eps_rel_deaths = likelihood_settings$eps_rel_deaths,
            ll_deaths_core = .ll_d_core,
            # Cases cell rule (default "daily"; "weekly" sums reporting-week
            # totals on the weeks the dispersion was estimated on, offsets
            # resolved once with k). NULL (an older control) means "daily".
            cases_scoring = likelihood_settings$cases_scoring,
            week_offset   = likelihood_settings$.cases_week_offset_resolved,
            weight_cases = likelihood_settings$weight_cases,
            weight_deaths = likelihood_settings$weight_deaths,
            weight_peak_timing = likelihood_settings$weight_peak_timing,
            weight_peak_magnitude = likelihood_settings$weight_peak_magnitude,
            weight_cumulative_total = likelihood_settings$weight_cumulative_total,
            weight_wis = likelihood_settings$weight_wis,
            sigma_peak_time = likelihood_settings$sigma_peak_time,
            sigma_peak_log = likelihood_settings$sigma_peak_log
          )
        } else {
          NA_real_
        }
      }, error = function(e) {
        warning("Simulation ", sim_id, " iteration ", j, " likelihood calculation failed: ",
                e$message, call. = FALSE, immediate. = FALSE)
        NA_real_
      })

      result_matrix[j, 5] <- likelihood

      # Capture raw per-(j, t) output for validation simresults
      if (!is.null(simresults_raw)) {
        raw_cases  <- model$results$reported_cases
        raw_deaths <- model$results$reported_deaths
        if (!is.null(raw_cases) && !is.null(raw_deaths)) {
          if (!is.matrix(raw_cases))  raw_cases  <- matrix(raw_cases,  nrow = 1)
          if (!is.matrix(raw_deaths)) raw_deaths <- matrix(raw_deaths, nrow = 1)
          n_j_raw <- nrow(raw_cases)
          n_t_raw <- ncol(raw_cases)
          simresults_raw[[j]] <- data.frame(
            sim    = sim_id,
            iter   = as.integer(j),
            j      = rep(seq_len(n_j_raw), times = n_t_raw),
            t      = rep(seq_len(n_t_raw), each  = n_j_raw),
            cases  = as.numeric(raw_cases),
            deaths = as.numeric(raw_deaths)
          )
          if (!is.null(params_sim$psi_jt)) {
            simresults_raw[[j]]$psi_jt <- params_sim$psi_jt[cbind(simresults_raw[[j]]$j, simresults_raw[[j]]$t)]
          }
        }
      }
    }

  }

  # Collapse iterations if n_iterations > 1
  if (n_iterations > 1 && nrow(result_matrix) > 0) {
    likelihoods <- result_matrix[, "likelihood"]
    # NA marks a failed replicate and is dropped; -Inf is a zero likelihood and
    # must stay in the log-mean-exp, or the collapse rewards failing replicates.
    valid_ll <- !is.na(likelihoods)

    if (any(valid_ll)) {
      collapsed_ll <- calc_log_mean_exp(likelihoods[valid_ll])
      # Parameters are identical across iterations (sampled once per sim), so take row 1 instead of averaging
      collapsed_params <- result_matrix[1, param_names_all]
      # seed_iter is NA for collapsed rows since they represent all iterations, not just iteration 1
      collapsed_row <- c(sim_id, 1, sim_id, NA_real_, collapsed_ll, collapsed_params)
      result_matrix <- matrix(collapsed_row, nrow = 1)
      colnames(result_matrix) <- c('sim', 'iter', 'seed_sim', 'seed_iter', 'likelihood', param_names_all)
    } else {
      result_matrix <- result_matrix[1, , drop = FALSE]
    }
  }

  # Write parameter file.
  #
  # write_shard = FALSE hands `result_matrix` back instead, so a caller running
  # several simulations can write them as ONE parquet. That matters because a
  # single-row shard at production width is almost entirely framing: 1,352
  # columns cost ~444 KB on disk for ~8 KB of data, so the 100,000-simulation
  # run wrote 52.9 GB of shards for 1.03 GB of data (pipeline plan item 6b).
  # The simulation and scoring above are untouched either way.
  # A failed write is a failed simulation (FALSE), not an error that would
  # abort a sequential run or reach the parallel tally as a try-error string.
  shard_written <- FALSE
  if (isTRUE(write_shard)) {
    output_file <- file.path(dir_cal_samples, sprintf("sim_%07d.parquet", sim_id))
    shard_written <- tryCatch({
      .mosaic_write_parquet(as.data.frame(result_matrix), output_file, io)
      TRUE
    }, error = function(e) {
      warning(sprintf("shard write failed for sim %d: %s", sim_id, conditionMessage(e)),
              call. = FALSE)
      FALSE
    })
  }

  # Write raw simulation results for validation (when save_simresults = TRUE)
  if (!is.null(simresults_raw) && !is.null(dir_cal_simresults)) {
    simresults_raw <- Filter(Negate(is.null), simresults_raw)
    if (length(simresults_raw) > 0) {
      raw_df <- do.call(rbind, simresults_raw)
      param_vals <- as.numeric(result_matrix[1, param_names_all])
      for (pi in seq_along(param_names_all)) {
        raw_df[[param_names_all[pi]]] <- param_vals[pi]
      }
      simresults_file <- file.path(dir_cal_simresults,
                                   sprintf("simresults_%07d.parquet", sim_id))
      # Validation output only: a failed write warns, it does not fail the sim.
      tryCatch(.mosaic_write_parquet(raw_df, simresults_file, io),
               error = function(e) warning(sprintf(
                 "simresults write failed for sim %d: %s", sim_id, conditionMessage(e)),
                 call. = FALSE))
    }
  }

  # No per-simulation gc(). The Python full GC went with the Python engine --
  # there is no reticulate finalizer queue or NumPy heap left to sweep -- and
  # the R `gc()` that stayed behind was measured at 292 ms on a warm worker
  # heap, 14.8% of the whole per-simulation worker budget across the two call
  # sites that used to be here. A forced full collection also defeats R's
  # generational collector. Removing it costs 44 MB of peak worker RSS
  # (913 -> 957 MB over 30 sims), which does not move the worker-count budget.
  # Measurements: v0.72.0 NEWS entry.
  if (!isTRUE(write_shard)) return(result_matrix)
  return(shard_written && file.exists(output_file))
}

# =============================================================================
# PUBLIC API FUNCTIONS
# =============================================================================

#' Run MOSAIC Calibration Workflow
#'
#' @description
#' **Complete Bayesian calibration workflow with full control over model specification.**
#'
#' This function accepts pre-configured config and priors objects, allowing:
#' \itemize{
#'   \item Completely custom configs (non-standard locations, custom data)
#'   \item Fine-grained control over all model parameters
#'   \item Testing with synthetic configurations
#' }
#'
#' Executes the full MOSAIC calibration workflow:
#' \enumerate{
#'   \item Simulation of prior draws: a fixed budget, or adaptive batches that
#'     stop when the sampling checks of the simulation-convergence status
#'     (\code{\link{calc_convergence_status}}; ensemble ESS, Monte Carlo error,
#'     k-hat) are green at two consecutive checks, or at
#'     \code{max_simulations_total}
#'   \item The coarsened posterior over all draws and the ensemble members
#'     (\code{targets$posterior_weighting}; default \code{"bestfit"}), with the
#'     convergence status of those members
#'   \item The posterior ensemble, the optional subset optimiser, and the
#'     parameter posterior from the same members
#'   \item Posterior predictive checks, \code{summary.json} (schema 2) and
#'     \code{calibration_diagnostics.json}
#' }
#'
#' @param config Named list of simulation configuration (REQUIRED). Contains location_name,
#'   reported_cases, reported_deaths, and all model parameters. Create with custom data
#'   or obtain via \code{get_location_config()}.
#' @param priors Named list of prior distributions (REQUIRED). Contains distribution
#'   specifications for all parameters. Create custom or obtain via \code{get_location_priors()}.
#' @param dir_output Character. Output directory for this calibration run (REQUIRED).
#'   All results will be saved here. Must be unique per run.
#' @param control Control list created with [mosaic_control_defaults()]. If \code{NULL}, uses defaults.
#'   Controls calibration strategy, parameter sampling, parallelization, and output settings.
#'   Key settings:
#'   \itemize{
#'     \item \code{calibration$n_simulations}: NULL for auto mode, integer for fixed mode
#'     \item \code{calibration$n_iterations}: stochastic engine iterations per parameter set (default: 3)
#'     \item \code{calibration$max_simulations_total}: Budget cap of the adaptive mode (default: 150000)
#'     \item \code{sampling}: Which parameters to sample vs hold fixed
#'     \item \code{parallel}: Cluster settings for parallel execution
#'   }
#' @param resume Logical (default \code{FALSE}). When \code{TRUE}, the run
#'   reconstructs calibration state from the per-sim shards already present in
#'   \code{<dir_output>/2_calibration/samples/} and continues instead of starting
#'   fresh. The shards on disk are the sole source of truth (an internal
#'   \code{resume_checkpoint.rds} restores the adaptive ESS/phase state when
#'   present, and is removed when the run completes; otherwise the state is
#'   reconstructed from the shards). Behaviour by mode: in adaptive (auto) mode
#'   the run continues from \code{max(sim_id)+1} and does \emph{not} backfill
#'   interior gaps (a lost/quarantined shard reduces the pool); in fixed mode any
#'   missing id within the target is re-run so the exact target is met. With
#'   \code{resume = FALSE}, shards already in the samples directory are moved to
#'   \code{2_calibration/samples_stale_<time>/} (never pooled), and the
#'   post-calibration artifacts of an earlier run into the same directory are
#'   removed before they are rebuilt.
#'
#'   Resume is \strong{rejected} (hard error) when:
#'   \itemize{
#'     \item \code{control$paths$clean_output = TRUE} (the wipe would delete the shards);
#'     \item the shards were already combined -- a consolidated
#'       \code{2_calibration/samples.parquet} exists that the on-disk shards would
#'       shrink (start fresh, or remove it to recompute). Combining deletes the
#'       shards and the resume checkpoint, so this covers not only a finished run
#'       but also one that \strong{died after combining shards and before writing
#'       \code{3_results/summary.json}} (e.g. in the ensemble or figure
#'       steps). Such a run cannot be resumed and must be restarted with
#'       \code{resume = FALSE}. \code{3_results/summary.json} is written last and
#'       is the marker of a completed run;
#'     \item the supplied \code{config}, \code{priors}, \code{control$likelihood},
#'       \code{control$sampling}, \code{control$calibration$n_iterations}, or the
#'       calibration mode (auto vs fixed) differ from those persisted in
#'       \code{1_inputs/} (each changes the draws or likelihood, making the pool
#'       incomparable);
#'     \item the run directory was created before MOSAIC v0.68.0, so its shards
#'       came from the Python \code{laser-cholera} engine rather than the R one
#'       (the two agree statistically but not draw-for-draw, so pooling them
#'       would give a posterior from neither simulator);
#'     \item the likelihood-value provenance differs -- i.e. the existing shards
#'       were scored by a different likelihood engine or implementation than the
#'       current session would produce (an archived Python-scored shard, or an
#'       R likelihood-code change that altered values).
#'   }
#'   If the originating MOSAIC version cannot be determined (no
#'   \code{1_inputs/environment.json}, or an unparseable version) the engine
#'   check is skipped with a warning. Has no effect when no shards exist
#'   (equivalent to a fresh run).
#' @param cluster Optional pre-built R parallel cluster. When provided, skips
#'   cluster creation and teardown, reusing existing workers. Useful for staged
#'   estimation where multiple \code{run_MOSAIC} calls share a cluster. The
#'   caller retains ownership of any cluster passed and is responsible for
#'   stopping it after \code{run_MOSAIC} returns.
#' @param ... Reserved. Supplying a removed argument (\code{dask_spec}) or any
#'   unrecognised argument raises an error naming it rather than silently
#'   ignoring it. See \link{removed_api}.
#'
#' @return Invisibly returns a list with:
#' \describe{
#'   \item{dirs}{Named list of output directories}
#'   \item{files}{Named list of key output files}
#'   \item{summary}{Named list with run statistics: batches, sims, \code{convergence_status} (green / yellow / red), \code{stopping_rule_met} (\code{NA} in fixed mode), the deprecated \code{converged} / \code{convergence_evaluated} (FALSE in fixed mode), resume counts and runtime}
#' }
#'
#' @section Control Structure:
#' See [mosaic_control_defaults()] for complete documentation. The control structure contains:
#' \describe{
#'   \item{calibration}{n_simulations, n_iterations, max_simulations_total, batch_size_adaptive, etc.}
#'   \item{sampling}{sample_tau_i, sample_mobility_gamma, sample_beta_j0_tot, etc.}
#'   \item{parallel}{enable, n_cores, type, progress}
#'   \item{paths}{clean_output, plots}
#'   \item{targets}{posterior_weighting, coarsening_alpha, best_subset_ratio, ESS_best, min/max_ensemble_members, status_thresholds, etc.}
#'   \item{io}{format, compression, compression_level}
#' }
#'
#' @section Output Files:
#' Results are organized in a structured directory tree:
#' \itemize{
#'   \item \code{1_inputs/}: Configuration files (JSON format)
#'   \item \code{2_calibration/samples/}: Simulation results (Parquet format)
#'   \item \code{2_calibration/diagnostics/}: ESS metrics, convergence results
#'   \item \code{2_calibration/posterior/}: Posterior quantiles and distributions
#'   \item \code{3_results/figures/}: Diagnostic, parameter, and prediction plots
#'   \item \code{3_results/}: Final combined results
#' }
#'
#' @examples
#' \dontrun{
#' # === BASIC CUSTOM CONFIG ===
#'
#' # Load and modify default config
#' config <- get_location_config(iso = "ETH")
#' config$population_size <- 1000000
#'
#' priors <- get_location_priors(iso = "ETH")
#'
#' run_MOSAIC(
#'   config = config,
#'   priors = priors,
#'   dir_output = "./output"
#' )
#'
#' # === MULTI-LOCATION WITH CUSTOM CONTROL ===
#'
#' config <- get_location_config(iso = c("ETH", "KEN", "TZA"))
#' priors <- get_location_priors(iso = c("ETH", "KEN", "TZA"))
#'
#' ctrl <- mosaic_control_defaults(
#'   calibration = list(
#'     n_simulations = 5000,  # Fixed mode
#'     n_iterations = 5
#'   ),
#'   parallel = list(enable = TRUE, n_cores = 16)
#' )
#'
#' run_MOSAIC(config, priors, "./output", ctrl)
#'
#' # === CUSTOM PRIORS FOR SENSITIVITY ===
#'
#' config <- get_location_config(iso = "ETH")
#' priors <- get_location_priors(iso = "ETH")
#'
#' # Tighten the departure-probability (tau_i) prior for ETH. Location priors
#' # live under priors$parameters_location; tau_i is lognormal(meanlog, sdlog).
#' tau_eth <- priors$parameters_location$tau_i$location$ETH$parameters
#' tau_eth$sdlog <- tau_eth$sdlog / 2
#' priors$parameters_location$tau_i$location$ETH$parameters <- tau_eth
#'
#' run_MOSAIC(config, priors, "./output")
#'
#' # === COMPLETELY CUSTOM CONFIG ===
#'
#' # Non-standard location names
#' custom_config <- list(
#'   location_name = c("Region1", "Region2"),
#'   reported_cases = my_cases_data,
#'   reported_deaths = my_deaths_data,
#'   # ... all other simulation parameters
#' )
#'
#' custom_priors <- list(
#'   # ... custom prior specifications
#' )
#'
#' run_MOSAIC(custom_config, custom_priors, "./output")
#'
#' # === RESUME AN INTERRUPTED RUN ===
#'
#' # If a calibration is killed mid-run, re-call with resume = TRUE and the same
#' # config/priors/output directory. Completed simulations on disk are reused and
#' # the run continues from the next sim_id (bit-identical to an uninterrupted run).
#' # This works only while the per-sim shards exist: a run that died after they
#' # were combined into samples.parquet (no 3_results/summary.json) must be rerun
#' # with resume = FALSE.
#' run_MOSAIC(config, priors, "./output", resume = TRUE)
#' }
#'
#' @seealso [mosaic_control_defaults()] for building control structures
#' @export
run_MOSAIC <- function(config,
                       priors,
                       dir_output,
                       control   = NULL,
                       resume    = FALSE,
                       cluster   = NULL,
                       ...) {

  # ===========================================================================
  # ARGUMENT VALIDATION
  # ===========================================================================

  stopifnot(
    "config is required and must be a list" =
      !missing(config) && is.list(config) && length(config) > 0,
    "priors is required and must be a list" =
      !missing(priors) && is.list(priors) && length(priors) > 0,
    "dir_output is required and must be character string" =
      !missing(dir_output) && is.character(dir_output) && length(dir_output) == 1L,
    "resume must be a single logical (TRUE/FALSE)" =
      is.logical(resume) && length(resume) == 1L && !is.na(resume)
  )

  # Validate config structure
  if (!"location_name" %in% names(config)) {
    stop("config must contain 'location_name' field", call. = FALSE)
  }

  # Extract iso_code from config for logging
  iso_code <- config$location_name

  # Removed arguments (`dask_spec`, and the engine-plotting arguments) are
  # rejected loudly rather than absorbed -- see .mosaic_reject_removed_args().
  .mosaic_reject_removed_args(list(...), "run_MOSAIC")

  # ===========================================================================
  # SETUP MOSAIC ROOT DIRECTORY
  # ===========================================================================

  # Check if root_directory is already set (from previous set_root_directory() call)
  root_dir <- getOption("root_directory")

  if (is.null(root_dir)) {
    stop(
      "MOSAIC root directory not set. Please set once per session:\n",
      "  set_root_directory('/path/to/MOSAIC')\n",
      "Or set the option directly:\n",
      "  options(root_directory = '/path/to/MOSAIC')\n",
      "The root should contain MOSAIC-pkg/, MOSAIC-data/, etc.",
      call. = FALSE
    )
  }

  if (!dir.exists(root_dir)) {
    stop("MOSAIC root directory does not exist: ", root_dir, call. = FALSE)
  }

  # Validate critical subdirectories exist
  required_dirs <- c("MOSAIC-pkg", "MOSAIC-data")
  for (d in required_dirs) {
    if (!dir.exists(file.path(root_dir, d))) {
      warning("Expected directory not found: ", file.path(root_dir, d), call. = FALSE)
    }
  }

  # ===========================================================================
  # LOAD CONTROL DEFAULTS AND EXTRACT PARAMETERS
  # ===========================================================================

  log_msg("Using user-provided config for: %s", paste(iso_code, collapse = ", "))
  .mosaic_validate_config(config, iso_code)

  log_msg("Using user-provided priors")
  .mosaic_validate_priors(priors, config)

  # Load control defaults if not provided
  if (is.null(control)) {
    control <- mosaic_control_defaults()
  }

  # Merge and validate control
  control <- .mosaic_validate_and_merge_control(control)

  # Resume is incompatible with clean_output: the directory wipe would delete
  # the very shards resume reads from. Fail fast before any heavy setup.
  if (isTRUE(resume) && isTRUE(control$paths$clean_output)) {
    stop("resume = TRUE is incompatible with control$paths$clean_output = TRUE: ",
         "cleaning the output directory would delete the shards resume reads from.",
         call. = FALSE)
  }

  # Extract parameters from control structure
  n_iterations <- control$calibration$n_iterations
  n_simulations <- control$calibration$n_simulations
  sampling_args <- control$sampling

  # Validate extracted parameters
  if (!is.numeric(n_iterations) || n_iterations < 1) {
    stop("control$calibration$n_iterations must be a positive integer", call. = FALSE)
  }
  if (!is.null(n_simulations) && !is.character(n_simulations) && (!is.numeric(n_simulations) || n_simulations < 1)) {
    stop("control$calibration$n_simulations must be NULL, 'auto', or a positive integer", call. = FALSE)
  }

  # Validate sampling_args
  sampling_args <- .mosaic_validate_sampling_args(sampling_args)

  # (No Python environment check: simulation and calibration are pure R since
  # v0.68.0. check_python_env() remains available for est_suitability() users.)

  # ===========================================================================
  # BLAS THREAD CONTROL CHECK (Critical for cluster performance)
  # ===========================================================================

  .mosaic_check_blas_control()

  # Pin every thread pool on THIS orchestrator process to 1 regardless of backend
  # (local-sequential or local-PSOCK). This previously ran only inside the
  # Dask branch, leaving the local in-process engine path (sequential calibration +
  # calc_model_ensemble's sequential branch) able to oversubscribe numba/MKL/
  # OpenBLAS on a multi-core host. Idempotent with the per-worker pins; PSOCK
  # workers set their own pins separately (fresh processes).
  .mosaic_set_blas_threads(1L)
  try(arrow::set_cpu_count(1L), silent = TRUE)
  try(arrow::set_io_thread_count(2L), silent = TRUE)

  # ===========================================================================
  # SETUP PATHS
  log_msg("Starting MOSAIC calibration")

  # Set root directory and get paths
  set_root_directory(root_dir)
  PATHS <- get_paths()

  # Create directory structure
  dirs <- .mosaic_ensure_dir_tree(
    dir_output = dir_output,
    clean_output = isTRUE(control$paths$clean_output)
  )

  # Direct every log_msg / log_info / log_warn / log_error / log_fatal call
  # (including those from helpers whose frames don't carry a dir_output
  # binding) to <dir_output>/run.log via a package-level option. Cleared on
  # function exit so a subsequent unrelated log_msg() call doesn't tee to a
  # stale file. Emit a single discoverability line so a tailing process can
  # `grep -m1 '\[LOG_DESTINATION\]'` to locate the file.
  .mosaic_log_file <- file.path(dir_output, "run.log")
  .mosaic_prev_log_file <- getOption("MOSAIC.log_file", default = NULL)
  options(MOSAIC.log_file = .mosaic_log_file)
  on.exit(options(MOSAIC.log_file = .mosaic_prev_log_file), add = TRUE)
  log_msg("[LOG_DESTINATION] stdout=true file=%s", .mosaic_log_file)

  # Run-completion flag for the on.exit [RUN_FATAL] sentinel below. Set TRUE
  # at the bottom of run_MOSAIC after [RUN_SUMMARY] is emitted, and also at
  # every early-return path that emits its own [RUN_FATAL]. If the function
  # exits via an uncaught stop() (e.g. client$gather raises FutureCancelledError
  # mid-calibration), the flag stays FALSE and on.exit emits a single
  # grep-friendly [RUN_FATAL] line so a tailing AI agent has an unambiguous
  # termination signal instead of a hanging tail with a stack trace at EOF.
  run_completed       <- FALSE
  run_fatal_start_time <- Sys.time()
  on.exit({
    if (!isTRUE(run_completed)) {
      runtime_min <- as.numeric(difftime(Sys.time(), run_fatal_start_time,
                                          units = "mins"))
      log_msg(paste0(
        "[RUN_FATAL] status=failed reason=runtime_error runtime_min=%.2f ",
        "dir_output=%s"),
        runtime_min, dir_output)
    }
  }, add = TRUE)

  # Conditionally create simresults directory for validation output
  save_simresults <- isTRUE(control$io$save_simresults)

  # `save_simresults` used to be rejected outright on the remote path, because
  # remote workers returned per-iter scalar likelihoods rather than full
  # time-series matrices and there was no raw (j, t) output to persist. With
  # only the local path left, the restriction is gone and every run can capture
  # diagnostics.
  if (save_simresults) {
    dirs$cal_simresults <- file.path(dir_output, "2_calibration/simulation_results")
    dir.create(dirs$cal_simresults, recursive = TRUE, showWarnings = FALSE)
  }

  # ===========================================================================
  # WRITE SETUP FILES (with cluster metadata)
  # ===========================================================================

  # On resume, verify the supplied config/priors/likelihood match those
  # persisted by the interrupted run BEFORE the setup files below overwrite
  # them. A mismatch changes the sampling/likelihood target and is a hard error.
  if (isTRUE(resume)) {
    .mosaic_resume_check_inputs(dirs, config, priors, control)

    # Refuse to resume into an existing consolidated samples.parquet when the
    # shards on disk would produce a SMALLER pool: combining + overwriting would
    # silently shrink/destroy the finished posterior. This covers both a fully
    # completed run (shards consolidated and deleted -> 0 shards) and the
    # dangerous mixed case (a large samples.parquet plus a few stray shards). A
    # crash mid-consolidation leaves the shards as a superset of samples.parquet,
    # so re-consolidation is idempotent and is allowed.
    samples_file <- file.path(dirs$calibration, "samples.parquet")
    if (file.exists(samples_file)) {
      n_consolidated <- tryCatch(arrow::open_dataset(samples_file)$num_rows,
                                 error = function(e) NA_integer_)
      n_valid_shards <- .mosaic_resume_scan(dirs$cal_samples)$n
      if (!is.na(n_consolidated) && n_valid_shards < n_consolidated) {
        stop(sprintf(paste0(
          "resume = TRUE but %s already holds a consolidated posterior (%d rows) and only %d ",
          "valid per-sim shard(s) remain. Resuming would overwrite it with a smaller pool and ",
          "lose draws. Start a fresh run in a new directory, or remove samples.parquet to ",
          "recompute from the shards."),
          file.path("2_calibration", "samples.parquet"), n_consolidated, n_valid_shards),
          call. = FALSE)
      }
    }
  } else {
    # A fresh (non-resume) run issues sim ids from 1 and the combine step reads
    # every sim_*.parquet in the samples directory, so shards left by an
    # earlier interrupted run would be pooled into this posterior unchecked
    # (possibly from other priors or another config). Move them aside.
    .mosaic_quarantine_stale_shards(dirs, log_warn)
  }

  # Capture full environment snapshot (versions, system, git, data)
  env_snapshot <- .mosaic_capture_environment(
    config = config, priors = priors, control = control
  )
  # Stamp the likelihood-value provenance and the engine semantics so a later
  # resume can refuse to pool shards scored or simulated by incompatible code.
  env_snapshot <- .mosaic_stamp_resume_provenance(env_snapshot)
  .mosaic_write_json(env_snapshot, file.path(dirs$inputs, "environment.json"), control$io)
  log_msg("  Saved %s", "1_inputs/environment.json")

  control_record <- list(
    control = .mosaic_control_for_json(control),
    n_iterations = n_iterations,
    iso_code = iso_code,
    timestamp = Sys.time(),
    paths = list(
      dir_output = dirs$root,
      dir_inputs = dirs$inputs,
      dir_calibration = dirs$calibration,
      dir_results = dirs$results
    )
  )

  log_msg("Writing setup files...")
  .mosaic_write_json(control_record, file.path(dirs$inputs, "control.json"), control$io, na = "null")
  log_msg("  Saved %s", "control.json")

  .mosaic_write_json(priors, file.path(dirs$inputs, "priors.json"), control$io)
  log_msg("  Saved %s", "priors.json")

  .mosaic_write_json(config, file.path(dirs$inputs, "config.json"), control$io)
  log_msg("  Saved %s", "config.json")

  # tau_i 95% CI artifact (Phase B) for the "spatial" figure group (fig 2).
  # Unconditional (not gated by plots); absence of the upstream fit => no file
  # and a point-only tau plot.
  tryCatch({
    tau_ci_path <- .mosaic_write_tau_ci(
      file.path(PATHS$MODEL_INPUT, "mobility_travel_prob_params.csv"),
      config$location_name, dirs$inputs)
    if (!is.null(tau_ci_path)) log_msg("  Saved %s", "1_inputs/mobility_tau_ci.csv")
  }, error = function(e)
    log_warn("mobility_tau_ci.csv write skipped: %s", e$message))

  # ===========================================================================
  # PARAMETER NAME DETECTION
  # ===========================================================================

  tmp <- convert_config_to_matrix(config)
  if ("seed" %in% names(tmp)) tmp <- tmp[names(tmp) != "seed"]
  param_names_all <- names(tmp)
  rm(tmp)

  # Precompute lookup for fast parameter extraction in worker hot loop
  param_lookup <- .mosaic_build_param_lookup(param_names_all, config$location_name)

  # Filter to estimated parameters for ESS tracking using metadata table.
  # Uses the `scale` column ("global" vs "location") from estimated_parameters
  # to determine which params have ISO suffixes, instead of fragile regex.
  est_params_df <- get("estimated_parameters", envir = asNamespace("MOSAIC"))
  if (!is.data.frame(est_params_df) || nrow(est_params_df) == 0) {
    stop("Failed to load MOSAIC::estimated_parameters")
  }

  estimable_global   <- est_params_df$parameter_name[est_params_df$scale == "global"]
  estimable_location <- est_params_df$parameter_name[est_params_df$scale == "location"]

  # Derive base name for each param: direct match for globals,
  # strip known location suffixes (_ISO) for location-specific params
  location_names <- config$location_name
  location_suffixes <- paste0("_", location_names)
  param_base <- param_names_all
  for (sfx in location_suffixes) {
    hits <- endsWith(param_names_all, sfx)
    param_base[hits] <- sub(paste0(sfx, "$"), "", param_names_all[hits])
  }

  is_estimable <- param_base %in% c(estimable_global, estimable_location)
  param_names_estimated <- param_names_all[is_estimable]
  # Exclude initial condition counts (proportions are estimable, counts are derived)
  param_names_estimated <- param_names_estimated[!grepl("^[NSEIRV][12]?_j_initial", param_names_estimated)]

  if (!length(param_names_estimated)) {
    stop("No estimated parameters found for ESS tracking")
  }

  # Derive disabled parameter names from sampling_args flags so the log
  # message reflects the actual number being sampled in this run.
  # Flags use "sample_<name>" convention; strip the prefix to get the config
  # key. Handle the special-case name mappings used in sample_parameters.R.
  disabled_base_params <- character(0)
  if (!is.null(sampling_args)) {
    disabled_flags <- names(sampling_args)[vapply(sampling_args, isFALSE, logical(1))]
    disabled_flags <- disabled_flags[startsWith(disabled_flags, "sample_")]
    disabled_base <- gsub("^sample_", "", disabled_flags)
    special_map <- list(
      beta_j0_tot = c("beta_j0_tot", "beta_j0_hum", "beta_j0_env"),
      initial_conditions = c("prop_S_initial", "prop_E_initial", "prop_I_initial",
                             "prop_R_initial", "prop_V1_initial", "prop_V2_initial")
    )
    resolved <- unlist(lapply(disabled_base, function(nm) {
      if (nm %in% names(special_map)) special_map[[nm]] else nm
    }))
    disabled_base_params <- unique(resolved)
  }

  param_names_sampled <- param_names_estimated[
    !(gsub("_[A-Z]{3}$", "", param_names_estimated) %in% disabled_base_params)
  ]

  log_msg("Parameters: %d sampled (of %d estimable, %d total) | Locations: %s",
          length(param_names_sampled), length(param_names_estimated),
          length(param_names_all),
          paste(config$location_name, collapse = ', '))

  # ===========================================================================
  # CLUSTER INITIALISATION
  # ===========================================================================

  # Resolve and normalize weights_time into the private slot used by all workers.
  # Normalize to sum to n_t (number of time points) so the scale is independent
  # of user-supplied magnitude.
  if (!is.null(control$likelihood$weights_time)) {
    n_t_cfg <- if (is.matrix(config$reported_cases)) ncol(config$reported_cases) else
                 length(config$reported_cases)
    wt_raw <- control$likelihood$weights_time
    if (length(wt_raw) == 1L) wt_raw <- rep(wt_raw, n_t_cfg)
    control$likelihood$.weights_time_resolved <- wt_raw / sum(wt_raw) * n_t_cfg
  } else {
    control$likelihood$.weights_time_resolved <- NULL
  }

  # Resolve the per-channel scored window (burn-in + deaths-era start) ONCE here
  # so both backends and every worker read identical indices. All-default knobs
  # => idx_cases = idx_deaths = 1 (no slicing, bit-identical). Stashed in a
  # private slot parallel to .weights_time_resolved; not serialized to
  # control.json (resolution happens after the inputs are written), so the
  # resume guard byte-compares only the public knobs in control$likelihood.
  control$likelihood$.score_window_resolved <-
    .mosaic_resolve_score_window(config, control)
  if (control$likelihood$.score_window_resolved$idx_cases > 1L ||
      control$likelihood$.score_window_resolved$idx_deaths > 1L) {
    log_msg("Scored window: cases from step %d, deaths from step %d (of %d) -- unscored head omitted",
            control$likelihood$.score_window_resolved$idx_cases,
            control$likelihood$.score_window_resolved$idx_deaths,
            control$likelihood$.score_window_resolved$n_time)
  }

  # A location with nothing scorable in either channel inside the scored window
  # scores NA for every draw. When that holds for every location, there is
  # nothing to weight, so stop here instead of running a calibration whose every
  # likelihood is NA. The cases gate mirrors calc_model_likelihood(): the weekly
  # core (cases_scoring = "weekly") needs three complete reporting weeks; per-day
  # cells (the default "daily") or an active shape term keep the
  # any-finite-observation gate.
  .weekly_gate <- .mosaic_weekly_cases_gate(control$likelihood)
  .unscorable <- .mosaic_unscorable_locations(
    config$reported_cases, config$reported_deaths,
    control$likelihood$.score_window_resolved$idx_cases,
    control$likelihood$.score_window_resolved$idx_deaths,
    weekly_cases = .weekly_gate)
  .scorable_rule <- if (.weekly_gate) {
    "three complete reporting weeks of reported_cases or a finite reported_deaths observation"
  } else "a finite reported_cases or reported_deaths observation"
  if (all(.unscorable)) {
    stop("No location has ", .scorable_rule, " in the scored window (",
         paste(config$location_name, collapse = ", "),
         "), so every likelihood would be NA. Check the observation data, date range ",
         "and burn-in / deaths-era settings.", call. = FALSE)
  }
  if (any(.unscorable)) {
    log_msg("Nothing scorable in the scored window for %s (needs %s): these locations contribute nothing to the likelihood",
            paste(config$location_name[.unscorable], collapse = ", "), .scorable_rule)
  }

  # Scored observation cells: the n_obs of the coarsened posterior's learning
  # rate eta = coarsening_alpha / n_obs. It depends on the observations and the
  # scored window only, so it is computed once; the auto-mode stopping rule and
  # the post-calibration posterior (every scheme: weight_all and the
  # convergence status) both read it.
  n_obs_scored <- .mosaic_n_scored_obs(config, control$likelihood$.score_window_resolved)
  if (!is.finite(n_obs_scored)) {
    stop("The posterior needs the number of scored observation cells, but the config has no ",
         "finite reported_cases or reported_deaths inside the scored window.", call. = FALSE)
  }
  log_msg("Coarsened posterior: alpha = %g over %s scored observation cells (eta = %.3g); ensemble: %s",
          control$targets$coarsening_alpha, format(n_obs_scored, big.mark = ","),
          control$targets$coarsening_alpha / n_obs_scored,
          switch(control$targets$posterior_weighting,
                 bestfit = sprintf("best-fit selection to %g x ESS_all, %d-%d members",
                                   control$targets$best_subset_ratio,
                                   as.integer(control$targets$min_ensemble_members),
                                   as.integer(control$targets$max_ensemble_members)),
                 coarsened = "systematic resample (posterior_weighting = \"coarsened\")",
                 sprintf("legacy tier-searched best subset (posterior_weighting = \"%s\")",
                         control$targets$posterior_weighting)))
  if (identical(control$likelihood$cases_scoring, "weekly")) {
    log_msg(paste0("  Note: n_obs counts location-DAYS under either cases rule. The weekly cases ",
                   "likelihood sums about one-seventh as many cases cells, so with the same ",
                   "coarsening_alpha the posterior is flatter than under daily scoring ",
                   "(raise coarsening_alpha to compensate)."))
  }
  # eta is pooled over every location's scored cells (red team round 1, D2:
  # unchanged in 1.4.0), so each location carries about alpha / n_locations.
  n_loc_run <- length(config$location_name)
  if (n_loc_run > 1L)
    log_msg(paste0("  Note: eta = alpha / n_obs pools the scored cells of all %d locations, so each location ",
                   "carries about alpha / %d = %.3g effective observations (alpha_per_location)"),
            n_loc_run, n_loc_run, control$targets$coarsening_alpha / n_loc_run)

  # Estimate the per-location NB dispersion ONCE per calibration. k is a property
  # of the OBSERVATIONS alone, so it is identical for every simulation; the
  # former code recomputed it inside every likelihood call (~7.2M redundant
  # evaluations for a 40-location, 30,000-simulation run). Stashed in a private
  # slot alongside .weights_time_resolved, so it reaches every PSOCK worker via
  # the existing clusterExport of likelihood_settings.
  # Resolved AFTER .score_window_resolved so the dispersion is estimated on the
  # SAME window the likelihood scores. burn_in_days defaults to 30, so scoring
  # otherwise starts at step 31 while k was estimated from step 1 -- the
  # "k-coherence" property the previous implementation documented and held.
  .nb_disp <- .mosaic_resolve_nb_dispersion(config, control,
                                            score_window = control$likelihood$.score_window_resolved)
  control$likelihood$.nb_k_cases_resolved  <- .nb_disp$cases$k
  control$likelihood$.nb_k_deaths_resolved <- .nb_disp$deaths$k
  control$likelihood$.nb_dispersion_table  <- .nb_disp$table
  # The weekly cases rule (cases_scoring = "weekly") sums cases over the
  # reporting weeks the dispersion was estimated on; resolved here once, with
  # k, for every worker.
  control$likelihood$.cases_week_offset_resolved <- .nb_disp$cases$week_offset
  log_msg("NB dispersion (weekly, conditional ML): cases median k = %s, deaths median k = %s | Poisson: %d cases, %d deaths of %d locations",
          .nb_disp$cases$summary, .nb_disp$deaths$summary,
          sum(is.infinite(.nb_disp$cases$k)), sum(is.infinite(.nb_disp$deaths$k)),
          length(.nb_disp$cases$k))
  .cases_scoring <- if (is.null(control$likelihood$cases_scoring)) "daily" else
    control$likelihood$cases_scoring
  log_msg("Cases likelihood: %s (cases_scoring = '%s'); dispersion from %s",
          if (identical(.cases_scoring, "weekly"))
            "NB on reporting-week totals at the weekly k" else "one NB cell per day at the weekly k",
          .cases_scoring,
          if (identical(.nb_disp$cases$summary, "user-supplied")) {
            "the user-supplied nb_k_cases"
          } else if (isTRUE(.nb_disp$tier_used)) {
            sprintf("observed weeks only (%d reconstructed/imputed weeks excluded)",
                    sum(.nb_disp$table$n_weeks_excluded[.nb_disp$table$channel == "cases"], na.rm = TRUE))
          } else if (is.null(config$reported_tier)) {
            "every week (config carries no reported_tier)"
          } else "every week (reported_tier was not applied; see the warning above)")
  # Bound-bind rate is a standing fit diagnostic: in a well-specified fit the
  # bounds should rarely bind. The retired k_min floor bound in 27 of 28
  # estimable locations, which was the defect rather than a setting.
  .nb_bind <- sum(.nb_disp$table$status %in% "clamped_lower_bound", na.rm = TRUE)
  .nb_near <- sum(.nb_disp$table$status %in% "near_lower_bound", na.rm = TRUE)
  .nb_noest <- sum(grepl("^no_estimate", .nb_disp$table$status), na.rm = TRUE)
  .nb_panel <- .nb_disp$table$location[.nb_disp$table$panel_trend %in% TRUE]
  if (.nb_bind > 0 || .nb_near > 0 || .nb_noest > 0)
    log_msg("NB dispersion: %d clamped at a hard bound, %d near it, %d with no own estimate (%d cases from the panel trend%s), of %d location-channels -- see 2_calibration/diagnostics/nb_dispersion.csv",
            .nb_bind, .nb_near, .nb_noest, length(.nb_panel),
            if (length(.nb_panel)) paste0(": ", paste(.nb_panel, collapse = ", ")) else "",
            nrow(.nb_disp$table))
  tryCatch({
    if (!dir.exists(dirs$cal_diag)) dir.create(dirs$cal_diag, recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(.nb_disp$table,
                     file.path(dirs$cal_diag, "nb_dispersion.csv"), row.names = FALSE)
  }, error = function(e)
    log_msg("WARNING: could not write nb_dispersion.csv: %s", conditionMessage(e)))

  # Deaths are scored with the reported CFR integrated out (v0.96.0): per
  # simulated path, the CFR's location offset and year deviations around the
  # config's time-varying mu_jt are solved analytically (Laplace) instead of
  # sampled. Everything that depends only on the observations -- weekly blocks,
  # weekly observed totals and weights, the prior widths -- is resolved here once
  # and reaches every worker through likelihood_settings. It estimates its own
  # per-location dispersion phi_j (deaths against observed cases, on the observed
  # weeks when the config carries reported_tier, as for the cases k); the NB
  # deaths k above is diagnostic only. The ensemble's observation-level deaths
  # draws use the same phi_j.
  control$likelihood$.deaths_integration <- .mosaic_resolve_deaths_integration(
    config, control, priors, score_window = control$likelihood$.score_window_resolved)
  if (!is.null(control$likelihood$.deaths_integration)) {
    local({
      di <- control$likelihood$.deaths_integration
      log_msg("Deaths likelihood: reported CFR integrated out per path (weekly quasi-Poisson, %d years, dispersion median %.2f, location sd median %.2f, year sd %.2f)",
              length(di$years), stats::median(di$dispersion), stats::median(di$sd_shift),
              di$sd_year)
      if (isTRUE(di$tier_used)) {
        short <- config$location_name[di$dispersion_observed_insufficient %in% TRUE]
        log_msg("Deaths dispersion from observed weeks only (%d reconstructed/imputed weeks excluded)%s",
                sum(di$dispersion_weeks_excluded),
                if (length(short)) sprintf("; too few observed weeks, so every week used for %s",
                                           paste(short, collapse = ", ")) else "")
      }
    })
    # Persisted so a post-hoc calc_model_ensemble() re-run can redraw deaths from
    # the calibrated CFR exactly as the run's own ensemble does.
    tryCatch({
      saveRDS(control$likelihood$.deaths_integration,
              file.path(dirs$calibration, "deaths_integration.rds"))
      log_msg("Saved 2_calibration/deaths_integration.rds")
    }, error = function(e)
      log_warn("deaths_integration.rds write skipped: %s", conditionMessage(e)))
  }


  # Resolve per-location (cross-location) influence weights. When the caller did
  # NOT supply weights_location, derive a data-driven default that down-weights
  # low-signal / absence countries so they cannot dominate the importance weights
  # (see .mosaic_derive_weights_location). The derivation returns NULL -- leaving
  # weights_location at NULL (= uniform rep(1, n_loc) in calc_model_likelihood)
  # -- whenever the per-cell weight matrices are absent or the data are uniform,
  # preserving byte-identical prior behavior. An explicitly-supplied
  # weights_location is a deliberate caller override and is left untouched. Both
  # Downstream consumers read control$likelihood$weights_location, so
  # resolving it here covers both backends in lockstep.
  if (is.null(control$likelihood$weights_location)) {
    wl_floor   <- 0.05
    derived_wl <- .mosaic_derive_weights_location(config, floor = wl_floor)
    if (!is.null(derived_wl)) {
      control$likelihood$weights_location <- derived_wl
      log_msg(paste0("Derived data-driven weights_location (down-weighting ",
                     "low-signal/absence countries): %d below 0.5, floor %.3f, ",
                     "range [%.3f, %.3f]"),
              sum(derived_wl < 0.5), wl_floor,
              min(derived_wl), max(derived_wl))
    }
  }

  # --- Local R parallel cluster ---

  # Force sequential if parallel is disabled
  if (!isTRUE(control$parallel$enable)) {
    control$parallel$n_cores <- 1L
  }

  # Connection budget, reported ONCE up front. Every PSOCK worker holds one R
  # connection out of a startup-fixed table (128 by default), so on a host with
  # more cores than that the run quietly does less work than asked -- the
  # calibration cluster clamps and both ensembles clamp with it. Surface it in
  # the run log now rather than leaving it to be inferred from worker counts
  # hours later. Raising it requires a restart: --max-connections is a startup
  # option, so nothing here can fix it in-session.
  if (control$parallel$n_cores > 1L) {
    .conn_free <- tryCatch(as.integer(parallelly::freeConnections()),
                           error = function(e) NA_integer_)
    .conn_all  <- tryCatch(as.integer(parallelly::availableConnections()),
                           error = function(e) NA_integer_)
    if (!is.na(.conn_free) && control$parallel$n_cores > .conn_free - 2L) {
      log_warn(paste0(
        "n_cores = %d exceeds the R connection budget (%d free of %d slots): ",
        "the calibration cluster AND both ensembles will use at most %d workers. ",
        "Restart R with `--max-connections=%d` (R >= 4.4.0, max 4096) to use all %d."),
        control$parallel$n_cores, .conn_free, .conn_all,
        max(1L, .conn_free - 2L),
        min(4096L, max(256L, control$parallel$n_cores + 64L)),
        control$parallel$n_cores)
    } else if (!is.na(.conn_free)) {
      log_msg("Connection budget OK: %d workers requested, %d of %d connections free",
              control$parallel$n_cores, .conn_free, .conn_all)
    }
  }

  # Determine cluster ownership:
  #   owns_cluster = TRUE  -> we created it, we tear it down
  #   owns_cluster = FALSE -> caller provided it, caller tears it down
  owns_cluster <- FALSE

  if (!is.null(cluster)) {
    # User provided a pre-built cluster (e.g. from make_mosaic_cluster)
    cl <- cluster
    owns_cluster <- FALSE
    log_msg("Using pre-built cluster (%d workers)", length(cl))
  } else if (control$parallel$n_cores > 1L) {
    # Create cluster internally via make_mosaic_cluster
    cl <- make_mosaic_cluster(
      n_cores = control$parallel$n_cores,
      type = control$parallel$type
    )
    owns_cluster <- TRUE
    # length(cl), not the request: make_mosaic_cluster() may have clamped it
    # to the free R connections.
    log_msg("Created %s cluster with %d workers%s", control$parallel$type, length(cl),
            if (length(cl) < control$parallel$n_cores)
              sprintf(" (requested %d)", control$parallel$n_cores) else "")

    # Register cleanup handler (will be called on normal exit or error)
    on.exit({
      if (owns_cluster && !is.null(cl)) {
        try({
          # Not stopCluster(): a worker that is not reading its socket (an
          # interrupted run, a task that stalled) survives the shutdown
          # request and then holds this process's stdout open, so a finished
          # run looks hung. See ?.mosaic_stop_cluster.
          .mosaic_stop_cluster(cl)
          log_msg("Cluster stopped successfully")
        }, silent = TRUE)
      }
    }, add = TRUE)
  } else {
    log_msg("Running sequentially (n_cores = 1)")
    cl <- NULL
  }

  # Export per-run data to workers (needed every run, even with reused cluster)
  if (!is.null(cl)) {
    likelihood_settings <- control$likelihood
    io_settings <- control$io

    parallel::clusterExport(cl,
      c("n_iterations", "priors", "config", "PATHS", "param_names_all", "param_lookup",
        "sampling_args", "dirs",
        "likelihood_settings", "io_settings"),
      envir = environment())

    # Install worker functions using the exported per-run variables. The
    # installer is reparented to globalenv(), so clusterCall serialises the
    # function alone rather than the whole run_MOSAIC frame, and the installed
    # closures resolve config/priors/... from the clusterExport'ed globals.
    parallel::clusterCall(cl, .mosaic_worker_installer())
  }


  # ===========================================================================
  # DETERMINE RUN MODE: AUTO vs FIXED
  # ===========================================================================

  nspec <- .mosaic_normalize_n_sims(n_simulations)
  state_file <- file.path(dirs$cal_state, "run_state.json")
  checkpoint_file <- file.path(dirs$cal_state, "resume_checkpoint.rds")

  state <- .mosaic_init_state(control, param_names_sampled, nspec)
  # The "coarsened" resample seed, so the adaptive gate resamples the members
  # the post-calibration block will build (.mosaic_scheme_members()).
  state$resample_seed <- .mosaic_resample_seed(config)
  state$n_locations <- length(config$location_name)

  # Resume: overlay the fresh-init state with state reconstructed from the
  # shards on disk. When resume = FALSE this branch is skipped and the run is
  # byte-identical to a fresh calibration. Only auto mode reconstructs the
  # adaptive ESS/phase state here; fixed mode recovers purely from done_ids
  # below (a single directory scan), so it is left out to avoid scanning twice.
  resumed_n_reused <- 0L
  # The adaptive gate's in-memory pool of (sim, likelihood, sampled parameters):
  # each check appends only the new shards (.mosaic_gate_pool_update()); a
  # resume seeds it from the shards on disk.
  gate_pool <- NULL
  if (isTRUE(resume) && identical(state$mode, "auto")) {
    state <- .mosaic_reconstruct_state(state, dirs, control, param_names_sampled, n_obs_scored)
    gate_pool <- state$.gate_pool
    state$.gate_pool <- NULL
    resumed_n_reused <- state$total_sims_run
    if (resumed_n_reused > 0L) {
      log_msg("[RESUME] Reusing %d existing simulation(s); continuing from sim_id %d in phase '%s' (batch %d)",
              resumed_n_reused, resumed_n_reused + 1L, state$phase, state$batch_number)
    }
  }
  # Record resume provenance for the end-of-run summary.
  state$resumed       <- isTRUE(resume)
  state$n_sims_reused <- if (isTRUE(resume)) resumed_n_reused else 0L

  if (isTRUE(resume) && identical(state$mode, "auto") && isTRUE(state$converged)) {
    # Already converged on the reconstructed pool: skip simulation entirely and
    # go straight to post-processing (the loop below breaks immediately).
    log_msg("[RESUME] Reconstructed state already converged \u2014 skipping simulation, proceeding to post-processing")
  } else {
    log_msg("Starting simulation (mode: %s)", state$mode)
  }
  start_time <- Sys.time()

  # Disk space check removed in v0.17.10 -- fragile across platforms (macOS df
  # output parsing) and low value on local machines. Cluster deployments should
  # check quotas at the job scheduler level, not inside R.

  # ===========================================================================
  # SIMULATION: FIXED MODE
  # ===========================================================================

  if (identical(state$mode, "fixed")) {

    target <- state$fixed_target

    log_msg("[FIXED MODE] Running exactly %d simulations", target)

    all_ids <- seq_len(target)
    # On resume, skip sim_ids whose shards already exist on disk. setdiff fills
    # any gaps (a missing id is re-run, reproducing the same draw via its seed)
    # so the fixed target is met exactly.
    done_ids <- if (isTRUE(resume)) {
      intersect(.mosaic_resume_scan(dirs$cal_samples)$ids, all_ids)
    } else {
      integer()
    }
    # Count already-complete shards toward the success tally so summary.json
    # reflects the full pool, not just the sims re-run this session.
    if (isTRUE(resume)) {
      state$total_sims_successful <- length(done_ids)
      state$n_sims_reused <- length(done_ids)
      if (length(done_ids) == 0L) {
        log_msg("[RESUME] No existing shards in target range \u2014 running all %d from scratch", target)
      } else {
        log_msg("[RESUME] Reusing %d of %d existing simulation(s); running the remaining %d",
                length(done_ids), target, target - length(done_ids))
      }
    }
    sim_ids <- setdiff(all_ids, done_ids)

    if (length(sim_ids) == 0L) {
      log_msg("Nothing to do: all %d simulations already complete", length(done_ids))
      # Reflect the complete on-disk pool in the run counters / summary.
      state$total_sims_run <- length(all_ids)
    } else {
      log_msg("Running %d simulations (%d-%d)", length(sim_ids), min(sim_ids), max(sim_ids))

      batch_start_time <- Sys.time()

      # Dispatch batch: R parallel or sequential.
      #
      # shard_batch_size > 1 changes the UNIT OF WORK from one simulation to a
      # contiguous run of them, so each task writes one shard holding many rows
      # instead of many shards holding one (pipeline plan item 6b). Everything
      # downstream is unaffected: the resume scan reads ids from the `sim`
      # column and the combine globs sim_*.parquet either way.
      shard_batch <- .mosaic_resolve_shard_batch(
        control$io$shard_batch_size,
        n_sims = length(sim_ids),
        n_workers = if (is.null(cl)) 1L else length(cl))

      success_indicators <- if (!is.null(cl) && shard_batch > 1L) {
        .mosaic_run_batch(
          sim_ids = .mosaic_chunk_ids(sim_ids, shard_batch),
          worker_func = function(sim_ids) .run_sim_worker_chunk(sim_ids),
          cl = cl,
          show_progress = control$parallel$progress,
          unit_runs = shard_batch * n_iterations
        )
      } else if (!is.null(cl)) {
        # Parallel: use worker function defined on cluster
        .mosaic_run_batch(
          sim_ids = sim_ids,
          worker_func = function(sim_id) .run_sim_worker(sim_id),
          cl = cl,
          show_progress = control$parallel$progress,
          unit_runs = n_iterations
        )
      } else {
        # Sequential. shard_batch_size is honoured here too -- a control setting
        # that silently did nothing on one of the two dispatch paths is the
        # failure mode of CLAUDE.md lesson #13 -- but only ABOVE 1, so the
        # default keeps running through the original per-simulation worker
        # rather than through new code.
        if (shard_batch > 1L) .mosaic_run_batch(
          sim_ids = .mosaic_chunk_ids(sim_ids, shard_batch),
          worker_func = function(ids) .mosaic_run_simulation_chunk(
            sim_ids            = ids,
            n_iterations       = n_iterations,
            priors             = priors,
            config             = config,
            PATHS              = PATHS,
            dir_cal_samples    = dirs$cal_samples,
            dir_cal_simresults = dirs$cal_simresults,
            param_names_all    = param_names_all,
            param_lookup       = param_lookup,
            sampling_args      = sampling_args,
            io                 = control$io,
            likelihood_settings = control$likelihood
                                 ),
          cl = cl,
          show_progress = control$parallel$progress
        ) else .mosaic_run_batch(
          sim_ids = sim_ids,
          worker_func = function(sim_id) .mosaic_run_simulation_worker(
            sim_id             = sim_id,
            n_iterations       = n_iterations,
            priors             = priors,
            config             = config,
            PATHS              = PATHS,
            dir_cal_samples    = dirs$cal_samples,
            dir_cal_simresults = dirs$cal_simresults,
            param_names_all    = param_names_all,
            param_lookup       = param_lookup,
            sampling_args      = sampling_args,
            io                 = control$io,
            likelihood_settings = control$likelihood
                                 ),
          cl = cl,
          show_progress = control$parallel$progress
        )
      }

      n_success_batch <- sum(unlist(success_indicators))
      state$total_sims_successful <- state$total_sims_successful + n_success_batch
      state$batch_success_rates <- c(state$batch_success_rates, (n_success_batch / length(sim_ids)) * 100)
      state$batch_sizes_used <- c(state$batch_sizes_used, length(sim_ids))
      state$batch_number <- state$batch_number + 1L
      state$total_sims_run <- length(all_ids)

      batch_runtime <- difftime(Sys.time(), batch_start_time, units = "mins")
      log_msg("Fixed batch complete: %d/%d successful (%.1f%%) in %.1f minutes",
              n_success_batch, length(sim_ids), tail(state$batch_success_rates, 1),
              as.numeric(batch_runtime))

      .mosaic_save_state(state, state_file)
      .mosaic_save_checkpoint(state, checkpoint_file)
    }

  # ===========================================================================
  # SIMULATION: AUTO MODE (ADAPTIVE BATCHING)
  # ===========================================================================

  } else {

    tg <- control$targets
    log_msg(paste0("Adaptive mode: stops when the sampling checks of the convergence status are GREEN at ",
                   "two consecutive checks (ensemble ESS >= %s, i.e. ESS_all >= %s %s; ",
                   "Monte Carlo error and k-hat bands in targets$status_thresholds; informativeness is ",
                   "reported, never blocks the stop) after at least %d batches and %s valid draws, or at ",
                   "max_simulations_total = %s simulations"),
            .mosaic_fmt_count(tg$ESS_best), .mosaic_fmt_count(.mosaic_scheme_ess_all_target(control)),
            if (identical(tg$posterior_weighting, "coarsened")) "for the resampled ensemble"
            else sprintf("at best_subset_ratio %g", tg$best_subset_ratio),
            as.integer(control$calibration$min_batches_adaptive),
            .mosaic_fmt_count(control$calibration$min_draws),
            .mosaic_fmt_count(control$calibration$max_simulations_total))
    log_msg(paste0("  Batches of %s until then; afterwards sized from the projection N x ESS_all target / ",
                   "ESS_all (at most %s) while the ESS growth slope >= %g, else growing by a quarter of ",
                   "the draws so far%s"),
            .mosaic_fmt_count(control$calibration$batch_size_adaptive),
            .mosaic_fmt_count(control$calibration$max_batch_predictive),
            tg$status_thresholds$slope_stall,
            if (!is.null(cl) && length(cl) > 1L)
              sprintf("; each batch rounded up to fill %d workers", length(cl)) else "")

    repeat {
      if (state$converged || state$total_sims_run >= control$calibration$max_simulations_total) break

      decision <- .mosaic_next_batch_size(state, control,
                                          n_workers = if (is.null(cl)) 1L else length(cl))
      current_batch_size <- decision$batch_size
      if (current_batch_size <= 0) {
        log_msg("No simulation budget left under max_simulations_total")
        break
      }

      batch_start <- state$total_sims_run + 1L
      batch_end <- min(state$total_sims_run + current_batch_size,
                       control$calibration$max_simulations_total)
      sim_ids <- batch_start:batch_end

      log_msg(paste(rep("-", 60), collapse = ""))
      log_msg("Batch %d: running %d simulations (%d-%d) -- %s",
              state$batch_number + 1L, length(sim_ids), min(sim_ids), max(sim_ids),
              decision$reason)

      # Run batch
      batch_start_time <- Sys.time()

      # Dispatch batch: R parallel or sequential. See the calibration-phase
      # dispatch above for why shard_batch_size changes the unit of work.
      # A batch rounded up to fill the cluster carries the shard it was
      # rounded to (.mosaic_round_batch_to_cluster()).
      shard_batch <- decision$shard_batch %||% .mosaic_resolve_shard_batch(
        control$io$shard_batch_size,
        n_sims = length(sim_ids),
        n_workers = if (is.null(cl)) 1L else length(cl))

      success_indicators <- if (!is.null(cl) && shard_batch > 1L) {
        .mosaic_run_batch(
          sim_ids = .mosaic_chunk_ids(sim_ids, shard_batch),
          worker_func = function(sim_ids) .run_sim_worker_chunk(sim_ids),
          cl = cl,
          show_progress = control$parallel$progress,
          unit_runs = shard_batch * n_iterations
        )
      } else if (!is.null(cl)) {
        # Parallel: use worker function defined on cluster
        .mosaic_run_batch(
          sim_ids = sim_ids,
          worker_func = function(sim_id) .run_sim_worker(sim_id),
          cl = cl,
          show_progress = control$parallel$progress,
          unit_runs = n_iterations
        )
      } else {
        # Sequential. shard_batch_size is honoured here too -- a control setting
        # that silently did nothing on one of the two dispatch paths is the
        # failure mode of CLAUDE.md lesson #13 -- but only ABOVE 1, so the
        # default keeps running through the original per-simulation worker
        # rather than through new code.
        if (shard_batch > 1L) .mosaic_run_batch(
          sim_ids = .mosaic_chunk_ids(sim_ids, shard_batch),
          worker_func = function(ids) .mosaic_run_simulation_chunk(
            sim_ids            = ids,
            n_iterations       = n_iterations,
            priors             = priors,
            config             = config,
            PATHS              = PATHS,
            dir_cal_samples    = dirs$cal_samples,
            dir_cal_simresults = dirs$cal_simresults,
            param_names_all    = param_names_all,
            param_lookup       = param_lookup,
            sampling_args      = sampling_args,
            io                 = control$io,
            likelihood_settings = control$likelihood
                                 ),
          cl = cl,
          show_progress = control$parallel$progress
        ) else .mosaic_run_batch(
          sim_ids = sim_ids,
          worker_func = function(sim_id) .mosaic_run_simulation_worker(
            sim_id             = sim_id,
            n_iterations       = n_iterations,
            priors             = priors,
            config             = config,
            PATHS              = PATHS,
            dir_cal_samples    = dirs$cal_samples,
            dir_cal_simresults = dirs$cal_simresults,
            param_names_all    = param_names_all,
            param_lookup       = param_lookup,
            sampling_args      = sampling_args,
            io                 = control$io,
            likelihood_settings = control$likelihood
                                 ),
          cl = cl,
          show_progress = control$parallel$progress
        )
      }

      n_success_batch <- sum(unlist(success_indicators))
      batch_success_rate <- (n_success_batch / length(sim_ids)) * 100

      state$total_sims_successful <- state$total_sims_successful + n_success_batch
      state$batch_success_rates <- c(state$batch_success_rates, batch_success_rate)
      state$batch_sizes_used <- c(state$batch_sizes_used, length(sim_ids))
      state$batch_number <- state$batch_number + 1L
      state$total_sims_run <- batch_end

      batch_runtime <- difftime(Sys.time(), batch_start_time, units = "mins")
      log_msg("Batch %d complete: %d/%d successful (%.1f%%) in %.1f minutes",
              state$batch_number, n_success_batch, length(sim_ids),
              batch_success_rate, as.numeric(batch_runtime))

      # Convergence check on every draw so far -- always run, including after
      # the batch that reaches the cap, so the final status describes the
      # whole pool.
      gate_pool <- .mosaic_gate_pool_update(gate_pool, dirs$cal_samples, param_names_sampled)
      state <- .mosaic_convergence_check_update_state(state, dirs, param_names_sampled, control,
                                                      n_obs_scored, results = gate_pool$data)
      .mosaic_save_state(state, state_file)
      .mosaic_save_checkpoint(state, checkpoint_file)
    }
    if (!isTRUE(state$converged)) {
      last <- utils::tail(state$convergence_history, 1L)
      log_warn("Budget cap reached: max_simulations_total = %s draws, convergence status %s (stopping rule not met)",
               .mosaic_fmt_count(control$calibration$max_simulations_total),
               if (length(last)) toupper(last[[1]]$status) else "not assessed")
    }
  }

  rm(gate_pool)

  # Stop R cluster only if we created it (not if caller provided it)
  if (exists("owns_cluster") && owns_cluster && !is.null(cl)) {
    .mosaic_stop_cluster(cl)
    cl <- NULL
  }

  # Parallelism for the POST-calibration ensembles (posterior + medoid).
  #
  # These cannot reuse `cl` -- it has just been stopped, and a caller-provided
  # cluster is the caller's to manage -- so calc_model_ensemble() builds its
  # own. But the decision of WHETHER to go parallel must not be read from
  # `control$parallel$enable` alone: a caller who supplies `cluster = cl` has
  # demonstrably got workers and may well have left `enable` at its default,
  # in which case the calibration ran on 100+ cores and the ensembles then ran
  # one simulation at a time. Treat a supplied cluster as consent to
  # parallelise, and size the ensemble cluster to it.
  ens_plan     <- .mosaic_ensemble_parallel_plan(cluster, control)
  ens_parallel <- ens_plan$parallel
  ens_n_cores  <- ens_plan$n_cores
  if (!is.null(ens_plan$note)) log_warn("%s", ens_plan$note)

  # ===========================================================================
  # COMBINE RESULTS AND ADD FLAGS
  log_msg("Combining simulation files")

  # Get list of simulation files
  parquet_files <- list.files(dirs$cal_samples, pattern = "^sim_.*\\.parquet$", full.names = TRUE)

  # Load and combine all simulation files
  # Default: streaming (memory-safe for large runs)
  # Override via control$io$load_method if needed
  load_method <- if (!is.null(control$io$load_method)) {
    control$io$load_method
  } else {
    "streaming"  # Safe default
  }

  load_chunk_size <- control$io$load_chunk_size %||% 5000L

  results <- .mosaic_load_and_combine_results(
    dir_params = dirs$cal_samples,
    method = load_method,
    chunk_size = load_chunk_size,
    verbose = TRUE
  )

  # Add index columns
  log_msg("Adding index columns to results matrix...")
  results$is_finite <- is.finite(results$likelihood) & !is.na(results$likelihood)
  results$is_valid <- results$is_finite
  results$is_outlier <- FALSE

  weighting_scheme <- control$targets$posterior_weighting
  # The Tukey lower fence and the Akaike weights of the retained draws
  # (weight_retained) belong to the legacy schemes: the "saturated" /
  # "tempered" tier search works on the retained draws, and "coarsened" keeps
  # them so a 1.3.0 run reproduces. Under "bestfit" every valid draw enters
  # the posterior, so is_retained = is_valid.
  legacy_scheme <- !identical(weighting_scheme, "bestfit")
  if (legacy_scheme && sum(results$is_valid) > 0) {
    valid_ll <- results$likelihood[results$is_valid]
    iqr_mult <- control$weights$iqr_multiplier
    lower_threshold <- .mosaic_lower_fence(valid_ll, iqr_mult)
    results$is_outlier[results$is_valid] <- valid_ll < lower_threshold

    log_msg("  Outlier detection (Tukey lower fence only, multiplier = %.1f; legacy scheme '%s'):",
            iqr_mult, weighting_scheme)
    log_msg("    - Lower threshold: %.1f | Outliers: %d (%.1f%%)",
            lower_threshold,
            sum(results$is_outlier),
            100 * sum(results$is_outlier) / sum(results$is_valid))
  }

  results$is_retained <- results$is_valid & !results$is_outlier
  results$is_best_subset <- FALSE
  results$is_best_model <- FALSE

  if (any(results$is_valid)) {
    results$is_best_model[which.max(results$likelihood)] <- TRUE
  }

  # Write combined simulations and clean up shards
  simulations_file <- file.path(dirs$calibration, "samples.parquet")
  .mosaic_write_parquet(results, simulations_file, control$io)

  # Delete individual simulation files after combining
  if (length(parquet_files) > 0) {
    unlink(parquet_files)
    log_msg("  Cleaned up %d individual simulation files", length(parquet_files))
  }

  # The resume checkpoint is now stale (shards consolidated into samples.parquet).
  # Remove it so the directory carries no dangling resume state.
  unlink(checkpoint_file, force = TRUE)

  log_msg("  Combined simulations saved: %s", basename(simulations_file))

  # ===========================================================================
  # POSTERIOR WEIGHTS, ENSEMBLE MEMBERS AND THE CONVERGENCE STATUS
  # ===========================================================================
  # weight_all is the coarsened (generalised-Bayes) posterior over every valid
  # draw, w propto exp(eta * (ll - max ll)), eta = coarsening_alpha / n_obs,
  # under every scheme. The ensemble members (is_best_subset / weight_best)
  # depend on the scheme:
  #   "bestfit" (default): the best-fit selection, .mosaic_bestfit_select(),
  #     carrying their coarsened weights renormalised;
  #   "coarsened" (1.3.0): a systematic resample of K draws, count / K weights;
  #   "saturated" / "tempered" (pre-1.3.0): the tier-searched best subset.
  # The parameter posterior, the ensemble, the medoid and the trajectories are
  # all built from these members (or the optimised subset, opt-in), and the
  # convergence status describes them (calc_convergence_status(), through
  # calc_weight_health()).
  run_fixed <- identical(state$mode, "fixed")
  eta_run <- .mosaic_coarsened_eta(control$targets$coarsening_alpha, n_obs_scored)
  # ESS_all replayed over the draws in simulation order (<= 40 log-spaced
  # points): the growth slope of a fixed-budget run, and the ess_path /
  # loglik_path of calibration_diagnostics.json for every run.
  ess_replay <- .mosaic_ess_replay(results$likelihood, results$sim, eta_run)
  hist_last <- utils::tail(state$convergence_history %||% list(), 1L)
  final_slope <- if (!run_fixed && length(hist_last) && is.finite(hist_last[[1]]$slope %||% NA)) {
    as.numeric(hist_last[[1]]$slope)
  } else {
    .mosaic_gate_slope(ess_replay$n, ess_replay$ess_all)
  }
  budget_exhausted <- !run_fixed && !isTRUE(state$converged) &&
    state$total_sims_run >= control$calibration$max_simulations_total

  weight_health <- NULL
  if (sum(results$is_valid) >= 2L) {
    weight_health <- tryCatch(
      .mosaic_gate_health(results, param_names_sampled, n_obs_scored, control,
                          slope = final_slope, budget_exhausted = budget_exhausted,
                          fixed_mode = run_fixed, n_attempted = as.numeric(state$total_sims_run),
                          n_locations = length(config$location_name)),
      error = function(e) {
        log_warn("Convergence status could not be computed: %s", conditionMessage(e))
        NULL
      })
  }
  # A run too degenerate for the status (fewer than two valid draws, or a
  # failed computation) still gets one: RED, with the reason, so summary.json,
  # both diagnostics JSONs and the report describe it (red team round 1, C2).
  if (is.null(weight_health)) {
    weight_health <- .mosaic_degenerate_health(
      n_valid = sum(results$is_valid), n_total = nrow(results), n_obs = n_obs_scored,
      control = control, fixed_mode = run_fixed, budget_exhausted = budget_exhausted,
      n_attempted = as.numeric(state$total_sims_run))
    log_warn("Convergence status: RED (%s)", weight_health$convergence$reasons[1])
  }

  coarsened_post <- NULL
  member_idx <- integer(0)
  ESS_B_final <- A_final <- CVw_final <- NA_real_
  gibbs_temperature_final <- NA_real_
  percentile_used <- 0
  convergence_tier <- weighting_scheme
  is_diag_best <- .mosaic_empty_is_diag()
  is_diag_all  <- .mosaic_empty_is_diag()
  results$weight_all <- 0
  if (legacy_scheme) results$weight_retained <- 0
  results$weight_best <- 0
  if (is.finite(eta_run)) results$weight_all <- .mosaic_tempered_weights(results$likelihood, eta_run)

  if (!any(results$is_valid)) {
    log_warn("No simulation has a finite likelihood: the posterior is undefined and no ensemble is built")

  } else if (identical(weighting_scheme, "bestfit")) {
    sel <- if (!isTRUE(weight_health$degenerate)) weight_health$bestfit else
      .mosaic_bestfit_select(results$likelihood, eta_run, control$targets$best_subset_ratio,
                             control$targets$min_ensemble_members,
                             control$targets$max_ensemble_members, sim = results$sim)
    member_idx <- sel$idx
    results$weight_best[member_idx] <- sel$weights
    is_diag_all <- calc_is_diagnostics(results$likelihood, method = control$targets$ESS_method)
    log_msg("Posterior (alpha = %g, n_obs = %s, eta = %.3g): ESS_all %s over %s valid draws",
            control$targets$coarsening_alpha, format(n_obs_scored, big.mark = ","), eta_run,
            .mosaic_fmt_count(calc_model_ess(results$weight_all, method = "kish")),
            format(sum(results$is_valid), big.mark = ","))
    if (isTRUE(weight_health$degenerate)) {
      log_msg("Ensemble: %d member%s (too few valid draws for a posterior; the convergence status is RED)",
              length(member_idx), if (length(member_idx) == 1L) "" else "s")
    } else {
      log_msg(paste0("Best-fit ensemble: %d members, the best draws by log-likelihood until their ESS ",
                     "reaches best_subset_ratio %g x ESS_all (ensemble ESS %.1f; %.1f%% of the posterior ",
                     "weight; size set by %s)"),
              length(member_idx), control$targets$best_subset_ratio,
              calc_model_ess(sel$weights, method = "kish"), 100 * sum(results$weight_all[member_idx]),
              if (identical(sel$binding, "ratio")) "the ratio" else sel$binding)
    }

  } else if (identical(weighting_scheme, "coarsened")) {
    coarsened_post <- .mosaic_coarsened_posterior(
      log_lik = results$likelihood,
      n_obs   = n_obs_scored,
      alpha   = control$targets$coarsening_alpha,
      min_K   = control$targets$min_ensemble_members,
      max_K   = control$targets$max_ensemble_members,
      seed    = .mosaic_resample_seed(config)
    )
    member_idx <- which(coarsened_post$counts > 0L)
    results$weight_all  <- coarsened_post$weights
    results$weight_best <- coarsened_post$member_weights
    percentile_used <- (length(member_idx) / nrow(results)) * 100

    # ESS_B / A / CVw of the member weights: reported, never gated.
    w_members <- coarsened_post$member_weights[member_idx]
    ESS_B_final <- calc_model_ess(w_members, method = control$targets$ESS_method)
    A_final     <- calc_model_agreement_index(w_members * length(w_members))$A
    CVw_final   <- calc_model_cvw(w_members * length(w_members))
    gibbs_temperature_final <- coarsened_post$eta
    is_diag_best <- calc_is_diagnostics(results$likelihood[member_idx],
                                        method = control$targets$ESS_method)
    is_diag_all  <- calc_is_diagnostics(results$likelihood, method = control$targets$ESS_method)

    log_msg("Coarsened posterior (alpha = %g, n_obs = %s, eta = %.3g): ESS_all %s over %s valid draws",
            control$targets$coarsening_alpha, format(n_obs_scored, big.mark = ","),
            coarsened_post$eta, .mosaic_fmt_count(coarsened_post$ess_kish),
            format(sum(results$is_valid), big.mark = ","))
    log_msg(paste0("  Ensemble members (posterior_weighting = \"coarsened\"): systematic resample of ",
                   "K = %d draws (seed %d; K = clamp(round(ESS_all), %d, %d)) -> %d unique draws, ",
                   "weighted by count / K"),
            coarsened_post$K, coarsened_post$seed, as.integer(control$targets$min_ensemble_members),
            as.integer(control$targets$max_ensemble_members), coarsened_post$n_unique)

  } else {
    tier_sel <- .mosaic_tier_best_subset(results, control, log = TRUE)
    member_idx <- which(results$sim %in% tier_sel$top_subset$sim)
    n_top_final <- tier_sel$n
    percentile_used <- tier_sel$percentile
    convergence_tier <- tier_sel$tier
    rm(tier_sel)

    if (length(member_idx) >= 2L) {
      # The legacy subset weights, the same helper results$weight_best uses.
      bsw_final <- .mosaic_best_subset_weights(results$likelihood[member_idx], weighting_scheme,
                                               verbose = control$logging$verbose)
      results$weight_best[member_idx] <- bsw_final$weights
      gibbs_temperature_final <- bsw_final$temperature
      ESS_B_final <- calc_model_ess(bsw_final$weights, method = control$targets$ESS_method)
      A_final     <- calc_model_agreement_index(bsw_final$weights * length(bsw_final$weights))$A
      CVw_final   <- calc_model_cvw(bsw_final$weights * length(bsw_final$weights))
      if (is.finite(ESS_B_final) && ESS_B_final < 0.05 * length(member_idx)) {
        log_warn(paste0("Best-subset weighting '%s' produced a near-degenerate posterior: ESS_B = %.2f ",
                        "over %d members (%.1f%% of mass on the single heaviest draw). Credible ",
                        "intervals from this subset are not trustworthy."),
                 weighting_scheme, ESS_B_final, length(member_idx), 100 * max(bsw_final$weights))
      }
      is_diag_best <- calc_is_diagnostics(results$likelihood[member_idx],
                                          method = control$targets$ESS_method)
      is_diag_all  <- calc_is_diagnostics(results$likelihood, method = control$targets$ESS_method)
      log_msg("Legacy best subset (%s, tier %s): %.1f%% (n=%d) | ESS_B=%s, A=%s, CVw=%s, T=%.4f",
              weighting_scheme, convergence_tier, percentile_used, n_top_final,
              if (is.finite(ESS_B_final)) sprintf("%.1f", ESS_B_final) else "NA",
              if (is.finite(A_final)) sprintf("%.3f", A_final) else "NA",
              if (is.finite(CVw_final)) sprintf("%.3f", CVw_final) else "NA", gibbs_temperature_final)
    } else {
      log_warn("Insufficient valid simulations (%d) in the legacy best subset", length(member_idx))
    }
  }
  results$is_best_subset[member_idx] <- TRUE

  # Akaike weights of the retained draws: legacy schemes only.
  if (legacy_scheme && sum(results$is_retained) > 0) {
    aic_retained <- -2 * results$likelihood[results$is_retained]
    delta_aic_retained <- aic_retained - min(aic_retained[is.finite(aic_retained)])
    results$weight_retained[results$is_retained] <- calc_model_weights_gibbs(
      x = pmin(delta_aic_retained, 25.0), eta = 0.5, verbose = control$logging$verbose)
  }

  # The convergence status of the members actually built. Under "bestfit" the
  # members ARE weight_health's best-fit selection; under the opt-in schemes
  # (and after the optional subset optimiser, below) the ensemble ESS, Monte
  # Carlo error and member counts are recomputed on the members used, through
  # calc_convergence_status() as always.
  # A degenerate run still reports the members it has (C2 note / RN5), so the
  # log, summary.json and the report agree on the count.
  if (isTRUE(weight_health$degenerate) && length(member_idx)) {
    weight_health$bestfit$n_members <- length(member_idx)
    weight_health$bestfit$n_unique <- length(member_idx)
  }
  conv_health <- weight_health
  if (!isTRUE(weight_health$degenerate) && length(member_idx) && !identical(weighting_scheme, "bestfit")) {
    conv_health <- tryCatch(
      .mosaic_members_health(
        weight_health, results, param_names_sampled, member_idx, results$weight_best[member_idx],
        n_members = if (!is.null(coarsened_post)) coarsened_post$K else length(member_idx),
        binding = if (!is.null(coarsened_post)) {
          .mosaic_resample_binding(coarsened_post$ess_kish, control$targets$min_ensemble_members,
                                   control$targets$max_ensemble_members)
        } else if (identical(convergence_tier, "fallback")) "max_members" else "ratio"),
      error = function(e) {
        log_warn("Convergence status of the members could not be computed: %s", conditionMessage(e))
        weight_health
      })
  }
  if (!is.null(conv_health)) .mosaic_log_status(conv_health, run_fixed)

  # subset_selection_summary.csv: the legacy schemes' tier / resample record.
  if (legacy_scheme) {
    subset_summary <- data.frame(
      min_search_size = control$targets$min_best_subset,
      max_search_size = control$targets$max_best_subset,
      optimal_size = length(member_idx),
      optimal_percentile = percentile_used,
      optimization_tier = convergence_tier,
      optimization_method = if (identical(weighting_scheme, "coarsened")) "systematic_resample" else "grid_search",
      n_selected = length(member_idx),
      ESS_B = ESS_B_final,
      A = A_final,
      CVw = CVw_final,
      gibbs_temperature = gibbs_temperature_final,
      meets_all_criteria = if (identical(weighting_scheme, "coarsened"))
                             identical(conv_health$status, "green")
                           else !identical(convergence_tier, "fallback"),
      timestamp = Sys.time()
    )
    summary_file <- file.path(dirs$cal_diag, "subset_selection_summary.csv")
    write.csv(subset_summary, summary_file, row.names = FALSE)
    log_msg("Saved %s", summary_file)
  }

  .mosaic_write_parquet(results, simulations_file, control$io)
  log_msg("Saved %s", simulations_file)
  gc(verbose = FALSE)

  # ===========================================================================
  # BUILD POSTERIOR ENSEMBLE (must run BEFORE posterior quantile construction
  # so that subset optimization, when enabled, can drive the canonical posterior)
  # ===========================================================================

  # Post-calibration stochastic ensemble sims run in-process on the local
  # cluster.
  #
  # Trajectory-capture flags are defined here, ahead of every consumer. They
  # used to live ~80 lines further down, which left `capture_trajectories =
  # .traj_enabled` reading an undefined object.
  .traj_enabled     <- isTRUE(control$predictions$capture_trajectories)
  .optimize_enabled <- isTRUE(control$predictions$optimize_subset)
  .traj_channels    <- control$predictions$trajectory_channels %||%
                         .MOSAIC_TRAJECTORY_CHANNELS_DEFAULT
  .traj_n_lines     <- as.integer(control$predictions$trajectory_n_lines %||% 150L)

  # Format a number-or-NA as a log-line-safe string. Replaces the prior
  # `ifelse(is.na(x), 0, x)` pattern which silently substituted 0 for NA in
  # the prose log lines (best / medoid / ensemble R^2 + bias), making
  # "computation failed" indistinguishable from "model fit is awful but
  # ran". The [RUN_SUMMARY] sentinel already emits literal NA; the prose
  # lines now do too.
  .fmt_num <- function(x, digits = 4L) {
    if (!is.finite(x)) "NA" else sprintf(paste0("%.", as.integer(digits), "f"), x)
  }

  # Ensemble central tendency. Resolved once here (per-channel) and applied
  # consistently to the medoid target, tier/best/medoid/ensemble metrics, the
  # subset optimizer, and the prediction plots. Defined before every consumer
  # (incl. the tier block) so a single choice governs the whole post-calibration
  # path. `.central()` is a closure over the resolved method; it takes the
  # ensemble as an argument so it is valid before any ensemble is built.
  central_method <- .mosaic_resolve_central_method(control$predictions$central_method)
  .central <- function(ens, chan) .mosaic_central_series(ens, chan, central_method)
  log_msg("Ensemble central tendency: cases=%s, deaths=%s",
          central_method[["cases"]], central_method[["deaths"]])

  # Observation model for the posterior predictive (v0.101.0): the per-location
  # weekly cases dispersion this run's likelihood scored with
  # (control$likelihood$.nb_k_cases_resolved) on the same weekly blocks. The
  # ensemble's intervals and its persisted cases_array/deaths_array are then
  # draws of the OBSERVED counts (deaths take the integrated deaths likelihood's
  # quasi-Poisson dispersion from the deaths integration); the central lines and
  # every consumer of member trajectories (medoid, trajectories, implied CFR,
  # subset selection) stay engine-level.
  obs_model_run <- .mosaic_resolve_observation_model(config, control)
  if (!is.null(obs_model_run))
    log_msg("Observation-level predictive: cases weekly NB k = %s (week offset %s)",
            paste(signif(obs_model_run$k_cases, 3), collapse = ", "),
            paste(obs_model_run$week_offset, collapse = ", "))

  # Whether to retain the dense 4-D arrays (observation-level cases_array/
  # deaths_array and engine-level cases_engine_array/deaths_engine_array) in the
  # *persisted* ensemble RDS files. Default FALSE => arrays are stripped at save time (via
  # .mosaic_ensemble_drop_arrays()) so the on-disk artifacts are small (~tens of
  # KB) and travel in git; every light field (central tendencies, envelopes,
  # weights, seeds, obs, metadata) is preserved and all current consumers work.
  # The in-memory object is NEVER stripped -- only the copy handed to saveRDS --
  # so medoid selection, subset optimization, trajectory reduction and
  # implied-CFR (which read the live object) are unaffected. Set TRUE for a
  # re-analysable raw archive (e.g. re-running subset optimization or the R_eff
  # posterior-resimulation CI path from the saved file).
  .persist_arrays <- isTRUE(control$io$persist_ensemble_arrays)

  # Every post-calibration artifact below is rebuilt from this run's samples.
  # Remove any left by an earlier run into the same dir_output first, so a
  # block that is skipped or fails this time cannot leave the previous run's
  # posterior on disk for render_MOSAIC_figures(), run_rolling_cv() or
  # MOSAIC-OCV to pick up.
  .mosaic_clear_posterior_artifacts(dirs, log_msg)

  # Initialize metric variables (populated after ensemble is built/resolved)
  r2_cases_ensemble          <- NA_real_
  r2_deaths_ensemble         <- NA_real_
  bias_ratio_cases_ensemble  <- NA_real_
  bias_ratio_deaths_ensemble <- NA_real_
  # Posterior reported CFR by location and year from the candidate ensemble. Held
  # separately because the subset optimizer replaces `ensemble` with an object
  # that carries no CFR draws; the trajectory panel and the medoid config use it.
  cfr_posterior_run <- NULL
  # Dual metrics: BOTH the mean- and median-derived ensemble R^2/bias are
  # emitted in summary.json during the transition so historical (median) runs
  # remain cross-walkable regardless of the canonical central_method.
  r2_cases_ensemble_mean     <- NA_real_
  r2_deaths_ensemble_mean    <- NA_real_
  r2_cases_ensemble_median   <- NA_real_
  r2_deaths_ensemble_median  <- NA_real_
  bias_ratio_cases_ensemble_mean    <- NA_real_
  bias_ratio_deaths_ensemble_mean   <- NA_real_
  bias_ratio_cases_ensemble_median  <- NA_real_
  bias_ratio_deaths_ensemble_median <- NA_real_
  n_ensemble_params          <- 0L
  n_ensemble_stochastic_per  <- as.integer(control$predictions$n_iter_ensemble %||% 10L)
  n_best_stochastic_per      <- as.integer(control$predictions$n_iter_best %||% 100L)

  # Tier-subset ensemble metrics -- populated only when optimize_subset = TRUE
  # (preserved for comparison against the canonical optimized-ensemble metrics).
  r2_cases_ensemble_tier          <- NA_real_
  r2_deaths_ensemble_tier         <- NA_real_
  bias_ratio_cases_ensemble_tier  <- NA_real_
  bias_ratio_deaths_ensemble_tier <- NA_real_
  n_ensemble_params_tier          <- NA_integer_

  ensemble       <- NULL
  ensemble_table <- NULL  # the ensemble prediction table (fit tables of calibration_diagnostics.json)
  fit_windows    <- control$predictions$fit_windows %||% c(365L, 120L, 90L, 60L, 30L)

  param_seeds <- NULL   # initialised here; assigned inside block below if best subset exists

  traj_scratch_handle <- NULL   # scratch descriptor from the candidate capture run
  cand_member_seeds   <- NULL   # candidate per-member seeds (scratch-key -> seed map)

  if (sum(results$is_best_subset) > 0) {
    best_subset_results <- results[results$is_best_subset == TRUE, ]
    nonzero <- best_subset_results$weight_best > 0
    param_seeds   <- best_subset_results$seed_sim[nonzero]
    param_weights <- best_subset_results$weight_best[nonzero]

    if (length(param_weights) > 0) {
      param_weights <- param_weights / sum(param_weights)
    } else {
      param_weights <- NULL
    }

    # Forecast-year CFR shift. Years after a location's latest observed year are
    # centred on the members' shared CFR deviation for that year: the weighted
    # mean over members of each path's posterior-mode deviation, from one run per
    # member (the first stochastic run of each, as in the ensemble below). A
    # path's own deviation also absorbs its case error that year, so carrying it
    # would carry the error; the mean keeps the shift the members share. It is
    # stored in the deaths integration, so every ensemble, the medoid and a
    # post-hoc re-run from deaths_integration.rds use the same value.
    if (.mosaic_has_forecast_years(control$likelihood$.deaths_integration) &&
        length(param_seeds) > 0) {
      log_msg("Forecast-year CFR: members' latest-year shift from %d param sets x 1 run...",
              length(param_seeds))
      pilot <- tryCatch(
        calc_model_ensemble(
          config                   = config,
          parameter_seeds          = param_seeds,
          parameter_weights        = param_weights,
          n_simulations_per_config = 1L,
          envelope_quantiles       = c(0.025, 0.975),
          PATHS                    = PATHS,
          priors                   = priors,
          sampling_args            = sampling_args,
          score_idx_cases          = control$likelihood$.score_window_resolved$idx_cases,
          score_idx_deaths         = control$likelihood$.score_window_resolved$idx_deaths,
          parallel                 = ens_parallel,
          n_cores                  = ens_n_cores,
          root_dir                 = root_dir,
          capture_trajectories     = FALSE,
          deaths_integration       = control$likelihood$.deaths_integration,
          verbose                  = FALSE
        ),
        error = function(e) {
          log_warn("forecast-year CFR shift not estimated (%s); forecast years revert to the prior level",
                   conditionMessage(e))
          NULL
        }
      )
      if (!is.null(pilot) && !is.null(pilot$forecast_shift)) {
        control$likelihood$.deaths_integration <- .mosaic_set_forecast_shift(
          control$likelihood$.deaths_integration, pilot$forecast_shift)
        ok <- is.finite(pilot$forecast_shift)
        log_msg("Forecast-year CFR shift (logit, latest observed year; %d location(s)): %s",
                sum(ok), paste(sprintf("%s %+.2f", pilot$location_names[ok], pilot$forecast_shift[ok]),
                               collapse = ", "))
        tryCatch({
          saveRDS(control$likelihood$.deaths_integration,
                  file.path(dirs$calibration, "deaths_integration.rds"))
          log_msg("Saved 2_calibration/deaths_integration.rds (with the forecast-year shift)")
        }, error = function(e)
          log_warn("deaths_integration.rds write skipped: %s", conditionMessage(e)))
      }
      rm(pilot)
    }

    log_msg("Computing weighted posterior ensemble (%d param sets x %d stochastic)...",
            length(param_seeds), n_ensemble_stochastic_per)

    # Trajectory capture is STREAM-TO-DISK at sim time (capture-don't-replay,
    # PLAN 14.G): the candidate ensemble run ALWAYS captures (whether or not
    # optimization is on), spilling per-sim channels to a scratch dir. The
    # reduction is DEFERRED (reduce_trajectories = FALSE) so it can run over the
    # FINAL displayed subset (optimized weights) AFTER optimize_ensemble_subset(),
    # with NO re-simulation (deviation-#1 fix, PLAN 14.B). run_MOSAIC owns the
    # scratch lifecycle (reduced + unlinked in the unified block below).
    traj_scratch_dir <- file.path(dirs$cal_state, "traj_scratch")
    # Guarantee the (potentially multi-GB) scratch is removed even if an uncaught
    # error or interrupt fires anywhere in the candidate -> optimize -> reduce
    # window (RUN_FATAL on.exit only LOGS, it does not unlink). The explicit
    # unlink in the unified reduce block below then becomes idempotent.
    if (.traj_enabled)
      on.exit(unlink(traj_scratch_dir, recursive = TRUE, force = TRUE), add = TRUE)

    ensemble <- tryCatch(
      calc_model_ensemble(
        config                   = config,
        parameter_seeds          = param_seeds,
        parameter_weights        = param_weights,
        n_simulations_per_config = n_ensemble_stochastic_per,
        envelope_quantiles       = c(0.025, 0.25, 0.75, 0.975),
        PATHS                    = PATHS,
        priors                   = priors,
        sampling_args            = sampling_args,
        score_idx_cases          = control$likelihood$.score_window_resolved$idx_cases,
        score_idx_deaths         = control$likelihood$.score_window_resolved$idx_deaths,
        parallel                 = ens_parallel,
        n_cores                  = ens_n_cores,
        root_dir                 = root_dir,
        capture_trajectories     = .traj_enabled,
        trajectory_channels      = .traj_channels,
        trajectory_n_lines       = .traj_n_lines,
        trajectory_scratch_dir   = if (.traj_enabled) traj_scratch_dir else NULL,
        reduce_trajectories      = FALSE,
        deaths_integration       = control$likelihood$.deaths_integration,
        observation_model        = obs_model_run,
        verbose                  = control$logging$verbose
      ),
      error = function(e) {
        log_warn("calc_model_ensemble failed: %s", e$message)
        NULL
      }
    )

    if (!is.null(ensemble)) {
      ensemble_rds <- file.path(dirs$calibration, "ensemble_candidate.rds")
      saveRDS(.mosaic_stamp_artifact(
                if (.persist_arrays) ensemble else .mosaic_ensemble_drop_arrays(ensemble)),
              ensemble_rds)
      log_msg("Saved 2_calibration/ensemble_candidate.rds")
      # Posterior reported CFR by location and year (from the members' post-hoc
      # CFR draws; covers forecast years past the last observation too).
      if (!is.null(ensemble$cfr_posterior)) {
        tryCatch({
          if (!dir.exists(dirs$res_posterior)) dir.create(dirs$res_posterior, recursive = TRUE, showWarnings = FALSE)
          utils::write.csv(ensemble$cfr_posterior,
                           file.path(dirs$res_posterior, "cfr_posterior.csv"), row.names = FALSE)
          log_msg("Saved 3_results/posterior/cfr_posterior.csv")
        }, error = function(e)
          log_warn("cfr_posterior.csv write skipped: %s", conditionMessage(e)))
        cfr_posterior_run <- ensemble$cfr_posterior
      }

      # Persist the engine spatial-structure arrays for the "spatial" figure
      # group (figs 5-6). These are the element-wise median across the FULL
      # posterior-ensemble members (DM#5) -- the canonical member set -- carried
      # on the candidate ensemble before any prediction-quality subset
      # refinement. Unconditional (gated by io, not plots); schema-stamped;
      # config order with location_name attached. Absent arrays (engine without
      # DerivedValues) are silently skipped.
      .mosaic_persist_spatial_artifacts(ensemble, dirs, log_msg, log_warn)

      # Trajectory channels were spilled to scratch during this candidate run
      # (reduce deferred). Capture the scratch handle + the candidate per-member
      # seeds NOW, before `ensemble` may be replaced by the optimized rebuild
      # (which drops attached fields, PLAN 14.B). The unified reduce+persist block
      # after the optimize section reduces from scratch over the FINAL displayed
      # subset (optimized when available) and unlinks the scratch.
      traj_scratch_handle <- ensemble$trajectory_scratch
      cand_member_seeds   <- ensemble$seeds
    }
  } else {
    log_warn("no best subset \u2014 skipping posterior ensemble")
  }

  # ===========================================================================
  # OPTIMIZE SUBSET -- refine best subset using ensemble prediction quality
  # ===========================================================================
  # When control$predictions$optimize_subset = TRUE, rescore the ensemble against
  # observed cases/deaths and write the optimizer-selected subset to new _opt
  # columns on the results frame. Downstream posterior construction is then
  # driven by the optimized subset via .mosaic_active_subset_cols().

  subset_opt <- NULL   # initialised here; assigned inside block below if optimize_subset=TRUE
  opt_metrics <- NULL  # legacy ESS_B / A / CVw of the optimised subset (summary.json *_optimized)
  is_diag_opt <- NULL  # eta = 1 likelihood concentration of the optimised subset
  ensemble_opt_saved <- FALSE   # TRUE once the optimizer writes ensemble_optimized.rds
  ensemble_is_optimized <- FALSE  # TRUE once `ensemble` is the optimized rebuild

  if (!is.null(ensemble) && isTRUE(control$predictions$optimize_subset)) {

    log_msg("Running post-ensemble subset optimization...")

    # Recompute locally to make the opt block's dependencies explicit and
    # robust to future refactors (don't rely on variables from the sibling
    # ensemble-build block's loose R scoping).
    .opt_bs   <- results[results$is_best_subset == TRUE, ]
    .opt_nz   <- .opt_bs$weight_best > 0
    cand_seeds       <- .opt_bs$seed_sim[.opt_nz]
    cand_likelihoods <- .opt_bs$likelihood[.opt_nz]
    # Under "bestfit" and "coarsened" the per-N weights are the members' own
    # weights (coarsened weights / resample count weights) renormalised over
    # the top N (the optimiser's weighting = "coarsened").
    opt_weighting <- if (weighting_scheme %in% c("bestfit", "coarsened")) "coarsened" else weighting_scheme
    cand_base_w <- if (identical(opt_weighting, "coarsened")) .opt_bs$weight_best[.opt_nz] else NULL

    # Runs serially: the dominant speedup is the stride search (control$
    # predictions$optimize_stride). The step is export-bound, so spinning up a
    # cluster for it scales poorly and is counterproductive once stride shrinks
    # the work; the function still accepts a `cl =` for power users who want to
    # parallelize a one-off direct call.
    subset_opt <- tryCatch(
      optimize_ensemble_subset(
        ensemble    = ensemble,
        likelihoods = cand_likelihoods,
        seeds       = cand_seeds,
        min_n       = as.integer(control$predictions$optimize_min_n %||% 30L),
        objective   = control$predictions$optimize_objective %||% "mae",
        central_method = central_method,
        stride      = as.integer(control$predictions$optimize_stride %||% 3L),
        ess_method  = control$targets$ESS_method,
        weighting   = opt_weighting,
        base_weights = cand_base_w,
        verbose     = control$logging$verbose
      ),
      error = function(e) {
        log_warn("optimize_ensemble_subset failed: %s", e$message)
        NULL
      }
    )

    if (!is.null(subset_opt) && !is.null(subset_opt$optimal_seeds) &&
        length(subset_opt$optimal_seeds) > 0) {

      # Sanity-check assumptions that the weight-mapping below depends on.
      # seed_sim is expected to be unique per sim; if duplicates appear, the
      # setNames() mapping would silently drop all but the last, producing
      # incorrect weight_best_opt values.
      if (anyDuplicated(subset_opt$optimal_seeds) > 0L) {
        stop("optimize_ensemble_subset returned duplicate optimal_seeds; ",
             "refusing to write ambiguous weight_best_opt.", call. = FALSE)
      }
      if (anyDuplicated(results$seed_sim[results$is_best_subset]) > 0L) {
        stop("results$seed_sim is not unique within is_best_subset; ",
             "cannot map optimal_seeds -> weight_best_opt unambiguously.",
             call. = FALSE)
      }

      results$is_best_subset_opt <- results$seed_sim %in% subset_opt$optimal_seeds
      results$weight_best_opt    <- 0

      seed_to_weight <- setNames(subset_opt$optimal_weights,
                                 as.character(subset_opt$optimal_seeds))
      opt_idx <- which(results$is_best_subset_opt)
      if (length(opt_idx) > 0) {
        results$weight_best_opt[opt_idx] <-
          seed_to_weight[as.character(results$seed_sim[opt_idx])]
        if (any(is.na(results$weight_best_opt[opt_idx]))) {
          stop("NA weight_best_opt values after seed->weight mapping; ",
               "seed_to_weight lookup failed for at least one row.",
               call. = FALSE)
        }
      }

      .mosaic_write_parquet(results, simulations_file, control$io)
      log_msg("Subset optimization (%s): %d members -> optimized %d (score %.4f -> %.4f)%s",
              subset_opt$objective,
              subset_opt$diagnostics_n, subset_opt$optimal_n,
              subset_opt$diagnostics_score, subset_opt$optimal_score,
              if (subset_opt$stability_flag) " [flat profile]" else "")
      log_msg("  _opt columns written to samples.parquet (%d sims selected)",
              sum(results$is_best_subset_opt))

      # The optimized subset drives the posterior densities under every scheme
      # (.mosaic_active_subset_cols()).
      if (subset_opt$optimal_n < 30L) {
        log_warn("optimal_n = %d < 30; KDE-based posterior densities may be degenerate",
                subset_opt$optimal_n)
      }

      # Save the optimized ensemble. The tier ensemble is already on disk as
      # ensemble_candidate.rds from the earlier ensemble-build block. Arrays are
      # stripped from the on-disk copy unless persist_ensemble_arrays=TRUE; the
      # in-memory subset_opt (used below) keeps its arrays.
      saveRDS(.mosaic_stamp_artifact(
                if (.persist_arrays) subset_opt$ensemble_optimized
                else .mosaic_ensemble_drop_arrays(subset_opt$ensemble_optimized)),
              file.path(dirs$calibration, "ensemble_optimized.rds"))
      log_msg("Saved 2_calibration/ensemble_optimized.rds")
      ensemble_opt_saved <- TRUE

      # Persist the full subset-optimization result (Phase 1b / G4) so the
      # renderer can reconstruct plot_model_subset_optimization() from disk.
      # subset_opt$ensemble_optimized re-embeds the heavy arrays, so strip them
      # from a shallow COPY of subset_opt before this save (never mutate the
      # in-memory subset_opt used later) unless persist_ensemble_arrays=TRUE.
      tryCatch({
        subset_opt_to_save <- subset_opt
        if (!.persist_arrays) {
          subset_opt_to_save$ensemble_optimized <-
            .mosaic_ensemble_drop_arrays(subset_opt_to_save$ensemble_optimized)
        }
        saveRDS(.mosaic_stamp_artifact(subset_opt_to_save),
                file.path(dirs$calibration, "subset_opt.rds"))
        log_msg("Saved 2_calibration/subset_opt.rds")
      }, error = function(e) log_warn("subset_opt.rds save failed: %s", e$message))

      # Record TIER ensemble metrics for comparison in summary.json, then
      # replace `ensemble` with the optimized one so all downstream work uses it.
      # The canonical r2_cases_ensemble field will be recomputed later from the
      # (now optimized) `ensemble`; these *_tier fields hold the pre-opt metrics.
      # Tier metrics use the same central_method as the canonical ensemble so
      # the pre-/post-optimization comparison is on the same central tendency.
      cen_c_tier_flat <- as.numeric(.mosaic_mask_central_for_scoring(
        .central(ensemble, "cases"), "cases", ensemble$artifact_mask))
      cen_d_tier_flat <- as.numeric(.mosaic_mask_central_for_scoring(
        .central(ensemble, "deaths"), "deaths", ensemble$artifact_mask))
      obs_c_flat_tier <- as.numeric(ensemble$obs_cases)
      obs_d_flat_tier <- as.numeric(ensemble$obs_deaths)

      n_ensemble_params_tier <- ensemble$n_param_sets
      r2_cases_ensemble_tier  <- calc_model_R2(obs_c_flat_tier, cen_c_tier_flat)
      r2_deaths_ensemble_tier <- calc_model_R2(obs_d_flat_tier, cen_d_tier_flat)
      bias_ratio_cases_ensemble_tier  <- tryCatch(
        calc_bias_ratio(obs_c_flat_tier, cen_c_tier_flat), error = function(e) NA_real_)
      bias_ratio_deaths_ensemble_tier <- tryCatch(
        calc_bias_ratio(obs_d_flat_tier, cen_d_tier_flat), error = function(e) NA_real_)

      ensemble <- subset_opt$ensemble_optimized
      ensemble_is_optimized <- TRUE
      # Trajectory reduction over the OPTIMIZED subset is handled by the unified
      # stream-to-disk reduce+persist block AFTER this optimize section -- it reads
      # the candidate run's scratch (no re-simulation) over the optimized member
      # seeds + weights (deviation-#1 fix, PLAN 14.B).

      # Persist evaluation table for post-hoc inspection
      diag_path <- file.path(dirs$res_fig_diag, "optimization_diagnostics.csv")
      utils::write.csv(subset_opt$evaluation_table, diag_path, row.names = FALSE)
      log_msg("Saved 3_results/figures/diagnostics/optimization_diagnostics.csv")

      # (The optimization-diagnostic figure is rendered later by
      # render_MOSAIC_figures() from subset_opt.rds; the evaluation_table CSV is
      # already written above, independent of plots.)

      # The posterior is now the optimised subset, so the convergence status
      # describes it: ensemble ESS, Monte Carlo error and member counts on the
      # members used (calc_convergence_status(), as always). Its legacy ESS_B /
      # A / CVw and eta = 1 concentration are reported alongside.
      opt_idx_all <- which(results$is_best_subset_opt)
      w_opt <- results$weight_best_opt[opt_idx_all]
      if (length(opt_idx_all) >= 2L && sum(w_opt) > 0) {
        w_opt_norm <- w_opt / sum(w_opt)
        opt_metrics <- list(
          n   = length(opt_idx_all),
          ess = calc_model_ess(w_opt_norm, method = control$targets$ESS_method),
          A   = calc_model_agreement_index(w_opt_norm * length(w_opt_norm))$A,
          cvw = calc_model_cvw(w_opt_norm * length(w_opt_norm)))
        is_diag_opt <- calc_is_diagnostics(results$likelihood[opt_idx_all],
                                           method = control$targets$ESS_method)
        if (!isTRUE(weight_health$degenerate)) {
          conv_health <- tryCatch(
            .mosaic_members_health(weight_health, results, param_names_sampled, opt_idx_all,
                                   w_opt_norm, n_members = length(opt_idx_all),
                                   binding = conv_health$bestfit$binding %||% "ratio"),
            error = function(e) {
              log_warn("Convergence status of the optimised subset could not be computed: %s",
                       conditionMessage(e))
              conv_health
            })
          # The stopping rule (adaptive) was met on the posterior_weighting
          # members; the status now describes the optimised members, so say
          # why the two may differ (red team round 1, D11).
          if (!identical(sort(opt_idx_all), sort(as.integer(member_idx))))
            conv_health <- .mosaic_note_optimised_status(conv_health, weighting_scheme,
                                                         adaptive = !run_fixed,
                                                         stopping_rule_met = isTRUE(state$converged))
          log_msg("Convergence status on the optimised subset the posterior uses:")
          .mosaic_log_status(conv_health, run_fixed)
        }
      }
    }
  }

  # ---------------------------------------------------------------------------
  # ensemble_optimized.rds fallback: MOSAIC-OCV (run_scenarios.R) hardcodes the
  # filename 2_calibration/ensemble_optimized.rds. When subset optimization is
  # disabled (control$predictions$optimize_subset != TRUE) or produced an empty
  # subset, the block above never wrote that file. Fall back to the candidate
  # ensemble so ensemble_optimized.rds always exists whenever an ensemble was
  # built. Keyed on whether THIS run's optimizer wrote the file, never on
  # file.exists(): a file from an earlier run into the same directory must be
  # overwritten, not kept. Slimmed under the same persist_ensemble_arrays flag;
  # non-fatal.
  if (!is.null(ensemble)) {
    ensemble_opt_path <- file.path(dirs$calibration, "ensemble_optimized.rds")
    if (!ensemble_opt_saved) {
      tryCatch({
        saveRDS(.mosaic_stamp_artifact(
                  if (.persist_arrays) ensemble
                  else .mosaic_ensemble_drop_arrays(ensemble)),
                ensemble_opt_path)
        log_msg("Saved 2_calibration/ensemble_optimized.rds (fallback: candidate ensemble; subset optimization skipped or empty)")
      }, error = function(e)
        log_warn("ensemble_optimized.rds fallback save failed: %s", e$message))
    }
  }

  # ===========================================================================
  # PARAMETER ESS AND CONVERGENCE DIAGNOSTICS (the members the posterior uses)
  # ===========================================================================
  # Computed here, after the optional subset optimiser, so every number
  # describes the members the posterior and the predictions are built from.
  active_cols <- .mosaic_active_subset_cols(results, control)
  log_msg("Active subset source: %s (%s / %s)",
          active_cols$source, active_cols$subset_col, active_cols$weight_col)

  # Variance-ratio (delta-method) ESS of each parameter's posterior mean under
  # the members' weights: a diagnostic, never a gate. parameter_ess.csv keeps
  # ess_marginal as a deprecated duplicate of ess for one release.
  log_msg("Calculating parameter ESS (members' weights; diagnostic)")
  post_w <- results[[active_cols$weight_col]]
  post_w[!is.finite(post_w)] <- 0
  ess_results <- if (sum(post_w) > 0) tryCatch(
    calc_model_ess_parameter(results = results, param_names = param_names_sampled,
                             likelihood_col = "likelihood", weights = post_w,
                             verbose = control$logging$verbose),
    error = function(e) {
      log_warn("parameter ESS not computed: %s", conditionMessage(e))
      NULL
    }) else NULL
  rm(post_w)
  ess_file <- file.path(dirs$cal_diag, "parameter_ess.csv")
  if (!is.null(ess_results)) {
    ess_results <- data.frame(parameter = ess_results$parameter, type = ess_results$type,
                              iso_code = ess_results$iso_code, ess = ess_results$ess_marginal,
                              ess_marginal = ess_results$ess_marginal, stringsAsFactors = FALSE)
    utils::write.csv(ess_results, ess_file, row.names = FALSE)
    log_msg("Saved %s", ess_file)
  } else if (file.exists(ess_file)) {
    unlink(ess_file)
  }

  log_msg("Calculating convergence diagnostics")
  diagnostics <- tryCatch(calc_convergence_diagnostics(
    n_total = nrow(results),
    n_successful = sum(results$is_valid),
    n_retained = sum(results$is_retained),
    n_best_subset = sum(results$is_best_subset),
    ess_best = ESS_B_final,
    A_best = A_final,
    cvw_best = CVw_final,
    percentile_used = percentile_used,
    convergence_tier = convergence_tier,
    param_ess_results = ess_results,
    target_ess_best = control$targets$ESS_best,
    target_A_best = control$targets$A_best %||% 0.70,
    target_cvw_best = control$targets$CVw_best %||% 1.0,
    target_max_best_subset = control$targets$max_best_subset,
    is_diagnostics = list(best = is_diag_best, all = is_diag_all, optimized = is_diag_opt),
    weight_health = conv_health,
    weighting = weighting_scheme,
    ess_method = control$targets$ESS_method,
    temperature = if (is.finite(gibbs_temperature_final)) gibbs_temperature_final else 1,
    verbose = FALSE
  ), error = function(e) {
    log_warn("convergence diagnostics not computed: %s", conditionMessage(e))
    NULL
  })

  # convergence_results.parquet: the per-draw likelihood and weight table the
  # dashboard reads. is_member flags the ensemble members (it was "retained"
  # before 1.4.0, which also named the Tukey fence); the fence columns are
  # written under the legacy schemes only.
  best_aic_val <- .mosaic_safe_min(-2 * results$likelihood[is.finite(results$likelihood)])
  convergence_results_df <- data.frame(
    sim = results$sim,
    seed = results$seed_sim,
    likelihood = results$likelihood,
    aic = -2 * results$likelihood,
    delta_aic = if (is.finite(best_aic_val)) -2 * results$likelihood - best_aic_val
                else rep(NA_real_, nrow(results)),
    # w_tilde: the members' normalised weights (sum to 1 over the members)
    w_tilde = results$weight_best,
    # w: w_tilde * N_total (the Kish ESS is invariant to this constant)
    w = results$weight_best * nrow(results),
    is_member = results$is_best_subset,
    is_best_subset = results$is_best_subset
  )
  if (legacy_scheme) {
    convergence_results_df$w_retained <- results$weight_retained
    convergence_results_df$is_retained <- results$is_retained
  }
  convergence_file <- file.path(dirs$cal_diag, "convergence_results.parquet")
  .mosaic_write_parquet(convergence_results_df, convergence_file, control$io)
  log_msg("Saved %s", convergence_file)
  rm(convergence_results_df)

  diagnostics_file <- file.path(dirs$cal_diag, "convergence_diagnostics.json")
  if (!is.null(diagnostics)) {
    .mosaic_atomic_write(diagnostics, diagnostics_file, function(data, file)
      jsonlite::write_json(data, file, pretty = TRUE, auto_unbox = TRUE, digits = NA, na = "null"))
    log_msg("Saved %s", diagnostics_file)
  }

  # ===========================================================================
  # UNIFIED TRAJECTORY REDUCE + PERSIST (stream-to-disk, no re-simulation).
  # Reduce the channels spilled to scratch during the candidate run over the
  # FINAL displayed subset & weights, so the trajectory medians are consistent
  # with the prediction plots (deviation-#1 fix, PLAN 14.B):
  #   * optimize ON  -> the OPTIMIZED subset: map ensemble_optimized's per-member
  #     seeds back to the candidate scratch keys (param_idx) via the candidate
  #     member seeds; weights = optimal_weights. reported_* are taken from the
  #     optimized engine cases/deaths arrays -> their central line is
  #     BIT-IDENTICAL to the prediction plots'.
  #   * optimize OFF -> the candidate subset (all members, weight_best).
  # Then inject the per-location endemic/epidemic CFR reference levels and persist;
  # finally unlink the scratch dir. NULL handle (capture off / no best subset) is
  # a no-op.
  # ===========================================================================
  if (.traj_enabled && !is.null(traj_scratch_handle) && !is.null(ensemble)) {
    # Scratch keys (candidate param_idx) of the FINAL members, in `ensemble`
    # order. NULL when an optimized ensemble's seeds cannot be mapped back:
    # then the artifact is skipped rather than reduced over the wrong members.
    traj_map <- .mosaic_trajectory_member_map(
      is_optimized      = ensemble_is_optimized,
      final_seeds       = ensemble$seeds,
      cand_member_seeds = cand_member_seeds,
      n_param           = ensemble$n_param_sets)
    if (is.null(traj_map)) {
      log_warn(paste0("trajectory artifact skipped: the optimized ensemble's member seeds ",
                      "could not be mapped to the candidate scratch (missing, duplicated or ",
                      "unmatched seeds)"))
    }
    final_pidx    <- traj_map
    final_weights <- ensemble$parameter_weights

    traj <- if (is.null(final_pidx)) NULL else tryCatch(
      .mosaic_build_trajectories(
        scratch_dir         = traj_scratch_handle$dir,
        subset_orig_pidx    = final_pidx,
        subset_weights      = final_weights,
        # Member trajectories, not their observation-level draws.
        cases_array         = .mosaic_engine_array(ensemble, "cases"),
        deaths_array        = .mosaic_engine_array(ensemble, "deaths"),
        n_stoch             = ensemble$n_simulations_per_config,
        n_locations         = ensemble$n_locations,
        n_time_points       = ensemble$n_time_points,
        location_names      = ensemble$location_names,
        date_start          = ensemble$date_start,
        date_stop           = ensemble$date_stop,
        n_successful        = ensemble$n_successful,
        obs_cases           = ensemble$obs_cases,
        obs_deaths          = ensemble$obs_deaths,
        trajectory_channels = traj_scratch_handle$trajectory_channels,
        # reported_* central line follows the canonical central_method so it is
        # bit-identical to the prediction plots' predicted_central (median or mean).
        central_method      = central_method,
        n_lines             = traj_scratch_handle$n_lines %||% 150L,
        verbose             = control$logging$verbose
      ),
      error = function(e) {
        log_warn("trajectory reduction failed: %s", e$message); NULL
      })

    if (!is.null(traj)) {
      traj$cfr_refs <- .mosaic_compute_cfr_refs(cfr_posterior_run, traj$location_names)
      ensemble$trajectories <- traj
      .mosaic_persist_trajectory_artifact(ensemble, dirs, log_msg, log_warn)

      # Text companion to the .rds above. The .rds is classified heavy by the
      # MOSAIC-results promoter and lands store:"pending", so without this the
      # only channels that reach the archive are reported_cases/reported_deaths
      # via the prediction CSVs. Wrapped: a failed export must not lose a run.
      tryCatch({
        written <- write_trajectory_csv(traj, dirs$res_predictions,
                                        verbose = FALSE)
        log_msg("Saved %d 3_results/predictions/trajectories_*.csv", length(written))
      }, error = function(e) {
        log_warn("trajectory CSV export failed: %s", e$message)
      })
    }
    # Stream-to-disk scratch is transient -- always clean it up.
    unlink(traj_scratch_handle$dir, recursive = TRUE, force = TRUE)
  }

  # ===========================================================================
  # MEDOID SEED -- computed once on the final ensemble after all subset logic
  # ===========================================================================
  # The medoid is the ensemble member whose predicted case trajectory is closest
  # (log-scale MAE) to the ensemble CENTRAL trajectory (mean or median per
  # central_method). Computing here ensures it always reflects the FINAL ensemble
  # (optimized when optimize_subset=TRUE, candidate otherwise), using the correct
  # central series and the matching seed vector. The distance pools every
  # location and every scored time step (.mosaic_medoid_distances()), so a
  # multi-location run is not represented by its first location alone. It is
  # cases-only by design (a joint cases+deaths distance is a future refinement).

  medoid_seed_sim <- NULL

  if (!is.null(ensemble)) {
    tryCatch({
      # Deliberate mix: per-member stochastic spread is summarized by its MEDIAN
      # (robust to a member's stochastic outliers), while the ensemble TARGET is
      # the canonical central series (the cases median by default) so the chosen
      # representative member tracks the reported curve.
      # Engine-level member trajectories: an observation-level draw is noise
      # around a member, not the member.
      medoid_distances <- .mosaic_medoid_distances(
        .mosaic_engine_array(ensemble, "cases"), .central(ensemble, "cases"),
        ensemble$artifact_mask)

      medoid_idx <- which.min(medoid_distances)

      # Seed of the member the metric actually selected. `ensemble$seeds` is carried
      # ALONGSIDE the arrays (member i <-> seeds[i]) through both calc_model_ensemble
      # and optimize_ensemble_subset, so the index that minimises the distance maps
      # to that exact member's seed and cannot drift the way a separately re-derived
      # vector (optimal_seeds / param_seeds) can. Prefer it; fall back only if absent.
      final_seeds <- if (!is.null(ensemble$seeds) &&
                         length(ensemble$seeds) == ensemble$n_param_sets) {
        ensemble$seeds
      } else if (!is.null(subset_opt) && !is.null(subset_opt$optimal_seeds) &&
                 length(subset_opt$optimal_seeds) == ensemble$n_param_sets) {
        subset_opt$optimal_seeds
      } else {
        param_seeds
      }

      if (medoid_idx >= 1L && medoid_idx <= length(final_seeds) &&
          !is.na(final_seeds[[medoid_idx]])) {
        medoid_seed_sim <- final_seeds[[medoid_idx]]
        log_msg("Medoid: param set %d of %d (seed=%d, MAE=%.4f)",
                medoid_idx, ensemble$n_param_sets,
                medoid_seed_sim, medoid_distances[medoid_idx])
      }
    }, error = function(e) {
      log_warn("medoid computation failed: %s", e$message)
    })
  }

  # ===========================================================================
  # POSTERIOR QUANTILES AND DISTRIBUTIONS
  # ===========================================================================

  log_msg("Calculating posterior quantiles")
  posterior_quantiles <- calc_model_posterior_quantiles(
    results = results,
    probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
    output_dir = dirs$cal_posterior,
    priors = priors,
    subset_col = active_cols$subset_col,
    weight_col = active_cols$weight_col,
    verbose = control$logging$verbose
  )
  log_msg("Saved %s", file.path(dirs$cal_posterior, "posterior_quantiles.csv"))

  # Calculate posterior distributions
  log_msg("Calculating posterior distributions")
  calc_model_posterior_distributions(
    quantiles_file = file.path(dirs$cal_posterior, "posterior_quantiles.csv"),
    priors_file = file.path(dirs$inputs, "priors.json"),
    output_dir = dirs$cal_posterior,
    control = control,
    verbose = control$logging$verbose
  )
  log_msg("Saved %s", file.path(dirs$cal_posterior, "posteriors.json"))

  # ===========================================================================
  # PARAMETER SENSITIVITY (data layer)
  # ===========================================================================
  # Compute HSIC sensitivity + write parameter_sensitivity.csv unconditionally
  # so the table exists regardless of plots. The figure (and the correlation
  # figure) are rendered later by render_MOSAIC_figures().
  tryCatch(
    calc_model_parameter_sensitivity(
      results_file = file.path(dirs$calibration, "samples.parquet"),
      priors_file  = file.path(dirs$inputs, "priors.json"),
      output_dir   = dirs$res_fig_diag,
      subset_col   = active_cols$sample_col,
      verbose      = control$logging$verbose
    ),
    error = function(e) log_warn("parameter sensitivity computation failed: %s", e$message)
  )

  # ===========================================================================
  # POSTERIOR PREDICTIVE CHECKS (medoid representative model)
  # ===========================================================================

  # ---------------------------------------------------------------------------
  # The pipeline produces TWO post-calibration models: the posterior ENSEMBLE
  # (built above) and the MEDOID representative member. The single
  # best-likelihood model is intentionally NOT produced (no config_best.json, no
  # best plot/metrics); `is_best_model` in samples.parquet still flags the
  # top-likelihood draw for diagnostics.
  #
  # Order of operations:
  #   1. Sample config_medoid (non-fatal; NULL skips the medoid build).
  #   2. Build the medoid mosaic_ensemble object on the local PSOCK /
  #      sequential path, compute R^2/bias, render the plot.
  # ---------------------------------------------------------------------------

  log_msg("Running posterior predictive checks")

  # Pre-sample the medoid config (when a medoid seed was identified). NULL
  # skips the medoid build block below.
  config_medoid <- NULL
  if (!is.null(medoid_seed_sim)) {
    config_medoid <- tryCatch(
      .mosaic_clamp_transmission_params(sample_parameters(
        PATHS       = PATHS,
        priors      = priors,
        config      = config,
        seed        = medoid_seed_sim,
        sample_args = sampling_args,
        verbose     = FALSE
      )),
      error = function(e) {
        log_warn("sample_parameters failed for medoid seed %d: %s",
                medoid_seed_sim, e$message)
        NULL
      }
    )
  }
  # The medoid's own posterior reported CFR, from its stochastic ensemble below.
  # config_medoid.json is written after that ensemble, so it can carry it.
  medoid_cfr_posterior <- NULL

  # ---------------------------------------------------------------------------
  # Medoid stochastic ensemble
  # ---------------------------------------------------------------------------
  # The medoid used to be dispatched remotely here, via a single-element
  # param_configs list kept in the param_idx=1 seed namespace (seeds
  # 1001..1000+N) so a remote run stayed bit-reproducible against a local
  # re-run. calc_model_ensemble() now always runs it locally on that same
  # namespace, so `medoid_precomputed` is retired along with its
  # dispatch-failed / zero-sims-succeeded fallbacks.

  # ===========================================================================
  # MEDOID MODEL -- ensemble member closest to the ensemble central trajectory
  # Runs after the posterior ensemble using the same pattern: config -> engine ->
  # R^2 -> plot.
  # ===========================================================================

  tryCatch({
    if (!is.null(config_medoid)) {

      # Run N stochastic reruns of the medoid config: R^2/bias and the
      # prediction plot both derive from the ensemble central series
      # (central_method: cases median, deaths mean by default), consistent with
      # the posterior ensemble.
      log_msg("Building medoid model stochastic ensemble (%d reruns, source=%s)...",
              n_best_stochastic_per,
              if (ens_parallel) "local-parallel" else "local-sequential")
      medoid_ensemble <- tryCatch(
        calc_model_ensemble(
          config                   = config_medoid,
          configs                  = list(config_medoid),
          n_simulations_per_config = n_best_stochastic_per,
          envelope_quantiles       = c(0.025, 0.975),
          score_idx_cases          = control$likelihood$.score_window_resolved$idx_cases,
          score_idx_deaths         = control$likelihood$.score_window_resolved$idx_deaths,
          parallel                 = ens_parallel,
          n_cores                  = ens_n_cores,
          root_dir                 = root_dir,
          capture_trajectories     = FALSE,  # PLAN 14.H B-MEDOID: a median over
          # ~100 identically-weighted reruns is meaningless and nothing consumes a
          # medoid trajectory artifact -- never capture for the medoid.
          deaths_integration       = control$likelihood$.deaths_integration,
          observation_model        = obs_model_run,
          verbose                  = control$logging$verbose
        ),
        error = function(e) {
          log_warn("medoid model stochastic ensemble failed: %s", e$message)
          NULL
        }
      )

      if (!is.null(medoid_ensemble)) {
        medoid_cfr_posterior <- medoid_ensemble$cfr_posterior
        # Persist the medoid ensemble object (Phase 1a / G3) unconditionally so
        # render_MOSAIC_figures() can reconstruct the medoid prediction plot
        # from disk without re-simulating (invariant P5). The posterior ensemble
        # is already serialized (ensemble_candidate.rds / ensemble_optimized.rds);
        # the medoid was the one ensemble plot exposed to accidental local
        # re-simulation.
        medoid_ensemble <- .mosaic_stamp_artifact(medoid_ensemble)
        # Strip arrays from the on-disk copy only (unless persist_ensemble_arrays
        # =TRUE); the in-memory medoid_ensemble keeps its arrays for the metrics
        # computed just below (though .central() reads only light fields).
        tryCatch({
          saveRDS(if (.persist_arrays) medoid_ensemble
                  else .mosaic_ensemble_drop_arrays(medoid_ensemble),
                  file.path(dirs$calibration, "medoid_ensemble.rds"))
          log_msg("Saved 2_calibration/medoid_ensemble.rds")
        }, error = function(e) log_warn("medoid_ensemble.rds save failed: %s", e$message))

        med_c_flat <- as.numeric(.mosaic_mask_central_for_scoring(
          .central(medoid_ensemble, "cases"), "cases", medoid_ensemble$artifact_mask))
        med_d_flat <- as.numeric(.mosaic_mask_central_for_scoring(
          .central(medoid_ensemble, "deaths"), "deaths", medoid_ensemble$artifact_mask))
        obs_c_flat <- as.numeric(medoid_ensemble$obs_cases)
        obs_d_flat <- as.numeric(medoid_ensemble$obs_deaths)

        r2_cases_med   <- tryCatch(calc_model_R2(obs_c_flat, med_c_flat),  error = function(e) NA_real_)
        r2_deaths_med  <- tryCatch(calc_model_R2(obs_d_flat, med_d_flat),  error = function(e) NA_real_)
        bias_cases_med <- tryCatch(calc_bias_ratio(obs_c_flat, med_c_flat), error = function(e) NA_real_)
        bias_deaths_med <- tryCatch(calc_bias_ratio(obs_d_flat, med_d_flat), error = function(e) NA_real_)

        log_msg("Medoid model R\u00b2 (1 params x %d stoch): cases = %s (bias=%s), deaths = %s (bias=%s)",
                n_best_stochastic_per,
                .fmt_num(r2_cases_med,    4L), .fmt_num(bias_cases_med,  2L),
                .fmt_num(r2_deaths_med,   4L), .fmt_num(bias_deaths_med, 2L))

        # Medoid prediction CSVs (data layer): assemble + write unconditionally
        # so the medoid predictions exist regardless of plots. The figure is
        # rendered later by render_MOSAIC_figures() from medoid_ensemble.rds.
        tryCatch({
          medoid_table <- .mosaic_assemble_prediction_table(
            ensemble       = medoid_ensemble,
            central_method = central_method
          )
          .mosaic_write_prediction_csvs(
            prediction_table = medoid_table,
            data_dir         = dirs$res_predictions,
            file_prefix      = "medoid",
            verbose          = control$logging$verbose
          )
        }, error = function(e) log_warn("medoid prediction CSV write failed: %s", e$message))
      }
    }
  }, error = function(e) {
    log_warn("medoid model block failed: %s", e$message)
  })

  # config_medoid.json, carrying the medoid's own posterior reported CFR
  # (.mosaic_write_config_medoid()).
  if (!is.null(config_medoid)) {
    config_medoid <- .mosaic_write_config_medoid(
      config_medoid, medoid_cfr_posterior, cfr_posterior_run,
      file.path(dirs$cal_best_model, "config_medoid.json"), log_msg, log_warn)
  }

  # ===========================================================================
  # ENSEMBLE METRICS, WINDOWED FIT, AND PREDICTIVE PLOTS
  # ===========================================================================
  # The `ensemble` variable is the canonical object -- the optimized ensemble
  # when optimize_subset = TRUE and the optimizer succeeded, otherwise the
  # tier ensemble.

  if (!is.null(ensemble)) {

    n_ensemble_params <- ensemble$n_param_sets

    obs_c_flat    <- as.numeric(ensemble$obs_cases)
    obs_d_flat    <- as.numeric(ensemble$obs_deaths)

    # Canonical ensemble metrics follow central_method (per channel).
    cen_c_mat <- .mosaic_mask_central_for_scoring(
      .central(ensemble, "cases"), "cases", ensemble$artifact_mask)
    cen_d_mat <- .mosaic_mask_central_for_scoring(
      .central(ensemble, "deaths"), "deaths", ensemble$artifact_mask)
    cen_c_flat <- as.numeric(cen_c_mat)
    cen_d_flat <- as.numeric(cen_d_mat)
    r2_cases_ensemble  <- calc_model_R2(obs_c_flat, cen_c_flat)
    r2_deaths_ensemble <- calc_model_R2(obs_d_flat, cen_d_flat)
    bias_ratio_cases_ensemble  <- tryCatch(
      calc_bias_ratio(obs_c_flat, cen_c_flat), error = function(e) NA_real_)
    bias_ratio_deaths_ensemble <- tryCatch(
      calc_bias_ratio(obs_d_flat, cen_d_flat), error = function(e) NA_real_)

    # Dual metrics (both central tendencies) for summary.json cross-walk. These
    # are independent of central_method so median runs stay comparable.
    mean_c_flat   <- as.numeric(.mosaic_mask_central_for_scoring(
      ensemble$cases_mean,    "cases",  ensemble$artifact_mask))
    mean_d_flat   <- as.numeric(.mosaic_mask_central_for_scoring(
      ensemble$deaths_mean,   "deaths", ensemble$artifact_mask))
    median_c_flat <- as.numeric(.mosaic_mask_central_for_scoring(
      ensemble$cases_median,  "cases",  ensemble$artifact_mask))
    median_d_flat <- as.numeric(.mosaic_mask_central_for_scoring(
      ensemble$deaths_median, "deaths", ensemble$artifact_mask))
    r2_cases_ensemble_mean    <- tryCatch(calc_model_R2(obs_c_flat, mean_c_flat),   error = function(e) NA_real_)
    r2_deaths_ensemble_mean   <- tryCatch(calc_model_R2(obs_d_flat, mean_d_flat),   error = function(e) NA_real_)
    r2_cases_ensemble_median  <- tryCatch(calc_model_R2(obs_c_flat, median_c_flat), error = function(e) NA_real_)
    r2_deaths_ensemble_median <- tryCatch(calc_model_R2(obs_d_flat, median_d_flat), error = function(e) NA_real_)
    bias_ratio_cases_ensemble_mean    <- tryCatch(calc_bias_ratio(obs_c_flat, mean_c_flat),   error = function(e) NA_real_)
    bias_ratio_deaths_ensemble_mean   <- tryCatch(calc_bias_ratio(obs_d_flat, mean_d_flat),   error = function(e) NA_real_)
    bias_ratio_cases_ensemble_median  <- tryCatch(calc_bias_ratio(obs_c_flat, median_c_flat), error = function(e) NA_real_)
    bias_ratio_deaths_ensemble_median <- tryCatch(calc_bias_ratio(obs_d_flat, median_d_flat), error = function(e) NA_real_)

    log_msg("Ensemble R\u00b2 (%d params x %d stoch, source=%s): cases = %s (bias=%s), deaths = %s (bias=%s)",
            n_ensemble_params, n_ensemble_stochastic_per,
            if (isTRUE(ensemble_is_optimized)) "optimized" else active_cols$source,
            .fmt_num(r2_cases_ensemble,          4L), .fmt_num(bias_ratio_cases_ensemble,  2L),
            .fmt_num(r2_deaths_ensemble,         4L), .fmt_num(bias_ratio_deaths_ensemble, 2L))

    # Period-weighted implied CFR per location (posterior distribution from
    # ensemble members: sum simulated reported_deaths / sum simulated
    # reported_cases over the calibration window, per (param_set, stoch)).
    # Surfaces in summary.json:cfr_implied, alongside the posterior reported CFR
    # by location and year in 3_results/posterior/cfr_posterior.csv (the
    # calibrated mu_jt, which these realized period CFRs approach at epidemic PPV).
    cfr_implied <- tryCatch(
         .mosaic_calc_cfr_period_implied(
              # The members' own period CFR: engine-level trajectories.
              cases_array     = .mosaic_engine_array(ensemble, "cases"),
              deaths_array    = .mosaic_engine_array(ensemble, "deaths"),
              obs_cases       = ensemble$obs_cases,
              obs_deaths      = ensemble$obs_deaths,
              location_names  = ensemble$location_names %||% iso_code,
              envelope_quantiles = c(0.025, 0.5, 0.975),
              member_weights  = ensemble$parameter_weights,
              score_idx       = max(ensemble$artifact_mask$score_idx_cases %||% 1L,
                                    ensemble$artifact_mask$score_idx_deaths %||% 1L)
         ),
         error = function(e) {
              log_warn("implied-CFR computation failed: %s", e$message)
              NULL
         }
    )

    if (!is.null(cfr_implied)) {
         for (iso in names(cfr_implied)) {
              ci <- cfr_implied[[iso]]
              if (is.finite(ci$predicted_median)) {
                   # CI fields are NA when n_members < MIN_MEMBERS_FOR_CI (=20).
                   # Render NA cleanly as "NA" instead of " NA%" via sprintf.
                   fmt_pct <- function(x) if (is.finite(x)) sprintf("%.3f%%", 100 * x) else "NA"
                   log_msg("Implied CFR (%s): predicted = %s [%s, %s]; observed = %s  (members=%d)",
                           iso,
                           fmt_pct(ci$predicted_median),
                           fmt_pct(ci$predicted_ci_lo),
                           fmt_pct(ci$predicted_ci_hi),
                           fmt_pct(ci$observed),
                           ci$n_members)
              }
         }
    }

    # Windowed model fit metrics on the canonical ensemble. Uses the chosen
    # central series (scoring-masked, as for the headline r2_*_ensemble) so the
    # windowed diagnostic agrees with the headline metrics + plots. Passed as
    # [n_loc x n_time] matrices: windows are taken over TIME and pool the
    # locations within them.
    n_ts      <- ensemble$n_time_points
    dates_vec <- seq.Date(as.Date(config$date_start), by = "day", length.out = n_ts)

    windowed_metrics <- .mosaic_compute_windowed_metrics(
      obs_cases  = ensemble$obs_cases,
      est_cases  = cen_c_mat,
      obs_deaths = ensemble$obs_deaths,
      est_deaths = cen_d_mat,
      dates      = dates_vec,
      windows    = fit_windows
    )

    wm_path <- file.path(dirs$res_fig_diag, "model_fit_windows.csv")
    utils::write.csv(windowed_metrics, wm_path, row.names = FALSE)
    log_msg("Saved 3_results/figures/diagnostics/model_fit_windows.csv")

    full_row      <- windowed_metrics[windowed_metrics$window == "full", ]
    short_windows <- windowed_metrics[windowed_metrics$window != "full", ]
    if (nrow(short_windows) > 0L) {
      last_row <- short_windows[nrow(short_windows), ]
      log_msg("Windowed fit: R2_cases [full=%.3f, %s=%.3f] | Bias [full=%.2f, %s=%.2f]",
              full_row$r2_cases, last_row$window, last_row$r2_cases,
              full_row$bias_cases, last_row$window, last_row$bias_cases)
    }

    # (The windowed-metrics figure is rendered later by render_MOSAIC_figures()
    # from model_fit_windows.csv, which is written above independent of plots.)

    # Posterior-ensemble prediction CSVs (data layer): assemble + write
    # unconditionally (independent of plots). The figure is rendered later by
    # render_MOSAIC_figures() from the persisted ensemble .rds.
    tryCatch({
      ensemble_table <- .mosaic_assemble_prediction_table(
        ensemble       = ensemble,
        central_method = central_method
      )
      .mosaic_write_prediction_csvs(
        prediction_table = ensemble_table,
        data_dir         = dirs$res_predictions,
        file_prefix      = "ensemble",
        verbose          = control$logging$verbose
      )
    }, error = function(e) log_warn("ensemble prediction CSV write failed: %s", e$message))

  } else {
    log_warn("ensemble is NULL \u2014 skipping ensemble R\u00b2 and predictive plots")
  }

  # ===========================================================================
  # COMBINE PREDICTION CSVs
  # ===========================================================================

  # Combine per-location prediction CSVs by type. Ensemble and medoid each
  # have their own column schema and cannot be rbind'd together.
  # Per-location files live in dirs$res_predictions alongside the combined
  # file. Pattern: predictions_<type>_<LOC>.csv (per-location) ->
  # predictions_<type>_all.csv (combined, multi-location only). For N=1 the
  # per-location CSV is canonical on its own and no _all.csv is written.
  for (pred_type in c("ensemble", "medoid")) {
    pred_csvs <- list.files(
      dirs$res_predictions,
      pattern = sprintf("^predictions_%s_.*\\.csv$", pred_type),
      full.names = TRUE
    )
    pred_csvs <- pred_csvs[!grepl(sprintf("predictions_%s_all\\.csv$", pred_type),
                                  pred_csvs)]
    if (length(pred_csvs) > 1) {
      combined <- tryCatch({
        pred_list <- lapply(pred_csvs, utils::read.csv, stringsAsFactors = FALSE)
        do.call(rbind, pred_list)
      }, error = function(e) {
        log_warn("Could not combine %s prediction CSVs: %s", pred_type, e$message)
        NULL
      })
      if (!is.null(combined)) {
        out_file <- file.path(dirs$res_predictions, sprintf("predictions_%s_all.csv", pred_type))
        utils::write.csv(combined, out_file, row.names = FALSE)
        log_msg("Combined %d %s prediction files into %s", length(pred_csvs), pred_type, basename(out_file))
      }
    }
  }

  # (Posterior-predictive-check figures are rendered later by
  # render_MOSAIC_figures() from the now-unconditional prediction CSVs.)

  runtime <- difftime(Sys.time(), start_time, units = "mins")
  log_msg("Calibration complete: %d batches, %d simulations, %.2f min",
          state$batch_number, state$total_sims_run, as.numeric(runtime))

  # ===========================================================================
  # WRITE RESULTS SUMMARY
  # ===========================================================================

  # Parameter estimates (tidy CSV for downstream use)
  log_msg("Writing parameter estimates...")
  .mosaic_write_parameter_estimates(dirs)
  log_msg("  Saved 3_results/posterior/parameter_estimates.csv")

  # (The psi_star diagnostic figure is rendered later by render_MOSAIC_figures()
  # from parameter_estimates.csv + config.json + the suitability inputs.)

  # Calibration diagnostics (3_results/calibration_diagnostics.json, schema 2):
  # every number of the calibration-diagnostics report, written whatever
  # `plots` is (CLAUDE.md lesson #10), and before the render below, which
  # draws the report (3_results/figures/diagnostics/calibration_report.pdf)
  # from it.
  tryCatch({
    cal_diag <- .mosaic_calibration_diagnostics(
      conv_health = conv_health, state = state, config = config, control = control,
      n_obs_scored = n_obs_scored, ess_replay = ess_replay,
      quantiles_file = file.path(dirs$cal_posterior, "posterior_quantiles.csv"),
      prediction_table = ensemble_table, is_diag_all = is_diag_all)
    .mosaic_write_calibration_diagnostics(cal_diag, file.path(dirs$results, "calibration_diagnostics.json"))
    log_msg("  Saved 3_results/calibration_diagnostics.json")
  }, error = function(e) log_warn("calibration_diagnostics.json not written: %s", conditionMessage(e)))

  # ===========================================================================
  # RENDER FIGURES (visualization layer)
  # ===========================================================================
  # All pipeline figures are now produced by a single pass over the finished
  # run directory. Every numeric artifact the figures consume has been written
  # unconditionally above (invariants P1/P2), so plots gate ONLY rendering. The
  # renderer is a pure read-render (P5): it never re-simulates.
  if (isTRUE(control$paths$plots)) {
    log_msg("Rendering figures from run directory...")
    # There is no cluster to borrow by this point: the calibration cluster was
    # stopped right after the calibration loop, and calc_model_ensemble() builds
    # and tears down its own. So render builds one, sized from the run's own
    # n_cores and capped internally by how many workers the memory-heavy figure
    # families can actually use. Nothing else holds sockets here, so R's
    # 128-connection ceiling is not in play. The central method this run
    # resolved is passed in, so the figures cannot take it from a file on disk
    # (summary.json is written only after this render).
    tryCatch(
      .mosaic_render_figures(
        dir_output     = dir_output,
        plots          = TRUE,
        verbose        = control$logging$verbose,
        n_cores        = if (isTRUE(control$parallel$enable))
                           control$parallel$n_cores else 1L,
        central_method = central_method
      ),
      error = function(e) log_warn("render_MOSAIC_figures failed: %s", e$message)
    )
  }

  # Summary JSON (machine-readable run summary, schema 2)
  log_msg("Writing summary...")
  summary_obj <- .mosaic_write_summary_json(dirs, state, start_time, config,
    nb_dispersion = control$likelihood$.nb_dispersion_table,
    r2_cases_ensemble = r2_cases_ensemble,
    r2_deaths_ensemble = r2_deaths_ensemble,
    bias_ratio_cases_ensemble = bias_ratio_cases_ensemble,
    bias_ratio_deaths_ensemble = bias_ratio_deaths_ensemble,
    central_method = central_method,
    r2_cases_ensemble_mean = r2_cases_ensemble_mean,
    r2_deaths_ensemble_mean = r2_deaths_ensemble_mean,
    r2_cases_ensemble_median = r2_cases_ensemble_median,
    r2_deaths_ensemble_median = r2_deaths_ensemble_median,
    bias_ratio_cases_ensemble_mean = bias_ratio_cases_ensemble_mean,
    bias_ratio_deaths_ensemble_mean = bias_ratio_deaths_ensemble_mean,
    bias_ratio_cases_ensemble_median = bias_ratio_cases_ensemble_median,
    bias_ratio_deaths_ensemble_median = bias_ratio_deaths_ensemble_median,
    n_ensemble_params = n_ensemble_params,
    n_ensemble_stochastic_per = n_ensemble_stochastic_per,
    r2_cases_ensemble_tier = r2_cases_ensemble_tier,
    r2_deaths_ensemble_tier = r2_deaths_ensemble_tier,
    bias_ratio_cases_ensemble_tier = bias_ratio_cases_ensemble_tier,
    bias_ratio_deaths_ensemble_tier = bias_ratio_deaths_ensemble_tier,
    n_ensemble_params_tier = n_ensemble_params_tier,
    cfr_implied = if (exists("cfr_implied", inherits = FALSE)) cfr_implied else NULL,
    conv_health = conv_health,
    weighting = weighting_scheme,
    param_ess = ess_results,
    n_simulations_total = nrow(results),
    n_retained = if (legacy_scheme) sum(results$is_retained) else NA_integer_,
    n_members_candidate = if (!is.null(subset_opt) && isTRUE(ensemble_is_optimized))
                            length(member_idx) else NA_integer_,
    opt_metrics = opt_metrics,
    io = control$io)
  log_msg("  Saved 3_results/summary.json")

  # Human-readable summary
  fmt_r2b <- function(r2, bias) {
    r2s <- if (is.na(r2)) "NA" else sprintf("%.4f", r2)
    bs  <- if (is.na(bias)) "" else sprintf(" (bias=%.2f)", bias)
    paste0(r2s, bs)
  }
  fmt_n <- function(x) if (length(x) == 1L && is.finite(x)) format(round(x), big.mark = ",") else "NA"
  status_str_h <- if (is.na(summary_obj$convergence_status)) "not assessed" else toupper(summary_obj$convergence_status)
  log_msg("=== Run Summary ===")
  log_msg("  Location: %s (%s to %s)", summary_obj$location, summary_obj$date_start, summary_obj$date_stop)
  log_msg("  Simulation convergence: %s%s%s", status_str_h,
          if (is.na(summary_obj$convergence_label)) "" else paste0(" -- ", summary_obj$convergence_label),
          if (identical(state$mode, "fixed")) " (fixed budget: assessed, not used to stop)"
          else if (isTRUE(summary_obj$stopping_rule_met)) " (adaptive: stopping rule met)"
          else " (adaptive: stopping rule not met)")
  if (nzchar(summary_obj$convergence_reasons_text %||% ""))
    log_msg("  Reasons: %s", summary_obj$convergence_reasons_text)
  vs_n <- function(x, tgt) {
    xs <- if (is.finite(x %||% NA)) .mosaic_fmt_thr(x, tgt %||% NA, 0) else "NA"
    if (is.finite(x %||% NA) && is.finite(tgt %||% NA) && x >= tgt)
      sprintf("%s >= target %s", xs, fmt_n(tgt)) else sprintf("%s (target %s)", xs, fmt_n(tgt))
  }
  log_msg("  Ensemble: %s member%s (%s unique), ensemble ESS %s, ESS_all %s (posterior_weighting = %s)",
          fmt_n(summary_obj$ensemble_members), if (isTRUE(summary_obj$ensemble_members == 1)) "" else "s",
          fmt_n(summary_obj$ensemble_members_unique),
          vs_n(summary_obj$ess_best, summary_obj$ess_best_target),
          vs_n(summary_obj$ess_all, summary_obj$ess_all_target), weighting_scheme)
  if (!is.null(summary_obj$sims_projected_for_green) && is.finite(summary_obj$sims_projected_for_green))
    log_msg("  Simulations projected for green: about %s%s",
            fmt_n(signif(summary_obj$sims_projected_for_green, 2)),
            if (isFALSE(summary_obj$projection_reliable)) " (indicative: ESS growth stalled or not yet known)" else "")
  log_msg("  R2 ensemble:   cases = %s | deaths = %s",
          fmt_r2b(summary_obj$r2_cases_ensemble, summary_obj$bias_ratio_cases_ensemble),
          fmt_r2b(summary_obj$r2_deaths_ensemble, summary_obj$bias_ratio_deaths_ensemble))
  if (is.finite(summary_obj$param_ess_min %||% NA))
    log_msg("  Per-parameter ESS of the members (diagnostic): min %.1f, median %.1f over %d parameters",
            summary_obj$param_ess_min, summary_obj$param_ess_median, as.integer(summary_obj$ess_n_params))
  log_msg("  Sims: %s total, %s valid", fmt_n(summary_obj$n_simulations_total),
          fmt_n(summary_obj$sims_used))
  if (isTRUE(state$resumed)) {
    log_msg("  Resumed: YES (%s sim(s) reused from a prior run)",
            format(state$n_sims_reused %||% 0L, big.mark = ","))
  }
  log_msg("===================")

  # Single grep-friendly run-end sentinel. A tailing operator (human or AI
  # monitoring the log) can latch on '\[RUN_SUMMARY\]' to know the pipeline
  # finished AND extract headline metrics in one pass. status / mode: see
  # .mosaic_run_status() (fixed mode never applies the stopping rule).
  int_s <- function(x) if (length(x) == 1L && is.finite(x)) sprintf("%d", as.integer(round(x))) else "NA"
  outputs_ok <- !is.na(summary_obj$r2_cases_ensemble)
  status_str <- .mosaic_run_status(state, outputs_ok)
  log_msg(paste0(
    "[RUN_SUMMARY] status=%s mode=%s convergence_status=%s stopping_rule_met=%s ",
    "outputs_ok=%s runtime_min=%.2f r2_cases_ensemble=%s ",
    "r2_deaths_ensemble=%s sims_total=%s sims_used=%s ensemble_members=%s ess_best=%s ",
    "ess_all=%s resumed=%s dir_output=%s"),
    status_str,
    if (is.null(state$mode)) "NA" else state$mode,
    if (is.na(summary_obj$convergence_status)) "NA" else summary_obj$convergence_status,
    if (is.na(summary_obj$stopping_rule_met)) "NA" else if (summary_obj$stopping_rule_met) "YES" else "NO",
    if (outputs_ok) "YES" else "NO",
    as.numeric(runtime),
    if (is.na(summary_obj$r2_cases_ensemble))  "NA" else sprintf("%.4f", summary_obj$r2_cases_ensemble),
    if (is.na(summary_obj$r2_deaths_ensemble)) "NA" else sprintf("%.4f", summary_obj$r2_deaths_ensemble),
    # machine-read sentinel: plain integers, no thousands separators (PN2)
    int_s(summary_obj$n_simulations_total), int_s(summary_obj$sims_used),
    int_s(summary_obj$ensemble_members),
    if (is.finite(summary_obj$ess_best %||% NA)) sprintf("%.1f", summary_obj$ess_best) else "NA",
    if (is.finite(summary_obj$ess_all %||% NA)) sprintf("%.1f", summary_obj$ess_all) else "NA",
    if (isTRUE(state$resumed)) "YES" else "NO",
    dir_output
  )

  # Reached normal end-of-run -- suppress the on.exit [RUN_FATAL] sentinel.
  run_completed <- TRUE

  # Finalize run state (mark as completed)
  .mosaic_finalize_state(state_file, summary_obj)

  # Return invisibly
  invisible(list(
    dirs = dirs,
    files = list(
      simulations = file.path(dirs$calibration, "samples.parquet"),
      ess_csv = file.path(dirs$cal_diag, "parameter_ess.csv"),
      posterior_quantiles = file.path(dirs$cal_posterior, "posterior_quantiles.csv"),
      posteriors_json = file.path(dirs$cal_posterior, "posteriors.json")
    ),
    summary = list(
      batches = state$batch_number,
      sims_total = state$total_sims_run,
      sims_success = state$total_sims_successful,
      convergence_status = summary_obj$convergence_status,
      stopping_rule_met = summary_obj$stopping_rule_met,
      converged = .mosaic_run_converged(state),
      convergence_evaluated = .mosaic_convergence_evaluated(state),
      resumed = isTRUE(state$resumed),
      sims_reused = state$n_sims_reused %||% 0L,
      runtime_min = as.numeric(runtime)
    )
  ))
}

#' @rdname run_MOSAIC
#' @export
run_mosaic <- run_MOSAIC

#' Build Complete MOSAIC Control Structure
#'
#' @description
#' Creates a complete control structure for \code{run_mosaic()}.
#' This is the primary interface for configuring MOSAIC execution settings, consolidating
#' calibration strategy, parameter sampling, parallelization, and output options.
#'
#' **Parameters are organized in workflow order:**
#' \enumerate{
#'   \item \code{calibration}: How to run (simulations, iterations, batch sizes)
#'   \item \code{sampling}: What to sample (which parameters to vary)
#'   \item \code{likelihood}: How to score model fit (likelihood components and weights)
#'   \item \code{targets}: The posterior, the ensemble and when to stop (convergence status)
#'   \item \code{parallel}: Infrastructure (cores, cluster type)
#'   \item \code{io}: Output format (file format, compression)
#'   \item \code{paths}: File management (output directories, plots)
#' }
#'
#' @param calibration List of calibration settings. Default is:
#'   \itemize{
#'     \item \code{n_simulations}: NULL for auto (adaptive) mode, or integer for fixed mode
#'     \item \code{n_iterations}: Number of stochastic engine iterations per parameter set (default: 3L)
#'     \item \code{max_simulations_total}: Budget cap of auto mode (default: 150000L). A run
#'       that reaches it before the convergence status is green ends with the
#'       \code{budget_cap_reached} flag
#'     \item \code{batch_size_adaptive}: Simulations per batch until the minimum run length is
#'       met, and the fallback size afterwards (default: 500L)
#'     \item \code{max_batch_predictive}: Cap on each projected batch (default: 10000L)
#'     \item \code{min_batches_adaptive}: Minimum batches before any stop or projection (default: 5L)
#'     \item \code{min_draws}: Minimum draws before any stop or projection (default: 5000L)
#'   }
#'   Auto mode computes the convergence status (\code{\link{calc_convergence_status}})
#'   on every draw so far after each batch, on the members of
#'   \code{posterior_weighting}, and stops when its sampling checks (ensemble
#'   ESS, Monte Carlo error, k-hat) are green at two consecutive checks and both
#'   minimum run lengths are met; informativeness never blocks the stop. Once
#'   the minimum is met, batches are sized from the projection N x ESS_all
#'   target / ESS_all (converted to simulations at the success rate) while the
#'   ESS growth slope is at least 0.8, and otherwise grow geometrically (a quarter of the draws so far,
#'   between \code{batch_size_adaptive} and \code{max_batch_predictive}), so a
#'   heavy-tailed run reaches a 150,000 cap in about 20 checks after the minimum run.
#'   In parallel each batch is rounded up to a multiple of workers x
#'   \code{io$shard_batch_size}, never beyond the budget left.
#'   \code{max_batches_adaptive},
#'   \code{max_batches_predictive} and \code{target_r2_adaptive} were retired in
#'   1.4.0 (warned and ignored when set).
#'
#' @param sampling List of parameter sampling flags (what to sample). Default is:
#'   \itemize{
#'     \item \code{sample_tau_i}: Sample the daily departure (travel) probability by location (default: TRUE)
#'     \item \code{sample_mobility_gamma}: Sample the gravity-model distance-decay exponent (default: TRUE)
#'     \item \code{sample_mobility_omega}: Sample the gravity-model population-scaling exponent (default: TRUE)
#'     \item \code{sample_iota}: Sample the incubation rate, E to I (default: TRUE)
#'     \item \code{sample_gamma_2}: Sample the asymptomatic recovery rate (default: TRUE; second-dose vaccine effectiveness is \code{sample_phi_2})
#'     \item \code{sample_alpha_1}: Sample within-metapop population mixing exponent (default: FALSE, PINNED)
#'     \item \code{sample_alpha_2}: Sample frequency-dependence degree (default: FALSE; pinned, weakly identified)
#'     \item ... (see \code{mosaic_control_defaults()} for complete list of 40 flags)
#'   }
#'
#' @param likelihood List of likelihood calculation settings (how to score model fit). Default is:
#'   \itemize{
#'     \item \code{weight_cases}: Weight for cases vs deaths (default: 1.0)
#'     \item \code{weight_deaths}: Weight for deaths vs cases (default: 1.0)
#'     \item \code{weight_wis}: WIS regularizer weight (default: 0, off). The 0.10
#'       suggested before v0.101.0 was tuned against the daily cases core, the
#'       default; against the weekly core (\code{cases_scoring = "weekly"}) a given
#'       weight weighs several times more (a median 4.8 times, range 1.8 to 6.5, on
#'       the v0.100.1 national re-selection pools; more where the cases dispersion
#'       is small), so about 0.02 keeps that balance there. Not re-tuned since;
#'       check the fit before relying on it
#'     \item \code{cases_scoring}: \code{"daily"} (default; one NB cell per day at
#'       the weekly k, the cell rule of v0.100.1 and earlier) or \code{"weekly"}
#'       (cases scored as NB on reporting-week totals). The daily rule is the
#'       default because the weekly rule fitted the cases worse in the v0.101.0
#'       likelihood gate. Either rule runs at the dispersion this version
#'       estimates, so the default does not reproduce a v0.100.1 run: a cases fit
#'       with no estimate of its own, or clamped at the lower bound, now takes the
#'       panel trend, a config with \code{reported_tier} restricts the cases k and
#'       the deaths dispersion to observed weeks, the ensemble intervals are
#'       observation-level (weekly NB at the scored k under either rule) and the
#'       cases central line is the median. Matching a v0.100.1 likelihood also
#'       needs that run's \code{nb_k_cases} (its \code{nb_dispersion.csv}) and a
#'       config without \code{reported_tier}; resume refuses to pool with v0.100.1
#'       simulations either way
#'     \item ... (see \code{mosaic_control_defaults()} for complete list)
#'   }
#'
#' @param targets List of posterior and convergence settings. Default is:
#'   \itemize{
#'     \item \code{posterior_weighting}: The ensemble members, from which the
#'       parameter posterior and the predictions are built (default:
#'       \code{"bestfit"}). \code{"bestfit"}: the draws in descending
#'       log-likelihood until the Kish ESS of their coarsened weights reaches
#'       \code{best_subset_ratio} x ESS_all, bounded by
#'       \code{min_ensemble_members} / \code{max_ensemble_members}, carrying
#'       those weights. \code{"coarsened"} (the 1.3.0 default): a systematic
#'       resample of K = clamp(round(ESS_all), min, max) draws weighted by
#'       count / K. \code{"saturated"} (pre-1.3.0) and \code{"tempered"}: the
#'       tier-searched best subset (with \code{ESS_best} meaning the tier target,
#'       100 unless set, and \code{A_best} 0.70, \code{CVw_best} 1.0,
#'       \code{min_best_subset}, \code{max_best_subset}). Formerly
#'       \code{best_subset_weighting} (accepted with a deprecation warning).
#'     \item \code{coarsening_alpha}: Effective number of independent observations of the
#'       coarsened posterior, \eqn{\eta = \alpha / n_{obs}} with \eqn{n_{obs}} the
#'       scored observation cells (default: 10). See \code{\link{calc_weight_health}}.
#'     \item \code{best_subset_ratio}: Share of ESS_all the best-fit ensemble holds (default: 0.2).
#'     \item \code{ESS_best}: Required ensemble ESS (default: 200; 100 under the
#'       legacy schemes). The convergence status is green at or above it, and auto
#'       mode simulates towards ESS_all = \code{ESS_best / best_subset_ratio}. Its
#'       meaning changed in 1.4.0 (it was the tier-search target in 1.3.x): an
#'       explicit value is honoured under every scheme, with a warning when it
#'       differs from the scheme's default.
#'     \item \code{min_draws} (in \code{calibration}) above
#'       \code{max_simulations_total} is clamped to it with a warning.
#'     \item \code{min_ensemble_members}, \code{max_ensemble_members}: Bounds on the
#'       ensemble size (default: 30, 1000; set the maximum lower for continental runs:
#'       the ensemble arrays grow with it).
#'     \item \code{status_thresholds}: Bands of the convergence status (default
#'       \code{list(ess_yellow_frac = 0.5, mce_green = 0.10, mce_yellow = 0.15,
#'       khat_green = 0.5, khat_yellow = 0.7, slope_stall = 0.8,
#'       flat_ess_frac = 0.01)}; \code{flat_ess_frac} is the untempered-ESS share
#'       of the draws at or above which the likelihood counts as flat); see
#'       \code{\link{calc_convergence_status}}.
#'     \item \code{min_best_subset}, \code{max_best_subset}: Bounds of the legacy tier
#'       search (default: 30, 1000); ignored (with a warning when set) under
#'       \code{"bestfit"} and \code{"coarsened"}.
#'     \item \code{ESS_method}: ESS formula, "kish" or "perplexity", of the legacy
#'       schemes' ESS_B / A / CVw, the subset optimiser and the \eqn{\eta = 1}
#'       likelihood-concentration numbers (default: "perplexity").
#'   }
#'   Deprecated in 1.4.0 (warned and ignored when set): \code{ESS_param},
#'   \code{ESS_param_prop} and \code{batch_ratio_max} (the per-parameter-ESS
#'   stopping rule, replaced by the convergence status); \code{A_best} and
#'   \code{CVw_best} outside the legacy schemes. \code{ESS_marginal_method} was
#'   retired in 1.3.0.
#'
#' @param predictions List of prediction generation settings. Default is:
#'   \itemize{
#'     \item \code{n_iter_ensemble}: Stochastic runs per posterior parameter set
#'       in the weighted ensemble (default: 10L). Total ensemble sims =
#'       N parameter sets x \code{n_iter_ensemble}.
#'     \item \code{n_iter_best}: Stochastic runs for the medoid single-config
#'       prediction plots (default: 100L). Applied identically to both models.
#'       These runs execute on an internal PSOCK cluster when
#'       \code{parallel$enable = TRUE}.
#'     \item \code{optimize_subset}: Logical; when \code{TRUE}, the post-ensemble
#'       optimizer (MAE / WIS / R^2+bias) refines the ensemble members used for
#'       \code{posteriors.json}, \code{posterior_quantiles.csv}, and the ensemble
#'       object driving all downstream plots and metrics, and the convergence
#'       status is then assessed on the optimised subset. The members
#'       are preserved in the \code{is_best_subset} / \code{weight_best} columns of
#'       \code{samples.parquet} for provenance; the optimized selection is written
#'       to new \code{is_best_subset_opt} / \code{weight_best_opt} columns. Default
#'       \code{FALSE} (it was \code{TRUE} in v1.2.x; reverted in 1.3.0 because, under
#'       the coarsened posterior, the optimizer re-truncates the members by their
#'       fit to the training data). An explicit \code{TRUE} is honoured
#'       under every weighting scheme. When \code{FALSE}, the ensemble members
#'       (\code{is_best_subset} / \code{weight_best}) are canonical and no
#'       \code{_opt} columns are written.
#'       The optimizer chooses among subsets of the simulated candidate ensemble,
#'       so the size it selects is between \code{optimize_min_n} and the
#'       number of members. \strong{Statistical note:} enabling this
#'       flag yields a posterior conditioned on ensemble predictive performance
#'       (MAE / WIS / R^2+bias on the training data) rather than a pure
#'       likelihood-weighted posterior.
#'     \item \code{optimize_min_n}: Minimum subset size the optimizer may select
#'       (default \code{30L}, raised from \code{4L} to guard against KDE
#'       degeneracy in \code{posteriors.json}).
#'     \item \code{optimize_objective}: Objective function, \code{"mae"} (default),
#'       \code{"r2_bias"}, or \code{"wis"}.
#'     \item \code{optimize_stride}: Integer >= 1 (default \code{3L}). \code{1L}
#'       evaluates every candidate subset size in \code{min_n:max_n} (exhaustive,
#'       bit-identical to the historical search). \code{> 1L} runs a
#'       coarse-then-refine search that evaluates a strided grid first and then
#'       refines around the best point. The default \code{3L} gives roughly a 3x
#'       speedup with negligible accuracy cost on the smooth \code{score(N)} curve
#'       (it re-examines a +/- 3 window around the coarse winner); raise it
#'       (\code{5L}/\code{10L}/\code{25L}) for more speed, or set \code{1L} to force
#'       the exhaustive search. Note it may select a slightly different
#'       \code{optimal_n} than exhaustive and records only the evaluated N's in the
#'       diagnostics table. (The \code{\link{optimize_ensemble_subset}} function's
#'       own \code{stride} default remains \code{1L} to preserve bit-identicality.)
#'     \item \code{central_method}: Ensemble central tendency, \code{"mean"}
#'       (the expected count, which never collapses to zero on sparse deaths) or
#'       \code{"median"} (the typical trajectory, robust to a few explosive
#'       members). Scalar or per-channel \code{c(cases=, deaths=)}; default
#'       \code{c(cases = "median", deaths = "mean")} since v0.101.0 (both mean
#'       from v0.98.0 to v0.100.x, both median from v0.46.1 to v0.97.x). Either
#'       way the line is a summary of the engine-level member trajectories; the
#'       intervals are observation-level. Governs the prediction
#'       trajectory + plots, the canonical \code{*_ensemble} R^2/bias metrics,
#'       the medoid target, and the subset-selection objective consistently.
#'   }
#'   The number of parameter sets in the ensemble is determined by the best subset
#'   (all sims with non-zero importance weights).
#'
#' @param weights List of importance-weight settings. Default is:
#'   \itemize{
#'     \item \code{iqr_multiplier}: Tukey lower-fence multiplier of the legacy
#'       schemes (default: 1.5; use 3.0 for extreme outliers only).
#'   }
#'   \code{floor} is deprecated (1.4.0; warned and ignored).
#'
#' @param parallel List of parallelization settings (infrastructure). Default is:
#'   \itemize{
#'     \item \code{enable}: Enable parallel execution (default: FALSE)
#'     \item \code{n_cores}: Number of simulation worker processes (default: 1L)
#'     \item \code{type}: Cluster type, "PSOCK" or "FORK" (default: "PSOCK")
#'     \item \code{progress}: Show progress bar (default: TRUE)
#'   }
#'
#' @param io List of I/O settings (output format). Default is:
#'   \itemize{
#'     \item \code{format}: Output format; only "parquet" is supported ("csv" is coerced to "parquet" with a warning)
#'     \item \code{compression}: Compression algorithm (default: "zstd")
#'     \item \code{compression_level}: Compression level (default: 3L)
#'     \item \code{persist_ensemble_arrays}: Retain the dense 4-D arrays --
#'       the observation-level \code{cases_array}/\code{deaths_array} and the
#'       engine-level \code{cases_engine_array}/\code{deaths_engine_array} --
#'       in the persisted ensemble RDS
#'       files (\code{ensemble_candidate.rds}, \code{ensemble_optimized.rds},
#'       \code{subset_opt.rds}, \code{medoid_ensemble.rds}). Default \code{FALSE}
#'       strips the arrays at save time so the on-disk artifacts are small
#'       (~tens of KB); every light field (central tendencies, envelopes,
#'       weights, seeds, obs, metadata) is preserved and all standard consumers
#'       (plotting, OCV, rolling CV) work unchanged. Set \code{TRUE} for a
#'       re-analysable raw archive (e.g. re-running subset optimization or the
#'       R_eff posterior-resimulation CI path from the saved file). The
#'       in-memory object used during the run is never affected (default:
#'       \code{FALSE}).
#'   }
#'   \code{verbose_weights} is deprecated (1.4.0; it had no effect).
#'
#' @param paths List of path and output settings (file management). Default is:
#'   \itemize{
#'     \item \code{clean_output}: Remove output directory if exists (default: FALSE)
#'     \item \code{plots}: Generate diagnostic plots (default: TRUE)
#'   }
#'
#' @param logging List of logging settings. Default is:
#'   \itemize{
#'     \item \code{verbose}: Enable detailed progress messages in sub-functions
#'       (default: FALSE).
#'   }
#'
#' @return A complete control list suitable for passing to \code{run_mosaic()}.
#'
#' @examples
#' # Default control settings
#' ctrl <- mosaic_control_defaults()
#'
#' # Quick parallel run with 8 cores
#' ctrl <- mosaic_control_defaults(
#'   parallel = list(enable = TRUE, n_cores = 8)
#' )
#'
#' # Fixed mode with 5000 simulations, 5 iterations each
#' ctrl <- mosaic_control_defaults(
#'   calibration = list(
#'     n_simulations = 5000,
#'     n_iterations = 5
#'   )
#' )
#'
#' # Auto mode with custom settings
#' ctrl <- mosaic_control_defaults(
#'   calibration = list(
#'     n_simulations = NULL,  # NULL = auto mode
#'     n_iterations = 3,
#'     max_simulations_total = 50000,
#'     batch_size_adaptive = 1000
#'   ),
#'   parallel = list(enable = TRUE, n_cores = 16)
#' )
#'
#' # Only sample specific parameters
#' ctrl <- mosaic_control_defaults(
#'   sampling = list(
#'     sample_tau_i = TRUE,
#'     sample_mobility_gamma = FALSE,
#'     sample_mobility_omega = FALSE,
#'     sample_beta_j0_tot = TRUE,
#'     sample_iota = FALSE,
#'     sample_gamma_2 = FALSE,
#'     sample_alpha_1 = FALSE
#'   )
#' )
#'
#' # Enable WIS regularizer and peak timing
#' ctrl <- mosaic_control_defaults(
#'   likelihood = list(
#'     weight_wis = 0.02,
#'     weight_peak_timing = 0.25
#'   )
#' )
#'
#' # The 1.3.0 resampled ensemble instead of the best-fit default
#' ctrl <- mosaic_control_defaults(
#'   targets = list(posterior_weighting = "coarsened")
#' )
#'
#' # Full workflow configuration (demonstrates logical order)
#' ctrl <- mosaic_control_defaults(
#'   calibration = list(n_simulations = NULL, n_iterations = 3),      # How to run
#'   sampling = list(sample_tau_i = TRUE, sample_beta_j0_tot = TRUE),  # What to sample
#'   likelihood = list(weight_wis = 0.02, weight_cases = 1.0),        # How to score
#'   targets = list(ESS_best = 200, best_subset_ratio = 0.2),         # When to stop
#'   parallel = list(enable = TRUE, n_cores = 16),                    # Infrastructure
#'   io = mosaic_io_presets("default"),                               # Output format
#'   paths = list(clean_output = FALSE, plots = TRUE)                 # File management
#' )
#'
#' @export
#' @rdname mosaic_control_defaults
mosaic_control_defaults <- function(calibration = NULL,
                           sampling = NULL,
                           likelihood = NULL,
                           targets = NULL,
                           predictions = NULL,
                           weights = NULL,
                           parallel = NULL,
                           io = NULL,
                           paths = NULL,
                           logging = NULL) {

  # Default calibration settings
  # Auto mode checks the convergence status (calc_convergence_status()) after
  # every batch and stops once its sampling checks are green at two consecutive
  # checks and both minimum run lengths are met, or at max_simulations_total.
  # Batches are batch_size_adaptive until the minimum run length is met, then
  # sized from the projection N x ESS_all target / ESS_all (capped at
  # max_batch_predictive) while the ESS growth slope is >= 0.8, rounded up to
  # fill the cluster in parallel.
  default_calibration <- list(
    n_simulations = NULL,      # NULL = auto mode, integer = fixed mode
    n_iterations = 3L,          # iterations per simulation
    max_simulations_total = 150000L,  # budget cap of auto mode (the stopping rule's cap)
    batch_size_adaptive = 500L,       # batch size before the projection applies, and its fallback
    max_batch_predictive = 10000L, # Cap on each projected batch (prevents multi-hour unchecked runs)
    min_batches_adaptive = 5L,        # minimum batches before any stop or projection
    min_draws = 5000L                 # minimum draws before any stop or projection
  )

  # Default sampling settings: one flag per sampled parameter group. The list
  # (and the meaning of each flag) lives in .mosaic_default_sample_args() so
  # sample_parameters(), create_sampling_args() and this function cannot drift.
  default_sampling <- .mosaic_default_sample_args()

  # Default likelihood calculation settings
  default_likelihood <- list(
    # === Component weights (0 = OFF; set > 0 to enable) ===
    weight_cases = 1.0,              # Weight for cases vs deaths
    weight_deaths = 1.0,             # Weight for deaths vs cases
    weight_peak_timing = 0,          # Scaled by N_obs / N_peaks; default OFF
    weight_peak_magnitude = 0,       # Scaled by N_obs / N_peaks; default OFF
    weight_cumulative_total = 0,     # Scaled by N_obs / N_timepoints; default OFF
    weight_wis = 0,                  # Scaled by N_obs / N_quantiles; default OFF (see ?calc_model_likelihood)

    # === Per-channel epsilon floor on the predicted mean ===
    # The NB density is evaluated at max(1e-4, eps_rel * mean(obs)) per location
    # and channel. Production scores ONE stochastic realisation, so a low-count
    # series is mostly structural zeros and the floor sets the price of a zero
    # prediction against a positive observation. Too small a floor makes zeros
    # ruinous and the likelihood optimum shifts onto draws that over-predict the
    # LEVEL (a Jensen gap, not a CFR misspecification). Deaths need a much larger
    # fraction than cases: 13.6% of scored deaths cells predict zero against a
    # positive observation versus 1.7% of cases cells. 0.25 for deaths is the
    # swept value at which the deaths level at the likelihood optimum is unbiased.
    # Since v0.96.0 run_MOSAIC() scores deaths with the reported CFR integrated
    # out (quasi-Poisson, weekly), so eps_rel_deaths applies only to a standalone
    # calc_model_likelihood() call without ll_deaths_core; the integrated score
    # uses eps_rel_cases as its weekly background, the same relative floor as cases.
    eps_rel_cases = 0.02,            # Relative floor, cases channel (and the integrated deaths background)
    eps_rel_deaths = 0.25,           # Relative floor, standalone NB deaths scoring only (not read by run_MOSAIC)

    # === Cases scoring resolution ===
    # "daily" (default): one NB cell per day at the weekly k, the cell rule of
    # v0.100.1 and earlier, at the dispersion this version estimates. It does NOT
    # reproduce a v0.100.1 run (panel-trend k for collapsed and clamped fits,
    # observed-week k and deaths phi under reported_tier, observation-level
    # intervals; matching a v0.100.1 likelihood also needs that run's nb_k_cases
    # and no reported_tier). "weekly": NB on reporting-week totals of observed and
    # simulated cases, on the weeks est_nb_dispersion() estimates k from. The
    # surveillance is weekly totals spread over days, so the daily rule counts a
    # week's level information ~5x (median over the v0.100.1 national runs); the
    # weekly rule was the 0.101.0 candidate default, but in the pre-registered
    # likelihood gate (KEN, ZMB, CMR, GHA; same data, dispersions, intervals and
    # seeds) it fitted the cases worse than daily on both counts of its direct
    # test (geometric-mean cases rWIS weekly/daily 1.068, median |log cases bias|
    # 0.254 vs 0.187), so the default stays daily pending investigation.
    cases_scoring = "daily",

    # === Peak controls ===
    sigma_peak_time = 1,             # Std dev for peak timing Gaussian (in weeks)
    sigma_peak_log = 0.5,            # Std dev for log peak magnitude

    # === Time/location weighting ===
    weights_time = NULL,             # Numeric vector of per-timestep weights (NULL = uniform)
    weights_location = NULL,         # Numeric vector of per-location weights (NULL = uniform)

    # === Per-channel scored window (burn-in + deaths-era start) ===
    # Run-time only; config_default stays the clean [date_start, date_stop]
    # window. burn_in_days defaults to 30 (NOT 0): the seeded E/I discharge into
    # cases settles by ~day 14-21, and since v0.96.0 deaths are drawn at onset and
    # reported on the case lag, so their transient settles with the cases; 30
    # clears both channels while costing ~1-2.5% of a multi-year window. Set burn_in_days = 0L to disable (restores pre-v0.47.3 scoring,
    # bit-identical). See .mosaic_resolve_score_window() and the optimal-window
    # research. NOTE: on very short windows 30 may clamp to near n_time; set 0L.
    burn_in_days = 30L,              # Leading steps dropped from BOTH channels (IC transient; 0L disables)
    deaths_score_start = NULL,       # NULL = full window, or Date/"YYYY-MM-DD" deaths-era start
    score_start_cases = NULL         # NULL = derive from burn_in_days, or Date/"YYYY-MM-DD"
  )

  # Default parallel settings
  default_parallel <- list(
    enable = FALSE,
    n_cores = 1L,
    type = "PSOCK",
    progress = TRUE
  )

  # Default path settings
  default_paths <- list(
    clean_output = FALSE,
    plots = TRUE
  )

  # Default target settings. The posterior over every valid draw is the
  # coarsened (generalised-Bayes) posterior w propto exp(eta * (ll - max ll)),
  # eta = coarsening_alpha / n_obs scored cells (weight_all). The ensemble and
  # the parameter posterior are built from the members of posterior_weighting.
  conv_defaults <- .mosaic_convergence_defaults()
  default_targets <- list(
    # Posterior weighting scheme. Changing it changes the posterior; it is NOT a
    # cosmetic switch.
    #   "bestfit" (default since 1.4.0): the draws in descending log-likelihood
    #     until the Kish ESS of their coarsened weights reaches
    #     best_subset_ratio x ESS_all, bounded by min/max_ensemble_members
    #     (.mosaic_bestfit_select()). They carry their coarsened weights,
    #     renormalised (is_best_subset / weight_best).
    #   "coarsened" (the 1.3.0 default, opt-in): a systematic resample of K =
    #     clamp(round(ESS_all), min_ensemble_members, max_ensemble_members) draws
    #     from the coarsened posterior, weighted by count / K.
    #   "saturated" (the default before 1.3.0, opt-in) and "tempered": the
    #     tier-searched best subset, weighted by exp(-0.5 * min(delta AIC, 4))
    #     or by adaptive-eta Gibbs weights. The tier search reads ESS_best (100
    #     unless set), A_best (0.70), CVw_best (1.0), min_best_subset and
    #     max_best_subset; kept so runs made before 1.3.0 are reproducible.
    posterior_weighting = "bestfit",

    # Effective number of independent observations the likelihood is trusted
    # with: eta = coarsening_alpha / n_obs scored cells.
    coarsening_alpha = 10,

    # The best-fit ensemble holds this share of ESS_all (ESS_best = ratio x
    # ESS_all by construction; about the top 38% of the posterior weight on the
    # 2026-10 production runs).
    best_subset_ratio = conv_defaults$best_subset_ratio,

    # Required ensemble ESS: the convergence status is GREEN at >= ESS_best and
    # auto mode simulates towards ESS_all = ESS_best / best_subset_ratio. Under
    # "saturated"/"tempered" it keeps its legacy meaning, the tier-search target
    # (100 unless set).
    ESS_best = conv_defaults$ESS_best,

    # Resource bounds on the ensemble (members are simulated n_iter_ensemble
    # times and the ensemble arrays grow with them; set lower for continental
    # runs).
    min_ensemble_members = conv_defaults$min_ensemble_members,
    max_ensemble_members = conv_defaults$max_ensemble_members,

    # Bands of the convergence status (calc_convergence_status()).
    status_thresholds = conv_defaults$thresholds,

    # Legacy tier-search bounds of "saturated"/"tempered" only.
    min_best_subset = 30,
    max_best_subset = 1000,

    # ESS formula ("kish" or "perplexity") of the legacy schemes' ESS_B / A /
    # CVw, the subset optimiser and the eta = 1 likelihood-concentration numbers.
    ESS_method = "perplexity"
  )

  # Default prediction settings
  default_predictions <- list(
    n_iter_ensemble    = 10L,            # Stochastic runs per posterior parameter set (ensemble)
    n_iter_best        = 100L,           # Stochastic runs for medoid single-config plots
    optimize_subset    = FALSE,          # Post-ensemble subset optimization (TRUE in 1.2.x; FALSE again
                                          # since 1.3.0: under the coarsened posterior it re-truncates the
                                          # resampled members by in-sample fit)
    optimize_min_n     = 30L,            # Minimum subset size -- raised from 4L (Fox et al. 2024)
                                          # to guard against KDE degeneracy when the
                                          # optimized subset drives posterior densities.
    optimize_objective = "mae",          # Objective: "mae", "r2_bias", or "wis"
    optimize_stride    = 3L,             # Subset-search stride: 1 = exhaustive
                                          # (every N in min_n:max_n); 3 (default) =
                                          # coarse-then-refine, ~3x fewer evals with
                                          # negligible accuracy cost on the smooth
                                          # score(N) curve. Raise (5/10/25) for more
                                          # speed; set 1 for a bit-identical exhaustive
                                          # search. (The optimize_ensemble_subset()
                                          # function default stays 1L for parity.)
    central_method     = c(cases = "median", deaths = "mean"),
                                          # Ensemble central tendency per channel
                                          # (default from v0.101.0): "median" (the
                                          # typical trajectory) for cases, "mean" (the
                                          # expected count, never 0 on sparse deaths)
                                          # for deaths. A scalar sets both. Drives
                                          # predictions, plots, *_ensemble metrics,
                                          # medoid, subset.
    capture_trajectories = TRUE,          # Capture comprehensive internal-state channels
                                          # (compartments + FOI + incidence + burden) from
                                          # the POSTERIOR ensemble and persist
                                          # 2_calibration/trajectories_ensemble.rds for the
                                          # "trajectories" figure group. RAM/payload is
                                          # LINEAR in trajectory_channels; set FALSE to
                                          # disable for cost-sensitive large remote runs.
    trajectory_channels = NULL,           # NULL => calc_model_ensemble's comprehensive
                                          # default set (the RAM lever; shorten to reduce
                                          # capture cost).
    trajectory_n_lines  = 150L            # Uniform-thinned actual member trajectories per
                                          # location for the spaghetti display.
  )

  # Default weight calculation settings
  default_weights <- list(
    iqr_multiplier = 1.5  # Tukey lower fence of the legacy schemes (1.5 = standard, 3.0 = extreme outliers only)
  )

  # Default I/O settings
  default_io <- list(
    format = "parquet",
    compression = "zstd",
    compression_level = 3L,
    load_method = "streaming",         # "streaming" (memory-safe) or "rbind" (legacy)
    load_chunk_size = 5000L,           # Files per chunk when loading many small parquets
    shard_batch_size = 100L,           # Simulations per shard file (1 = one file per simulation).
                                       # 100 measured 57x faster to combine and
                                       # 34.6x smaller on disk than 1, with
                                       # byte-identical output. Clamped down when
                                       # the budget is too small to keep every
                                       # worker fed.
    save_simresults = FALSE,           # Save raw per-(sim,iter,j,t) output for validation
    persist_ensemble_arrays = FALSE    # Retain the dense observation- and engine-level arrays in persisted ensemble RDS files (FALSE => stripped at save; small artifacts)
  )

  # Default logging settings
  default_logging <- list(
    verbose = FALSE                    # Enable detailed progress messages in sub-functions
  )

  # Merge user-provided settings with defaults
  # Order follows workflow: calibration -> sampling -> likelihood -> targets -> predictions -> weights -> parallel -> io -> paths -> logging
  list(
    calibration = if (is.null(calibration)) default_calibration else modifyList(default_calibration, calibration),
    sampling = if (is.null(sampling)) default_sampling else modifyList(default_sampling, sampling),
    likelihood = if (is.null(likelihood)) default_likelihood else modifyList(default_likelihood, likelihood),
    targets = if (is.null(targets)) default_targets else {
      # A legacy scheme keeps its legacy tier target unless ESS_best is given.
      if (isTRUE((targets$posterior_weighting %||% targets$best_subset_weighting) %in% c("saturated", "tempered")) &&
          is.null(targets$ESS_best))
        default_targets$ESS_best <- 100
      modifyList(default_targets, targets)
    },
    predictions = if (is.null(predictions)) default_predictions else modifyList(default_predictions, predictions),
    weights = if (is.null(weights)) default_weights else modifyList(default_weights, weights),
    parallel = if (is.null(parallel)) default_parallel else modifyList(default_parallel, parallel),
    io = if (is.null(io)) default_io else modifyList(default_io, io),
    paths = if (is.null(paths)) default_paths else modifyList(default_paths, paths),
    logging = if (is.null(logging)) default_logging else modifyList(default_logging, logging)
  )
}

