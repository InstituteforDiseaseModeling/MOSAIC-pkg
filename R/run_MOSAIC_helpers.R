# =============================================================================
# MOSAIC: run_mosaic_helpers.R
# Internal helper functions for run_mosaic()
# =============================================================================

# All functions prefixed with . (not exported - internal use only)

# Constants for validation
.MOSAIC_MAX_ITERATIONS <- 1000L
.MOSAIC_MIN_BATCH_SIZE <- 10L
.MOSAIC_MAX_BATCH_SIZE <- 1000000L
.MOSAIC_MIN_SIMULATIONS <- 100L
.MOSAIC_MAX_SIMULATIONS <- 100000000L

# =============================================================================
# Persisted-artifact schema stamping (Phase 1c)
# =============================================================================

# Schema version stamped on every persisted .rds run_MOSAIC() writes for the
# standalone renderer (render_MOSAIC_figures()). Bump when the on-disk object
# layout the renderer depends on changes incompatibly, so the renderer can
# warn + skip rather than mis-render an old run directory.
.MOSAIC_ARTIFACT_SCHEMA_VERSION <- 1L

#' Stamp a persisted artifact with the renderer schema version
#'
#' Adds a \code{mosaic_schema_version} attribute (and class tag) so
#' \code{render_MOSAIC_figures()} can validate compatibility of an on-disk
#' \code{.rds} before consuming it. Pass-through for \code{NULL}.
#'
#' @param x The object to stamp (typically a \code{mosaic_ensemble} or
#'   subset-optimization result list).
#' @return \code{x} with \code{attr(x, "mosaic_schema_version")} set.
#' @noRd
.mosaic_stamp_artifact <- function(x) {
  if (is.null(x)) return(x)
  attr(x, "mosaic_schema_version") <- .MOSAIC_ARTIFACT_SCHEMA_VERSION
  x
}

#' Read the renderer schema version off a persisted artifact
#'
#' @param x An object loaded from an \code{.rds}.
#' @return Integer schema version, or \code{NA_integer_} when unstamped.
#' @noRd
.mosaic_artifact_schema_version <- function(x) {
  v <- attr(x, "mosaic_schema_version")
  if (is.null(v)) NA_integer_ else as.integer(v)
}

#' Persist the engine spatial-structure arrays for the "spatial" figure group
#'
#' Extracts the element-wise-median spatial arrays carried on a
#' \code{mosaic_ensemble} (\code{spatial_hazard_ensemble} J x T,
#' \code{coupling_ensemble} J x J, \code{pi_ij_ensemble} J x J; aggregated
#' across the posterior-ensemble members, DM#5) and writes them to
#' \code{2_calibration/} as schema-stamped \code{.rds} objects in config order.
#' Each array is wrapped with its \code{location_name} labels and an
#' \code{estimator} tag (\code{"ensemble_median"} vs \code{"medoid_single"}).
#' Missing arrays (engine without \code{DerivedValues}) are silently skipped.
#'
#' @param ensemble A \code{mosaic_ensemble} object.
#' @param dirs The directory list from \code{.mosaic_ensure_dir_tree()}.
#' @param log_msg,log_warn Logging callbacks.
#' @param estimator Character label recorded on each artifact.
#' @return Invisibly \code{TRUE} if anything was written, else \code{FALSE}.
#' @noRd
.mosaic_persist_spatial_artifacts <- function(ensemble, dirs, log_msg, log_warn,
                                              estimator = "ensemble_median") {
  if (is.null(ensemble)) return(invisible(FALSE))
  wrote <- FALSE

  .write_one <- function(arr, file, kind) {
    if (is.null(arr)) return(invisible(FALSE))
    obj <- list(
      array         = arr,
      location_name = ensemble$location_names,
      estimator     = estimator,
      kind          = kind,
      date_start    = ensemble$date_start,
      date_stop     = ensemble$date_stop,
      n_members     = ensemble$n_param_sets
    )
    tryCatch({
      saveRDS(.mosaic_stamp_artifact(obj), file.path(dirs$calibration, file))
      log_msg("Saved 2_calibration/%s", file)
      TRUE
    }, error = function(e) {
      log_warn("%s save failed: %s", file, e$message); FALSE
    })
  }

  if (.write_one(ensemble$spatial_hazard_ensemble,
                 "spatial_hazard_ensemble.rds", "spatial_hazard")) wrote <- TRUE
  if (.write_one(ensemble$coupling_ensemble,
                 "coupling_ensemble.rds", "coupling")) wrote <- TRUE
  if (.write_one(ensemble$pi_ij_ensemble,
                 "pi_ij_ensemble.rds", "pi_ij")) wrote <- TRUE

  invisible(wrote)
}

#' Posterior reported-CFR reference for the trajectory CFR panel
#'
#' The ensemble's posterior reported CFR by location and year
#' (\code{calc_model_ensemble()$cfr_posterior}), in config location order, for
#' the dashed reference on the trajectory CFR(t) panel. On epidemic-PPV ticks the
#' simulated reported CFR should sit on this line. Returns \code{NULL} when the
#' ensemble carries no posterior (deaths were not redrawn).
#'
#' @param cfr_posterior Data frame from \code{calc_model_ensemble()$cfr_posterior}, or \code{NULL}.
#' @param location_names Character vector of locations (config order).
#' @return A data.frame \code{location, year, cfr_median, cfr_lower, cfr_upper}, or \code{NULL}.
#' @noRd
.mosaic_compute_cfr_refs <- function(cfr_posterior, location_names) {
  if (is.null(cfr_posterior) || !is.data.frame(cfr_posterior) || !nrow(cfr_posterior)) return(NULL)
  keep <- cfr_posterior$location %in% location_names
  if (!any(keep)) return(NULL)
  out <- cfr_posterior[keep, c("location", "year", "cfr_median", "cfr_lower", "cfr_upper"), drop = FALSE]
  out <- out[order(match(out$location, location_names), out$year), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Write config_medoid.json with the medoid's posterior reported CFR
#'
#' The reported CFR is integrated out, not sampled, so the sampled medoid config
#' still carries the prior \code{mu_jt}. It is shifted to the medoid's OWN
#' posterior CFR (\code{medoid_cfr_posterior}, from the medoid ensemble's post-hoc
#' CFR draws), so a re-simulation of the file -- rolling-CV projections,
#' scenarios -- reproduces the medoid predictions' deaths level. The run-level
#' posterior is the fallback when the medoid ensemble failed; it is not the
#' default because the medoid's conditional CFR differs from the ensemble's by the
#' medoid path's own case misfit. With neither, or when the shift is refused
#' (a posterior CFR no per-onset probability can produce), the prior is written.
#'
#' @param config_medoid The sampled medoid config.
#' @param medoid_cfr_posterior,run_cfr_posterior \code{cfr_posterior} data frames, or \code{NULL}.
#' @param path Output file.
#' @param log_msg,log_warn Logging functions.
#' @return The config written (invisibly the same object the file holds).
#' @noRd
.mosaic_write_config_medoid <- function(config_medoid, medoid_cfr_posterior, run_cfr_posterior,
                                        path, log_msg = function(...) invisible(NULL),
                                        log_warn = function(...) invisible(NULL)) {
  post <- if (!is.null(medoid_cfr_posterior)) medoid_cfr_posterior else run_cfr_posterior
  source_lbl <- if (!is.null(medoid_cfr_posterior)) "medoid" else if (!is.null(run_cfr_posterior)) "run" else "none"
  if (!is.null(post)) {
    config_medoid <- tryCatch(.mosaic_apply_cfr_posterior(config_medoid, post),
      error = function(e) {
        log_warn("medoid config keeps the prior mu_jt: %s", conditionMessage(e))
        source_lbl <<- "none"
        config_medoid
      })
  }
  tryCatch({
    # 17 significant digits + temp-file-then-rename, like the 1_inputs writers:
    # this is the config that downstream re-simulation reproduces the medoid from.
    .mosaic_write_json(config_medoid, path)
    log_msg("Saved %s (reported CFR: %s)", path,
            switch(source_lbl, medoid = "medoid posterior", run = "run posterior", "prior"))
  }, error = function(e) log_warn("config_medoid.json write failed: %s", conditionMessage(e)))
  config_medoid
}

#' Persist the compact trajectory artifact for the "trajectories" figure group
#'
#' Writes the \code{mosaic_trajectories} object carried on a
#' \code{mosaic_ensemble} (\code{$trajectories}; comprehensive internal-state
#' channels — per-channel weighted median + uniform-thinned actual member lines +
#' derived series) to \code{2_calibration/trajectories_ensemble.rds},
#' schema-stamped. The channels are reduced from the candidate run's scratch over
#' the FINAL displayed subset and its weights: the optimized subset
#' (\code{optimal_weights}) when subset optimization succeeded, otherwise the
#' candidate best subset (\code{weight_best}); see
#' \code{.mosaic_trajectory_member_map()}. A \code{NULL} \code{$trajectories}
#' (capture off, or the capability check warned-and-skipped) is silently skipped.
#'
#' @param ensemble A \code{mosaic_ensemble} object.
#' @param dirs Directory list from \code{.mosaic_ensure_dir_tree()}.
#' @param log_msg,log_warn Logging callbacks.
#' @return Invisibly \code{TRUE} if written, else \code{FALSE}.
#' @noRd
.mosaic_persist_trajectory_artifact <- function(ensemble, dirs, log_msg, log_warn) {
  if (is.null(ensemble) || is.null(ensemble$trajectories)) return(invisible(FALSE))
  tryCatch({
    saveRDS(.mosaic_stamp_artifact(ensemble$trajectories),
            file.path(dirs$calibration, "trajectories_ensemble.rds"))
    log_msg("Saved 2_calibration/trajectories_ensemble.rds")
    invisible(TRUE)
  }, error = function(e) {
    log_warn("trajectories_ensemble.rds save failed: %s", e$message)
    invisible(FALSE)
  })
}

# =============================================================================
# Transmission-parameter guardrail
# =============================================================================

#' Clamp transmission parameters into engine-valid ranges
#'
#' Applied to every sampled parameter set before it is simulated, so that
#' calibration scoring and post-calibration prediction (best / medoid /
#' posterior ensemble) use IDENTICAL clamped values. Prevents the error where
#' \code{p = -expm1(-rate) > 1} when \code{rate < 0} (originally the
#' laser-cholera ValueError of GitHub #24; the R engine inherits the same
#' constraint, since it is a property of the chain-binomial parameterisation
#' rather than of the implementation) and keeps probabilities in \[0, 1\]. Idempotent -- clamping an
#' already-clamped config is a no-op -- so it is safe to apply at every sampling
#' site without changing values that are already in range.
#'
#' @param params A config/parameter list from \code{sample_parameters()} (or NULL).
#' @return \code{params} with transmission fields clamped (NULL passed through).
#' @noRd
.mosaic_clamp_transmission_params <- function(params) {
  if (is.null(params)) return(params)
  if (!is.null(params$beta_j0_tot)) params$beta_j0_tot <- pmax(params$beta_j0_tot, 1e-10)
  if (!is.null(params$beta_j0_hum)) params$beta_j0_hum <- pmax(params$beta_j0_hum, 0)
  if (!is.null(params$beta_j0_env)) params$beta_j0_env <- pmax(params$beta_j0_env, 0)
  if (!is.null(params$p_beta))      params$p_beta      <- pmin(pmax(params$p_beta, 1e-6), 1 - 1e-6)
  if (!is.null(params$tau_i))       params$tau_i       <- pmin(pmax(params$tau_i, 0), 1)
  params
}

# =============================================================================
# Ensemble central-tendency selection
# =============================================================================

#' Resolve a central_method specification into a per-channel character vector
#'
#' \code{central_method} selects which weighted ensemble summary
#' (\code{"mean"} or \code{"median"}) of the engine-level member trajectories
#' is treated as the canonical central trajectory for predictions, plots,
#' ensemble R^2/bias metrics, the medoid target, and the subset-selection
#' objective. The package default (v0.101.0) is the weighted MEDIAN for cases --
#' the typical trajectory, robust to a few explosive members -- and the weighted
#' MEAN for deaths, the expected count, which never collapses to zero on sparse
#' deaths (the weighted median does). Both were the mean from v0.98.0 to
#' v0.100.x and from v0.38.0 to v0.46.0, and both the median from v0.46.1 to
#' v0.97.x; a scalar reproduces runs made then.
#'
#' Accepts a scalar (applies to both channels) or a named vector to set cases
#' and deaths independently, e.g. \code{c(cases = "median", deaths = "mean")}.
#'
#' @param x \code{NULL}, a scalar \code{"mean"}/\code{"median"}, or a named
#'   vector with \code{"cases"} and/or \code{"deaths"}. \code{NULL} or an
#'   unset channel falls back to the package default for that channel
#'   (\code{"median"} for cases, \code{"mean"} for deaths).
#' @return Named character vector \code{c(cases = ., deaths = .)}, each
#'   \code{"mean"} or \code{"median"}.
#' @noRd
.mosaic_resolve_central_method <- function(x = NULL) {
  valid <- c("mean", "median")
  ch    <- c("cases", "deaths")
  out   <- stats::setNames(c("median", "mean"), ch)

  if (is.null(x) || length(x) == 0L) {
    return(out)
  }

  if (is.null(names(x)) || all(!nzchar(names(x)))) {
    if (length(x) != 1L) {
      stop("central_method must be a single value or a named vector with ",
           "'cases'/'deaths'.", call. = FALSE)
    }
    m <- match.arg(as.character(x), valid)
    out[] <- m
    return(out)
  }

  nm      <- names(x)
  if (any(!nzchar(nm))) {
    stop("central_method per-channel values must all be named ('cases'/'deaths'); ",
         "got an unnamed element.", call. = FALSE)
  }
  unknown <- setdiff(nm[nzchar(nm)], ch)
  if (length(unknown)) {
    stop("central_method has unknown channel name(s): ",
         paste(unknown, collapse = ", "),
         ". Use 'cases' and/or 'deaths'.", call. = FALSE)
  }
  for (c_ in ch) {
    if (c_ %in% nm) out[[c_]] <- match.arg(as.character(x[[c_]]), valid)
  }
  out
}

#' Prepare the control list for 1_inputs/control.json
#'
#' jsonlite writes an atomic vector as a bare array and drops its names, so a
#' per-channel \code{central_method = c(cases = , deaths = )} would reach
#' control.json without its channel labels. The resolved value is persisted as a
#' named list, which serialises as \code{{"cases": ..., "deaths": ...}}. A value
#' that does not resolve is left as supplied; \code{run_MOSAIC()} never writes
#' one, because \code{.mosaic_validate_and_merge_control()} rejects it first.
#' @param control Validated control list.
#' @return \code{control} with \code{predictions$central_method} as a named list.
#' @noRd
.mosaic_control_for_json <- function(control) {
  cm <- tryCatch(.mosaic_resolve_central_method(control$predictions$central_method),
                 error = function(e) NULL)
  if (!is.null(cm)) control$predictions$central_method <- as.list(cm)
  # Adaptive mode: n_simulations is NULL, which jsonlite writes as {} (an
  # empty object that consumers read as a value, P10). NA is written as JSON
  # null by .mosaic_write_json(na = "null"), and reads back as NULL (auto mode).
  if ("calibration" %in% names(control) && is.null(control$calibration$n_simulations))
    control$calibration["n_simulations"] <- list(NA)
  control
}

#' Extract the canonical central trajectory matrix from a mosaic_ensemble
#'
#' Returns the \code{<channel>_mean} or \code{<channel>_median} matrix
#' (\code{[n_loc x n_time]}) per the resolved \code{central_method}.
#'
#' @param ens A \code{mosaic_ensemble} object (carries both \code{*_mean} and
#'   \code{*_median} fields).
#' @param chan \code{"cases"} or \code{"deaths"}.
#' @param method Named per-channel vector from
#'   \code{.mosaic_resolve_central_method()}.
#' @return Numeric matrix; the requested central trajectory.
#' @noRd
.mosaic_central_series <- function(ens, chan, method) {
  field <- paste0(chan, "_", method[[chan]])
  out   <- ens[[field]]
  # Fall back to the median if a (e.g. older) ensemble lacks the *_mean field,
  # mirroring plot_model_ensemble()/run_rolling_cv() so no consumer mis-scores
  # against a NULL series.
  if (is.null(out)) out <- ens[[paste0(chan, "_median")]]
  out
}

#' Mask engine-artifact time positions in a central series for scoring
#'
#' Sets the engine-artifact time-positions to \code{NA} in a central-series
#' matrix so they are dropped pairwise by \code{calc_model_R2()}/
#' \code{calc_bias_ratio()} (both \code{na_rm = TRUE} by default). This is applied
#' to the EST series only; the observed series stays unmasked, and the ensemble's
#' raw central/array fields are never mutated -- only the matrix passed here is
#' transformed. Artifacts: (1) the first \code{cases_warmup} cases timesteps are an
#' initial-condition warm-up transient; (2) when \code{deaths_final} is set, the
#' final deaths timestep, a structural zero in the laser-cholera engine's output
#' (reported deaths written at \code{tick} rather than \code{tick + 1}, so the
#' last row was trimmed; laser-cholera issue #82). The R engine reports deaths on
#' the cases' row since v0.96.0, so \code{calc_model_ensemble()} records
#' \code{deaths_final = FALSE}.
#'
#' Masks by COLUMN (= time), so it is correct for any number of locations (rows);
#' scoring sites flatten column-major via \code{as.numeric()}.
#'
#' @param mat Numeric matrix \code{[n_loc x n_time]} (a vector is coerced to a
#'   single-row matrix). The central series to mask.
#' @param chan \code{"cases"} or \code{"deaths"}.
#' @param spec Artifact-mask list (\code{ens$artifact_mask}). If \code{NULL},
#'   falls back to \code{list(cases_warmup = 2L, deaths_final = TRUE)}: an
#'   ensemble saved before the field existed came from the laser-cholera engine.
#' @return The matrix with artifact columns set to \code{NA}.
#' @noRd
.mosaic_mask_central_for_scoring <- function(mat, chan, spec) {
  if (is.null(spec)) spec <- list(cases_warmup = 2L, deaths_final = TRUE)
  if (is.null(mat)) return(mat)
  if (is.null(dim(mat))) mat <- matrix(mat, nrow = 1L)
  nc <- ncol(mat)
  if (is.null(nc) || nc < 1L) return(mat)

  # Per-channel scored-window start (burn-in / deaths-era start). Columns
  # strictly BEFORE score_idx_{cases,deaths} are unscored and NA'd here so the
  # R2/bias sites drop them pairwise, exactly mirroring the likelihood-input
  # slice. spec fallback (older ensembles) is 1L = no-op, so existing scoring is
  # unchanged. This is a UNION with the engine-artifact masks below (warm-up /
  # final-deaths); a burn-in that exceeds n_cases_warmup_mask supersedes it.
  score_idx <- if (identical(chan, "cases")) spec$score_idx_cases else spec$score_idx_deaths
  if (!is.null(score_idx)) {
    si <- as.integer(score_idx)
    if (length(si) == 1L && !is.na(si) && si > 1L) {
      si <- min(si, nc + 1L)
      mat[, seq_len(si - 1L)] <- NA_real_
    }
  }

  if (identical(chan, "cases")) {
    k <- as.integer(spec$cases_warmup)
    if (length(k) == 1L && !is.na(k) && k > 0L) {
      k <- min(k, nc)
      mat[, seq_len(k)] <- NA_real_
    }
  } else if (identical(chan, "deaths")) {
    if (isTRUE(spec$deaths_final)) {
      mat[, nc] <- NA_real_
    }
  }
  mat
}

#' Distance from each ensemble member to the ensemble central cases trajectory
#'
#' The medoid is the member whose predicted cases are closest to the ensemble
#' central series. Each member is summarised by the median over its stochastic
#' runs of its ENGINE-level trajectories (\code{.mosaic_engine_array()}; an
#' observation-level draw is noise around a member, not the member); the
#' distance is the mean absolute difference of \code{log(x + eps)}
#' over every location and every SCORED time step (the central series is
#' passed through \code{.mosaic_mask_central_for_scoring()} first, so the
#' burn-in head and engine artifacts do not count). Every location therefore
#' carries equal weight, and a single-location run reduces to the per-location
#' log-MAE.
#'
#' @param cases_array Numeric array \code{[n_loc, n_time, n_param, n_stoch]} of
#'   engine-level reported cases.
#' @param central Numeric matrix \code{[n_loc, n_time]}, the central cases series.
#' @param mask_spec Artifact-mask list (\code{ens$artifact_mask}).
#' @param eps Offset added before the log.
#' @return Numeric vector of length \code{n_param}.
#' @noRd
.mosaic_medoid_distances <- function(cases_array, central, mask_spec, eps = 1.0) {
  d <- dim(cases_array)
  if (length(d) != 4L) stop("cases_array must be [n_loc, n_time, n_param, n_stoch]")
  if (is.null(dim(central))) central <- matrix(central, nrow = 1L)
  if (!identical(as.integer(dim(central)), as.integer(d[1:2])))
    stop("central series dimensions do not match cases_array")
  member_agg <- apply(cases_array, c(1L, 2L, 3L), stats::median)
  dim(member_agg) <- d[1:3]
  target <- .mosaic_mask_central_for_scoring(central, "cases", mask_spec)
  log_target <- log(target + eps)
  vapply(seq_len(d[3]), function(i) {
    mean(abs(log(member_agg[, , i] + eps) - log_target), na.rm = TRUE)
  }, numeric(1L))
}

# =============================================================================
# Per-channel scored-window resolution (burn-in + deaths-era start)
# =============================================================================

#' Resolve the per-channel scored time window from control$likelihood
#'
#' MOSAIC seeds initial conditions at \code{date_start}; the seeded E/I
#' discharge into reported cases over the first ~1-2 weeks, producing a day-1
#' spike the forced steady state can't match. Separately, deaths are often
#' unfittable before a per-country date (non-stationary observed CFR). Both are
#' solved by NOT scoring part of the time axis: a per-channel scored start index
#' (in time-step space, 1-based) computed from run-time-only knobs.
#'
#' \itemize{
#'   \item \code{burn_in_days} -- integer >= 0 leading steps to drop from BOTH
#'     channels (the IC transient). Package default \code{30} (set via
#'     \code{mosaic_control_defaults()}); \code{0} disables slicing.
#'   \item \code{deaths_score_start} -- \code{NULL} (full window) or a
#'     \code{Date}/\code{"YYYY-MM-DD"}; deaths are scored from \code{max(burn_in,
#'     offset_to_this_date)}. Default \code{NULL}.
#'   \item \code{score_start_cases} -- optional explicit cases start
#'     (\code{Date}/\code{"YYYY-MM-DD"}); \code{NULL} => derived from
#'     \code{burn_in_days}. Default \code{NULL}.
#' }
#'
#' The opt-out (\code{burn_in_days = 0}, both starts \code{NULL}) yields
#' \code{idx_cases = idx_deaths = 1L} => NO slicing => scoring is bit-identical
#' to the pre-feature behavior. (As of v0.47.3 the package default is \code{30},
#' so this is the explicit opt-out, not the default.) Indices are clamped to
#' \code{[1L, n_time]}.
#'
#' @param config The simulation config list (\code{date_start}, \code{reported_cases}).
#' @param control The resolved control list (reads \code{control$likelihood}).
#' @return \code{list(idx_cases, idx_deaths, n_time)} with integer indices.
#' @noRd
.mosaic_resolve_score_window <- function(config, control) {
  lik <- control$likelihood
  n_time <- if (is.matrix(config$reported_cases)) ncol(config$reported_cases) else
              length(config$reported_cases)
  n_time <- as.integer(n_time)
  if (is.null(n_time) || is.na(n_time) || n_time < 1L) n_time <- 1L

  # No knobs => no slicing (bit-identical default). NULL likelihood, or all
  # knobs absent/at their defaults, returns idx = 1.
  out <- list(idx_cases = 1L, idx_deaths = 1L, n_time = n_time)
  if (is.null(lik)) return(out)

  date_start <- tryCatch(as.Date(config$date_start), error = function(e) NA)

  # Offset (in steps from date_start, 0-based) of a target date. Daily grid:
  # one step per day. Returns 0L when the date is at/before date_start or
  # unresolvable, so a start at/before the window scores the full window.
  date_offset <- function(d) {
    if (is.null(d) || is.na(date_start)) return(0L)
    dd <- tryCatch(as.Date(d), error = function(e) NA)
    if (is.na(dd)) return(0L)
    off <- as.integer(round(as.numeric(dd - date_start)))
    max(0L, off)
  }

  burn_in <- lik$burn_in_days
  burn_in <- if (is.null(burn_in)) 0L else as.integer(round(as.numeric(burn_in)))
  if (is.na(burn_in) || burn_in < 0L) burn_in <- 0L

  # Cases start: the later of burn_in_days and score_start_cases (a date), so
  # score_start_cases can only move the start later, never into the burn-in
  # head. Index is offset + 1 (1-based scored start). Cases may start later
  # than deaths; the worker then masks the residual cases prefix.
  cases_off <- if (!is.null(lik$score_start_cases)) {
    max(burn_in, date_offset(lik$score_start_cases))
  } else {
    burn_in
  }
  # Deaths start: never before the burn-in head; later if deaths_score_start set.
  deaths_off <- max(burn_in, date_offset(lik$deaths_score_start))

  idx_cases  <- min(max(1L, cases_off  + 1L), n_time)
  idx_deaths <- min(max(1L, deaths_off + 1L), n_time)

  list(idx_cases = idx_cases, idx_deaths = idx_deaths, n_time = n_time)
}


# =============================================================================
# VALIDATION
# =============================================================================

#' Validate and Merge Control Settings
#' @noRd
.mosaic_validate_and_merge_control <- function(control) {
  def <- mosaic_control_defaults()

  # Deep merge user control into defaults
  for (nm in names(control)) {
    if (is.list(control[[nm]]) && nm %in% names(def) && is.list(def[[nm]])) {
      # Nested list: merge recursively
      for (sub_nm in names(control[[nm]])) {
        def[[nm]][[sub_nm]] <- control[[nm]][[sub_nm]]
      }
    } else {
      # Top-level: direct replacement
      def[[nm]] <- control[[nm]]
    }
  }

  # RETIRED (v0.92.0): nb_k_min_cases / nb_k_min_deaths.
  #
  # These were a floor on a marginal method-of-moments dispersion estimate that
  # was mis-specified for a non-stationary series, so the floor bound in 27 of 28
  # estimable locations and was in practice the dispersion parameter itself.
  # Dispersion is now estimated per location by est_nb_dispersion(). A floor
  # would silently override that estimate, which is exactly the failure mode
  # being removed, so the setting is rejected rather than migrated.
  #
  # Detection MUST key off the user's ORIGINAL `control` (lesson #13): the merge
  # above has already populated `def`, so a test against `def` can never fire.
  .retired_nbk <- intersect(c("nb_k_min_cases", "nb_k_min_deaths"),
                            names(control[["likelihood"]]))
  if (length(.retired_nbk)) {
    warning(sprintf(
      paste0("control$likelihood$%s is RETIRED and has been ignored. NB dispersion is now ",
             "estimated per location from the weekly observations by est_nb_dispersion(). ",
             "To set the cases dispersion explicitly, use control$likelihood$nb_k_cases, which ",
             "REPLACES the estimate (scalar or one value per location). nb_k_deaths is not used: ",
             "run_MOSAIC() scores deaths with the reported CFR integrated out (dispersion phi_j)."),
      paste(.retired_nbk, collapse = " and ")), call. = FALSE)
    for (nm in .retired_nbk) def$likelihood[[nm]] <- NULL
  }

  # BACKWARD COMPATIBILITY: renamed control parameters (v0.22.16; fixed v0.37.1).
  #
  # Detection MUST key off the user's ORIGINAL `control`, not the merged `def`.
  # The canonical keys (*_adaptive / *_total / ESS_method) always carry a
  # non-NULL default from mosaic_control_defaults() after the deep-merge above,
  # so the prior guard `is.null(def$calibration$<new>)` never fired -- the legacy
  # value was silently dropped (run reverted to defaults) and the deprecation
  # warning never appeared.
  #
  # The dominant usage is `control <- mosaic_control_defaults(); control$..$old <- x`,
  # so the canonical key is present but holding its DEFAULT. We therefore treat the
  # canonical key as "user-set" only when it differs from the pristine default:
  #   - legacy set, canonical untouched -> honour legacy, deprecation warning
  #   - legacy set, canonical user-set  -> canonical wins, "ignored" warning
  .ctl_defaults <- mosaic_control_defaults()
  .mosaic_migrate_renamed <- function(def, section, old, new) {
    user_old <- control[[section]][[old]]
    if (is.null(user_old)) return(def)
    canon         <- control[[section]][[new]]
    canon_default <- .ctl_defaults[[section]][[new]]
    canon_user_set <- !is.null(canon) && !isTRUE(all.equal(canon, canon_default))
    if (!canon_user_set) {
      def[[section]][[new]] <- user_old
      warning(sprintf("control$%s$%s is deprecated; use control$%s$%s instead.",
                      section, old, section, new), call. = FALSE)
    } else {
      warning(sprintf(paste0("control$%s$%s is deprecated and was ignored because ",
                             "control$%s$%s is also set; use control$%s$%s."),
                      section, old, section, new, section, new), call. = FALSE)
    }
    def[[section]][[old]] <- NULL
    def
  }

  .mosaic_renamed_params <- list(
    c("calibration", "batch_size",            "batch_size_adaptive"),
    c("calibration", "min_batches",           "min_batches_adaptive"),
    c("calibration", "max_predictive_batch",  "max_batch_predictive"),
    c("calibration", "max_simulations",       "max_simulations_total"),
    c("targets",     "ess_method",            "ESS_method"),
    c("targets",     "best_subset_weighting", "posterior_weighting")
  )
  for (.p in .mosaic_renamed_params) {
    def <- .mosaic_migrate_renamed(def, .p[[1]], .p[[2]], .p[[3]])
  }

  # RETIRED (1.3.0): targets$ESS_marginal_method. It chose the grid estimate
  # (KDE or binned) of the per-parameter ESS, which did not measure the
  # posterior and was removed. Detected in the user's ORIGINAL control
  # (lesson #13): the defaults no longer carry the key, so presence means the
  # user set it (directly, or through a control.json from an older run).
  if ("ESS_marginal_method" %in% names(control[["targets"]])) {
    warning(paste0("control$targets$ESS_marginal_method is retired (MOSAIC 1.3.0) and was ",
                   "ignored. The grid-based marginal ESS it selected was removed: the ",
                   "per-parameter ESS is now the function-specific ESS of the posterior ",
                   "weights (calc_model_ess_parameter()), which needs no method."),
            call. = FALSE)
    def$targets$ESS_marginal_method <- NULL
  }

  # RETIRED (1.4.0): the per-parameter-ESS stopping rule and its two-phase
  # batch planner. Auto mode now stops when calc_convergence_status() is GREEN
  # (or at max_simulations_total). None of these keys is in the defaults any
  # more, so presence in the ORIGINAL control means the user set it (lesson #13).
  .retired_14 <- list(
    calibration = c("max_batches_adaptive", "max_batches_predictive", "target_r2_adaptive",
                    "max_batches", "target_r2", "target_r2_ess"),
    targets     = c("ESS_param", "ESS_param_prop", "batch_ratio_max"),
    weights     = "floor",
    io          = "verbose_weights")
  .retired_why <- c(
    calibration = paste0("the adaptive mode runs until the convergence status is green ",
                         "(and min_batches_adaptive / min_draws are met) or max_simulations_total"),
    targets     = paste0("the convergence status (calc_convergence_status()) replaces the ",
                         "per-parameter ESS and seed-batch-ratio gate; the targets are ESS_best ",
                         "and targets$status_thresholds, and the per-parameter ESS is reported only"),
    weights     = "weight_all is the coarsened posterior over all valid draws, which has no floor",
    io          = "it had no effect")
  for (.sec in names(.retired_14)) {
    .hit <- intersect(.retired_14[[.sec]], names(control[[.sec]]))
    if (length(.hit)) {
      warning(sprintf("control$%s$%s %s deprecated (MOSAIC 1.4.0) and ignored: %s.",
                      .sec, paste(.hit, collapse = sprintf(", control$%s$", .sec)),
                      if (length(.hit) > 1L) "are" else "is", .retired_why[[.sec]]),
              call. = FALSE)
      for (.k in .hit) def[[.sec]][[.k]] <- NULL
    }
  }

  # Tier-search settings belong to the legacy "saturated" / "tempered" schemes.
  # Under them ESS_best keeps its legacy meaning (the tier target, 100 unless
  # the user changed it from the 1.4.0 default), and A_best / CVw_best default
  # to their pre-1.4.0 values, so those schemes reproduce their releases.
  # Under "bestfit" / "coarsened" they are ignored, with a warning when the
  # user set one (judged from the ORIGINAL control, lesson #13: a full
  # mosaic_control_defaults() list carries min/max_best_subset at their
  # defaults and must stay silent).
  .legacy_scheme <- isTRUE(def$targets$posterior_weighting %in% c("saturated", "tempered"))
  .user_set <- function(k) {
    v <- control[["targets"]][[k]]
    !is.null(v) && (is.null(.ctl_defaults$targets[[k]]) ||
                    !isTRUE(all.equal(v, .ctl_defaults$targets[[k]])))
  }
  # ESS_best changed meaning in 1.4.0 (D12): under "bestfit" / "coarsened" it
  # is the ensemble ESS target of the convergence status (default 200); in
  # 1.3.x it was the tier-search target (default 100).
  # Lesson #13 (red team round 2, CN1): the dominant usage is
  # `ctrl <- mosaic_control_defaults(); ctrl$targets$... <- x`, so a raw value
  # equal to the 1.4.0 default (200) is the default, not a choice: it counts
  # as unset. Unset, a legacy scheme takes its 1.2.1 tier target 100 (so the
  # 1.2.1 golden reproduces under that usage) and the other schemes 200. A
  # value that differs from 200 is the user's: honoured under every scheme,
  # with a warning unless it is the legacy scheme's own default 100, so a
  # 1.3.x control.json or an older launcher setting 100 cannot halve the
  # stopping target silently.
  .ess_best_raw <- control[["targets"]][["ESS_best"]]
  .ess_best_v14 <- .ctl_defaults$targets$ESS_best
  .ess_best_default <- if (.legacy_scheme) 100 else .ess_best_v14
  .ess_best_set <- is.numeric(.ess_best_raw) && length(.ess_best_raw) == 1L && is.finite(.ess_best_raw) &&
    .ess_best_raw > 0 && !isTRUE(all.equal(.ess_best_raw, .ess_best_v14))
  if (!.ess_best_set && (is.null(.ess_best_raw) || isTRUE(all.equal(.ess_best_raw, .ess_best_v14)))) {
    def$targets$ESS_best <- .ess_best_default
  } else if (.ess_best_set && !isTRUE(all.equal(.ess_best_raw, .ess_best_default))) {
    warning(sprintf(paste0(
      "control$targets$ESS_best = %s is honoured, but its meaning changed in MOSAIC 1.4.0: it is the ",
      "ensemble ESS target of the convergence status%s (default %s under posterior_weighting = \"%s\"; ",
      "auto mode simulates towards ESS_all = ESS_best / best_subset_ratio). In MOSAIC 1.3.x it was the ",
      "tier-search target (default 100). Remove it to use the default."),
      format(.ess_best_raw), if (.legacy_scheme) " and the tier-search target" else "",
      format(.ess_best_default), def$targets$posterior_weighting), call. = FALSE)
  }
  if (.legacy_scheme) {
    def$targets$A_best   <- def$targets$A_best   %||% 0.70
    def$targets$CVw_best <- def$targets$CVw_best %||% 1.0
  } else {
    .tier_set <- Filter(.user_set, c("A_best", "CVw_best", "min_best_subset", "max_best_subset"))
    if (length(.tier_set)) {
      warning(sprintf(paste0(
        "control$targets$%s %s ignored under posterior_weighting = \"%s\": %s belong%s to the ",
        "legacy tier search of \"saturated\" / \"tempered\". The ensemble size is bounded by ",
        "min_ensemble_members and max_ensemble_members."),
        paste(.tier_set, collapse = ", $"), if (length(.tier_set) > 1L) "are" else "is",
        def$targets$posterior_weighting, if (length(.tier_set) > 1L) "they" else "it",
        if (length(.tier_set) > 1L) "" else "s"), call. = FALSE)
    }
    def$targets$A_best <- NULL
    def$targets$CVw_best <- NULL
  }

  # The status bands: defaults merged with the user's overrides, validated
  # (.mosaic_convergence_thresholds() stops on an unknown name or a bad value).
  def$targets$status_thresholds <- tryCatch(
    .mosaic_convergence_thresholds(as.list(def$targets$status_thresholds)),
    error = function(e) stop("control$targets$status_thresholds: ", conditionMessage(e), call. = FALSE))

  .pos_int <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 1 && x == round(x)

  # TYPE VALIDATION
  stopifnot(
    "parallel$enable must be logical" =
      is.logical(def$parallel$enable) && length(def$parallel$enable) == 1,
    "parallel$n_cores must be positive integer" =
      is.numeric(def$parallel$n_cores) && def$parallel$n_cores > 0,
    "parallel$type must be 'PSOCK' or 'FORK'" =
      def$parallel$type %in% c("PSOCK", "FORK"),
    "calibration$max_simulations_total must be positive integer" =
      is.numeric(def$calibration$max_simulations_total) && def$calibration$max_simulations_total > 0,
    "calibration$batch_size_adaptive must be positive integer" =
      is.numeric(def$calibration$batch_size_adaptive) && def$calibration$batch_size_adaptive > 0,
    "calibration$min_batches_adaptive must be positive integer" =
      is.numeric(def$calibration$min_batches_adaptive) && def$calibration$min_batches_adaptive > 0,
    "calibration$min_draws must be a single non-negative whole number" =
      is.numeric(def$calibration$min_draws) && length(def$calibration$min_draws) == 1L &&
      is.finite(def$calibration$min_draws) && def$calibration$min_draws >= 0 &&
      def$calibration$min_draws == round(def$calibration$min_draws),
    "calibration$max_batch_predictive must be a single positive whole number" =
      .pos_int(def$calibration$max_batch_predictive),
    "targets$posterior_weighting must be one of 'bestfit', 'coarsened', 'saturated', 'tempered'" =
      is.character(def$targets$posterior_weighting) &&
      length(def$targets$posterior_weighting) == 1L &&
      def$targets$posterior_weighting %in% c("bestfit", "coarsened", "saturated", "tempered"),
    "targets$best_subset_ratio must be a single number in (0, 1]" =
      is.numeric(def$targets$best_subset_ratio) && length(def$targets$best_subset_ratio) == 1L &&
      is.finite(def$targets$best_subset_ratio) && def$targets$best_subset_ratio > 0 &&
      def$targets$best_subset_ratio <= 1,
    "targets$ESS_best must be a single positive finite number" =
      is.numeric(def$targets$ESS_best) && length(def$targets$ESS_best) == 1L &&
      is.finite(def$targets$ESS_best) && def$targets$ESS_best > 0,
    "targets$min_ensemble_members must be a single positive whole number" =
      .pos_int(def$targets$min_ensemble_members),
    "targets$max_ensemble_members must be a single whole number >= min_ensemble_members" =
      .pos_int(def$targets$max_ensemble_members) &&
      def$targets$max_ensemble_members >= def$targets$min_ensemble_members,
    "targets$min_best_subset must be positive integer >= 10" =
      is.numeric(def$targets$min_best_subset) &&
      def$targets$min_best_subset >= 10,
    "targets$max_best_subset must be >= min_best_subset" =
      is.numeric(def$targets$max_best_subset) &&
      def$targets$max_best_subset >= def$targets$min_best_subset,
    "targets$coarsening_alpha must be a single positive finite number" =
      is.numeric(def$targets$coarsening_alpha) && length(def$targets$coarsening_alpha) == 1L &&
      is.finite(def$targets$coarsening_alpha) && def$targets$coarsening_alpha > 0,
    "targets$A_best must be in [0, 1]" =
      is.null(def$targets$A_best) ||
      (is.numeric(def$targets$A_best) && def$targets$A_best >= 0 && def$targets$A_best <= 1),
    "targets$CVw_best must be positive" =
      is.null(def$targets$CVw_best) || (is.numeric(def$targets$CVw_best) && def$targets$CVw_best > 0)
  )

  # SEMANTIC VALIDATION (reasonable bounds and logical consistency)

  # Check n_iterations is reasonable
  if (!is.null(def$calibration$n_iterations)) {
    if (def$calibration$n_iterations > .MOSAIC_MAX_ITERATIONS) {
      stop("calibration$n_iterations (", def$calibration$n_iterations,
           ") exceeds maximum (", .MOSAIC_MAX_ITERATIONS, ")",
           call. = FALSE)
    }
  }

  # Check batch_size_adaptive is reasonable
  if (def$calibration$batch_size_adaptive < .MOSAIC_MIN_BATCH_SIZE ||
      def$calibration$batch_size_adaptive > .MOSAIC_MAX_BATCH_SIZE) {
    stop("calibration$batch_size_adaptive (", def$calibration$batch_size_adaptive,
         ") must be between ", .MOSAIC_MIN_BATCH_SIZE, " and ",
         .MOSAIC_MAX_BATCH_SIZE, call. = FALSE)
  }

  # Check max_simulations_total is reasonable
  if (def$calibration$max_simulations_total < .MOSAIC_MIN_SIMULATIONS ||
      def$calibration$max_simulations_total > .MOSAIC_MAX_SIMULATIONS) {
    stop("calibration$max_simulations_total (", def$calibration$max_simulations_total,
         ") must be between ", .MOSAIC_MIN_SIMULATIONS, " and ",
         .MOSAIC_MAX_SIMULATIONS, call. = FALSE)
  }

  # min_draws above the budget would make the stopping rule unreachable: the
  # run could only end at max_simulations_total (C6). Clamp with a warning,
  # in adaptive mode only: a fixed budget (n_simulations set) uses neither (PN1).
  if (is.null(def$calibration$n_simulations) &&
      def$calibration$min_draws > def$calibration$max_simulations_total) {
    warning(sprintf(paste0("calibration$min_draws (%s) exceeds calibration$max_simulations_total (%s), so the ",
                           "stopping rule could never be met; min_draws clamped to %s."),
                    format(def$calibration$min_draws, big.mark = ",", scientific = FALSE),
                    format(def$calibration$max_simulations_total, big.mark = ",", scientific = FALSE),
                    format(def$calibration$max_simulations_total, big.mark = ",", scientific = FALSE)),
            call. = FALSE)
    def$calibration$min_draws <- def$calibration$max_simulations_total
  }

  # Check batch_size_adaptive < max_simulations_total
  if (def$calibration$batch_size_adaptive >= def$calibration$max_simulations_total) {
    stop("calibration$batch_size_adaptive (", def$calibration$batch_size_adaptive,
         ") must be less than calibration$max_simulations_total (",
         def$calibration$max_simulations_total, ")", call. = FALSE)
  }

  # The cases scoring rule must be valid before any worker starts: a bad value
  # would otherwise fail inside every likelihood call and surface only as an
  # all-NA calibration.
  cs <- def$likelihood$cases_scoring
  if (!is.null(cs) && !(is.character(cs) && length(cs) == 1L && cs %in% c("daily", "weekly"))) {
    stop("likelihood$cases_scoring must be \"daily\" (default) or \"weekly\", got: ",
         paste(format(cs), collapse = ", "), call. = FALSE)
  }
  # The central method is first resolved after calibration, once the shards are
  # consolidated, where a bad value would end the run with no ensemble and a
  # directory that cannot be resumed. Check it here instead.
  tryCatch(.mosaic_resolve_central_method(def$predictions$central_method),
           error = function(e) stop(
             "control$predictions$central_method is invalid (", conditionMessage(e),
             "). Use \"mean\", \"median\" or a vector named by channel, e.g. ",
             "c(cases = \"median\", deaths = \"mean\") (the default).", call. = FALSE))

  # IO VALIDATION
  def$io <- .mosaic_validate_io(def$io)

  def
}

#' Validate I/O Settings
#'
#' Every shard, combined sample file and resume scan is parquet, so parquet is
#' the only output format. \code{format = "csv"} (accepted before, and then
#' silently written as parquet) is coerced to \code{"parquet"} with a warning.
#'
#' @param io The merged \code{control$io} list.
#' @return \code{io}, with \code{format} coerced to \code{"parquet"} if needed.
#' @noRd
.mosaic_validate_io <- function(io) {
  # Format check
  if (identical(io$format, "csv")) {
    warning("io$format = 'csv' is not supported: calibration output is always parquet ",
            "(shards, samples.parquet and resume all read parquet). Using 'parquet'.",
            call. = FALSE, immediate. = TRUE)
    io$format <- "parquet"
  }
  if (!identical(io$format, "parquet")) {
    stop("io$format must be 'parquet', got: ", paste(io$format, collapse = ", "), call. = FALSE)
  }

  # Compression check
  valid_compressions <- c("none", "uncompressed", "snappy", "gzip", "lz4", "zstd")
  if (!io$compression %in% valid_compressions) {
    stop("io$compression must be one of: ",
         paste(valid_compressions, collapse = ", "),
         "\nGot: ", io$compression, call. = FALSE)
  }

  # Level check for zstd
  if (io$compression == "zstd" && !is.null(io$compression_level)) {
    if (!is.numeric(io$compression_level) ||
        io$compression_level < 1 ||
        io$compression_level > 22) {
      stop("io$compression_level for zstd must be integer 1-22, got: ",
           io$compression_level, call. = FALSE)
    }
  }

  invisible(io)
}

#' Validate Sampling Arguments
#' @noRd
.mosaic_validate_sampling_args <- function(sampling_args) {
  if (!is.list(sampling_args)) {
    stop("sampling_args must be a list", call. = FALSE)
  }

  # Check that all elements are logical
  non_logical <- names(sampling_args)[!sapply(sampling_args, is.logical)]
  if (length(non_logical) > 0) {
    warning("Non-logical values in sampling_args: ",
            paste(non_logical, collapse = ", "),
            call. = FALSE)
  }

  sampling_args
}

#' Validate Config
#' @noRd
.mosaic_validate_config <- function(config, iso_code) {
  if (!is.list(config) || length(config) == 0) {
    stop("Config must be a non-empty list", call. = FALSE)
  }

  # Check required fields
  required_fields <- c("location_name", "reported_cases", "reported_deaths")
  missing <- setdiff(required_fields, names(config))
  if (length(missing) > 0) {
    stop("Config missing required fields: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  # Optionally warn if config locations don't match iso_code
  if (!is.null(config$location_name)) {
    config_locations <- config$location_name
    if (!all(iso_code %in% config_locations)) {
      warning("iso_code contains locations not in config: ",
              paste(setdiff(iso_code, config_locations), collapse = ", "),
              "\nThis may be intentional if using custom config.",
              call. = FALSE)
    }
  }

  invisible(TRUE)
}

#' Validate Priors
#' @noRd
.mosaic_validate_priors <- function(priors, config) {
  if (!is.list(priors) || length(priors) == 0) {
    stop("Priors must be a non-empty list", call. = FALSE)
  }

  # Skip known metadata/structural fields that don't need 'distribution'
  metadata_fields <- c("metadata", "parameters_global", "parameters_location", "mu_jt",
                       "simulation", "reporting", "climate", "vaccination")

  # Check that each prior has distribution field
  for (param_name in names(priors)) {
    # Skip metadata fields and non-list items
    if (!is.list(priors[[param_name]]) || param_name %in% metadata_fields) next

    if (!"distribution" %in% names(priors[[param_name]])) {
      warning("Prior for '", param_name, "' missing 'distribution' field",
              call. = FALSE)
    }
  }

  invisible(TRUE)
}

# =============================================================================
# I/O FUNCTIONS
# =============================================================================

#' Write Parquet with Compression (NFS-Safe)
#'
#' Uses atomic write with rename for filesystem safety.
#' Disk space is checked once at calibration start, not per file.
#'
#' @noRd
.mosaic_write_parquet <- function(df, path, io) {
  # Define write function
  write_func <- function(data, file) {
    if (io$compression %in% c("none", "uncompressed")) {
      arrow::write_parquet(data, file, compression = "uncompressed")
    } else if (is.null(io$compression_level) || io$compression != "zstd") {
      arrow::write_parquet(data, file, compression = io$compression)
    } else {
      arrow::write_parquet(data, file,
        compression = io$compression,
        compression_level = io$compression_level)
    }
  }

  # Use NFS-safe atomic write
  .mosaic_atomic_write(df, path, write_func)
}

#' Resolve Active Subset/Weight Columns for Posterior Construction
#'
#' The parameter posterior (\code{posterior_quantiles.csv},
#' \code{posteriors.json}, \code{parameter_estimates.csv}, the posterior
#' density plots) is computed from exactly the draws the predictions use: the
#' ensemble members (\code{is_best_subset} / \code{weight_best}), or the
#' optimised subset (\code{is_best_subset_opt} / \code{weight_best_opt}) when
#' \code{control$predictions$optimize_subset = TRUE} produced one. This holds
#' under every weighting scheme. Under \code{"bestfit"} (the default since
#' 1.4.0) the members are the best-fit selection with their coarsened weights;
#' under \code{"coarsened"} the systematic resample, weighted by count / K;
#' under the legacy schemes the tier-searched best subset. The all-draws
#' coarsened weights (\code{weight_all}) define the posterior the members are
#' selected from and are not a posterior source.
#'
#' The \code{any(results$is_best_subset_opt)} guard handles the edge case where
#' \code{optimize_subset = TRUE} but the optimizer silently failed (e.g.,
#' \code{stability_flag} + \code{optimal_n == 0}) -- in which case we fall back
#' to the members.
#'
#' @param results data.frame with at least \code{is_best_subset} and
#'   \code{weight_best} columns, optionally \code{is_best_subset_opt} and
#'   \code{weight_best_opt}.
#' @param control MOSAIC control list. A list without
#'   \code{targets$posterior_weighting} (or its pre-1.4.0 name
#'   \code{best_subset_weighting}) is read as \code{"saturated"}.
#' @return Named list (see \code{.mosaic_posterior_cols()}).
#' @noRd
.mosaic_active_subset_cols <- function(results, control) {
  use_opt <- isTRUE(control$predictions$optimize_subset) &&
             "is_best_subset_opt" %in% names(results) &&
             "weight_best_opt"    %in% names(results) &&
             isTRUE(any(as.logical(results$is_best_subset_opt), na.rm = TRUE))
  .mosaic_posterior_cols(control$targets$posterior_weighting %||%
                           control$targets$best_subset_weighting %||% "saturated", use_opt)
}

#' Posterior columns of a run
#'
#' The one mapping from a run to the samples.parquet columns its posterior
#' consumers read, shared by \code{run_MOSAIC()} (through
#' \code{.mosaic_active_subset_cols()}) and \code{render_MOSAIC_figures()}. The
#' columns are the prediction members under every scheme; the scheme only
#' labels \code{source}.
#'
#' @param weighting \code{"bestfit"}, \code{"coarsened"}, \code{"saturated"}
#'   or \code{"tempered"}.
#' @param use_opt Logical; the run has a usable optimised subset.
#' @return \code{list(subset_col, weight_col, sample_col, source)}:
#'   \code{subset_col}/\code{weight_col} drive the weighted parameter posterior
#'   (quantiles, posteriors.json, posterior density plots); \code{sample_col}
#'   selects the posterior draws of sample-based consumers (the same members);
#'   \code{source} is \code{"optimized"}, or \code{"bestfit"},
#'   \code{"resample"} (coarsened) or \code{"tier"} (legacy schemes) for the
#'   unoptimised members.
#' @noRd
.mosaic_posterior_cols <- function(weighting, use_opt = FALSE) {
  use_opt <- isTRUE(use_opt)
  list(
    subset_col = if (use_opt) "is_best_subset_opt" else "is_best_subset",
    weight_col = if (use_opt) "weight_best_opt"    else "weight_best",
    sample_col = if (use_opt) "is_best_subset_opt" else "is_best_subset",
    source     = if (use_opt) "optimized"
                 else if (identical(weighting, "bestfit")) "bestfit"
                 else if (identical(weighting, "coarsened")) "resample" else "tier"
  )
}

#' Significant digits for run-input JSON
#'
#' 17 significant digits round-trip every double exactly. jsonlite's
#' \code{digits = NA} writes 15, which changes values by ~1e-15 relative; that is
#' enough to change a member re-simulated from \code{1_inputs/config.json} and
#' \code{priors.json} (integer rounding and binomial draws amplify it), so a
#' post-hoc reconstruction such as the R_eff re-simulation would not reproduce
#' the calibration members.
#' @noRd
.MOSAIC_JSON_DIGITS <- I(17)

#' Write JSON with Atomic Rename (NFS-Safe)
#' @noRd
.mosaic_write_json <- function(obj, path, io, na = c("string", "null")) {
  na <- match.arg(na)
  # Define write function. digits = I(17) makes jsonlite write NA as the
  # string "NA" by default; na = "null" writes JSON null (control.json).
  write_func <- function(data, file) {
    jsonlite::write_json(data, file, pretty = TRUE, auto_unbox = TRUE,
                         digits = .MOSAIC_JSON_DIGITS, na = na)
  }

  # Use NFS-safe atomic write
  .mosaic_atomic_write(obj, path, write_func)
}

# =============================================================================
# RESULT LOADING AND COMBINING
# =============================================================================

#' Load and Combine Simulation Results
#'
#' Loads individual sim_*.parquet files and combines into single data frame.
#' Two methods available: streaming (default, memory-safe) or rbind (legacy).
#'
#' @param dir_params Directory containing sim_*.parquet files
#' @param method Character. "streaming" (default) uses arrow::open_dataset,
#'   "rbind" loads all files into memory. Streaming is recommended for large runs.
#' @param verbose Logical. Print progress messages
#' @return Data frame with combined simulation results
#' @noRd
.mosaic_load_and_combine_results <- function(dir_params,
                                             method = c("streaming", "rbind"),
                                             chunk_size = 5000L,
                                             verbose = TRUE) {

  method <- match.arg(method)

  # Find simulation files
  files <- list.files(dir_params, pattern = "^sim_.*\\.parquet$", full.names = TRUE)

  if (length(files) == 0) {
    stop("No simulation results to process in: ", dir_params, call. = FALSE)
  }

  if (verbose) {
    total_size_mb <- sum(file.size(files)) / 1024^2
    log_msg("Loading %d simulation files (%.1f MB on disk)", length(files), total_size_mb)
    log_msg("Method: %s", method)
  }

  load_start <- Sys.time()

  # Choose loading strategy
  results <- switch(method,
    streaming = {
      # Chunked loading: process files in batches to avoid OOM with many small parquets.
      # Arrow's open_dataset() %>% collect() materializes everything at once, which causes
      # OOM with 40K+ single-row parquets (10-20KB overhead per file expands to several GB).
      n_files <- length(files)
      if (n_files <= chunk_size) {
        # Small enough to load in one shot.
        #
        # unify_schemas = TRUE is REQUIRED, not an optimisation knob. Without it
        # open_dataset() adopts the schema of the first file it reads and
        # silently DROPS any column a later file adds -- no error, no warning,
        # just missing columns in samples.parquet. The chunked branch below
        # tolerates that case via rbindlist(fill = TRUE); this branch did not,
        # and the difference was invisible because both return a data frame of
        # the right row count.
        #
        # This branch became production's normal path in v0.87.0: batching 100
        # simulations per shard turns a 100,000-simulation run into ~1,000
        # files, well under chunk_size = 5000. Before that flip the same run
        # wrote 100,000 files and took the chunked branch, so the gap never
        # mattered in practice. Flipping the default without fixing this would
        # have routed every production run onto the one path that can lose
        # columns -- and run_MOSAIC.R deletes the shards straight after
        # combining, so the loss would be unrecoverable.
        #
        # Cost is bounded by construction: this branch only runs at <= 5,000
        # files, where the schema scan is a small fraction of the read. (The
        # chunked branch is a different story -- there unify_schemas measured
        # 2.1x slower on dugong, which is why v0.86.0 reverted it there.)
        arrow::open_dataset(dir_params, format = "parquet",
                            unify_schemas = TRUE) %>%
          dplyr::collect() %>%
          as.data.frame()
      } else {
        # Chunked approach: bound the working set by reading `chunk_size` files
        # at a time and rbindlist each chunk.
        #
        # v0.79.0 replaced the per-file read here with
        # arrow::open_dataset(chunk_files, unify_schemas = TRUE) on the strength
        # of a LAPTOP measurement (1.57x faster at 1,352 columns). Measured on
        # dugong -- the hardware production actually runs on -- it is 2.1x
        # SLOWER, so v0.86.0 reverted it. At 20,000 real shards of 1,355 columns:
        #
        #   per-file rbindlist                47.18 ms/shard   ->  78.6 min at 100k
        #   open_dataset(unify_schemas=TRUE) 100.48 ms/shard   -> 167.5 min at 100k
        #
        # The 78.6 min projection agrees with the 82.2 min this path actually
        # took in the 100,000-simulation production run, which is what makes the
        # comparison trustworthy. `unify_schemas = TRUE` is the likely cost: it
        # scans every file's schema footer before reading any data, a second full
        # traversal that a laptop's page cache hides and a VM disk does not. It
        # is also not optional -- without it open_dataset silently DROPS columns
        # a later file adds, which is why the fast path cannot simply skip it.
        #
        # Do not re-introduce this without measuring on the target hardware. The
        # real fix for combine cost is fewer, fatter shards
        # (control$io$shard_batch_size), which takes the same work to 0.82
        # ms/shard -- 57x faster than this path and 122x faster than the
        # open_dataset one.
        n_chunks <- ceiling(n_files / chunk_size)
        if (verbose) log_msg("Loading in %d chunks of up to %d files", n_chunks, chunk_size)
        chunk_list <- vector("list", n_chunks)
        for (ci in seq_len(n_chunks)) {
          idx_start <- (ci - 1L) * chunk_size + 1L
          idx_end <- min(ci * chunk_size, n_files)
          chunk_files <- files[idx_start:idx_end]
          chunk_list[[ci]] <- data.table::rbindlist(
            lapply(chunk_files, arrow::read_parquet),
            fill = TRUE
          )
          if (verbose && ci %% 5 == 0) {
            log_msg("  Loaded chunk %d/%d (%d files)", ci, n_chunks, idx_end)
          }
        }
        as.data.frame(data.table::rbindlist(chunk_list, fill = TRUE))
      }
    },

    rbind = {
      # Legacy approach: load all into memory then combine
      # Faster for small datasets, but can OOM for large runs
      if (verbose && length(files) > 10000) {
        warning("Using rbind method with ", length(files),
                " files may cause memory issues. Consider method='streaming'",
                call. = FALSE, immediate. = TRUE)
      }

      df_list <- lapply(files, arrow::read_parquet)

      # Use data.table if available (much faster than do.call(rbind, ...))
      if (requireNamespace("data.table", quietly = TRUE)) {
        results <- data.table::rbindlist(df_list, fill = TRUE)
        as.data.frame(results)
      } else {
        do.call(rbind, df_list)
      }
    }
  )

  if (verbose) {
    load_time <- difftime(Sys.time(), load_start, units = "secs")
    log_msg("Loaded %d rows \u00D7 %d columns in %.1f seconds",
            nrow(results), ncol(results), as.numeric(load_time))

    # Memory usage info
    size_mb <- as.numeric(object.size(results)) / 1024^2
    log_msg("Results in memory: %.1f MB", size_mb)
  }

  results
}

# =============================================================================
# SIMULATION FUNCTIONS
# =============================================================================

#' Normalize n_sims Argument
#'
#' @note "algo" mode is deprecated alias for "auto"
#' @noRd
.mosaic_normalize_n_sims <- function(n_sims) {
  # Handle NULL as auto mode (for backward compatibility)
  if (is.null(n_sims)) {
    return(list(mode = "auto", fixed_target = NA_integer_))
  }

  if (is.character(n_sims)) {
    v <- tolower(n_sims)
    if (!v %in% c("auto", "algo")) {
      stop("n_sims must be NULL, 'auto', or a positive integer, got: ", n_sims,
           call. = FALSE)
    }
    # Warn about deprecated alias
    if (v == "algo") {
      warning("n_sims='algo' is deprecated, use 'auto' instead", call. = FALSE)
    }
    list(mode = "auto", fixed_target = NA_integer_)
  } else if (is.numeric(n_sims) && length(n_sims) == 1L && is.finite(n_sims) && n_sims > 0) {
    list(mode = "fixed", fixed_target = as.integer(n_sims))
  } else {
    stop("n_sims must be NULL, 'auto', or a positive integer", call. = FALSE)
  }
}

#' Ensure Directory Tree Exists
#' @noRd
.mosaic_ensure_dir_tree <- function(dir_output, clean_output) {
  d <- list(
    root              = dir_output,
    inputs            = file.path(dir_output, "1_inputs"),
    calibration       = file.path(dir_output, "2_calibration"),
    cal_samples       = file.path(dir_output, "2_calibration/samples"),
    cal_best_model    = file.path(dir_output, "2_calibration/best_model"),
    cal_posterior      = file.path(dir_output, "2_calibration/posterior"),
    cal_diag          = file.path(dir_output, "2_calibration/diagnostics"),
    cal_state         = file.path(dir_output, "2_calibration/state"),
    cal_simresults    = NULL,  # Created conditionally when save_simresults = TRUE
    results           = file.path(dir_output, "3_results"),
    res_posterior      = file.path(dir_output, "3_results/posterior"),
    res_predictions    = file.path(dir_output, "3_results/predictions"),
    res_figures        = file.path(dir_output, "3_results/figures"),
    res_fig_diag       = file.path(dir_output, "3_results/figures/diagnostics"),
    res_fig_post       = file.path(dir_output, "3_results/figures/posterior"),
    res_fig_post_detail = file.path(dir_output, "3_results/figures/posterior/detail"),
    res_fig_pred       = file.path(dir_output, "3_results/figures/predictions"),
    res_fig_ppc        = file.path(dir_output, "3_results/figures/ppc"),
    res_fig_spatial    = file.path(dir_output, "3_results/figures/spatial"),
    res_fig_trajectories = file.path(dir_output, "3_results/figures/trajectories")
  )

  if (clean_output && dir.exists(d$root)) {
    message("Cleaning output directory: ", d$root)
    unlink(d$root, recursive = TRUE, force = TRUE)
  }

  invisible(lapply(Filter(Negate(is.null), d), dir.create, recursive = TRUE, showWarnings = FALSE))
  d
}

# =============================================================================
# STATE MANAGEMENT
# =============================================================================

#' Initialize Calibration State
#' @noRd
.mosaic_init_state <- function(control, param_names_est, nspec) {
  list(
    total_sims_run = 0L,
    total_sims_successful = 0L,
    batch_number = 0L,
    batch_success_rates = numeric(),
    batch_sizes_used = integer(),
    phase = if (identical(nspec$mode, "fixed")) "fixed" else "adaptive",
    # One entry per adaptive convergence check (run_state.json
    # convergence_history): batch, n, ess_all, ess_best, mce_p90, khat, slope,
    # status.
    convergence_history = list(),
    param_names_est = param_names_est,
    converged = FALSE,
    mode = nspec$mode,
    fixed_target = nspec$fixed_target,
    # What the auto-mode stopping rule measures (persisted in the resume
    # checkpoint so a resume never mixes two metrics in one history)
    gate_metric = .mosaic_gate_metric_id(control),
    # Internal timestamp for state file provenance
    .created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
}

#' Save Run State as JSON
#'
#' Writes a lightweight JSON representation of the workflow state.
#' The persisted state is a slim subset of the full in-memory state,
#' containing only what is needed for external monitoring.
#' Uses atomic write via tempfile + rename.
#'
#' @noRd
.mosaic_save_state <- function(state, path) {
  hist <- state$convergence_history %||% list()
  last <- if (length(hist)) hist[[length(hist)]] else NULL
  persisted <- list(
    schema_version = 2L,
    status = "running",
    created_at = if (!is.null(state$.created_at)) state$.created_at
                 else format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    mode = state$mode,
    phase = state$phase,
    batch = state$batch_number,
    sims_completed = state$total_sims_successful,
    sims_target = if (identical(state$mode, "fixed")) state$fixed_target else NULL,
    # Adaptive mode: TRUE once the stopping rule is met (green at two
    # consecutive checks, minimum run length met). Fixed mode: null (P13).
    stopping_rule_met = if (identical(state$mode, "fixed")) NULL else isTRUE(state$converged),
    convergence_status = if (!is.null(last)) last$status else NULL,
    gate_metric = state$gate_metric,
    convergence_history = hist
  )

  # Atomic write: tempfile + rename
  tmp <- tempfile(tmpdir = dirname(path), fileext = ".json.tmp")
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(persisted, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null",
                       na = "null", digits = NA)
  file.rename(tmp, path)
}

# =============================================================================
# RESUME SUPPORT
# =============================================================================
#
# Resuming an interrupted calibration treats the per-sim parquet shards in
# 2_calibration/samples/ as the single source of truth. Because each sim's
# parameters are a deterministic function of seed = sim_id (content-addressed
# seeds; see .mosaic_run_simulation_worker), a resumed run must continue from
# max(sim_id on disk) + 1 -- never count + 1 -- or it would regenerate existing
# draws and double-count mass in the post-hoc importance weights.
#
# Decision-relevant state that cannot be recovered from shards alone
# (convergence_history, batch counters, the stopping-rule flag) is persisted each
# batch to an internal RDS checkpoint (resume_checkpoint.rds). This is distinct
# from the slim, human-facing run_state.json monitoring file.

#' Scan Calibration Samples Directory for Resume
#'
#' Inventories the per-sim parquet shards to support resume. Sweeps orphaned
#' atomic-write temp files left by a hard crash mid-rename, validates each shard
#' is readable with at least one row, and quarantines unreadable shards. Returns
#' the sorted valid sim IDs and the high-water-mark.
#'
#' @param dir_cal_samples Path to 2_calibration/samples
#' @param quarantine Logical; move unreadable shards to samples/.quarantine/
#' @return list(ids = sorted integer vector, watermark = integer, n = integer)
#' @noRd
.mosaic_resume_scan <- function(dir_cal_samples, quarantine = TRUE) {
  empty <- list(ids = integer(0), watermark = 0L, n = 0L)
  if (is.null(dir_cal_samples) || !dir.exists(dir_cal_samples)) return(empty)

  # Sweep orphaned atomic-write temp files (.mosaic_tmp_*) from an interrupted
  # rename. They never match the sim_*.parquet glob but are dead weight.
  tmp_files <- list.files(dir_cal_samples, pattern = "^\\.mosaic_tmp_",
                          all.files = TRUE, full.names = TRUE)
  if (length(tmp_files)) {
    unlink(tmp_files, force = TRUE)
    log_msg("[RESUME] Swept %d orphaned temp file(s)", length(tmp_files))
  }

  files <- list.files(dir_cal_samples, pattern = "^sim_.*\\.parquet$",
                      full.names = FALSE)
  if (!length(files)) return(empty)

  # Validate each shard by actually READING its data (not just parquet footer
  # metadata): this decodes the data pages, so a torn/byte-corrupt shard throws,
  # and we additionally require the load-bearing columns (sim, likelihood) to be
  # present with a numeric likelihood. A footer-valid but data-corrupt or
  # schema-wrong shard would otherwise survive the scan and abort/poison the
  # downstream combine.
  #
  # This read is NOT cheap at production scale, whatever an earlier comment here
  # claimed: a one-row shard with 1,352 columns is ~444 KB on disk (~98% parquet
  # column framing), so a resume of a 100,000-simulation run re-reads ~53 GB
  # before doing any work -- the same cost as the combine, paid twice. Shrinking
  # it is pipeline plan item 6b (fewer, fatter shards), not something this
  # function can fix on its own.
  # Ids come from each shard's `sim` COLUMN, not from its filename.
  #
  # The column is what the combine and every downstream consumer already treat
  # as the simulation id, so reading it here removes a second, parallel source
  # of truth that could disagree with it. It also stops the scan caring how many
  # rows a shard holds or how it is named, which is what lets a shard carry more
  # than one simulation (pipeline plan item 6b) without touching this function
  # again.
  #
  # Accumulation is pre-allocated per file rather than grown with c(): at
  # 100,000 shards the append form is quadratic, which is the pattern CLAUDE.md
  # calls out under "Memory issues".
  bad_i  <- logical(length(files))
  id_acc <- vector("list", length(files))
  for (i in seq_along(files)) {
    f <- files[i]
    ids_f <- tryCatch({
      df <- as.data.frame(arrow::read_parquet(file.path(dir_cal_samples, f)))
      if (!all(c("sim", "likelihood") %in% names(df))) NULL
      else if (!is.numeric(df$likelihood)) NULL
      else if (nrow(df) < 1L) NULL
      else {
        sim_ids <- suppressWarnings(as.integer(df$sim))
        sim_ids <- sim_ids[!is.na(sim_ids)]
        if (!length(sim_ids)) NULL else sim_ids
      }
    }, error = function(e) NULL)
    if (is.null(ids_f)) bad_i[i] <- TRUE else id_acc[[i]] <- ids_f
  }
  bad    <- files[bad_i]
  ok_ids <- unlist(id_acc, use.names = FALSE)
  if (is.null(ok_ids)) ok_ids <- integer(0)

  if (length(bad)) {
    if (quarantine) {
      qdir <- file.path(dir_cal_samples, ".quarantine")
      dir.create(qdir, recursive = TRUE, showWarnings = FALSE)
      # Clear any prior-quarantine collisions so the rename can't silently fail.
      targets <- file.path(qdir, bad)
      unlink(targets[file.exists(targets)], force = TRUE)
      sources <- file.path(dir_cal_samples, bad)
      moved   <- file.rename(sources, targets)
      # If a rename failed (e.g. cross-device .quarantine), the bad shard would
      # otherwise remain in the live sim_*.parquet glob and abort the combine.
      # Delete the unreadable source so it can never re-enter the pool.
      if (any(!moved)) unlink(sources[!moved], force = TRUE)
    }
    # Name the affected shards and the consequence: in auto mode these draws are
    # NOT regenerated (the frontier moves past the watermark), so the pool shrinks
    # by this many; in fixed mode the missing ids are re-run to hit the target.
    log_msg("[RESUME] Quarantined %d unreadable/invalid shard(s) to %s (corrupt, or missing sim/likelihood column): %s",
            length(bad),
            if (quarantine) file.path(basename(dir_cal_samples), ".quarantine") else "(not moved)",
            paste(utils::head(bad, 10L), collapse = ", "))
  }

  ok_ids <- sort(unique(ok_ids))
  if (!length(ok_ids)) return(empty)
  list(ids = ok_ids, watermark = max(ok_ids), n = length(ok_ids))
}

#' Save Internal Resume Checkpoint
#'
#' Persists the decision-relevant calibration state to RDS so an interrupted run
#' resumes with bit-identical stop/continue/phase decisions. Internal durability
#' state, distinct from the slim run_state.json monitoring file. Atomic write;
#' failures warn but never abort the run in progress.
#'
#' @noRd
.mosaic_save_checkpoint <- function(state, checkpoint_file) {
  ckpt <- list(
    schema_version      = 2L,
    gate_metric         = state$gate_metric,
    total_sims_run      = state$total_sims_run,
    batch_number        = state$batch_number,
    phase               = state$phase,
    converged           = state$converged,
    convergence_history = state$convergence_history,
    batch_success_rates = state$batch_success_rates,
    batch_sizes_used    = state$batch_sizes_used
  )
  tryCatch(
    .mosaic_atomic_write(ckpt, checkpoint_file, saveRDS),
    error = function(e)
      log_warn("[RESUME] failed to write resume checkpoint: %s", conditionMessage(e))
  )
  invisible(checkpoint_file)
}

#' Load Internal Resume Checkpoint
#' @noRd
.mosaic_load_checkpoint <- function(checkpoint_file) {
  if (is.null(checkpoint_file) || !file.exists(checkpoint_file)) return(NULL)
  tryCatch(readRDS(checkpoint_file), error = function(e) {
    log_warn("[RESUME] resume checkpoint unreadable (%s); reconstructing from shards",
            conditionMessage(e))
    NULL
  })
}

# -----------------------------------------------------------------------------
# Post-calibration resume (MOSAIC 1.5.0)
# -----------------------------------------------------------------------------
# Combining the shards into samples.parquet deletes them, so before 1.5.0 a run
# that died in post-calibration (CFR pilot, ensemble, R_eff inputs, figures)
# had to redo its sampling. The sampling marker closes that: it is written
# atomically right after the combined samples.parquet and before the shards are
# deleted, and records everything post-calibration reads from the sampling
# phase -- the calibration state, the columns the shards carried, the draws'
# ids and likelihoods (to verify samples.parquet against), and the global RNG
# state -- so a resumed post-calibration starts exactly where an uninterrupted
# run would. samples.parquet itself is rewritten during post-calibration with
# the weight and member columns; the resume reads back only the shard columns
# and recomputes the combine-time flags, which are a function of the
# likelihoods alone.

#' Path of the sampling-complete marker
#' @param dirs Directory list from .mosaic_ensure_dir_tree().
#' @noRd
.mosaic_sampling_marker_file <- function(dirs) {
  file.path(dirs$cal_state, "sampling_complete.rds")
}

#' Record that sampling finished and samples.parquet holds every draw
#'
#' @param dirs Directory list.
#' @param results The combined draws, flagged as written to samples.parquet.
#' @param state The calibration state at the end of sampling.
#' @param shard_cols The columns the shards carried (before the combine-time
#'   flags were added).
#' @return Invisibly, the marker path.
#' @noRd
.mosaic_write_sampling_marker <- function(dirs, results, state, shard_cols) {
  state$.gate_pool <- NULL
  marker <- list(
    schema_version = 1L,
    mosaic_version = as.character(utils::packageVersion("MOSAIC")),
    written_at     = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    n_rows         = nrow(results),
    shard_cols     = shard_cols,
    sim            = results$sim,
    likelihood     = results$likelihood,
    state          = state,
    random_seed    = get0(".Random.seed", envir = globalenv(), inherits = FALSE)
  )
  path <- .mosaic_sampling_marker_file(dirs)
  .mosaic_atomic_write(marker, path, saveRDS)
  invisible(path)
}

#' Read the sampling marker of a run whose sampling finished
#'
#' Returns the marker when it is readable, carries the current schema and
#' \code{2_calibration/samples.parquet} holds as many rows as it recorded;
#' otherwise \code{NULL} (with a log line naming the reason when a marker
#' exists), so the caller falls back to the shard-based resume and its guards.
#' @param dirs Directory list.
#' @return The marker list, or \code{NULL}.
#' @noRd
.mosaic_read_sampling_marker <- function(dirs) {
  path <- .mosaic_sampling_marker_file(dirs)
  if (!file.exists(path)) return(NULL)
  reject <- function(why) {
    log_warn("[RESUME] Sampling marker %s ignored: %s", file.path("2_calibration", "state",
                                                                  basename(path)), why)
    NULL
  }
  m <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(m) || !identical(m$schema_version, 1L) || is.null(m$state) ||
      !length(m$shard_cols))
    return(reject("unreadable or written by an incompatible MOSAIC version"))
  samples_file <- file.path(dirs$calibration, "samples.parquet")
  if (!file.exists(samples_file)) return(reject("2_calibration/samples.parquet is missing"))
  n <- tryCatch(arrow::open_dataset(samples_file)$num_rows, error = function(e) NA_integer_)
  if (!isTRUE(n == m$n_rows))
    return(reject(sprintf("samples.parquet has %s rows, the marker recorded %d",
                          format(n), as.integer(m$n_rows))))
  m
}

#' Read the sampled draws back from samples.parquet for a post-calibration resume
#'
#' Selects the columns the shards carried (post-calibration adds weight and
#' member columns to samples.parquet) and verifies that the draws' ids and
#' likelihoods are exactly those the sampling marker recorded.
#' @param samples_file Path to samples.parquet.
#' @param marker Output of .mosaic_read_sampling_marker().
#' @return A data.frame with the shard columns, in the combine's row order.
#' @noRd
.mosaic_load_sampled_results <- function(samples_file, marker) {
  cols <- as.character(marker$shard_cols)
  df <- as.data.frame(arrow::read_parquet(samples_file, col_select = dplyr::all_of(cols)))
  df <- df[cols]
  if (!identical(df$sim, marker$sim) || !identical(df$likelihood, marker$likelihood))
    stop("2_calibration/samples.parquet does not hold the draws its sampling marker recorded ",
         "(sim ids or likelihoods differ), so post-calibration cannot resume from it. Start a ",
         "fresh run (resume = FALSE or a new dir_output).", call. = FALSE)
  df
}

#' Restore the global RNG state recorded by the sampling marker
#'
#' Post-calibration draws on the global stream in places (cluster ports, the
#' HSIC subsample when a renderer runs in-process), so a resumed
#' post-calibration starts from the state an uninterrupted run had at the same
#' point. \code{NULL} (the RNG had not been used) removes \code{.Random.seed}.
#' @param seed The recorded \code{.Random.seed}, or \code{NULL}.
#' @noRd
.mosaic_restore_rng <- function(seed) {
  if (is.null(seed)) {
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      rm(".Random.seed", envir = globalenv())
  } else {
    assign(".Random.seed", seed, envir = globalenv())
  }
  invisible(NULL)
}

#' Key output files of a run (the \code{files} element run_MOSAIC() returns)
#' @param dirs Directory list.
#' @noRd
.mosaic_run_files <- function(dirs) {
  list(
    simulations = file.path(dirs$calibration, "samples.parquet"),
    ess_csv = file.path(dirs$cal_diag, "parameter_ess.csv"),
    posterior_quantiles = file.path(dirs$cal_posterior, "posterior_quantiles.csv"),
    posteriors_json = file.path(dirs$cal_posterior, "posteriors.json")
  )
}

#' run_MOSAIC()'s return value for a resume of a run that already completed
#'
#' Nothing is recomputed: the calibration counts come from the sampling marker
#' and the convergence status from \code{3_results/summary.json}.
#' @param dirs Directory list.
#' @param state The calibration state recorded by the sampling marker.
#' @noRd
.mosaic_completed_run_result <- function(dirs, state) {
  s <- tryCatch(jsonlite::read_json(file.path(dirs$results, "summary.json"),
                                    simplifyVector = TRUE),
                error = function(e) list())
  one <- function(x, na) if (length(x) == 1L) x else na
  list(
    dirs = dirs,
    files = .mosaic_run_files(dirs),
    summary = list(
      batches = state$batch_number,
      sims_total = state$total_sims_run,
      sims_success = state$total_sims_successful,
      convergence_status = one(s$convergence_status, NA_character_),
      stopping_rule_met = one(s$stopping_rule_met, NA),
      converged = .mosaic_run_converged(state),
      convergence_evaluated = .mosaic_convergence_evaluated(state),
      resumed = TRUE,
      sims_reused = state$total_sims_successful,
      runtime_min = 0
    )
  )
}

#' Log run_MOSAIC()'s grep-friendly RUN_SUMMARY end-of-run sentinel
#'
#' A tailing operator (human or AI monitoring the log) can latch on
#' the RUN_SUMMARY tag to know the pipeline finished AND extract headline metrics
#' in one pass. status / mode: see .mosaic_run_status() (fixed mode never
#' applies the stopping rule).
#' @param summary_obj The summary list (.mosaic_write_summary_json(), or
#'   summary.json read back: missing values may then be NULL).
#' @param state The calibration state.
#' @param runtime_min Runtime in minutes.
#' @param dir_output The run directory.
#' @noRd
.mosaic_log_run_summary <- function(summary_obj, state, runtime_min, dir_output) {
  one <- function(x) if (length(x) == 1L) x else NA
  so <- lapply(summary_obj[intersect(c("convergence_status", "stopping_rule_met", "r2_cases_ensemble",
                                       "r2_deaths_ensemble", "n_simulations_total", "sims_used",
                                       "ensemble_members", "ess_best", "ess_all"), names(summary_obj))], one)
  g <- function(k) if (is.null(so[[k]])) NA else so[[k]]
  int_s <- function(x) if (length(x) == 1L && is.finite(x)) sprintf("%d", as.integer(round(x))) else "NA"
  outputs_ok <- !is.na(g("r2_cases_ensemble"))
  status_str <- .mosaic_run_status(state, outputs_ok)
  log_msg(paste0(
    "[RUN_SUMMARY] status=%s mode=%s convergence_status=%s stopping_rule_met=%s ",
    "outputs_ok=%s runtime_min=%.2f r2_cases_ensemble=%s ",
    "r2_deaths_ensemble=%s sims_total=%s sims_used=%s ensemble_members=%s ess_best=%s ",
    "ess_all=%s resumed=%s dir_output=%s"),
    status_str,
    if (is.null(state$mode)) "NA" else state$mode,
    if (is.na(g("convergence_status"))) "NA" else g("convergence_status"),
    if (is.na(g("stopping_rule_met"))) "NA" else if (isTRUE(g("stopping_rule_met"))) "YES" else "NO",
    if (outputs_ok) "YES" else "NO",
    as.numeric(runtime_min),
    if (is.na(g("r2_cases_ensemble")))  "NA" else sprintf("%.4f", g("r2_cases_ensemble")),
    if (is.na(g("r2_deaths_ensemble"))) "NA" else sprintf("%.4f", g("r2_deaths_ensemble")),
    # machine-read sentinel: plain integers, no thousands separators (PN2)
    int_s(g("n_simulations_total")), int_s(g("sims_used")),
    int_s(g("ensemble_members")),
    if (is.finite(g("ess_best"))) sprintf("%.1f", g("ess_best")) else "NA",
    if (is.finite(g("ess_all"))) sprintf("%.1f", g("ess_all")) else "NA",
    if (isTRUE(state$resumed)) "YES" else "NO",
    dir_output
  )
  invisible(status_str)
}

#' Finish the end-of-run bookkeeping of a completed run killed before it
#'
#' run_MOSAIC() writes summary.json, logs the RUN_SUMMARY line and then finalizes
#' run_state.json (status "completed", _README.md). A run killed between the
#' first and the last is complete but looks unfinished; a resume, which is
#' otherwise a no-op on a completed run, finishes it from summary.json.
#' @param dirs Directory list.
#' @param state The calibration state recorded by the sampling marker.
#' @param dir_output The run directory.
#' @return Invisibly, TRUE when it finalized, FALSE when nothing was needed.
#' @noRd
.mosaic_finalize_if_unfinished <- function(dirs, state, dir_output) {
  state_file <- file.path(dirs$cal_state, "run_state.json")
  persisted <- tryCatch(jsonlite::read_json(state_file), error = function(e) NULL)
  if (is.null(persisted) || identical(persisted$status, "completed")) return(invisible(FALSE))
  s <- tryCatch(jsonlite::read_json(file.path(dirs$results, "summary.json"), simplifyVector = TRUE),
                error = function(e) NULL)
  if (is.null(s)) return(invisible(FALSE))
  log_msg("[RESUME] run_state.json was not finalized (the run was interrupted after summary.json): finalizing it")
  state$resumed <- TRUE
  .mosaic_log_run_summary(s, state, 0, dir_output)
  .mosaic_finalize_state(state_file, s)
  invisible(TRUE)
}

#' Add the combine-time index columns to the combined draws
#'
#' \code{is_finite}/\code{is_valid}, the legacy schemes' Tukey lower fence
#' (\code{is_outlier}), \code{is_retained}, and the \code{is_best_subset} /
#' \code{is_best_model} flags, all functions of the likelihood alone. Shared by
#' the combine step and the post-calibration resume, which recomputes them from
#' the shard columns.
#' @param results The combined draws.
#' @param control Control list.
#' @return \code{results} with the index columns.
#' @noRd
.mosaic_flag_results <- function(results, control) {
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
  if (!identical(weighting_scheme, "bestfit") && sum(results$is_valid) > 0) {
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
  results
}

#' Reconstruct Calibration State From Disk for Resume
#'
#' Overlays the fresh-init `state` with state recovered from the shards on disk
#' (and the resume checkpoint if present). Always sets total_sims_run from the
#' disk watermark so the next sim_id never collides with an existing draw.
#'
#' The convergence history must have been measured by the stopping rule this
#' session runs (\code{.mosaic_gate_metric_id()}). A 1.4.0 checkpoint with the
#' same metric is restored as is, and the pool is re-checked when the shards
#' on disk differ from what its last entry saw. A checkpoint from MOSAIC 1.3.x
#' or earlier (a per-parameter-ESS \code{ess_tracking}, no
#' \code{convergence_history}), one under other best-fit settings, or no
#' checkpoint at all has its history re-derived from the shards: the status is
#' recomputed at the earlier batch boundaries (the checkpoint's batch sizes, or
#' multiples of \code{batch_size_adaptive}; at most 40 points) and on the whole
#' pool, so measurements of two metrics are never mixed.
#'
#' @param state Fresh state from .mosaic_init_state()
#' @param dirs Directory list
#' @param control Control list
#' @param param_names_est Estimated-parameter names
#' @param n_obs Scored observation cells (for the stopping rule)
#' @return Reconstructed state (unchanged if no shards exist -> fresh run)
#' @noRd
.mosaic_reconstruct_state <- function(state, dirs, control, param_names_est, n_obs) {
  scan <- .mosaic_resume_scan(dirs$cal_samples)

  if (scan$n == 0L) {
    log_msg("[RESUME] No existing shards in %s \u2014 starting fresh run", dirs$cal_samples)
    return(state)
  }

  # Disk watermark is authoritative for the sim_id frontier (avoids seed reuse).
  state$total_sims_run        <- scan$watermark
  state$total_sims_successful <- scan$n

  if (!identical(state$mode, "auto")) {
    log_msg("[RESUME] Fixed mode: %d shards present (watermark %d)", scan$n, scan$watermark)
    return(state)
  }

  ckpt <- .mosaic_load_checkpoint(file.path(dirs$cal_state, "resume_checkpoint.rds"))
  bs <- as.integer(control$calibration$batch_size_adaptive %||% 500L)
  if (!is.null(ckpt)) {
    state$batch_number        <- ckpt$batch_number        %||% state$batch_number
    state$batch_success_rates <- ckpt$batch_success_rates %||% numeric()
    state$batch_sizes_used    <- ckpt$batch_sizes_used    %||% integer()
  } else {
    # Estimate batches from the shard COUNT (not the max-id watermark, which
    # over-counts when failed sims leave gaps).
    state$batch_number <- max(1L, as.integer(ceiling(scan$n / max(bs, 1L))))
  }
  state$phase <- "adaptive"

  # The gate's in-memory pool, read from the shards once here (the only full
  # read of an adaptive run); run_MOSAIC() takes it over from state$.gate_pool.
  pool <- tryCatch(.mosaic_gate_pool_update(NULL, dirs$cal_samples, param_names_est),
                   error = function(e) NULL)
  results <- pool$data
  if (is.null(results) || !nrow(results)) return(state)
  state$.gate_pool <- pool

  gate_now  <- state$gate_metric %||% .mosaic_gate_metric_id(control)
  gate_ckpt <- if (is.null(ckpt$gate_metric)) NA_character_ else as.character(ckpt$gate_metric)
  hist_ckpt <- ckpt$convergence_history %||% list()
  same <- !is.null(ckpt) && identical(gate_ckpt, gate_now) && length(hist_ckpt) > 0L

  if (same) {
    state$convergence_history <- hist_ckpt
    state$converged <- isTRUE(ckpt$converged)
    last_n <- as.numeric(hist_ckpt[[length(hist_ckpt)]]$n %||% NA)
    n_now <- sum(is.finite(results$likelihood))
    log_msg("[RESUME] Restored checkpoint: %d sims, batch %d, %d convergence check(s), stopping rule met = %s",
            state$total_sims_run, state$batch_number, length(hist_ckpt), isTRUE(state$converged))
    # Re-check when the pool differs from what the last check saw (a partial
    # batch wrote after it, or shards were quarantined on this scan).
    if (!isTRUE(n_now == last_n)) {
      state$converged <- FALSE
      if (isTRUE(n_now > last_n)) {
        # A partial batch wrote shards after the last check: count it as a
        # batch, so its check is not labelled with the previous batch number
        # (two history entries with one batch label, C14).
        state$batch_number <- state$batch_number + 1L
        state$batch_sizes_used <- c(state$batch_sizes_used,
                                    as.integer(max(0, scan$watermark - sum(as.numeric(state$batch_sizes_used)))))
      } else {
        # Fewer draws than the last check saw (shards quarantined): drop the
        # entries that counted them.
        keep <- vapply(state$convergence_history, function(e) isTRUE(as.numeric(e$n %||% NA) <= n_now),
                       logical(1))
        state$convergence_history <- state$convergence_history[keep]
      }
      state <- .mosaic_convergence_check_update_state(state, dirs, param_names_est, control,
                                                      n_obs, results = results)
    }
    return(state)
  }

  log_msg(paste0("[RESUME] %s; the convergence history is re-derived from the %d shards on disk ",
                 "with this session's rule ('%s'), so measurements of two metrics are never mixed"),
          if (is.null(ckpt)) "No checkpoint"
          else if (is.na(gate_ckpt) || !grepl("^convergence_status/", gate_ckpt))
            sprintf("The checkpoint was made by an earlier stopping rule ('%s')",
                    if (is.na(gate_ckpt)) "unstamped, before MOSAIC 1.3.0" else gate_ckpt)
          else sprintf("The checkpoint's rule ('%s') differs", gate_ckpt),
          scan$n, gate_now)
  # Batch boundaries: from the checkpoint's batch sizes, or (no checkpoint) at
  # every batch_size_adaptive-th shard in id order, as many as the batches the
  # shard count implies, so gaps in the ids never invent extra batches.
  ids_sorted <- sort(as.numeric(results$sim[is.finite(results$sim)]))
  cum <- if (length(state$batch_sizes_used)) cumsum(as.numeric(state$batch_sizes_used))
         else if (state$batch_number > 1L && length(ids_sorted))
           ids_sorted[pmin(length(ids_sorted), bs * seq_len(state$batch_number - 1L))]
         else numeric(0)
  bounds <- sort(unique(cum[cum > 0 & cum < scan$watermark]))
  if (length(bounds) > 39L) bounds <- bounds[unique(round(seq(1, length(bounds), length.out = 39L)))]
  # Each re-derived entry is labelled with the batch it closes (its position in
  # the cumulative batch sizes), and the final check with a later number, so a
  # thinned or round-0 history never carries one batch label twice (C14).
  bidx <- match(bounds, cum)
  state$batch_number <- max(as.integer(state$batch_number), if (length(bidx)) max(bidx) + 1L else 1L)
  state$convergence_history <- list()
  state$converged <- FALSE
  for (k in seq_along(bounds)) {
    sub <- results[results$sim <= bounds[k], , drop = FALSE]
    if (sum(is.finite(sub$likelihood)) < 2L) next
    wh <- tryCatch(.mosaic_gate_health(sub, param_names_est, n_obs, control,
                                       history = state$convergence_history,
                                       n_attempted = bounds[k], members = TRUE,
                                       seed = state$resample_seed, n_locations = state$n_locations),
                   error = function(e) NULL)
    if (!is.null(wh))
      state$convergence_history <- c(state$convergence_history,
                                     list(.mosaic_history_entry(wh, bidx[k])))
  }
  .mosaic_convergence_check_update_state(state, dirs, param_names_est, control, n_obs,
                                         results = results)
}

#' Which Transmission Engine Produced a Run Directory?
#'
#' Classifies a persisted MOSAIC version string as having simulated with the
#' Python \code{laser-cholera} engine or the pure-R engine. The cutover is
#' v0.68.0: every earlier version called Python, every later one calls
#' \code{run_simulation()} in R.
#'
#' This replaced a pair of helpers that compared two \emph{laser-cholera}
#' versions across the v0.12 -> v0.13 deaths-likelihood-scale boundary. That
#' comparison stopped meaning anything at the v0.68.0 cutover: its "current"
#' operand was read from \code{importlib.metadata.version("laser-cholera")},
#' i.e. whichever wheel happened to be installed, which no longer describes what
#' simulated anything -- and once v0.69.0 dropped the wheel from
#' \code{environment.yml} it would have been permanently absent, firing the
#' guard's "SKIPPED" warning on every single resume. The hazard it guarded did
#' not go away, though; it got larger. A change of engine is a superset of a
#' change of deaths scale, and the MOSAIC version records it exactly, with no
#' Python needed.
#'
#' Tolerant of suffixes ("0.68.0.9000", "0.67.0-dev"): each dotted component is
#' reduced to its leading run of digits. Returns NA when the version cannot be
#' parsed to at least major.minor.
#'
#' @param v A MOSAIC version string as recorded in \code{1_inputs/environment.json}.
#' @return "python", "R", or NA_character_.
#' @noRd
.mosaic_run_engine <- function(v) {
  if (is.null(v) || length(v) != 1L || is.na(v) || !nzchar(v)) return(NA_character_)
  parts <- strsplit(as.character(v), "\\.")[[1]]
  nums  <- suppressWarnings(as.integer(sub("^([0-9]+).*$", "\\1", parts)))
  if (length(nums) < 2L || is.na(nums[1]) || is.na(nums[2])) return(NA_character_)
  if (nums[1] > 0L || nums[2] >= 68L) "R" else "python"
}


#' R-side Likelihood Implementation Version
#'
#' Identifies the numeric behaviour of the R \code{calc_model_likelihood()}
#' pipeline. Bump this string whenever a change alters the likelihood VALUES it
#' produces (e.g. the v0.22.20-21 N_obs shape-term normalization) so that resume
#' refuses to pool shards scored by an incompatible likelihood implementation.
#' @noRd
#' @note v0.93.0 replaced the `-y*log(1e6)` zero penalty with an eps-floored
#'   density -- a change in likelihood VALUES -- but left this string at
#'   "R/v0.92.0", so a resume could have pooled v0.92 and v0.93 shards. The
#'   per-channel `eps_rel` change re-bumps it and closes that window too.
#'   v0.96.0 scores deaths with the reported CFR integrated out
#'   (\code{calc_log_likelihood_deaths_integrated()}), weekly, instead of the
#'   eps-floored daily negative binomial on one realisation. v0.97.0 changes that
#'   score from a negative binomial to quasi-Poisson with a per-location
#'   dispersion and an additive background, smooths the year deviations, scores
#'   edge weeks and makes the deaths confidence weights mass-preserving. v0.97.2
#'   makes the year deviations yearly levels (blended at each 1 January)
#'   instead of an interpolated curve. v0.98.0 centred each forecast year's
#'   deviation on the latest observed year's; v0.99.0 centres it instead on a
#'   fixed shift that run_MOSAIC() sets after calibration (0 while calibrating).
#'   Either way the values change only where a scored day's blend reaches a
#'   forecast year (data ending within 30 days of a 1 January).
#'   The likelihood review re-bumps it: the integrated deaths score's
#'   per-location offset width now averages over the years each location
#'   observes (not the pooled years), the n_iterations collapse keeps -Inf
#'   replicates, and the cumulative shape term sums over scored cells only.
#'   v0.101.0 adds the weekly cases rule (\code{cases_scoring = "weekly"}; the
#'   default stays the daily cells, which the control comparison on resume
#'   pins) and changes the cases dispersion: estimated from observed weeks only
#'   (config \code{reported_tier}), and a location whose fit collapses, is
#'   clamped at the lower bound or has too few observed weeks takes the
#'   cross-country panel trend. The integrated deaths score's dispersion phi is
#'   likewise estimated from observed weeks only when the config carries
#'   \code{reported_tier} (every scored week when they are too few). Its
#'   development builds stamped "R/v0.101.0+weekly_cases", whose clamped fits
#'   kept the 0.1 bound: the resolved k is not part of control.json, so only
#'   this tag keeps their shards apart. v0.103.0 also censors a cases fit
#'   within its 95% interval of the lower bound (status near_lower_bound,
#'   UGA on config_default v7.0) and re-derives the panel trend on
#'   config_default v7.0 without it; both change the k a location is scored
#'   at, so shards scored by 0.101.0-0.102.0 (v6.1 trend constants) or by the
#'   0.103.0 development builds (v7.0 constants with UGA's fit in, both
#'   stamped "R/v0.101.0+clamped_k_trend") are refused. v1.1.0 re-derives the
#'   panel trend on config_default v7.1 (the refreshed AI-enhanced
#'   surveillance), which changes the k of every location that takes it, so
#'   shards scored with the v7.0 constants are refused.
.mosaic_likelihood_impl_version <- function() "R/v1.1.0+v7.1_k_trend"

#' R Engine Semantics Version
#'
#' Identifies what the transmission engine SIMULATES, as distinct from how the
#' draws are scored (\code{.mosaic_likelihood_impl_version()}). Bump it whenever
#' a change makes \code{run_simulation()} produce different trajectories for the
#' same config and seed, so resume refuses to pool shards simulated on either
#' side of the change. It is persisted in \code{1_inputs/environment.json} as
#' \code{engine_semantics}; a run directory without the field predates the stamp.
#'
#' \code{"R/v0.100.0+review_engine"} covers three changes from the deep review:
#' the seasonal Fourier term is evaluated at the calendar day of year of
#' \code{date_start} (\code{par$season_t0} in \code{sim_params()}) instead of
#' tick 1 = 1 January; the t = 0 split of initial infections into symptomatic
#' and asymptomatic is a binomial draw at the rng-only site
#' \code{infectious/sigma_split_t0}; and the psi_star calibration is applied
#' to pinned (unsampled) psi_star values too.
#' @return A single character string.
#' @noRd
.mosaic_engine_semantics_version <- function() "R/v0.100.0+review_engine"

#' Likelihood-Value Provenance Descriptor
#'
#' Captures WHO computed the likelihood stored in each shard, and the version of
#' that implementation, so resume can refuse to pool incomparable likelihoods.
#' The likelihood is computed in R by \code{calc_model_likelihood()} on the
#' orchestrator, so \code{engine} is always \code{"R"} and the version is the R
#' implementation tag.
#'
#' The \code{engine} field is retained rather than dropped because shards
#' written by the removed Dask backend recorded \code{engine = "python"} with a
#' laser-cholera version as their \code{impl_version}. Resume must still be able
#' to read those and refuse to pool them with R-scored draws -- deleting the
#' field would make an old shard indistinguishable from a current one.
#'
#' The function took an \code{lc_version} argument until v0.69.0. It never read
#' it: C-1 reduced the body to a constant when scoring became R-only, leaving a
#' parameter every caller filled and nothing consumed.
#'
#' @return list(engine, impl_version)
#' @noRd
.mosaic_likelihood_provenance <- function() {
  list(
    engine       = "R",
    impl_version = .mosaic_likelihood_impl_version()
  )
}

#' Stamp Resume Provenance Into the Environment Snapshot
#'
#' Adds the two fields \code{.mosaic_resume_check_inputs()} compares on resume:
#' \code{likelihood_provenance} (who/what scored the shards; the scorer is always
#' R now, but an R likelihood change that alters values is still a reason to
#' refuse pooling) and \code{engine_semantics} (what the engine simulates).
#' @param env_snapshot List from \code{.mosaic_capture_environment()}.
#' @return \code{env_snapshot} with both fields set.
#' @noRd
.mosaic_stamp_resume_provenance <- function(env_snapshot) {
  env_snapshot$likelihood_provenance <- .mosaic_likelihood_provenance()
  env_snapshot$engine_semantics      <- .mosaic_engine_semantics_version()
  env_snapshot
}

#' Verify Resume Inputs Match the Interrupted Run
#'
#' On resume, the incoming config/priors must match those persisted in 1_inputs/.
#' Changing the prior or likelihood target makes existing shards incomparable to
#' new draws, so a mismatch is a hard error. Comparison uses the same serializer
#' that wrote the files (byte-exact for identical inputs); if serialization
#' cannot be performed the check downgrades to a warning rather than blocking.
#' Run directories written before v0.93.1 hold 15-significant-digit JSON, so a
#' persisted file matching the incoming object at either precision passes.
#'
#' Also guards the transmission engine: resuming a run directory created before
#' MOSAIC v0.68.0 is a hard error, because its shards came from the Python
#' laser-cholera engine and pooling them with R-engine draws would produce a
#' posterior from neither simulator. An R-engine run directory must also carry
#' the current \code{engine_semantics} stamp
#' (\code{.mosaic_engine_semantics_version()}); one without it, or with another
#' value, was simulated by an engine that produces different trajectories and is
#' refused. When \code{control} is supplied, the
#' likelihood target (\code{control$likelihood}) is also compared, since it is
#' not part of config.json/priors.json but changing it re-scores draws under a
#' different target.
#'
#' @noRd
.mosaic_resume_check_inputs <- function(dirs, config, priors, control = NULL) {
  # digits = NA (15 significant) by default: the control sub-objects are
  # compared after parsing control.json back, and at 15 digits a pre-v0.93.1
  # file and a current one serialise identically while real changes still
  # differ. config/priors are compared as raw file text, at both precisions.
  serialize_obj <- function(obj, digits = NA) {
    tmp <- tempfile(fileext = ".json")
    on.exit(unlink(tmp), add = TRUE)
    ok <- tryCatch({
      jsonlite::write_json(obj, tmp, pretty = TRUE, auto_unbox = TRUE, digits = digits)
      TRUE
    }, error = function(e) FALSE)
    if (!ok) return(NA_character_)
    paste(readLines(tmp, warn = FALSE), collapse = "\n")
  }
  check_one <- function(obj, file, label) {
    if (!file.exists(file)) return(invisible())
    incoming  <- serialize_obj(obj, .MOSAIC_JSON_DIGITS)
    persisted <- tryCatch(paste(readLines(file, warn = FALSE), collapse = "\n"),
                          error = function(e) NA_character_)
    if (is.na(incoming) || is.na(persisted)) {
      warning(sprintf("resume: could not compare %s against 1_inputs/; skipping integrity check",
                      label), call. = FALSE)
      return(invisible())
    }
    if (!identical(incoming, persisted) &&
        !identical(serialize_obj(obj), persisted)) {
      stop(sprintf(paste0("resume: supplied %s differs from 1_inputs/%s.json. Changing %s ",
                          "alters the sampling/likelihood target, making the existing shards ",
                          "incomparable to new draws. Resume with identical %s, or start a ",
                          "fresh run in a new directory."),
                   label, label, label, label), call. = FALSE)
    }
    invisible()
  }
  check_one(priors, file.path(dirs$inputs, "priors.json"), "priors")
  check_one(config, file.path(dirs$inputs, "config.json"), "config")

  # Several control fields beyond config/priors change the draws or the stored
  # likelihood and so must also match on resume, or the pooled shards become
  # incomparable. control.json nests the full control under $control (plus a
  # per-run timestamp), so compare the relevant sub-objects rather than
  # byte-comparing the whole file:
  #   - control$likelihood        : weights/sigmas -> the scoring target
  #   - control$sampling          : which of the ~301 params are sampled; changing
  #                                 it shifts the RNG stream so sample_parameters(
  #                                 seed = sim_id) yields different draws per id
  #   - calibration$n_iterations  : the stored likelihood is log_mean_exp over
  #                                 n_iterations stochastic replicates (and the
  #                                 per-iteration seed embeds n_iterations), so
  #                                 changing it puts new shards on a different scale
  #   - calibration mode (auto/fixed) : switching discards the adaptive state and
  #                                 mixes bookkeeping
  ctrl_file <- file.path(dirs$inputs, "control.json")
  if (!is.null(control) && file.exists(ctrl_file)) {
    persisted_ctrl <- tryCatch(jsonlite::fromJSON(ctrl_file, simplifyVector = TRUE),
                               error = function(e) NULL)
    pc <- tryCatch(persisted_ctrl$control, error = function(e) NULL)
    if (!is.null(pc)) {
      mismatch <- function(incoming, persisted) {
        a <- serialize_obj(incoming); b <- serialize_obj(persisted)
        !is.na(a) && !is.na(b) && !identical(a, b)
      }
      fail_field <- function(field) {
        stop(sprintf(paste0(
          "resume: supplied %s differs from 1_inputs/control.json. Changing it alters the ",
          "draws or the likelihood scale, making new simulations incomparable to the existing ",
          "shards. Resume with the same %s, or start a fresh run in a new directory."),
          field, field), call. = FALSE)
      }
      if (!is.null(control$likelihood) && !is.null(pc$likelihood) &&
          mismatch(control$likelihood, pc$likelihood)) fail_field("control$likelihood")
      if (!is.null(control$sampling) && !is.null(pc$sampling) &&
          mismatch(control$sampling, pc$sampling)) fail_field("control$sampling")
      in_iter <- control$calibration$n_iterations
      pc_iter <- pc$calibration$n_iterations
      if (!is.null(in_iter) && !is.null(pc_iter) &&
          !isTRUE(all.equal(as.numeric(in_iter), as.numeric(pc_iter))))
        fail_field("control$calibration$n_iterations")
      # Tolerant mode classifier (NULL/empty/character -> auto, positive number
      # -> fixed); robust to how a NULL n_simulations round-trips through JSON,
      # which .mosaic_normalize_n_sims would reject with an error.
      mode_of <- function(n) {
        if (is.null(n) || length(n) == 0L || is.character(n)) return("auto")
        if (is.numeric(n) && length(n) == 1L && is.finite(n) && n > 0) return("fixed")
        "auto"
      }
      in_mode <- mode_of(control$calibration$n_simulations)
      pc_mode <- mode_of(pc$calibration$n_simulations)
      if (!identical(in_mode, pc_mode)) {
        stop(sprintf(paste0(
          "resume: calibration mode changed (persisted '%s' vs supplied '%s'). Switching ",
          "between fixed and adaptive(auto) mode discards the adaptive state and mixes ",
          "incompatible bookkeeping. Resume in the original mode, or start a fresh run."),
          pc_mode, in_mode), call. = FALSE)
      }
    }
  }

  # Transmission-engine check: v0.68.0 replaced the Python laser-cholera engine
  # with the pure-R one. The two agree statistically but not draw-for-draw, so
  # resuming a Python-engine run directory here would pool shards from two
  # different simulators into one posterior -- a posterior from neither.
  #
  # The discriminator is the MOSAIC version persisted in environment.json, not
  # a laser-cholera version: the engine no longer has a version of its own that
  # is separable from the package's. This also removes the last reason for this
  # function to touch Python at all.
  env_file <- file.path(dirs$inputs, "environment.json")
  persisted_env <- NULL
  persisted_mosaic <- NA_character_
  if (file.exists(env_file)) {
    persisted_env <- tryCatch(
      jsonlite::fromJSON(env_file, simplifyVector = TRUE),
      error = function(e) NULL
    )
    persisted_mosaic <- tryCatch(persisted_env$R$MOSAIC, error = function(e) NA_character_)
    if (is.null(persisted_mosaic) || length(persisted_mosaic) != 1L)
      persisted_mosaic <- NA_character_
  }
  persisted_engine <- .mosaic_run_engine(persisted_mosaic)

  if (identical(persisted_engine, "python")) {
    stop(sprintf(paste0(
      "resume: this run directory was created by MOSAIC %s, which simulated with the Python ",
      "laser-cholera engine. MOSAIC %s simulates in R (run_simulation()). The two engines agree ",
      "statistically but not draw-for-draw, so pooling their shards in 2_calibration/samples/ ",
      "would produce a posterior from neither. Start a fresh run in a new directory."),
      persisted_mosaic, as.character(utils::packageVersion("MOSAIC"))), call. = FALSE)
  } else if (is.na(persisted_engine)) {
    # Surface a SKIPPED guard rather than passing silently -- this is the exact
    # condition (no environment.json, or an unparseable version) under which an
    # undetected engine change could mix simulators.
    reason <- if (!file.exists(env_file))
      "no 1_inputs/environment.json in this run directory"
    else
      "no parseable MOSAIC version recorded in 1_inputs/environment.json"
    warning(sprintf(paste0(
      "resume: the transmission-engine guard was SKIPPED (%s). If this run was started before ",
      "MOSAIC v0.68.0 its shards came from the Python engine, and the resumed posterior would ",
      "mix two simulators."), reason), call. = FALSE)
  }

  # Engine-semantics check: within the R engine, a release that changes what
  # run_simulation() produces for a given config and seed makes shards from
  # before and after it incomparable, even though the MOSAIC version test above
  # classifies both as "R". A run directory written without the stamp was
  # simulated before it existed, and therefore before the changes it marks.
  if (identical(persisted_engine, "R")) {
    cur_sem  <- .mosaic_engine_semantics_version()
    pers_sem <- tryCatch(persisted_env$engine_semantics, error = function(e) NULL)
    if (is.null(pers_sem) || length(pers_sem) != 1L || !identical(as.character(pers_sem), cur_sem)) {
      pers_lab <- if (is.null(pers_sem) || length(pers_sem) != 1L)
        "none recorded (the run predates the engine-semantics stamp)" else
        sprintf("'%s'", as.character(pers_sem))
      stop(sprintf(paste0(
        "resume: this run directory was simulated under different engine semantics ",
        "(persisted: %s; current: '%s'). run_simulation() now produces different ",
        "trajectories for the same config and seed (seasonal calendar phase, the t = 0 ",
        "symptomatic split, pinned psi_star), so pooling the existing shards with new draws ",
        "would mix two models. Start a fresh run in a new directory."),
        pers_lab, cur_sem), call. = FALSE)
    }
  }

  # Likelihood-value provenance: refuse to pool shards scored by a different
  # likelihood implementation than the current session would produce. The shard
  # files do not record provenance, so this comparison against the persisted
  # environment.json is the only line of defence. Absent on pre-feature runs
  # (skipped, like above).
  #
  # There used to be an allow-list here permitting a resume across two
  # laser-cholera versions whose on-worker Python likelihood values were
  # verified byte-identical. It went with the Dask backend: scoring is now
  # always R-side, so the condition `engine == "python"` could never be true
  # again and the branch was dead. Any run whose shards were scored in Python
  # now fails this check outright, which is the correct outcome -- those
  # likelihoods cannot be reproduced by the current code path.
  persisted_prov <- tryCatch(persisted_env$likelihood_provenance, error = function(e) NULL)
  if (!is.null(persisted_prov)) {
    cur_prov <- .mosaic_likelihood_provenance()
    a <- serialize_obj(cur_prov); b <- serialize_obj(persisted_prov)
    if (!is.na(a) && !is.na(b) && !identical(a, b)) {
      pers_engine <- persisted_prov$engine %||% "?"
      extra <- if (identical(pers_engine, "python")) paste0(
        " These shards were scored on-worker by the removed Python/Dask backend;",
        " their likelihood values are not reproducible in R.") else ""
      stop(sprintf(paste0(
        "resume: likelihood provenance differs (persisted engine '%s' / impl '%s' vs current ",
        "engine '%s' / impl '%s'). The shards on disk were scored by a different likelihood ",
        "engine or implementation, so pooling them with new draws would mix incomparable ",
        "likelihoods.%s Start a fresh run in a new directory."),
        pers_engine, persisted_prov$impl_version %||% "?",
        cur_prov$engine, cur_prov$impl_version, extra), call. = FALSE)
    }
  }

  invisible(TRUE)
}

#' Identifier of the auto-mode stopping-rule metric
#'
#' Stamped on the resume checkpoint, so a resumed run never mixes measurements
#' of two different gate metrics in one convergence history. It changes
#' whenever what the rule measures changes: the rule itself (the 1.4.0
#' convergence status, evaluated on the best-fit selection), the coarsening
#' alpha that defines the posterior, the best-fit settings and the status
#' bands.
#' @param control Merged control list.
#' @return A single character string.
#' @noRd
.mosaic_gate_metric_id <- function(control) {
  tg <- control$targets
  f <- function(x) format(as.numeric(x %||% NA), digits = 15)
  th <- .mosaic_convergence_thresholds(as.list(tg$status_thresholds %||% list()))
  sprintf("convergence_status/v1.4.0/alpha=%s/ratio=%s/ess_best=%s/members=%s-%s/bands=%s",
          f(tg$coarsening_alpha %||% 10), f(tg$best_subset_ratio), f(tg$ESS_best),
          f(tg$min_ensemble_members), f(tg$max_ensemble_members),
          paste(vapply(th, f, character(1)), collapse = ","))
}

#' Log-log ESS growth slope over the latter part of a run
#'
#' \code{.mosaic_ess_growth_slope()} over the points at or beyond a quarter of
#' the largest draw count (at least three points), so the slope describes the
#' recent growth rather than the first batches. Used by the adaptive gate (on
#' the convergence history) and by the fixed-mode replay (on log-spaced
#' points), so both report the same quantity.
#' @param n Draw counts.
#' @param ess ESS_all at those counts.
#' @return Numeric scalar, or \code{NA_real_} with fewer than three usable points.
#' @noRd
.mosaic_gate_slope <- function(n, ess) {
  n <- suppressWarnings(as.numeric(n)); ess <- suppressWarnings(as.numeric(ess))
  ok <- is.finite(n) & is.finite(ess) & n > 0 & ess > 0
  if (sum(ok) < 3L) return(NA_real_)
  n_last <- max(3L, length(unique(n[ok & n >= max(n[ok]) / 4])))
  .mosaic_ess_growth_slope(n[ok], ess[ok], n_last = n_last)
}

#' Weight health of a pool of draws as the adaptive gate scores it
#'
#' \code{calc_weight_health()} at the run's best-fit settings
#' (\code{control$targets}): the coarsened posterior over every valid draw, the
#' best-fit selection and its Monte Carlo error, and the status. With
#' \code{members = TRUE} (the adaptive gate) the status is then re-derived on
#' the members of the configured \code{posterior_weighting}
#' (\code{.mosaic_scheme_members()}): the best-fit selection under
#' \code{"bestfit"}, the systematic resample under \code{"coarsened"} and the
#' tier-searched subset under \code{"saturated"} / \code{"tempered"} (whose
#' ensemble ESS target is the run's \code{ESS_best}), so the stopping rule is
#' met on the members the ensemble is built from (red team round 1, D11).
#'
#' @param results Results frame of every draw so far (\code{sim},
#'   \code{likelihood}, parameters).
#' @param param_names Sampled parameter names.
#' @param n_obs Scored observation cells.
#' @param control Merged control list.
#' @param history Convergence history so far (list of entries with \code{n}
#'   and \code{ess_all}); the slope is computed over it plus this pool, unless
#'   \code{slope} is supplied.
#' @param slope Optional growth slope (fixed mode: from the replay).
#' @param budget_exhausted,fixed_mode Passed to \code{calc_weight_health()}.
#' @param n_attempted Simulations attempted so far (valid or not); converts
#'   projected valid draws to simulations. \code{NA} = every draw valid.
#' @param members Logical; re-derive the status on the scheme's members.
#' @param seed Resample seed of \code{"coarsened"} (\code{.mosaic_resample_seed()}).
#' @param n_locations Locations in the run (\code{alpha_per_location}).
#' @return The \code{calc_weight_health()} result (from
#'   \code{.mosaic_members_health()} when the scheme's members differ from the
#'   best-fit selection).
#' @noRd
.mosaic_gate_health <- function(results, param_names, n_obs, control, history = list(),
                                slope = NULL, budget_exhausted = FALSE, fixed_mode = FALSE,
                                n_attempted = NA_real_, members = FALSE, seed = NULL,
                                n_locations = NULL) {
  tg <- control$targets
  ll <- results$likelihood
  eta <- .mosaic_coarsened_eta(tg$coarsening_alpha, n_obs)
  if (!is.finite(eta))
    stop("the convergence status needs n_obs (scored observation cells) to define the posterior",
         call. = FALSE)
  if (is.null(slope)) {
    ess_now <- calc_model_ess(.mosaic_tempered_weights(ll, eta), method = "kish")
    hn <- c(vapply(history, function(e) as.numeric(e$n %||% NA), numeric(1)), sum(is.finite(ll)))
    he <- c(vapply(history, function(e) as.numeric(e$ess_all %||% NA), numeric(1)), ess_now)
    slope <- .mosaic_gate_slope(hn, he)
  }
  wh <- calc_weight_health(
    log_lik = ll,
    params = results[, intersect(param_names, names(results)), drop = FALSE],
    n_obs = n_obs, sim = results$sim,
    coarsening_alpha = tg$coarsening_alpha,
    best_subset_ratio = tg$best_subset_ratio,
    ess_best_target = tg$ESS_best,
    min_members = tg$min_ensemble_members,
    max_members = tg$max_ensemble_members,
    thresholds = as.list(tg$status_thresholds %||% list()),
    slope = slope, budget_exhausted = budget_exhausted, fixed_mode = fixed_mode,
    n_attempted = if (length(n_attempted) == 1L && is.finite(n_attempted)) n_attempted else NULL,
    n_locations = n_locations,
    ess_all_target = .mosaic_scheme_ess_all_target(control),
    max_simulations = if (isTRUE(fixed_mode)) NULL else control$calibration$max_simulations_total,
    batch_size_min = control$calibration$batch_size_adaptive %||% 500L,
    batch_size_max = control$calibration$max_batch_predictive %||% 10000L)
  if (!isTRUE(members)) return(wh)
  m <- .mosaic_scheme_members(results, control, n_obs, seed = seed)
  if (is.null(m)) return(wh)
  .mosaic_members_health(wh, results, param_names, m$idx, m$w, n_members = m$n_members,
                         binding = m$binding)
}

#' ESS_all target of the configured posterior_weighting
#'
#' The ESS_all the scheme's ensemble needs for its ESS target (red team round
#' 2, TN3): under "bestfit" \code{ESS_best / best_subset_ratio} (the members
#' hold that share of ESS_all); under "coarsened" \code{ESS_best}, because the
#' resample's ensemble ESS is about ESS_all (K = round(ESS_all) draws), so the
#' gate turns green near ESS_all = ESS_best. The legacy tier schemes keep the
#' best-fit rule (their subset is sized by the tier search, not by ESS_all).
#' Used by the batch projection, its \code{target_met} branch and the status
#' reasons (\code{calc_weight_health(ess_all_target =)}).
#' @param control Merged control list.
#' @return Numeric scalar.
#' @noRd
.mosaic_scheme_ess_all_target <- function(control) {
  tg <- control$targets
  if (identical(tg$posterior_weighting, "coarsened")) as.numeric(tg$ESS_best)
  else as.numeric(tg$ESS_best) / as.numeric(tg$best_subset_ratio)
}

#' The ensemble members of the configured posterior_weighting
#'
#' The member selection \code{run_MOSAIC()} applies after calibration, for the
#' adaptive gate: \code{NULL} under \code{"bestfit"} (the members are
#' \code{calc_weight_health()}'s best-fit selection); under \code{"coarsened"}
#' the systematic resample of \code{.mosaic_coarsened_posterior()} with the
#' run's seed; under \code{"saturated"} / \code{"tempered"} the Tukey lower
#' fence, the tier search (\code{.mosaic_tier_best_subset()}) on the retained
#' draws and the legacy weights (\code{.mosaic_best_subset_weights()}). The
#' same primitives and arguments as the post-calibration block, so the gate
#' and the final status score the same members on the same draws.
#'
#' @param results Results frame (\code{sim}, \code{likelihood}).
#' @param control Merged control list.
#' @param n_obs Scored observation cells.
#' @param seed Resample seed (\code{.mosaic_resample_seed(config)}); 0-based
#'   default when \code{NULL}.
#' @return \code{NULL}, or \code{list(idx, w, n_members, binding)}.
#' @noRd
.mosaic_scheme_members <- function(results, control, n_obs, seed = NULL) {
  tg <- control$targets
  scheme <- tg$posterior_weighting %||% "bestfit"
  if (identical(scheme, "bestfit")) return(NULL)
  if (identical(scheme, "coarsened")) {
    cp <- .mosaic_coarsened_posterior(
      log_lik = results$likelihood, n_obs = n_obs, alpha = tg$coarsening_alpha,
      min_K = tg$min_ensemble_members, max_K = tg$max_ensemble_members,
      seed = seed %||% .mosaic_resample_seed(NULL))
    idx <- which(cp$counts > 0L)
    if (!length(idx)) return(NULL)
    return(list(idx = idx, w = cp$member_weights[idx], n_members = cp$K,
                binding = .mosaic_resample_binding(cp$ess_kish, tg$min_ensemble_members,
                                                   tg$max_ensemble_members)))
  }
  res <- results
  res$is_valid <- is.finite(res$likelihood)
  res$is_outlier <- FALSE
  if (any(res$is_valid)) {
    fence <- .mosaic_lower_fence(res$likelihood[res$is_valid], control$weights$iqr_multiplier %||% 1.5)
    res$is_outlier[res$is_valid] <- res$likelihood[res$is_valid] < fence
  }
  res$is_retained <- res$is_valid & !res$is_outlier
  if (!any(res$is_retained)) return(NULL)
  ts <- .mosaic_tier_best_subset(res, control, log = FALSE)
  idx <- which(res$sim %in% ts$top_subset$sim)
  if (length(idx) < 2L) return(NULL)
  bsw <- .mosaic_best_subset_weights(res$likelihood[idx], scheme)
  list(idx = idx, w = bsw$weights, n_members = length(idx),
       binding = if (identical(ts$tier, "fallback")) "max_members" else "ratio")
}

#' One convergence-history entry (run_state.json \code{convergence_history})
#' @param wh A \code{calc_weight_health()} result.
#' @param batch Batch number.
#' @return \code{list(batch, n, ess_all, ess_best, mce_p90, khat, slope, status,
#'   sampling_green)}; \code{sampling_green} is TRUE when the three sampling
#'   checks (ESS, Monte Carlo error, k-hat) are green, which is what the
#'   stopping rule reads (\code{.mosaic_stop_rule()}).
#' @noRd
.mosaic_history_entry <- function(wh, batch) {
  num <- function(x) { x <- suppressWarnings(as.numeric(x)); if (length(x) == 1L && !is.na(x)) x else NA_real_ }
  list(batch = as.integer(batch), n = as.integer(wh$n_valid),
       ess_all = num(wh$coarsened$ess_kish), ess_best = num(wh$bestfit$ess_best),
       mce_p90 = num(wh$mce$p90), khat = num(wh$coarsened$khat), slope = num(wh$slope),
       status = as.character(wh$status), sampling_green = isTRUE(.mosaic_sampling_green(wh)))
}

#' One run-log line summarising a convergence status
#'
#' Every check on one line, informativeness included, each with its colour.
#' A value that meets its target reads "212 >= 200"; one that does not reads
#' "150 of 200".
#' @param wh A \code{calc_weight_health()} result.
#' @return Character scalar.
#' @noRd
.mosaic_status_line <- function(wh) {
  fmt_n <- function(x) if (length(x) == 1L && is.finite(x)) .mosaic_fmt_count(x) else "NA"
  vs <- function(x, tgt) {
    x <- suppressWarnings(as.numeric(x)); tgt <- suppressWarnings(as.numeric(tgt))
    xs <- if (length(x) == 1L && is.finite(x)) .mosaic_fmt_thr(x, tgt, 0) else "NA"
    if (length(x) == 1L && length(tgt) == 1L && is.finite(x) && is.finite(tgt) && x >= tgt)
      sprintf("%s >= %s", xs, fmt_n(tgt))
    else sprintf("%s of %s", xs, fmt_n(tgt))
  }
  ck <- wh$convergence$checks
  col <- function(id) {
    if (is.null(ck)) return("NA")
    v <- if (is.data.frame(ck)) ck$colour[ck$check == id] else NA_character_
    if (length(v) == 1L && !is.na(v)) toupper(v) else "NA"
  }
  khat <- suppressWarnings(as.numeric(wh$coarsened$khat))
  mce <- suppressWarnings(as.numeric(wh$mce$p90))
  slope <- suppressWarnings(as.numeric(wh$slope))
  flat <- suppressWarnings(as.logical(unlist(wh$convergence$flat %||% wh$flat))[1])
  inform <- if (is.na(flat)) "not assessed" else if (flat) "flat likelihood" else "data inform the posterior"
  sprintf(paste0("%s%s | ensemble ESS %s [%s] (ESS_all %s, %s) | Monte Carlo error %s, 90th ",
                 "percentile [%s] | k-hat %s [%s] | informativeness: %s [%s] | ESS growth slope %s"),
          toupper(wh$status),
          if (isTRUE(.mosaic_sampling_green(wh)) && !identical(wh$status, "green")) " (sampling checks green)" else "", vs(wh$bestfit$ess_best, wh$targets$ess_best_target), col("ess"),
          vs(wh$coarsened$ess_kish, wh$targets$ess_all_target),
          { nm <- suppressWarnings(as.numeric(wh$bestfit$n_members))
            sprintf("%s member%s", fmt_n(nm), if (length(nm) == 1L && isTRUE(nm == 1)) "" else "s") },
          if (length(mce) == 1L && is.finite(mce))
            .mosaic_fmt_thr(mce, c(wh$targets$thresholds$mce_green, wh$targets$thresholds$mce_yellow), 1,
                            scale = 100, suffix = "%")
          else if (length(mce) == 1L && is.infinite(mce)) "not estimable" else "NA", col("mce"),
          if (length(khat) == 1L && is.finite(khat))
            .mosaic_fmt_thr(khat, c(wh$targets$thresholds$khat_green, wh$targets$thresholds$khat_yellow), 2,
                            big.mark = "")
          else .mosaic_weight_health_fields(wh)$khat_status %|NA|% "undefined", col("khat"),
          inform, col("informativeness"),
          if (length(slope) == 1L && is.finite(slope)) sprintf("%.2f", slope) else "NA (fewer than 3 checks)")
}

#' Running in-memory pool of the draws the adaptive gate scores
#'
#' The convergence status needs only each draw's simulation id,
#' log-likelihood and sampled parameters. Rather than re-reading every shard at
#' every check (whose cost grows with the run: about 25 ms per 100-draw shard,
#' so ~40 s per check at 150,000 draws), the pool keeps those columns in memory
#' and reads only the shard files it has not seen. A resume starts it from the
#' shards on disk once (\code{.mosaic_reconstruct_state()}).
#'
#' @param pool \code{NULL} (empty) or a previous result.
#' @param dir_cal_samples The shard directory.
#' @param param_names Sampled parameter names.
#' @return \code{list(data, files)}: \code{data} a data frame with \code{sim},
#'   \code{likelihood} and the parameter columns, ordered by \code{sim};
#'   \code{files} the shard paths already read.
#' @noRd
.mosaic_gate_pool_update <- function(pool, dir_cal_samples, param_names) {
  if (is.null(pool)) pool <- list(data = NULL, files = character(0))
  files <- list.files(dir_cal_samples, pattern = "^sim_.*\\.parquet$", full.names = TRUE)
  new <- setdiff(files, pool$files)
  if (!length(new)) return(pool)
  cols <- c("sim", "likelihood", param_names)
  parts <- lapply(new, function(f) tryCatch(
    as.data.frame(arrow::read_parquet(f, col_select = dplyr::any_of(cols))),
    error = function(e) NULL))
  ok <- !vapply(parts, is.null, logical(1))
  add <- as.data.frame(data.table::rbindlist(parts[ok], fill = TRUE))
  data <- if (is.null(pool$data)) add else
    as.data.frame(data.table::rbindlist(list(pool$data, add), fill = TRUE))
  data <- data[!duplicated(data$sim), , drop = FALSE]
  list(data = data[order(data$sim), , drop = FALSE], files = c(pool$files, new[ok]))
}

#' Convergence check after an adaptive batch, and the stop decision
#'
#' The auto-mode stopping rule. On every draw so far it computes the 1.4.0
#' convergence status on the members of the configured
#' \code{posterior_weighting} (\code{.mosaic_gate_health(members = TRUE)}),
#' appends an entry to \code{state$convergence_history} and sets
#' \code{state$converged} when the three sampling checks (ensemble ESS, Monte
#' Carlo error, k-hat) are green at two consecutive checks and both minimum run
#' lengths are met (\code{min_batches_adaptive} batches and \code{min_draws}
#' valid draws) at the latest (\code{.mosaic_stop_rule()} on the entries'
#' \code{sampling_green}; red team round 1, D5). Data informativeness is
#' reported but never blocks the stop: more simulations cannot make a flat
#' likelihood informative, so such a run stops YELLOW with the stopping rule
#' met. Otherwise the run continues to \code{max_simulations_total}.
#'
#' @param state Calibration state (\code{state$resample_seed} is the
#'   \code{"coarsened"} resample seed).
#' @param dirs Directory list (shards are read from \code{dirs$cal_samples}).
#' @param param_names_est Sampled parameter names.
#' @param control Merged control list.
#' @param n_obs Scored observation cells (\code{.mosaic_n_scored_obs()}).
#' @param results Optional results frame of every draw so far; read from the
#'   shards when \code{NULL}. Lets an offline replay drive the same rule.
#' @param log Logical; write the status line to the run log.
#' @return The updated state; \code{state$last_health} holds the
#'   \code{calc_weight_health()} result of this check.
#' @noRd
.mosaic_convergence_check_update_state <- function(state, dirs, param_names_est, control,
                                                   n_obs, results = NULL, log = TRUE) {
  .log <- if (isTRUE(log)) log_msg else function(...) invisible(NULL)
  if (is.null(results)) {
    files <- list.files(dirs$cal_samples, pattern = "^sim_.*\\.parquet$", full.names = TRUE)
    if (!length(files)) return(state)
    results <- tryCatch(
      .mosaic_load_and_combine_results(
        dir_params = dirs$cal_samples, method = "streaming",
        chunk_size = control$io$load_chunk_size %||% 5000L, verbose = FALSE),
      error = function(e) NULL)
  }
  if (is.null(results) || !nrow(results)) return(state)
  if (sum(is.finite(results$likelihood)) < 2L) {
    .log("Convergence check skipped: fewer than two draws have a finite likelihood")
    return(state)
  }

  cal <- control$calibration
  exhausted <- state$total_sims_run >= cal$max_simulations_total
  wh <- tryCatch(
    .mosaic_gate_health(results, param_names_est, n_obs, control,
                        history = state$convergence_history %||% list(),
                        budget_exhausted = exhausted,
                        n_attempted = as.numeric(state$total_sims_run %||% NA),
                        members = TRUE, seed = state$resample_seed,
                        n_locations = state$n_locations),
    error = function(e) {
      .log("Convergence check failed: %s", conditionMessage(e))
      NULL
    })
  if (is.null(wh)) return(state)

  state$convergence_history <- c(state$convergence_history %||% list(),
                                 list(.mosaic_history_entry(wh, state$batch_number)))
  state$last_health <- wh
  min_batches <- as.integer(cal$min_batches_adaptive %||% 1L)
  min_draws <- as.numeric(cal$min_draws %||% 0)
  min_ok <- state$batch_number >= min_batches && wh$n_valid >= min_draws
  state$converged <- .mosaic_stop_rule(state$convergence_history, min_ok)

  .log("Convergence check (batch %d, %s valid draws): %s", state$batch_number,
       .mosaic_fmt_count(wh$n_valid), .mosaic_status_line(wh))
  # The stopping rule reads the three sampling checks (ESS, Monte Carlo error,
  # k-hat); informativeness is reported but does not block the stop.
  samp_green <- isTRUE(.mosaic_sampling_green(wh))
  if (!identical(wh$status, "green"))
    .log("  Not green: %s", paste(wh$convergence$reasons, collapse = "; "))
  if (!samp_green) {
    invisible(NULL)
  } else if (!min_ok) {
    .log(paste0("  Sampling checks green, but the minimum run length is not met (%d of %d batches, %s of %s ",
                "draws): continuing"),
         state$batch_number, min_batches, .mosaic_fmt_count(wh$n_valid), .mosaic_fmt_count(min_draws))
  } else if (!isTRUE(state$converged)) {
    .log("  Sampling checks green (1 of 2 consecutive checks needed): continuing")
  } else {
    .log("  Stopping rule met: sampling checks green at two consecutive checks (2 of 2), %s valid draws; status %s",
         .mosaic_fmt_count(wh$n_valid), toupper(wh$status))
  }
  if (length(wh$convergence$flags))
    .log("  Flags: %s", paste(wh$convergence$flag_text, collapse = "; "))
  rm(results)
  gc(verbose = FALSE)
  state
}

#' Size the next adaptive batch: the ESS projection, else geometric growth
#'
#' The posterior ESS over all draws (ESS_all) grows in proportion to the draws
#' (a median log-log slope of 1.01 over the 28 national production runs of
#' 2026-10), so the total needed for the target is
#' \eqn{N_{needed} = N \times ESS_{all}^{target} / ESS_{all}}
#' (\code{.mosaic_projected_draws()}). The next batch is
#' \code{ceiling(safety_factor * N_needed) - N}, at least \code{batch_size_min}
#' and at most \code{batch_size_max} and \code{remaining}.
#'
#' The projection is used only while the measured log-log growth slope is at
#' least \code{slope_min} (an unknown slope, fewer than three checks, blocks
#' it). The Pareto k-hat does not gate it (red team round 1, D3): with bounded
#' weights k-hat falls as draws accumulate while ESS_all keeps growing in
#' proportion to N, so the slope is the direct test of whether the projection
#' holds. Otherwise, and when ESS_all already meets its target while another
#' check is not green, the batch grows geometrically: \code{growth * N},
#' clamped to \code{[batch_size_min, batch_size_max]}.
#'
#' Why \code{growth = 0.25}: each check costs a full status evaluation, so a
#' fixed fallback batch makes the number of checks linear in the budget (290 at
#' 150,000 draws in batches of 500). Growing N by a quarter per check makes it
#' logarithmic until the \code{max_batch_predictive} cap binds: 5,000 to
#' 40,000 draws in about 10 checks (log 8 / log 1.25), then batches of 10,000
#' to 150,000 in 11 more, about 21 checks in all. A quarter is also small
#' enough for the stop: the Monte Carlo error falls as 1/sqrt(N), so one
#' growth step moves it by about 11 percent, and a run turning green
#' overshoots its first green N by at most 25 percent.
#'
#' @param n_draws Valid draws so far.
#' @param ess_all Kish ESS of the coarsened weights over those draws.
#' @param ess_all_target Target ESS_all (\code{ESS_best / best_subset_ratio}).
#' @param slope Log-log ESS growth slope over recent checks (\code{NA} = unknown).
#' @param batch_size_min Smallest batch (\code{batch_size_adaptive}).
#' @param batch_size_max Largest batch (\code{max_batch_predictive}).
#' @param remaining Draws left under \code{max_simulations_total}.
#' @param safety_factor Multiplier on the projected total (1.1: a small
#'   overshoot so the status is green at the next check, not just short).
#' @param slope_min Validity bound of the projection (\code{slope_stall}).
#' @param growth Geometric growth fraction of the fallback (0.25).
#' @return \code{list(batch_size, method, projected_total, reason)};
#'   \code{method} is \code{"projection"}, \code{"geometric"} or
#'   \code{"target_met"} (geometric growth while another check is not green).
#' @noRd
.mosaic_projected_batch_size <- function(n_draws, ess_all, ess_all_target, slope = NA_real_,
                                         batch_size_min, batch_size_max = Inf, remaining = Inf,
                                         safety_factor = 1.1, slope_min = 0.8, growth = 0.25) {
  num1 <- function(x, nm) {
    if (is.null(x)) x <- NA_real_
    if (length(x) != 1L || !(is.numeric(x) || (is.logical(x) && is.na(x))))
      stop("batch sizing: ", nm, " must be a single number (or NA).", call. = FALSE)
    as.numeric(x)
  }
  n_draws <- num1(n_draws, "n_draws"); ess_all <- num1(ess_all, "ess_all")
  ess_all_target <- num1(ess_all_target, "ess_all_target")
  slope <- num1(slope, "slope")
  batch_size_min <- num1(batch_size_min, "batch_size_min")
  batch_size_max <- num1(batch_size_max, "batch_size_max")
  remaining <- num1(remaining, "remaining")
  if (!is.finite(batch_size_min) || batch_size_min < 1)
    stop("batch sizing: batch_size_min must be a positive number.", call. = FALSE)
  if (is.na(batch_size_max) || batch_size_max < batch_size_min)
    stop("batch sizing: batch_size_max must be >= batch_size_min.", call. = FALSE)
  if (is.na(remaining)) remaining <- Inf

  out <- function(size, method, projected, reason)
    list(batch_size = as.integer(max(0, min(size, remaining))), method = method,
         projected_total = projected, reason = reason)
  geometric <- function() {
    n <- if (is.finite(n_draws) && n_draws > 0) n_draws else 0
    min(max(ceiling(growth * n), batch_size_min), batch_size_max)
  }

  if (is.finite(ess_all) && is.finite(ess_all_target) && ess_all >= ess_all_target)
    return(out(geometric(), "target_met", NA_real_,
               sprintf("ESS_all %s meets its target %s; another check is not yet green (geometric growth)",
                       .mosaic_fmt_count(ess_all), .mosaic_fmt_count(ess_all_target))))

  blocked <- if (!is.finite(slope)) "growth slope unknown (fewer than three checks)"
             else if (slope < slope_min) sprintf("growth slope %.2f < %g", slope, slope_min)
  projected <- .mosaic_projected_draws(n_draws, ess_all, ess_all_target)
  if (length(blocked) || !is.finite(projected))
    return(out(geometric(), "geometric", projected,
               sprintf("projection not used (%s): geometric growth",
                       if (length(blocked)) blocked
                       else "ESS_all or the draw count is not positive")))

  size <- ceiling(safety_factor * projected) - n_draws
  size <- min(max(size, batch_size_min), batch_size_max)
  out(size, "projection", projected,
      sprintf("projection: about %s valid draws in total for ESS_all %s (now %s at %s valid draws)",
              .mosaic_fmt_count(projected), .mosaic_fmt_count(ess_all_target),
              .mosaic_fmt_count(ess_all), .mosaic_fmt_count(n_draws)))
}

#' Round an adaptive batch up so it fills the cluster
#'
#' A batch of \code{size} simulations is dispatched in shards of
#' \code{.mosaic_resolve_shard_batch()} simulations (at most the configured
#' \code{shard_batch_size}, and small enough to give every worker at least four
#' tasks). When \code{size} is not a multiple of \code{n_workers} x shard, the
#' last wave of tasks leaves workers idle; on 168 workers a 10,000-simulation
#' batch in shards of 14 is 4.3 waves, so the last wave runs on 26 percent of
#' the cluster (P4). The batch is rounded up to the next multiple of
#' \code{n_workers} x shard, and the shard is returned so the dispatcher uses
#' the same one. Never more than \code{remaining}; sequential runs
#' (\code{n_workers <= 1}) are unchanged.
#'
#' @param size Batch size before rounding.
#' @param shard_batch_size \code{control$io$shard_batch_size}.
#' @param n_workers Workers in the cluster (1 when sequential).
#' @param remaining Simulations left under \code{max_simulations_total}.
#' @param batch_size_max Largest batch (\code{max_batch_predictive}): a rounded
#'   batch above it is rounded down to whole waves instead (unless that is
#'   less than one wave, when the unrounded size, capped, is used).
#' @return \code{list(batch_size, shard_batch)}; \code{shard_batch} is
#'   \code{NULL} when the batch was not rounded (the dispatcher resolves it).
#' @noRd
.mosaic_round_batch_to_cluster <- function(size, shard_batch_size, n_workers, remaining = Inf,
                                           batch_size_max = Inf) {
  size <- as.integer(size)
  nw <- suppressWarnings(as.integer(n_workers)[1])
  if (is.na(nw) || nw <= 1L || is.na(size) || size <= 0L) return(list(batch_size = size, shard_batch = NULL))
  shard <- .mosaic_resolve_shard_batch(shard_batch_size, n_sims = size, n_workers = nw)
  unit <- as.numeric(nw) * shard
  rounded <- ceiling(size / unit) * unit
  # Never above max_batch_predictive (CN2): round down to a whole number of
  # waves instead, unless that is less than one wave.
  if (is.finite(batch_size_max) && rounded > batch_size_max) {
    down <- floor(batch_size_max / unit) * unit
    rounded <- if (down >= unit) down else min(size, batch_size_max)
    if (down < unit) return(list(batch_size = as.integer(min(rounded, remaining)), shard_batch = NULL))
  }
  if (is.finite(remaining) && rounded > remaining)
    return(list(batch_size = as.integer(min(size, remaining)), shard_batch = NULL))
  list(batch_size = as.integer(rounded), shard_batch = as.integer(shard))
}

#' Size of the next adaptive batch
#'
#' \code{batch_size_adaptive} until the minimum run length
#' (\code{min_batches_adaptive} batches and \code{min_draws} valid draws) is
#' met; then \code{.mosaic_projected_batch_size()} on the latest convergence
#' check: the projection \eqn{N \times ESS_{all}^{target} / ESS_{all}} while
#' the growth slope is >= 0.8, else geometric growth (a quarter of the draws so
#' far).
#' Always bounded by \code{max_batch_predictive} (projected batches) and the
#' budget left under \code{max_simulations_total}.
#' In parallel the size is then rounded up to fill the cluster
#' (\code{.mosaic_round_batch_to_cluster()}), never beyond the budget left.
#'
#' The projection works in valid draws (finite likelihood); a batch is a
#' number of simulations. When some simulations fail, the projected draws are
#' converted at the run's success rate so the batch reaches the projected
#' number of valid draws.
#'
#' @param state Calibration state.
#' @param control Merged control list.
#' @param n_workers Workers in the cluster (1 when sequential).
#' @return \code{list(batch_size, method, projected_total, reason, shard_batch)}.
#' @noRd
.mosaic_next_batch_size <- function(state, control, n_workers = 1L) {
  cal <- control$calibration
  remaining <- max(0, cal$max_simulations_total - state$total_sims_run)
  floor_bs <- as.integer(cal$batch_size_adaptive)
  hist <- state$convergence_history %||% list()
  last <- if (length(hist)) hist[[length(hist)]] else NULL
  min_ok <- !is.null(last) &&
    state$batch_number >= as.integer(cal$min_batches_adaptive %||% 1L) &&
    isTRUE(as.numeric(last$n) >= as.numeric(cal$min_draws %||% 0))
  out <- if (!min_ok) {
    list(batch_size = as.integer(min(floor_bs, remaining)), method = "minimum_run",
         projected_total = NA_real_,
         reason = "minimum run length not yet met (no stop or projection before it)")
  } else {
    tg <- control$targets
    d <- .mosaic_projected_batch_size(
      n_draws = as.numeric(last$n), ess_all = as.numeric(last$ess_all %||% NA),
      ess_all_target = .mosaic_scheme_ess_all_target(control),
      slope = as.numeric(last$slope %||% NA),
      batch_size_min = floor_bs, batch_size_max = cal$max_batch_predictive,
      remaining = Inf,
      slope_min = as.numeric(tg$status_thresholds$slope_stall %||% 0.8))
    # Valid draws -> simulations at the run's success rate (D7).
    rate <- .mosaic_success_rate(as.numeric(last$n), state$total_sims_run)
    if (is.finite(rate) && rate > 0 && rate < 1)
      d$batch_size <- as.integer(min(ceiling(d$batch_size / rate), max(cal$max_batch_predictive, floor_bs)))
    d$batch_size <- as.integer(min(d$batch_size, remaining))
    d
  }
  rd <- .mosaic_round_batch_to_cluster(out$batch_size, control$io$shard_batch_size, n_workers, remaining,
                                       batch_size_max = max(cal$max_batch_predictive, floor_bs))
  if (!is.null(rd$shard_batch) && rd$batch_size != out$batch_size)
    out$reason <- sprintf("%s; rounded %s from %s to %s to fill %d workers x %d per shard",
                          out$reason, if (rd$batch_size > out$batch_size) "up" else "down",
                          .mosaic_fmt_count(out$batch_size), .mosaic_fmt_count(rd$batch_size),
                          as.integer(n_workers), rd$shard_batch)
  out$batch_size <- rd$batch_size
  out$shard_batch <- rd$shard_batch
  out
}

#' Share of attempted simulations with a finite likelihood
#'
#' Valid draws (finite likelihood, \code{summary.json} \code{sims_used}) over
#' simulations attempted; \code{NA} before any simulation. Converts projected
#' valid draws into simulations (D7).
#' @param n_valid Valid draws.
#' @param n_attempted Simulations attempted.
#' @return Numeric scalar in \code{[0, 1]} or \code{NA}.
#' @noRd
.mosaic_success_rate <- function(n_valid, n_attempted) {
  run <- suppressWarnings(as.numeric(n_attempted %||% NA))
  ok <- suppressWarnings(as.numeric(n_valid %||% NA))
  if (length(run) != 1L || length(ok) != 1L || !is.finite(run) || run <= 0 || !is.finite(ok)) return(NA_real_)
  min(1, max(0, ok / run))
}

#' Convergence status of the ensemble members actually built
#'
#' \code{calc_weight_health()} scores the best-fit selection. When the
#' members are something else (the "coarsened" resample, the legacy tier
#' subset, or the optimised subset), the ensemble ESS, the Monte Carlo error,
#' the member counts and the posterior mass are recomputed on them and the
#' status is re-derived through \code{calc_convergence_status()}, with the
#' all-draws quantities (ESS_all, k-hat, eta*, slope) and flags unchanged.
#'
#' @param wh A \code{calc_weight_health()} result for the same draws.
#' @param results Results frame (\code{sim}, \code{likelihood}, parameters).
#' @param param_names Sampled parameter names.
#' @param idx Row positions of the members.
#' @param w Member weights aligned with \code{idx}.
#' @param n_members Ensemble size reported (\code{K} for the resample, else
#'   \code{length(idx)}).
#' @param binding Which bound set the size: \code{"ratio"},
#'   \code{"min_members"} or \code{"max_members"}.
#' @return \code{wh} with \code{bestfit}, \code{mce}, \code{convergence},
#'   \code{status} and \code{reasons} describing the members.
#' @noRd
.mosaic_members_health <- function(wh, results, param_names, idx, w, n_members = length(idx),
                                   binding = "ratio") {
  idx <- as.integer(idx)
  w <- as.numeric(w); w <- w / sum(w)
  valid <- is.finite(results$likelihood)
  P <- .mosaic_health_param_matrix(results[, intersect(param_names, names(results)), drop = FALSE], valid)
  n_batches <- as.integer(wh$targets$n_batches %||% 10L)
  mce <- if (ncol(P)) .mosaic_member_mce(P, idx, w, results$sim, n_batches)
         else list(per_parameter = numeric(0), median = NA_real_, p90 = NA_real_, max = NA_real_,
                   n_parameters = 0L, n_batches = n_batches, n_batches_used = NA_integer_)
  w_all <- .mosaic_tempered_weights(results$likelihood, wh$coarsened$eta)
  ess_best <- calc_model_ess(w, method = "kish")
  wh$bestfit <- utils::modifyList(wh$bestfit, list(
    n_members = as.integer(n_members), n_unique = length(idx), ess_best = ess_best,
    mass = sum(w_all[idx]), binding = binding, capped = !identical(binding, "ratio"),
    capped_reason = if (identical(binding, "ratio")) NA_character_ else
                      sprintf("ensemble size set by %s", binding),
    ll_cut = min(results$likelihood[idx]), idx = idx, sim = results$sim[idx], weights = w))
  wh$mce <- mce
  # The one status rule, with every all-draws quantity (and the run context)
  # from wh and the member quantities replaced.
  wh$convergence <- .mosaic_health_status(wh, ess_best = ess_best, mce_p90 = mce$p90, size_bound = binding)
  wh$flat <- wh$convergence$flat %||% wh$flat
  wh$status <- wh$convergence$status
  wh$reasons <- paste(wh$convergence$reasons, collapse = "; ")
  wh
}

#' Convergence status of a run too degenerate for calc_weight_health()
#'
#' A run with fewer than two valid draws (or whose status computation failed)
#' still gets a status, so summary.json, convergence_diagnostics.json,
#' calibration_diagnostics.json and the report describe it rather than falling
#' back to "no status" (red team round 1, C2): \code{calc_convergence_status()}
#' with no ensemble ESS, which is RED, plus a leading reason naming the cause.
#' The run-level quantities (alpha, eta, n_obs, the fixed-budget and
#' budget-cap flags) are carried so summary.json still reports them.
#'
#' @param n_valid,n_total Valid draws and draws in samples.parquet.
#' @param n_obs Scored observation cells.
#' @param control Merged control list.
#' @param fixed_mode,budget_exhausted As for \code{calc_weight_health()}.
#' @param n_attempted Simulations attempted.
#' @return A list shaped like a \code{calc_weight_health()} result, with
#'   \code{degenerate = TRUE}.
#' @noRd
.mosaic_degenerate_health <- function(n_valid, n_total, n_obs, control, fixed_mode = FALSE,
                                      budget_exhausted = FALSE, n_attempted = NA_real_) {
  tg <- control$targets
  th <- .mosaic_convergence_thresholds(as.list(tg$status_thresholds %||% list()))
  eta <- .mosaic_coarsened_eta(tg$coarsening_alpha, n_obs)
  ess_all_target <- .mosaic_scheme_ess_all_target(control)
  conv <- calc_convergence_status(
    ess_best = NA_real_, ess_best_target = tg$ESS_best, mce_p90 = NA_real_, khat = NA_real_,
    flat = NA, ess_all = NA_real_, ess_all_target = ess_all_target, n_draws = n_valid,
    budget_exhausted = budget_exhausted, fixed_mode = fixed_mode, thresholds = th,
    n_attempted = if (length(n_attempted) == 1L && is.finite(n_attempted)) n_attempted else n_total)
  # Fewer than two valid draws: calc_convergence_status() itself leads with
  # "the run failed: ..." (n_draws < 2). A status that failed to compute on a
  # larger pool gets its own leading reason here.
  conv$status <- "red"
  # A failed run is not "more simulations needed": a fourth label, for this
  # case only (round 3; the three SCHEMA labels stay for every other run).
  conv$label <- if (n_valid < 2L) "Run failed: no usable draws" else "Not converged: more simulations needed"
  if (n_valid >= 2L)
    conv$reasons <- c("the convergence status could not be computed (see the run log): treat the run as not converged",
                      conv$reasons)
  list(n_draws = n_total, n_valid = n_valid, degenerate = TRUE,
       context = list(budget_exhausted = isTRUE(budget_exhausted), fixed_mode = isTRUE(fixed_mode),
                      n_attempted = n_attempted),
       targets = list(ess_best_target = tg$ESS_best, best_subset_ratio = tg$best_subset_ratio,
                      ess_all_target = ess_all_target,
                      min_members = as.integer(tg$min_ensemble_members),
                      max_members = as.integer(tg$max_ensemble_members), n_batches = 10L,
                      thresholds = th),
       coarsened = list(alpha = tg$coarsening_alpha, n_obs = n_obs, eta = eta, ess_kish = NA_real_,
                        ess_prop = NA_real_, khat = NA_real_, khat_status = NA_character_),
       bestfit = list(n_members = NA_integer_, n_unique = NA_integer_, ess_best = NA_real_,
                      ess_target = tg$ESS_best, mass = NA_real_, binding = NA_character_,
                      idx = integer(0), sim = integer(0), weights = numeric(0)),
       mce = list(per_parameter = numeric(0), median = NA_real_, p90 = NA_real_, max = NA_real_,
                  n_parameters = 0L),
       eta_star = list(eta = NA_real_, n_eff = NA_real_),
       slope = NA_real_, flat = NA, convergence = conv,
       status = "red", reasons = paste(conv$reasons, collapse = "; "))
}

#' Add the subset-optimiser note to a convergence status
#'
#' With \code{optimize_subset = TRUE} the status describes the optimised
#' members, while an adaptive run's stopping rule was met on the members of
#' \code{posterior_weighting}. When the optimised status is not green, a reason
#' line says so, so a run can read "stopping rule met" beside a non-green
#' status without contradicting itself (red team round 1, D11). A green status
#' keeps no reasons (summary.json \code{convergence_reasons} is empty when
#' green).
#'
#' @param wh A \code{calc_weight_health()} result describing the optimised members.
#' @param scheme \code{posterior_weighting}.
#' @param adaptive,stopping_rule_met The run's mode and stop.
#' @return \code{wh} with the note appended to its reasons.
#' @noRd
.mosaic_note_optimised_status <- function(wh, scheme, adaptive, stopping_rule_met) {
  if (is.null(wh$convergence) || identical(wh$status, "green")) return(wh)
  note <- if (isTRUE(adaptive) && isTRUE(stopping_rule_met))
    sprintf(paste0("the stopping rule was met on the \"%s\" ensemble members; the subset optimiser ",
                   "(optimize_subset = TRUE) then changed the ensemble, and this status describes the ",
                   "optimised members"), scheme)
  else sprintf(paste0("this status describes the members chosen by the subset optimiser ",
                      "(optimize_subset = TRUE), not the \"%s\" ensemble members"), scheme)
  wh$convergence$reasons <- c(as.character(unlist(wh$convergence$reasons)), note)
  wh$reasons <- paste(wh$convergence$reasons, collapse = "; ")
  wh
}

#' Write a convergence status to the run log
#' @param wh A \code{calc_weight_health()} result (possibly from
#'   \code{.mosaic_members_health()}).
#' @param fixed Logical; a fixed-budget run.
#' @return Invisible NULL.
#' @noRd
.mosaic_log_status <- function(wh, fixed = FALSE) {
  log_msg("Convergence status: %s%s", .mosaic_status_line(wh),
          if (isTRUE(fixed)) " [fixed budget: assessed, not used to stop]" else "")
  log_msg("  %s", wh$convergence$label)
  if (length(wh$convergence$reasons))
    log_msg("  Reasons: %s", paste(wh$convergence$reasons, collapse = "; "))
  flags <- setdiff(as.character(unlist(wh$convergence$flags)), "fixed_budget")
  if (length(flags))
    log_msg("  Flags: %s", paste(wh$convergence$flag_text[wh$convergence$flags %in% flags],
                                 collapse = "; "))
  invisible(NULL)
}

#' Log-spaced replay of the posterior ESS and best fit over draws in sim order
#'
#' Recomputes, on at most \code{n_points} log-spaced prefixes of the valid
#' draws taken in simulation order, the Kish ESS of the coarsened weights
#' (ESS_all), the gap of the best log-likelihood found so far to the final
#' best, and the gap of the 100th best. Cheap: one weight vector per point.
#' Gives fixed-budget runs their ESS growth slope and every run the
#' \code{ess_path} / \code{loglik_path} of calibration_diagnostics.json.
#' @param log_lik Per-draw log-likelihoods.
#' @param sim Simulation ids (draw order).
#' @param eta Learning rate of the coarsened posterior.
#' @param n_points Maximum number of points. Default 40.
#' @return \code{data.frame(n, ess_all, best_gap, top100_gap)} (zero rows when
#'   fewer than two valid draws).
#' @noRd
.mosaic_ess_replay <- function(log_lik, sim, eta, n_points = 40L) {
  ok <- is.finite(log_lik)
  ll <- as.numeric(log_lik[ok])[order(sim[ok])]
  N <- length(ll)
  empty <- data.frame(n = integer(0), ess_all = numeric(0), best_gap = numeric(0),
                      top100_gap = numeric(0))
  if (N < 2L || !is.finite(eta) || eta <= 0) return(empty)
  lo <- max(2, min(100, N), N / 400)
  ns <- unique(as.integer(round(exp(seq(log(lo), log(N), length.out = max(2L, n_points))))))
  ns <- ns[ns >= 2L & ns <= N]
  llmax <- max(ll)
  rows <- lapply(ns, function(n) {
    l <- ll[seq_len(n)]
    data.frame(n = n,
               ess_all = calc_model_ess(.mosaic_tempered_weights(l, eta), method = "kish"),
               best_gap = max(l) - llmax,
               top100_gap = if (n >= 100L) sort(l, decreasing = TRUE)[100L] - llmax else NA_real_)
  })
  do.call(rbind, rows)
}

# =============================================================================
# WEIGHT CALCULATIONS
# =============================================================================

#' Tukey lower fence of the log-likelihoods
#'
#' Only the lower fence is applied. Log-likelihoods have a long left tail (many
#' poor fits) but are bounded above near 0; an upper fence Q3 + k * IQR would
#' discard the highest-likelihood simulations, exactly the models the posterior
#' wants. Draws below the fence are flagged \code{is_outlier} and leave the
#' retained set the legacy tier search works on.
#' @param valid_ll Finite log-likelihoods.
#' @param iqr_mult \code{control$weights$iqr_multiplier}.
#' @return The fence, \code{Q1 - iqr_mult * IQR}.
#' @noRd
.mosaic_lower_fence <- function(valid_ll, iqr_mult) {
  q1 <- stats::quantile(valid_ll, 0.25, na.rm = TRUE)
  q3 <- stats::quantile(valid_ll, 0.75, na.rm = TRUE)
  q1 - iqr_mult * (q3 - q1)
}

#' Tier-searched best subset of the legacy weighting schemes
#'
#' The best-subset selection of \code{posterior_weighting = "saturated"} /
#' \code{"tempered"}: tries the tiers of \code{get_default_subset_tiers()} in
#' order with \code{grid_search_best_subset()} on the retained draws and stops at
#' the first that converges; when none does, falls back to the top
#' \code{max_best_subset} draws by likelihood. \code{run_MOSAIC()} calls it once
#' after calibration (logging each tier) and the auto-mode stopping rule calls it
#' silently, so both see the same subset. Not used under \code{"coarsened"}.
#'
#' @param results Results frame with \code{likelihood}, \code{sim} and
#'   \code{is_retained}.
#' @param control Merged control list.
#' @param log Logical; log the tier search.
#' @return \code{list(top_subset, n, percentile, tier, optimal_subset_result)};
#'   \code{tier} is \code{"fallback"} when no tier converged.
#' @noRd
.mosaic_tier_best_subset <- function(results, control, log = TRUE) {
  .log  <- if (isTRUE(log)) log_msg  else function(...) invisible(NULL)
  .warn <- if (isTRUE(log)) log_warn else function(...) invisible(NULL)
  scheme <- control$targets$posterior_weighting %||% "saturated"

  .log("Optimizing subset selection (testing up to 30 tiers with grid search)...")

  subset_tiers <- get_default_subset_tiers(
    target_ESS_best = control$targets$ESS_best,
    target_A = control$targets$A_best,
    target_CVw = control$targets$CVw_best
  )

  optimal_subset_result <- NULL
  tier_used <- NULL
  retained <- results[results$is_retained, ]

  for (tier_name in names(subset_tiers)) {
    tier <- subset_tiers[[tier_name]]

    .log("  Testing tier '%s' (ESS_B=%.0f, A=%.2f, CVw=%.2f)...",
         tier$name, tier$ESS_B, tier$A, tier$CVw)

    tier_result <- grid_search_best_subset(
      results = retained,
      target_ESS = tier$ESS_B,
      target_A = tier$A,
      target_CVw = tier$CVw,
      min_size = control$targets$min_best_subset,
      max_size = control$targets$max_best_subset,
      ess_method = control$targets$ESS_method,
      # Search under the scheme weight_best and the final gate use, so a tier
      # is only certified at a size where the posterior's own weights meet it.
      weighting = scheme,
      verbose = control$logging$verbose
    )

    if (tier_result$converged) {
      # CRITICAL: Calculate percentile from absolute count
      tier_percentile <- (tier_result$n / nrow(results)) * 100
      .log("    \u2713 Tier '%s' converged at n=%d (%.1f%% of all draws)",
           tier$name, tier_result$n, tier_percentile)
      optimal_subset_result <- tier_result
      tier_used <- tier$name
      break
    } else {
      .log("    \u2717 Tier '%s' failed to converge", tier$name)
    }
  }

  if (!is.null(optimal_subset_result)) {
    # CRITICAL: Use $n (not $n_selected) and calculate percentile from count
    top_subset <- optimal_subset_result$subset
    n_top <- optimal_subset_result$n
    tier_used_out <- tier_used
  } else {
    # Fallback: use max_best_subset directly (from retained only)
    n_top <- min(control$targets$max_best_subset, nrow(retained))
    top_subset <- retained[order(retained$likelihood, decreasing = TRUE), ][seq_len(n_top), ]
    tier_used_out <- "fallback"

    .log("  All tiers failed - using fallback (top %d simulations, %.1f%%)",
         n_top, (n_top / nrow(results)) * 100)
    if (!identical(scheme, "saturated")) {
      .warn(paste0("Best-subset tier search ran under posterior_weighting = '%s'; ",
                   "every tier failed, so the posterior is the top %d draws by ",
                   "likelihood with posthoc_criteria_met = NO. '%s' weights are ",
                   "sharper than 'saturated' and may not meet any tier at this ",
                   "run size."),
            scheme, n_top, scheme)
    }

    # Create minimal optimal_subset_result for consistency
    optimal_subset_result <- list(
      subset = top_subset,
      n = n_top,
      converged = FALSE,
      metrics = list(ESS = NA_real_, A = NA_real_, CVw = NA_real_)
    )
  }

  list(top_subset = top_subset, n = n_top,
       percentile = (n_top / nrow(results)) * 100,
       tier = tier_used_out, optimal_subset_result = optimal_subset_result)
}

#' Best-subset posterior weights under control$targets$posterior_weighting
#'
#' The best-subset weights used by the convergence gate (ESS_B, A, CVw) and by
#' \code{results$weight_best} (which drives the posterior quantiles,
#' posteriors.json, the ensemble parameter weights and the optimizer), so the
#' gated weights are the weights the posterior uses. The subset-size tier
#' search (\code{grid_search_best_subset()}) and the subset optimizer
#' (\code{optimize_ensemble_subset()}) call it with the same scheme, so the
#' subset size is chosen, and the displayed ensemble weighted, with the
#' posterior's weights.
#' \code{"saturated"}: \eqn{w \propto \exp(-0.5 \min(\Delta AIC, 4))};
#' \code{"tempered"}: adaptive-eta Gibbs weights
#' (\code{.mosaic_calc_adaptive_gibbs_weights()}).
#'
#' @param likelihood Numeric log-likelihoods of the best-subset members.
#' @param scheme \code{"saturated"} or \code{"tempered"}.
#' @param verbose Passed to the weight function.
#' @return \code{list(weights, temperature, effective_range)}; \code{weights}
#'   is normalised and aligned with \code{likelihood}.
#' @noRd
.mosaic_best_subset_weights <- function(likelihood, scheme = "saturated", verbose = FALSE) {
  if (length(scheme) != 1L || !scheme %in% c("saturated", "tempered")) {
    stop("control$targets$posterior_weighting must be 'saturated' or 'tempered'; got '",
         paste(scheme, collapse = ", "), "'.", call. = FALSE)
  }
  if (identical(scheme, "tempered")) {
    # Adaptive-eta Gibbs weights: eta is chosen so the worst retained draw
    # sits at `weight_floor`, rather than saturating delta at a fixed 4.
    # NOTE this is SHARPER than the saturated default, not softer -- see the
    # warning on control$targets$posterior_weighting.
    ad <- .mosaic_calc_adaptive_gibbs_weights(likelihood = likelihood, verbose = verbose)
    return(list(weights = ad$weights, temperature = ad$temperature,
                effective_range = ad$effective_range))
  }
  # Truncate to effective range. NOTE: this SATURATES delta at 4 rather than
  # applying the Delta <= 6 cut-off; every draw past 4 receives the same weight
  # exp(-2), so weight ratios are capped at exp(2) = 7.39 and the subset
  # posterior is close to uniform regardless of fit.
  aic <- -2 * likelihood
  delta <- aic - min(aic[is.finite(aic)])
  w <- calc_model_weights_gibbs(x = pmin(delta, 4.0), eta = 0.5, verbose = verbose)
  list(weights = w, temperature = 0.5, effective_range = 4.0)
}

#' Calculate Adaptive Gibbs Weights
#'
#' Unified adaptive weight calculation using Gibbs tempering with automatic
#' effective range tuning to prevent numerical underflow.
#'
#' @param likelihood Numeric vector of log-likelihood values
#' @param weight_floor Minimum weight for any model (default: 1e-15)
#'   Prevents numerical underflow by ensuring worst model gets at least this weight.
#'   Research-backed value: 10x above machine epsilon, safe up to DeltaAIC = 69.
#' @param verbose Logical, print diagnostics
#'
#' @return List containing:
#'   - weights: Normalized weight vector (sum = 1)
#'   - temperature: Gibbs temperature used
#'   - effective_range: Effective AIC range used for temperature calculation
#'   - metrics: List with ESS_kish, ESS_perplexity, actual_range, n_valid
#'
#' @details
#' Algorithm:
#' 1. Convert likelihood to AIC: aic = -2 * likelihood
#' 2. Calculate delta_aic from best model
#' 3. Calculate adaptive effective range to ensure w_min >= weight_floor
#' 4. Calculate inverse temperature: eta = actual_range / effective_range
#' 5. Compute Gibbs weights via calc_model_weights_gibbs
#'
#' The adaptive method automatically adjusts to prevent numerical underflow while
#' maintaining discrimination among models. No manual tuning required.
#'
#' Edge cases:
#' - All likelihoods identical: Returns uniform weights
#' - Actual range < 1e-6: Returns uniform weights
#' - Too few valid samples (< 2): Returns uniform weights
#'
#' @noRd
.mosaic_calc_adaptive_gibbs_weights <- function(
  likelihood,
  weight_floor = 1e-15,
  verbose = FALSE
) {

  # ===========================================================================
  # Input validation
  # ===========================================================================

  if (!is.numeric(likelihood) || length(likelihood) == 0) {
    stop(".mosaic_calc_adaptive_gibbs_weights: likelihood must be non-empty numeric vector",
         call. = FALSE)
  }

  if (weight_floor <= 0 || weight_floor >= 1) {
    stop(".mosaic_calc_adaptive_gibbs_weights: weight_floor must be in (0, 1)",
         call. = FALSE)
  }

  n_total <- length(likelihood)

  # ===========================================================================
  # Filter to valid likelihoods
  # ===========================================================================

  valid_idx <- is.finite(likelihood) & !is.na(likelihood)
  n_valid <- sum(valid_idx)

  if (n_valid == 0) {
    warning(".mosaic_calc_adaptive_gibbs_weights: No valid likelihoods, returning uniform weights",
            call. = FALSE)
    return(list(
      weights = rep(1.0 / n_total, n_total),
      temperature = NA_real_,
      effective_range = NA_real_,
      metrics = list(
        ESS_kish = n_total,
        ESS_perplexity = n_total,
        actual_range = NA_real_,
        n_valid = 0
      )
    ))
  }

  if (n_valid < 2) {
    warning(".mosaic_calc_adaptive_gibbs_weights: Only ", n_valid,
            " valid likelihood(s), returning uniform weights", call. = FALSE)
    return(list(
      weights = rep(1.0 / n_total, n_total),
      temperature = NA_real_,
      effective_range = NA_real_,
      metrics = list(
        ESS_kish = n_total,
        ESS_perplexity = n_total,
        actual_range = NA_real_,
        n_valid = n_valid
      )
    ))
  }

  # ===========================================================================
  # Calculate AIC and delta AIC
  # ===========================================================================

  aic <- numeric(n_total)
  aic[valid_idx] <- -2 * likelihood[valid_idx]
  aic[!valid_idx] <- Inf

  best_aic <- min(aic[valid_idx])
  delta_aic <- aic - best_aic

  # ===========================================================================
  # Calculate AIC range
  # ===========================================================================

  actual_range <- diff(range(delta_aic[valid_idx], na.rm = TRUE))

  # ===========================================================================
  # Check for uniform case
  # ===========================================================================

  if (actual_range < 1e-6) {
    # All models essentially identical
    if (verbose) {
      message("  All models have nearly identical likelihood (range < 1e-6)")
      message("  Returning uniform weights")
    }
    return(list(
      weights = rep(1.0 / n_total, n_total),
      temperature = 1.0,  # Arbitrary, not used
      effective_range = 0.0,
      metrics = list(
        ESS_kish = n_total,
        ESS_perplexity = n_total,
        actual_range = actual_range,
        n_valid = n_valid
      )
    ))
  }

  # ===========================================================================
  # Calculate adaptive effective range
  # ===========================================================================

  # Find worst delta_aic across all valid models
  max_delta_aic <- max(delta_aic[valid_idx], na.rm = TRUE)

  # Calculate effective_range needed to keep worst weight >= floor
  # From: exp(-max_delta_aic / (2*temp)) >= floor
  # Solve for effective_range:
  #   temp = 0.5 * (effective_range / actual_range)
  #   exp(-max_delta_aic / (2 * 0.5 * (effective_range / actual_range))) >= floor
  #   exp(-max_delta_aic * actual_range / effective_range) >= floor
  #   -max_delta_aic * actual_range / effective_range >= log(floor)
  #   effective_range >= -max_delta_aic * actual_range / log(floor)
  effective_range <- actual_range * max_delta_aic / (-log(weight_floor))

  # ===========================================================================
  # Calculate inverse temperature (eta) and weights
  # ===========================================================================

  # calc_model_weights_gibbs uses inverse temperature (eta) in: w prop.to exp(-eta * x)
  # We want: exp(-max_delta_aic * eta) >= floor
  # Therefore: eta = -log(floor) / max_delta_aic
  #
  # Alternatively, from temp = 0.5 * (effective_range / actual_range):
  # eta = 1/(2*temp) = actual_range / effective_range
  eta <- actual_range / effective_range

  # For diagnostics, also calculate equivalent "temperature" in textbook sense
  # (not used in weight calculation, just for reporting)
  temperature <- 0.5 * (effective_range / actual_range)

  # Calculate Gibbs weights using inverse temperature -- valid models only.
  # delta_aic[!valid_idx] = Inf; passing the full vector to calc_model_weights_gibbs
  # would crash because that function stop()s on any non-finite input. Compute
  # weights on the valid subset, then place them back into a full-length vector
  # (invalid models receive weight 0 and are silently dropped by calc_model_ess).
  weights_valid <- calc_model_weights_gibbs(
    x = delta_aic[valid_idx],
    eta = eta,
    verbose = FALSE
  )
  weights <- numeric(n_total)
  weights[valid_idx] <- weights_valid

  # ===========================================================================
  # Calculate ESS metrics
  # ===========================================================================

  ESS_kish <- calc_model_ess(weights, method = "kish")
  ESS_perplexity <- calc_model_ess(weights, method = "perplexity")

  # ===========================================================================
  # Verbose diagnostics
  # ===========================================================================

  if (verbose) {
    message("Adaptive Gibbs Weight Calculation:")
    message("  Total models: ", n_total)
    message("  Valid models: ", n_valid)
    message("  Actual \u0394AIC range: ", sprintf("%.2f", actual_range))
    message("  Max \u0394AIC: ", sprintf("%.2f", max_delta_aic))
    message("  Weight floor: ", sprintf("%.2e", weight_floor))
    message("  Adaptive effective range: ", sprintf("%.2f", effective_range))
    message("  Temperature: ", sprintf("%.4f", temperature))
    message("  ESS (Kish): ", sprintf("%.1f", ESS_kish))
    message("  ESS (Perplexity): ", sprintf("%.1f", ESS_perplexity))
  }

  # ===========================================================================
  # Return results
  # ===========================================================================

  list(
    weights = weights,
    temperature = temperature,
    effective_range = effective_range,
    metrics = list(
      ESS_kish = ESS_kish,
      ESS_perplexity = ESS_perplexity,
      actual_range = actual_range,
      n_valid = n_valid
    )
  )
}

# =============================================================================
# EXECUTION
# =============================================================================

#' Map the final ensemble members to their trajectory scratch keys
#'
#' Trajectory channels are spilled to scratch during the CANDIDATE ensemble run,
#' keyed by candidate member index. When the displayed ensemble is the
#' candidate, member i is key i. When it is the optimized rebuild (re-sorted by
#' likelihood and subset), members are mapped by seed. If that map cannot be
#' built (seeds missing, duplicated or unmatched), positional keys would bind
#' the channel panels to the wrong members, so NULL is returned and the caller
#' skips the artifact.
#'
#' @param is_optimized Logical; whether \code{ensemble} is the optimized rebuild.
#' @param final_seeds Per-member seeds of the final ensemble.
#' @param cand_member_seeds Per-member seeds of the candidate ensemble.
#' @param n_param Number of members in the final ensemble.
#' @return Integer vector of scratch keys (length \code{n_param}), or NULL.
#' @noRd
.mosaic_trajectory_member_map <- function(is_optimized, final_seeds, cand_member_seeds,
                                          n_param) {
  if (!isTRUE(is_optimized)) return(seq_len(n_param))
  if (is.null(final_seeds) || is.null(cand_member_seeds) ||
      length(final_seeds) != n_param || anyNA(cand_member_seeds) ||
      anyDuplicated(cand_member_seeds)) return(NULL)
  idx <- match(final_seeds, cand_member_seeds)
  if (anyNA(idx)) return(NULL)
  idx
}

#' Parallelism for the post-calibration ensembles
#'
#' A caller-supplied cluster is consent to parallelise the ensembles and sets
#' their size, but it is not stopped (the caller owns it), so its workers keep
#' their connection slots and memory while \code{calc_model_ensemble()} starts
#' its own cluster. The size is clamped to the free R connections here, with
#' the same rule \code{calc_model_ensemble()} applies internally, only so the
#' note can state the size the ensembles will actually get; the note warns
#' that both clusters are resident. Reusing the caller's cluster would need a
#' \code{cluster} argument on the exported \code{calc_model_ensemble()} and
#' is not done.
#'
#' @param cluster The \code{cluster} argument of \code{run_MOSAIC()} (or NULL).
#' @param control The resolved control list.
#' @return \code{list(parallel, n_cores, note)}; \code{note} is NULL unless a
#'   caller-supplied cluster is present.
#' @noRd
.mosaic_ensemble_parallel_plan <- function(cluster, control) {
  parallel <- isTRUE(control$parallel$enable) || !is.null(cluster)
  n_cores  <- if (!is.null(cluster) && length(cluster) > 1L) length(cluster)
              else control$parallel$n_cores
  note <- NULL
  if (!is.null(cluster) && parallel && isTRUE(n_cores > 1L)) {
    n_req   <- as.integer(n_cores)
    n_cores <- .mosaic_clamp_psock_workers(n_req, reserve = 2L, verbose = FALSE)
    note <- sprintf(paste0(
      "The caller-supplied cluster (%d workers) stays alive during the post-calibration ",
      "ensembles, which start their own %d-worker cluster%s, so host memory holds both. ",
      "Pass no cluster to let run_MOSAIC() stop its own before the ensembles."),
      length(cluster), n_cores,
      if (n_cores < n_req) sprintf(" (reduced from %d by the R connection budget)", n_req) else "")
  }
  list(parallel = parallel, n_cores = n_cores, note = note)
}

#' Build the function that installs the calibration worker on each PSOCK node
#'
#' Returns a function for \code{parallel::clusterCall()} that assigns
#' \code{.run_sim_worker} and \code{.run_sim_worker_chunk} into each worker's
#' global environment. Its enclosing environment is \code{globalenv()}: a
#' closure created inside \code{run_MOSAIC()} would carry (and serialise to
#' every worker) the whole run frame, ~10 MB per worker at 40 locations, and the
#' installed workers would read that copy instead of the values sent by
#' \code{clusterExport()} (\code{n_iterations}, \code{priors}, \code{config},
#' \code{PATHS}, \code{dirs}, \code{param_names_all}, \code{param_lookup},
#' \code{sampling_args}, \code{io_settings}, \code{likelihood_settings}).
#'
#' @return A zero-argument function with \code{globalenv()} as its environment.
#' @noRd
.mosaic_worker_installer <- function() {
  f <- function() {
    assign(".run_sim_worker", function(sim_id) {
      MOSAIC:::.mosaic_run_simulation_worker(
        sim_id = sim_id,
        n_iterations = n_iterations,
        priors = priors,
        config = config,
        PATHS = PATHS,
        dir_cal_samples = dirs$cal_samples,
        dir_cal_simresults = dirs$cal_simresults,
        param_names_all = param_names_all,
        param_lookup = param_lookup,
        sampling_args = sampling_args,
        io = io_settings,
        likelihood_settings = likelihood_settings
      )
    }, envir = .GlobalEnv)
    assign(".run_sim_worker_chunk", function(sim_ids) {
      MOSAIC:::.mosaic_run_simulation_chunk(
        sim_ids = sim_ids,
        n_iterations = n_iterations,
        priors = priors,
        config = config,
        PATHS = PATHS,
        dir_cal_samples = dirs$cal_samples,
        dir_cal_simresults = dirs$cal_simresults,
        param_names_all = param_names_all,
        param_lookup = param_lookup,
        sampling_args = sampling_args,
        io = io_settings,
        likelihood_settings = likelihood_settings
      )
    }, envir = .GlobalEnv)
    NULL
  }
  environment(f) <- globalenv()
  f
}

#' Resolve control$io$shard_batch_size to a usable positive integer
#'
#' Garbage (NULL, NA, a string, a vector, a negative) falls back to 1, i.e. the
#' historical one-file-per-simulation behaviour, rather than propagating into
#' the dispatch. A silently-wrong batch size would change how many simulations a
#' crash costs, so it degrades to the safe value instead of guessing.
#'
#' @param x The raw control value.
#' @return A single positive integer.
#' @noRd
.mosaic_resolve_shard_batch <- function(x, n_sims = NA_integer_,
                                       n_workers = NA_integer_,
                                       tasks_per_worker = 4L) {
  if (is.null(x)) return(1L)
  n <- suppressWarnings(as.integer(x)[1])
  if (is.na(n) || n < 1L) return(1L)

  # Cap so the batch never starves the cluster. A fixed batch is a fixed number
  # of TASKS, and at small budgets that can be fewer tasks than workers: 500
  # simulations at 100 per shard is 5 tasks, so 5 of 24 workers do anything.
  # Keep at least `tasks_per_worker` tasks each so the load balancer has
  # something to balance.
  #
  # At production scale this never binds: 100,000 simulations across 80 workers
  # allows 312 per shard, well above the default of 100.
  ns <- suppressWarnings(as.integer(n_sims)[1])
  nw <- suppressWarnings(as.integer(n_workers)[1])
  if (!is.na(ns) && !is.na(nw) && ns > 0L && nw > 0L) {
    cap <- as.integer(max(1L, floor(ns / (nw * max(1L, tasks_per_worker)))))
    n <- min(n, cap)
  }
  as.integer(max(1L, n))
}

#' Split simulation ids into contiguous chunks
#'
#' Contiguous on purpose: the shard is named for the \code{min}-\code{max} range
#' it covers, which is only informative if the ids in it are consecutive.
#'
#' @param sim_ids Integer vector of ids, assumed sorted.
#' @param size Chunk size (>= 1).
#' @return A list of integer vectors, each of length \code{size} except possibly
#'   the last.
#' @noRd
.mosaic_chunk_ids <- function(sim_ids, size) {
  sim_ids <- as.integer(sim_ids)
  if (!length(sim_ids)) return(list())
  size <- .mosaic_resolve_shard_batch(size)
  if (size <= 1L) return(as.list(sim_ids))
  unname(split(sim_ids, ceiling(seq_along(sim_ids) / size)))
}

#' Run Simulation Batch (Sequential or Parallel)
#'
#' Abstraction layer that runs simulations either sequentially (lapply) or
#' in parallel (parLapply) depending on whether a cluster object is provided.
#'
#' @param sim_ids Vector of simulation IDs to run
#' @param worker_func Simulation worker function
#' @param cl Cluster object (NULL for sequential, cluster for parallel)
#' @param show_progress Logical, whether to show progress bar
#' @param unit_runs Engine runs per task (\code{n_iterations}, times the shard
#'   size for chunked tasks); scales the parallel idle timeout.
#' @return List of success indicators from worker function; a task that raised
#'   an R-level error on a worker is recorded as \code{FALSE}.
#' @noRd
.mosaic_run_batch <- function(sim_ids, worker_func, cl, show_progress, unit_runs = 1L) {
  if (is.null(cl)) {
    # Sequential execution
    if (isTRUE(show_progress)) {
      # Simple progress bar with block character (no color codes)
      # style = 1: Shows elapsed and remaining time with percentage
      pbo <- pbapply::pboptions(type = "timer", char = "\u2588", style = 1)
      on.exit(pbapply::pboptions(pbo), add = TRUE)

      # Wrap worker to suppress unwanted output
      wrapped_func <- function(id) {
        capture.output(result <- worker_func(id), type = "output")
        result
      }

      pbapply::pblapply(sim_ids, wrapped_func)
    } else {
      lapply(sim_ids, worker_func)
    }
  } else {
    # Parallel execution.
    #
    # Worker-death-robust gather: parLapply()/pblapply(cl=) collect results with a
    # BLOCKING unserialize() that, on Linux, hangs the master FOREVER if a PSOCK
    # worker PROCESS dies mid-task -- an OOM kill, or any other fatal C-level
    # abort (an R-level error instead comes back as a try-error value, mapped
    # to FALSE below). The embedded Python interpreter used to be the likeliest source
    # of such an abort; the engine is pure R since v0.68.0, so OOM is now the
    # realistic case, but a blocking gather is just as unrecoverable either way. Calibration runs 10,000s of sims
    # per country, the highest-exposure parallel gather in the package, so route it
    # through the same socketSelect()-timeout dispatch used by calc_model_ensemble()
    # (.mosaic_cluster_lapply_robust): a dead worker degrades on the survivors with
    # a warning; total silence past the idle timeout stop()s with a diagnostic
    # instead of hanging.
    #
    # worker_func is `function(sim_id) .run_sim_worker(sim_id)`; `.run_sim_worker`
    # is installed on each worker's .GlobalEnv (clusterCall in run_MOSAIC). Reparent
    # the closure to globalenv() so the robust gather's per-task dispatch ships a
    # tiny payload (not the heavy run_MOSAIC frame) while still resolving
    # .run_sim_worker on the worker.
    environment(worker_func) <- globalenv()
    idle_timeout <- .mosaic_calibration_idle_timeout(unit_runs)
    res <- .mosaic_cluster_lapply_robust(
      cl, sim_ids, worker_func,
      idle_timeout_sec = idle_timeout,
      progress = isTRUE(show_progress),
      label = sprintf(paste0(
        "run_MOSAIC simulation batch (idle timeout %d s for %d engine run(s) per task; if ",
        "tasks are merely slow, raise options(MOSAIC.calibration_sec_per_engine_run) or ",
        "options(MOSAIC.ensemble_worker_timeout_sec))."),
        as.integer(idle_timeout), as.integer(unit_runs))
    )
    # A crashed worker yields a list(.mosaic_worker_died=TRUE, success=FALSE, ...)
    # marker; the calibration success tally expects a scalar logical per sim
    # (sum(unlist(success_indicators))). Coerce a dead-worker task to FALSE: it
    # counts as a failed sim and the batch degrades gracefully (the lost sim_id is
    # simply re-drawn on resume).
    # A task function that threw on the worker is likewise a failed sim, but
    # both kinds are surfaced with their count and first error text so a
    # systematic worker-side failure is not reduced to "every sim failed".
    .mosaic_warn_dispatch_failures(res, "run_MOSAIC simulation batch")
    lapply(res, function(r)
      if (is.list(r) && (isTRUE(r$.mosaic_worker_died) || isTRUE(r$.mosaic_task_error))) FALSE else r)
  }
}

#' Idle timeout for the calibration parallel gather
#'
#' The robust gather stops when no busy worker returns a result within the
#' timeout. A calibration task is \code{unit_runs} engine runs (a shard of
#' simulations times \code{n_iterations}), so a fixed 1800 s can be exceeded by
#' a healthy but large task. The timeout is the larger of
#' \code{getOption("MOSAIC.ensemble_worker_timeout_sec", 1800)} and
#' \code{unit_runs * getOption("MOSAIC.calibration_sec_per_engine_run", 30)}
#' (30 s is ~20x a measured 40-location engine run).
#'
#' @param unit_runs Engine runs per task.
#' @return Timeout in seconds.
#' @noRd
.mosaic_calibration_idle_timeout <- function(unit_runs = 1L) {
  base    <- as.numeric(getOption("MOSAIC.ensemble_worker_timeout_sec", 1800))
  per_run <- as.numeric(getOption("MOSAIC.calibration_sec_per_engine_run", 30))
  unit_runs <- suppressWarnings(as.numeric(unit_runs))
  if (length(unit_runs) != 1L || !is.finite(unit_runs) || unit_runs < 1) unit_runs <- 1
  max(base, unit_runs * per_run)
}


