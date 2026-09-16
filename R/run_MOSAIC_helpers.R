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

#' Weighted per-location endemic/epidemic CFR reference levels for the CFR panel
#'
#' Computes the best-subset weighted median of the implied surveillance CFR
#' (\code{cfr_baseline_<iso>} / \code{cfr_epidemic_<iso>} columns written by
#' \code{calc_implied_cfr()}) per location, for the dashed regime reference lines
#' on the trajectory CFR(t) panel (DM F4). Uses the candidate best subset
#' (\code{is_best_subset} / \code{weight_best}) to match the trajectory weighting.
#' Returns \code{NULL} when neither column family is present (e.g. gamma_1 absent).
#'
#' @param results The samples/results data.frame (from samples.parquet).
#' @param location_names Character vector of locations (config order).
#' @return A data.frame \code{location, cfr_baseline, cfr_epidemic} (weighted
#'   medians; \code{NA} where a column is absent), or \code{NULL}.
#' @noRd
.mosaic_compute_cfr_refs <- function(results, location_names) {
  if (is.null(results) || !is.data.frame(results) ||
      !("is_best_subset" %in% names(results))) return(NULL)
  sub <- results[results$is_best_subset %in% TRUE, , drop = FALSE]
  if (nrow(sub) == 0L) return(NULL)
  w <- sub$weight_best
  if (is.null(w) || all(!is.finite(w)) || sum(w, na.rm = TRUE) == 0) return(NULL)
  bcols <- paste0("cfr_baseline_", location_names)
  ecols <- paste0("cfr_epidemic_", location_names)
  if (!any(c(bcols, ecols) %in% names(sub))) return(NULL)  # no CFR columns at all
  .wmed <- function(col) if (col %in% names(sub))
    weighted_quantiles(sub[[col]], w, 0.5) else NA_real_
  data.frame(
    location     = location_names,
    cfr_baseline = vapply(bcols, .wmed, numeric(1)),
    cfr_epidemic = vapply(ecols, .wmed, numeric(1)),
    row.names    = NULL, stringsAsFactors = FALSE)
}

#' Persist the compact trajectory artifact for the "trajectories" figure group
#'
#' Writes the \code{mosaic_trajectories} object carried on a
#' \code{mosaic_ensemble} (\code{$trajectories}; comprehensive internal-state
#' channels — per-channel weighted median + uniform-thinned actual member lines +
#' derived series) to \code{2_calibration/trajectories_ensemble.rds},
#' schema-stamped. Persisted off the CANDIDATE ensemble (PLAN sec 14.B B-PERSIST:
#' the optimized rebuild drops attached fields), so the trajectories reflect the
#' candidate best subset / \code{weight_best}. A \code{NULL} \code{$trajectories}
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
#' (\code{"mean"} or \code{"median"}) is treated as the canonical central
#' trajectory for predictions, plots, ensemble R^2/bias metrics, the medoid
#' target, and the subset-selection objective. The weighted MEAN is the
#' unbiased estimator of expected counts (\eqn{E[\sum]=\sum E}) and never
#' collapses to zero, so it is the package default; \code{"median"} reproduces
#' historical (pre-feature) runs.
#'
#' Accepts a scalar (applies to both channels) or a named vector to set cases
#' and deaths independently, e.g. \code{c(cases = "median", deaths = "mean")}.
#'
#' @param x \code{NULL}, a scalar \code{"mean"}/\code{"median"}, or a named
#'   vector with \code{"cases"} and/or \code{"deaths"}. \code{NULL} or an
#'   unset channel falls back to \code{"median"}.
#' @return Named character vector \code{c(cases = ., deaths = .)}, each
#'   \code{"mean"} or \code{"median"}.
#' @noRd
.mosaic_resolve_central_method <- function(x = NULL) {
  valid <- c("mean", "median")
  ch    <- c("cases", "deaths")
  out   <- stats::setNames(rep("median", 2L), ch)

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
#' initial-condition warm-up transient; (2) the final deaths timestep is a
#' structural zero (reported deaths are written at \code{tick} rather than
#' \code{tick + 1}, so the last row is trimmed -- laser-cholera issue #82,
#' reproduced by the R engine because the trim rule was ported verbatim).
#'
#' Masks by COLUMN (= time), so it is correct for any number of locations (rows);
#' scoring sites flatten column-major via \code{as.numeric()}.
#'
#' @param mat Numeric matrix \code{[n_loc x n_time]} (a vector is coerced to a
#'   single-row matrix). The central series to mask.
#' @param chan \code{"cases"} or \code{"deaths"}.
#' @param spec Artifact-mask list (\code{ens$artifact_mask}). If \code{NULL},
#'   falls back to \code{list(cases_warmup = 2L, deaths_final = TRUE)} so older
#'   ensembles or sub-ensembles lacking the field still mask correctly.
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

  # Cases start: explicit score_start_cases (date) overrides burn-in; otherwise
  # burn_in_days. Index is offset + 1 (1-based scored start).
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
    c("calibration", "batch_size",           "batch_size_adaptive"),
    c("calibration", "min_batches",          "min_batches_adaptive"),
    c("calibration", "max_batches",          "max_batches_adaptive"),
    c("calibration", "target_r2",            "target_r2_adaptive"),
    c("calibration", "target_r2_ess",        "target_r2_adaptive"),
    c("calibration", "max_predictive_batch", "max_batch_predictive"),
    c("calibration", "max_simulations",      "max_simulations_total"),
    c("targets",     "ess_method",           "ESS_method")
  )
  for (.p in .mosaic_renamed_params) {
    def <- .mosaic_migrate_renamed(def, .p[[1]], .p[[2]], .p[[3]])
  }

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
    "calibration$max_batches_adaptive must be positive integer" =
      is.numeric(def$calibration$max_batches_adaptive) && def$calibration$max_batches_adaptive > 0,
    "calibration$target_r2_adaptive must be in [0, 1]" =
      is.numeric(def$calibration$target_r2_adaptive) &&
      def$calibration$target_r2_adaptive >= 0 && def$calibration$target_r2_adaptive <= 1,
    "targets$ESS_param must be positive" =
      is.numeric(def$targets$ESS_param) && def$targets$ESS_param > 0,
    "targets$ESS_param_prop must be in [0, 1]" =
      is.numeric(def$targets$ESS_param_prop) &&
      def$targets$ESS_param_prop >= 0 && def$targets$ESS_param_prop <= 1,
    "targets$A_best must be in [0, 1]" =
      is.numeric(def$targets$A_best) &&
      def$targets$A_best >= 0 && def$targets$A_best <= 1,
    "targets$CVw_best must be positive" =
      is.numeric(def$targets$CVw_best) && def$targets$CVw_best > 0,
    "targets$min_best_subset must be positive integer >= 10" =
      is.numeric(def$targets$min_best_subset) &&
      def$targets$min_best_subset >= 10,
    "targets$max_best_subset must be >= min_best_subset" =
      is.numeric(def$targets$max_best_subset) &&
      def$targets$max_best_subset >= def$targets$min_best_subset
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

  # Check batch_size_adaptive < max_simulations_total
  if (def$calibration$batch_size_adaptive >= def$calibration$max_simulations_total) {
    stop("calibration$batch_size_adaptive (", def$calibration$batch_size_adaptive,
         ") must be less than calibration$max_simulations_total (",
         def$calibration$max_simulations_total, ")", call. = FALSE)
  }

  # LOGICAL CONSISTENCY
  if (def$calibration$min_batches_adaptive > def$calibration$max_batches_adaptive) {
    stop("calibration$min_batches_adaptive (", def$calibration$min_batches_adaptive,
         ") must be <= calibration$max_batches_adaptive (", def$calibration$max_batches_adaptive, ")",
         call. = FALSE)
  }

  # IO VALIDATION
  .mosaic_validate_io(def$io)

  def
}

#' Validate I/O Settings
#' @noRd
.mosaic_validate_io <- function(io) {
  # Format check
  if (!io$format %in% c("csv", "parquet")) {
    stop("io$format must be 'csv' or 'parquet', got: ", io$format, call. = FALSE)
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

  # Warning for suboptimal choices
  if (io$format == "csv" && io$compression == "none") {
    warning("CSV without compression is 2-3x larger and 6-30x slower than parquet.\n",
            "Consider using mosaic_io_presets('default') for production runs.",
            call. = FALSE, immediate. = TRUE)
  }

  invisible(TRUE)
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
  metadata_fields <- c("metadata", "parameters_global", "parameters_location",
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
#' When \code{control$predictions$optimize_subset = TRUE} and the run produced
#' \code{is_best_subset_opt} / \code{weight_best_opt} columns, those are the
#' canonical subset for posterior work. Otherwise the tier-selected columns
#' (\code{is_best_subset} / \code{weight_best}) are canonical.
#'
#' The \code{any(results$is_best_subset_opt)} guard handles the edge case where
#' \code{optimize_subset = TRUE} but the optimizer silently failed (e.g.,
#' \code{stability_flag} + \code{optimal_n == 0}) -- in which case we fall back
#' to the tier subset.
#'
#' @param results data.frame with at least \code{is_best_subset} and
#'   \code{weight_best} columns, optionally \code{is_best_subset_opt} and
#'   \code{weight_best_opt}.
#' @param control MOSAIC control list.
#' @return Named list with \code{subset_col}, \code{weight_col}, and
#'   \code{source} ("tier" or "optimized").
#' @noRd
.mosaic_active_subset_cols <- function(results, control) {
  use_opt <- isTRUE(control$predictions$optimize_subset) &&
             "is_best_subset_opt" %in% names(results) &&
             "weight_best_opt"    %in% names(results) &&
             isTRUE(any(as.logical(results$is_best_subset_opt), na.rm = TRUE))
  list(
    subset_col = if (use_opt) "is_best_subset_opt" else "is_best_subset",
    weight_col = if (use_opt) "weight_best_opt"    else "weight_best",
    source     = if (use_opt) "optimized"          else "tier"
  )
}

#' Write JSON with Atomic Rename (NFS-Safe)
#' @noRd
.mosaic_write_json <- function(obj, path, io) {
  # Define write function
  write_func <- function(data, file) {
    jsonlite::write_json(data, file, pretty = TRUE, auto_unbox = TRUE, digits = NA)
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
        # Small enough to load in one shot
        arrow::open_dataset(dir_params, format = "parquet") %>%
          dplyr::collect() %>%
          as.data.frame()
      } else {
        # Chunked approach: bound the working set by reading `chunk_size` files at
        # a time. Each chunk goes through arrow::open_dataset() on the FILE VECTOR
        # rather than one read_parquet() per file, so arrow assembles the chunk in
        # its own C++ layer instead of returning 5,000 intermediate data.frames to
        # R. Measured at the production column count (1,352 cols, 1,000 single-row
        # shards, min of 3 on an M1 Max):
        #
        #   per-file rbindlist(fill = TRUE)      21.47 s   1.00x  (what this replaced)
        #   open_dataset(), default schema        7.81 s   2.75x  -- WRONG, see below
        #   open_dataset(unify_schemas = TRUE)   13.68 s   1.57x  -- what we use
        #
        # The default is not merely faster, it is silently incorrect here. Given a
        # file vector, open_dataset() adopts the FIRST file's schema and does not
        # raise when a later file disagrees: it drops that file's extra columns and
        # returns NA. A shard set where some shards carry a column others lack
        # therefore comes back missing data with no error to catch -- which is why
        # this cannot be guarded with tryCatch. `unify_schemas = TRUE` reads every
        # shard's schema and takes their union, reproducing exactly what
        # rbindlist(fill = TRUE) did. Half the available speedup buys that, and it
        # is not optional: test-combine-shards.R pins the behaviour.
        #
        # The tryCatch below is for arrow failing outright on a chunk (a torn or
        # unreadable shard), not for schema drift, which is handled above.
        n_chunks <- ceiling(n_files / chunk_size)
        if (verbose) log_msg("Loading in %d chunks of up to %d files", n_chunks, chunk_size)
        chunk_list <- vector("list", n_chunks)
        n_fallback <- 0L
        for (ci in seq_len(n_chunks)) {
          idx_start <- (ci - 1L) * chunk_size + 1L
          idx_end <- min(ci * chunk_size, n_files)
          chunk_files <- files[idx_start:idx_end]
          chunk_list[[ci]] <- tryCatch(
            data.table::as.data.table(
              dplyr::collect(
                arrow::open_dataset(chunk_files, format = "parquet",
                                    unify_schemas = TRUE)
              )
            ),
            error = function(e) {
              n_fallback <<- n_fallback + 1L
              data.table::rbindlist(
                lapply(chunk_files, arrow::read_parquet),
                fill = TRUE
              )
            }
          )
          if (verbose && ci %% 5 == 0) {
            log_msg("  Loaded chunk %d/%d (%d files)", ci, n_chunks, idx_end)
          }
        }
        if (n_fallback > 0L) {
          log_msg("  %d/%d chunk(s) could not be read as a dataset; read per-file instead",
                  n_fallback, n_chunks)
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
    phase = if (identical(nspec$mode, "fixed")) "fixed" else "calibration",
    calib_batches = 0L,
    r2_ess = NA_real_,
    calibration_done = FALSE,
    ess_history = list(),
    ess_tracking = list(),
    param_names_est = param_names_est,
    converged = FALSE,
    predictive_done = FALSE,
    mode = nspec$mode,
    fixed_target = nspec$fixed_target,
    # Track batches within each phase (used for predictive batch limits)
    phase_batch_count = 0L,
    phase_last = NULL,
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
  # Extract only the fields needed for monitoring
  persisted <- list(
    schema_version = 1L,
    status = "running",
    created_at = if (!is.null(state$.created_at)) state$.created_at
                 else format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    phase = state$phase,
    batch = state$batch_number,
    sims_completed = state$total_sims_successful,
    sims_target = if (identical(state$mode, "fixed")) state$fixed_target else NULL,
    converged = isTRUE(state$converged),
    ess_regression_r2 = if (!is.na(state$r2_ess)) round(state$r2_ess, 4) else NULL,
    ess_min = if (length(state$ess_tracking) > 0) {
      round(tail(vapply(state$ess_tracking, `[[`, numeric(1), "min_ess"), 1), 1)
    } else NULL
  )

  # Atomic write: tempfile + rename
  tmp <- tempfile(tmpdir = dirname(path), fileext = ".json.tmp")
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(persisted, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
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
# (ess_tracking, phase, batch counters, convergence flags) is persisted each
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
    schema_version      = 1L,
    total_sims_run      = state$total_sims_run,
    batch_number        = state$batch_number,
    phase               = state$phase,
    phase_batch_count   = state$phase_batch_count,
    calib_batches       = state$calib_batches,
    r2_ess              = state$r2_ess,
    calibration_done    = state$calibration_done,
    predictive_done     = state$predictive_done,
    converged           = state$converged,
    ess_tracking        = state$ess_tracking,
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

#' Reconstruct Calibration State From Disk for Resume
#'
#' Overlays the fresh-init `state` with state recovered from the shards on disk
#' (and the resume checkpoint if present). Always sets total_sims_run from the
#' disk watermark so the next sim_id never collides with an existing draw.
#'
#' @param state Fresh state from .mosaic_init_state()
#' @param dirs Directory list
#' @param control Control list
#' @param param_names_est Estimated-parameter names
#' @return Reconstructed state (unchanged if no shards exist -> fresh run)
#' @noRd
.mosaic_reconstruct_state <- function(state, dirs, control, param_names_est) {
  scan <- .mosaic_resume_scan(dirs$cal_samples)

  if (scan$n == 0L) {
    log_msg("[RESUME] No existing shards in %s \u2014 starting fresh run", dirs$cal_samples)
    return(state)
  }

  # Disk watermark is authoritative for the sim_id frontier (avoids seed reuse).
  state$total_sims_run        <- scan$watermark
  state$total_sims_successful <- scan$n

  ckpt <- .mosaic_load_checkpoint(file.path(dirs$cal_state, "resume_checkpoint.rds"))

  if (!is.null(ckpt)) {
    # ---- Exact restore from checkpoint -------------------------------------
    state$batch_number        <- ckpt$batch_number        %||% state$batch_number
    state$phase               <- ckpt$phase               %||% state$phase
    state$phase_batch_count   <- ckpt$phase_batch_count   %||% state$phase_batch_count
    state$calib_batches       <- ckpt$calib_batches       %||% state$calib_batches
    state$r2_ess              <- ckpt$r2_ess              %||% state$r2_ess
    state$calibration_done    <- isTRUE(ckpt$calibration_done)
    state$predictive_done     <- isTRUE(ckpt$predictive_done)
    state$converged           <- isTRUE(ckpt$converged)
    state$ess_tracking        <- ckpt$ess_tracking        %||% list()
    state$batch_success_rates <- ckpt$batch_success_rates %||% numeric()
    state$batch_sizes_used    <- ckpt$batch_sizes_used    %||% integer()

    # ess_tracking total_sims is a row COUNT, so compare against the shard
    # count (not the max-id watermark, which would over-fire on failed-sim gaps).
    last_count <- if (length(state$ess_tracking))
      tail(vapply(state$ess_tracking, `[[`, numeric(1), "total_sims"), 1) else 0
    log_msg("[RESUME] Restored checkpoint: %d sims, batch %d, phase '%s', converged=%s",
            state$total_sims_run, state$batch_number, state$phase, isTRUE(state$converged))

    # Refresh ESS/convergence from the ACTUAL on-disk pool whenever it differs
    # from what the checkpoint was certified against:
    #   - more shards than the checkpoint (a partial batch wrote after it), or
    #   - FEWER shards (some were quarantined on this scan) -- in which case a
    #     stale converged=TRUE no longer matches the pool, so clear it and let
    #     the ESS check re-derive convergence rather than trusting the flag.
    if (identical(state$mode, "auto") && scan$n != last_count) {
      if (state$converged && scan$n < last_count) state$converged <- FALSE
      state <- .mosaic_ess_check_update_state(state, dirs, param_names_est, control)
    }

  } else if (identical(state$mode, "auto")) {
    # ---- Lightweight bootstrap (no checkpoint; e.g. pre-feature run) --------
    bs <- control$calibration$batch_size_adaptive %||% 1000L
    # Estimate batches from the shard COUNT (not the max-id watermark, which
    # over-counts when failed sims leave gaps).
    state$batch_number <- max(1L, as.integer(ceiling(scan$n / max(bs, 1L))))
    # If the recovered pool already exhausted the adaptive batch budget, mark
    # calibration done so resume proceeds to the predictive phase rather than
    # re-running the full adaptive allotment from the resume point.
    max_adaptive <- control$calibration$max_batches_adaptive %||% 8L
    if (state$batch_number >= max_adaptive) state$calibration_done <- TRUE
    log_msg("[RESUME] No checkpoint; bootstrapping from %d shards (watermark %d, batch %d%s)",
            scan$n, scan$watermark, state$batch_number,
            if (isTRUE(state$calibration_done)) ", calibration budget already met" else "")
    state <- .mosaic_ess_check_update_state(state, dirs, param_names_est, control)

  } else {
    log_msg("[RESUME] Fixed mode: %d shards present (watermark %d)", scan$n, scan$watermark)
  }

  state
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
.mosaic_likelihood_impl_version <- function() "R/v0.22.21"

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

#' Verify Resume Inputs Match the Interrupted Run
#'
#' On resume, the incoming config/priors must match those persisted in 1_inputs/.
#' Changing the prior or likelihood target makes existing shards incomparable to
#' new draws, so a mismatch is a hard error. Comparison uses the same serializer
#' that wrote the files (byte-exact for identical inputs); if serialization
#' cannot be performed the check downgrades to a warning rather than blocking.
#'
#' Also guards the transmission engine: resuming a run directory created before
#' MOSAIC v0.68.0 is a hard error, because its shards came from the Python
#' laser-cholera engine and pooling them with R-engine draws would produce a
#' posterior from neither simulator. When \code{control} is supplied, the
#' likelihood target (\code{control$likelihood}) is also compared, since it is
#' not part of config.json/priors.json but changing it re-scores draws under a
#' different target.
#'
#' @noRd
.mosaic_resume_check_inputs <- function(dirs, config, priors, control = NULL) {
  serialize_obj <- function(obj) {
    tmp <- tempfile(fileext = ".json")
    on.exit(unlink(tmp), add = TRUE)
    ok <- tryCatch({
      jsonlite::write_json(obj, tmp, pretty = TRUE, auto_unbox = TRUE, digits = NA)
      TRUE
    }, error = function(e) FALSE)
    if (!ok) return(NA_character_)
    paste(readLines(tmp, warn = FALSE), collapse = "\n")
  }
  check_one <- function(obj, file, label) {
    if (!file.exists(file)) return(invisible())
    incoming  <- serialize_obj(obj)
    persisted <- tryCatch(paste(readLines(file, warn = FALSE), collapse = "\n"),
                          error = function(e) NA_character_)
    if (is.na(incoming) || is.na(persisted)) {
      warning(sprintf("resume: could not compare %s against 1_inputs/; skipping integrity check",
                      label), call. = FALSE)
      return(invisible())
    }
    if (!identical(incoming, persisted)) {
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
  #   - control$likelihood        : weights/sigmas/k_min -> the scoring target
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

#' Decide Next Batch Size
#' @noRd
.mosaic_decide_next_batch <- function(state, control, ess_tracking) {

  # Phase tracking is now managed in main loop
  # This function just decides what to do next

  # Calibration phase
  if (identical(state$phase, "calibration") && !isTRUE(state$calibration_done)) {
    return(list(
      phase = "calibration",
      batch_size = control$calibration$batch_size_adaptive
    ))
  }

  # Predictive batch
  # Require calibration_done before running predictive
  if (isTRUE(state$calibration_done) && !isTRUE(state$predictive_done)) {
    res <- tryCatch({
      calc_bookend_batch_size(
        ess_history = ess_tracking,
        target_ess = control$targets$ESS_param,
        max_total_sims = control$calibration$max_simulations_total,
        target_r_squared = control$calibration$target_r2_adaptive
      )
    }, error = function(e) NULL)

    # Log predictive batch calculation details
    if (!is.null(res) && !is.null(res$model) && res$batch_size > 0) {
      log_msg("Predictive batch calculation:")
      log_msg("  Model: %s (R\u00B2 = %.4f)", res$model, res$r_squared)
      log_msg("  Current ESS: %.1f \u2192 Target: %.0f", res$current_ess, res$target_ess)
      log_msg("  Predicted batch size: %.0f sims (safety factor: %.2f)",
              res$batch_size, res$safety_factor)
      log_msg("  Expected total after batch: %.0f sims", res$total_predicted)
    }

    # Determine batch size: use model prediction if available, else fall back
    # to the adaptive batch size (ensures we never run trivially small batches
    # or stall when the model can't predict).
    floor_size <- control$calibration$batch_size_adaptive

    if (!is.null(res) && res$batch_size > 0) {
      size <- as.integer(res$batch_size)
    } else {
      # Model returned 0 or failed -- use floor as fallback
      if (!is.null(res) && !is.null(res$message)) {
        log_msg("Predictive model: %s \u2014 using batch_size_adaptive as fallback", res$message)
      } else if (is.null(res)) {
        log_msg("Predictive batch calculation failed \u2014 using batch_size_adaptive as fallback")
      }
      size <- as.integer(floor_size)
    }

    # Apply floor: never run a predictive batch smaller than batch_size_adaptive
    if (size < floor_size) {
      size <- as.integer(floor_size)
    }

    # Cap predictive batch to avoid multi-hour single batches with no
    # checkpointing. Capped batches run with ESS re-evaluation between each;
    # predictive_done is set by .mosaic_ess_check_update_state() when the
    # ESS gap closes or max_batches_predictive is reached.
    max_pred <- control$calibration$max_batch_predictive
    if (!is.null(max_pred) && size > max_pred) {
      log_msg("  Capping predictive batch: %d \u2192 %d (max_batch_predictive)", size, max_pred)
      size <- as.integer(max_pred)
    }

    return(list(
      phase = "predictive",
      batch_size = size
    ))
  }

  # If we reach here, calibration_done and predictive_done are both TRUE
  # but convergence hasn't been declared. This shouldn't happen in normal
  # operation -- the predictive phase should run until convergence or its
  # batch limit. Return batch_size=0 to signal the loop to stop.
  list(
    phase = "predictive",
    batch_size = 0L,
    message = "Predictive phase exhausted without convergence"
  )
}

#' ESS Check and Update State
#' @noRd
.mosaic_ess_check_update_state <- function(state, dirs, param_names_est, control) {

  # Load all simulation files fresh from disk
  files <- list.files(dirs$cal_samples, pattern = "^sim_.*\\.parquet$", full.names = TRUE)
  if (!length(files)) return(state)

  log_msg("Checking ESS convergence...")
  load_start <- Sys.time()

  # Use efficient streaming method (same as final results loading)
  # This is much faster and more memory-efficient than rbindlist for large datasets
  ess_check_results <- tryCatch({
    .mosaic_load_and_combine_results(
      dir_params = dirs$cal_samples,
      method = "streaming",
      chunk_size = control$io$load_chunk_size %||% 5000L,
      verbose = FALSE
    )
  }, error = function(e) NULL)

  load_time <- difftime(Sys.time(), load_start, units = "secs")

  if (is.null(ess_check_results) || !nrow(ess_check_results)) {
    return(state)
  }

  # Skip ESS calculation if insufficient samples
  # calc_model_ess_parameter requires at least 50 samples
  if (nrow(ess_check_results) < 50) {
    log_msg("  Skipping ESS check: %d simulations (need at least 50)", nrow(ess_check_results))
    return(state)
  }

  # Calculate ESS using specified method from control.
  # Use the same adaptive n_grid as the final post-hoc ESS calculation in
  # run_MOSAIC (n_grid = 100 * (1 + log(ESS_param/100))). A coarser grid
  # over-estimates ESS for tight posteriors, which caused the loop to declare
  # convergence before the finer-grid post-hoc calculation could confirm it.
  ess_target_val <- control$targets$ESS_param %||% 100
  n_grid_adaptive <- as.integer(round(100 * (1 + log(max(ess_target_val, 100) / 100))))

  ess_current <- tryCatch({
    calc_model_ess_parameter(
      results = ess_check_results,
      param_names = param_names_est,
      likelihood_col = "likelihood",
      n_grid = n_grid_adaptive,
      method = control$targets$ESS_method,
      marginal_method = control$targets$ESS_marginal_method %||% "kde",
      verbose = FALSE
    )
  }, error = function(e) {
    log_msg("  ESS calculation failed: %s", e$message)
    NULL
  })

  if (is.null(ess_current) || !"ess_marginal" %in% names(ess_current)) {
    return(state)
  }

  # Store ESS history
  state$ess_history[[length(state$ess_history) + 1L]] <- list(
    batch = state$batch_number,
    total_sims = nrow(ess_check_results),
    ess_values = ess_current
  )

  # Calculate percentile-based threshold
  percentile_cutoff <- 1 - control$targets$ESS_param_prop
  threshold_ess <- as.numeric(stats::quantile(
    ess_current$ess_marginal,
    probs = percentile_cutoff,
    na.rm = TRUE
  ))

  state$ess_tracking[[length(state$ess_tracking) + 1L]] <- list(
    batch = state$batch_number,
    total_sims = nrow(ess_check_results),
    threshold_ess = threshold_ess,
    min_ess = min(ess_current$ess_marginal, na.rm = TRUE),
    median_ess = stats::median(ess_current$ess_marginal, na.rm = TRUE),
    max_ess = max(ess_current$ess_marginal, na.rm = TRUE)
  )

  # Report current ESS status
  log_msg("  ESS at %.0f%% percentile: %.1f (target: %d, min: %.1f)",
          percentile_cutoff * 100,
          threshold_ess,
          control$targets$ESS_param,
          min(ess_current$ess_marginal, na.rm = TRUE))

  # Calibration R^2 check
  # ESS is calculated for ALL batches (data accumulates from batch 1)
  # But calibration model fitting only starts when we have min_batches data points
  # This ensures the model uses data from ALL batches (1, 2, 3, ..., N)
  if (identical(state$phase, "calibration") &&
      !isTRUE(state$calibration_done) &&
      state$batch_number >= control$calibration$min_batches_adaptive &&
      length(state$ess_tracking) >= control$calibration$min_batches_adaptive) {

    ess_df <- data.frame(
      sims = vapply(state$ess_tracking, `[[`, numeric(1), "total_sims"),
      threshold_ess = vapply(state$ess_tracking, `[[`, numeric(1), "threshold_ess")
    )

    # Fit sqrt-linear model: ESS ~ sqrt(n) is typical scaling for importance sampling
    ess_df$sqrt_sims <- sqrt(ess_df$sims)
    ess_lm <- stats::lm(threshold_ess ~ sqrt_sims, data = ess_df)
    lm_summary <- summary(ess_lm)
    r2 <- lm_summary$r.squared
    coef <- stats::coef(ess_lm)
    intercept <- coef[1]
    slope <- coef[2]

    state$calib_batches <- state$calib_batches + 1L
    state$r2_ess <- r2

    # Calculate estimated simulations to reach target ESS
    # Model: ESS = intercept + slope x sqrt(n)
    # Solving for n: n = ((target_ess - intercept) / slope)^2
    target_ess <- control$targets$ESS_param
    # Guard against NA/NaN coefficients (occurs when ess_df has only 1 row,
    # making the 2-parameter lm underdetermined -- R sets slope=NA)
    est_sims <- if (isTRUE(slope > 0) && isTRUE((target_ess - intercept) > 0)) {
      ((target_ess - intercept) / slope)^2
    } else {
      NA_real_
    }

    # Print model fit diagnostics
    # slope/r2 may be NA when ESS has plateaued (constant response -> rank-deficient lm)
    slope_print <- if (is.finite(slope)) slope else 0
    r2_print    <- if (is.finite(r2))    r2    else 0
    log_msg("Calibration convergence check (batch %d):", state$batch_number)
    if (!is.na(est_sims)) {
      log_msg("  Model: ESS = %.2f + %.4f \u00D7 sqrt(n)  |  ESS regression R\u00B2 = %.4f (target %.2f) | Est. Sims: %.0f",
              intercept, slope_print, r2_print, control$calibration$target_r2_adaptive, round(est_sims))
    } else {
      log_msg("  Model: ESS = %.2f + %.4f \u00D7 sqrt(n)  |  ESS regression R\u00B2 = %.4f (target %.2f) | Est. Sims: N/A%s",
              intercept, slope_print, r2_print, control$calibration$target_r2_adaptive,
              if (!is.finite(slope)) " [ESS plateau \u2014 slope undefined]" else "")
    }
    log_msg("  Data points: %d measurements (batches 1-%d) | Simulations: %d-%d",
            nrow(ess_df), state$batch_number, min(ess_df$sims), max(ess_df$sims))

    # Check if calibration should end.
    # R^2 is only used as an exit signal when there are at least 5 data points
    # (3 residual df). A 2-parameter model fit to exactly min_batches=3 points
    # has only 1 residual df, making R^2 trivially near 1 for any monotone ESS
    # trajectory. The max_batches hard limit is always honoured regardless.
    min_r2_points <- 5L
    r2_converged <- r2 >= control$calibration$target_r2_adaptive && nrow(ess_df) >= min_r2_points
    if (r2_converged) {
      log_msg("  ESS regression R\u00B2 criterion met with %d data points (min required: %d)",
              nrow(ess_df), min_r2_points)
    } else if (r2 >= control$calibration$target_r2_adaptive && nrow(ess_df) < min_r2_points) {
      log_msg("  ESS regression R\u00B2 = %.4f >= target, but only %d data point(s) \u2014 need >= %d for reliable fit",
              r2, nrow(ess_df), min_r2_points)
    }

    if (r2_converged || state$batch_number >= control$calibration$max_batches_adaptive) {

      # Calculate remaining gap
      current_n <- nrow(ess_check_results)
      remaining_sims <- if (!is.na(est_sims) && est_sims > current_n) {
        est_sims - current_n
      } else {
        0
      }

      # Decide whether to end calibration or continue
      # Check max_batches FIRST to ensure hard limit is enforced
      if (state$batch_number >= control$calibration$max_batches_adaptive) {
        # Hit max batches limit - always exit regardless of R^2 or gap
        state$calibration_done <- TRUE
        if (r2 < control$calibration$target_r2_adaptive) {
          log_msg("  \u2192 Calibration complete: reached max_batches (%d) before ESS regression R\u00B2 converged (%.4f < %.2f)",
                  control$calibration$max_batches_adaptive, r2, control$calibration$target_r2_adaptive)
        } else {
          log_msg("  \u2192 Calibration complete: reached max_batches (%d)",
                  control$calibration$max_batches_adaptive)
        }
        if (remaining_sims > 0) {
          log_msg("    Estimated gap: %.0f sims \u2192 proceeding to predictive phase", ceiling(remaining_sims))
        }

      } else if (threshold_ess >= target_ess) {
        # Already at or above target ESS
        # Don't log here - convergence check will announce it
        state$calibration_done <- TRUE

      } else if (remaining_sims > 0 && remaining_sims < control$calibration$batch_size_adaptive) {
        # Small gap remaining - continue calibration instead of transitioning
        log_msg("  \u2192 Calibration R\u00B2 achieved, but gap is small")
        log_msg("    Current: %d sims | Estimated need: %.0f sims | Gap: %.0f sims",
                current_n, round(est_sims), ceiling(remaining_sims))
        log_msg("    Continuing calibration (gap < batch_size_adaptive)")
        # Don't set calibration_done, continue with one more batch

      } else {
        # R^2 converged and gap is large enough for predictive phase
        state$calibration_done <- TRUE
        log_msg("  \u2192 Calibration complete: R\u00B2 converged")
        if (remaining_sims > 0) {
          log_msg("    Estimated gap: %.0f sims \u2192 proceeding to predictive batch", ceiling(remaining_sims))
        }
      }
    }

  # Predictive phase: continue running capped batches until ESS gap closes
  # or safety batch limit is reached. Each iteration re-evaluates via
  # calc_bookend_batch_size() in .mosaic_decide_next_batch().
  } else if (identical(state$phase, "predictive") &&
             !is.null(state$phase_batch_count) &&
             state$phase_batch_count >= 1) {

    pred_gap <- NA_real_
    if (length(state$ess_tracking)) {
      cur_thresh <- tail(vapply(state$ess_tracking, `[[`, numeric(1), "threshold_ess"), 1)
      pred_gap <- control$targets$ESS_param - cur_thresh
    }

    max_pred_batches <- control$calibration$max_batches_predictive %||% 10L

    if (!is.na(pred_gap) && pred_gap <= 0) {
      state$predictive_done <- TRUE
      log_msg("  \u2192 Predictive phase complete: ESS gap closed (gap = %.1f)", pred_gap)

    } else if (state$phase_batch_count >= max_pred_batches) {
      state$predictive_done <- TRUE
      log_msg("  \u2192 Predictive batch limit reached (%d/%d)",
              state$phase_batch_count, max_pred_batches)

    } else {
      pred_gap_str <- if (is.na(pred_gap)) "N/A" else sprintf("%.1f", pred_gap)
      log_msg("  \u2192 Predictive batch %d/%d complete, ESS gap = %s \u2014 continuing predictive phase",
              state$phase_batch_count, max_pred_batches, pred_gap_str)
    }
  }

  # Check convergence.
  # Denominator is the total number of sampled parameters (not just those with
  # a valid ESS estimate). Parameters whose KDE returned NA are counted as
  # "not converged" -- otherwise silent KDE failures would shrink the
  # denominator and falsely inflate prop_converged, causing the loop to exit
  # before the post-hoc ESS calculation can confirm the 0.975 target.
  n_total        <- length(param_names_est)
  n_converged    <- sum(ess_current$ess_marginal >= control$targets$ESS_param, na.rm = TRUE)
  n_ess_computed <- sum(!is.na(ess_current$ess_marginal))
  prop_converged <- if (n_total > 0L) n_converged / n_total else 0

  if (prop_converged >= control$targets$ESS_param_prop) {
    # Only declare convergence after min_batches to ensure sufficient exploration.
    # Early batches can spuriously meet ESS targets with small samples.
    if (state$batch_number >= control$calibration$min_batches_adaptive) {
      state$converged <- TRUE
      log_msg("  \u2192 CONVERGENCE ACHIEVED: %.1f%% of parameters at ESS >= %.0f (%d/%d; %d ESS computed)",
              prop_converged * 100, control$targets$ESS_param,
              n_converged, n_total, n_ess_computed)
    } else {
      log_msg("  ESS criterion met (%.1f%% >= %.0f) but batch %d < min_batches %d \u2014 continuing",
              prop_converged * 100, control$targets$ESS_param,
              state$batch_number, control$calibration$min_batches_adaptive)
    }
  }

  # Clean up and return
  rm(ess_check_results)
  gc(verbose = FALSE)

  state
}

# =============================================================================
# WEIGHT CALCULATIONS
# =============================================================================

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

#' Run Simulation Batch (Sequential or Parallel)
#'
#' Abstraction layer that runs simulations either sequentially (lapply) or
#' in parallel (parLapply) depending on whether a cluster object is provided.
#'
#' @param sim_ids Vector of simulation IDs to run
#' @param worker_func Simulation worker function
#' @param cl Cluster object (NULL for sequential, cluster for parallel)
#' @param show_progress Logical, whether to show progress bar
#' @return List of success indicators from worker function
#' @noRd
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
.mosaic_resolve_shard_batch <- function(x) {
  if (is.null(x)) return(1L)
  n <- suppressWarnings(as.integer(x)[1])
  if (is.na(n) || n < 1L) return(1L)
  n
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

.mosaic_run_batch <- function(sim_ids, worker_func, cl, show_progress) {
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
    # abort (NOT an R-level error, which the worker already turns into a FALSE
    # record). The embedded Python interpreter used to be the likeliest source
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
    res <- .mosaic_cluster_lapply_robust(
      cl, sim_ids, worker_func,
      idle_timeout_sec = getOption("MOSAIC.ensemble_worker_timeout_sec", 1800),
      progress = isTRUE(show_progress),
      label = "run_MOSAIC simulation batch"
    )
    # A crashed worker yields a list(.mosaic_worker_died=TRUE, success=FALSE, ...)
    # marker; the calibration success tally expects a scalar logical per sim
    # (sum(unlist(success_indicators))). Coerce a dead-worker task to FALSE: it
    # counts as a failed sim and the batch degrades gracefully (the lost sim_id is
    # simply re-drawn on resume).
    lapply(res, function(r)
      if (is.list(r) && isTRUE(r$.mosaic_worker_died)) FALSE else r)
  }
}


