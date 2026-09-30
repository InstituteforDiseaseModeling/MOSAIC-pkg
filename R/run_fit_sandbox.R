#' Deterministic Fit-Diagnostic Sandbox: One Simulation with Parameter Overrides
#'
#' @description
#' Runs a \strong{single deterministic} simulation from a calibration config
#' (typically a run's medoid config) with optional point-value parameter overrides,
#' then scores the result against the observed series carried in the config. This is
#' the experiment unit behind the active fit-diagnostic workflow (the \code{diagnose-fit}
#' skill): ~1-2 seconds per run, no calibration machinery, so a modeller (or the
#' \code{mosaic-calibration-doctor} agent) can test hypotheses about which parameters
#' drive a fit deficiency before committing to an expensive recalibration.
#'
#' It is country-agnostic — nothing is hard-coded to a specific location. The observed
#' data, dates, and locations are read from the supplied config.
#'
#' @details
#' Generalises the project-local \code{sensitivity_sandbox.R} pattern into the package.
#' Predicted and observed series are aggregated (summed) across the selected
#' \code{locations} to a single series before scoring, matching the country-level
#' diagnostic use case; pass a single index in \code{locations} for a per-patch view.
#' Full metrics are delegated to [calc_fit_diagnostics()].
#'
#' Scoring is paired and windowed the way [run_MOSAIC()] scores a fit. For each
#' day, the scored predicted total sums only the location-days that carry an
#' observation, so a location with no surveillance contributes to neither side
#' (a day with no observation at any selected location is \code{NA}, never 0).
#' The leading unscored steps -- the default likelihood scored window
#' ([mosaic_control_defaults()] \code{likelihood}: \code{burn_in_days}, 30 days)
#' and, for cases, the two-step initial-condition warm-up -- are dropped from
#' both series before any metric is computed. A run calibrated with a
#' non-default \code{burn_in_days}/\code{score_start_cases}/\code{deaths_score_start}
#' is still scored here on the default window.
#' The returned \code{predictions} use the same pairing but are not windowed:
#' on a day where at least one selected location is observed, \code{observed}
#' and \code{predicted_*} are both summed over exactly those observed locations
#' (\code{n_locations_observed} records how many), so the two columns are always
#' comparable; on a day with no observation at any selected location,
#' \code{observed} is \code{NA} and \code{predicted_*} is the full aggregate
#' over all selected locations. For a single location this is simply its own
#' observed and predicted series.
#'
#' On a config that predates the v0.96.0 mortality model (it carries any of
#' \code{mu_j_baseline}, \code{mu_j_epidemic_factor}, \code{CFR_target},
#' \code{mu_j}), the engine uses \code{CFR_target} as a constant reported CFR and
#' ignores \code{mu_jt}, so on such a config a \code{CFR_target} override is applied
#' and a \code{mu_jt} override is skipped with a warning.
#'
#' @param config A config as a named list, or a path to a config JSON
#'   (e.g. \code{.../2_calibration/best_model/config_medoid.json}).
#' @param params Named list of point-value parameter overrides applied to the config
#'   before the run (unknown names are skipped with a warning). Default \code{list()}.
#' @param seed Integer RNG seed for the simulation run. Default \code{42L}.
#' @param locations Integer indices of location rows to aggregate. Default \code{NULL}
#'   (all locations).
#' @param full_metrics Logical; if \code{TRUE} (default) compute the full
#'   [calc_fit_diagnostics()] bias/shape/variance scorecard, else only top-line
#'   R\eqn{^2}/bias/CFR.
#' @param outdir Optional directory; if supplied, writes
#'   \code{predictions_ensemble.csv} and \code{metrics.json} under
#'   \code{outdir/<run_label>/}. Default \code{NULL} (return only).
#' @param run_label Character label for the run (used for the output subdirectory and
#'   recorded in metrics). Default \code{"fit_sandbox"}.
#' @param quiet Logical passed to [run_simulation()]. Default \code{TRUE}.
#' @param .sim_runner Function used to run the model; defaults to [run_simulation()].
#'   Exposed as a seam for testing with a stubbed engine.
#'
#' @return A named list with \code{predictions} (long data.frame in the standard
#'   ensemble format, plus \code{n_locations_observed}), \code{metrics} (top-line metrics, including the 1-based
#'   \code{score_idx_cases}/\code{score_idx_deaths} scored-window starts, plus, when
#'   \code{full_metrics=TRUE}, \code{fit_diagnostics} and a merged \code{scorecard}),
#'   \code{params_applied} (data.frame of old/new values), and \code{run_label}.
#'
#' @seealso [calc_fit_diagnostics()], [run_simulation()]
#' @export
run_fit_sandbox <- function(config,
                            params = list(),
                            seed = 42L,
                            locations = NULL,
                            full_metrics = TRUE,
                            outdir = NULL,
                            run_label = "fit_sandbox",
                            quiet = TRUE,
                            .sim_runner = run_simulation) {

  # ---- Resolve config ------------------------------------------------------
  if (is.character(config) && length(config) == 1L) {
    if (!file.exists(config)) stop("run_fit_sandbox: config file not found: ", config)
    config <- .mosaic_read_json_cached(config)
  }
  if (!is.list(config)) stop("run_fit_sandbox: `config` must be a list or a path to a config JSON.")

  # ---- Apply overrides -----------------------------------------------------
  # A pre-v0.96.0 config is resolved by .mosaic_mu_jt_matrix(): the engine reads
  # its CFR_target as a constant reported CFR and never reads its mu_jt. So on
  # such a config the CFR_target override is the one that takes effect, and a
  # mu_jt override would be recorded in params_applied yet change nothing.
  legacy_cfg <- any(vapply(.MOSAIC_LEGACY_MORTALITY_FIELDS,
                           function(f) !is.null(config[[f]]), logical(1)))
  applied <- list()
  for (nm in names(params)) {
    if (legacy_cfg && identical(nm, "mu_jt")) {
      warning(paste0("run_fit_sandbox: this config predates the v0.96.0 mortality model, so the ",
                     "engine ignores `mu_jt` and uses `CFR_target` as the reported CFR - ",
                     "skipping; override `CFR_target` instead"))
      next
    }
    if (nm %in% .MOSAIC_REMOVED_MORTALITY_PARAMS && !(legacy_cfg && identical(nm, "CFR_target"))) {
      # Retired in v0.96.0: the engine reads only mu_jt (or, on a legacy config,
      # CFR_target), so an override of any other mortality field does nothing.
      warning(sprintf(paste0("run_fit_sandbox: '%s' was removed from the model in v0.96.0 - ",
                             "skipping; override %s (the reported CFR) instead"), nm,
                      if (legacy_cfg) "`CFR_target`" else "`mu_jt`"))
      next
    }
    if (!nm %in% names(config)) {
      warning(sprintf("run_fit_sandbox: '%s' not in config - skipping", nm))
      next
    }
    old_val <- config[[nm]]
    new_val <- params[[nm]]
    # Broadcast a scalar override onto a per-location (vector) or per-location x
    # day (matrix) parameter so the engine's shape checks still hold; a matrix
    # keeps its dimensions.
    if (length(old_val) > 1L && length(new_val) == 1L) {
      new_val <- if (is.matrix(old_val)) array(new_val, dim = dim(old_val), dimnames = dimnames(old_val))
                 else rep(new_val, length(old_val))
    }
    config[[nm]] <- new_val
    applied[[nm]] <- list(old = old_val, new = new_val)
  }
  params_applied <- if (length(applied)) {
    data.frame(
      parameter = names(applied),
      old = vapply(applied, function(x) suppressWarnings(as.numeric(x$old)[1]), numeric(1)),
      new = vapply(applied, function(x) suppressWarnings(as.numeric(x$new)[1]), numeric(1)),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(parameter = character(0), old = numeric(0), new = numeric(0))
  }

  # ---- Run a single deterministic simulation -------------------------------
  # The engine writes no files of its own, so there is no scratch dir to make
  # and no `visualize`/`pdf`/`outdir` to suppress: those were the Python
  # Analyzer's arguments, and passing them now raises (removed_api). Every
  # argument here must be a formal of the default runner, run_simulation() --
  # asserted by test-run_fit_sandbox.R, because the tests stub .sim_runner
  # and a stub that swallows `...` cannot see a call the real runner rejects.
  model <- .sim_runner(config = config, seed = seed, quiet = quiet)

  pred_cases_mat  <- .fit_as_matrix(model$results$reported_cases)
  pred_deaths_mat <- .fit_as_matrix(model$results$reported_deaths)
  obs_cases_mat   <- .fit_as_matrix(config$reported_cases)
  obs_deaths_mat  <- .fit_as_matrix(config$reported_deaths)

  loc_idx <- if (is.null(locations)) seq_len(nrow(pred_cases_mat)) else as.integer(locations)
  loc_idx <- loc_idx[loc_idx >= 1L & loc_idx <= nrow(pred_cases_mat)]
  if (!length(loc_idx)) stop("run_fit_sandbox: no valid location rows selected.")

  # Paired aggregates: per day, observed and predicted are both summed over
  # only the selected location-days that carry an observation, so a missing
  # observation is never summed in as a zero and never meets a predicted value
  # it has no counterpart for. Used for both the scored metrics and the table.
  sc_cases  <- .fit_agg_paired(obs_cases_mat,  pred_cases_mat,  loc_idx)
  sc_deaths <- .fit_agg_paired(obs_deaths_mat, pred_deaths_mat, loc_idx)
  # Table series: the paired aggregate where any location is observed; on a day
  # with none, observed is NA and predicted falls back to the full aggregate.
  tab <- function(sc, pred_m) {
    full <- colSums(pred_m[loc_idx, seq_along(sc$pred), drop = FALSE])
    list(obs = sc$obs, pred = ifelse(sc$n_obs > 0L, sc$pred, full), n_obs = sc$n_obs)
  }
  tab_cases  <- tab(sc_cases,  pred_cases_mat)
  tab_deaths <- tab(sc_deaths, pred_deaths_mat)

  dates <- seq.Date(as.Date(config$date_start), as.Date(config$date_stop), by = "day")
  n <- min(length(dates), length(sc_cases$obs), length(sc_deaths$obs))
  dates <- dates[seq_len(n)]
  cut <- function(tb) lapply(tb, function(v) v[seq_len(n)])
  tab_cases <- cut(tab_cases); tab_deaths <- cut(tab_deaths)

  # Drop the unscored head, exactly as run_MOSAIC() does before its R2/bias:
  # the likelihood's scored window (burn-in + per-channel starts) and, for
  # cases, the calc_model_ensemble() initial-condition warm-up (2 steps).
  sw <- .mosaic_resolve_score_window(
    config, list(likelihood = mosaic_control_defaults()$likelihood))
  head_cases  <- max(.FIT_CASES_WARMUP, sw$idx_cases - 1L)
  head_deaths <- sw$idx_deaths - 1L
  mask_head <- function(v, k) { v <- v[seq_len(n)]; if (k > 0L) v[seq_len(min(k, n))] <- NA_real_; v }
  sc_obs_cases   <- mask_head(sc_cases$obs,   head_cases)
  sc_pred_cases  <- mask_head(sc_cases$pred,  head_cases)
  sc_obs_deaths  <- mask_head(sc_deaths$obs,  head_deaths)
  sc_pred_deaths <- mask_head(sc_deaths$pred, head_deaths)

  loc_label <- if (length(loc_idx) == 1L && !is.null(config$location_name)) {
    as.character(config$location_name[loc_idx])
  } else if (length(loc_idx) > 1L) "AGGREGATE" else "location"

  # ---- Predictions data.frame (standard ensemble format) -------------------
  # One deterministic run is its own mean and median.
  mk <- function(metric, tb) {
    pred <- tb$pred
    data.frame(
      location = loc_label, date = as.character(dates), metric = metric,
      observed = tb$obs, predicted_central = pred, predicted_mean = pred,
      predicted_median = pred, central_method = "mean",
      ci_1_lower = pred, ci_1_upper = pred, ci_2_lower = pred, ci_2_upper = pred,
      n_locations_observed = as.integer(tb$n_obs),
      stringsAsFactors = FALSE
    )
  }
  predictions <- rbind(mk("Suspected Cases", tab_cases),
                       mk("Deaths", tab_deaths))

  # ---- Metrics -------------------------------------------------------------
  metrics <- list(
    run_label   = run_label,
    seed        = as.integer(seed),
    locations   = loc_idx,
    score_idx_cases  = as.integer(head_cases + 1L),
    score_idx_deaths = as.integer(head_deaths + 1L),
    r2_cases    = calc_model_R2(sc_obs_cases,  sc_pred_cases,  method = "corr"),
    r2_deaths   = calc_model_R2(sc_obs_deaths, sc_pred_deaths, method = "corr"),
    bias_cases  = calc_bias_ratio(sc_obs_cases,  sc_pred_cases),
    bias_deaths = calc_bias_ratio(sc_obs_deaths, sc_pred_deaths),
    # Named pair c(reported, symptomatic): the config's window-mean reported
    # CFR (mu_jt) and the per-onset fatality probability it implies.
    cfr_implied  = .fit_cfr_implied(config, loc_idx),
    cfr_observed = .fit_cfr_observed(obs_cases_mat, obs_deaths_mat, loc_idx)
  )

  if (isTRUE(full_metrics)) {
    # NOTE: config$epidemic_threshold is an Isym/N point-PREVALENCE fraction (~1e-6),
    # not a case count, so it must NOT be used as the observed-count split threshold.
    # Let calc_fit_diagnostics use its data-driven default (75th pctile of positive obs).
    cases_diag  <- calc_fit_diagnostics(sc_obs_cases,  sc_pred_cases,  dates)
    deaths_diag <- calc_fit_diagnostics(sc_obs_deaths, sc_pred_deaths, dates)
    metrics$fit_diagnostics <- list(cases = cases_diag, deaths = deaths_diag)
    metrics$scorecard <- c(
      bias_cases  = unname(cases_diag$scorecard["bias"]),
      bias_deaths = unname(deaths_diag$scorecard["bias"]),
      peak_timing = unname(cases_diag$scorecard["peak_timing"]),
      peak_shape  = unname(cases_diag$scorecard["peak_shape"]),
      variance    = unname(cases_diag$scorecard["variance"])
    )
  }

  # ---- Optional write ------------------------------------------------------
  if (!is.null(outdir)) {
    dir_out <- file.path(outdir, run_label)
    dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(predictions, file.path(dir_out, "predictions_ensemble.csv"),
                     row.names = FALSE)
    jsonlite::write_json(metrics, file.path(dir_out, "metrics.json"),
                         pretty = TRUE, auto_unbox = TRUE, digits = 8, null = "null")
  }

  list(predictions = predictions, metrics = metrics,
       params_applied = params_applied, run_label = run_label)
}

# ---- Internal helpers ------------------------------------------------------

# Leading cases steps blanked as the initial-condition warm-up transient; the
# calc_model_ensemble() default (n_cases_warmup_mask = 2L) that run_MOSAIC() uses.
.FIT_CASES_WARMUP <- 2L

# Paired aggregate of observed and predicted: per day, sum both over only the
# selected location-days where BOTH are finite, so the predicted total never
# includes a location whose observation is missing. A day with no such cell is
# NA in both. Returns list(obs, pred, n_obs), each of length ncol, where n_obs
# is the number of paired locations summed on each day.
.fit_agg_paired <- function(obs_m, pred_m, loc_idx) {
  nt <- min(ncol(obs_m), ncol(pred_m))
  o <- obs_m[loc_idx, seq_len(nt), drop = FALSE]
  p <- pred_m[loc_idx, seq_len(nt), drop = FALSE]
  ok <- is.finite(o) & is.finite(p)
  o[!ok] <- 0; p[!ok] <- 0
  any_ok <- colSums(ok) > 0
  so <- colSums(o); sp <- colSums(p)
  so[!any_ok] <- NA_real_; sp[!any_ok] <- NA_real_
  list(obs = as.numeric(so), pred = as.numeric(sp), n_obs = as.integer(colSums(ok)))
}

# Coerce a run_simulation/config field (matrix or vector) to a numeric matrix with
# locations in rows. The engine always returns a matrix now, but config-supplied
# observed series are still sometimes bare vectors.
.fit_as_matrix <- function(x) {
  if (is.null(x)) stop("run_fit_sandbox: expected a results/observed field but got NULL.")
  if (is.null(dim(x)) || length(dim(x)) == 1L) return(matrix(as.numeric(x), nrow = 1L))
  m <- as.matrix(x)
  storage.mode(m) <- "double"
  m
}

# Observed CFR: sum of deaths / sum of cases over the selected location-days
# where both are observed. NA when no cases are observed.
.fit_cfr_observed <- function(obs_cases_m, obs_deaths_m, loc_idx) {
  nt <- min(ncol(obs_cases_m), ncol(obs_deaths_m))
  oc <- obs_cases_m[loc_idx, seq_len(nt), drop = FALSE]
  od <- obs_deaths_m[loc_idx, seq_len(nt), drop = FALSE]
  ok <- is.finite(oc) & is.finite(od)
  sc <- sum(oc[ok])
  if (sc > 0) sum(od[ok]) / sc else NA_real_
}

# The config's reported CFR over the observed window, and the per-onset fatality
# probability the engine derives from it. Since v0.96.0 the reported CFR is a
# model input, `mu_jt` (location x day), so there is nothing to back out of a
# hazard. `reported` is its mean over the selected locations, weighted by the
# observed reported cases on each day -- the same weighting as an observed CFR
# (sum of deaths / sum of cases), so the two are directly comparable -- and
# falls back to the plain mean when no cases are observed. `symptomatic` is
# `reported * rho / (rho_deaths * chi_epidemic)`, the probability that a
# symptomatic onset is fatal. Returns NAs when a piece is missing.
.fit_cfr_implied <- function(config, loc_idx = NULL) {
  na_pair <- c(reported = NA_real_, symptomatic = NA_real_)
  nL <- length(config$location_name)
  nT <- as.integer(as.Date(config$date_stop) - as.Date(config$date_start)) + 1L
  if (!nL || is.na(nT) || is.null(config$rho) || is.null(config$rho_deaths) ||
      is.null(config$chi_epidemic)) return(na_pair)
  mu <- tryCatch(.mosaic_config_mu_jt(config, nL, nT), error = function(e) NULL)
  if (is.null(mu)) return(na_pair)
  sel <- if (is.null(loc_idx)) seq_len(nL) else loc_idx[loc_idx >= 1L & loc_idx <= nL]
  if (!length(sel)) return(na_pair)
  w <- config$reported_cases
  if (!is.null(w) && !is.matrix(w) && nL == 1L && length(w) == nT) w <- matrix(w, nrow = 1L)
  w <- if (is.matrix(w) && identical(dim(w), c(nL, nT))) w[sel, , drop = FALSE] else NULL
  m <- mu[sel, , drop = FALSE]
  reported <- if (!is.null(w) && any(is.finite(w) & w > 0)) {
    ok <- is.finite(w) & w > 0
    sum(m[ok] * w[ok]) / sum(w[ok])
  } else mean(m)
  c(reported = reported,
    symptomatic = if (config$rho_deaths > 0)
      reported * config$rho / (config$rho_deaths * config$chi_epidemic) else NA_real_)
}
