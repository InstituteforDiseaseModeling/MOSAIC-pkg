#' Deterministic Fit-Diagnostic Sandbox: One LASER Run with Parameter Overrides
#'
#' @description
#' Runs a \strong{single deterministic} LASER simulation from a calibration config
#' (typically a run's medioid config) with optional point-value parameter overrides,
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
#' @param config A config as a named list, or a path to a config JSON
#'   (e.g. \code{.../2_calibration/best_model/config_medioid.json}).
#' @param params Named list of point-value parameter overrides applied to the config
#'   before the run (unknown names are skipped with a warning). Default \code{list()}.
#' @param seed Integer RNG seed for the LASER run. Default \code{42L}.
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
#' @param quiet Logical passed to [run_LASER()]. Default \code{TRUE}.
#' @param .laser_runner Function used to run the model; defaults to [run_LASER()].
#'   Exposed as a seam for testing with a stubbed engine.
#'
#' @return A named list with \code{predictions} (long data.frame in the standard
#'   ensemble format), \code{metrics} (top-line metrics plus, when
#'   \code{full_metrics=TRUE}, \code{fit_diagnostics} and a merged \code{scorecard}),
#'   \code{params_applied} (data.frame of old/new values), and \code{run_label}.
#'
#' @seealso [calc_fit_diagnostics()], [run_LASER()]
#' @export
run_fit_sandbox <- function(config,
                            params = list(),
                            seed = 42L,
                            locations = NULL,
                            full_metrics = TRUE,
                            outdir = NULL,
                            run_label = "fit_sandbox",
                            quiet = TRUE,
                            .laser_runner = run_LASER) {

  # ---- Resolve config ------------------------------------------------------
  if (is.character(config) && length(config) == 1L) {
    if (!file.exists(config)) stop("run_fit_sandbox: config file not found: ", config)
    config <- jsonlite::fromJSON(config, simplifyVector = TRUE, simplifyMatrix = TRUE)
  }
  if (!is.list(config)) stop("run_fit_sandbox: `config` must be a list or a path to a config JSON.")

  # ---- Apply overrides -----------------------------------------------------
  applied <- list()
  for (nm in names(params)) {
    if (!nm %in% names(config)) {
      warning(sprintf("run_fit_sandbox: '%s' not in config - skipping", nm))
      next
    }
    old_val <- config[[nm]]
    new_val <- params[[nm]]
    # Broadcast a scalar override onto a per-location (vector) parameter so the
    # engine's length == n_locations assertion still holds (LASER hard-asserts this).
    if (length(old_val) > 1L && length(new_val) == 1L) new_val <- rep(new_val, length(old_val))
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
  # Hermetic per-call scratch dir for any engine artifacts; cleaned up on exit.
  scratch <- tempfile("fit_sandbox_")
  dir.create(scratch, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)
  model <- .laser_runner(config = config, seed = seed, quiet = quiet,
                         visualize = FALSE, pdf = FALSE, outdir = scratch)

  pred_cases_mat  <- .fit_as_matrix(model$results$reported_cases)
  pred_deaths_mat <- .fit_as_matrix(model$results$reported_deaths)
  obs_cases_mat   <- .fit_as_matrix(config$reported_cases)
  obs_deaths_mat  <- .fit_as_matrix(config$reported_deaths)

  loc_idx <- if (is.null(locations)) seq_len(nrow(pred_cases_mat)) else as.integer(locations)
  loc_idx <- loc_idx[loc_idx >= 1L & loc_idx <= nrow(pred_cases_mat)]
  if (!length(loc_idx)) stop("run_fit_sandbox: no valid location rows selected.")

  agg <- function(m) if (length(loc_idx) == 1L) as.numeric(m[loc_idx, ]) else colSums(m[loc_idx, , drop = FALSE], na.rm = TRUE)
  pred_cases  <- agg(pred_cases_mat)
  pred_deaths <- agg(pred_deaths_mat)
  obs_cases   <- agg(obs_cases_mat)
  obs_deaths  <- agg(obs_deaths_mat)

  dates <- seq.Date(as.Date(config$date_start), as.Date(config$date_stop), by = "day")
  n <- min(length(dates), length(pred_cases), length(obs_cases),
           length(pred_deaths), length(obs_deaths))
  dates <- dates[seq_len(n)]
  pred_cases <- pred_cases[seq_len(n)];   obs_cases <- obs_cases[seq_len(n)]
  pred_deaths <- pred_deaths[seq_len(n)]; obs_deaths <- obs_deaths[seq_len(n)]

  loc_label <- if (length(loc_idx) == 1L && !is.null(config$location_name)) {
    as.character(config$location_name[loc_idx])
  } else if (length(loc_idx) > 1L) "AGGREGATE" else "location"

  # ---- Predictions data.frame (standard ensemble format) -------------------
  mk <- function(metric, obs, pred) data.frame(
    location = loc_label, date = as.character(dates), metric = metric,
    observed = obs, predicted_median = pred,
    ci_1_lower = pred, ci_1_upper = pred, ci_2_lower = pred, ci_2_upper = pred,
    stringsAsFactors = FALSE
  )
  predictions <- rbind(mk("Suspected Cases", obs_cases, pred_cases),
                       mk("Deaths", obs_deaths, pred_deaths))

  # ---- Metrics -------------------------------------------------------------
  metrics <- list(
    run_label   = run_label,
    seed        = as.integer(seed),
    locations   = loc_idx,
    r2_cases    = calc_model_R2(obs_cases,  pred_cases,  method = "corr"),
    r2_deaths   = calc_model_R2(obs_deaths, pred_deaths, method = "corr"),
    bias_cases  = calc_bias_ratio(obs_cases,  pred_cases),
    bias_deaths = calc_bias_ratio(obs_deaths, pred_deaths),
    cfr_implied  = .fit_cfr_implied(config, loc_idx),
    cfr_observed = if (sum(obs_cases, na.rm = TRUE) > 0)
      sum(obs_deaths, na.rm = TRUE) / sum(obs_cases, na.rm = TRUE) else NA_real_
  )

  if (isTRUE(full_metrics)) {
    # NOTE: config$epidemic_threshold is an Isym/N point-PREVALENCE fraction (~1e-6),
    # not a case count, so it must NOT be used as the observed-count split threshold.
    # Let calc_fit_diagnostics use its data-driven default (75th pctile of positive obs).
    cases_diag  <- calc_fit_diagnostics(obs_cases,  pred_cases,  dates)
    deaths_diag <- calc_fit_diagnostics(obs_deaths, pred_deaths, dates)
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

# Coerce a run_LASER/config field (numpy array via reticulate, matrix, or vector)
# to a numeric matrix with locations in rows.
.fit_as_matrix <- function(x) {
  if (is.null(x)) stop("run_fit_sandbox: expected a results/observed field but got NULL.")
  if (is.null(dim(x)) || length(dim(x)) == 1L) return(matrix(as.numeric(x), nrow = 1L))
  m <- as.matrix(x)
  storage.mode(m) <- "double"
  m
}

# Implied CFR from config reporting-chain parameters (NA if any piece missing).
# mu_j_baseline is per-location; average it over the aggregated locations.
.fit_cfr_implied <- function(config, loc_idx = NULL) {
  mu  <- config[["mu_j_baseline"]]
  rd  <- config[["rho_deaths"]]
  rho <- config[["rho"]]
  ce  <- config[["chi_endemic"]]; cp <- config[["chi_epidemic"]]
  if (is.null(mu) || is.null(rd) || is.null(rho) || is.null(ce) || is.null(cp)) return(NA_real_)
  mu <- as.numeric(mu)
  if (!is.null(loc_idx)) { sel <- loc_idx[loc_idx >= 1L & loc_idx <= length(mu)]; if (length(sel)) mu <- mu[sel] }
  mu_bar <- mean(mu, na.rm = TRUE)
  chi <- 0.5 * (as.numeric(ce)[1] + as.numeric(cp)[1])
  rho1 <- as.numeric(rho)[1]
  if (!is.finite(rho1) || rho1 == 0 || !is.finite(mu_bar)) return(NA_real_)
  mu_bar * as.numeric(rd)[1] * chi / rho1
}
