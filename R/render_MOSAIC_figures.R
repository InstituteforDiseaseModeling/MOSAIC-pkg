# How many workers the two memory-heavy per-location figure families may use.
# Each worker rendering posterior-detail pages re-reads samples.parquet (~1.2 GB
# at 100,000 x 1,523) and each trajectory worker holds the exported
# trajectories_ensemble object (~79 MB at 40 locations). Neither cost scales
# with the number of locations, only with the number of workers, so the fan-out
# is capped independently of the calibration cluster's size -- an 80-worker
# production cluster would otherwise want ~96 GB for the detail pages alone.
.MOSAIC_DETAIL_MAX_WORKERS <- 8L

#' Unnest single-element lists produced by jsonlite
#'
#' \code{jsonlite::read_json(simplifyVector = FALSE)} wraps every scalar in a
#' length-1 list. This collapses those so the prior/posterior objects can be
#' indexed by name. Package-scoped so the renderer and
#' \code{plot_model_distributions()} parse identically -- it used to be a local
#' closure inside the latter, which made it unreachable from anywhere else.
#'
#' @param x A parsed JSON object.
#' @return \code{x} with single-element unnamed lists collapsed.
#' @noRd
.mosaic_unnest_json <- function(x) {
  if (is.list(x) && length(x) == 1 && !is.null(names(x))) return(x)
  if (is.list(x) && length(x) == 1) return(x[[1]])
  if (is.list(x)) return(lapply(x, .mosaic_unnest_json))
  x
}

#' Location codes carried by a parsed prior/posterior methods list
#'
#' Single source of truth for "which locations does this run estimate
#' per-location parameters for". \code{plot_model_distributions()} uses it to
#' decide what to draw; \code{render_MOSAIC_figures()} uses it to decide what to
#' fan out. Keeping one derivation avoids the lockstep-sibling drift of
#' CLAUDE.md lesson #11.
#'
#' @param methods_data Named list of unnested prior/posterior objects.
#' @return Character vector of ISO codes, or NULL when none are present.
#' @noRd
.mosaic_location_codes_from_methods <- function(methods_data) {
  for (method_data in methods_data) {
    pl <- method_data$parameters_location
    if (is.null(pl)) next
    for (param in names(pl)) {
      if (!is.null(pl[[param]]$location)) return(names(pl[[param]]$location))
    }
  }
  NULL
}

#' Location codes for a run, read from its posterior (and quantile) artifacts
#'
#' File-level wrapper over \code{.mosaic_location_codes_from_methods()}, unioned
#' with the location column of the quantiles CSV when one is given -- the
#' posterior-detail plots derive their location set from both sources, so both
#' must be represented or a location would silently never render.
#'
#' @param posteriors_file Path to the posteriors JSON, or NULL.
#' @param quantiles_file Optional path to the posterior quantiles CSV.
#' @return Character vector of ISO codes (possibly empty).
#' @noRd
.mosaic_posterior_location_codes <- function(posteriors_file, quantiles_file = NULL) {
  codes <- character(0)

  if (!is.null(posteriors_file) && file.exists(posteriors_file)) {
    obj <- tryCatch(
      .mosaic_unnest_json(jsonlite::read_json(posteriors_file, simplifyVector = FALSE)),
      error = function(e) {
        # Never silent: a swallowed failure here degrades to quantiles-only and
        # silently drops whole locations from the figure set.
        warning("could not parse ", posteriors_file, " for location codes: ",
                conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    if (!is.null(obj)) {
      from_json <- .mosaic_location_codes_from_methods(list(obj))
      if (!is.null(from_json)) codes <- union(codes, from_json)
    }
  }

  if (!is.null(quantiles_file) && file.exists(quantiles_file)) {
    q <- tryCatch(utils::read.csv(quantiles_file, stringsAsFactors = FALSE),
                  error = function(e) NULL)
    if (!is.null(q) && "location" %in% names(q)) {
      loc <- unique(q$location[!is.na(q$location) & nzchar(q$location)])
      codes <- union(codes, loc)
    }
  }

  codes
}

#' Build a per-location render worker in a minimal environment
#'
#' Defined at FILE scope on purpose. A closure created inside
#' \code{render_MOSAIC_figures()} would carry that function's frame as its
#' environment, and \code{parLapply} serialises a closure together with its
#' environment -- by the time figures render, that frame holds the ensemble and
#' trajectory objects, so every worker chunk would ship hundreds of megabytes.
#' Built here the closure's parent is the package namespace, so only
#' \code{fn_name} (a string), \code{args} (file paths and flags) and
#' \code{arg_name} travel.
#'
#' @param fn_name Name of the exported MOSAIC plotting function to call.
#' @param args Named list of arguments shared across locations.
#' @param arg_name Name of the argument that receives the per-location value.
#' @return A function of one argument suitable for \code{parLapplyLB}.
#' @noRd
.mosaic_mk_render_worker <- function(fn_name, args, arg_name) {
  force(fn_name); force(args); force(arg_name)
  function(el) {
    a <- args
    a[[arg_name]] <- el
    do.call(getExportedValue("MOSAIC", fn_name), a)
  }
}

#' Render one trajectory page from worker globals
#'
#' Takes \code{traj} and \code{out_traj} from the worker's global environment
#' (put there once per worker by \code{clusterExport}) rather than as arguments,
#' so the ~79 MB trajectories object is shipped once per worker instead of once
#' per page.
#'
#' @param location Location code to render.
#' @return Invisibly, whatever \code{plot_model_trajectories()} returns.
#' @noRd
.mosaic_traj_render_worker <- function(location) {
  plot_model_trajectories(
    trajectories = get("traj", envir = globalenv()),
    location     = location,
    output_dir   = get("out_traj", envir = globalenv()),
    verbose      = FALSE
  )
}

#' Render all MOSAIC figures from a finished run directory
#'
#' Reconstructs every \code{run_MOSAIC()} pipeline figure \strong{from the data
#' artifacts on disk} in a finished output directory, writing them into
#' \code{3_results/figures/**}. This is the visualization layer of the
#' modeling/visualization split: \code{run_MOSAIC()} writes a complete,
#' self-describing run directory (all numeric artifacts, ensemble \code{.rds}
#' objects, diagnostic CSVs) independent of plotting, and this function turns
#' that directory into figures. It can therefore be run post-hoc, on a different
#' machine, or repeatedly without re-running calibration.
#'
#' @section Pure read-render (no re-simulation):
#' This function \strong{never} calls \code{calc_model_ensemble()},
#' \code{run_simulation()}, or \code{sample_parameters()}. Ensemble plots are
#' reconstructed from the persisted \code{.rds} objects
#' (\code{2_calibration/ensemble_optimized.rds} or
#' \code{ensemble_candidate.rds}, and \code{medoid_ensemble.rds}). A missing,
#' corrupt, or schema-incompatible artifact causes the affected figure to be
#' \strong{warned-and-skipped}, never rebuilt — rebuilding would trigger local
#' simulation on the client (\code{calc_model_ensemble()} always simulates, via
#' PSOCK or sequentially), which this function deliberately avoids. Every figure
#' is wrapped in \code{tryCatch} so one failure never aborts the rest.
#'
#' @param dir_output Character. Path to a finished \code{run_MOSAIC()} output
#'   directory (the one containing \code{1_inputs/}, \code{2_calibration/},
#'   \code{3_results/}).
#' @param which Character vector selecting figure groups to render, or
#'   \code{NULL} (default) for all. Valid groups: \code{"convergence"},
#'   \code{"posterior"}, \code{"predictions"}, \code{"ppc"},
#'   \code{"sensitivity"}, \code{"psi_star"},
#'   \code{"spatial"}, \code{"trajectories"}.
#' @param plots Logical. Master switch. When \code{FALSE} the function returns
#'   immediately without rendering (mirrors \code{control$paths$plots}). Default
#'   \code{TRUE}.
#' @param verbose Logical. Print progress messages. Default \code{TRUE}.
#' @param cl Optional PSOCK cluster (from \code{\link{make_mosaic_cluster}}) used
#'   to render the three per-location figure families in parallel: the
#'   prior/posterior distributions, the per-category posterior detail pages, and
#'   the trajectory pages. A cluster passed here is \strong{borrowed, never
#'   stopped} -- the caller owns its lifecycle. One figure per worker process,
#'   each opening and closing its own graphics device, so no device is ever
#'   shared.
#' @param n_cores Integer. When \code{cl} is \code{NULL} and this is greater
#'   than 1, render builds its own PSOCK cluster of this size and stops it before
#'   returning. Capped at \code{.MOSAIC_DETAIL_MAX_WORKERS} for the two
#'   memory-heavy families regardless. Default \code{1L} (serial, unchanged).
#'
#' @section Parallel rendering:
#' At 40 locations this stage is the largest single-threaded block in a
#' production run: measured at 35.9 min of a 260-min 100,000-simulation run, of
#' which \code{plot_model_posteriors_detail()} alone was 20.0 min for 286 PDFs.
#' The work is embarrassingly parallel -- every page is an independent
#' \code{ggsave} to its own filename -- so passing \code{cl} or \code{n_cores}
#' divides it across workers. \code{run_MOSAIC()} passes \code{n_cores}: by the
#' time figures render, the calibration cluster has already been stopped (it goes
#' at \code{R/run_MOSAIC.R}, right after the calibration loop) and
#' \code{calc_model_ensemble()}'s own cluster has come and gone too, so there is
#' nothing left to borrow and nothing to contend with for R's 128-connection
#' ceiling.
#'
#' Per-location failures are isolated: a worker that errors on one location
#' produces a warning on the master and the remaining locations still render,
#' matching the serial path's per-figure \code{tryCatch}.
#'
#' @return Invisibly, a named logical vector indicating which figure groups were
#'   attempted (\code{TRUE}) vs skipped (\code{FALSE}).
#'
#' @seealso \code{\link{run_MOSAIC}} (writes the run directory),
#'   \code{\link{plot_model_ensemble}}, \code{\link{plot_model_ppc}}.
#'
#' @export
render_MOSAIC_figures <- function(dir_output,
                                  which   = NULL,
                                  plots   = TRUE,
                                  verbose = TRUE,
                                  cl      = NULL,
                                  n_cores = 1L) {

  if (!isTRUE(plots)) {
    if (verbose) message("render_MOSAIC_figures: plots = FALSE; nothing to render.")
    return(invisible(stats::setNames(logical(0), character(0))))
  }

  if (missing(dir_output) || is.null(dir_output) || !nzchar(dir_output))
    stop("dir_output is required")
  if (!dir.exists(dir_output))
    stop("dir_output does not exist: ", dir_output)

  valid_groups <- c("convergence", "posterior", "predictions", "ppc",
                    "sensitivity", "psi_star", "spatial",
                    "trajectories")
  if (is.null(which)) {
    which <- valid_groups
  } else {
    unknown <- setdiff(which, valid_groups)
    if (length(unknown))
      stop("Unknown figure group(s): ", paste(unknown, collapse = ", "),
           ". Valid: ", paste(valid_groups, collapse = ", "))
  }

  # Directory tree (creates any missing figures subdirs; clean_output = FALSE
  # so existing artifacts are never clobbered).
  dirs <- .mosaic_ensure_dir_tree(dir_output, clean_output = FALSE)

  .vmsg <- function(...) if (verbose) message(sprintf(...))

  # --- Cluster lifecycle -----------------------------------------------------
  # A cluster passed in is the caller's; one built here is ours to stop. Size is
  # capped at .MOSAIC_DETAIL_MAX_WORKERS because the two memory-heavy families
  # cannot use more than that anyway, so a bigger cluster would only cost
  # spin-up time and sockets.
  if (is.null(cl)) {
    n_cores <- suppressWarnings(as.integer(n_cores))
    if (length(n_cores) != 1L || is.na(n_cores)) n_cores <- 1L
    n_cores <- min(n_cores, .MOSAIC_DETAIL_MAX_WORKERS)
    if (n_cores > 1L) {
      # require_root = FALSE: rendering workers are handed explicit paths from the
      # run directory, so demanding set_root_directory() would make this function
      # unusable exactly where it is meant to shine -- post-hoc, on a machine
      # with no MOSAIC tree. Without it the cluster silently failed to start and
      # every render quietly ran serially.
      cl <- tryCatch(make_mosaic_cluster(n_cores = n_cores, type = "PSOCK",
                                         require_root = FALSE),
                     error = function(e) {
                       warning("render_MOSAIC_figures: could not start a cluster (",
                               conditionMessage(e), "); rendering serially.",
                               call. = FALSE)
                       NULL
                     })
      if (!is.null(cl)) {
        .vmsg("Rendering figures on %d workers", length(cl))
        on.exit(try(.mosaic_stop_cluster(cl), silent = TRUE), add = TRUE)
      }
    }
  }

  # --- Per-location dispatch -------------------------------------------------
  # Maps `f` over `x`, on `cl` when one was supplied and there is more than one
  # element to do. Errors are returned as strings rather than thrown, so one bad
  # location cannot abort the group (and a worker-side condition still surfaces
  # on the master, which is not true of a bare warning() inside a PSOCK worker).
  # parLapplyLB, not parLapply: per-location cost varies several-fold with how
  # many parameters a location estimates, so static chunking would leave workers
  # idle behind a long tail.
  .render_map <- function(x, f, label, on = cl) {
    if (!length(x)) return(invisible(NULL))
    wrapped <- function(el) tryCatch({ f(el); NULL },
                                     error = function(e) conditionMessage(e))
    res <- if (is.null(on) || length(on) < 2L || length(x) < 2L) {
      lapply(x, wrapped)
    } else {
      parallel::parLapplyLB(on, x, wrapped)
    }
    for (i in seq_along(res)) {
      if (!is.null(res[[i]]))
        warning(label, " failed for ", as.character(x[[i]])[1], ": ", res[[i]],
                call. = FALSE)
    }
    invisible(NULL)
  }

  # --- Schema-checked .rds loader (P5: load, never rebuild) -------------------
  .load_rds <- function(path, label) {
    if (!file.exists(path)) {
      warning(label, ": artifact not found (", path, "); skipping figure.",
              call. = FALSE)
      return(NULL)
    }
    obj <- tryCatch(readRDS(path), error = function(e) {
      warning(label, ": failed to read ", path, " (", conditionMessage(e),
              "); skipping figure.", call. = FALSE)
      NULL
    })
    if (is.null(obj)) return(NULL)
    sv <- .mosaic_artifact_schema_version(obj)
    if (!is.na(sv) && sv > .MOSAIC_ARTIFACT_SCHEMA_VERSION) {
      warning(label, ": artifact schema v", sv, " is newer than this package ",
              "(v", .MOSAIC_ARTIFACT_SCHEMA_VERSION, "); skipping figure.",
              call. = FALSE)
      return(NULL)
    }
    obj
  }

  # --- Resolve the canonical posterior ensemble .rds --------------------------
  # Prefer the optimized ensemble (canonical when optimize_subset succeeded),
  # falling back to the candidate (tier) ensemble.
  .resolve_ensemble_rds <- function() {
    opt  <- file.path(dirs$calibration, "ensemble_optimized.rds")
    cand <- file.path(dirs$calibration, "ensemble_candidate.rds")
    if (file.exists(opt)) opt else if (file.exists(cand)) cand else NA_character_
  }

  # --- Resolve subset_col from samples.parquet (no control needed) ------------
  .resolve_subset_col <- function() {
    sp <- file.path(dirs$calibration, "samples.parquet")
    if (!file.exists(sp)) return("is_best_subset")
    res <- tryCatch(arrow::read_parquet(sp), error = function(e) NULL)
    if (is.null(res)) return("is_best_subset")
    if ("is_best_subset_opt" %in% names(res) &&
        isTRUE(any(as.logical(res$is_best_subset_opt), na.rm = TRUE)))
      "is_best_subset_opt" else "is_best_subset"
  }

  # --- Resolve central_method from control.json (default median) --------------
  .resolve_central <- function() {
    cj <- file.path(dirs$inputs, "control.json")
    cm <- "median"
    if (file.exists(cj)) {
      ctrl <- tryCatch(jsonlite::fromJSON(cj, simplifyVector = TRUE),
                       error = function(e) NULL)
      v <- tryCatch(ctrl$predictions$central_method, error = function(e) NULL)
      if (!is.null(v) && length(v) >= 1L) cm <- v
    }
    .mosaic_resolve_central_method(cm)
  }

  files <- list(
    samples     = file.path(dirs$calibration, "samples.parquet"),
    priors      = file.path(dirs$inputs, "priors.json"),
    quantiles   = file.path(dirs$cal_posterior, "posterior_quantiles.csv"),
    posteriors  = file.path(dirs$cal_posterior, "posteriors.json"),
    wm_csv      = file.path(dirs$res_fig_diag, "model_fit_windows.csv")
  )

  subset_col <- .resolve_subset_col()
  weight_col <- if (identical(subset_col, "is_best_subset_opt")) "weight_best_opt" else "weight_best"
  central_method <- .resolve_central()

  attempted <- stats::setNames(logical(length(valid_groups)), valid_groups)

  # ===========================================================================
  # CONVERGENCE
  # ===========================================================================
  if ("convergence" %in% which) {
    attempted["convergence"] <- TRUE
    .vmsg("Rendering convergence figures...")

    tryCatch(
      plot_model_convergence_status(
        results_dir = dirs$cal_diag,
        plots_dir   = dirs$res_fig_diag,
        verbose     = verbose
      ),
      error = function(e) warning("convergence_status plot failed: ",
                                  conditionMessage(e), call. = FALSE)
    )

    if (file.exists(files$samples)) {
      tryCatch({
        results <- arrow::read_parquet(files$samples)
        plot_model_likelihood(results = results, output_dir = dirs$res_fig_diag,
                              verbose = verbose)
      }, error = function(e) warning("likelihood curve plot failed: ",
                                     conditionMessage(e), call. = FALSE))
    }

    # Windowed-metrics figure (from the unconditional CSV).
    if (file.exists(files$wm_csv)) {
      tryCatch({
        wm <- utils::read.csv(files$wm_csv, stringsAsFactors = FALSE)
        .mosaic_plot_windowed_metrics(
          wm, file.path(dirs$res_fig_diag, "model_fit_windows.png"))
      }, error = function(e) warning("windowed metrics plot failed: ",
                                     conditionMessage(e), call. = FALSE))
    }

    # Subset-optimization figure (from subset_opt.rds, when present).
    subset_opt <- .load_rds(file.path(dirs$calibration, "subset_opt.rds"),
                            "subset_optimization")
    if (!is.null(subset_opt)) {
      tryCatch(
        plot_model_subset_optimization(
          subset_opt  = subset_opt,
          output_dir  = dirs$res_fig_diag,
          file_prefix = "subset_optimization",
          verbose     = verbose
        ),
        error = function(e) warning("subset optimization plot failed: ",
                                    conditionMessage(e), call. = FALSE)
      )
    }
  }

  # ===========================================================================
  # POSTERIOR (quantiles, distributions, detail)
  # ===========================================================================
  if ("posterior" %in% which) {
    attempted["posterior"] <- TRUE
    .vmsg("Rendering posterior figures...")

    if (file.exists(files$quantiles)) {
      tryCatch(
        plot_model_posterior_quantiles(
          csv_files  = files$quantiles,
          output_dir = dirs$res_fig_post,
          verbose    = verbose
        ),
        error = function(e) warning("posterior quantiles plot failed: ",
                                    conditionMessage(e), call. = FALSE)
      )
    } else {
      warning("posterior quantiles: ", files$quantiles, " not found; skipping.",
              call. = FALSE)
    }

    if (file.exists(files$priors) && file.exists(files$posteriors)) {
      .dist_args <- list(json_files   = c(files$priors, files$posteriors),
                         method_names = c("Prior", "Posterior"),
                         output_dir   = dirs$res_fig_post)
      dist_locs <- tryCatch(
        .mosaic_posterior_location_codes(files$posteriors),
        error = function(e) NULL
      )
      if (is.null(cl) || length(cl) < 2L || is.null(dist_locs) || length(dist_locs) < 2L) {
        tryCatch(do.call(plot_model_distributions, .dist_args),
                 error = function(e) warning("distributions plot failed: ",
                                             conditionMessage(e), call. = FALSE))
      } else {
        # Global page first (it is a single figure, not per-location), then fan
        # the per-location pages out. `locations = character(0)` renders only the
        # global page; a location vector renders only those locations.
        tryCatch(do.call(plot_model_distributions,
                         c(.dist_args, list(locations = character(0)))),
                 error = function(e) warning("distributions plot failed (global): ",
                                             conditionMessage(e), call. = FALSE))
        .render_map(dist_locs,
                    .mosaic_mk_render_worker("plot_model_distributions",
                                             .dist_args, "locations"),
                    "distributions plot")
      }
    }

    if (file.exists(files$quantiles) && file.exists(files$samples) &&
        file.exists(files$priors)) {
      .det_args <- list(
        quantiles_file  = files$quantiles,
        results_file    = files$samples,
        priors_file     = files$priors,
        posteriors_file = if (file.exists(files$posteriors)) files$posteriors else NULL,
        output_dir      = dirs$res_fig_post_detail,
        subset_col      = subset_col,
        weight_col      = weight_col,
        verbose         = verbose
      )
      det_locs <- tryCatch(
        .mosaic_posterior_location_codes(files$posteriors, files$quantiles),
        error = function(e) NULL
      )
      # Each worker re-reads samples.parquet rather than receiving it: the read
      # is 0.2 s (measured at 100,000 x 1,523 on dugong) while the frame is
      # ~1.2 GB, so shipping it would cost far more than re-reading it. The
      # trade is worker MEMORY, which is why the fan-out is capped -- see
      # .MOSAIC_DETAIL_MAX_WORKERS.
      if (is.null(cl) || length(cl) < 2L || is.null(det_locs) || length(det_locs) < 2L) {
        tryCatch(do.call(plot_model_posteriors_detail, .det_args),
                 error = function(e) warning("posteriors detail plot failed: ",
                                             conditionMessage(e), call. = FALSE))
      } else {
        sub_cl <- cl[seq_len(min(length(cl), .MOSAIC_DETAIL_MAX_WORKERS))]
        tryCatch(do.call(plot_model_posteriors_detail,
                         c(.det_args, list(locations = character(0)))),
                 error = function(e) warning("posteriors detail plot failed (global): ",
                                             conditionMessage(e), call. = FALSE))
        .render_map(det_locs,
                    .mosaic_mk_render_worker("plot_model_posteriors_detail",
                                             .det_args, "locations"),
                    "posteriors detail plot", on = sub_cl)
      }
    }
  }

  # ===========================================================================
  # SENSITIVITY (HSIC + correlation)
  # ===========================================================================
  if ("sensitivity" %in% which) {
    attempted["sensitivity"] <- TRUE
    .vmsg("Rendering parameter sensitivity / correlation figures...")

    if (file.exists(files$samples)) {
      tryCatch(
        plot_model_parameter_sensitivity(
          results_file = files$samples,
          priors_file  = if (file.exists(files$priors)) files$priors else NULL,
          output_dir   = dirs$res_fig_diag,
          subset_col   = subset_col,
          verbose      = verbose
        ),
        error = function(e) warning("parameter sensitivity plot failed: ",
                                    conditionMessage(e), call. = FALSE)
      )
      tryCatch(
        plot_model_parameter_correlation(
          results_file = files$samples,
          priors_file  = if (file.exists(files$priors)) files$priors else NULL,
          output_dir   = dirs$res_fig_diag,
          subset_col   = subset_col,
          verbose      = verbose
        ),
        error = function(e) warning("parameter correlation plot failed: ",
                                    conditionMessage(e), call. = FALSE)
      )
    } else {
      warning("sensitivity: ", files$samples, " not found; skipping.",
              call. = FALSE)
    }
  }

  # ===========================================================================
  # PREDICTIONS (ensemble + medoid prediction plots from persisted .rds)
  # ===========================================================================
  if ("predictions" %in% which) {
    attempted["predictions"] <- TRUE
    .vmsg("Rendering prediction figures...")

    # Posterior ensemble.
    ens_rds <- .resolve_ensemble_rds()
    if (!is.na(ens_rds)) {
      ensemble <- .load_rds(ens_rds, "ensemble predictions")
      if (!is.null(ensemble) && inherits(ensemble, "mosaic_ensemble")) {
        tryCatch(
          plot_model_ensemble(
            ensemble       = ensemble,
            output_dir     = dirs$res_fig_pred,
            file_prefix    = "ensemble",
            title_label    = "Posterior Ensemble",
            central_method = central_method,
            verbose        = verbose
          ),
          error = function(e) warning("ensemble prediction plot failed: ",
                                      conditionMessage(e), call. = FALSE)
        )
      } else if (!is.null(ensemble)) {
        warning("ensemble predictions: ", basename(ens_rds),
                " is not a mosaic_ensemble; skipping.", call. = FALSE)
      }
    } else {
      warning("ensemble predictions: no ensemble .rds found; skipping.",
              call. = FALSE)
    }

    # Medoid.
    medoid_ensemble <- .load_rds(file.path(dirs$calibration, "medoid_ensemble.rds"),
                                 "medoid predictions")
    if (!is.null(medoid_ensemble) && inherits(medoid_ensemble, "mosaic_ensemble")) {
      tryCatch(
        plot_model_ensemble(
          ensemble       = medoid_ensemble,
          output_dir     = dirs$res_fig_pred,
          file_prefix    = "medoid",
          title_label    = "Medoid Model",
          central_method = central_method,
          verbose        = verbose
        ),
        error = function(e) warning("medoid prediction plot failed: ",
                                    conditionMessage(e), call. = FALSE)
      )
    } else if (!is.null(medoid_ensemble)) {
      warning("medoid predictions: medoid_ensemble.rds is not a mosaic_ensemble; skipping.",
              call. = FALSE)
    }
  }

  # ===========================================================================
  # PPC (posterior predictive checks from the prediction CSVs)
  # ===========================================================================
  if ("ppc" %in% which) {
    attempted["ppc"] <- TRUE
    .vmsg("Rendering posterior predictive check figures...")

    pred_csvs <- list.files(dirs$res_predictions, pattern = "\\.csv$")
    if (length(pred_csvs) > 0L) {
      tryCatch(
        plot_model_ppc(
          predictions_dir = dirs$res_predictions,
          output_dir      = dirs$res_figures,
          verbose         = verbose
        ),
        error = function(e) {
          if (grepl("unused argument", conditionMessage(e))) {
            warning("plot_model_ppc using legacy signature; skipping.", call. = FALSE)
          } else {
            warning("PPC plot failed: ", conditionMessage(e), call. = FALSE)
          }
        }
      )
    } else {
      warning("ppc: no prediction CSVs in ", dirs$res_predictions, "; skipping.",
              call. = FALSE)
    }
  }

  # ===========================================================================
  # PSI_STAR (raw LSTM psi vs calibrated psi*)
  # ===========================================================================
  if ("psi_star" %in% which) {
    attempted["psi_star"] <- TRUE
    .vmsg("Rendering psi_star diagnostic figures...")

    # Derive the location list from config.json (avoids needing it passed in).
    location_names <- NULL
    cj <- file.path(dirs$inputs, "config.json")
    if (file.exists(cj)) {
      cfg <- tryCatch(jsonlite::fromJSON(cj, simplifyVector = TRUE),
                      error = function(e) NULL)
      if (!is.null(cfg)) location_names <- cfg$location_name %||% cfg$location
    }

    if (!is.null(location_names) && length(location_names) > 0L) {
      PATHS <- tryCatch(get_paths(), error = function(e) NULL)
      if (!is.null(PATHS)) {
        tryCatch(
          plot_psi_star_diagnostic(
            dirs           = dirs,
            PATHS          = PATHS,
            location_names = as.character(location_names),
            verbose        = verbose
          ),
          error = function(e) warning("psi_star diagnostic plot failed: ",
                                      conditionMessage(e), call. = FALSE)
        )
      } else {
        warning("psi_star: get_paths() unavailable; skipping.", call. = FALSE)
      }
    } else {
      warning("psi_star: no location_name in config.json; skipping.", call. = FALSE)
    }
  }

  # ===========================================================================
  # SPATIAL (mobility figs 1-4 from config.json; hazard/coupling figs 5-6 from
  # persisted engine arrays). Pure read-render (P5): config + .rds + a packaged
  # basemap only -- never a simulation or a GeoBoundaries API call.
  # ===========================================================================
  if ("spatial" %in% which) {
    attempted["spatial"] <- TRUE
    .vmsg("Rendering spatial-structure figures...")

    out_dir <- dirs$res_fig_spatial
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    .save_fig <- function(p, file, width = 8, height = 6) {
      tryCatch(
        ggplot2::ggsave(file.path(out_dir, file), plot = p,
                        width = width, height = height, dpi = 200),
        error = function(e) warning("spatial: failed to save ", file, " (",
                                    conditionMessage(e), ")", call. = FALSE))
    }

    # --- Load config.json -----------------------------------------------------
    cfg <- NULL
    cj <- file.path(dirs$inputs, "config.json")
    if (file.exists(cj)) {
      cfg <- tryCatch(jsonlite::fromJSON(cj, simplifyVector = TRUE),
                      error = function(e) NULL)
    }

    if (is.null(cfg)) {
      warning("spatial: 1_inputs/config.json missing or unreadable; ",
              "skipping mobility figures.", call. = FALSE)
    } else {
      mf <- tryCatch(calc_mobility_flux(cfg), error = function(e) {
        warning("spatial: calc_mobility_flux failed (", conditionMessage(e),
                "); skipping mobility figures.", call. = FALSE)
        NULL
      })

      if (!is.null(mf)) {
        # Fig 1: pi_ij. Prefer the persisted ENGINE pi_ij over the R recompute
        # when present (F4); reconcile its diagonal to NA for display.
        pi_disp <- mf$pi
        pij_art <- .load_rds(file.path(dirs$calibration, "pi_ij_ensemble.rds"),
                             "engine pi_ij")
        if (!is.null(pij_art) && is.list(pij_art) && !is.null(pij_art$array)) {
          pe <- pij_art$array
          if (is.matrix(pe) && all(dim(pe) == dim(mf$pi))) {
            diag(pe) <- NA_real_
            ln <- pij_art$location_name %||% mf$location_name
            dimnames(pe) <- list(ln, ln)
            pi_disp <- pe
          }
        }
        pi_lat <- mf$coords[, "latitude"]
        tryCatch(.save_fig(plot_diffusion_pi(pi_disp,
                                             rownames(pi_disp) %||% mf$location_name,
                                             latitude = pi_lat),
                           "diffusion_pi.png", width = 9, height = 9),
                 error = function(e) warning("spatial: diffusion_pi failed: ",
                                             conditionMessage(e), call. = FALSE))

        # Fig 2: departure tau (+ CI if artifact present).
        tau_ci <- NULL
        tau_ci_file <- file.path(dirs$inputs, "mobility_tau_ci.csv")
        if (file.exists(tau_ci_file)) {
          tau_ci <- tryCatch(utils::read.csv(tau_ci_file, stringsAsFactors = FALSE),
                             error = function(e) NULL)
        } else {
          warning("spatial: mobility_tau_ci.csv not found; tau plot is ",
                  "point-only.", call. = FALSE)
        }
        tryCatch(.save_fig(plot_departure_tau(mf$tau, mf$N, mf$location_name, ci = tau_ci),
                           "departure_tau.png", width = 7, height = 9),
                 error = function(e) warning("spatial: departure_tau failed: ",
                                             conditionMessage(e), call. = FALSE))

        # Fig 3: modeled flux matrix.
        tryCatch(.save_fig(plot_mobility_flux_matrix(mf$flux, mf$location_name,
                                                     latitude = mf$coords[, "latitude"]),
                           "mobility_flux_matrix.png", width = 9, height = 9),
                 error = function(e) warning("spatial: flux_matrix failed: ",
                                             conditionMessage(e), call. = FALSE))

        # Fig 4: mobility network over the packaged Africa basemap (or
        # centroid-only when no basemap matches). Never an API call (P5).
        basemap <- .mosaic_load_spatial_basemap(mf$location_name, verbose)
        tryCatch(.save_fig(plot_mobility_flux_network(mf$flux, mf$coords,
                                                      mf$location_name,
                                                      basemap = basemap),
                           "mobility_flux_network.png", width = 9, height = 9),
                 error = function(e) warning("spatial: flux_network failed: ",
                                             conditionMessage(e), call. = FALSE))
      }
    }

    # --- Fig 5: spatial hazard (engine array, element-wise median) ------------
    sh_art <- .load_rds(file.path(dirs$calibration, "spatial_hazard_ensemble.rds"),
                        "spatial hazard")
    if (!is.null(sh_art) && is.list(sh_art) && !is.null(sh_art$array)) {
      H <- sh_art$array
      if (is.matrix(H)) {
        if (is.null(rownames(H)) && !is.null(sh_art$location_name) &&
            nrow(H) == length(sh_art$location_name))
          rownames(H) <- sh_art$location_name
        tryCatch(.save_fig(plot_spatial_hazard(H), "spatial_hazard.png",
                           width = 10, height = 7),
                 error = function(e) warning("spatial: hazard plot failed: ",
                                             conditionMessage(e), call. = FALSE))
      }
    }

    # --- Fig 6: coupling C_ij (engine array, element-wise median; NaN-masked) -
    cpl_art <- .load_rds(file.path(dirs$calibration, "coupling_ensemble.rds"),
                         "coupling")
    if (!is.null(cpl_art) && is.list(cpl_art) && !is.null(cpl_art$array)) {
      C <- cpl_art$array
      if (is.matrix(C)) {
        if (is.null(rownames(C)) && !is.null(cpl_art$location_name) &&
            nrow(C) == length(cpl_art$location_name))
          dimnames(C) <- list(cpl_art$location_name, cpl_art$location_name)
        tryCatch(.save_fig(plot_spatial_correlation_heatmap(C),
                           "spatial_coupling.png", width = 7, height = 6),
                 error = function(e) warning("spatial: coupling plot failed: ",
                                             conditionMessage(e), call. = FALSE))
      }
    }
  }

  # ===========================================================================
  # TRAJECTORIES (comprehensive internal-state channels: compartments, FOI,
  # incidence, burden + derived). Pure read-render (P5): loads the persisted
  # trajectories_ensemble.rds and renders one figure per location -- never a
  # engine replay. Warned-and-skipped when the artifact is absent (capture was
  # off, or the run predates the feature / an old worker image dropped it).
  # ===========================================================================
  if ("trajectories" %in% which) {
    attempted["trajectories"] <- TRUE
    .vmsg("Rendering trajectory figures...")

    traj <- .load_rds(file.path(dirs$calibration, "trajectories_ensemble.rds"),
                      "trajectories")
    if (!is.null(traj) && inherits(traj, "mosaic_trajectories")) {
      locs <- traj$location_names
      if (is.null(locs) || !length(locs)) {
        warning("trajectories: artifact carries no location_names; skipping.",
                call. = FALSE)
      } else {
        # Every page needs the whole `traj` object, so unlike the two families
        # above it is exported rather than re-read -- ONCE per worker via
        # clusterExport, never once per page. The artifact is ~79 MB at 40
        # locations x 100k, so the fan-out is capped the same way the detail
        # pages are.
        out_traj <- dirs$res_fig_trajectories
        traj_cl  <- if (is.null(cl)) NULL
                    else cl[seq_len(min(length(cl), .MOSAIC_DETAIL_MAX_WORKERS))]
        if (!is.null(traj_cl) && length(traj_cl) > 1L && length(locs) > 1L) {
          .traj_env <- new.env(parent = emptyenv())
          assign("traj",     traj,     envir = .traj_env)
          assign("out_traj", out_traj, envir = .traj_env)
          parallel::clusterExport(traj_cl, c("traj", "out_traj"), envir = .traj_env)
          # Reparent to globalenv() before dispatch. R serialises a function
          # that is a NAMESPACE BINDING by reference -- it sends the name, and
          # the worker looks it up in ITS MOSAIC, which is whatever library()
          # loaded. An internal added in this version is then "object not found"
          # on any worker running a different build. Reparenting ships the body
          # by value instead, and its `plot_model_trajectories` call resolves
          # through the worker's search path. Same reason .mosaic_run_batch()
          # does this to its worker_func.
          traj_worker <- .mosaic_traj_render_worker
          environment(traj_worker) <- globalenv()
          .render_map(locs, traj_worker, "trajectories plot", on = traj_cl)
        } else {
          for (loc in locs) {
            tryCatch(
              plot_model_trajectories(
                trajectories = traj,
                location     = loc,
                output_dir   = out_traj,
                verbose      = verbose
              ),
              error = function(e) warning("trajectories plot failed for ", loc,
                                          ": ", conditionMessage(e), call. = FALSE)
            )
          }
        }
      }
    } else if (!is.null(traj)) {
      warning("trajectories: trajectories_ensemble.rds is not a ",
              "mosaic_trajectories; skipping.", call. = FALSE)
    }
  }

  if (verbose) message("render_MOSAIC_figures complete.")
  invisible(attempted)
}

#' Load the spatial-figure network basemap (Natural Earth, offline)
#'
#' Returns an \pkg{sf} polygon layer for the mobility-network backdrop, or
#' \code{NULL} for the centroid-only fallback. Uses
#' \code{rnaturalearth::ne_countries(continent = "Africa", scale = 50)}, which is
#' served \strong{offline} from the bundled \pkg{rnaturalearthdata} (public
#' domain) — it never downloads and never calls a GeoBoundaries /
#' \code{get_country_shp()} API (P5). The \strong{full} continent is returned
#' (all African countries) so neighbors render as reference outlines; the plotter
#' (\code{\link{plot_mobility_flux_network}}) crops the view to the network
#' countries' bounding box. The Natural Earth \code{iso_a3} field is exposed as
#' \code{iso3}. Returns \code{NULL} (centroid-only fallback) when \pkg{sf} or
#' \pkg{rnaturalearth} is unavailable or the layer cannot be built.
#'
#' @param location_name Character vector of the run's locations (config order).
#'   Retained for signature stability; the full basemap is returned regardless.
#' @param verbose Logical.
#' @return An \pkg{sf} object (the full Africa basemap with an \code{iso3}
#'   column), or \code{NULL}.
#' @noRd
.mosaic_load_spatial_basemap <- function(location_name, verbose = TRUE) {
  if (!requireNamespace("sf", quietly = TRUE) ||
      !requireNamespace("rnaturalearth", quietly = TRUE)) {
    if (verbose)
      message("spatial: 'sf'/'rnaturalearth' unavailable; centroid-only network.")
    return(NULL)
  }
  bm <- tryCatch(
    rnaturalearth::ne_countries(continent = "Africa", scale = 50,
                                returnclass = "sf"),
    error = function(e) NULL)
  if (is.null(bm) || !nrow(bm)) return(NULL)

  # Expose the Natural Earth ISO3 ("iso_a3") as `iso3` for the plotter's crop.
  # iso_a3 covers every African country incl. SSD/TZA/RWA (adm0_a3 does not).
  iso_field <- intersect(c("iso_a3", "iso_a3_eh"), names(bm))[1]
  if (is.na(iso_field)) return(NULL)
  bm$iso3 <- as.character(bm[[iso_field]])
  bm[, "iso3"]
}
