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
#' \code{run_LASER()}, or \code{sample_parameters()}. Ensemble plots are
#' reconstructed from the persisted \code{.rds} objects
#' (\code{2_calibration/ensemble_optimized.rds} or
#' \code{ensemble_candidate.rds}, and \code{medoid_ensemble.rds}). A missing,
#' corrupt, or schema-incompatible artifact causes the affected figure to be
#' \strong{warned-and-skipped}, never rebuilt — rebuilding would trigger local
#' simulation on the client (\code{calc_model_ensemble(precomputed_results =
#' NULL)} falls back to PSOCK/sequential), which this function deliberately
#' avoids. Every figure is wrapped in \code{tryCatch} so one failure never
#' aborts the rest.
#'
#' @param dir_output Character. Path to a finished \code{run_MOSAIC()} output
#'   directory (the one containing \code{1_inputs/}, \code{2_calibration/},
#'   \code{3_results/}).
#' @param which Character vector selecting figure groups to render, or
#'   \code{NULL} (default) for all. Valid groups: \code{"convergence"},
#'   \code{"posterior"}, \code{"predictions"}, \code{"ppc"},
#'   \code{"sensitivity"}, \code{"psi_star"}, \code{"parameters"},
#'   \code{"spatial"}.
#' @param plots Logical. Master switch. When \code{FALSE} the function returns
#'   immediately without rendering (mirrors \code{control$paths$plots}). Default
#'   \code{TRUE}.
#' @param verbose Logical. Print progress messages. Default \code{TRUE}.
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
                                  verbose = TRUE) {

  if (!isTRUE(plots)) {
    if (verbose) message("render_MOSAIC_figures: plots = FALSE; nothing to render.")
    return(invisible(stats::setNames(logical(0), character(0))))
  }

  if (missing(dir_output) || is.null(dir_output) || !nzchar(dir_output))
    stop("dir_output is required")
  if (!dir.exists(dir_output))
    stop("dir_output does not exist: ", dir_output)

  valid_groups <- c("convergence", "posterior", "predictions", "ppc",
                    "sensitivity", "psi_star", "parameters", "spatial")
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
      tryCatch(
        plot_model_distributions(
          json_files   = c(files$priors, files$posteriors),
          method_names = c("Prior", "Posterior"),
          output_dir   = dirs$res_fig_post
        ),
        error = function(e) warning("distributions plot failed: ",
                                    conditionMessage(e), call. = FALSE)
      )
    }

    if (file.exists(files$quantiles) && file.exists(files$samples) &&
        file.exists(files$priors)) {
      tryCatch(
        plot_model_posteriors_detail(
          quantiles_file  = files$quantiles,
          results_file    = files$samples,
          priors_file     = files$priors,
          posteriors_file = if (file.exists(files$posteriors)) files$posteriors else NULL,
          output_dir      = dirs$res_fig_post_detail,
          subset_col      = subset_col,
          weight_col      = weight_col,
          verbose         = verbose
        ),
        error = function(e) warning("posteriors detail plot failed: ",
                                    conditionMessage(e), call. = FALSE)
      )
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
  # PARAMETERS (parameter-vs-likelihood scatter; wired-in orphan, 2d)
  # ===========================================================================
  if ("parameters" %in% which) {
    attempted["parameters"] <- TRUE
    .vmsg("Rendering parameter-vs-likelihood figures...")

    if (file.exists(files$samples)) {
      tryCatch({
        results <- arrow::read_parquet(files$samples)
        plot_model_parameters(results = results, output_dir = dirs$res_fig_post,
                              verbose = verbose)
      }, error = function(e) warning("parameter-vs-likelihood plot failed: ",
                                     conditionMessage(e), call. = FALSE))
    } else {
      warning("parameters: ", files$samples, " not found; skipping.",
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
