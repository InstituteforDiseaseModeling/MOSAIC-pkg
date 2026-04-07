#' Update Priors from Posteriors for Staged Estimation
#'
#' Merges posterior distributions from a completed calibration stage into a
#' complete priors object, producing a new priors object suitable as input to
#' the next calibration stage.
#'
#' @param priors Complete priors object (list) or path to a priors JSON file.
#' @param posteriors Posteriors object (list) or path to a posteriors JSON file
#'   (as produced by \code{calc_model_posterior_distributions}).
#' @param verbose Logical. Print merge diagnostics (default TRUE).
#'
#' @return A complete priors object (list) with identical top-level structure to
#'   the input \code{priors}. Posterior fits replace their corresponding prior
#'   entries; all other entries are preserved unchanged.
#'
#' @details
#' The posteriors object produced by \code{calc_model_posterior_distributions}
#' is \emph{pruned}: it only contains parameters that were sampled and
#' successfully fitted. Parameters that were frozen (not sampled) are removed
#' entirely. A valid priors object for \code{sample_parameters} must be
#' \emph{complete} --- it must have entries for every parameter that could be
#' sampled. This function merges the two: posterior fits replace their
#' corresponding priors, everything else is preserved from the original priors.
#'
#' \strong{Merge rules (per parameter):}
#' \itemize{
#'   \item Posterior has a fitted distribution (beta, gamma, lognormal, normal,
#'     truncnorm, uniform, gompertz): \strong{replace} the prior.
#'   \item Posterior has \code{"fixed"} distribution: \strong{replace} the
#'     prior. The parameter had zero posterior variance and will return a
#'     constant when sampled.
#'   \item Posterior has \code{"failed"} distribution: \strong{keep the
#'     original prior}. The fitting failed, so the prior is more informative
#'     than a failure marker.
#'   \item Parameter absent from posteriors: \strong{keep the original prior}.
#'     It was not sampled in this stage.
#' }
#'
#' The function also strips any residual fitted-diagnostic metadata
#' (e.g. \code{fitted_mode}, \code{fitted_ci}) from posterior parameter
#' entries, keeping only the canonical distribution fields that
#' \code{sample_from_prior} reads.
#'
#' @section Validation:
#' Before returning, the function checks that:
#' \itemize{
#'   \item The output has \code{metadata}, \code{parameters_global}, and
#'     \code{parameters_location} top-level slots.
#'   \item Every parameter entry has \code{distribution} (character) and
#'     \code{parameters} (list) fields.
#'   \item No \code{"failed"} distribution markers remain (they are replaced
#'     with original priors).
#'   \item Global parameter count matches the original priors.
#'   \item Location-specific parameter groups and their location counts match.
#' }
#'
#' @examples
#' \dontrun{
#' # After a completed calibration stage
#' priors <- jsonlite::read_json("1_inputs/priors.json")
#' posteriors <- jsonlite::read_json("2_calibration/posterior/posteriors.json")
#'
#' # Merge posteriors into priors for the next stage
#' updated_priors <- update_priors_from_posteriors(priors, posteriors)
#'
#' # Use in next calibration stage
#' run_MOSAIC(priors = updated_priors, ...)
#' }
#'
#' @export
update_priors_from_posteriors <- function(priors, posteriors, verbose = TRUE) {

  # ---------------------------------------------------------------------------
  # Load from file paths if needed; capture paths for provenance
  # ---------------------------------------------------------------------------
  priors_path <- NULL
  posteriors_path <- NULL

  if (is.character(priors) && length(priors) == 1L) {
    if (!file.exists(priors)) stop("Priors file not found: ", priors)
    if (verbose) message("Loading priors from: ", priors)
    priors_path <- priors
    priors <- jsonlite::read_json(priors)
  }
  if (is.character(posteriors) && length(posteriors) == 1L) {
    if (!file.exists(posteriors)) stop("Posteriors file not found: ", posteriors)
    if (verbose) message("Loading posteriors from: ", posteriors)
    posteriors_path <- posteriors
    posteriors <- jsonlite::read_json(posteriors)
  }

  # ---------------------------------------------------------------------------
  # Validate input structure
  # ---------------------------------------------------------------------------
  .validate_priors_structure(priors, label = "priors")
  .validate_posteriors_structure(posteriors, label = "posteriors")

  # ---------------------------------------------------------------------------
  # Canonical distribution parameter fields (shared with calc_model_posterior_distributions)
  # ---------------------------------------------------------------------------
  dist_core_fields <- .mosaic_dist_core_fields()

  # ---------------------------------------------------------------------------
  # Start from a deep copy of priors (base of the output)
  # ---------------------------------------------------------------------------
  updated <- priors

  # Update metadata — full provenance chain so that any downstream consumer
  # can trace exactly which priors and posteriors were combined.
  # Priority: file path passed by caller > path embedded in metadata > version/date > "in-memory"
  prior_id <- priors_path %||%
              posteriors$metadata$source_priors %||%
              priors$metadata$version %||%
              "in-memory list"
  posterior_id <- posteriors_path %||%
                  posteriors$metadata$source_quantiles %||%
                  posteriors$metadata$date %||%
                  "in-memory list"

  updated$metadata$description <- "Priors updated from posterior distributions (staged estimation)"
  updated$metadata$date <- as.character(Sys.Date())
  updated$metadata$source_priors <- prior_id
  updated$metadata$source_posteriors <- posterior_id

  # ---------------------------------------------------------------------------
  # Merge global parameters
  # ---------------------------------------------------------------------------
  n_replaced <- 0L
  n_failed_kept <- 0L
  n_skipped <- 0L
  failed_kept_params <- character()

  post_global <- posteriors$parameters_global
  if (!is.null(post_global) && length(post_global) > 0) {
    for (param_name in names(post_global)) {
      post_entry <- post_global[[param_name]]
      dist_type <- post_entry$distribution

      if (is.null(dist_type)) {
        if (verbose) message("  [SKIP] ", param_name, " (global): no distribution field")
        n_skipped <- n_skipped + 1L
        next
      }

      # Failed or frozen posteriors: keep original prior
      if (dist_type %in% c("failed", "frozen")) {
        n_failed_kept <- n_failed_kept + 1L
        failed_kept_params <- c(failed_kept_params, param_name)
        reason <- if (dist_type == "frozen") "frozen in previous stage" else "posterior fitting failed"
        if (verbose) {
          message("  [KEEP PRIOR] ", param_name, " (global): ", reason)
        }
        next
      }

      # Check target exists in priors
      if (is.null(updated$parameters_global[[param_name]])) {
        if (verbose) message("  [SKIP] ", param_name, " (global): not in original priors")
        n_skipped <- n_skipped + 1L
        next
      }

      # Guard: reject distribution family changes (except when prior is uniform).
      # A posterior fitted as e.g. gompertz replacing a lognormal prior can
      # produce pathological parameter values after further processing
      # (inflate_priors, staged estimation). Only uniform priors are allowed to
      # change family because they have no informative shape and any fitted
      # continuous distribution is an improvement.
      prior_dist <- updated$parameters_global[[param_name]]$distribution
      if (!is.null(prior_dist) &&
          !identical(tolower(prior_dist), "uniform") &&
          !identical(tolower(dist_type), tolower(prior_dist))) {
        if (verbose) message("  [KEEP PRIOR] ", param_name,
                             " (global): posterior family '", dist_type,
                             "' differs from prior family '", prior_dist, "'")
        n_failed_kept <- n_failed_kept + 1L
        failed_kept_params <- c(failed_kept_params, param_name)
        next
      }

      # Clean parameters: keep only canonical fields
      clean_entry <- .clean_posterior_entry(post_entry, dist_core_fields)
      updated$parameters_global[[param_name]] <- clean_entry
      n_replaced <- n_replaced + 1L
    }
  }

  if (verbose) {
    message(sprintf("Global: %d replaced, %d kept (frozen/failed/family-mismatch), %d skipped",
                    n_replaced, n_failed_kept, n_skipped))
  }

  # ---------------------------------------------------------------------------
  # Merge location-specific parameters
  # ---------------------------------------------------------------------------
  n_loc_replaced <- 0L
  n_loc_failed_kept <- 0L
  n_loc_skipped <- 0L
  loc_failed_kept_params <- character()

  post_location <- posteriors$parameters_location
  if (!is.null(post_location) && length(post_location) > 0) {
    for (param_base in names(post_location)) {
      post_param <- post_location[[param_base]]
      post_locations <- post_param$location

      if (is.null(post_locations) || length(post_locations) == 0) {
        n_loc_skipped <- n_loc_skipped + 1L
        next
      }

      # Check target parameter group exists in priors
      if (is.null(updated$parameters_location[[param_base]])) {
        if (verbose) {
          message("  [SKIP] ", param_base, " (location): not in original priors")
        }
        n_loc_skipped <- n_loc_skipped + length(post_locations)
        next
      }

      for (iso in names(post_locations)) {
        post_entry <- post_locations[[iso]]
        dist_type <- post_entry$distribution

        if (is.null(dist_type)) {
          n_loc_skipped <- n_loc_skipped + 1L
          next
        }

        # Failed or frozen posteriors: keep original prior
        if (dist_type %in% c("failed", "frozen")) {
          n_loc_failed_kept <- n_loc_failed_kept + 1L
          loc_failed_kept_params <- c(loc_failed_kept_params, paste0(param_base, "_", iso))
          reason <- if (dist_type == "frozen") "frozen in previous stage" else "posterior fitting failed"
          if (verbose) {
            message("  [KEEP PRIOR] ", param_base, "_", iso, ": ", reason)
          }
          next
        }

        # Check target location exists in priors
        if (is.null(updated$parameters_location[[param_base]]$location[[iso]])) {
          if (verbose) {
            message("  [SKIP] ", param_base, "_", iso, ": not in original priors")
          }
          n_loc_skipped <- n_loc_skipped + 1L
          next
        }

        # Guard: reject distribution family changes (except when prior is uniform)
        prior_dist_loc <- updated$parameters_location[[param_base]]$location[[iso]]$distribution
        if (!is.null(prior_dist_loc) &&
            !identical(tolower(prior_dist_loc), "uniform") &&
            !identical(tolower(dist_type), tolower(prior_dist_loc))) {
          if (verbose) message("  [KEEP PRIOR] ", param_base, "_", iso,
                               ": posterior family '", dist_type,
                               "' differs from prior family '", prior_dist_loc, "'")
          n_loc_failed_kept <- n_loc_failed_kept + 1L
          loc_failed_kept_params <- c(loc_failed_kept_params, paste0(param_base, "_", iso))
          next
        }

        # Clean and replace
        clean_entry <- .clean_posterior_entry(post_entry, dist_core_fields)
        updated$parameters_location[[param_base]]$location[[iso]] <- clean_entry
        n_loc_replaced <- n_loc_replaced + 1L
      }
    }
  }

  if (verbose) {
    message(sprintf("Location: %d replaced, %d kept (frozen/failed/family-mismatch), %d skipped",
                    n_loc_replaced, n_loc_failed_kept, n_loc_skipped))
  }

  # ---------------------------------------------------------------------------
  # Record merge counts in metadata for downstream audit
  # ---------------------------------------------------------------------------
  total_replaced <- n_replaced + n_loc_replaced
  total_failed <- n_failed_kept + n_loc_failed_kept
  all_failed_params <- c(failed_kept_params, loc_failed_kept_params)

  updated$metadata$merge_summary <- list(
    n_replaced        = total_replaced,
    n_failed_kept     = total_failed,
    n_skipped         = n_skipped + n_loc_skipped,
    failed_parameters = if (total_failed > 0) all_failed_params else list()
  )

  # ---------------------------------------------------------------------------
  # Validate output integrity
  # ---------------------------------------------------------------------------
  .validate_updated_priors(updated, priors, verbose = verbose)

  if (verbose) {
    message(sprintf("\nSummary: %d posteriors merged, %d failures reverted to prior, %d skipped",
                    total_replaced, total_failed, n_skipped + n_loc_skipped))
    if (total_failed > 0) {
      message("  Failed parameters (original prior kept): ",
              paste(all_failed_params, collapse = ", "))
    }
  }

  updated
}


# =============================================================================
# Internal helpers
# =============================================================================

#' Clean a posterior entry to canonical form
#'
#' Strips fitted-diagnostic metadata from the parameters list, keeping only
#' the fields that \code{sample_from_prior} reads.
#'
#' @param entry List with \code{distribution} and \code{parameters} fields.
#' @param core_fields Named list mapping distribution type to canonical field names.
#' @return Cleaned entry with only \code{distribution} and \code{parameters}.
#' @noRd
.clean_posterior_entry <- function(entry, core_fields) {
  dist_type <- entry$distribution
  params <- entry$parameters

  # Look up canonical fields for this distribution type
  keep <- core_fields[[dist_type]]
  if (!is.null(keep) && !is.null(params)) {
    params <- params[intersect(names(params), keep)]
  }

  list(distribution = dist_type, parameters = params)
}


#' Validate priors structure
#' @noRd
.validate_priors_structure <- function(obj, label = "priors") {
  if (!is.list(obj)) {
    stop(label, " must be a list")
  }
  required <- c("parameters_global", "parameters_location")
  missing <- setdiff(required, names(obj))
  if (length(missing) > 0) {
    stop(label, " is missing required top-level slots: ",
         paste(missing, collapse = ", "))
  }
}


#' Validate posteriors structure
#' @noRd
.validate_posteriors_structure <- function(obj, label = "posteriors") {
  if (!is.list(obj)) {
    stop(label, " must be a list")
  }
  # Posteriors must have at least one of the parameter sections
  has_global <- !is.null(obj$parameters_global) && length(obj$parameters_global) > 0
  has_location <- !is.null(obj$parameters_location) && length(obj$parameters_location) > 0
  if (!has_global && !has_location) {
    stop(label, " has no parameters (both parameters_global and parameters_location are empty)")
  }
}


#' Validate the merged output against the original priors
#'
#' Checks structural integrity: parameter counts, no failed markers, valid entries.
#' @noRd
.validate_updated_priors <- function(updated, original, verbose = TRUE) {
  errors <- character()

  # Check top-level slots
  for (slot in c("metadata", "parameters_global", "parameters_location")) {
    if (is.null(updated[[slot]])) {
      errors <- c(errors, sprintf("Missing top-level slot: %s", slot))
    }
  }

  # Check global parameter count matches
  n_orig_global <- length(original$parameters_global)
  n_upd_global <- length(updated$parameters_global)
  if (n_orig_global != n_upd_global) {
    errors <- c(errors, sprintf(
      "Global parameter count mismatch: original has %d, updated has %d",
      n_orig_global, n_upd_global
    ))
  }

  # Check location parameter groups match
  orig_loc_names <- sort(names(original$parameters_location))
  upd_loc_names <- sort(names(updated$parameters_location))
  if (!identical(orig_loc_names, upd_loc_names)) {
    missing_groups <- setdiff(orig_loc_names, upd_loc_names)
    extra_groups <- setdiff(upd_loc_names, orig_loc_names)
    if (length(missing_groups) > 0) {
      errors <- c(errors, sprintf("Missing location parameter groups: %s",
                                  paste(missing_groups, collapse = ", ")))
    }
    if (length(extra_groups) > 0) {
      errors <- c(errors, sprintf("Extra location parameter groups: %s",
                                  paste(extra_groups, collapse = ", ")))
    }
  }

  # Check no "failed" or "frozen" markers leaked through
  non_prior_types <- c("failed", "frozen")
  for (param_name in names(updated$parameters_global)) {
    entry <- updated$parameters_global[[param_name]]
    if (is.list(entry) && entry$distribution %in% non_prior_types) {
      errors <- c(errors, sprintf(
        "%s marker leaked into output: parameters_global$%s",
        entry$distribution, param_name))
    }
  }
  for (param_base in names(updated$parameters_location)) {
    locs <- updated$parameters_location[[param_base]]$location
    for (iso in names(locs)) {
      entry <- locs[[iso]]
      if (is.list(entry) && entry$distribution %in% non_prior_types) {
        errors <- c(errors, sprintf(
          "%s marker leaked into output: parameters_location$%s$location$%s",
          entry$distribution, param_base, iso))
      }
    }
  }

  # Check every entry has distribution + parameters
  for (param_name in names(updated$parameters_global)) {
    entry <- updated$parameters_global[[param_name]]
    if (!is.list(entry) || is.null(entry$distribution) || is.null(entry$parameters)) {
      errors <- c(errors, sprintf(
        "Malformed entry: parameters_global$%s (missing distribution or parameters)",
        param_name))
    }
  }
  for (param_base in names(updated$parameters_location)) {
    locs <- updated$parameters_location[[param_base]]$location
    for (iso in names(locs)) {
      entry <- locs[[iso]]
      if (!is.list(entry) || is.null(entry$distribution) || is.null(entry$parameters)) {
        errors <- c(errors, sprintf(
          "Malformed entry: parameters_location$%s$location$%s", param_base, iso))
      }
    }
  }

  # Check location counts per group match
  for (param_base in intersect(names(original$parameters_location),
                               names(updated$parameters_location))) {
    n_orig <- length(original$parameters_location[[param_base]]$location)
    n_upd <- length(updated$parameters_location[[param_base]]$location)
    if (n_orig != n_upd) {
      errors <- c(errors, sprintf(
        "Location count mismatch for %s: original %d, updated %d",
        param_base, n_orig, n_upd))
    }
  }

  if (length(errors) > 0) {
    stop("Output validation failed:\n  - ", paste(errors, collapse = "\n  - "))
  }

  if (verbose) {
    message("Output validation passed")
  }

  invisible(TRUE)
}
