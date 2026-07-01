# -----------------------------------------------------------------------------
# Post-hoc driver: apply the Cori R_eff reduction to an existing MOSAIC output
# directory.
# -----------------------------------------------------------------------------
# Reads the captured ensemble trajectories + medoid config from a single model
# output directory, runs calc_Reff(), writes the tidy reproductive_numbers table
# (.csv + .rds) to 3_results/posterior/, and (optionally) renders plot_Reff() to
# 3_results/figures/reproductive_number/. Designed to be mapped over the per-ISO
# model tree by an orchestrator; it is robust to missing files / missing
# incidence channel and returns a status row rather than crashing.
# -----------------------------------------------------------------------------

#' Add Cori R_eff to an existing MOSAIC model output directory
#'
#' Post-hoc driver that applies the Cori (2013) effective reproductive number
#' reduction (\code{\link{calc_Reff}}) to a \strong{single} MOSAIC model output
#' directory laid out by \code{\link{run_MOSAIC}}. It loads the captured ensemble
#' trajectories (\code{2_calibration/trajectories_ensemble.rds}) and the medoid
#' config (\code{1_inputs/config.json}), computes the per-location R_eff series,
#' writes the tidy \code{reproductive_numbers} table to
#' \code{3_results/posterior/reproductive_numbers.csv} (and \code{.rds}), and
#' optionally renders \code{\link{plot_Reff}} to
#' \code{3_results/figures/reproductive_number/}.
#'
#' \strong{Robust by design.} Missing files, a trajectory artifact without the
#' \code{incidence} channel, or a \code{calc_Reff()} error are handled by
#' skipping with an informative message/warning and returning a status row; the
#' function does not crash. It is therefore safe to map over the full per-ISO
#' model tree.
#'
#' @param output_dir Character. Path to ONE MOSAIC model output directory (the
#'   directory that contains \code{1_inputs/}, \code{2_calibration/}, and
#'   \code{3_results/}). No path is hardcoded; all I/O is relative to this.
#' @param recompute_ci Logical. When \code{TRUE}, build a proper posterior
#'   credible interval by \strong{re-simulating} the saved posterior ensemble
#'   (\code{2_calibration/ensemble_candidate.rds}) and computing R_eff per member,
#'   then weighted quantiles (median + 95\% interval). This captures the daily
#'   S->E infection \code{incidence} channel that the persisted trajectory
#'   artifact does not retain at a daily grid. A faithfulness gate confirms the
#'   re-sim reproduces the saved \code{cases_array} before any CI is written.
#'   When \code{FALSE} (default) the cheap point-estimate path is used (renewal on
#'   the medoid weighted-median incidence from \code{trajectories_ensemble.rds};
#'   CI columns are populated only if the artifact carries daily-consecutive
#'   per-member lines, otherwise NA).
#' @param burn_in_days Integer or \code{NULL}. Number of leading days to exclude
#'   from the R_eff output (set to \code{NA} in the table). \code{NULL} (default)
#'   reads \code{control$likelihood$burn_in_days} from
#'   \code{1_inputs/control.json}; if that is \code{0} or absent it defaults to
#'   \code{30} days. Only consumed on the \code{recompute_ci = TRUE} path.
#' @param infectiousness_floor Numeric scalar \eqn{\ge 0}. Passed to
#'   \code{\link{calc_Reff}}; minimum generation-weighted past infectiousness
#'   required to report \eqn{R_t} (guards the initial-condition seed spike and
#'   deep inter-epidemic troughs). Default \code{1}.
#' @param plots Logical. Render and save \code{\link{plot_Reff}} (PNG + PDF) to
#'   \code{3_results/figures/reproductive_number/}. Default \code{TRUE}.
#' @param overwrite Logical. If \code{FALSE} and the output CSV already exists,
#'   skip recomputation and return a \code{"skipped_exists"} status. Default
#'   \code{TRUE}.
#' @param verbose Logical. Emit progress messages. Default \code{TRUE}.
#'
#' @return Invisibly, a one-row \code{data.frame} status with columns:
#'   \describe{
#'     \item{dir}{The \code{output_dir}.}
#'     \item{status}{One of \code{"ok"}, \code{"skipped_exists"},
#'       \code{"skipped_missing_trajectories"}, \code{"skipped_missing_config"},
#'       \code{"skipped_no_incidence"}, or \code{"error"}.}
#'     \item{n_locations}{Number of locations in the artifact (\code{NA} if not
#'       loaded).}
#'     \item{ci_available}{Logical; whether the posterior CI columns are
#'       populated (\code{FALSE} on production-default strided artifacts).}
#'     \item{csv}{Path to the written CSV (\code{NA} when not written).}
#'     \item{rds}{Path to the written RDS (\code{NA} when not written).}
#'     \item{plot}{Path to the written PNG (\code{NA} when not written).}
#'     \item{message}{Human-readable detail.}
#'   }
#'
#' @seealso \code{\link{calc_Reff}}, \code{\link{plot_Reff}}.
#'
#' @examples
#' \dontrun{
#' # Single model directory
#' add_reproductive_numbers("/path/to/output/national/MOZ")
#'
#' # Map over a per-ISO tree
#' dirs <- list.dirs("/path/to/output/national", recursive = FALSE)
#' status <- do.call(rbind, lapply(dirs, add_reproductive_numbers))
#' }
#'
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom utils write.csv
#' @importFrom ggplot2 ggsave
add_reproductive_numbers <- function(output_dir,
                                     recompute_ci = FALSE,
                                     burn_in_days = NULL,
                                     infectiousness_floor = 1,
                                     plots     = TRUE,
                                     overwrite = TRUE,
                                     verbose   = TRUE) {

  .status <- function(status, n_locations = NA_integer_, ci_available = NA,
                      csv = NA_character_, rds = NA_character_,
                      plot = NA_character_, message = "") {
    data.frame(dir = output_dir, status = status,
               n_locations = n_locations, ci_available = ci_available,
               csv = csv, rds = rds, plot = plot, message = message,
               stringsAsFactors = FALSE)
  }
  .msg <- function(...) if (isTRUE(verbose)) message(...)

  if (missing(output_dir) || is.null(output_dir) || !nzchar(output_dir))
    stop("add_reproductive_numbers: `output_dir` is required.")
  if (!dir.exists(output_dir)) {
    warning("add_reproductive_numbers: output_dir does not exist: ", output_dir,
            call. = FALSE)
    return(invisible(.status("error", message = "output_dir does not exist")))
  }

  traj_path <- file.path(output_dir, "2_calibration", "trajectories_ensemble.rds")
  cfg_path  <- file.path(output_dir, "1_inputs", "config.json")
  post_dir  <- file.path(output_dir, "3_results", "posterior")
  fig_dir   <- file.path(output_dir, "3_results", "figures", "reproductive_number")
  csv_path  <- file.path(post_dir, "reproductive_numbers.csv")
  rds_path  <- file.path(post_dir, "reproductive_numbers.rds")

  # --- Idempotency guard -----------------------------------------------------
  if (!isTRUE(overwrite) && file.exists(csv_path)) {
    .msg("add_reproductive_numbers: ", csv_path,
         " exists and overwrite=FALSE; skipping.")
    return(invisible(.status("skipped_exists", csv = csv_path,
                             message = "output exists; overwrite=FALSE")))
  }

  # --- Required inputs present? ----------------------------------------------
  if (!file.exists(traj_path)) {
    warning("add_reproductive_numbers: missing trajectories artifact: ",
            traj_path, call. = FALSE)
    return(invisible(.status("skipped_missing_trajectories",
                             message = "trajectories_ensemble.rds not found")))
  }
  if (!file.exists(cfg_path)) {
    warning("add_reproductive_numbers: missing config: ", cfg_path, call. = FALSE)
    return(invisible(.status("skipped_missing_config",
                             message = "config.json not found")))
  }

  # --- Load ------------------------------------------------------------------
  traj <- tryCatch(readRDS(traj_path), error = function(e) e)
  if (inherits(traj, "error")) {
    warning("add_reproductive_numbers: failed to read trajectories: ",
            conditionMessage(traj), call. = FALSE)
    return(invisible(.status("error",
                             message = paste0("readRDS failed: ",
                                              conditionMessage(traj)))))
  }
  cfg <- tryCatch(jsonlite::fromJSON(cfg_path), error = function(e) e)
  if (inherits(cfg, "error")) {
    warning("add_reproductive_numbers: failed to read config: ",
            conditionMessage(cfg), call. = FALSE)
    return(invisible(.status("skipped_missing_config",
                             message = paste0("fromJSON failed: ",
                                              conditionMessage(cfg)))))
  }

  # --- Guard: incidence channel present? -------------------------------------
  inc_med <- tryCatch(traj$summary[["incidence"]]$median,
                      error = function(e) NULL)
  if (is.null(inc_med) || !is.matrix(inc_med)) {
    warning("add_reproductive_numbers: trajectory artifact lacks the ",
            "`incidence` channel (summary$incidence$median); skipping ",
            output_dir, ".", call. = FALSE)
    n_loc <- tryCatch(as.integer(traj$n_locations), error = function(e) NA_integer_)
    return(invisible(.status("skipped_no_incidence", n_locations = n_loc,
                             message = "no incidence channel in artifact")))
  }

  # --- Compute R_eff ---------------------------------------------------------
  if (isTRUE(recompute_ci)) {
    reff <- tryCatch(
      .add_reff_recompute_ci(output_dir = output_dir, base_config = cfg,
                             burn_in_days = burn_in_days,
                             infectiousness_floor = infectiousness_floor,
                             verbose = verbose),
      error = function(e) e)
  } else {
    reff <- tryCatch(
      calc_Reff(traj, cfg, infectiousness_floor = infectiousness_floor,
                verbose = verbose),
      error = function(e) e)
  }
  if (inherits(reff, "error")) {
    warning("add_reproductive_numbers: R_eff computation failed for ", output_dir,
            ": ", conditionMessage(reff), call. = FALSE)
    return(invisible(.status("error",
                             message = paste0("R_eff failed: ",
                                              conditionMessage(reff)))))
  }

  n_loc <- length(unique(as.character(reff$location)))
  ci_source <- attr(reff, "ci_source")
  ci_available <- !is.null(ci_source) &&
    !grepl("^unavailable", ci_source) &&
    "q2.5" %in% names(reff) && any(is.finite(reff$q2.5))

  # --- Write table (.csv + .rds), atomic via tempfile + rename ---------------
  if (!dir.exists(post_dir))
    dir.create(post_dir, recursive = TRUE, showWarnings = FALSE)

  csv_written <- NA_character_
  rds_written <- NA_character_
  ok_write <- tryCatch({
    tmp_csv <- tempfile(tmpdir = post_dir, fileext = ".csv")
    tmp_rds <- tempfile(tmpdir = post_dir, fileext = ".rds")
    utils::write.csv(as.data.frame(reff), tmp_csv, row.names = FALSE)
    saveRDS(reff, tmp_rds)
    if (!file.rename(tmp_csv, csv_path)) {
      file.copy(tmp_csv, csv_path, overwrite = TRUE); unlink(tmp_csv)
    }
    if (!file.rename(tmp_rds, rds_path)) {
      file.copy(tmp_rds, rds_path, overwrite = TRUE); unlink(tmp_rds)
    }
    TRUE
  }, error = function(e) e)
  if (inherits(ok_write, "error")) {
    warning("add_reproductive_numbers: failed to write outputs for ", output_dir,
            ": ", conditionMessage(ok_write), call. = FALSE)
    return(invisible(.status("error", n_locations = n_loc,
                             ci_available = ci_available,
                             message = paste0("write failed: ",
                                              conditionMessage(ok_write)))))
  }
  csv_written <- csv_path
  rds_written <- rds_path
  .msg("add_reproductive_numbers: wrote ", csv_path)
  .msg("add_reproductive_numbers: wrote ", rds_path)

  # --- Plot ------------------------------------------------------------------
  plot_written <- NA_character_
  if (isTRUE(plots)) {
    plot_ok <- tryCatch({
      if (!dir.exists(fig_dir))
        dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
      run_id <- basename(output_dir)
      png_path <- file.path(fig_dir, paste0("reproductive_number_", run_id, ".png"))
      pdf_path <- file.path(fig_dir, paste0("reproductive_number_", run_id, ".pdf"))
      p <- plot_Reff(reff)
      plot_w <- if (n_loc <= 1L) 10 else if (n_loc <= 6L) 12 else 16
      plot_h <- if (n_loc <= 1L) 5  else
                max(5, ceiling(n_loc / 3L) * 3L)
      ggplot2::ggsave(png_path, plot = p, width = plot_w, height = plot_h,
                      dpi = 300, limitsize = FALSE)
      ggplot2::ggsave(pdf_path, plot = p, width = plot_w, height = plot_h,
                      limitsize = FALSE)
      png_path
    }, error = function(e) e)
    if (inherits(plot_ok, "error")) {
      warning("add_reproductive_numbers: R_eff table written but plot failed ",
              "for ", output_dir, ": ", conditionMessage(plot_ok), call. = FALSE)
    } else {
      plot_written <- plot_ok
      .msg("add_reproductive_numbers: wrote ", plot_written)
    }
  }

  invisible(.status("ok", n_locations = n_loc, ci_available = ci_available,
                    csv = csv_written, rds = rds_written, plot = plot_written,
                    message = paste0("ci_source=", ci_source)))
}

# -----------------------------------------------------------------------------
# Re-simulation driver: build a faithful posterior R_eff CI for ONE output dir.
# -----------------------------------------------------------------------------
#' Re-simulate the saved posterior ensemble and assemble a R_eff CI table
#'
#' Loads \code{2_calibration/ensemble_candidate.rds}, \code{1_inputs/priors.json},
#' and the calibration \code{control$sampling} + \code{burn_in_days} from
#' \code{1_inputs/control.json}, re-simulates the posterior members via
#' \code{\link{.mosaic_reff_resim_ci}} (faithfulness-gated against the saved
#' \code{cases_array}), computes per-member R_eff with each member's own kernel,
#' and returns a tidy \code{reproductive_numbers} data.frame with the same schema
#' as \code{\link{calc_Reff}}. The leading \code{burn_in_days} are set to
#' \code{NA} in every output column.
#'
#' \strong{Column semantics (phase-coherent headline).}
#' \describe{
#'   \item{\code{central}}{The MEDOID trajectory's R_t -- a single coherent
#'     member's series (\code{attr "central_definition" = "medoid_trajectory"},
#'     or \code{"member_with_median_peak_Rt"} on the documented fallback). This is
#'     the headline; it captures the explosive single-trajectory peak.}
#'   \item{\code{q2.5}/\code{q50}/\code{q97.5}}{The per-calendar-day cross-member
#'     weighted quantiles (\code{attr "band_definition" =
#'     "per_calendar_day_cross_member_weighted_quantiles"}). This calendar-date
#'     envelope does NOT represent the epidemic's peak R_t: members are
#'     phase-misaligned so the per-day median regresses toward 1.}
#' }
#' The explosivity statistic -- the posterior-weighted quantiles of each member's
#' TIME-MAX R_t (post-burn-in, floor-gated) -- is carried in attribute
#' \code{"peak_Rt"} (a per-location \code{data.frame} with \code{location},
#' \code{q2.5}/\code{q50}/\code{q97.5}, and \code{n_members}; schema pinned with
#' \code{plot_Reff()}).
#'
#' @keywords internal
#' @noRd
.add_reff_recompute_ci <- function(output_dir, base_config, burn_in_days = NULL,
                                   infectiousness_floor = 1, verbose = TRUE) {
  ens_path <- file.path(output_dir, "2_calibration", "ensemble_candidate.rds")
  pri_path <- file.path(output_dir, "1_inputs", "priors.json")
  ctl_path <- file.path(output_dir, "1_inputs", "control.json")
  if (!file.exists(ens_path))
    stop("recompute_ci: missing ensemble_candidate.rds at ", ens_path)
  if (!file.exists(pri_path))
    stop("recompute_ci: missing priors.json at ", pri_path)
  if (!file.exists(ctl_path))
    stop("recompute_ci: missing control.json at ", ctl_path)

  ens    <- readRDS(ens_path)
  if (is.null(ens$cases_array)) {
    stop("recompute_ci: ensemble_candidate.rds was saved WITHOUT the dense ",
         "cases_array (run_MOSAIC default control$io$persist_ensemble_arrays = ",
         "FALSE strips the 4-D arrays at save time). The posterior ",
         "re-simulation CI path requires the per-member arrays. Re-run the ",
         "calibration with control$io$persist_ensemble_arrays = TRUE, or use ",
         "the trajectories-based CI path (2_calibration/trajectories_ensemble.rds).",
         call. = FALSE)
  }
  priors <- jsonlite::fromJSON(pri_path, simplifyVector = FALSE)
  ctl    <- jsonlite::fromJSON(ctl_path)
  control <- if (!is.null(ctl$control)) ctl$control else ctl
  sampling_args <- control$sampling

  # Resolve burn_in_days: arg > control$likelihood$burn_in_days > 30 default.
  bid <- burn_in_days
  if (is.null(bid)) {
    bid <- tryCatch(as.integer(control$likelihood$burn_in_days),
                    error = function(e) NA_integer_)
  }
  bid <- suppressWarnings(as.integer(bid))
  if (length(bid) != 1L || is.na(bid) || bid <= 0L) {
    if (verbose)
      message("  burn_in_days resolved to 0/absent; defaulting to 30 days.")
    bid <- 30L
  }
  if (verbose) message("  Excluding first ", bid, " day(s) as burn-in.")

  # Match run_MOSAIC's medoid target: the cases central_method (default median).
  cases_cm <- tryCatch(
    .mosaic_resolve_central_method(control$predictions$central_method)[["cases"]],
    error = function(e) "median")
  if (length(cases_cm) != 1L || is.na(cases_cm)) cases_cm <- "median"

  PATHS <- get_paths()

  res <- .mosaic_reff_resim_ci(
    ensemble = ens, base_config = base_config, priors = priors,
    sampling_args = sampling_args, PATHS = PATHS,
    probs = c(0.025, 0.5, 0.975),
    infectiousness_floor = infectiousness_floor, burn_in_days = bid,
    cases_central_method = cases_cm,
    verbose = verbose)

  if (verbose)
    message(sprintf(paste0("  Faithfulness gate PASSED (robust statistical ",
                          "equivalence): p%.0f rel_err = %.4f, ensemble-agg ",
                          "rel_err = %.4f, median cor = %.4f, %d/%d member ",
                          "outliers (worst rel_err = %.3f) over %d members."),
                    res$gate_frac * 100, res$gate_rel_err_pct, res$gate_agg_rel_err,
                    res$gate_cor_median, res$gate_n_outliers, res$n_members,
                    res$gate_rel_err_max, res$n_members))

  nL    <- length(ens$location_names)
  Tn    <- dim(ens$cases_array)[2L]
  locs  <- as.character(ens$location_names)
  probs <- res$probs
  prob_cols <- .mosaic_reff_prob_colnames(probs)   # "q2.5","q50","q97.5"

  d0    <- tryCatch(as.Date(ens$date_start), error = function(e) NA)
  dates <- if (!is.na(d0)) d0 + (seq_len(Tn) - 1L) else as.Date(NA) + seq_len(Tn)

  burn_idx <- if (bid >= 1L) seq_len(min(bid, Tn)) else integer(0)

  central <- res$central_mat
  qmats   <- res$qmats
  if (length(burn_idx)) {
    central[, burn_idx] <- NA_real_
    qmats[, burn_idx, ] <- NA_real_
  }

  parts <- vector("list", nL)
  for (i in seq_len(nL)) {
    df <- data.frame(
      location = locs[i], date = dates, t = seq_len(Tn),
      estimand = "R_eff", central = central[i, ],
      stringsAsFactors = FALSE)
    for (k in seq_along(probs)) df[[prob_cols[k]]] <- qmats[i, , k]
    parts[[i]] <- df
  }
  out <- do.call(rbind, parts)
  rownames(out) <- NULL

  attr(out, "location_names") <- locs
  attr(out, "dates")          <- dates
  attr(out, "kernel")         <- "moment_matched_per_member"
  attr(out, "series")         <- "infection_incidence"
  attr(out, "kernel_params")  <- res$kernel_params
  attr(out, "probs")          <- probs
  attr(out, "ci_source")      <- "weighted_quantiles_resimulated"
  attr(out, "central_definition") <- res$central_definition
  attr(out, "band_definition")    <-
    "per_calendar_day_cross_member_weighted_quantiles"
  attr(out, "peak_Rt")          <- res$peak_Rt
  attr(out, "medoid_member")    <- res$medoid_member
  attr(out, "burn_in_days")     <- bid
  attr(out, "gate_rel_err_pct") <- res$gate_rel_err_pct
  attr(out, "gate_rel_err_max") <- res$gate_rel_err_max
  attr(out, "gate_agg_rel_err") <- res$gate_agg_rel_err
  attr(out, "gate_cor_median")  <- res$gate_cor_median
  attr(out, "gate_cor_min")     <- res$gate_cor_min
  attr(out, "gate_max_abs_diff") <- res$gate_max_abs_diff
  attr(out, "gate_n_outliers")  <- res$gate_n_outliers
  attr(out, "n_members")        <- res$n_members
  attr(out, "caveat")         <- paste0(
    "Cori R_eff computed on RE-SIMULATED posterior-member infection incidence; ",
    "per-member weighted quantiles (median + 95% CI). Burn-in (", bid,
    " days) excluded. Descriptor of the model trajectory, not a ",
    "first-principles R0.")
  class(out) <- c("reproductive_numbers", "data.frame")
  out
}
