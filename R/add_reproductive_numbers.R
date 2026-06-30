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
  reff <- tryCatch(
    calc_Reff(traj, cfg, infectiousness_floor = infectiousness_floor,
              verbose = verbose),
    error = function(e) e)
  if (inherits(reff, "error")) {
    warning("add_reproductive_numbers: calc_Reff() failed for ", output_dir,
            ": ", conditionMessage(reff), call. = FALSE)
    return(invisible(.status("error",
                             message = paste0("calc_Reff failed: ",
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
