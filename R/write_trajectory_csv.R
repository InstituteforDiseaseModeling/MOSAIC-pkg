#' Write ensemble trajectory channels to per-location CSV
#'
#' Exports the ensemble-median daily trajectory of every captured channel to a
#' plain-text CSV, one file per location, as
#' \code{trajectories_<LOC>.csv}.
#'
#' @section Why this exists:
#' \code{run_MOSAIC()} reduces the per-member channel arrays into a
#' \code{mosaic_trajectories} object and persists it to
#' \code{2_calibration/trajectories_ensemble.rds}. That file is an R binary, so
#' the MOSAIC-results promoter classifies it as heavy and records it
#' \code{store:"pending"} — declared in the manifest but never committed. The
#' consequence is that only \code{reported_cases} and \code{reported_deaths}
#' (via the prediction CSVs) reach the archive, while \code{incidence},
#' \code{new_symptomatic}, \code{disease_deaths}, the compartments and the
#' derived channels do not, and are unreadable outside R even when present.
#'
#' A text CSV is copied into git by the existing promoter with no schema or
#' promoter change.
#'
#' @section Format and size:
#' WIDE, not long: one row per \code{(location, date)} with one column per
#' channel. A long table repeats \code{location} and \code{date} once per
#' channel, which for 24 channels measured 2.72 MB against 582 KB wide on a
#' national model (4.7x), and 410 KB against 202 KB after gzip.
#'
#' Values are rounded to \code{digits} significant figures. At the default 6
#' this measured 582 KB per national model (202 KB packed) against 970 KB
#' (373 KB packed) at full precision — model output does not carry 15
#' significant figures of information.
#'
#' The per-member \code{$lines} component is deliberately NOT exported: at 6
#' significant figures it measured 62.7 MB raw / 7.8 MB packed for a single
#' national model, which belongs in blob storage rather than git.
#'
#' @section What is lost:
#' The summary carries the weighted \strong{median} only, so these are central
#' trajectories with no credible intervals. Intervals for these channels
#' require the per-member data. \code{reported_cases} and
#' \code{reported_deaths} keep their intervals in the prediction CSVs.
#'
#' @param trajectories A \code{mosaic_trajectories} object, or a path to a
#'   \code{trajectories_ensemble.rds} file.
#' @param dir_out Directory to write into; created if absent.
#' @param channels Optional character vector selecting channels. Default
#'   \code{NULL} writes every channel present.
#' @param digits Significant figures to round values to. Default 6.
#' @param verbose Logical; print one message per file written.
#'
#' @return Invisibly, the character vector of written file paths.
#'
#' @examples
#' \dontrun{
#' # From a run directory, or to backfill an already-promoted model:
#' write_trajectory_csv(
#'   file.path(dir_output, "2_calibration", "trajectories_ensemble.rds"),
#'   file.path(dir_output, "3_results", "predictions")
#' )
#' }
#' @export
write_trajectory_csv <- function(trajectories, dir_out, channels = NULL,
                                 digits = 6L, verbose = TRUE) {

     if (is.character(trajectories)) {
          if (length(trajectories) != 1L || !file.exists(trajectories)) {
               stop("`trajectories` is a path but the file does not exist: ",
                    trajectories, call. = FALSE)
          }
          trajectories <- readRDS(trajectories)
     }

     if (!is.list(trajectories) || is.null(trajectories$summary) ||
         is.null(trajectories$channels)) {
          stop("`trajectories` is not a mosaic_trajectories object ",
               "(no $summary / $channels).", call. = FALSE)
     }

     available <- as.character(trajectories$channels)
     use <- if (is.null(channels)) available else {
          missing_ch <- setdiff(channels, available)
          if (length(missing_ch)) {
               stop("channel(s) not present in this artifact: ",
                    paste(missing_ch, collapse = ", "),
                    ". Available: ", paste(available, collapse = ", "),
                    call. = FALSE)
          }
          as.character(channels)
     }
     if (!length(use)) return(invisible(character(0)))

     locs <- as.character(trajectories$location_names)
     n_t  <- as.integer(trajectories$n_time_points)
     if (!length(locs) || is.na(n_t) || n_t < 1L) {
          stop("trajectory artifact has no location_names / n_time_points.",
               call. = FALSE)
     }

     # The artifact records date_start and the tick count but no date vector.
     # This is the same derivation plot_model_ensemble() uses for the prediction
     # CSVs, so the two files share a date axis exactly.
     dates <- seq(as.Date(trajectories$date_start), by = "day", length.out = n_t)

     # Assemble once for all locations, then split — each channel's median is a
     # single [n_loc x n_t] matrix, so slicing per location inside the channel
     # loop would re-walk every matrix once per location.
     cols <- list()
     for (ch in use) {
          m <- trajectories$summary[[ch]]$median
          if (is.null(m)) {
               # A channel listed but not reduced: emit NA rather than dropping the
               # column, so the file's shape does not vary silently between models.
               cols[[ch]] <- rep(NA_real_, length(locs) * n_t)
               next
          }
          m <- matrix(as.numeric(m), nrow = length(locs), ncol = n_t)
          cols[[ch]] <- signif(as.vector(t(m)), digits)
     }

     tbl <- data.frame(
          location = rep(locs, each = n_t),
          date     = rep(dates, times = length(locs)),
          stringsAsFactors = FALSE
     )
     for (ch in use) tbl[[ch]] <- cols[[ch]]

     if (!dir.exists(dir_out)) {
          dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
     }

     written <- character(0)
     for (loc in locs) {
          out <- file.path(dir_out, paste0("trajectories_", loc, ".csv"))
          utils::write.csv(tbl[tbl$location == loc, , drop = FALSE], out,
                           row.names = FALSE)
          written <- c(written, out)
          if (verbose) message("  Saved: ", out)
     }
     invisible(written)
}
