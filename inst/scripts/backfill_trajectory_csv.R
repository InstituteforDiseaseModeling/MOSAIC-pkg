#!/usr/bin/env Rscript
# =============================================================================
# backfill_trajectory_csv.R
#
# Generate 3_results/predictions/trajectories_<LOC>.csv for runs that predate
# the exporter, by reading the trajectories_ensemble.rds each run already wrote.
#
# WHY THIS IS NEEDED
#   run_MOSAIC() now writes the CSV alongside the .rds, but the 31 models
#   already promoted to MOSAIC-results were produced before that. Their .rds
#   files still exist in the source run tree and their sha256 matches the
#   promoted manifest, so the channels can be recovered WITHOUT re-running
#   anything.
#
# WHAT IT DOES NOT DO
#   It does not touch MOSAIC-results. Promoted versions are immutable
#   (vYYYY-MM-DD.NN with integrity.tree_sha256), so adding a file to an
#   existing version would break that contract. This writes into the SOURCE run
#   directory only; getting the result into the archive is a separate,
#   deliberate promotion step -- and because the artifact set grows, that means
#   a new version, not an edit in place.
#
# USAGE
#   Rscript backfill_trajectory_csv.R <run-root> [--dry-run] [--digits N]
#
#   <run-root>  a directory containing run dirs at any depth, e.g.
#               ~/MOSAIC/output/full_metapop_nmme
#
# EXAMPLE
#   Rscript backfill_trajectory_csv.R ~/MOSAIC/output/full_metapop_nmme --dry-run
# =============================================================================

suppressMessages(library(MOSAIC))

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || args[1] %in% c("-h", "--help")) {
     cat("Usage: Rscript backfill_trajectory_csv.R <run-root> [--dry-run] [--digits N]\n")
     quit(status = if (length(args)) 0L else 1L)
}

root    <- normalizePath(args[1], mustWork = TRUE)
dry_run <- "--dry-run" %in% args
digits  <- if ("--digits" %in% args) {
     as.integer(args[which(args == "--digits") + 1L])
} else 6L
if (is.na(digits) || digits < 1L) stop("--digits must be a positive integer")

rds <- list.files(root, pattern = "^trajectories_ensemble\\.rds$",
                  recursive = TRUE, full.names = TRUE)
if (!length(rds)) {
     cat("No trajectories_ensemble.rds found under", root, "\n")
     quit(status = 1L)
}

cat(sprintf("Found %d trajectory artifact(s) under %s\n", length(rds), root))
if (dry_run) cat("DRY RUN -- no files will be written\n")
cat(sprintf("%-52s %7s %7s %10s\n", "run", "locs", "chans", "bytes out"))

total <- 0
for (f in rds) {
     # .../<run>/2_calibration/trajectories_ensemble.rds -> .../<run>
     run_dir <- dirname(dirname(f))
     out_dir <- file.path(run_dir, "3_results", "predictions")
     label   <- substr(sub(paste0("^", root, "/?"), "", run_dir), 1, 50)

     ok <- tryCatch({
          tr <- readRDS(f)
          if (dry_run) {
               cat(sprintf("%-52s %7d %7d %10s\n", label,
                           length(tr$location_names), length(tr$channels), "-"))
          } else {
               written <- write_trajectory_csv(tr, out_dir, digits = digits,
                                               verbose = FALSE)
               b <- sum(file.size(written))
               total <- total + b
               cat(sprintf("%-52s %7d %7d %10s\n", label,
                           length(tr$location_names), length(tr$channels),
                           format(b, big.mark = ",")))
          }
          TRUE
     }, error = function(e) {
          # One unreadable artifact must not abort the sweep.
          cat(sprintf("%-52s %s\n", label, paste("ERROR:", conditionMessage(e))))
          FALSE
     })
}

if (!dry_run) {
     cat(sprintf("\nTotal written: %s bytes across %d run(s)\n",
                 format(total, big.mark = ","), length(rds)))
     cat("These are SOURCE run dirs. Promote deliberately -- the artifact set\n",
         "changes, so it is a new version, not an edit to an existing one.\n", sep = "")
}
