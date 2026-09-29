#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# MOSAIC data update -- cron / CLI entry point.
#
# Thin wrapper around MOSAIC::update_mosaic_data(). All logic lives in the
# package (R/update_mosaic_data.R) so it is documented, testable and visible
# to R CMD check; this file only parses arguments and sets an exit code.
#
#   Rscript inst/scripts/update_mosaic_data.R --root=~/MOSAIC --dry-run
#   Rscript inst/scripts/update_mosaic_data.R --root=~/MOSAIC
#   Rscript inst/scripts/update_mosaic_data.R --root=~/MOSAIC \
#           --steps=3A,3D,3E --no-refresh-repos
#
# Exit codes:  0 = all steps ok   1 = one or more failed/blocked
#              2 = the run itself could not start
#
# From an installed package:
#   Rscript "$(Rscript -e 'cat(system.file("scripts/update_mosaic_data.R", package="MOSAIC"))')" --root=~/MOSAIC
#
# This builds DATA only. Fitting the suitability LSTM is model fitting and is
# not reachable from here -- call MOSAIC::est_suitability() on a box with the
# TF env and enough RAM, on its own schedule.
#
# Cron (weekly, Mondays 03:00):
#   0 3 * * 1 cd /path/to/MOSAIC-pkg && \
#     Rscript inst/scripts/update_mosaic_data.R --root=$HOME/MOSAIC \
#     >> $HOME/mosaic-data-update.log 2>&1
# ---------------------------------------------------------------------------

suppressMessages(library(MOSAIC))

args <- commandArgs(trailingOnly = TRUE)

has <- function(flag) any(args == flag)
val <- function(key, default = NULL) {
     hit <- grep(paste0("^", key, "="), args, value = TRUE)
     if (!length(hit)) return(default)
     sub(paste0("^", key, "="), "", hit[[1L]])
}
split_csv <- function(x) if (is.null(x)) NULL else trimws(strsplit(x, ",")[[1]])

# Reject anything unrecognised. Without this, `--steps 3A` (space instead of
# `=`) silently yields steps = NULL -> run all 51 steps live, and `--dryrun`
# silently runs for real. Fail loudly instead.
.known_flags <- c("--no-refresh-repos", "--stop-on-error",
                  "--dry-run", "--quiet", "--help", "-h")
.known_opts  <- c("--root", "--steps", "--skip", "--date-stop")
.bad <- args[!(args %in% .known_flags) &
             !grepl(paste0("^(", paste(.known_opts, collapse = "|"), ")="), args)]
if (length(.bad)) {
     message("FATAL: unrecognised argument(s): ", paste(.bad, collapse = " "))
     message("  Options take the form --key=value (no space). Try --help.")
     quit(status = 2L)
}

if (has("--help") || has("-h")) {
     cat("Usage: Rscript update_mosaic_data.R [options]\n\n",
         "  --root=PATH         MOSAIC parent directory (default: last set root)\n",
         "  --steps=A,B         Only these step or group ids (e.g. 3A,3D or 2)\n",
         "  --skip=A,B          Exclude these step or group ids\n",
         "  --date-stop=DATE    Upper date bound (default: today + 540 days)\n",
         "  --no-refresh-repos  Skip the git pull of the scraper repos\n",
         "  --stop-on-error     Abort at the first failure\n",
         "  --dry-run           Print preflight + plan, run nothing\n",
         "  --quiet             Suppress progress output\n",
         "  -h, --help          This message\n", sep = "")
     quit(status = 0L)
}

res <- tryCatch(
     MOSAIC::update_mosaic_data(
          root                = val("--root"),
          steps               = split_csv(val("--steps")),
          skip                = split_csv(val("--skip")),
          refresh_repos       = !has("--no-refresh-repos"),
          # Must match update_mosaic_data()'s own default. Passing a bare
          # Sys.Date() here (as this script did until v0.91.14) silently
          # overrode the +540 default and produced a vaccination matrix 139
          # days shorter than the psi horizon, failing the config build with
          # a misleading "nu_1_jt must be a matrix with ... columns" error.
          date_stop           = if (is.null(val("--date-stop"))) Sys.Date() + 540
                                else as.Date(val("--date-stop")),
          dry_run             = has("--dry-run"),
          stop_on_error       = has("--stop-on-error"),
          verbose             = !has("--quiet")
     ),
     error = function(e) {
          message("FATAL: ", conditionMessage(e))
          quit(status = 2L)
     }
)

if (has("--dry-run")) quit(status = 0L)

bad <- sum(res$status %in% c("failed", "blocked", "not_run", "pending"))
quit(status = if (bad > 0L) 1L else 0L)
