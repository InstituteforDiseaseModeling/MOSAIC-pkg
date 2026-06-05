#' Simple Logging Function with Timestamps
#'
#' A utility function for logging messages with ISO 8601 timestamps to both
#' the console and an optional log file. Used throughout
#' \code{\link{run_MOSAIC}} and its helpers.
#'
#' @param msg Character string. The message to log. May include sprintf-style
#'   formatting placeholders (e.g., \code{"\%d"}, \code{"\%s"}, \code{"\%.2f"}).
#' @param ... Additional arguments passed to \code{sprintf} for message
#'   formatting.
#'
#' @details
#' Timestamps are emitted in ISO 8601 format (e.g.
#' \code{"2026-06-05T14:32:01-0700"}) so that a downstream consumer (a human
#' tail, a monitoring AI agent, a log shipper) can parse elapsed time without
#' ambiguity.
#'
#' Lines are written to stdout and, if a log file path is known, also
#' appended to that file. The path is resolved in this order:
#' \enumerate{
#'   \item \code{getOption("MOSAIC.log_file")} — preferred. Set this once at
#'         the start of a pipeline (\code{run_MOSAIC()} does this
#'         automatically) and every \code{log_msg} call — including those
#'         inside helper functions whose calling frame has no
#'         \code{dir_output} variable — will write to the same file.
#'   \item A \code{dir_output} binding in the calling frame (legacy
#'         fallback). The file path becomes \code{<dir_output>/run.log}. This
#'         preserves backward compatibility for callers that don't set the
#'         option.
#' }
#'
#' If neither source resolves a path, lines go to stdout only.
#'
#' @return Invisible NULL. Function is called for its side effects.
#'
#' @examples
#' \dontrun{
#' # Recommended: set the log file once at the start of a long-running
#' # pipeline, then every helper's log_msg() call lands in the same file.
#' options(MOSAIC.log_file = file.path(tempdir(), "run.log"))
#' on.exit(options(MOSAIC.log_file = NULL), add = TRUE)
#'
#' log_msg("Starting analysis")
#' log_msg("Processing %d simulations with %d cores", 100, 8)
#' log_warn("Worker %d failed: %s", 17L, "OOM")
#' log_fatal("Aborting — convergence target unreachable")
#' }
#'
#' @export
log_msg <- function(msg, ...) {
  timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  formatted <- if (length(list(...)) > 0L) sprintf(msg, ...) else msg
  log_entry <- sprintf("[%s] %s", timestamp, formatted)
  cat(log_entry, "\n", sep = "")

  log_file <- getOption("MOSAIC.log_file", default = NULL)

  if (is.null(log_file)) {
    # Legacy fallback: dir_output in calling frame
    if (exists("dir_output", envir = parent.frame())) {
      dir_output <- get("dir_output", envir = parent.frame())
      if (is.character(dir_output) && length(dir_output) == 1L && nzchar(dir_output)) {
        if (!dir.exists(dir_output)) {
          dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)
        }
        log_file <- file.path(dir_output, "run.log")
      }
    }
  }

  if (!is.null(log_file) && is.character(log_file) && length(log_file) == 1L) {
    log_dir <- dirname(log_file)
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    }
    cat(log_entry, "\n", file = log_file, append = TRUE, sep = "")
  }

  invisible(NULL)
}

#' Severity-tagged logging helpers
#'
#' Thin wrappers around \code{\link{log_msg}} that prepend a severity tag
#' (\code{[INFO]}, \code{[WARN]}, \code{[ERROR]}, \code{[FATAL]}) after the
#' timestamp. Use these in place of bare prefixes (e.g. \code{"WARNING:"},
#' \code{"ERROR:"}) so that a downstream consumer can filter by severity
#' with a single regex.
#'
#' \code{log_fatal} is intended to be called immediately before a
#' \code{stop()} or an early-return failure exit — it records the terminal
#' status in the log file so that a tailing process can grep one canonical
#' line instead of guessing whether the pipeline aborted.
#'
#' @param msg Character string with optional sprintf-style placeholders.
#' @param ... Additional arguments passed to sprintf.
#'
#' @return Invisible NULL.
#'
#' @examples
#' \dontrun{
#' log_info("Calibration converged after %d batches", 12L)
#' log_warn("Worker %d returned non-finite likelihood; dropping", 33L)
#' log_error("client$gather() failed: %s", "ConnectionError")
#' log_fatal("Best-seed sample_parameters failed; cannot continue")
#' }
#'
#' @name severity_logging
#' @export
log_info <- function(msg, ...) {
  log_msg(paste0("[INFO] ", msg), ...)
}

#' @rdname severity_logging
#' @export
log_warn <- function(msg, ...) {
  log_msg(paste0("[WARN] ", msg), ...)
}

#' @rdname severity_logging
#' @export
log_error <- function(msg, ...) {
  log_msg(paste0("[ERROR] ", msg), ...)
}

#' @rdname severity_logging
#' @export
log_fatal <- function(msg, ...) {
  log_msg(paste0("[FATAL] ", msg), ...)
}
