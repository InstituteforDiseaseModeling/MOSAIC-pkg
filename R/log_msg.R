#' Simple Logging Function with Timestamps
#'
#' A utility function for logging messages with timestamps to both console and
#' optional log files. Useful for tracking progress in long-running simulations
#' and model calibration workflows.
#'
#' @param msg Character string. The message to log. Can include sprintf-style
#'   formatting placeholders (e.g., "%d", "%s", "%.2f").
#' @param ... Additional arguments passed to sprintf for message formatting.
#'
#' @details
#' The function automatically adds a timestamp in "YYYY-MM-DD HH:MM:SS" format
#' to each message. Messages are printed to the console and, if a 'dir_output'
#' variable exists in the calling environment, also appended to a 'run.log' file
#' in that directory (dir_output/run.log). If the directory does not exist, it
#' will be created automatically.
#'
#' This is particularly useful for:
#' \itemize{
#'   \item Model calibration workflows (logs entire BFRS → NPE sequence)
#'   \item Long-running simulations
#'   \item Parallel processing tasks
#'   \item Debugging and progress tracking
#' }
#'
#' @return Invisible NULL. Function is called for its side effects (logging).
#'
#' @examples
#' \dontrun{
#' # Simple message
#' log_msg("Starting analysis")
#'
#' # Formatted message
#' log_msg("Processing %d simulations with %d cores", 100, 8)
#'
#' # With output directory for file logging
#' dir_output <- tempdir()
#' log_msg("Results saved to %s", dir_output)
#'
#' # Check the log file
#' log_file <- file.path(dir_output, "run.log")
#' if (file.exists(log_file)) {
#'   cat(readLines(log_file), sep = "\n")
#' }
#' }
#'
#' @export
log_msg <- function(msg, ...) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s", timestamp, sprintf(msg, ...))
  cat(log_entry, "\n")

  # Check if dir_output exists in the calling environment
  if (exists("dir_output", envir = parent.frame())) {
    dir_output <- get("dir_output", envir = parent.frame())

    # Create directory if it doesn't exist
    if (!dir.exists(dir_output)) {
      dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)
    }

    log_file <- file.path(dir_output, "run.log")
    cat(log_entry, "\n", file = log_file, append = TRUE)
  }

  invisible(NULL)
}