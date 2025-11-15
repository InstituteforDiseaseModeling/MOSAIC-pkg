#' Attach MOSAIC Python Environment
#'
#' @description
#' Explicitly attaches the r-mosaic Python environment to the current R session.
#' This function initializes Python with the r-mosaic environment, making all
#' MOSAIC Python dependencies (laser_cholera, torch, zuko, etc.) available.
#'
#' The function:
#' \itemize{
#'   \item Sets the RETICULATE_PYTHON environment variable
#'   \item Initializes Python with r-mosaic
#'   \item Verifies the environment is working
#'   \item Provides clear error messages if attachment fails
#' }
#'
#' @param silent Logical. If TRUE, suppresses informational messages. Default: FALSE.
#'
#' @return Invisible TRUE if successful, stops with error otherwise
#'
#' @details
#' This function is called automatically when MOSAIC is loaded (via .onAttach).
#' You typically don't need to call it manually unless:
#' \itemize{
#'   \item You want to verify the Python environment is working
#'   \item You detached and want to re-attach without restarting R
#'   \item You're writing a script that needs explicit environment control
#' }
#'
#' Once Python is initialized, reticulate prevents switching to a different
#' environment without restarting R. If Python is already initialized with
#' a different environment, this function will fail with an error message
#' instructing you to restart R.
#'
#' @examples
#' \dontrun{
#' # Manually attach MOSAIC environment
#' attach_mosaic_env()
#'
#' # Attach silently (no messages)
#' attach_mosaic_env(silent = TRUE)
#'
#' # Verify after attaching
#' check_python_env()
#' }
#'
#' @seealso
#' \code{\link{detach_mosaic_env}} for detaching the environment,
#' \code{\link{check_python_env}} for checking the current environment,
#' \code{\link{check_dependencies}} for verifying the setup,
#' \code{\link{install_dependencies}} for installing the environment
#'
#' @export
#'
attach_mosaic_env <- function(silent = FALSE) {

     # Get the MOSAIC Python environment paths
     paths <- MOSAIC::get_python_paths()

     # Check if the MOSAIC environment exists
     if (!dir.exists(paths$env) || !file.exists(paths$exec)) {
          cli::cli_alert_danger("MOSAIC Python environment not found at {paths$env}")
          cli::cli_text("")
          cli::cli_text("To install: {.run MOSAIC::install_dependencies()}")
          stop("MOSAIC Python environment not installed", call. = FALSE)
     }

     if (!silent) cli::cli_alert_info("Attaching MOSAIC Python environment...")

     # Set environment variable
     Sys.setenv(RETICULATE_PYTHON = paths$norm)

     # Check if Python is already initialized
     if (reticulate::py_available(initialize = FALSE)) {
          current_config <- reticulate::py_config()
          current_python <- normalizePath(current_config$python, winslash = "/", mustWork = FALSE)

          # Check if already using r-mosaic
          if (grepl("r-mosaic", current_python)) {
               if (!silent) {
                    cli::cli_alert_success("Already attached to r-mosaic Python environment")
               }
               return(invisible(TRUE))
          } else {
               # Python initialized with different environment
               cli::cli_alert_danger("Python is already initialized with a different environment")
               cli::cli_text("Current: {current_python}")
               cli::cli_text("Target: {paths$norm}")
               cli::cli_text("")
               cli::cli_text("Due to reticulate limitations, you must restart R to switch environments.")
               cli::cli_text("")

               if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
                    cli::cli_text("To restart: {.run rstudioapi::restartSession()}")
                    cli::cli_text("Or use: {.run MOSAIC::detach_mosaic_env()} for guided restart")
               } else {
                    cli::cli_text("Use: {.run MOSAIC::detach_mosaic_env()} for guided restart")
               }

               stop("Cannot attach r-mosaic - Python already initialized with different environment",
                    call. = FALSE)
          }
     } else {
          # Python not initialized - initialize it now
          tryCatch({
               reticulate::use_condaenv(paths$norm, required = TRUE)

               # Verify it worked
               config <- reticulate::py_config()
               if (!grepl("r-mosaic", config$python)) {
                    stop("Failed to activate r-mosaic environment. ",
                         "Activated: ", config$python,
                         call. = FALSE)
               }

               if (!silent) {
                    cli::cli_alert_success("Successfully attached to r-mosaic Python environment")
                    cli::cli_text("Python: {config$python}")
                    cli::cli_text("Version: {config$version}")
               }

          }, error = function(e) {
               cli::cli_alert_danger("Error attaching Python environment: {e$message}")
               cli::cli_text("")
               cli::cli_text("Troubleshooting:")
               cli::cli_text("  1. Check environment: {.run MOSAIC::check_python_env()}")
               cli::cli_text("  2. Verify dependencies: {.run MOSAIC::check_dependencies()}")
               cli::cli_text("  3. Reinstall if needed: {.run MOSAIC::install_dependencies()}")
               stop("Failed to attach Python environment", call. = FALSE)
          })
     }

     return(invisible(TRUE))
}
