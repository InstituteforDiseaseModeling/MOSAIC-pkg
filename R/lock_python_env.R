#' Lock Python Environment to r-mosaic
#'
#' @description
#' Ensures the r-mosaic Python environment is active and prevents switching.
#' This function should be called at the start of parallel workflows or
#' any script that requires stable Python environment access.
#'
#' The function:
#' \itemize{
#'   \item Verifies the r-mosaic environment exists
#'   \item Sets and locks the RETICULATE_PYTHON environment variable
#'   \item Initializes Python with the r-mosaic environment
#'   \item Imports laser_cholera to ensure it's available
#'   \item Fails loudly if any step fails
#' }
#'
#' @return Invisible TRUE if successful, stops with error otherwise
#'
#' @details
#' Once Python is initialized with a specific environment in an R session,
#' reticulate prevents switching to a different environment. This function
#' leverages that behavior by initializing with r-mosaic first, effectively
#' locking the session to that environment.
#'
#' Best practice: Call this function immediately after loading the MOSAIC
#' package and before any other operations that might initialize Python.
#'
#' @examples
#' \dontrun{
#' # At the start of a calibration script
#' library(MOSAIC)
#' set_root_directory("/path/to/MOSAIC")
#'
#' # Lock Python environment before creating cluster
#' lock_python_env()
#'
#' # Now safe to create cluster and run simulations
#' cl <- makeCluster(9)
#' # ... rest of script
#' }
#'
#' @seealso
#' \code{\link{check_python_env}} for checking the current environment,
#' \code{\link{use_mosaic_env}} for switching to MOSAIC environment,
#' \code{\link{check_dependencies}} for verifying the setup,
#' \code{\link{install_dependencies}} for installing the environment
#'
#' @export
#'
lock_python_env <- function() {

    cli::cli_h1("Locking Python Environment to r-mosaic")

    # Get the MOSAIC Python environment paths
    paths <- MOSAIC::get_python_paths()

    # Check if the MOSAIC environment exists
    if (!dir.exists(paths$env) || !file.exists(paths$exec)) {
        cli::cli_alert_danger("MOSAIC Python environment not found at {paths$env}")
        cli::cli_text("")
        cli::cli_text("To install: {.run MOSAIC::install_dependencies()}")
        stop("MOSAIC Python environment not installed", call. = FALSE)
    }

    cli::cli_alert_success("MOSAIC environment found at {paths$env}")

    # Set and lock environment variable
    cli::cli_text("Setting RETICULATE_PYTHON environment variable...")
    Sys.setenv(RETICULATE_PYTHON = paths$norm)

    # Check if Python is already initialized
    if (reticulate::py_available(initialize = FALSE)) {
        current_config <- reticulate::py_config()
        current_python <- normalizePath(current_config$python, winslash = "/", mustWork = FALSE)

        # Check if already using r-mosaic
        if (grepl("r-mosaic", current_python)) {
            cli::cli_alert_success("Python already initialized with r-mosaic")
            cli::cli_text("Python: {current_python}")
            cli::cli_text("Version: {current_config$version}")
        } else {
            # Python initialized with different environment
            cli::cli_alert_danger("Python is already initialized with a different environment")
            cli::cli_text("Current: {current_python}")
            cli::cli_text("Target: {paths$norm}")
            cli::cli_text("")
            cli::cli_text("Due to reticulate limitations, you must restart R to switch environments.")

            if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
                cli::cli_text("To restart: {.run rstudioapi::restartSession()}")
            }

            stop("Cannot switch from existing Python environment without restarting R",
                 call. = FALSE)
        }
    } else {
        # Python not initialized - initialize it now
        cli::cli_text("Initializing Python with r-mosaic environment...")

        tryCatch({
            reticulate::use_condaenv(paths$norm, required = TRUE)

            # Verify it worked
            config <- reticulate::py_config()
            if (!grepl("r-mosaic", config$python)) {
                stop("Failed to activate r-mosaic environment. ",
                     "Activated: ", config$python,
                     call. = FALSE)
            }

            cli::cli_alert_success("Python initialized with r-mosaic")
            cli::cli_text("Python: {config$python}")
            cli::cli_text("Version: {config$version}")

        }, error = function(e) {
            cli::cli_alert_danger("Error initializing Python: {e$message}")
            cli::cli_text("")
            cli::cli_text("Troubleshooting:")
            cli::cli_text("  1. Check environment: {.run MOSAIC::check_python_env()}")
            cli::cli_text("  2. Verify dependencies: {.run MOSAIC::check_dependencies()}")
            cli::cli_text("  3. Reinstall if needed: {.run MOSAIC::install_dependencies()}")
            stop("Failed to initialize Python with r-mosaic", call. = FALSE)
        })
    }

    # Import laser-cholera to ensure it's available and lock in the environment
    cli::cli_text("Importing laser_cholera to verify installation...")

    lc <- tryCatch({
        reticulate::import("laser_cholera.metapop.model")
    }, error = function(e) {
        cli::cli_alert_danger("Failed to import laser_cholera: {e$message}")
        cli::cli_text("")
        cli::cli_text("The r-mosaic environment may be corrupted or incomplete.")
        cli::cli_text("To fix:")
        cli::cli_text("  1. Check dependencies: {.run MOSAIC::check_dependencies()}")
        cli::cli_text("  2. Reinstall: {.run MOSAIC::install_dependencies()}")
        stop("Cannot import laser_cholera from r-mosaic environment", call. = FALSE)
    })

    cli::cli_alert_success("laser_cholera imported successfully")
    cli::cli_text("")

    # Final summary
    cli::cli_rule("Python Environment Locked")
    cli::cli_text("")
    cli::cli_alert_success("Session is locked to r-mosaic Python environment")
    cli::cli_text("Environment path: {paths$env}")
    cli::cli_text("Python executable: {paths$exec}")
    cli::cli_text("")
    cli::cli_alert_info("This environment will remain active for the entire R session")
    cli::cli_text("To check status: {.run MOSAIC::check_python_env()}")
    cli::cli_text("")

    return(invisible(TRUE))
}
