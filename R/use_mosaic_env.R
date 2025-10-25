#' Use MOSAIC Python Environment
#'
#' @description
#' Switches the current R session to use the MOSAIC Python environment
#' at ~/.virtualenvs/r-mosaic. This function attempts to switch from other
#' Python environments (like r-keras) to the MOSAIC environment.
#'
#' @return Invisible TRUE if successful, FALSE otherwise.
#'
#' @details
#' This function attempts to switch the Python environment mid-session.
#' Due to reticulate limitations, if Python has already been initialized
#' with a different environment, a complete switch may require restarting R.
#' The function will provide appropriate guidance based on the current state.
#'
#' @examples
#' \dontrun{
#' # Switch to MOSAIC environment after using est_suitability()
#' use_mosaic_env()
#'
#' # At the start of an R session
#' library(MOSAIC)
#' use_mosaic_env()
#' }
#'
#' @seealso
#' \code{\link{check_python_env}} for checking the current environment,
#' \code{\link{check_dependencies}} for verifying the setup,
#' \code{\link{install_dependencies}} for installing the environment
#'
#' @export
#'
use_mosaic_env <- function() {

  # Get the MOSAIC Python environment paths
  paths <- MOSAIC::get_python_paths()

  # Check if the MOSAIC environment exists
  if (!dir.exists(paths$env) || !file.exists(paths$exec)) {
    cli::cli_alert_danger("MOSAIC Python environment not found at {paths$env}")
    cli::cli_text("To install: {.run MOSAIC::install_dependencies()}")
    return(invisible(FALSE))
  }

  # Check if Python is already initialized
  if (reticulate::py_available(initialize = FALSE)) {
    current_config <- reticulate::py_config()
    current_python <- normalizePath(current_config$python, winslash = "/", mustWork = FALSE)

    # Check if already using MOSAIC
    if (grepl("r-mosaic", current_python)) {
      cli::cli_alert_success("Already using MOSAIC Python environment")
      cli::cli_text("Optional: {.run MOSAIC::check_dependencies()} to verify all packages")
      return(invisible(TRUE))
    }

    # Python is initialized with different environment
    cli::cli_alert_warning("Python is initialized with a different environment")

    # Identify the current environment
    env_name <- "unknown"
    if (grepl("r-keras", current_python)) {
      env_name <- "r-keras"
    } else if (grepl("r-reticulate", current_python)) {
      env_name <- "r-reticulate"
    } else if (grepl("\\.virtualenvs", current_python)) {
      env_parts <- strsplit(current_python, "[\\\\/]")[[1]]
      venv_idx <- which(env_parts == ".virtualenvs")
      if (length(venv_idx) > 0 && length(env_parts) > venv_idx) {
        env_name <- env_parts[venv_idx + 1]
      }
    }

    cli::cli_text("Current environment: {env_name}")
    cli::cli_text("Path: {current_python}")

    # Attempt to switch
    cli::cli_alert_info("Attempting to switch to MOSAIC environment...")
    cli::cli_alert_warning("Note: Due to reticulate limitations, a full switch requires restarting R")

    # Set environment variable for future operations
    Sys.setenv(RETICULATE_PYTHON = paths$norm)

    # Try to add MOSAIC paths to Python path
    tryCatch({
      # Add MOSAIC site-packages to Python path
      mosaic_site_packages <- if (.Platform$OS.type == "windows") {
        file.path(paths$env, "Lib", "site-packages")
      } else {
        file.path(paths$env, "lib", "python3.12", "site-packages")
      }

      if (dir.exists(mosaic_site_packages)) {
        # Import sys and check if already in path
        reticulate::py_run_string("import sys")
        current_paths <- reticulate::py_eval("sys.path")
        mosaic_norm <- normalizePath(mosaic_site_packages, winslash = "/")

        if (!mosaic_norm %in% current_paths) {
          reticulate::py_run_string(sprintf("sys.path.insert(0, '%s')", mosaic_norm))
          cli::cli_alert_info("Added MOSAIC packages to Python path")
        }
      }

      cli::cli_alert_warning("Partial switch completed - MOSAIC packages are now accessible")
      cli::cli_text("For a complete switch:")

      if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        cli::cli_text("1. Restart R: {.run rstudioapi::restartSession()}")
      } else {
        cli::cli_text("1. Restart your R session")
      }
      cli::cli_text("2. Run: {.run MOSAIC::use_mosaic_env()}")

      return(invisible(TRUE))

    }, error = function(e) {
      cli::cli_alert_danger("Could not modify Python environment: {e$message}")
      cli::cli_text("Please restart R and run {.run MOSAIC::use_mosaic_env()} first")
      return(invisible(FALSE))
    })

  } else {
    # Python not initialized - clean switch
    cli::cli_alert_info("Activating MOSAIC Python environment...")

    # Set environment variable
    Sys.setenv(RETICULATE_PYTHON = paths$norm)

    # Use the conda environment
    tryCatch({
      reticulate::use_condaenv(paths$norm, required = TRUE)

      # Verify it worked
      config <- reticulate::py_config()
      if (grepl("r-mosaic", config$python)) {
        cli::cli_alert_success("MOSAIC Python environment activated")
        cli::cli_text("Optional: {.run MOSAIC::check_dependencies()} to verify all packages")
        return(invisible(TRUE))
      } else {
        cli::cli_alert_danger("Failed to activate MOSAIC environment")
        return(invisible(FALSE))
      }

    }, error = function(e) {
      cli::cli_alert_danger("Error activating MOSAIC environment: {e$message}")
      return(invisible(FALSE))
    })
  }
}