#' Check Python Environment for MOSAIC
#'
#' @description
#' Displays information about the Python environment currently attached to the R session
#' and checks whether it matches the MOSAIC Python environment at ~/.virtualenvs/r-mosaic.
#' Provides clickable links to relevant functions based on the environment status.
#'
#' @return Logical. TRUE if the r-mosaic Python environment is linked to the current session, FALSE otherwise.
#' @export
#'

check_python_env <- function() {

     cli::cli_h1("Python Environment Status")

     # Retrieve the expected environment paths using get_python_paths().
     paths <- MOSAIC::get_python_paths()

     # Initialize return value
     is_mosaic_env <- FALSE

     # Check if Python is initialized
     if (!reticulate::py_available(initialize = FALSE)) {
          cli::cli_alert_info("No Python environment is currently attached to this R session.")

          # Check if MOSAIC env exists
          if (!dir.exists(paths$env) || !file.exists(paths$exec)) {
               cli::cli_alert_warning("MOSAIC Python environment not found at {paths$env}")
               cli::cli_text("To install: {.run MOSAIC::install_dependencies()}")
          } else {
               cli::cli_alert_success("MOSAIC Python environment exists at {paths$env}")
               cli::cli_text("To use it: {.run MOSAIC::use_mosaic_env()}")
          }
          return(invisible(is_mosaic_env))
     }

     # Python is initialized - get current configuration
     config <- reticulate::py_config()
     current_python <- normalizePath(config$python, winslash = "/", mustWork = FALSE)

     # Display current Python info
     cli::cli_h2("Current Python Configuration")
     cli::cli_text("Python executable: {current_python}")
     cli::cli_text("Python version: {config$version}")

     # Check which environment is active
     if (grepl("r-mosaic", current_python)) {
          is_mosaic_env <- TRUE
          cli::cli_alert_success("✓ Using MOSAIC Python environment")
          cli::cli_text("Optional: {.run MOSAIC::check_dependencies()} to verify all packages")

     } else if (grepl("r-keras", current_python)) {
          cli::cli_alert_warning("Using r-keras environment (likely from est_suitability())")
          cli::cli_text("To switch to MOSAIC: {.run MOSAIC::use_mosaic_env()}")

     } else if (grepl("r-reticulate", current_python)) {
          cli::cli_alert_warning("Using r-reticulate environment")
          cli::cli_text("To switch to MOSAIC: {.run MOSAIC::use_mosaic_env()}")

     } else {
          cli::cli_alert_warning("Using non-MOSAIC Python environment")

          # Try to identify the environment name
          env_name <- "unknown"
          if (grepl("\\.virtualenvs", current_python)) {
               env_parts <- strsplit(current_python, "[\\\\/]")[[1]]
               venv_idx <- which(env_parts == ".virtualenvs")
               if (length(venv_idx) > 0 && length(env_parts) > venv_idx) {
                    env_name <- env_parts[venv_idx + 1]
               }
          }

          if (env_name != "unknown") {
               cli::cli_text("Environment name: {env_name}")
          }

          cli::cli_text("To switch to MOSAIC: {.run MOSAIC::use_mosaic_env()}")
     }

     # Check if MOSAIC env exists for installation reminder
     if (!grepl("r-mosaic", current_python)) {
          if (!dir.exists(paths$env) || !file.exists(paths$exec)) {
               cli::cli_h2("MOSAIC Environment Status")
               cli::cli_alert_warning("MOSAIC environment not installed at {paths$env}")
               cli::cli_text("To install: {.run MOSAIC::install_dependencies()}")
          }
     }

     return(invisible(is_mosaic_env))
}
