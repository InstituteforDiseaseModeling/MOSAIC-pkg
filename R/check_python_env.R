#' Check Python Environment for MOSAIC
#'
#' @description
#' Checks whether the current R session is attached to the MOSAIC Python environment.
#' The environment is fixed to "~/.virtualenvs/r-mosaic". If the environment is missing,
#' it instructs the user to run MOSAIC::install_dependencies(). If a different Python interpreter
#' is attached, it alerts the user with a restart suggestion.
#'
#' @return No return value. Outputs messages to the console.
#' @export
#'

check_python_env <- function() {

     # Retrieve the expected environment paths using get_python_paths().
     paths <- MOSAIC::get_python_paths()

     # Check that the desired environment exists.
     if (!dir.exists(paths$env) || !file.exists(paths$exe)) {
          cli::cli_alert_danger("Python environment not found or incomplete at {paths$env}.")
          cli::cli_text("To finish setup, run {.run MOSAIC::install_dependencies()}.")
          return(invisible(NULL))
     }

     # If a Python interpreter is already attached...
     if (reticulate::py_available()) {
          config <- reticulate::py_config()
          current_python_norm <- normalizePath(config$python, winslash = "/", mustWork = FALSE)

          if (current_python_norm != paths$norm) {
               cli::cli_alert_danger("R session is attached to a different Python interpreter!")
               cli::cli_text("Activated: {current_python_norm}")
               cli::cli_text("Expected:  {paths$norm}")

               if (requireNamespace("rstudioapi", quietly = TRUE)) {
                    cli::cli_text("To restart your R session, run: {.run rstudioapi::restartSession()}")
               } else {
                    cli::cli_text("Please restart your R session manually.")
               }
          } else {
               cli::cli_alert_success("Correct Python environment found in {paths$env}.")
               cli::cli_text("To check setup, run {.run MOSAIC::check_dependencies()}.")
          }
     } else {

          # If no Python interpreter is attached, attach the desired conda environment.
          Sys.setenv(RETICULATE_PYTHON = paths$norm)
          reticulate::use_condaenv(paths$norm, required = TRUE)
          cli::cli_alert_success("Python environment at {paths$env} is attached to the current R session.")
          cli::cli_text("To check entire setup, run {.run MOSAIC::check_dependencies()}.")
     }

     return(invisible(NULL))
}
