#' Detach MOSAIC Python Environment
#'
#' @description
#' Detaches the r-mosaic Python environment from the current R session.
#' This function provides guided instructions for properly detaching and
#' restarting R to use a different Python environment.
#'
#' @param restart Logical. If TRUE and in RStudio, automatically restart the R session.
#'   Default: FALSE (just provide instructions).
#'
#' @return Invisible NULL. If restart = TRUE and in RStudio, the session will restart
#'   and this function will not return.
#'
#' @details
#' Due to reticulate's design, once Python is initialized in an R session,
#' it cannot be switched to a different environment without restarting R.
#'
#' This function:
#' \itemize{
#'   \item Unsets the RETICULATE_PYTHON environment variable
#'   \item Provides instructions for restarting R
#'   \item Optionally triggers automatic restart (RStudio only)
#' }
#'
#' After detaching:
#' \itemize{
#'   \item The RETICULATE_PYTHON variable is cleared
#'   \item The current Python session remains active (cannot be changed)
#'   \item When R restarts, MOSAIC will NOT auto-attach to r-mosaic
#'   \item You can set a different RETICULATE_PYTHON before loading MOSAIC
#' }
#'
#' @examples
#' \dontrun{
#' # Detach and get restart instructions
#' detach_mosaic_env()
#'
#' # Detach and automatically restart (RStudio only)
#' detach_mosaic_env(restart = TRUE)
#'
#' # To use a different Python after detaching:
#' # 1. detach_mosaic_env()
#' # 2. Restart R
#' # 3. Sys.setenv(RETICULATE_PYTHON = "/path/to/other/python")
#' # 4. library(MOSAIC)  # Will use your specified Python
#' }
#'
#' @seealso
#' \code{\link{attach_mosaic_env}} for attaching the environment,
#' \code{\link{check_python_env}} for checking the current environment
#'
#' @export
#'
detach_mosaic_env <- function(restart = FALSE) {

     cli::cli_h2("Detaching MOSAIC Python Environment")
     cli::cli_text("")

     # Check current state
     reticulate_python <- Sys.getenv("RETICULATE_PYTHON", unset = "")
     python_initialized <- reticulate::py_available(initialize = FALSE)

     if (reticulate_python != "") {
          cli::cli_alert_info("Unsetting RETICULATE_PYTHON environment variable")
          Sys.unsetenv("RETICULATE_PYTHON")
          cli::cli_text("Current value: {reticulate_python}")
          cli::cli_text("")
     } else {
          cli::cli_alert_info("RETICULATE_PYTHON is not currently set")
          cli::cli_text("")
     }

     if (python_initialized) {
          cli::cli_alert_warning("Python is already initialized in this session")

          current_config <- reticulate::py_config()
          cli::cli_text("Active Python: {current_config$python}")
          cli::cli_text("")

          cli::cli_text("Due to reticulate limitations, the active Python session cannot be changed")
          cli::cli_text("without restarting R. The RETICULATE_PYTHON variable has been cleared,")
          cli::cli_text("which will take effect in the next R session.")
          cli::cli_text("")
     } else {
          cli::cli_alert_success("Python has not been initialized yet")
          cli::cli_text("The RETICULATE_PYTHON variable has been cleared.")
          cli::cli_text("")
     }

     # Provide restart instructions
     in_rstudio <- requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()

     if (python_initialized || reticulate_python != "") {
          cli::cli_h3("Next Steps")
          cli::cli_text("")

          if (in_rstudio) {
               if (restart) {
                    cli::cli_alert_info("Restarting R session...")
                    cli::cli_text("")
                    cli::cli_text("After restart, MOSAIC will NOT auto-attach to r-mosaic.")
                    cli::cli_text("You can:")
                    cli::cli_text("  1. Set RETICULATE_PYTHON to a different environment before loading MOSAIC")
                    cli::cli_text("  2. Or call {.run MOSAIC::attach_mosaic_env()} to re-attach to r-mosaic")
                    cli::cli_text("")

                    # Short delay to allow user to read the message
                    Sys.sleep(2)

                    rstudioapi::restartSession()
                    # If restart succeeds, this won't return
               } else {
                    cli::cli_text("To restart R in RStudio:")
                    cli::cli_text("  Option 1: {.run rstudioapi::restartSession()}")
                    cli::cli_text("  Option 2: Session menu > Restart R")
                    cli::cli_text("  Option 3: {.run MOSAIC::detach_mosaic_env(restart = TRUE)}")
                    cli::cli_text("")
               }
          } else {
               cli::cli_text("To complete detachment:")
               cli::cli_text("  1. Restart your R session")
               cli::cli_text("  2. Do NOT load MOSAIC if you want to use a different Python")
               cli::cli_text("  3. Or call {.run MOSAIC::attach_mosaic_env()} to re-attach to r-mosaic")
               cli::cli_text("")
          }

          cli::cli_h3("Using a Different Python Environment")
          cli::cli_text("")
          cli::cli_text("If you want to use a different Python after restarting:")
          cli::cli_text("  1. Set RETICULATE_PYTHON before loading any packages:")
          cli::cli_text("     {.code Sys.setenv(RETICULATE_PYTHON = \"/path/to/python\")}")
          cli::cli_text("  2. Then load MOSAIC:")
          cli::cli_text("     {.code library(MOSAIC)}")
          cli::cli_text("")
          cli::cli_text("MOSAIC will respect your RETICULATE_PYTHON setting if already set.")
          cli::cli_text("")

     } else {
          cli::cli_alert_success("Detachment complete")
          cli::cli_text("")
          cli::cli_text("When you next load MOSAIC, it will NOT auto-attach to r-mosaic")
          cli::cli_text("unless you call {.run MOSAIC::attach_mosaic_env()}")
          cli::cli_text("")
     }

     return(invisible(NULL))
}
