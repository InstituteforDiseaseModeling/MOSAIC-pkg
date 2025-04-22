#' Remove the MOSAIC Python Environment
#'
#' @description
#' This function checks whether the MOSAIC Python conda environment exists at "~/.MOSAIC_conda_env".
#' If it exists, it unlinks it from \code{reticulate}, removes the directory, and resets the RETICULATE_PYTHON global variable.
#'
#' @param force Logical. If TRUE, removes the environment without prompting for confirmation. Default is FALSE.
#'
#' @return No return value.
#' @export
#'

remove_MOSAIC_python_env <- function(force = FALSE) {

     # Determine the environment directory
     env_dir <- normalizePath(file.path("~", ".MOSAIC_conda_env"), winslash = "/", mustWork = FALSE)

     # Check if the environment exists
     if (!dir.exists(env_dir)) {
          message("MOSAIC Python environment does not exist at: ", env_dir)
          return(invisible(NULL))
     }

     message("MOSAIC Python environment found at: ", env_dir)

     # Prompt for confirmation if not forced
     if (!force) {
          ans <- readline(prompt = "Are you sure you want to remove the MOSAIC Python environment? [y/N]: ")
          if (tolower(ans) != "y") {
               message("Cancelled removal of MOSAIC Python environment.")
               return(invisible(NULL))
          }
     }

     Sys.unsetenv("RETICULATE_PYTHON")
     message("RETICULATE_PYTHON global variable has been reset.")

     message("Unlinking MOSAIC Python environment...")
     unlink(env_dir, recursive = TRUE, force = TRUE)

     message("Cleaning up...")
     if (.Platform$OS.type == "windows") {
          system_cmd <- paste("rmdir /S /Q", shQuote(env_dir))
     } else {
          system_cmd <- paste("rm -rf", shQuote(env_dir))
     }

     system(system_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)

     # Confirm removal
     if (!dir.exists(env_dir)) {
          message("MOSAIC Python environment successfully removed.")
     } else {
          message("Failed to remove MOSAIC Python environment at ", env_dir)
     }
}
