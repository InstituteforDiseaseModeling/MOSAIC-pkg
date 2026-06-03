#' Get Python Environment Paths
#'
#' @description
#' Returns the paths related to the desired MOSAIC Python environment
#'
#' @return A list with the following named elements:
#' \describe{
#'   \item{env}{The full absolute path to the desired Python environment directory
#'   (the root folder of the virtual environment).}
#'   \item{exec}{The expected path to the Python executable within that environment,
#'   constructed using OS-specific conventions.}
#'   \item{norm}{The normalized (canonical) absolute path of the Python executable with
#'   symbolic links and/or relative components resolved. Note: for uv-provisioned
#'   virtual environments `bin/python` is a symlink to a shared managed interpreter,
#'   so `norm` resolves to that base interpreter and loses the venv identity. Use
#'   `exec` (not `norm`) when setting `RETICULATE_PYTHON` or matching the venv path.}
#' }
#'
#' @export
#'

get_python_paths <- function() {

     # Set the desired environment directory.
     desired_env_dir <- normalizePath(file.path("~", ".virtualenvs", "r-mosaic"), winslash = "/", mustWork = FALSE)

     # Construct the expected Python executable path based on OS.
     desired_python_exec <- if (.Platform$OS.type == "windows") {
          file.path(desired_env_dir, "Scripts", "python.exe")
     } else {
          file.path(desired_env_dir, "bin", "python")
     }

     # Normalize the Python executable path.
     desired_python_norm <- normalizePath(desired_python_exec, winslash = "/", mustWork = FALSE)

     # Return a list with elements 'env', 'exec', and 'norm'
     list(env = desired_env_dir,
          exec = desired_python_exec,
          norm = desired_python_norm)

}
