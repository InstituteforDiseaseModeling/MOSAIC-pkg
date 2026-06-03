#' Install R and Python Dependencies for MOSAIC
#'
#' @description
#' Provisions the MOSAIC Python environment using [uv](https://docs.astral.sh/uv/),
#' the fast Python package manager that backs reticulate's default workflow. The
#' environment is a standard virtual environment stored at `~/.virtualenvs/r-mosaic`
#' (see [get_python_paths()]). uv supplies a standalone Python interpreter (the
#' version is read from `inst/python/python_version.txt`) — no conda or Miniconda
#' installation is required — and installs the packages declared in
#' `inst/python/requirements.txt`, including the LASER disease transmission engine
#' (`laser-cholera`).
#'
#' After provisioning, the environment is activated for the current R session via
#' [reticulate::use_virtualenv()].
#'
#' @param force Logical. If TRUE, deletes and recreates the virtual environment from
#'   scratch. Default is FALSE.
#'
#' @details
#' Requires the `uv` binary on the system `PATH` (or in a standard install location
#' such as `~/.local/bin`). Install it once with:
#'
#' ```sh
#' curl -LsSf https://astral.sh/uv/install.sh | sh
#' ```
#'
#' @return No return value. Called for its side effect of provisioning and activating
#'   the `r-mosaic` virtual environment.
#'
#' @section Errors:
#' Aborts (via [cli::cli_abort()]) if the `uv` binary cannot be located, if
#' `requirements.txt` is missing from the package, if `uv` fails to create the
#' environment or install packages, or if reticulate cannot activate the resulting
#' environment.
#'
#' @seealso [check_dependencies()] to verify the installed environment,
#'   [remove_python_env()] to delete it.
#'
#' @export
#'

install_dependencies <- function(force = FALSE) {

     cli::cli_h1("Installing MOSAIC Dependencies")
     Sys.unsetenv("RETICULATE_PYTHON")

     # -----------------------------------------------------------------------
     # Ensure required R package (reticulate) is available.
     # -----------------------------------------------------------------------

     if (!requireNamespace("reticulate", quietly = TRUE)) {
          cli::cli_alert_info("Installing R package: reticulate")
          utils::install.packages("reticulate", dependencies = TRUE)
     }


     # -----------------------------------------------------------------------
     # Locate the uv binary (reticulate's default Python provisioner).
     # -----------------------------------------------------------------------

     uv <- .mosaic_find_uv()
     if (is.null(uv)) {
          cli::cli_abort(c(
               "Could not find the {.code uv} binary required to provision the Python environment.",
               "i" = "Install uv with: {.code curl -LsSf https://astral.sh/uv/install.sh | sh}",
               "i" = "Then restart R and run {.run MOSAIC::install_dependencies()} again."
          ))
     }
     cli::cli_alert_success("Found uv at: {uv}")


     # -----------------------------------------------------------------------
     # Locate the requirements file and target Python version in the package.
     # -----------------------------------------------------------------------

     req_path <- system.file("python", "requirements.txt", package = "MOSAIC")
     if (!nzchar(req_path) || !file.exists(req_path)) {
          cli::cli_abort("requirements.txt not found in the MOSAIC package (inst/python/requirements.txt).")
     }
     cli::cli_alert_info("Using requirements.txt at: {req_path}")

     py_version <- .mosaic_python_version()
     cli::cli_alert_info("Target Python version: {py_version}")


     # -----------------------------------------------------------------------
     # Get the desired Python environment paths using get_python_paths().
     # -----------------------------------------------------------------------

     paths <- MOSAIC::get_python_paths()


     # -----------------------------------------------------------------------
     # Remove the existing environment if force = TRUE.
     # -----------------------------------------------------------------------

     if (force && dir.exists(paths$env)) {
          cli::cli_alert_info("Removing existing environment (force = TRUE) at: {paths$env}")
          unlink(paths$env, recursive = TRUE, force = TRUE)
     }


     # -----------------------------------------------------------------------
     # Create the virtual environment with uv (downloads a standalone Python of
     # the requested version if one is not already managed by uv).
     # -----------------------------------------------------------------------

     if (!dir.exists(paths$env)) {
          cli::cli_alert_info("Creating virtual environment at: {paths$env}")
          status <- system2(uv,
                            args = c("venv", shQuote(paths$env),
                                     "--python", py_version, "--seed"))
          if (!identical(status, 0L)) {
               cli::cli_abort("uv failed to create the virtual environment at {paths$env} (exit status {status}).")
          }
     } else {
          cli::cli_alert_info("Virtual environment already exists at: {paths$env}. Updating dependencies...")
     }


     # -----------------------------------------------------------------------
     # Install / update packages from requirements.txt into the environment.
     # -----------------------------------------------------------------------

     cli::cli_alert_info("Installing Python packages from requirements.txt...")
     status <- system2(uv,
                       args = c("pip", "install",
                                "--python", shQuote(paths$exec),
                                "-r", shQuote(req_path)))
     if (!identical(status, 0L)) {
          cli::cli_abort("uv failed to install Python packages from {req_path} (exit status {status}).")
     }


     # -----------------------------------------------------------------------
     # Verify that the expected Python executable exists.
     # -----------------------------------------------------------------------

     if (!file.exists(paths$exec)) {
          cli::cli_abort("Python executable not found at {paths$exec}. The virtual environment may not have been created properly.")
     }


     # -----------------------------------------------------------------------
     # Activate the r-mosaic virtualenv for reticulate.
     # -----------------------------------------------------------------------

     # Use the non-normalized venv exec path: uv venvs symlink bin/python to a
     # shared managed interpreter, so normalizePath() would resolve away the venv.
     Sys.setenv(RETICULATE_PYTHON = paths$exec)
     reticulate::use_virtualenv(paths$env, required = TRUE)
     config <- reticulate::py_config()

     if (grepl(paths$env, config$python, fixed = TRUE)) {

          cli::cli_alert_success("Reticulate has activated the r-mosaic virtualenv at '{paths$env}' for MOSAIC!")
          cli::cli_alert_info("Python configuration:")
          print(config)
          cli::cli_text(paste0("RETICULATE_PYTHON: ", Sys.getenv('RETICULATE_PYTHON')))
          cli::cli_text("")

     } else {

          cli::cli_abort("Failed to activate the virtual environment at {paths$env}.\nActivated Python: {config$python}")
     }

     cli::cli_text("To remove the current installation, run {.run MOSAIC::remove_python_env()}.")
     cli::cli_text("To check setup, run {.run MOSAIC::check_dependencies()}.")

}


#' Locate the uv binary
#'
#' @description
#' Internal helper that searches the system `PATH` and common per-user install
#' locations for the `uv` binary used to provision the MOSAIC Python environment.
#'
#' @return Character path to the `uv` binary, or NULL if it cannot be found.
#'
#' @keywords internal
#' @noRd
.mosaic_find_uv <- function() {

     on_path <- Sys.which("uv")
     if (nzchar(on_path)) return(unname(on_path))

     exe <- if (.Platform$OS.type == "windows") "uv.exe" else "uv"
     candidates <- c(
          file.path("~", ".local", "bin", exe),
          file.path("~", ".cargo", "bin", exe)
     )
     candidates <- path.expand(candidates)

     for (cand in candidates) {
          if (file.exists(cand)) return(normalizePath(cand, winslash = "/", mustWork = FALSE))
     }

     NULL
}


#' Read the target Python version for the MOSAIC environment
#'
#' @description
#' Internal helper that reads `inst/python/python_version.txt` from the installed
#' MOSAIC package and returns the declared Python version (e.g. "3.12").
#'
#' @return Character scalar with the target Python version. Falls back to "3.12"
#'   if the file cannot be located.
#'
#' @keywords internal
#' @noRd
.mosaic_python_version <- function() {

     vpath <- system.file("python", "python_version.txt", package = "MOSAIC")
     if (!nzchar(vpath) || !file.exists(vpath)) return("3.12")

     ver <- readLines(vpath, warn = FALSE)
     ver <- trimws(ver[nzchar(trimws(ver))])
     if (length(ver) == 0L) return("3.12")
     ver[1]
}
