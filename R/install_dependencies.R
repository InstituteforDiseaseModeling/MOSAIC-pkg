#' Install R and Python Dependencies for MOSAIC
#'
#' @description
#' Sets up a conda environment for MOSAIC using the `environment.yml` file located in the package.
#' The environment is stored in a fixed directory ("~/.virtualenvs/r-mosaic") and installs the LASER disease
#' transmission model simulation tool. This function also installs the R packages `keras3` and `tensorflow`,
#' and ensures that the Keras + TensorFlow Python backend is available for use in R.
#'
#' @param force Logical. If TRUE, deletes and recreates the conda environment from scratch. Default is FALSE.
#'
#' @return No return value
#' @export
#'

install_dependencies <- function(force = FALSE) {

     cli::cli_h1("Installing MOSAIC Dependencies")
     Sys.unsetenv("RETICULATE_PYTHON")

     # -----------------------------------------------------------------------
     # Ensure required R packages (reticulate and yaml) are available.
     # -----------------------------------------------------------------------

     if (!requireNamespace("reticulate", quietly = TRUE)) {
          cli::cli_alert_info("Installing R package: reticulate")
          utils::install.packages("reticulate", dependencies = TRUE)
     }

     if (!requireNamespace("yaml", quietly = TRUE)) {
          cli::cli_alert_info("Installing R package: yaml")
          utils::install.packages("yaml", dependencies = TRUE)
     }

     reticulate <- getNamespace("reticulate")


     # -----------------------------------------------------------------------
     # Check for Conda availability.
     # -----------------------------------------------------------------------

     conda_path <- Sys.which("conda")
     conda_path_reticulate <- reticulate::miniconda_path()

     # Check if conda binary actually exists (not just if path is non-empty)
     conda_binary_exists <- FALSE
     if (nzchar(conda_path)) {
          conda_binary_exists <- TRUE
     } else if (nzchar(conda_path_reticulate)) {
          # Check if conda binary exists in the miniconda path
          potential_conda <- file.path(conda_path_reticulate, "bin", "conda")
          if (file.exists(potential_conda)) {
               conda_binary_exists <- TRUE
          }
     }

     if (conda_binary_exists) {

          cli::cli_alert_success("Conda is available at: {conda_path} {conda_path_reticulate}")

     } else {

          cli::cli_alert_warning("No conda installation found. Installing Miniconda...")
          reticulate::install_miniconda(force)
          new_conda_path <- reticulate::miniconda_path()
          cli::cli_alert_success("Miniconda installed at: {new_conda_path}")
     }

     # Unset any forced Python settings.
     Sys.unsetenv("RETICULATE_PYTHON")


     # -----------------------------------------------------------------------
     # Locate environment.yml in the package.
     # -----------------------------------------------------------------------

     env_yml_path <- system.file("py", "environment.yml", package = "MOSAIC")
     if (!file.exists(env_yml_path)) cli::cli_abort("environment.yml not found at: {env_yml_path}")
     cli::cli_alert_info("Using environment.yml at: {env_yml_path}")


     # -----------------------------------------------------------------------
     # Get the desired Python environment paths using get_python_paths().
     # -----------------------------------------------------------------------

     paths <- MOSAIC::get_python_paths()


     # -----------------------------------------------------------------------
     # Remove existing conda environment if force = TRUE.
     # -----------------------------------------------------------------------

     if (force && dir.exists(paths$env)) {

          cli::cli_alert_info("Removing existing conda environment (force = TRUE) at: {paths$env}")
          unlink(paths$env, recursive = TRUE, force = TRUE)

          system_cmd <- if (.Platform$OS.type == "windows") {
               paste("rmdir /S /Q", shQuote(paths$env))
          } else {
               paste("rm -rf", shQuote(paths$env))
          }

          system(system_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
     }


     # -----------------------------------------------------------------------
     # Parse environment.yml and extract dependencies.
     # -----------------------------------------------------------------------
     env_specs <- yaml::read_yaml(env_yml_path)
     deps <- env_specs$dependencies

     conda_packages <- c()
     pip_packages <- c()

     for (dep in deps) {

          if (is.character(dep)) {
               conda_packages <- c(conda_packages, dep)
          } else if (is.list(dep) && !is.null(dep$pip)) {
               pip_packages <- c(pip_packages, dep$pip)
          }
     }

     # Extract Python version if specified (e.g., "python=3.12").
     py_version <- NULL
     if (any(grepl("^python=", conda_packages))) {
          py_version <- sub("^python=", "", conda_packages[grep("^python=", conda_packages)][1])
          conda_packages <- conda_packages[!grepl("^python=", conda_packages)]
     }


     # -----------------------------------------------------------------------
     # Create or update the conda environment.
     # -----------------------------------------------------------------------

     if (!dir.exists(paths$env)) {

          cli::cli_alert_info("Creating new conda environment at: {paths$env}")
          reticulate::conda_create(envname = paths$env,
                                   python_version = py_version,
                                   packages = conda_packages)

          if (length(pip_packages) > 0) {
               reticulate::conda_install(envname = paths$env,
                                         packages = pip_packages,
                                         pip = TRUE)
          }

     } else {

          cli::cli_alert_info("Conda environment already exists at: {paths$env}. Updating dependencies...")
          if (length(conda_packages) > 0) {
               reticulate::conda_install(envname = paths$env,
                                         packages = conda_packages,
                                         pip = FALSE)
          }

          if (length(pip_packages) > 0) {
               reticulate::conda_install(envname = paths$env,
                                         packages = pip_packages,
                                         pip = TRUE)
          }
     }


     # -----------------------------------------------------------------------
     # Verify that the expected Python executable exists.
     # -----------------------------------------------------------------------

     if (!file.exists(paths$exe)) cli::cli_abort("Python executable not found at {py_exe}. The conda environment may not have been created properly.")


     # -----------------------------------------------------------------------
     # Activate the conda environment for reticulate.
     # -----------------------------------------------------------------------

     paths <- MOSAIC::get_python_paths()
     Sys.setenv(RETICULATE_PYTHON = paths$norm)
     reticulate::use_condaenv(paths$norm, required = TRUE)
     config <- reticulate::py_config()

     if (grepl(paths$env, config$python)) {

          cli::cli_alert_success("Reticulate has activated the conda environment at '{paths$env}' for MOSAIC!")
          cli::cli_alert_info("Python configuration:")
          print(config)
          cli::cli_text(paste0("RETICULATE_PYTHON: ", Sys.getenv('RETICULATE_PYTHON')))
          cli::cli_text("")

     } else {

          cli::cli_abort("Failed to activate the conda environment at {paths$env}.\nActivated Python: {config$python}")
     }

     cli::cli_text("To remove the current installation, run {.run MOSAIC::remove_MOSAIC_python_env()}.")
     cli::cli_text("To check setup, run {.run MOSAIC::check_dependencies()}.")

}
