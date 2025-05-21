#' Install R and Python Dependencies for MOSAIC using a Conda Environment at a Fixed Location
#'
#' @description
#' Sets up a conda environment for MOSAIC using the `environment.yml` file located in the package.
#' The environment is stored in a fixed, cross-platform directory ("~/.MOSAIC_conda_env"). Installs
#' the R packages `keras3` and `tensorflow` if needed, and ensures that the Keras + TensorFlow
#' Python backend is available for use in R.
#'
#' @param force Logical. If TRUE, deletes and recreates the conda environment from scratch. Default is FALSE.
#'
#' @return No return value. Side effect: installs R and Python dependencies in a conda environment.
#' @export
#'
install_dependencies2 <- function(force = FALSE) {

     Sys.unsetenv("RETICULATE_PYTHON")

     # Set the conda environment directory manually (cross-platform, stored in the user's home directory)
     env_dir <- normalizePath(file.path("~", ".MOSAIC_conda_env"), winslash = "/", mustWork = FALSE)
     message("Conda environment will be stored at: ", env_dir)

     # -----------------------------------------------------------------------
     # 0. Ensure required R packages (reticulate and yaml) are available
     # -----------------------------------------------------------------------
     if (!requireNamespace("reticulate", quietly = TRUE)) {
          message("Installing R package: reticulate")
          utils::install.packages("reticulate", dependencies = TRUE)
     }
     if (!requireNamespace("yaml", quietly = TRUE)) {
          message("Installing R package: yaml")
          utils::install.packages("yaml", dependencies = TRUE)
     }
     reticulate <- getNamespace("reticulate")

     # -----------------------------------------------------------------------
     # 1. Locate the environment.yml file in the package using system.file
     # -----------------------------------------------------------------------
     env_yml_path <- system.file("py", "environment.yml", package = "MOSAIC")
     if (!file.exists(env_yml_path)) {
          stop("environment.yml not found at: ", env_yml_path)
     }
     message("Using environment.yml at: ", env_yml_path)

     # -----------------------------------------------------------------------
     # 2. Remove the existing conda environment if force == TRUE
     # -----------------------------------------------------------------------
     if (force && dir.exists(env_dir)) {
          message("Removing existing conda environment (force = TRUE): ", env_dir)
          unlink(env_dir, recursive = TRUE, force = TRUE)
     }

     # -----------------------------------------------------------------------
     # 3. Create or update the conda environment using reticulate and environment.yml
     # -----------------------------------------------------------------------
     # Parse the YAML file to extract dependencies
     env_specs <- yaml::read_yaml(env_yml_path)
     deps <- env_specs$dependencies

     # Separate out conda and pip dependencies
     conda_packages <- c()
     pip_packages <- c()
     for (dep in deps) {
          if (is.character(dep)) {
               conda_packages <- c(conda_packages, dep)
          } else if (is.list(dep) && !is.null(dep$pip)) {
               pip_packages <- c(pip_packages, dep$pip)
          }
     }

     # Extract Python version if specified (e.g., "python=3.12")
     py_version <- NULL
     if (any(grepl("^python=", conda_packages))) {
          py_version <- sub("^python=", "", conda_packages[grep("^python=", conda_packages)][1])
          conda_packages <- conda_packages[!grepl("^python=", conda_packages)]
     }

     # Check if the environment exists (by checking if the directory exists)
     env_exists <- dir.exists(env_dir)
     if (!env_exists) {
          message("Creating new conda environment at: ", env_dir)
          reticulate::conda_create(envname = env_dir,
                                   python_version = py_version,
                                   packages = conda_packages)
          if (length(pip_packages) > 0) {
               reticulate::conda_install(envname = env_dir,
                                         packages = pip_packages,
                                         pip = TRUE)
          }
     } else {
          message("Conda environment already exists at: ", env_dir, ". Updating dependencies ...")
          if (length(conda_packages) > 0) {
               reticulate::conda_install(envname = env_dir,
                                         packages = conda_packages,
                                         pip = FALSE)
          }
          if (length(pip_packages) > 0) {
               reticulate::conda_install(envname = env_dir,
                                         packages = pip_packages,
                                         pip = TRUE)
          }
     }

     # -----------------------------------------------------------------------
     # 4. Explicitly verify that the expected Python executable exists
     # -----------------------------------------------------------------------
     py_exe <- file.path(env_dir, "bin", "python")
     if (!file.exists(py_exe)) stop("Python executable not found at ", py_exe, ". The conda environment may not have been created properly.")

     # -----------------------------------------------------------------------
     # 5. Use this conda environment for reticulate
     # -----------------------------------------------------------------------
     reticulate::use_condaenv(env_dir, required = TRUE)

     # Retrieve the current Python configuration.
     config <- reticulate::py_config()

     # Check that the active python executable is located within your desired environment.
     if (grepl(env_dir, config$python)) {
          message("\n🚀 Reticulate has activated the conda environment at '", env_dir, "' for MOSAIC!")
          message("Python configuration:")
          print(config)
          message("")
     } else {
          stop("Failed to activate the conda environment at ", env_dir,
               "\nActivated Python: ", config$python)
     }


}
