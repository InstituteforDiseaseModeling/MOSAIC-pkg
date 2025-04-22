#' Check Installed R and Python Dependencies for MOSAIC
#'
#' @description
#' This function checks the MOSAIC Python conda environment, verifies that the expected Python packages are installed,
#' and confirms that the R packages `keras3` and `tensorflow` are present and configured correctly.
#' It prints the currently active Python configuration and confirms whether the backend is working.
#'
#' @return No return value. Prints diagnostic messages about environment status and package versions.
#' @export
#'

check_dependencies <- function() {

     message("Checking Python and R dependencies for MOSAIC...")

     pkgname <- "MOSAIC"

     if (!requireNamespace("reticulate", quietly = TRUE)) {
          message("❌ The 'reticulate' package is required but not installed. Please install it with install.packages('reticulate').")
          return()
     }

     if (!requireNamespace("keras3", quietly = TRUE)) {
          message("❌ The 'keras3' package is required but not installed. Please install it with install.packages('keras3').")
          return()
     }

     if (!requireNamespace("tensorflow", quietly = TRUE)) {
          message("❌ The 'tensorflow' package is required but not installed. Please install it with install.packages('tensorflow').")
          return()
     }

     reticulate <- getNamespace("reticulate")
     keras3 <- getNamespace("keras3")
     tensorflow <- getNamespace("tensorflow")

     # -----------------------------------------------------------------------
     # Locate the conda environment directory created by install_dependencies
     # -----------------------------------------------------------------------
     env_dir <- normalizePath(file.path("~", ".MOSAIC_conda_env"), winslash = "/", mustWork = FALSE)

     # Determine Python executable location in the conda environment
     python_exec <- if (.Platform$OS.type == "windows") {
          file.path(env_dir, "Scripts", "python.exe")
     } else {
          file.path(env_dir, "bin", "python")
     }

     if (!dir.exists(env_dir) || !file.exists(python_exec)) {
          message("❌ Conda environment not found or incomplete at ", env_dir)
          cli::cli_text("To finish setup, run {.run MOSAIC::install_dependencies()}. To re-install, run {.run MOSAIC::install_dependencies(force=T)}.")
          return(invisible(NULL))
     } else {
          message("🐍 Found Python conda environment")
     }

     # -----------------------------------------------------------------------
     # Activate the conda environment using reticulate
     # -----------------------------------------------------------------------
     reticulate::use_condaenv(env_dir, required = TRUE)
     config <- reticulate::py_config()

     # Import system module to retrieve additional attributes
     sys <- tryCatch(reticulate::import("sys"), error = function(e) {
          message("❌ Unable to import Python 'sys' module: ", e$message)
          return(NULL)
     })

     if (!is.null(sys)) {

          tryCatch({
               py_env_path <- reticulate::py_get_attr(sys, "prefix")
               message("✔️  Virtual Environment Root: ", py_env_path)
          }, error = function(e){
               message("❌ Warning: Unable to retrieve environment root: ", e$message)
          })

          tryCatch({
               py_exec <- reticulate::py_get_attr(sys, "executable")
               message("✔️  Python Executable: ", py_exec)
          }, error = function(e){
               message("❌ Warning: Unable to retrieve Python executable: ", e$message)
          })

          tryCatch({
               py_version <- reticulate::py_get_attr(sys, "version")
               py_version <- strsplit(as.character(py_version), " ")[[1]][1]
               message("✔️  Python Version: ", py_version)
          }, error = function(e){
               message("❌ Warning: Unable to retrieve Python version: ", e$message)
          })

     }

     # -----------------------------------------------------------------------
     # Check Python package versions based on environment.yml
     # -----------------------------------------------------------------------
     env_yml_path <- system.file("py", "environment.yml", package = pkgname)

     if (file.exists(env_yml_path)) {

          env_specs <- yaml::read_yaml(env_yml_path)
          deps <- env_specs$dependencies

          pkg_names <- c()

          for (dep in deps) {

               if (is.character(dep)) {
                    pkg_names <- c(pkg_names, dep)
               } else if (is.list(dep) && !is.null(dep$pip)) {
                    pkg_names <- c(pkg_names, unlist(dep$pip))
               }

          }

          pkg_names <- unique(pkg_names)
          pkg_names <- pkg_names[!grepl("^python=", pkg_names)]

          # Replace GitHub install line (if present) with the importable package name.
          sel <- grep("laser-cholera", pkg_names)
          if (length(sel) > 0) pkg_names[sel] <- "laser_cholera"

          for (pkg in pkg_names) {

               tryCatch({

                    module <- reticulate::import(pkg, delay_load = TRUE)
                    version <- module[["__version__"]]
                    message(sprintf("✔️  %s: %s", pkg, version))

                    if (pkg == "laser_cholera") {

                         message("   LASER details:")
                         freeze <- system2(command = python_exec, args = c("-m", "pip", "freeze"), stdout = TRUE)
                         laser_lines <- grep("laser", freeze, value = TRUE)
                         laser_lines <- paste0("   ", laser_lines)
                         message(paste(laser_lines, collapse = "\n"))

                    }

               }, error = function(e) {

                    message(sprintf("❌ %s cannot be found in the Python environment.", pkg))

               })
          }
     } else {

          message("❌ environment.yml not found in package.")
     }

     message("")

     # -----------------------------------------------------------------------
     # R package checks
     # -----------------------------------------------------------------------

     message("Checking R package dependencies...")
     message("✔️  ", R.version.string)

     if (!requireNamespace("tensorflow", quietly = TRUE)) {
          message("❌ The 'tensorflow' package is required but not installed. Please install it with install.packages('tensorflow').")
          return(invisible(NULL))
     } else {
          message("✔️  tensorflow: ", as.character(packageVersion("tensorflow")))
     }

     if (!requireNamespace("keras3", quietly = TRUE)) {
          message("❌ The 'keras3' package is required but not installed. Please install it with install.packages('keras3').")
          return(invisible(NULL))
     } else {
          message("✔️  keras3: ", as.character(packageVersion("keras3")))
     }



     # -----------------------------------------------------------------------
     # Keras3 backend check
     # -----------------------------------------------------------------------
     backend_name <- tryCatch({
          keras3::config_backend()
     }, error = function(e) {
          message("❌ keras3 backend is not available or failed to load. ", e$message)
          return(invisible(NULL))
     })

     if (!is.null(backend_name)) {
          message("✔️  keras3 backend detected.")
     } else {
          message("❌ keras3 is not connected to a valid backend. Run keras::install_keras() or MOSAIC::install_dependencies(force = TRUE).")
          return(invisible(NULL))
     }

     tf_info <- tryCatch(
          unlist(tensorflow::tf_config()),
          error = function(e) {
               message("❌ Cannot confirm tensorflow backend: ", e$message)
               return(invisible(NULL))
          }
     )

     if (!is.null(tf_info) && as.logical(tf_info["available"])) {
          message("✔️  keras3 backend: tensorflow ", tf_info[["version_str"]])
          message("✔️  keras3 backend Python path: ", tf_info[["python"]])
     } else {
          message("❌ TensorFlow backend does not appear to be available.")
     }

}
