#' Check Installed R and Python Dependencies for MOSAIC
#'
#' @description
#' This function checks the MOSAIC Python virtual environment, installed Python packages,
#' and verifies that the `keras3` and `tensorflow` R packages are installed and configured correctly.
#' It prints the currently active Python environment and confirms whether the backend is working.
#'
#' @return No return value. Prints diagnostic messages about environment status and package versions.
#' @export
#'

check_installed_dependencies <- function(pkgname = 'MOSAIC') {

     # Check reticulate
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

     # Locate virtualenv path
     python_env_path <- system.file("py/mosaic-python-env", package = pkgname)

     if (!dir.exists(python_env_path)) {
          message("❌ Python virtual environment not found.")
          message("To finish setup, run: MOSAIC::install_dependencies()")
          return()
     } else {
          message("🐍 Found Python virtual environment")
     }

     # Activate the Python environment
     reticulate::use_virtualenv(python_env_path, required = TRUE)
     sys <- reticulate::import("sys")

     # Print env info
     tryCatch({
          py_env_path <- reticulate::py_get_attr(sys, "prefix")
          message("✔️  Virtual Environment Root: ", py_env_path)
     }, error = function(e){
          message("❌ Warning: Unable to retrieve virtual environment root.")
     })

     tryCatch({
          py_exec <- reticulate::py_get_attr(sys, "executable")
          message("✔️  Python Executable: ", py_exec)
     }, error = function(e){
          message("❌ Warning: Unable to retrieve Python executable path.")
     })

     tryCatch({
          py_version <- reticulate::py_get_attr(sys, "version")
          py_version <- strsplit(as.character(py_version), " ")[[1]][1]
          packageStartupMessage("✔️  Python Version: ", py_version)
     }, error = function(e){
          message("❌ Warning: Unable to retrieve Python version.")
     })

     # -----------------------------
     # Check Python package versions
     # -----------------------------

     # Locate the requirements.txt file
     requirements_path <- system.file("py/requirements.txt", package = "MOSAIC")

     # Read and extract package names
     req_lines <- readLines(requirements_path)
     pkg_names <- unique(req_lines)

     # Loop through and check each package
     for (pkg in pkg_names) {
          tryCatch({
               module <- reticulate::import(pkg, delay_load = TRUE)
               version <- module[["__version__"]]
               message(sprintf("✔️  %s: %s", pkg, version))
          }, error = function(e) {
               message(sprintf("❌ %s is not available in the Python environment.", pkg))
          })
     }

     message("")

     # ------------------------
     # R package checks
     # ------------------------

     if (!requireNamespace("keras3", quietly = TRUE)) {
          message("❌ The 'keras3' package is required but not installed. Please install it with install.packages('keras3').")
          return(invisible(NULL))
     } else {
          message("📦 R package 'keras3' is installed.")
     }

     if (!requireNamespace("tensorflow", quietly = TRUE)) {
          message("❌ The 'tensorflow' package is required but not installed. Please install it with install.packages('tensorflow').")
          return(invisible(NULL))
     } else {
          message("📦 R package 'tensorflow' is installed.")
     }

     # ------------------------
     # Keras3 backend check
     # ------------------------

     if (requireNamespace("keras3", quietly = TRUE) && requireNamespace("tensorflow", quietly = TRUE)) {

          backend_name <- tryCatch({
               keras3::config_backend()
          }, error = function(e) {
               message("❌ Keras backend is not available or failed to load. ", e$message)
               return(invisible(NULL))
          })

          if (!is.null(backend_name)) {
               message("✔️  Keras backend detected.")
          } else {
               message("Keras is not connected to a valid backend. Run keras::install_keras() or install_dependencies(force = TRUE).")
               return(invisible(NULL))
          }

          tf_info <- tryCatch(
               unlist(tensorflow::tf_config()),
               error = function(e) {
                    message("❌ Cannot confirm tensorflow backend.")
                    return(invisible(NULL))
               }
          )

          if (as.logical(tf_info['available'])) {
               message("✔️  Keras backend: tensorflow ", tf_info[['version_str']])
               message("✔️  Keras backend path: ", tf_info[['python']])
          }

     } else {
          message("❌ Cannot confirm tensorflow backend.")
     }


}
