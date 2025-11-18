#' Check Installed R and Python Dependencies for MOSAIC
#'
#' @description
#' This function checks the MOSAIC Python conda environment, verifies that the expected Python
#' packages are installed, and confirms that the R packages `keras3` and `tensorflow` are present and
#' configured correctly. It prints the currently active Python configuration and confirms whether the
#' backend is working.
#'
#' @return No return value. Prints diagnostic messages about environment status and package versions.
#' @export
#'
check_dependencies <- function() {

     cli::cli_h1("Checking Python and R dependencies for MOSAIC")
     pkgname <- "MOSAIC"

     # Check for required R packages.
     if (!requireNamespace("reticulate", quietly = TRUE)) {
          cli::cli_alert_danger("The 'reticulate' package is required but not installed. Please install it with install.packages('reticulate').")
          return()
     }
     if (!requireNamespace("keras3", quietly = TRUE)) {
          cli::cli_alert_danger("The 'keras3' package is required but not installed. Please install it with install.packages('keras3').")
          return()
     }
     if (!requireNamespace("tensorflow", quietly = TRUE)) {
          cli::cli_alert_danger("The 'tensorflow' package is required but not installed. Please install it with install.packages('tensorflow').")
          return()
     }

     reticulate <- getNamespace("reticulate")
     keras3     <- getNamespace("keras3")
     tensorflow <- getNamespace("tensorflow")

     # -----------------------------------------------------------------------
     # Get the desired Python environment paths using MOSAIC::get_python_paths()
     # -----------------------------------------------------------------------
     paths <- MOSAIC::get_python_paths()

     # Check that the desired environment exists.
     if (!dir.exists(paths$env) || !file.exists(paths$exe)) {
          cli::cli_alert_danger("Conda environment not found or incomplete at {paths$env}.")
          cli::cli_text("To finish setup, run {.run MOSAIC::install_dependencies()}. To re-install, run {.run MOSAIC::install_dependencies(force=T)}.")
          return(invisible(NULL))
     } else {
          cli::cli_alert_success("Found Python conda environment at {paths$env}.")
     }

     # -----------------------------------------------------------------------
     # Properly initialize Python environment using attach_mosaic_env
     # This ensures Python is initialized correctly, especially in non-interactive sessions
     # -----------------------------------------------------------------------

     tryCatch({
          MOSAIC::attach_mosaic_env(silent = TRUE)
     }, error = function(e) {
          cli::cli_alert_danger("Failed to attach Python environment: {e$message}")
          cli::cli_text("To diagnose: {.run MOSAIC::check_python_env()}")
          cli::cli_text("To reinstall: {.run MOSAIC::install_dependencies(force=TRUE)}")
          return(invisible(NULL))
     })


     # -----------------------------------------------------------------------
     # Import sys module to retrieve additional Python attributes.
     # -----------------------------------------------------------------------
     sys <- tryCatch(reticulate::import("sys"), error = function(e) {
          cli::cli_alert_danger("Unable to import Python 'sys' module: {e$message}")
          return(NULL)
     })

     if (!is.null(sys)) {
          tryCatch({
               py_env_path <- reticulate::py_get_attr(sys, "prefix")
               cli::cli_alert_success("Virtual Environment Root: {py_env_path}")
          }, error = function(e) {
               cli::cli_alert_warning("Unable to retrieve environment root: {e$message}")
          })

          tryCatch({
               py_exec_value <- reticulate::py_get_attr(sys, "executable")
               cli::cli_alert_success("Python Executable: {py_exec_value}")
          }, error = function(e) {
               cli::cli_alert_warning("Unable to retrieve Python executable: {e$message}")
          })

          tryCatch({
               py_version <- reticulate::py_get_attr(sys, "version")
               py_version <- strsplit(as.character(py_version), " ")[[1]][1]
               cli::cli_alert_success("Python Version: {py_version}")
          }, error = function(e) {
               cli::cli_alert_warning("Unable to retrieve Python version: {e$message}")
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

          sel <- grep("laser-cholera", pkg_names)
          if (length(sel) > 0) pkg_names[sel] <- "laser_cholera"

          sel <- grep("laser-core", pkg_names)
          if (length(sel) > 0) pkg_names[sel] <- "laser_core"

          for (pkg_spec in pkg_names) {

               # Extract package name from version specifications
               # Handle patterns like: "numpy=2.1.3", "sbi==0.23.2", "pytorch::pytorch=2.5.1"
               pkg_import_name <- pkg_spec

               # Remove channel prefix if present (e.g., "pytorch::pytorch" -> "pytorch")
               pkg_import_name <- sub("^[^:]+::", "", pkg_import_name)

               # Extract base package name (remove version specs)
               pkg_import_name <- sub("[=><!].*$", "", pkg_import_name)

               # Handle special import name mappings
               import_map <- c(
                    "pytorch" = "torch",
                    "scikit-learn" = "sklearn",
                    "laser-cholera" = "laser_cholera",
                    "laser-core" = "laser_core"
               )

               if (pkg_import_name %in% names(import_map)) {
                    pkg_import_name <- import_map[[pkg_import_name]]
               }

               # Skip special entries
               if (pkg_import_name %in% c("pip", "python")) next
               if (grepl("^https?://", pkg_spec)) next
               if (grepl("^libblas", pkg_import_name)) next

               tryCatch({

                    module <- reticulate::import(pkg_import_name, delay_load = TRUE)
                    version <- module[["__version__"]]

                    # Show both the expected spec and actual version
                    if (grepl("[=><!]", pkg_spec)) {
                         expected <- sub("^[^=><!]+", "", pkg_spec)
                         cli::cli_alert_success("{pkg_import_name}: {version} (expected {expected})")
                    } else {
                         cli::cli_alert_success("{pkg_import_name}: {version}")
                    }

                    if (pkg_import_name == "laser_cholera") {
                         cli::cli_alert_info("LASER built with:")
                         freeze <- system2(command = paths$exe, args = c("-m", "pip", "freeze"), stdout = TRUE)
                         laser_lines <- grep("laser", freeze, value = TRUE)
                         laser_lines <- paste0("   ", laser_lines)
                         cli::cli_text("{laser_lines}")
                    }

               }, error = function(e) {
                    cli::cli_alert_danger("{pkg_import_name} cannot be found in the Python environment (from spec: {pkg_spec}).")
               })
          }

     } else {
          cli::cli_alert_danger("environment.yml not found in package.")
     }

     # -----------------------------------------------------------------------
     # Check R package dependencies.
     # -----------------------------------------------------------------------

     cli::cli_h2("Checking R package dependencies")
     cli::cli_alert_success("{R.version.string}")

     if (!requireNamespace("tensorflow", quietly = TRUE)) {
          cli::cli_alert_danger("The 'tensorflow' package is required but not installed. Please install it with install.packages('tensorflow').")
          return(invisible(NULL))
     } else {
          cli::cli_alert_success("tensorflow: {as.character(packageVersion('tensorflow'))}")
     }

     if (!requireNamespace("keras3", quietly = TRUE)) {
          cli::cli_alert_danger("The 'keras3' package is required but not installed. Please install it with install.packages('keras3').")
          return(invisible(NULL))
     } else {
          cli::cli_alert_success("keras3: {as.character(packageVersion('keras3'))}")
     }

     # -----------------------------------------------------------------------
     # Keras3 backend check.
     # -----------------------------------------------------------------------

     backend_name <- tryCatch({
          keras3::config_backend()
     }, error = function(e) {
          cli::cli_alert_danger("keras3 backend is not available or failed to load. {e$message}")
          return(invisible(NULL))
     })

     if (!is.null(backend_name)) {
          cli::cli_alert_success("keras3 backend detected.")
     } else {
          cli::cli_alert_danger("keras3 is not connected to a valid backend. Run keras::install_keras() or MOSAIC::install_dependencies(force = TRUE).")
          return(invisible(NULL))
     }

     tf_info <- tryCatch(

          unlist(tensorflow::tf_config()),
          error = function(e) {
               cli::cli_alert_danger("Cannot confirm tensorflow backend: {e$message}")
               return(invisible(NULL))
          }
     )

     if (!is.null(tf_info) && as.logical(tf_info["available"])) {
          cli::cli_alert_success("keras3 backend: tensorflow {tf_info[['version_str']]}")
          cli::cli_alert_success("keras3 backend Python path: {tf_info[['python']]}")
     } else {
          cli::cli_alert_danger("TensorFlow backend does not appear to be available.")
     }

     # -----------------------------------------------------------------------
     # Final confirmation.
     # -----------------------------------------------------------------------

     cli::cli_h2("Confirming R session link to Python evironment")
     config <- reticulate::py_config()

     # Confirm that reticulate is now pointing to the correct Python interpreter.
     if (grepl(paths$env, config$python)) {

          cli::cli_alert_success("Reticulate has activated the conda environment at '{paths$env}' for MOSAIC!")
          cli::cli_alert_info("Python configuration:")
          print(config)
          cli::cli_text(paste0("RETICULATE_PYTHON: ", Sys.getenv('RETICULATE_PYTHON')))
          cli::cli_text("")
          cli::cli_alert_success("All dependencies are confirmed. Your R session is ready to go!")

     } else {

          cli::cli_abort("Failed to activate the conda environment at {paths$env}.\nActivated Python: {config$python}")
     }


}
