#' Install R and Python Dependencies for MOSAIC
#'
#' @description
#' Sets up the Python virtual environment and installs packages listed in `requirements.txt`,
#' along with the R packages `keras3` and `tensorflow`. Ensures that the Keras + TensorFlow backend is usable.
#'
#' @param force Logical. If TRUE, deletes and recreates the Python virtual environment from scratch. Default is FALSE.
#'
#' @return No return value. Side effect: installs R and Python dependencies and configures the backend.
#' @export
#'

install_dependencies <- function(force = FALSE) {

     # ------------------------
     # 0. Load tensorflow before reticulate initializes Python
     # ------------------------

     if (!requireNamespace("reticulate", quietly = TRUE)) {
          message("Installing R package: reticulate")
          utils::install.packages("reticulate", dependencies = TRUE)
     }

     reticulate <- getNamespace("reticulate")

     # ------------------------
     # 1. Python environment
     # ------------------------

     env_path <- file.path(system.file(package = "MOSAIC"), "py", "mosaic-python-env")
     req_path <- system.file("py/requirements.txt", package = "MOSAIC")

     if (!file.exists(req_path)) {
          stop("requirements.txt not found at: ", req_path)
     }

     message("Python packages to be installed from requirements.txt:")
     req_lines <- readLines(req_path)
     for (line in req_lines) {
          message("  - ", line)
     }

     if (force && dir.exists(env_path)) {
          message("Removing existing Python environment (force = TRUE): ", env_path)
          unlink(env_path, recursive = TRUE, force = TRUE)
     }

     if (!reticulate::virtualenv_exists(env_path)) {
          message("Creating Python virtual environment at: ", env_path)
          reticulate::virtualenv_create(envname = env_path)
     } else {
          message("Python virtual environment already exists at: ", env_path)
     }

     # Set RETICULATE_PYTHON before initialization
     py_exec <- file.path(env_path, if (.Platform$OS.type == "windows") "Scripts/python.exe" else "bin/python")
     Sys.setenv(RETICULATE_PYTHON = py_exec)

     reticulate::use_virtualenv(env_path, required = TRUE)

     message("")

     message("Installing Python packages one by one using pip...")

     req_lines <- readLines(req_path)
     req_lines <- trimws(sub("#.*$", "", req_lines))      # Remove inline comments
     req_lines <- req_lines[nzchar(req_lines)]            # Remove empty lines

     for (pkg in req_lines) {

          message("📦 Installing: ", pkg)
          pip_args <- c("-m", "pip", "install", pkg)

          output <- tryCatch(
               system2(command = py_exec, args = pip_args, stdout = TRUE, stderr = TRUE),
               error = function(e) paste("❌ Error during installation of", pkg, ":", e$message)
          )

          message(paste(output, collapse = "\n"))
          message("")
     }

     if (reticulate::py_available()) {

          message("🚀 Reticulate has embedded the virtual Python environment 'mosaic-python-env' within MOSAIC R package!")
          reticulate::py_config()
          message("")

     }



     # -------------------------------------
     # 2. Keras and tensorflow backend setup
     # -------------------------------------

     message("Checking keras and tensorflow backend...")

     if (!requireNamespace("keras3", quietly = TRUE)) {
          stop("❌ The 'keras3' package is required but not installed.\n",
               "Please install it using: utils::install.packages('keras3', dependencies = TRUE) before running MOSAIC::install_dependencies().")
     } else {
          message("📦 R package 'keras3' is installed.")
          keras3 <- getNamespace("keras3")
     }

     if (!requireNamespace("tensorflow", quietly = TRUE)) {
          utils::install.packages('tensorflow', dependencies = TRUE)
     } else {
          message("📦 R package 'tensorflow' is installed.")
     }

     tensorflow <- getNamespace("tensorflow")


     if (force) {

          message("Installing Keras + TensorFlow Python backend...")
          Sys.setenv(RETICULATE_PYTHON = file.path(env_path, "bin", "python"))
          keras3::install_keras(
               method = "virtualenv",
               envname = env_path,
               backend = "tensorflow",
               tensorflow = "cpu",
               extra_packages = character()
          )

     } else {

          # Check is tensorflow backend is installed

          backend_ok <- tryCatch({
               backend <- keras3::config_backend()
               message("✔️  Keras 3 backend detected: ", backend)
               TRUE
          }, error = function(e) {
               message("Keras 3 backend not found.")
               FALSE
          })


          if (!backend_ok) {

               message("Installing Keras + TensorFlow Python backend...")
               Sys.setenv(RETICULATE_PYTHON = file.path(env_path, "bin", "python"))
               keras3::install_keras(
                    method = "virtualenv",
                    envname = env_path,
                    backend = "tensorflow",
                    tensorflow = "cpu",
                    extra_packages = character()
               )

          }


     }


     invisible(NULL)


}
