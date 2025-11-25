.onLoad <- function(libname, pkgname) {

     # CRITICAL FIX: Allow duplicate OpenMP libraries
     # PyTorch and data.table both use OpenMP (libomp.dylib on macOS)
     # Without this, loading data.table after PyTorch causes R to crash with:
     # "OMP: Error #15: Initializing libomp.dylib, but found libomp.dylib already initialized"
     #
     # This is a known issue when mixing packages with OpenMP:
     # - PyTorch (via reticulate) loads libomp from conda/Homebrew
     # - data.table loads libomp from R compilation
     # - Both try to initialize OpenMP → crash
     #
     # Setting KMP_DUPLICATE_LIB_OK=TRUE allows both to coexist
     # This is the standard workaround for R packages using OpenMP with Python
     if (Sys.getenv("KMP_DUPLICATE_LIB_OK") == "") {
          Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")
     }

     # Additional OpenMP safety: Limit thread affinity issues
     # MKL and OpenMP can conflict on thread binding
     if (Sys.getenv("KMP_AFFINITY") == "") {
          Sys.setenv(KMP_AFFINITY = "none")
     }

     # CRITICAL: Force data.table to single-threaded mode
     # When PyTorch (via reticulate) has already initialized OpenMP,
     # data.table's multi-threaded operations cause segfaults even with
     # KMP_DUPLICATE_LIB_OK=TRUE. The conflict occurs during actual operations,
     # not just library loading.
     #
     # Solution: Force data.table to use single-threaded mode via environment variable
     # This is read by data.table during initialization and cannot be changed after loading
     if (Sys.getenv("R_DATATABLE_NUM_THREADS") == "") {
          Sys.setenv(R_DATATABLE_NUM_THREADS = "1")
     }

     # Also set general OpenMP thread limit as a backup
     # (though data.table primarily respects R_DATATABLE_NUM_THREADS)
     if (Sys.getenv("OMP_NUM_THREADS") == "") {
          Sys.setenv(OMP_NUM_THREADS = "1")
     }

     # Suppress OpenMP informational messages
     # LASER (laser-cholera) uses deprecated omp_set_nested() which triggers
     # "OMP: Info #276" warnings in newer OpenMP versions (5.0+)
     # This is harmless but clutters output - suppress info-level messages
     if (Sys.getenv("KMP_WARNINGS") == "") {
          Sys.setenv(KMP_WARNINGS = "0")
     }

     # Set RETICULATE_PYTHON before reticulate is imported (if not already set)
     # This prevents reticulate from initializing with the wrong Python environment
     # when library(MOSAIC) is called

     current_python <- Sys.getenv("RETICULATE_PYTHON", unset = NA)

     # Only set if user hasn't already specified a Python
     if (is.na(current_python) || current_python == "") {

          # Construct path to r-mosaic environment
          # (inline logic to avoid dependency on package functions during load)
          mosaic_env_dir <- path.expand(file.path("~", ".virtualenvs", "r-mosaic"))
          mosaic_python <- if (.Platform$OS.type == "windows") {
               file.path(mosaic_env_dir, "Scripts", "python.exe")
          } else {
               file.path(mosaic_env_dir, "bin", "python")
          }

          # Only set if the environment actually exists
          # If it doesn't exist, reticulate will do its normal discovery
          # and check_python_env() in .onAttach will provide clear error
          if (file.exists(mosaic_python)) {
               Sys.setenv(RETICULATE_PYTHON = mosaic_python)

               # CRITICAL FIX for GLIBCXX version errors on older Linux (e.g., Ubuntu 20.04)
               # When R embeds Python via reticulate, Python C extensions (pyarrow, numba, etc.)
               # need libstdc++ with GLIBCXX_3.4.29+, but Ubuntu 20.04 only has 3.4.28.
               #
               # Setting LD_LIBRARY_PATH doesn't work because R has already loaded system libstdc++.
               # Solution: Use dyn.load() to explicitly preload conda's libstdc++ BEFORE reticulate
               # initializes Python. This way, Python extensions use the preloaded version.
               if (.Platform$OS.type == "unix") {
                    mosaic_libstdcxx <- file.path(mosaic_env_dir, "lib", "libstdc++.so.6")
                    if (file.exists(mosaic_libstdcxx)) {
                         tryCatch({
                              dyn.load(mosaic_libstdcxx, local = FALSE, now = TRUE)
                         }, error = function(e) {
                              # Silently ignore - may already be loaded or system incompatible
                              # check_dependencies() will catch any resulting import errors
                         })
                    }
               }
          }
     }

}

.onAttach <- function(libname, pkgname) {

     pkg_version <- utils::packageVersion("MOSAIC")

     packageStartupMessage(
          "\n",
          " __  __   ___   ____     _     ___  ____       __      ___    _____  _____   _____ ___\n",
          "|  \\/  | / _ \\ / ___|   / \\   |_ _|/ ___|   __/ /_    / /    /   |  / ___/ / ____// __ \\\n",
          "| |\\/| || | | |\\___ \\  / _ \\   | || |      /_  __/   / /    / /| |  \\__ \\ / __/  / /_/ /\n",
          "| |  | || |_| | ___) |/ ___ \\  | || |___    /_/     / /___ / ___ | ___/ // /___ / _, _/\n",
          "|_|  |_| \\___/ |____//_/   \\_\\|___|\\____|          /_____//_/  |_|/____//_____//_/ |_|\n",
          "\n",
          "Welcome to the Metapopulation Outbreak Simulation with Agent-based Implementation\n",
          "for Cholera (MOSAIC) featuring the Light-agent Spatial Model for ERadication (LASER)!\n",
          "\n",
          "Version: ", as.character(pkg_version), "\n"
     )

     # Only actively attach Python in interactive sessions
     # For scripts/workers, RETICULATE_PYTHON (set in .onLoad) is sufficient
     # Python will initialize lazily when first used
     if (interactive()) {
          # Automatically attach r-mosaic Python environment
          # This initializes Python and makes laser_cholera available immediately
          attachment_success <- tryCatch({
               MOSAIC::attach_mosaic_env(silent = TRUE)
               TRUE
          }, error = function(e) {
               # If auto-attach fails, provide helpful guidance
               cli::cli_alert_warning("Failed to auto-attach r-mosaic Python environment")
               cli::cli_text("Error: {e$message}")
               cli::cli_text("")
               cli::cli_text("To diagnose: {.run MOSAIC::check_python_env()}")
               cli::cli_text("To install: {.run MOSAIC::install_dependencies()}")
               cli::cli_text("")
               FALSE
          })

          # Inform users about detachment option if attachment succeeded
          if (attachment_success) {
               cli::cli_text("")
               cli::cli_alert_info("Python environment automatically attached to r-mosaic")
               cli::cli_text("To use a different Python: {.run MOSAIC::detach_mosaic_env()}")
               cli::cli_text("")
          }
     }

}
