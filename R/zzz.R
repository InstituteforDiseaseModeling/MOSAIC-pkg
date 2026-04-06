.onLoad <- function(libname, pkgname) {

     # ===========================================================================
     # OpenMP / threading safety
     # ===========================================================================
     #
     # MOSAIC loads three packages that each bundle their own OpenMP runtime:
     #
     #   libomp    — Clang/LLVM OpenMP, loaded by data.table (macOS ARM)
     #   libiomp5  — Intel KMP OpenMP, loaded by numba (laser-cholera JIT)
     #   libgomp   — GNU OpenMP, loaded by scipy (laser-cholera >= 0.12.1)
     #
     # pip, conda, and R package managers install these independently with no
     # coordination. Having all three in the same process causes SIGSEGV crashes
     # at __kmp_suspend_initialize_thread when any runtime initialises threads
     # after another has already claimed shared data structures.
     #
     # KMP_DUPLICATE_LIB_OK=TRUE suppresses "duplicate library" errors (same
     # library loaded twice) but does NOT prevent GNU + Intel cross-runtime
     # conflicts. The root fix is to stop numba loading libiomp5 at all by
     # switching it to the workqueue threading backend, which uses numba's own
     # built-in thread pool and requires no external OpenMP library.

     # Switch numba to its workqueue threading backend.
     # This eliminates libiomp5 from the process, leaving only libomp (data.table)
     # and libgomp (scipy) which coexist without conflict.
     # Performance impact is negligible for laser-cholera's spatial SEIR kernels.
     if (Sys.getenv("NUMBA_THREADING_LAYER") == "") {
          Sys.setenv(NUMBA_THREADING_LAYER = "workqueue")
     }

     # Allow duplicate loads of the same OpenMP library (belt-and-suspenders for
     # libomp, which data.table and macOS system tools may both load).
     if (Sys.getenv("KMP_DUPLICATE_LIB_OK") == "") {
          Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")
     }

     # Prevent Intel KMP from pinning threads to cores — can cause deadlocks when
     # multiple runtimes are present.
     if (Sys.getenv("KMP_AFFINITY") == "") {
          Sys.setenv(KMP_AFFINITY = "none")
     }

     # Force data.table to single-threaded mode. data.table reads this at
     # initialisation; setting it here (before data.table loads) ensures it
     # never attempts multi-threaded OpenMP operations that could conflict.
     if (Sys.getenv("R_DATATABLE_NUM_THREADS") == "") {
          Sys.setenv(R_DATATABLE_NUM_THREADS = "1")
     }

     # General OpenMP thread limit — suppresses thread oversubscription and
     # reduces the surface area for cross-runtime conflicts in the main process.
     # PSOCK workers and Dask cloud workers set their own limits independently.
     if (Sys.getenv("OMP_NUM_THREADS") == "") {
          Sys.setenv(OMP_NUM_THREADS = "1")
     }

     # Suppress Intel KMP informational messages (e.g. "OMP: Info #276" from
     # deprecated omp_set_nested() calls in older laser-cholera versions).
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
          # This initializes Python and makes laser.cholera available immediately
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
