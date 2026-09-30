.onLoad <- function(libname, pkgname) {

     # ===========================================================================
     # OpenMP / threading safety
     # ===========================================================================
     #
     # MOSAIC can load more than one package that bundles its own OpenMP runtime:
     #
     #   libomp    — Clang/LLVM OpenMP, loaded by data.table (macOS ARM)
     #   libiomp5  — Intel KMP OpenMP, present in some TensorFlow/MKL builds
     #
     # pip, conda, and R package managers install these independently with no
     # coordination. Having several in the same process causes SIGSEGV crashes
     # at __kmp_suspend_initialize_thread when any runtime initialises threads
     # after another has already claimed shared data structures.
     #
     # The third runtime used to be libgomp via scipy, pulled in by the Python
     # engine, and the sharpest edge was numba's libiomp5 (the laser-cholera
     # JIT). Both left with the engine in v0.68.0, and numba left the Python
     # environment entirely in v0.69.0, so the NUMBA_THREADING_LAYER=workqueue
     # workaround that used to sit here was setting a variable for a package
     # that is no longer installed. What remains is the TensorFlow path, which
     # only the suitability model touches -- calibration workers are now pure R
     # and start no OpenMP runtime beyond data.table's.

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
     # PSOCK workers set their own limits independently.
     if (Sys.getenv("OMP_NUM_THREADS") == "") {
          Sys.setenv(OMP_NUM_THREADS = "1")
     }

     # Suppress Intel KMP informational messages (e.g. "OMP: Info #276" from
     # deprecated omp_set_nested() calls in MKL-linked builds).
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
          "for Cholera (MOSAIC)!\n",
          "\n",
          "Version: ", as.character(pkg_version), "\n"
     )

     # Python is NOT attached here, in any session.
     #
     # .onLoad has already set RETICULATE_PYTHON (above), which is the only
     # thing needed to make reticulate resolve the right interpreter. Attaching
     # additionally called reticulate::py_config(), and THAT is what forces
     # Python to initialise -- measured at 5.2 s on every interactive
     # library(MOSAIC).
     #
     # Nothing on the simulation or calibration path touches Python: the
     # transmission engine has been pure R since v0.68.0 and psi enters the
     # engine as a precomputed psi_jt matrix baked into the config. The only
     # consumer is est_suitability() and the keras3 suitability pipeline, which
     # initialises Python lazily on its first keras3 call and picks up the same
     # RETICULATE_PYTHON.
     #
     # So the cost was paid by every interactive session and the benefit
     # collected by the few that refit psi. Call MOSAIC::attach_mosaic_env()
     # explicitly if you want Python up front.

}
