.onAttach <- function(libname, pkgname) {

     packageStartupMessage("
 __  __   ___   ____     _     ___  ____
|  \\/  | / _ \\ / ___|   / \\   |_ _|/ ___|
| |\\/| || | | |\\___ \\  / _ \\   | || |
| |  | || |_| | ___) |/ ___ \\  | || |___
|_|  |_| \\___/ |____//_/   \\_\\|___|\\____|

Welcome to the MOSAIC package!
")

     # Locate virtualenv path
     python_env_path <- system.file("py/mosaic-python-env", package = pkgname)

     python_exec <- if (.Platform$OS.type == "windows") {
          file.path(python_env_path, "Scripts", "python.exe")
     } else {
          file.path(python_env_path, "bin", "python")
     }

     if (!dir.exists(python_env_path) || !file.exists(python_exec)) {
          message("❌ Python virtual environment not found or incomplete.")
          message("To finish setup, run: MOSAIC::install_dependencies(). To re-install, run MOSAIC::install_dependencies(force = TRUE).")
          return()
     } else {
          message("🐍 Found Python virtual environment")
     }

}
