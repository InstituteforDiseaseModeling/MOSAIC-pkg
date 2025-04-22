.onAttach <- function(libname, pkgname) {

     packageStartupMessage(
          "\n",
          " __  __   ___   ____     _     ___  ____       __      ___    _____  _____   _____ ___\n",
          "|  \\/  | / _ \\ / ___|   / \\   |_ _|/ ___|   __/ /_    / /    /   |  / ___/ / ____// __ \\\n",
          "| |\\/| || | | |\\___ \\  / _ \\   | || |      /_  __/   / /    / /| |  \\__ \\ / __/  / /_/ /\n",
          "| |  | || |_| | ___) |/ ___ \\  | || |___    /_/     / /___ / ___ | ___/ // /___ / _, _/\n",
          "|_|  |_| \\___/ |____//_/   \\_\\|___|\\____|          /_____//_/  |_|/____//_____//_/ |_|\n",
          "\n",
          "Welcome to the Metapopulation Outbreak Simulation with Agent-based Implementation\n",
          "for Cholera (MOSAIC) featuring the Light-agent Spatial Model for ERadication (LASER)!\n"
     )

     if (interactive()) {

          env_dir <- normalizePath(file.path("~", ".MOSAIC_conda_env"), winslash = "/", mustWork = FALSE)

          python_exec <- if (.Platform$OS.type == "windows") {
               file.path(env_dir, "Scripts", "python.exe")
          } else {
               file.path(env_dir, "bin", "python")
          }

          if (!dir.exists(env_dir) || !file.exists(python_exec)) {
               packageStartupMessage("❌ Python environment not found or incomplete.")
               cli::cli_text("To finish setup, run {.run MOSAIC::install_dependencies()}.")
          } else {
               packageStartupMessage("🐍 Found Python environment")
               cli::cli_text("To check setup, run {.run MOSAIC::check_dependencies()}.")
          }

     }

}
