library(usethis)
library(reticulate)

# Define the path for the virtual environment inside the package
pkg_path <- usethis::proj_path()  # Get the package root directory
python_env_path <- file.path(pkg_path, "inst", "py", "mosaic-python-env")

# Unlink/delete env if exists
if (dir.exists(python_env_path)) {
     unlink(python_env_path, recursive = TRUE)
     message("Existing virtual environment removed.")
}

# Create new env
if (!dir.exists(python_env_path)) {
     message("Creating Python virtual environment...")
     reticulate::virtualenv_create(envname = python_env_path)
}

# Install required Python packages
reticulate::virtualenv_install(envname = python_env_path, packages = c("sciris", "numpy", "pandas"))

# Check if the virtual environment exists
if (dir.exists(python_env_path)) {
     message("✅ P Python virtual environment successfully created at: ", python_env_path)
} else {
     stop("❌ Python virtual environment not found. Run virtualenv_create() to create it.")
}

#use_virtualenv(python_env_path, required = TRUE)
py_config()

# Import Python modules
sc <- reticulate::import("sciris", convert = FALSE)
np <- reticulate::import("numpy", convert = FALSE)
pd <- reticulate::import("pandas", convert = FALSE)

if (is.null(sc) || is.null(np) || is.null(pd)) {
     stop("❌ One or more Python modules failed to import!")
} else {
     message("✅ All Python modules imported successfully!")
}

tryCatch({
     message("✅ Sciris version: ", sc[["__version__"]])
     message("✅ NumPy version: ", np[["__version__"]])
     message("✅ Pandas version: ", pd[["__version__"]])
}, error = function(e) {
     stop("❌ One or more Python modules failed to import correctly: ", e$message)
})
