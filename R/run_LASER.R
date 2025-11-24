#' Run LASER Model Simulation
#'
#' A wrapper function to execute a LASER model simulation via the Python interface (reticulate).
#' Automatically suppresses NumPy divide-by-zero warnings that can occur during vaccination
#' calculations when susceptible and exposed compartments are both zero.
#'
#' @param config Character or list. Either a path to a LASER configuration file (YAML/JSON) or a configuration list object.
#' @param seed Integer. Random seed for reproducibility. Defaults to 123L.
#' @param quiet Logical. If TRUE, suppress the progress bar during model execution. Defaults to FALSE.
#' @param visualize Logical. If TRUE, generate and display visualizations during the run. Defaults to FALSE.
#' @param pdf Logical. If TRUE, save visualizations as PDF files. Defaults to FALSE.
#' @param outdir Character. Directory where LASER outputs (e.g., logs, results) will be written. Defaults to a temporary directory.
#' @param py_module Python module. An optional pre-loaded reticulate LASER module. If NULL, the module is imported via reticulate::import("laser_cholera.metapop.model").
#'
#' @return A Python object (reticulate) representing the LASER model simulation results.
#'
#' @examples
#' \dontrun{
#' # Run with config file path:
#' result <- run_LASER(
#'   config    = "path/to/laser_params.yml",
#'   seed      = 20250418L,
#'   quiet     = FALSE,
#'   visualize = FALSE,
#'   pdf       = FALSE,
#'   outdir    = "./laser_output"
#' )
#'
#' # Run with config object:
#' result <- run_LASER(
#'   config = config_default,
#'   seed   = 123L,
#'   quiet  = TRUE
#' )
#'
#' # Inspect results:
#' print(result)
#' }
#'
#' @export
#'

run_LASER <- function(
          config,
          seed      = 123L,
          quiet     = FALSE,
          visualize = FALSE,
          pdf       = FALSE,
          outdir    = tempdir(),
          py_module = NULL
) {

     # Import LASER Python module if not provided
     if (is.null(py_module)) {
          if (!quiet) {
               message("Loading LASER module...")
          }
          py_module <- reticulate::import("laser_cholera.metapop.model", convert = FALSE)
     }

     # Suppress NumPy divide-by-zero warnings
     warnings <- reticulate::import("warnings", convert = FALSE)
     warnings$filterwarnings("ignore", message = "invalid value encountered in divide")
     warnings$filterwarnings("ignore", category = reticulate::import("numpy", convert = FALSE)$VisibleDeprecationWarning)

     # Execute the model (map R's 'config' parameter to Python's 'paramfile')
     result <- py_module$run_model(
          paramfile = config,
          seed      = as.integer(seed),
          quiet     = quiet,
          visualize = visualize,
          pdf       = pdf,
          outdir    = outdir
     )

     return(result)
}

#' @rdname run_LASER
#' @export
run_laser <- run_LASER
