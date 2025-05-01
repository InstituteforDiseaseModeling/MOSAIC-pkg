#' Run LASER Model Simulation
#'
#' A wrapper function to execute a LASER model simulation via the Python interface (reticulate).
#'
#' @param paramfile Character. Path to the LASER parameter file (e.g., YAML or JSON) used to configure the simulation.
#' @param seed Integer. Random seed for reproducibility. Defaults to 123L.
#' @param visualize Logical. If TRUE, generate and display visualizations during the run. Defaults to FALSE.
#' @param pdf Logical. If TRUE, save visualizations as PDF files. Defaults to FALSE.
#' @param outdir Character. Directory where LASER outputs (e.g., logs, results) will be written. Defaults to a temporary directory.
#' @param py_module Python module. An optional pre-loaded reticulate LASER module. If NULL, the module is imported via reticulate::import("laser").
#'
#' @return A Python object (reticulate) representing the LASER model simulation results.
#'
#' @examples
#' \dontrun{
#' # Run with default settings:
#' result <- run_LASER_model(
#'   paramfile = "path/to/laser_params.yml",
#'   seed      = 20250418L,
#'   visualize = FALSE,
#'   pdf       = FALSE,
#'   outdir    = "./laser_output"
#' )
#'
#' # Inspect results:
#' print(result)
#' }
#'
#' @export
#'

run_LASER_model <- function(
          paramfile,
          seed      = 123L,
          visualize = FALSE,
          pdf       = FALSE,
          outdir    = tempdir(),
          py_module = NULL
) {

     # Import LASER Python module if not provided
     if (is.null(py_module)) {
          py_module <- reticulate::import("laser", convert = FALSE)
     }

     # Execute the model
     result <- py_module$metapop$model$run_model(
          paramfile = paramfile,
          seed      = as.integer(seed),
          visualize = visualize,
          pdf       = pdf,
          outdir    = outdir
     )

     return(result)
}
