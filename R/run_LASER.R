#' Run LASER Model Simulation
#'
#' A wrapper function to execute a LASER model simulation via the Python interface (reticulate).
#' Automatically suppresses NumPy divide-by-zero warnings that can occur during vaccination
#' calculations when susceptible and exposed compartments are both zero.
#'
#' @param config Character or list. Either a path to a JSON configuration file or a configuration list object.
#' @param seed Integer or NULL. Random seed for reproducibility. If NULL (default), uses \code{config$seed} when present, otherwise defaults to 123L.
#' @param quiet Logical. If TRUE, suppress the progress bar during model execution. Defaults to FALSE.
#' @param ... Reserved. Supplying a removed argument (\code{py_module},
#'   \code{visualize}, \code{pdf}, \code{outdir}) raises an error naming it
#'   rather than silently ignoring it. See \link{deprecated_dask}.
#'
#' @return A Python object (reticulate) representing the LASER model simulation results.
#'
#' @examples
#' \dontrun{
#' # Run with config file path:
#' result <- run_LASER(
#'   config = "path/to/laser_params.json",
#'   seed   = 20250418L,
#'   quiet  = FALSE
#' )
#'
#' # Run with config object (uses config$seed if present, else 123L):
#' result <- run_LASER(
#'   config = config_default,
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
          seed  = NULL,
          quiet = FALSE,
          ...
) {

     # `visualize`, `pdf` and `outdir` drove the Python engine's matplotlib
     # Analyzer, which is not part of the R contract; `py_module` let a caller
     # hand in a pre-imported module. All four had zero callers and are removed.
     # Supplying one errors rather than being absorbed by `...`.
     .mosaic_reject_removed_args(list(...), "run_LASER")

     # Resolve seed: explicit arg > config$seed > default 123L
     if (is.null(seed)) {
          seed <- if (is.list(config) && !is.null(config$seed)) as.integer(config$seed) else 123L
     } else {
          seed <- as.integer(seed)
     }

     if (!quiet) {
          message("Loading LASER module...")
     }
     py_module <- reticulate::import("laser.cholera.metapop.model", convert = FALSE)
     .mosaic_strip_laser_file_handler()

     # Suppress NumPy divide-by-zero warnings
     warnings <- reticulate::import("warnings", convert = FALSE)
     warnings$filterwarnings("ignore", message = "invalid value encountered in divide")
     warnings$filterwarnings("ignore", category = reticulate::import("numpy", convert = FALSE)$VisibleDeprecationWarning)

     # Wrap length-1 location-specific array params as R lists so reticulate
     # passes them as Python lists (not scalars) for single-location runs
     if (is.list(config)) {
          config <- MOSAIC:::.mosaic_prepare_config_for_python(config)
     }

     # Execute the model (map R's 'config' parameter to Python's 'paramfile')
     result <- py_module$run_model(
          paramfile = config,
          seed      = as.integer(seed),
          quiet     = quiet,
          visualize = FALSE,
          pdf       = FALSE,
          outdir    = tempdir()
     )

     return(result)
}

#' @rdname run_LASER
#' @export
run_laser <- run_LASER
