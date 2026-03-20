#' Create a Reusable MOSAIC Parallel Cluster
#'
#' Creates a properly configured parallel cluster for use with \code{run_MOSAIC}.
#' The cluster handles all one-time setup: library loading, thread safety,
#' Python/LASER import, and root directory propagation. It can be passed to
#' multiple \code{run_MOSAIC} calls (e.g. across staged estimation) to avoid
#' the overhead of repeated cluster creation.
#'
#' @param n_cores Integer. Number of worker processes (default: \code{parallel::detectCores() - 1}).
#' @param type Character. Cluster type: \code{"PSOCK"} (default, all platforms) or
#'   \code{"FORK"} (Linux/Mac only, faster startup).
#'
#' @return A \code{cluster} object (from \code{parallel::makeCluster}) ready to
#'   pass to \code{run_MOSAIC(cluster = cl)}.
#'
#' @details
#' The cluster setup includes:
#' \enumerate{
#'   \item Thread environment variables (\code{OMP_NUM_THREADS}, \code{MKL_NUM_THREADS},
#'     \code{TBB_NUM_THREADS}, \code{NUMBA_NUM_THREADS}, \code{OPENBLAS_NUM_THREADS})
#'     set to 1 in both the main process and each worker to prevent oversubscription.
#'   \item BLAS threads limited to 1 per worker via \code{.mosaic_set_blas_threads(1L)}.
#'   \item Libraries loaded on each worker: \code{MOSAIC}, \code{reticulate}, \code{arrow}.
#'   \item \code{laser.cholera} Python module imported once per worker.
#'   \item Root directory propagated from the main process via \code{set_root_directory()}.
#' }
#'
#' The caller is responsible for stopping the cluster when done:
#' \code{parallel::stopCluster(cl)}.
#'
#' @examples
#' \dontrun{
#' library(MOSAIC)
#' set_root_directory("~/MOSAIC")
#'
#' # Create cluster once
#' cl <- make_mosaic_cluster(n_cores = 8)
#'
#' # Use across multiple calibration stages
#' result_s1 <- run_MOSAIC(config, priors, "./stage_1", control, cluster = cl)
#' result_s2 <- run_MOSAIC(config, priors_s2, "./stage_2", control, cluster = cl)
#'
#' # Clean up
#' parallel::stopCluster(cl)
#' }
#'
#' @seealso [run_MOSAIC()] for the calibration workflow that accepts this cluster.
#' @export
make_mosaic_cluster <- function(n_cores = parallel::detectCores() - 1L,
                                type = "PSOCK") {

  # Validate
  if (!is.numeric(n_cores) || n_cores < 1L) {
    stop("n_cores must be a positive integer")
  }
  n_cores <- as.integer(n_cores)
  type <- match.arg(type, c("PSOCK", "FORK"))

  root_dir <- getOption("root_directory")
  if (is.null(root_dir)) {
    stop("Root directory not set. Call set_root_directory() before make_mosaic_cluster().")
  }

  # Set thread env vars in main process before spawning workers
  Sys.setenv(
    TBB_NUM_THREADS = "1",
    NUMBA_NUM_THREADS = "1",
    OMP_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1",
    OPENBLAS_NUM_THREADS = "1"
  )

  message(sprintf("Creating %s cluster with %d cores...", type, n_cores))
  cl <- parallel::makeCluster(n_cores, type = type)

  # One-time worker initialization
  .root_dir_val <- root_dir
  parallel::clusterExport(cl, varlist = c(".root_dir_val"), envir = environment())

  parallel::clusterEvalQ(cl, {
    .libPaths(c("~/R/library", .libPaths()))

    library(MOSAIC)
    library(reticulate)
    library(arrow)

    # Single-threaded BLAS per worker
    MOSAIC:::.mosaic_set_blas_threads(1L)

    # Single-threaded Python/Numba per worker
    Sys.setenv(
      TBB_NUM_THREADS = "1",
      NUMBA_NUM_THREADS = "1",
      OMP_NUM_THREADS = "1",
      MKL_NUM_THREADS = "1",
      OPENBLAS_NUM_THREADS = "1"
    )

    set_root_directory(.root_dir_val)
    PATHS <- get_paths()

    # Import laser-cholera once per worker
    lc <- reticulate::import("laser.cholera.metapop.model")
    assign("lc", lc, envir = .GlobalEnv)

    # Suppress NumPy warnings
    warnings_py <- reticulate::import("warnings")
    warnings_py$filterwarnings("ignore", message = "invalid value encountered in divide")
    NULL
  })

  message(sprintf("Cluster ready (%d workers)", n_cores))
  cl
}
