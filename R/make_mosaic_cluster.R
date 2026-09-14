#' Create a Reusable MOSAIC Parallel Cluster
#'
#' Creates a properly configured parallel cluster for use with \code{run_MOSAIC}.
#' The cluster handles all one-time setup: library loading, thread safety,
#' and root directory propagation. It can be passed to
#' multiple \code{run_MOSAIC} calls (e.g. across staged estimation) to avoid
#' the overhead of repeated cluster creation.
#'
#' @param n_cores Integer. Number of worker processes (default:
#'   \code{parallel::detectCores() - 1}). Clamped down to the number of free R
#'   connections if that is smaller, with a message -- see Details.
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
#'   \item Libraries loaded on each worker: \code{MOSAIC}, \code{arrow}.
#'   \item Root directory propagated from the main process via \code{set_root_directory()}.
#' }
#'
#' \strong{Connection cap.} Each worker holds one R connection. A default R
#' build permits 128 connections in total, three of which are already taken by
#' stdin, stdout and stderr, so requesting more than ~126 workers fails inside
#' \code{parallel::makeCluster()}. \code{n_cores} is therefore clamped to
#' \code{parallelly::freeConnections() - 2} (two held back for worker parquet
#' I/O) and a message reports the clamp. R 4.4.0 and later accept
#' \code{--max-connections=N} up to 4096 to raise the ceiling.
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

  # Cap by available connections, not by core count alone.
  #
  # Every worker holds one R connection, and a default R build permits 128 in
  # total with stdin/stdout/stderr already taken. So on any host with more than
  # ~126 usable cores the `detectCores() - 1L` default fails outright inside
  # makeCluster() -- which is not hypothetical: dugong has 176 cores. Two
  # connections are held back for the workers' own parquet I/O.
  free <- parallelly::freeConnections()
  if (n_cores > free - 2L) {
    n_avail <- max(1L, free - 2L)
    message(sprintf(paste0(
      "Requested %d workers but only %d R connections are free; using %d.\n",
      "  R's default build caps total connections at 128. R >= 4.4.0 accepts\n",
      "  `--max-connections=N` (up to 4096) to raise it; MOSAIC's DESCRIPTION\n",
      "  floors at R 4.1.1, so on an older R this cap is hard."),
      n_cores, free, n_avail))
    n_cores <- n_avail
  }

  # Set the canonical thread-env set in main process before spawning workers
  MOSAIC:::.mosaic_set_all_thread_env(1L)

  message(sprintf("Creating %s cluster with %d cores...", type, n_cores))
  cl <- parallel::makeCluster(n_cores, type = type)

  # One-time worker initialization
  .root_dir_val <- root_dir
  # Give workers the SAME library paths as this parent (so they load the same
  # MOSAIC build) rather than assuming a hardcoded ~/R/library.
  .parent_libs <- .libPaths()
  parallel::clusterExport(cl, varlist = c(".root_dir_val", ".parent_libs"), envir = environment())

  parallel::clusterEvalQ(cl, {
    .libPaths(unique(c(.parent_libs, .libPaths())))

    library(MOSAIC)
    library(arrow)

    # Single-threaded BLAS / Python / Numba per worker
    # (.mosaic_set_blas_threads also calls .mosaic_set_all_thread_env)
    MOSAIC:::.mosaic_set_blas_threads(1L)

    set_root_directory(.root_dir_val)
    PATHS <- get_paths()

    NULL
  })

  message(sprintf("Cluster ready (%d workers)", n_cores))
  cl
}
