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
#' @param require_root Logical. When \code{TRUE} (default) a root directory must
#'   be set and is propagated to the workers, which simulation workers need to
#'   resolve \code{get_paths()}. \code{FALSE} allows a cluster without one, for
#'   workers handed explicit paths -- \code{\link{render_MOSAIC_figures}} runs
#'   post-hoc on a finished run directory and may have no MOSAIC tree at all.
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
#' \code{parallel::stopCluster(cl)}. The returned object also carries a
#' \code{"mosaic_worker_pids"} attribute recorded at creation, which
#' \code{run_MOSAIC()} uses to reap any worker that survives
#' \code{stopCluster()} -- see \code{?.mosaic_stop_cluster}.
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
                                type = "PSOCK",
                                require_root = TRUE) {

  # Validate
  if (!is.numeric(n_cores) || n_cores < 1L) {
    stop("n_cores must be a positive integer")
  }
  n_cores <- as.integer(n_cores)
  type <- match.arg(type, c("PSOCK", "FORK"))

  # The root directory exists to give workers the same get_paths() view as the
  # parent. Simulation workers need it; figure-rendering workers do not -- they
  # are handed explicit paths out of a finished run directory, and
  # render_MOSAIC_figures() is documented as runnable post-hoc on a machine that
  # has no MOSAIC tree at all. require_root = FALSE serves that case rather than
  # forcing a second, near-duplicate cluster builder to exist alongside this one.
  root_dir <- getOption("root_directory")
  if (is.null(root_dir) && isTRUE(require_root)) {
    stop("Root directory not set. Call set_root_directory() before make_mosaic_cluster().")
  }

  # Cap by available connections, not by core count alone. Two connections are
  # held back for the workers' own parquet I/O. The same
  # .mosaic_clamp_psock_workers() helper (below) guards every PSOCK site in the
  # package, because an unclamped one does not degrade, it throws.
  n_cores <- .mosaic_clamp_psock_workers(n_cores, reserve = 2L,
                                         what = "cluster workers")

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

    # NULL only when require_root = FALSE and no root was set (figure rendering);
    # skip rather than fail, so those workers come up without a MOSAIC tree.
    if (!is.null(.root_dir_val)) {
      set_root_directory(.root_dir_val)
      PATHS <- get_paths()
    }

    NULL
  })

  # Record the worker PIDs now, while every worker is known to be idle. They
  # cannot be obtained later in the one case that needs them -- a wedged worker
  # would make the query queue behind the task that wedged it -- so
  # .mosaic_stop_cluster() reads them from here. See ?.mosaic_stop_cluster.
  attr(cl, "mosaic_worker_pids") <- .mosaic_cluster_worker_pids(cl)

  message(sprintf("Cluster ready (%d workers)", n_cores))
  cl
}

# ---------------------------------------------------------------------------
# Connection-budget clamp shared by every PSOCK site in the package
# ---------------------------------------------------------------------------

#' Clamp a PSOCK worker count to the free-connection budget
#'
#' Every PSOCK worker holds one R connection for its lifetime, and R allocates
#' a fixed connection table at startup -- 128 slots by default, three of which
#' are already taken by \code{stdin}, \code{stdout} and \code{stderr}. So a
#' host with more cores than that (dugong has 176) cannot put every core in one
#' cluster unless R was started with \code{--max-connections=N} (R >= 4.4.0,
#' maximum 4096).
#'
#' This is the single place that decision is made. It exists because an
#' unclamped \code{parallel::makeCluster()} does not degrade gracefully -- it
#' raises \code{"all 128 connections are in use"}. When that throw happens
#' inside a \code{tryCatch} (as it did in \code{calc_model_ensemble()}) the
#' whole stage is silently skipped rather than merely running narrower.
#'
#' @param n_cores Integer. Requested number of workers.
#' @param reserve Integer. Connections held back for the caller's own non-worker
#'   I/O (parquet writes, log sinks). Default 2.
#' @param what Character. Noun used in the clamp message.
#' @param verbose Logical. Emit a message explaining the clamp and its remedy.
#'
#' @return Integer. \code{n_cores}, or the largest count the free-connection
#'   budget supports (never below 1).
#'
#' @noRd
.mosaic_clamp_psock_workers <- function(n_cores,
                                        reserve = 2L,
                                        what = "workers",
                                        verbose = TRUE) {

  n_cores <- as.integer(n_cores)
  if (length(n_cores) != 1L || is.na(n_cores)) {
    stop("n_cores must be a single non-NA integer")
  }

  free <- tryCatch(as.integer(parallelly::freeConnections()),
                   error = function(e) NA_integer_)
  # No budget reading available (unexpected): leave the request untouched
  # rather than invent a cap.
  if (is.na(free)) return(n_cores)

  budget <- max(1L, free - as.integer(reserve))
  if (n_cores <= budget) return(n_cores)

  if (isTRUE(verbose)) {
    total <- tryCatch(as.integer(parallelly::availableConnections()),
                      error = function(e) NA_integer_)
    message(sprintf(paste0(
      "Requested %d %s but only %d R connections are free; using %d.\n",
      "  R allocates %s connection slots at startup (3 are stdin/stdout/stderr).\n",
      "  Restart R with `--max-connections=N` (R >= 4.4.0, max 4096) to raise it,\n",
      "  e.g. `Rscript --max-connections=512 your_script.R`."),
      n_cores, what, free, budget,
      if (is.na(total)) "a fixed number of" else format(total)))
  }

  budget
}
