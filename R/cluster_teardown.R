#' Stop a PSOCK cluster and reap workers that outlive the request
#'
#' \code{parallel::stopCluster()} shuts a worker down by writing a shutdown
#' message to its socket. A worker that is not currently \emph{reading} that
#' socket never receives it, so \code{stopCluster()} returns cleanly while the
#' worker survives its own cluster. That happens whenever a run is interrupted
#' mid-task, or a task stalls inside \code{run_simulation()} and the gather
#' gives up on it.
#'
#' A leaked worker is not merely idle: it inherited the parent's stdout, so the
#' pipe never reaches EOF and a \emph{finished} run looks like it is hanging
#' (no output from \code{tail}, no R master in the process table). It also holds
#' ~1 GB of RSS and one of R's 128 connection slots, which is enough to make a
#' later cluster creation in the same session fail.
#'
#' \code{.mosaic_stop_cluster()} therefore calls \code{stopCluster()} and then
#' SIGKILLs any recorded worker PID that is still a live PSOCK worker.
#'
#' @section Why the PIDs are collected up front:
#' The PIDs cannot be asked for at teardown time in the case that matters --
#' \code{clusterEvalQ(cl, Sys.getpid())} would queue behind the very task that
#' is stuck. \code{make_mosaic_cluster()} therefore records them on the cluster
#' object as the \code{"mosaic_worker_pids"} attribute at creation, while every
#' worker is known to be idle, and \code{.mosaic_stop_cluster()} reads that
#' attribute. For a cluster built elsewhere (no attribute) it falls back to
#' querying, which works unless a worker is already wedged -- the same
#' behaviour as before this function existed, never worse.
#'
#' @section Scope:
#' The kill is Linux-only (it reads \code{/proc/<pid>/cmdline}) and fires only
#' for processes whose command line still contains \code{RSOCK}. On any other
#' platform, and for \code{FORK} workers (whose command line is the parent's),
#' this degrades to plain \code{stopCluster()}.
#'
#' @param cl A cluster object from \code{parallel::makeCluster()} or
#'   \code{make_mosaic_cluster()}.
#' @param pids Integer worker PIDs. Defaults to the
#'   \code{"mosaic_worker_pids"} attribute of \code{cl}, else a live query.
#' @return \code{NULL}, invisibly.
#' @keywords internal
.mosaic_stop_cluster <- function(cl, pids = NULL) {

     if (is.null(cl)) return(invisible(NULL))
     if (is.null(pids)) {
          pids <- attr(cl, "mosaic_worker_pids", exact = TRUE)
          if (is.null(pids)) pids <- .mosaic_cluster_worker_pids(cl)
     }

     try(parallel::stopCluster(cl), silent = TRUE)

     if (!length(pids) || !identical(.Platform$OS.type, "unix")) {
          return(invisible(NULL))
     }
     for (p in pids) {
          # readBin, NOT readLines: /proc/<pid>/cmdline is NUL-separated and
          # readLines() truncates at the first NUL, returning the interpreter
          # path with none of its arguments -- so a grepl("RSOCK", .) guard over
          # readLines() can never match and silently disables the kill.
          if (grepl("RSOCK", .mosaic_proc_cmdline(p), fixed = TRUE)) {
               try(tools::pskill(p, tools::SIGKILL), silent = TRUE)
          }
     }
     invisible(NULL)
}

#' @rdname dot-mosaic_stop_cluster
#' @keywords internal
.mosaic_cluster_worker_pids <- function(cl) {
     if (is.null(cl)) return(integer(0))
     tryCatch(as.integer(unlist(parallel::clusterEvalQ(cl, Sys.getpid()))),
              error = function(e) integer(0))
}

#' @rdname dot-mosaic_stop_cluster
#' @keywords internal
.mosaic_proc_cmdline <- function(pid) {
     f <- sprintf("/proc/%d/cmdline", pid)
     # A dead PID is the EXPECTED case here (the worker shut down cleanly), so
     # it must not surface as a warning from file(): these run inside test
     # teardown, where a stream of "cannot open file" warnings reads as a
     # failure. Checked before opening rather than suppressed after.
     if (!file.exists(f)) return("")
     tryCatch({
          r <- readBin(f, "raw", n = 16384L)
          rawToChar(r[r != as.raw(0)])
     }, error = function(e) "", warning = function(w) "")
}

#' @rdname dot-mosaic_stop_cluster
#' @keywords internal
.mosaic_psock_alive <- function(pids) {
     if (!length(pids)) return(0L)
     sum(vapply(pids, function(p)
          grepl("RSOCK", .mosaic_proc_cmdline(p), fixed = TRUE), logical(1)))
}
