#' Removed arguments and functions from the Dask/Coiled backend
#'
#' The Dask/Coiled distributed backend was removed when the transmission engine
#' moved from Python to R: it existed to make a 2 GB-per-worker,
#' 3.3 s-per-import Python engine affordable across many machines, and the R
#' engine needs neither. With it went \code{check_coiled_workspace()},
#' \code{mosaic_dask_presets()}, and the \code{dask_spec} argument to
#' \code{run_MOSAIC()} / \code{run_rolling_cv()}.
#'
#' \code{make_mosaic_cluster()} is NOT removed: despite its Dask-era framing it
#' builds the \emph{local} PSOCK cluster that the surviving backend runs on, and
#' \code{run_MOSAIC()} calls it directly. Its per-worker Python engine import
#' goes at the engine cutover, not here.
#'
#' Nothing is silently absorbed. Every removed name raises an error that says
#' what it was and what to do instead, because the alternative -- accepting an
#' argument and ignoring it -- is precisely the failure mode of CLAUDE.md
#' lesson #13, where a back-compat shim silently reverted user settings to
#' defaults for fifteen versions with no warning. A loud error is cheaper than
#' a deprecation cycle and far safer than silence.
#'
#' These stubs are scheduled for removal one minor version after the engine
#' cutover.
#'
#' @name deprecated_dask
#' @keywords internal
NULL

# Removed run_* arguments -> the message explaining each.
.MOSAIC_REMOVED_ARGS <- c(
     dask_spec = paste0(
          "The Dask/Coiled backend has been removed; simulations now run on ",
          "the local R parallel cluster, which the pure-R engine makes fast ",
          "enough that distributed compute is no longer needed. Drop the ",
          "`dask_spec` argument and set `control$parallel$n_cores` instead."),
     py_module = paste0(
          "The transmission engine is now pure R, so there is no Python module ",
          "to pass. Drop the `py_module` argument."),
     visualize = paste0(
          "Engine-side plotting (the Python Analyzer / matplotlib path) was not ",
          "ported. Drop `visualize` and use the plot_* functions on the returned ",
          "results instead."),
     pdf = paste0(
          "Engine-side plotting was not ported. Drop `pdf` and use the plot_* ",
          "functions on the returned results instead."),
     outdir = paste0(
          "The engine no longer writes files of its own, so `outdir` has no ",
          "meaning. Drop it.")
)

#' Reject removed arguments captured by \code{...}
#'
#' @param dots The result of \code{list(...)} in the calling function.
#' @param fn Name of the calling function, for the error message.
#' @return Invisibly \code{TRUE}; errors if any removed or unknown argument was
#'   supplied.
#' @keywords internal
.mosaic_reject_removed_args <- function(dots, fn) {

     if (length(dots) == 0L) return(invisible(TRUE))

     nms <- names(dots)
     if (is.null(nms)) nms <- rep("", length(dots))

     removed <- intersect(nms, names(.MOSAIC_REMOVED_ARGS))
     if (length(removed)) {
          stop(sprintf("%s(): `%s` has been removed.\n%s", fn, removed[1],
                       .MOSAIC_REMOVED_ARGS[[removed[1]]]), call. = FALSE)
     }

     # Anything else in `...` is a typo or a stale argument. Erroring here is
     # the "unknown key validator" whose absence let renamed control parameters
     # be silently dropped for fifteen versions (CLAUDE.md lesson #13).
     unknown <- nms[nzchar(nms)]
     if (length(unknown)) {
          stop(sprintf("%s(): unknown argument(s) %s.", fn,
                       paste0("`", unknown, "`", collapse = ", ")), call. = FALSE)
     }
     stop(sprintf("%s(): unnamed arguments passed through `...` are not accepted.", fn),
          call. = FALSE)
}

#' @rdname deprecated_dask
#' @param ... Ignored; present only so the stub accepts any old call shape.
#' @export
check_coiled_workspace <- function(...) {
     stop("check_coiled_workspace() has been removed along with the Dask/Coiled ",
          "backend. It reported Coiled core limits, active clusters and subnet IP ",
          "pressure before a remote run. Use check_dependencies() to validate the ",
          "Python environment.", call. = FALSE)
}

#' @rdname deprecated_dask
#' @export
mosaic_dask_presets <- function(...) {
     stop("mosaic_dask_presets() has been removed along with the Dask/Coiled ",
          "backend. It sized remote Coiled clusters; local parallelism is set ",
          "with control$parallel$n_cores. See mosaic_io_presets() for the ",
          "remaining preset helper.", call. = FALSE)
}
