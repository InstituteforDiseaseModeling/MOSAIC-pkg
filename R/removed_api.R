#' Removed MOSAIC functions and arguments
#'
#' Names that MOSAIC used to export and no longer does. Each raises an error
#' saying what it was and what to call instead.
#'
#' Nothing is silently absorbed. The alternative -- accepting an argument or a
#' name and quietly doing something else -- is precisely the failure mode of
#' CLAUDE.md lesson #13, where a back-compat shim silently reverted user
#' settings to defaults for fifteen versions with no warning. A loud error is
#' cheaper than a deprecation cycle and far safer than silence.
#'
#' \strong{v0.70.0 -- the LASER naming.} \code{LASER} named the Python
#' \code{laser-cholera} package that MOSAIC used to shell out to. The engine has
#' been pure R since v0.68.0 and the dependency went in v0.69.0, so the name
#' pointed at something that no longer exists. \code{run_LASER()} is now
#' \code{\link{run_simulation}}, \code{make_LASER_config()} is
#' \code{\link{make_simulation_config}}, and \code{get_default_LASER_config()}
#' -- a byte-for-byte duplicate of \code{\link{get_default_config}} with zero
#' callers -- is gone in favour of the latter.
#'
#' \strong{v0.67.0 -- the Dask/Coiled backend.} Removed with the distributed
#' backend: the \code{dask_spec} argument to \code{run_MOSAIC()} /
#' \code{run_rolling_cv()}, plus \code{check_coiled_workspace()} and
#' \code{mosaic_dask_presets()} (both deleted outright in v0.70.0, one minor
#' version after the engine cutover, as scheduled).
#'
#' @name removed_api
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

# Renamed in v0.70.0 -> the message explaining each. Kept as a table rather
# than five hand-written bodies so the stub, its error text and the test that
# asserts the table is complete cannot drift apart.
.MOSAIC_RENAMED_FUNS <- c(
     run_LASER                = "run_simulation",
     run_laser                = "run_simulation",
     make_LASER_config        = "make_simulation_config",
     get_default_LASER_config = "get_default_config"
)

.mosaic_renamed_stop <- function(old) {
     new <- .MOSAIC_RENAMED_FUNS[[old]]
     stop(sprintf(paste0(
          "%s() has been renamed to %s(). `LASER` named the Python laser-cholera ",
          "package MOSAIC used to call out to; the engine has been pure R since ",
          "v0.68.0 and the dependency was removed in v0.69.0, so the old name ",
          "pointed at something that no longer exists. The arguments are unchanged."),
          old, new), call. = FALSE)
}

#' @rdname removed_api
#' @param ... Ignored; present only so the stub accepts any old call shape.
#' @export
run_LASER <- function(...) .mosaic_renamed_stop("run_LASER")

#' @rdname removed_api
#' @export
run_laser <- function(...) .mosaic_renamed_stop("run_laser")

#' @rdname removed_api
#' @export
make_LASER_config <- function(...) .mosaic_renamed_stop("make_LASER_config")

#' @rdname removed_api
#' @export
get_default_LASER_config <- function(...) .mosaic_renamed_stop("get_default_LASER_config")
