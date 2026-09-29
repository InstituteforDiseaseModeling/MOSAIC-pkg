#' Apply a run suffix to an artifact filename
#'
#' Single implementation of the run-suffix convention shared by
#' \code{\link{est_mobility}}, \code{\link{plot_mobility}} and
#' \code{\link{plot_mobility_fused}}. A non-default run must never write to a
#' production filename.
#'
#' This exists because the convention was previously re-implemented per
#' function and got it wrong twice: once via a lowercase-only regex that let
#' \code{mobility_M/D/N.csv} through unsuffixed (clobbering the production
#' air-derived connectivity), and once in \code{plot_mobility_fused()} where
#' the figure paths were string literals and \code{suffix} routed only the
#' reads. One helper, one place to get it right.
#'
#' @param path A file path.
#' @param suffix Suffix to insert before the extension. \code{""} returns the
#'   path unchanged.
#' @return The path with \code{suffix} inserted before its final extension.
#' @keywords internal
#' @noRd
.mosaic_suffix_path <- function(path, suffix) {
     if (!nzchar(suffix)) return(path)
     sub("(\\.[A-Za-z0-9]+)$", paste0(suffix, "\\1"), path)
}


#' Validate a run suffix against the run's identity
#'
#' @param suffix The suffix.
#' @param is_default `TRUE` when the run uses production settings.
#' @return `suffix`, invisibly, or an error.
#' @keywords internal
#' @noRd
.mosaic_check_suffix <- function(suffix, is_default) {
     if (!is.character(suffix) || length(suffix) != 1L || is.na(suffix)) {
          stop("`suffix` must be a single non-NA character string; got ",
               paste(utils::capture.output(str(suffix)), collapse = " "), call. = FALSE)
     }
     if (!is_default && !nzchar(suffix)) {
          stop("A non-default run must not write to production filenames.\n",
               "  Supply a non-empty `suffix`, or leave it NULL to have one derived.",
               call. = FALSE)
     }
     invisible(suffix)
}
