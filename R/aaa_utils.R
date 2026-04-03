# =============================================================================
# MOSAIC internal utilities
#
# This file is named aaa_utils.R so it is loaded first by R (alphabetical
# order), guaranteeing that shared operators like %||% are available to all
# other package files regardless of load order — including during
# devtools::load_all() and in parallel worker processes.
# =============================================================================

#' Null-coalescing operator
#'
#' Returns \code{a} if it is not \code{NULL}, otherwise returns \code{b}.
#' Equivalent to Python's \code{a or b} pattern for NULL checks.
#'
#' @param a Value to test.
#' @param b Default value returned when \code{a} is \code{NULL}.
#' @return \code{a} if non-NULL, else \code{b}.
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a
