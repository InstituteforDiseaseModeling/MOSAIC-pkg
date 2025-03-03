#' Check Affine Normalization (Zero-Centered Min-Max Scaling)
#'
#' Verifies that a numeric vector has been affine normalized such that it is
#' zero-centered and its minimum value is at least \code{-1}. Specifically, the function checks that:
#'
#' \itemize{
#'   \item The mean of the vector (ignoring \code{NA}s) is approximately zero.
#'   \item The minimum value (ignoring \code{NA}s) is greater than or equal to \code{-1}.
#'   \item The maximum value (ignoring \code{NA}s) is above zero.
#' }
#'
#' If any of these conditions fail, the function stops execution and returns an
#' informative error message.
#'
#' @param x A numeric vector that has been affine normalized. This vector may contain \code{NA} values.
#' @param verbose Logical; if \code{TRUE}, the function prints details of the check to the console.
#'
#' @return Invisibly returns \code{NULL} if the check passes. Otherwise, the function stops
#'         with an error message detailing the failed condition(s).
#'
#' @details The check uses a tolerance of \code{1e-8} to account for floating point precision.
#'          The conditions verified correspond to a transformation defined as:
#'
#' \deqn{x_{\text{scaled}} = \frac{x - \mu}{\mu - \min(x)}}
#'
#' where \eqn{\mu} is the mean of \code{x}. Note that
#' while the transformation forces a zero-centered result, the minimum value need not be
#' exactly \code{-1} but should be no less than \code{-1}.
#'
#' @examples
#' # Example vector after affine normalization
#' x <- c(-1, 0, 0.5, 1.2, NA)
#'
#' # Check without printing details:
#' \dontrun{
#'   check_affine_normalization(x)
#' }
#'
#' # Check with verbose output:
#' \dontrun{
#'   check_affine_normalization(x, verbose = TRUE)
#' }
#'
#' @export
#'

check_affine_normalization <- function(x, verbose = FALSE) {

     tol <- 1e-8  # Tolerance for floating point precision

     # Compute statistics ignoring NA values
     mean_x <- mean(x, na.rm = TRUE)
     min_x  <- min(x, na.rm = TRUE)
     max_x  <- max(x, na.rm = TRUE)

     # Check if the mean is approximately zero, minimum is >= -1, and maximum is above 0
     mean_check <- abs(mean_x) < tol
     min_check  <- min_x >= (-1 - tol)  # Expect min_x to be no less than -1
     max_check  <- max_x > 0

     # If any check fails, construct an error message and stop execution.
     if (!(mean_check && min_check && max_check)) {

          err_msg <- ""
          if (!mean_check) err_msg <- paste0(err_msg, "Mean is not zero. Found: ", mean_x, ". ")
          if (!min_check)  err_msg <- paste0(err_msg, "Min is less than -1. Found: ", min_x, ". ")
          if (!max_check)  err_msg <- paste0(err_msg, "Max is not above zero. Found: ", max_x, ".")
          stop("Zero-centered min-max scaling check failed: ", err_msg)

     } else {

          if (verbose) {
               cat("Zero-centered min-max scaling check passed:\n")
               cat("Mean:", mean_x, "\n")
               cat("Min:", min_x, "\n")
               cat("Max:", max_x, "\n")
          }

     }

}
