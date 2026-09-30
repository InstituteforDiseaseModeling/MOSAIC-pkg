#' Use MOSAIC Python Environment
#'
#' @description
#' **Note**: This function is maintained for backward compatibility.
#' For new code, use \code{\link{attach_mosaic_env}} instead.
#'
#' Switches the current R session to use the MOSAIC Python environment
#' at ~/.virtualenvs/r-mosaic. This function attempts to switch from other
#' Python environments (like r-keras) to the MOSAIC environment.
#'
#' @return Invisible TRUE if successful, FALSE otherwise.
#'
#' @details
#' This function attempts to switch the Python environment mid-session.
#' Due to reticulate limitations, if Python has already been initialized
#' with a different environment, a complete switch may require restarting R.
#' The function will provide appropriate guidance based on the current state.
#'
#' **Recommended**: Use \code{\link{attach_mosaic_env}} for clearer semantics
#' and better error handling. Loading MOSAIC does not attach Python: it only
#' sets RETICULATE_PYTHON to r-mosaic (when unset), so reticulate binds r-mosaic
#' on the first Python call.
#'
#' @examples
#' \dontrun{
#' # Old approach (still works)
#' use_mosaic_env()
#'
#' # New approach (recommended)
#' attach_mosaic_env()
#'
#' # Loading MOSAIC points reticulate at r-mosaic; Python starts lazily
#' library(MOSAIC)
#' }
#'
#' @seealso
#' \code{\link{attach_mosaic_env}} for the recommended function,
#' \code{\link{detach_mosaic_env}} for detaching the environment,
#' \code{\link{check_python_env}} for checking the current environment,
#' \code{\link{check_dependencies}} for verifying the setup,
#' \code{\link{install_dependencies}} for installing the environment
#'
#' @export
#'
use_mosaic_env <- function() {

  # This function is now a simple wrapper around attach_mosaic_env()
  # Maintained for backward compatibility

  cli::cli_alert_info("Note: use_mosaic_env() is now an alias for attach_mosaic_env()")
  cli::cli_text("For new code, consider using attach_mosaic_env() directly")
  cli::cli_text("")

  # Call the new function
  MOSAIC::attach_mosaic_env(silent = FALSE)

}