#' Run MOSAIC Calibration by ISO Code (DEPRECATED - Simple Interface)
#'
#' @description
#' **DEPRECATED: Use [run_MOSAIC()] directly.**
#'
#' This function is deprecated. Use run_MOSAIC() with get_location_config() and
#' get_location_priors() instead for better control and clearer code.
#'
#' @param iso_code Character vector of ISO3 country codes
#' @param dir_output Output directory path
#' @param control Control list from mosaic_control_defaults()
#' @param resume Logical, resume from checkpoint
#'
#' @return Run results (invisibly)
#' @export
run_mosaic_iso <- function(iso_code,
                           dir_output,
                           control = NULL,
                           resume = FALSE) {

  .Deprecated("run_MOSAIC",
              msg = "run_mosaic_iso() is deprecated. Use run_MOSAIC() directly:\n  config <- get_location_config(iso = 'ETH')\n  priors <- get_location_priors(iso = 'ETH')\n  run_MOSAIC(config, priors, dir_output)")

  config <- get_location_config(iso = iso_code)
  priors <- get_location_priors(iso = iso_code)

  if (is.null(control)) {
    control <- mosaic_control_defaults()
  }

  run_MOSAIC(
    config = config,
    priors = priors,
    dir_output = dir_output,
    control = control,
    resume = resume
  )
}

#' @rdname run_mosaic_iso
#' @export
run_MOSAIC_iso <- run_mosaic_iso
