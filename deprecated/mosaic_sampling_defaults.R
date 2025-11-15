#' Get Default Parameter Sampling Settings (DEPRECATED)
#'
#' @description
#' **DEPRECATED**: This function has been removed.
#'
#' Default sampling settings are now built into `mosaic_control_defaults()`.
#' Use the `sampling` parameter in `mosaic_control_defaults()` instead.
#'
#' @return Named list with logical flags for which parameters to sample
#' @export
mosaic_sampling_defaults <- function() {
  .Deprecated("mosaic_control_defaults",
              msg = "mosaic_sampling_defaults() is deprecated. Use the 'sampling' parameter in mosaic_control_defaults() instead.")

  list(
    sample_tau_i = TRUE,
    sample_mobility_gamma = TRUE,
    sample_mobility_omega = TRUE,
    sample_mu_j = TRUE,
    sample_iota = TRUE,
    sample_gamma_2 = TRUE,
    sample_alpha_1 = FALSE
  )
}
