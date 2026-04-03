#' Plot Stochastic Model Fit: Timeseries with Uncertainty Envelopes
#'
#' @description
#' Convenience wrapper that calls \code{\link{calc_model_ensemble}} followed by
#' \code{\link{plot_model_ensemble}}. Preserved for backward compatibility.
#'
#' For new code, prefer calling the two functions separately so the ensemble
#' object can be reused for R² and bias ratio calculations without running a
#' second set of simulations.
#'
#' @param config Named list. Model configuration as returned by
#'   \code{\link{sample_parameters}}.
#' @param n_simulations Integer. Number of stochastic simulations. Default \code{100L}.
#' @param output_dir Character. Directory for plots and prediction CSVs.
#' @param envelope_quantiles Numeric vector of length 2. Uncertainty envelope
#'   quantiles. Default \code{c(0.025, 0.975)}.
#' @param save_predictions Logical. Save per-location prediction CSVs. Default
#'   \code{FALSE}.
#' @param parallel Logical. Use R parallel cluster. Default \code{FALSE}.
#' @param n_cores Integer or \code{NULL}. Cores when \code{parallel = TRUE}.
#' @param root_dir Character. MOSAIC root directory (required when
#'   \code{parallel = TRUE}).
#' @param verbose Logical. Print progress. Default \code{TRUE}.
#'
#' @return Invisibly returns the plot list from \code{\link{plot_model_ensemble}},
#'   which includes \code{simulation_stats} containing the ensemble object fields.
#'
#' @seealso \code{\link{calc_model_ensemble}}, \code{\link{plot_model_ensemble}}
#'
#' @export
plot_model_fit_stochastic <- function(config,
                                      n_simulations      = 100L,
                                      output_dir,
                                      envelope_quantiles = c(0.025, 0.975),
                                      save_predictions   = FALSE,
                                      parallel           = FALSE,
                                      n_cores            = NULL,
                                      root_dir           = NULL,
                                      verbose            = TRUE) {

  ensemble <- calc_model_ensemble(
    config             = config,
    n_simulations      = n_simulations,
    envelope_quantiles = envelope_quantiles,
    parallel           = parallel,
    n_cores            = n_cores,
    root_dir           = root_dir,
    verbose            = verbose
  )

  plot_model_ensemble(
    ensemble         = ensemble,
    output_dir       = output_dir,
    save_predictions = save_predictions,
    verbose          = verbose
  )
}
