#' Plot Stochastic Model Fit with Parameter and Stochastic Uncertainty
#'
#' @description
#' \lifecycle{deprecated}
#'
#' This function is deprecated. Use \code{\link{calc_model_ensemble}} followed by
#' \code{\link{plot_model_ensemble}} instead. This wrapper calls both internally
#' for backward compatibility.
#'
#' @param configs List of pre-sampled configuration objects.
#' @param config Base configuration object.
#' @param parameter_seeds Numeric vector of seeds for parameter sampling.
#' @param parameter_weights Numeric vector of weights for each parameter set.
#' @param n_simulations_per_config Integer. Stochastic sims per param set.
#' @param PATHS List of paths from \code{get_paths()}.
#' @param priors Priors object.
#' @param sampling_args Named list of additional arguments for \code{sample_parameters()}.
#' @param output_dir Character. Directory for output plots and CSVs.
#' @param envelope_quantiles Numeric vector of quantiles for CIs.
#' @param save_predictions Logical. Save prediction CSVs.
#' @param parallel Logical. Use parallel computation.
#' @param n_cores Integer. Number of cores for parallel.
#' @param root_dir Character. MOSAIC root directory.
#' @param verbose Logical. Print progress messages.
#' @param plot_decomposed Ignored (kept for signature compatibility).
#' @param precomputed_results Optional precomputed LASER results.
#'
#' @return Invisibly returns the plot list from \code{\link{plot_model_ensemble}}.
#'
#' @seealso \code{\link{calc_model_ensemble}}, \code{\link{plot_model_ensemble}}
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_point geom_line facet_grid facet_wrap scale_color_manual scale_fill_manual scale_alpha_manual scale_y_continuous scale_x_date theme_minimal theme element_text element_blank labs ggsave
#' @importFrom dplyr filter mutate
#' @importFrom scales comma
#' @importFrom reticulate import
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom parallel makeCluster stopCluster clusterEvalQ clusterExport clusterCall detectCores
#' @importFrom pbapply pblapply
plot_model_fit_stochastic_param <- function(
    configs = NULL,
    config = NULL,
    parameter_seeds = NULL,
    parameter_weights = NULL,
    n_simulations_per_config = 30,
    PATHS = NULL,
    priors = NULL,
    sampling_args = list(),
    output_dir,
    envelope_quantiles = c(0.025, 0.25, 0.75, 0.975),
    save_predictions = FALSE,
    parallel = FALSE,
    n_cores = NULL,
    root_dir = NULL,
    verbose = TRUE,
    plot_decomposed = FALSE,
    precomputed_results = NULL) {

    .Deprecated("calc_model_ensemble + plot_model_ensemble",
                msg = paste0("plot_model_fit_stochastic_param() is deprecated. ",
                             "Use calc_model_ensemble() + plot_model_ensemble() instead."))

    # Determine which config to use as the base
    base_config <- if (!is.null(configs)) configs[[1]] else config

    ensemble <- calc_model_ensemble(
      config                   = base_config,
      parameter_seeds          = parameter_seeds,
      configs                  = configs,
      parameter_weights        = parameter_weights,
      n_simulations_per_config = n_simulations_per_config,
      envelope_quantiles       = envelope_quantiles,
      PATHS                    = PATHS,
      priors                   = priors,
      sampling_args            = sampling_args,
      parallel                 = parallel,
      n_cores                  = n_cores,
      root_dir                 = root_dir,
      precomputed_results      = precomputed_results,
      verbose                  = verbose
    )

    result <- plot_model_ensemble(
      ensemble         = ensemble,
      output_dir       = output_dir,
      save_predictions = save_predictions,
      verbose          = verbose
    )

    invisible(result)
}
