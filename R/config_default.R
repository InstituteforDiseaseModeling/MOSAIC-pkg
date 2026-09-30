#' Default Simulation Configuration
#'
#' The **canonical** simulation parameter object shipped with MOSAIC. It holds
#' default values for every model parameter, the initial state vectors, the
#' daily input matrices and the observed surveillance series for the 40
#' modelled locations, and is the template that \code{get_location_config()}
#' subsets and \code{sample_parameters()} draws into.
#'
#' @format A named **list** built by the `data-raw/make_config_default.R`
#'   script and persisted as `data/config_default.rda` (its version is in
#'   `metadata$version`). Element groups:
#'   * **Metadata** -- `metadata$version`, `metadata$date` and
#'     `metadata$description` (the full change log) for provenance tracking;
#'   * **Simulation window and locations** -- `seed`, `date_start`, `date_stop`,
#'     `location_name`, `longitude`, `latitude`;
#'   * **Initial conditions** -- counts (`N_j_initial`, `S_j_initial`,
#'     `E_j_initial`, `I_j_initial`, `R_j_initial`, `V1_j_initial`,
#'     `V2_j_initial`) and the matching proportions (`prop_*_initial`);
#'   * **Global scalars** -- disease and immunity rates (`iota`, `gamma_1`,
#'     `gamma_2`, `epsilon`, `phi_1`, `phi_2`, `omega_1`, `omega_2`), the
#'     observation process (`rho`, `rho_deaths`, `sigma`, `chi_endemic`,
#'     `chi_epidemic`, `delta_reporting_cases`), mobility (`mobility_omega`,
#'     `mobility_gamma`), mixing (`alpha_2`), shedding and dose (`zeta_1`,
#'     `zeta_2`, `zeta_ratio`, `kappa`), environmental decay
#'     (`decay_days_short`, `decay_days_spread`, `decay_days_long`,
#'     `decay_shape_1`, `decay_shape_2`) and the seasonal period `p`;
#'   * **Per-location vectors** -- transmission (`beta_j0_tot`, `p_beta` and the
#'     derived `beta_j0_hum`, `beta_j0_env`), `alpha_1`, `tau_i`, `theta_j`,
#'     seasonality (`a_1_j`, `a_2_j`, `b_1_j`, `b_2_j`), `epidemic_threshold`
#'     and the suitability calibration `psi_star_a`, `psi_star_b`, `psi_star_z`,
#'     `psi_star_k`;
#'   * **Location x day matrices** -- birth and death rates (`b_jt`, `d_jt`),
#'     one- and two-dose vaccination (`nu_1_jt`, `nu_2_jt`, with the eligible
#'     compartments in `nu_jt_sources`), environmental suitability (`psi_jt`,
#'     uncalibrated: \code{sample_parameters()} applies `psi_star_*` to it), and
#'     the reported case fatality ratio `mu_jt`, the deaths input of the
#'     mortality model since MOSAIC v0.96.0;
#'   * **Observed data** -- `reported_cases` and `reported_deaths` with their
#'     per-observation confidence weights `reported_cases_weight` and
#'     `reported_deaths_weight`, and the `epidemic_peaks` data frame used by the
#'     peak shape terms of the likelihood.
#'
#' @details
#' The object is self-contained: it references no external files and runs
#' directly, e.g. \code{run_simulation(config_default)}. The engine does not
#' read `psi_star_*`, so a direct run uses the raw `psi_jt` and ignores the
#' shipped `psi_star_b = 1`; to simulate with the calibrated suitability, pass
#' the config through \code{sample_parameters()} first. For tutorials or
#' fast tests the smaller toy configs [`config_simulation_epidemic`] and
#' [`config_simulation_endemic`] are also available.
#'
#' **Note on Initial Condition Formats**: This configuration includes both count
#' (`*_j_initial`) and proportion (`prop_*_initial`) representations of initial
#' conditions. The count fields are required by the transmission model for simulation,
#' while the proportion fields are optional and provided for statistical analysis
#' convenience. Both formats are automatically maintained in sync during
#' parameter sampling operations.
#'
#' @usage
#' config_default
#'
#' @seealso
#' * `data-raw/make_config_default.R` – the script that builds this object.
#' * [make_simulation_config()] – the validator/factory function used internally
#'   by the build script to validate every parameter.
#' * [config_simulation_epidemic] – one-year outbreak toy data set.
#' * [config_simulation_endemic] – 5-year endemic toy data set.
#'
#' @keywords datasets
"config_default"
