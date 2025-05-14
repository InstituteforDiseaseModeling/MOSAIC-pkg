#' LASER Configuration for an epidemic simulation
#'
#' A **toy, short-horizon** parameter set that generates a single-year
#' cholera **epidemic** across three small metapopulation patches.  The object
#' is produced by `make_simulation_epidemic_LASER_config_files.R` and is meant
#' for unit tests, vignettes, and examples that need a fast-running outbreak
#' with clearly defined peaks.  All parameters and initial conditions are
#' internally generated – no external input files are required.
#'
#' @format A named **list** with the full LASER parameter structure created by
#'   [MOSAIC::make_LASER_config()].  Elements include model constants
#'   (`phi_1`, `gamma_1`, …), time-dependent matrices (`b_jt`, `psi_jt`),
#'   initial state vectors (`S_j_initial`, `I_j_initial`, …), and placeholders
#'   for `reported_cases` / `reported_deaths`.
#'
#' @details
#' * **Time frame:** 1 January 2020 – 31 December 2020 (365 days).
#' * **Locations:** “FOO”, “BAR”, “BAZ” with populations 5 k, 10 k, 20 k.
#' * **Initial immunity/infection mix:** 50 % susceptible, 20–30 % first-dose
#'   vaccinated, a handful of seed infections in “FOO”.
#' * **Seasonality:** Moderate amplitude (≈0.15) sine forcing of β produces a
#'   single pronounced epidemic wave in each site.
#' * **Environment:** Flat suitability (`psi_jt = 0.7`), short environmental
#'   half-life (`decay_days_long = 90`).
#'
#' This configuration is small enough to simulate in a few hundred
#' milliseconds yet still exercises all major LASER components, making it
#' ideal for automated tests.
#'
#' @usage
#' config_simulation_epidemic
#'
#' @seealso
#'   * [config_simulation_endemic] for a long-run endemic scenario.
#'   * [config_default] for the comprehensive default configuration.
#'   * `make_simulation_epidemic_LASER_config_files.R` for the script that
#'     creates this object.
#'
#' @keywords datasets
"config_simulation_epidemic"
