#' LASER Configuration for an endemic simulation
#'
#' A **long-horizon endemic** parameter set that sustains low-level
#' transmission with seasonal and multi-year waves driven by environmental
#' persistence.  The object is created by
#' `make_simulation_endemic_LASER_config_files.R` and is intended for
#' demonstrations of equilibrium behaviour, immunity waning, and repeated
#' outbreaks.
#'
#' @format A named **list** identical in structure to
#'   [config_simulation_epidemic] but spanning 5 years of daily timesteps.
#'
#' @details
#' * **Time frame:** 1 January 2020 – 31 December 2034 (≈ 5 500 days).
#' * **Locations:** “FOO”, “BAR”, “BAZ” with populations 8 k, 12 k, 20 k.
#' * **Initial immunity/infection mix:** 48 % susceptible, 30 % vaccinated,
#'   20 % recovered, 1.5 % exposed, 0.5 % infectious.
#' * **Seasonality & environment:**
#'   • Annual β forcing amplitude ≈0.10 with random phase per site.
#'   • Environmental suitability (`psi_jt`) combines annual and 4-year sine
#'     components; vibrios persist ~1 year (`decay_days_long = 365`).
#' * **Immunity waning:** `omega_1 ≈ 1/180`, `omega_2 ≈ 1/300` days.
#'
#' The long duration and environmental carry-over allow recovered and
#' vaccinated individuals to lose protection, replenishing susceptibles and
#' producing recurrent outbreaks that resemble real-world endemic dynamics.
#'
#' @usage
#' config_simulation_endemic
#'
#' @seealso
#'   * [config_simulation_epidemic] for the short epidemic scenario.
#'   * [config_default] for the full default configuration.
#'   * `make_simulation_endemic_LASER_config_files.R` for the generator
#'     script.
#'
#' @keywords datasets
"config_simulation_endemic"
