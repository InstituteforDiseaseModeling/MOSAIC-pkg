#' Epidemic Simulation LASER Configuration
#'
#' A **toy one-year outbreak** parameter set for quick unit-tests and
#' vignette examples.
#' The object is created by `make_simulated_LASER_config_files.R` and saved in
#' the package as **`config_simulation_epidemic`**.
#'
#' @format A named **list** identical in structure to
#'   [config_default] but much smaller.  Key elements include
#'   day-by-day matrices (`b_jt`, `psi_jt`, `mu_jt`, …), initial state vectors,
#'   and placeholder `reported_cases` / `reported_deaths`.
#'
#' @details
#' | Aspect | Setting |
#' | ------ | ------- |
#' | **Time span** | 1 Jan 2020 – 31 Dec 2020 (365 days). |
#' | **Locations** | “FOO” (5 k), “BAR” (10 k), “BAZ” (20 k). |
#' | **Initial mix** | 50 % susceptible, 20–30 % first-dose vaccinated, 3 seeding infections in “FOO”. |
#' | **Transmission** | Baseline β fixed per site: **0.30**, **0.50**, **0.40** with ±0.15 seasonal sine forcing. |
#' | **Mobility** | Daily departure probabilities τ<sub>i</sub> = 0.005, 0.002, 0.006. |
#' | **Environment** | Flat suitability (ψ = 0.7), short environmental half-life (decay_days_long = 90). |
#' | **Observed data** | `reported_cases` / `reported_deaths` matrices are filled with **NA** (no real surveillance data). |
#' | **Seed** | `set.seed(999999999)` for full reproducibility. |
#'
#' These settings produce a single, clearly defined epidemic wave in each
#' patch—especially pronounced in “BAR”, which has the highest baseline
#' transmission (β = 0.50).  The configuration is small enough to run in
#' fractions of a second yet exercises all major LASER components, making it
#' ideal for automated tests and teaching materials.
#'
#' @usage
#' config_simulation_epidemic
#'
#' @seealso
#'   * [config_simulation_endemic] – long-run endemic scenario.
#'   * [config_default] – comprehensive default configuration.
#'   * `make_simulated_LASER_config_files.R` – script that generates this object.
#'
#' @keywords datasets
"config_simulation_epidemic"
