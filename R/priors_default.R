#' Default prior distributions for MOSAIC model parameters
#'
#' A list of informative prior distributions for all MOSAIC model parameters,
#' organised into global (single-value) priors and location-specific
#' (per-country) priors for 40 SSA / N-Africa countries. Built by
#' \code{data-raw/make_priors_default.R} (see that script's block comments for
#' the literature sources and derivation of each prior). Current version:
#' \code{priors_default$metadata$version}.
#'
#' @format A list with 3 main components.
#' \describe{
#'   \item{metadata}{Version, build date, and a free-text description listing
#'     the changes that produced this version of the priors.}
#'   \item{parameters_global}{Named list of 27 global priors (single value
#'     shared across all locations). See `Global parameters` below.}
#'   \item{parameters_location}{Named list of 22 location-specific prior
#'     families. Each entry holds a `location` sublist keyed by ISO3 country
#'     code, so e.g. `parameters_location$beta_j0_tot$location$ETH` gives the
#'     prior for Ethiopia's total baseline transmission rate.}
#' }
#'
#' Each leaf entry has the shape
#' \code{list(description, distribution, parameters)} where `parameters` holds
#' the distribution-specific hyperparameters (e.g. `shape1`/`shape2` for beta,
#' `meanlog`/`sdlog` for lognormal, `mean`/`sd`/`a`/`b` for truncated normal).
#'
#' @section Global parameters (27):
#'
#' Transmission and FOI structure:
#' \itemize{
#'   \item \code{alpha_1} -- FOI mixing exponent on `(I/N)` (Beta; 0 = no
#'     mixing, 1 = well-mixed).
#'   \item \code{alpha_2} -- Exponent on `N_jt` in the FOI denominator (Beta).
#'     1 = frequency-dependent transmission; 0 = density-dependent.
#'   \item \code{kappa} -- Half-saturation V. cholerae concentration at which
#'     the environmental dose-response is 50\% (Lognormal; *not* a carrying
#'     capacity).
#' }
#'
#' Environmental persistence (V. cholerae decay in water):
#' \itemize{
#'   \item \code{decay_days_short} -- Short-end decay timescale (Truncnorm,
#'     days).
#'   \item \code{decay_days_spread} -- Spread used to derive `decay_days_long
#'     = decay_days_short + decay_days_spread` (Truncnorm). `decay_days_long`
#'     itself is derived at sampling time and has no first-class prior.
#'   \item \code{decay_shape_1}, \code{decay_shape_2} -- Truncnorm shape
#'     coefficients in the Beta-shaped mapping from suitability $\\psi$ to
#'     decay rate (NOT natural-infection immunity decay).
#' }
#'
#' Disease progression (rates, per day):
#' \itemize{
#'   \item \code{iota} -- Incubation rate `E -> I` (Lognormal; *rate*, not
#'     period). Prior median ~0.71/day.
#'   \item \code{gamma_1} -- Symptomatic shedding-duration rate `I_1 -> R`
#'     (Lognormal). Prior median 0.1/day (~10-day shedding); 95\% CI
#'     ~3.75 - 26.6 days.
#'   \item \code{gamma_2} -- Asymptomatic shedding-duration rate `I_2 -> R`
#'     (Lognormal). Prior median 0.5/day (~2-day shedding); 95\% CI
#'     ~0.91 - 4.39 days.
#'   \item \code{epsilon} -- Natural-infection immunity waning rate `R -> S`
#'     (Lognormal). Prior mean 3.9e-4/day -> ~7-yr immunity (King et al. 2008
#'     + project's two-cohort re-fit).
#' }
#'
#' Vaccination (OCV):
#' \itemize{
#'   \item \code{phi_1}, \code{phi_2} -- One-dose / two-dose vaccine
#'     effectiveness at the moment of dose delivery (Beta).
#'   \item \code{omega_1}, \code{omega_2} -- One-dose / two-dose vaccine-induced
#'     immunity waning rates (Gamma, per day).
#' }
#'
#' Symptom expression, care-seeking, surveillance:
#' \itemize{
#'   \item \code{sigma} -- Fraction of infections that are symptomatic (Beta).
#'   \item \code{rho} -- Care-seeking rate: probability a symptomatic
#'     individual presents to surveillance (Beta; not a reporting fraction).
#'   \item \code{rho_deaths} -- Surveillance capture rate of true cholera
#'     deaths (Beta; random-effects meta-analysis of Routh 2017, Shikanga 2009,
#'     Bwire 2013 — see
#'     \code{MOSAIC-pkg/claude/rho_deaths_research/SYNTHESIS_REPORT.md}).
#'     Consumed by laser-cholera v0.13+ in the deaths likelihood: observed
#'     surveillance \code{reported_deaths} is compared against simulated
#'     \code{reported_deaths = round(disease_deaths * rho_deaths)} (lagged
#'     by \code{delta_reporting_deaths}). Production prior is the recommended
#'     prediction-interval variant Beta(6.30, 8.52); the informative variant
#'     Beta(36.95, 51.02) is retained for sensitivity per SYNTHESIS_REPORT
#'     sec 3.4.
#'   \item \code{chi_endemic}, \code{chi_epidemic} -- Positive predictive
#'     value among suspected cases during endemic vs epidemic phases (Beta).
#'   \item \code{delta_reporting_cases} -- Symptom-onset-to-surveillance-report
#'     delay (Truncnorm, days). *Not* infection-to-report -- incubation is
#'     handled separately by the E compartment and `iota`.
#'   \item \code{delta_reporting_deaths} -- Death-event-to-death-report
#'     delay in days (Truncnorm). This is the time from a true cholera death
#'     to its appearance in surveillance reports. The symptom-onset-to-death
#'     interval itself is implicit in the SEIR dynamics
#'     (\eqn{\gamma_1^{-1}} ~ 5-7 days symptomatic to recovery/death).
#'     Consumed by laser-cholera v0.13+ at \code{infectious.py:88-92} as
#'     \code{reported_deaths[t] = round(disease_deaths[t-delta] * rho_deaths)}.
#' }
#'
#' Environmental shedding intensity (V. cholerae cells per person per day):
#' \itemize{
#'   \item \code{zeta_1} -- Symptomatic shedding rate (Lognormal).
#'   \item \code{zeta_2} -- Asymptomatic shedding rate (Lognormal). Also has
#'     a first-class prior, but is typically *derived at sampling time* as
#'     `zeta_1 / zeta_ratio` rather than drawn independently.
#'   \item \code{zeta_ratio} -- `zeta_1 / zeta_2` (Lognormal; ~5-OOM
#'     literature spread retained intentionally).
#' }
#'
#' Mobility (gravity-model exponents):
#' \itemize{
#'   \item \code{mobility_gamma}, \code{mobility_omega} -- Distance-decay and
#'     population-scaling exponents (Gamma).
#' }
#'
#' @section Location-specific parameters (22):
#'
#' Each carries a per-iso prior under
#' `parameters_location$<param>$location$<ISO3>`. Distribution family is the
#' same across isos for a given parameter (noted below); hyperparameters
#' differ.
#'
#' Transmission and contact:
#' \itemize{
#'   \item \code{beta_j0_tot} -- Total baseline transmission rate per location
#'     (Lognormal). `beta_j0_hum` and `beta_j0_env` are *derived* as
#'     `p_beta * beta_j0_tot` and `(1 - p_beta) * beta_j0_tot`; neither is
#'     directly sampled.
#'   \item \code{p_beta} -- Fraction of total transmission attributable to
#'     direct human-to-human contact (Beta).
#'   \item \code{tau_i} -- Daily departure probability per location, gravity
#'     model (Beta).
#'   \item \code{theta_j} -- WASH-coverage fraction per location (Beta).
#' }
#'
#' Seasonality (Fourier coefficients, all fit to *cases*):
#' \itemize{
#'   \item \code{a_1_j}, \code{a_2_j}, \code{b_1_j}, \code{b_2_j} -- Annual +
#'     semi-annual Fourier coefficients (Normal). Earlier doc versions
#'     erroneously labelled `b_1`/`b_2` as "for deaths" -- all four are
#'     fitted to cases.
#' }
#'
#' Initial conditions (per-compartment proportions of population at t=0):
#' \itemize{
#'   \item \code{prop_S_initial}, \code{prop_V1_initial}, \code{prop_V2_initial},
#'     \code{prop_E_initial}, \code{prop_I_initial}, \code{prop_R_initial} --
#'     All Beta priors per iso. Generated by `est_initial_S/V1_V2/E_I/R()`
#'     from demography, surveillance history, OCV campaigns, etc.
#' }
#'
#' Disease-mortality:
#' \itemize{
#'   \item \code{mu_j_baseline} -- Per-iso baseline CFR rate (Gamma).
#'   \item \code{mu_j_slope} -- Per-iso slope coupling CFR to outbreak
#'     intensity (Gamma).
#'   \item \code{mu_j_epidemic_factor} -- Per-iso multiplier applied to
#'     `mu_j_baseline` once epidemic phase is detected (Gamma).
#' }
#'
#' Phase switching:
#' \itemize{
#'   \item \code{epidemic_threshold} -- Per-iso `I_sym / N` threshold above
#'     which `chi_epidemic` and `mu_j_epidemic_factor` engage (Truncnorm).
#' }
#'
#' Environmental-suitability calibration (used by the LSTM $\\psi$ pipeline):
#' \itemize{
#'   \item \code{psi_star_a} -- Gain on the logit-scale suitability score
#'     (Truncnorm).
#'   \item \code{psi_star_b} -- Logit-scale offset (Normal).
#'   \item \code{psi_star_z} -- Smoothing weight for the causal EWMA (Beta).
#'   \item \code{psi_star_k} -- Time-offset (lag) parameter (Truncnorm).
#' }
#'
#' @section Countries:
#'
#' Location-specific parameters cover 40 SSA / North-African countries
#' (ISO3 codes): AGO, BDI, BEN, BFA, BWA, CAF, CIV, CMR, COD, COG, ERI, ETH,
#' GAB, GHA, GIN, GMB, GNB, GNQ, KEN, LBR, MLI, MOZ, MRT, MWI, NAM, NER, NGA,
#' RWA, SEN, SLE, SOM, SSD, SWZ, TCD, TGO, TZA, UGA, ZAF, ZMB, ZWE.
#'
#' @usage
#' priors_default
#'
#' @seealso
#' * [config_default] -- Default LASER configuration that pairs with these priors.
#' * [sample_parameters()] -- Draws samples from these priors.
#' * [config_simulation_epidemic] -- One-year outbreak toy configuration.
#' * [config_simulation_endemic] -- Multi-year endemic toy configuration.
#'
#' @keywords datasets
"priors_default"
