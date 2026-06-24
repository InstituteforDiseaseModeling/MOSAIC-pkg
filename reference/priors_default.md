# Default prior distributions for MOSAIC model parameters

A list of informative prior distributions for all MOSAIC model
parameters, organised into global (single-value) priors and
location-specific (per-country) priors for 40 SSA / N-Africa countries.
Built by `data-raw/make_priors_default.R` (see that script's block
comments for the literature sources and derivation of each prior).
Current version: `priors_default$metadata$version`.

## Usage

``` r
priors_default
```

## Format

A list with 3 main components.

- metadata:

  Version, build date, and a free-text description listing the changes
  that produced this version of the priors.

- parameters_global:

  Named list of 27 global priors (single value shared across all
  locations). See `Global parameters` below.

- parameters_location:

  Named list of 22 location-specific prior families. Each entry holds a
  `location` sublist keyed by ISO3 country code, so e.g.
  `parameters_location$beta_j0_tot$location$ETH` gives the prior for
  Ethiopia's total baseline transmission rate.

Each leaf entry has the shape
`list(description, distribution, parameters)` where `parameters` holds
the distribution-specific hyperparameters (e.g. `shape1`/`shape2` for
beta, `meanlog`/`sdlog` for lognormal, `mean`/`sd`/`a`/`b` for truncated
normal).

## Global parameters (26)

Transmission and FOI structure:

- `alpha_2` – Exponent on `N_jt` in the FOI denominator (Beta). 1 =
  frequency-dependent transmission; 0 = density-dependent. A single
  global scalar (`alpha_1` is now per-location – see below).

- `kappa` – Half-saturation V. cholerae concentration at which the
  environmental dose-response is 50\\ capacity).

Environmental persistence (V. cholerae decay in water):

- `decay_days_short` – Short-end decay timescale (Truncnorm, days).

- `decay_days_spread` – Spread used to derive
  `decay_days_long = decay_days_short + decay_days_spread` (Truncnorm).
  `decay_days_long` itself is derived at sampling time and has no
  first-class prior.

- `decay_shape_1`, `decay_shape_2` – Truncnorm shape coefficients in the
  Beta-shaped mapping from suitability \$\psi\$ to decay rate (NOT
  natural-infection immunity decay).

Disease progression (rates, per day):

- `iota` – Incubation rate `E -> I` (Lognormal; *rate*, not period).
  Prior median ~0.71/day.

- `gamma_1` – Symptomatic shedding-duration rate `I_1 -> R` (Lognormal).
  Prior median 0.1/day (~10-day shedding); 95\\ ~3.75 - 26.6 days.

- `gamma_2` – Asymptomatic shedding-duration rate `I_2 -> R`
  (Lognormal). Prior median 0.5/day (~2-day shedding); 95\\ ~0.91 - 4.39
  days.

- `epsilon` – Natural-infection immunity waning rate `R -> S`
  (Lognormal). Prior mean 3.9e-4/day -\> ~7-yr immunity (King et al.
  2008 + project's two-cohort re-fit).

Vaccination (OCV):

- `phi_1`, `phi_2` – One-dose / two-dose vaccine effectiveness at the
  moment of dose delivery (Beta).

- `omega_1`, `omega_2` – One-dose / two-dose vaccine-induced immunity
  waning rates (Gamma, per day).

Symptom expression, care-seeking, surveillance:

- `sigma` – Fraction of infections that are symptomatic (Beta).

- `rho` – Care-seeking rate: probability a symptomatic individual
  presents to surveillance (Beta; not a reporting fraction).

- `rho_deaths` – Surveillance capture rate of true cholera deaths (Beta;
  random-effects meta-analysis of Routh 2017, Shikanga 2009, Bwire 2013
  — see `MOSAIC-pkg/claude/rho_deaths_research/SYNTHESIS_REPORT.md`).
  Consumed by laser-cholera v0.13+ in the deaths likelihood: observed
  surveillance `reported_deaths` is compared against simulated
  `reported_deaths = round(disease_deaths * rho_deaths)` (lagged by
  `delta_reporting_deaths`). Production prior is the recommended
  prediction-interval variant Beta(6.30, 8.52); the informative variant
  Beta(36.95, 51.02) is retained for sensitivity per SYNTHESIS_REPORT
  sec 3.4.

- `chi_endemic`, `chi_epidemic` – Positive predictive value among
  suspected cases during endemic vs epidemic phases (Beta).

- `delta_reporting_cases` – Symptom-onset-to-surveillance-report delay
  (Truncnorm, days). *Not* infection-to-report – incubation is handled
  separately by the E compartment and `iota`.

- `delta_reporting_deaths` – Death-event-to-death-report delay in days
  (Truncnorm). This is the time from a true cholera death to its
  appearance in surveillance reports. The symptom-onset-to-death
  interval itself is implicit in the SEIR dynamics (\\\gamma_1^{-1}\\ ~
  5-7 days symptomatic to recovery/death). Consumed by laser-cholera
  v0.13+ at `infectious.py:88-92` as
  `reported_deaths[t] = round(disease_deaths[t-delta] * rho_deaths)`.

Environmental shedding intensity (V. cholerae cells per person per day):

- `zeta_1` – Symptomatic shedding rate (Lognormal).

- `zeta_2` – Asymptomatic shedding rate (Lognormal). Also has a
  first-class prior, but is typically *derived at sampling time* as
  `zeta_1 / zeta_ratio` rather than drawn independently.

- `zeta_ratio` – `zeta_1 / zeta_2` (Lognormal; ~5-OOM literature spread
  retained intentionally).

Mobility (gravity-model exponents):

- `mobility_gamma`, `mobility_omega` – Distance-decay and
  population-scaling exponents (Gamma).

## Location-specific parameters (23)

Each carries a per-iso prior under
`parameters_location$<param>$location$<ISO3>`. Distribution family is
the same across isos for a given parameter (noted below);
hyperparameters differ (except `alpha_1`, which uses a shared marginal
across isos).

Transmission and contact:

- `alpha_1` – FOI mixing exponent on the infectious term (Beta; 0 = no
  mixing, 1 = well-mixed). Relocated from global to per-location
  (v15.16) with a shared informative `Beta(28.4, 71.6)` for every iso;
  the engine applies it elementwise per patch (a scalar `alpha_1` is
  still accepted and broadcast for national/legacy configs).

- `beta_j0_tot` – Total baseline transmission rate per location
  (Lognormal). `beta_j0_hum` and `beta_j0_env` are *derived* as
  `p_beta * beta_j0_tot` and `(1 - p_beta) * beta_j0_tot`; neither is
  directly sampled.

- `p_beta` – Fraction of total transmission attributable to direct
  human-to-human contact (Beta).

- `tau_i` – Daily departure probability per location, gravity model
  (Beta).

- `theta_j` – WASH-coverage fraction per location (Beta).

Seasonality (Fourier coefficients, all fit to *cases*):

- `a_1_j`, `a_2_j`, `b_1_j`, `b_2_j` – Annual + semi-annual Fourier
  coefficients (Normal). Earlier doc versions erroneously labelled
  `b_1`/`b_2` as "for deaths" – all four are fitted to cases.

Initial conditions (per-compartment proportions of population at t=0):

- `prop_S_initial`, `prop_V1_initial`, `prop_V2_initial`,
  `prop_E_initial`, `prop_I_initial`, `prop_R_initial` – All Beta priors
  per iso. Generated by `est_initial_S/V1_V2/E_I/R()` from demography,
  surveillance history, OCV campaigns, etc.

Disease-mortality:

- `mu_j_baseline` – Per-iso baseline CFR rate (Gamma).

- `mu_j_slope` – Per-iso slope coupling CFR to outbreak intensity
  (Gamma).

- `mu_j_epidemic_factor` – Per-iso multiplier applied to `mu_j_baseline`
  once epidemic phase is detected (Gamma).

Phase switching:

- `epidemic_threshold` – Per-iso `I_sym / N` threshold above which
  `chi_epidemic` and `mu_j_epidemic_factor` engage (Truncnorm).

Environmental-suitability calibration (used by the LSTM \$\psi\$
pipeline):

- `psi_star_a` – Gain on the logit-scale suitability score (Truncnorm).

- `psi_star_b` – Logit-scale offset (Normal).

- `psi_star_z` – Smoothing weight for the causal EWMA (Beta).

- `psi_star_k` – Time-offset (lag) parameter (Truncnorm).

## Countries

Location-specific parameters cover 40 SSA / North-African countries
(ISO3 codes): AGO, BDI, BEN, BFA, BWA, CAF, CIV, CMR, COD, COG, ERI,
ETH, GAB, GHA, GIN, GMB, GNB, GNQ, KEN, LBR, MLI, MOZ, MRT, MWI, NAM,
NER, NGA, RWA, SEN, SLE, SOM, SSD, SWZ, TCD, TGO, TZA, UGA, ZAF, ZMB,
ZWE.

## See also

- [config_default](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/config_default.md)
  – Default LASER configuration that pairs with these priors.

- [`sample_parameters()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sample_parameters.md)
  – Draws samples from these priors.

- [config_simulation_epidemic](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/config_simulation_epidemic.md)
  – One-year outbreak toy configuration.

- [config_simulation_endemic](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/config_simulation_endemic.md)
  – Multi-year endemic toy configuration.
