library(MOSAIC)
library(jsonlite)

# make_priors_default.R - Generate default prior distributions for MOSAIC model parameters
# Canonical source for priors_default.rda (v0.28.4: retired duplicate make_priors.R).

# Set up paths
MOSAIC::set_root_directory("~/MOSAIC")
PATHS <- MOSAIC::get_paths()

# Load config_default to get the global date_start
config_default <- MOSAIC::config_default
date_start <- as.Date(config_default$date_start)

j <- MOSAIC::iso_codes_mosaic

#----------------------------------------
# Load surveillance and demographics data for epidemic_threshold priors
#----------------------------------------

surv_weekly <- read.csv(
     file.path(PATHS$DATA_PROCESSED, "cholera/weekly/cholera_surveillance_weekly_combined.csv"),
     stringsAsFactors = FALSE
)

dem_annual <- read.csv(
     file.path(PATHS$DATA_PROCESSED, "demographics/demographics_mosaic_countries_2000_2024_annual.csv"),
     stringsAsFactors = FALSE
)

priors_default <- list(
     metadata = list(
          version = "15.8",
          date = Sys.Date(),
          description = "Default informative prior distributions for MOSAIC model parameters. v15.8 (2026-06-03): rho (cases-side care-seeking) re-derived as Beta(5.38, 7.10), mean 0.423, 95% CI [0.19, 0.70], ESS ~12.5, from random-effects pooling of TWO Wiens et al. 2025 (PMC12013865) case-definition strata: general diarrhea (29.9% [25.3, 35.1], n=122 obs) and severe diarrhea + cholera (58.6% [39.9, 75.2], n=22 obs). Pooling both strata captures the severity spectrum of symptomatic cholera (mild-to-moderate + severe), avoiding the upward bias of the severe-only stratum (dominated by outbreak-response settings) and the downward bias of the general stratum (broader population including many self-resolving episodes). The 12 GEMS Nasrin 2013 pediatric MSD entries previously included alongside Wiens were dropped because (a) GEMS measures pediatric MSD, a different population than MOSAIC's all-ages cholera; (b) the 12-strata-to-1 pooling was upside-down dimensionally; (c) Wiens already includes GEMS-derived data at the population level (6 of its Study IDs are tagged GEMS/HUAS). The prior mean moves from 0.276 to 0.423 - a moderate shift in the direction implied by the cholera-specific evidence while staying within clinical plausibility (per-episode CFR sanity check passes for all high-N test countries: MOZ 3.4%, ETH 9.1%, KEN 9.9%, COD 14.6%). See MOSAIC-pkg/R/get_rho_care_seeking_params.R for the full rationale. v15.7 (2026-06-02): rho_deaths switched back to the informative variant Beta(36.95, 51.02) (pooled-mean CI fit, ESS ~88, sd ~0.05). Rationale: MOSAIC's deaths likelihood identifies the product mu_j_baseline * rho_deaths per country, leaving a flat (sloppy) factorization direction. The narrow rho_deaths prior pins it near 0.42 during calibration sampling so mu_j_baseline posteriors carry the cross-country CFR signal cleanly. The wider prediction-interval variant Beta(6.30, 8.52) is retained for sensitivity analysis (see SYNTHESIS_REPORT.md sec 3.2). v15.6 (2026-06-02): mu_j_baseline derivation corrected for laser-cholera v0.13+ schema: cfr_to_mu_adjustment = rho / (rho_deaths * chi) (was rho/chi pre-v0.13). Per-country Gamma priors derived directly from the data-informed CFR (hierarchical GAM) using the steady-state identity mu_j_baseline = CFR * rho / (rho_deaths * chi); the rho, rho_deaths, chi means are computed inline from their actual Beta priors so the conversion factor stays in sync. Per-country prior means are ~2.36x their pre-v15.6 values (this corrects the pre-v0.13 under-scaling where mu_j_baseline implicitly absorbed 1/rho_deaths). MOZ-specific mu_j_baseline override Gamma(2, 1176) and mu_j_epidemic_factor override Gamma(1.5, 0.5) dropped — both were calibrated under the pre-v0.13 misspecified likelihood and are superseded by the universal data-driven prior. v15.5 (2026-06-02): (a) rho_deaths switched from informative Beta(36.95, 51.02) -> recommended Beta(6.30, 8.52) per SYNTHESIS_REPORT.md sec 3.4; both share centre ~0.42, but Beta(6.30, 8.52) fits the 95% prediction interval and is the production default (encodes both pooled mean precision AND between-study heterogeneity); informative variant retained for sensitivity. (b) delta_reporting_deaths description corrected from 'Symptom-onset-to-death-report' to 'Death-event-to-death-report' to match laser-cholera v0.13+ engine semantics (the symptom-onset-to-death lag is implicit in gamma_1^-1 in the SEIR dynamics, not in this parameter). v15.4 (2026-06-01): rho_deaths replaced (Beta(3, 2) -> Beta(36.95, 51.02)) using random-effects meta-analysis (DerSimonian-Laird, logit scale) on three SSA studies (Routh 2017 Tanzania, Shikanga 2009 Kenya, Bwire 2013 Uganda); the informative variant is fit to the 95% CI of the pooled mean. New prior: mean 0.42, 95% CI [0.32, 0.52]. The previous Beta(3, 2) attribution to Finger 2024 was incorrect (editorial, no quantitative anchor); see MOSAIC-pkg/claude/rho_deaths_research/SYNTHESIS_REPORT.md. v15.3 (2026-06-01): beta_j0_tot location prior for ETH recentred from the global median 2e-5 to 1.75e-6 (Ethiopia is low-incidence; the global value over-predicts reported cases ~8x). Derived from fixed-ensemble fitting against current ETH surveillance + raw LSTM suitability; conditional on the suitability-window mean. v15.2 (2026-05-29): removed 2x sd variance-inflation step on epsilon (sd back to 2.0e-4 from 4.0e-4); the inflation had pushed the upper-tail natural-immunity duration to ~53 yr with no documented rationale. v15.1 (2026-04-29): rho_deaths added as a first-class global prior, Beta(3, 2), reflecting ~60% surveillance capture of true cholera deaths (Finger et al. 2024; laser-cholera#49). v15.0 (2026-04-23): zeta_1, zeta_2, and zeta_ratio re-estimated from literature meta-analysis (~6 OOM scale shift on zeta_1). zeta_2 added as first-class prior."
     ),
     parameters_global = list(),    # Single parameters used by all locations
     parameters_location = list()   # Location specific parameters
)

#----------------------------------------
# Global parameters in alphabetical order
#----------------------------------------

# alpha_1 - Population mixing within metapops
beta_fit_alpha_1 <- fit_beta_from_ci(mode_val = 0.25, ci_lower = 0.05, ci_upper = 0.5)

priors_default$parameters_global$alpha_1 <- list(
     description = "Population mixing within metapops (0-1, 1 = well-mixed)",
     distribution = "beta",
     parameters = list(shape1 = beta_fit_alpha_1$shape1, shape2 = beta_fit_alpha_1$shape2)
)

# alpha_2 - Degree of frequency driven transmission
beta_fit_alpha_2 <- fit_beta_from_ci(mode_val = 0.5, ci_lower = 0.25, ci_upper = 0.75)

priors_default$parameters_global$alpha_2 <- list(
     description = "Degree of frequency driven transmission (0-1)",
     distribution = "beta",
     parameters = list(shape1 = beta_fit_alpha_2$shape1, shape2 = beta_fit_alpha_2$shape2)
)

# decay_days_spread - Spread between min and max V. cholerae survival time (days).
# Replaces the direct decay_days_long prior (v0.27.0). decay_days_long is now a DERIVED
# quantity in sample_parameters.R: decay_days_long = decay_days_short + decay_days_spread.
# This algebraically guarantees decay_days_short < decay_days_long (required by
# make_LASER_config()) without the post-hoc swap that previously corrupted the
# joint distribution, and preserves the biological upper bound across staged posteriors
# (the old Uniform(30, 365) prior was fit as unbounded Lognormal at stage 2+).
# Truncnorm(mean=180, sd=95, a=1, b=365) matches the prior-predictive of the old
# Uniform(30, 365) minus TruncNorm(16, 7) (implied mean ≈ 182, sd ≈ 96) and the
# historical posterior shape from MOZ_v43 / calibration_test_46-48 runs (posterior
# implied spread mean ≈ 185, sd ≈ 94, q0.975 ≈ 340 — hugging the 365 ceiling).
priors_default$parameters_global$decay_days_spread <- list(
     description = "Spread between min and max V. cholerae survival time (days)",
     distribution = "truncnorm",
     parameters = list(mean = 180, sd = 95, a = 1, b = 365)
)

# decay_days_short - Minimum V. cholerae survival time
# Upper bound relaxed from 29 to 60 in v0.27.0: the 29 cap was only there to
# guarantee a 1-day gap below decay_days_long's lower bound of 30. That ordering
# is now enforced algebraically via decay_days_spread, so the short bound can
# reflect only the biological upper limit on minimum V. cholerae survival.
# TruncNorm(mean=16, sd=7, a=0.01, b=60): MOZ calibration_test_19 / MOZ_v43
# posteriors concentrated tightly at ~16-17 days with sd ~6, indicating strong
# data support for 2-3 week minimum environmental persistence. The prior wastes
# little mass below 5 days while leaving meaningful support out to ~30 days.
priors_default$parameters_global$decay_days_short <- list(
     description = "Minimum V. cholerae survival time (days)",
     distribution = "truncnorm",
     parameters = list(mean = 16, sd = 7, a = 0.01, b = 60)
)

# decay_shape_1 - First shape parameter of Beta distribution for V. cholerae decay rate transformation
# Truncnorm(mean=3, sd=5, a=0.1, b=10): near-flat across [0.1, 10] (truncation dominates)
# with slight pull away from step-function extremes (s > ~8). Changed from Uniform(0.1, 10)
# to eliminate the uniform->lognormal stage-2 posterior family transition that previously
# allowed values past the biological ceiling of 10. Bounds [0.1, 10] preserved; lower bound
# 0.1 allows U-shaped (arcsine-type) mapping; upper bound 10 supported by 85% of 84 country-
# level calibrations having posterior q97.5 > 4.8 against the old bound=5.
priors_default$parameters_global$decay_shape_1 <- list(
     description = "First shape parameter of Beta distribution for V. cholerae decay",
     distribution = "truncnorm",
     parameters = list(mean = 3, sd = 5, a = 0.1, b = 10.0)
)

# decay_shape_2 - Second shape parameter of Beta distribution for V. cholerae decay rate transformation
# Same rationale as decay_shape_1: Truncnorm(mean=3, sd=5, a=0.1, b=10) preserves the [0.1, 10]
# support across all calibration stages (prior->posterior->prior uses the family-match guard in
# update_priors_from_posteriors.R, so bounds never leak). The truncnorm has slight pull away
# from biologically implausible step-function extremes (s > ~8) without being strongly informative.
priors_default$parameters_global$decay_shape_2 <- list(
     description = "Second shape parameter of Beta distribution for V. cholerae decay",
     distribution = "truncnorm",
     parameters = list(mean = 3, sd = 5, a = 0.1, b = 10.0)
)

# epsilon - Natural immunity waning rate
# sd = 2.0e-4 derived from the 95% CI [1.7e-4, 1.03e-3] reported in King et al.
# 2008 and the project's own 2-cohort re-fit (~7 yr mean duration). A 2x
# variance-inflation step previously applied here pushed the upper tail to
# ~53 yr immunity duration with no documented rationale and has been removed.
priors_default$parameters_global$epsilon <- list(
     description = "Natural immunity waning rate (per day)",
     distribution = "lognormal",
     parameters = list(mean = 3.9e-4, sd = 2.0e-4)
)

# gamma_1 - Symptomatic/severe shedding duration rate
priors_default$parameters_global$gamma_1 <- list(
     description = "Symptomatic/severe shedding duration rate (per day)",
     distribution = "lognormal",
     parameters = list(
          meanlog = log(1/10),  # log of median rate: 1/10 per day = 10 days shedding
          sdlog = 0.5           # 95% CI on shedding duration ~3.75-26.6 days
     )
)

# gamma_2 - Asymptomatic/mild shedding duration rate
priors_default$parameters_global$gamma_2 <- list(
     description = "Asymptomatic/mild shedding duration rate (per day)",
     distribution = "lognormal",
     parameters = list(
          meanlog = log(1/2),   # log of median rate: 1/2 per day = 2 days shedding
          sdlog = 0.4           # 95% CI on shedding duration ~0.91-4.39 days
     )
)


# iota - Incubation rate (1/days)

priors_default$parameters_global$iota <- list(
     description = "Incubation rate (1/days)",
     distribution = "lognormal",
     parameters = list(meanlog = -0.337, sdlog = 0.4)  # Wider distribution
)

# No variance inflation for iota (factor = 1)

# kappa - Concentration of V. cholerae which leads to 50% infectious dose
# UPDATED v0.28.16: Derived from est_kappa_prior() meta-analysis of 13 literature
# sources (Hornick 1971, Cash 1974, Levine 1981/1988, Tacket 1999, QMRA synthesis,
# expert reviews). Weighted lognormal fit anchored on 8 buffered-volunteer-challenge
# studies (weight=1) with expert reviews downweighted (0 to 0.5 weight).
# Result: LN(meanlog=11.92, sdlog=1.83), median=1.51e5 CFU, 95% CI [4.15e3, 5.46e6].
kappa_prior_fit <- MOSAIC::est_kappa_prior(PATHS = PATHS)
priors_default$parameters_global$kappa <- list(
     description = "Concentration of V. cholerae for 50% infectious dose (meta-analyzed from 13 literature sources)",
     distribution = "lognormal",
     parameters = list(meanlog = kappa_prior_fit$fit$meanlog, sdlog = kappa_prior_fit$fit$sdlog)
)


# load param_gravity_model.csv and get gamma distributions that have mode equal to the values in the .csv
param_gravity <- read.csv(file.path(PATHS$MODEL_INPUT, "param_gravity_model.csv"))
mobility_gamma_mode <- param_gravity$parameter_value[param_gravity$variable_name == "mobility_gamma"]
mobility_omega_mode <- param_gravity$parameter_value[param_gravity$variable_name == "mobility_omega"]

# Gamma distribution mode = (shape - 1) / rate when shape > 1
# Define rate and solve for shape: shape = mode * rate + 1
gamma_rate <- 2
mobility_gamma_shape <- mobility_gamma_mode * gamma_rate + 1
mobility_omega_shape <- mobility_omega_mode * gamma_rate + 1

# mobility_gamma - Mobility distance decay parameter
priors_default$parameters_global$mobility_gamma <- list(
     description = "Mobility distance decay parameter",
     distribution = "gamma",
     parameters = list(shape = mobility_gamma_shape, rate = gamma_rate)
)

# mobility_omega - Mobility population scaling parameter
priors_default$parameters_global$mobility_omega <- list(
     description = "Mobility population scaling parameter",
     distribution = "gamma",
     parameters = list(shape = mobility_omega_shape, rate = gamma_rate)
)

# Load vaccine effectiveness parameters from est_vaccine_effectiveness output
vaccine_param_file <- file.path(PATHS$MODEL_INPUT, "param_vaccine_effectiveness.csv")
if (file.exists(vaccine_param_file)) {
     param_vaccine <- read.csv(vaccine_param_file)
} else {
     warning("Vaccine effectiveness parameter file not found. Using default values.")
     # Define default values as fallback
     param_vaccine <- data.frame(
          variable_name = rep(c("omega_1", "omega_2", "phi_1", "phi_2"), each = 3),
          parameter_name = rep(c("mean", "low", "high"), 4),
          parameter_value = c(
               # omega_1 defaults
               0.0007, 0.0001, 0.002,
               # omega_2 defaults
               0.0005, 0.00001, 0.001,
               # phi_1 defaults
               0.787, 0.7, 0.85,
               # phi_2 defaults
               0.768, 0.65, 0.85
          )
     )
}

# Helper function to extract parameter value from the loaded data
get_vaccine_param <- function(var_name, param_name) {
     val <- param_vaccine$parameter_value[param_vaccine$variable_name == var_name &
                                               param_vaccine$parameter_name == param_name]
     if (length(val) == 0 || is.na(val)) {
          stop(paste("Missing parameter:", var_name, param_name))
     }
     return(val)
}

# omega_1 - Vaccine waning rate (one dose)
# Based on Xu et al. (2024) meta-regression, fitted using est_vaccine_effectiveness()
uncertainty_inflation <- 0.05  # Increase CI width

omega_1_mean <- get_vaccine_param("omega_1", "mean")
omega_1_low <- get_vaccine_param("omega_1", "low")
omega_1_high <- get_vaccine_param("omega_1", "high")

# Validate inputs
if (omega_1_low >= omega_1_high) {
     warning("omega_1: low >= high, swapping values")
     temp <- omega_1_low
     omega_1_low <- omega_1_high
     omega_1_high <- temp
}

# Ensure mean is within bounds
if (omega_1_mean < omega_1_low || omega_1_mean > omega_1_high) {
     warning("omega_1: mean outside of CI, adjusting to midpoint")
     omega_1_mean <- (omega_1_low + omega_1_high) / 2
}

# Inflate uncertainty
omega_1_range <- omega_1_high - omega_1_low
omega_1_low_inflated <- pmax(0.00001, omega_1_low - omega_1_range * uncertainty_inflation)
omega_1_high_inflated <- omega_1_high + omega_1_range * uncertainty_inflation

# Final check
if (omega_1_low_inflated >= omega_1_high_inflated) {
     stop("omega_1: Invalid CI after inflation. Check input data.")
}

omega_1_fit <- fit_gamma_from_ci(
     mode_val = omega_1_mean,
     ci_lower = omega_1_low_inflated,
     ci_upper = omega_1_high_inflated
)

priors_default$parameters_global$omega_1 <- list(
     description = "Vaccine waning rate (one dose, per day)",
     distribution = "gamma",
     parameters = list(
          shape = omega_1_fit$shape,
          rate = omega_1_fit$rate
     )
)

# omega_2 - Vaccine waning rate (two dose)
# Based on Xu et al. (2024) meta-regression, fitted using est_vaccine_effectiveness()
omega_2_mean <- get_vaccine_param("omega_2", "mean")
omega_2_low <- get_vaccine_param("omega_2", "low")
omega_2_high <- get_vaccine_param("omega_2", "high")

# Validate inputs
if (omega_2_low >= omega_2_high) {
     warning("omega_2: low >= high, swapping values")
     temp <- omega_2_low
     omega_2_low <- omega_2_high
     omega_2_high <- temp
}

# Ensure mean is within bounds
if (omega_2_mean < omega_2_low || omega_2_mean > omega_2_high) {
     warning("omega_2: mean outside of CI, adjusting to midpoint")
     omega_2_mean <- (omega_2_low + omega_2_high) / 2
}

# Inflate uncertainty
omega_2_range <- omega_2_high - omega_2_low
omega_2_low_inflated <- pmax(0.00001, omega_2_low - omega_2_range * uncertainty_inflation)
omega_2_high_inflated <- omega_2_high + omega_2_range * uncertainty_inflation

# Final check
if (omega_2_low_inflated >= omega_2_high_inflated) {
     stop("omega_2: Invalid CI after inflation. Check input data.")
}

omega_2_fit <- fit_gamma_from_ci(
     mode_val = omega_2_mean,
     ci_lower = omega_2_low_inflated,
     ci_upper = omega_2_high_inflated
)

priors_default$parameters_global$omega_2 <- list(
     description = "Vaccine waning rate (two dose, per day)",
     distribution = "gamma",
     parameters = list(
          shape = omega_2_fit$shape,
          rate = omega_2_fit$rate
     )
)


# phi_1 - Initial vaccine effectiveness (one dose)
# Based on Xu et al. (2024), fitted using est_vaccine_effectiveness()
uncertainty_inflation <- 0.05  # Reuse same inflation factor
phi_1_mean <- get_vaccine_param("phi_1", "mean")
phi_1_low <- get_vaccine_param("phi_1", "low")
phi_1_high <- get_vaccine_param("phi_1", "high")

phi_1_low_inflated <- pmax(0.001, phi_1_low * (1 - uncertainty_inflation))
phi_1_high_inflated <- pmin(0.999, phi_1_high * (1 + uncertainty_inflation))

# Final check
if (phi_1_low_inflated >= phi_1_high_inflated) {
     stop("phi_1: Invalid CI after inflation. Check input data.")
}

phi_1_fit <- fit_beta_from_ci(
     mode_val = phi_1_mean,
     ci_lower = phi_1_low_inflated,
     ci_upper = phi_1_high_inflated
)

priors_default$parameters_global$phi_1 <- list(
     description = "Initial vaccine effectiveness (one dose)",
     distribution = "beta",
     parameters = list(
          shape1 = phi_1_fit$shape1,
          shape2 = phi_1_fit$shape2
     )
)

# phi_2 - Initial vaccine effectiveness (two dose)
# Based on Xu et al. (2024), fitted using est_vaccine_effectiveness()
phi_2_mean <- get_vaccine_param("phi_2", "mean")
phi_2_low <- get_vaccine_param("phi_2", "low")
phi_2_high <- get_vaccine_param("phi_2", "high")

phi_2_low_inflated <- pmax(0.001, phi_2_low * (1 - uncertainty_inflation))
phi_2_high_inflated <- pmin(0.999, phi_2_high * (1 + uncertainty_inflation))

# Final check
if (phi_2_low_inflated >= phi_2_high_inflated) {
     stop("phi_2: Invalid CI after inflation. Check input data.")
}

phi_2_fit <- fit_beta_from_ci(
     mode_val = phi_2_mean,
     ci_lower = phi_2_low_inflated,
     ci_upper = phi_2_high_inflated
)

priors_default$parameters_global$phi_2 <- list(
     description = "Initial vaccine effectiveness (two dose)",
     distribution = "beta",
     parameters = list(
          shape1 = phi_2_fit$shape1,
          shape2 = phi_2_fit$shape2
     )
)



# ---- chi_endemic and chi_epidemic (PPV of clinical case definition) ----
# Beta distribution parameters from Weins et al. 2023 (PLOS Medicine,
# doi:10.1371/journal.pmed.1004286), fit in get_suspected_cases().
# Low estimate (all settings) -> chi_endemic; High estimate (outbreaks) -> chi_epidemic.
chi_param_file <- file.path(PATHS$MODEL_INPUT, "param_chi_suspected_cases.csv")
if (file.exists(chi_param_file)) {
     param_chi <- read.csv(chi_param_file)

     chi_endemic_shape1 <- param_chi$parameter_value[
          grepl("chi_endemic.*Low estimate", param_chi$variable_description) &
               param_chi$parameter_name == "shape1"
     ]
     chi_endemic_shape2 <- param_chi$parameter_value[
          grepl("chi_endemic.*Low estimate", param_chi$variable_description) &
               param_chi$parameter_name == "shape2"
     ]
     chi_epidemic_shape1 <- param_chi$parameter_value[
          grepl("chi_epidemic.*High estimate", param_chi$variable_description) &
               param_chi$parameter_name == "shape1"
     ]
     chi_epidemic_shape2 <- param_chi$parameter_value[
          grepl("chi_epidemic.*High estimate", param_chi$variable_description) &
               param_chi$parameter_name == "shape2"
     ]

     if (length(chi_endemic_shape1) == 0 || length(chi_endemic_shape2) == 0) {
          warning("Could not extract chi_endemic shape params from file. Using defaults.")
          chi_endemic_shape1 <- 5.43
          chi_endemic_shape2 <- 5.01
     }
     if (length(chi_epidemic_shape1) == 0 || length(chi_epidemic_shape2) == 0) {
          warning("Could not extract chi_epidemic shape params from file. Using defaults.")
          chi_epidemic_shape1 <- 4.79
          chi_epidemic_shape2 <- 1.53
     }
} else {
     warning("Chi PPV parameter file not found. Using default values.")
     chi_endemic_shape1  <- 5.43;  chi_endemic_shape2  <- 5.01
     chi_epidemic_shape1 <- 4.79;  chi_epidemic_shape2 <- 1.53
}

# chi_endemic - PPV among suspected cases during endemic periods (Weins et al. 2023 low estimate)
# Beta(5.43, 5.01) -> median ~0.52, 95% CI [0.24, 0.80]
priors_default$parameters_global$chi_endemic <- list(
     description = "PPV among suspected cases during endemic periods (Weins et al. 2023, all settings)",
     distribution = "beta",
     parameters = list(shape1 = chi_endemic_shape1, shape2 = chi_endemic_shape2)
)

# chi_epidemic - PPV among suspected cases during epidemic periods (Weins et al. 2023 high estimate)
# Beta(4.79, 1.53) -> median ~0.78, 95% CI [0.40, 0.99]
priors_default$parameters_global$chi_epidemic <- list(
     description = "PPV among suspected cases during epidemic periods (Weins et al. 2023, during outbreaks)",
     distribution = "beta",
     parameters = list(shape1 = chi_epidemic_shape1, shape2 = chi_epidemic_shape2)
)

# rho - Care-seeking rate (probability a symptomatic infection presents as a suspected case)
# As of v15.8 (2026-06-03): anchored on the random-effects pool of TWO Wiens
# et al. 2025 (PMC12013865) case-definition strata:
#   - general diarrhea: 29.9% [25.3, 35.1] (n=122 obs)
#   - severe diarrhea + cholera: 58.6% [39.9, 75.2] (n=22 obs)
# Pooling both captures the severity spectrum of symptomatic cholera
# (mild-to-moderate + severe). The previous GEMS (Nasrin 2013) co-anchor was
# dropped because (1) GEMS measures pediatric MSD which is broader / less severe
# than cholera, (2) the 12-strata-to-1 pooling was upside-down dimensionally
# vs Wiens's 23-study meta-analysis, and (3) Wiens already includes
# GEMS-derived data at the population level. See R/get_rho_care_seeking_params.R
# for the full rationale and methodology. Values read from
# param_rho_care_seeking.csv.
rho_param_file <- file.path(PATHS$MODEL_INPUT, "param_rho_care_seeking.csv")
if (file.exists(rho_param_file)) {
     param_rho    <- read.csv(rho_param_file, stringsAsFactors = FALSE)
     rho_shape1   <- param_rho$parameter_value[param_rho$parameter_name == "shape1"]
     rho_shape2   <- param_rho$parameter_value[param_rho$parameter_name == "shape2"]
     if (length(rho_shape1) == 0 || length(rho_shape2) == 0) {
          warning("Could not extract rho shape params from file. Using Beta(3, 7) fallback.")
          rho_shape1 <- 3.0; rho_shape2 <- 7.0
     }
} else {
     warning("param_rho_care_seeking.csv not found. Using Beta(3, 7) fallback.")
     rho_shape1 <- 3.0; rho_shape2 <- 7.0
}

priors_default$parameters_global$rho <- list(
     description = "Care-seeking rate: probability a symptomatic infection is reported as suspected (Wiens et al. 2025 random-effects pool of general + severe/cholera strata)",
     distribution = "beta",
     parameters = list(shape1 = rho_shape1, shape2 = rho_shape2)
)

# rho_deaths - Death detection rate (probability a true cholera death is captured
# by surveillance). Beta(36.95, 51.02): mean 0.420, 95% CI [0.319, 0.524],
# 50% CI [0.391, 0.450], effective sample size ~88.
#
# Derived from random-effects (DerSimonian-Laird) meta-analysis on the logit scale
# of three Sub-Saharan African empirical studies:
#   Routh 2017     (Tanzania, EID)       0.475 [0.376, 0.576]  - binomial CI 48/101
#   Shikanga 2009  (Kenya, AJTMH)        0.342 [0.231, 0.452]  - approx binomial CI
#   Bwire 2013     (Uganda, PLOS NTDs)   0.500 [0.330, 0.950]  - sensitivity range
#
# Pooled (logit-RE): mean 0.419, 95% CI of pool [0.320, 0.525], 95% prediction
# interval [0.162, 0.728]; tau^2 = 0.046, I^2 = 32%.
#
# Production choice (v15.7+): use the informative variant fit to the 95% CI of
# the pooled mean (NOT the prediction interval). Rationale: MOSAIC's deaths
# likelihood identifies the PRODUCT mu_j_baseline * rho_deaths per country,
# leaving a flat (sloppy) direction along mu_baseline * rho_deaths = const. A
# narrow rho_deaths prior pins it near the meta-analytic estimate during
# calibration sampling, letting mu_j_baseline posteriors carry the cross-country
# CFR signal cleanly. The wider prediction-interval variant Beta(6.30, 8.52)
# (SYNTHESIS_REPORT sec 3.2) is more honest about between-study heterogeneity
# but creates posterior degeneracy with mu_baseline; retained for sensitivity
# analysis only. All three anchor studies are SSA outbreak settings - the very
# regime MOSAIC calibrates - so the pooled-mean precision is the operative
# target for production runs.
#
# Methodology parallels R/get_rho_care_seeking_params.R (cases-side rho).
# Full provenance: MOSAIC-pkg/claude/rho_deaths_research/SYNTHESIS_REPORT.md
# (sec 3.3 documents this informative variant; sec 3.2 documents the wider
# variant; sec 3.4 documents the production-vs-sensitivity tradeoff).
# Figure: MOSAIC-pkg/claude/rho_deaths_prior_comparison_v6.png.
# See also laser-cholera#49 for the engine-side reported_deaths implementation.
priors_default$parameters_global$rho_deaths <- list(
     description = "Death detection rate: probability a true cholera death is captured by surveillance (random-effects meta-analysis of Routh 2017, Shikanga 2009, Bwire 2013; informative variant fit to pooled-mean CI for cleaner mu_j_baseline identifiability)",
     distribution = "beta",
     parameters = list(shape1 = 36.95, shape2 = 51.02)
)

# sigma - Proportion symptomatic
priors_default$parameters_global$sigma <- list(
     description = "Proportion symptomatic",
     distribution = "beta",
     parameters = list(shape1 = 4.30, shape2 = 13.51)
)

# zeta_1 - Symptomatic shedding rate (V. cholerae cells per infected person per day)
# UPDATED v0.29.0: Derived from est_zeta_1_prior() weighted-MLE meta-analysis of
# stool concentration x time-averaged daily stool volume anchors (Nelson 2009,
# Merrell 2002, Harris 2012, Kaper 1995, etc.) plus severity-weighted pool
# (endemic mix 0.2/0.4/0.4) and volume-sensitivity rows. See
# plan_zeta_priors_implementation.md Section 5.
zeta_1_res <- MOSAIC::est_zeta_1_prior(PATHS)
priors_default$parameters_global$zeta_1 <- list(
     description = "Symptomatic shedding rate (V. cholerae cells per infected person per day); literature meta-analysis",
     distribution = "lognormal",
     parameters = list(meanlog = zeta_1_res$fit$meanlog,
                       sdlog   = zeta_1_res$fit$sdlog)
)

# zeta_2 - Asymptomatic shedding rate (V. cholerae cells per infected person per day)
# UPDATED v0.29.0: Derived from est_zeta_2_prior(); single-primary-source anchor
# (Nelson 2009) with hard sdlog floor of 2.0 to reflect n_independent = 1.
# Stored as a first-class prior; sample_parameters() still derives zeta_2 at
# sampling time as zeta_2 = zeta_1 / zeta_ratio to guarantee zeta_1 > zeta_2
# algebraically. This prior is the literature-derived *reference* distribution
# for validation. See plan_zeta_priors_implementation.md Section 6.
zeta_2_res <- MOSAIC::est_zeta_2_prior(PATHS)
priors_default$parameters_global$zeta_2 <- list(
     description = "Asymptomatic shedding rate (V. cholerae cells per infected person per day); derived at sampling time as zeta_1/zeta_ratio, this prior is the literature-derived reference for validation",
     distribution = "lognormal",
     parameters = list(meanlog = zeta_2_res$fit$meanlog,
                       sdlog   = zeta_2_res$fit$sdlog)
)

# zeta_ratio - Symptomatic-to-asymptomatic shedding ratio (zeta_1 / zeta_2).
# UPDATED v0.29.0: DIRECT literature-anchor channel only (A). The combined
# channel (C) was switched off 2026-04-23 because its median ~2e5 overestimates
# zeta_ratio relative to the modelling-convention + household-transmission
# evidence (Smith 2026 ~1.6x, Chao/Finger ~10, etc.). The direct channel
# takes Smith 2026, Nelson 2009 paired, Chao 2011, Finger 2018, Sugimoto
# 2014, etc. as literature anchors (median 763, sdlog 4.807). See
# plan_zeta_priors_implementation.md Section 7.2 (Table 7.A).
# zeta_2 is derived: zeta_2 = zeta_1 / zeta_ratio. Guarantees zeta_1 > zeta_2
# algebraically when zeta_ratio > 1.
zeta_ratio_res <- MOSAIC::est_zeta_ratio_prior(
     PATHS,
     zeta_1_fit = zeta_1_res,   # full return list; .extract_fit() unwraps
     zeta_2_fit = zeta_2_res
)
priors_default$parameters_global$zeta_ratio <- list(
     description = "Ratio of symptomatic to asymptomatic shedding rate (zeta_1 / zeta_2); direct literature-anchor channel (Smith 2026, Chao 2011, Finger 2018, Nelson 2009 paired, etc.)",
     distribution = "lognormal",
     parameters = list(meanlog = zeta_ratio_res$diagnostics$fit_direct$meanlog,
                       sdlog   = zeta_ratio_res$diagnostics$fit_direct$sdlog)
)

# delta_reporting_cases - Symptom-onset-to-case reporting delay
# Incubation is already handled by the E compartment (iota parameter); this
# captures only the lag from symptom onset to surveillance report.
# Updated from TruncNorm(mean=2, sd=2) based on MOZ calibration tests 19-28:
# posteriors consistently collapse toward 0-1 days (KL=14.2 in test_19).
# New prior: TruncNorm(mean=1, sd=1.5, a=0, b=7) — mode near 1 day, most mass
# in 0-3 day range while still permitting longer delays for countries with
# slower paper-based reporting systems.
# Sampled value is rounded to the nearest integer before passing to make_LASER_config().
priors_default$parameters_global$delta_reporting_cases <- list(
     description = "Symptom-onset-to-case reporting delay in days (integer, 0-7)",
     distribution = "truncnorm",
     parameters = list(mean = 1, sd = 1.5, a = 0, b = 7)
)

# delta_reporting_deaths - Death-event-to-death-report delay (NOT symptom-onset-to-report)
# laser-cholera v0.13+ at infectious.py:88-92 implements this as a pure
# post-mortem reporting lag: reported_deaths[t] = round(disease_deaths[t-delta] * rho_deaths).
# The symptom-onset-to-death interval is implicit in the SEIR dynamics
# (gamma_1^-1 ~ 5-7 days symptomatic to recovery/death), NOT folded into this
# parameter. Prior anchors: SSA routine surveillance (IDSR) typically reports
# deaths within 3-7 days of the event (Routh 2017 Tanzania urban epidemic,
# Bwire 2013 Uganda) including death registration, facility notification,
# and weekly aggregation cycles.
# Prior: TruncNorm(mean=4, sd=3, a=1, b=14) - mode near 3-5 days, hard ceiling at 14.
# Lower bound a=1: same-day death-to-report is implausible in any realistic
# surveillance system that requires registration + notification + aggregation.
# Sampled value is rounded to the nearest integer before passing to make_LASER_config().
priors_default$parameters_global$delta_reporting_deaths <- list(
     description = "Death-event-to-death-report delay in days (integer, 1-14)",
     distribution = "truncnorm",
     parameters = list(mean = 4, sd = 3, a = 1, b = 14)
)


#---------------------------------------------------
# Location specific parameters in alphabetical order
#---------------------------------------------------

# beta_j0_tot - Total base transmission rate (human + environmental)
#
# Switched from Gompertz to Lognormal. Rationale:
#   - The Gompertz fit was insensitive to the mode_val argument: the shape was
#     driven entirely by (ci_lower, ci_upper), and the intended ci_lower=1e-8
#     could not be achieved (actual Q2.5 = 6.93e-7, 100x off).
#   - Raising ci_upper in the Gompertz shifted the ENTIRE distribution upward,
#     inflating the prior median from 1.89e-5 to 3.80e-5 — too aggressive.
#   - Lognormal allows independent control of the median (via meanlog) and the
#     spread (via sdlog), so the upper tail can be extended without moving the
#     prior center.
#
# Design: median = 2e-5, Q97.5 = 2e-4
#   meanlog = log(2e-5) = -10.8198
#   sdlog   = (log(2e-4) - log(2e-5)) / qnorm(0.975) = log(10) / 1.96 = 1.1748
#
# This gives:
#   Q10  = 4.4e-6  Q50 = 2.0e-5  Q90 = 9.0e-5  Q97.5 = 2.0e-4
#   P(beta_j0_env > 5e-5) = 13%  (was 6% with Gompertz)
#   P(beta_j0_env > 1e-4) = 4%   (was <1% with Gompertz)
#   Fraction of samples below 3e-5 (MOZ-optimal range) = 64% (was 67%)
#
# The extended upper tail allows exploration of dominant-waterborne settings
# across the 40-country SSA ensemble without over-inflating the space for
# typical endemic settings.

beta_j0_tot_meanlog <- log(2e-5)
beta_j0_tot_sdlog   <- (log(2e-4) - log(2e-5)) / qnorm(0.975)

priors_default$parameters_location$beta_j0_tot <- list(
     description = "Total base transmission rate (human + environmental); lognormal with median=2e-5 and Q97.5=2e-4",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$beta_j0_tot$location[[iso]] <- list(
          distribution = "lognormal",
          parameters = list(
               meanlog = beta_j0_tot_meanlog,
               sdlog   = beta_j0_tot_sdlog
          )
     )
}

# --- Per-country recentred transmission defaults ---------------------------
# ETH: the global median (2e-5) over-predicts reported cholera ~8x for Ethiopia.
# Recentred to median 1.75e-6 from fixed-ensemble fitting against current ETH
# surveillance + raw LSTM suitability (see MOSAIC-pkg/local/collab_ETH_v2/).
# NOTE: this median is conditional on the suitability-window mean (psi_bar), which
# the LASER environmental term normalises by (beta_jt_env = beta_j0_env * psi_t/psi_bar).
# If date_start/date_stop change materially the effective magnitude shifts, so
# re-validate -- or, preferably, replace with a calibration-derived median.
priors_default$parameters_location$beta_j0_tot$location[["ETH"]]$parameters$meanlog <- log(1.75e-6)


# p_beta - Proportion of human-to-human vs environmental transmission
beta_fit_p_beta <- fit_beta_from_ci(mode_val = 0.33, ci_lower = 0.1, ci_upper = 0.5)


priors_default$parameters_location$p_beta <- list(
     description = "Proportion of total base transmission that is human-to-human (0–1)",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$p_beta$location[[iso]] <- list(
          distribution = "beta",
          parameters = list(
               shape1 = beta_fit_p_beta$shape1,
               shape2 = beta_fit_p_beta$shape2
          )
     )
}













# tau_i - Country-level travel probabilities
# Beta distribution with parameters loaded from param_tau_departure.csv

tau_uncertainty_factor <- 0.001  # Increase tau uncertainty

priors_default$parameters_location$tau_i <- list(
     description = "Country-level travel probabilities",
     location = list()
)

# Load tau parameters from file
tau_param_file <- file.path(PATHS$MODEL_INPUT, "param_tau_departure.csv")
if (file.exists(tau_param_file)) {
     param_tau <- read.csv(tau_param_file)

     # Extract beta parameters for each location
     for (iso in j) {
          tau_shape1_orig <- param_tau$parameter_value[
               param_tau$i == iso &
                    param_tau$parameter_distribution == "beta" &
                    param_tau$parameter_name == "shape1"
          ]
          tau_shape2_orig <- param_tau$parameter_value[
               param_tau$i == iso &
                    param_tau$parameter_distribution == "beta" &
                    param_tau$parameter_name == "shape2"
          ]

          if (length(tau_shape1_orig) > 0 && length(tau_shape2_orig) > 0) {
               # Calculate the mean of the original distribution
               mean_tau <- tau_shape1_orig / (tau_shape1_orig + tau_shape2_orig)

               # Calculate the concentration (precision) of the original distribution
               concentration_orig <- tau_shape1_orig + tau_shape2_orig

               # Adjust concentration by uncertainty factor
               # Lower factor = lower concentration = higher uncertainty
               concentration_new <- concentration_orig * tau_uncertainty_factor

               # Recalculate shape parameters maintaining the same mean
               tau_shape1 <- mean_tau * concentration_new
               tau_shape2 <- (1 - mean_tau) * concentration_new

               priors_default$parameters_location$tau_i$location[[iso]] <- list(
                    distribution = "beta",
                    parameters = list(
                         shape1 = tau_shape1,
                         shape2 = tau_shape2
                    )
               )
          } else {
               # Default values if not found (very small travel probability)
               warning(paste("tau parameters not found for", iso, "- using defaults"))
               # Apply uncertainty factor to defaults as well
               default_shape1 <- 100 * tau_uncertainty_factor
               default_shape2 <- 1000000 * tau_uncertainty_factor
               priors_default$parameters_location$tau_i$location[[iso]] <- list(
                    distribution = "beta",
                    parameters = list(
                         shape1 = default_shape1,
                         shape2 = default_shape2
                    )
               )
          }
     }

} else {
     warning("tau parameter file not found. Using default values.")
     # Set default values for all locations with uncertainty adjustment
     for (iso in j) {
          priors_default$parameters_location$tau_i$location[[iso]] <- list(
               distribution = "beta",
               parameters = list(
                    shape1 = 100 * tau_uncertainty_factor,
                    shape2 = 1000000 * tau_uncertainty_factor
               )
          )
     }
}





# theta_j - WASH coverage
# Beta distribution fitted from weighted mean WASH estimates with uncertainty
priors_default$parameters_location$theta_j <- list(
     description = "WASH coverage index (proportion with adequate WASH)",
     location = list()
)

theta_uncertainty_one_sided <- 0.05

# Load WASH estimates
wash_param_file <- file.path(PATHS$MODEL_INPUT, "param_theta_WASH.csv")
if (file.exists(wash_param_file)) {

     param_wash <- read.csv(wash_param_file)

     for (iso in j) {

          wash_value <- param_wash$parameter_value[param_wash$j == iso]

          if (length(wash_value) > 0) {

               ci_lower <- pmax(0.001, wash_value - theta_uncertainty_one_sided)
               ci_upper <- pmin(0.999, wash_value + theta_uncertainty_one_sided)

          } else {

               wash_value <- mean(param_wash$parameter_value, na.rm=T)
               ci_lower <- pmax(0.001, wash_value - theta_uncertainty_one_sided*1.25)
               ci_upper <- pmin(0.999, wash_value + theta_uncertainty_one_sided*1.25)

          }

          theta_fit <- fit_beta_from_ci(
               mode_val = wash_value,
               ci_lower = ci_lower,
               ci_upper = ci_upper
          )

          priors_default$parameters_location$theta_j$location[[iso]] <- list(
               distribution = "beta",
               parameters = list(
                    shape1 = theta_fit$shape1,
                    shape2 = theta_fit$shape2
               )
          )

     }

} else {

     # Fallback defaults
     for (iso in j) {
          priors_default$parameters_location$theta_j$location[[iso]] <- list(
               distribution = "beta",
               parameters = list(
                    shape1 = 13.44396,
                    shape2 = 8.236964
               )
          )
     }
}




# Seasonality parameters (a_1, a_2, b_1, b_2) - Fourier wave function parameters
# Normal distributions with parameters loaded from param_seasonal_dynamics.csv
seasonality_uncertainty_factor <- 0.5  # Increase seasonality uncertainty

# Load seasonal dynamics parameters
seasonal_param_file <- file.path(PATHS$MODEL_INPUT, "param_seasonal_dynamics.csv")
seasonal_params_exist <- file.exists(seasonal_param_file)

if (seasonal_params_exist) {
     param_seasonal <- read.csv(seasonal_param_file)
     # Filter for cases response only
     param_seasonal <- param_seasonal[param_seasonal$response == "cases", ]
} else {
     warning("Seasonal dynamics parameter file not found. Using default values.")
}

# Create priors for each seasonality parameter
# Map R config keys (a_1_j etc.) to CSV parameter column values (a_1 etc.)
seasonality_csv_lookup <- c("a_1_j" = "a_1", "a_2_j" = "a_2", "b_1_j" = "b_1", "b_2_j" = "b_2")

for (param in names(seasonality_csv_lookup)) {
     param_name <- param  # Storage key in priors list
     csv_param  <- seasonality_csv_lookup[[param]]  # CSV column value

     priors_default$parameters_location[[param_name]] <- list(
          description = paste0("Seasonality Fourier coefficient ", param, " (cases)"),
          location = list()
     )

     # Load parameters for each location
     for (iso in j) {
          if (seasonal_params_exist) {
               # Extract mean and standard error for this parameter and location
               param_row <- param_seasonal[
                    param_seasonal$country_iso_code == iso &
                         param_seasonal$parameter == csv_param,
               ]

               if (nrow(param_row) > 0) {
                    mean_orig <- param_row$mean[1]
                    se_orig <- param_row$se[1]

                    # Adjust standard error by uncertainty factor
                    # Lower factor = higher SE = higher uncertainty
                    se_adjusted <- se_orig / sqrt(seasonality_uncertainty_factor)

                    priors_default$parameters_location[[param_name]]$location[[iso]] <- list(
                         distribution = "normal",
                         parameters = list(
                              mean = mean_orig,
                              sd = se_adjusted
                         )
                    )
               } else {
                    # Default values if not found
                    warning(paste("Seasonal parameter", param, "not found for", iso, "- using defaults"))
                    priors_default$parameters_location[[param_name]]$location[[iso]] <- list(
                         distribution = "normal",
                         parameters = list(
                              mean = 0,
                              sd = 0.5 / sqrt(seasonality_uncertainty_factor)
                         )
                    )
               }
          } else {
               # Default values if file doesn't exist
               priors_default$parameters_location[[param_name]]$location[[iso]] <- list(
                    distribution = "normal",
                    parameters = list(
                         mean = 0,
                         sd = 0.5 / sqrt(seasonality_uncertainty_factor)
                    )
               )
          }
     }
}



#---------------------------------------------------
# Initial conditions parameters (location-specific)
#---------------------------------------------------

# Initial condition proportions for each compartment
# Using biologically plausible Beta priors as defaults
# These can be replaced by more informative priors when est_initial_conditions() is called
# Priors are designed to sum to approximately 1.0 in expectation while reflecting
# typical epidemiological patterns in African cholera settings

# prop_S_initial will be estimated using constrained residual method

# prop_V1_initial - Initial proportion with one vaccine dose
priors_default$parameters_location$prop_V1_initial <- list(
     description = "Initial proportion in one-dose vaccine (V1) compartment",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$prop_V1_initial$location[[iso]] <- list(
          distribution = "beta",
          parameters = list(shape1 = 0.5, shape2 = 49.5)
     )
}

# prop_V2_initial - Initial proportion with two vaccine doses
priors_default$parameters_location$prop_V2_initial <- list(
     description = "Initial proportion in two-dose vaccine (V2) compartment",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$prop_V2_initial$location[[iso]] <- list(
          distribution = "beta",
          parameters = list(shape1 = 0.5, shape2 = 99.5)
     )
}

# v0.28.5: Override the uniform-across-countries fallback above with country-specific
# Beta priors derived from OCV campaign history (GTFCC request log). Countries with
# no pre-t0 OCV campaigns retain the fallback values. See R/est_initial_V1_V2.R.
message("Building OCV data-driven V1/V2 initial-condition priors...")
ocv_priors <- est_initial_V1_V2(PATHS = PATHS, config = config_default,
                                 date_start = date_start, verbose = FALSE)
for (iso in j) {
     if (!is.null(ocv_priors$parameters_location$prop_V1_initial$location[[iso]])) {
          priors_default$parameters_location$prop_V1_initial$location[[iso]] <-
               ocv_priors$parameters_location$prop_V1_initial$location[[iso]]
     }
     if (!is.null(ocv_priors$parameters_location$prop_V2_initial$location[[iso]])) {
          priors_default$parameters_location$prop_V2_initial$location[[iso]] <-
               ocv_priors$parameters_location$prop_V2_initial$location[[iso]]
     }
}

# prop_E_initial - Initial proportion exposed
priors_default$parameters_location$prop_E_initial <- list(
     description = "Initial proportion in exposed (E) compartment",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$prop_E_initial$location[[iso]] <- list(
          distribution = "beta",
          parameters = list(shape1 = 0.01, shape2 = 99999.99)
     )
}

# prop_I_initial - Initial proportion infected
priors_default$parameters_location$prop_I_initial <- list(
     description = "Initial proportion in infected (I) compartment",
     location = list()
)

# Load population data to calculate location-specific proportions
pop_file <- file.path(PATHS$DATA_DEMOGRAPHICS, "UN_world_population_prospects_daily.csv")
if (file.exists(pop_file)) {
     population_data <- read.csv(pop_file, stringsAsFactors = FALSE)
     population_data$date <- as.Date(population_data$date)

     for (iso in j) {
          # Get population at model start date
          pop_loc <- population_data[population_data$iso_code == iso, ]
          if (nrow(pop_loc) > 0) {
               time_diffs <- abs(as.numeric(difftime(pop_loc$date, date_start, units = "days")))
               closest_idx <- which.min(time_diffs)
               population <- pop_loc$total_population[closest_idx]

               if (!is.na(population) && population > 0) {
                    # Calculate proportion for 100 infected
                    target_infected <- 100
                    mean_prop <- target_infected / population

                    # Beta(1, b) where mean = 1/(1+b)
                    # Solve for b: mean_prop = 1/(1+b) => b = (1/mean_prop) - 1
                    shape2_val <- max(2, (1 / mean_prop) - 1)  # Ensure b >= 2 for stability

                    priors_default$parameters_location$prop_I_initial$location[[iso]] <- list(
                         distribution = "beta",
                         parameters = list(
                              shape1 = 1,
                              shape2 = shape2_val
                         )
                    )
               } else {
                    # Fallback if population invalid
                    priors_default$parameters_location$prop_I_initial$location[[iso]] <- list(
                         distribution = "beta",
                         parameters = list(
                              shape1 = 1,
                              shape2 = 9999
                         )
                    )
               }
          } else {
               # Fallback if no population data
               priors_default$parameters_location$prop_I_initial$location[[iso]] <- list(
                    distribution = "beta",
                    parameters = list(
                         shape1 = 1,
                         shape2 = 9999
                    )
               )
          }
     }
} else {
     # Fallback if population file doesn't exist
     warning("Population file not found. Using default I compartment priors.")
     for (iso in j) {
          priors_default$parameters_location$prop_I_initial$location[[iso]] <- list(
               distribution = "beta",
               parameters = list(
                    shape1 = 1,
                    shape2 = 9999
               )
          )
     }
}

# prop_R_initial - Initial proportion recovered/immune
priors_default$parameters_location$prop_R_initial <- list(
     description = "Initial proportion in recovered/immune (R) compartment",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$prop_R_initial$location[[iso]] <- list(
          distribution = "beta",
          parameters = list(shape1 = 3.5, shape2 = 14)
     )
}



# V1/V2 initial conditions are data-driven elsewhere in the pipeline since v0.22.11;
# est_initial_V1_V2() override removed. Default Beta priors above are retained.



# Update default priors with estimated initial conditions for E and I

# Define location-specific variance inflation for E/I compartments
# Higher values = more uncertainty in initial E/I estimates
# E/I have higher baseline uncertainty due to short-term dynamics
# Only includes ISO codes in MOSAIC::iso_codes_mosaic
variance_inflation_E_I <- c(
     "AGO" = 120,  # Angola: Improving surveillance
     "BDI" = 110,  # Burundi: Limited resources
     "BEN" = 110,  # Benin: Moderate surveillance
     "BFA" = 110,  # Burkina Faso: Moderate uncertainty
     "BWA" = 65,   # Botswana: Good health systems
     "CAF" = 140,  # Central African Republic: Limited data quality
     "CIV" = 105,  # Côte d'Ivoire: Moderate systems
     "CMR" = 105,  # Cameroon: Moderate data quality
     "COD" = 160,  # Democratic Republic of Congo: Large, varied conditions
     "COG" = 100,  # Congo: Moderate uncertainty
     "ERI" = 140,  # Eritrea: Limited international data
     "ETH" = 100,  # Ethiopia: Large system, variable quality
     "GAB" = 100,  # Gabon: Moderate surveillance (default)
     "GHA" = 80,   # Ghana: Good health systems
     "GIN" = 110,  # Guinea: Moderate data quality
     "GMB" = 110,  # Gambia: Small, limited data
     "GNB" = 140,  # Guinea-Bissau: Poor data quality
     "GNQ" = 100,  # Equatorial Guinea: Moderate uncertainty
     "KEN" = 40,   # Kenya: Good surveillance
     "LBR" = 100,  # Liberia: Better data quality
     "MLI" = 80,  # Mali: Some data limitations
     "MOZ" = 30,  # Mozambique: Moderate uncertainty increase
     "MRT" = 130,  # Mauritania: Limited resources
     "MWI" = 30,   # Malawi: Moderate uncertainty increase
     "NAM" = 80,   # Namibia: Good health systems (default)
     "NER" = 130,  # Niger: Limited resources
     "NGA" = 80,   # Nigeria: Large system, better data
     "RWA" = 55,   # Rwanda: Excellent health systems
     "SEN" = 95,   # Senegal: Moderate surveillance
     "SLE" = 100,  # Sierra Leone: Improved surveillance
     "SOM" = 200,  # Somalia: Very limited surveillance data
     "SSD" = 180,  # South Sudan: Conflict-affected, high uncertainty
     "SWZ" = 90,   # Eswatini: Small, moderate systems
     "TCD" = 150,  # Chad: High cholera burden, more uncertainty
     "TGO" = 120,  # Togo: Limited resources
     "TZA" = 90,   # Tanzania: Moderate data quality
     "UGA" = 80,   # Uganda: Good health systems
     "ZAF" = 40,   # South Africa: Excellent surveillance
     "ZMB" = 30,   # Zambia: Good surveillance system
     "ZWE" = 30   # Zimbabwe: Moderate uncertainty increase
)

# Use location-specific variance inflation with single function call
initial_conditions_E_I <- est_initial_E_I(
     PATHS = PATHS,
     priors = priors_default,
     config = config_default,
     n_samples = 100,
     t0 = date_start,
     lookback_days = 3,
     variance_inflation = variance_inflation_E_I,  # Named vector for location-specific values
     verbose = FALSE,
     parallel = TRUE
)

     n_updated_E_I <- 0

     # Update prop_E_initial for each location
     for (loc in names(initial_conditions_E_I$parameters_location$prop_E_initial$parameters$location)) {
          # Only update if location already exists in priors_default
          if (!is.null(priors_default$parameters_location$prop_E_initial$location[[loc]])) {
               loc_estimate <- initial_conditions_E_I$parameters_location$prop_E_initial$parameters$location[[loc]]

               if (!is.na(loc_estimate$shape1)) {
                    # Extract parameters and create proper structure
                    priors_default$parameters_location$prop_E_initial$location[[loc]] <- list(
                         distribution = "beta",
                         parameters = list(
                              shape1 = loc_estimate$shape1,
                              shape2 = loc_estimate$shape2
                         )
                    )
                    n_updated_E_I <- n_updated_E_I + 1
               }
          }
     }

     # Update prop_I_initial for each location
     for (loc in names(initial_conditions_E_I$parameters_location$prop_I_initial$parameters$location)) {
          # Only update if location already exists in priors_default
          if (!is.null(priors_default$parameters_location$prop_I_initial$location[[loc]])) {
               loc_estimate <- initial_conditions_E_I$parameters_location$prop_I_initial$parameters$location[[loc]]

               if (!is.na(loc_estimate$shape1)) {
                    # Extract parameters and create proper structure
                    priors_default$parameters_location$prop_I_initial$location[[loc]] <- list(
                         distribution = "beta",
                         parameters = list(
                              shape1 = loc_estimate$shape1,
                              shape2 = loc_estimate$shape2
                         )
                    )
               }
          }
     }


# =============================================================================
# POST-ESTIMATION ADJUSTMENT: Lower mean initial E/I for specific countries
# =============================================================================

# Countries showing systematic overestimation of initial conditions
# Apply scaling factors to reduce mean E and I while preserving relative uncertainty

adjustment_factors_E_I <- list(
     AGO = 0.01,
     BEN = 0.01,
     BFA = 0.01,
     BWA = 0.00,  # Near-zero: no active cholera at model start (uses Beta(0.01, 99999.99))
     CAF = 0.01,
     CIV = 0.01,
     CMR = 0.01,
     COD = 0.75,
     COG = 0.05,
     ERI = 0.00,  # Near-zero: very limited international data; no active cholera
     GAB = 0.00,  # Near-zero: no active cholera at model start
     GHA = 0.001,
     GIN = 0.001,
     GMB = 0.001,
     GNB = 0.001,
     GNQ = 0.00,  # Near-zero: no active cholera at model start
     KEN = 1.2,
     LBR = 0.001,
     MLI = 0.00,  # Near-zero: no active cholera at model start
     MOZ = 0.4,
     MRT = 0.00,  # Near-zero: no active cholera at model start
     MWI = 0.3,
     NAM = 0.2,
     NER = 0.1,
     NGA = 1.1,
     RWA = 0.01,
     SEN = 0.001,
     SLE = 0.001,
     SOM = 0.5,
     SSD = 0.1,
     SWZ = 0.00,  # Near-zero: no active cholera at model start
     TCD = 0.001,
     TGO = 0.1,
     TZA = 0.05,
     UGA = 0.3,
     ZAF = 0.01,
     ZMB = 0.3,
     ZWE = 0.075
)

cat("\nApplying post-estimation mean adjustments for initial E and I:\n")

for (iso in names(adjustment_factors_E_I)) {
     scaling_factor <- adjustment_factors_E_I[[iso]]

     # Adjust prop_E_initial
     if (!is.null(priors_default$parameters_location$prop_E_initial$location[[iso]])) {
          old_params_E <- priors_default$parameters_location$prop_E_initial$location[[iso]]$parameters
          old_mean_E <- old_params_E$shape1 / (old_params_E$shape1 + old_params_E$shape2)

          if (scaling_factor == 0) {
               # Zero scaling: no active E at model start.
               # Cannot compute new Beta via mean/CV rescale (0/0 = NaN).
               # Use the minimum-mass default prior instead: Beta(0.01, 99999.99)
               # gives mean ~1e-7, placing virtually all mass at 0.
               priors_default$parameters_location$prop_E_initial$location[[iso]]$parameters <- list(
                    shape1 = 0.01,
                    shape2 = 99999.99
               )
               cat(sprintf("  %s E: %.6f -> ~0 (near-zero prior, 100%% reduction)\n",
                           iso, old_mean_E))
          } else {
               # Calculate new mean (scaled down)
               new_mean_E <- old_mean_E * scaling_factor

               # Preserve relative uncertainty (CV)
               # CV = sqrt(variance) / mean for Beta distribution
               old_var_E <- (old_params_E$shape1 * old_params_E$shape2) /
                            ((old_params_E$shape1 + old_params_E$shape2)^2 *
                             (old_params_E$shape1 + old_params_E$shape2 + 1))
               old_cv_E <- sqrt(old_var_E) / old_mean_E

               # Fit new Beta with scaled mean and same CV
               new_var_E <- (new_mean_E * old_cv_E)^2

               # Beta parameters from mean and variance
               common_term_E <- new_mean_E * (1 - new_mean_E) / new_var_E - 1
               new_shape1_E <- max(0.01, new_mean_E * common_term_E)
               new_shape2_E <- max(0.01, (1 - new_mean_E) * common_term_E)

               priors_default$parameters_location$prop_E_initial$location[[iso]]$parameters <- list(
                    shape1 = new_shape1_E,
                    shape2 = new_shape2_E
               )

               cat(sprintf("  %s E: %.6f -> %.6f (%.0f%% reduction)\n",
                           iso, old_mean_E, new_mean_E, (1 - scaling_factor) * 100))
          }
     }

     # Adjust prop_I_initial
     if (!is.null(priors_default$parameters_location$prop_I_initial$location[[iso]])) {
          old_params_I <- priors_default$parameters_location$prop_I_initial$location[[iso]]$parameters
          old_mean_I <- old_params_I$shape1 / (old_params_I$shape1 + old_params_I$shape2)

          if (scaling_factor == 0) {
               # Zero scaling: no active I at model start.
               # Use the minimum-mass default prior: Beta(0.01, 99999.99) ~ mean 1e-7.
               priors_default$parameters_location$prop_I_initial$location[[iso]]$parameters <- list(
                    shape1 = 0.01,
                    shape2 = 99999.99
               )
               cat(sprintf("  %s I: %.6f -> ~0 (near-zero prior, 100%% reduction)\n",
                           iso, old_mean_I))
          } else {
               # Calculate new mean (scaled down)
               new_mean_I <- old_mean_I * scaling_factor

               # Preserve relative uncertainty (CV)
               old_var_I <- (old_params_I$shape1 * old_params_I$shape2) /
                            ((old_params_I$shape1 + old_params_I$shape2)^2 *
                             (old_params_I$shape1 + old_params_I$shape2 + 1))
               old_cv_I <- sqrt(old_var_I) / old_mean_I

               # Fit new Beta with scaled mean and same CV
               new_var_I <- (new_mean_I * old_cv_I)^2

               # Beta parameters from mean and variance
               common_term_I <- new_mean_I * (1 - new_mean_I) / new_var_I - 1
               new_shape1_I <- max(0.01, new_mean_I * common_term_I)
               new_shape2_I <- max(0.01, (1 - new_mean_I) * common_term_I)

               priors_default$parameters_location$prop_I_initial$location[[iso]]$parameters <- list(
                    shape1 = new_shape1_I,
                    shape2 = new_shape2_I
               )

               cat(sprintf("  %s I: %.6f -> %.6f (%.0f%% reduction)\n",
                           iso, old_mean_I, new_mean_I, (1 - scaling_factor) * 100))
          }
     }
}

cat("\nPost-estimation adjustments complete.\n")


# Update default priors with estimated initial conditions for R

# Define location-specific variance inflation for R compartment
# Higher values = more uncertainty, allowing for greater variation in estimates
# Only includes ISO codes in MOSAIC::iso_codes_mosaic
variance_inflation_R <- c(
     "AGO" = 3,   # Angola: Further reduced
     "BDI" = 4,   # Burundi: Further reduced
     "BEN" = 4,   # Benin: Further reduced
     "BFA" = 14,  # Burkina Faso: Moderate uncertainty
     "BWA" = 50,  # Botswana: Maximum uncertainty
     "CAF" = 24,  # Central African Republic: Increased - limited data quality
     "CIV" = 13,  # Côte d'Ivoire: Moderate systems
     "CMR" = 4,   # Cameroon: Further reduced
     "COD" = 2,   # Democratic Republic of Congo: Further decreased
     "COG" = 4,   # Congo: Further reduced
     "ERI" = 100, # Eritrea: Extreme maximum uncertainty - very limited international data
     "ETH" = 13,  # Ethiopia: Large system, variable quality
     "GAB" = 20,  # Gabon: High uncertainty
     "GHA" = 3,   # Ghana: Further reduced
     "GIN" = 4,   # Guinea: Further reduced
     "GMB" = 60,  # Gambia: Maximum uncertainty - small, limited data
     "GNB" = 0.5, # Guinea-Bissau: Increased slightly from ultra-low
     "GNQ" = 4,   # Equatorial Guinea: Further reduced
     "KEN" = 5,   # Kenya: Good surveillance
     "LBR" = 1,   # Liberia: Minimum variance inflation
     "MLI" = 14,  # Mali: Slightly decreased - data limitations
     "MOZ" = 1.5, # Mozambique: Further decreased
     "MRT" = 5,   # Mauritania: Further reduced
     "MWI" = 2,   # Malawi: Decreased further
     "NAM" = 8,   # Namibia: Good health systems (default)
     "NER" = 5,   # Niger: Further reduced
     "NGA" = 3,   # Nigeria: Further reduced
     "RWA" = 7,   # Rwanda: Excellent health systems
     "SEN" = 3,   # Senegal: Further reduced
     "SLE" = 2,   # Sierra Leone: Decreased further
     "SOM" = 2,   # Somalia: Increased uncertainty
     "SSD" = 4,   # South Sudan: Decreased further
     "SWZ" = 2,   # Eswatini: Decreased more
     "TCD" = 4,   # Chad: Decreased further
     "TGO" = 6,   # Togo: Further reduced
     "TZA" = 4,   # Tanzania: Further reduced
     "UGA" = 8,   # Uganda: Increased uncertainty
     "ZAF" = 6,   # South Africa: Excellent surveillance
     "ZMB" = 3,   # Zambia: Further reduced
     "ZWE" = 1.5  # Zimbabwe: Further decreased
)

# Use location-specific variance inflation with single function call
initial_conditions_R <- est_initial_R(
     PATHS = PATHS,
     priors = priors_default,
     config = config_default,
     n_samples = 100,
     t0 = date_start,
     disaggregate = TRUE,
     variance_inflation = variance_inflation_R,  # Named vector for location-specific values
     verbose = FALSE,
     parallel = TRUE
)


# Update initial conditions priors with priors from est_initial_R()
# The new structure matches priors_default exactly, so integration is simple
# Only update locations that already exist - do NOT create new locations

n_updated_R <- 0

# Update prop_R_initial for each location
for (loc in names(initial_conditions_R$parameters_location$prop_R_initial$parameters$location)) {

     # Only update if location already exists in priors_default
     if (!is.null(priors_default$parameters_location$prop_R_initial$location[[loc]])) {
          loc_estimate <- initial_conditions_R$parameters_location$prop_R_initial$parameters$location[[loc]]

          if (!is.na(loc_estimate$shape1)) {
               # Extract parameters and create proper structure
               priors_default$parameters_location$prop_R_initial$location[[loc]] <- list(
                    distribution = "beta",
                    parameters = list(
                         shape1 = loc_estimate$shape1,
                         shape2 = loc_estimate$shape2
                    )
               )
               n_updated_R <- n_updated_R + 1
          }
     }
}


# Update default priors with estimated initial conditions for S (constrained residual)

# Define location-specific variance inflation for S compartment
# S is calculated as constrained residual, so variance inflation is typically 0 or very small
# Only use non-zero values for locations where you want to allow more uncertainty in the residual
# Only includes ISO codes in MOSAIC::iso_codes_mosaic
variance_inflation_S <- c(
     "AGO" = 0.00,  # Angola: Standard residual calculation
     "BDI" = 0.02,  # Burundi: Slight flexibility
     "BEN" = 0.00,  # Benin: Standard residual calculation
     "BFA" = 0.02,  # Burkina Faso: Slight flexibility
     "BWA" = 0.00,  # Botswana: Good systems, no inflation needed
     "CAF" = 0.05,  # Central African Republic: Allow slight flexibility
     "CIV" = 0.00,  # Côte d'Ivoire: Standard residual calculation
     "CMR" = 0.00,  # Cameroon: Standard residual calculation
     "COD" = 0.05,  # Democratic Republic of Congo: Large varied conditions
     "COG" = 0.02,  # Congo: Slight flexibility
     "ERI" = 0.05,  # Eritrea: Limited data
     "ETH" = 0.01,  # Ethiopia: Minimal flexibility
     "GAB" = 0.00,  # Gabon: Standard residual calculation (default)
     "GHA" = 0.00,  # Ghana: Good systems, no inflation needed
     "GIN" = 0.02,  # Guinea: Slight flexibility
     "GMB" = 0.02,  # Gambia: Slight flexibility
     "GNB" = 0.04,  # Guinea-Bissau: More flexibility for poor data
     "GNQ" = 0.02,  # Equatorial Guinea: Slight flexibility
     "KEN" = 0.00,  # Kenya: Good systems, no inflation needed
     "LBR" = 0.01,  # Liberia: Minimal flexibility
     "MLI" = 0.03,  # Mali: Some flexibility for data limitations
     "MOZ" = 0.03,  # Mozambique: Some flexibility for variable quality
     "MRT" = 0.03,  # Mauritania: Some flexibility
     "MWI" = 0.00,  # Malawi: Standard residual calculation
     "NAM" = 0.00,  # Namibia: Good systems, no inflation needed (default)
     "NER" = 0.03,  # Niger: Some flexibility for limited resources
     "NGA" = 0.00,  # Nigeria: Standard residual calculation
     "RWA" = 0.00,  # Rwanda: Good systems, no inflation needed
     "SEN" = 0.00,  # Senegal: Standard residual calculation
     "SLE" = 0.01,  # Sierra Leone: Minimal flexibility
     "SOM" = 0.10,  # Somalia: Most flexibility due to very limited data
     "SSD" = 0.08,  # South Sudan: More flexibility due to poor data
     "SWZ" = 0.00,  # Eswatini: Standard residual calculation
     "TCD" = 0.05,  # Chad: Allow slight flexibility
     "TGO" = 0.02,  # Togo: Slight flexibility
     "TZA" = 0.00,  # Tanzania: Standard residual calculation
     "UGA" = 0.00,  # Uganda: Standard residual calculation
     "ZAF" = 0.00,  # South Africa: Good systems, no inflation needed
     "ZMB" = 0.00,  # Zambia: Good systems, no inflation needed
     "ZWE" = 0.03   # Zimbabwe: Some flexibility for economic challenges
)

# Use location-specific variance inflation with single function call
initial_conditions_S <- est_initial_S(
     PATHS = PATHS,
     priors = priors_default,
     config = config_default,
     n_samples = 100,
     t0 = date_start,
     variance_inflation = variance_inflation_S,  # Named vector for location-specific values
     verbose = FALSE,
     min_S_proportion = 0.001  # Default minimum S proportion
)


# Update initial conditions priors with estimates from est_initial_S()
# The new structure matches priors_default exactly, so integration is simple
# Only update locations that already exist - do NOT create new locations

n_updated_S <- 0

# Update prop_S_initial for each location
for (loc in names(initial_conditions_S$parameters_location$prop_S_initial$parameters$location)) {
     # Only update if location already exists in config
     if (loc %in% config_default$location_name) {
          loc_estimate <- initial_conditions_S$parameters_location$prop_S_initial$parameters$location[[loc]]

          if (!is.na(loc_estimate$shape1)) {
               # Create the S compartment in priors_default if it doesn't exist
               if (is.null(priors_default$parameters_location$prop_S_initial)) {
                    priors_default$parameters_location$prop_S_initial <- list(
                         description = "Initial proportion in susceptible (S) compartment from constrained residual",
                         location = list()
                    )
               }

               # Extract parameters and create proper structure
               priors_default$parameters_location$prop_S_initial$location[[loc]] <- list(
                    distribution = "beta",
                    parameters = list(
                         shape1 = loc_estimate$shape1,
                         shape2 = loc_estimate$shape2
                    )
               )
               n_updated_S <- n_updated_S + 1

          }
     }
}



# Add mu_j_baseline priors from disease mortality data
#
# mu_j_baseline is the daily per-capita cholera mortality hazard applied to
# the symptomatic compartment (Isym) in LASER. We derive its per-country
# prior by inverting the steady-state data-generating identity under the
# laser-cholera v0.13+ schema:
#
#   reported_deaths = round(disease_deaths * rho_deaths)           (engine)
#   disease_deaths  ~ Binomial(Isym, mu_jt)                        (engine)
#   reported_cases  = round(Isym * rho / chi_eff)                  (engine)
#
# Taking expectations and ratios at steady state:
#
#   CFR_observed = E[reported_deaths] / E[reported_cases]
#                = (Isym * mu_jt * rho_deaths) / (Isym * rho / chi)
#                = mu_jt * rho_deaths * chi / rho
#
# Solving for mu_j_baseline (the un-modulated component of mu_jt):
#
#   mu_j_baseline = CFR_observed * rho / (rho_deaths * chi)
#
# (sigma — the symptomatic fraction — cancels because both observation
#  pathways start from Isym, not from infections.) The empirical observed
#  CFR comes from the hierarchical GAM in est_CFR_hierarchical.R, which
#  fits binomial(deaths, cases) ~ s(year) + country effects to the
#  refreshed WHO annual surveillance data.
#
# Pre-v0.13.0 history: the engine emitted disease_deaths directly (no
# rho_deaths) and MOSAIC scored those against observed reported_deaths.
# The identity reduced to mu_j_baseline = CFR * rho / chi (~ CFR * 0.43),
# which implicitly absorbed 1/rho_deaths in the posterior. Per-country
# mu_j_baseline priors prior to v15.6 were under-scaled by a factor of
# ~rho_deaths (~2.36x). Posteriors from pre-v0.32.0 calibrations are
# interpretable as mu_baseline_v0.13 * rho_deaths under the new schema.

# Load disease mortality parameter data
mu_file <- file.path(PATHS$MODEL_INPUT, "param_mu_disease_mortality.csv")
if (file.exists(mu_file)) {
     mu_data <- read.csv(mu_file, stringsAsFactors = FALSE)

     # Calculate location-specific priors from Beta parameters
     # We'll use data from recent years (2020-2025) for more current estimates
     recent_years <- 2021:2025
     mu_recent <- mu_data[mu_data$t %in% recent_years, ]

     # Compute Beta-prior means inline so the conversion factor cannot
     # silently desync from the actual rho/chi/rho_deaths priors above.
     .beta_mean <- function(p) p$shape1 / (p$shape1 + p$shape2)
     rho_mean         <- .beta_mean(priors_default$parameters_global$rho$parameters)
     rho_deaths_mean  <- .beta_mean(priors_default$parameters_global$rho_deaths$parameters)
     chi_end_mean     <- .beta_mean(priors_default$parameters_global$chi_endemic$parameters)
     chi_epi_mean     <- .beta_mean(priors_default$parameters_global$chi_epidemic$parameters)
     chi_mean         <- 0.5 * (chi_end_mean + chi_epi_mean)
     # cfr_to_mu_adjustment ≈ 1.015 under v0.13+ (was 0.43 under pre-v0.13)
     cfr_to_mu_adjustment <- rho_mean / (rho_deaths_mean * chi_mean)
     message(sprintf(
          "  CFR->mu_j_baseline adjustment: rho/(rho_deaths*chi) = %.4f/(%.4f*%.4f) = %.4f",
          rho_mean, rho_deaths_mean, chi_mean, cfr_to_mu_adjustment
     ))

     # Initialize mu_j_baseline prior structure
     priors_default$parameters_location$mu_j_baseline <- list(
          description = "Baseline daily cholera mortality hazard applied to the symptomatic compartment (Isym) in LASER, per day. Derived from observed reported CFR via the v0.13+ steady-state identity: mu_j_baseline = reported_CFR * rho / (rho_deaths * chi).",
          location = list()
     )

     n_mu_j_added <- 0

     # Calculate statistics for each location
     for (loc in j) {
          loc_data <- mu_recent[mu_recent$j == loc, ]

          if (nrow(loc_data) > 0) {
               # Get mean values
               mean_data <- loc_data[loc_data$parameter_name == "mean", ]

               if (nrow(mean_data) > 0) {
                    # Get Beta parameters for CI calculation
                    shape1_data <- loc_data[loc_data$parameter_name == "shape1", ]
                    shape2_data <- loc_data[loc_data$parameter_name == "shape2", ]

                    # Calculate mean CFR across recent years
                    mean_cfr <- mean(mean_data$parameter_value, na.rm = TRUE)

                    # Calculate approximate CI from Beta parameters
                    ci_lower_vals <- c()
                    ci_upper_vals <- c()

                    for (year in recent_years) {
                         s1_row <- shape1_data[shape1_data$t == year, ]
                         s2_row <- shape2_data[shape2_data$t == year, ]

                         if (nrow(s1_row) > 0 && nrow(s2_row) > 0) {
                              s1 <- s1_row$parameter_value[1]
                              s2 <- s2_row$parameter_value[1]

                              if (!is.na(s1) && !is.na(s2) && s1 > 0 && s2 > 0) {
                                   ci_lower_vals <- c(ci_lower_vals, qbeta(0.025, s1, s2))
                                   ci_upper_vals <- c(ci_upper_vals, qbeta(0.975, s1, s2))
                              }
                         }
                    }

                    # Use mean of CIs across years
                    ci_lower <- mean(ci_lower_vals, na.rm = TRUE)
                    ci_upper <- mean(ci_upper_vals, na.rm = TRUE)

                    # Handle edge cases
                    if (is.na(mean_cfr) || mean_cfr <= 0) {
                         mean_cfr <- 0.02  # Default 2% CFR
                         ci_lower <- 0.005
                         ci_upper <- 0.08
                    }

                    # Clamp mean CFR to plausible range; ci_lower/ci_upper not used in fitting
                    mean_cfr <- max(min(mean_cfr, 0.4), 0.002)  # Keep mean in [0.2%, 40%]

                    # Convert observed reported CFR to mu_j_baseline mean
                    # via the v0.13+ identity (see header). adjustment is
                    # computed once before the loop from the actual prior
                    # Beta parameters.
                    mean_mu <- mean_cfr * cfr_to_mu_adjustment

                    cat(sprintf("  %s: reported_CFR=%.3f%% -> mu_j_baseline=%.6f (adjustment=%.3f)\n",
                                loc, mean_cfr * 100, mean_mu, cfr_to_mu_adjustment))

                    # Fit gamma using derived mean and a fixed target CV of 50%.
                    # The CFR data CIs are extremely tight (large Beta shape params)
                    # which would produce an over-confident prior. CV=50% (shape=4)
                    # allows the data to substantially update mu_j_baseline.
                    target_cv <- 0.5   # 50% CV → shape = 4
                    target_shape <- 1 / target_cv^2   # shape = 1/CV^2 = 4
                    target_rate  <- target_shape / mean_mu

                    priors_default$parameters_location$mu_j_baseline$location[[loc]] <- list(
                         distribution = "gamma",
                         parameters = list(
                              shape = target_shape,
                              rate  = target_rate
                         )
                    )

                    n_mu_j_added <- n_mu_j_added + 1
               }
          }

          # Add default if location not found
          if (is.null(priors_default$parameters_location$mu_j_baseline$location[[loc]])) {
               # Default: assume 0.35% reported CFR × cfr_to_mu_adjustment ≈ 0.0036 mean
               # Shape=4 (CV=50%) to allow substantial data updating.
               default_mean_mu <- 0.0035 * cfr_to_mu_adjustment
               priors_default$parameters_location$mu_j_baseline$location[[loc]] <- list(
                    distribution = "gamma",
                    parameters = list(
                         shape = 4,
                         rate  = 4 / default_mean_mu
                    )
               )
               n_mu_j_added <- n_mu_j_added + 1
          }
     }


} else {
     warning("Disease mortality parameter file not found. Using default mu_j_baseline priors.")

     # Add default gamma priors for all locations
     priors_default$parameters_location$mu_j_baseline <- list(
          description = "Baseline daily mortality rate per symptomatic infected (per day), derived from observed reported CFR: mu_j_baseline = reported_CFR × rho / chi",
          location = list()
     )

     for (loc in j) {
          # Default: 0.35% reported CFR × (0.275/0.64) ≈ 0.0015, CV=50% (shape=4)
          priors_default$parameters_location$mu_j_baseline$location[[loc]] <- list(
               distribution = "gamma",
               parameters = list(
                    shape = 4,
                    rate  = 2667   # Gives mean = 4/2667 ≈ 0.0015
               )
          )
     }
}

#----------------------------------------
# Additional IFR parameters for threshold-dependent model
#----------------------------------------

# mu_j_slope - Temporal trend in IFR per location
priors_default$parameters_location$mu_j_slope <- list(
     description = "Temporal trend in baseline IFR (proportion change over simulation period)",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$mu_j_slope$location[[iso]] <- list(
          distribution = "normal",
          parameters = list(
               mean = 0,      # No trend by default
               sd = 0.05      # ±10% change over simulation period (95% CI)
          )
     )
}

# mu_j_epidemic_factor - Proportional IFR increase during epidemics
# Gamma(shape=1, rate=2): mode=0 (no epidemic effect most probable), mean=0.5,
# 95th pct ~1.5. Encodes weakly informative belief that epidemic-mode CFR increase
# is modest but uncertain, with exponential decay away from zero.
priors_default$parameters_location$mu_j_epidemic_factor <- list(
     description = "Proportional increase in IFR during epidemic periods (e.g., 0.5 = 50% increase)",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$mu_j_epidemic_factor$location[[iso]] <- list(
          distribution = "gamma",
          parameters = list(
               shape = 1,  # Exponential: mode=0, most mass near zero
               rate  = 2   # Mean=0.5, 95th pct ~1.5
          )
     )
}

# MOZ-specific overrides dropped (v15.6): both the mu_j_baseline override
# Gamma(2, 1176) and the mu_j_epidemic_factor override Gamma(1.5, 0.5)
# were calibrated under the pre-v0.13 likelihood that compared observed
# reported_deaths against simulated disease_deaths directly (i.e. with
# the rho_deaths factor implicitly absorbed into mu_j_baseline). Under
# the v0.13+ corrected derivation, MOZ inherits the universal data-driven
# prior derived from its WHO CFR estimate (mean reported CFR ~0.43% over
# 2021-2025), yielding mu_j_baseline mean ~0.0044 (close to the previous
# override centre of 0.0017 multiplied by the ~2.36x rho_deaths
# correction). If calibration evidence warrants a country-specific
# epidemic_factor for MOZ under the new schema, re-derive at that point
# and re-introduce the override; for now MOZ uses the global default.

# epidemic_threshold - Location-specific epidemic regime activation threshold
#
# Units: dimensionless daily Isym/N point prevalence fraction.
# The LASER engine compares epidemic_threshold against
#   Isym[t - delta_reporting_cases] / N[t - delta_reporting_cases]
# at every daily tick to decide whether to apply epidemic-mode IFR and chi_epidemic.
#
# Derivation of prior means:
#   Reported weekly incidence (cases/100k/wk) is converted to Isym/N via:
#     Isym/N = (reported_per_100k / 1e5) * (chi_endemic / rho) / (7 * gamma_1)
#   (Little's Law under approximate steady state; sdlog = 0.5 captures factor-of-2
#    uncertainty from this approximation.)
#
# Data source: PATHS$DATA_PROCESSED cholera/weekly/cholera_surveillance_weekly_combined.csv
#   Median weekly reported incidence per 100k across outbreak-positive weeks (cases > 0).
#   Countries with < 10 outbreak weeks use the Zheng global reference (0.7/100k/wk),
#   the published SSA median from Zheng et al. (2022) IJID.
#
# Distribution: Lognormal(meanlog = log(prior_mean), sdlog = 0.5)

# Helper: convert Zheng weekly reported incidence to Isym/N point prevalence
convert_zheng_threshold <- function(zheng_weekly_per_100k, rho, chi, gamma_1) {
     (zheng_weekly_per_100k / 1e5) * (chi / rho) / (7 * gamma_1)
}

# Extract model parameters from config — do NOT hardcode these values
rho_val    <- config_default$rho
chi_val    <- config_default$chi_endemic
gamma1_val <- config_default$gamma_1

# Compute per-country median weekly incidence per 100k during outbreak-positive weeks.
# Cap the population join year at the maximum available year in demographics (2024)
# to handle surveillance records in 2025 that have no matching population row.
dem_max_year  <- max(dem_annual$year)

outbreak_rows <- surv_weekly[
     surv_weekly$iso_code %in% j &
     !is.na(surv_weekly$cases) &
     surv_weekly$cases > 0,
]
outbreak_rows$dem_year <- pmin(outbreak_rows$year, dem_max_year)

merged_surv <- merge(
     outbreak_rows,
     dem_annual[, c("iso_code", "year", "population")],
     by.x = c("iso_code", "dem_year"),
     by.y = c("iso_code", "year"),
     all.x = TRUE
)
merged_surv$weekly_incidence_per_100k <- merged_surv$cases / merged_surv$population * 1e5

# Per-country summary: outbreak week count and median weekly incidence per 100k
country_threshold_data <- do.call(rbind, lapply(j, function(iso) {
     rows <- merged_surv[
          merged_surv$iso_code == iso &
          !is.na(merged_surv$weekly_incidence_per_100k),
     ]
     data.frame(
          iso_code                  = iso,
          n_outbreak_weeks          = nrow(rows),
          median_incidence_per_100k = if (nrow(rows) > 0) median(rows$weekly_incidence_per_100k, na.rm = TRUE) else NA_real_,
          stringsAsFactors          = FALSE
     )
}))

# Countries with < 10 outbreak weeks fall back to the Zheng global SSA reference value.
# 0.7 per 100k per week is the published median from Zheng et al. (2022) IJID across
# SSA districts — a conservative (upper-side) choice for low-burden / data-sparse countries.
ZHENG_GLOBAL_FALLBACK_PER_100K   <- 0.7
MIN_OUTBREAK_WEEKS_FOR_DATA_PRIOR <- 10

country_threshold_data$use_fallback <- (
     country_threshold_data$n_outbreak_weeks < MIN_OUTBREAK_WEEKS_FOR_DATA_PRIOR |
     is.na(country_threshold_data$median_incidence_per_100k)
)

country_threshold_data$prior_mean <- ifelse(
     !country_threshold_data$use_fallback,
     convert_zheng_threshold(
          country_threshold_data$median_incidence_per_100k,
          rho_val, chi_val, gamma1_val
     ),
     convert_zheng_threshold(
          ZHENG_GLOBAL_FALLBACK_PER_100K,
          rho_val, chi_val, gamma1_val
     )
)

# v0.28.0: Switched from Lognormal to Truncnorm to eliminate stage-2+ posterior
# drift past 1% daily symptomatic prevalence (biologically unreachable epidemic
# regime). Lognormal was unbounded above; update_priors_from_posteriors.R's
# family-match guard now preserves the [a, b] support across all stages.
# Natural-scale CV = 0.65 approximately matches the old lognormal sdlog=0.5
# spread (CV ≈ 0.53) with a small inflation buffer.
#
# v0.28.2: Removed the absolute lower floor of 1e-6. With a floor, two
# countries with very low Zheng prior means (BEN, CIV) had a > prior_mean,
# yielding an ill-posed truncnorm whose mean lay below the lower bound
# (fit_truncnorm_from_ci() rejects mode_val <= a). Pure proportional lower
# bound (pm/10) avoids this. The 1% upper cap remains — that's the real
# safety net against epidemic-regime-unreachable drift.
EPIDEMIC_THRESHOLD_SD_REL    <- 0.65   # natural-scale CV
EPIDEMIC_THRESHOLD_UPPER_ABS <- 0.01   # global cap: 1% daily symp prevalence = severe epidemic

priors_default$parameters_location$epidemic_threshold <- list(
     description = paste0(
          "Dimensionless daily Isym/N prevalence threshold for epidemic regime activation. ",
          "Compared against Isym[t - delta_reporting_cases] / N[t - delta_reporting_cases] in LASER. ",
          "Derived from observed median weekly reported incidence per 100k (outbreak-positive weeks) ",
          "converted via Zheng formula using config rho, chi_endemic, and gamma_1. ",
          "Truncnorm(mean = prior_mean, sd = 0.65*prior_mean, ",
          "a = prior_mean/10, b = min(0.01, prior_mean*10))."
     ),
     location = list()
)

for (iso in j) {
     idx <- which(country_threshold_data$iso_code == iso)
     pm  <- country_threshold_data$prior_mean[idx]
     priors_default$parameters_location$epidemic_threshold$location[[iso]] <- list(
          distribution = "truncnorm",
          parameters   = list(
               mean = pm,
               sd   = pm * EPIDEMIC_THRESHOLD_SD_REL,
               a    = pm / 10,
               b    = min(EPIDEMIC_THRESHOLD_UPPER_ABS, pm * 10)
          )
     )
}

# Verification summary
n_data_prior <- sum(!country_threshold_data$use_fallback)
n_fallback   <- sum(country_threshold_data$use_fallback)
all_mean <- sapply(j, function(iso)
     priors_default$parameters_location$epidemic_threshold$location[[iso]]$parameters$mean)

cat(sprintf(
     "\n[epidemic_threshold priors] Data-derived: %d | Fallback: %d | Total: %d\n",
     n_data_prior, n_fallback, length(j)
))
cat(sprintf(
     "[epidemic_threshold priors] prior_mean range: [%.2e, %.2e]\n",
     min(all_mean), max(all_mean)
))
for (iso in c("ETH", "COD", "SLE", "BWA")) {
     p  <- priors_default$parameters_location$epidemic_threshold$location[[iso]]$parameters
     fb <- if (country_threshold_data$use_fallback[country_threshold_data$iso_code == iso]) " [fallback]" else ""
     cat(sprintf("  %s%s: mean=%.2e  sd=%.2e  bounds=[%.2e, %.2e]\n",
                 iso, fb, p$mean, p$sd, p$a, p$b))
}

#----------------------------------------
# Psi star calibration parameters (location-specific)
#----------------------------------------

# psi_star_a - Shape/gain parameter for logit-scale suitability calibration (location-specific)
priors_default$parameters_location$psi_star_a <- list(
     description = "Shape/gain parameter for logit calibration of NN suitability psi (a>1 sharpens peaks, a<1 flattens)",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$psi_star_a$location[[iso]] <- list(
          distribution = "truncnorm",
          parameters = list(
               mean = 1,    # Neutral value: a=1 is identity (no transformation); mode=1
               sd   = 1.0,  # 95% CI: ~[0.08, 3.03]; P(a>2)=18.9% matches old Lognormal(0,0.9) at 22.1%
               a    = 0,    # Lower bound enforces a > 0 (required by calc_psi_star)
               b    = Inf   # No upper bound
          )
     )
}

# psi_star_b - Scale/offset parameter for logit-scale suitability calibration (location-specific)
priors_default$parameters_location$psi_star_b <- list(
     description = "Scale/offset parameter for logit calibration of NN suitability psi (shifts baseline up/down)",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$psi_star_b$location[[iso]] <- list(
          distribution = "normal",
          parameters = list(
               mean = 0,        # Centered at no offset
               sd = 2.5         # 95% CI: [-4.90, 4.90]; old posterior lower tail (-4.65) was below sd=2.0 95th
          )
     )
}

# MOZ-specific override: posterior from calibration_test_19 consistently shifted positive
# (posterior mean=+0.40 from prior mean=0), indicating the raw NN suitability scores are
# systematically slightly underestimated for Mozambique. Tighten sd slightly (2.5->2.0)
# since the 2.5 prior was wider than evidence supports for this location.
priors_default$parameters_location$psi_star_b$location[["MOZ"]] <- list(
     distribution = "normal",
     parameters = list(
          mean = 0.4,   # Evidence-based shift: posterior mean from calibration_test_19
          sd = 2.0      # Slightly tighter than global; 95% CI: [-3.52, 4.32]
     )
)

# psi_star_z - Smoothing weight parameter for causal EWMA (location-specific)
priors_default$parameters_location$psi_star_z <- list(
     description = "Smoothing weight for causal EWMA of calibrated suitability (z=1: no smoothing, z<1: smoothing)",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$psi_star_z$location[[iso]] <- list(
          distribution = "beta",
          parameters = list(
               shape1 = 2,      # Beta(2,1): mode=1 (null: no smoothing); mean=0.667; monotonically
               shape2 = 1       # decreasing toward z=0. Encodes null assumption that z=1 is the
                                # identity (no EWMA smoothing). Beta(1,1) was equally permissive of
                                # z=0 (max smoothing) and z=1 (null), producing U-shaped posteriors
                                # (shape1<1, shape2<1) in staged calibration when the likelihood is
                                # flat across z — amplifying bimodality into the final ensemble.
          )
     )
}

# psi_star_k - Time offset parameter for suitability calibration (location-specific)
# Biological rationale: k > 0 delays the suitability signal (epidemic follows/lags the peak),
# k < 0 advances it (epidemic precedes the peak).
# Both directions are permitted: in some settings epidemics may precede a broad suitability
# peak (e.g. early season explosive outbreaks) or lag it (slow accumulation in low-WASH areas).
# Bounds [-90, 90] cover the full plausible range; centred at 0 with sd=25 keeps most mass
# within ±50 days while allowing the data to identify the direction.
priors_default$parameters_location$psi_star_k <- list(
     description = "Time offset in days for suitability calibration (k>0: epidemic lags suitability peak; k<0: epidemic precedes suitability peak). Bounded to [-90, 90]: both lag and advance are permitted.",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$psi_star_k$location[[iso]] <- list(
          distribution = "truncnorm",
          parameters = list(
               mean = 0,        # Centered at no offset; data identifies direction
               sd = 25,         # Most mass within ±50 days
               a = -90,         # Lower bound: up to 90 days advance
               b = 90           # Upper bound: up to 90 days lag
          )
     )
}

# MOZ-specific override: posterior from calibration_test_19 shifted to mean ≈ -4.5 days,
# indicating Mozambique epidemics slightly precede the suitability peak (epidemic leads
# suitability by ~5 days). Re-centre the prior at -5 and tighten sd (25->20) to
# concentrate mass on the evidence-supported direction while preserving full [-90,90] range.
priors_default$parameters_location$psi_star_k$location[["MOZ"]] <- list(
     distribution = "truncnorm",
     parameters = list(
          mean = -5,   # Evidence-based: epidemic slightly precedes suitability peak in MOZ
          sd = 20,     # Slightly tighter than global; most mass within ±40 days
          a = -90,     # Lower bound unchanged
          b = 90       # Upper bound unchanged
     )
)


# Save to file and add to MOSAIC R package

fp <- file.path(PATHS$ROOT, 'MOSAIC-pkg/inst/extdata/priors_default.json')

# save to file. digits = NA preserves full numerical precision; the default
# digits = 4 rounds small bounds (e.g. 2.277e-05) to 0, silently corrupting
# truncnorm epidemic_threshold entries and any other small-scale priors
# (bug discovered in v0.28.7 plot_model_distributions debugging).
jsonlite::write_json(priors_default, fp, pretty = TRUE, auto_unbox = TRUE, digits = NA)

# Read back to verify
tmp_priors <- jsonlite::fromJSON(fp, simplifyVector = FALSE)

identical(priors_default$parameters_global$alpha_1, tmp_priors$parameters_global$alpha_1)

# Note: R data object is saved as priors_default to match config_default naming convention
usethis::use_data(priors_default, overwrite = TRUE)

