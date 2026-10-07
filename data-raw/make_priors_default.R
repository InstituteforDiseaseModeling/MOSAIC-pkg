library(MOSAIC)
library(jsonlite)

# make_priors_default.R - Generate default prior distributions for MOSAIC model parameters
# Canonical source for priors_default.rda (v0.28.4: retired duplicate make_priors.R).

# Set up paths
MOSAIC::set_root_directory("~/MOSAIC")
PATHS <- MOSAIC::get_paths()

# See the matching note in make_config_default.R: MODEL_INPUT/MODEL_OUTPUT are
# the only two get_paths() entries inside MOSAIC-pkg, so under a git worktree
# they point back at the canonical checkout. This script reads
# cfr_hierarchical_estimates.csv (the mu_jt prior) from MODEL_INPUT, so
# leaving it unpatched makes a worktree build silently source another tree's
# CFR artifacts. Re-point exactly those two at the tree we are running in.
.pkg_here <- normalizePath(getwd(), mustWork = TRUE)
if (!file.exists(file.path(.pkg_here, "DESCRIPTION"))) {
     stop("Run this script from the MOSAIC-pkg root: no DESCRIPTION in ", .pkg_here)
}
PATHS$MODEL_INPUT  <- file.path(.pkg_here, "model", "input")
PATHS$MODEL_OUTPUT <- file.path(.pkg_here, "model", "output")

# Load config_default to get the global fit-window start. The build start date can
# be overridden via the MOSAIC_BUILD_DATE_START env var so a rebuild at a new start
# flows the SAME start date into BOTH this script and make_config_default.R;
# otherwise it falls back to the installed config_default.
config_default <- MOSAIC::config_default
.env_ds    <- Sys.getenv("MOSAIC_BUILD_DATE_START", "")
date_start <- if (nzchar(.env_ds)) as.Date(.env_ds) else as.Date(config_default$date_start)

# Build-order guard, the counterpart of make_config_default.R's check of
# priors_default$metadata$build_date_start. With the env var unset that script
# uses its own default literal while this one uses the installed config_default,
# and the two differ exactly when the default window has just moved and the
# installed object predates it: this script would build priors for the old window
# and only the config step would notice. Stop before any estimation instead.
if (!nzchar(.env_ds)) {
     .cfg_line <- grep("^date_start <- if \\(nzchar\\(\\.env_ds\\)\\)",
                       readLines(file.path(.pkg_here, "data-raw", "make_config_default.R")),
                       value = TRUE)
     .cfg_default <- sub('^.*else as\\.Date\\("([0-9]{4}-[0-9]{2}-[0-9]{2})"\\)\\s*$', "\\1", .cfg_line)
     if (length(.cfg_default) != 1L || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", .cfg_default)) {
          stop("Cannot read the default date_start of data-raw/make_config_default.R to check ",
               "it against the installed config_default; export MOSAIC_BUILD_DATE_START.")
     }
     if (format(date_start) != .cfg_default) {
          stop(sprintf(paste0(
               "BUILD-ORDER DESYNC: MOSAIC_BUILD_DATE_START is unset, so this script would build ",
               "priors for the installed config_default's date_start = %s, but ",
               "make_config_default.R defaults to %s. Export MOSAIC_BUILD_DATE_START=%s for ",
               "every step of the rebuild (priors, install, config)."),
               format(date_start), .cfg_default, .cfg_default))
     }
}

# Initial-condition epoch: ic_t0 = date_start. The initial conditions are the
# model state on the first simulated day, so every est_initial_*() call below
# is anchored there. (Up to v0.100.0 ic_t0 was the month with the most
# active-case countries in [date_start, date_start + 12 months] -- 2023-02-01
# for the 2023 build -- which seeded the 2023-01-01 simulation from a window a
# month later: TZA (~25 cases/day in early January), UGA, AGO and BEN reported
# cases around 1 January but none in the 3 days before 1 February and got the
# near-zero template, so TZA failed to ignite in ~90% of draws.) The E/I window
# straddles date_start (see est_initial_E_I() below), so that an outbreak under
# way on the first day but reported only after it still informs E and I. (The
# five countries it was added for in v17.0 -- TZA, ZAF, ZWE, SSD, UGA -- drew
# their window's cases from AI Fourier reconstructions starting 2 January
# 2023, which the v0.101.0 surveillance reconciliation removed; from v17.1
# they are quiet starts.)

j <- MOSAIC::iso_codes_mosaic

#----------------------------------------
# Load surveillance and demographics data for epidemic_threshold priors
#----------------------------------------

surv_weekly <- read.csv(
     file.path(PATHS$DATA_PROCESSED, "cholera/weekly/cholera_surveillance_weekly_combined.csv"),
     stringsAsFactors = FALSE
)

ic_t0 <- date_start
message(sprintf("Initial-condition epoch ic_t0 = date_start = %s", format(ic_t0)))

# Annual population by country-year from the maintained UN WPP output of
# process_UN_demographics_data() (1967-2100, estimates + medium projection).
# Replaces demographics_mosaic_countries_2000_2024_annual.csv, which no R/
# function produces; its `population` column is identical to total_population
# for every country-year in 2000-2024.
dem_annual <- read.csv(
     file.path(PATHS$DATA_PROCESSED, "demographics/UN_world_population_prospects_annual.csv"),
     stringsAsFactors = FALSE
)
dem_annual$population <- dem_annual$total_population

priors_default <- list(
     metadata = list(
          version = "18.1",
          date = Sys.Date(),
          # Build-time fit-window start these priors were derived against. Recorded so
          # make_config_default.R can assert its own date_start matches the priors it
          # sources from (cross-artifact desync guard: catches e.g. a 2015-priors /
          # 2023-config mismatch). The initial conditions are estimated at it (ic_t0).
          build_date_start = as.character(date_start),
          description = "Default informative prior distributions for MOSAIC model parameters. v18.1 (2026-10-07): REBUILD ON THE REFRESHED AI-ENHANCED SURVEILLANCE (MOSAIC v1.1.0): MOSAIC-data beb95f5, the combined surveillance regenerated by MOSAIC 1.0.2's process_cholera_surveillance_data() from the October 2026 full rerun of ai-cholera-data-mining (dec875e5): AI inferred zeros kept at their confidence weight except within 28 days of a positive count of the same country, and the curations CIV-2025-first-report, CIV-2025-W53-restatement and BEN-2023-W52-restatement. build_date_start (2018-01-01), est_initial_E_I() v1.3.0 and every other input are those of v18.0. Built in two passes against config v7.1 and reproduced byte for byte by a third. CHANGED vs v18.0: only the initial-condition priors (prop_E/prop_I/prop_S_initial) of ETH, MLI and RWA; every other prior is identical. Expected initial E + I: ETH 1,036 -> 171 (its window still falls back to the AI country reconstruction of December 2017, which the rerun restates ~6x lower, ~47 instead of ~290 cases/week), RWA 250 -> 5 (its window now holds AI observed counts of 1-2 cases/week instead of only a regional reconstruction, so it is estimated rather than seeded), MLI 4 -> 409 (AI documented zeros fill its window and it now counts as a seeded quiet start). Quiet start: {quiet_start_seeded}. v18.0 (2026-10-03): REBUILD AT A 2018-01-01 START (MOSAIC v0.103.0), on the inputs of v17.1 (MOSAIC-data 922ef89 surveillance, ees-cholera-mapping 780eb54) with est_initial_E_I() v1.3.0. build_date_start and ic_t0 are 2018-01-01, so every initial-condition prior is re-estimated there; E/I from the 28-day window 2017-12-18 to 2018-01-14. Built in two passes: est_initial_V1_V2() divides by the installed config_default's N_j_initial, so a first pass against config v6.2 divided by 2023 populations; these priors are the second pass, against config v7.0 (prop_V1 x1.11-1.19 in 13 locations and prop_V2 in NGA and ZMB against the first pass), which a third pass reproduced byte for byte. CHANGED vs v17.1: prop_E/prop_I_initial in 17 locations. Quiet start: BDI BEN BFA CAF CIV CMR GHA GIN NAM NER RWA SSD SWZ TCD TGO UGA ZAF ZWE. BEN, CMR and GIN join (no cases in their 2018 window, cases later), BDI joins because its window holds only imputed rows beside observed zeros, and COG and TZA leave (their 2018 windows hold cases). est_initial_E_I() v1.3.0 sets imputed (tier-3, AI Fourier) rows aside where the window holds an observed or reconstructed count, reads a window without one from its country-level reconstructions (metadata$imputed_window_fallback: ETH, whose December 2017 reconstruction ran ~300 cases/week after an observed 61), never counts regional reconstructions, and counts only observed or reconstructed later cases in the quiet-start test. Expected initial E + I, N x (E[prop_E] + E[prop_I]) at the UN WPP population of each start: ETH 149 -> 1,022 (imputed-window fallback), KEN 1,676 -> 666 and ZMB 36 -> 1,105 (window read without its imputed rows), BDI 54 -> 234 (seeded), RWA 247 (seeded: its window holds only a regional reconstruction), MLI 4 (near-zero template: its later cases are all imputed), NGA 297 -> 1,126, COD 2,580 -> 3,632, MWI 7,956 -> 225, MOZ 1,374 -> 134, SOM 1,037 -> 271, TZA 1,313 -> 633, AGO 4 -> 189. prop_R_initial in all 40 locations: median ratio 1.77 (IQR 1.47-1.87), mostly five fewer years of waning on older immunity, lower where 2018-2022 outbreaks were large (NGA 0.53, MWI 0.78, NER 0.84, CMR 0.85). prop_V1_initial in 16 and prop_V2_initial in 13 locations, from the campaigns before 2018-01-01: 19.8M -> 11.8M people in V1 and 13.4M -> 5.0M in V2 at each start's population; 27 locations carry the V1 template Beta(0.5, 49.5) and 38 the V2 template Beta(0.5, 99.5) (24 and 27 in v17.1). prop_S_initial moves through the residual everywhere. metadata$imputed_window_fallback is new. UNCHANGED vs v17.1: every global parameter and every other location parameter (epidemic_threshold, the mu_jt block, a_1/a_2/b_1/b_2, tau_i, beta_j0_tot, psi_star_*, alpha_1 and the rest): their inputs and estimators are those of v17.1 and none reads the start date. Corrected in the v17.1 entry below: the seasonal SDs fell with the fits' standard errors, not with the envelope scaling. v17.1 (2026-10-01): REBUILD (MOSAIC v0.101.0) on the corrected surveillance of MOSAIC-data 04a6d0f (WHO multi-week reports spread, cross-source double counting removed, imputed rows only filling the gap to the WHO account of the year, curated windows including the shaped, report-dated ZAF 2023 outbreak, Monday-Sunday WHO weeks) and on seasonal dynamics re-estimated on it; the estimators and this builder's logic are unchanged. CHANGED vs v17.0: a_1/a_2/b_1/b_2 in 16 locations, from param_seasonal_dynamics.csv re-estimated with unchanged arguments on MOSAIC-data 93596a1 and again on 04a6d0f: ZAF max |delta mean| 1.41 (its envelope 1 + f(t) peaked on 31 Aug at 2.59, the week-35 booking of 1,390 cases; it now peaks on 28 May at 2.51, the Hammanskraal outbreak), NAM 0.13 and CIV 0.11 (their curated 2025 windows), the other 13 <= 0.03; the SDs fall with the fits' standard errors (ZAF 0.30 -> 0.14, CIV 0.22 -> 0.14; ZAF's standard error before the envelope scaling fell 0.52 -> 0.21 while the scaling rose 0.41 -> 0.46). Seasonal prior draws with min(1 + f(t)) <= 0: 32% (33% at v17.0). prop_E/prop_I_initial: SSD, TZA, UGA, ZAF and ZWE become quiet starts with the Beta(1, 1e5) seeding prior, because the cases in their 28-day window were AI Fourier reconstructions, which the reconciliation removed (quiet start: BFA CAF CIV COG GHA NAM NER RWA SSD SWZ TCD TGO TZA UGA ZAF ZWE); expected initial E + I, N x (E[prop_E] + E[prop_I]): SSD 45 -> 230, TZA 231 -> 1332, UGA 4 -> 973, ZAF 93 -> 1264, ZWE 551 -> 327. Window-based priors move with their window's cases in AGO, BDI, BEN, COD, ETH, KEN, MWI and ZMB: expected E + I x0.06 (ZMB, AI rows removed) to x1.34 (KEN, a WHO report spread into the window). prop_R_initial moves by at most 0.51% (ZAF) in the 16 seasonal locations (est_initial_R() spreads each year's cases with the seasonal priors) and prop_S_initial by at most 0.47% (SSD) in the 21 locations whose E, I or R prior moved. epidemic_threshold: 18 locations move with the outbreak weeks of the corrected surveillance; ZAF leaves the Zheng fallback (7 -> 26 outbreak weeks), mean 1.18e-5 -> 1.07e-7 (x0.009); the other 17 x0.61 (GHA) to x1.28 (RWA). UNCHANGED vs v17.0: every global parameter, the mu_jt block, prop_V1/V2_initial and every other location parameter. v17.0 (2026-09-30): DEEP-REVIEW REBUILD (MOSAIC v0.100.1) on the v0.100.0 estimators and refreshed data. CHANGED vs v16.1: sigma Beta(4.30, 13.51) -> Beta(3.75, 7.12) (mean 0.24 -> 0.35), read from param_sigma_prop_symptomatic.csv (est_symptomatic_prop()) instead of hardcoded; the old value was the same fit on a table whose Harris et al. 2008 row was mistranscribed as 0.184 (the paper reports 127 of 202 infections symptomatic, 0.629). chi_endemic Beta(5.43, 5.01) -> Beta(5.56, 5.10) and chi_epidemic Beta(4.79, 1.53) -> Beta(4.97, 1.58), refit at the published 2.5% quantile (was 0.0275); means unchanged. zeta_ratio: the direct-channel lognormal is truncated below at lower = 1 so zeta_2 <= zeta_1 (median ~75 -> ~185; meanlog/sdlog unchanged). alpha_2 Beta(7.5, 7.5) -> Beta(6.92, 6.92), phi_1 Beta(91.84, 25.49) -> Beta(84.37, 23.48), phi_2 Beta(206.96, 56.53) -> Beta(196.47, 53.70), p_beta Beta(7.03, 13.24) -> Beta(5.48, 10.10) and every theta_j: the same centres and requested CIs, now hit exactly by the corrected fit_beta_from_ci() (the old shapes were too narrow); theta_j ERI 0.708 -> 0.720 and BWA +0.001 from the corrected WASH imputation. a_1/a_2/b_1/b_2: from the regenerated param_seasonal_dynamics.csv, whose case fits are scaled so min(1 + f(t)) >= 0.1 AT THE PRIOR MEANS (29 of 40 v16.1 means gave a negative envelope); under independent draws from these priors the envelope still dips below zero in ~33% of draws (the engine clamps the human force of infection at zero there), reported by the builder rather than removed by shrinking the SDs. Initial conditions are estimated at date_start (ic_t0 = 2023-01-01; v16.1 used 2023-02-01, a month after the simulation start). prop_E/prop_I_initial from the revised est_initial_E_I(): the model's rho / chi_endemic / delta_reporting_cases priors, zero draws kept, mean-anchored Beta with uniform variance inflation 10, a 28-day surveillance window straddling date_start, 1000 seeded draws; a quiet-start location -- one that reports cases later in the config window (after the window, up to date_stop) but either none in the window or too few for one expected initial infection (N x (E[prop_E] + E[prop_I]) < 1 under the window-based Beta) -- gets the weak seeding prior Beta(1, 1e5) for E and for I (mean 1e-5, the v16.1 scale), standing in for undetected circulation or importation (quiet start: BFA CAF CIV COG GHA NAM NER RWA SWZ TCD TGO; listed in metadata$quiet_start_seeded); a location with no cases anywhere up to date_stop keeps the near-zero Beta(0.01, 99999.99), and window-based priors implying >= 1 expected initial infection are kept. Every location except the silent ones starts with E + I >= 1 in >= 98% of sample_parameters() draws (lowest UGA and AGO, whose data-based priors imply ~4-6 expected initial infections); TZA, AGO, BEN and UGA were 0.09-0.13 in v16.1. prop_R_initial from est_initial_R() with the model's rho / chi priors and a mean-keeping refit floored at shape1 >= 1: means fall a median ~25x vs v16.1. prop_V1/V2_initial are effective (phi-weighted) immunisations, V2 ~0.64x. prop_S_initial keeps its Monte Carlo SD (inert in sampling). The initial-condition Monte Carlo is seeded (per-location derived seeds), so a rebuild is byte-reproducible. epidemic_threshold: 15 of 40 centres move by up to 15% (UN WPP population, refreshed surveillance). UNCHANGED in value vs v16.1: tau_i (overland lognormal), mobility_gamma/mobility_omega (blend-mode Gammas), kappa, zeta_1, zeta_2, gamma_1/2, iota, epsilon, rho, rho_deaths, delta_reporting_cases, decay_*, alpha_1, beta_j0_tot, psi_star_*, and the mu_jt block (the CFR GAM input is identical). kappa's description now names the per-capita dose W/N. v16.1 (2026-09-29): the mu_jt block is rebuilt from the revised est_CFR_hierarchical() (MOSAIC v0.97.0: in-progress calendar years excluded; each country's trend held flat after its own last WHO-annual year), and sd_product is re-described: it is the residual error of the GAM centre against the observed reported CFR in the calibration window (sd(log) 0.19-0.32 over 15-17 countries with >= 50 deaths, 2023-26), not a WHO-annual vs weekly product mismatch (the two products agree to sd(log) 0.03). Value unchanged at 0.3. No other prior changes. v16.0 (2026-09-28): CFR v2.1 MORTALITY MODEL (MOSAIC v0.96.0). NEW top-level mu_jt block: the prior for the reported case fatality ratio, which the engine reads as config$mu_jt and run_MOSAIC() integrates out per simulated path (a location offset and one deviation per calendar year on the logit scale, solved by a Laplace step). Per location and year it carries the est_CFR_hierarchical() WHO-annual GAM centre (logit_mean) and its SE (logit_se); globally sd_year (the GAM country-year SD, 0.70), sd_product (0.3, the WHO-annual vs weekly-surveillance product mismatch) and tau. REMOVED: CFR_target, mu_j_epidemic_factor (location) and delta_reporting_deaths (global), which parameterised the retired daily-hazard mortality model and its separate post-mortem lag; deaths are now drawn at symptom onset and reported on the case lag. rho_deaths stays pinned at 0.42 (it cancels exactly from reported deaths). v15.20 (2026-09-28): mu_j_slope REMOVED (CFR restructure R3). The per-location N(0, 0.05) prior on a linear-in-time trend in baseline IFR is deleted, together with the engine term it fed -- run_simulation() no longer multiplies the mortality hazard by (1 + mu_j_slope * tick/nticks) (R/sim_components.R), and mu_jt is now TWO multiplicative components (per-patch baseline x epidemic escalation) rather than three. NO OTHER PRIOR MOVES. Four independent lines of evidence, all pointing the same way. (1) NOT ESTIMABLE: the information-weighted variance of the regressor tau_t = t/nticks is only 0.0387 at ETH (vs 1/12 = 0.0833 for a uniform spread) because deaths concentrate in a narrow band, giving se(slope) = 0.417-0.491 against a prior SD of 0.05 -- a data/prior SE ratio of 8.3-9.8 and a variance reduction of 1.0-1.2%. Posterior shrinkage 0.5 would need about 74,300 deaths in one country; the largest series shipped is COD at 4,139 and all 40 locations pooled hold ~15,600, so even a single trend shared across Sub-Saharan Africa is 5x short. Measured posterior/prior SD on a 50,000-draw reference run is 0.969: the posterior IS the prior. The minimum trend detectable at 80% power is 117% over the window, against a prior that allows +/-10%. (2) NOT IN THE DATA: 3 of 21 countries show a significant weekly CFR trend and the signs are MIXED; the between-country spread of the implied trend is 7.3x wider than the prior; and in COD the weekly and annual trends have OPPOSITE signs, which is outbreak composition rather than lethality. (3) NOT IN THE LITERATURE: no secular trend in cholera CFR is documented. WHO's own Yemen-excluded global series is flat (1.7%, 1.4%, 1.5% for 2017, 2019, 2020); the headline global swing (1.8 -> 0.5 -> 0.2 -> 1.9 -> 0.5 -> 1.1%) is one country's denominator moving. (4) DOUBLE-COUNTED: est_CFR_hierarchical() already fits an s(year) smooth, so the temporal component of country CFR is inside CFR_target and a second free trend on top of it is not identified even in principle. PINNING VERIFIED INERT BEFORE REMOVAL (Stage 1, 5 national medoids ETH/COD/MOZ/KEN/NGA x 24 parameter draws x 8 seeds/arm, arms paired at the PARAMETER level so only mu_j_slope differs): total reported deaths with the drawn slope vs the slope forced to 0 have geometric-mean ratio 1.0034 (95% CI [0.9991, 1.0077], sd(log) 0.0239), which is SMALLER than the pure Monte-Carlo noise floor of the same comparison (sd(log) 0.0331 between two disjoint seed sets at identical parameters); cases are untouched (pooled ratio 1.0000). The term explains 0.05% of the across-draw deaths-level variance. CORRECTION TO THE EVIDENCE BASE: the pre-registered claim that the parameter 'injects +/-30% of uncontrolled deaths level per draw' is NOT reproduced -- the sweep behind it (deaths bias 1.18 -> 1.95) ran the slope out to about +/-1.2, which is 24 prior SD. Measured inside the actual N(0, 0.05) 95% interval (+/-0.098) total deaths move only [0.952, 1.040] at COD and [0.961, 1.039] at ETH, i.e. +/-4%, and log(deaths ratio) = 0.403 * slope (the death-weighted mean t_factor is 0.40). So removal is justified as removing DEAD WEIGHT -- 40 sampled dimensions carrying ~0.01 nats -- and NOT as removing a large uncontrolled level injection; the identifiability report's prediction that the deaths-bias IQR would narrow by >=20% is falsified (measured -2.6% to +1.6%, i.e. noise). SHIPPED ARTIFACTS ARE BIT-IDENTICAL: config_default has carried mu_j_slope = 0 for every location since the field existed, so (1 + 0 * t) = 1 exactly and deleting the factor changes no simulation output on any shipped or fixture config (verified over 26 scenarios x 722 result-channel digests in both engine modes, rng and replay, at 40 and 1 patches, including the 1,398-tick full-length oracle fixture). The parameter is NOT re-added to any config: make_simulation_config() keeps a deprecated, ignored mu_j_slope formal purely so pre-v0.95.0 configs on disk still replay. Also in this build: all non-ASCII characters removed from the shipped strings (em-dash, en-dash, section sign) to clear the R CMD check 'data for non-ASCII characters' warning. v15.19 (2026-09-23): TWO PARAMETERS PINNED (CFR restructure R2). No prior DISTRIBUTION changes value or shape and no per-country number moves; what changes is that rho_deaths and delta_reporting_deaths are no longer DRAWN -- sample_rho_deaths and sample_delta_reporting_deaths now default FALSE in sample_parameters() and run_MOSAIC(), so calibration holds them at config_default's 0.42 and 5 respectively. Both priors are RETAINED here as the literature record, as the source of those two point values, and for explicit sensitivity runs. (a) rho_deaths PINNED at 0.42 because it CANCELS IDENTICALLY under B2: sample_parameters() derives mu_j_baseline = CFR_target * (1-exp(-gamma_1)) * rho / (rho_deaths * chi_epidemic), so mu is proportional to 1/rho_deaths, while the engine draws reported_deaths ~ Binom(disease_deaths, rho_deaths) with disease_deaths driven by mu_jt (R/sim_components.R:198-206) -- the two occurrences cancel in the deaths mean, the Fisher information is exactly zero to O(mu^2), and the parameter beats a 400x resampling null in 1 of 27 production countries with posterior/prior variance ratio 1.09 (below chance). This RETIRES the v15.7 rationale, which is pre-B2 and wrong: it said the deaths likelihood identifies the PRODUCT mu_j_baseline * rho_deaths so a narrow prior pins the sloppy direction, but the B2 derivation had already removed that direction -- there is no product left to pin. Falsifiable gate executed before shipping (4 arms x 48 seeds x 5 national medoids, ETH/SOM/ZWE/COD/NGA): pinning moves realized total deaths 0.3-1.3% (|t| <= 2.4) while the two half-changes move them 3.6-27% (|t| 21-43) and their ratios multiply to 1.00 +/- 0.01 -- the cancellation demonstrated as the product of two individually 20-40 sigma effects. (b) delta_reporting_deaths PINNED at 5 days, the rounded truncated median (4.60) and mean (4.86) of the retained TruncNorm(4,3,[1,14]) and the midpoint of the 3-7 day IDSR death-to-report window (Routh 2017, Bwire 2013) anchoring it. No observational anchor exists to calibrate against: cholera deaths and cases are reported on the SAME WHO bulletin row, the weekly cross-correlation of the two observed series peaks at lag 0 in 11 of 15 countries, and the posterior beats a resampling null in 6 of 27. Because this one is a LAG and therefore not inert, it was gated on the SHAPE terms: sampled-vs-pinned moves WIS by -1.2% to +1.0% in 4 of 5 medoids (median -0.02%) with deaths peak-timing error unchanged. FLAGGED, not acted on: at every value in the prior support the simulated deaths series runs 1-28 days LATE (CCF-optimal lag negative in 4 of 4 refined countries), so the fit prefers a delay at or below the prior's lower bound; that is the ~19-day structural infection-to-reported-death dwell deficit and must not be absorbed by an administrative reporting lag. Neither pin is RECALIBRATION-GATED for correctness (a) or level (b), but both change the sampled dimension count, so posterior artefacts from earlier runs carry a drawn rho_deaths/delta column where new runs carry a constant. v15.18 (2026-06-26): mu_j_epidemic_factor per-location prior RE-SHAPED Gamma(shape=1,rate=2) -> Gamma(shape=3,rate=6) to thin the heavy exponential right tail (old p95 1.50 / p99 2.30 -> new p95 ~1.05 / p99 ~1.40) while KEEPING the literature-anchored +50% mean outbreak-CFR escalation (mean stays 0.5; mode moves 0->0.33). RATIONALE: the parameter is statistically UNIDENTIFIED (calibration leaves posterior ~ prior), so the old prior's tail mass on near-catastrophic IFR multipliers (up to ~3.3x baseline) propagated undamped into the best-subset/medoid and over-predicted deaths (MOZ medoid drew 1.68 ~ p93 of the old prior; ETH 1.02 ~ p87). shape=3 puts the mode above zero (biologically honest for an epidemic-FLAGGED tick) and leaves the >1.5 catastrophic tail to the identified chi_epidemic PPV switch + per-country mu_j_baseline. Joint statistician (sizing: shape>=2, thin tail, safe because unidentified) + disease-modeler (center 0.5, mode>0, p99<=1.4) recommendation from the 5-country NMME deaths-bias diagnosis. CALIBRATION-AFFECTING (deaths channel); RECALIBRATION-GATED (effect appears only on a fresh calibration). NOTE: this is a SECONDARY deaths-bias handle (dominant driver is CFR_target posterior drift, handled separately). v15.17 (2026-06-26): rebuild combining (a) a DOC reconciliation and (b) a data-driven initial-condition refresh. (a) DOC: corrected five stale comment/string spots that quoted the continuous-time `gamma_1`/chi-blend approximation of the B2 mu_j_baseline derivation; the shipped sample-time code (sample_parameters.R B2.1) uses the engine-correct discrete form `CFR_target * (1 - exp(-gamma_1)) * rho / (rho_deaths * chi_epidemic)`. (b) DATA REFRESH: re-derived against current surveillance -- the initial-condition compartment priors (prop_S_initial / prop_R_initial across all 40 ISOs, prop_E_initial / prop_I_initial across the 12 active-case ISOs) were re-seeded at ic_t0 from updated case data, shifting their Beta shape parameters (208 scalar updates; max relative shape shift ~4x on small-mean compartments, IC means remain within plausible compartment ranges). No global parameters, derivation logic, or sampling behavior changed; CALIBRATION-AFFECTING via IC seeding only. v15.16 (2026-06-24): alpha_1 (within-metapop population-mixing exponent) RELOCATED from a single global scalar prior to a PER-LOCATION prior -- parameters_location$alpha_1$location[[iso]] now carries a SHARED informative marginal Beta(shape1=28.4, shape2=71.6) for EVERY ISO (mean 0.284, sd ~0.045, 95% CI ~[0.20,0.38], cleanly within the engine (0,1] range invariant). This lets the per-location sampler draw a length-nL alpha_1 vector that the dual-mode laser-cholera engine applies elementwise per patch in the FOI (humantohuman.py power(effective_i, alpha_1)), while the tight shared prior emulates hierarchical shrinkage (MOSAIC's independent-per-ISO sampler cannot express a true hierarchy) and starves the alpha_1<->beta_j0_tot degeneracy. alpha_2 (frequency-driven transmission degree) is DELIBERATELY KEPT as a single GLOBAL SCALAR prior (weakly identified given psi absorbs environmental signal; user decision). Scalar-alpha_1 configs remain valid (engine broadcasts a scalar to all patches), so national nL=1 and legacy configs are unaffected. v15.15 (2026-06-23): B2 DYNAMIC per-country mu_j_baseline <-> sampled gamma_1 coupling (RECALIBRATION-GATED; statistician spec MOSAIC-pkg/claude/prior_fix_spec/SPEC_B2.md, the durable Phase-2 replacement for the v15.14 B1 static re-anchor). The per-country mu_j_baseline GAMMA LOCATION PRIOR is REMOVED and replaced by a per-country CFR_target LOGNORMAL location prior (meanlog = log(mean_cfr) [the same WHO hierarchical-GAM CFR the B1 build used; the CFR is now the prior MEDIAN], sdlog = 0.787 global). sample_parameters() now DERIVES mu_j_baseline at sample time from the already-sampled chain factor: mu_j_baseline = CFR_target * (1 - exp(-gamma_1)) * rho / (rho_deaths * chi_epidemic) [the B2.1 engine-correct chain factor: the recovery-tick dwell factor is the per-tick recovery probability (1 - exp(-gamma_1)), NOT the continuous-rate gamma_1, and the effective PPV is chi_epidemic, NOT the 0.5*(chi_endemic+chi_epidemic) blend, because the engine's reported_cases is an Isym stock-read dominated by epidemic-regime ticks]. Substituting into the v0.14.0 reported-CFR identity cancels the entire chain factor, so the realized implied reported CFR == CFR_target for EVERY draw, regardless of where gamma_1/chi/rho/rho_deaths land -- gamma_1 stays fully free for the cases fit and the deaths channel no longer absorbs chain-factor drift (the structural cure for the B1 defect that a static gamma_1/chi anchor only approximated at the cohort median). The v15.10/v15.14 ETH-only x0.497 dwell stop-gap is REMOVED: B2 subsumes it structurally (ETH's mu is derived from ETH's OWN sampled gamma_1 every draw, so ETH implied CFR is pinned to CFR_target_ETH with no hand-tuned residual). CV SIZING (statistician SPEC_B2 sec.2): sdlog_cfr=0.787 PRESERVES today's implied-CFR prior spread (Var(log)=Var(log mu_Gamma4)+Var(log chain)=0.2231+0.3964=0.6195 -> sd 0.787, CV~0.93). The brief's 'match marginal mu Gamma(4) CV=0.5' target is INFEASIBLE (the now-sampled chain factor alone has Var(log)=0.396 > 0.223), so B2 necessarily WIDENS the marginal mu prior to CV~1.33 by design -- harmless because mu is a latent nuisance and the data identifies the implied CFR (== CFR_target). FLAG for disease-modeler: confirm CFR_target per-country centers (the WHO-GAM mean_cfr) are biologically sane as the model's TARGET reported CFR; FLAG for statistician: sdlog_cfr=0.787 sizing. v15.14 (2026-06-22): RECALIBRATION-GATED bundled Stage-1 prior fixes (statistician spec MOSAIC-pkg/claude/prior_fix_spec/SPEC_prior_fixes.md), from the 19-ISO single-location metapop review at /Users/johngiles/MOSAIC/output/full_metapop/stage1_individual/. (Fix 1) mu_j_baseline CFR->mu chain factor B1 STATIC RE-ANCHOR: the derivation balanced the realized-CFR chain factor (chi/gamma_1) at the prior centers, but calibration consistently pulls gamma_1 DOWN (posterior median ~0.092, 16/19 below 0.10) and chi UP (~0.70), inflating implied CFR by a median ~1.25x. Re-anchored the derivation inputs from the prior-mean gamma_1=0.1133 / chi=0.639 to posterior-consistent gamma_1=0.10 (lognormal median, the shipped config scalar) / chi=0.70, so cfr_to_mu_adjustment 0.1832->0.1474 and every per-country mu_j_baseline center scales x0.804 (~1.25x deaths/implied-CFR reduction; cases untouched, mu_j never enters the case channel). The mu_j_baseline magnitude and rho_deaths v0.13 factor are unchanged (project_mu_j_baseline_already_fixed); gamma_1 prior WIDTH not narrowed (deferred biology check). The v15.10 ETH-only dwell stop-gap is RETAINED on top of B1: B1 is a UNIFORM x0.804 fold calibrated to the cohort-median gamma_1, but ETH calibrates gamma_1 well below the cohort median (~0.076, long-dwell tail) so the uniform fold under-corrects ETH; removing the stop-gap would push ETH mu_j_baseline 0.00088->0.00177 (x2.01) and ETH deaths bias ~1.83->~3.7 (mu is a linear deaths lever). ETH is held at the SAME total correction DEPTH as v15.10 (x0.40 on the old prior-center adjustment 0.1832): since B1 already supplies x0.804 of that depth, the ETH residual on top of B1 is 0.40/0.804 ~ 0.497, keeping ETH mu_j_baseline ~0.00088 (NOT 0.00177). NOT applied to any other country. B1 under-corrects ETH specifically; B2 (dynamic per-country gamma_1-coupled CFR->mu derivation, Phase 2) will subsume the ETH stop-gap properly. Also fixed the stale inline comment (the adjustment was mislabelled ~0.115; the verified value at the current rho/chi/rho_deaths priors is 0.183 pre-re-anchor, 0.1474 post). (Fix 2) beta_j0_tot per-country PARTIAL-SHRINKAGE RECENTER (w=0.5 geometric mean of v15.9 prior center and Stage-1 posterior median: new_center = sqrt(prior*post_median)), beta_j0_tot weakly identified (Stage-1 ESS ~110-135). 18 of the 19-ISO cohort recentered (LBR EXCLUDED -- unidentified, no transmission signal, stays at 2e-5 global default); recenter is SYMMETRIC (BDI/MWI/NGA/RWA/TZA move UP -- the v15.9 sweep over-corrected them down). Prior WIDTH (sdlog 1.1748) KEPT for every country (Stage-3 warm-start inflation guard). beta_j0_hum/beta_j0_env are derived at sample time (p_beta*beta_j0_tot), not stored separately, so the recenter propagates to both. NOTE: the v15.9 COG beta_j0_tot override (4.6667e-6) is KEPT (COG is OUTSIDE the Stage-1 19-ISO cohort, so it is not recentered and must not be reset to the 2e-5 global default -- doing so would be a ~4.3x unintended increase); COG is the only non-cohort country carrying a beta override. v15.13 (2026-06-20): rebuilt at the 2023-01-01 production window alongside config_default v4.3 under the relaxed surveillance trust-tier gate (process_cholera_surveillance_data v0.47.1, fourier_* kept + down-weighted). epidemic_threshold and IC seeding (ic_t0 = 2023-02-01 for the 2023 build) re-derived against the regenerated multi-source combined weekly/daily surveillance with fourier reconstructions retained; build_date_start = 2023-01-01. v15.12 (2026-06-19): multi-source surveillance integration. (a) Initial-condition seeding epoch DECOUPLED from the fit-window date_start: all est_initial_* calls (V1_V2, E_I, R, S) and the population-at-t0 match now use ic_t0 = max(date_start, 2023-02-01). Empirically the 2015 IC lookback window has 0 active-case countries and late-2022 only ~6 (vs 11 at 2023-02-01), so seeding from an early window cold-starts ICs (near-zero E/I, R_eff<1, no ignition); the floor pins ICs to the proven data-rich anchor regardless of how early the fit window starts, and breaks the circular hazard whereby a <2023 config rebuild would poison the next priors rebuild's IC epoch. (b) epidemic_threshold now derives from the multi-source combined weekly file (include_ai=TRUE adds JHU/AI back-history) with AI rows EXCLUDED (source != 'AI') for parity with est_seasonal_dynamics; more outbreak weeks shift some per-country medians and may flip countries off the Zheng 0.7/100k fallback (MIN_OUTBREAK_WEEKS gate). v15.11 (2026-06-18): psi_star_b prior re-centred mean 0->+1.0 (sd kept 2.5) for all 40 countries to match the new per-capita D-scale suitability psi (target_D_rate_per_country_floored), which sits at a much lower level than the old transmission_intensity scale (per-country mean COD 0.93->0.43, global 0.14->0.10). At the prior center (a=1) the calc_psi_star transform is an odds-multiply psi*=sigma(logit(psi)+b). The +1.0 level shift acts on the delta (environmental decay) channel, NOT beta_env: beta_env (envtohuman.py:24) is self-normalized as beta_j0_env*(psi*/psi_bar*) so a uniform b ~cancels in the low-psi regime (no-op), whereas delta decay (environmental.py:150) reads psi* on its ABSOLUTE level via survival_days = days_short + pbeta(psi*|s1,s2)*(days_long-days_short), so a higher psi* lengthens modelled V. cholerae reservoir survival. The D scale pinned most countries near the days_short floor (~16d) at the old b=0 center; +1.0 raises psi* (COD mean 0.36->0.53, MOZ 0.12->0.21) so survival climbs toward the days_long ceiling at seasonal peaks while staying off the floor in low-burden countries. Validated against the spec eq:decay-priors envelope (claude/validate_psi_star_b_delta_survival.R, 2026-06-19): at the decay prior means (days_short=16, days_long=196, s1=s2=3) post-shift per-country survival lies entirely within ~16-196d (0/40 exceed the 196d ceiling, 0/40 below the 16d floor; median +4.9d/+17% to mean survival; survival is structurally bounded above by days_long since pbeta<=1, so the shift only moves countries ALONG the [16,196] curve and cannot breach the envelope). Gives calibration a sensible STARTING center, not a constraint (sd=2.5 retains both-direction freedom; the global decay days/shape params are also sampled). The MOZ-specific psi_star_b override (mean +0.4, fit on the old scale) is removed/folded into the general center. psi_star_a unchanged (a=1 identity is scale-invariant). v15.10 (2026-06-18): ETH-only mu_j_baseline dwell-mismatch STOP-GAP -- scale Ethiopia's derived mu_j_baseline mean by 0.40 (Gamma rate 1816->4540, CV unchanged). The CFR->mu identity uses gamma_1 at its PRIOR MEAN (~0.114, 8.8d dwell), but ETH calibration drifts gamma_1 to the long-dwell tail (~0.076, ~14d); since per-case CFR scales as mu_j/gamma_1, realized reported CFR inflates to ~3.7% vs observed ~1.2% (deaths over-predict ~3.3x while cases stay near-unbiased). The x0.40 re-center returns deterministic reported CFR to ~1.5% (deaths bias ~1.3x) with cases unaffected (mu_j/rho_deaths do not enter the case channel; GTFCC treated-CFR target <1%). NOT a structural cure: the dwell mismatch affects all countries; the durable fix is a dwell-adjusted CFR->mu derivation and/or the run_MOSAIC best-subset weighting fix (the dAIC-4 truncation currently discards the deaths signal, so the ensemble reports near the prior center). Do not chase the exact x0.31 point estimate (overfits the broken weighting). See disease-modeler memory project_eth_deaths_cfr_dwell_mismatch. v15.9 (2026-06-16): laser-cholera v0.14.0 (issue #67) adjustments. (a) mu_j_baseline CFR->mu identity gains a gamma_1 (dwell) factor -- mu = CFR * gamma_1 * rho / (rho_deaths * chi) -- because v0.14.0 reports the new_symptomatic INCIDENCE flow (= gamma_1 * Isym at steady state) instead of the Isym prevalence stock; per-country mu_j_baseline prior means drop ~9x (gamma_1 prior mean ~0.113) vs v15.8. (b) beta_j0_tot per-country medians recentred for the 14 cases-data countries from the v0.14.0 beta*mu magnitude sweep (e.g. ETH 1.75e-6->1.39e-5, SSD 2e-5->2.83e-4, NGA 2e-5->4.74e-6); the 26 no-data countries keep the 2e-5 global default (the sweep found no transferable beta shift, geomean ~1.2x / log-CV 1.5). Supersedes the v15.3 ETH-only override. See MOSAIC-pkg/claude/beta_percountry_sweep.R + beta_mu_percountry_sweep.R and memory project_laser_cholera_reported_cases_fix_67. v15.8 (2026-06-03): rho (cases-side care-seeking) re-derived as Beta(5.38, 7.10), mean 0.423, 95% CI [0.19, 0.70], ESS ~12.5, from random-effects pooling of TWO Wiens et al. 2025 (PMC12013865) case-definition strata: general diarrhea (29.9% [25.3, 35.1], n=122 obs) and severe diarrhea + cholera (58.6% [39.9, 75.2], n=22 obs). Pooling both strata captures the severity spectrum of symptomatic cholera (mild-to-moderate + severe), avoiding the upward bias of the severe-only stratum (dominated by outbreak-response settings) and the downward bias of the general stratum (broader population including many self-resolving episodes). The 12 GEMS Nasrin 2013 pediatric MSD entries previously included alongside Wiens were dropped because (a) GEMS measures pediatric MSD, a different population than MOSAIC's all-ages cholera; (b) the 12-strata-to-1 pooling was upside-down dimensionally; (c) Wiens already includes GEMS-derived data at the population level (6 of its Study IDs are tagged GEMS/HUAS). The prior mean moves from 0.276 to 0.423 - a moderate shift in the direction implied by the cholera-specific evidence while staying within clinical plausibility (per-episode CFR sanity check passes for all high-N test countries: MOZ 3.4%, ETH 9.1%, KEN 9.9%, COD 14.6%). See MOSAIC-pkg/R/get_rho_care_seeking_params.R for the full rationale. v15.7 (2026-06-02): rho_deaths switched back to the informative variant Beta(36.95, 51.02) (pooled-mean CI fit, ESS ~88, sd ~0.05). Rationale: MOSAIC's deaths likelihood identifies the product mu_j_baseline * rho_deaths per country, leaving a flat (sloppy) factorization direction. The narrow rho_deaths prior pins it near 0.42 during calibration sampling so mu_j_baseline posteriors carry the cross-country CFR signal cleanly. The wider prediction-interval variant Beta(6.30, 8.52) is retained for sensitivity analysis (see SYNTHESIS_REPORT.md sec 3.2). v15.6 (2026-06-02): mu_j_baseline derivation corrected for laser-cholera v0.13+ schema: cfr_to_mu_adjustment = rho / (rho_deaths * chi) (was rho/chi pre-v0.13). Per-country Gamma priors derived directly from the data-informed CFR (hierarchical GAM) using the steady-state identity mu_j_baseline = CFR * rho / (rho_deaths * chi); the rho, rho_deaths, chi means are computed inline from their actual Beta priors so the conversion factor stays in sync. Per-country prior means are ~2.36x their pre-v15.6 values (this corrects the pre-v0.13 under-scaling where mu_j_baseline implicitly absorbed 1/rho_deaths). MOZ-specific mu_j_baseline override Gamma(2, 1176) and mu_j_epidemic_factor override Gamma(1.5, 0.5) dropped -- both were calibrated under the pre-v0.13 misspecified likelihood and are superseded by the universal data-driven prior. v15.5 (2026-06-02): (a) rho_deaths switched from informative Beta(36.95, 51.02) -> recommended Beta(6.30, 8.52) per SYNTHESIS_REPORT.md sec 3.4; both share centre ~0.42, but Beta(6.30, 8.52) fits the 95% prediction interval and is the production default (encodes both pooled mean precision AND between-study heterogeneity); informative variant retained for sensitivity. (b) delta_reporting_deaths description corrected from 'Symptom-onset-to-death-report' to 'Death-event-to-death-report' to match laser-cholera v0.13+ engine semantics (the symptom-onset-to-death lag is implicit in gamma_1^-1 in the SEIR dynamics, not in this parameter). v15.4 (2026-06-01): rho_deaths replaced (Beta(3, 2) -> Beta(36.95, 51.02)) using random-effects meta-analysis (DerSimonian-Laird, logit scale) on three SSA studies (Routh 2017 Tanzania, Shikanga 2009 Kenya, Bwire 2013 Uganda); the informative variant is fit to the 95% CI of the pooled mean. New prior: mean 0.42, 95% CI [0.32, 0.52]. The previous Beta(3, 2) attribution to Finger 2024 was incorrect (editorial, no quantitative anchor); see MOSAIC-pkg/claude/rho_deaths_research/SYNTHESIS_REPORT.md. v15.3 (2026-06-01): beta_j0_tot location prior for ETH recentred from the global median 2e-5 to 1.75e-6 (Ethiopia is low-incidence; the global value over-predicts reported cases ~8x). Derived from fixed-ensemble fitting against current ETH surveillance + raw LSTM suitability; conditional on the suitability-window mean. v15.2 (2026-05-29): removed 2x sd variance-inflation step on epsilon (sd back to 2.0e-4 from 4.0e-4); the inflation had pushed the upper-tail natural-immunity duration to ~53 yr with no documented rationale. v15.1 (2026-04-29): rho_deaths added as a first-class global prior, Beta(3, 2), reflecting ~60% surveillance capture of true cholera deaths (Finger et al. 2024; laser-cholera#49). v15.0 (2026-04-23): zeta_1, zeta_2, and zeta_ratio re-estimated from literature meta-analysis (~6 OOM scale shift on zeta_1). zeta_2 added as first-class prior."
     ),
     parameters_global = list(),    # Single parameters used by all locations
     parameters_location = list()   # Location specific parameters
)

# The changelog head "vX.Y (YYYY-MM-DD)" is hand-written; metadata$date is the
# build day. Flag a mismatch so provenance keyed on metadata$date is not silently
# a day off (priors v16.1 shipped date 2026-09-28 under a 2026-09-29 head).
.changelog_date <- sub("^.*? v[0-9.]+ \\(([0-9]{4}-[0-9]{2}-[0-9]{2})\\).*$", "\\1",
                       priors_default$metadata$description, perl = TRUE)
if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", .changelog_date) &&
    .changelog_date != as.character(priors_default$metadata$date)) {
     warning("priors_default changelog head is dated ", .changelog_date,
             " but metadata$date (build day) is ", priors_default$metadata$date,
             "; update the version heading.")
}

#----------------------------------------
# Global parameters in alphabetical order
#----------------------------------------

# alpha_1 - Population mixing within metapops (PER-LOCATION as of v15.16)
# Relocated from parameters_global to parameters_location: a SHARED informative
# marginal Beta(28.4, 71.6) for every ISO (mean 0.284, sd ~0.045, 95% CI
# ~[0.20, 0.38]). The shared tight prior emulates hierarchical shrinkage (the
# per-ISO sampler cannot express a true hierarchy) and starves the
# alpha_1 <-> beta_j0_tot degeneracy, while still letting real per-location
# signal move it. The engine is dual-mode (laser-cholera params.py:557-564): a
# length-nL alpha_1 is asserted shape == (num_nodes,) and applied elementwise
# per patch in the FOI; a scalar alpha_1 is broadcast (legacy/national configs
# stay valid). Center owned by disease-modeler; concentration (100) is a
# statistician sign-off knob. alpha_2 (below) stays GLOBAL SCALAR by design.
priors_default$parameters_location$alpha_1 <- list(
     description = "Population mixing within metapops (0-1, 1 = well-mixed); per-location shared marginal Beta(28.4, 71.6)",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$alpha_1$location[[iso]] <- list(
          distribution = "beta",
          parameters = list(shape1 = 28.4, shape2 = 71.6)
     )
}

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
# make_simulation_config()) without the post-hoc swap that previously corrupted the
# joint distribution, and preserves the biological upper bound across staged posteriors
# (the old Uniform(30, 365) prior was fit as unbounded Lognormal at stage 2+).
# Truncnorm(mean=180, sd=95, a=1, b=365) matches the prior-predictive of the old
# Uniform(30, 365) minus TruncNorm(16, 7) (implied mean ~ 182, sd ~ 96) and the
# historical posterior shape from MOZ_v43 / calibration_test_46-48 runs (posterior
# implied spread mean ~ 185, sd ~ 94, q0.975 ~ 340 -- hugging the 365 ceiling).
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

# kappa - Half-saturation dose of the environmental dose-response D/(kappa + D).
# Since MOSAIC v0.89.0 the engine's dose is the PER-CAPITA load D = W/N (cells
# per resident, sim_components.R EnvToHuman), not a water concentration; the
# volunteer ID50 data below are the literature anchor for its scale.
# UPDATED v0.28.16: Derived from est_kappa_prior() meta-analysis of 13 literature
# sources (Hornick 1971, Cash 1974, Levine 1981/1988, Tacket 1999, QMRA synthesis,
# expert reviews). Weighted lognormal fit anchored on the 5 weight-1
# buffered-volunteer-challenge rows (Hornick buffered, Cash, Levine 1981,
# Levine 1988, Tacket) with expert reviews downweighted (0 to 0.5 weight).
# Result at the current table: LN(meanlog ~11.77, sdlog ~1.82), median ~1.3e5;
# the exact values are in est_kappa_prior()$fit / model/input/param_kappa_prior.csv.
kappa_prior_fit <- MOSAIC::est_kappa_prior(PATHS = PATHS)
priors_default$parameters_global$kappa <- list(
     description = "Half-saturation dose of the environmental dose-response, applied to the per-capita environmental load W/N (anchored on volunteer ID50 data meta-analysed from 13 literature sources)",
     distribution = "lognormal",
     parameters = list(meanlog = kappa_prior_fit$fit$meanlog, sdlog = kappa_prior_fit$fit$sdlog)
)


# Gravity kernel: prefer the BLEND fit (air + raked overland), matching what
# data-raw/make_config_default.R writes into config_default$mobility_gamma /
# $mobility_omega. These Gammas are MODE-matched (shape = mode*rate + 1), so the
# config point estimate is the prior MODE, not its mean (mean = mode + 1/rate).
# Leaving this on the air-only file left the prior mode at the air values
# (1.3622 / 0.6164) while the config shipped the blend (1.8997 / 0.6271).
# NOTE tau_i deliberately does NOT follow the blend here -- it is overland-only
# (see the tau_overland_file block below). Kernel = blend, departure = overland.
.grav_f <- file.path(PATHS$MODEL_INPUT, "param_gravity_model_blend.csv")
if (!file.exists(.grav_f)) {
     warning("param_gravity_model_blend.csv not found; falling back to the air-only ",
             "param_gravity_model.csv, so the mobility prior modes will not match the ",
             "config_default blend kernel. Re-run est_mobility(od_source = \"blend\").",
             immediate. = TRUE)
     .grav_f <- file.path(PATHS$MODEL_INPUT, "param_gravity_model.csv")
}
message("priors gravity source: ", basename(.grav_f))
param_gravity <- read.csv(.grav_f)
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
          chi_endemic_shape1 <- 5.56
          chi_endemic_shape2 <- 5.10
     }
     if (length(chi_epidemic_shape1) == 0 || length(chi_epidemic_shape2) == 0) {
          warning("Could not extract chi_epidemic shape params from file. Using defaults.")
          chi_epidemic_shape1 <- 4.97
          chi_epidemic_shape2 <- 1.58
     }
} else {
     warning("Chi PPV parameter file not found. Using default values.")
     chi_endemic_shape1  <- 5.56;  chi_endemic_shape2  <- 5.10
     chi_epidemic_shape1 <- 4.97;  chi_epidemic_shape2 <- 1.58
}

# The fallbacks above are the get_suspected_cases() fits to the published
# (2.5%, 50%, 97.5%) triples (0.24, 0.52, 0.80) and (0.40, 0.78, 0.99); until
# v0.100.0 the fit used a 0.0275 lower quantile (Beta(5.43, 5.01) / Beta(4.79, 1.53)).
# chi_endemic - PPV among suspected cases during endemic periods (Weins et al. 2023 low estimate)
# Beta(5.56, 5.10) -> median ~0.52, 95% CI [0.24, 0.80]
priors_default$parameters_global$chi_endemic <- list(
     description = "PPV among suspected cases during endemic periods (Weins et al. 2023, all settings)",
     distribution = "beta",
     parameters = list(shape1 = chi_endemic_shape1, shape2 = chi_endemic_shape2)
)

# chi_epidemic - PPV among suspected cases during epidemic periods (Weins et al. 2023 high estimate)
# Beta(4.97, 1.58) -> median ~0.79, 95% CI [0.40, 0.98]
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
# PINNED (v15.19, CFR restructure R2). sample_rho_deaths now defaults FALSE in
# both sample_parameters() and run_MOSAIC(), so calibration holds rho_deaths at
# config_default$rho_deaths = 0.42 (this prior's mean, to 3 d.p. 0.4199). The
# prior is RETAINED here as the literature record, as the source of that 0.42,
# and for explicit sensitivity runs (sample_rho_deaths = TRUE).
#
# WHY PINNED. Since v16.0 (MOSAIC v0.96.0) the engine converts the reported CFR
# mu_jt to a per-onset fatality probability p = mu_jt * rho / (rho_deaths *
# chi_epidemic) and then thins true deaths by rho_deaths, so rho_deaths cancels
# EXACTLY from expected reported deaths (it sets only the unreported, true deaths)
# and the deaths likelihood carries no information about it. (The same
# cancellation held under the retired B2 derivation, v15.15-v15.20: empirically
# the parameter beat a 400x resampling null in 1 of 27 production countries with
# posterior/prior variance ratio 1.09; claude/cfr_review/05_run_empirics.md.)
# Pinning removes an inert sampled dimension and changes nothing else.
#
# Prior choice (unchanged from v15.7): the informative variant fit to the 95% CI
# of the pooled MEAN, not the wider prediction interval Beta(6.30, 8.52)
# (SYNTHESIS_REPORT sec 3.2). All three anchor studies are SSA outbreak settings
# - the regime MOSAIC calibrates - so the pooled-mean centre is the right point
# value to pin at. The wide variant remains the sensitivity-analysis alternative,
# but note that widening it only adds an inert dimension (it cancels from reported
# deaths).
#
# Methodology parallels R/get_rho_care_seeking_params.R (cases-side rho).
# Full provenance: MOSAIC-pkg/claude/rho_deaths_research/SYNTHESIS_REPORT.md
# (sec 3.3 documents this informative variant; sec 3.2 documents the wider
# variant; sec 3.4 documents the production-vs-sensitivity tradeoff).
# Figure: MOSAIC-pkg/claude/rho_deaths_prior_comparison_v6.png.
# See also laser-cholera#49 for the engine-side reported_deaths implementation.
priors_default$parameters_global$rho_deaths <- list(
     description = "Death detection rate: probability a true cholera death is captured by surveillance (random-effects meta-analysis of Routh 2017, Shikanga 2009, Bwire 2013; informative variant fit to the pooled-mean CI). PINNED at 0.42 as of v15.19: the engine's per-onset fatality probability is mu_jt * rho / (rho_deaths * chi_epidemic) and it thins true deaths by rho_deaths, so it cancels exactly from reported deaths (it sets only true deaths) and carries no likelihood information. Retained as the literature record and for sensitivity runs (sample_rho_deaths = TRUE).",
     distribution = "beta",
     parameters = list(shape1 = 36.95, shape2 = 51.02)
)

# sigma - Proportion of infections that are symptomatic
# Read from param_sigma_prop_symptomatic.csv, written by est_symptomatic_prop():
# a least-squares Beta quantile fit to the sero-survey / cohort table of
# get_symptomatic_prop_data() (Nelson 2009, Leung & Matrajt 2021, Harris 2012,
# Finger 2024, Jackson 2013, Bart 1970 x2, Harris 2008, Hegde 2023), the
# method 04-model-description.Rmd documents. Up to priors v16.1 the value was
# hardcoded as Beta(4.30, 13.51) (mean 0.24): that was this same fit on a table
# whose Harris et al. 2008 row was mistranscribed as 0.184 [0.112, 0.256]. The
# paper (PLoS NTD 2(4):e221) reports 127 of 202 culture-confirmed household-
# contact infections symptomatic, 0.629 [0.558, 0.695]; with the corrected row
# (and the 0.025 quantile typo fixed) the fit is Beta(3.75, 7.12), mean 0.35,
# 95% [0.11, 0.64]. The Haiti population sero-surveys (Jackson 2013 0.21,
# Finger 2024 0.24) sit near its 20th percentile.
sigma_param_file <- file.path(PATHS$MODEL_INPUT, "param_sigma_prop_symptomatic.csv")
sigma_shape1 <- sigma_shape2 <- numeric(0)
if (file.exists(sigma_param_file)) {
     param_sigma  <- read.csv(sigma_param_file, stringsAsFactors = FALSE)
     sigma_shape1 <- param_sigma$parameter_value[param_sigma$parameter_name == "shape1"]
     sigma_shape2 <- param_sigma$parameter_value[param_sigma$parameter_name == "shape2"]
}
if (length(sigma_shape1) != 1L || length(sigma_shape2) != 1L) {
     stop("param_sigma_prop_symptomatic.csv missing or lacks one shape1/shape2 row. ",
          "Run get_symptomatic_prop_data() and est_symptomatic_prop() first.")
}
priors_default$parameters_global$sigma <- list(
     description = "Proportion of infections that are symptomatic (Beta quantile fit to the sero-survey and cohort table of get_symptomatic_prop_data(); est_symptomatic_prop())",
     distribution = "beta",
     parameters = list(shape1 = sigma_shape1, shape2 = sigma_shape2)
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
# Stored as a first-class prior; sample_parameters() derives zeta_2 at
# sampling time as zeta_2 = zeta_1 / zeta_ratio, and the zeta_ratio prior's
# truncation at 1 (below) keeps zeta_2 <= zeta_1. This prior is the
# literature-derived *reference* distribution for validation. See plan_zeta_priors_implementation.md Section 6.
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
# 2014, etc. as literature anchors (priors v16.1: meanlog 4.31 = median ~74,
# sdlog 4.39; the fitted values are in model/input/param_zeta_ratio_prior.csv).
# See plan_zeta_priors_implementation.md Section 7.2 (Table 7.A).
# est_zeta_ratio_prior()$fit IS the direct channel (since v0.100.0; before that
# $fit and the CSV carried the combined channel while this block shipped A).
# zeta_2 is derived: zeta_2 = zeta_1 / zeta_ratio, so zeta_1 >= zeta_2 needs
# zeta_ratio >= 1. The untruncated direct channel puts ~16% of its mass below 1
# ($fit$p_below_1), inherited from the Smith 2026 household-transmission OR
# interval (0.11-3.23), which is not a per-day shedding ratio. Symptomatic
# stool carries 1e5-1e8 cells/mL at up to litres/day (est_zeta_1_prior
# anchors) vs ~1e3-1e5 cells/g for asymptomatic carriers (Nelson 2009;
# Kaper 1995; est_zeta_2_prior anchors), and 04-model-description samples the
# ratio to rule out zeta_1 < zeta_2. So the shipped prior is the direct
# channel TRUNCATED below at 1 (parameters$lower = 1, honoured by
# sample_from_prior() and kept through update_priors_from_posteriors() and
# inflate_priors()). meanlog/sdlog are those of the untruncated lognormal
# (4.31 / 4.39 at the v0.100.0 anchors); truncation removes 16.3% of the mass,
# moving the median from ~75 to ~185, the mean from ~1.2e6 to ~1.4e6, and
# the 95% interval to ~[1.4, 5.7e5].
zeta_ratio_res <- MOSAIC::est_zeta_ratio_prior(
     PATHS,
     zeta_1_fit = zeta_1_res,   # full return list; .extract_fit() unwraps
     zeta_2_fit = zeta_2_res
)
priors_default$parameters_global$zeta_ratio <- list(
     description = "Ratio of symptomatic to asymptomatic shedding rate (zeta_1 / zeta_2); direct literature-anchor channel (Smith 2026, Chao 2011, Finger 2018, Nelson 2009 paired, etc.), lognormal truncated below at 1 so that the derived zeta_2 = zeta_1 / zeta_ratio never exceeds zeta_1",
     distribution = "lognormal",
     parameters = list(meanlog = zeta_ratio_res$fit$meanlog,
                       sdlog   = zeta_ratio_res$fit$sdlog,
                       lower   = zeta_ratio_res$fit$lower)
)

# delta_reporting_cases - Symptom-onset-to-case reporting delay
# Incubation is already handled by the E compartment (iota parameter); this
# captures only the lag from symptom onset to surveillance report.
# Updated from TruncNorm(mean=2, sd=2) based on MOZ calibration tests 19-28:
# posteriors consistently collapse toward 0-1 days (KL=14.2 in test_19).
# New prior: TruncNorm(mean=1, sd=1.5, a=0, b=7) -- mode near 1 day, most mass
# in 0-3 day range while still permitting longer delays for countries with
# slower paper-based reporting systems.
# Sampled value is rounded to the nearest integer before passing to make_simulation_config().
priors_default$parameters_global$delta_reporting_cases <- list(
     description = "Symptom-onset-to-case reporting delay in days (integer, 0-7)",
     distribution = "truncnorm",
     parameters = list(mean = 1, sd = 1.5, a = 0, b = 7)
)

# delta_reporting_deaths - REMOVED in priors_default v16.0 (MOSAIC v0.96.0). A
# death is now drawn at symptom onset and reported on the case lag,
# delta_reporting_cases, as the surveillance record it is scored against does
# (deaths and cases share the same WHO bulletin row; the weekly cross-correlation
# of the observed series peaks at lag 0 in 11 of 15 countries,
# claude/cfr_review/05_run_empirics.md). The separate post-mortem lag and its
# TruncNorm(4, 3, [1, 14]) prior have no engine term left to feed.

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
#     inflating the prior median from 1.89e-5 to 3.80e-5 -- too aggressive.
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

# --- Per-country recentred transmission defaults (laser-cholera v0.14.0) ----
# v15.9 (2026-06-16): per-country beta_j0_tot medians recentred for laser-cholera
# v0.14.0 (incidence-based reported_cases, issue #67) from the v0.14.0 beta*mu
# magnitude sweep (single-simulation bias->1 against surveillance under config_default
# defaults; see MOSAIC-pkg/claude/beta_percountry_sweep.R). These were the 14
# cases-data countries with a solved beta* (NAM excluded: beta-insensitive, an
# IC/suitability issue not a transmission one). The 26 no-data countries keep the
# global default (2e-5): the sweep found NO transferable beta shift (geomean ~1.2x,
# log-CV 1.5, bidirectional). Supersedes the v15.3 ETH-only override (1.75e-6,
# v0.13-era; v0.14.0 needs ETH ~8x higher). NOTE: env-term medians are conditional
# on the suitability-window mean (psi_bar); re-validate if date_start/stop shift.
#
# v15.14 PER-COUNTRY PARTIAL-SHRINKAGE RECENTER (Stage-1 metapop review, 2026-06-22):
# beta_j0_tot is weakly identified (Stage-1 ESS ~110-135, second-lowest of all
# params), so single-location posteriors give a reliable DIRECTION but a noisy
# point value. We recenter each country's meanlog as a geometric mean (w=0.5) of
# its current v15.9 prior center and its Stage-1 posterior median:
#   new_center = exp(0.5*log(prior_center) + 0.5*log(post_median))
#              = sqrt(prior_center * post_median)
# w=0.5 is deliberately conservative: (i) the Stage-3 warm-start inflates
# beta_j0_tot x2 (a single-patch estimate fed into a network that adds
# transmission), so a full recenter to the point would fight the joint fit;
# (ii) ESS ~130 is genuinely uncertain per country; (iii) the geometric midpoint
# honors the direction without committing to the point. The recenter is symmetric:
# five countries move UP (BDI, MWI, NGA, RWA, TZA) because the v15.9 sweep
# over-corrected them down; this is NOT a flat cut. The prior WIDTH (sdlog 1.1748)
# is KEPT for every country (do not tighten -- Stage-3 inflation guard). Only
# meanlog moves. beta_j0_hum/beta_j0_env are NOT stored separately: they are
# derived at sample time as p_beta*beta_j0_tot and (1-p_beta)*beta_j0_tot
# (sample_parameters.R), so this recenter propagates to both consistently with no
# separate edit. LBR is NOT recentered -- Stage-1 confirms LBR is unidentified
# (1724 cases over 1107 nonzero days, max 4/day, no epidemic structure, r2_corr ~0
# at every beta scale, 1 death); its posterior carries no transmission signal, so
# it stays at the 2e-5 global default (flagged as a data limitation). Source:
# Stage-1 single-location fits at
# /Users/johngiles/MOSAIC/output/full_metapop/stage1_individual/<ISO>/2_calibration/
# posterior/posterior_quantiles.csv; statistician spec
# MOSAIC-pkg/claude/prior_fix_spec/SPEC_prior_fixes.md sec 3.3.
beta_j0_tot_v0140 <- c(
     AGO = 2.008e-05, BDI = 1.599e-05, CMR = 1.884e-05, COD = 3.525e-05,
     ETH = 1.411e-05, KEN = 1.308e-05, MOZ = 4.041e-05, MWI = 4.515e-05,
     NER = 1.571e-05, NGA = 5.911e-06, RWA = 1.197e-06, SOM = 8.756e-05,
     SSD = 2.515e-04, TZA = 7.311e-06, UGA = 1.157e-05, ZAF = 1.420e-05,
     ZMB = 3.077e-05, ZWE = 6.564e-05,
     # COG is OUTSIDE the Stage-1 19-ISO cohort, so it is NOT recentered. It is
     # carried over UNCHANGED at its existing v15.9 override (4.6667e-06) -- the
     # v15.14 cohort recenter must not reset non-cohort overrides to the 2e-5
     # global default (doing so was a ~4.3x unintended increase). COG keeps its
     # prior-derived low-incidence override until it enters a future fit cohort.
     COG = 4.6667e-06
)
for (iso in names(beta_j0_tot_v0140)) {
     if (!is.null(priors_default$parameters_location$beta_j0_tot$location[[iso]])) {
          priors_default$parameters_location$beta_j0_tot$location[[iso]]$parameters$meanlog <- log(beta_j0_tot_v0140[[iso]])
     }
}


# p_beta - Proportion of human-to-human vs environmental transmission
beta_fit_p_beta <- fit_beta_from_ci(mode_val = 0.33, ci_lower = 0.1, ci_upper = 0.5)


priors_default$parameters_location$p_beta <- list(
     description = "Proportion of total base transmission that is human-to-human (0-1)",
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













# tau_i - Daily departure probability (per-day away fraction; 1 tick = 1 day).
# Lognormal, from the evidence-anchored OVERLAND departure prior in
# param_tau_departure_overland.csv (est_overland_tau_prior(), E3 evidence;
# tau_daily = tau_weekly / 7). The air-derived Beta prior from
# param_tau_departure.csv is only the fallback when that file is absent or
# incomplete (see the tau_overland_file block below).

tau_uncertainty_factor <- 0.001  # Increase tau uncertainty

priors_default$parameters_location$tau_i <- list(
     description = "Country-level travel probabilities",
     location = list()
)

# Load tau parameters from file
# Overland departure prior (lognormal, evidence-anchored) takes precedence.
# NOTE tau_uncertainty_factor is deliberately NOT applied to it: that factor
# exists to widen a Beta fitted to huge OAG counts, and the overland prior's
# width is specified directly as a 95% span. Applying both would be double
# counting, and the Beta route's 0.001 factor is what puts the prior mode at
# ZERO departure for 31 of 40 countries.
tau_overland_file <- file.path(PATHS$MODEL_INPUT, "param_tau_departure_overland.csv")
tau_used_overland <- FALSE
if (file.exists(tau_overland_file)) {
     tau_ov <- read.csv(tau_overland_file, stringsAsFactors = FALSE)
     if (all(c("iso_code", "meanlog", "sdlog") %in% names(tau_ov)) &&
         !all(is.na(tau_ov$meanlog))) {
          n_set <- 0L
          for (iso in j) {
               r <- tau_ov[tau_ov$iso_code == iso, , drop = FALSE]
               if (nrow(r) == 1L && is.finite(r$meanlog) && is.finite(r$sdlog)) {
                    priors_default$parameters_location$tau_i$location[[iso]] <- list(
                         distribution = "lognormal",
                         parameters = list(meanlog = r$meanlog, sdlog = r$sdlog)
                    )
                    n_set <- n_set + 1L
               }
          }
          if (n_set == length(j)) {
               tau_used_overland <- TRUE
               message(sprintf("tau_i: overland LOGNORMAL prior for %d locations (median %.3g/day, 95%% span %.0fx)",
                               n_set, stats::median(exp(tau_ov$meanlog)),
                               stats::median(tau_ov$ci_hi / tau_ov$ci_lo)))
          } else {
               warning("Overland tau covered ", n_set, " of ", length(j),
                       " locations; falling back to the air-derived Beta prior.")
          }
     }
}

tau_param_file <- file.path(PATHS$MODEL_INPUT, "param_tau_departure.csv")
if (!tau_used_overland && file.exists(tau_param_file)) {
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

} else if (!tau_used_overland) {
     # NB the !tau_used_overland guard must be on BOTH branches. Putting it
     # only on the `if` sends control into this `else` when the overland
     # prior succeeded, silently overwriting all 40 lognormals with Beta
     # defaults -- the message reports success and the object says otherwise.
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

# The engine uses the seasonal coefficients as a multiplicative envelope,
# beta_j0_hum * (1 + f(t)), and clamps a negative rate to zero, so at the prior
# means 1 + f(t) must stay positive over the year. est_seasonal_dynamics()
# (v0.100.0) shrinks the case-fit amplitude to keep min(1 + f) >= 0.1
# (Keeling & Rohani 2008 section 5.2: |beta_1| < 1 for multiplicative forcing).
.season_min_envelope <- vapply(j, function(iso) {
     cf <- vapply(names(seasonality_csv_lookup), function(k)
          priors_default$parameters_location[[k]]$location[[iso]]$parameters$mean,
          numeric(1))
     t <- seq_len(365L)
     1 + min(cf[["a_1_j"]] * cos(2 * pi * t / 365) + cf[["b_1_j"]] * sin(2 * pi * t / 365) +
             cf[["a_2_j"]] * cos(4 * pi * t / 365) + cf[["b_2_j"]] * sin(4 * pi * t / 365))
}, numeric(1))
if (any(.season_min_envelope <= 0)) {
     stop("Seasonal prior means give a non-positive transmission envelope min(1 + f(t)) for: ",
          paste(sprintf("%s (%.2f)", j[.season_min_envelope <= 0],
                        .season_min_envelope[.season_min_envelope <= 0]), collapse = ", "),
          ". Re-run est_seasonal_dynamics() (>= v0.100.0) to rebuild param_seasonal_dynamics.csv.")
}

# The positivity above holds at the prior MEANS only. Under independent draws
# from these Normal priors the envelope dips below zero in a share of draws:
# 33% averaged over the 40 locations at the v17.0 build (ZAF 0.77, RWA 0.73,
# GAB 0.72, CIV 0.66), because est_seasonal_dynamics() scales each strongly
# seasonal fit until its trough sits exactly at the 0.1 floor, so ~half of any
# symmetric prior around such a mean lies beyond it. Shrinking the prior SDs
# until P(min(1 + f) <= 0) < 5% was rejected: it needs SD factors 0.12-0.63 in
# 31 locations (median 0.34), i.e. priors 3-8x tighter than the case-fit SE
# (already x sqrt(2) above), which would pin the amplitude to a mean that the
# floor scaling placed, not the data. A negative draw is still a valid engine
# state: the engine clamps the human force of infection at zero, i.e. no
# human-to-human transmission in that part of the low season. The share is
# reported here so a rebuild that changes it is visible.
.season_p_nonpos <- MOSAIC:::.mosaic_with_local_seed(20260930L, {
     .t <- seq_len(365L)
     .B <- rbind(cos(2 * pi * .t / 365), sin(2 * pi * .t / 365),
                 cos(4 * pi * .t / 365), sin(4 * pi * .t / 365))
     vapply(j, function(iso) {
          pp <- lapply(c("a_1_j", "b_1_j", "a_2_j", "b_2_j"), function(k)   # rows of .B
               priors_default$parameters_location[[k]]$location[[iso]]$parameters)
          m <- vapply(pp, `[[`, numeric(1), "mean"); sdv <- vapply(pp, `[[`, numeric(1), "sd")
          X <- matrix(stats::rnorm(4L * 4000L, m, sdv), nrow = 4L)
          mean(1 + apply(crossprod(X, .B), 1L, min) <= 0)
     }, numeric(1))
})
message(sprintf(paste0("Seasonal prior draws with min(1 + f(t)) <= 0: mean %.0f%% over %d ",
                       "locations; highest: %s"),
                100 * mean(.season_p_nonpos), length(j),
                paste(sprintf("%s %.2f", names(sort(.season_p_nonpos, decreasing = TRUE))[1:5],
                              sort(.season_p_nonpos, decreasing = TRUE)[1:5]), collapse = ", ")))



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
                                 date_start = ic_t0, verbose = FALSE)
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
               time_diffs <- abs(as.numeric(difftime(pop_loc$date, ic_t0, units = "days")))
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



# V1/V2 initial conditions: the fallback Beta priors above are overridden per
# country by est_initial_V1_V2() in the v0.28.5 block earlier in this script
# (OCV campaign history, effective coverage phi * doses since v0.100.0).



# Update default priors with estimated initial conditions for E and I

# Variance inflation for the E/I Beta priors (re-derived for the v0.100.0
# estimator; replaces the hand-tuned per-country table, 30-200).
# Meaning: est_initial_E_I() keeps the Monte Carlo MEAN m and fits the Beta's
# spread to the 95% CI [m / VI, m * VI] on the logit scale. The MC spread itself
# is not used, so VI is the whole width of the prior.
# Derivation (uniform VI = 10):
#   - Reporting chain: E/I scale with chi_endemic / (rho * sigma). Under the
#     global priors (rho, chi_endemic, sigma Betas in this file) that ratio has
#     a 95% range of about m / 4.3 to 3.1 m (1e5 draws; chi / rho alone
#     m / 2.9 to 2.3 m), log-SD ~0.65.
#   - Dwell times: E ~ onsets / iota, I ~ onsets / gamma_1; the iota and
#     gamma_1 lognormal priors have sdlog 0.40 and 0.50.
#   - Combined log-SD sqrt(0.65^2 + 0.45^2) ~ 0.79, i.e. x/ 4.7 at 95%; a
#     further factor ~2 covers what the MC omits (the finite reporting window,
#     onset-to-report timing, small-count noise). 4.7 x 2 ~ 10.
# With VI = 10 the mean-anchored Beta has shape1 ~1.6 (at m = 1e-5): unimodal,
# 95% range ~[m / 13, 3.1 m], and P(< m / 100) ~ 0.001. The old table's values
# (30-200) give shape1 < 1 -- a density that diverges at zero, i.e. a prior
# that favours no infection at t0 in countries whose t0 window has reported
# cases (zero-case windows get the Beta(0.01, 99999.99) template instead).
# Their per-country "surveillance quality" labels were uncited, and the
# surveillance difference they described is already in each country's case
# data; the reporting-chain priors are global, so there is no data basis for a
# per-country width. They were also never in effect: the pre-v0.100.0 refit
# collapsed the prior to ~3x wide whatever VI was.
variance_inflation_E_I <- 10

# Seed for the initial-condition Monte Carlo (est_initial_E_I/R/S). Each
# location's draws use a seed derived from this and its ISO code, so a rebuild
# from the same inputs gives byte-identical priors, with or without forking.
# Monte Carlo size (5 seeds, 2023 build): E/I prior-mean CV across seeds 8%
# (max 17%) at n = 100 vs 2-3% (max 5%) at n = 1000 at the same run time, so
# E/I use 1000. prop_R stays at 100: its MC CV is 12% (max 23%) against a prior
# CV of ~1.0, and n = 1000 would add ~40 min to the build. prop_S is inert.
ic_seed <- 20260930L

# Estimate E/I initial-condition priors
initial_conditions_E_I <- est_initial_E_I(
     PATHS = PATHS,
     priors = priors_default,
     config = config_default,
     n_samples = 1000,
     t0 = ic_t0,
     # 28-day window straddling date_start: two reporting weeks either side
     # (surveillance is weekly, downscaled to days), long against the ~10-day
     # symptomatic dwell (1/gamma_1) and short against the seasonal cycle. A
     # 3-day lookback missed outbreaks under way on day 1 (see the ic_t0 note).
     lookback_days = 14,
     lookahead_days = 14,
     # Quiet-start seeding floor: a location that reports observed or
     # reconstructed (tier 1-2) cases later in the config window (after this
     # window, up to date_stop) but either none in this window or too few for
     # one expected initial infection (N * (E[prop_E] + E[prop_I]) < 1 under
     # the window's Beta) gets Beta(1, 1e5) for E and for I instead of its
     # window-based prior (the near-zero Beta(0.01, 99999.99) when the window
     # is empty). Imputed (tier-3, AI Fourier) rows are not reports: they are
     # set aside wherever the window holds a tier 1-2 count, and a window
     # without one reads its country-level reconstructions, never regional
     # ones (metadata$imputed_window_fallback; ETH at 2018-01-01, whose
     # December 2017 reconstruction ran ~300 cases/week after an observed 61).
     # Absent an importation mechanism this stands in for undetected
     # circulation or re-introduction, so a single-location fit can reach the
     # later outbreak (GHA 2024, NER 2024, TCD 2025, ...). Mean 1e-5 per
     # compartment is the v16.1 prior mean for these countries (BFA, CIV, NER,
     # AGO, ...), i.e. E + I ~ 2e-5 N: ~25 people in SWZ (1.2M) to ~1,300 in
     # TZA (67M). Mode at zero (shape1 = 1), so P(E + I >= 1) ~ 1 for N >= 1e6.
     # Locations with no cases anywhere up to date_stop keep the near-zero
     # template, and window-based priors that already imply >= 1 expected
     # initial infection (AGO, BEN, KEN, ...) are left alone.
     quiet_start = "seed",
     quiet_seed_shape1 = 1,
     quiet_seed_shape2 = 1e5,
     variance_inflation = variance_inflation_E_I,  # uniform scalar, see derivation above
     verbose = FALSE,
     parallel = TRUE,
     seed = ic_seed
)

     n_updated_E_I <- 0
     priors_default$metadata$quiet_start_seeded <- initial_conditions_E_I$metadata$quiet_start_seeded
     priors_default$metadata$imputed_window_fallback <- initial_conditions_E_I$metadata$imputed_window_fallback
     priors_default$metadata$description <- sub(
          "{quiet_start_seeded}",
          paste(initial_conditions_E_I$metadata$quiet_start_seeded, collapse = " "),
          priors_default$metadata$description, fixed = TRUE)
     message("Quiet-start seeding prior for: ",
             paste(initial_conditions_E_I$metadata$quiet_start_seeded, collapse = ", "))
     message("E/I window read from country-level imputed rows (no tier 1-2 count) for: ",
             paste(initial_conditions_E_I$metadata$imputed_window_fallback, collapse = ", "))

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


# No post-estimation E/I rescaling (removed v0.100.0). Up to v0.99.10 a table
# of hand-tuned per-country factors (AGO 0.01, COD 0.75, KEN 1.2, MOZ 0.4, ...,
# 0 = "no active cholera at t0") multiplied the est_initial_E_I() means. They
# compensated for defects of that estimator: E built from already-reported
# cases, a hardcoded rho ~ U(0.2, 0.7) instead of the model's rho / chi_endemic
# / delta_reporting_cases priors, zero draws dropped before averaging, and a
# Beta refit that collapsed to a near point mass. The v0.100.0 estimator fixes
# those at source (E in balance with the onset rate, I from observed onsets,
# the model's reporting-chain priors, zero draws kept), and a location whose
# surveillance window has zero reported cases (method "observed_zero") or no
# usable surveillance at all (all NA, method "no_data_default") already gets the
# near-zero Beta(0.01, 99999.99) template prior, which is what the old 0
# factors encoded by hand. Re-applying factors tuned against the old
# estimator would mix the two, so none are applied; any per-country adjustment
# must be re-derived from calibration evidence against the new estimator.


# Update default priors with estimated initial conditions for R

# Define location-specific variance inflation for R compartment
# Higher values = more uncertainty, allowing for greater variation in estimates
# Meaning: an SD multiplier in a method-of-moments Beta refit that keeps the
# Monte Carlo mean (v0.100.0; before that the CI half-widths were scaled
# linearly and passed to a mode-exact fit, so the factors were tuned against a
# different refit and do NOT carry their old meaning). The R draws now use the
# model's rho / chi_endemic priors (E[chi / rho] ~ 1.36) instead of the
# never-resolved fallback chi / rho = 5, and the refit keeps the Monte Carlo
# mean instead of the old mode fit's inflated mean, so prop_R_initial means fall
# ~10-50x at this rebuild (ETH ~48x), not the ~3.7x of the chi/rho change alone.
# With those small means a large factor collapses the Beta onto 0 (ETH at VI 13
# gave shape1 0.0035, median 5.5e-87), so fit_beta_with_variance_inflation_R()
# floors shape1 at 1 and the assert after est_initial_R() below rejects any
# prop_R prior whose median is below 0.1x its mean. The per-country factors
# still need re-deriving against calibration evidence under this refit.
# Only includes ISO codes in MOSAIC::iso_codes_mosaic
variance_inflation_R <- c(
     "AGO" = 3,   # Angola: Further reduced
     "BDI" = 4,   # Burundi: Further reduced
     "BEN" = 4,   # Benin: Further reduced
     "BFA" = 14,  # Burkina Faso: Moderate uncertainty
     "BWA" = 50,  # Botswana: Maximum uncertainty
     "CAF" = 24,  # Central African Republic: Increased - limited data quality
     "CIV" = 13,  # Cote d'Ivoire: Moderate systems
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
     t0 = ic_t0,
     disaggregate = TRUE,
     variance_inflation = variance_inflation_R,  # Named vector for location-specific values
     verbose = FALSE,
     parallel = TRUE,
     seed = ic_seed
)


# Guard: no prop_R prior may be a near point mass at 0 (median << mean).
local({
     locs_R <- initial_conditions_R$parameters_location$prop_R_initial$parameters$location
     bad <- vapply(names(locs_R), function(loc) {
          a <- locs_R[[loc]]$shape1; b <- locs_R[[loc]]$shape2
          if (is.null(a) || is.null(b) || !is.finite(a) || !is.finite(b)) return(FALSE)
          stats::qbeta(0.5, a, b) < 0.1 * a / (a + b)
     }, logical(1))
     if (any(bad)) {
          stop("prop_R_initial prior median < 0.1 x mean for: ",
               paste(names(locs_R)[bad], collapse = ", "),
               " -- re-derive variance_inflation_R before rebuilding.", call. = FALSE)
     }
})

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

# No variance inflation for S. variance_inflation is an SD multiplier in the
# method-of-moments refit (v0.100.0), so the former per-country table of
# 0.01-0.10 ("slight flexibility") actually SHRANK the S prior's SD 10-100x
# (shape1 up to ~1.9e6) while countries set to 0 kept the Monte Carlo SD.
# Every location now keeps its Monte Carlo spread (1 = unchanged).
# The prop_S_initial prior is inert in sampling: sample_parameters() draws V1,
# V2, E, I and R and takes S as the simplex residual, never from this prior.
# It is kept for reference and plots only.
variance_inflation_S <- 1

# Use location-specific variance inflation with single function call
initial_conditions_S <- est_initial_S(
     PATHS = PATHS,
     priors = priors_default,
     config = config_default,
     n_samples = 100,
     t0 = ic_t0,
     variance_inflation = variance_inflation_S,  # 1 = keep the Monte Carlo SD
     verbose = FALSE,
     min_S_proportion = 0.001,  # Default minimum S proportion
     seed = ic_seed
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



# mu_jt - Reported case fatality ratio by location and year (v16.0, MOSAIC v0.96.0)
#
# The engine reads the reported CFR as a [location x day] matrix, config$mu_jt,
# and converts it each tick to the probability that a symptomatic onset is
# fatal: p = mu_jt * rho / (rho_deaths * chi_epidemic). mu_jt is NOT sampled.
# In calibration it is integrated out per simulated path
# (calc_log_likelihood_deaths_integrated(), run_MOSAIC()): given the path's
# onsets, expected reported deaths are linear in the CFR, so the CFR is modelled
# as
#     logit mu_jt = logit mu0_jt + a_j + delta_{j,y(t)}
#     a_j ~ N(0, sd_shift_j^2),  sd_shift_j^2 = sd_product^2 + mean_y(logit_se_{j,y}^2)
#     delta_{j,y} ~ N(0, sd_year^2)
# and a_j and the year deviations are solved per path by a Laplace step.
# This block holds everything that needs:
#   * location[[iso]]: year, logit_mean (= logit mu0, the config centre) and
#     logit_se (the SE of the country-trend mean) from est_CFR_hierarchical()
#     (model/input/cfr_hierarchical_estimates.csv; binomial GAM on all
#     WHO-annual years with a global trend, country intercepts, per-country
#     drift and a country-year random effect).
#   * sd_year: the GAM's country-year random-effect SD (sigma), i.e. the
#     year-to-year spread of a country's CFR about its trend (0.70 logit).
#   * sd_product: residual error of the GAM centre against the observed reported
#     CFR in the calibration window: sd(log) 0.19-0.32 over the 15-17 countries
#     with >= 50 deaths, 2023-26; 0.3 sits at the top of that range. It is not a
#     product mismatch -- the WHO-annual and weekly surveillance products agree to
#     sd(log) 0.03 in matched windows -- and the observed/centre geometric mean
#     (1.07 over 2023-26, t = 0.9) is not applied: the location offset absorbs it.
#   * tau: the GAM's between-country SD, for reference.
# The resolver in run_MOSAIC() uses the years inside the config window.
cfr_est_file <- file.path(PATHS$MODEL_INPUT, "cfr_hierarchical_estimates.csv")
cfr_sum_file <- file.path(PATHS$MODEL_INPUT, "cfr_model_summary.rds")
if (!file.exists(cfr_est_file) || !file.exists(cfr_sum_file)) {
     stop("cfr_hierarchical_estimates.csv / cfr_model_summary.rds not found in ",
          PATHS$MODEL_INPUT, ". Run est_CFR_hierarchical(PATHS) first.")
}
cfr_est <- read.csv(cfr_est_file, stringsAsFactors = FALSE)
cfr_sum <- readRDS(cfr_sum_file)
if (!is.numeric(cfr_sum$sigma) || !is.finite(cfr_sum$sigma) || cfr_sum$sigma <= 0)
     stop("cfr_model_summary.rds carries no positive country-year SD (sigma).")
# The shipped mu_jt (priors v16.1 / config v5.1) was built from an
# est_CFR_hierarchical() run at its DEFAULTS, min_cases = 1 and k_year = 12
# (recorded in cfr_model_summary.rds). update_mosaic_data() step 2B has passed
# min_cases = 3, k_year = 15, so a pipeline refresh would silently change the
# CFR centres; flag any CSV not produced with the defaults.
if (!isTRUE(all.equal(c(cfr_sum$min_cases_threshold, cfr_sum$k_year), c(1, 12)))) {
     warning(sprintf(paste0("cfr_hierarchical_estimates.csv was built with min_cases = %s, ",
                            "k_year = %s; the shipped mu_jt used the est_CFR_hierarchical() ",
                            "defaults (1, 12). Confirm the change is intended before shipping."),
                     format(cfr_sum$min_cases_threshold), format(cfr_sum$k_year)),
             immediate. = TRUE)
}
mu_jt_years_min <- 2010L   # the earliest supported build start is 2015 (psi floor 2010)
priors_default$mu_jt <- MOSAIC:::.mosaic_mu_jt_prior(cfr_est, location_name = j,
                                                     sd_year = cfr_sum$sigma, tau = cfr_sum$tau,
                                                     sd_product = 0.3, year_min = mu_jt_years_min)
message(sprintf("  mu_jt prior: %d locations, years %d-%d, sd_year %.3f, sd_product %.2f",
                length(priors_default$mu_jt$location), mu_jt_years_min, max(cfr_est$year),
                priors_default$mu_jt$sd_year, priors_default$mu_jt$sd_product))

# CFR_target, mu_j_baseline and mu_j_epidemic_factor - REMOVED in v16.0 (MOSAIC
# v0.96.0). They parameterised the retired mortality model: a daily hazard on the
# symptomatic stock (mu_j_baseline, derived at sample time from CFR_target by the
# B2.1 chain factor) escalated by mu_j_epidemic_factor on epidemic-flagged ticks.
# Deaths are now drawn at onset from mu_jt above.

# epidemic_threshold - Location-specific epidemic regime activation threshold
#
# Units: dimensionless daily Isym/N point prevalence fraction.
# The simulation engine compares epidemic_threshold against
#   Isym[t - delta_reporting_cases] / N[t - delta_reporting_cases]
# at every daily tick to decide whether to apply chi_epidemic (the case-reporting PPV).
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

# Extract model parameters from config -- do NOT hardcode these values
rho_val    <- config_default$rho
chi_val    <- config_default$chi_endemic
gamma1_val <- config_default$gamma_1

# Compute per-country median weekly incidence per 100k during outbreak-positive weeks.
# Cap the population join year at the maximum available year in demographics so a
# surveillance record past the demographic horizon still gets a population row
# (the UN WPP file runs to 2100, so the cap only binds for a truncated file).
dem_max_year  <- max(dem_annual$year)

# Exclude AI-mined rows from the epidemic_threshold derivation, for parity with
# est_seasonal_dynamics() (which also excludes AI). The combined weekly file now
# carries AI observed/documented_zero rows (include_ai=TRUE); AI signal belongs
# to the suitability/LSTM path, not to data-derived transmission priors. Direct
# WHO/JHU/SUPP rows have source NA-or-non-AI and are kept.
outbreak_rows <- surv_weekly[
     surv_weekly$iso_code %in% j &
     (is.na(surv_weekly$source) | surv_weekly$source != "AI") &
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
# SSA districts -- a conservative (upper-side) choice for low-burden / data-sparse countries.
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
# spread (CV ~ 0.53) with a small inflation buffer.
#
# v0.28.2: Removed the absolute lower floor of 1e-6. With a floor, two
# countries with very low Zheng prior means (BEN, CIV) had a > prior_mean,
# yielding an ill-posed truncnorm whose mean lay below the lower bound
# (fit_truncnorm_from_ci() rejects mode_val <= a). Pure proportional lower
# bound (pm/10) avoids this. The 1% upper cap remains -- that's the real
# safety net against epidemic-regime-unreachable drift.
EPIDEMIC_THRESHOLD_SD_REL    <- 0.65   # natural-scale CV
EPIDEMIC_THRESHOLD_UPPER_ABS <- 0.01   # global cap: 1% daily symp prevalence = severe epidemic

priors_default$parameters_location$epidemic_threshold <- list(
     description = paste0(
          "Dimensionless daily Isym/N prevalence threshold for epidemic regime activation. ",
          "Compared against Isym[t - delta_reporting_cases] / N[t - delta_reporting_cases] in run_simulation(), ",
          "where it switches reported-case ascertainment from chi_endemic to chi_epidemic. ",
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
               # Re-centred to mean = +1.0 for the new per-capita D-scale suitability psi
               # ("target_D_rate_per_country_floored", model/input/pred_psi_suitability_day.csv).
               # The transform (R/calc_psi_star.R:114) is psi* = sigma(a*logit(psi)+b); at the
               # prior CENTER (a=1) this is an odds-multiply psi* = sigma(logit(psi)+b).
               #
               # WHICH CHANNEL THE +1.0 ACTS ON. psi* feeds two engine channels with OPPOSITE
               # scale-sensitivity, and the +1.0 level shift is justified by the SECOND of them:
               #   (1) beta_env (envtohuman.py:24): beta_jt_env = beta_j0_env*(psi*/psi_bar*), i.e.
               #       SELF-NORMALIZED per patch over time. In the low-psi D regime the odds-multiply
               #       approximates psi* ~ e^b*psi, so a uniform b CANCELS in the psi*/psi_bar* ratio
               #       and is ~a no-op for beta_env (beta_j0_env already carries the forcing level).
               #       The +1.0 is NOT justified by, and does little to, this channel.
               #   (2) delta decay (environmental.py:150, map_suitability_to_decay): reads psi* on its
               #       ABSOLUTE level via survival_days = days_short + pbeta(psi*|s1,s2)*(days_long -
               #       days_short) (eq:delta). Here the level genuinely matters: a higher psi* lengthens
               #       modelled V. cholerae environmental survival. THIS is where +1.0 has its effect.
               #
               # WHY +1.0 IS DEFENSIBLE ON THE DELTA CHANNEL. The D scale lowered psi vs the old
               # transmission_intensity scale (per-country mean COD 0.93->0.43, MOZ 0.16; global all-
               # country mean 0.14->0.10). At the old b=0 center most countries sat at near-floor raw D
               # suitability, pinning modelled reservoir survival near the days_short floor (~16 d) even
               # in high-burden settings. The +1.0 odds-shift raises psi* (at a=1: COD mean 0.36->0.53,
               # SOM 0.27->0.44, MOZ 0.12->0.21) so the implied survival climbs toward the days_long
               # ceiling at seasonal peaks while staying off the floor in low-burden countries.
               # Crucially, because survival_days is bounded above by days_long (pbeta <= 1), the shift
               # only moves each country ALONG the [days_short, days_long] curve -- it CANNOT push
               # survival past the biological envelope, so the larger forcing is structurally safe.
               #
               # SPEC-ENVELOPE VALIDATION (claude/validate_psi_star_b_delta_survival.R, 2026-06-19).
               # Evaluated at the decay PRIOR MEANS (days_short=16, days_long=196, s1=s2=3; eq:decay-
               # priors, 04-model-description.Rmd L213-223) over the actual per-country D-psi time series:
               # post-shift survival lies entirely within the spec's ~16-196 day V. cholerae survival
               # envelope (0/40 countries exceed the 196 d ceiling; 0/40 below the 16 d floor). The shift
               # raises per-country MEAN survival by a median of ~4.9 d (~17%); peak-suitability survival
               # in high-burden COD/SOM reaches the 196 d ceiling exactly as eq:decay-priors intends
               # (196 d at psi*->1). The +1.0 is thus consistent with the published decay-prior anchor.
               #
               # A larger "median->0.5" recenter (b~=4.6) was REJECTED: it saturates peaks (and would peg
               # most countries at the 196 d survival ceiling year-round, flattening the seasonal decay
               # dynamic range). sd kept at 2.5 (95% CI [-3.90, 5.90]) so calibration retains full per-
               # country freedom in BOTH directions -- this is a better STARTING center, not a constraint;
               # the decay shape/days params are global+sampled so calibration can further adjust survival.
               # Anchor data: model/input/pred_psi_suitability_day.csv (D-psi computed 2026-06-18).
               mean = 1.0,
               sd   = 2.5       # 95% CI: [-3.90, 5.90]
          )
     )
}

# NOTE (D-scale): the prior MOZ-specific psi_star_b override (mean=+0.40 from
# calibration_test_19) was an evidence-based positive shift fit on top of the OLD b=0 center
# under the OLD high-level transmission_intensity psi scale. Under the new D-scale psi that
# posterior is no longer transferable (the input scale changed entirely), and the new general
# center (+1.0) already exceeds the old MOZ +0.4. The MOZ override is therefore folded into the
# general center (removed) rather than re-derived.

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
                                # flat across z -- amplifying bimodality into the final ensemble.
          )
     )
}

# psi_star_k - Time offset parameter for suitability calibration (location-specific)
# Biological rationale: k > 0 delays the suitability signal (epidemic follows/lags the peak),
# k < 0 advances it (epidemic precedes the peak).
# Both directions are permitted: in some settings epidemics may precede a broad suitability
# peak (e.g. early season explosive outbreaks) or lag it (slow accumulation in low-WASH areas).
# Bounds [-90, 90] cover the full plausible range; centred at 0 with sd=25 keeps most mass
# within +/-50 days while allowing the data to identify the direction.
priors_default$parameters_location$psi_star_k <- list(
     description = "Time offset in days for suitability calibration (k>0: epidemic lags suitability peak; k<0: epidemic precedes suitability peak). Bounded to [-90, 90]: both lag and advance are permitted.",
     location = list()
)

for (iso in j) {
     priors_default$parameters_location$psi_star_k$location[[iso]] <- list(
          distribution = "truncnorm",
          parameters = list(
               mean = 0,        # Centered at no offset; data identifies direction
               sd = 25,         # Most mass within +/-50 days
               a = -90,         # Lower bound: up to 90 days advance
               b = 90           # Upper bound: up to 90 days lag
          )
     )
}

# MOZ-specific override: posterior from calibration_test_19 shifted to mean ~ -4.5 days,
# indicating Mozambique epidemics slightly precede the suitability peak (epidemic leads
# suitability by ~5 days). Re-centre the prior at -5 and tighten sd (25->20) to
# concentrate mass on the evidence-supported direction while preserving full [-90,90] range.
priors_default$parameters_location$psi_star_k$location[["MOZ"]] <- list(
     distribution = "truncnorm",
     parameters = list(
          mean = -5,   # Evidence-based: epidemic slightly precedes suitability peak in MOZ
          sd = 20,     # Slightly tighter than global; most mass within +/-40 days
          a = -90,     # Lower bound unchanged
          b = 90       # Upper bound unchanged
     )
)


# Save to file and add to MOSAIC R package

# Write into the package tree this script is RUN FROM, not the canonical
# checkout (see the matching note in make_config_default.R): PATHS$ROOT is the
# data root, so a worktree run would clobber ~/MOSAIC/MOSAIC-pkg while
# use_data() wrote the .rda locally.
.pkg_dir <- normalizePath(getwd(), mustWork = TRUE)
if (!file.exists(file.path(.pkg_dir, "DESCRIPTION"))) {
     stop("Run this script from the MOSAIC-pkg root: no DESCRIPTION in ", .pkg_dir)
}
fp <- file.path(.pkg_dir, 'inst/extdata/priors_default.json')

# save to file. digits = NA preserves full numerical precision; the default
# digits = 4 rounds small bounds (e.g. 2.277e-05) to 0, silently corrupting
# truncnorm epidemic_threshold entries and any other small-scale priors
# (bug discovered in v0.28.7 plot_model_distributions debugging).
jsonlite::write_json(priors_default, fp, pretty = TRUE, auto_unbox = TRUE, digits = NA)

# Read back to verify
tmp_priors <- jsonlite::fromJSON(fp, simplifyVector = FALSE)

identical(priors_default$parameters_location$alpha_1, tmp_priors$parameters_location$alpha_1)

# Note: R data object is saved as priors_default to match config_default naming convention
usethis::use_data(priors_default, overwrite = TRUE)

