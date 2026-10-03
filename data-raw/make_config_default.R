library(MOSAIC)

# Set up paths - critical for finding input files
# Set root to parent directory containing all MOSAIC repos
MOSAIC::set_root_directory("/Users/johngiles/MOSAIC")
PATHS <- MOSAIC::get_paths()

# `get_paths()` resolves everything under the canonical ~/MOSAIC root. 43 of its
# 45 entries point at the shared DATA repos, which is correct and must not move.
# But MODEL_INPUT/MODEL_OUTPUT live INSIDE MOSAIC-pkg, so under a git worktree
# they silently point back at the canonical checkout -- this script would then
# read another tree's model/input artifacts while writing its .rda locally, so a
# fix to e.g. est_CFR_hierarchical()'s outputs would never reach the priors.
# Re-point exactly those two at the tree we are running in.
.pkg_here <- normalizePath(getwd(), mustWork = TRUE)
if (!file.exists(file.path(.pkg_here, "DESCRIPTION"))) {
     stop("Run this script from the MOSAIC-pkg root: no DESCRIPTION in ", .pkg_here)
}
PATHS$MODEL_INPUT  <- file.path(.pkg_here, "model", "input")
PATHS$MODEL_OUTPUT <- file.path(.pkg_here, "model", "output")

# date_start: start of the calibration fit window. DEFAULT 2023-01-01, OVERRIDABLE via
# the MOSAIC_BUILD_DATE_START env var -- the single source of truth that flows the SAME
# start date into BOTH this script and make_priors_default.R. CONFIGURABLE to an earlier
# start (e.g. 2015-01-01): all covariates (psi from 2010-04-01, demographics/vaccination
# from 2000, CFR from 1970) cover >=2015, and make_priors_default.R estimates the initial
# conditions at date_start itself (E/I from a 28-day surveillance window straddling it),
# so an early start seeds from that era's data. The floor guard below rejects a start
# before psi coverage (2010-04-01).
#
# REBUILD ORDER for a non-default (back-history) start -- the env var is REQUIRED so both
# builders agree (make_priors' fallback otherwise reads the STALE installed
# config_default$date_start, silently desyncing the windows):
#   export MOSAIC_BUILD_DATE_START=2015-01-01
#   1. Rscript data-raw/make_priors_default.R     # initial conditions at the new start
#   2. Rscript -e 'devtools::install(".")'         # so THIS script sees the new priors_default
#   3. Rscript data-raw/make_config_default.R      # sources beta/IC means from new priors; mu_jt from model/input
# For the default 2023 build leave the env var unset (both scripts fall back to 2023-01-01).
.env_ds    <- Sys.getenv("MOSAIC_BUILD_DATE_START", "")
date_start <- if (nzchar(.env_ds)) as.Date(.env_ds) else as.Date("2023-01-01")

# -----------------------------------------------------------------------------
# Env-desync fail-loud assert (build-order guard)
# -----------------------------------------------------------------------------
# make_priors_default.R falls back to the INSTALLED config_default's date_start
# when MOSAIC_BUILD_DATE_START is unset, so a build that skips the
# devtools::install(".") step between step 1 (priors) and step 2 (this script)
# can silently produce e.g. 2015 priors against a 2023 config (or vice versa)
# with NO warning. make_priors_default.R records the window it was built for in
# priors_default$metadata$build_date_start; assert it matches the date_start this
# script resolved. If it differs we stop() and point at the documented rebuild
# order. For back-compat with older priors that predate the metadata field, skip
# with a warning() rather than erroring.
.priors_bds <- tryCatch(MOSAIC::priors_default$metadata$build_date_start,
                        error = function(e) NULL)
if (is.null(.priors_bds) || !nzchar(as.character(.priors_bds))) {
     warning(paste0(
          "priors_default$metadata$build_date_start is absent (older priors build) -- ",
          "cannot verify the config/priors calibration windows are in sync. ",
          "Proceeding, but rebuild priors_default to record the build window."))
} else if (!identical(date_start, as.Date(.priors_bds))) {
     stop(sprintf(paste0(
          "BUILD-ORDER DESYNC: this config resolves date_start = %s but the installed ",
          "priors_default was built for date_start = %s. The windows MUST match. ",
          "Rebuild in order: (1) Rscript data-raw/make_priors_default.R, ",
          "(2) Rscript -e 'devtools::install(\".\")' (so this script sees the new ",
          "priors_default), then (3) re-run this script -- with MOSAIC_BUILD_DATE_START ",
          "set to the SAME value for all steps (unset = 2023-01-01)."),
          format(date_start), format(as.Date(.priors_bds))))
}

# date_stop: derived from the psi forecast horizon so the simulation window
# tracks whatever the latest LSTM environmental-suitability forecast extends
# to. The same file (pred_psi_suitability_day.csv) is read in full at the
# psi_jt assembly step further down; reading just the date column here is
# cheap (~150K rows). When the LSTM is re-run with a longer/shorter horizon
# this picks up the change automatically -- no hand-bump of date_stop needed.
psi_path <- file.path(PATHS$MODEL_INPUT, "pred_psi_suitability_day.csv")
if (!file.exists(psi_path)) {
     stop(sprintf("psi forecast file not found at %s -- cannot derive date_stop.",
                  psi_path))
}
# date_stop: the latest date through which EVERY modeled location still has a
# genuine (non-filled) suitability value. est_suitability() now drops trailing
# carry-forward fill, so per-location series end at their own covariate-coverage
# horizon (ragged). We truncate the common simulation window to the
# shortest-covered location so psi_jt never contains a forward-filled flat tail
# (which suppresses the environmental force of infection and causes an artificial
# end-of-series drop). When the LSTM is re-run with a different horizon this
# picks up the change automatically -- no hand-bump of date_stop needed.
j <- MOSAIC::iso_codes_mosaic
psi_dates <- read.csv(psi_path)[, c("iso_code", "date")]
psi_dates$date <- as.Date(psi_dates$date)
psi_dates <- psi_dates[psi_dates$iso_code %in% j, ]
last_by_iso <- tapply(as.integer(psi_dates$date), psi_dates$iso_code, max, na.rm = TRUE)
missing_iso <- setdiff(j, names(last_by_iso))
if (length(missing_iso) > 0) {
     warning(sprintf(
          "No psi predictions for %d modeled location(s): %s. Excluded from the common-coverage window.",
          length(missing_iso), paste(missing_iso, collapse = ", ")))
}
date_stop <- as.Date(min(last_by_iso, na.rm = TRUE), origin = "1970-01-01")
limiting_iso <- names(last_by_iso)[which.min(last_by_iso)]

# Floor guard: date_start must not precede psi coverage for every modeled
# location, else psi_jt (assembled by acast over [date_start, date_stop] below)
# would be short of length(t) and fail make_simulation_config's ncol(psi_jt)==length(t)
# check with a cryptic error. Fail early and clearly instead.
psi_start_by_iso <- tapply(as.integer(psi_dates$date), psi_dates$iso_code, min, na.rm = TRUE)
psi_floor <- as.Date(max(psi_start_by_iso, na.rm = TRUE), origin = "1970-01-01")
if (date_start < psi_floor) {
     stop(sprintf(
          "date_start (%s) precedes psi coverage: the latest per-location psi start is %s. Set date_start >= %s, or regenerate pred_psi_suitability_day.csv with an earlier pred_date_start.",
          format(date_start), format(psi_floor), format(psi_floor)))
}

message(sprintf(
     "Simulation window: %s to %s (%d days). date_stop = common psi coverage across modeled locations (limited by %s); source %s.",
     format(date_start), format(date_stop),
     as.integer(date_stop - date_start) + 1L,
     limiting_iso, basename(psi_path)
))

message("Set simulation time steps and locations")
t <- seq.Date(date_start, date_stop, by = "day")

message("Get population size of each location (N_j)")
tmp <- read.csv(file.path(PATHS$MODEL_INPUT, 'param_N_population_size.csv'))
tmp$t <- as.Date(tmp$t)
tmp <- tmp[tmp$j %in% j & tmp$t == date_start,]
N_j <- tmp$parameter_value
names(N_j) <- tmp$j
sel <- match(j, names(N_j))
N_j <- as.integer(N_j[sel])

# -----------------------------------------------------------------------------
# Initial conditions per location, seeded from per-iso priors.
#
# Earlier versions hard-coded `S/N = 0.50`, `R/N = 0.50` and pinned V1/V2/E to
# zero across all 40 countries -- bypassing the per-iso `prop_*_initial`
# Beta priors built by est_initial_S/V1_V2/E_I/R(). The flat 50/50 split was
# the dominant defect behind ETH (and 39 other countries) failing to sustain
# transmission in the default fit: with R0 ~ 1.44, R_eff(t=0) = 1.44 * 0.5
# = 0.72, so the 100-case seed decays before it can grow. At the prior mean
# S/N ~ 0.80, R_eff ~ 1.15 and outbreaks become possible.
#
# Seeding strategy: pull the mean of each per-iso prop_*_initial Beta prior,
# normalise across the six compartments so each row sums to 1.0, then
# Hamilton-apportion to integer counts that sum exactly to N_j.
# -----------------------------------------------------------------------------

.compartment_props <- c("S","V1","V2","E","I","R")
.prop_means <- vapply(
     j,
     function(iso) {
          vapply(
               paste0("prop_", .compartment_props, "_initial"),
               function(pname) {
                    loc <- priors_default$parameters_location[[pname]]$location[[iso]]
                    if (is.null(loc)) return(NA_real_)
                    p <- loc$parameters
                    if (loc$distribution == "beta") return(unname(p$shape1 / (p$shape1 + p$shape2)))
                    if (loc$distribution == "truncnorm" || loc$distribution == "normal") return(unname(p$mean))
                    NA_real_
               },
               numeric(1)
          )
     },
     numeric(6)
)
# .prop_means is a 6 x length(j) matrix with rownames "prop_X_initial"
rownames(.prop_means) <- .compartment_props
.prop_means <- t(.prop_means)            # now length(j) x 6
.prop_means[is.na(.prop_means)] <- 0     # any missing iso => 0 for that compartment

# Per-row normalisation so each iso sums to 1 (independent Beta priors do not
# sum to 1 exactly; ETH typical row sum ~0.96-1.01).
.row_sums <- rowSums(.prop_means)
.prop_means <- .prop_means / .row_sums

# Hamilton (largest-remainder) apportionment to integer counts per iso.
.counts_mat <- matrix(0L, nrow = length(j), ncol = 6,
                      dimnames = list(j, .compartment_props))
for (jj in seq_along(j)) {
     target  <- .prop_means[jj, ] * N_j[jj]
     base    <- floor(target)
     rem     <- target - base
     deficit <- as.integer(N_j[jj] - sum(base))
     if (deficit > 0) {
          ord <- order(rem, decreasing = TRUE)
          base[ord[seq_len(deficit)]] <- base[ord[seq_len(deficit)]] + 1
     } else if (deficit < 0) {
          ord <- order(rem, decreasing = FALSE)
          take <- min(-deficit, length(ord))
          base[ord[seq_len(take)]] <- base[ord[seq_len(take)]] - 1
     }
     .counts_mat[jj, ] <- as.integer(base)
}
stopifnot(all(.counts_mat >= 0L))
stopifnot(all(rowSums(.counts_mat) == N_j))

S_j  <- .counts_mat[, "S"]
V1_j <- .counts_mat[, "V1"]
V2_j <- .counts_mat[, "V2"]
E_j  <- .counts_mat[, "E"]
I_j  <- .counts_mat[, "I"]
R_j  <- .counts_mat[, "R"]
prop_S_initial  <- .prop_means[, "S"]
prop_V1_initial <- .prop_means[, "V1"]
prop_V2_initial <- .prop_means[, "V2"]
prop_E_initial  <- .prop_means[, "E"]
prop_I_initial  <- .prop_means[, "I"]
prop_R_initial  <- .prop_means[, "R"]

message("Get birth rate of each location (b_j)")
tmp <- read.csv(file.path(PATHS$MODEL_INPUT, 'param_b_birth_rate.csv'))
tmp$t <- as.Date(tmp$t)
tmp <- tmp[tmp$j %in% j,]
tmp <- tmp[tmp$t >= date_start & tmp$t <= date_stop,]
b_jt <- reshape2::acast(tmp, j ~ t, value.var = "parameter_value")
sel <- match(j, row.names(b_jt))
b_jt <- b_jt[sel,]
sel <- match(t, colnames(b_jt))
b_jt <- b_jt[,sel]

message("Get death rate of each location (d_j)")
tmp <- read.csv(file.path(PATHS$MODEL_INPUT, 'param_d_death_rate.csv'))
tmp$t <- as.Date(tmp$t)
tmp <- tmp[tmp$j %in% j,]
tmp <- tmp[tmp$t >= date_start & tmp$t <= date_stop,]
d_jt <- reshape2::acast(tmp, j ~ t, value.var = "parameter_value")
sel <- match(j, row.names(d_jt))
d_jt <- d_jt[sel,]
sel <- match(t, colnames(d_jt))
d_jt <- d_jt[,sel]

# Reported case fatality ratio mu_jt (v5.0, MOSAIC v0.96.0): a [nL x nT] matrix,
# one value per location and day, that the engine reads directly and converts each
# tick to the probability that a symptomatic onset is fatal. It is built from the
# per-location, per-year WHO-annual estimates written by est_CFR_hierarchical()
# (model/input/cfr_hierarchical_estimates.csv), interpolated on the logit scale
# between mid-years. Days past the last estimated year carry that year's value
# forward: in rolling-origin testing, carrying the last year forward predicted the
# next one and two years better than projecting the GAM.
#
# This replaces the B2.1 derivation (mu_j_baseline = CFR_target x chain anchor)
# and mu_j_epidemic_factor, which the engine no longer has.
cfr_est_file <- file.path(PATHS$MODEL_INPUT, "cfr_hierarchical_estimates.csv")
if (!file.exists(cfr_est_file)) {
     stop("cfr_hierarchical_estimates.csv not found in ", PATHS$MODEL_INPUT,
          ". Run est_CFR_hierarchical(PATHS) first.")
}
cfr_est <- read.csv(cfr_est_file, stringsAsFactors = FALSE)
mu_jt <- MOSAIC::make_mu_jt(cfr_est, location_name = j,
                            date_start = date_start, date_stop = date_stop)
message(sprintf("mu_jt built from WHO-annual GAM estimates: %d locations x %d days, median %.2f%% (range %.2f%%-%.2f%%)",
                nrow(mu_jt), ncol(mu_jt), 100 * stats::median(mu_jt), 100 * min(mu_jt), 100 * max(mu_jt)))

#####
# Vaccination: first-dose (nu_1_jt) and second-dose (nu_2_jt) rates
#####

# nu is the doses shipped per request, spread over days at max_rate_per_day by
# est_vaccination_rate(), which also splits each day into first and second
# doses from the request's GTFCC Round events (process_GTFCC_vaccination_data():
# shipped doses divided across the rounds in proportion to the doses
# administered in each, first round before second round). nu_1 + nu_2 = nu on
# every location-day; .vacc_nu_jt() refuses files that are out of step. Doses
# with no round information (a GTFCC request without Round events, or a
# WHO-only shipment) count as first doses. In the engine, second doses move
# phi_2 of their recipients from V1 to V2, capped at the V1 stock, so a
# two-dose campaign immunises its first-round recipients once and upgrades them
# to two-dose waning, instead of counting both rounds as new first doses as
# configs up to v6.2 did (nu_2_jt was 0 everywhere). Second rounds are pre-2023
# campaigns: the ICG suspended the two-dose regimen for outbreak response in
# October 2022 (WHO news release, 19 Oct 2022), and in the GTFCC log at
# ees-cholera-mapping 780eb54 no request delivered from 2023 has a second round,
# so nu_2_jt is zero over a 2023+ window.
message("Add first- and second-dose vaccination rates over time for each location (nu_1_jt, nu_2_jt)")
nu_suffix <- c("GTFCC_WHO", "WHO", "GTFCC")
nu_suffix <- nu_suffix[file.exists(file.path(PATHS$MODEL_INPUT,
                                             sprintf("param_nu_vaccination_rate_%s.csv", nu_suffix)))]
if (!length(nu_suffix)) {
     stop("No param_nu_vaccination_rate_<GTFCC_WHO|WHO|GTFCC>.csv in ", PATHS$MODEL_INPUT,
          ". Run est_vaccination_rate() first.")
}
nu_suffix <- nu_suffix[1]
if (nu_suffix != "GTFCC_WHO") warning("Using the ", nu_suffix, " vaccination rate files, not GTFCC_WHO")
nu_doses <- MOSAIC:::.vacc_nu_jt(PATHS$MODEL_INPUT, nu_suffix, location_name = j, dates = t)
nu_1_jt <- nu_doses$nu_1_jt
nu_2_jt <- nu_doses$nu_2_jt
message(sprintf("nu from the %s files: %s first doses and %s second doses over the window (%.1f%% second)",
                nu_suffix, format(sum(nu_1_jt), big.mark = ","), format(sum(nu_2_jt), big.mark = ","),
                100 * sum(nu_2_jt) / max(1, sum(nu_1_jt) + sum(nu_2_jt))))

message("Add fourier params for seasonal force of infection")
tmp <- read.csv(file.path(PATHS$MODEL_INPUT, "param_seasonal_dynamics.csv"))

sel <- tmp$response == 'cases' & tmp$parameter == 'a_1'
a1 <- tmp$mean[sel]
names(a1) <- tmp$country_iso_code[sel]
a1 <- a1[match(j, names(a1))]

sel <- tmp$response == 'cases' & tmp$parameter == 'a_2'
a2 <- tmp$mean[sel]
names(a2) <- tmp$country_iso_code[sel]
a2 <- a2[match(j, names(a2))]

sel <- tmp$response == 'cases' & tmp$parameter == 'b_1'
b1 <- tmp$mean[sel]
names(b1) <- tmp$country_iso_code[sel]
b1 <- b1[match(j, names(b1))]

sel <- tmp$response == 'cases' & tmp$parameter == 'b_2'
b2 <- tmp$mean[sel]
names(b2) <- tmp$country_iso_code[sel]
b2 <- b2[match(j, names(b2))]

# The engine's human-transmission envelope is beta_j0_hum * (1 + f(t)) with a
# negative rate clamped to zero, so 1 + f(t) must stay positive over the year
# (est_seasonal_dynamics() >= v0.100.0 keeps min(1 + f) >= 0.1).
.season_min_envelope <- vapply(seq_along(j), function(i) {
     1 + MOSAIC:::.seasonal_envelope_min(c(a_1 = a1[[i]], b_1 = b1[[i]], a_2 = a2[[i]], b_2 = b2[[i]]))
}, numeric(1))
if (any(.season_min_envelope <= 0)) {
     stop("param_seasonal_dynamics.csv gives a non-positive transmission envelope min(1 + f(t)) for: ",
          paste(j[.season_min_envelope <= 0], collapse = ", "),
          ". Re-run est_seasonal_dynamics() (>= v0.100.0).")
}


message("Get departure probability of each location (tau_j)")
# OVERLAND ONLY (user decision, 2026-09-18): tau_i is the evidence-anchored
# overland departure probability from est_overland_tau_prior() -- the SAME
# object data-raw/make_priors_default.R centres the tau_i lognormal prior on.
# Sourcing both from param_tau_departure_overland.csv makes config_default$tau_i
# identical to the prior median for all 40 locations. Sourcing tau_i from the
# additive blend instead put the config ~18% above its own prior median (up to
# 2.2x for air-dominated NAM/GAB/BWA).
# The gravity kernel (mobility_gamma/mobility_omega) DELIBERATELY stays on the
# BLEND fit -- see the gravity block below. Do not "make them consistent".
# Ordered fallbacks: overland -> additive blend -> air-only.
.tau_ov <- file.path(PATHS$MODEL_INPUT, 'param_tau_departure_overland.csv')
if (file.exists(.tau_ov)) {
     message("config tau_i source: ", basename(.tau_ov))
     tmp <- read.csv(.tau_ov, stringsAsFactors = FALSE)
     tau_i <- tmp$tau_daily[match(j, tmp$iso_code)]
     names(tau_i) <- j
} else {
     .tau_f <- file.path(PATHS$MODEL_INPUT, 'param_tau_departure_blend.csv')
     if (!file.exists(.tau_f)) .tau_f <- file.path(PATHS$MODEL_INPUT, 'param_tau_departure.csv')
     message("config tau_i source: ", basename(.tau_f))
     tmp <- read.csv(.tau_f)
     tmp <- tmp[tmp$i %in% j,]
     tmp <- tmp[tmp$parameter_name =='mean',]
     sel <- match(j, tmp$i)
     tau_i <- tmp$parameter_value[sel]
     names(tau_i) <- tmp$i[sel]
}
stopifnot(length(tau_i) == length(j), all(is.finite(tau_i)), all(tau_i > 0 & tau_i < 1))

message("Gravity model parameters")
tmp <- read.csv(file.path(PATHS$MODEL_INPUT, "mobility_lon_lat.csv"))

lon <- tmp$lon
names(lon) <- tmp$iso3
longitude <- lon[match(j, names(lon))]

lat <- tmp$lat
names(lat) <- tmp$iso3
latitude <- lat[match(j, names(lat))]

.grav_f <- file.path(PATHS$MODEL_INPUT, "mobility_gravity_params_blend.csv")
if (!file.exists(.grav_f)) {
     warning("mobility_gravity_params_blend.csv not found; falling back to the air-only ",
             "mobility_gravity_params.csv, so the config kernel will not match the ",
             "priors_default blend modes. Re-run est_mobility(od_source = \"blend\").",
             immediate. = TRUE)
     .grav_f <- file.path(PATHS$MODEL_INPUT, "mobility_gravity_params.csv")
}
message("config gravity source: ", basename(.grav_f))
tmp <- read.csv(.grav_f, row.names=1)
mobility_omega <- tmp['omega', 'mean']
mobility_gamma <- tmp['gamma', 'mean']

message("Get WASH variables for each location")
tmp <- read.csv(file.path(PATHS$MODEL_INPUT, 'param_theta_WASH.csv'))
tmp <- tmp[tmp$j %in% j,]
sel <- match(j, tmp$j)
theta_j <- tmp$parameter_value[sel]
names(theta_j) <- tmp$j[sel]

message("Calculate transmission parameters from beta_j0_tot and p_beta")

# Set default values for beta_j0_tot and p_beta
# beta_j0_tot is sourced PER-COUNTRY from the priors_default location medians
# (lognormal median = exp(meanlog)) rather than a single global constant, mirroring
# how the initial conditions are sourced above. This lets
# country-specific transmission defaults (e.g. ETH, recentred to 1.75e-6) flow
# through automatically; every other country still resolves to its prior median (2e-5).
p_beta_default <- 0.33        # Proportion human transmission (matching prior mode)

# Create vectors for all locations
beta_j0_tot <- vapply(j, function(iso) {
     exp(priors_default$parameters_location$beta_j0_tot$location[[iso]]$parameters$meanlog)
}, numeric(1))
p_beta <- rep(p_beta_default, length(j))

# Calculate derived parameters
beta_j0_hum <- p_beta * beta_j0_tot        # Human transmission component
beta_j0_env <- (1 - p_beta) * beta_j0_tot  # Environmental transmission component

# Add names for clarity
names(beta_j0_tot) <- j
names(p_beta) <- j
names(beta_j0_hum) <- j
names(beta_j0_env) <- j

# Print summary for verification
message(sprintf("  beta_j0_tot = %.2e to %.2e (per-country prior medians; ETH = %.2e)",
                min(beta_j0_tot), max(beta_j0_tot), beta_j0_tot[["ETH"]]))
message(sprintf("  p_beta = %.2f (%.0f%% human, %.0f%% environmental)",
                p_beta_default, p_beta_default * 100, (1 - p_beta_default) * 100))
message(sprintf("  beta_j0_hum = %.2e (human component)", beta_j0_hum[1]))
message(sprintf("  beta_j0_env = %.2e (environmental component)", beta_j0_env[1]))




message("Get environmental suitability (psi) for each location")

tmp <- read.csv(file.path(PATHS$MODEL_INPUT, 'pred_psi_suitability_day.csv'))
tmp$date <- as.Date(tmp$date)
tmp <- tmp[tmp$iso_code %in% j,]
tmp <- tmp[tmp$date >= date_start & tmp$date <= date_stop,]
if (!"psi" %in% names(tmp))
     stop("pred_psi_suitability_day.csv lacks the canonical `psi` column; regenerate it with est_suitability() v0.34+ BEFORE rebuilding config_default (Option A output schema).")
psi_jt <- reshape2::acast(tmp, iso_code ~ date, value.var = "psi", fun.aggregate = mean)
sel <- match(j, row.names(psi_jt))
psi_jt <- psi_jt[sel,]

message("Get reported cholera cases and deaths data (for model fitting)")
# Source: the MULTI-SOURCE combined daily file (WHO+JHU+AI observed/documented_zero
# +SUPP), produced by process_cholera_surveillance_data(include_ai=TRUE) -> daily
# downscale. Replaces the legacy WHO-only daily file. Trust tiering is applied
# upstream: only assumed_zero rows are NA-blanked; fourier_* reconstructions have
# been kept since v0.47.1 (scored at their reduced confidence_weight, and since
# v0.101.0 only where they fill the gap to the WHO account of the year);
# documented_zero arrives as a real 0. Each cell carries a per-week
# confidence_weight in [0,1] (direct WHO/JHU/SUPP = 1.0), which we assemble into
# parallel weight matrices below.
df_daily <- read.csv(file.path(PATHS$DATA_CHOLERA_DAILY, "cholera_surveillance_daily_combined.csv"), stringsAsFactors = FALSE)
df_daily$date <- as.Date(df_daily$date)
if (!"confidence_weight" %in% names(df_daily)) {
     stop("combined daily file lacks confidence_weight; regenerate via process_cholera_surveillance_data(include_ai = TRUE) on the current package version before rebuilding config_default.")
}

mat_cases  <- matrix(NA_real_, nrow = length(j), ncol = length(t), dimnames = list(j, as.character(t)))
mat_deaths <- matrix(NA_real_, nrow = length(j), ncol = length(t), dimnames = list(j, as.character(t)))
# Parallel per-observation confidence-weight matrices (same shape/alignment as the
# fit matrices). CONSUMED downstream: run_MOSAIC() passes them to
# calc_model_likelihood() as weights_obs_cases / weights_obs_deaths (and on to
# est_nb_dispersion()), and calc_log_likelihood_deaths_integrated() reads
# config$reported_deaths_weight directly. A matched cell with a missing weight defaults to 1.0 (full
# trust), so a WHO-only / no-AI rebuild yields all-ones weights and is
# likelihood-identical to the legacy config. Dimnames are dropped on JSON
# round-trip -- consumers must align positionally (rows = location_name, cols = t).
mat_cases_weight  <- matrix(NA_real_, nrow = length(j), ncol = length(t), dimnames = list(j, as.character(t)))
mat_deaths_weight <- matrix(NA_real_, nrow = length(j), ncol = length(t), dimnames = list(j, as.character(t)))
# Surveillance trust tier of each observed week (MOSAIC v0.101.0), from the
# combined file's disaggregation_method via the combiner's own rule
# (.surveillance_tier): 1 observed (direct WHO/JHU/SUPP rows, AI observed and
# documented_zero), 2 reconstructed (who_catchup_*: a WHO multi-week report
# spread over its weeks), 3 imputed (fourier_* and other modelled rows); NA where
# the week has neither a case nor a death count. est_nb_dispersion() estimates
# the cases dispersion from tier-1 weeks only. The confidence weights cannot
# stand in for it: AI observed weeks and documented zeros carry 0.8-0.95 while
# spread WHO reports carry 0.5-0.9.
if (!"disaggregation_method" %in% names(df_daily)) {
     stop("combined daily file lacks disaggregation_method; regenerate via process_cholera_surveillance_data(include_ai = TRUE) before rebuilding config_default.")
}
mat_tier <- matrix(NA_integer_, nrow = length(j), ncol = length(t), dimnames = list(j, as.character(t)))

for (i in seq_along(j)) {

     iso_data <- df_daily[df_daily$iso_code == j[i], ]
     if (nrow(iso_data) == 0) next

     # The combined daily file is square (one row per iso-day), but guard against
     # any duplicate (iso, date) by preferring the row that carries a non-NA value.
     oc <- order(iso_data$date, is.na(iso_data$cases))
     od <- order(iso_data$date, is.na(iso_data$deaths))
     mc <- match(t, iso_data$date[oc])
     md <- match(t, iso_data$date[od])

     cases_v  <- iso_data$cases[oc][mc]
     deaths_v <- iso_data$deaths[od][md]
     cw_c     <- iso_data$confidence_weight[oc][mc]
     cw_d     <- iso_data$confidence_weight[od][md]

     mat_cases[i, ]  <- cases_v
     mat_deaths[i, ] <- deaths_v
     # Weight present iff the value is present; matched-but-unweighted -> 1.0.
     mat_cases_weight[i, ]  <- ifelse(is.na(cases_v),  NA_real_, ifelse(is.na(cw_c), 1.0, cw_c))
     mat_deaths_weight[i, ] <- ifelse(is.na(deaths_v), NA_real_, ifelse(is.na(cw_d), 1.0, cw_d))
     # One surveillance row supplies each week, so its tier holds for both
     # channels; present wherever either count is.
     tier_v <- MOSAIC:::.surveillance_tier(iso_data$disaggregation_method[oc][mc])
     mat_tier[i, ] <- ifelse(is.na(cases_v) & is.na(deaths_v), NA_integer_, tier_v)
}

message("Define a base list of arguments (all parameters that are common to all calls)")

# Calculate initial condition proportions from counts
prop_S_initial <- S_j / N_j
prop_E_initial <- E_j / N_j
prop_I_initial <- I_j / N_j
prop_R_initial <- R_j / N_j
prop_V1_initial <- V1_j / N_j
prop_V2_initial <- V2_j / N_j

# Add names to match location names
names(prop_S_initial) <- j
names(prop_E_initial) <- j
names(prop_I_initial) <- j
names(prop_R_initial) <- j
names(prop_V1_initial) <- j
names(prop_V2_initial) <- j

# Validate that proportions sum to 1.0 for each location
for (i in seq_along(j)) {
    prop_sum <- prop_S_initial[i] + prop_E_initial[i] + prop_I_initial[i] +
                prop_R_initial[i] + prop_V1_initial[i] + prop_V2_initial[i]
    if (abs(prop_sum - 1.0) > 1e-6) {
        warning(sprintf("Initial condition proportions don't sum to 1.0 for %s: sum = %.6f",
                       j[i], prop_sum))
    }
}

message("Initial condition proportions calculated and validated")

# zeta_ratio pinned default = MEDIAN of the shipped zeta_ratio prior, taken
# from priors_default so the two cannot drift: the truncated median when the
# prior carries lower/upper (priors >= v17.0: lower = 1, median ~185), else
# exp(meanlog) (v16.1 untruncated: ~74.7). Median, not mode: the direct-channel
# mode is pathological for sdlog ~4.4. Used for the zeta_ratio field and the
# derived zeta_2 placeholder.
.zr_prior <- MOSAIC::priors_default$parameters_global$zeta_ratio$parameters
.zeta_ratio_default <- MOSAIC:::.qlnorm_trunc(0.5, .zr_prior$meanlog, .zr_prior$sdlog,
                                              .zr_prior$lower, .zr_prior$upper)
stopifnot(is.finite(.zeta_ratio_default), .zeta_ratio_default >= 1)

default_args <- list(
     output_file_path = NULL, # Return config back to R env in list form (nothing written to file)
     seed = 123,
     date_start = date_start,
     date_stop = date_stop,
     location_name = j,
     N_j_initial = N_j,
     S_j_initial = S_j,
     E_j_initial = E_j,
     I_j_initial = I_j,
     R_j_initial = R_j,
     V1_j_initial = V1_j,
     V2_j_initial = V2_j,
     prop_S_initial = prop_S_initial,
     prop_E_initial = prop_E_initial,
     prop_I_initial = prop_I_initial,
     prop_R_initial = prop_R_initial,
     prop_V1_initial = prop_V1_initial,
     prop_V2_initial = prop_V2_initial,
     b_jt = b_jt,
     d_jt = d_jt,
     nu_1_jt = nu_1_jt,
     nu_2_jt = nu_2_jt,
     phi_1 = 0.788,               # Mode of Beta(91.84, 25.49); Xu et al. 2024 fit
     phi_2 = 0.788,               # Mode of Beta(206.96, 56.53); constrained phi_2 >= phi_1
     omega_1 = 0.000705,          # Mode of Gamma(23.33, 31693.83); half-life ~2.7 years
     omega_2 = 0.000358,          # Mode of Gamma(2.69, 4720.84); half-life ~5.3 years
     nu_jt_sources = c("S", "E", "Isym", "Iasym", "R"),
     iota = 1/1.4,
     gamma_1 = 0.1,       # Symptomatic recovery ~10 days (was 0.2 = 5 days; posteriors consistently 0.09-0.11)
     gamma_2 = 0.5,       # Asymptomatic recovery ~2 days (was 0.1 = 10 days; posteriors consistently 0.34-0.78)
     epsilon = 0.0003,
     mu_jt = mu_jt,       # Reported CFR by location and day (v5.0): WHO-annual GAM, see above
     # Mean of the priors_default sigma Beta (est_symptomatic_prop() fit, ~0.35
     # at priors v17.0; the old fixed 0.25 matched the pre-v17.0 Beta(4.30, 13.51)).
     sigma = with(MOSAIC::priors_default$parameters_global$sigma$parameters,
                  shape1 / (shape1 + shape2)),
     # Case reporting parameters (surveillance PPV, regime-dependent)
     rho = 0.423,               # Care-seeking rate (mean of Beta(5.38, 7.10) prior, Wiens et al. 2025 RE pool of general + severe/cholera strata; see R/get_rho_care_seeking_params.R)
     # Death detection rate: probability a true cholera death is captured by
     # surveillance. Mean of the informative Beta(36.95, 51.02) prior (RE
     # meta-analysis of Routh 2017 Tanzania, Shikanga 2009 Kenya, Bwire 2013
     # Uganda; claude/rho_deaths_research/SYNTHESIS_REPORT.md). PINNED
     # (sample_rho_deaths defaults FALSE): the engine's per-onset fatality
     # probability is mu_jt * rho / (rho_deaths * chi_epidemic) and reported deaths
     # are thinned by rho_deaths, so it cancels exactly from reported deaths and
     # sets only true (unreported) deaths.
     rho_deaths = 0.42,
     chi_endemic = 0.50,          # PPV among suspected cases during endemic periods (50%)
     chi_epidemic = 0.75,         # PPV among suspected cases during epidemic periods (75%)
     # Per-iso Isym/N threshold for the case-reporting PPV switch. Seeded from
     # the per-iso Truncnorm prior means (range ~7e-7 NGA to ~8.7e-6 COD;
     # ETH 3.36e-6). The earlier flat 1/10000 default was 12-142x above the
     # prior means and never let any country enter "epidemic" phase, so
     # chi_epidemic never engaged.
     epidemic_threshold = vapply(
          j,
          function(iso) {
               loc <- priors_default$parameters_location$epidemic_threshold$location[[iso]]
               if (is.null(loc)) return(1e-5)
               p <- loc$parameters
               if (loc$distribution == "truncnorm") return(unname(p$mean))
               if (loc$distribution == "gamma")     return(unname(p$shape / p$rate))
               1e-5
          },
          numeric(1)
     ),
     delta_reporting_cases = 0,   # Symptom-onset-to-case reporting delay (was 2; posteriors collapse to 0 in every test, KL=14.2)
     longitude         = longitude,
     latitude          = latitude,
     mobility_omega    = mobility_omega,
     mobility_gamma    = mobility_gamma,
     tau_i             = tau_i,
     beta_j0_tot = beta_j0_tot,      # Total transmission rate (optional, but included when available)
     p_beta = p_beta,                # Proportion of human transmission (optional, but included when available)
     beta_j0_hum = beta_j0_hum,      # Human transmission component (calculated from beta_j0_tot * p_beta)
     a_1_j = a1,
     a_2_j = a2,
     b_1_j = b1,
     b_2_j = b2,
     p     = 365,
     # alpha_1 is now stored as a length-nL vector (D1, v4.7): rep(0.27, nL).
     # The engine is dual-mode (broadcasts a scalar or applies a length-nL vector
     # elementwise per patch), and the priors are now PER-LOCATION (priors_default
     # v15.16), so the seed config must carry a length-nL alpha_1 to make the
     # convert_matrix_to_config round-trip robust (a scalar seed silently drops
     # per-ISO values for idx 2..nL). 0.27 = the validated global center; posteriors
     # consistently 0.21-0.40 (freezing at 0.975 destroys fit -- test_25).
     alpha_1 = rep(0.27, length(j)),
     alpha_2 = 0.50,       # Frequency-driven transmission GLOBAL SCALAR (was 0.33; posteriors 0.22-0.70, prior median 0.50)
     beta_j0_env = beta_j0_env,      # UPDATED: Now calculated from beta_j0_tot * (1 - p_beta)
     theta_j = theta_j,
     # psi_star calibration parameters. psi_star_b default is sourced from the
     # priors_default psi_star_b mean (re-centered +1.0 for the D-scale psi in
     # v15.11) so the default config's psi* level matches the calibration prior
     # center; a/z/k keep the neutral no-transformation defaults.
     psi_star_a = setNames(rep(1.0, length(j)), j),    # Default: no shape/gain transformation
     psi_star_b = setNames(vapply(j, function(iso) {
          loc <- priors_default$parameters_location$psi_star_b$location[[iso]]
          if (is.null(loc)) 0.0 else as.numeric(loc$parameters$mean)
     }, numeric(1)), j),                               # Sourced from priors_default psi_star_b mean (D-scale, +1.0)
     psi_star_z = setNames(rep(1.0, length(j)), j),    # Default: no smoothing
     psi_star_k = setNames(rep(0.0, length(j)), j),    # Default: no time offset
     psi_jt = psi_jt,
     # v0.29.0: zeta_* defaults rescaled from the Frame-B 70k/300 scale to the
     # biological scale implied by the literature meta-analysis in
     # est_zeta_1_prior() / est_zeta_2_prior() / est_zeta_ratio_prior().
     # zeta_1 uses the MODE of its lognormal prior (mode = exp(meanlog - sdlog^2));
     # zeta_ratio uses the MEDIAN of its direct-channel prior -- of the
     # TRUNCATED distribution when the prior carries lower = 1 (~185, not the
     # untruncated exp(meanlog) ~75) -- because the direct-channel mode is
     # pathological (~1e-7) due to the wide sdlog (4.39) reflecting the 5-OOM
     # tension in direct literature. See .zeta_ratio_default above.
     zeta_1 = 3.29e8,      # Mode of LN(25.654, 2.458) = exp(25.654 - 2.458^2)
                           # v0.29.1 bias-corrected (was 2.148e10 under
                           # pre-correction LN(26.641, 1.688); new value
                           # reflects lowered V_sev 8->4 L/day, lowered mild
                           # concentration 10^6->10^5, removed derived pool
                           # row, downweighted review sources)
     zeta_2 = 3.29e8 / .zeta_ratio_default,  # DERIVED at sampling time (= zeta_1/zeta_ratio);
                                   # tracked placeholder so run_MOSAIC's
                                   # param_names_all picks it up for samples.parquet.
                                   # = z1_mode / z_ratio_median (~1.8e6 truncated)
     kappa = 10^6,
     decay_days_short = 16, # Min V. cholerae survival (was 3; prior median 16, posteriors 15-48)
     decay_days_long = 200, # Max V. cholerae survival; DERIVED at sampling time from short + spread
     decay_shape_1 = 5,
     decay_shape_2 = 2.5,
     reported_cases = mat_cases,
     reported_deaths = mat_deaths,
     # Observed epidemic peaks (iso_code, peak_date) shipped with the default config
     # so the Python likelihood port (laser-cholera#47) can compute the peak-timing
     # and peak-magnitude shape terms without an extra runtime injection. Slim
     # 2-column form matches what calc_model_likelihood() consumes on both sides.
     # Filtered to locations actually present in this config AND to the
     # configured [date_start, date_stop] window -- peaks outside this window
     # would silently snap to t=1 or t=N in the peak-shape likelihood terms
     # (see .filter_epidemic_peaks docstring).
     epidemic_peaks = local({
          ep <- MOSAIC:::.filter_epidemic_peaks(
               MOSAIC::epidemic_peaks,
               date_start     = date_start,
               date_stop      = date_stop,
               location_names = j
          )
          data.frame(
               iso_code  = as.character(ep$iso_code),
               peak_date = as.character(ep$peak_date),
               stringsAsFactors = FALSE
          )
     })
)

config_default <- do.call(make_simulation_config, default_args)

# Derived-parameter tracking fields not accepted by make_simulation_config signature.
# Injected into config_default (and the written JSONs below) so run_MOSAIC's
# convert_config_to_matrix picks them up for samples.parquet.
.decay_days_spread_default <- 184   # Spread; prior median 180 (decay_days_long = short + spread)

# Add metadata for provenance tracking
config_default$metadata <- list(
     version = "6.2",
     date = as.character(Sys.Date()),
     description = "Default simulation configuration for MOSAIC cholera metapopulation model. v6.2 (2026-10-03): REBUILD (MOSAIC v0.102.0). Sources priors_default v17.1, not rebuilt (the priors do not read psi). Only the psi_jt rows of CIV, GMB, TGO and UGA change: psi is C3 with its bias correction re-applied under the v0.102.0 rule of calibrate_psi_predictions(), which falls back to identity where a per-country fit would shrink psi's logit-scale amplitude below 0.5x. The four sat on the old 0.5x floor blend, a map set by the guard constants rather than the data, so their psi is now the LSTM's own pred_smooth; mean psi over the window CIV 0.011 -> 0.029, GMB 0.007 -> 0.010, TGO 0.256 -> 0.068, UGA 0.091 -> 0.123. Every other field is identical to v6.1. Corrected in the v6.1 entry below: the deaths dispersion falls back to every scored week where tier-1 weeks are too few. v6.1 (2026-10-01): REBUILD (MOSAIC v0.101.0). Sources priors_default v17.1. Window unchanged, 2023-01-01 to 2027-04-29, on the production psi refit C3 (est_suitability() lstm_v2, 10 seeds 11-110, trained on the corrected surveillance with the Nino4 NMME gap-fill ENSO input; psi_jt replaced: median per-location r 0.92 against v6.0's psi over the window, median mean |diff| 0.042; mean ratio CIV 0.04, ZAF 0.50, GHA 1.71, TGO 2.42, SWZ 3.17). NEW reported_tier [location x day], the surveillance trust tier of each observed day's week: 1 observed, 2 reconstructed (a WHO multi-week report spread over its weeks), 3 imputed, NA where neither cases nor deaths are observed (33,209 / 1,365 / 638 location-days); read by the dispersion estimates (the cases dispersion uses tier-1 weeks only; the deaths dispersion uses tier-1 weeks where they are enough and falls back to every scored week where they are too few) and ignored by the engine. reported_cases/reported_deaths and their weights from the corrected combined surveillance (MOSAIC-data 922ef89: WHO multi-week reports spread, cross-source double counting removed, imputed rows only filling the gap to the WHO account of the year, documented absences, curated windows including the shaped, report-dated ZAF 2023 outbreak, Monday-Sunday WHO weeks): observed case cells 36,984 -> 35,212 (490 NA -> value, 2,262 value -> NA) and 1,627 values change; window cases 845,234 -> 832,887 (ZAF 3,086 -> 1,404, CIV 1,016 -> 505, SSD -13%). epidemic_peaks 62 -> 64 rows (added ZAF 2023-05-29, GHA 2024-12-08, CIV 2025-07-07, TCD 2026-08-23, COD 2026-08-30; removed ZAF 2023-08-31, GHA 2024-11-18, COD 2026-08-02). Initial conditions are the v17.1 prior means: SSD, TZA, UGA, ZAF and ZWE are quiet starts (E + I 45 -> 229, 233 -> 1,338, 4 -> 973, 94 -> 1,264, 551 -> 326), E + I follow their window's cases in AGO, BDI, BEN, COD, ETH, KEN, MWI and ZMB (ZMB 656 -> 37, KEN 1,266 -> 1,694), and R, S, V1 and V2 move by at most 0.42% through the v17.1 R and S priors and the row normalisation. epidemic_threshold in 18 locations from priors v17.1 (ZAF 1.18e-5 -> 1.07e-7, off the Zheng fallback; the others x0.61 to x1.28). Seasonal a/b in 16 locations from param_seasonal_dynamics.csv re-estimated on the corrected surveillance (ZAF, NAM and CIV move most; the others by <= 0.03). UNCHANGED: N_j, b_jt, d_jt, nu_1_jt/nu_2_jt, mu_jt, beta_j0_*, psi_star_*, tau_i, mobility, theta_j and every other point parameter. v6.0 (2026-09-30): DEEP-REVIEW REBUILD (MOSAIC v0.100.1). Sources priors_default v17.0. Window 2023-01-01 to 2027-04-29 (was 2027-02-04) on the 10-seed production psi (psi_jt replaced). sigma is the mean of the v17.0 sigma prior (0.25 -> 0.345); zeta_ratio is the median of the truncated zeta_ratio prior (74.69 -> ~185) and zeta_2 = zeta_1 / zeta_ratio (4.4e6 -> 1.78e6). Initial conditions are the means of the v17.0 initial-condition priors, estimated at date_start (R ~25x lower, E/I from the 28-day window straddling 2023-01-01; quiet-start locations, with cases later in the window but none at the start or too few for one expected initial infection, carry the weak Beta(1, 1e5) seeding prior's mean, ~2e-5 N in E + I). Seasonal a/b from the regenerated param_seasonal_dynamics.csv (positive envelope at these point values; builder stop). nu_1_jt from the deduplicated GTFCC/WHO OCV campaign file (2023-27 doses 96.3M -> 85.2M). d_jt from regenerated demographic inputs (<= 0.02%); theta_j ERI/BWA. reported_cases/reported_deaths and their weights from the corrected combined surveillance (MOSAIC-data 64b69ab: WHO W53/MMWR re-dating, JHU phantom weeks dropped, observed-over-imputed combiner): over the v5.1 window 304 case cells change NA status (119 NA -> value, 185 value -> NA) and 2,437 of 36,865 observed case cells change value (LBR +14%, ZMB +3% cases). epidemic_peaks 54 -> 62 rows (peaks detected on observed weeks only). UNCHANGED: tau_i (overland departure file), mobility_gamma/mobility_omega (blend gravity fit), mu_jt values, b_jt, beta_j0_*, p_beta, rho, chi, all other point parameters. v5.1 (2026-09-29): mu_jt rebuilt from the revised est_CFR_hierarchical() (MOSAIC v0.97.0): a calendar year still in progress when its WHO dashboard snapshot was taken is excluded from the GAM (its deaths lag its cases, and the same weeks are scored by the calibration), and each country's trend is held flat after that country's own last WHO-annual year instead of extrapolating (SOM, BFA, LBR, BEN end in 2022). 34 of 40 locations move more than 5% in 2026 (NGA +33%); the geometric mean against the scored 2023-26 CFR moves from 0.93 to 0.97. No other field changes. v5.0 (2026-09-28): CFR v2.1 MORTALITY MODEL (MOSAIC v0.96.0). NEW mu_jt: the reported case fatality ratio as a [nL x nT] matrix, one value per location and day, built by make_mu_jt() from the per-location, per-year WHO-annual estimates of est_CFR_hierarchical() (model/input/cfr_hierarchical_estimates.csv; binomial GAM on all WHO-annual years 1970-2026 with a global trend, country intercepts, per-country drift and a country-year random effect), interpolated on the logit scale between mid-years and carried flat past the last estimated year. The engine converts it each tick to the probability that a symptomatic onset is fatal, mu_jt * rho / (rho_deaths * chi_epidemic), draws deaths at onset, and reports them on the case lag. REMOVED: mu_j_baseline, mu_j_epidemic_factor, CFR_target and delta_reporting_deaths (the daily hazard on the symptomatic stock, its epidemic escalation, its B2.1 derivation target and its separate post-mortem lag). In calibration the reported CFR is integrated out per simulated path around this mu_jt (see priors_default v16.0 mu_jt). Sources priors_default v16.0. No window/psi/surveillance/transmission change vs v4.9. v4.9 (2026-09-28): SCHEMA REMOVAL -- the per-location mu_j_slope field is no longer built, validated, or shipped (CFR restructure R3, MOSAIC v0.95.0). The engine term it fed, (1 + mu_j_slope * tick/nticks), is deleted from R/sim_components.R, so mu_jt is now TWO multiplicative components (per-patch baseline x epidemic escalation) rather than three. The linear-in-time trend was not estimable from the deaths series (~74,000 deaths needed for posterior shrinkage 0.5 against 4,139 in the largest shipped series and ~15,600 pooled over all 40 locations; measured posterior/prior SD 0.969), it double-counted the s(year) smooth already inside CFR_target, and no secular trend in cholera CFR is documented (WHO's Yemen-excluded series is flat at 1.7/1.4/1.5% for 2017/2019/2020). SIMULATION OUTPUT IS BIT-IDENTICAL: this config has shipped mu_j_slope = 0 for every location since the field existed, so the deleted factor evaluated to exactly 1 (verified over 26 scenarios x 722 result-channel digests in both rng and replay engine modes, at 40 and 1 patches, including the 1,398-tick full-length oracle fixture). make_simulation_config() keeps a deprecated, ignored mu_j_slope formal so pre-v0.95.0 configs on disk still replay without an 'unused argument' error. Sources priors_default v15.20. No window/psi/surveillance/prior-value change vs v4.8. v4.8 (2026-09-23): SCHEMA REMOVAL -- the [nL x nT] mu_jt matrix is no longer built, validated, or shipped (CFR restructure R6 defect #1). It was dead payload that the engine never read: run_simulation() derives its own per-tick mortality hazard from mu_j_baseline / mu_j_epidemic_factor (R/sim_components.R), while this matrix carried RAW reported CFR, 2.5-7.6x the mu_j_baseline actually used, and was clusterExport'ed (~1 MB at 40 x 3322, ~11% of the object) to every PSOCK worker on every run. No engine input changed, so simulation output is bit-identical. make_simulation_config() keeps a deprecated, ignored mu_jt formal so pre-v0.94.0 configs on disk still replay without an 'unused argument' error. ALSO (CFR restructure R2): rho_deaths (0.42) and delta_reporting_deaths (5) are no longer mere fallbacks -- they are now the PINNED constants the sampler uses, because sample_rho_deaths and sample_delta_reporting_deaths default FALSE in sample_parameters() and run_MOSAIC(). Their VALUES are unchanged, so no shipped field moves; only their status and the surrounding provenance comments do. rho_deaths is pinned because it cancels identically from the deaths mean under the B2 derivation (mu_j_baseline is proportional to 1/rho_deaths, the engine thins disease_deaths by rho_deaths; measured: a 0.25-0.65 sweep WITH re-derivation moves realized deaths 0.3-1.3 percent at n=48 seeds/arm across 5 national medoids, against 6-27 percent for either half-change alone). delta_reporting_deaths is pinned because no observational anchor exists (deaths and cases share the same WHO bulletin row). No window/psi/surveillance/prior-value change vs v4.7. v4.7 (2026-06-24): alpha_1 (within-metapop population-mixing exponent) is now stored as a LENGTH-nL VECTOR (rep(0.27, length(j))) instead of a global scalar (D1 of the per-location-alpha_1 plan). The laser-cholera engine is dual-mode (params.py: a scalar alpha_1 is broadcast to all patches, a length-(num_nodes,) vector is applied elementwise per patch in the FOI), so this is engine-valid and the prior was relocated to PER-LOCATION in priors_default v15.16 (shared Beta(28.4,71.6) per ISO). The seed config MUST carry a length-nL alpha_1 so the convert_matrix_to_config round-trip preserves per-ISO calibrated values (a scalar seed silently drops idx 2..nL). alpha_2 is DELIBERATELY KEPT as a global scalar (0.50). Scalar-alpha_1 configs remain valid for national/legacy use. No window/psi/surveillance/prior-value change vs v4.6 (priors_default bumped to v15.16 only for the alpha_1 relocation). v4.6 (2026-06-23): B2.1 ENGINE-CORRECT chain factor (RECALIBRATION-GATED; statistician-validated). The B2 derivation (v4.5) computed mu_j_baseline = CFR_target * gamma_1 * rho / (rho_deaths * chi_blend); the laser-cholera deaths/reported_cases mechanism actually implies CFR_target * (1-exp(-gamma_1)) * rho / (rho_deaths * chi_epidemic) -- two corrections: (1) reported_cases scales with INCIDENCE not prevalence-days so the recovery-tick factor is the survival complement (1-exp(-gamma_1)) NOT gamma_1; (2) reported_cases is an Isym stock-read dominated by epidemic-regime ticks so the effective PPV leans to chi_epidemic NOT the 0.5*(chi_endemic+chi_epidemic) blend (statistician memory b2-cfr-chain-factor-diagnosis). This reduces realized deaths bias 16-28% on the problem countries with no harm to the good ones; an irreducible ~1.3-1.5x dynamics-dependent residual (realized epidemic-fraction + spatial coupling) remains and cannot be absorbed by any closed form. sample_parameters() derives mu with the B2.1 chain; config_default's static mu_j_baseline anchor is correspondingly rebuilt as CFR_target * (1-exp(-0.10))*rho_mean/(rho_deaths_mean*chi_epidemic=0.75) -> cfr_to_mu_adjustment ~0.1303 (was ~0.1467 under v4.5, ratio 0.888). priors_default is UNCHANGED at v15.15 (B2.1 changes only the DERIVATION, not the CFR_target prior). The ETH-only dwell stop-gap remains GONE (B2 subsumes it structurally). No window/psi/surveillance change vs v4.5. v4.5 (2026-06-23): B2 DYNAMIC mu_j_baseline <-> sampled gamma_1 coupling (RECALIBRATION-GATED; statistician spec MOSAIC-pkg/claude/prior_fix_spec/SPEC_B2.md, sourcing priors_default v15.15). priors_default v15.15 REPLACED the per-country mu_j_baseline Gamma location prior with a per-country CFR_target lognormal prior (median = the same WHO-GAM CFR the B1 build used, sdlog=0.787); sample_parameters() now DERIVES mu_j_baseline at sample time so realized implied CFR == CFR_target for every draw. config_default now ships a NEW per-country CFR_target field (the prior median) AND a mu_j_baseline numeric default built as CFR_target * the STATIC chain anchor; the v15.10/v15.14 ETH-only x0.497 dwell stop-gap is GONE (B2 subsumes it structurally). No window/psi/surveillance change vs v4.4. v4.4 (2026-06-22): rebuilt at the 2023-01-01 production window to source the RECALIBRATION-GATED priors_default v15.14 Stage-1 fixes (statistician spec MOSAIC-pkg/claude/prior_fix_spec/SPEC_prior_fixes.md). mu_j_baseline per-country defaults now carry the B1 chain-factor re-anchor (gamma_1 0.1133->0.10, chi 0.639->0.70 in the CFR->mu derivation: every country's Gamma mean x0.804, ~1.25x deaths/implied-CFR reduction; cases untouched) and the v15.10 ETH-only x0.40 dwell stop-gap is removed (subsumed by the global re-anchor). beta_j0_tot per-country defaults now carry the w=0.5 geometric-mean partial-shrinkage recenter of 18 Stage-1 ISOs toward their single-location posterior medians (symmetric: BDI/MWI/NGA/RWA/TZA move UP; LBR excluded/unidentified -> 2e-5 default; the v15.9 COG override is dropped as COG is outside the 19-ISO cohort -> 2e-5 default); beta_j0_hum/beta_j0_env are re-derived as p_beta*beta_j0_tot so they propagate consistently. No window/schema change vs v4.3. v4.3 (2026-06-20): rebuilt at the 2023-01-01 production window under the relaxed surveillance trust-tier gate (process_cholera_surveillance_data v0.47.1). fourier_* synthetic reconstructions of real annual/quarterly totals are now KEPT in the reported_cases/reported_deaths fit target (811 fourier weeks in the 2023+ window: 312 k1 + 428 k2 + 7 k3 + 12 East Africa + 52 Southern Africa) carrying their lower per-week confidence_weight (~0.4-0.5) in reported_cases_weight/reported_deaths_weight, which calc_model_likelihood() consumes as a per-observation weight; only assumed_zero weeks remain NA-blanked. This adds many (0,1)-weighted cells vs the prior fourier-hard-drop build. v4.2 (2026-06-19): psi_jt regenerated from the CLEAN 'G' suitability re-run with the B1-fixed bias-correction (calibrate_psi_predictions robust guard, MOSAIC v0.45.0). The v4.0/v4.1 psi was produced by an unregularized per-country affine bias-correction that CORRUPTED low-signal countries (ZAF ~9.7x logit blow-up, GMB flat-constant, SEN/SWZ gutted); the fixed correction (identity fallback for rank-deficient/degenerate fits + bounded affine + amplitude clamp, plus a check_psi_amplitude monitor) de-corrupts them (GMB flat->real seasonality; ZAF psi range 0.65->0.02) while leaving good countries (COD/SOM/MOZ) bit-unchanged. Same 'G' config as v4.0 (D per-capita-per-country target + AI + confidence_weight ON + rw_subsample=5 tiling + fit_date_start=2010 + n_seeds=10); 8 low-signal countries sit at the amplitude clamp (~2x / 0.5x) but the correction only rescales the genuine LSTM seasonal shape there (logit corr=1.0), and beta_env is self-normalized so the scaling is largely absorbed. v4.1 (2026-06-19): multi-source fit target + configurable window. (a) reported_cases/reported_deaths now read from the MULTI-SOURCE combined daily file (cholera_surveillance_daily_combined.csv, include_ai=TRUE) instead of the WHO-only daily file -- adds the JHU back-history and AI observed/documented_zero (confirmed-absence) weeks under the WHO>JHU>AI>SUPP merge; fourier/assumed_zero are NA-blanked upstream. (b) date_start is now a configurable top-of-script constant (DEFAULT 2023-01-01, one month earlier than the legacy 2023-02-01 at the clean year boundary for direct comparability; settable to >=2015 for back-history builds, with a psi-coverage floor guard). (c) NEW reported_cases_weight/reported_deaths_weight matrices (per-observation confidence_weight in [0,1]; direct sources = 1.0) injected post-make_simulation_config and carried in the .rda + JSON, and CONSUMED by calc_model_likelihood: run_MOSAIC passes them as weights_obs_cases/weights_obs_deaths (run_MOSAIC.R:429-430) and derives a per-location weights_location from them. IC seeding is floored at a data-rich epoch in priors_default v15.12 (ic_t0 = max(date_start, 2023-02-01)) so the window change does not cold-start ICs. v4.0 (2026-06-19): psi_jt regenerated from the WINNING 'G' suitability config of the 6-variant psi->LASER calibration tournament: per-capita per-country target (target_D_rate_per_country_floored) + AI-enhanced surveillance (include_ai=TRUE, confidence_weight ON so AI/synthetic rows are down-weighted) + rolling-CV rw_subsample=5 (tiling, = the 5-month test window: full coverage, no fold overlap) + fit_date_start=2010 (modest AI back-history; 2000 was tested and degraded fit via pre-2010 pure-synthetic data) + n_seeds=10 (bumped from 5 for ensemble stability on this load-bearing artifact). Median R2_cases across 15 data countries improved old 0.589 -> D 0.687 -> E(+AI) 0.718 -> G 0.722, with G also best-among-per-capita on bias. psi_star_b prior stays at the v15.11 per-capita re-center (+1.0). v3.9 (2026-06-18): psi_jt switched to the per-capita per-country suitability response (est_suitability response_var='target_D_rate_per_country_floored', now the package default), selected over 'transmission_intensity' by a 15-country psi->LASER calibration case-skill comparison (D lifts cases-R2 for the priority cluster COD 0.51->0.73 / SOM 0.34->0.82 / ETH 0.61->0.80; regressions on 5 low-burden countries RWA/MWI/AGO/NAM/SSD accepted for the global default). D psi has a much lower level, so psi_star_b default prior was re-centered 0->+1.0 in priors_default v15.11 (calc_psi_star odds-multiply offset), the stale MOZ psi_star_b override removed, and config_default now sources psi_star_b from the priors_default mean. v3.8 (2026-06-18): ETH-only mu_j_baseline synced to the priors_default v15.10 dwell-mismatch STOP-GAP (x0.40; Gamma mean 0.00220230 -> 0.00088092). config_default sources mu_j_baseline directly from priors_default Gamma means (see L~199), so a full regen reproduces the stop-gap automatically; the shipped .rda was patched surgically (no full regen) because priors_default v15.10 was patched surgically. Completes the half-applied v15.10 change (priors_default.rda/.json were updated but config_default.rda was not), fixing the Lesson-#12 drift guard in tests/testthat/test-cfr-pipeline-consistency.R. See disease-modeler memory project_eth_deaths_cfr_dwell_mismatch. v3.7 (2026-06-04): date_stop is now derived from the maximum date in pred_psi_suitability_day.csv (the LSTM environmental-suitability forecast horizon) rather than being a hard-coded 2026-03-31. date_start remains anchored to WHO surveillance availability (2023-02-01). This couples the simulation window to whatever the latest psi forecast extends to, so an LSTM rerun with a different horizon picks up automatically. v3.6 (2026-06-02): mu_j_baseline per-country defaults now sourced UNIVERSALLY from priors_default Gamma means (was: rowMeans of raw CFR matrix with ETH-only hand-patch). priors_default v15.6+ applies the v0.13+ identity mu_j_baseline = CFR * rho / (rho_deaths * chi) so config_default and priors_default agree by construction. Ordering dependency: make_priors_default.R must be run before make_config_default.R. The MOSAIC-data WHO annual file was refreshed through 2025 calendar year (2024 + 2025 dashboard CSVs ingested, 2026 partial snapshot included). v3.5 (2026-06-02): rho_deaths default changed from 0.6 to 0.42 (informative Beta(36.95, 51.02) mean) following the random-effects meta-analysis of three SSA studies (Routh 2017 Tanzania, Shikanga 2009 Kenya, Bwire 2013 Uganda); see claude/rho_deaths_research/SYNTHESIS_REPORT.md. v3.4 (2026-06-01): beta_j0_tot now sourced PER-COUNTRY from priors_default location medians (was a global 2e-5 constant); ETH resolves to its recentred 1.75e-6 median while all other countries are unchanged at 2e-5. Also ETH mu_j_baseline sourced from its prior mean (reporting-adjusted CFR) instead of mean(mu_jt), fixing a ~2x deaths over-prediction. With these, the fixed-ensemble ETH default fits observed cases at bias~1.05 (R2corr~0.50) and deaths at bias~0.9. v3.3 (2026-06-01): epidemic_peaks filtered to [date_start, date_stop] at build time -- the 82 rows outside the config window were silently snapping to t=1/t=N in the peak-shape likelihood terms and bloating the JSON (47 rows shipped, was 129). v3.2 (2026-05-28): epidemic_peaks (iso_code, peak_date) shipped in default config so the Python likelihood port (laser-cholera#47) can compute peak-timing / peak-magnitude shape terms without a runtime injection. v3.1 (2026-04-30): nu_jt_sources added explicitly (laser-cholera#102); eligible pool for first-dose OCV is [S, E, Isym, Iasym, R]. v3.0 (2026-04-23): zeta_1, zeta_2, and zeta_ratio placeholder defaults rescaled from Frame-B (70k / 300) to the biological scale (~2.1e11 / 4.5e4) implied by the literature meta-analysis in est_zeta_*_prior() (priors_default v15.0). v2.1: Refreshed psi_jt from LSTM refit on corrected ERA5 soil_moisture_0_to_10cm_mean (open-meteo-pipeline#5). v2.0: Updated defaults from MOZ calibration evidence (tests 19-28)."
)

# The changelog head "vX.Y (YYYY-MM-DD)" is hand-written; metadata$date is the
# build day. Flag a mismatch (config v5.1 shipped date 2026-09-28 under a
# 2026-09-29 head).
.changelog_date <- sub("^.*? v[0-9.]+ \\(([0-9]{4}-[0-9]{2}-[0-9]{2})\\).*$", "\\1",
                       config_default$metadata$description, perl = TRUE)
if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", .changelog_date) &&
    .changelog_date != config_default$metadata$date) {
     warning("config_default changelog head is dated ", .changelog_date,
             " but metadata$date (build day) is ", config_default$metadata$date,
             "; update the version heading.")
}

# Validate transmission parameter relationships
# Note: Using the original vectors since beta_j0_tot and p_beta are not in config
validation_tol <- 1e-10
for (i in 1:length(j)) {
     total_check <- config_default$beta_j0_hum[i] + config_default$beta_j0_env[i]
     if (abs(total_check - beta_j0_tot[i]) > validation_tol) {
          warning(sprintf("Transmission parameter inconsistency for %s: hum + env != tot", j[i]))
     }

     prop_check <- config_default$beta_j0_hum[i] / beta_j0_tot[i]
     if (abs(prop_check - p_beta[i]) > validation_tol) {
          warning(sprintf("Transmission parameter inconsistency for %s: hum/tot != p_beta", j[i]))
     }
}
message("Transmission parameter validation complete")



# --------------------------------------------------------------------------- #
# Write the JSON artifact. Set write_gz = TRUE to also produce the .json.gz;
# write_gz = FALSE is the default. When both flags are TRUE the .json and
# .json.gz are byte-equal by construction (written from the same in-memory
# JSON string -- no parallel serialisation).
# --------------------------------------------------------------------------- #

# Write into the package tree this script is RUN FROM, not the canonical
# checkout. `PATHS$ROOT` is the data root and always resolves to
# ~/MOSAIC/MOSAIC-pkg, so deriving the output path from it made this script
# silently clobber the main checkout when run from a git worktree -- while
# `usethis::use_data()` (which resolves the active project) correctly wrote the
# .rda next to the script. The two artifacts then came from different trees.
pkg_dir <- normalizePath(getwd(), mustWork = TRUE)
if (!file.exists(file.path(pkg_dir, "DESCRIPTION"))) {
     stop("Run this script from the MOSAIC-pkg root: no DESCRIPTION in ", pkg_dir)
}
fp_json <- file.path(pkg_dir, 'inst/extdata/config_default.json')

args <- config_default
args$metadata <- NULL          # excluded from JSON; kept on the .rda below
args$output_file_path <- NULL  # return the validated list instead of writing
params_validated <- do.call(MOSAIC::make_simulation_config, args)
rm(args)

# Tracking fields that make_simulation_config rejects as unknown args
params_validated$zeta_ratio       <- .zeta_ratio_default
params_validated$decay_days_spread <- .decay_days_spread_default

# Per-observation confidence-weight matrices (n_loc x n_t, aligned to
# reported_cases/reported_deaths). make_simulation_config() rejects unknown args, so
# (like zeta_ratio) they are injected AFTER validation. Not read by the engine (it
# ignores unknown keys), but consumed by the likelihood: run_MOSAIC() passes them
# as weights_obs_cases / weights_obs_deaths, and
# calc_log_likelihood_deaths_integrated() reads config$reported_deaths_weight.
params_validated$reported_cases_weight  <- mat_cases_weight
params_validated$reported_deaths_weight <- mat_deaths_weight
# Surveillance trust tiers (n_loc x n_t, aligned likewise): read by the
# dispersion estimate (.mosaic_resolve_nb_dispersion), ignored by the engine.
params_validated$reported_tier <- mat_tier

MOSAIC::write_json_or_gz(
     params_validated,
     fp_json,
     write_json = TRUE,
     write_gz   = FALSE
)

# Attach tracking fields to the rda-bound config_default and persist
config_default$zeta_ratio        <- .zeta_ratio_default
config_default$decay_days_spread <- .decay_days_spread_default
config_default$reported_cases_weight  <- mat_cases_weight
config_default$reported_deaths_weight <- mat_deaths_weight
config_default$reported_tier          <- mat_tier

tmp_config <- MOSAIC::read_json_to_list(fp_json)
identical(config_default, tmp_config)
all.equal(config_default, tmp_config)

usethis::use_data(config_default, overwrite = TRUE)
