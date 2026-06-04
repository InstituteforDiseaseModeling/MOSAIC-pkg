library(MOSAIC)

# Set up paths - critical for finding input files
# Set root to parent directory containing all MOSAIC repos
MOSAIC::set_root_directory("/Users/johngiles/MOSAIC")
PATHS <- MOSAIC::get_paths()

# Use 2023-2025 dates to match WHO surveillance data
date_start <- as.Date("2023-02-01")
date_stop <- as.Date("2026-03-31")

message("Set simulation time steps and locations")
t <- seq.Date(date_start, date_stop, by = "day")
j <- MOSAIC::iso_codes_mosaic

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

# Populate mu_jt from hierarchical CFR estimates
# Check if param_mu_disease_mortality.csv exists
cfr_file <- file.path(PATHS$MODEL_INPUT, "param_mu_disease_mortality.csv")
if (file.exists(cfr_file)) {
     # Read CFR estimates
     cfr_df <- read.csv(cfr_file)
     cfr_point <- cfr_df[cfr_df$parameter_distribution == "point", ]

     # Get years from simulation period
     sim_years <- seq(as.numeric(format(date_start, "%Y")),
                      as.numeric(format(date_stop, "%Y")))

     # Calculate global mean as fallback (only for simulation years)
     cfr_subset <- cfr_point[cfr_point$t %in% sim_years, ]
     global_mean_cfr <- mean(cfr_subset$parameter_value, na.rm = TRUE)

     # Create mu_jt matrix with same dimensions as d_jt
     mu_jt <- d_jt

     # Fill matrix with yearly CFR values
     for (i in seq_along(j)) {
          for (day_idx in seq_along(t)) {
               year <- as.numeric(format(as.Date(t[day_idx]), "%Y"))
               cfr_values <- cfr_point$parameter_value[cfr_point$j == j[i] & cfr_point$t == year]

               # Handle multiple values by taking the mean, or use global mean if empty/NA
               if (length(cfr_values) > 0 && !all(is.na(cfr_values))) {
                    mu_jt[i, day_idx] <- mean(cfr_values, na.rm = TRUE)
               } else {
                    mu_jt[i, day_idx] <- global_mean_cfr
               }
          }
     }

     message("mu_jt populated from CFR hierarchical model estimates")
} else {
     # Fallback to default if file doesn't exist
     mu_jt <- d_jt
     mu_jt[,] <- 0.01
     warning("param_mu_disease_mortality.csv not found. Using default CFR of 0.01")
}

# Extract mu_j_baseline from priors_default per-country Gamma means (v3.6+).
#
# Pre-v3.6 this used `rowMeans(mu_jt)` which fed RAW reported CFR into LASER
# as a daily mortality hazard — wrong by a factor of ~rho/(rho_deaths*chi)
# ~= 1.015. The make_priors_default.R derivation applies the v0.13+ identity
# `mu_j_baseline = CFR * rho / (rho_deaths * chi)` and writes the result to
# priors_default. Pulling the Gamma mean here keeps config_default and
# priors_default in sync by construction — the consistency regression test
# in tests/testthat/ asserts they match within 1% per country.
#
# Ordering dependency: make_priors_default.R must run BEFORE this script so
# the package's priors_default data object reflects the current CFR estimates.
mu_j_baseline <- vapply(
     j,
     function(iso) {
          loc <- priors_default$parameters_location$mu_j_baseline$location[[iso]]
          if (is.null(loc)) {
               warning(sprintf("priors_default has no mu_j_baseline for %s; using fallback", iso))
               return(0.0035 * 1.015)  # ~0.35% CFR × v0.13+ adjustment
          }
          p <- loc$parameters
          if (loc$distribution != "gamma") stop("expected gamma prior for mu_j_baseline; got ", loc$distribution)
          unname(p$shape / p$rate)   # gamma prior mean
     },
     numeric(1)
)
names(mu_j_baseline) <- j

# Initialize mu_j_slope to 0 (no temporal trend by default).
# Prior is Gamma centred on 0 for each iso; default of 0 (== prior mode) is fine.
mu_j_slope <- rep(0, length(j))
names(mu_j_slope) <- j

# Seed mu_j_epidemic_factor from per-iso prior means rather than zero.
# Prior is Gamma per iso (most isos mean=0.5, MOZ mean=3.0). The previous
# hardcoded 0 sat at the support boundary -- the epidemic-phase IFR
# multiplier could never engage, so the death series LL was stuck at
# baseline regardless of outbreak phase.
mu_j_epidemic_factor <- vapply(
     j,
     function(iso) {
          loc <- priors_default$parameters_location$mu_j_epidemic_factor$location[[iso]]
          if (is.null(loc)) return(0.5)  # fallback if iso missing from prior
          p <- loc$parameters
          if (loc$distribution == "gamma") return(unname(p$shape / p$rate))
          if (loc$distribution %in% c("truncnorm","normal")) return(unname(p$mean))
          0.5
     },
     numeric(1)
)
names(mu_j_epidemic_factor) <- j

message("Created mu_j_baseline, mu_j_slope, and mu_j_epidemic_factor parameters from mu_jt")

#####
# Vaccination rate needs work
# Must be square by dates and locations
#####

message("Add vaccination rate over time for each location (nu_jt)")
# Try to load the WHO vaccination rate file (for backward compatibility)
# or the GTFCC_WHO combined file if available
if (file.exists(file.path(PATHS$MODEL_INPUT, 'param_nu_vaccination_rate_GTFCC_WHO.csv'))) {
     tmp <- read.csv(file.path(PATHS$MODEL_INPUT, 'param_nu_vaccination_rate_GTFCC_WHO.csv'))
     message("Using combined GTFCC+WHO vaccination data")
} else if (file.exists(file.path(PATHS$MODEL_INPUT, 'param_nu_vaccination_rate_WHO.csv'))) {
     tmp <- read.csv(file.path(PATHS$MODEL_INPUT, 'param_nu_vaccination_rate_WHO.csv'))
     message("Using WHO vaccination data")
} else if (file.exists(file.path(PATHS$MODEL_INPUT, 'param_nu_vaccination_rate_GTFCC.csv'))) {
     tmp <- read.csv(file.path(PATHS$MODEL_INPUT, 'param_nu_vaccination_rate_GTFCC.csv'))
     message("Using GTFCC vaccination data")
} else {
     # Fall back to legacy filename if it exists
     tmp <- read.csv(file.path(PATHS$MODEL_INPUT, 'param_nu_vaccination_rate.csv'))
     warning("Using legacy vaccination rate file without source suffix")
}
tmp$t <- as.Date(tmp$t)
tmp <- tmp[tmp$j %in% j,]
tmp <- tmp[tmp$t >= date_start & tmp$t <= date_stop,]
nu_jt <- reshape2::acast(tmp, j ~ t, value.var = "parameter_value")

nu_1_jt <- nu_2_jt <- nu_jt
nu_2_jt[,] <- 0

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


message("Get departure probability of each location (tau_j)")
tmp <- read.csv(file.path(PATHS$MODEL_INPUT, 'param_tau_departure.csv'))
tmp <- tmp[tmp$i %in% j,]
tmp <- tmp[tmp$parameter_name =='mean',]
sel <- match(j, tmp$i)
tau_i <- tmp$parameter_value[sel]
names(tau_i) <- tmp$i[sel]

message("Gravity model parameters")
tmp <- read.csv(file.path(PATHS$MODEL_INPUT, "mobility_lon_lat.csv"))

lon <- tmp$lon
names(lon) <- tmp$iso3
longitude <- lon[match(j, names(lon))]

lat <- tmp$lat
names(lat) <- tmp$iso3
latitude <- lat[match(j, names(lat))]

tmp <- read.csv(file.path(PATHS$MODEL_INPUT, "mobility_gravity_params.csv"), row.names=1)
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
# how the initial conditions and mu_j_* defaults are sourced above. This lets
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
psi_jt <- reshape2::acast(tmp, iso_code ~ date, value.var = "pred_smooth", fun.aggregate = mean)
sel <- match(j, row.names(psi_jt))
psi_jt <- psi_jt[sel,]

message("Get reported cholera cases and deaths data (for model fitting)")
df_daily <- read.csv(file.path(PATHS$DATA_WHO_DAILY, "cholera_country_daily_processed.csv"), stringsAsFactors = FALSE)
df_daily$date <- as.Date(df_daily$date)

mat_cases <- matrix(NA, nrow = length(j), ncol = length(t), dimnames = list(j, as.character(t)))
mat_deaths <- matrix(NA, nrow = length(j), ncol = length(t), dimnames = list(j, as.character(t)))

for (i in seq_along(j)) {

     current_iso <- j[i]
     iso_data <- df_daily[df_daily$iso_code == current_iso, ]

     for (k in seq_along(t)) {

          current_date <- t[k]
          row_idx <- which(iso_data$date == current_date)

          # If there is one or more matching row, take the first match
          if (length(row_idx) >= 1) {

               mat_cases[i, k] <- iso_data$cases[row_idx[1]]
               mat_deaths[i, k] <- iso_data$deaths[row_idx[1]]

          }
     }
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
     mu_jt = mu_jt,
     mu_j_baseline = mu_j_baseline,       # NEW: Baseline IFR (replaces mu_j)
     mu_j_slope = mu_j_slope,              # Temporal trend in IFR
     mu_j_epidemic_factor = mu_j_epidemic_factor,  # NEW: Epidemic increase factor
     sigma = 0.25,
     # Case reporting parameters for calc_cases_from_infections()
     rho = 0.423,                 # Care-seeking rate (mean of Beta(5.38, 7.10) prior, Wiens et al. 2025 RE pool of general + severe/cholera strata; see R/get_rho_care_seeking_params.R)
     rho_deaths = 0.42,           # Death detection rate: probability a true cholera death is captured by surveillance (mean of informative Beta(36.95, 51.02) prior; RE meta-analysis of Routh 2017, Shikanga 2009, Bwire 2013 — see claude/rho_deaths_research/SYNTHESIS_REPORT.md)
     chi_endemic = 0.50,          # PPV among suspected cases during endemic periods (50%)
     chi_epidemic = 0.75,         # PPV among suspected cases during epidemic periods (75%)
     # Per-iso Isym/N threshold for PPV / IFR phase switching. Seeded from
     # the per-iso Truncnorm prior means (range ~7e-7 NGA to ~8.7e-6 COD;
     # ETH 3.36e-6). The earlier flat 1/10000 default was 12-142x above the
     # prior means and never let any country enter "epidemic" phase, so
     # chi_epidemic and mu_j_epidemic_factor never engaged.
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
     delta_reporting_cases = 0,   # Case reporting delay (was 2; posteriors collapse to 0 in every test, KL=14.2)
     delta_reporting_deaths = 5,  # Death reporting delay (was 7; posteriors consistently at 4-6 days)
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
     alpha_1 = 0.27,       # Population mixing (was 0.975; posteriors consistently 0.21-0.40. Freezing at 0.975 destroys fit — test_25)
     alpha_2 = 0.50,       # Frequency-driven transmission (was 0.33; posteriors 0.22-0.70, prior median 0.50)
     beta_j0_env = beta_j0_env,      # UPDATED: Now calculated from beta_j0_tot * (1 - p_beta)
     theta_j = theta_j,
     # psi_star calibration parameters (default to no transformation)
     psi_star_a = setNames(rep(1.0, length(j)), j),    # Default: no shape/gain transformation
     psi_star_b = setNames(rep(0.0, length(j)), j),    # Default: no scale/offset transformation
     psi_star_z = setNames(rep(1.0, length(j)), j),    # Default: no smoothing
     psi_star_k = setNames(rep(0.0, length(j)), j),    # Default: no time offset
     psi_jt = psi_jt,
     # v0.29.0: zeta_* defaults rescaled from the Frame-B 70k/300 scale to the
     # biological scale implied by the literature meta-analysis in
     # est_zeta_1_prior() / est_zeta_2_prior() / est_zeta_ratio_prior().
     # zeta_1 uses the MODE of its lognormal prior (mode = exp(meanlog - sdlog^2));
     # zeta_ratio uses the MEDIAN of its direct-channel prior (exp(meanlog))
     # because the direct-channel mode is pathological (~1e-7) due to the
     # wide sdlog (4.807) reflecting the 5-OOM tension in direct literature.
     zeta_1 = 3.29e8,      # Mode of LN(25.654, 2.458) = exp(25.654 - 2.458^2)
                           # v0.29.1 bias-corrected (was 2.148e10 under
                           # pre-correction LN(26.641, 1.688); new value
                           # reflects lowered V_sev 8->4 L/day, lowered mild
                           # concentration 10^6->10^5, removed derived pool
                           # row, downweighted review sources)
     zeta_2 = 3.29e8 / 74.69,      # DERIVED at sampling time (= zeta_1/zeta_ratio);
                                   # tracked placeholder so run_MOSAIC's
                                   # param_names_all picks it up for samples.parquet.
                                   # = z1_mode / z_ratio_median = 4.40e6
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
     # configured [date_start, date_stop] window — peaks outside this window
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

config_default <- do.call(make_LASER_config, default_args)

# Derived-parameter tracking fields not accepted by make_LASER_config signature.
# Injected into config_default (and the written JSONs below) so run_MOSAIC's
# convert_config_to_matrix picks them up for samples.parquet.
.zeta_ratio_default <- 74.69        # Median of direct-channel LN(4.313, 4.394) = exp(4.313); v0.29.1 bias-corrected (was 763 pre-correction). Median (not mode) used because mode is pathological for sdlog=4.394.
.decay_days_spread_default <- 184   # Spread; prior median 180 (decay_days_long = short + spread)

# Add metadata for provenance tracking
config_default$metadata <- list(
     version = "3.5",
     date = as.character(Sys.Date()),
     description = "Default LASER configuration for MOSAIC cholera metapopulation model. v3.6 (2026-06-02): mu_j_baseline per-country defaults now sourced UNIVERSALLY from priors_default Gamma means (was: rowMeans of raw CFR matrix with ETH-only hand-patch). priors_default v15.6+ applies the v0.13+ identity mu_j_baseline = CFR * rho / (rho_deaths * chi) so config_default and priors_default agree by construction. Ordering dependency: make_priors_default.R must be run before make_config_default.R. The MOSAIC-data WHO annual file was refreshed through 2025 calendar year (2024 + 2025 dashboard CSVs ingested, 2026 partial snapshot included). v3.5 (2026-06-02): rho_deaths default changed from 0.6 to 0.42 (informative Beta(36.95, 51.02) mean) following the random-effects meta-analysis of three SSA studies (Routh 2017 Tanzania, Shikanga 2009 Kenya, Bwire 2013 Uganda); see claude/rho_deaths_research/SYNTHESIS_REPORT.md. v3.4 (2026-06-01): beta_j0_tot now sourced PER-COUNTRY from priors_default location medians (was a global 2e-5 constant); ETH resolves to its recentred 1.75e-6 median while all other countries are unchanged at 2e-5. Also ETH mu_j_baseline sourced from its prior mean (reporting-adjusted CFR) instead of mean(mu_jt), fixing a ~2x deaths over-prediction. With these, the fixed-ensemble ETH default fits observed cases at bias~1.05 (R2corr~0.50) and deaths at bias~0.9. v3.3 (2026-06-01): epidemic_peaks filtered to [date_start, date_stop] at build time -- the 82 rows outside the config window were silently snapping to t=1/t=N in the peak-shape likelihood terms and bloating the JSON (47 rows shipped, was 129). v3.2 (2026-05-28): epidemic_peaks (iso_code, peak_date) shipped in default config so the Python likelihood port (laser-cholera#47) can compute peak-timing / peak-magnitude shape terms without a runtime injection. v3.1 (2026-04-30): nu_jt_sources added explicitly (laser-cholera#102); eligible pool for first-dose OCV is [S, E, Isym, Iasym, R]. v3.0 (2026-04-23): zeta_1, zeta_2, and zeta_ratio placeholder defaults rescaled from Frame-B (70k / 300) to the biological scale (~2.1e11 / 4.5e4) implied by the literature meta-analysis in est_zeta_*_prior() (priors_default v15.0). v2.1: Refreshed psi_jt from LSTM refit on corrected ERA5 soil_moisture_0_to_10cm_mean (open-meteo-pipeline#5). v2.0: Updated defaults from MOZ calibration evidence (tests 19-28)."
)

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

pkg_dir <- file.path(PATHS$ROOT, "MOSAIC-pkg")
fp_json <- file.path(pkg_dir, 'inst/extdata/config_default.json')

args <- config_default
args$metadata <- NULL          # excluded from JSON; kept on the .rda below
args$output_file_path <- NULL  # return the validated list instead of writing
params_validated <- do.call(MOSAIC::make_LASER_config, args)
rm(args)

# Tracking fields that make_LASER_config rejects as unknown args
params_validated$zeta_ratio       <- .zeta_ratio_default
params_validated$decay_days_spread <- .decay_days_spread_default

MOSAIC::write_json_or_gz(
     params_validated,
     fp_json,
     write_json = TRUE,
     write_gz   = FALSE
)

# Attach tracking fields to the rda-bound config_default and persist
config_default$zeta_ratio        <- .zeta_ratio_default
config_default$decay_days_spread <- .decay_days_spread_default

tmp_config <- MOSAIC::read_json_to_list(fp_json)
identical(config_default, tmp_config)
all.equal(config_default, tmp_config)

usethis::use_data(config_default, overwrite = TRUE)
