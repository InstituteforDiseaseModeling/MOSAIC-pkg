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

# Get number of infected individuals in each location
# New default: 100 infected per location for better epidemic seeding
I_j <- rep(100, length(N_j))
I_j <- as.integer(I_j)

# Get number of exposed individuals in each location
# Set to zero for simple method compatibility
E_j <- rep(0, length(N_j))
E_j <- as.integer(E_j)

# Get number of susceptible individuals in each location
# Adjusted to account for 100 infected
S_j <- N_j - as.integer(N_j * 0.5) - I_j
S_j <- pmax(S_j, 1)  # Ensure at least 1 susceptible

# Vaccination compartments - set to zero by default
V1_j <- rep(0, length(N_j))
V1_j <- as.integer(V1_j)

V2_j <- rep(0, length(N_j))
V2_j <- as.integer(V2_j)

# Calculate recovered to balance the equation
# R = N - (S + E + I + V1 + V2)
R_j <- N_j - S_j - E_j - I_j - V1_j - V2_j
R_j <- pmax(R_j, 0)  # Ensure non-negative

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

# Extract mu_j_baseline from mu_jt (location-specific baseline CFR)
# Use the mean value across time for each location
mu_j_baseline <- rowMeans(mu_jt, na.rm = TRUE)
names(mu_j_baseline) <- j

# Initialize mu_j_slope to 0 (no temporal trend by default)
mu_j_slope <- rep(0, length(j))
names(mu_j_slope) <- j

# Initialize mu_j_epidemic_factor to 0.5 (50% increase during epidemics by default)
mu_j_epidemic_factor <- rep(0, length(j))
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
# Updated to match prior medians (was 1e-6 which is 20x below posterior range 5e-6 to 2e-5)
beta_j0_tot_default <- 2e-5  # Total transmission rate (prior median from Lognormal)
p_beta_default <- 0.33        # Proportion human transmission (matching prior mode)

# Create vectors for all locations
beta_j0_tot <- rep(beta_j0_tot_default, length(j))
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
message(sprintf("  beta_j0_tot = %.2e (total transmission rate)", beta_j0_tot_default))
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
     rho = 0.265,                 # Care-seeking rate (mean of Beta(6.8, 17.9) prior, GEMS + Wiens 2025)
     rho_deaths = 0.6,            # Death detection rate: probability a true cholera death is captured by surveillance (mean of Beta(3, 2) prior; Finger et al. 2024)
     chi_endemic = 0.50,          # PPV among suspected cases during endemic periods (50%)
     chi_epidemic = 0.75,         # PPV among suspected cases during epidemic periods (75%)
     epidemic_threshold = rep(1/10000, length(j)),  # Per-location Isym/N threshold for PPV switching (1 per 10k)
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
     # Filtered to locations actually present in this config — peaks for locations
     # outside iso_codes_mosaic (e.g. SDN) would never be looked up by the per-loc
     # scoring loop and only bloat the JSON.
     epidemic_peaks = local({
          ep <- MOSAIC::epidemic_peaks
          ep <- ep[ep$iso_code %in% j, , drop = FALSE]
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
     version = "3.2",
     date = as.character(Sys.Date()),
     description = "Default LASER configuration for MOSAIC cholera metapopulation model. v3.2 (2026-05-28): epidemic_peaks (iso_code, peak_date) shipped in default config so the Python likelihood port (laser-cholera#47) can compute peak-timing / peak-magnitude shape terms without a runtime injection. v3.1 (2026-04-30): nu_jt_sources added explicitly (laser-cholera#102); eligible pool for first-dose OCV is [S, E, Isym, Iasym, R]. v3.0 (2026-04-23): zeta_1, zeta_2, and zeta_ratio placeholder defaults rescaled from Frame-B (70k / 300) to the biological scale (~2.1e11 / 4.5e4) implied by the literature meta-analysis in est_zeta_*_prior() (priors_default v15.0). v2.1: Refreshed psi_jt from LSTM refit on corrected ERA5 soil_moisture_0_to_10cm_mean (open-meteo-pipeline#5). v2.0: Updated defaults from MOZ calibration evidence (tests 19-28)."
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
# Write the JSON artifact, plus an optional .json.gz sidecar.
#
# Build the validated parameter list once (no output_file_path -> returns the
# validated list), attach tracking fields that make_LASER_config does not
# accept as args, then write the .json once with MOSAIC::write_json_with_optional_gz().
#
# The .json.gz sidecar is OFF by default. To produce it on a given run set
# the environment variable before invoking R:
#   MOSAIC_WRITE_GZ_SIDECARS=true Rscript data-raw/make_config_default.R
# The sidecar is a byte-for-byte gzip of the just-written .json (via
# R.utils::gzip), so the pair cannot drift.
# --------------------------------------------------------------------------- #

pkg_dir <- file.path(PATHS$ROOT, "MOSAIC-pkg")
fp_json <- file.path(pkg_dir, 'inst/extdata/default_parameters.json')

args <- config_default
args$metadata <- NULL          # excluded from JSON; kept on the .rda below
args$output_file_path <- NULL  # return the validated list instead of writing
params_validated <- do.call(MOSAIC::make_LASER_config, args)
rm(args)

# Tracking fields that make_LASER_config rejects as unknown args
params_validated$zeta_ratio       <- .zeta_ratio_default
params_validated$decay_days_spread <- .decay_days_spread_default

MOSAIC::write_json_with_optional_gz(
     params_validated,
     fp_json,
     gz_sidecar = MOSAIC:::.mosaic_write_gz_sidecars()
)

# Attach tracking fields to the rda-bound config_default and persist
config_default$zeta_ratio        <- .zeta_ratio_default
config_default$decay_days_spread <- .decay_days_spread_default

tmp_config <- MOSAIC::read_json_to_list(fp_json)
identical(config_default, tmp_config)
all.equal(config_default, tmp_config)

usethis::use_data(config_default, overwrite = TRUE)
