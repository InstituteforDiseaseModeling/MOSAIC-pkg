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
mu_j_epidemic_factor <- rep(0.5, length(j))
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

sel <- tmp$response == 'cases' & tmp$parameter == 'a1'
a1 <- tmp$mean[sel]
names(a1) <- tmp$country_iso_code[sel]
a1 <- a1[match(j, names(a1))]

sel <- tmp$response == 'cases' & tmp$parameter == 'a2'
a2 <- tmp$mean[sel]
names(a2) <- tmp$country_iso_code[sel]
a2 <- a2[match(j, names(a2))]

sel <- tmp$response == 'cases' & tmp$parameter == 'b1'
b1 <- tmp$mean[sel]
names(b1) <- tmp$country_iso_code[sel]
b1 <- b1[match(j, names(b1))]

sel <- tmp$response == 'cases' & tmp$parameter == 'b2'
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
# These match the priors in make_priors.R
beta_j0_tot_default <- 1e-6  # Total transmission rate (matching prior mode)
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
     phi_1 = 0.787,
     phi_2 = 0.768,
     omega_1 = 0.00073,
     omega_2 = 0.000485,
     iota = 1/1.4,
     gamma_1 = 0.2,
     gamma_2 = 0.1,
     epsilon = 0.0003,
     mu_jt = mu_jt,
     mu_j_baseline = mu_j_baseline,       # NEW: Baseline IFR (replaces mu_j)
     mu_j_slope = mu_j_slope,              # Temporal trend in IFR
     mu_j_epidemic_factor = mu_j_epidemic_factor,  # NEW: Epidemic increase factor
     sigma = 0.25,
     # Case reporting parameters for calc_cases_from_infections()
     rho = 0.7,                   # Proportion of symptomatic infections seeking care
     chi_endemic = 0.50,          # PPV among suspected cases during endemic periods (50%)
     chi_epidemic = 0.75,         # PPV among suspected cases during epidemic periods (75%)
     epidemic_threshold = 1/10000,  # Infection rate threshold for PPV switching (1 per 10k per day)
     delta_reporting_cases = 2,   # NEW: Infection-to-case report delay in days
     delta_reporting_deaths = 7,  # NEW: Infection-to-death report delay in days
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
     alpha_1 = 0.975,
     alpha_2 = 0.33,
     beta_j0_env = beta_j0_env,      # UPDATED: Now calculated from beta_j0_tot * (1 - p_beta)
     theta_j = theta_j,
     # psi_star calibration parameters (default to no transformation)
     psi_star_a = setNames(rep(1.0, length(j)), j),    # Default: no shape/gain transformation
     psi_star_b = setNames(rep(0.0, length(j)), j),    # Default: no scale/offset transformation
     psi_star_z = setNames(rep(1.0, length(j)), j),    # Default: no smoothing
     psi_star_k = setNames(rep(0.0, length(j)), j),    # Default: no time offset
     psi_jt = psi_jt,
     zeta_1 = 7.5,
     zeta_2 = 2.5,
     kappa = 10^6,
     decay_days_short = 3,
     decay_days_long = 120,
     decay_shape_1 = 5,
     decay_shape_2 = 2.5,
     reported_cases = mat_cases,
     reported_deaths = mat_deaths
)

config_default <- do.call(make_LASER_config, default_args)

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



# Define output file paths using PATHS
# The inst/extdata directory is in the MOSAIC-pkg directory
pkg_dir <- file.path(PATHS$ROOT, "MOSAIC-pkg")
file_paths <- list(
     file.path(pkg_dir, 'inst/extdata/default_parameters.json'),
     file.path(pkg_dir, 'inst/extdata/default_parameters.json.gz')
)



# Loop over the file paths and call make_LASER_config() for each
for (fp in file_paths) {

     args <- config_default
     args$output_file_path <- fp
     do.call(MOSAIC::make_LASER_config, args)
     rm(args)

}

tmp_config <- jsonlite::fromJSON(file_paths[[1]])

identical(config_default, tmp_config)
all.equal(config_default, tmp_config)

usethis::use_data(config_default, overwrite = TRUE)
