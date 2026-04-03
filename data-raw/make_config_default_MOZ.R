library(MOSAIC)

# make_config_default_MOZ.R - Generate MOZ-specific LASER config with extended date range
# Uses combined JHU + WHO surveillance data (2017-2025) instead of WHO-only (2023+)

MOSAIC::set_root_directory("/Users/johngiles/MOSAIC")
PATHS <- MOSAIC::get_paths()

# Start ~2 weeks before first positive obs (day 58 = 2017-08-15 in original series).
# The 43-day buffer allows moment-matched ICs to generate natural transmission
# dynamics before the first observed cases appear, avoiding phantom early cases.
date_start <- as.Date("2017-08-01")
date_stop <- as.Date("2026-03-31")

message("Set simulation time steps and locations")
t <- seq.Date(date_start, date_stop, by = "day")
j <- "MOZ"  # Single location

message("Get population size (N_j)")
tmp <- read.csv(file.path(PATHS$MODEL_INPUT, 'param_N_population_size.csv'))
tmp$t <- as.Date(tmp$t)
tmp <- tmp[tmp$j == j & tmp$t == date_start,]
N_j <- as.integer(tmp$parameter_value)
names(N_j) <- j

# Initial conditions: June 2017 is start of JHU surveillance for MOZ
# S=80%, I=100 for reasonable epidemic seeding
I_j <- as.integer(100)
names(I_j) <- j

E_j <- as.integer(0)
names(E_j) <- j

S_j <- N_j - as.integer(N_j * 0.8) - I_j
S_j <- as.integer(N_j * 0.8)
names(S_j) <- j

V1_j <- as.integer(0)
names(V1_j) <- j

V2_j <- as.integer(0)
names(V2_j) <- j

R_j <- N_j - S_j - E_j - I_j - V1_j - V2_j
R_j <- pmax(R_j, 0L)
names(R_j) <- j

message("Get birth rate (b_jt)")
tmp <- read.csv(file.path(PATHS$MODEL_INPUT, 'param_b_birth_rate.csv'))
tmp$t <- as.Date(tmp$t)
tmp <- tmp[tmp$j == j & tmp$t >= date_start & tmp$t <= date_stop,]
b_jt <- matrix(tmp$parameter_value, nrow = 1, dimnames = list(j, as.character(tmp$t)))

message("Get death rate (d_jt)")
tmp <- read.csv(file.path(PATHS$MODEL_INPUT, 'param_d_death_rate.csv'))
tmp$t <- as.Date(tmp$t)
tmp <- tmp[tmp$j == j & tmp$t >= date_start & tmp$t <= date_stop,]
d_jt <- matrix(tmp$parameter_value, nrow = 1, dimnames = list(j, as.character(tmp$t)))

# Populate mu_jt from hierarchical CFR estimates
message("Get case fatality rate (mu_jt)")
cfr_file <- file.path(PATHS$MODEL_INPUT, "param_mu_disease_mortality.csv")
if (file.exists(cfr_file)) {
     cfr_df <- read.csv(cfr_file)
     cfr_point <- cfr_df[cfr_df$parameter_distribution == "point", ]

     sim_years <- seq(as.numeric(format(date_start, "%Y")),
                      as.numeric(format(date_stop, "%Y")))

     # MOZ-specific mean as fallback
     cfr_moz <- cfr_point[cfr_point$j == j & cfr_point$t %in% sim_years, ]
     global_mean_cfr <- mean(cfr_moz$parameter_value, na.rm = TRUE)
     if (is.na(global_mean_cfr)) {
          cfr_subset <- cfr_point[cfr_point$t %in% sim_years, ]
          global_mean_cfr <- mean(cfr_subset$parameter_value, na.rm = TRUE)
     }

     mu_jt <- d_jt
     for (day_idx in seq_along(t)) {
          year <- as.numeric(format(t[day_idx], "%Y"))
          cfr_values <- cfr_point$parameter_value[cfr_point$j == j & cfr_point$t == year]
          if (length(cfr_values) > 0 && !all(is.na(cfr_values))) {
               mu_jt[1, day_idx] <- mean(cfr_values, na.rm = TRUE)
          } else {
               mu_jt[1, day_idx] <- global_mean_cfr
          }
     }
     message("mu_jt populated from CFR hierarchical model estimates")
} else {
     mu_jt <- d_jt
     mu_jt[,] <- 0.01
     warning("param_mu_disease_mortality.csv not found. Using default CFR of 0.01")
}

mu_j_baseline <- rowMeans(mu_jt, na.rm = TRUE)
names(mu_j_baseline) <- j

mu_j_slope <- setNames(-0.05, j)              # MOZ 7 best: -0.099; slight declining trend default
mu_j_epidemic_factor <- setNames(1.25, j)      # Prior mean of Gamma(2.5, 2.0); 2.25x mortality during epidemics

message("Get vaccination rate (nu_jt)")
if (file.exists(file.path(PATHS$MODEL_INPUT, 'param_nu_vaccination_rate_GTFCC_WHO.csv'))) {
     tmp <- read.csv(file.path(PATHS$MODEL_INPUT, 'param_nu_vaccination_rate_GTFCC_WHO.csv'))
     message("Using combined GTFCC+WHO vaccination data")
} else if (file.exists(file.path(PATHS$MODEL_INPUT, 'param_nu_vaccination_rate_WHO.csv'))) {
     tmp <- read.csv(file.path(PATHS$MODEL_INPUT, 'param_nu_vaccination_rate_WHO.csv'))
     message("Using WHO vaccination data")
} else {
     tmp <- read.csv(file.path(PATHS$MODEL_INPUT, 'param_nu_vaccination_rate.csv'))
     warning("Using legacy vaccination rate file")
}
tmp$t <- as.Date(tmp$t)
tmp <- tmp[tmp$j == j & tmp$t >= date_start & tmp$t <= date_stop,]
nu_jt <- matrix(tmp$parameter_value, nrow = 1, dimnames = list(j, as.character(tmp$t)))

nu_1_jt <- nu_jt
nu_2_jt <- nu_jt
nu_2_jt[,] <- 0

message("Get Fourier seasonality parameters")
tmp <- read.csv(file.path(PATHS$MODEL_INPUT, "param_seasonal_dynamics.csv"))
tmp <- tmp[tmp$country_iso_code == j & tmp$response == 'cases',]

a1 <- setNames(tmp$mean[tmp$parameter == 'a1'], j)
a2 <- setNames(tmp$mean[tmp$parameter == 'a2'], j)
b1 <- setNames(tmp$mean[tmp$parameter == 'b1'], j)
b2 <- setNames(tmp$mean[tmp$parameter == 'b2'], j)

message("Get departure probability (tau_i)")
tmp <- read.csv(file.path(PATHS$MODEL_INPUT, 'param_tau_departure.csv'))
tmp <- tmp[tmp$i == j & tmp$parameter_name == 'mean',]
tau_i <- setNames(tmp$parameter_value, j)

message("Get gravity model parameters")
tmp <- read.csv(file.path(PATHS$MODEL_INPUT, "mobility_lon_lat.csv"))
longitude <- setNames(tmp$lon[tmp$iso3 == j], j)
latitude <- setNames(tmp$lat[tmp$iso3 == j], j)

tmp <- read.csv(file.path(PATHS$MODEL_INPUT, "mobility_gravity_params.csv"), row.names = 1)
mobility_omega <- tmp['omega', 'mean']
mobility_gamma <- tmp['gamma', 'mean']

message("Get WASH coverage (theta_j)")
tmp <- read.csv(file.path(PATHS$MODEL_INPUT, 'param_theta_WASH.csv'))
theta_j <- setNames(tmp$parameter_value[tmp$j == j], j)

message("Set transmission parameters")
beta_j0_tot <- setNames(1e-5, j)
p_beta <- setNames(0.33, j)
beta_j0_hum <- p_beta * beta_j0_tot
beta_j0_env <- (1 - p_beta) * beta_j0_tot

message("Get environmental suitability (psi_jt)")
tmp <- read.csv(file.path(PATHS$MODEL_INPUT, 'pred_psi_suitability_day.csv'))
tmp$date <- as.Date(tmp$date)
tmp <- tmp[tmp$iso_code == j & tmp$date >= date_start & tmp$date <= date_stop,]
# Deduplicate until process_cholera_surveillance_data() is re-run with %G/%V fix
tmp <- aggregate(pred_smooth ~ date, data = tmp, FUN = mean)
tmp <- tmp[order(tmp$date),]
psi_jt <- matrix(tmp$pred_smooth, nrow = 1, dimnames = list(j, as.character(tmp$date)))

# KEY CHANGE: Read from combined daily surveillance (JHU + WHO) instead of WHO-only
message("Get reported cholera cases and deaths (combined JHU + WHO)")
surv_file <- file.path(PATHS$DATA_PROCESSED, "cholera/daily/cholera_surveillance_daily_combined.csv")
df_daily <- read.csv(surv_file, stringsAsFactors = FALSE)
df_daily$date <- as.Date(df_daily$date)
df_daily <- df_daily[df_daily$iso_code == j,]

mat_cases <- matrix(NA, nrow = 1, ncol = length(t), dimnames = list(j, as.character(t)))
mat_deaths <- matrix(NA, nrow = 1, ncol = length(t), dimnames = list(j, as.character(t)))

for (k in seq_along(t)) {
     row_idx <- which(df_daily$date == t[k])
     if (length(row_idx) >= 1) {
          mat_cases[1, k] <- df_daily$cases[row_idx[1]]
          mat_deaths[1, k] <- df_daily$deaths[row_idx[1]]
     }
}

n_obs_cases <- sum(!is.na(mat_cases))
n_obs_deaths <- sum(!is.na(mat_deaths))
message(sprintf("Surveillance data: %d days with cases, %d days with deaths (of %d total days)",
                n_obs_cases, n_obs_deaths, length(t)))

# Calculate initial condition proportions
prop_S_initial <- setNames(S_j / N_j, j)
prop_E_initial <- setNames(E_j / N_j, j)
prop_I_initial <- setNames(I_j / N_j, j)
prop_R_initial <- setNames(R_j / N_j, j)
prop_V1_initial <- setNames(V1_j / N_j, j)
prop_V2_initial <- setNames(V2_j / N_j, j)

# Validate proportions sum to 1
prop_sum <- prop_S_initial + prop_E_initial + prop_I_initial +
            prop_R_initial + prop_V1_initial + prop_V2_initial
if (abs(prop_sum - 1.0) > 1e-6) {
     warning(sprintf("Initial condition proportions don't sum to 1.0 for %s: sum = %.6f", j, prop_sum))
}

message("Building config via make_LASER_config()")

default_args <- list(
     output_file_path = NULL,
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
     gamma_1 = 0.1,
     gamma_2 = 0.5,
     epsilon = 0.0004,    # MOZ 2 best model: 4e-4 (~7yr immunity duration)
     mu_jt = mu_jt,
     mu_j_baseline = mu_j_baseline,
     mu_j_slope = mu_j_slope,
     mu_j_epidemic_factor = mu_j_epidemic_factor,
     sigma = 0.25,
     rho = 0.265,
     chi_endemic = 0.50,
     chi_epidemic = 0.75,
     epidemic_threshold = setNames(1/10000, j),
     delta_reporting_cases = 0,
     delta_reporting_deaths = 5,
     longitude = longitude,
     latitude = latitude,
     mobility_omega = mobility_omega,
     mobility_gamma = mobility_gamma,
     tau_i = tau_i,
     beta_j0_tot = beta_j0_tot,
     p_beta = p_beta,
     beta_j0_hum = beta_j0_hum,
     a_1_j = a1,
     a_2_j = a2,
     b_1_j = b1,
     b_2_j = b2,
     p = 365,
     alpha_1 = 0.27,
     alpha_2 = 0.50,
     beta_j0_env = beta_j0_env,
     theta_j = theta_j,
     psi_star_a = setNames(0.5, j),      # MOZ 7 best: 0.38; centered near new prior mean
     psi_star_b = setNames(0.0, j),      # MOZ 7 best: -1.93; neutral default for new prior
     psi_star_z = setNames(0.6, j),      # MOZ 7 best: 0.615; moderate smoothing
     psi_star_k = setNames(-10, j),      # MOZ 7 best: -21.1; centered near new prior mean
     psi_jt = psi_jt,
     zeta_1 = 665,
     zeta_2 = 10,
     kappa = 10^6,
     decay_days_short = 18,     # MOZ 7 best: 19.8 days
     decay_days_long = 365,     # Upper bound matches prior U(30, 365)
     decay_shape_1 = 6.0,      # MOZ 7 best: 6.85; rapid initial decay
     decay_shape_2 = 4.5,      # MOZ 7 best: 4.30; right-skewed decay profile
     reported_cases = mat_cases,
     reported_deaths = mat_deaths
)

config_default_MOZ <- do.call(make_LASER_config, default_args)

config_default_MOZ$metadata <- list(
     version = "2.3",
     date = as.character(Sys.Date()),
     description = "MOZ-specific LASER configuration with extended date range (2017-2026). Updated from test_31 MOZ 6-7 analysis. Uses combined JHU + WHO surveillance data."
)

message("Transmission parameter validation")
total_check <- config_default_MOZ$beta_j0_hum + config_default_MOZ$beta_j0_env
if (abs(total_check - beta_j0_tot) > 1e-10) {
     warning("Transmission parameter inconsistency: hum + env != tot")
}

# Save outputs
pkg_dir <- file.path(PATHS$ROOT, "MOSAIC-pkg")
file_paths <- list(
     file.path(pkg_dir, 'inst/extdata/default_parameters_MOZ.json'),
     file.path(pkg_dir, 'inst/extdata/default_parameters_MOZ.json.gz')
)

for (fp in file_paths) {
     args <- config_default_MOZ
     args$metadata <- NULL
     args$output_file_path <- fp
     do.call(MOSAIC::make_LASER_config, args)
     rm(args)
}

tmp_config <- jsonlite::fromJSON(file_paths[[1]])
all.equal(config_default_MOZ, tmp_config)

usethis::use_data(config_default_MOZ, overwrite = TRUE)

message("Done. config_default_MOZ saved to data/ and inst/extdata/")
