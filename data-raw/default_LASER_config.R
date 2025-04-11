library(MOSAIC)

date_start <- as.Date("2023-01-01")
date_stop <- as.Date("2024-12-17")

# Set simulation time steps
t <- seq.Date(date_start, date_stop, by = "day")
str(t)

# Set simulation locations
j <- MOSAIC::iso_codes_mosaic
str(j)

# Get population size of each location (N_j)
tmp <- read.csv(file.path(getwd(), 'model/input/param_N_population_size.csv'))
tmp$t <- as.Date(tmp$t)
tmp <- tmp[tmp$j %in% j & tmp$t == date_start,]
N_j <- tmp$parameter_value
names(N_j) <- tmp$j
sel <- match(j, names(N_j))
N_j <- N_j[sel]
str(N_j)

# Get number of susceptible individuals in each location
S_j <- N_j - 1

# Get number of infected individuals in each location
I_j <- N_j
I_j[] <- 1

# Get birth rate of each location (b_j)
tmp <- read.csv(file.path(getwd(), 'model/input/param_b_birth_rate.csv'))
tmp$t <- as.Date(tmp$t)
tmp <- tmp[tmp$j %in% j,]
tmp <- tmp[tmp$t >= date_start & tmp$t <= date_stop,]
b_jt <- reshape2::acast(tmp, j ~ t, value.var = "parameter_value")
sel <- match(j, row.names(b_jt))
b_jt <- b_jt[sel,]
sel <- match(t, colnames(b_jt))
b_jt <- b_jt[,sel]
str(b_jt)

# Get death rate of each location (d_j)
tmp <- read.csv(file.path(getwd(), 'model/input/param_d_death_rate.csv'))
tmp$t <- as.Date(tmp$t)
tmp <- tmp[tmp$j %in% j,]
tmp <- tmp[tmp$t >= date_start & tmp$t <= date_stop,]
d_jt <- reshape2::acast(tmp, j ~ t, value.var = "parameter_value")
sel <- match(j, row.names(d_jt))
d_jt <- d_jt[sel,]
sel <- match(t, colnames(d_jt))
d_jt <- d_jt[,sel]
str(d_jt)

# Stealing dimensions from demographics to define IFR
# Note the matrix allow for time-varying IFR but we leave this constant for now
mu_jt <- d_jt
mu_jt[,] <- 0.01
str(mu_jt)

#####
# Vaccination rate needs work
# Must be square by dates and locations
#####

# Add vaccination rate over time for each location (nu_jt)
tmp <- read.csv(file.path(getwd(), 'model/input/param_nu_vaccination_rate.csv'))
tmp$t <- as.Date(tmp$t)
tmp <- tmp[tmp$j %in% j,]
tmp <- tmp[tmp$t >= date_start & tmp$t <= date_stop,]
nu_jt <- reshape2::acast(tmp, j ~ t, value.var = "parameter_value")

nu_1_jt <- nu_2_jt <- nu_jt
nu_2_jt[,] <- 0

# Add fourier params for seasonal force of infection
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


# Get departure probability of each location (tau_j)
tmp <- read.csv(file.path(getwd(), 'model/input/param_tau_departure.csv'))
tmp <- tmp[tmp$i %in% j,]
tmp <- tmp[tmp$parameter_name =='mean',]
sel <- match(j, tmp$i)
tau_i <- tmp$parameter_value[sel]
names(tau_i) <- tmp$i[sel]
str(tau_i)

# Gravity model parameters
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

# Get WASH variables for each location
tmp <- read.csv(file.path(getwd(), 'model/input/param_theta_WASH.csv'))
tmp <- tmp[tmp$j %in% j,]
sel <- match(j, tmp$j)
theta_j <- tmp$parameter_value[sel]
names(theta_j) <- tmp$j[sel]
str(theta_j)

# Get environmental suitability (psi) for each location
tmp <- read.csv(file.path(getwd(), 'model/input/pred_psi_suitability_day.csv'))
tmp <- tmp[tmp$iso_code %in% j,]
tmp <- tmp[tmp$date >= date_start & tmp$date <= date_stop,]
psi_jt <- reshape2::acast(tmp, iso_code ~ date, value.var = "pred_smooth")
sel <- match(j, row.names(psi_jt))
psi_jt <- psi_jt[sel,]
str(psi_jt)




# Define output file paths for all seven formats
file_paths <- list(
     file.path(getwd(), 'inst/extdata/default_parameters.json'),
     file.path(getwd(), 'inst/extdata/default_parameters.json.gz')#,
     #file.path(getwd(), 'inst/extdata/default_parameters.h5'),
     #file.path(getwd(), 'inst/extdata/default_parameters.h5.gz'),
     #file.path(getwd(), 'inst/extdata/default_parameters.yaml'),
     #file.path(getwd(), 'inst/extdata/default_parameters.yaml.gz'),
     #file.path(getwd(), 'inst/extdata/default_parameters.obj'),
     #file.path(getwd(), 'inst/extdata/default_parameters.obj.gz')
)


# Define a base list of arguments (all parameters that are common to all calls)
base_args <- list(
     seed = 123,
     date_start = date_start,
     date_stop = date_stop,
     location_name = j,
     S_j_initial = S_j,
     E_j_initial = N_j * 0,
     I_j_initial = I_j,
     R_j_initial = N_j * 0,
     V1_j_initial = N_j * 0,
     V2_j_initial = N_j * 0,
     b_jt = b_jt,
     d_jt = b_jt,
     nu_1_jt = nu_1_jt,
     nu_2_jt = nu_2_jt,
     phi_1 = 0.64,
     phi_2 = 0.85,
     omega_1 = 0.0006,
     omega_2 = 0.0004,
     iota = 1/1.4,
     gamma_1 = 0.14,
     gamma_2 = 0.33,
     epsilon = 0.0003,
     mu_jt = mu_jt,
     rho = 0.52,
     sigma = 0.24,
     longitude         = longitude,
     latitude          = latitude,
     mobility_omega    = mobility_omega,
     mobility_gamma    = mobility_gamma,
     tau_i             = tau_i,
     beta_j0_hum = rep(0.1, length(j)),
     a_1_j = a1,
     a_2_j = a2,
     b_1_j = b1,
     b_2_j = b2,
     p     = 365,
     alpha_1 = 0.95,
     alpha_2 = 0.95,
     beta_j0_env = rep(0.2, length(j)),
     theta_j = theta_j,
     psi_jt = psi_jt,
     zeta_1 = 7.5,
     zeta_2 = 2.5,
     kappa = 10^5,
     decay_days_short = 3,
     decay_days_long = 90,
     decay_shape_1 = 1,
     decay_shape_2 = 1,
     return = c("LL", "S", "E", "I", "R", "V1", "V2", "W", "C", "D")
)


# Loop over the file paths and call make_LASER_config() for each one.
for (fp in file_paths) {

     compress_flag <- grepl("\\.gz$", fp, ignore.case = TRUE)
     args <- c(list(output_file_path = fp, compress = compress_flag), base_args)
     do.call(make_LASER_config, args)
}


config <- jsonlite::fromJSON(file_paths[[1]])

