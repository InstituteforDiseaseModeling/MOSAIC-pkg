
path_to_json_file <- file.path(getwd(), 'inst/extdata/default_parameters.json')

date_start <- as.Date("2023-01-01")
date_stop <- as.Date("2024-12-17")

# Set simulation time steps
t <- seq.Date(date_start, date_stop, by = "day")
str(t)

# Ste simulation locations
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
S_j <- N_j - 10

# Get number of infected individuals in each location
I_j <- N_j
I_j[] <- 10

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


# Add seasonal pattern to human to human force of infection (daily time scale)
beta_j0_hum <- rep(0.2, length(j))

tmp <- read.csv(file.path(getwd(), 'model/input/pred_seasonal_dynamics_day.csv'))
tmp <- tmp[tmp$iso_code %in% j,]
beta_j_seasonality <- acast(tmp, iso_code ~ day, value.var = "fitted_values_fourier_cases")
sel <- match(j, row.names(beta_j_seasonality))
beta_j_seasonality <- beta_j_seasonality[sel,]
str(beta_j_seasonality)

# Adjust so that beta_j_seasonality is expressed in units of transmission rate (instead of units of standard deviation)
max_z_score <- ceiling(max(abs(beta_j_seasonality))*10)/10
beta_j0_hum/max_z_score

# Get departure probability of each location (tau_j)
tmp <- read.csv(file.path(getwd(), 'model/input/param_tau_departure.csv'))
tmp <- tmp[tmp$i %in% j,]
tmp <- tmp[tmp$parameter_name =='mean',]
sel <- match(j, tmp$i)
tau_i <- tmp$parameter_value[sel]
names(tau_i) <- tmp$i[sel]
str(tau_i)

# Get diffusion probability between all locations (pi_ij)
tmp <- read.csv(file.path(getwd(), 'model/input/pred_pi_diffusion.csv'))
tmp <- tmp[tmp$origin %in% j,]
tmp <- tmp[tmp$destination %in% j,]
pi_ij <- reshape2::acast(tmp, origin ~ destination, value.var = "value")
sel_rows <- match(j, row.names(pi_ij))
sel_cols <- match(j, colnames(pi_ij))
pi_ij <- pi_ij[sel_rows, sel_cols]
str(pi_ij)

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

make_LASER_config(
     output_file_path = path_to_json_file,
     date_start = date_start,
     date_stop = date_stop,
     location_id = seq_along(j),
     location_name = j,
     N_j_initial = N_j,
     S_j_initial = S_j,
     E_j_initial = N_j*0,
     I_j_initial = I_j,
     R_j_initial = N_j*0,
     V1_j_initial = N_j*0,
     V2_j_initial = N_j*0,
     b_jt = b_jt,
     d_jt = b_jt,
     nu_jt = nu_jt, # No vaccination
     phi = 0.64,
     omega = 0.00057,
     epsilon = 0.00039,
     gamma = 0.1,
     mu = 0.015,
     rho = 0.52,
     sigma = 0.24,
     beta_j0_hum = rep(0.2, length(j)),
     beta_j_seasonality = beta_j_seasonality,
     tau_i = tau_i,
     pi_ij = pi_ij,
     alpha = 0.95,
     beta_j0_env = rep(0.4, length(j)), # Assume environmental transmission is twice that of human to human
     theta_j = theta_j,
     psi_jt = psi_jt,
     zeta = 0.5,
     kappa = 10^5,
     delta_min = 1/3,
     delta_max = 1/90
)

config <- jsonlite::fromJSON(path_to_json_file)


