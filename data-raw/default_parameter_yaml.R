
path_to_yaml_file <- file.path(getwd(), 'inst/extdata/default_parameters.yaml')

date_start <- as.Date("2023-01-01")
date_stop <- as.Date("2024-12-31")
t <- seq.Date(date_start, date_stop, by = "day")
j <- MOSAIC::iso_codes_mosaic

# Get birth rate of each location (b_j)
tmp <- read.csv(file.path(getwd(), 'model/input/param_b_birth_rate.csv'))
sel <- match(j, tmp$j)
b_j <- tmp$parameter_value[sel]
names(b_j) <- tmp$j[sel]

# Get death rate of each location (d_j)
tmp <- read.csv(file.path(getwd(), 'model/input/param_d_death_rate.csv'))
sel <- match(j, tmp$j)
d_j <- tmp$parameter_value[sel]
names(d_j) <- tmp$j[sel]

# Get population size of each location (N_j)
tmp <- read.csv(file.path(getwd(), 'model/input/param_N_population_size.csv'))
sel <- match(j, tmp$j)
N_j <- tmp$parameter_value[sel]
names(N_j) <- tmp$j[sel]

# Get departure probability of each location (tau_j)
tmp <- read.csv(file.path(getwd(), 'model/input/param_tau_departure.csv'))
tmp <- tmp[tmp$parameter_name =='mean',]
sel <- match(j, tmp$i)
tau_i <- tmp$parameter_value[sel]
names(tau_i) <- tmp$i[sel]

# Get diffusion probability between all locations (pi_ij)
tmp <- read.csv(file.path(getwd(), 'model/input/pred_pi_diffusion.csv'))
pi_ij <- reshape2::acast(tmp, origin ~ destination, value.var = "value")

# Get WASH variables for each location
tmp <- read.csv(file.path(getwd(), 'model/input/WASH_weighted_mean_theta.csv'))
theta_j <- tmp$Weighted_Mean_WASH
names(theta_j) <- tmp$iso_code

# Get environmental suitability (psi) for each location
tmp <- read.csv(file.path(getwd(), 'model/input/pred_psi_suitability.csv'))
theta_j <- tmp$Weighted_Mean_WASH
names(theta_j) <- tmp$iso_code

make_param_yaml(
     output_file_path = path_to_yaml_file,
     date_start = date_start,
     date_stop = date_stop,
     location_id = seq_along(j),
     location_name = j,
     N_j_initial = N_j,
     S_j_initial = round(N_j*0.99),
     I_j_initial = round(N_j*0.01),
     R_j_initial = rep(0, length(N_j)),
     b_j = b_j,
     d_j = b_j,
     nu_jt = matrix(data = 0, nrow = length(j), ncol = length(t)), # No vaccination
     phi = 0.64,
     omega = 0.00057,
     epsilon = 0.00039,
     gamma = 0.1,
     mu = 0.015,
     rho = 0.52,
     sigma = 0.24,
     beta_j0_hum = rep(0.2, length(j)),
     tau_i = tau_i,
     pi_ij = pi_ij,
     alpha = 0.95,
     beta_j0_env = rep(0.4, length(j)), # Assume environmental transmission is twice that of human to human
     theta_j = theta_j,
     psi_jt = matrix(data = 0, nrow = 2, ncol = 31),
     zeta = 0.5,
     kappa = 10^5,
     delta_min = 1/3,
     delta_max = 1/90
)

yaml_contents <- yaml::read_yaml(path_to_yaml_file)
print(yaml_contents)

