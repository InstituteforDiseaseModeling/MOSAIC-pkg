
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

nu_1_jt <- nu_2_jt <- nu_jt
nu_2_jt[,] <- 0

# Add seasonal pattern to human to human force of infection (daily time scale)
beta_j0_hum <- rep(0.2, length(j))

tmp <- read.csv(file.path(getwd(), 'model/input/pred_seasonal_dynamics_day.csv'))
tmp <- tmp[tmp$iso_code %in% j,]
beta_j_seasonality <- reshape2::acast(tmp, iso_code ~ day, value.var = "fitted_values_fourier_cases")
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

# Create default configuration files in all six formats

# Define output file paths for all six formats
path_to_json_file     <- file.path(getwd(), 'inst/extdata/default_parameters.json')
path_to_json_gz_file  <- file.path(getwd(), 'inst/extdata/default_parameters.json.gz')
path_to_h5_file       <- file.path(getwd(), 'inst/extdata/default_parameters.h5')
path_to_h5_gz_file    <- file.path(getwd(), 'inst/extdata/default_parameters.h5.gz')
path_to_yaml_file     <- file.path(getwd(), 'inst/extdata/default_parameters.yaml')
path_to_yaml_gz_file  <- file.path(getwd(), 'inst/extdata/default_parameters.yaml.gz')

# Define the six output file paths
file_paths <- list(
     path_to_json_file,      # .json
     path_to_json_gz_file,   # .json.gz
     path_to_h5_file,        # .h5
     path_to_h5_gz_file,     # .h5.gz
     path_to_yaml_file,      # .yaml
     path_to_yaml_gz_file    # .yaml.gz
)

# Define a base list of arguments (all parameters that are common to all calls)
base_args <- list(
     seed = 123,
     date_start = date_start,
     date_stop = date_stop,
     location_id = seq_along(j),
     location_name = j,
     N_j_initial = N_j,
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
     mu = 0.015,
     rho = 0.52,
     sigma = 0.24,
     beta_j0_hum = rep(0.2, length(j)),
     beta_j_seasonality = beta_j_seasonality,
     tau_i = tau_i,
     pi_ij = pi_ij,
     alpha_1 = 0.95,
     alpha_2 = 0.95,
     beta_j0_env = rep(0.4, length(j)),
     theta_j = theta_j,
     psi_jt = psi_jt,
     zeta_1 = 7.5,
     zeta_2 = 2.5,
     kappa = 10^5,
     delta_min = 1/3,
     delta_max = 1/90
)

# Loop over the file paths and call make_LASER_config() for each one.
for (fp in file_paths) {
     # Set compress_flag to TRUE if the filename ends with .gz; otherwise, use the default compress value.
     compress_flag <- grepl("\\.gz$", fp, ignore.case = TRUE)

     # Combine the output file path and compress flag with the base arguments.
     args <- c(list(output_file_path = fp, compress = compress_flag), base_args)

     # Call make_LASER_config with all the arguments.
     do.call(make_LASER_config, args)
}

### Testing ###


#####
# Testing Section
#####

# Load required packages
library(reshape2)
library(ggplot2)

# Create temporary file paths for each of the six file formats
test_file_paths <- list(
     json      = tempfile(pattern = "default_params", fileext = ".json"),
     json_gz   = tempfile(pattern = "default_params", fileext = ".json.gz"),
     h5        = tempfile(pattern = "default_params", fileext = ".h5"),
     h5_gz     = tempfile(pattern = "default_params", fileext = ".h5.gz"),
     yaml      = tempfile(pattern = "default_params", fileext = ".yaml"),
     yaml_gz   = tempfile(pattern = "default_params", fileext = ".yaml.gz")
)

# Create a data frame to store results
results <- data.frame(
     format     = character(),
     file_path  = character(),
     write_time = numeric(),
     read_time  = numeric(),
     file_size  = numeric(),
     stringsAsFactors = FALSE
)

# Loop over each file format, write the config, record times, and get file size
for(fmt in names(test_file_paths)) {
     fp <- test_file_paths[[fmt]]

     # Set compress flag: if the file name ends with ".gz", compress is TRUE.
     compress_flag <- grepl("\\.gz$", fp, ignore.case = TRUE)

     # Combine base arguments with the output file path and compress flag
     args <- c(list(output_file_path = fp, compress = compress_flag), base_args)

     # Write the configuration file and record the elapsed write time
     write_time <- system.time({
          do.call(make_LASER_config, args)
     })["elapsed"]

     # Choose the appropriate read function based on the file extension
     read_func <- NULL
     if (grepl("\\.json", fp)) {
          read_func <- read_json_to_list
     } else if (grepl("\\.yaml", fp)) {
          read_func <- read_yaml_to_list
     } else if (grepl("\\.h5", fp)) {
          read_func <- read_hdf5_to_list
     }

     # Read the configuration file and record the elapsed read time
     read_time <- system.time({
          config_list <- read_func(fp)
     })["elapsed"]

     # Get the file size in bytes
     file_size <- file.info(fp)$size

     # Append the results to the data frame
     results <- rbind(results, data.frame(
          format     = fmt,
          file_path  = fp,
          write_time = write_time,
          read_time  = read_time,
          file_size  = file_size,
          stringsAsFactors = FALSE
     ))
}

# Print the resulting data frame
print(results)

# Reshape data to long format for plotting the write and read times
results_long <- melt(results,
                     id.vars = c("format", "file_path", "file_size"),
                     measure.vars = c("write_time", "read_time"),
                     variable.name = "operation",
                     value.name = "time")

# Plot write and read times by file format
ggplot(results_long, aes(x = format, y = time, fill = operation)) +
     geom_bar(stat = "identity", position = "dodge") +
     labs(title = "Write and Read Times by File Format",
          x = "File Format", y = "Time (seconds)") +
     theme_minimal()

# Plot file sizes by file format
ggplot(results, aes(x = format, y = file_size)) +
     geom_bar(stat = "identity", fill = "steelblue") +
     labs(title = "File Size by Format",
          x = "File Format", y = "File Size (bytes)") +
     theme_minimal()


# Derive FileType and Compression columns from the 'results' data frame
results$FileType <- gsub("_gz", "", results$format)
results$Compression <- ifelse(grepl("gz", results$format), "Compressed", "Uncompressed")
results$Label <- paste(results$FileType, "(", results$Compression, ")")

# Scatter plot: File Size vs Write Time
p1 <- ggplot(results, aes(x = file_size, y = write_time, color = Label, shape = Label)) +
     geom_point(size = 3) +
     labs(title = "File Size vs Write Time",
          x = "File Size (bytes)",
          y = "Write Time (seconds)",
          color = "File Type",
          shape = "File Type") +
     theme_bw()

# Scatter plot: File Size vs Read Time
p2 <- ggplot(results, aes(x = file_size, y = read_time, color = Label, shape = Label)) +
     geom_point(size = 3) +
     labs(title = "File Size vs Read Time",
          x = "File Size (bytes)",
          y = "Read Time (seconds)",
          color = "File Type",
          shape = "File Type") +
     theme_bw()

# Scatter plot: Write Time vs Read Time
p3 <- ggplot(results, aes(x = write_time, y = read_time, color = Label, shape = Label)) +
     geom_point(size = 3) +
     labs(title = "Write Time vs Read Time",
          x = "Write Time (seconds)",
          y = "Read Time (seconds)",
          color = "File Type",
          shape = "File Type") +
     theme_bw()

# Print the plots
print(p1)
print(p2)
print(p3)





if (F) {

     # JSON (plain)
     make_LASER_config(
          output_file_path   = path_to_json_file,
          compress = TRUE,
          seed = 123,
          date_start         = date_start,
          date_stop          = date_stop,
          location_id        = seq_along(j),
          location_name      = j,
          N_j_initial        = N_j,
          S_j_initial        = S_j,
          E_j_initial        = N_j * 0,
          I_j_initial        = I_j,
          R_j_initial        = N_j * 0,
          V1_j_initial       = N_j * 0,
          V2_j_initial       = N_j * 0,
          b_jt               = b_jt,
          d_jt               = b_jt,
          nu_1_jt            = nu_1_jt,
          nu_2_jt            = nu_2_jt,
          phi_1              = 0.64,
          phi_2              = 0.85,
          omega_1            = 0.0006,
          omega_2            = 0.0004,
          iota               = 1/1.4,
          gamma_1            = 0.14,
          gamma_2            = 0.33,
          epsilon            = 0.0003,
          mu                 = 0.015,
          rho                = 0.52,
          sigma              = 0.24,
          beta_j0_hum        = rep(0.2, length(j)),
          beta_j_seasonality = beta_j_seasonality,
          tau_i              = tau_i,
          pi_ij              = pi_ij,
          alpha_1            = 0.95,
          alpha_2            = 0.95,
          beta_j0_env        = rep(0.4, length(j)),
          theta_j            = theta_j,
          psi_jt             = psi_jt,
          zeta_1             = 7.5,
          zeta_2             = 2.5,
          kappa              = 10^5,
          delta_min          = 1/3,
          delta_max          = 1/90
     )

     # JSON (gzipped)
     make_LASER_config(
          output_file_path   = path_to_json_gz_file,
          compress = TRUE,
          seed = 123,
          date_start         = date_start,
          date_stop          = date_stop,
          location_id        = seq_along(j),
          location_name      = j,
          N_j_initial        = N_j,
          S_j_initial        = S_j,
          E_j_initial        = N_j * 0,
          I_j_initial        = I_j,
          R_j_initial        = N_j * 0,
          V1_j_initial       = N_j * 0,
          V2_j_initial       = N_j * 0,
          b_jt               = b_jt,
          d_jt               = b_jt,
          nu_1_jt            = nu_1_jt,
          nu_2_jt            = nu_2_jt,
          phi_1              = 0.64,
          phi_2              = 0.85,
          omega_1            = 0.0006,
          omega_2            = 0.0004,
          iota               = 1/1.4,
          gamma_1            = 0.14,
          gamma_2            = 0.33,
          epsilon            = 0.0003,
          mu                 = 0.015,
          rho                = 0.52,
          sigma              = 0.24,
          beta_j0_hum        = rep(0.2, length(j)),
          beta_j_seasonality = beta_j_seasonality,
          tau_i              = tau_i,
          pi_ij              = pi_ij,
          alpha_1            = 0.95,
          alpha_2            = 0.95,
          beta_j0_env        = rep(0.4, length(j)),
          theta_j            = theta_j,
          psi_jt             = psi_jt,
          zeta_1             = 7.5,
          zeta_2             = 2.5,
          kappa              = 10^5,
          delta_min          = 1/3,
          delta_max          = 1/90
     )

     # HDF5 (plain)
     make_LASER_config(
          output_file_path   = path_to_h5_file,
          compress = FALSE,
          seed = 123,
          date_start         = date_start,
          date_stop          = date_stop,
          location_id        = seq_along(j),
          location_name      = j,
          N_j_initial        = N_j,
          S_j_initial        = S_j,
          E_j_initial        = N_j * 0,
          I_j_initial        = I_j,
          R_j_initial        = N_j * 0,
          V1_j_initial       = N_j * 0,
          V2_j_initial       = N_j * 0,
          b_jt               = b_jt,
          d_jt               = b_jt,
          nu_1_jt            = nu_1_jt,
          nu_2_jt            = nu_2_jt,
          phi_1              = 0.64,
          phi_2              = 0.85,
          omega_1            = 0.0006,
          omega_2            = 0.0004,
          iota               = 1/1.4,
          gamma_1            = 0.14,
          gamma_2            = 0.33,
          epsilon            = 0.0003,
          mu                 = 0.015,
          rho                = 0.52,
          sigma              = 0.24,
          beta_j0_hum        = rep(0.2, length(j)),
          beta_j_seasonality = beta_j_seasonality,
          tau_i              = tau_i,
          pi_ij              = pi_ij,
          alpha_1            = 0.95,
          alpha_2            = 0.95,
          beta_j0_env        = rep(0.4, length(j)),
          theta_j            = theta_j,
          psi_jt             = psi_jt,
          zeta_1             = 7.5,
          zeta_2             = 2.5,
          kappa              = 10^5,
          delta_min          = 1/3,
          delta_max          = 1/90
     )

     # HDF5 (gzipped)
     make_LASER_config(
          output_file_path   = path_to_h5_gz_file,
          compress = TRUE,
          seed = 123,
          date_start         = date_start,
          date_stop          = date_stop,
          location_id        = seq_along(j),
          location_name      = j,
          N_j_initial        = N_j,
          S_j_initial        = S_j,
          E_j_initial        = N_j * 0,
          I_j_initial        = I_j,
          R_j_initial        = N_j * 0,
          V1_j_initial       = N_j * 0,
          V2_j_initial       = N_j * 0,
          b_jt               = b_jt,
          d_jt               = b_jt,
          nu_1_jt            = nu_1_jt,
          nu_2_jt            = nu_2_jt,
          phi_1              = 0.64,
          phi_2              = 0.85,
          omega_1            = 0.0006,
          omega_2            = 0.0004,
          iota               = 1/1.4,
          gamma_1            = 0.14,
          gamma_2            = 0.33,
          epsilon            = 0.0003,
          mu                 = 0.015,
          rho                = 0.52,
          sigma              = 0.24,
          beta_j0_hum        = rep(0.2, length(j)),
          beta_j_seasonality = beta_j_seasonality,
          tau_i              = tau_i,
          pi_ij              = pi_ij,
          alpha_1            = 0.95,
          alpha_2            = 0.95,
          beta_j0_env        = rep(0.4, length(j)),
          theta_j            = theta_j,
          psi_jt             = psi_jt,
          zeta_1             = 7.5,
          zeta_2             = 2.5,
          kappa              = 10^5,
          delta_min          = 1/3,
          delta_max          = 1/90
     )

     # YAML (plain)
     make_LASER_config(
          output_file_path   = path_to_yaml_file,
          compress = FALSE,
          seed = 123,
          date_start         = date_start,
          date_stop          = date_stop,
          location_id        = seq_along(j),
          location_name      = j,
          N_j_initial        = N_j,
          S_j_initial        = S_j,
          E_j_initial        = N_j * 0,
          I_j_initial        = I_j,
          R_j_initial        = N_j * 0,
          V1_j_initial       = N_j * 0,
          V2_j_initial       = N_j * 0,
          b_jt               = b_jt,
          d_jt               = b_jt,
          nu_1_jt            = nu_1_jt,
          nu_2_jt            = nu_2_jt,
          phi_1              = 0.64,
          phi_2              = 0.85,
          omega_1            = 0.0006,
          omega_2            = 0.0004,
          iota               = 1/1.4,
          gamma_1            = 0.14,
          gamma_2            = 0.33,
          epsilon            = 0.0003,
          mu                 = 0.015,
          rho                = 0.52,
          sigma              = 0.24,
          beta_j0_hum        = rep(0.2, length(j)),
          beta_j_seasonality = beta_j_seasonality,
          tau_i              = tau_i,
          pi_ij              = pi_ij,
          alpha_1            = 0.95,
          alpha_2            = 0.95,
          beta_j0_env        = rep(0.4, length(j)),
          theta_j            = theta_j,
          psi_jt             = psi_jt,
          zeta_1             = 7.5,
          zeta_2             = 2.5,
          kappa              = 10^5,
          delta_min          = 1/3,
          delta_max          = 1/90
     )

     # YAML (gzipped)
     make_LASER_config(
          output_file_path   = path_to_yaml_gz_file,
          compress = TRUE,
          seed = 123,
          date_start         = date_start,
          date_stop          = date_stop,
          location_id        = seq_along(j),
          location_name      = j,
          N_j_initial        = N_j,
          S_j_initial        = S_j,
          E_j_initial        = N_j * 0,
          I_j_initial        = I_j,
          R_j_initial        = N_j * 0,
          V1_j_initial       = N_j * 0,
          V2_j_initial       = N_j * 0,
          b_jt               = b_jt,
          d_jt               = b_jt,
          nu_1_jt            = nu_1_jt,
          nu_2_jt            = nu_2_jt,
          phi_1              = 0.64,
          phi_2              = 0.85,
          omega_1            = 0.0006,
          omega_2            = 0.0004,
          iota               = 1/1.4,
          gamma_1            = 0.14,
          gamma_2            = 0.33,
          epsilon            = 0.0003,
          mu                 = 0.015,
          rho                = 0.52,
          sigma              = 0.24,
          beta_j0_hum        = rep(0.2, length(j)),
          beta_j_seasonality = beta_j_seasonality,
          tau_i              = tau_i,
          pi_ij              = pi_ij,
          alpha_1            = 0.95,
          alpha_2            = 0.95,
          beta_j0_env        = rep(0.4, length(j)),
          theta_j            = theta_j,
          psi_jt             = psi_jt,
          zeta_1             = 7.5,
          zeta_2             = 2.5,
          kappa              = 10^5,
          delta_min          = 1/3,
          delta_max          = 1/90
     )

     config <- jsonlite::fromJSON(path_to_json_file)


}
