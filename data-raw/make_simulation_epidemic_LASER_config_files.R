# ---------------------------------------------------------------------------
# make_simulated_LASER_config_files.R
# ---------------------------------------------------------------------------
# Generate a *toy* LASER parameter configuration for unit‑testing and examples.
# The script is intentionally self‑contained: it does **not** rely on external
# CSVs or APIs.  All inputs are procedurally generated so the file can be run
# from a clean checkout of the MOSAIC package.
#
# Output
#   inst/extdata/simulated_parameters.json
#   inst/extdata/simulated_parameters.json.gz
#
# Usage
#   source("make_simulated_LASER_config_files.R")
# ---------------------------------------------------------------------------

library(MOSAIC)
library(jsonlite)

seed <- as.integer(999999999)
set.seed(seed)

# --------------------------- 1. Time & locations --------------------------- #

date_start <- as.Date("2020-01-01")
date_stop  <- as.Date("2020-12-31")
t          <- seq.Date(date_start, date_stop, by = "day")

# Three realistic ISO‑alpha‑3 codes present in `MOSAIC::iso_codes_mosaic`.
# The **choice of codes is arbitrary** – only the count matters for the toy.
# Populations are small to keep run‑times short.

j      <- c("FOO", "BAR", "BAZ")
n_loc  <- length(j)
N_j    <- c(5000, 10000, 20000)           # total population per site
names(N_j) <- j

# --------------------------- 2. Initial states ---------------------------- #

S_prop <- 0.5                                 # 0.5 % initially infected/exp.
S_j    <- as.integer(N_j * S_prop)
V1_j <- as.integer(c(N_j[1]*0.2, N_j[2]*0.3, N_j[3]*0.1))
V2_j <- rep(0L, n_loc)
E_j <- rep(0L, n_loc)
I_j <- rep(0L, n_loc); I_j[1] <- 3L             # seed infections in site 1
R_j <- as.integer(N_j - S_j - I_j - E_j - V1_j - V2_j)


# --------------------------- 3. Demography -------------------------------- #

b_rate <- 0.00005                                # births ≈ 1.8 % yr‑¹
d_rate <- 0.00004                                # deaths ≈ 1.5 % yr‑¹

b_jt <- matrix(b_rate,  n_loc, length(t), dimnames = list(j, t))
d_jt <- matrix(d_rate, n_loc, length(t), dimnames = list(j, t))
mu_jt <- matrix(0.01,  n_loc, length(t), dimnames = list(j, t))  # IFR

nu_1_jt <- nu_2_jt <- matrix(0, n_loc, length(t), dimnames = list(j, t))

# --------------------------- 4. Transmission ------------------------------ #

baseline_beta <- runif(n_loc, 0.30, 0.40)
amp_beta      <- 0.15
phase_shift   <- runif(n_loc, 0, 2 * pi)

p <- 365                                        # season length (days)
a_1_j <-  amp_beta * cos(phase_shift)
a_2_j <- rep(0, n_loc)
b_1_j <-  amp_beta * sin(phase_shift)
b_2_j <- rep(0, n_loc)


# --------------------------- 5. Mobility ----------------------------------- #

longitude <- c(-1.0232, 45.9062, 27.8493); names(longitude) <- j
latitude  <- c( 7.9465, -0.0236, -13.1339); names(latitude) <- j

mobility_omega <- 2e-5
mobility_gamma <- 2
tau_i          <- setNames(rep(0.10, n_loc), j)   # daily depart probability

# --------------------------- 6. Environment & WASH ------------------------ #

psi_jt  <- matrix(0.7,  n_loc, length(t), dimnames = list(j, t))  # flat env.
theta_j <- setNames(rep(0.3, n_loc), j)

# --------------------------- 7. Observed data ----------------------------- #

mat_cases  <- matrix(NA, n_loc, length(t), dimnames = list(j, t))
mat_deaths <- matrix(NA, n_loc, length(t), dimnames = list(j, t))

# --------------------------- 8. Wrap into config -------------------------- #

sim_args <- list(
     output_file_path = NULL,
     seed             = seed,
     date_start       = date_start,
     date_stop        = date_stop,
     location_name    = j,
     N_j_initial      = N_j,
     S_j_initial      = S_j,
     E_j_initial      = E_j,
     I_j_initial      = I_j,
     R_j_initial      = R_j,
     V1_j_initial     = V1_j,
     V2_j_initial     = V2_j,
     b_jt             = b_jt,
     d_jt             = d_jt,
     nu_1_jt          = nu_1_jt,
     nu_2_jt          = nu_2_jt,
     phi_1            = 0.64,
     phi_2            = 0.85,
     omega_1          = 0.0006,
     omega_2          = 0.0004,
     iota             = 1/1.4,
     gamma_1          = 0.2,
     gamma_2          = 0.1,
     epsilon          = 0.0003,
     mu_jt            = mu_jt,
     rho              = 0.52,
     sigma            = 0.24,
     longitude        = longitude,
     latitude         = latitude,
     mobility_omega   = mobility_omega,
     mobility_gamma   = mobility_gamma,
     tau_i            = tau_i,
     beta_j0_hum      = baseline_beta,
     a_1_j            = a_1_j,
     a_2_j            = a_2_j,
     b_1_j            = b_1_j,
     b_2_j            = b_2_j,
     p                = p,
     alpha_1          = 0.95,
     alpha_2          = 0.95,
     beta_j0_env      = baseline_beta * 0.5,
     theta_j          = theta_j,
     psi_jt           = psi_jt,
     zeta_1           = 7.5,
     zeta_2           = 2.5,
     kappa            = 1e5,
     decay_days_short = 3,
     decay_days_long  = 90,
     decay_shape_1    = 1,
     decay_shape_2    = 1,
     reported_cases   = mat_cases,
     reported_deaths  = mat_deaths
)

sim_config <- do.call(MOSAIC::make_LASER_config, sim_args)

# --------------------------- 9. Write to disk ----------------------------- #

out_dir <- file.path(getwd(), "inst", "extdata")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

file_paths <- c(
     file.path(out_dir, "simulated_parameters.json"),
     file.path(out_dir, "simulated_parameters.json.gz")
)

for (fp in file_paths) {
     args2 <- sim_config
     args2$output_file_path <- fp
     do.call(MOSAIC::make_LASER_config, args2)
}

message("Toy LASER config written to:\n",
        paste("  •", normalizePath(file_paths), collapse = "\n"))

# --------------------------- 10. Sanity check ----------------------------- #

identical(sim_config, jsonlite::fromJSON(file_paths[[1]]))


mpm <- reticulate::import("laser_cholera.metapop.model")
sim <- mpm$run_model(paramfile = sim_config)



exp_cases  <- sim$patches$expected_cases
exp_deaths <- sim$patches$disease_deaths


# Convert to matrices ------------------------------------------------------ #

exp_cases_mat  <- if (is.matrix(exp_cases)) exp_cases else as.matrix(exp_cases)
exp_deaths_mat <- if (is.matrix(exp_deaths)) exp_deaths else as.matrix(exp_deaths)

# Ensure orientation is [n_loc, time] ------------------------------------- #

if (nrow(exp_cases_mat) != n_loc) exp_cases_mat  <- t(exp_cases_mat[-1,])
if (nrow(exp_deaths_mat) != n_loc) exp_deaths_mat <- t(exp_deaths_mat[-1,])

# Basic base‑R visualisation ---------------------------------------------- #

op <- par(no.readonly = TRUE)
on.exit(par(op), add = TRUE)

par(mfrow = c(2, 1), mar = c(4, 4, 3, 2) + 0.1)

matplot(t, t(exp_cases_mat), type = "l", lty = 1, lwd = 2,
        xlab = "Date", ylab = "Expected cases",
        main = "Expected cholera cases")
legend("topright", legend = j, col = seq_len(n_loc), lty = 1, lwd = 2, bty = "n")

matplot(t, t(exp_deaths_mat), type = "l", lty = 1, lwd = 2,
        xlab = "Date", ylab = "Expected deaths",
        main = "Expected cholera deaths")
legend("topright", legend = j, col = seq_len(n_loc), lty = 1, lwd = 2, bty = "n")



config_simulation_epidemic <- sim_config
usethis::use_data(config_simulation_epidemic, overwrite = TRUE)
