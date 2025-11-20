# ---------------------------------------------------------------------------
# make_simulated_LASER_config_files.R  (sim_endemic)
# ---------------------------------------------------------------------------
# Generate an *endemic* LASER parameter configuration for unit‑testing and
# extended‑horizon demonstrations.
#
# ▸ Goal: produce low‑level persistent transmission with periodic (seasonal +
#   multi‑year) outbreaks driven by environmental persistence. Vibrios should
#   survive long enough in water to re‑seed infection after vaccine‑ and
#   infection‑derived immunity wanes.
# ▸ Key levers:
#     • 15‑year time span (2020‑01‑01 → 2034‑12‑31).
#     • Environmental suitability `psi_jt` fluctuates annually **and** on a
#       4‑year cycle.
#     • Longer environmental half‑life (`decay_days_long = 365`).
#     • Moderate immunity waning (ω₁, ω₂ ≈ 1 / 180 days ≈ 0.0056).
#     • Immunity coverage at t0: 30 % vaccinated, 20 % recovered, 2 % exposed /
#       infected, 48 % susceptible.
#
# Output
#   inst/extdata/sim_endemic_parameters.json
#   inst/extdata/sim_endemic_parameters.json.gz
#
# Usage
#   source("make_simulated_LASER_config_files.R")
# ---------------------------------------------------------------------------

library(MOSAIC)
library(jsonlite)
library(reticulate)


seed <- as.integer(999999999)
set.seed(seed)


# --------------------------- 1. Time & locations --------------------------- #

date_start <- as.Date("2020-01-01")
date_stop  <- as.Date("2024-12-31")         # 15 years

t          <- seq.Date(date_start, date_stop, by = "day")
T_len      <- length(t)

# Three placeholder ISO‑alpha‑3 codes – only the **count** matters.

j      <- c("FOO", "BAR", "BAZ")
n_loc  <- length(j)
N_j    <- c( 8000, 12000, 20000)             # heterogeneous populations
names(N_j) <- j

# --------------------------- 2. Initial states ---------------------------- #

S_j_prop <- 0.1
V1_j_prop <- 0.30
E_j_prop  <- 0
I_j_prop  <- 0.00125        # seed infections
R_j_prop  <- 1 - (S_j_prop + V1_j_prop + E_j_prop + I_j_prop)

S_j <- as.integer(N_j * S_j_prop)
V1_j <- as.integer(N_j * V1_j_prop)
V2_j <- rep(0L, n_loc)
E_j  <- pmax(1L, as.integer(N_j * E_j_prop))
I_j  <- pmax(1L, as.integer(N_j * I_j_prop))
R_j  <- pmax(0L, N_j - S_j - V1_j - V2_j - E_j - I_j)

# --------------------------- 3. Demography -------------------------------- #

b_rate <- 0.00005        # births ≈ 1.8 % yr‑¹
d_rate <- 0.00004        # deaths ≈ 1.5 % yr‑¹

b_jt <- matrix(b_rate,  n_loc, T_len, dimnames = list(j, t))
d_jt <- matrix(d_rate, n_loc, T_len, dimnames = list(j, t))
mu_jt <- matrix(0.01,  n_loc, T_len, dimnames = list(j, t))   # IFR

nu_1_jt <- nu_2_jt <- matrix(0, n_loc, T_len, dimnames = list(j, t))

# --------------------------- 4. Transmission (human‑to‑human) ------------- #

baseline_beta <- runif(n_loc, 0.25, 0.32)     # slightly lower than sim1
baseline_beta <- c(0.3, 0.5, 0.4)*0.6
amp_beta      <- 0.2                         # annual amplitude
phase_shift   <- runif(n_loc, 0, 2 * pi)

p <- 365                                       # annual Fourier period (days)
# First‑harmonic Fourier coefficients
a_1_j <-  amp_beta * cos(phase_shift)
b_1_j <-  amp_beta * sin(phase_shift)
# No 90‑/180‑day harmonics, but keep placeholders for completeness
a_2_j <- b_2_j <- rep(0, n_loc)

# --------------------------- 5. Mobility ----------------------------------- #

longitude <- c(-1.0232, 45.9062, 27.8493); names(longitude) <- j
latitude  <- c( 7.9465, -0.0236, -13.1339); names(latitude)  <- j

mobility_omega <- 2e-6
mobility_gamma <- 1.7
tau_i          <- setNames(c(0.005, 0.002, 0.006), j)   # daily depart probability

# --------------------------- 6. Environment & WASH ------------------------ #
# ψ_{jt} combines an annual sine and a longer 4‑year sine. Values are capped
# to [0, 1].

psi_jt <- matrix(NA_real_, n_loc, T_len, dimnames = list(j, t))
for (idx in seq_len(n_loc)) {
     annual     <- 0.25 * sin(2 * pi * seq_len(T_len) / 365 + phase_shift[idx])
     quad_year  <- 0.15 * sin(2 * pi * seq_len(T_len) / (365 * 4) + phase_shift[idx]/2)
     psi_raw    <- 0.45 + annual + quad_year       # baseline 0.55
     psi_jt[idx, ] <- pmax(0, pmin(1, psi_raw))
}

theta_j <- setNames(rep(0.35, n_loc), j)       # contamination fraction

# --------------------------- 7. Observed data placeholders ---------------- #

mat_cases  <- matrix(NA, n_loc, T_len, dimnames = list(j, t))
mat_deaths <- matrix(NA, n_loc, T_len, dimnames = list(j, t))

# --------------------------- 8. Wrap into config -------------------------- #

sim_args <- list(
     output_file_path = NULL,
     seed             = seed,      # distinct RNG seed
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
     omega_1          = 0.0056,    # ≈ 1 / 180 days (moderate waning)
     omega_2          = 0.0033,    # ≈ 1 / 300 days
     iota             = 1 / 1.4,
     gamma_1          = 0.20,
     gamma_2          = 0.10,
     epsilon          = 0.0005,
     mu_jt            = mu_jt,
     chi_endemic      = 0.5,       # PPV during endemic periods
     chi_epidemic     = 0.75,      # PPV during epidemic periods
     epidemic_threshold = 0.0001,  # incidence threshold for epidemic definition
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
     alpha_1          = 0.90,      # slightly lower protection → faster loss
     alpha_2          = 0.90,
     beta_j0_env      = baseline_beta * 0.5,  # stronger env. contribution
     theta_j          = theta_j,
     psi_jt           = psi_jt,
     psi_star_a       = rep(1, n_loc),    # Identity gain (no calibration)
     psi_star_b       = rep(0, n_loc),    # No offset
     psi_star_z       = rep(1, n_loc),    # No smoothing (use raw psi_jt)
     psi_star_k       = rep(0, n_loc),    # No time lag
     zeta_1           = 7.5,
     zeta_2           = 2.5,
     kappa            = 1e5,
     decay_days_short = 1,         # short burst
     decay_days_long  = 90,       # vibrios can persist ≈ 1 yr in water
     decay_shape_1    = 1,
     decay_shape_2    = 1,         # Linear/concave response to suitability
     reported_cases   = mat_cases,
     reported_deaths  = mat_deaths
)

sim_config <- do.call(MOSAIC::make_LASER_config, sim_args)

# --------------------------- 9. Write to disk ----------------------------- #

out_dir <- file.path(getwd(), "inst", "extdata")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

file_paths <- c(
     file.path(out_dir, "sim_endemic_parameters.json"),
     file.path(out_dir, "sim_endemic_parameters.json.gz")
)

for (fp in file_paths) {
     args2 <- sim_config
     args2$output_file_path <- fp
     do.call(MOSAIC::make_LASER_config, args2)
}

message("Endemic LASER config written to:\n",
        paste("  •", normalizePath(file_paths), collapse = "\n"))


# --------------------------- 11. Run simulation --------------------------- #

mpm <- reticulate::import("laser_cholera.metapop.model")
sim <- mpm$run_model(paramfile = sim_config)




exp_cases  <- sim$patches$expected_cases
exp_deaths <- sim$patches$disease_deaths

sim_config$reported_cases <- t(exp_cases[-1,])
sim_config$reported_deaths <- t(exp_deaths[-1,])

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

config_simulation_endemic <- sim_config
usethis::use_data(config_simulation_endemic, overwrite = TRUE)
