# =============================================================================
# simulate_outbreak_settings.R
# -----------------------------------------------------------------------------
# Forward-simulate cholera transmission under five contrasting epidemiological
# settings, save per-setting time series, and render comparison figures.
#
# Purpose
#   A teaching / scenario-generation script for studies that do NOT calibrate to
#   data but instead need representative transmission dynamics for different
#   outbreak settings -- e.g. evaluating rapid diagnostic tests (RDTs) and
#   outbreak-detection definitions across endemic vs. sporadic vs. rare regimes.
#
# The five settings
#   1. epidemic   Single explosive outbreak then fade-out (1 year).
#   2. endemic    Sustained low-level transmission with seasonal waves (5 years).
#   3. recurring  Distinct epidemic wave every year, troughs between (5 years).
#   4. sporadic   Mostly quiet; several small, self-limiting clusters (5 years).
#   5. rare       Long quiet period punctuated by ONE large outbreak (8 years).
#
#   "epidemic" and "endemic" use the validated configs that ship with MOSAIC
#   (config_simulation_epidemic / config_simulation_endemic). "recurring",
#   "sporadic" and "rare" are built here with build_regime_config() to show how
#   to construct custom settings by tuning a handful of transmission levers.
#
# How the settings are produced (the levers)
#   * beta_hum / beta_env   human-to-human and environment-to-human transmission.
#                           Keep the base subcritical (R0 < 1) for sporadic/rare
#                           so nothing self-ignites between triggers.
#   * amp_beta              seasonal forcing amplitude (annual sinusoid on beta).
#   * psi_jt                environmental suitability over time. A transient
#                           "bump" (e.g. a flood) is what ignites sporadic
#                           clusters and the single rare flare.
#   * decay_days_long       environmental reservoir persistence. Longer = embers
#                           survive longer, so a late trigger can still ignite.
#   * epsilon               immunity waning rate (R -> S). Faster waning
#                           replenishes susceptibles and sustains recurring waves.
#   * omega_1 / omega_2     vaccine-derived immunity waning.
#   * S_prop / V1_prop      initial susceptible / one-dose-vaccinated fractions.
#
# Usage (from anywhere, with MOSAIC installed and its Python env available)
#   Rscript -e 'source(system.file("examples","simulate_outbreak_settings.R",package="MOSAIC"))'
#   # or open in RStudio and source it. Set out_root below to change where
#   # outputs are written (default: ~/MOSAIC/output/simulation_settings).
#
# Output (organised directory)
#   <out_root>/
#     ├── figures/   one PNG per setting + all_settings_overview.png
#     ├── data/      one tidy CSV per setting (date, location, cases, deaths)
#     └── summary.csv
# =============================================================================

suppressMessages(library(MOSAIC))

# -----------------------------------------------------------------------------
# 0. Output directory + global settings
# -----------------------------------------------------------------------------

out_root <- path.expand("~/MOSAIC/output/simulation_settings")
fig_dir  <- file.path(out_root, "figures")
data_dir <- file.path(out_root, "data")
for (d in c(out_root, fig_dir, data_dir)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

SEED <- 123L   # stochastic LASER draw; fixed for reproducibility

# Three toy patches shared by all constructed settings (populations differ).
J     <- c("FOO", "BAR", "BAZ")
N_J   <- c(8000, 12000, 20000); names(N_J) <- J
N_LOC <- length(J)
PHASE <- c(0, 2.094, 4.189)   # deterministic per-patch seasonal phase offsets

# -----------------------------------------------------------------------------
# 1. Helpers to shape environmental suitability psi_jt over time
# -----------------------------------------------------------------------------

# Flat suitability.
psi_flat <- function(value) function(idx, tt) rep(value, length(tt))

# Seasonal suitability: annual sinusoid + optional multi-year (quadrennial) one.
psi_seasonal <- function(base, annual_amp, quad_amp = 0) function(idx, tt)
  base +
  annual_amp * sin(2 * pi * tt / 365 + PHASE[idx]) +
  quad_amp   * sin(2 * pi * tt / (365 * 4) + PHASE[idx] / 2)

# One Gaussian "bump" (a transient suitability spike, e.g. a flood).
psi_bump <- function(base, height, peak_day, width) function(idx, tt)
  base + height * exp(-0.5 * ((tt - peak_day) / width)^2)

# Several Gaussian bumps at scattered times.
psi_multibump <- function(base, height, peak_days, width) function(idx, tt) {
  v <- rep(base, length(tt))
  for (pd in peak_days) v <- v + height * exp(-0.5 * ((tt - pd) / width)^2)
  v
}

# Day index (1-based) of a calendar date relative to a start date.
day_index <- function(start, when) as.integer(as.Date(when) - as.Date(start)) + 1L

# -----------------------------------------------------------------------------
# 2. Build a LASER config for a custom transmission setting
# -----------------------------------------------------------------------------
# Returns a validated config list (via make_LASER_config) ready for run_LASER().
# S_prop / V1_prop may be scalar (shared) or length-3 (per patch).

build_regime_config <- function(date_start, date_stop,
                                 beta_hum, beta_env, amp_beta,
                                 psi_fun, decay_days_long,
                                 omega_1, omega_2, epsilon,
                                 S_prop, V1_prop, I_seed_counts,
                                 seed = 999999999L) {
  set.seed(seed)
  t     <- seq.Date(as.Date(date_start), as.Date(date_stop), by = "day")
  T_len <- length(t)

  # Initial compartment counts
  S_j  <- as.integer(N_J * S_prop)
  V1_j <- as.integer(N_J * V1_prop)
  V2_j <- rep(0L, N_LOC)
  E_j  <- rep(0L, N_LOC)
  I_j  <- as.integer(I_seed_counts)
  R_j  <- as.integer(N_J - S_j - V1_j - V2_j - E_j - I_j)
  if (any(R_j < 0)) stop("Initial S + V1 + I exceed N for at least one patch; lower S_prop/V1_prop.")

  # Demography / IFR (held constant across settings)
  b_jt    <- matrix(0.00005, N_LOC, T_len, dimnames = list(J, t))
  d_jt    <- matrix(0.00004, N_LOC, T_len, dimnames = list(J, t))
  mu_jt   <- matrix(0.01,    N_LOC, T_len, dimnames = list(J, t))
  nu_1_jt <- nu_2_jt <- matrix(0, N_LOC, T_len, dimnames = list(J, t))

  # Transmission: split total beta into human + environmental shares so the
  # make_LASER_config tolerance check (beta_j0_hum == p_beta * beta_j0_tot) holds.
  beta_tot <- beta_hum + beta_env
  p_beta   <- beta_hum / beta_tot
  a_1_j    <- amp_beta * cos(PHASE)
  b_1_j    <- amp_beta * sin(PHASE)

  # Environmental suitability matrix, clipped to [0, 1]
  psi_jt <- matrix(NA_real_, N_LOC, T_len, dimnames = list(J, t))
  for (i in seq_len(N_LOC)) psi_jt[i, ] <- pmax(0, pmin(1, psi_fun(i, seq_len(T_len))))

  mat_na <- matrix(NA, N_LOC, T_len, dimnames = list(J, t))

  args <- list(
    output_file_path = NULL, seed = as.integer(seed),
    date_start = as.Date(date_start), date_stop = as.Date(date_stop),
    location_name = J, N_j_initial = N_J,
    S_j_initial = S_j, E_j_initial = E_j, I_j_initial = I_j, R_j_initial = R_j,
    V1_j_initial = V1_j, V2_j_initial = V2_j,
    prop_S_initial = setNames(S_j / N_J, J), prop_E_initial = setNames(E_j / N_J, J),
    prop_I_initial = setNames(I_j / N_J, J), prop_R_initial = setNames(R_j / N_J, J),
    prop_V1_initial = setNames(V1_j / N_J, J), prop_V2_initial = setNames(V2_j / N_J, J),
    b_jt = b_jt, d_jt = d_jt, nu_1_jt = nu_1_jt, nu_2_jt = nu_2_jt,
    phi_1 = 0.64, phi_2 = 0.85, omega_1 = omega_1, omega_2 = omega_2,
    nu_jt_sources = c("S", "E", "Isym", "Iasym", "R"), iota = 1 / 1.4,
    gamma_1 = 0.2, gamma_2 = 0.1, epsilon = epsilon, mu_jt = mu_jt,
    mu_j_baseline = setNames(rep(0.01, N_LOC), J),
    mu_j_slope = setNames(rep(0, N_LOC), J),
    mu_j_epidemic_factor = setNames(rep(0, N_LOC), J),
    chi_endemic = 0.5, chi_epidemic = 0.75, epidemic_threshold = 0.0001,
    rho = 0.52, rho_deaths = 0.42, sigma = 0.24,
    delta_reporting_cases = 0, delta_reporting_deaths = 5,
    longitude = setNames(c(-1.0232, 45.9062, 27.8493), J),
    latitude  = setNames(c(7.9465, -0.0236, -13.1339), J),
    mobility_omega = 2e-6, mobility_gamma = 1.7,
    tau_i = setNames(c(0.005, 0.002, 0.006), J),
    beta_j0_tot = beta_tot, p_beta = p_beta, beta_j0_hum = beta_hum,
    a_1_j = a_1_j, a_2_j = rep(0, N_LOC), b_1_j = b_1_j, b_2_j = rep(0, N_LOC), p = 365,
    alpha_1 = 0.9, alpha_2 = 0.9, beta_j0_env = beta_env,
    theta_j = setNames(rep(0.35, N_LOC), J), psi_jt = psi_jt,
    psi_star_a = rep(1, N_LOC), psi_star_b = rep(0, N_LOC),
    psi_star_z = rep(1, N_LOC), psi_star_k = rep(0, N_LOC),
    zeta_1 = 7.5, zeta_2 = 2.5, kappa = 1e5,
    decay_days_short = 1, decay_days_long = decay_days_long,
    decay_shape_1 = 1, decay_shape_2 = 1,
    reported_cases = mat_na, reported_deaths = mat_na
  )
  do.call(MOSAIC::make_LASER_config, args)
}

# -----------------------------------------------------------------------------
# 3. Define the five settings
# -----------------------------------------------------------------------------

settings <- list(

  epidemic = list(
    label = "Epidemic",
    description = "Single explosive outbreak then fade-out (1 year).",
    config = MOSAIC::config_simulation_epidemic
  ),

  endemic = list(
    label = "Endemic",
    description = "Sustained low-level transmission with seasonal waves (5 years).",
    config = MOSAIC::config_simulation_endemic
  ),

  recurring = list(
    label = "Recurring",
    description = "Distinct epidemic wave every year (5 years).",
    config = build_regime_config(
      "2020-01-01", "2024-12-31",
      beta_hum = c(0.14, 0.20, 0.17), beta_env = c(0.05, 0.07, 0.06), amp_beta = 0.95,
      psi_fun = psi_seasonal(base = 0.45, annual_amp = 0.30), decay_days_long = 90,
      omega_1 = 0.0110, omega_2 = 0.0066, epsilon = 0.0045,   # fast waning -> sustained waves
      S_prop = 0.40, V1_prop = 0.05, I_seed_counts = c(5, 5, 5))
  ),

  sporadic = list(
    label = "Sporadic",
    description = "Mostly quiet with several small, self-limiting clusters (5 years).",
    config = build_regime_config(
      "2020-01-01", "2024-12-31",
      beta_hum = c(0.06, 0.09, 0.075), beta_env = c(0.05, 0.06, 0.055), amp_beta = 0.30,
      psi_fun = psi_multibump(base = 0.08, height = 0.25, width = 10,
        peak_days = day_index("2020-01-01",
          c("2020-05-01", "2021-03-01", "2022-07-01", "2023-05-01", "2024-09-01"))),
      decay_days_long = 120,
      omega_1 = 0.0056, omega_2 = 0.0033, epsilon = 0.0006,
      S_prop = 0.45, V1_prop = 0.10, I_seed_counts = c(2, 2, 2))
  ),

  rare = list(
    label = "Rare outbreak",
    description = "Long quiet period punctuated by one large outbreak (8 years).",
    config = build_regime_config(
      "2018-01-01", "2025-12-31",
      beta_hum = c(0.042, 0.063, 0.0525), beta_env = c(0.0245, 0.0315, 0.028), amp_beta = 0.25,
      psi_fun = psi_bump(base = 0.06, height = 0.92,
        peak_day = day_index("2018-01-01", "2023-06-01"), width = 55),
      decay_days_long = 365,   # persistent reservoir keeps embers alive until the trigger
      omega_1 = 0.0056, omega_2 = 0.0033, epsilon = 0.0008,
      S_prop = 0.55, V1_prop = 0.05, I_seed_counts = c(2, 2, 2))
  )
)

# -----------------------------------------------------------------------------
# 4. Run each setting, save tidy CSV + per-setting figure
# -----------------------------------------------------------------------------

palette3   <- c("#009ADE", "#FF1F5B", "#00CD6C")   # one colour per patch
aggregate_cases <- list()                          # for the overview figure
summary_rows    <- list()

for (key in names(settings)) {
  s   <- settings[[key]]
  cfg <- s$config
  message(sprintf("[%s] running LASER (%s) ...", s$label,
                  paste(cfg$date_start, "->", cfg$date_stop)))

  model  <- run_LASER(config = cfg, seed = SEED, quiet = TRUE)
  cases  <- as.matrix(reticulate::py_to_r(model$results$reported_cases))   # [patch x time]
  deaths <- as.matrix(reticulate::py_to_r(model$results$reported_deaths))
  dates  <- seq.Date(as.Date(cfg$date_start), as.Date(cfg$date_stop), by = "day")
  locs   <- cfg$location_name

  # --- tidy CSV: date, location, cases, deaths ---
  tidy <- data.frame(
    date     = rep(dates, times = nrow(cases)),
    location = rep(locs,  each  = ncol(cases)),
    cases    = as.vector(t(cases)),
    deaths   = as.vector(t(deaths))
  )
  write.csv(tidy, file.path(data_dir, paste0(key, ".csv")), row.names = FALSE)

  # --- per-setting figure: cases (top) + deaths (bottom), by patch ---
  png(file.path(fig_dir, paste0(key, ".png")), width = 1000, height = 700, res = 120)
  par(mfrow = c(2, 1), mar = c(3, 4.5, 3, 1), oma = c(2, 0, 2, 0))
  matplot(dates, t(cases), type = "l", lty = 1, lwd = 2, col = palette3,
          xlab = "", ylab = "Reported cases",
          main = sprintf("%s\n%s", s$label, s$description))
  matplot(dates, t(deaths), type = "l", lty = 1, lwd = 2, col = palette3,
          xlab = "Date", ylab = "Reported deaths", main = "")
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", legend = locs, col = palette3, lty = 1, lwd = 2,
         horiz = TRUE, bty = "n", xpd = TRUE)
  dev.off()

  aggregate_cases[[key]] <- list(dates = dates, total = colSums(cases, na.rm = TRUE), label = s$label)
  summary_rows[[key]] <- data.frame(
    setting     = s$label,
    start       = as.character(cfg$date_start),
    stop        = as.character(cfg$date_stop),
    days        = length(dates),
    total_cases = round(sum(cases,  na.rm = TRUE)),
    total_deaths= round(sum(deaths, na.rm = TRUE)),
    peak_daily_cases = round(max(colSums(cases, na.rm = TRUE)))
  )
  message(sprintf("    total cases = %s | peak/day = %s | saved %s.csv, %s.png",
                  format(round(sum(cases, na.rm = TRUE)), big.mark = ","),
                  format(round(max(colSums(cases, na.rm = TRUE))), big.mark = ","),
                  key, key))
}

# -----------------------------------------------------------------------------
# 5. Overview figure: aggregate daily cases, one panel per setting
# -----------------------------------------------------------------------------

png(file.path(fig_dir, "all_settings_overview.png"), width = 1100, height = 1500, res = 130)
par(mfrow = c(length(settings), 1), mar = c(2.5, 4.5, 2.5, 1), oma = c(2, 0, 2, 0))
for (key in names(settings)) {
  a <- aggregate_cases[[key]]
  plot(a$dates, a$total, type = "l", lwd = 2, col = "#0054A6",
       xlab = "", ylab = "Cases / day", main = a$label)
  grid(col = "gray90")
}
mtext("Cholera transmission under five outbreak settings (total reported cases/day across patches)",
      side = 3, outer = TRUE, font = 2, cex = 0.9)
mtext("Date", side = 1, outer = TRUE, line = 0.5)
dev.off()

# -----------------------------------------------------------------------------
# 6. Summary table
# -----------------------------------------------------------------------------

summary_df <- do.call(rbind, summary_rows)
rownames(summary_df) <- NULL
write.csv(summary_df, file.path(out_root, "summary.csv"), row.names = FALSE)

message("\n================ SUMMARY ================")
print(summary_df, row.names = FALSE)
message(sprintf("\nAll outputs written under:\n  %s", out_root))
message("  figures/  (one PNG per setting + all_settings_overview.png)")
message("  data/     (one tidy CSV per setting)")
message("  summary.csv")
