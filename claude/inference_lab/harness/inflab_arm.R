#!/usr/bin/env Rscript
# Inference-lab arm runner. Namespaced (inflab_*) to avoid colliding with the
# psi agent's run_arm.R / output paths on the same host.
#
# env: ARM_ID, N_SIMS, SEED, CORES, T_CUT, ISO, PATCH (optional R file)
LIB <- Sys.getenv("LIB", "~/R/library")
.libPaths(c(LIB, "~/R/library", .libPaths()))
suppressMessages(library(MOSAIC))

ARM   <- Sys.getenv("ARM_ID", "baseline")
N     <- as.integer(Sys.getenv("N_SIMS", "10000"))
SEED  <- as.integer(Sys.getenv("SEED", "1"))
CORES <- as.integer(Sys.getenv("CORES", "40"))
TCUT  <- Sys.getenv("T_CUT", "2025-09-01")
ISO   <- Sys.getenv("ISO", "ETH")
PATCH <- Sys.getenv("PATCH", "")
OUT   <- file.path("~/inflab", sprintf("%s_%s_n%d_s%d", ARM, ISO, N, SEED))

cat(sprintf("[inflab] arm=%s iso=%s n=%d seed=%d cores=%d t_cut=%s\n",
            ARM, ISO, N, SEED, CORES, TCUT))
cat(sprintf("[inflab] MOSAIC %s from %s\n", as.character(packageVersion("MOSAIC")), LIB))

set_root_directory("~/MOSAIC")
config <- get_location_config(iso = ISO)
priors <- get_location_priors(iso = ISO)

# ---- hold out the tail from the LIKELIHOOD only; the simulation still runs it
if (nzchar(TCUT)) {
     d   <- seq(as.Date(config$date_start), by = "day",
                length.out = ncol(config$reported_cases))
     oos <- which(d > as.Date(TCUT))
     stopifnot(length(oos) > 0)
     config$reported_cases_weight[, oos]  <- 0
     config$reported_deaths_weight[, oos] <- 0
     cat(sprintf("[inflab] holdout: %d of %d days masked from the likelihood (> %s)\n",
                 length(oos), length(d), TCUT))
}

ctrl <- mosaic_control_defaults(
     calibration = list(n_simulations = N, n_iterations = 3L),
     parallel    = list(enable = TRUE, n_cores = CORES),
     paths       = list(plots = TRUE, clean_output = FALSE)
)
# ETH is single-location: the three spatial/mobility params are inert here.
ctrl$sampling$sample_tau_i          <- FALSE
ctrl$sampling$sample_mobility_gamma <- FALSE
ctrl$sampling$sample_mobility_omega <- FALSE
ctrl$sampling$sample_kappa          <- FALSE   # fixed at 1e6 by decision

# ---- likelihood settings from the environment (control-level; no reinstall)
lk_env <- c(nb_k_min_cases="NB_K_CASES", nb_k_min_deaths="NB_K_DEATHS",
            weight_cases="W_CASES", weight_deaths="W_DEATHS",
            weight_peak_timing="W_PKT", weight_peak_magnitude="W_PKM",
            weight_cumulative_total="W_CUM", weight_wis="W_WIS")
for (nm in names(lk_env)) {
     v <- Sys.getenv(lk_env[[nm]], "")
     if (nzchar(v)) {
          ctrl$likelihood[[nm]] <- as.numeric(v)
          cat(sprintf("[inflab] likelihood$%s = %s\n", nm, v))
     }
}

# ---- arm-specific patch: may modify `config`, `priors`, `ctrl`, or the package
if (nzchar(PATCH) && file.exists(PATCH)) {
     cat(sprintf("[inflab] applying patch: %s\n", PATCH))
     source(PATCH, local = FALSE)
}

t0 <- Sys.time()
run_MOSAIC(config = config, priors = priors, dir_output = OUT, control = ctrl)
cat(sprintf("[inflab] DONE arm=%s seed=%d wall_min=%.2f\n", ARM, SEED,
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))
