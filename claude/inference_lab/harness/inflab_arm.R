#!/usr/bin/env Rscript
# Inference-lab arm runner. Namespaced (inflab_*) to avoid colliding with the
# psi agent's run_arm.R / output paths on the same host.
#
# env: ARM_ID, N_SIMS, SEED, CORES, T_CUT, ISO, PATCH (optional R file)
.libPaths(c("~/R/library", .libPaths()))
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
cat(sprintf("[inflab] MOSAIC %s\n", as.character(packageVersion("MOSAIC"))))

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

# ---- arm-specific patch: may modify `config`, `priors`, `ctrl`, or the package
if (nzchar(PATCH) && file.exists(PATCH)) {
     cat(sprintf("[inflab] applying patch: %s\n", PATCH))
     source(PATCH, local = FALSE)
}

t0 <- Sys.time()
run_MOSAIC(config = config, priors = priors, dir_output = OUT, control = ctrl)
cat(sprintf("[inflab] DONE arm=%s seed=%d wall_min=%.2f\n", ARM, SEED,
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))
