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

# ---- temporal ramp: exponential recency weighting on the NB core
# RAMP_HL = half-life in DAYS. w_t = 0.5 ^ (days_before_cut / HL), capped at 1
# from the cut onward. run_MOSAIC normalises to mean 1, so the LL scale is
# preserved. NOTE (run_MOSAIC.R:327): the shape terms do NOT honour
# weights_time -- they scan the raw series -- so the ramp acts on the NB core only.
ramp_hl <- suppressWarnings(as.numeric(Sys.getenv("RAMP_HL", "")))
if (!is.na(ramp_hl) && ramp_hl > 0) {
     dts <- seq(as.Date(config$date_start), by = "day",
                length.out = ncol(config$reported_cases))
     ref <- if (nzchar(TCUT)) as.Date(TCUT) else max(dts)
     age <- pmax(0, as.numeric(ref - dts))          # days BEFORE the cut
     wt  <- 0.5 ^ (age / ramp_hl)
     ctrl$likelihood$weights_time <- wt
     cat(sprintf("[inflab] temporal ramp: half-life %.0f d | weight at cut %.3f, 1y before %.3f, 5y before %.4f\n",
                 ramp_hl, 1, 0.5^(365/ramp_hl), 0.5^(1825/ramp_hl)))
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
