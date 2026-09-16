# =============================================================================
# make_fixtures.R -- one-time generator for the frozen benchmark workloads.
#
# Run ONCE, commit the outputs, never run again casually. The whole point of
# these files is that they do NOT track the package: `config_default` is
# rebuilt from live surveillance data, and its window has already moved
# 1278 -> 1367 -> 1398 ticks across releases -- a +9.4% change in work volume
# with no engine change, which is indistinguishable from a real regression in
# a wall-clock ledger.
#
#   Rscript inst/bench/make_fixtures.R <lib.loc>
# =============================================================================
args <- commandArgs(TRUE)
lib  <- if (length(args)) args[1] else .libPaths()[1]
suppressMessages(library(MOSAIC, lib.loc = lib))
`%||%` <- function(a, b) if (is.null(a)) b else a
out <- Sys.getenv("BENCH_FIXTURE_DIR", unset = "inst/bench/fixtures")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

cfg_path <- system.file("extdata", "config_default.json", package = "MOSAIC", lib.loc = lib)
cfg <- jsonlite::fromJSON(cfg_path, simplifyVector = TRUE)
J <- length(cfg$location_name)
T0 <- as.integer(as.Date(cfg$date_stop) - as.Date(cfg$date_start)) + 1L
cat(sprintf("source config: J=%d T=%d md5=%s\n", J, T0, unname(tools::md5sum(cfg_path))))

# ---- helpers ---------------------------------------------------------------
# Truncate every time-varying field to the first `n` ticks. Generic on ncol so
# a field added later is handled rather than silently left at full width --
# sim_params() validates EVERY matrix against nticks, so a missed field is a
# hard error, not a silent wrong answer. epidemic_peaks is filtered to the new
# window and DROPPED if empty: an empty list crashes the Python engine at
# params.py:584 (dict_to_propertysetex builds a column-less frame then reads
# .iso_code), and the R engine handles a missing key correctly.
trunc_T <- function(cfg, n) {
     stopifnot(n <= T0)
     for (nm in names(cfg)) {
          x <- cfg[[nm]]
          if (is.matrix(x) && ncol(x) == T0) cfg[[nm]] <- x[, seq_len(n), drop = FALSE]
     }
     cfg$date_stop <- as.character(as.Date(cfg$date_start) + n - 1L)
     if (!is.null(cfg$epidemic_peaks)) {
          ep <- cfg$epidemic_peaks
          keep <- as.Date(ep[[grep("date|peak", names(ep), value = TRUE)[1]]]) <= as.Date(cfg$date_stop)
          keep[is.na(keep)] <- FALSE
          ep <- ep[keep, , drop = FALSE]
          if (nrow(ep) == 0L) cfg$epidemic_peaks <- NULL else cfg$epidemic_peaks <- ep
     }
     cfg
}

save_fx <- function(obj, name) {
     p <- file.path(out, name); saveRDS(obj, p, compress = "xz")
     cat(sprintf("  %-26s %8.1f KB\n", name, file.size(p) / 1024)); invisible(p)
}

# ---- 1. single location (MOZ), full window ---------------------------------
moz <- MOSAIC::get_location_config(iso = "MOZ", config = cfg)
save_fx(moz, "config_moz.rds")

# ---- 2. short window, all locations ----------------------------------------
save_fx(trunc_T(cfg, 400L), "config_short.rds")

# ---- 3. high vaccination: light the second-dose block -----------------------
# config_default has nu_2_jt identically zero (0 of 55,920 cells), so
# `if (any(nu2 != 0))` in sim_phase_vaccinated() never fires and the entire
# phi_2 transit is cold in every default-config benchmark. Deriving doses from
# nu_1_jt keeps the schedule plausible and guarantees the branch executes
# wherever first doses are delivered.
hv <- trunc_T(cfg, 400L)
hv$nu_2_jt <- round(0.3 * hv$nu_1_jt)
cat(sprintf("  high-vacc nu_2_jt non-zero cells: %d of %d\n",
            sum(hv$nu_2_jt != 0), length(hv$nu_2_jt)))
save_fx(hv, "config_highvacc.rds")

# ---- 4. priors -------------------------------------------------------------
# Frozen for the same reason as the config: a priors change moves the
# draw-failure rate, and a failed draw is ~20x cheaper than a completed sim,
# so throughput shifts with the engine untouched.
save_fx(MOSAIC::priors_default, "priors.rds")

# ---- 5. machine calibrator --------------------------------------------------
# Version-independent by construction: pure rbinom/rpois over a frozen argument
# set, touching no MOSAIC code. Sized and shaped to match the engine's real
# draw mix (~82% binomial, np spanning the BINV/BTPE algorithm switch at 30).
set.seed(20260915)
nsite <- 2000L; npatch <- 40L
calib <- list(
     binom = lapply(seq_len(round(nsite * 0.82)), function(i)
          list(n = as.integer(round(runif(npatch, 5e2, 5e5))),
               p = 10^runif(npatch, -5, -1.2))),
     pois  = lapply(seq_len(round(nsite * 0.18)), function(i)
          10^runif(npatch, 0, 11))
)
save_fx(calib, "calibrator_args.rds")
cat("done\n")
