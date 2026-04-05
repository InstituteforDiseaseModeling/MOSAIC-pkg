# =============================================================================
# Calibration Test 43 — Refined Design B with inflate_priors() and
#                        mosaic_adaptive_s3_weights()
# =============================================================================
#
# DESIGN EVOLUTION
# ----------------
# test_42 established the 3-stage NB pipeline (Cases→Deaths→Joint) and showed:
#   - Design B (Narrow-then-Solve) more efficient than Design A (Solve-then-Mix)
#   - S2→S3 deaths gains preserved with deaths-heavy S3 weighting
#   - Remaining problem: deaths bias ratio still 2.7–4x after S3
#
# Expert review identified two improvements for test_43:
#
#   1. inflate_priors() — broaden S2 CFR posteriors before S3 (variance
#      inflation / kernel smoothing, Liu & West 2001). Prevents weight
#      collapse when S3 joint likelihood prefers slightly different
#      transmission trajectory than S2 assumed. Applied to 4 CFR params
#      only; transmission params stay tight (preserves S1 cases fit).
#
#   2. mosaic_adaptive_s3_weights() — data-driven w_cases/w_deaths for S3
#      based on how well each outcome was fit in S1/S2. Combines R²-
#      deficiency and bias-deficiency. Countries with poor deaths fit and
#      good cases fit automatically get more deaths weight.
#
# 3-STAGE PIPELINE
# ----------------
#   S1  Cases    Auto. ESS_param=100, prop=0.80, A=0.70, CVw=2.0.
#                All 43 params free. Default priors. NB only.
#                weight_cases=1.0, weight_deaths=0.1. max_batches=20.
#
#   S2  Deaths   Auto. ESS_param=100, prop=1.00 (all 4 must converge),
#                A=0.70, CVw=1.5. 4 CFR params free, S1 config frozen.
#                weight_cases=0.0, weight_deaths=1.0. max_batches=20.
#
#   Pre-S3:      inflate_priors(ALL params, f=1.5) — broadens both
#                transmission (S1) and CFR (S2) posteriors before S3 to
#                prevent IS weight collapse in the joint stage.
#                mosaic_adaptive_s3_weights() sets per-country w_cases/w_deaths.
#
#   S3  Joint    Auto. ESS_param=500, prop=0.95, A=0.90, CVw=1.0.
#                All 43 params free. Full convergence. max_batches=50.
#
# CONVERGENCE SETTINGS GROUNDED IN:
#   ESS targets:   Gelman et al. 2014 (BDA3 ESS ≥ 100-500)
#   Agreement (A): Elvira et al. 2022; MOSAIC docs target A > 0.7-0.8
#   CVw targets:   Kong et al. 1994 (CVw ≲ 2)
#   target_r2:     R² of ESS ~ sqrt(n) trajectory; default 0.90
#   Inflation:     Liu & West 2001 (kernel smoothing / variance tempering)
#
# LOCATIONS: NGA Nigeria | MOZ Mozambique | ETH Ethiopia | KEN Kenya | COD DRC
# All single-location models (mobility frozen).
# Default config: 2023-02-01 to 2026-03-31.
#
# =============================================================================

library(MOSAIC)
library(jsonlite)
library(arrow)

set_root_directory("/Users/johngiles/MOSAIC")

log_msg(paste(rep("=", 80), collapse = ""))
log_msg("Calibration Test 43: Design B + inflate_priors + adaptive_s3_weights")
log_msg(paste(rep("=", 80), collapse = ""))

base_dir <- "/Users/johngiles/MOSAIC/MOSAIC-pkg/local/calibration/calibration_test_43"

locations <- list(
  NGA = "Nigeria",
  MOZ = "Mozambique",
  ETH = "Ethiopia",
  KEN = "Kenya",
  COD = "DRC"
)

# Variance inflation factor applied to ALL priors before S3.
# f=1.5 inflates pre-truncation variance by 1.5x (mean preserved) for every
# fitted distribution — both transmission params (from S1) and CFR params
# (from S2). Prevents IS weight collapse in S3 when the joint optimum differs
# from the staged single-outcome optima (Liu & West 2001).
S3_INFLATION_FACTOR <- 2.0

# =============================================================================
# DESIGN B SETTINGS — revised from test_42 analysis
# =============================================================================
# Settings grounded in MOSAIC calibration documentation and literature:
#   ESS targets:   Gelman et al. 2014 (BDA3), Bürkner 2017
#   Agreement (A): Elvira et al. 2022; docs target A > 0.7-0.8
#   CVw targets:   Kong et al. 1994 (CVw ≲ 2 for balanced weights)
#   target_r2:     R² of ESS ~ sqrt(n) trajectory; default 0.90

design_B <- list(
  s1 = list(
    batch_size     = 500L,
    min_batches    = 5L,
    max_batches    = 20L,
    target_r2      = 0.90,
    ESS_param      = 200L,
    ESS_param_prop = 0.95,   # 95% of 43 params = ~41 must hit ESS_param
    ESS_best       = 200L,
    A_best         = 0.70,   # Agreement Index ≥ 0.70 (docs: target > 0.7-0.8)
    CVw_best       = 2.0     # Kong et al. 1994: CVw ≲ 2
  ),
  s2 = list(
    batch_size     = 500L,
    min_batches    = 5L,
    max_batches    = 20L,
    target_r2      = 0.90,
    ESS_param      = 200L,
    ESS_param_prop = 1.00,   # ALL 4 CFR params must converge
    ESS_best       = 200L,
    A_best         = 0.70,   # Agreement Index ≥ 0.70
    CVw_best       = 1.5     # Tighter than S1: 4 params, smoother posterior
  ),
  s3 = list(
    batch_size     = 500L,
    min_batches    = 5L,
    max_batches    = 50L,
    target_r2      = 0.90,
    ESS_param      = 500L,   # Full convergence: BDA3/Bürkner target ESS 500+
    ESS_param_prop = 0.95,   # 95% of 43 params = ~41 must hit ESS_param
    ESS_best       = 500L,
    A_best         = 0.90,   # High consensus for final production posterior
    CVw_best       = 1.0     # Tight weight balance for final stage
  )
)

# =============================================================================
# SHARED SETTINGS
# =============================================================================

n_cores <- 9L
n_iter  <- 2L

# S2: all params frozen except 4 CFR params
s2_deaths_freeze <- list(
  sample_initial_conditions      = FALSE,
  ic_moment_match                = FALSE,
  sample_tau_i                   = FALSE,
  sample_mobility_gamma          = FALSE,
  sample_mobility_omega          = FALSE,
  sample_kappa                   = FALSE,
  sample_alpha_1                 = FALSE,
  sample_alpha_2                 = FALSE,
  sample_epsilon                 = FALSE,
  sample_iota                    = FALSE,
  sample_gamma_1                 = FALSE,
  sample_gamma_2                 = FALSE,
  sample_phi_1                   = FALSE,
  sample_phi_2                   = FALSE,
  sample_omega_1                 = FALSE,
  sample_omega_2                 = FALSE,
  sample_rho                     = FALSE,
  sample_sigma                   = FALSE,
  sample_beta_j0_tot             = FALSE,
  sample_p_beta                  = FALSE,
  sample_theta_j                 = FALSE,
  sample_a_1_j                   = FALSE,
  sample_a_2_j                   = FALSE,
  sample_b_1_j                   = FALSE,
  sample_b_2_j                   = FALSE,
  sample_psi_star_a              = FALSE,
  sample_psi_star_b              = FALSE,
  sample_psi_star_z              = FALSE,
  sample_psi_star_k              = FALSE,
  sample_decay_days_short        = FALSE,
  sample_decay_days_long         = FALSE,
  sample_decay_shape_1           = FALSE,
  sample_decay_shape_2           = FALSE,
  sample_zeta_1                  = FALSE,
  sample_zeta_2                  = FALSE,
  sample_mu_j_baseline           = TRUE,
  sample_mu_j_slope              = TRUE,
  sample_mu_j_epidemic_factor    = TRUE,
  sample_epidemic_threshold      = TRUE
)

# =============================================================================
# CONTROL FACTORY
# =============================================================================

make_ctrl <- function(d_stage,
                      weight_cases   = 1.0,
                      weight_deaths  = 1.0,
                      weights_time   = NULL,
                      plots          = TRUE,
                      extra_sampling = list()) {

  base_sampling <- list(
    sample_initial_conditions = TRUE,
    ic_moment_match           = TRUE,
    sample_tau_i              = FALSE,
    sample_mobility_gamma     = FALSE,
    sample_mobility_omega     = FALSE,
    sample_kappa              = FALSE
  )
  sampling <- modifyList(base_sampling, extra_sampling)

  mosaic_control_defaults(
    calibration = list(
      n_simulations = NULL,
      n_iterations  = n_iter,
      batch_size    = d_stage$batch_size,
      min_batches   = d_stage$min_batches,
      max_batches   = d_stage$max_batches,
      target_r2     = d_stage$target_r2
    ),
    sampling = sampling,
    likelihood = list(
      weight_cases      = weight_cases,
      weight_deaths     = weight_deaths,
      add_max_terms     = FALSE,
      nb_k_min_cases    = 3,
      nb_k_min_deaths   = 3,
      enable_guardrails = FALSE,
      weights_time      = weights_time
    ),
    targets = list(
      ESS_param      = d_stage$ESS_param,
      ESS_param_prop = d_stage$ESS_param_prop,
      ESS_best       = d_stage$ESS_best,
      A_best         = d_stage$A_best,
      CVw_best       = d_stage$CVw_best
    ),
    predictions = list(
      best_model_n_sims         = 10L,
      ensemble_n_sims_per_param = 2L
    ),
    parallel = list(enable = TRUE, n_cores = n_cores),
    paths    = list(plots = plots)
  )
}

# =============================================================================
# MAIN LOOP
# =============================================================================

all_results   <- list()
t_start_total <- Sys.time()

for (iso in names(locations)) {

  label   <- locations[[iso]]
  dir_iso <- file.path(base_dir, iso)

  log_msg(paste(rep("=", 70), collapse = ""))
  log_msg("LOCATION: %s (%s)", label, iso)
  log_msg(paste(rep("=", 70), collapse = ""))

  config       <- get_location_config(iso = iso)
  priors       <- get_location_priors(iso = iso)
  n_t               <- ncol(config$reported_cases)
  weights_time      <- seq(0.75, 1.25, length.out = n_t)  # cases ramp: S1 and S3
  weights_time_unif <- rep(1.0, n_t)  # uniform: S2 deaths-only
  # Temporal ramp is outcome-specific: for cases the 0.75->1.25 ramp deweights
  # early (potentially noisy) case reports. For deaths, early data is equally
  # or more informative for CFR estimation — uniform weights avoid biasing the
  # CFR posterior toward recent epidemic values.

  stage_results <- list()

  # -------------------------------------------------------------------------
  # STAGE 1 — Cases (soft narrowing, all params free)
  # -------------------------------------------------------------------------
  dir_s1 <- file.path(dir_iso, "stage_1")

  if (!file.exists(file.path(dir_s1, "3_results/summary.json"))) {
    log_msg("[%s] S1: Cases (auto, ESS=%d, prop=%.0f%%, A_best=%.0f%%)",
            iso, design_B$s1$ESS_param,
            design_B$s1$ESS_param_prop * 100, design_B$s1$A_best * 100)
    tryCatch(
      run_MOSAIC(
        config     = config,
        priors     = priors,
        dir_output = dir_s1,
        control    = make_ctrl(
          d_stage       = design_B$s1,
          weight_cases  = 1.0,
          weight_deaths = 0.1,
          weights_time  = weights_time,
          plots         = TRUE
        )
      ),
      error = function(e) log_msg("[%s] S1 ERROR: %s", iso, e$message)
    )
  } else {
    log_msg("[%s] S1 already complete — skipping", iso)
  }

  stage_results$S1 <- tryCatch(
    jsonlite::read_json(file.path(dir_s1, "3_results/summary.json")),
    error = function(e) list()
  )

  s1_best_config <- file.path(dir_s1, "2_calibration/best_model/config_best.json")
  s1_posteriors  <- file.path(dir_s1, "2_calibration/posterior/posteriors.json")

  if (!file.exists(s1_posteriors)) {
    log_msg("[%s] S1 posteriors missing — skipping S2+S3", iso)
    all_results[[iso]] <- stage_results
    next
  }

  priors_s2 <- update_priors_from_posteriors(
    priors     = priors,
    posteriors = s1_posteriors,
    verbose    = FALSE
  )

  # -------------------------------------------------------------------------
  # STAGE 2 — Deaths (4 CFR params only, S1 config frozen)
  # -------------------------------------------------------------------------
  dir_s2 <- file.path(dir_iso, "stage_2")

  if (file.exists(s1_best_config)) {
    if (!file.exists(file.path(dir_s2, "3_results/summary.json"))) {
      log_msg("[%s] S2: Deaths (auto, ESS=%d, prop=%.0f%%, 4 CFR params)",
              iso, design_B$s2$ESS_param,
              design_B$s2$ESS_param_prop * 100)
      config_s1_best <- jsonlite::read_json(s1_best_config, simplifyVector = TRUE)
      tryCatch(
        run_MOSAIC(
          config     = config_s1_best,
          priors     = priors_s2,
          dir_output = dir_s2,
          control    = make_ctrl(
            d_stage        = design_B$s2,
            weight_cases   = 0.0,
            weight_deaths  = 1.0,
            weights_time   = weights_time_unif,  # uniform: avoids biasing CFR to recent values
            plots          = TRUE,
            extra_sampling = s2_deaths_freeze
          )
        ),
        error = function(e) log_msg("[%s] S2 ERROR: %s", iso, e$message)
      )
    } else {
      log_msg("[%s] S2 already complete — skipping", iso)
    }
  } else {
    log_msg("[%s] S2 skipped — S1 best config missing", iso)
  }

  stage_results$S2 <- tryCatch(
    jsonlite::read_json(file.path(dir_s2, "3_results/summary.json")),
    error = function(e) list()
  )

  s2_posteriors <- file.path(dir_s2, "2_calibration/posterior/posteriors.json")

  # -------------------------------------------------------------------------
  # STAGE 3 — Joint (adaptive weights, inflated CFR priors)
  # -------------------------------------------------------------------------
  dir_s3 <- file.path(dir_iso, "stage_3")

  if (file.exists(s1_posteriors) && file.exists(s2_posteriors)) {
    if (!file.exists(file.path(dir_s3, "3_results/summary.json"))) {

      # --- Compute adaptive weights from S1/S2 summaries ---
      summ_s1  <- stage_results$S1
      summ_s2  <- stage_results$S2
      adaptive <- NULL

      if (length(summ_s1) > 0 && length(summ_s2) > 0 &&
          !is.null(summ_s1$r2_cases)         && !is.null(summ_s2$r2_deaths) &&
          !is.null(summ_s1$bias_ratio_cases) && !is.null(summ_s2$bias_ratio_deaths)) {

        # Fraction floored: simulations that hit the likelihood floor
        frac_floor_cases <- tryCatch({
          s <- arrow::read_parquet(file.path(dir_s1, "2_calibration/samples.parquet"),
                                   col_select = "likelihood")
          mean(s$likelihood <= -1e8, na.rm = TRUE)
        }, error = function(e) NULL)

        frac_floor_deaths <- tryCatch({
          s <- arrow::read_parquet(file.path(dir_s2, "2_calibration/samples.parquet"),
                                   col_select = "likelihood")
          mean(s$likelihood <= -1e8, na.rm = TRUE)
        }, error = function(e) NULL)

        adaptive <- tryCatch(
          mosaic_adaptive_s3_weights(
            r2_cases            = summ_s1$r2_cases,
            r2_deaths           = summ_s2$r2_deaths,
            bias_cases          = summ_s1$bias_ratio_cases,
            bias_deaths         = summ_s2$bias_ratio_deaths,
            frac_floored_cases  = frac_floor_cases,
            frac_floored_deaths = frac_floor_deaths,
            verbose             = TRUE
          ),
          error = function(e) {
            log_msg("[%s] Adaptive weights failed (%s) — using fallback 0.30/0.70", iso, e$message)
            NULL
          }
        )
      }

      # Fallback to Design B defaults if adaptive computation failed
      w_cases  <- if (!is.null(adaptive)) adaptive$weight_cases  else 0.30
      w_deaths <- if (!is.null(adaptive)) adaptive$weight_deaths else 0.70

      log_msg("[%s] S3 weights: w_cases=%.3f w_deaths=%.3f (%s)",
              iso, w_cases, w_deaths,
              if (!is.null(adaptive) && !isTRUE(adaptive$degenerate)) "adaptive" else "fallback")

      # --- Build S3 priors: S1 + S2 posteriors, all params inflated ---
      # Step 1: chain S2 posteriors on top of S1 posteriors.
      #   priors_s3_base contains:
      #     - Transmission params: S1 posteriors (cases-only)
      #     - CFR params: S2 posteriors (deaths-only)
      priors_s3_base <- update_priors_from_posteriors(
        priors     = priors_s2,
        posteriors = s2_posteriors,
        verbose    = FALSE
      )

      # Step 2: inflate ALL params by f=1.5 before S3.
      # Applies to both transmission params (S1 posteriors) and CFR params
      # (S2 posteriors). Prevents IS weight collapse when the S3 joint optimum
      # differs from the staged single-outcome optima (Liu & West 2001).
      # f=1.5 broadens pre-truncation variance 1.5x while preserving means.
      log_msg("[%s] Inflating ALL priors x%.1f before S3", iso, S3_INFLATION_FACTOR)
      priors_s3 <- inflate_priors(
        priors           = priors_s3_base,
        inflation_factor = S3_INFLATION_FACTOR,
        verbose          = FALSE   # suppress per-param messages for 43 params
      )

      log_msg("[%s] S3: Joint (auto, ESS=%d, prop=%.0f%%, A_best=%.0f%%)",
              iso, design_B$s3$ESS_param,
              design_B$s3$ESS_param_prop * 100, design_B$s3$A_best * 100)

      tryCatch(
        run_MOSAIC(
          config     = config,
          priors     = priors_s3,
          dir_output = dir_s3,
          control    = make_ctrl(
            d_stage       = design_B$s3,
            weight_cases  = w_cases,
            weight_deaths = w_deaths,
            weights_time  = weights_time,
            plots         = TRUE
          )
        ),
        error = function(e) log_msg("[%s] S3 ERROR: %s", iso, e$message)
      )

    } else {
      log_msg("[%s] S3 already complete — skipping", iso)
    }
  } else {
    log_msg("[%s] S3 skipped — S1 or S2 posteriors missing", iso)
  }

  stage_results$S3 <- tryCatch(
    jsonlite::read_json(file.path(dir_s3, "3_results/summary.json")),
    error = function(e) list()
  )

  all_results[[iso]] <- stage_results
}

# =============================================================================
# SUMMARY
# =============================================================================

fmt_v <- function(v) if (is.null(v) || is.na(v)) "  ----" else sprintf("%6.4f", v)
fmt_b <- function(v) if (is.null(v) || is.na(v)) "  ----" else sprintf("%6.3f", v)
fmt_n <- function(v) if (is.null(v) || is.na(v)) "  ----" else sprintf("%6d", as.integer(v))

log_msg(paste(rep("=", 80), collapse = ""))
log_msg("CALIBRATION TEST 43 — RESULTS SUMMARY")
log_msg(paste(rep("=", 80), collapse = ""))
log_msg("Design B + inflate_priors(ALL params, f=%.1f) + adaptive_s3_weights", S3_INFLATION_FACTOR)
log_msg("")

for (metric in c("r2_cases", "r2_deaths", "bias_ratio_cases", "bias_ratio_deaths")) {
  fmt <- if (grepl("bias", metric)) fmt_b else fmt_v
  log_msg("--- %s ---", toupper(metric))
  log_msg(sprintf("%-6s  %10s  %10s  %10s  %9s", "Loc", "S1", "S2", "S3", "S2→S3Δ"))
  log_msg(paste(rep("-", 52), collapse = ""))
  for (iso in names(locations)) {
    v1 <- tryCatch(all_results[[iso]]$S1[[metric]]%||%NA, error=function(e)NA)
    v2 <- tryCatch(all_results[[iso]]$S2[[metric]]%||%NA, error=function(e)NA)
    v3 <- tryCatch(all_results[[iso]]$S3[[metric]]%||%NA, error=function(e)NA)
    delta <- if(!is.na(v2)&&!is.na(v3)) sprintf("%+9.4f", v3-v2) else "      --"
    log_msg("%-6s  %s  %s  %s  %s", iso, fmt(v1), fmt(v2), fmt(v3), delta)
  }
  log_msg("")
}

log_msg("--- SIMS PER STAGE ---")
log_msg(sprintf("%-6s  %8s  %8s  %8s  %8s", "Loc", "S1", "S2", "S3", "Total"))
log_msg(paste(rep("-", 46), collapse = ""))
for (iso in names(locations)) {
  n1  <- tryCatch(as.integer(all_results[[iso]]$S1$n_simulations_total%||%NA), error=function(e)NA)
  n2  <- tryCatch(as.integer(all_results[[iso]]$S2$n_simulations_total%||%NA), error=function(e)NA)
  n3  <- tryCatch(as.integer(all_results[[iso]]$S3$n_simulations_total%||%NA), error=function(e)NA)
  tot <- sum(c(n1,n2,n3), na.rm=TRUE)
  log_msg("%-6s  %s  %s  %s  %8d", iso, fmt_n(n1), fmt_n(n2), fmt_n(n3), tot)
}

total_mins <- as.numeric(difftime(Sys.time(), t_start_total, units = "mins"))
log_msg("")
log_msg(paste(rep("=", 80), collapse = ""))
log_msg("Total runtime: %.1f min (%.1f hrs)", total_mins, total_mins / 60)
log_msg("Output: %s", base_dir)
log_msg("")
log_msg("KEY QUESTIONS:")
log_msg("  1. Does inflate_priors reduce deaths bias (was 2.7-4x in test_42)?")
log_msg("  2. Do adaptive weights produce sensible per-country splits?")
log_msg("  3. Does S2->S3 delta r2_deaths remain positive (no collapse)?")
log_msg("  4. Does cases fit hold at S3 despite deaths-heavy adaptive weights?")
log_msg(paste(rep("=", 80), collapse = ""))
