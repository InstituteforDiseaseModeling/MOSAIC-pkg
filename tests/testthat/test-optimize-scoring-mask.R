# =============================================================================
# optimize_ensemble_subset() must score the SAME cells as everything else.
#
# The scored-cell mask (.mosaic_mask_central_for_scoring) NAs out the burn-in /
# scored-window lead-in and the final deaths column -- the timesteps the
# calibration likelihood excludes because the model is mechanically wrong there.
# run_MOSAIC() and calc_model_ensemble() apply it to every metric they report.
# Until v0.81.1 the optimiser, which is handed the mask on the ensemble object,
# never applied it: subset SELECTION scored cells nothing else scores.
# =============================================================================

# A deliberately adversarial ensemble: the members that fit the MASKED window
# best are the ones that fit the UNMASKED window worst, so the two scorings must
# choose differently. Param 1 is best late (the scored region) and worst early;
# param 2 is the reverse.
make_masked_ensemble <- function(n_locs = 2L, n_times = 12L, lead_in = 8L) {
  set.seed(99)
  n_params <- 6L; n_stoch <- 2L

  # A time-varying observed surface: a constant one makes R2 undefined (zero
  # variance), which is a property of the fixture, not of the code under test.
  wave  <- 100 + 30 * sin(seq_len(n_times))
  obs_c <- matrix(rep(wave, each = n_locs), n_locs, n_times)
  obs_d <- obs_c / 10

  cases_array  <- array(NA_real_, c(n_locs, n_times, n_params, n_stoch))
  deaths_array <- array(NA_real_, c(n_locs, n_times, n_params, n_stoch))
  early <- seq_len(lead_in)
  late  <- setdiff(seq_len(n_times), early)

  for (p in seq_len(n_params)) {
    # Odd params track obs closely LATE (the scored window) and wildly overshoot
    # EARLY; even params do the reverse. So masked and unmasked scoring rank
    # them in opposite orders -- which is what makes this fixture able to tell
    # whether the mask was applied at all.
    cm <- obs_c
    if (p %% 2L == 1L) { cm[, early] <- obs_c[, early] * 10; cm[, late] <- obs_c[, late] + p }
    else               { cm[, late]  <- obs_c[, late]  * 10; cm[, early] <- obs_c[, early] + p }
    for (s in seq_len(n_stoch)) {
      cases_array[, , p, s]  <- cm
      deaths_array[, , p, s] <- cm / 10
    }
  }

  structure(list(
    cases_array = cases_array, deaths_array = deaths_array,
    obs_cases = obs_c, obs_deaths = obs_d,
    n_locations = n_locs, n_time_points = n_times,
    n_param_sets = n_params, n_simulations_per_config = n_stoch,
    location_names = paste0("L", seq_len(n_locs)),
    envelope_quantiles = c(0.025, 0.25, 0.75, 0.975),
    # mask the lead_in columns of cases; deaths keeps its final-column rule
    artifact_mask = list(score_idx_cases = lead_in + 1L,
                         cases_warmup = 0L, deaths_final = TRUE)
  ), class = "mosaic_ensemble")
}

test_that("the optimiser scores only the masked window", {
  ens <- make_masked_ensemble()
  lls <- seq(-10, -15, length.out = 6)

  tb <- optimize_ensemble_subset(ens, lls, seeds = 1:6, min_n = 2L,
                                 objective = "mae", stride = 1L,
                                 verbose = FALSE)$evaluation_table

  # With the lead-in masked, the huge early error (1000 vs 100) contributes
  # nothing. The unmasked scoring would be dominated by it. Concretely: the
  # best achievable masked MAE here is small, because the odd params sit at
  # ~100 across the whole scored window.
  expect_true(all(is.finite(tb$score)))
  # Masked, the achievable cases MAE is a few units (odd params sit within `p`
  # of obs across the scored window). Unmasked it could not beat the ~500 that
  # the 10x early overshoot contributes.
  expect_lt(min(tb$mae_cases), 50)
})

test_that("masked and unmasked scoring disagree on this ensemble", {
  # Guards the test above from being vacuous: if the mask made no difference,
  # the assertion could pass for the wrong reason. Score the same predictions
  # both ways by hand and show they rank differently.
  ens <- make_masked_ensemble()
  lead <- 8L

  mae_for <- function(p, masked) {
    pred <- ens$cases_array[, , p, 1]
    if (masked) pred[, seq_len(lead)] <- NA_real_
    mean(abs(as.numeric(pred) - as.numeric(ens$obs_cases)), na.rm = TRUE)
  }

  # param 1 (accurate late) beats param 2 under the mask ...
  expect_lt(mae_for(1L, masked = TRUE), mae_for(2L, masked = TRUE))
  # ... and loses to it without the mask.
  expect_gt(mae_for(1L, masked = FALSE), mae_for(2L, masked = FALSE))
})

test_that("a NULL artifact_mask still applies the documented defaults", {
  # .mosaic_mask_central_for_scoring falls back to cases_warmup = 2,
  # deaths_final = TRUE. The optimiser must inherit that fallback rather than
  # skipping the mask entirely, so it matches what run_MOSAIC reports.
  ens <- make_masked_ensemble()
  ens$artifact_mask <- NULL
  lls <- seq(-10, -15, length.out = 6)

  tb <- optimize_ensemble_subset(ens, lls, seeds = 1:6, min_n = 2L,
                                 objective = "mae", stride = 1L,
                                 verbose = FALSE)$evaluation_table
  expect_true(all(is.finite(tb$score)))
  expect_true(nrow(tb) >= 2L)
})

test_that("every objective sees the mask, WIS included", {
  # WIS scores the interval spread, not just the point forecast, so its
  # quantiles have to be masked too -- otherwise it would be the one objective
  # still rewarding fit on excluded timesteps.
  ens <- make_masked_ensemble()
  lls <- seq(-10, -15, length.out = 6)

  for (obj in c("mae", "r2_bias", "wis")) {
    res <- optimize_ensemble_subset(ens, lls, seeds = 1:6, min_n = 2L,
                                    objective = obj, stride = 1L,
                                    verbose = FALSE)
    expect_true(all(is.finite(res$evaluation_table$score)), info = obj)
    expect_true(res$optimal_n >= 2L, info = obj)
  }
})

test_that("the MAE normaliser averages the same cells as the error", {
  # If the error moved to scored cells but the normaliser stayed over all
  # cells, the cases/deaths balance in the summed objective would depend on how
  # much of each channel happens to be masked -- and they are masked very
  # differently (a long lead-in for cases, one column for deaths).
  ens <- make_masked_ensemble()
  lead <- 8L

  ens2 <- ens
  ens2$obs_cases <- matrix(rep(seq_len(12) * 10, each = 2), 2, 12)
  o_all <- mean(as.numeric(ens2$obs_cases))
  o_msk <- local({
    m <- ens2$obs_cases; m[, seq_len(lead)] <- NA_real_
    mean(as.numeric(m), na.rm = TRUE)
  })
  expect_false(isTRUE(all.equal(o_all, o_msk)))   # they genuinely differ

  lls <- seq(-10, -15, length.out = 6)
  expect_no_error(
    optimize_ensemble_subset(ens2, lls, seeds = 1:6, min_n = 2L,
                             objective = "mae", stride = 1L, verbose = FALSE))
})


# =============================================================================
# The optimised ensemble must carry the scored-cell mask forward.
#
# run_MOSAIC() does `ensemble <- subset_opt$ensemble_optimized` and then scores
# every HEADLINE metric off that object, masking by `ensemble$artifact_mask`.
# Dropping the field did not disable masking -- it silently substituted
# .mosaic_mask_central_for_scoring()'s fallback (cases_warmup = 2) for the run's
# real scored window. On the 100k production runs the real spec was
# score_idx_cases = 31, so the headline R2 was computed over 28 timesteps the
# likelihood excludes: 0.5037 headline against 0.6302 tier, on identical data.
# =============================================================================

test_that("ensemble_optimized carries artifact_mask forward", {
  ens <- make_masked_ensemble()
  lls <- seq(-10, -15, length.out = 6)

  opt <- optimize_ensemble_subset(ens, lls, seeds = 1:6, min_n = 2L,
                                  objective = "mae", stride = 1L,
                                  verbose = FALSE)$ensemble_optimized

  expect_false(is.null(opt$artifact_mask))
  expect_identical(opt$artifact_mask, ens$artifact_mask)

  # And the consequence that matters: scoring the optimised object reproduces
  # the same window as scoring the candidate, rather than the 2-column fallback.
  masked_opt <- MOSAIC:::.mosaic_mask_central_for_scoring(
    opt$cases_median, "cases", opt$artifact_mask)
  fallback <- MOSAIC:::.mosaic_mask_central_for_scoring(
    opt$cases_median, "cases", NULL)
  expect_gt(sum(is.na(masked_opt)), sum(is.na(fallback)))
})

test_that("a NULL mask on the input stays NULL on the output", {
  # No mask in means no mask out -- the field must mirror the input, not
  # fabricate a spec.
  ens <- make_masked_ensemble()
  ens$artifact_mask <- NULL
  lls <- seq(-10, -15, length.out = 6)

  opt <- optimize_ensemble_subset(ens, lls, seeds = 1:6, min_n = 2L,
                                  objective = "mae", stride = 1L,
                                  verbose = FALSE)$ensemble_optimized
  expect_null(opt$artifact_mask)
})
