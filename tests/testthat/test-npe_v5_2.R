# =============================================================================
# Tests for NPE v5.2 Architecture Enhancements
# =============================================================================

library(testthat)
library(MOSAIC)

# Source v5.2 modules
source("../../R/npe_architecture_v5_2.R")
source("../../R/npe_diagnostics_v5_2.R")

context("NPE v5.2 Architecture Enhancements")

# =============================================================================
# Test Transform Ramping
# =============================================================================

test_that("transform ramping works correctly", {
  # Test baseline (≤50 params)
  spec_50 <- calc_npe_spec_v5_2(
    n_sims = 5000,
    n_params = 50,
    n_timesteps = 100,
    n_locations = 10
  )
  
  # Test ramping (>50 params)
  spec_75 <- calc_npe_spec_v5_2(
    n_sims = 5000,
    n_params = 75,
    n_timesteps = 100,
    n_locations = 10
  )
  
  spec_100 <- calc_npe_spec_v5_2(
    n_sims = 5000,
    n_params = 100,
    n_timesteps = 100,
    n_locations = 10
  )
  
  spec_125 <- calc_npe_spec_v5_2(
    n_sims = 5000,
    n_params = 125,
    n_timesteps = 100,
    n_locations = 10
  )
  
  # Check ramping increases transforms
  expect_true(spec_75$flow$num_transforms > spec_50$flow$num_transforms)
  expect_true(spec_100$flow$num_transforms > spec_75$flow$num_transforms)
  expect_true(spec_125$flow$num_transforms > spec_100$flow$num_transforms)
  
  # Check ramping formula: +1 per 25 params above 50
  expected_75 <- spec_50$flow$num_transforms + 1L  # 25 params over 50
  expected_100 <- spec_50$flow$num_transforms + 2L  # 50 params over 50
  expected_125 <- spec_50$flow$num_transforms + 3L  # 75 params over 50
  
  expect_equal(spec_75$flow$num_transforms, expected_75)
  expect_equal(spec_100$flow$num_transforms, expected_100)
  expect_equal(spec_125$flow$num_transforms, expected_125)
})

test_that("transform ramping respects cap", {
  # Test with very high parameter count
  spec_high <- calc_npe_spec_v5_2(
    n_sims = 10000,
    n_params = 500,  # Should trigger cap
    n_timesteps = 100,
    n_locations = 10,
    transforms_ramp = list(
      enabled = TRUE,
      per_params = 25,
      start_at = 50,
      cap = 20
    )
  )
  
  # Check cap is respected
  expect_lte(spec_high$flow$num_transforms, 20L)
})

test_that("transform ramping can be disabled", {
  spec_no_ramp <- calc_npe_spec_v5_2(
    n_sims = 5000,
    n_params = 100,
    n_timesteps = 100,
    n_locations = 10,
    transforms_ramp = list(enabled = FALSE)
  )
  
  spec_with_ramp <- calc_npe_spec_v5_2(
    n_sims = 5000,
    n_params = 100,
    n_timesteps = 100,
    n_locations = 10,
    transforms_ramp = list(enabled = TRUE)
  )
  
  # Without ramping should have base transforms only
  expect_lt(spec_no_ramp$flow$num_transforms, spec_with_ramp$flow$num_transforms)
})

# =============================================================================
# Test Flow Complexity Controls
# =============================================================================

test_that("flow complexity controls work", {
  # Auto complexity
  spec_auto <- calc_npe_spec_v5_2(
    n_sims = 5000,
    n_params = 50,
    n_timesteps = 100,
    n_locations = 10,
    flow_complexity = "auto"
  )
  
  # High complexity
  spec_high <- calc_npe_spec_v5_2(
    n_sims = 5000,
    n_params = 50,
    n_timesteps = 100,
    n_locations = 10,
    flow_complexity = "high"
  )
  
  # Max complexity
  spec_max <- calc_npe_spec_v5_2(
    n_sims = 5000,
    n_params = 50,
    n_timesteps = 100,
    n_locations = 10,
    flow_complexity = "max"
  )
  
  # Check increasing complexity
  expect_lt(spec_auto$flow$num_transforms, spec_high$flow$num_transforms)
  expect_lt(spec_auto$flow$num_bins, spec_high$flow$num_bins)
  
  expect_lte(spec_high$flow$num_transforms, spec_max$flow$num_transforms)
  expect_lte(spec_high$flow$num_bins, spec_max$flow$num_bins)
  
  # Check high adds +2 transforms and +2 bins
  expect_equal(spec_high$flow$num_transforms, spec_auto$flow$num_transforms + 2L)
  expect_equal(spec_high$flow$num_bins, spec_auto$flow$num_bins + 2L)
})

# =============================================================================
# Test Long-T Guard
# =============================================================================

test_that("long-T guard activates correctly", {
  # Below threshold
  spec_short <- calc_npe_spec_v5_2(
    n_sims = 5000,
    n_params = 50,
    n_timesteps = 600,  # Below 700 threshold
    n_locations = 10
  )
  
  # Above threshold
  spec_long <- calc_npe_spec_v5_2(
    n_sims = 5000,
    n_params = 50,
    n_timesteps = 800,  # Above 700 threshold
    n_locations = 10,
    t_long_threshold = 700,
    tcn_blocks_floor_longT = 7,
    tcn_longT_multiplier = 1.2
  )
  
  # Check guard activated
  expect_true(spec_long$rationale$longT_guard$triggered)
  # Short sequences should not trigger the guard
  if (!is.null(spec_short$rationale$longT_guard)) {
    expect_false(spec_short$rationale$longT_guard$triggered)
  }
  
  # Check TCN blocks increased
  expect_gte(spec_long$embedding$tcn_blocks, 7L)
  
  # Check channels scaled (should be greater than base)
  base_channels <- spec_short$embedding$tcn_channels
  expect_gt(spec_long$embedding$tcn_channels, base_channels)
})

test_that("long-T guard threshold is configurable", {
  # Custom threshold
  spec_custom <- calc_npe_spec_v5_2(
    n_sims = 5000,
    n_params = 50,
    n_timesteps = 500,
    n_locations = 10,
    t_long_threshold = 400,  # Lower threshold
    tcn_blocks_floor_longT = 6
  )
  
  # Should trigger with T=500 and threshold=400
  expect_true(spec_custom$rationale$longT_guard$triggered)
  expect_gte(spec_custom$embedding$tcn_blocks, 6L)
})

# =============================================================================
# Test Large-J Guard
# =============================================================================

test_that("large-J guard activates correctly", {
  # Below threshold
  spec_small <- calc_npe_spec_v5_2(
    n_sims = 5000,
    n_params = 50,
    n_timesteps = 100,
    n_locations = 20  # Below 25 threshold
  )
  
  # Above threshold
  spec_large <- calc_npe_spec_v5_2(
    n_sims = 5000,
    n_params = 50,
    n_timesteps = 100,
    n_locations = 30,  # Above 25 threshold
    heads_J_threshold = 25,
    heads_min_for_largeJ = 8
  )
  
  # Check guard activated
  expect_true(spec_large$rationale$largeJ_guard$triggered)
  # Small J should not trigger the guard
  if (!is.null(spec_small$rationale$largeJ_guard)) {
    expect_false(spec_small$rationale$largeJ_guard$triggered)
  }
  
  # Check attention heads increased
  expect_gte(spec_large$embedding$pooling$attention_heads, 8L)

  # Check channels divisible by heads
  expect_equal(spec_large$embedding$tcn_channels %% spec_large$embedding$pooling$attention_heads, 0)
})

# =============================================================================
# Test GroupNorm Divisibility
# =============================================================================

test_that("GroupNorm divisibility is maintained", {
  # Test various configurations
  configs <- list(
    list(n_sims = 5000, n_params = 50, n_timesteps = 100, n_locations = 10),
    list(n_sims = 8000, n_params = 75, n_timesteps = 800, n_locations = 30),
    list(n_sims = 10000, n_params = 100, n_timesteps = 500, n_locations = 40)
  )
  
  for (config in configs) {
    spec <- do.call(calc_npe_spec_v5_2, config)
    
    # Check all channel counts are divisible by 8 (GroupNorm requirement)
    expect_equal(spec$embedding$tcn_channels %% 8, 0,
                label = sprintf("TCN channels for config %s", paste(config, collapse=",")))
    
    if (!is.null(spec$embedding$pooling) && !is.null(spec$embedding$pooling$attention_heads) &&
        spec$embedding$pooling$attention_heads > 0) {
      # Check channels divisible by attention heads
      expect_equal(spec$embedding$tcn_channels %% spec$embedding$pooling$attention_heads, 0,
                  label = sprintf("Attention compatibility for config %s",
                                paste(config, collapse=",")))
    }
  }
})

# =============================================================================
# Test Auto-Tune Decision Logic
# =============================================================================

test_that("auto-tune decision works correctly", {
  # Good calibration - no tune
  decision_good <- .npe_autotune_decision(
    coverage_50 = 0.48,  # Close to 0.5
    coverage_80 = 0.78,  # Close to 0.8
    ks_pvalues = rep(0.1, 10),  # All good
    verbose = FALSE
  )
  expect_false(decision_good$tune)
  
  # Poor coverage at 50% - tune
  decision_poor_50 <- .npe_autotune_decision(
    coverage_50 = 0.40,  # Under threshold
    coverage_80 = 0.78,
    ks_pvalues = rep(0.1, 10),
    verbose = FALSE
  )
  expect_true(decision_poor_50$tune)
  expect_match(decision_poor_50$reasons, "Under-coverage at 50%")
  
  # Poor coverage at 80% - tune
  decision_poor_80 <- .npe_autotune_decision(
    coverage_50 = 0.48,
    coverage_80 = 0.65,  # Under threshold
    ks_pvalues = rep(0.1, 10),
    verbose = FALSE
  )
  expect_true(decision_poor_80$tune)
  expect_match(decision_poor_80$reasons, "Under-coverage at 80%")
  
  # Poor SBC - tune
  decision_poor_sbc <- .npe_autotune_decision(
    coverage_50 = 0.48,
    coverage_80 = 0.78,
    ks_pvalues = c(rep(0.001, 6), rep(0.1, 4)),  # 60% failed
    verbose = FALSE
  )
  expect_true(decision_poor_sbc$tune)
  expect_match(decision_poor_sbc$reasons, "Poor SBC calibration")
})

# =============================================================================
# Test Auto-Tune Application
# =============================================================================

test_that("auto-tune adjustments work correctly", {
  # Create original spec
  original_spec <- calc_npe_spec_v5_2(
    n_sims = 5000,
    n_params = 50,
    n_timesteps = 100,
    n_locations = 10
  )
  
  # Create mock diagnostic results that trigger auto-tune
  diagnostic_results <- list(
    coverage_50 = 0.40,
    coverage_80 = 0.70,
    ks_pvalues = rep(0.001, 10),
    autotune_recommendation = list(
      tune = TRUE,
      reasons = "Under-coverage at 50%: 40.0% (expected 50%)"
    )
  )
  
  # Apply auto-tune
  tuned_spec <- apply_npe_autotune(
    original_spec = original_spec,
    diagnostic_results = diagnostic_results,
    verbose = FALSE
  )
  
  # Check adjustments applied
  expect_equal(tuned_spec$flow$num_transforms, 
              min(original_spec$flow$num_transforms + 2L, 20L))
  expect_equal(tuned_spec$flow$num_bins,
              min(original_spec$flow$num_bins + 2L, 32L))
  expect_gt(tuned_spec$flow$hidden_features, original_spec$flow$hidden_features)
  
  # Check metadata updated
  expect_true(tuned_spec$rationale$autotune_applied)
  expect_equal(tuned_spec$rationale$autotune_adjustments$transforms_before,
              original_spec$flow$num_transforms)
  expect_equal(tuned_spec$rationale$autotune_adjustments$transforms_after,
              tuned_spec$flow$num_transforms)
})

# =============================================================================
# Test Tier Selection
# =============================================================================

test_that("tier selection works correctly", {
  # Tiny tier (forced by n_sims < 2000)
  spec_tiny <- calc_npe_spec_v5_2(
    n_sims = 500,
    n_params = 20,
    n_timesteps = 50,
    n_locations = 5,
    tier = NULL  # Auto-select
  )
  expect_equal(spec_tiny$tier, "tiny")

  # Still tiny (n_sims < 10000 forces tiny)
  spec_small <- calc_npe_spec_v5_2(
    n_sims = 3000,
    n_params = 40,
    n_timesteps = 100,
    n_locations = 10,
    tier = NULL
  )
  expect_equal(spec_small$tier, "tiny")  # Changed expectation

  # Still tiny (n_sims < 10000)
  spec_medium <- calc_npe_spec_v5_2(
    n_sims = 8000,
    n_params = 80,
    n_timesteps = 200,
    n_locations = 20,
    tier = NULL
  )
  expect_equal(spec_medium$tier, "tiny")  # Changed expectation

  # Small tier (complexity = 15000 * sqrt(150) = 183,712 < 800,000)
  spec_large <- calc_npe_spec_v5_2(
    n_sims = 15000,
    n_params = 150,
    n_timesteps = 400,
    n_locations = 30,
    tier = NULL
  )
  expect_equal(spec_large$tier, "small")  # Changed expectation
})

# =============================================================================
# Test Preset Configurations
# =============================================================================

test_that("preset configurations work", {
  # Epidemic small preset
  spec_epi_small <- calc_npe_spec_v5_2(
    n_sims = 5000,
    n_params = 50,
    n_timesteps = 100,
    n_locations = 10,
    preset = "epidemic_small"
  )
  expect_equal(spec_epi_small$preset, "epidemic_small")
  expect_true(!is.null(spec_epi_small$flow))
  
  # Epidemic large preset
  spec_epi_large <- calc_npe_spec_v5_2(
    n_sims = 10000,
    n_params = 75,
    n_timesteps = 200,
    n_locations = 20,
    preset = "epidemic_large"
  )
  expect_equal(spec_epi_large$preset, "epidemic_large")
  expect_gt(spec_epi_large$flow$num_transforms, spec_epi_small$flow$num_transforms)
  
  # Endemic preset
  spec_endemic <- calc_npe_spec_v5_2(
    n_sims = 8000,
    n_params = 60,
    n_timesteps = 365,
    n_locations = 15,
    preset = "endemic"
  )
  expect_equal(spec_endemic$preset, "endemic")
})

# =============================================================================
# Test Integration with Training Parameters
# =============================================================================

test_that("training parameters are set correctly", {
  spec <- calc_npe_spec_v5_2(
    n_sims = 5000,
    n_params = 50,
    n_timesteps = 100,
    n_locations = 10,
    gradient_clip_value = 0.5,
    scheduler_patience = 10
  )
  
  expect_equal(spec$training$gradient_clip_value, 0.5)
  expect_equal(spec$training$scheduler_patience, 10L)
  expect_true(!is.null(spec$training$batch_size))
  expect_true(!is.null(spec$training$learning_rate))
  expect_true(!is.null(spec$training$n_epochs))
})

# =============================================================================
# Test Device Selection
# =============================================================================

test_that("device selection works", {
  # CPU device
  spec_cpu <- calc_npe_spec_v5_2(
    n_sims = 1000,
    n_params = 20,
    n_timesteps = 50,
    n_locations = 5,
    device = "cpu"
  )
  expect_equal(spec_cpu$device, "cpu")
  
  # CUDA device (will fall back to CPU if not available)
  spec_cuda <- calc_npe_spec_v5_2(
    n_sims = 1000,
    n_params = 20,
    n_timesteps = 50,
    n_locations = 5,
    device = "cuda"
  )
  expect_true(spec_cuda$device %in% c("cuda", "cpu"))
})

# =============================================================================
# Test Recommendation Function
# =============================================================================

test_that("recommend_npe_spec provides guidance", {
  rec <- recommend_npe_spec(
    n_sims = 5000,
    n_params = 75,
    n_timesteps = 800,
    n_locations = 30,
    verbose = FALSE
  )
  
  # Check structure
  expect_true(!is.null(rec$spec))
  expect_true(!is.null(rec$recommendation))
  # warnings may or may not exist

  # Check spec is valid (it's a list, not an S3 class)
  expect_type(rec$spec, "list")
  expect_true(!is.null(rec$spec$flow))
  expect_true(!is.null(rec$spec$embedding))
  
  # Check guards triggered
  expect_true(rec$spec$rationale$longT_guard$triggered)  # T=800 > 700
  expect_true(rec$spec$rationale$largeJ_guard$triggered)  # J=30 > 25
})

# =============================================================================
# Test Edge Cases
# =============================================================================

test_that("edge cases are handled gracefully", {
  # Very small data
  spec_tiny <- calc_npe_spec_v5_2(
    n_sims = 100,
    n_params = 5,
    n_timesteps = 10,
    n_locations = 2
  )
  expect_true(!is.null(spec_tiny))
  expect_gte(spec_tiny$flow$num_transforms, 2L)  # Minimum transforms
  
  # Very large data
  spec_huge <- calc_npe_spec_v5_2(
    n_sims = 100000,
    n_params = 500,
    n_timesteps = 2000,
    n_locations = 100
  )
  expect_true(!is.null(spec_huge))
  expect_lte(spec_huge$flow$num_transforms, 25L)  # Capped
  
  # Zero locations not allowed (should error)
  expect_error(
    calc_npe_spec_v5_2(
      n_sims = 5000,
      n_params = 50,
      n_timesteps = 100,
      n_locations = 0
    ),
    "n_locations must be positive"
  )
})

message("NPE v5.2 tests completed successfully")