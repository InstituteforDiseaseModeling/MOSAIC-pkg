# =============================================================================
# test-ic_moment_match.R — Tests for ic_moment_match in sample_parameters()
#
# Covers:
#   1. Control infrastructure (flag registration, validation, defaults)
#   2. Deterministic math (moment_match_E_I formula and normalization)
#   3. Guard clauses (zero obs, near-zero sigma*rho, 10% cap)
#   4. Integration with sample_parameters() and sample_initial_conditions
# =============================================================================

# ---------------------------------------------------------------------------
# Helper: build a minimal config for testing (avoids loading real data)
# ---------------------------------------------------------------------------
make_test_config <- function(N = 1000000L,
                             reported_cases = NULL,
                             sigma = 0.5,
                             rho = 0.2,
                             chi_endemic = 0.5,
                             iota = 0.5) {
  if (is.null(reported_cases)) {
    # 100 days, first 10 are zero, then 10 cases/day for a week
    reported_cases <- c(rep(0, 10), rep(10, 7), rep(5, 83))
  }
  list(
    N_j_initial     = N,
    reported_cases  = reported_cases,
    sigma           = sigma,
    rho             = rho,
    chi_endemic     = chi_endemic,
    iota            = iota,
    location_name   = "TST"
  )
}

# Helper: build a sampled_props matrix (single location)
make_test_props <- function(S = 0.80, V1 = 0.03, V2 = 0.01,
                            E = 0.001, I = 0.001, R = 0.158) {
  m <- matrix(c(S, V1, V2, E, I, R), nrow = 1)
  colnames(m) <- c("S", "V1", "V2", "E", "I", "R")
  rownames(m) <- "TST"
  m
}


# ===========================================================================
# 1. CONTROL INFRASTRUCTURE
# ===========================================================================

test_that("ic_moment_match is registered in mosaic_control_defaults", {
  ctrl <- MOSAIC::mosaic_control_defaults()
  expect_true("ic_moment_match" %in% names(ctrl$sampling))
  expect_false(ctrl$sampling$ic_moment_match)
})

test_that("ic_moment_match is accepted by sample_parameters sample_args", {
  # Should not warn about unknown parameter
  expect_no_warning({
    # Use a minimal call that exercises the arg-parsing path
    # We can't run the full function without priors/config, so test
    # via the internal default_sample_args mechanism
    ctrl <- MOSAIC::mosaic_control_defaults()
    ctrl$sampling$ic_moment_match <- TRUE
    # Validation should pass (it's logical)
    MOSAIC:::.mosaic_validate_sampling_args(ctrl$sampling)
  })
})

test_that("ic_moment_match defaults to FALSE in sample_parameters", {
  ctrl <- MOSAIC::mosaic_control_defaults()
  expect_identical(ctrl$sampling$ic_moment_match, FALSE)
})

test_that(".mosaic_validate_sampling_args accepts ic_moment_match as logical", {
  args <- list(sample_initial_conditions = TRUE, ic_moment_match = TRUE)
  # Should not warn
  expect_no_warning(MOSAIC:::.mosaic_validate_sampling_args(args))
})

test_that(".mosaic_validate_sampling_args warns if ic_moment_match is non-logical", {
  args <- list(ic_moment_match = "moment_match")
  expect_warning(
    MOSAIC:::.mosaic_validate_sampling_args(args),
    "Non-logical"
  )
})


# ===========================================================================
# 2. DETERMINISTIC MATH (moment_match_E_I)
# ===========================================================================

test_that("moment_match_E_I derives I from obs_week1 * chi / (sigma * rho)", {
  cfg <- make_test_config(
    N = 1000000L,
    reported_cases = c(rep(0, 5), rep(100, 7), rep(50, 88)),
    sigma = 0.5,
    rho = 0.2,
    chi_endemic = 0.4,
    iota = 0.3
  )
  props <- make_test_props()

  result <- MOSAIC:::moment_match_E_I(cfg, props, "TST", verbose = FALSE)

  # Expected I = obs_week1 * chi / (sigma * rho)
  obs_week1 <- 100  # first 7 positive days are all 100
  expected_I_count <- obs_week1 * 0.4 / (0.5 * 0.2)  # = 400
  expected_E_count <- expected_I_count * 0.3            # = 120
  expected_prop_I <- expected_I_count / 1000000
  expected_prop_E <- expected_E_count / 1000000

  expect_equal(result[1, "I"], expected_prop_I, tolerance = 1e-10)
  expect_equal(result[1, "E"], expected_prop_E, tolerance = 1e-10)
})

test_that("moment_match_E_I normalizes compartments to sum to 1", {
  cfg <- make_test_config(reported_cases = c(rep(0, 3), rep(50, 7), rep(20, 90)))
  props <- make_test_props()

  result <- MOSAIC:::moment_match_E_I(cfg, props, "TST", verbose = FALSE)

  expect_equal(sum(result[1, ]), 1.0, tolerance = 1e-12)
  expect_true(all(result[1, ] >= 0))
})

test_that("moment_match_E_I does not modify V1, V2, R", {
  cfg <- make_test_config(reported_cases = c(rep(0, 3), rep(50, 7), rep(20, 90)))
  props <- make_test_props(V1 = 0.03, V2 = 0.01, R = 0.15)

  result <- MOSAIC:::moment_match_E_I(cfg, props, "TST", verbose = FALSE)

  # V1, V2, R should be unchanged (only E, I, S are modified)
  expect_equal(result[1, "V1"], 0.03)
  expect_equal(result[1, "V2"], 0.01)
  expect_equal(result[1, "R"], 0.15)
})

test_that("moment_match_E_I adjusts S as residual", {
  cfg <- make_test_config(reported_cases = c(rep(0, 3), rep(50, 7), rep(20, 90)))
  props <- make_test_props(V1 = 0.03, V2 = 0.01, R = 0.15)

  result <- MOSAIC:::moment_match_E_I(cfg, props, "TST", verbose = FALSE)

  # S should equal 1 - (V1 + V2 + E + I + R)
  expected_S <- 1 - result[1, "V1"] - result[1, "V2"] - result[1, "E"] -
                result[1, "I"] - result[1, "R"]
  expect_equal(result[1, "S"], expected_S, tolerance = 1e-12)
})

test_that("moment_match_E_I uses first positive observation window", {
  # 50 leading zeros, then cases start at day 51
  cases <- c(rep(0, 50), rep(200, 7), rep(100, 43))
  cfg <- make_test_config(
    reported_cases = cases,
    sigma = 0.25,
    rho = 0.4,
    chi_endemic = 0.5,
    iota = 0.2
  )
  props <- make_test_props()

  result <- MOSAIC:::moment_match_E_I(cfg, props, "TST", verbose = FALSE)

  # obs_week1 should be 200 (first 7 positive days)
  expected_I <- 200 * 0.5 / (0.25 * 0.4)  # = 1000
  expected_prop_I <- expected_I / 1000000

  expect_equal(result[1, "I"], expected_prop_I, tolerance = 1e-10)
})

test_that("moment_match_E_I handles partial first week (< 7 positive days)", {
  # Only 3 days of data after the first positive observation
  cases <- c(rep(0, 10), 80, 100, 120)
  cfg <- make_test_config(
    N = 1000000L,
    reported_cases = cases,
    sigma = 0.5,
    rho = 0.2,
    chi_endemic = 0.5,
    iota = 0.5
  )
  props <- make_test_props()

  result <- MOSAIC:::moment_match_E_I(cfg, props, "TST", verbose = FALSE)

  # obs_week1 = mean(80, 100, 120) = 100
  expected_I <- 100 * 0.5 / (0.5 * 0.2)  # = 500
  expected_prop_I <- expected_I / 1000000

  expect_equal(result[1, "I"], expected_prop_I, tolerance = 1e-10)
})

test_that("moment_match_E_I reproduces the reporting chain identity", {
  # For any valid parameters: I * sigma * rho / chi_endemic ≈ obs_week1
  cfg <- make_test_config(
    N = 10000000L,
    reported_cases = c(rep(0, 5), rep(500, 7), rep(200, 88)),
    sigma = 0.3,
    rho = 0.15,
    chi_endemic = 0.6,
    iota = 0.4
  )
  props <- make_test_props()

  result <- MOSAIC:::moment_match_E_I(cfg, props, "TST", verbose = FALSE)

  # Reconstruct
  I_count <- result[1, "I"] * 10000000
  est_t1 <- I_count * 0.3 * 0.15 / 0.6
  obs_week1 <- 500  # all 7 positive days are 500

  expect_equal(est_t1, obs_week1, tolerance = 0.01)
})


# ===========================================================================
# 3. GUARD CLAUSES
# ===========================================================================

test_that("moment_match_E_I falls back when all obs are zero", {
  cfg <- make_test_config(reported_cases = rep(0, 100))
  props <- make_test_props(E = 0.001, I = 0.002)
  original_E <- props[1, "E"]
  original_I <- props[1, "I"]

  result <- MOSAIC:::moment_match_E_I(cfg, props, "TST", verbose = FALSE)

  # E and I should be unchanged (fallback to prior)
  expect_equal(result[1, "E"], original_E)
  expect_equal(result[1, "I"], original_I)
})

test_that("moment_match_E_I falls back when all obs are NA", {
  cfg <- make_test_config(reported_cases = rep(NA, 100))
  props <- make_test_props(E = 0.001, I = 0.002)
  original_E <- props[1, "E"]
  original_I <- props[1, "I"]

  result <- MOSAIC:::moment_match_E_I(cfg, props, "TST", verbose = FALSE)

  expect_equal(result[1, "E"], original_E)
  expect_equal(result[1, "I"], original_I)
})

test_that("moment_match_E_I falls back when sigma*rho near zero", {
  cfg <- make_test_config(
    reported_cases = c(rep(0, 5), rep(100, 7), rep(50, 88)),
    sigma = 1e-12,
    rho = 1e-12
  )
  props <- make_test_props(E = 0.001, I = 0.002)
  original_E <- props[1, "E"]
  original_I <- props[1, "I"]

  result <- MOSAIC:::moment_match_E_I(cfg, props, "TST", verbose = FALSE)

  expect_equal(result[1, "E"], original_E)
  expect_equal(result[1, "I"], original_I)
})

test_that("moment_match_E_I caps E+I at 10% of population", {
  # Very high obs relative to small reporting product → uncapped would exceed 10%
  cfg <- make_test_config(
    N = 100000L,
    reported_cases = c(rep(0, 3), rep(10000, 7), rep(5000, 90)),
    sigma = 0.01,
    rho = 0.01,
    chi_endemic = 0.5,
    iota = 1.0
  )
  props <- make_test_props()

  result <- MOSAIC:::moment_match_E_I(cfg, props, "TST", verbose = FALSE)

  # E + I should not exceed 10%
  EI_sum <- result[1, "E"] + result[1, "I"]
  expect_lte(EI_sum, 0.10 + 1e-12)

  # And proportions should still sum to 1
  expect_equal(sum(result[1, ]), 1.0, tolerance = 1e-12)
})

test_that("moment_match_E_I preserves E/I ratio when cap triggers", {
  cfg <- make_test_config(
    N = 100000L,
    reported_cases = c(rep(0, 3), rep(10000, 7), rep(5000, 90)),
    sigma = 0.01,
    rho = 0.01,
    chi_endemic = 0.5,
    iota = 0.4  # E = 0.4 * I
  )
  props <- make_test_props()

  result <- MOSAIC:::moment_match_E_I(cfg, props, "TST", verbose = FALSE)

  # E/I ratio should be preserved at iota = 0.4
  ratio <- result[1, "E"] / result[1, "I"]
  expect_equal(ratio, 0.4, tolerance = 1e-6)
})

test_that("moment_match_E_I ensures at least 1 person in E and I", {
  # Very tiny obs_week1 with large population → counts would round to 0
  cfg <- make_test_config(
    N = 100000000L,  # 100 million
    reported_cases = c(rep(0, 10), 0.001, rep(0, 89)),
    sigma = 0.5,
    rho = 0.5,
    chi_endemic = 0.5,
    iota = 0.5
  )
  props <- make_test_props()

  result <- MOSAIC:::moment_match_E_I(cfg, props, "TST", verbose = FALSE)

  # Proportions should be at least 1/N
  expect_gte(result[1, "E"], 1 / 100000000)
  expect_gte(result[1, "I"], 1 / 100000000)
})


# ===========================================================================
# 4. INTEGRATION WITH sample_parameters()
# ===========================================================================

test_that("sample_parameters accepts ic_moment_match via sample_args", {
  skip_if_not_installed("MOSAIC")
  skip_on_cran()

  root <- if (dir.exists("/workspace/MOSAIC")) "/workspace/MOSAIC" else "~/MOSAIC"
  skip_if_not(dir.exists(root), paste("MOSAIC root not found at", root))
  set_root_directory(root)
  config <- config_default_MOZ
  priors <- priors_default_MOZ

  # Should not error or warn about unknown parameter
  expect_no_warning({
    cfg <- sample_parameters(
      seed = 99,
      priors = priors,
      config = config,
      sample_args = list(ic_moment_match = TRUE),
      verbose = FALSE,
      validate = FALSE
    )
  })

  # Should return a valid config with integer IC counts
  expect_true(is.integer(cfg$E_j_initial))
  expect_true(is.integer(cfg$I_j_initial))
  expect_true(is.integer(cfg$S_j_initial))

  # Compartments must sum to N
  total <- cfg$S_j_initial + cfg$V1_j_initial + cfg$V2_j_initial +
           cfg$E_j_initial + cfg$I_j_initial + cfg$R_j_initial
  expect_equal(total, cfg$N_j_initial)
})

test_that("ic_moment_match=FALSE produces same result as omitting it", {
  skip_if_not_installed("MOSAIC")
  skip_on_cran()

  root <- if (dir.exists("/workspace/MOSAIC")) "/workspace/MOSAIC" else "~/MOSAIC"
  skip_if_not(dir.exists(root), paste("MOSAIC root not found at", root))
  set_root_directory(root)
  config <- config_default_MOZ
  priors <- priors_default_MOZ

  cfg_default <- sample_parameters(
    seed = 42, priors = priors, config = config,
    verbose = FALSE, validate = FALSE
  )
  cfg_explicit <- sample_parameters(
    seed = 42, priors = priors, config = config,
    sample_args = list(ic_moment_match = FALSE),
    verbose = FALSE, validate = FALSE
  )

  expect_equal(cfg_default$E_j_initial, cfg_explicit$E_j_initial)
  expect_equal(cfg_default$I_j_initial, cfg_explicit$I_j_initial)
  expect_equal(cfg_default$S_j_initial, cfg_explicit$S_j_initial)
})

test_that("ic_moment_match=TRUE has no effect when sample_initial_conditions=FALSE", {
  skip_if_not_installed("MOSAIC")
  skip_on_cran()

  root <- if (dir.exists("/workspace/MOSAIC")) "/workspace/MOSAIC" else "~/MOSAIC"
  skip_if_not(dir.exists(root), paste("MOSAIC root not found at", root))
  set_root_directory(root)
  config <- config_default_MOZ
  priors <- priors_default_MOZ

  cfg_frozen <- sample_parameters(
    seed = 42, priors = priors, config = config,
    sample_args = list(sample_initial_conditions = FALSE, ic_moment_match = TRUE),
    verbose = FALSE, validate = FALSE
  )
  cfg_frozen_no_mm <- sample_parameters(
    seed = 42, priors = priors, config = config,
    sample_args = list(sample_initial_conditions = FALSE, ic_moment_match = FALSE),
    verbose = FALSE, validate = FALSE
  )

  # ICs should be identical (both use fixed defaults, moment_match has no effect)
  expect_equal(cfg_frozen$E_j_initial, cfg_frozen_no_mm$E_j_initial)
  expect_equal(cfg_frozen$I_j_initial, cfg_frozen_no_mm$I_j_initial)
  expect_equal(cfg_frozen$S_j_initial, cfg_frozen_no_mm$S_j_initial)
})

test_that("ic_moment_match=TRUE reproduces reporting chain identity across seeds", {
  skip_if_not_installed("MOSAIC")
  skip_on_cran()

  root <- if (dir.exists("/workspace/MOSAIC")) "/workspace/MOSAIC" else "~/MOSAIC"
  skip_if_not(dir.exists(root), paste("MOSAIC root not found at", root))
  set_root_directory(root)
  config <- config_default_MOZ
  priors <- priors_default_MOZ

  obs_raw <- as.numeric(unlist(config$reported_cases))
  pos_idx <- which(!is.na(obs_raw) & obs_raw > 0)
  first_pos <- pos_idx[1]
  window_end <- min(first_pos + 6, length(obs_raw))
  obs_week1 <- mean(obs_raw[first_pos:window_end], na.rm = TRUE)

  for (s in 1:5) {
    cfg <- sample_parameters(
      seed = s, priors = priors, config = config,
      sample_args = list(ic_moment_match = TRUE),
      verbose = FALSE, validate = FALSE
    )

    est_t1 <- cfg$I_j_initial * cfg$sigma * cfg$rho / cfg$chi_endemic
    ratio <- est_t1 / obs_week1

    # Ratio should be close to 1 (integer rounding causes small deviations)
    expect_gt(ratio, 0.5)
    expect_lt(ratio, 2.0)
  }
})
