test_that("calc_model_ensemble requires config", {
  expect_error(calc_model_ensemble(config = NULL), "config is required")
})

test_that("calc_model_ensemble requires parameter_seeds or configs", {
  mock_config <- list(
    reported_cases = matrix(rpois(20, 10), nrow = 2),
    reported_deaths = matrix(rpois(20, 1), nrow = 2),
    location_name = c("A", "B"),
    date_start = "2023-01-01",
    date_stop = "2023-01-10"
  )
  expect_error(
    calc_model_ensemble(config = mock_config),
    "Must provide either"
  )
})

test_that("calc_model_ensemble validates envelope_quantiles", {
  mock_config <- list(
    reported_cases = 1:10,
    reported_deaths = 1:10,
    location_name = "A",
    date_start = "2023-01-01",
    date_stop = "2023-01-10"
  )

  # Odd number of quantiles

  expect_error(
    calc_model_ensemble(config = mock_config, configs = list(mock_config),
                        envelope_quantiles = c(0.025, 0.5, 0.975)),
    "even number"
  )

  # Out of order
  expect_error(
    calc_model_ensemble(config = mock_config, configs = list(mock_config),
                        envelope_quantiles = c(0.975, 0.025)),
    "ascending order"
  )
})

test_that("calc_model_ensemble validates parameter_weights", {
  mock_config <- list(
    reported_cases = 1:10,
    reported_deaths = 1:10,
    location_name = "A",
    date_start = "2023-01-01",
    date_stop = "2023-01-10"
  )

  # Wrong length
  expect_error(
    calc_model_ensemble(config = mock_config, configs = list(mock_config, mock_config),
                        parameter_weights = c(1)),
    "same length"
  )

  # Negative weights
  expect_error(
    calc_model_ensemble(config = mock_config, configs = list(mock_config, mock_config),
                        parameter_weights = c(1, -1)),
    "non-negative"
  )
})

test_that("calc_model_ensemble validates artifact-mask params", {
  mock_config <- list(
    reported_cases = 1:10,
    reported_deaths = 1:10,
    location_name = "A",
    date_start = "2023-01-01",
    date_stop = "2023-01-10"
  )

  expect_error(
    calc_model_ensemble(config = mock_config, configs = list(mock_config),
                        n_cases_warmup_mask = -1L),
    "non-negative integer"
  )
  expect_error(
    calc_model_ensemble(config = mock_config, configs = list(mock_config),
                        n_cases_warmup_mask = c(1L, 2L)),
    "non-negative integer"
  )
  expect_error(
    calc_model_ensemble(config = mock_config, configs = list(mock_config),
                        mask_final_deaths_step = "yes"),
    "single logical"
  )
  expect_error(
    calc_model_ensemble(config = mock_config, configs = list(mock_config),
                        mask_final_deaths_step = c(TRUE, FALSE)),
    "single logical"
  )
})

test_that("mosaic_ensemble object has expected structure", {
  # Create a minimal precomputed results scenario
  n_locs <- 2
  n_times <- 5
  n_params <- 3
  n_stoch <- 2

  mock_config <- list(
    reported_cases = matrix(rpois(n_locs * n_times, 10), nrow = n_locs),
    reported_deaths = matrix(rpois(n_locs * n_times, 1), nrow = n_locs),
    location_name = c("LOC_A", "LOC_B"),
    date_start = "2023-01-01",
    date_stop = "2023-01-05"
  )

  configs <- lapply(1:n_params, function(i) mock_config)

  # Build precomputed results
  precomputed <- list()
  idx <- 0
  for (p in 1:n_params) {
    for (s in 1:n_stoch) {
      idx <- idx + 1
      precomputed[[idx]] <- list(
        param_idx = p,
        stoch_idx = s,
        reported_cases = matrix(rpois(n_locs * n_times, 10), nrow = n_locs),
        reported_deaths = matrix(rpois(n_locs * n_times, 1), nrow = n_locs),
        success = TRUE
      )
    }
  }

  ens <- calc_model_ensemble(
    config = mock_config,
    configs = configs,
    parameter_weights = c(0.5, 0.3, 0.2),
    n_simulations_per_config = n_stoch,
    envelope_quantiles = c(0.025, 0.975),
    precomputed_results = precomputed,
    verbose = FALSE
  )

  expect_s3_class(ens, "mosaic_ensemble")

  # Check all expected fields
  expect_true(!is.null(ens$cases_mean))
  expect_true(!is.null(ens$cases_median))
  expect_true(!is.null(ens$deaths_mean))
  expect_true(!is.null(ens$deaths_median))
  expect_true(!is.null(ens$ci_bounds))
  expect_true(!is.null(ens$obs_cases))
  expect_true(!is.null(ens$obs_deaths))
  expect_true(!is.null(ens$cases_array))
  expect_true(!is.null(ens$deaths_array))
  expect_true(!is.null(ens$parameter_weights))
  expect_equal(ens$n_param_sets, n_params)
  expect_equal(ens$n_simulations_per_config, n_stoch)
  expect_equal(ens$n_locations, n_locs)
  expect_equal(ens$n_time_points, n_times)
  expect_equal(ens$location_names, c("LOC_A", "LOC_B"))

  # Check dimensions
  expect_equal(dim(ens$cases_mean), c(n_locs, n_times))
  expect_equal(dim(ens$cases_median), c(n_locs, n_times))
  expect_equal(dim(ens$cases_array), c(n_locs, n_times, n_params, n_stoch))

  # Weights should be normalized
  expect_equal(sum(ens$parameter_weights), 1.0)

  # artifact_mask carries the default resolved spec (defaults match plot_model_ensemble)
  expect_equal(ens$artifact_mask, list(cases_warmup = 2L, deaths_final = TRUE))
})

test_that("calc_model_ensemble records non-default artifact_mask without mutating raw series", {
  n_locs <- 2; n_times <- 5; n_params <- 2; n_stoch <- 1

  mock_config <- list(
    reported_cases = matrix(rpois(n_locs * n_times, 10), nrow = n_locs),
    reported_deaths = matrix(rpois(n_locs * n_times, 1), nrow = n_locs),
    location_name = c("LOC_A", "LOC_B"),
    date_start = "2023-01-01",
    date_stop = "2023-01-05"
  )
  configs <- lapply(1:n_params, function(i) mock_config)

  precomputed <- list()
  idx <- 0
  for (p in 1:n_params) for (s in 1:n_stoch) {
    idx <- idx + 1
    precomputed[[idx]] <- list(
      param_idx = p, stoch_idx = s,
      reported_cases = matrix(rpois(n_locs * n_times, 10), nrow = n_locs),
      reported_deaths = matrix(rpois(n_locs * n_times, 1), nrow = n_locs),
      success = TRUE
    )
  }

  ens <- calc_model_ensemble(
    config = mock_config, configs = configs,
    n_simulations_per_config = n_stoch,
    envelope_quantiles = c(0.025, 0.975),
    precomputed_results = precomputed,
    n_cases_warmup_mask = 3L, mask_final_deaths_step = FALSE,
    verbose = FALSE
  )

  expect_equal(ens$artifact_mask, list(cases_warmup = 3L, deaths_final = FALSE))

  # Raw central series must NOT contain mask-induced NAs (spec is carried, not applied)
  expect_false(any(is.na(ens$cases_mean)))
  expect_false(any(is.na(ens$deaths_mean)))
  expect_false(any(is.na(ens$cases_median)))
  expect_false(any(is.na(ens$deaths_median)))
})

test_that("uniform weights produce same result as unweighted mean", {
  set.seed(123)
  n_locs <- 1
  n_times <- 10
  n_params <- 4
  n_stoch <- 1

  mock_config <- list(
    reported_cases = matrix(rpois(n_times, 10), nrow = 1),
    reported_deaths = matrix(rpois(n_times, 1), nrow = 1),
    location_name = "A",
    date_start = "2023-01-01",
    date_stop = "2023-01-10"
  )

  configs <- lapply(1:n_params, function(i) mock_config)

  # Fixed deterministic results
  precomputed <- lapply(1:n_params, function(p) {
    list(
      param_idx = p,
      stoch_idx = 1L,
      reported_cases = matrix(p * (1:n_times), nrow = 1),
      reported_deaths = matrix(p * rep(1, n_times), nrow = 1),
      success = TRUE
    )
  })

  ens_uniform <- calc_model_ensemble(
    config = mock_config,
    configs = configs,
    parameter_weights = rep(1, n_params),
    n_simulations_per_config = 1L,
    precomputed_results = precomputed,
    verbose = FALSE
  )

  # With uniform weights, mean should be the arithmetic mean
  expected_cases_mean <- Reduce("+", lapply(1:n_params, function(p) p * (1:n_times))) / n_params
  expect_equal(as.numeric(ens_uniform$cases_mean), expected_cases_mean, tolerance = 1e-10)
})

test_that("non-uniform weights produce different predictions than uniform", {
  set.seed(456)
  n_locs <- 1
  n_times <- 10
  n_params <- 3
  n_stoch <- 1

  mock_config <- list(
    reported_cases = matrix(rpois(n_times, 10), nrow = 1),
    reported_deaths = matrix(rpois(n_times, 1), nrow = 1),
    location_name = "A",
    date_start = "2023-01-01",
    date_stop = "2023-01-10"
  )

  configs <- lapply(1:n_params, function(i) mock_config)

  # Deliberately different results per param
  precomputed <- list(
    list(param_idx = 1, stoch_idx = 1, reported_cases = matrix(rep(100, n_times), nrow = 1),
         reported_deaths = matrix(rep(10, n_times), nrow = 1), success = TRUE),
    list(param_idx = 2, stoch_idx = 1, reported_cases = matrix(rep(0, n_times), nrow = 1),
         reported_deaths = matrix(rep(0, n_times), nrow = 1), success = TRUE),
    list(param_idx = 3, stoch_idx = 1, reported_cases = matrix(rep(50, n_times), nrow = 1),
         reported_deaths = matrix(rep(5, n_times), nrow = 1), success = TRUE)
  )

  ens_uniform <- calc_model_ensemble(
    config = mock_config, configs = configs,
    parameter_weights = c(1, 1, 1),
    n_simulations_per_config = 1L,
    precomputed_results = precomputed, verbose = FALSE
  )

  ens_weighted <- calc_model_ensemble(
    config = mock_config, configs = configs,
    parameter_weights = c(10, 0.01, 0.01),
    n_simulations_per_config = 1L,
    precomputed_results = precomputed, verbose = FALSE
  )

  # Weighted median should differ from uniform median
  expect_false(isTRUE(all.equal(
    as.numeric(ens_uniform$cases_median),
    as.numeric(ens_weighted$cases_median)
  )))

  # Heavily weighted toward param 1 (100s), so weighted mean should be closer to 100
  expect_true(mean(ens_weighted$cases_mean) > mean(ens_uniform$cases_mean))
})

test_that("ensemble weighting pairs each (param,stoch) prediction with its OWN param weight (n_stoch>1 alignment)", {
  # Regression guard for the value<->weight alignment. as.vector(cases_array[i,j,,])
  # flattens the [param, stoch] slice param-fastest, so sim_weights must repeat
  # param-fastest (rep(w, times=n_stoch)) to attach each (param,stoch) prediction
  # to its own parameter's weight. The previous code used rep(w, each=n_stoch),
  # which mis-pairs them whenever n_stoch > 1. This test fails if that regresses.
  n_params <- 3L; n_stoch <- 2L
  vals <- c(10, 20, 30)          # each param emits a constant value across its stoch runs
  pw   <- c(0.7, 0.2, 0.1)       # param 1 dominates

  mock_config <- list(
    reported_cases = matrix(0, 1, 1), reported_deaths = matrix(0, 1, 1),
    location_name = "A", date_start = "2023-01-01", date_stop = "2023-01-01"
  )
  configs <- lapply(seq_len(n_params), function(i) mock_config)
  precomputed <- list(); idx <- 0L
  for (p in seq_len(n_params)) for (s in seq_len(n_stoch)) {
    idx <- idx + 1L
    precomputed[[idx]] <- list(param_idx = p, stoch_idx = s,
      reported_cases = matrix(vals[p], 1, 1),
      reported_deaths = matrix(vals[p], 1, 1), success = TRUE)
  }

  ens <- calc_model_ensemble(
    config = mock_config, configs = configs, parameter_weights = pw,
    n_simulations_per_config = n_stoch, envelope_quantiles = c(0.025, 0.975),
    precomputed_results = precomputed, verbose = FALSE
  )

  flat_vals    <- rep(vals, times = n_stoch)          # = as.vector(cases_array[1,1,,]) (param-fastest)
  aligned_w    <- rep(pw, times = n_stoch) / n_stoch  # CORRECT (what the fixed code uses)
  misaligned_w <- rep(pw, each  = n_stoch) / n_stoch  # the OLD bug

  # The ensemble median must equal the param-ALIGNED weighted quantile (bit-identical).
  expect_equal(as.numeric(ens$cases_median),
               weighted_quantiles(flat_vals, aligned_w, 0.5))
  # ...and the aligned vs misaligned results genuinely differ here, so this test
  # would FAIL if the weighting regressed to rep(..., each = n_stoch).
  expect_false(isTRUE(all.equal(
    weighted_quantiles(flat_vals, aligned_w,    0.5),
    weighted_quantiles(flat_vals, misaligned_w, 0.5)
  )))
})

# --- R-1: precomputed param_idx <-> weight alignment guard (v0.36.2) ----------
# On the Dask post-cal path, configs were compacted (dropped failed re-samples)
# while weights were derived from the uncompacted mask, so a dropped config
# shifted every later prediction onto the wrong weight and left a trailing all-NA
# slice — a silent wrong ensemble. The producer fix compacts seeds+weights in
# lockstep; this consumer-side guard makes any future mispairing fail loud.

test_that("calc_model_ensemble (precomputed) errors when param_idx overflows n_param_sets (R-1)", {
  mock_config <- list(
    reported_cases = matrix(0, 1, 1), reported_deaths = matrix(0, 1, 1),
    location_name = "A", date_start = "2023-01-01", date_stop = "2023-01-01"
  )
  precomputed <- list(
    list(param_idx = 1L, stoch_idx = 1L, reported_cases = matrix(10, 1, 1),
         reported_deaths = matrix(1, 1, 1), success = TRUE),
    list(param_idx = 4L, stoch_idx = 1L, reported_cases = matrix(10, 1, 1),
         reported_deaths = matrix(1, 1, 1), success = TRUE)   # 4 > n_param_sets (3)
  )
  expect_error(
    calc_model_ensemble(config = mock_config, parameter_seeds = c(1L, 2L, 3L),
                        parameter_weights = c(0.5, 0.3, 0.2), n_simulations_per_config = 1L,
                        precomputed_results = precomputed, verbose = FALSE),
    "misaligned"
  )
})

test_that("calc_model_ensemble (precomputed) warns when a param slice has no predictions (R-1)", {
  mock_config <- list(
    reported_cases = matrix(0, 1, 1), reported_deaths = matrix(0, 1, 1),
    location_name = "A", date_start = "2023-01-01", date_stop = "2023-01-01"
  )
  # param_idx covers {1,2} but there are 3 parameter sets => the dropped-config gap
  precomputed <- list(
    list(param_idx = 1L, stoch_idx = 1L, reported_cases = matrix(10, 1, 1),
         reported_deaths = matrix(1, 1, 1), success = TRUE),
    list(param_idx = 2L, stoch_idx = 1L, reported_cases = matrix(20, 1, 1),
         reported_deaths = matrix(2, 1, 1), success = TRUE)
  )
  expect_warning(
    calc_model_ensemble(config = mock_config, parameter_seeds = c(1L, 2L, 3L),
                        parameter_weights = c(0.5, 0.3, 0.2), n_simulations_per_config = 1L,
                        precomputed_results = precomputed, verbose = FALSE),
    "no precomputed predictions"
  )
})

test_that("calc_model_ensemble (precomputed) pairs dense param_idx with the right weight (R-1)", {
  # The fixed production path: seeds/weights compacted in lockstep so param_idx
  # densely covers 1:n_param_sets. No warning, and the weighted mean attaches
  # each prediction to its OWN parameter weight.
  mock_config <- list(
    reported_cases = matrix(0, 1, 1), reported_deaths = matrix(0, 1, 1),
    location_name = "A", date_start = "2023-01-01", date_stop = "2023-01-01"
  )
  vals <- c(10, 20, 30)
  pw   <- c(0.7, 0.2, 0.1)
  precomputed <- lapply(seq_len(3), function(p) list(
    param_idx = p, stoch_idx = 1L,
    reported_cases = matrix(vals[p], 1, 1), reported_deaths = matrix(vals[p], 1, 1),
    success = TRUE))
  ens <- calc_model_ensemble(config = mock_config, parameter_seeds = c(1L, 2L, 3L),
                             parameter_weights = pw, n_simulations_per_config = 1L,
                             precomputed_results = precomputed, verbose = FALSE)
  expect_equal(as.numeric(ens$cases_mean), weighted.mean(vals, pw))
})
