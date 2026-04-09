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
        disease_deaths = matrix(rpois(n_locs * n_times, 1), nrow = n_locs),
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
      disease_deaths = matrix(p * rep(1, n_times), nrow = 1),
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
         disease_deaths = matrix(rep(10, n_times), nrow = 1), success = TRUE),
    list(param_idx = 2, stoch_idx = 1, reported_cases = matrix(rep(0, n_times), nrow = 1),
         disease_deaths = matrix(rep(0, n_times), nrow = 1), success = TRUE),
    list(param_idx = 3, stoch_idx = 1, reported_cases = matrix(rep(50, n_times), nrow = 1),
         disease_deaths = matrix(rep(5, n_times), nrow = 1), success = TRUE)
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
