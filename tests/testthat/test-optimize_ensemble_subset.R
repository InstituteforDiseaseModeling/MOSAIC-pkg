# Helper: build a minimal mock mosaic_ensemble object for testing
make_mock_ensemble <- function(n_locs = 2, n_times = 10, n_params = 20, n_stoch = 3,
                               signal_strength = 5) {
  set.seed(42)

  # Observed data with a clear temporal pattern
  obs_cases  <- matrix(rpois(n_locs * n_times, lambda = 50 + signal_strength * (1:n_times)),
                       nrow = n_locs, ncol = n_times, byrow = TRUE)
  obs_deaths <- matrix(rpois(n_locs * n_times, lambda = 5 + 0.5 * (1:n_times)),
                       nrow = n_locs, ncol = n_times, byrow = TRUE)

  # Simulated predictions: better param sets (lower index) track obs more closely
  cases_array  <- array(NA_real_, dim = c(n_locs, n_times, n_params, n_stoch))
  deaths_array <- array(NA_real_, dim = c(n_locs, n_times, n_params, n_stoch))

  for (p in seq_len(n_params)) {
    noise_scale <- 0.5 + (p - 1) * 0.1  # param 1 is best, param n_params is worst
    for (s in seq_len(n_stoch)) {
      cases_array[, , p, s]  <- obs_cases * (1 + rnorm(n_locs * n_times, 0, noise_scale * 0.3))
      deaths_array[, , p, s] <- obs_deaths * (1 + rnorm(n_locs * n_times, 0, noise_scale * 0.3))
    }
  }

  # Weights: top params get higher weight
  weights <- rev(seq_len(n_params))
  weights <- weights / sum(weights)

  # Compute medians (simplified — just use first param's first stoch for mock).
  # `times` matches the param-fastest as.vector() layout (see calc_model_ensemble).
  sim_weights <- rep(weights, times = n_stoch) / n_stoch
  cases_median  <- matrix(NA_real_, n_locs, n_times)
  deaths_median <- matrix(NA_real_, n_locs, n_times)
  for (i in seq_len(n_locs)) {
    for (j in seq_len(n_times)) {
      cases_median[i, j]  <- MOSAIC::weighted_quantiles(as.vector(cases_array[i, j, , ]), sim_weights, 0.5)
      deaths_median[i, j] <- MOSAIC::weighted_quantiles(as.vector(deaths_array[i, j, , ]), sim_weights, 0.5)
    }
  }

  structure(
    list(
      cases_array    = cases_array,
      deaths_array   = deaths_array,
      cases_mean     = cases_median,  # simplified for tests
      cases_median   = cases_median,
      deaths_mean    = deaths_median,
      deaths_median  = deaths_median,
      ci_bounds      = list(
        cases  = list(list(lower = cases_median * 0.8, upper = cases_median * 1.2)),
        deaths = list(list(lower = deaths_median * 0.8, upper = deaths_median * 1.2))
      ),
      obs_cases      = obs_cases,
      obs_deaths     = obs_deaths,
      parameter_weights = weights,
      n_param_sets   = as.integer(n_params),
      n_simulations_per_config = as.integer(n_stoch),
      n_successful   = as.integer(n_params * n_stoch),
      location_names = paste0("LOC_", LETTERS[seq_len(n_locs)]),
      n_locations    = as.integer(n_locs),
      n_time_points  = as.integer(n_times),
      date_start     = "2023-01-01",
      date_stop      = "2023-03-01",
      envelope_quantiles = c(0.025, 0.975)
    ),
    class = "mosaic_ensemble"
  )
}

# Generate mock likelihoods: best params get highest likelihood
make_mock_likelihoods <- function(n_params = 20) {
  # Descending: param 1 is best
  -100 - seq(0, by = 0.5, length.out = n_params)
}


# ===========================================================================
# Input validation tests
# ===========================================================================

test_that("optimize_ensemble_subset rejects non-ensemble input", {
  expect_error(
    optimize_ensemble_subset(ensemble = list(), likelihoods = 1:5),
    "mosaic_ensemble"
  )
})

test_that("optimize_ensemble_subset rejects mismatched likelihood length", {
  ens <- make_mock_ensemble(n_params = 10)
  expect_error(
    optimize_ensemble_subset(ens, likelihoods = 1:5),
    "must match"
  )
})

test_that("optimize_ensemble_subset rejects non-finite likelihoods", {
  ens <- make_mock_ensemble(n_params = 10)
  lls <- make_mock_likelihoods(10)
  lls[3] <- NA
  expect_error(
    optimize_ensemble_subset(ens, likelihoods = lls),
    "finite"
  )
})

test_that("optimize_ensemble_subset soft-clamps min_n > n_params with warning", {
  ens <- make_mock_ensemble(n_params = 5)
  lls <- make_mock_likelihoods(5)
  expect_warning(
    result <- optimize_ensemble_subset(ens, lls, min_n = 10, verbose = FALSE),
    "clamping"
  )
  expect_equal(result$optimal_n, 5L)
  expect_equal(nrow(result$evaluation_table), 1L)
})


# ===========================================================================
# Core functionality tests
# ===========================================================================

test_that("optimize_ensemble_subset returns correct structure with mae", {
  ens <- make_mock_ensemble()
  lls <- make_mock_likelihoods(20)

  result <- optimize_ensemble_subset(ens, lls, min_n = 4L, objective = "mae", verbose = FALSE)

  expect_s3_class(result, "mosaic_subset_optimization")
  expect_true(is.data.frame(result$evaluation_table))
  expect_equal(nrow(result$evaluation_table), 20 - 4 + 1)  # min_n to n_params
  expect_true(result$optimal_n >= 4L && result$optimal_n <= 20L)
  expect_s3_class(result$ensemble_optimized, "mosaic_ensemble")
  expect_equal(result$ensemble_optimized$n_param_sets, result$optimal_n)
  expect_equal(result$diagnostics_n, 20L)
  expect_equal(result$objective, "mae")
  expect_true(is.logical(result$stability_flag))
})

test_that("optimize_ensemble_subset returns correct structure with r2_bias", {
  ens <- make_mock_ensemble()
  lls <- make_mock_likelihoods(20)

  result <- optimize_ensemble_subset(ens, lls, min_n = 4L, objective = "r2_bias", verbose = FALSE)

  expect_s3_class(result, "mosaic_subset_optimization")
  expect_equal(result$objective, "r2_bias")
  expect_true(all(c("r2_cases", "r2_deaths", "bias_cases", "bias_deaths") %in%
                    names(result$evaluation_table)))
})

test_that("optimize_ensemble_subset returns correct structure with wis", {
  ens <- make_mock_ensemble()
  lls <- make_mock_likelihoods(20)

  result <- optimize_ensemble_subset(ens, lls, min_n = 4L, objective = "wis", verbose = FALSE)

  expect_s3_class(result, "mosaic_subset_optimization")
  expect_equal(result$objective, "wis")
})

test_that("evaluation table has all metrics regardless of objective", {
  ens <- make_mock_ensemble()
  lls <- make_mock_likelihoods(20)

  result <- optimize_ensemble_subset(ens, lls, min_n = 4L, objective = "mae", verbose = FALSE)
  expected_cols <- c("n", "r2_cases", "r2_deaths", "bias_cases", "bias_deaths",
                     "mae_cases", "mae_deaths", "ess", "score")
  expect_true(all(expected_cols %in% names(result$evaluation_table)))
})

test_that("optimized ensemble is valid mosaic_ensemble with correct dimensions", {
  ens <- make_mock_ensemble(n_locs = 2, n_times = 10, n_params = 15, n_stoch = 3)
  lls <- make_mock_likelihoods(15)

  result <- optimize_ensemble_subset(ens, lls, min_n = 4L, objective = "mae", verbose = FALSE)
  opt_ens <- result$ensemble_optimized

  expect_equal(opt_ens$n_locations, 2L)
  expect_equal(opt_ens$n_time_points, 10L)
  expect_equal(opt_ens$n_param_sets, result$optimal_n)
  expect_equal(dim(opt_ens$cases_array)[3], result$optimal_n)
  expect_equal(dim(opt_ens$deaths_array)[3], result$optimal_n)
  expect_equal(nrow(opt_ens$cases_median), 2L)
  expect_equal(ncol(opt_ens$cases_median), 10L)
  expect_equal(length(opt_ens$parameter_weights), result$optimal_n)
  expect_true(abs(sum(opt_ens$parameter_weights) - 1.0) < 1e-10)
})


# ===========================================================================
# Sorting tests
# ===========================================================================

test_that("optimize_ensemble_subset handles unsorted likelihoods", {
  ens <- make_mock_ensemble(n_params = 10)
  lls_sorted <- make_mock_likelihoods(10)

  # Shuffle likelihoods (and verify it still works)
  shuf <- sample(10)
  lls_shuffled <- lls_sorted[shuf]

  # Need to also shuffle the arrays in the ensemble to match
  ens_shuf <- ens
  ens_shuf$cases_array  <- ens$cases_array[, , shuf, , drop = FALSE]
  ens_shuf$deaths_array <- ens$deaths_array[, , shuf, , drop = FALSE]

  result <- optimize_ensemble_subset(ens_shuf, lls_shuffled, min_n = 4L,
                                     objective = "mae", verbose = FALSE)

  expect_s3_class(result, "mosaic_subset_optimization")
  expect_true(result$optimal_n >= 4L)
})


# ===========================================================================
# Seeds round-trip tests
# ===========================================================================

test_that("optimize_ensemble_subset returns NULL optimal_seeds when seeds not supplied", {
  ens <- make_mock_ensemble(n_params = 10)
  lls <- make_mock_likelihoods(10)

  result <- optimize_ensemble_subset(ens, lls, min_n = 4L, verbose = FALSE)

  expect_null(result$optimal_seeds)
})

test_that("optimize_ensemble_subset returns aligned optimal_seeds when seeds supplied", {
  ens <- make_mock_ensemble(n_params = 10)
  lls <- make_mock_likelihoods(10)
  seeds <- 1001:1010  # seeds aligned with param sets 1..10

  result <- optimize_ensemble_subset(ens, lls, seeds = seeds,
                                     min_n = 4L, objective = "mae", verbose = FALSE)

  expect_equal(length(result$optimal_seeds), result$optimal_n)
  # Likelihoods are already sorted descending (param 1 best), so top-N seeds
  # are the first N of the input seeds vector.
  expect_equal(result$optimal_seeds, seeds[seq_len(result$optimal_n)])
})

test_that("optimize_ensemble_subset preserves seed mapping under shuffled input", {
  ens <- make_mock_ensemble(n_params = 10)
  lls_sorted <- make_mock_likelihoods(10)
  seeds_sorted <- 1001:1010

  # Shuffle all three in parallel
  set.seed(7)
  shuf <- sample(10)
  lls_shuf <- lls_sorted[shuf]
  seeds_shuf <- seeds_sorted[shuf]

  ens_shuf <- ens
  ens_shuf$cases_array  <- ens$cases_array[, , shuf, , drop = FALSE]
  ens_shuf$deaths_array <- ens$deaths_array[, , shuf, , drop = FALSE]

  result <- optimize_ensemble_subset(ens_shuf, lls_shuf, seeds = seeds_shuf,
                                     min_n = 4L, objective = "mae", verbose = FALSE)

  # Internal sort maps top-N back to the originally-best seeds, regardless of
  # the shuffled input order.
  expect_equal(sort(result$optimal_seeds),
               sort(seeds_sorted[seq_len(result$optimal_n)]))
})

test_that("optimize_ensemble_subset rejects mismatched seeds length", {
  ens <- make_mock_ensemble(n_params = 10)
  lls <- make_mock_likelihoods(10)
  expect_error(
    optimize_ensemble_subset(ens, lls, seeds = 1:5, min_n = 4L, verbose = FALSE),
    "must match"
  )
})


# ===========================================================================
# Edge case tests
# ===========================================================================

test_that("optimize_ensemble_subset works with min_n = n_params (single evaluation)", {
  ens <- make_mock_ensemble(n_params = 5)
  lls <- make_mock_likelihoods(5)

  result <- optimize_ensemble_subset(ens, lls, min_n = 5L, objective = "mae", verbose = FALSE)

  expect_equal(nrow(result$evaluation_table), 1L)
  expect_equal(result$optimal_n, 5L)
})

test_that("optimize_ensemble_subset works with equal likelihoods", {
  ens <- make_mock_ensemble(n_params = 10)
  lls <- rep(-100, 10)  # all equal

  result <- optimize_ensemble_subset(ens, lls, min_n = 4L, objective = "mae", verbose = FALSE)

  expect_s3_class(result, "mosaic_subset_optimization")
  # With equal likelihoods, all weights should be uniform at each N
  expect_true(all(is.finite(result$evaluation_table$score)))
})

test_that("stability guard selects largest N when profile is flat", {
  ens <- make_mock_ensemble(n_params = 5, n_stoch = 1)
  # Make all predictions identical so scores are flat
  for (p in 2:5) {
    ens$cases_array[, , p, ]  <- ens$cases_array[, , 1, ]
    ens$deaths_array[, , p, ] <- ens$deaths_array[, , 1, ]
  }
  lls <- make_mock_likelihoods(5)

  result <- optimize_ensemble_subset(ens, lls, min_n = 4L, objective = "mae", verbose = FALSE)

  expect_true(result$stability_flag)
  expect_equal(result$optimal_n, 5L)  # should pick largest N
})


# ===========================================================================
# Lever B: PSOCK cell-split parallelism is bit-identical to serial
# ===========================================================================

test_that("parallel (cl, stride=1) is bit-identical to serial across objectives", {
  skip_if_testthat_parallel()  # spawning a PSOCK cluster inside a testthat worker is unsafe
  skip_on_cran()
  # The worker resolves MOSAIC:::.optimize_eval_cell_block from the INSTALLED
  # namespace (library(MOSAIC) on each worker). Skip if the installed package
  # predates this refactor (e.g. running via load_all against a stale install).
  skip_if_not(isTRUE(tryCatch(
    is.function(get(".optimize_eval_cell_block", envir = asNamespace("MOSAIC"))),
    error = function(e) FALSE)),
    "installed MOSAIC lacks .optimize_eval_cell_block")

  cl <- tryCatch(parallel::makeCluster(2L, type = "PSOCK"), error = function(e) NULL)
  skip_if(is.null(cl), "could not create PSOCK cluster")
  on.exit(parallel::stopCluster(cl), add = TRUE)
  invisible(parallel::clusterEvalQ(cl, suppressMessages(library(MOSAIC))))

  ens <- make_mock_ensemble(n_locs = 3, n_times = 8, n_params = 16, n_stoch = 3)
  lls <- make_mock_likelihoods(16)
  seeds <- 2001:2016

  for (o in c("mae", "r2_bias", "wis")) {
    ser <- optimize_ensemble_subset(ens, lls, seeds = seeds, min_n = 4L,
                                    objective = o, central_method = "mean",
                                    verbose = FALSE)
    par <- optimize_ensemble_subset(ens, lls, seeds = seeds, min_n = 4L,
                                    objective = o, central_method = "mean",
                                    cl = cl, verbose = FALSE)
    expect_identical(par$optimal_n, ser$optimal_n, info = o)
    expect_identical(par$optimal_seeds, ser$optimal_seeds, info = o)
    expect_equal(par$optimal_weights, ser$optimal_weights, tolerance = 0, info = o)
    expect_equal(par$evaluation_table, ser$evaluation_table, tolerance = 0, info = o)
    eo <- par$ensemble_optimized; er <- ser$ensemble_optimized
    expect_equal(eo$cases_median,  er$cases_median,  tolerance = 0, info = o)
    expect_equal(eo$deaths_median, er$deaths_median, tolerance = 0, info = o)
    expect_equal(eo$cases_mean,    er$cases_mean,    tolerance = 0, info = o)
    expect_equal(eo$deaths_mean,   er$deaths_mean,   tolerance = 0, info = o)
    expect_equal(eo$ci_bounds,     er$ci_bounds,     tolerance = 0, info = o)
  }
})


# ===========================================================================
# Lever C: stride-then-refine selects a valid N and a subset of exhaustive N's
# ===========================================================================

test_that("stride > 1 returns valid optimal_n and a subset of the exhaustive N's", {
  ens <- make_mock_ensemble(n_locs = 2, n_times = 10, n_params = 30, n_stoch = 3)
  lls <- make_mock_likelihoods(30)
  min_n <- 4L

  exhaustive <- optimize_ensemble_subset(ens, lls, min_n = min_n,
                                         objective = "mae", verbose = FALSE)

  for (stride in c(3L, 5L)) {
    res <- optimize_ensemble_subset(ens, lls, min_n = min_n, objective = "mae",
                                    stride = stride, verbose = FALSE)
    # optimal_n is within [min_n, max_n]
    expect_true(res$optimal_n >= min_n && res$optimal_n <= 30L,
                info = paste("stride", stride))
    # evaluation_table N's are a strict subset of the exhaustive N's
    expect_true(all(res$evaluation_table$n %in% exhaustive$evaluation_table$n),
                info = paste("stride", stride))
    expect_true(nrow(res$evaluation_table) < nrow(exhaustive$evaluation_table),
                info = paste("stride", stride))
    # rows are sorted ascending by N
    expect_false(is.unsorted(res$evaluation_table$n), info = paste("stride", stride))
    # returned object is well-formed
    expect_s3_class(res, "mosaic_subset_optimization")
    expect_s3_class(res$ensemble_optimized, "mosaic_ensemble")
    expect_equal(res$ensemble_optimized$n_param_sets, res$optimal_n,
                 info = paste("stride", stride))
  }
})

test_that("stride = 1 reproduces the exhaustive search exactly", {
  ens <- make_mock_ensemble(n_params = 20)
  lls <- make_mock_likelihoods(20)
  a <- optimize_ensemble_subset(ens, lls, min_n = 4L, objective = "mae", verbose = FALSE)
  b <- optimize_ensemble_subset(ens, lls, min_n = 4L, objective = "mae",
                                stride = 1L, verbose = FALSE)
  expect_equal(a$evaluation_table, b$evaluation_table, tolerance = 0)
  expect_identical(a$optimal_n, b$optimal_n)
})


# ===========================================================================
# WIS helper tests
# ===========================================================================

test_that(".compute_wis_from_quantiles returns finite positive value", {
  set.seed(1)
  n <- 100
  obs <- rpois(n, 50)
  q50 <- obs + rnorm(n, 0, 5)
  q25 <- q50 - 10
  q75 <- q50 + 10
  q025 <- q50 - 25
  q975 <- q50 + 25

  wis <- .compute_wis_from_quantiles(obs, q025, q25, q50, q75, q975)

  expect_true(is.finite(wis))
  expect_true(wis >= 0)
})

test_that(".compute_wis_from_quantiles is zero for perfect forecasts", {
  n <- 50
  obs <- rep(100, n)
  # Perfect point forecast and intervals that exactly cover the observation
  wis <- .compute_wis_from_quantiles(obs, obs, obs, obs, obs, obs)

  expect_equal(wis, 0)
})


# ===========================================================================
# R-5: weighted-mean renormalization over surviving sims (v0.36.2)
# ===========================================================================

test_that("optimize_ensemble_subset renormalizes the weighted mean over surviving sims", {
  # One param's predictions are NA (a failed sim). The optimized-ensemble MEAN
  # must drop that term AND renormalize the surviving weights, not divide by the
  # full weight (which biased the mean toward 0). The median path already did
  # this via weighted_quantiles, so the two stats are now consistent.
  n_locs <- 1L; n_times <- 5L; n_params <- 4L; n_stoch <- 1L
  obs_cases  <- matrix(c(50, 60, 70, 80, 90), n_locs, n_times)
  obs_deaths <- matrix(c(5, 6, 7, 8, 9),     n_locs, n_times)

  cases_array  <- array(NA_real_, dim = c(n_locs, n_times, n_params, n_stoch))
  deaths_array <- array(NA_real_, dim = c(n_locs, n_times, n_params, n_stoch))
  for (p in c(1L, 2L, 4L)) {              # param 3 left all-NA (simulated failure)
    cases_array[1, , p, 1]  <- as.numeric(obs_cases)
    deaths_array[1, , p, 1] <- as.numeric(obs_deaths)
  }

  ens <- structure(list(
    cases_array = cases_array, deaths_array = deaths_array,
    cases_mean = obs_cases, cases_median = obs_cases,
    deaths_mean = obs_deaths, deaths_median = obs_deaths,
    ci_bounds = list(
      cases  = list(list(lower = obs_cases * 0.8,  upper = obs_cases * 1.2)),
      deaths = list(list(lower = obs_deaths * 0.8, upper = obs_deaths * 1.2))
    ),
    obs_cases = obs_cases, obs_deaths = obs_deaths,
    parameter_weights = rep(1 / n_params, n_params),
    n_param_sets = n_params, n_simulations_per_config = n_stoch,
    n_successful = as.integer(n_params * n_stoch),
    location_names = "LOC_A", n_locations = n_locs, n_time_points = n_times,
    date_start = "2023-01-01", date_stop = "2023-01-05",
    envelope_quantiles = c(0.025, 0.975)
  ), class = "mosaic_ensemble")

  # Equal likelihoods => uniform optimal weights; min_n = n_params => use all 4.
  result <- optimize_ensemble_subset(ens, likelihoods = rep(-100, n_params),
                                     min_n = n_params, objective = "mae", verbose = FALSE)

  # Renormalized (correct): mean == obs. The pre-fix divide-by-full-weight would
  # have produced 0.75 * obs (one of four equal weights lost to the NA).
  expect_equal(as.numeric(result$ensemble_optimized$cases_mean),  as.numeric(obs_cases))
  expect_equal(as.numeric(result$ensemble_optimized$deaths_mean), as.numeric(obs_deaths))
})
