# Tests for calc_model_ess_parameter(): the function-specific ESS (Owen 2013)

# Helper: create mock results data frame
make_mock_results <- function(n = 500, n_params = 3, seed = 42) {
  set.seed(seed)
  df <- data.frame(
    sim = 1:n,
    likelihood = rnorm(n, mean = -100, sd = 20)
  )
  for (i in seq_len(n_params)) {
    df[[paste0("param_", i)]] <- rnorm(n)
  }
  # Add required columns for estimated_parameters lookup
  df$beta_j0_tot_MOZ <- rnorm(n, 0.001, 0.0005)
  df$psi_star_a_MOZ <- rnorm(n, 1.0, 0.3)
  df$gamma_1 <- runif(n, 0.05, 0.15)
  df
}

# Helper: get param names that pass the estimated_parameters filter
get_valid_params <- function() {
  c("beta_j0_tot_MOZ", "psi_star_a_MOZ", "gamma_1")
}

test_that("returns one row per parameter with the documented columns", {
  df <- make_mock_results()
  params <- get_valid_params()
  result <- MOSAIC::calc_model_ess_parameter(df, params, verbose = FALSE)
  expect_true(is.data.frame(result))
  expect_equal(names(result), c("parameter", "type", "iso_code", "ess_marginal"))
  expect_equal(nrow(result), length(params))
  expect_true(all(is.finite(result$ess_marginal)))
  expect_true(all(result$ess_marginal >= 1 & result$ess_marginal <= nrow(df)))
  expect_identical(result$type, c("location", "location", "global"))
  expect_identical(result$iso_code, c("MOZ", "MOZ", NA_character_))
})

test_that("ESS falls as the weights concentrate", {
  df <- make_mock_results(n = 500)
  params <- get_valid_params()
  df_uniform <- df
  df_uniform$likelihood <- rnorm(500, -100, 1)
  df_conc <- df
  df_conc$likelihood <- rep(-200, 500)
  df_conc$likelihood[1] <- -50
  ess_uniform <- MOSAIC::calc_model_ess_parameter(df_uniform, params)
  ess_conc <- MOSAIC::calc_model_ess_parameter(df_conc, params)
  expect_true(all(ess_conc$ess_marginal < ess_uniform$ess_marginal))
  expect_equal(ess_conc$ess_marginal, rep(1, 3))
})

test_that("the retired grid-estimate arguments warn and are ignored; others error", {
  df <- make_mock_results()
  params <- get_valid_params()
  ref <- MOSAIC::calc_model_ess_parameter(df, params)
  expect_warning(r <- MOSAIC::calc_model_ess_parameter(df, params, marginal_method = "kde",
                                                       n_grid = 230, method = "perplexity"),
                 "defunct since MOSAIC 1.3.0")
  expect_identical(r, ref)
  expect_error(MOSAIC::calc_model_ess_parameter(df, params, bogus = 1), "unused argument")
})

# ---- ess_marginal: function-specific ESS (Owen 2013, section 9.3) ------------

test_that("ess_marginal is 1 for a point mass and N for uniform weights", {
  set.seed(11)
  n <- 3000
  df <- data.frame(gamma_1 = runif(n, 0.05, 1), psi_star_a_MOZ = rnorm(n),
                   likelihood = c(0, rep(-1e6, n - 1)))
  pm <- MOSAIC::calc_model_ess_parameter(df, c("gamma_1", "psi_star_a_MOZ"))
  expect_equal(pm$ess_marginal, c(1, 1))
  df$likelihood <- 0
  un <- MOSAIC::calc_model_ess_parameter(df, c("gamma_1", "psi_star_a_MOZ"))
  expect_equal(un$ess_marginal, c(n, n))
})

test_that("ess_marginal matches the hand-computed variance ratio and its Kish bound", {
  # x = (-1, 1, 0, 0), w = (.4, .4, .1, .1): mu = 0, sum w d^2 = 0.8, sum w^2 d^2 = 0.32
  # -> 2.5, below the Kish ESS 1 / 0.34 = 2.94 (heavy weights in the tails).
  expect_equal(MOSAIC:::.mosaic_function_specific_ess(c(-1, 1, 0, 0), c(.4, .4, .1, .1)), 2.5,
               tolerance = 1e-12)
  # Two draws 0.99 / 0.01: the unbounded ratio is 1 / (2 * 0.99 * 0.01) = 50.5;
  # the Kish bound gives 1 / (0.99^2 + 0.01^2).
  expect_equal(MOSAIC:::.mosaic_function_specific_ess(c(0, 1), c(.99, .01)),
               1 / (0.99^2 + 0.01^2), tolerance = 1e-12)
  # A near point mass whose runner-up weights are positive but tiny stays ~1.
  set.seed(14)
  expect_lt(MOSAIC:::.mosaic_function_specific_ess(runif(5000), c(1, rep(1e-15, 4999))), 1.0001)
  # zero weighted spread falls back to the Kish ESS of the weights
  expect_equal(MOSAIC:::.mosaic_function_specific_ess(rep(5, 4), c(0.4, 0.3, 0.2, 0.1)),
               1 / sum(c(0.4, 0.3, 0.2, 0.1)^2), tolerance = 1e-12)
})

test_that("ess_marginal falls monotonically as tempered weights concentrate", {
  set.seed(12)
  n <- 5000
  th <- runif(n)
  base <- -(th - 0.4)^2
  ess <- vapply(c(10, 100, 1000, 1e4, 1e5), function(s) {
    df <- data.frame(gamma_1 = th, likelihood = s * base)
    MOSAIC::calc_model_ess_parameter(df, "gamma_1")$ess_marginal
  }, numeric(1))
  expect_true(all(diff(ess) < 0))
  expect_gt(ess[1], 0.5 * n)
  expect_lt(ess[5], 150)
})

test_that("ess_marginal uses supplied weights", {
  set.seed(13)
  n <- 2000
  df <- data.frame(gamma_1 = runif(n), likelihood = c(0, rep(-1e6, n - 1)))
  a <- MOSAIC::calc_model_ess_parameter(df, "gamma_1")
  b <- MOSAIC::calc_model_ess_parameter(df, "gamma_1", weights = rep(1, n))
  expect_equal(a$ess_marginal, 1)
  expect_equal(b$ess_marginal, n)
  expect_error(MOSAIC::calc_model_ess_parameter(df, "gamma_1", weights = rep(1, 3)),
               "one entry per row")
})
