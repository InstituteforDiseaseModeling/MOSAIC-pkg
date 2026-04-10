# Tests for calc_model_ess_parameter() — three marginal methods

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

test_that("all three methods return same output format", {
  df <- make_mock_results()
  params <- get_valid_params()

  for (mm in c("binned", "kde")) {
    result <- MOSAIC::calc_model_ess_parameter(
      df, params, marginal_method = mm, verbose = FALSE)
    expect_true(is.data.frame(result))
    expect_equal(names(result), c("parameter", "type", "iso_code", "ess_marginal"))
    expect_equal(nrow(result), length(params))
    expect_true(all(is.finite(result$ess_marginal)))
  }
})

test_that("binned method returns finite ESS", {
  df <- make_mock_results()
  result <- MOSAIC::calc_model_ess_parameter(
    df, get_valid_params(), marginal_method = "binned", verbose = FALSE)
  expect_true(all(result$ess_marginal > 0))
  expect_true(all(result$ess_marginal <= nrow(df)))
})

test_that("binned method is sensitive to weight concentration", {
  df <- make_mock_results(n = 500)
  params <- get_valid_params()

  # Uniform-ish weights (all likelihoods similar)
  df_uniform <- df
  df_uniform$likelihood <- rnorm(500, -100, 1)  # tight spread

  # Concentrated weights (one sim dominates)
  df_conc <- df
  df_conc$likelihood <- rep(-200, 500)
  df_conc$likelihood[1] <- -50  # one dominant sim

  ess_uniform <- MOSAIC::calc_model_ess_parameter(
    df_uniform, params, marginal_method = "binned", verbose = FALSE)
  ess_conc <- MOSAIC::calc_model_ess_parameter(
    df_conc, params, marginal_method = "binned", verbose = FALSE)

  # Concentrated weights should produce lower ESS
  expect_true(mean(ess_conc$ess_marginal) < mean(ess_uniform$ess_marginal))
})

test_that("method='kish' and method='perplexity' both work with all marginal methods", {
  df <- make_mock_results()
  params <- get_valid_params()

  for (mm in c("binned", "kde")) {
    for (m in c("kish", "perplexity")) {
      result <- MOSAIC::calc_model_ess_parameter(
        df, params, method = m, marginal_method = mm, verbose = FALSE)
      expect_true(all(is.finite(result$ess_marginal)),
        info = sprintf("method=%s, marginal_method=%s", m, mm))
    }
  }
})

test_that("default marginal_method is binned", {
  df <- make_mock_results()
  params <- get_valid_params()

  # Without specifying marginal_method
  result_default <- MOSAIC::calc_model_ess_parameter(df, params, verbose = FALSE)
  # Explicitly binned
  result_binned <- MOSAIC::calc_model_ess_parameter(
    df, params, marginal_method = "binned", verbose = FALSE)

  expect_equal(result_default$ess_marginal, result_binned$ess_marginal)
})
