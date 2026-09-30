# calc_model_ess(): pins the values quoted in its @examples so the
# documentation cannot drift from the code again (the perplexity example once
# said ~1.5; the true value is exp(entropy) = 1.7425).

test_that("calc_model_ess matches its documented example values", {
  w_uniform <- rep(1 / 10, 10)
  expect_equal(calc_model_ess(w_uniform, method = "kish"), 10)
  expect_equal(calc_model_ess(w_uniform, method = "perplexity"), 10)

  w_conc <- c(0.9, rep(0.01, 10))
  # Kish: 1 / sum(w^2) = 1 / (0.81 + 10 * 1e-4) = 1.2330
  expect_equal(calc_model_ess(w_conc, method = "kish"), 1 / (0.81 + 10 * 1e-4))
  expect_equal(round(calc_model_ess(w_conc, method = "kish"), 2), 1.23)
  # Perplexity: exp(-sum(w log w)) = 1.7425
  expect_equal(calc_model_ess(w_conc, method = "perplexity"),
               exp(-(0.9 * log(0.9) + 10 * 0.01 * log(0.01))))
  expect_equal(round(calc_model_ess(w_conc, method = "perplexity"), 2), 1.74)
})
