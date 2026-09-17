# The convergence gate scores `is_best_subset` (the tier subset), but when
# subset optimization succeeds the posterior is built from the smaller
# `is_best_subset_opt`. Verified on the 100k production run: the gate passed on
# 115 draws (ESS_B 107.48, target 100) while the 60 draws actually used scored
# 53.14 -- so the run was certified on a population it does not use. These
# metrics make that gap visible. They are REPORTED, NOT GATED.

test_that("ESS on the used subset is computed from the weights the posterior uses", {
  set.seed(4)
  w <- c(rep(1, 10), rep(exp(-2), 50))      # saturated-style weights, n = 60
  wn <- w / sum(w)
  expect_equal(calc_model_ess(wn, method = "perplexity"),
               exp(-sum(wn * log(wn))), tolerance = 1e-10)
  # A smaller subset of the same shape has strictly lower ESS, which is the
  # whole point: scoring 115 and using 60 overstates the posterior's support.
  w115 <- c(rep(1, 19), rep(exp(-2), 96)); w115 <- w115 / sum(w115)
  expect_gt(calc_model_ess(w115, method = "perplexity"),
            calc_model_ess(wn,   method = "perplexity"))
})

test_that("the reported optimized metrics never alter the convergence verdict", {
  # calc_convergence_diagnostics() must not read the *_optimized keys: they are
  # patched into the JSON after it returns. If a future edit wires them into the
  # gate, this test says so.
  src <- paste(deparse(calc_convergence_diagnostics), collapse = "\n")
  for (k in c("ess_best_optimized", "A_B_optimized", "cvw_B_optimized",
              "ess_is_optimized")) {
    expect_false(grepl(k, src, fixed = TRUE), info = k)
  }
})

test_that("exact IS ESS is available for the optimized subset", {
  d <- calc_is_diagnostics(c(0, -1e6, -2e6), method = "perplexity")
  expect_lt(d$ess_is, 2)
  expect_true(is.na(d$khat) || is.numeric(d$khat))
})
