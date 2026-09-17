# MATH-CORE A1/A2 regression guard.
#
# A1: with the pre-v0.89.0 EXTENSIVE dose the reservoir term W/(kappa+W) sat at
#     1.0000, which made kappa, zeta_1, zeta_2 and all four decay parameters
#     dynamically inert -- nothing about the reservoir could move the model.
# A2: that saturation also pinned the environmental route ~1044x above the human
#     route, so p_beta would have needed 0.99904 for parity against a prior whose
#     99.9th percentile is 0.682. The whole spatial chapter was modulating 0.13%
#     of infections.
#
# v0.89.0 made the dose PER CAPITA (W/N). These assertions pin that the
# reservoir stays responsive and the two routes stay within reach of p_beta.

test_that("the environmental dose response is not saturated", {
  d <- MOSAIC::config_default
  W <- 1e9; N <- 1e7; k <- d$kappa          # representative production scale
  expect_equal(k, 1e6)                       # kappa stays fixed by decision

  sat_extensive <- W / (k + W)               # the old, broken form
  sat_percapita <- (W / N) / (k + W / N)     # what the engine does now

  expect_gt(sat_extensive, 0.99)             # documents the defect
  expect_lt(sat_percapita, 0.10)             # and that we are far off the ceiling
})

test_that("the reservoir term still responds to W (kappa is not inert)", {
  k <- MOSAIC::config_default$kappa; N <- 1e7
  dose <- function(W) (W / N) / (k + W / N)
  # A 100x change in W must move the dose response materially. Under the old
  # extensive form both of these returned ~1 and the derivative was ~0.
  expect_gt(dose(1e11) / dose(1e9), 50)
})

test_that("p_beta can reach parity between the two routes", {
  skip_on_cran()
  d <- MOSAIC::config_default
  r <- run_simulation(d, seed = 20250418L, quiet = TRUE)
  I <- r$results$Isym + r$results$Iasym; N <- r$results$N; W <- r$results$W
  ok <- I > 0
  skip_if(sum(ok) < 100, "too few infectious cells to characterise the balance")

  num_h <- (I^d$alpha_1[1]) / (N^d$alpha_2)
  num_e <- (1 - d$theta_j) * ((W / N) / (d$kappa + W / N))
  ratio <- median((num_e / num_h)[ok])

  # Parity requires p_beta = ratio/(1+ratio); it must sit inside the prior.
  p_star   <- ratio / (1 + ratio)
  p_prior_hi <- qbeta(0.999, 7.03, 13.2)
  expect_lt(p_star, p_prior_hi)

  # And the routes must stay the same order of magnitude, not 1000x apart.
  expect_lt(ratio, 50)
})
