# The config validator, the model spec and the engine must agree on alpha_2.
# Before v0.90.1 they did not: make_simulation_config() and
# MOSAIC-docs/04-model-description.Rmd both call alpha_2 = 0 legal
# (density-dependent transmission, N^0 = 1), but the engine validated it with
# `positive = TRUE` and rejected it -- while accepting alpha_2 > 1, which the
# config validator forbids. Both directions are pinned here.

test_that("the engine accepts the full documented alpha_2 range, including 0", {
  for (v in c(0, 0.5, 1)) {
    expect_silent(MOSAIC:::.sim_scalar(v, "alpha_2", lower = 0, upper = 1))
  }
})

test_that("the engine rejects alpha_2 outside [0, 1], as the config validator does", {
  expect_error(MOSAIC:::.sim_scalar(5, "alpha_2", lower = 0, upper = 1))
  expect_error(MOSAIC:::.sim_scalar(-1, "alpha_2", lower = 0, upper = 1))
})

test_that("alpha_2 = 0 survives a real simulation and is density-dependent", {
  fx     <- readRDS(test_path("fixtures", "replay_full_pipeline.rds"))
  cfg    <- fx$meta$config_list
  comps  <- as.character(fx$meta$components)

  cfg0 <- cfg; cfg0$alpha_2 <- 0
  expect_no_error(
    out0 <- run_simulation(config = cfg0, seed = 42L, components = comps, quiet = TRUE)
  )
  # N^0 = 1, so the FOI denominator vanishes and transmission is strictly
  # higher than the frequency-dependent case on the same seed.
  cfg1 <- cfg; cfg1$alpha_2 <- 1
  out1 <- run_simulation(config = cfg1, seed = 42L, components = comps, quiet = TRUE)
  expect_gt(sum(out0$results$incidence), sum(out1$results$incidence))
})

test_that("config validator and engine agree on the alpha_2 boundary", {
  # make_simulation_config()'s documented contract is "[0, 1]", the same range
  # the engine enforces above. Exercised behaviourally so a reworded message
  # cannot turn this into a skip.
  args <- MOSAIC::config_default
  args[c("metadata", "zeta_ratio", "decay_days_spread", "reported_cases_weight",
         "reported_deaths_weight", "reported_tier", "output_file_path")] <- NULL
  build <- function(v) { a <- args; a$alpha_2 <- v; do.call(MOSAIC::make_simulation_config, a) }
  for (v in c(0, 1)) {
    expect_no_error(cfg <- build(v))
    expect_identical(cfg$alpha_2, v)
    expect_silent(MOSAIC:::.sim_scalar(v, "alpha_2", lower = 0, upper = 1))
  }
  for (v in c(-0.01, 1.01)) {
    expect_error(build(v), "alpha_2")
    expect_error(MOSAIC:::.sim_scalar(v, "alpha_2", lower = 0, upper = 1))
  }
})
