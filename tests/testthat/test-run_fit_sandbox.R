# run_fit_sandbox is tested with a stubbed engine via .sim_runner, so these
# tests exercise the sandbox's own logic without paying for a real simulation.
# The stub returns a controllable deterministic result.
#
# The catch, and why the last test in this file exists: a stub is more
# permissive than the real runner. This file's stubs used to take
# `visualize`/`pdf`/`outdir` because run_fit_sandbox() passed them -- and when
# those arguments were removed with the Python engine, run_simulation() started
# raising on them while every test here kept passing, because the stubs still
# accepted them. The production call was broken and invisible. So: assert the
# call shape against the real runner's formals.

make_test_config <- function() {
  d0 <- as.Date("2021-01-01"); d1 <- as.Date("2022-12-31")
  dts <- seq(d0, d1, by = "day"); doy <- as.integer(format(dts, "%j"))
  seas <- 40 * exp(-((doy - 80)^2) / 1500) + 25 * exp(-((doy - 290)^2) / 1200) + 1
  list(
    date_start = as.character(d0), date_stop = as.character(d1),
    location_name = "TST",
    reported_cases  = matrix(round(seas), nrow = 1),
    reported_deaths = matrix(round(seas * 0.01), nrow = 1),
    beta_j0_hum = 3e-6, beta_j0_env = 2e-6,
    mu_jt = 0.02,
    rho_deaths = 0.6, rho = 0.2, gamma_1 = 0.1,
    chi_endemic = 0.5, chi_epidemic = 0.9, epidemic_threshold = 30,
    .seas = seas
  )
}

# Stub: predicted cases = beta-scaled seasonal curve; deaths ~ matched.
stub_runner <- function(config, seed, quiet, ...) {
  mult <- if (!is.null(config$beta_j0_hum)) config$beta_j0_hum / 3e-6 else 1
  seas <- config$.seas
  list(results = list(
    reported_cases  = matrix(mult * seas, nrow = 1),
    reported_deaths = matrix(0.01 * seas, nrow = 1)
  ))
}

test_that("overrides are applied and unknown parameters warn and are skipped", {
  cfg <- make_test_config()
  expect_warning(
    res <- run_fit_sandbox(cfg, params = list(beta_j0_hum = 1.5e-6, NOT_A_PARAM = 1),
                           .sim_runner = stub_runner),
    "NOT_A_PARAM"
  )
  expect_true("beta_j0_hum" %in% res$params_applied$parameter)
  expect_false("NOT_A_PARAM" %in% res$params_applied$parameter)
  expect_equal(res$params_applied$new[res$params_applied$parameter == "beta_j0_hum"], 1.5e-6)
})

test_that("predictions use the standard ensemble format and both metrics", {
  cfg <- make_test_config()
  res <- run_fit_sandbox(cfg, .sim_runner = stub_runner)
  expect_setequal(
    colnames(res$predictions),
    c("location", "date", "metric", "observed", "predicted_central",
      "predicted_mean", "predicted_median", "central_method",
      "ci_1_lower", "ci_1_upper", "ci_2_lower", "ci_2_upper")
  )
  expect_setequal(unique(res$predictions$metric), c("Suspected Cases", "Deaths"))
})

# Since v0.96.0 the reported CFR is a model input (config$mu_jt), so the sandbox
# reports it directly instead of backing it out of a hazard: `reported` is its
# mean over the selected locations and days, and `symptomatic` is the per-onset
# fatality probability the engine uses, reported * rho / (rho_deaths * chi_epidemic).
# It reads mu_jt through the engine's own resolver, so a legacy config gets the
# same treatment the engine gives it.
test_that("implied CFR is the config's reported CFR and the engine's per-onset probability", {
  cfg <- make_test_config()
  res <- run_fit_sandbox(cfg, .sim_runner = stub_runner)
  expect_named(res$metrics$cfr_implied, c("reported", "symptomatic"))
  expect_equal(unname(res$metrics$cfr_implied["reported"]), 0.02, tolerance = 1e-12)
  expect_equal(unname(res$metrics$cfr_implied["symptomatic"]),
               0.02 * cfg$rho / (cfg$rho_deaths * cfg$chi_epidemic), tolerance = 1e-12)
})

test_that("implied CFR averages mu_jt over the selected locations and days only", {
  cfg <- make_test_config()
  nT <- ncol(cfg$reported_cases)
  cfg$location_name <- c("TST", "TS2", "TS3")
  cfg$mu_jt <- rbind(rep(0.01, nT), c(rep(0.02, nT / 2), rep(0.04, nT / 2)), rep(0.05, nT))
  seas <- cfg$.seas
  cfg$reported_cases  <- matrix(rep(round(seas), each = 3), nrow = 3)
  cfg$reported_deaths <- matrix(rep(round(seas * 0.01), each = 3), nrow = 3)
  runner3 <- function(config, seed, quiet) list(results = list(
    reported_cases  = matrix(rep(seas, each = 3), nrow = 3),
    reported_deaths = matrix(rep(0.01 * seas, each = 3), nrow = 3)))

  res_all <- run_fit_sandbox(cfg, .sim_runner = runner3)
  expect_equal(unname(res_all$metrics$cfr_implied["reported"]), mean(cfg$mu_jt), tolerance = 1e-12)
  res_one <- run_fit_sandbox(cfg, locations = 2L, .sim_runner = runner3)
  expect_equal(unname(res_one$metrics$cfr_implied["reported"]), 0.03, tolerance = 1e-12)
})

test_that("implied CFR weights mu_jt by the observed cases, as an observed CFR does", {
  cfg <- make_test_config()
  nT <- ncol(cfg$reported_cases)
  cfg$mu_jt <- matrix(c(rep(0.01, nT / 2), rep(0.05, nT / 2)), nrow = 1)
  cfg$reported_cases <- matrix(c(rep(1, nT / 2), rep(9, nT / 2)), nrow = 1)
  res <- run_fit_sandbox(cfg, .sim_runner = stub_runner)
  expect_equal(unname(res$metrics$cfr_implied["reported"]), (0.01 * 1 + 0.05 * 9) / 10, tolerance = 1e-12)
  # Days with no observed cases (the forecast tail) carry no weight.
  cfg$reported_cases[1, (nT / 2 + 1):nT] <- NA
  res2 <- run_fit_sandbox(cfg, .sim_runner = stub_runner)
  expect_equal(unname(res2$metrics$cfr_implied["reported"]), 0.01, tolerance = 1e-12)
})

test_that("implied CFR is NA when a reporting parameter or mu_jt is missing", {
  cfg <- make_test_config(); cfg$chi_epidemic <- NULL
  res <- run_fit_sandbox(cfg, .sim_runner = stub_runner)
  expect_true(all(is.na(res$metrics$cfr_implied)))
  expect_named(res$metrics$cfr_implied, c("reported", "symptomatic"))
  cfg <- make_test_config(); cfg$mu_jt <- NULL
  res <- run_fit_sandbox(cfg, .sim_runner = stub_runner)
  expect_true(all(is.na(res$metrics$cfr_implied)))
})

test_that("merged scorecard exposes the five dimensions", {
  cfg <- make_test_config()
  res <- run_fit_sandbox(cfg, .sim_runner = stub_runner)
  expect_setequal(names(res$metrics$scorecard),
                  c("bias_cases", "bias_deaths", "peak_timing", "peak_shape", "variance"))
  # baseline stub is unbiased on cases -> PASS
  expect_equal(unname(res$metrics$scorecard["bias_cases"]), "PASS")
})

test_that("halving beta scales predicted cases bias toward 0.5x", {
  cfg <- make_test_config()
  res <- run_fit_sandbox(cfg, params = list(beta_j0_hum = 1.5e-6), .sim_runner = stub_runner)
  expect_equal(res$metrics$bias_cases, 0.5, tolerance = 0.02)
})

test_that("scalar override of a per-location (vector) parameter broadcasts to field length", {
  cfg <- make_test_config()
  # make beta_j0_hum a length-3 per-location vector and a 3-row observed matrix
  cfg$beta_j0_hum <- rep(3e-6, 3)
  seas <- cfg$.seas
  cfg$reported_cases  <- matrix(rep(round(seas), each = 3), nrow = 3)
  cfg$reported_deaths <- matrix(rep(round(seas * 0.01), each = 3), nrow = 3)
  seen_len <- NULL
  capture_runner <- function(config, seed, quiet) {
    seen_len <<- length(config$beta_j0_hum)
    nd <- length(seas)
    list(results = list(reported_cases  = matrix(rep(seas, each = 3), nrow = 3),
                        reported_deaths = matrix(rep(0.01 * seas, each = 3), nrow = 3)))
  }
  res <- run_fit_sandbox(cfg, params = list(beta_j0_hum = 1.5e-6), .sim_runner = capture_runner)
  expect_equal(seen_len, 3)                          # scalar broadcast to length 3
  expect_equal(res$params_applied$new[res$params_applied$parameter == "beta_j0_hum"], 1.5e-6)
})

test_that("outdir writes predictions CSV and metrics JSON", {
  cfg <- make_test_config()
  tmp <- file.path(tempdir(), "fit_sbx_test")
  unlink(tmp, recursive = TRUE)
  run_fit_sandbox(cfg, outdir = tmp, run_label = "lbl", .sim_runner = stub_runner)
  expect_true(file.exists(file.path(tmp, "lbl", "predictions_ensemble.csv")))
  expect_true(file.exists(file.path(tmp, "lbl", "metrics.json")))
})

test_that("the sandbox calls its runner with arguments run_simulation() accepts", {
  cfg <- make_test_config()
  seen <- NULL
  arg_runner <- function(...) {
    seen <<- names(list(...))
    stub_runner(...)
  }
  run_fit_sandbox(cfg, .sim_runner = arg_runner)

  expect_true(length(seen) > 0L)
  expect_true(all(nzchar(seen)))            # every argument passed by name
  expect_setequal(setdiff(seen, names(formals(run_simulation))), character(0))

  # And the default runner really is run_simulation(), so the check above is about
  # the function production uses rather than an unrelated signature.
  expect_identical(formals(run_fit_sandbox)$.sim_runner, quote(run_simulation))
})

test_that("a scalar mu_jt override keeps the matrix shape, and retired mortality overrides warn", {
  cfg <- make_test_config()
  nT <- ncol(cfg$reported_cases)
  cfg$location_name <- c("TST", "TS2")
  cfg$mu_jt <- matrix(0.02, 2, nT)
  cfg$reported_cases <- matrix(rep(round(cfg$.seas), each = 2), nrow = 2)
  cfg$reported_deaths <- matrix(rep(round(cfg$.seas * 0.01), each = 2), nrow = 2)
  seen <- NULL
  runner2 <- function(config, seed, quiet) {
    seen <<- config$mu_jt
    list(results = list(reported_cases = matrix(rep(cfg$.seas, each = 2), nrow = 2),
                        reported_deaths = matrix(rep(0.01 * cfg$.seas, each = 2), nrow = 2)))
  }
  res <- run_fit_sandbox(cfg, params = list(mu_jt = 0.05), .sim_runner = runner2)
  expect_identical(dim(seen), c(2L, nT))
  expect_true(all(seen == 0.05))
  expect_warning(run_fit_sandbox(cfg, params = list(mu_j_baseline = 0.1), .sim_runner = runner2),
                 "removed from the model in v0.96.0")
})
