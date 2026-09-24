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
    mu_j_baseline = 0.02, mu_j_epidemic_factor = 0.5,
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
    c("location", "date", "metric", "observed", "predicted_median",
      "ci_1_lower", "ci_1_upper", "ci_2_lower", "ci_2_upper")
  )
  expect_setequal(unique(res$predictions$metric), c("Suspected Cases", "Deaths"))
})

# The sandbox must not carry its own copy of the implied-CFR algebra. Until
# v0.93.0 .fit_cfr_implied() was the PRE-v0.88.0 identity: no (1 - exp(-gamma_1))
# dwell divisor (~10.5x understatement at gamma_1 = 0.1) and the retired
# 0.5*(chi_endemic + chi_epidemic) blend. These three tests pin the delegation
# itself, not a restatement of the formula -- a restatement is exactly what
# drifted last time (CLAUDE.md Lesson #11).
test_that("implied CFR delegates to .mosaic_add_implied_cfr_columns, not a local copy", {
  cfg <- make_test_config()
  res <- run_fit_sandbox(cfg, .sim_runner = stub_runner)

  canonical <- MOSAIC:::.mosaic_add_implied_cfr_columns(
    data.frame(rho = cfg$rho, rho_deaths = cfg$rho_deaths,
               chi_endemic = cfg$chi_endemic, chi_epidemic = cfg$chi_epidemic,
               gamma_1 = cfg$gamma_1,
               mu_j_baseline_L1 = cfg$mu_j_baseline,
               mu_j_epidemic_factor_L1 = cfg$mu_j_epidemic_factor),
    iso_codes = "L1", verbose = FALSE
  )

  expect_named(res$metrics$cfr_implied, c("baseline", "epidemic"))
  expect_equal(unname(res$metrics$cfr_implied["baseline"]),
               canonical$cfr_baseline_L1, tolerance = 1e-12)
  expect_equal(unname(res$metrics$cfr_implied["epidemic"]),
               canonical$cfr_epidemic_L1, tolerance = 1e-12)
})

test_that("implied CFR carries the incidence-dwell divisor the old local copy omitted", {
  cfg <- make_test_config()
  res <- run_fit_sandbox(cfg, .sim_runner = stub_runner)

  dwell <- 1 - exp(-cfg$gamma_1)
  expect_equal(unname(res$metrics$cfr_implied["baseline"]),
               cfg$mu_j_baseline * cfg$rho_deaths * cfg$chi_endemic / (cfg$rho * dwell),
               tolerance = 1e-12)

  # And it is NOT the retired blend-without-dwell value. At gamma_1 = 0.1 the old
  # number was ~10.5x too small; assert the gap rather than just the new value, so
  # a silent reversion cannot pass.
  old_wrong <- cfg$mu_j_baseline * cfg$rho_deaths *
    (0.5 * (cfg$chi_endemic + cfg$chi_epidemic)) / cfg$rho
  expect_gt(unname(res$metrics$cfr_implied["baseline"]) / old_wrong, 5)
})

test_that("implied CFR averages mu over the selected locations only", {
  cfg <- make_test_config()
  cfg$mu_j_baseline <- c(0.01, 0.03, 0.05)
  cfg$mu_j_epidemic_factor <- c(0, 0, 0)
  seas <- cfg$.seas
  cfg$reported_cases  <- matrix(rep(round(seas), each = 3), nrow = 3)
  cfg$reported_deaths <- matrix(rep(round(seas * 0.01), each = 3), nrow = 3)
  runner3 <- function(config, seed, quiet) list(results = list(
    reported_cases  = matrix(rep(seas, each = 3), nrow = 3),
    reported_deaths = matrix(rep(0.01 * seas, each = 3), nrow = 3)))

  dwell <- 1 - exp(-cfg$gamma_1)
  cfr_of <- function(mu) mu * cfg$rho_deaths * cfg$chi_endemic / (cfg$rho * dwell)

  res_all <- run_fit_sandbox(cfg, .sim_runner = runner3)
  expect_equal(unname(res_all$metrics$cfr_implied["baseline"]),
               mean(cfr_of(c(0.01, 0.03, 0.05))), tolerance = 1e-12)

  res_one <- run_fit_sandbox(cfg, locations = 2L, .sim_runner = runner3)
  expect_equal(unname(res_one$metrics$cfr_implied["baseline"]),
               cfr_of(0.03), tolerance = 1e-12)
})

test_that("implied CFR is NA when gamma_1 is absent, never the dwell-free value", {
  cfg <- make_test_config()
  cfg$gamma_1 <- NULL
  res <- run_fit_sandbox(cfg, .sim_runner = stub_runner)
  expect_true(all(is.na(res$metrics$cfr_implied)))
  expect_named(res$metrics$cfr_implied, c("baseline", "epidemic"))
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
