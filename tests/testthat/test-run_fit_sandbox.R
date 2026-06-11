# run_fit_sandbox is tested with a stubbed engine via .laser_runner so no Python /
# laser-cholera is required. The stub returns a controllable deterministic result.

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
    mu_j_baseline = 0.02, rho_deaths = 0.6, rho = 0.2,
    chi_endemic = 0.5, chi_epidemic = 0.9, epidemic_threshold = 30,
    .seas = seas
  )
}

# Stub: predicted cases = beta-scaled seasonal curve; deaths ~ matched.
stub_runner <- function(config, seed, quiet, visualize, pdf, outdir) {
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
                           .laser_runner = stub_runner),
    "NOT_A_PARAM"
  )
  expect_true("beta_j0_hum" %in% res$params_applied$parameter)
  expect_false("NOT_A_PARAM" %in% res$params_applied$parameter)
  expect_equal(res$params_applied$new[res$params_applied$parameter == "beta_j0_hum"], 1.5e-6)
})

test_that("predictions use the standard ensemble format and both metrics", {
  cfg <- make_test_config()
  res <- run_fit_sandbox(cfg, .laser_runner = stub_runner)
  expect_setequal(
    colnames(res$predictions),
    c("location", "date", "metric", "observed", "predicted_median",
      "ci_1_lower", "ci_1_upper", "ci_2_lower", "ci_2_upper")
  )
  expect_setequal(unique(res$predictions$metric), c("Suspected Cases", "Deaths"))
})

test_that("implied CFR follows mu * rho_deaths * chi / rho", {
  cfg <- make_test_config()
  res <- run_fit_sandbox(cfg, .laser_runner = stub_runner)
  chi <- 0.5 * (cfg$chi_endemic + cfg$chi_epidemic)
  expect_equal(res$metrics$cfr_implied,
               cfg$mu_j_baseline * cfg$rho_deaths * chi / cfg$rho, tolerance = 1e-8)
})

test_that("merged scorecard exposes the five dimensions", {
  cfg <- make_test_config()
  res <- run_fit_sandbox(cfg, .laser_runner = stub_runner)
  expect_setequal(names(res$metrics$scorecard),
                  c("bias_cases", "bias_deaths", "peak_timing", "peak_shape", "variance"))
  # baseline stub is unbiased on cases -> PASS
  expect_equal(unname(res$metrics$scorecard["bias_cases"]), "PASS")
})

test_that("halving beta scales predicted cases bias toward 0.5x", {
  cfg <- make_test_config()
  res <- run_fit_sandbox(cfg, params = list(beta_j0_hum = 1.5e-6), .laser_runner = stub_runner)
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
  capture_runner <- function(config, seed, quiet, visualize, pdf, outdir) {
    seen_len <<- length(config$beta_j0_hum)
    nd <- length(seas)
    list(results = list(reported_cases  = matrix(rep(seas, each = 3), nrow = 3),
                        reported_deaths = matrix(rep(0.01 * seas, each = 3), nrow = 3)))
  }
  res <- run_fit_sandbox(cfg, params = list(beta_j0_hum = 1.5e-6), .laser_runner = capture_runner)
  expect_equal(seen_len, 3)                          # scalar broadcast to length 3
  expect_equal(res$params_applied$new[res$params_applied$parameter == "beta_j0_hum"], 1.5e-6)
})

test_that("outdir writes predictions CSV and metrics JSON", {
  cfg <- make_test_config()
  tmp <- file.path(tempdir(), "fit_sbx_test")
  unlink(tmp, recursive = TRUE)
  run_fit_sandbox(cfg, outdir = tmp, run_label = "lbl", .laser_runner = stub_runner)
  expect_true(file.exists(file.path(tmp, "lbl", "predictions_ensemble.csv")))
  expect_true(file.exists(file.path(tmp, "lbl", "metrics.json")))
})
