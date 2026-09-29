# Run inputs (1_inputs/config.json, priors.json) must round-trip every double
# exactly: post-hoc reconstructions (the R_eff re-simulation) rebuild members
# from these files, and a ~1e-15 change is amplified by the engine's integer
# rounding and binomial draws into a different trajectory. jsonlite's
# digits = NA writes 15 significant digits, which does not round-trip.

test_that(".mosaic_write_json round-trips doubles exactly", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  obj <- list(a = 0.1 + 0.2, b = c(1 / 3, 2 / 3, pi * 1e-9, 4.7e-9 + 1e-24),
              m = matrix(c(1 / 7, exp(1), sqrt(2), 1e-300), 2, 2),
              i = 5L, s = "ETH")
  MOSAIC:::.mosaic_write_json(obj, path, io = NULL)
  back <- jsonlite::fromJSON(path)
  expect_identical(back$a, obj$a)
  expect_identical(back$b, obj$b)
  expect_identical(back$m, obj$m)
  expect_identical(back$i, obj$i)
  expect_identical(back$s, obj$s)
  # The old 15-digit writer does not, which is what this guards against.
  legacy <- jsonlite::fromJSON(jsonlite::toJSON(obj, auto_unbox = TRUE, digits = NA))
  expect_false(identical(legacy$a, obj$a))
})

test_that("config_default numeric fields survive the run-input writer exactly", {
  cfg <- MOSAIC::config_default
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  MOSAIC:::.mosaic_write_json(cfg, path, io = NULL)
  back <- jsonlite::fromJSON(path)
  num <- names(cfg)[vapply(cfg, function(x) is.double(x) && length(x) > 0, logical(1))]
  expect_gt(length(num), 10L)
  for (nm in num)
    expect_identical(as.numeric(back[[nm]]), as.numeric(cfg[[nm]]), info = nm)
})

test_that("resume accepts 1_inputs written at either precision, and rejects changes", {
  base <- tempfile("json_prec_"); inp <- file.path(base, "1_inputs")
  dir.create(inp, recursive = TRUE)
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  dirs <- list(inputs = inp)
  priors <- list(zeta_2 = list(shape = 1 / 3, rate = 0.1 + 0.2))
  config <- list(location_name = "ETH", beta_j0_env = c(1 / 7, 2 / 7))

  # New run directories (17 significant digits).
  MOSAIC:::.mosaic_write_json(priors, file.path(inp, "priors.json"), io = NULL)
  MOSAIC:::.mosaic_write_json(config, file.path(inp, "config.json"), io = NULL)
  expect_no_error(suppressWarnings(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors)))

  # Run directories written before v0.93.1 (15 significant digits).
  wj <- function(x, f) jsonlite::write_json(x, f, pretty = TRUE, auto_unbox = TRUE,
                                            digits = NA)
  wj(priors, file.path(inp, "priors.json"))
  wj(config, file.path(inp, "config.json"))
  expect_no_error(suppressWarnings(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors)))

  # A genuinely different config is still refused at either precision.
  changed <- config; changed$beta_j0_env[2] <- 0.3
  expect_error(suppressWarnings(MOSAIC:::.mosaic_resume_check_inputs(dirs, changed, priors)),
               "differs from 1_inputs/config.json")
  MOSAIC:::.mosaic_write_json(config, file.path(inp, "config.json"), io = NULL)
  expect_error(suppressWarnings(MOSAIC:::.mosaic_resume_check_inputs(dirs, changed, priors)),
               "differs from 1_inputs/config.json")
})

test_that("resume still detects a changed control$likelihood in a 17-digit control.json", {
  base <- tempfile("json_ctl_"); inp <- file.path(base, "1_inputs")
  dir.create(inp, recursive = TRUE)
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  dirs <- list(inputs = inp)
  control <- list(likelihood = list(weight_cases = 1, weight_deaths = 1 / 3,
                                    burn_in_days = 14L))
  MOSAIC:::.mosaic_write_json(list(control = control, timestamp = "t0"),
                              file.path(inp, "control.json"), io = NULL)
  expect_no_error(suppressWarnings(
    MOSAIC:::.mosaic_resume_check_inputs(dirs, list(), list(), control)))
  changed <- control; changed$likelihood$weight_deaths <- 0.5
  expect_error(suppressWarnings(
    MOSAIC:::.mosaic_resume_check_inputs(dirs, list(), list(), changed)),
               "control\\$likelihood")
})
