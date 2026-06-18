# Tier-1 (CI, pure-R, no keras): est_suitability() dispatcher + deprecation
# handling. The routing assertions mock .est_suitability_lstm_v2 /
# .est_suitability_legacy via testthat::local_mocked_bindings, so they never
# touch Python. The deprecation-message tests, however, are NOT mocked: after
# emitting the message est_suitability() proceeds into the real lstm_v2 path,
# which loads the tensorflow R package (whose .onLoad imports Python TF). In an
# env without the Python tensorflow module (e.g. the TF-stripped worker image)
# that load errors, so those tests guard with skip_without_tensorflow().

fake_paths <- list()  # dispatch logic never touches PATHS

# Skip when the Python tensorflow module is unavailable (e.g. the worker image
# strips it). Keeps the suite portable; a no-op where TF is installed.
skip_without_tensorflow <- function() {
  testthat::skip_if_not_installed("reticulate")
  testthat::skip_if_not(reticulate::py_module_available("tensorflow"),
                        "Python tensorflow module not available")
}

test_that("supplying both bias_correct and the deprecated calibrate errors", {
  expect_error(
    est_suitability(fake_paths, bias_correct = FALSE, calibrate = FALSE),
    "only one of `bias_correct` or the deprecated `calibrate`")
  expect_error(
    est_suitability(fake_paths, bias_correct = TRUE, calibrate = TRUE),
    "only one of `bias_correct` or the deprecated `calibrate`")
})

test_that("calibrate alone is mapped to bias_correct with a deprecation message", {
  skip_without_tensorflow()
  expect_message(
    tryCatch(est_suitability(fake_paths, calibrate = FALSE), error = function(e) NULL),
    "`calibrate` is deprecated and was mapped to `bias_correct`")
})

test_that("frozen v0.33 legacy knobs are ignored with a deprecation message", {
  skip_without_tensorflow()
  for (arg in list(list(n_splits = 5), list(seed_base = 1), list(split_method = "x"),
                   list(train_prop = 0.5), list(fine_tune_epochs = 3),
                   list(fine_tune_lr = 0.01), list(exclude_covariates = "foo"))) {
    expect_message(
      tryCatch(do.call(est_suitability, c(list(fake_paths), arg)),
               error = function(e) NULL),
      "ignored. The legacy path is frozen at v0.33 settings")
  }
})

test_that("truly unknown args warn", {
  skip_without_tensorflow()
  expect_warning(
    tryCatch(est_suitability(fake_paths, totally_bogus_arg = 1),
             error = function(e) NULL),
    "ignoring unrecognized argument")
})

test_that("architecture is validated with match.arg", {
  expect_error(est_suitability(fake_paths, architecture = "not_an_arch"),
               "'arg' should be one of|should be one of")
})

test_that("default architecture routes to lstm_v2 and forwards bias_correct/response_var", {
  captured <- NULL
  testthat::local_mocked_bindings(
    .est_suitability_lstm_v2 = function(...) {
      captured <<- c(list(branch = "lstm_v2"), list(...)); "ok"
    },
    .package = "MOSAIC")
  est_suitability(fake_paths)                       # default architecture
  expect_equal(captured$branch, "lstm_v2")
  expect_true(captured$bias_correct)                # default TRUE
  expect_equal(captured$response_var, "target_D_rate_per_country_floored")  # per-capita per-country (new default)
  # calibrate -> bias_correct mapping reaches the lstm_v2 branch with the value
  captured <- NULL
  suppressMessages(est_suitability(fake_paths, calibrate = FALSE))
  expect_equal(captured$branch, "lstm_v2")
  expect_false(captured$bias_correct)
})

test_that("architecture='lstm_v1_legacy' routes to the legacy path (not lstm_v2)", {
  captured <- NULL
  testthat::local_mocked_bindings(
    .est_suitability_legacy = function(...) {
      captured <<- c(list(branch = "legacy"), list(...)); "ok"
    },
    .package = "MOSAIC")
  est_suitability(fake_paths, architecture = "lstm_v1_legacy")
  expect_equal(captured$branch, "legacy")
  expect_true(captured$bias_correct)
})
