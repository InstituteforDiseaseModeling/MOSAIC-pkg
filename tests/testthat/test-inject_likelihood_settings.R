# Tests for .mosaic_inject_likelihood_settings() and the Dask-path keep list
# in .extract_base_config(). Closes the validation gap from
# plan_calc_model_likelihood_python_port.md §3.5 (Phase 1 unit test).

test_that("inject_likelihood_settings adds 13 expected keys", {
  cfg <- list(
    location_name = c("MOZ", "ETH"),
    date_start    = "2023-01-01",
    date_stop     = "2023-12-31"
  )
  ls <- list(
    weight_cases             = 1.0,
    weight_deaths            = 1.0,
    .weights_time_resolved   = NULL,
    weights_location         = NULL,
    nb_k_min_cases           = 3,
    nb_k_min_deaths          = 3,
    weight_peak_timing       = 0,
    weight_peak_magnitude    = 0,
    weight_cumulative_total  = 0,
    weight_wis               = 0,
    sigma_peak_time          = 1,
    sigma_peak_log           = 0.5
  )

  out <- MOSAIC:::.mosaic_inject_likelihood_settings(cfg, ls)

  # Keys with non-NULL values must be present after injection. Keys with
  # NULL inputs (weights_time, weights_location in this test) are absent
  # from the dict by R semantics — that's fine because the Python analyzer
  # reads them via getattr(p, "key", None), which treats absent and None
  # identically.
  expected_present <- c(
    "calc_likelihood",
    "weight_cases", "weight_deaths",
    "nb_k_min_cases", "nb_k_min_deaths",
    "weight_peak_timing", "weight_peak_magnitude",
    "weight_cumulative_total", "weight_wis",
    "sigma_peak_time", "sigma_peak_log",
    "epidemic_peaks"
  )
  expect_true(all(expected_present %in% names(out)))
  expect_true(all(c("location_name", "date_start", "date_stop") %in% names(out)))

  # Sanity: the two NULL-valued keys are correctly absent from the list
  # (consequence of R's `x[[k]] <- NULL` removal semantics).
  expect_false("weights_time" %in% names(out))
  expect_false("weights_location" %in% names(out))

  # calc_likelihood is always TRUE post-injection
  expect_true(isTRUE(out$calc_likelihood))

  # Scalars round-trip
  expect_equal(out$weight_cases, 1.0)
  expect_equal(out$weight_deaths, 1.0)
  expect_equal(out$nb_k_min_cases, 3)
  expect_equal(out$nb_k_min_deaths, 3)
  expect_equal(out$sigma_peak_time, 1)
  expect_equal(out$sigma_peak_log, 0.5)

  # Default-OFF shape weights stay 0
  expect_equal(out$weight_peak_timing, 0)
  expect_equal(out$weight_peak_magnitude, 0)
  expect_equal(out$weight_cumulative_total, 0)
  expect_equal(out$weight_wis, 0)

  # NULL weight vectors round-trip as NULL
  expect_null(out$weights_time)
  expect_null(out$weights_location)

  # epidemic_peaks is a two-column data.frame with character columns
  expect_s3_class(out$epidemic_peaks, "data.frame")
  expect_named(out$epidemic_peaks, c("iso_code", "peak_date"))
  expect_type(out$epidemic_peaks$iso_code, "character")
  expect_type(out$epidemic_peaks$peak_date, "character")
  expect_gt(nrow(out$epidemic_peaks), 0)
})

test_that("inject_likelihood_settings forwards .weights_time_resolved (not raw)", {
  cfg <- list(location_name = "MOZ")
  ls <- list(
    weight_cases             = 1.0,
    weight_deaths            = 1.0,
    weights_time             = c(1, 2, 3),                   # raw — must NOT win
    .weights_time_resolved   = c(0.5, 1.0, 1.5),             # resolved — must be forwarded
    weights_location         = NULL,
    nb_k_min_cases           = 3,
    nb_k_min_deaths          = 3,
    weight_peak_timing       = 0,
    weight_peak_magnitude    = 0,
    weight_cumulative_total  = 0,
    weight_wis               = 0,
    sigma_peak_time          = 1,
    sigma_peak_log           = 0.5
  )
  out <- MOSAIC:::.mosaic_inject_likelihood_settings(cfg, ls)
  expect_equal(out$weights_time, c(0.5, 1.0, 1.5))
})

test_that("inject_likelihood_settings is purely additive (does not remove keys)", {
  cfg <- list(
    location_name  = "MOZ",
    reported_cases = matrix(0, 1, 10),
    arbitrary_key  = "preserved"
  )
  ls <- list(
    weight_cases             = 1.0,
    weight_deaths            = 1.0,
    .weights_time_resolved   = NULL,
    weights_location         = NULL,
    nb_k_min_cases           = 3,
    nb_k_min_deaths          = 3,
    weight_peak_timing       = 0,
    weight_peak_magnitude    = 0,
    weight_cumulative_total  = 0,
    weight_wis               = 0,
    sigma_peak_time          = 1,
    sigma_peak_log           = 0.5
  )
  out <- MOSAIC:::.mosaic_inject_likelihood_settings(cfg, ls)
  expect_equal(out$arbitrary_key, "preserved")
  expect_equal(dim(out$reported_cases), c(1L, 10L))
})

test_that(".extract_base_config keeps the new injected keys", {
  cfg <- list(
    # Original keep-list members
    b_jt          = matrix(0, 1, 5),
    location_name = "MOZ",
    date_start    = "2023-01-01",
    date_stop     = "2023-12-31",
    # New injected members (issue #100)
    calc_likelihood          = TRUE,
    weight_cases             = 1.0,
    weight_deaths            = 1.0,
    weights_time             = c(0.5, 1.0, 1.5),
    weights_location         = NULL,
    nb_k_min_cases           = 3,
    nb_k_min_deaths          = 3,
    weight_peak_timing       = 0,
    weight_peak_magnitude    = 0,
    weight_cumulative_total  = 0,
    weight_wis               = 0,
    sigma_peak_time          = 1,
    sigma_peak_log           = 0.5,
    epidemic_peaks           = data.frame(iso_code = "MOZ", peak_date = "2023-06-01"),
    # A field that should be dropped (not in keep list)
    arbitrary_drop           = "not in keep list"
  )

  out <- MOSAIC:::.extract_base_config(cfg)

  # All injected keys survive
  for (k in c("calc_likelihood",
              "weight_cases", "weight_deaths",
              "weights_time", "weights_location",
              "nb_k_min_cases", "nb_k_min_deaths",
              "weight_peak_timing", "weight_peak_magnitude",
              "weight_cumulative_total", "weight_wis",
              "sigma_peak_time", "sigma_peak_log",
              "epidemic_peaks")) {
    expect_true(k %in% names(out), info = paste("missing key:", k))
  }

  # Non-keep-list fields are dropped
  expect_false("arbitrary_drop" %in% names(out))
})

test_that(".extract_base_config does NOT inject keys when none are present", {
  # Local-path config (no Dask, no inject) — keep list must not synthesize
  # likelihood-control keys out of nowhere.
  cfg <- list(
    b_jt          = matrix(0, 1, 5),
    location_name = "MOZ",
    date_start    = "2023-01-01",
    date_stop     = "2023-12-31"
    # NO likelihood-control keys, NO epidemic_peaks, NO calc_likelihood
  )
  out <- MOSAIC:::.extract_base_config(cfg)

  injected_keys <- c(
    "calc_likelihood",
    "weight_cases", "weight_deaths",
    "weights_time", "weights_location",
    "nb_k_min_cases", "nb_k_min_deaths",
    "weight_peak_timing", "weight_peak_magnitude",
    "weight_cumulative_total", "weight_wis",
    "sigma_peak_time", "sigma_peak_log",
    "epidemic_peaks"
  )
  for (k in injected_keys) {
    expect_false(k %in% names(out), info = paste("unexpectedly present:", k))
  }
})
