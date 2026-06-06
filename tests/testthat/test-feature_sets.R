test_that("get_feature_set resolves v7.3 / default and rejects unknown", {
  v73 <- get_feature_set("v7.3")
  expect_type(v73, "character")
  expect_length(v73, 38L)
  expect_identical(v73, MINFEAT_V7_3_FEATURE_SET)
  expect_null(get_feature_set("default"))
  expect_error(get_feature_set("bogus"))
})

test_that("v7.3 features all exist in the production suitability CSV (schema-drift guard)", {
  csv <- file.path("..", "..", "..", "MOSAIC-data", "processed", "cholera", "weekly",
                   "cholera_country_weekly_suitability_data.csv")
  # also try the absolute location used in production
  if (!file.exists(csv))
    csv <- "/Users/johngiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv"
  testthat::skip_if_not(file.exists(csv), "suitability CSV not available in this environment")
  hdr <- names(utils::read.csv(csv, nrows = 1L, stringsAsFactors = FALSE))
  missing <- setdiff(MINFEAT_V7_3_FEATURE_SET, hdr)
  expect_true(length(missing) == 0L,
              info = paste("v7.3 features missing from suitability CSV:",
                           paste(missing, collapse = ", ")))
  expect_true("target_C_rate_global" %in% hdr)
})
