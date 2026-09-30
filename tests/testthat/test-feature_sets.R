test_that("get_feature_set resolves v7.3 / v7.4 / default and rejects unknown", {
  v73 <- get_feature_set("v7.3")
  expect_type(v73, "character")
  expect_length(v73, 38L)
  expect_identical(v73, MINFEAT_V7_3_FEATURE_SET)

  v74 <- get_feature_set("v7.4")
  expect_type(v74, "character")
  expect_length(v74, 42L)
  expect_identical(v74, MINFEAT_V7_4_FEATURE_SET)
  # v7.4 is a strict superset of v7.3 plus exactly the 4 hazard channels
  expect_true(all(v73 %in% v74))
  expect_setequal(setdiff(v74, v73),
                  c("emdat_cyclone_prob", "emdat_cyclone_prob_12w_max",
                    "drought_prob", "drought_prob_26w_mean"))

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
