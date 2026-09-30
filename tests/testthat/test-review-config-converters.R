# Regression tests for the config-group review fixes to the config converters:
# convert_config_to_matrix(), convert_config_to_dataframe() and get_param_names()
# now share one parameter list, so the seasonality, PPV and IC-proportion fields
# are carried by all three.

test_that("convert_config_to_dataframe() matches convert_config_to_matrix() column for column", {
  cfg <- get_location_config(iso = c("ETH", "KEN"))
  vec <- convert_config_to_matrix(cfg)
  df  <- convert_config_to_dataframe(cfg)
  expect_identical(names(df), names(vec))
  expect_equal(vec, unlist(df))
  for (col in c("a_1_j_ETH", "b_2_j_KEN", "prop_S_initial_ETH", "prop_V2_initial_KEN",
                "chi_endemic", "chi_epidemic")) {
    expect_true(col %in% names(df), info = col)
  }
})

test_that("get_param_names() on a config reports chi, seasonality and IC-proportion fields", {
  cfg <- get_location_config(iso = c("ETH", "KEN"))
  pn  <- get_param_names(cfg)
  expect_true(all(c("chi_endemic", "chi_epidemic") %in% pn$global))
  for (p in c("a_1_j", "a_2_j", "b_1_j", "b_2_j", "prop_S_initial", "prop_V1_initial",
              "N_j_initial", "alpha_1", "beta_j0_tot")) {
    expect_true(p %in% pn$location$ETH, info = p)
  }
  # Every base name the matrix converter emits is also known to get_param_names()
  base_from_matrix <- unique(sub("_(ETH|KEN)$", "", names(convert_config_to_matrix(cfg))))
  expect_true(all(base_from_matrix %in% c(pn$global, pn$location$ETH)))
})

test_that("get_param_names() on config_default is not missing any converter field", {
  pn <- get_param_names(config_default)
  expect_true(all(c("chi_endemic", "a_1_j", "prop_S_initial") %in% pn$all))
})
