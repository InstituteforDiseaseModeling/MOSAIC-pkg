# =============================================================================
# test-staged_metapop_prior_ops.R
#
# Regression tests for the config/priors manipulation ops the STAGED metapop
# calibration relies on, exercised on the REAL 40-location config_default /
# priors_default (the existing suites used only 2-location toy fixtures and
# never the actual Stage-2 partial-fold scenario). Covers the gaps surfaced by
# the v0.47.5 deep review:
#   1. update_priors_from_posteriors partial-fold invariance (39 others + globals)
#   2. .validate_updated_priors catches location NAME drift (not just count)
#   3. get_location_priors / get_location_config return the same location order
#   4. inflate_priors(beta_j0_tot) scope + lognormal mean/variance on full 40-loc
#   5. convert_config_to_matrix / convert_matrix_to_config 40-loc round-trip
# =============================================================================

test_that("update_priors_from_posteriors folds a partial posterior without touching other locations or globals", {
  base <- MOSAIC::priors_default
  skip_if(is.null(base$parameters_location$beta_j0_tot$location$ETH),
          "priors_default lacks beta_j0_tot$location$ETH")
  eth_base <- base$parameters_location$beta_j0_tot$location$ETH

  # Synthetic posterior covering ONLY ETH's beta_j0_tot (the Stage-2 partial fold).
  post <- list(
    metadata = list(version = "test"),
    parameters_location = list(
      beta_j0_tot = list(
        description = base$parameters_location$beta_j0_tot$description,
        location = list(
          ETH = list(distribution = "lognormal",
                     parameters = list(meanlog = -8.0, sdlog = 0.5))))))

  upd <- update_priors_from_posteriors(base, post, verbose = FALSE)

  # (a) ETH beta_j0_tot changed
  expect_false(identical(upd$parameters_location$beta_j0_tot$location$ETH, eth_base))
  # (b) every OTHER location of beta_j0_tot is byte-identical to base
  others <- setdiff(names(base$parameters_location$beta_j0_tot$location), "ETH")
  for (loc in others)
    expect_identical(upd$parameters_location$beta_j0_tot$location[[loc]],
                     base$parameters_location$beta_j0_tot$location[[loc]])
  # (c) every OTHER location-parameter group is byte-identical to base
  for (p in setdiff(names(base$parameters_location), "beta_j0_tot"))
    expect_identical(upd$parameters_location[[p]], base$parameters_location[[p]])
  # (d) globals untouched (posterior carried no globals)
  expect_identical(upd$parameters_global, base$parameters_global)
  # (e) location set + order within the group preserved
  expect_identical(names(upd$parameters_location$beta_j0_tot$location),
                   names(base$parameters_location$beta_j0_tot$location))
})


test_that(".validate_updated_priors catches location NAME drift, not just count", {
  base <- MOSAIC::priors_default
  drifted <- base
  # Relabel one location in one group: count stays the same, a name is wrong.
  locs <- drifted$parameters_location$beta_j0_tot$location
  nm <- names(locs)
  nm[nm == "AGO"] <- "XXX"
  names(locs) <- nm
  drifted$parameters_location$beta_j0_tot$location <- locs

  # Name-drift (count preserved) must now ERROR (it silently passed pre-v0.47.5).
  expect_error(
    MOSAIC:::.validate_updated_priors(drifted, base, verbose = FALSE),
    "name mismatch")
  # Control: an unmodified structure must still validate cleanly.
  expect_error(
    MOSAIC:::.validate_updated_priors(base, base, verbose = FALSE),
    NA)
})


test_that("get_location_priors and get_location_config return locations in the same canonical order", {
  req <- c("NGA", "ETH", "KEN")   # deliberately non-canonical request order
  gp <- get_location_priors(req)
  gc <- get_location_config(req)

  p_order <- names(gp$parameters_location[[1]]$location)
  c_order <- gc$location_name
  # The two extractors must agree (post-v0.47.5 both follow canonical source order).
  expect_identical(p_order, c_order)
  # ...and it is the canonical config order subset, NOT the requested order.
  canon <- intersect(MOSAIC::config_default$location_name, req)
  expect_identical(c_order, canon)
  expect_false(identical(c_order, req))   # guard: would catch a regression to requested-order
})


test_that("inflate_priors(beta_j0_tot, 2) widens only that group on the full 40-loc priors", {
  base <- MOSAIC::priors_default
  inf  <- inflate_priors(base, inflation_factor = 2, params = "beta_j0_tot", verbose = FALSE)

  # (a) only beta_j0_tot changed; all other groups + globals byte-identical
  for (p in setdiff(names(base$parameters_location), "beta_j0_tot"))
    expect_identical(inf$parameters_location[[p]], base$parameters_location[[p]])
  expect_identical(inf$parameters_global, base$parameters_global)

  # (b) all 40 lognormal entries: mean preserved, variance ~2x
  locs <- names(base$parameters_location$beta_j0_tot$location)
  expect_gte(length(locs), 40L)
  lnmean <- function(p) exp(p$meanlog + p$sdlog^2 / 2)
  lnvar  <- function(p) (exp(p$sdlog^2) - 1) * exp(2 * p$meanlog + p$sdlog^2)
  for (loc in locs) {
    b <- base$parameters_location$beta_j0_tot$location[[loc]]
    n <- inf$parameters_location$beta_j0_tot$location[[loc]]
    skip_if_not(identical(b$distribution, "lognormal"))
    expect_equal(lnmean(n$parameters), lnmean(b$parameters), tolerance = 1e-6)
    expect_equal(lnvar(n$parameters) / lnvar(b$parameters), 2, tolerance = 1e-3)
  }
})


test_that("convert_config_to_matrix / convert_matrix_to_config round-trips the full 40-loc config", {
  cfg  <- MOSAIC::config_default
  vec  <- convert_config_to_matrix(cfg)
  back <- convert_matrix_to_config(vec, config_base = cfg)
  # Per-location transmission vector and a global recovery rate round-trip exactly.
  expect_equal(back$beta_j0_tot, cfg$beta_j0_tot, tolerance = 1e-10)
  expect_equal(back$gamma_1,     cfg$gamma_1,     tolerance = 1e-10)
  expect_length(back$beta_j0_tot, length(cfg$beta_j0_tot))
  # NOTE: convert_config_to_dataframe is NOT param-set-equivalent to the matrix
  # path (it omits per-location a_*_j/b_*_j and prop_*_initial — review item LOW-3,
  # deferred); it is unused by run_MOSAIC, so only the matrix path is asserted here.
})
