# =============================================================================
# test-cfr-pipeline-consistency.R
#
# Guards the invariant that config_default and priors_default agree per country
# on the reported case fatality ratio. The two data objects are built by
# independent scripts (data-raw/make_config_default.R and
# data-raw/make_priors_default.R), and silent drift between them is the bug class
# behind CLAUDE.md Lesson #12.
#
# Since config_default v5.0 / priors_default v16.0 (MOSAIC v0.96.0) the reported
# CFR is the model input config_default$mu_jt, a [location x day] matrix built by
# make_mu_jt() from est_CFR_hierarchical()'s per-location, per-year estimates and
# interpolated on the logit scale between mid-years. priors_default$mu_jt carries
# the same per-year centres (logit_mean), their SEs and the widths the integrated
# deaths likelihood uses. So on every 1 July inside the window:
#   qlogis(config_default$mu_jt[iso, 1 July of year y]) == priors mu_jt[[iso]]$logit_mean[y]
# A mismatch means one of the .rda files has gone stale.
# =============================================================================

library(testthat)

test_that("config_default$mu_jt equals the priors_default mu_jt centres at every mid-year", {
  cfg <- MOSAIC::config_default
  pri <- MOSAIC::priors_default
  expect_true(is.matrix(cfg$mu_jt))
  dates <- as.Date(cfg$date_start) + seq_len(ncol(cfg$mu_jt)) - 1L
  expect_identical(dim(cfg$mu_jt), c(length(cfg$location_name), length(dates)))
  expect_equal(max(dates), as.Date(cfg$date_stop))
  mid <- which(format(dates, "%m-%d") == "07-01")
  expect_gt(length(mid), 0L)
  yr <- as.integer(format(dates[mid], "%Y"))
  for (i in seq_along(cfg$location_name)) {
    iso <- cfg$location_name[i]
    L <- pri$mu_jt$location[[iso]]
    expect_false(is.null(L), info = iso)
    expect_true(all(yr %in% L$year), info = iso)
    expect_equal(stats::qlogis(cfg$mu_jt[i, mid]), L$logit_mean[match(yr, L$year)],
                 tolerance = 1e-8, info = iso)
  }
})

test_that("priors_default$mu_jt carries positive widths and SEs for every config location", {
  pri <- MOSAIC::priors_default
  cfg <- MOSAIC::config_default
  mj <- pri$mu_jt
  expect_false(is.null(mj))
  expect_true(is.numeric(mj$sd_year) && length(mj$sd_year) == 1L && mj$sd_year > 0)
  expect_true(is.numeric(mj$sd_product) && length(mj$sd_product) == 1L && mj$sd_product > 0)
  expect_setequal(names(mj$location), cfg$location_name)
  for (iso in names(mj$location)) {
    L <- mj$location[[iso]]
    expect_identical(length(L$year), length(L$logit_se), info = iso)
    expect_true(all(is.finite(L$logit_se) & L$logit_se > 0), info = iso)
    expect_false(anyDuplicated(L$year) > 0, info = iso)
  }
})

test_that("the retired mortality fields are gone from both default objects", {
  cfg <- MOSAIC::config_default
  pri <- MOSAIC::priors_default
  for (f in c("mu_j_baseline", "mu_j_epidemic_factor", "CFR_target", "delta_reporting_deaths", "mu_j_slope"))
    expect_null(cfg[[f]], info = f)
  for (f in c("mu_j_baseline", "mu_j_epidemic_factor", "CFR_target", "mu_j_slope"))
    expect_null(pri$parameters_location[[f]], info = f)
  expect_null(pri$parameters_global$delta_reporting_deaths)
})

test_that("rho_deaths uses the informative Beta(36.95, 51.02) prior", {
  rd <- MOSAIC::priors_default$parameters_global$rho_deaths
  expect_equal(rd$distribution, "beta")
  expect_equal(rd$parameters$shape1, 36.95, tolerance = 1e-6)
  expect_equal(rd$parameters$shape2, 51.02, tolerance = 1e-6)
})

test_that("config_default mu_jt is a plausible reported CFR and feasible for the engine", {
  cfg <- MOSAIC::config_default
  expect_true(all(cfg$mu_jt > 0 & cfg$mu_jt < 0.15))
  # Dense-data countries sit within the range WHO annual reports for 2023-2025.
  for (iso in intersect(c("MOZ", "KEN", "ETH", "COD", "NGA"), cfg$location_name)) {
    m <- mean(cfg$mu_jt[match(iso, cfg$location_name), ])
    expect_gt(m, 0.002, label = iso); expect_lt(m, 0.06, label = iso)
  }
  # p_fatal = mu_jt * rho / (rho_deaths * chi_epidemic) must be a probability.
  expect_lt(max(cfg$mu_jt) * cfg$rho / (cfg$rho_deaths * cfg$chi_epidemic), 1)
})
