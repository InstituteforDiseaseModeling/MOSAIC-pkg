# =============================================================================
# test-sample_parameters_mu_jt.R
#
# The reported CFR mu_jt is NOT sampled (v0.96.0): it is integrated out of the
# deaths likelihood per simulated path. The sampler must pass the config's
# mu_jt through untouched, never write the retired mortality parameters back
# into a config (that would mark it as a pre-v0.96.0 config), and warn about
# retired priors and sampling flags instead of silently honouring them.
# =============================================================================

library(testthat)
tryCatch(MOSAIC::set_root_directory("~/MOSAIC"), error = function(e) NULL)

.RETIRED <- c("CFR_target", "mu_j_baseline", "mu_j_epidemic_factor", "mu_j_slope", "delta_reporting_deaths")

test_that("sample_parameters passes mu_jt through and writes no retired mortality field", {
  skip_if(is.null(getOption("root_directory")), "MOSAIC root directory not set")
  cfg <- sample_parameters(seed = 3L, verbose = FALSE)
  expect_identical(cfg$mu_jt, MOSAIC::config_default$mu_jt)
  for (f in .RETIRED) expect_null(cfg[[f]], info = f)
})

test_that("a pre-v16.0 priors object's retired priors are skipped with a warning", {
  skip_if(is.null(getOption("root_directory")), "MOSAIC root directory not set")
  pri <- MOSAIC::priors_default
  pri$parameters_location$CFR_target <- list(description = "legacy", location = setNames(
    lapply(MOSAIC::config_default$location_name, function(i)
      list(distribution = "lognormal", parameters = list(meanlog = log(0.02), sdlog = 0.787))),
    MOSAIC::config_default$location_name))
  pri$parameters_global$delta_reporting_deaths <- list(
    distribution = "truncnorm", parameters = list(mean = 4, sd = 3, a = 1, b = 14))
  rm(list = intersect(c("removed_prior_CFR_target", "removed_prior_delta_reporting_deaths"),
                      ls(MOSAIC:::.mosaic_once)), envir = MOSAIC:::.mosaic_once)
  w <- character(0)
  cfg <- withCallingHandlers(
    sample_parameters(priors = pri, seed = 3L, verbose = FALSE),
    warning = function(cnd) { w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning") })
  expect_true(any(grepl("`CFR_target`, which was removed", w)))
  expect_true(any(grepl("`delta_reporting_deaths`, which was removed", w)))
  for (f in .RETIRED) expect_null(cfg[[f]], info = f)
})

test_that("retired sampling flags warn and are ignored", {
  skip_if(is.null(getOption("root_directory")), "MOSAIC root directory not set")
  expect_warning(
    cfg <- sample_parameters(seed = 3L, verbose = FALSE,
                             sample_args = list(sample_mu_j_baseline = TRUE)),
    "sample_mu_j_baseline was removed in MOSAIC v0.96.0")
  expect_null(cfg$mu_j_baseline)
})

test_that("the priors mu_jt block passes validation and is filtered by location", {
  expect_silent(MOSAIC:::.mosaic_validate_priors(MOSAIC::priors_default, MOSAIC::config_default))
  sub <- MOSAIC::get_location_priors(c("MWI", "MOZ"), MOSAIC::priors_default)
  expect_setequal(names(sub$mu_jt$location), c("MWI", "MOZ"))
  expect_identical(sub$mu_jt$sd_year, MOSAIC::priors_default$mu_jt$sd_year)
  expect_identical(sub$mu_jt$location$MOZ, MOSAIC::priors_default$mu_jt$location$MOZ)
})
