# =============================================================================
# test-est_CFR_hierarchical.R
#
# est_CFR_hierarchical() fits the WHO-annual reported CFR with a binomial GAM
# (global trend + country intercepts + per-country drift + a country-year random
# effect) and writes the per-location, per-year estimates that make_mu_jt() and
# priors_default$mu_jt are built from. These tests run it on synthetic data with
# a known year-to-year SD, so they need no MOSAIC-data checkout.
# =============================================================================

skip_if_not_installed("mgcv")

.synthetic_who <- function(seed = 1, sigma = 0.7, tau = 0.5, years = 1990:2024) {
  set.seed(seed)
  isos <- c(MOSAIC::iso_codes_mosaic[1:12], "ZZA", "ZZB")
  base <- stats::setNames(qlogis(0.02) + rnorm(length(isos), 0, tau), isos)
  rows <- expand.grid(iso_code = isos, year = years, stringsAsFactors = FALSE)
  rows$cases_total <- rpois(nrow(rows), 3000)
  eta <- base[rows$iso_code] + rnorm(nrow(rows), 0, sigma)
  rows$deaths_total <- rbinom(nrow(rows), rows$cases_total, plogis(eta))
  rows$country <- rows$iso_code
  rbind(rows, data.frame(iso_code = "AFRO", year = years, cases_total = 1e5,
                         deaths_total = 2000, country = "AFRO Region"))
}

.run_est <- function(who, ...) {
  d <- tempfile("est_cfr_"); dir.create(d)
  dir.create(file.path(d, "who")); dir.create(file.path(d, "input"))
  utils::write.csv(who, file.path(d, "who", "who_afro_annual.csv"), row.names = FALSE)
  paths <- list(DATA_WHO_ANNUAL = file.path(d, "who"), MODEL_INPUT = file.path(d, "input"))
  res <- est_CFR_hierarchical(paths, validate = FALSE, save_diagnostics = FALSE, verbose = FALSE, ...)
  list(res = res, dir = file.path(d, "input"))
}

# One shared fit for the read-only checks (each bam fit takes several seconds).
.shared <- local({ cache <- NULL; function() { if (is.null(cache)) cache <<- .run_est(.synthetic_who()); cache } })

test_that("the fit recovers the year-to-year SD and writes every output", {
  out <- .shared()
  expect_equal(out$res$summary$sigma, 0.7, tolerance = 0.15)
  expect_true(out$res$summary$tau > 0)
  for (f in c("param_mu_disease_mortality.csv", "cfr_hierarchical_estimates.csv",
              "cfr_temporal_trend.csv", "cfr_country_effects.csv", "cfr_model_summary.rds"))
    expect_true(file.exists(file.path(out$dir, f)), info = f)
  # The AFRO aggregate never enters the fit.
  expect_false("AFRO" %in% out$res$country_effects$iso_code)
})

test_that("predictions cover every MOSAIC location, with predictive widths and no clamp", {
  out <- .shared()
  p <- out$res$predictions
  expect_setequal(unique(p$iso_code), MOSAIC::iso_codes_mosaic)
  expect_true(all(p$logit_sd > p$cfr_se))
  expect_equal(p$logit_sd, sqrt(p$cfr_se^2 + out$res$summary$sigma^2 +
                                  ifelse(p$pooled, out$res$summary$tau^2, 0)), tolerance = 1e-10)
  expect_equal(p$cfr_estimate, plogis(p$logit_mean), tolerance = 1e-12)
  # Locations with no data are pooled onto the global curve and carry tau^2.
  unseen <- setdiff(MOSAIC::iso_codes_mosaic, MOSAIC::iso_codes_mosaic[1:12])
  expect_true(all(p$pooled[p$iso_code %in% unseen]))
  expect_false(any(p$pooled[p$iso_code %in% MOSAIC::iso_codes_mosaic[1:12]]))
})

test_that("carry_forward holds each location's last-data-year value through the forecast years", {
  out <- .shared()   # data 1990-2024, forecast_years = 3 (the default)
  p <- out$res$predictions
  expect_identical(range(p$year), c(1990L, 2027L))
  expect_true(all(p$is_forecast == (p$year > 2024L)))
  for (iso in MOSAIC::iso_codes_mosaic[1:3]) {
    q <- p[p$iso_code == iso, ]
    expect_equal(q$logit_mean[q$year > 2024], rep(q$logit_mean[q$year == 2024], 3), tolerance = 1e-12)
  }
})

test_that("the parameter table distinguishes the point value from the logit-normal mean", {
  out <- .shared()
  tab <- utils::read.csv(file.path(out$dir, "param_mu_disease_mortality.csv"), stringsAsFactors = FALSE)
  p <- out$res$predictions
  row_of <- function(dist, par) tab[tab$parameter_distribution == dist & tab$parameter_name == par &
                                      tab$j == "AGO" & tab$t == 2020, "parameter_value"]
  expect_equal(row_of("point", "mean"), p$cfr_estimate[p$iso_code == "AGO" & p$year == 2020], tolerance = 1e-10)
  expect_equal(row_of("logitnormal", "mean"), p$logit_mean[p$iso_code == "AGO" & p$year == 2020], tolerance = 1e-10)
  expect_lt(row_of("logitnormal", "mean"), 0)
})

test_that("population_weighted is deprecated and ignored", {
  expect_warning(w <- .run_est(.synthetic_who(), population_weighted = TRUE), "deprecated and ignored")
  expect_equal(w$res$predictions$logit_mean, .shared()$res$predictions$logit_mean)
})
