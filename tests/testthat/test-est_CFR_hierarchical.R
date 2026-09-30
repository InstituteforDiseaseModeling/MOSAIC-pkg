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
  args <- utils::modifyList(list(PATHS = paths, validate = FALSE, save_diagnostics = FALSE,
                                 verbose = FALSE), list(...))
  res <- do.call(est_CFR_hierarchical, args)
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
  expect_equal(row_of("point", "median"), p$cfr_estimate[p$iso_code == "AGO" & p$year == 2020], tolerance = 1e-10)
  expect_equal(row_of("logitnormal", "mean"), p$logit_mean[p$iso_code == "AGO" & p$year == 2020], tolerance = 1e-10)
  expect_lt(row_of("logitnormal", "mean"), 0)
})

test_that("population_weighted is deprecated and ignored", {
  expect_warning(w <- .run_est(.synthetic_who(), population_weighted = TRUE), "deprecated and ignored")
  expect_equal(w$res$predictions$logit_mean, .shared()$res$predictions$logit_mean)
})

# Edge-case panel: country 2's data end in 2015 and 2024 is an in-progress year.
.edge_who <- function() {
  who <- .synthetic_who(seed = 2)
  early <- MOSAIC::iso_codes_mosaic[2]
  who <- who[!(who$iso_code == early & who$year > 2015), ]          # data end in 2015
  who$source <- "dashboard_annual"
  who$source[who$year == 2024 & who$iso_code != "AFRO"] <- "dashboard_snapshot_2024-05-01"   # partial year
  who
}

test_that("an in-progress year is excluded and an early-ending country is held at its own last year", {
  early <- MOSAIC::iso_codes_mosaic[2]
  out <- .run_est(.edge_who())
  expect_identical(out$res$summary$last_data_year, 2023L)          # 2024 rows dropped
  p <- out$res$predictions
  q <- p[p$iso_code == early, ]
  expect_true(all(q$is_forecast == (q$year > 2015)))
  # Its own trend is frozen at 2015 (a constant offset from the global curve,
  # which a country absent from the data follows) while the global curve runs
  # on to the global last year; after that everything is flat.
  glob <- p[p$iso_code == MOSAIC::iso_codes_mosaic[20], ]
  expect_true(all(glob$pooled))
  off <- q$logit_mean - glob$logit_mean
  expect_equal(off[q$year > 2015], rep(off[q$year == 2015], sum(q$year > 2015)), tolerance = 1e-8)
  expect_equal(diff(q$logit_mean[q$year >= 2023]), rep(0, sum(q$year > 2023)), tolerance = 1e-10)
  other <- p[p$iso_code == MOSAIC::iso_codes_mosaic[1], ]
  expect_true(all(other$is_forecast == (other$year > 2023)))
})

test_that("the validation block reports both forecast rules with a proper coverage", {
  # validate = TRUE refits the model once per held-out year, which made this
  # the single slowest test in the fast tier; it runs in the nightly slow tier,
  # on the same edge-case panel (early-ending country, in-progress year).
  skip_if_slow()
  out <- .run_est(.edge_who(), validate = TRUE)
  expect_identical(out$res$summary$last_data_year, 2023L)
  v <- out$res$validation$summary
  expect_setequal(v$method, c("carry_forward", "project"))
  expect_true(all(v$coverage95 >= 0 & v$coverage95 <= 1))
})

test_that("forecast_method = 'project' extends the trend where carry_forward holds it", {
  who <- .synthetic_who(seed = 3, years = 2005:2024)   # short panel: two fits, kept cheap
  a <- MOSAIC:::.cfr_estimate(who, forecast_years = 3L, forecast_method = "carry_forward")
  b <- MOSAIC:::.cfr_estimate(who, forecast_years = 3L, forecast_method = "project")
  iso <- MOSAIC::iso_codes_mosaic[1]
  pa <- a$predictions[a$predictions$iso_code == iso, ]
  pb <- b$predictions[b$predictions$iso_code == iso, ]
  expect_equal(pa$logit_mean[pa$year <= 2024], pb$logit_mean[pb$year <= 2024], tolerance = 1e-10)
  expect_false(isTRUE(all.equal(pa$logit_mean[pa$year > 2024], pb$logit_mean[pb$year > 2024])))
  expect_equal(diff(pa$logit_mean[pa$year >= 2024]), rep(0, 3), tolerance = 1e-10)
})
