# Regression tests for the reported-CFR fixes from the production-readiness
# review (group cfr-cv): process_CFR_data() (in-progress years, CIV spelling
# split, stale Beta shapes, 2.5% quantile), the param-table point label, the
# refresh registry's GAM settings, and plot_CFR_hierarchical()'s pooled key.

.cfr_who_synth <- function() {
  set.seed(5)
  isos <- MOSAIC::iso_codes_mosaic[1:6]
  who <- expand.grid(iso_code = isos, year = 2014:2025, stringsAsFactors = FALSE)
  who$cases_total <- rpois(nrow(who), 2000)
  who$deaths_total <- rbinom(nrow(who), who$cases_total, 0.02)
  who$country <- who$iso_code
  who$source <- "annual_report"
  # CIV spelled two ways (the dashboard's curly apostrophe)
  civ <- data.frame(iso_code = "CIV", year = 2014:2025, cases_total = 300,
                    deaths_total = 6, country = "Cote d'Ivoire", source = "annual_report",
                    stringsAsFactors = FALSE)
  civ$country[civ$year == 2025] <- "Côte D’ivoire"
  # an in-progress 2026 row with zero deaths, from a dated dashboard snapshot
  prog <- data.frame(iso_code = isos[1], year = 2026, cases_total = 5000, deaths_total = 0,
                     country = isos[1],
                     source = "dashboard:cholera_adm0_public_snapshot_2026-09-17.csv",
                     stringsAsFactors = FALSE)
  # a country-year with cases but no deaths reported
  na_row <- data.frame(iso_code = isos[2], year = 2013 + 1, cases_total = 99999, deaths_total = NA,
                       country = isos[2], source = "annual_report", stringsAsFactors = FALSE)
  afro <- data.frame(iso_code = "AFRO", year = 2014:2026, cases_total = 1e5, deaths_total = 2000,
                     country = "AFRO Region",
                     source = c(rep("annual_report", 12),
                                "dashboard:cholera_adm0_public_snapshot_2026-09-17.csv"),
                     stringsAsFactors = FALSE)
  rbind(who, civ, prog, na_row, afro)
}

.cfr_run_process <- function(who, min_obs) {
  d <- tempfile("cfr_proc_"); dir.create(file.path(d, "who"), recursive = TRUE)
  dir.create(file.path(d, "tables"))
  utils::write.csv(who, file.path(d, "who", "who_afro_annual.csv"), row.names = FALSE)
  P <- list(DATA_WHO_ANNUAL = file.path(d, "who"), DOCS_TABLES = file.path(d, "tables"))
  out <- suppressMessages(process_CFR_data(P, min_obs = min_obs))
  list(out = out, P = P)
}

test_that("process_CFR_data drops in-progress years, merges CIV spellings, keeps paired counts", {
  who <- .cfr_who_synth()
  r <- .cfr_run_process(who, min_obs = 150)
  out <- r$out
  expect_true(is.data.frame(out))
  # output named for the last COMPLETE year (the 2026 snapshot rows are dropped)
  expect_true(file.exists(file.path(r$P$DATA_WHO_ANNUAL, "case_fatality_ratio_2014_2025.csv")))
  expect_false(file.exists(file.path(r$P$DATA_WHO_ANNUAL, "case_fatality_ratio_2014_2026.csv")))
  i1 <- MOSAIC::iso_codes_mosaic[1]
  keep <- who$iso_code == i1 & who$year <= 2025
  expect_equal(out$cases_total[out$iso_code == i1], sum(who$cases_total[keep]))
  # one CIV row with the full totals
  expect_equal(sum(out$iso_code == "CIV"), 1L)
  expect_equal(out$cases_total[out$iso_code == "CIV"], 300 * 12)
  expect_identical(out$country[out$iso_code == "CIV"], "Cote d'Ivoire")
  # the year with cases but no deaths is left out of both totals
  i2 <- MOSAIC::iso_codes_mosaic[2]
  keep2 <- who$iso_code == i2 & !is.na(who$deaths_total)
  expect_equal(out$cases_total[out$iso_code == i2], sum(who$cases_total[keep2]))
})

test_that("process_CFR_data gives AFRO-filled rows the AFRO Beta", {
  # min_obs = 1: no country falls below it, so the last loop-fitted `prm` used
  # to be a country's own Beta and leaked into every absent country's row.
  r <- .cfr_run_process(.cfr_who_synth(), min_obs = 1)
  out <- r$out
  afro <- out[out$iso_code == "AFRO", ]
  absent <- out[is.na(out$cases_total), ]
  expect_gt(nrow(absent), 0L)
  expect_true(all(absent$shape1 == afro$shape1))
  expect_true(all(absent$shape2 == afro$shape2))
})

test_that("process_CFR_data writes Betas that reproduce each row's CFR and 95% CI", {
  # propvacc::get_beta_params() stopped at Beta(~0.009, ~1.9) (mean ~0.005) for
  # every country with a ~2% CFR, so the shapes did not describe the CFR at all.
  out <- .cfr_run_process(.cfr_who_synth(), min_obs = 150)$out
  fitted <- out[is.finite(out$shape1) & is.finite(out$cases_total), ]
  expect_gt(nrow(fitted), 3L)
  for (i in seq_len(nrow(fitted))) {
    r <- fitted[i, ]
    q <- stats::qbeta(c(0.025, 0.5, 0.975), r$shape1, r$shape2)
    expect_rel_equal(q, c(r$cfr_lo, r$cfr, r$cfr_hi), rel = 0.01)
  }
})

test_that(".cfr_beta_from_ci matches the requested quantiles, including 2.5% vs 2.75%", {
  for (n in c(3600, 24000, 1.3e6)) {
    d <- round(0.02 * n)
    ci <- stats::binom.test(d, n)$conf.int
    p25 <- MOSAIC:::.cfr_beta_from_ci(d, n, c(0.025, 0.5, 0.975), c(ci[1], d / n, ci[2]))
    p275 <- MOSAIC:::.cfr_beta_from_ci(d, n, c(0.0275, 0.5, 0.975), c(ci[1], d / n, ci[2]))
    expect_rel_equal(p25$shape1 / (p25$shape1 + p25$shape2), 0.02, rel = 0.01)
    expect_rel_equal(stats::qbeta(0.025, p25$shape1, p25$shape2), ci[1], rel = 0.005)
    expect_rel_equal(stats::qbeta(0.0275, p275$shape1, p275$shape2), ci[1], rel = 0.005)
    expect_false(isTRUE(all.equal(p25$shape1, p275$shape1)))
  }
  # zero deaths: no Beta has median 0, so the Jeffreys posterior is returned
  z <- MOSAIC:::.cfr_beta_from_ci(0, 50, c(0.025, 0.5, 0.975), c(0, 0, 0.07))
  expect_equal(c(z$shape1, z$shape2), c(0.5, 50.5))
})

test_that("process_CFR_data removes a superseded artifact that counted an in-progress year", {
  who <- .cfr_who_synth()
  d <- tempfile("cfr_stale_"); dir.create(file.path(d, "who"), recursive = TRUE)
  dir.create(file.path(d, "tables"))
  utils::write.csv(who, file.path(d, "who", "who_afro_annual.csv"), row.names = FALSE)
  for (dd in c("who", "tables")) for (y in c(2024, 2026))
    writeLines("stale", file.path(d, dd, sprintf("case_fatality_ratio_2014_%d.csv", y)))
  P <- list(DATA_WHO_ANNUAL = file.path(d, "who"), DOCS_TABLES = file.path(d, "tables"))
  expect_message(process_CFR_data(P, min_obs = 150), "Removed superseded CFR artifact")
  for (dd in c("who", "tables")) {
    f <- sort(list.files(file.path(d, dd), pattern = "^case_fatality_ratio_2014_"))
    expect_identical(f, c("case_fatality_ratio_2014_2024.csv", "case_fatality_ratio_2014_2025.csv"))
  }
})

test_that("process_CFR_data gives a country whose only data are in-progress the AFRO CFR", {
  who <- .cfr_who_synth()
  only <- data.frame(iso_code = "TCD", year = 2026, cases_total = 726, deaths_total = 46,
                     country = "Chad",
                     source = "dashboard:cholera_adm0_public_snapshot_2026-09-17.csv",
                     stringsAsFactors = FALSE)
  out <- .cfr_run_process(rbind(who, only), min_obs = 150)$out
  caf <- out[out$iso_code == "TCD", ]
  afro <- out[out$iso_code == "AFRO", ]
  expect_equal(nrow(caf), 1L)
  expect_true(is.na(caf$cases_total))
  expect_equal(caf$cfr, afro$cfr)
})

test_that("the parameter table labels the logit-normal median as 'median'", {
  pred <- data.frame(iso_code = "AAA", year = 2024L, cfr_estimate = plogis(-3.5),
                     logit_mean = -3.5, logit_sd = 0.4)
  tab <- MOSAIC:::.cfr_param_table(pred)
  pt <- tab[tab$parameter_distribution == "point", ]
  expect_identical(pt$parameter_name, "median")
  expect_equal(pt$parameter_value, plogis(-3.5))
})

test_that("the refresh registry fits the CFR GAM with the package defaults", {
  reg <- MOSAIC:::.mosaic_data_steps(as.Date("2026-01-01"))
  st <- reg[[which(vapply(reg, `[[`, "", "id") == "est_CFR_hierarchical")]]
  expect_false(grepl("Bayesian", st$desc))
  seen <- new.env()
  local_mocked_bindings(
    est_CFR_hierarchical = function(PATHS, ...) { seen$args <- list(...); invisible(NULL) },
    .package = "MOSAIC")
  st$run(list())
  expect_false(any(c("min_cases", "k_year", "k_trend") %in% names(seen$args)))
})

test_that("plot_CFR_hierarchical keys its fit legend on the estimates' pooled flag", {
  skip_if_not_installed("mgcv"); skip_if_not_installed("ggplot2"); skip_if_not_installed("scales")
  set.seed(9)
  isos <- MOSAIC::iso_codes_mosaic[1:8]
  who <- expand.grid(iso_code = isos, year = 2000:2024, stringsAsFactors = FALSE)
  who$cases_total <- rpois(nrow(who), 2500)
  small <- isos[1]                                   # fitted from its own data, all years < 20 cases
  who$cases_total[who$iso_code == small] <- 10
  # a high CFR sorts it into the first page of the (two-part) summary plot
  who$deaths_total <- rbinom(nrow(who), who$cases_total,
                             ifelse(who$iso_code == small, 0.3, 0.02))
  who$country <- who$iso_code
  d <- tempfile("cfr_plot_"); for (s in c("who", "input", "fig")) dir.create(file.path(d, s), recursive = TRUE)
  utils::write.csv(who, file.path(d, "who", "who_afro_annual.csv"), row.names = FALSE)
  P <- list(DATA_WHO_ANNUAL = file.path(d, "who"), MODEL_INPUT = file.path(d, "input"),
            DOCS_FIGURES = file.path(d, "fig"))
  fit <- est_CFR_hierarchical(P, validate = FALSE, save_diagnostics = FALSE, verbose = FALSE)
  expect_false(any(fit$predictions$pooled[fit$predictions$iso_code == small]))
  res <- suppressWarnings(plot_CFR_hierarchical(P, verbose = FALSE))
  sd <- res$summary$data
  expect_true(small %in% sd$iso_code)
  expect_true(sd$own_fit[sd$iso_code == small])      # own curve, not "population average"
  expect_false(sd$has_historical_data[sd$iso_code == small])
  # every location's legend key matches the fit's pooled flag
  pooled <- tapply(fit$predictions$pooled, fit$predictions$iso_code, all)
  expect_equal(as.vector(sd$own_fit), as.vector(!pooled[as.character(sd$iso_code)]))
})
