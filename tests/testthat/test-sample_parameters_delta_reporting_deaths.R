# =============================================================================
# test-sample_parameters_delta_reporting_deaths.R
#
# delta_reporting_deaths is a DEATH-EVENT-to-death-report delay (CLAUDE.md
# Lesson #12c): R/sim_components.R reads disease_deaths at
# tick - delta_reporting_deaths and thins by rho_deaths, so the onset-to-death
# interval lives in gamma_1^-1, NOT in this parameter.
#
# It is PINNED at config_default$delta_reporting_deaths = 5 as of the CFR
# restructure (R2). There is no observational anchor: deaths and cases are
# reported on the same WHO bulletin row, so the weekly cross-correlation peaks
# at lag 0 in 11 of 15 countries and the posterior beats a resampling null in
# only 6 of 27 (claude/cfr_review/05_run_empirics.md). 5 days is the rounded
# prior median (4.60) and mean (4.86) of the retained TruncNorm(4, 3, [1, 14])
# and the midpoint of the 3-7 day IDSR death-to-report window (Routh 2017
# Tanzania; Bwire 2013 Uganda) that anchors the prior.
# =============================================================================

library(testthat)
library(MOSAIC)

tryCatch(set_root_directory("~/MOSAIC"), error = function(e) NULL)

skip_if_no_delta_deaths_prior <- function() {
  testthat::skip_if(is.null(getOption("root_directory")),
                    "MOSAIC root directory not set")
  pri <- tryCatch(MOSAIC::priors_default, error = function(e) NULL)
  if (is.null(pri) || is.null(pri$parameters_global$delta_reporting_deaths)) {
    testthat::skip("priors_default$parameters_global$delta_reporting_deaths not populated")
  }
}

test_that("the delta_reporting_deaths prior is retained as TruncNorm(4, 3, [1, 14])", {
  skip_if_no_delta_deaths_prior()
  prior <- MOSAIC::priors_default$parameters_global$delta_reporting_deaths
  expect_equal(prior$distribution, "truncnorm")
  expect_equal(prior$parameters$mean, 4)
  expect_equal(prior$parameters$sd, 3)
  expect_equal(prior$parameters$a, 1)
  expect_equal(prior$parameters$b, 14)
  # Lesson #12c: the label must describe the engine's rule, not the analyst's
  # intuition. Guard the mislabel PHRASE, not the bare word "onset" -- the
  # correct description has to mention onset in order to disclaim it ("NOT
  # symptom-onset-to-report ... the onset-to-death interval is implicit in
  # gamma_1^-1"), so a bare grepl("onset") fires on the very text that proves
  # the label is right.
  expect_false(grepl("symptom-onset-to-(death-)?report\\s+delay",
                     prior$description, ignore.case = TRUE))
  expect_true(grepl("death-event", prior$description, ignore.case = TRUE))
})

test_that("the pinned value 5 is the rounded centre of the retained prior", {
  skip_if_no_delta_deaths_prior()
  pinned <- MOSAIC::config_default$delta_reporting_deaths
  expect_equal(pinned, 5)
  p <- MOSAIC::priors_default$parameters_global$delta_reporting_deaths$parameters
  # Truncated-normal median and mean of TruncNorm(4, 3) on [1, 14] both round to 5.
  z <- function(x) stats::pnorm((x - p$mean) / p$sd)
  med <- p$mean + p$sd * stats::qnorm(z(p$a) + 0.5 * (z(p$b) - z(p$a)))
  expect_equal(round(med), pinned)
  set.seed(1L)
  draws <- stats::rnorm(2e5, p$mean, p$sd)
  draws <- draws[draws >= p$a & draws <= p$b]
  expect_equal(round(mean(draws)), pinned)
})

test_that("delta_reporting_deaths is PINNED by default and still drawable on request", {
  skip_if_no_delta_deaths_prior()
  pinned <- MOSAIC::config_default$delta_reporting_deaths
  for (s in c(3L, 19L, 777L)) {
    cfg <- sample_parameters(seed = s, verbose = FALSE)
    expect_equal(cfg$delta_reporting_deaths, pinned, info = sprintf("seed %d", s))
  }
  # Setting the flag TRUE must restore an integer draw inside the prior support.
  draws <- vapply(1:30, function(s) {
    sample_parameters(seed = s, verbose = FALSE,
                      sample_args = list(sample_delta_reporting_deaths = TRUE)
    )$delta_reporting_deaths
  }, numeric(1))
  expect_true(all(draws == as.integer(draws)))
  expect_true(all(draws >= 1 & draws <= 14))
  expect_gt(stats::sd(draws), 0)   # guard: a pinned default would give sd == 0
})
