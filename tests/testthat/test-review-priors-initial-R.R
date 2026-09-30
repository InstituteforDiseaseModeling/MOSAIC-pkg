# Regression tests (deep review, priors group) for est_initial_R().

.fake_initial_R_paths <- function() {
     root <- withr::local_tempdir(.local_envir = parent.frame())
     who_dir <- file.path(root, "processed", "WHO", "annual")
     dem_dir <- file.path(root, "demographics")
     dir.create(who_dir, recursive = TRUE)
     dir.create(dem_dir, recursive = TRUE)
     utils::write.csv(data.frame(iso_code = "ETH", year = 2020:2022,
                                 cases_total = c(2000, 1500, 1000)),
                      file.path(who_dir, "who_afro_annual.csv"), row.names = FALSE)
     utils::write.csv(data.frame(iso_code = "ETH", year = 2020:2023,
                                 total_population = 1e8),
                      file.path(dem_dir, "UN_world_population_prospects_annual.csv"),
                      row.names = FALSE)
     list(DATA_PROCESSED = file.path(root, "processed"), DATA_DEMOGRAPHICS = dem_dir)
}

.initial_R_mean <- function(PATHS, priors) {
     set.seed(1)
     out <- suppressWarnings(est_initial_R(PATHS, priors, list(location_name = "ETH"),
                                           n_samples = 200, t0 = as.Date("2023-01-01"),
                                           disaggregate = FALSE, verbose = FALSE))
     p <- out$parameters_location$prop_R_initial$parameters$location$ETH
     p$shape1 / (p$shape1 + p$shape2)
}

test_that("est_initial_R samples the global rho prior (not a hardcoded 0.1)", {
     # Before v0.99.11 est_initial_R read priors$parameters_location$rho, which
     # does not exist, and fell back to rho = 0.1 on every draw -- so the model's
     # own rho prior had no effect on prop_R_initial.
     PATHS <- .fake_initial_R_paths()
     pr_hi <- MOSAIC::priors_default
     pr_lo <- MOSAIC::priors_default
     pr_hi$parameters_global$rho <- list(distribution = "beta",
                                         parameters = list(shape1 = 5e4, shape2 = 5e4))
     pr_lo$parameters_global$rho <- list(distribution = "beta",
                                         parameters = list(shape1 = 1e4, shape2 = 9e4))
     m_hi <- .initial_R_mean(PATHS, pr_hi)   # rho ~ 0.5
     m_lo <- .initial_R_mean(PATHS, pr_lo)   # rho ~ 0.1
     # infections = cases * chi / (rho * sigma): R scales ~ 1/rho
     expect_gt(m_lo / m_hi, 3.5)
     expect_lt(m_lo / m_hi, 6.5)
})

test_that("est_initial_R warns instead of silently hardcoding a missing rho prior", {
     PATHS <- .fake_initial_R_paths()
     pr <- MOSAIC::priors_default
     pr$parameters_global[["rho"]] <- NULL
     expect_warning(
          est_initial_R(PATHS, pr, list(location_name = "ETH"), n_samples = 20,
                        t0 = as.Date("2023-01-01"), disaggregate = FALSE, verbose = FALSE),
          "rho"
     )
})

test_that("disaggregation reads the shipped a_1_j/b_1_j/a_2_j/b_2_j priors", {
     fp <- MOSAIC:::.est_initial_R_fourier_priors(MOSAIC::priors_default, "ETH")
     expect_named(fp, c("a_1_j", "b_1_j", "a_2_j", "b_2_j"))
     expect_true(all(vapply(fp, function(x) identical(x$distribution, "normal"), logical(1))))
     expect_null(MOSAIC:::.est_initial_R_fourier_priors(list(parameters_location = list()), "ETH"))
})

test_that("disaggregation weights days by the envelope 1 + f(t), period 365", {
     # Before v0.99.11 the weights were pmax(0, f(t)) with period 365.25: f is
     # zero-mean, so every case went into the ~half of the year where f > 0.
     a1 <- 0.5; b1 <- 0.2; a2 <- 0.1; b2 <- -0.1
     d <- disagg_annual_cases_to_daily(3650, 2023, a1, b1, a2, b2)
     t <- 1:365
     env <- 1 + a1 * cos(2 * pi * t / 365) + b1 * sin(2 * pi * t / 365) +
          a2 * cos(4 * pi * t / 365) + b2 * sin(4 * pi * t / 365)
     expect_equal(d$cases, 3650 * env / sum(env), tolerance = 1e-12)
     expect_true(all(d$cases > 0))
     # Flat seasonality spreads cases uniformly
     u <- disagg_annual_cases_to_daily(365, 2023, 0, 0, 0, 0)
     expect_equal(u$cases, rep(1, 365))
})
