# Reproducibility of the initial-condition estimators under an explicit seed
# (make_priors_default.R passes one so a rebuild gives byte-identical priors).

.seed_fake_EI_paths <- function(envir = parent.frame()) {
     root <- withr::local_tempdir(.local_envir = envir)
     dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 90)
     utils::write.csv(data.frame(date = dates, iso_code = "TCD",
                                 cases = rep(c(2, 5, 0, 7, 3), length.out = 90)),
                      file.path(root, "cholera_surveillance_daily_combined.csv"), row.names = FALSE)
     utils::write.csv(data.frame(date = as.Date("2024-01-15"), iso_code = "TCD",
                                 total_population = 1e6),
                      file.path(root, "UN_world_population_prospects_daily.csv"), row.names = FALSE)
     list(DATA_CHOLERA_DAILY = root, DATA_DEMOGRAPHICS = root)
}

.seed_fake_R_paths <- function(envir = parent.frame()) {
     root <- withr::local_tempdir(.local_envir = envir)
     who_dir <- file.path(root, "processed", "WHO", "annual")
     dem_dir <- file.path(root, "demographics")
     dir.create(who_dir, recursive = TRUE)
     dir.create(dem_dir, recursive = TRUE)
     utils::write.csv(data.frame(iso_code = rep(c("ETH", "MOZ"), each = 3), year = rep(2020:2022, 2),
                                 cases_total = c(2000, 1500, 1000, 800, 3000, 400)),
                      file.path(who_dir, "who_afro_annual.csv"), row.names = FALSE)
     utils::write.csv(data.frame(iso_code = rep(c("ETH", "MOZ"), each = 4), year = rep(2020:2023, 2),
                                 total_population = rep(c(1e8, 3e7), each = 4)),
                      file.path(dem_dir, "UN_world_population_prospects_annual.csv"), row.names = FALSE)
     list(DATA_PROCESSED = file.path(root, "processed"), DATA_DEMOGRAPHICS = dem_dir)
}

.seed_EI <- function(PATHS, seed, n = 50, parallel = FALSE) {
     suppressWarnings(est_initial_E_I(PATHS, MOSAIC::priors_default,
                                      list(location_name = "TCD", date_start = "2024-03-01"),
                                      n_samples = n, t0 = as.Date("2024-03-01"), lookback_days = 30,
                                      verbose = FALSE, parallel = parallel, variance_inflation = 10,
                                      seed = seed))$parameters_location
}

test_that("est_initial_E_I with a seed is reproducible and leaves the caller's RNG alone", {
     PATHS <- .seed_fake_EI_paths()
     set.seed(99); before <- .Random.seed
     a <- .seed_EI(PATHS, seed = 123)
     expect_identical(.Random.seed, before)
     b <- .seed_EI(PATHS, seed = 123)
     expect_identical(a, b)
     c <- .seed_EI(PATHS, seed = 124)
     expect_false(identical(a$prop_E_initial$parameters$location$TCD$shape1,
                            c$prop_E_initial$parameters$location$TCD$shape1))
})

test_that("est_initial_E_I with a seed gives the same result with and without parallel", {
     skip_on_os("windows")
     PATHS <- .seed_fake_EI_paths()
     expect_identical(.seed_EI(PATHS, seed = 7, n = 100, parallel = FALSE),
                      .seed_EI(PATHS, seed = 7, n = 100, parallel = TRUE))
})

test_that("est_initial_R with a seed is reproducible, order- and parallel-independent", {
     PATHS <- .seed_fake_R_paths()
     run <- function(locs, parallel = FALSE) {
          suppressWarnings(est_initial_R(PATHS, MOSAIC::priors_default, list(location_name = locs),
                                         n_samples = 40, t0 = as.Date("2023-01-01"),
                                         disaggregate = FALSE, verbose = FALSE,
                                         parallel = parallel, seed = 11)
          )$parameters_location$prop_R_initial$parameters$location
     }
     set.seed(5); before <- .Random.seed
     a <- run(c("ETH", "MOZ"))
     expect_identical(.Random.seed, before)
     expect_identical(a, run(c("ETH", "MOZ")))
     # A location's draws depend on the seed and its ISO code only.
     expect_identical(a$MOZ$shape1, run(c("MOZ", "ETH"))$MOZ$shape1)
})

test_that("est_initial_S with a seed is reproducible", {
     cfg <- list(location_name = c("ETH", "MOZ"), date_start = "2023-01-01")
     s1 <- suppressWarnings(est_initial_S(list(), MOSAIC::priors_default, cfg, n_samples = 50,
                                          verbose = FALSE, seed = 3))
     s2 <- suppressWarnings(est_initial_S(list(), MOSAIC::priors_default, cfg, n_samples = 50,
                                          verbose = FALSE, seed = 3))
     expect_identical(s1$parameters_location, s2$parameters_location)
})

test_that("seed arguments are validated", {
     expect_error(est_initial_S(list(), MOSAIC::priors_default,
                                list(location_name = "ETH", date_start = "2023-01-01"),
                                seed = "a"), "seed")
})

test_that("derived per-location seeds are distinct for every ISO code", {
     isos <- unique(c(MOSAIC::iso_codes_mosaic, MOSAIC::iso_codes_africa))
     for (base in c(1L, 20260930L)) {
          seeds <- vapply(isos, function(k) MOSAIC:::.mosaic_derive_seed(base, k), integer(1))
          expect_false(anyNA(seeds))
          expect_equal(length(unique(seeds)), length(isos))
     }
     # Different base seeds give different seeds for the same key.
     expect_false(MOSAIC:::.mosaic_derive_seed(1, "ETH") == MOSAIC:::.mosaic_derive_seed(2, "ETH"))
})
