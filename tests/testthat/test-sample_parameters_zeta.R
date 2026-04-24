library(testthat)
library(MOSAIC)

# Set root directory (required for sample_parameters to load defaults)
tryCatch(set_root_directory("~/MOSAIC"), error = function(e) NULL)

test_that("sample_parameters produces valid zeta tuples under new priors", {
     skip_if(is.null(getOption("root_directory")), "MOSAIC root directory not set")
     cfg <- sample_parameters(seed = 1L, verbose = FALSE)
     expect_true(is.finite(cfg$zeta_1))
     expect_true(is.finite(cfg$zeta_2))
     expect_true(is.finite(cfg$zeta_ratio))
     # zeta_2 = zeta_1 / zeta_ratio exact
     expect_equal(cfg$zeta_2,
                  cfg$zeta_1 / cfg$zeta_ratio,
                  tolerance = 1e-10)
})

test_that("zeta draws fall in expected ranges in >=95% of draws", {
     skip_if(is.null(getOption("root_directory")), "MOSAIC root directory not set")
     draws <- lapply(1:100, function(s) sample_parameters(seed = s, verbose = FALSE))
     z1  <- vapply(draws, function(d) d$zeta_1,     numeric(1))
     zr  <- vapply(draws, function(d) d$zeta_ratio, numeric(1))
     expect_gte(mean(z1 > 1e9  & z1 < 1e14), 0.95)
     expect_gte(mean(zr > 1e0  & zr < 1e10), 0.95)
})
