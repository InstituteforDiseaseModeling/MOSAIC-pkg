library(testthat)
library(MOSAIC)

# Set root directory (required for sample_parameters to load defaults)
tryCatch(set_root_directory("~/MOSAIC"), error = function(e) NULL)

test_that("sample_parameters produces valid zeta tuples under new priors", {
     skip_if(is.null(getOption("root_directory")), "MOSAIC root directory not set")
     # Structural identity (holds for any seed) -> memoized fixture.
     cfg <- .cached_sampled_config(1L)
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
     # 40 draws: the coverage bands below span several orders of magnitude
     # (>=95% within multi-OOM ranges), so 40 is ample and keeps the loop cheap.
     draws <- lapply(1:40, function(s) sample_parameters(seed = s, verbose = FALSE))
     z1  <- vapply(draws, function(d) d$zeta_1,     numeric(1))
     zr  <- vapply(draws, function(d) d$zeta_ratio, numeric(1))
     # zeta_1 ~ LN(25.65, 2.46) after v0.29.1 bias corrections.
     # 95% coverage of (1e8, 1e14) ~ 99%; old (1e9, 1e14) bound now too tight.
     expect_gte(mean(z1 > 1e8  & z1 < 1e14), 0.95)
     # zeta_ratio uses the DIRECT literature-anchor channel (LN(6.638, 4.807)),
     # whose sdlog reflects the genuine 5-OOM tension in direct literature.
     # 95% coverage range is [1e-2, 1e8] -- wider than the combined-channel
     # range because the direct channel is intentionally uninformative.
     expect_gte(mean(zr > 1e-2 & zr < 1e8), 0.95)
})
