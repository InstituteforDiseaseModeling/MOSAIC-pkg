# Deep review (suitability-11): calc_psi_star() crashed on a length-1 series
# with the default fill_method = "locf" (the loops ran over c(2, 1) and c(0, 1),
# growing the vector and then indexing psi_star[0]), and the "linear" branch
# errored whenever exactly one value was non-NA.

test_that("calc_psi_star handles a length-1 series with the default locf fill", {
     out <- MOSAIC::calc_psi_star(0.3, a = 1, b = 0)
     expect_length(out, 1L)
     expect_equal(out, 0.3, tolerance = 1e-12)

     out2 <- MOSAIC::calc_psi_star(0.3, a = 2, b = -0.5, z = 0.5)
     expect_length(out2, 1L)
     expect_equal(out2, plogis(2 * qlogis(0.3) - 0.5), tolerance = 1e-12)
})

test_that("calc_psi_star handles a length-1 NA series (falls back to plogis(b))", {
     out <- MOSAIC::calc_psi_star(NA_real_, a = 1, b = 0.4)
     expect_length(out, 1L)
     expect_equal(out, plogis(0.4), tolerance = 1e-12)
})

test_that("linear fill with a single observed value extends it as a constant", {
     psi <- c(NA, NA, 0.4, NA)
     out <- MOSAIC::calc_psi_star(psi, a = 1, b = 0, fill_method = "linear")
     expect_length(out, 4L)
     expect_equal(out, rep(0.4, 4L), tolerance = 1e-12)

     out1 <- MOSAIC::calc_psi_star(0.25, fill_method = "linear")
     expect_equal(out1, 0.25, tolerance = 1e-12)
})

test_that("locf fill is unchanged for ordinary series", {
     psi <- c(NA, 0.2, NA, 0.6, NA)
     out <- MOSAIC::calc_psi_star(psi, a = 1, b = 0)
     expect_equal(out, c(0.2, 0.2, 0.2, 0.6, 0.6), tolerance = 1e-12)
})

test_that("plain NAs do not trigger the Inf/-Inf warning; Inf still does", {
     expect_no_warning(MOSAIC::calc_psi_star(c(NA, 0.2, NA, 0.6)))
     expect_warning(MOSAIC::calc_psi_star(c(Inf, 0.2, 0.6)), "Non-finite")
})
