# Deep review (suitability-15): check_affine_normalization() documented a 1e-8
# tolerance while using 1e-2, and returned the value of its if/else instead of
# the documented invisible(NULL).

test_that("check_affine_normalization returns invisible NULL on success", {
     x <- c(-1, -0.5, 0, 0.5, 1)
     res <- withVisible(MOSAIC::check_affine_normalization(x))
     expect_null(res$value)
     expect_false(res$visible)
})

test_that("check_affine_normalization tolerance is the documented 1e-2", {
     expect_silent(MOSAIC::check_affine_normalization(c(-1, 0, 1) + 0.005))
     expect_error(MOSAIC::check_affine_normalization(c(-1, 0, 1) + 0.05), "Mean is not zero")
     expect_error(MOSAIC::check_affine_normalization(c(-1.5, 0.5, 1)), "Min is less than -1")
})
