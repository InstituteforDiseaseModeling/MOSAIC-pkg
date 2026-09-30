# Regression tests (deep review, priors group) for est_WASH_coverage() helpers.

test_that("WASH imputation pairs each similarity weight with its own country's value", {
     # Before v0.100.0 values were taken in wash_data row order while the
     # weights were sorted by similarity, so weights and values were mispaired.
     values <- c(AAA = 0.1, BBB = 0.5, CCC = 0.9, DDD = 0.3)
     sim <- c(CCC = 0.9, AAA = 0.1, BBB = 0.5, DDD = 0.05)
     expected <- (0.9 * 0.9 + 0.5 * 0.5 + 0.1 * 0.1) / (0.9 + 0.5 + 0.1)
     expect_equal(MOSAIC:::.wash_impute_from_similar(values, sim), expected)
})

test_that("WASH imputation works with fewer than three similar countries", {
     values <- c(AAA = 0.2, BBB = 0.6)
     sim <- c(BBB = 0.8, AAA = 0.2)
     expect_equal(MOSAIC:::.wash_impute_from_similar(values, sim),
                  (0.8 * 0.6 + 0.2 * 0.2) / 1.0)
     expect_true(is.na(MOSAIC:::.wash_impute_from_similar(values, c(ZZZ = 1))))
})
