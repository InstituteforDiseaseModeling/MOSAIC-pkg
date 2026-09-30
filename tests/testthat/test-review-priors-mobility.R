# Regression test (deep review, priors group): est_mobility() partial-coverage
# OD sources restrict M to an intersection; D and N must follow.

test_that("D and N are aligned to the (possibly restricted) mobility matrix", {
     iso <- c("AGO", "BDI", "BEN")
     D <- matrix(1:9, 3, 3, dimnames = list(origin = iso, destination = iso))
     N <- c(AGO = 10L, BDI = 20L, BEN = 30L)
     out <- MOSAIC:::.est_mobility_align_to(c("BEN", "AGO"), D, N)
     expect_equal(dimnames(out$D), list(origin = c("BEN", "AGO"), destination = c("BEN", "AGO")))
     expect_equal(out$D["BEN", "AGO"], D["BEN", "AGO"])
     expect_equal(out$N, c(BEN = 30L, AGO = 10L))
     expect_error(MOSAIC:::.est_mobility_align_to(c("AGO", "ZZZ"), D, N), "ZZZ")
})
