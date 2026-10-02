# plot_CFR_by_country(): regression tests for the Beta-density panel, whose
# uniform 1,000-point grid on [0, 1] (spacing ~0.001) could not draw the AFRO
# Region density (SD ~1.2e-4 from ~1.4 million cases): every grid point fell in
# its tails and the curve rendered as a flat line.

# Shapes from case_fatality_ratio_2014_2025.csv (MOSAIC-data, 2026-09): the AFRO
# pool, the highest-CFR country (Congo) and a zero-death country (Liberia, a
# J-shaped Beta(0.5, n + 0.5) unbounded at 0).
.cfr_shapes <- data.frame(country = c("AFRO Region", "Congo", "Liberia"),
                          shape1 = c(28129.1793, 73.02946, 0.5),
                          shape2 = c(1412325.180, 842.4485, 580.5),
                          stringsAsFactors = FALSE)

.trapezoid <- function(x, y) sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1)) / 2)

test_that("the adaptive grid resolves the narrow AFRO density", {
     d <- MOSAIC:::.cfr_beta_density_grid(.cfr_shapes)
     expect_setequal(unique(d$country), .cfr_shapes$country)
     expect_true(all(is.finite(d$density)))

     afro <- d[d$country == "AFRO Region", ]
     a <- .cfr_shapes$shape1[1]; b <- .cfr_shapes$shape2[1]
     peak <- stats::dbeta((a - 1) / (a + b - 2), a, b)
     expect_gt(max(afro$density), 0.99 * peak)
     # The curve integrates to ~1 on the grid, i.e. its whole bulk is sampled
     expect_gt(.trapezoid(afro$x, afro$density), 0.999)
     congo <- d[d$country == "Congo", ]
     expect_gt(.trapezoid(congo$x, congo$density), 0.999)

     # The old uniform grid on [0, 1] missed the AFRO bulk entirely
     x_old <- seq(0, 1, length.out = 1000)
     expect_lt(max(stats::dbeta(x_old, a, b)), 0.01 * peak)
})

test_that("the grid spans the densities' mass and keeps J-shaped curves finite", {
     d <- MOSAIC:::.cfr_beta_density_grid(.cfr_shapes)
     x_max <- 1.1 * max(stats::qbeta(1 - 1e-4, .cfr_shapes$shape1, .cfr_shapes$shape2))
     expect_equal(max(d$x), x_max)
     expect_equal(min(d$x), 0)
     lib <- d[d$country == "Liberia", ]
     # unbounded at 0: drawn from the first grid point above 0, not from 0
     expect_gt(min(lib$x), 0)
     expect_equal(min(lib$x), x_max / 999)
     expect_equal(max(lib$density), stats::dbeta(x_max / 999, 0.5, 580.5))
     # an x range already reaching 1 is capped there
     wide <- MOSAIC:::.cfr_beta_density_grid(data.frame(country = "U", shape1 = 1, shape2 = 1))
     expect_equal(range(wide$x), c(0, 1))
     expect_equal(nrow(MOSAIC:::.cfr_beta_density_grid(.cfr_shapes[0, ])), 0L)
})

test_that("plot_CFR_by_country() draws the AFRO density and returns both plots", {
     local_null_device()
     who <- withr::local_tempdir()
     fig <- withr::local_tempdir()
     cfr <- data.frame(
          country = c("AFRO Region", "Angola", "Congo", "Liberia", "Mozambique"),
          iso_code = c("AFRO", "AGO", "COG", "LBR", "MOZ"),
          cases_total = c(1444871, 38984, 970, 580, 91969),
          deaths_total = c(28215, 969, 77, 0, 392),
          stringsAsFactors = FALSE)
     cfr$shape1 <- c(28129.1793, 953.6379, 73.02946, 0.5, 382.5905)
     cfr$shape2 <- c(1412325.180, 37395.542, 842.4485, 580.5, 89277.36)
     cfr$cfr <- c(0.019527695, 0.024856351, 0.07938144, 0, 0.004262306)
     cfr$cfr_lo <- stats::qbeta(0.025, cfr$shape1, cfr$shape2)
     cfr$cfr_hi <- stats::qbeta(0.975, cfr$shape1, cfr$shape2)
     utils::write.csv(cfr, file.path(who, "case_fatality_ratio_2014_2025.csv"), row.names = FALSE)

     res <- suppressMessages(plot_CFR_by_country(list(DATA_WHO_ANNUAL = who, DOCS_FIGURES = fig)))
     expect_named(res, c("cfr_and_cases", "beta_distributions"))
     expect_s3_class(res$cfr_and_cases, "ggplot")
     expect_s3_class(res$beta_distributions, "ggplot")
     expect_true(file.exists(file.path(fig, "case_fatality_ratio_and_cases_total_by_country.png")))
     expect_true(file.exists(file.path(fig, "case_fatality_ratio_beta_distributions.png")))

     bd <- res$beta_distributions$data
     expect_setequal(unique(as.character(bd$country)), c("AFRO Region", "Liberia", "Congo"))
     afro <- bd[bd$country == "AFRO Region", ]
     expect_gt(max(afro$density), 3000)          # analytic peak ~3,460
     expect_identical(res$beta_distributions$labels$y, "Density (square-root scale)")
     y_scale <- ggplot2::ggplot_build(res$beta_distributions)$plot$scales$get_scales("y")
     expect_identical(y_scale$trans$name %||% y_scale$get_transformation()$name, "sqrt")
})
