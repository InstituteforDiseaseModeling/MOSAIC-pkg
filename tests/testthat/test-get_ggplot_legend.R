# get_ggplot_legend() under ggplot2 >= 3.5 (per-position guide-box slots) and
# for plots without a legend (single-location flux network, which errored
# "attempt to select less than one element" and silently lost the figure).

test_that("the legend is extracted for right and bottom legend positions", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = factor(gear))) +
    ggplot2::geom_point()
  for (pos in c("right", "bottom")) {
    lg <- get_ggplot_legend(p + ggplot2::theme(legend.position = pos))
    expect_s3_class(lg, "gtable")
    expect_false(inherits(lg, "zeroGrob"))
  }
})

test_that("a plot with no legend gives an empty grob, not an error", {
  empty <- data.frame(x = numeric(0), y = numeric(0), v = numeric(0))
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = empty, ggplot2::aes(x, y, color = v)) +
    ggplot2::theme_void() + ggplot2::theme(legend.position = "bottom")
  lg <- get_ggplot_legend(p)
  expect_s3_class(lg, "null")
  skip_if_not_installed("gridExtra")
  f <- withr::local_tempfile(fileext = ".png")
  grDevices::png(f); on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(gridExtra::grid.arrange(grobs = list(p, lg), ncol = 1))
})

test_that("a single-location flux network renders", {
  skip_if_not_installed("gridExtra")
  skip_if_not_installed("ggrepel")
  flux <- matrix(0, 1, 1, dimnames = list("MOZ", "MOZ"))
  coords <- matrix(c(-18.7, 35.5), 1, 2, dimnames = list("MOZ", c("latitude", "longitude")))
  f <- withr::local_tempfile(fileext = ".png")
  grDevices::png(f); on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(plot_mobility_flux_network(flux, coords, "MOZ", basemap = NULL))
})
