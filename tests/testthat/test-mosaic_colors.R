# =============================================================================
# Tests for MOSAIC Color Palette Framework
# =============================================================================

# --- mosaic_colors() ---

test_that("mosaic_colors returns valid hex colors", {
  cols <- mosaic_colors()
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
  expect_true(length(cols) > 0)
})

test_that("mosaic_colors returns named vector", {
  cols <- mosaic_colors()
  expect_false(is.null(names(cols)))
  expect_true("cases" %in% names(cols))
  expect_true("deaths" %in% names(cols))
  expect_true("data" %in% names(cols))
})

test_that("mosaic_colors subsets by name", {
  cols <- mosaic_colors("cases", "deaths")
  expect_length(cols, 2)
  expect_equal(names(cols), c("cases", "deaths"))
})

test_that("mosaic_colors returns set by name", {
  comp <- mosaic_colors("compartments")
  expect_true("S" %in% names(comp))
  expect_true("I" %in% names(comp))
  expect_length(comp, 7)

  cal <- mosaic_colors("calibration")
  expect_length(cal, 2)
  expect_equal(names(cal), c("Prior", "Posterior"))

  stat <- mosaic_colors("status")
  expect_length(stat, 4)
})

test_that("mosaic_colors looks up compartment names directly", {
  cols <- mosaic_colors("S", "E", "I", "R")
  expect_length(cols, 4)
  expect_equal(names(cols), c("S", "E", "I", "R"))
})

test_that("mosaic_colors warns on unknown names", {
  expect_warning(mosaic_colors("nonexistent"), "Unknown color name")
})

test_that("core semantic colors are stable", {
  cols <- mosaic_colors()
  expect_equal(unname(cols["mosaic_blue"]), "#0167AF")
  expect_equal(unname(cols["deaths"]), "#B5123B")
  expect_equal(unname(cols["data"]), "#2D2D2D")
})


# --- mosaic_palette() ---

test_that("mosaic_palette dispatches correctly", {
  d <- mosaic_palette(5, "discrete")
  expect_length(d, 5)

  s <- mosaic_palette(7, "sequential")
  expect_length(s, 7)

  div <- mosaic_palette(9, "diverging")
  expect_length(div, 9)
})


# --- mosaic_pal_discrete() ---

test_that("mosaic_pal_discrete returns correct n", {
  for (n in c(1, 3, 7, 12, 20)) {
    cols <- mosaic_pal_discrete(n)
    expect_length(cols, n)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}", cols)))
  }
})

test_that("mosaic_pal_discrete direction reversal works", {
  fwd <- mosaic_pal_discrete(5)
  rev_cols <- mosaic_pal_discrete(5, direction = -1)
  expect_equal(fwd, rev(rev_cols))
})


# --- mosaic_pal_sequential() ---

test_that("mosaic_pal_sequential returns correct n", {
  cols <- mosaic_pal_sequential(10, "blues")
  expect_length(cols, 10)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
})

test_that("mosaic_pal_sequential all palettes work", {
  for (pal in c("blues", "reds", "greens", "teals", "heat", "grays")) {
    cols <- mosaic_pal_sequential(5, pal)
    expect_length(cols, 5)
  }
})

test_that("mosaic_pal_sequential direction reversal works", {
  fwd <- mosaic_pal_sequential(5, "blues", direction = 1)
  rev_cols <- mosaic_pal_sequential(5, "blues", direction = -1)
  expect_equal(fwd, rev(rev_cols))
})

test_that("mosaic_pal_sequential begin/end subsetting works", {
  full <- mosaic_pal_sequential(5, "blues")
  sub <- mosaic_pal_sequential(5, "blues", begin = 0.2, end = 0.8)
  expect_length(sub, 5)
  expect_false(identical(full, sub))
})


# --- mosaic_pal_diverging() ---

test_that("mosaic_pal_diverging returns correct n", {
  cols <- mosaic_pal_diverging(7, "blue_red")
  expect_length(cols, 7)
})

test_that("mosaic_pal_diverging has neutral midpoint for odd n", {
  cols <- mosaic_pal_diverging(7, "blue_red")
  mid_rgb <- grDevices::col2rgb(cols[4]) / 255
  expect_true(max(mid_rgb) - min(mid_rgb) < 0.05)
})

test_that("mosaic_pal_diverging all palettes work", {
  for (pal in c("blue_red", "blue_orange", "green_purple")) {
    cols <- mosaic_pal_diverging(5, pal)
    expect_length(cols, 5)
  }
})


# --- mosaic_pal_uncertainty() ---

test_that("mosaic_pal_uncertainty returns correct n_levels", {
  cols <- mosaic_pal_uncertainty("#0167AF", n_levels = 3)
  expect_length(cols, 3)
  expect_equal(names(cols), c("50%", "75%", "95%"))
})

test_that("uncertainty levels get progressively lighter", {
  cols <- mosaic_pal_uncertainty("#0167AF", n_levels = 3)
  sums <- colSums(grDevices::col2rgb(cols))
  expect_true(all(diff(sums) > 0))
})

test_that("mosaic_pal_uncertainty works with any base color", {
  for (base in c("#0167AF", "#B5123B", "#009988", "#228833")) {
    cols <- mosaic_pal_uncertainty(base)
    expect_length(cols, 3)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
  }
})

test_that("mosaic_pal_uncertainty accepts custom labels", {
  cols <- mosaic_pal_uncertainty(labels = c("a", "b", "c"))
  expect_equal(names(cols), c("a", "b", "c"))
})


# --- mosaic_pal_scenarios() ---

test_that("mosaic_pal_scenarios returns correct n", {
  for (n in c(2, 5, 8, 12, 16, 20)) {
    cols <- mosaic_pal_scenarios(n)
    expect_length(cols, n)
  }
})

test_that("mosaic_pal_scenarios baseline is always MOSAIC blue", {
  for (n in c(2, 8, 16, 20)) {
    cols <- mosaic_pal_scenarios(n)
    expect_equal(unname(cols[1]), "#0167AF")
  }
})

test_that("mosaic_pal_scenarios accepts labels", {
  cols <- mosaic_pal_scenarios(3, labels = c("Base", "Vax", "WASH"))
  expect_equal(names(cols), c("Base", "Vax", "WASH"))
})


# --- mosaic_pal_countries() ---

test_that("mosaic_pal_countries returns 40 colors by default", {
  cols <- mosaic_pal_countries()
  expect_equal(length(cols), length(MOSAIC::iso_codes_mosaic))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
  expect_true("ETH" %in% names(cols))
})

test_that("mosaic_pal_countries subsets by iso_codes", {
  cols <- mosaic_pal_countries(c("ETH", "KEN", "TZA"))
  expect_length(cols, 3)
  expect_equal(names(cols), c("ETH", "KEN", "TZA"))
})

test_that("mosaic_pal_countries group_by_region FALSE works", {
  cols <- mosaic_pal_countries(group_by_region = FALSE)
  expect_equal(length(cols), length(MOSAIC::iso_codes_mosaic))
})


# --- mosaic_color_variant() ---

test_that("mosaic_color_variant lighten produces lighter colors", {
  lighter <- mosaic_color_variant("#0167AF", "lighten", 0.3)
  expect_true(grepl("^#[0-9A-Fa-f]{6}$", lighter))
  expect_gt(sum(grDevices::col2rgb(lighter)), sum(grDevices::col2rgb("#0167AF")))
})

test_that("mosaic_color_variant darken produces darker colors", {
  darker <- mosaic_color_variant("#0167AF", "darken", 0.3)
  expect_lt(sum(grDevices::col2rgb(darker)), sum(grDevices::col2rgb("#0167AF")))
})

test_that("mosaic_color_variant works with vectors", {
  cols <- mosaic_color_variant(c("#0167AF", "#B5123B"), "lighten", 0.3)
  expect_length(cols, 2)
})


# --- theme_mosaic() ---

test_that("theme_mosaic returns a ggplot2 theme", {
  skip_if_not_installed("ggplot2")
  th <- theme_mosaic()
  expect_s3_class(th, "theme")
})

test_that("theme_mosaic grid options work", {
  skip_if_not_installed("ggplot2")
  expect_s3_class(theme_mosaic(grid = "none"), "theme")
  expect_s3_class(theme_mosaic(grid = "both"), "theme")
  expect_s3_class(theme_mosaic(grid = "major"), "theme")
})


# --- ggplot2 scales ---

test_that("scale functions return ggplot2 scale objects", {
  skip_if_not_installed("ggplot2")
  expect_s3_class(scale_color_mosaic_d(), "Scale")
  expect_s3_class(scale_fill_mosaic_d(), "Scale")
  expect_s3_class(scale_color_mosaic_c(), "Scale")
  expect_s3_class(scale_fill_mosaic_c(), "Scale")
  expect_s3_class(scale_color_mosaic_div(), "Scale")
  expect_s3_class(scale_fill_mosaic_div(), "Scale")
  expect_s3_class(scale_color_mosaic_scenario(), "Scale")
  expect_s3_class(scale_fill_mosaic_scenario(), "Scale")
})
