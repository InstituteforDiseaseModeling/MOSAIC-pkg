# Tests for plot_Reff() -- the R_eff time-series renderer.
#
# Covers: returns a ggplot for the CI-available case (ribbon drawn) and the
# CI-NA case (ribbon suppressed, no error), single + multi-location, and
# warm-up NA leading-row handling.

# -----------------------------------------------------------------------------
# Synthetic reproductive_numbers fixtures (mirror calc_Reff() schema)
# -----------------------------------------------------------------------------
make_reff_df <- function(locs = "LOC1", Tn = 60L, ci = TRUE,
                         ci_source = NULL, n_warmup_na = 2L) {
  d0 <- as.Date("2023-01-01")
  parts <- lapply(locs, function(loc) {
    central <- 1 + 0.5 * sin(seq_len(Tn) / 8)
    if (n_warmup_na > 0L)
      central[seq_len(min(n_warmup_na, Tn))] <- NA_real_
    df <- data.frame(
      location = loc,
      date     = d0 + (seq_len(Tn) - 1L),
      t        = seq_len(Tn),
      estimand = "R_eff",
      central  = central,
      stringsAsFactors = FALSE)
    if (ci) {
      df$q2.5  <- central - 0.3
      df$q25   <- central - 0.1
      df$q50   <- central
      df$q75   <- central + 0.1
      df$q97.5 <- central + 0.3
    } else {
      df$q2.5  <- NA_real_; df$q25 <- NA_real_; df$q50 <- NA_real_
      df$q75   <- NA_real_; df$q97.5 <- NA_real_
    }
    df
  })
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  if (is.null(ci_source))
    ci_source <- if (ci) "weighted_quantiles_per_member" else
      "unavailable_strided_lines"
  attr(out, "ci_source") <- ci_source
  class(out) <- c("reproductive_numbers", "data.frame")
  out
}

test_that("plot_Reff returns a ggplot when the CI is available (ribbon drawn)", {
  reff <- make_reff_df(ci = TRUE)
  p <- plot_Reff(reff)
  expect_s3_class(p, "ggplot")
  # A ribbon layer is present when q* are non-NA.
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true(any(grepl("Ribbon", geoms)))
  expect_true(any(grepl("Line", geoms)))
})

test_that("plot_Reff suppresses the ribbon when the CI is all-NA (no error)", {
  reff <- make_reff_df(ci = FALSE, ci_source = "unavailable_strided_lines")
  expect_silent(p <- plot_Reff(reff))
  expect_s3_class(p, "ggplot")
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false(any(grepl("Ribbon", geoms)))   # no ribbon
  expect_true(any(grepl("Line", geoms)))       # central line still drawn
  # Caption notes the unavailable CI.
  expect_match(p$labels$caption, "unavailable", ignore.case = TRUE)
})

test_that("plot_Reff facets multi-location input and titles single-location", {
  reff_multi <- make_reff_df(locs = c("LOC1", "LOC2", "LOC3"), ci = TRUE)
  p_multi <- plot_Reff(reff_multi)
  expect_s3_class(p_multi, "ggplot")
  expect_s3_class(p_multi$facet, "FacetWrap")

  reff_single <- make_reff_df(locs = "MOZ", ci = TRUE)
  p_single <- plot_Reff(reff_single)
  expect_match(p_single$labels$title, "MOZ")
})

test_that("plot_Reff drops leading warm-up NA rows without erroring", {
  reff <- make_reff_df(ci = TRUE, n_warmup_na = 5L)
  p <- plot_Reff(reff)
  expect_s3_class(p, "ggplot")
  # The built data starts at the first finite central (warm-up trimmed).
  built <- ggplot2::ggplot_build(p)
  expect_true(nrow(built$data[[1]]) > 0L)
})

test_that("plot_Reff honors show_iqr = FALSE (only 95% ribbon)", {
  reff <- make_reff_df(ci = TRUE)
  p_iqr  <- plot_Reff(reff, show_iqr = TRUE)
  p_noiqr <- plot_Reff(reff, show_iqr = FALSE)
  n_ribbon <- function(p)
    sum(grepl("Ribbon", vapply(p$layers, function(l) class(l$geom)[1], character(1))))
  expect_equal(n_ribbon(p_iqr), 2L)
  expect_equal(n_ribbon(p_noiqr), 1L)
})

test_that("plot_Reff defaults to median + 95% only (purple, no 50% band)", {
  reff <- make_reff_df(ci = TRUE)
  p <- plot_Reff(reff)   # show_iqr defaults FALSE now
  n_ribbon <- sum(grepl("Ribbon",
    vapply(p$layers, function(l) class(l$geom)[1], character(1))))
  expect_equal(n_ribbon, 1L)   # only the 95% ribbon
  # The median line is purple (#762A83) and present.
  line_layers <- Filter(function(l) inherits(l$geom, "GeomLine"), p$layers)
  expect_true(length(line_layers) >= 1L)
  cols <- vapply(line_layers, function(l) {
    cc <- l$aes_params$colour
    if (is.null(cc)) NA_character_ else as.character(cc)
  }, character(1))
  expect_true("#762A83" %in% cols)
})

test_that("plot_Reff plots q50 as the median when present", {
  reff <- make_reff_df(ci = TRUE)
  # Make q50 distinguishable from central so we can confirm q50 is plotted.
  reff$q50 <- reff$central + 0.25
  p <- plot_Reff(reff)
  built <- ggplot2::ggplot_build(p)
  # The purple median line layer's y should track q50 (central + 0.25), not central.
  line_idx <- which(vapply(p$layers,
    function(l) inherits(l$geom, "GeomLine"), logical(1)))
  ld <- built$data[[line_idx[length(line_idx)]]]
  ld <- ld[is.finite(ld$y), , drop = FALSE]
  pd <- reff[order(reff$date), , drop = FALSE]
  pd <- pd[is.finite(pd$central), , drop = FALSE]
  expect_equal(unname(ld$y), unname(pd$q50), tolerance = 1e-6)
})

test_that("plot_Reff validates input", {
  expect_error(plot_Reff(list(a = 1)), "data.frame")
  expect_error(plot_Reff(data.frame(x = 1)), "missing required column")
  empty <- make_reff_df(ci = TRUE)[0, ]
  expect_error(plot_Reff(empty), "zero rows")
})
