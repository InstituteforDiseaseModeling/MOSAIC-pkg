# Tests for plot_Reff() -- the phase-coherent R_eff time-series renderer.
#
# Covers the NEW estimand schema: the headline line is the MEDOID trajectory
# (`central`, coherent), the 95% band (q2.5-q97.5) is the faint per-calendar-date
# cross-member range, and the per-member peak R_t annotation comes from attr
# `peak_Rt`. Asserts: medoid line renders, faint band + caption present,
# peak annotation appears when peak_Rt is set and is omitted when NULL,
# graceful all-NA handling, single + multi-location, warm-up NA trimming.

# -----------------------------------------------------------------------------
# Synthetic reproductive_numbers fixtures (mirror the NEW calc_Reff() schema)
# -----------------------------------------------------------------------------
make_reff_df <- function(locs = "LOC1", Tn = 60L, ci = TRUE,
                         ci_source = NULL, n_warmup_na = 2L,
                         peak = TRUE) {
  d0 <- as.Date("2023-01-01")
  parts <- lapply(locs, function(loc) {
    # `central` = medoid trajectory R_t: a coherent peak around 2.5.
    central <- 1 + 1.5 * exp(-((seq_len(Tn) - 25) / 8)^2)
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
      # Per-calendar-date cross-member envelope (deliberately flatter than the
      # medoid peak: phase-misaligned member peaks pull the per-date range down).
      env <- 1 + 0.4 * exp(-((seq_len(Tn) - 25) / 14)^2)
      df$q2.5  <- env - 0.25
      df$q25   <- env - 0.1
      df$q50   <- env
      df$q75   <- env + 0.1
      df$q97.5 <- env + 0.25
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
  attr(out, "ci_source")          <- ci_source
  attr(out, "central_definition") <- "medoid_trajectory"
  attr(out, "band_definition")    <-
    "per_calendar_day_cross_member_weighted_quantiles"
  if (isTRUE(peak)) {
    attr(out, "peak_Rt") <- data.frame(
      location = locs,
      q2.5     = rep(2.1, length(locs)),
      q50      = rep(2.8, length(locs)),
      q97.5    = rep(3.4, length(locs)),
      n_members = rep(50L, length(locs)),
      stringsAsFactors = FALSE)
  }
  class(out) <- c("reproductive_numbers", "data.frame")
  out
}

test_that("plot_Reff renders the medoid central line as the headline", {
  reff <- make_reff_df(ci = TRUE)
  # smooth_days = 1 -> headline line is the raw medoid `central` (no smoothing),
  # so we can assert it tracks `central` exactly rather than a rolling mean.
  p <- plot_Reff(reff, smooth_days = 1L)
  expect_s3_class(p, "ggplot")
  line_idx <- which(vapply(p$layers,
    function(l) inherits(l$geom, "GeomLine"), logical(1)))
  expect_true(length(line_idx) >= 1L)
  built <- ggplot2::ggplot_build(p)
  ld <- built$data[[line_idx[length(line_idx)]]]
  ld <- ld[is.finite(ld$y), , drop = FALSE]
  src <- reff[order(reff$date), , drop = FALSE]
  src <- src[is.finite(src$central), , drop = FALSE]
  expect_equal(unname(ld$y), unname(src$central), tolerance = 1e-6)
  # Confirm it is NOT tracking q50 (medoid peak is well above the envelope).
  expect_gt(max(ld$y), max(src$q50) + 0.5)
})

test_that("plot_Reff smooths the headline line by default but keeps raw daily", {
  reff <- make_reff_df(ci = TRUE)
  p <- plot_Reff(reff, smooth_days = 14L)
  line_idx <- which(vapply(p$layers,
    function(l) inherits(l$geom, "GeomLine"), logical(1)))
  # Two line geoms: faint raw daily `central` + bold smoothed `central_smooth`.
  expect_gte(length(line_idx), 2L)
  built <- ggplot2::ggplot_build(p)
  headline <- built$data[[line_idx[length(line_idx)]]]
  headline <- headline[is.finite(headline$y), , drop = FALSE]
  src <- reff[order(reff$date), , drop = FALSE]
  src <- src[is.finite(src$central), , drop = FALSE]
  # Smoothed peak is strictly lower than the raw daily peak.
  expect_lt(max(headline$y), max(src$central))
})

test_that("plot_Reff draws the faint 95% band and an explanatory caption", {
  reff <- make_reff_df(ci = TRUE)
  p <- plot_Reff(reff)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true(any(grepl("Ribbon", geoms)))
  # The 95% ribbon is rendered faint (low alpha).
  rib <- Filter(function(l) inherits(l$geom, "GeomRibbon"), p$layers)
  alphas <- vapply(rib, function(l) {
    a <- l$aes_params$alpha; if (is.null(a)) NA_real_ else as.numeric(a)
  }, numeric(1))
  expect_true(any(is.finite(alphas) & alphas <= 0.4))
  # Caption makes clear the band is the per-date cross-member range, not peak.
  expect_match(p$labels$caption, "ACROSS members", ignore.case = TRUE)
  expect_match(p$labels$caption, "peak", ignore.case = TRUE)
})

test_that("plot_Reff annotates the per-member peak R_t when peak_Rt is set", {
  reff_single <- make_reff_df(locs = "MOZ", ci = TRUE, peak = TRUE)
  p <- plot_Reff(reff_single)
  # Single location -> peak annotation in the subtitle.
  expect_false(is.null(p$labels$subtitle))
  expect_match(p$labels$subtitle, "Peak R_t \\(per-member\\)")
  expect_match(p$labels$subtitle, "2.80")
  expect_match(p$labels$subtitle, "\\[2.10, 3.40\\]")

  reff_multi <- make_reff_df(locs = c("LOC1", "LOC2"), ci = TRUE, peak = TRUE)
  pm <- plot_Reff(reff_multi)
  # Multi-location -> peak annotation as an in-panel geom_text layer.
  has_text <- any(vapply(pm$layers,
    function(l) inherits(l$geom, "GeomText"), logical(1)))
  expect_true(has_text)
})

test_that("plot_Reff omits the peak annotation when peak_Rt is NULL", {
  reff <- make_reff_df(locs = "MOZ", ci = TRUE, peak = FALSE)
  expect_null(attr(reff, "peak_Rt"))
  p <- plot_Reff(reff)
  expect_s3_class(p, "ggplot")
  expect_null(p$labels$subtitle)

  reff_multi <- make_reff_df(locs = c("LOC1", "LOC2"), ci = TRUE, peak = FALSE)
  pm <- plot_Reff(reff_multi)
  has_text <- any(vapply(pm$layers,
    function(l) inherits(l$geom, "GeomText"), logical(1)))
  expect_false(has_text)
})

test_that("plot_Reff suppresses the band when the CI is all-NA (no error)", {
  reff <- make_reff_df(ci = FALSE, ci_source = "unavailable_strided_lines")
  expect_silent(p <- plot_Reff(reff))
  expect_s3_class(p, "ggplot")
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false(any(grepl("Ribbon", geoms)))   # no band
  expect_true(any(grepl("Line", geoms)))       # medoid line still drawn
  expect_match(p$labels$caption, "unavailable", ignore.case = TRUE)
})

test_that("plot_Reff does not error on all-NA central", {
  reff <- make_reff_df(ci = TRUE, n_warmup_na = 0L)
  reff$central <- NA_real_
  expect_error(plot_Reff(reff), "no finite")
})

test_that("plot_Reff facets multi-location and titles single-location", {
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
  built <- ggplot2::ggplot_build(p)
  expect_true(nrow(built$data[[1]]) > 0L)
})

test_that("plot_Reff honors show_iqr = FALSE (only 95% band)", {
  reff <- make_reff_df(ci = TRUE)
  p_iqr  <- plot_Reff(reff, show_iqr = TRUE)
  p_noiqr <- plot_Reff(reff, show_iqr = FALSE)
  n_ribbon <- function(p)
    sum(grepl("Ribbon", vapply(p$layers, function(l) class(l$geom)[1], character(1))))
  expect_equal(n_ribbon(p_iqr), 2L)
  expect_equal(n_ribbon(p_noiqr), 1L)
})

test_that("plot_Reff median line is purple (#762A83)", {
  reff <- make_reff_df(ci = TRUE)
  p <- plot_Reff(reff)
  line_layers <- Filter(function(l) inherits(l$geom, "GeomLine"), p$layers)
  expect_true(length(line_layers) >= 1L)
  cols <- vapply(line_layers, function(l) {
    cc <- l$aes_params$colour
    if (is.null(cc)) NA_character_ else as.character(cc)
  }, character(1))
  expect_true("#762A83" %in% cols)
})

test_that("plot_Reff validates input", {
  expect_error(plot_Reff(list(a = 1)), "data.frame")
  expect_error(plot_Reff(data.frame(x = 1)), "missing required column")
  empty <- make_reff_df(ci = TRUE)[0, ]
  expect_error(plot_Reff(empty), "zero rows")
})
