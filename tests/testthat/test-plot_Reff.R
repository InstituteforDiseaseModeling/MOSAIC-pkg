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
  expect_match(p$labels$subtitle, "Peak R_eff \\(per-member\\)")
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

test_that("plot_Reff total line is purple (#762A83)", {
  reff <- make_reff_df(ci = TRUE)
  p <- plot_Reff(reff)
  built <- ggplot2::ggplot_build(p)
  line_idx <- which(vapply(p$layers,
    function(l) inherits(l$geom, "GeomLine"), logical(1)))
  cols <- unique(unlist(lapply(line_idx, function(k) toupper(built$data[[k]]$colour))))
  expect_true("#762A83" %in% cols)
})

# -----------------------------------------------------------------------------
# Route stacking: R_env area from 0, R_hum stacked on top, total = their sum
# -----------------------------------------------------------------------------
add_routes <- function(reff, f_env = 0.7) {
  e <- reff; e$estimand <- "R_env"; e$central <- f_env * reff$central
  h <- reff; h$estimand <- "R_hum"; h$central <- (1 - f_env) * reff$central
  for (q in c("q2.5", "q25", "q50", "q75", "q97.5")) e[[q]] <- h[[q]] <- NA_real_
  out <- rbind(reff, h, e)
  for (a in c("ci_source", "peak_Rt")) attr(out, a) <- attr(reff, a)
  out
}

test_that("plot_Reff stacks R_hum on top of R_env and tops out at R_eff", {
  reff <- add_routes(make_reff_df(locs = "MOZ", ci = FALSE, peak = TRUE))
  p <- plot_Reff(reff, smooth_days = 1L)
  built <- ggplot2::ggplot_build(p)
  rib_idx <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomRibbon"),
                          logical(1)))
  expect_length(rib_idx, 2L)
  env <- built$data[[rib_idx[1]]]; hum <- built$data[[rib_idx[2]]]
  env <- env[is.finite(env$ymax), ]; hum <- hum[is.finite(hum$ymax), ]
  src <- reff[reff$estimand == "R_eff" & is.finite(reff$central), ]
  expect_equal(unname(env$ymin), rep(0, nrow(env)))
  expect_equal(unname(env$ymax), 0.7 * src$central, tolerance = 1e-8)
  expect_equal(unname(hum$ymin), unname(env$ymax), tolerance = 1e-8)
  expect_equal(unname(hum$ymax), src$central, tolerance = 1e-8)
  expect_true(all(c("#009988", "#EE7733") %in% toupper(c(env$fill, hum$fill))))
  expect_match(p$labels$caption, "stacked")
})

test_that("plot_Reff draws the total alone when routes = FALSE or absent", {
  reff <- add_routes(make_reff_df(ci = FALSE))
  p <- plot_Reff(reff, routes = FALSE)
  expect_false(any(vapply(p$layers, function(l) inherits(l$geom, "GeomRibbon"),
                          logical(1))))
  p2 <- plot_Reff(make_reff_df(ci = FALSE))
  expect_false(any(vapply(p2$layers, function(l) inherits(l$geom, "GeomRibbon"),
                          logical(1))))
})

test_that("plot_Reff reads the R_eff rows of an estimand-keyed peak_Rt", {
  reff <- add_routes(make_reff_df(locs = "MOZ", ci = FALSE, peak = FALSE))
  attr(reff, "peak_Rt") <- data.frame(
    location = "MOZ", estimand = c("R_eff", "R_hum", "R_env"),
    q2.5 = c(2, 0.1, 1.5), q50 = c(3, 0.2, 2.5), q97.5 = c(4, 0.3, 3.5),
    n_members = 10L, stringsAsFactors = FALSE)
  p <- plot_Reff(reff)
  expect_match(p$labels$subtitle, "3.00 \\[2.00, 4.00\\]")
})

test_that("plot_Reff validates input", {
  expect_error(plot_Reff(list(a = 1)), "data.frame")
  expect_error(plot_Reff(data.frame(x = 1)), "missing required column")
  empty <- make_reff_df(ci = TRUE)[0, ]
  expect_error(plot_Reff(empty), "zero rows")
})

test_that("smoothed stack sums to the smoothed total when route NA patterns differ", {
  reff <- add_routes(make_reff_df(locs = "MOZ", ci = FALSE, peak = FALSE, n_warmup_na = 0L))
  gap <- reff$estimand == "R_hum" & reff$t %in% c(20:23, 40)
  reff$central[gap] <- NA_real_
  reff$central[reff$estimand == "R_eff" & reff$t %in% c(20:23, 40)] <- NA_real_
  p <- plot_Reff(reff, smooth_days = 7L)
  built <- ggplot2::ggplot_build(p)
  rib_idx <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1)))
  hum <- built$data[[rib_idx[2]]]
  src <- reff[reff$estimand == "R_eff", ]
  both <- is.finite(src$central)
  expect_equal(unname(hum$ymax[is.finite(hum$ymax)]),
               MOSAIC:::.reff_roll_mean(ifelse(both, src$central, NA), 7L)[is.finite(hum$ymax)],
               tolerance = 1e-10)
})
