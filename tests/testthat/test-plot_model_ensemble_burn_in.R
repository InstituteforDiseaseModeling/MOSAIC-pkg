# plot_model_ensemble(show_burn_in = TRUE) draws the predictions from the first
# time step and shades the steps before the scored window (burn-in and cases
# warm-up), with a dashed "scored from <date>" marker at the scored-window
# start. Display only: the exported prediction CSVs keep the unscored head
# blank (the v1.0 acceptance evaluator relies on those NA rows) and the caption
# metrics stay on the scored window.
#
# Golden files: fixtures/predictions_burnin_golden_{AAA,BBB}.csv and the
# captions below were produced from make_burnin_ensemble() by the code at
# c93b870aa, before show_burn_in existed:
#   .mosaic_write_prediction_csvs(
#     .mosaic_assemble_prediction_table(ens, central_method = "mean"),
#     data_dir, file_prefix = "burnin_golden")
#   plot_model_ensemble(ens, output_dir, central_method = "mean")$...$caption

# 2 locations x 8 daily steps. Cases scored from step 4 (warm-up 2, burn-in 3
# steps), deaths from step 3, so the two channels' unscored heads differ:
# cases steps 1-3 and deaths steps 1-2 are blank in the CSV.
make_burnin_ensemble <- function() {
  n_locs <- 2L; n_times <- 8L
  base_c <- c(0, 5, 9, 12, 20, 30, 25, 18)
  base_d <- c(0, 0, 0.5, 1, 1.5, 2.25, 1, 0.75)
  cases_mean   <- t(vapply(seq_len(n_locs), function(i) base_c * i / 3, numeric(n_times)))
  deaths_mean  <- t(vapply(seq_len(n_locs), function(i) base_d * i, numeric(n_times)))
  cases_median <- cases_mean * 0.9
  deaths_median <- floor(deaths_mean)
  obs_cases  <- t(vapply(seq_len(n_locs), function(i) c(1, 4, 10, 11, 22, 28, NA, NA) * i, numeric(n_times)))
  obs_deaths <- t(vapply(seq_len(n_locs), function(i) c(0, 0, 1, 1, 2, 2, NA, NA) * i, numeric(n_times)))
  ci <- function(m) list(list(lower = m * 0.5, upper = m * 1.5),
                         list(lower = m * 0.8, upper = m * 1.2))
  structure(list(
    cases_mean = cases_mean, cases_median = cases_median,
    deaths_mean = deaths_mean, deaths_median = deaths_median,
    ci_bounds = list(cases = ci(cases_mean), deaths = ci(deaths_mean)),
    obs_cases = obs_cases, obs_deaths = obs_deaths,
    cases_array = NULL, deaths_array = NULL,
    parameter_weights = c(0.5, 0.3, 0.2), seeds = 1:3,
    n_param_sets = 3L, n_simulations_per_config = 2L, n_successful = 6L,
    location_names = c("AAA", "BBB"), n_locations = n_locs, n_time_points = n_times,
    date_start = "2024-01-01", date_stop = "2024-01-08",
    envelope_quantiles = c(0.025, 0.25, 0.75, 0.975),
    artifact_mask = list(cases_warmup = 2L, deaths_final = FALSE,
                         score_idx_cases = 4L, score_idx_deaths = 3L)
  ), class = "mosaic_ensemble")
}

burnin_dates <- as.Date("2024-01-01") + 0:7

golden_captions <- c(
  AAA = paste0("Ribbons show 95% and 50% intervals | Central: cases=mean, deaths=mean\n",
               "Cases: Obs = 61, Pred = 21, R\u00b2 = 0.947, Bias = 0.34 | ",
               "Deaths: Obs = 6, Pred = 5, R\u00b2 = 0.757, Bias = 0.88"),
  BBB = paste0("Ribbons show 95% and 50% intervals | Central: cases=mean, deaths=mean\n",
               "Cases: Obs = 122, Pred = 41, R\u00b2 = 0.947, Bias = 0.34 | ",
               "Deaths: Obs = 12, Pred = 10, R\u00b2 = 0.757, Bias = 0.88"),
  cases_all  = "Total: Obs = 183, Pred = 62, R\u00b2 = 0.97, Bias = 0.34 (central: mean)",
  deaths_all = "Total: Obs = 18, Pred = 16, R\u00b2 = 0.815, Bias = 0.88 (central: mean)"
)

render_burnin <- function(ens, ...) {
  out <- withr::local_tempdir(.local_envir = parent.frame())
  plot_model_ensemble(ens, output_dir = out, central_method = "mean",
                      verbose = FALSE, ...)
}

# The default figure, with the burn-in shown and hidden, rendered once per file:
# every render writes the full PDF set, and several tests only inspect it.
# `ensemble` is the object that was rendered.
shared_render <- local({
  cache <- list()
  function(show_burn_in = TRUE) {
    key <- as.character(show_burn_in)
    if (is.null(cache[[key]])) {
      ens <- make_burnin_ensemble()
      out <- withr::local_tempdir(.local_envir = testthat::teardown_env())
      draw <- function() plot_model_ensemble(ens, output_dir = out, central_method = "mean",
                                             show_burn_in = show_burn_in, verbose = FALSE)
      # A hidden head raises ggplot2's removed-rows warnings.
      res <- if (show_burn_in) draw() else suppressWarnings(draw())
      cache[[key]] <<- list(result = res, ensemble = ens)
    }
    cache[[key]]
  }
})

layers_of <- function(p, geom) {
  unname(which(vapply(p$layers, function(l) inherits(l$geom, geom), logical(1))))
}

# The drawn series for one panel, ordered by date.
panel_series <- function(p, geom, panel, cols = "y") {
  d <- ggplot2::layer_data(p, layers_of(p, geom)[1])
  d <- d[as.integer(d$PANEL) == panel, , drop = FALSE]
  d[order(d$x), c("x", cols), drop = FALSE]
}

captions_of <- function(res) {
  c(AAA = res$individual$AAA$labels$caption,
    BBB = res$individual$BBB$labels$caption,
    cases_all  = sub("\nGenerated: .*$", "", res$cases_faceted$labels$caption),
    deaths_all = sub("\nGenerated: .*$", "", res$deaths_faceted$labels$caption))
}

test_that("prediction CSVs keep the unscored head blank and match the pre-change output", {
  skip_if_not_installed("ggplot2")
  local_null_device()
  ens <- make_burnin_ensemble()
  ref <- make_burnin_ensemble()

  # Plotting (burn-in shown) must not touch the ensemble the CSV is built from.
  expect_identical(shared_render()$ensemble, ref)

  td <- withr::local_tempdir()
  tbl <- MOSAIC:::.mosaic_assemble_prediction_table(ens, central_method = "mean")
  MOSAIC:::.mosaic_write_prediction_csvs(tbl, data_dir = td,
                                         file_prefix = "burnin_golden", verbose = FALSE)
  for (loc in ens$location_names) {
    f <- paste0("predictions_burnin_golden_", loc, ".csv")
    expect_identical(readLines(file.path(td, f)),
                     readLines(testthat::test_path("fixtures", f)),
                     info = f)

    df <- utils::read.csv(file.path(td, f), stringsAsFactors = FALSE)
    pred_cols <- c("predicted_central", "predicted_mean", "predicted_median",
                   grep("^ci_", names(df), value = TRUE))
    cas <- df[df$metric == "Suspected Cases", ]
    dea <- df[df$metric == "Deaths", ]
    expect_true(all(is.na(as.matrix(cas[1:3, pred_cols]))), info = loc)
    expect_true(all(is.finite(as.matrix(cas[4:8, pred_cols]))), info = loc)
    expect_true(all(is.na(as.matrix(dea[1:2, pred_cols]))), info = loc)
    expect_true(all(is.finite(as.matrix(dea[3:8, pred_cols]))), info = loc)
  }
})

test_that("show_burn_in draws the central line and ribbons from the first step", {
  skip_if_not_installed("ggplot2")
  local_null_device()
  ens <- make_burnin_ensemble()
  res <- shared_render()$result

  for (i in seq_along(ens$location_names)) {
    p <- res$individual[[ens$location_names[i]]]
    # facet_grid(metric ~ .): panel 1 = Suspected Cases, panel 2 = Deaths.
    lc <- panel_series(p, "GeomLine", 1L)
    ld <- panel_series(p, "GeomLine", 2L)
    expect_equal(lc$x, as.numeric(burnin_dates))
    expect_equal(lc$y, ens$cases_mean[i, ])
    expect_equal(ld$y, ens$deaths_mean[i, ])
    # Widest ribbon first.
    rc <- panel_series(p, "GeomRibbon", 1L, c("ymin", "ymax"))
    rd <- panel_series(p, "GeomRibbon", 2L, c("ymin", "ymax"))
    expect_equal(rc$ymin, ens$ci_bounds$cases[[1]]$lower[i, ])
    expect_equal(rc$ymax, ens$ci_bounds$cases[[1]]$upper[i, ])
    expect_equal(rd$ymin, ens$ci_bounds$deaths[[1]]$lower[i, ])
    expect_equal(rd$ymax, ens$ci_bounds$deaths[[1]]$upper[i, ])
  }

  # Faceted per-channel plots: facet_wrap(~ location), AAA is panel 1.
  expect_equal(panel_series(res$cases_faceted, "GeomLine", 1L)$y, ens$cases_mean[1, ])
  expect_equal(panel_series(res$deaths_faceted, "GeomLine", 2L)$y, ens$deaths_mean[2, ])
})

test_that("show_burn_in = FALSE blanks the unscored head as before", {
  skip_if_not_installed("ggplot2")
  local_null_device()
  ens <- make_burnin_ensemble()
  res <- shared_render(FALSE)$result
  p <- res$individual$AAA

  lc <- panel_series(p, "GeomLine", 1L)
  ld <- panel_series(p, "GeomLine", 2L)
  expect_false(any(is.finite(lc$y[lc$x < as.numeric(burnin_dates[4])])))
  expect_equal(lc$y[lc$x >= as.numeric(burnin_dates[4])], ens$cases_mean[1, 4:8])
  expect_false(any(is.finite(ld$y[ld$x < as.numeric(burnin_dates[3])])))
  expect_equal(ld$y[ld$x >= as.numeric(burnin_dates[3])], ens$deaths_mean[1, 3:8])
  for (g in c("GeomRect", "GeomVline", "GeomText")) {
    expect_length(layers_of(p, g), 0L)
    expect_length(layers_of(res$cases_faceted, g), 0L)
  }
})

test_that("the unscored span and scored-window marker follow each channel's start", {
  skip_if_not_installed("ggplot2")
  local_null_device()
  res <- shared_render()$result
  p <- res$individual$AAA
  start_c <- as.numeric(as.Date("2024-01-04"))
  start_d <- as.numeric(as.Date("2024-01-03"))

  rect <- ggplot2::layer_data(p, layers_of(p, "GeomRect"))
  rect <- rect[order(as.integer(rect$PANEL)), ]
  expect_equal(rect$xmin, c(-Inf, -Inf))
  expect_equal(rect$xmax, c(start_c, start_d))
  vl <- ggplot2::layer_data(p, layers_of(p, "GeomVline"))
  expect_equal(vl$xintercept[order(as.integer(vl$PANEL))], c(start_c, start_d))
  # The span sits under the ribbons; the label is drawn last.
  expect_lt(layers_of(p, "GeomRect"), min(layers_of(p, "GeomRibbon")))
  expect_equal(layers_of(p, "GeomText"), length(p$layers))

  # Different starts: one label per channel panel.
  lab <- ggplot2::layer_data(p, layers_of(p, "GeomText"))
  lab <- lab[order(as.integer(lab$PANEL)), ]
  expect_equal(as.integer(lab$PANEL), 1:2)
  expect_equal(trimws(lab$label), c("scored from 2024-01-04", "scored from 2024-01-03"))

  # Faceted per-channel plots: the span in every location panel, one label.
  for (pf in list(res$cases_faceted, res$deaths_faceted)) {
    expect_setequal(as.integer(ggplot2::layer_data(pf, layers_of(pf, "GeomRect"))$PANEL), 1:2)
    lf <- ggplot2::layer_data(pf, layers_of(pf, "GeomText"))
    expect_equal(nrow(lf), 1L)
    expect_equal(as.integer(lf$PANEL), 1L)
  }
  expect_equal(trimws(ggplot2::layer_data(res$deaths_faceted,
                                          layers_of(res$deaths_faceted, "GeomText"))$label),
               "scored from 2024-01-03")
})

test_that("a common scored-window start gets a single label; no head draws no marker", {
  skip_if_not_installed("ggplot2")
  local_null_device()
  ens <- make_burnin_ensemble()

  # Burn-in 3 steps on both channels (warm-up 2 is inside it): one label, on
  # the cases panel; both panels still shaded.
  ens$artifact_mask$score_idx_deaths <- 4L
  p <- render_burnin(ens)$individual$AAA
  lab <- ggplot2::layer_data(p, layers_of(p, "GeomText"))
  expect_equal(nrow(lab), 1L)
  expect_equal(as.integer(lab$PANEL), 1L)
  expect_equal(trimws(lab$label), "scored from 2024-01-04")
  expect_equal(nrow(ggplot2::layer_data(p, layers_of(p, "GeomRect"))), 2L)

  # Only the cases warm-up: a 2-step cases span, nothing on deaths.
  ens$artifact_mask$score_idx_cases  <- 1L
  ens$artifact_mask$score_idx_deaths <- 1L
  p <- render_burnin(ens)$individual$AAA
  rect <- ggplot2::layer_data(p, layers_of(p, "GeomRect"))
  expect_equal(as.integer(rect$PANEL), 1L)
  expect_equal(rect$xmax, as.numeric(as.Date("2024-01-03")))

  # Nothing unscored: no span, marker or label.
  p <- render_burnin(ens, n_cases_warmup_mask = 0L)$individual$AAA
  for (g in c("GeomRect", "GeomVline", "GeomText")) expect_length(layers_of(p, g), 0L)

  # Whole cases series unscored: shaded to the panel edge, no marker or label.
  ens$artifact_mask$score_idx_cases <- 20L
  p <- render_burnin(ens, n_cases_warmup_mask = 0L)$individual$AAA
  rect <- ggplot2::layer_data(p, layers_of(p, "GeomRect"))
  expect_equal(rect$xmax, Inf)
  for (g in c("GeomVline", "GeomText")) expect_length(layers_of(p, g), 0L)
})

test_that("caption metrics are unchanged by show_burn_in", {
  skip_if_not_installed("ggplot2")
  local_null_device()
  shown  <- captions_of(shared_render()$result)
  hidden <- captions_of(shared_render(FALSE)$result)
  expect_identical(shown, golden_captions)
  expect_identical(hidden, golden_captions)
})

test_that("a supplied prediction_table is drawn as given, its blank head filled from the ensemble", {
  skip_if_not_installed("ggplot2")
  local_null_device()
  ens <- make_burnin_ensemble()
  # A masked table whose central line is the MEDIAN, plotted with
  # central_method = "mean": the table's own central_method governs the fill,
  # and the caption is labelled and scored by it too, with a warning that the
  # argument was overridden.
  tbl <- MOSAIC:::.mosaic_assemble_prediction_table(ens, central_method = "median")

  expect_warning(res <- render_burnin(ens, prediction_table = tbl),
                 "prediction_table carries central_method cases=median, deaths=median")
  p <- res$individual$AAA
  expect_equal(panel_series(p, "GeomLine", 1L)$y, ens$cases_median[1, ])
  expect_equal(panel_series(p, "GeomLine", 2L)$y, ens$deaths_median[1, ])
  rc <- panel_series(p, "GeomRibbon", 1L, c("ymin", "ymax"))
  expect_equal(rc$ymin, ens$ci_bounds$cases[[1]]$lower[1, ])
  out <- withr::local_tempdir()
  med_captions <- captions_of(plot_model_ensemble(ens, output_dir = out, central_method = "median",
                                                  verbose = FALSE))
  expect_match(med_captions[["AAA"]], "Central: cases=median, deaths=median", fixed = TRUE)
  expect_identical(captions_of(res), med_captions)
  # With the default argument the table governs silently.
  expect_no_warning(dflt <- plot_model_ensemble(ens, output_dir = out, prediction_table = tbl,
                                                verbose = FALSE))
  expect_identical(captions_of(dflt), med_captions)

  # Without the burn-in the supplied table is drawn untouched.
  p0 <- suppressWarnings(render_burnin(ens, prediction_table = tbl,
                                       show_burn_in = FALSE))$individual$AAA
  expect_false(any(is.finite(panel_series(p0, "GeomLine", 1L)$y[1:3])))

  # An ensemble that cannot supply the head (one interval pair where its
  # envelope quantiles define two): warn, draw the table as is. The blank head
  # also raises ggplot2's removed-rows warnings, collected here.
  bad <- ens
  bad$ci_bounds$cases <- bad$ci_bounds$cases[1]
  msgs <- character(0)
  res <- withCallingHandlers(
    render_burnin(bad, prediction_table = tbl),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_true(any(grepl("burn-in could not be drawn", msgs)))
  lc <- panel_series(res$individual$AAA, "GeomLine", 1L)
  expect_false(any(is.finite(lc$y[1:3])))
  expect_equal(lc$y[4:8], ens$cases_median[1, 4:8])
})

test_that("an observation-model ensemble keeps the shaded burn-in, the central line and its intervals", {
  skip_if_not_installed("ggplot2")
  local_null_device()
  # calc_model_ensemble() since v0.101.0: ci_bounds are observation-level
  # predictive intervals (wider than the engine envelope, drawn in the unscored
  # head too) around the engine-level central line, and the package default
  # central line is the median for cases and the mean for deaths.
  ens <- make_burnin_ensemble()
  ci_obs <- function(m) list(list(lower = m * 0.3, upper = m * 2),
                             list(lower = m * 0.7, upper = m * 1.4))
  ens$ci_bounds <- list(cases = ci_obs(ens$cases_mean), deaths = ci_obs(ens$deaths_mean))
  ens$observation_model <- list(cases = TRUE, deaths = TRUE, k_cases = c(2, 3),
                                week_offset = c(0L, 0L), phi_deaths = c(1.5, 2))
  out <- withr::local_tempdir()
  res <- plot_model_ensemble(ens, output_dir = out, verbose = FALSE)

  p <- res$individual$AAA
  expect_equal(panel_series(p, "GeomLine", 1L)$y, ens$cases_median[1, ])
  expect_equal(panel_series(p, "GeomLine", 2L)$y, ens$deaths_mean[1, ])
  rc <- panel_series(p, "GeomRibbon", 1L, c("ymin", "ymax"))
  rd <- panel_series(p, "GeomRibbon", 2L, c("ymin", "ymax"))
  expect_equal(rc$ymin, ens$ci_bounds$cases[[1]]$lower[1, ])
  expect_equal(rc$ymax, ens$ci_bounds$cases[[1]]$upper[1, ])
  expect_equal(rd$ymax, ens$ci_bounds$deaths[[1]]$upper[1, ])
  lab <- ggplot2::layer_data(p, layers_of(p, "GeomText"))
  expect_equal(trimws(lab$label[order(as.integer(lab$PANEL))]),
               c("scored from 2024-01-04", "scored from 2024-01-03"))

  # The cases median is 0.9 x the mean, so against the golden mean captions the
  # correlation R2 is unchanged and cases Pred and Bias scale by 0.9 (AAA:
  # 18.6 of 61 observed, 0.305; all: 55.8 of 183); deaths keep the mean. The
  # captions also say that the engine-level line can lie above the
  # observation-level 50% band (release red team OBS-2).
  line_note <- paste0("Line: engine-level central trajectory, before observation noise; ",
                      "it can lie above the 50% band where the reporting dispersion k is small\n")
  faceted_note <- function(where) paste0(
    "\nRibbons: 95% and 50% observation-level predictive intervals; line: engine-level ",
    "central trajectory, before observation noise, which can lie above the 50% band ", where)
  expected <- c(
    AAA = paste0("Ribbons show 95% and 50% observation-level predictive intervals | ",
                 "Central: cases=median, deaths=mean\n", line_note,
                 "Cases: Obs = 61, Pred = 19, R² = 0.947, Bias = 0.3 | ",
                 "Deaths: Obs = 6, Pred = 5, R² = 0.757, Bias = 0.88"),
    BBB = paste0("Ribbons show 95% and 50% observation-level predictive intervals | ",
                 "Central: cases=median, deaths=mean\n", line_note,
                 "Cases: Obs = 122, Pred = 37, R² = 0.947, Bias = 0.3 | ",
                 "Deaths: Obs = 12, Pred = 10, R² = 0.757, Bias = 0.88"),
    cases_all  = paste0("Total: Obs = 183, Pred = 56, R² = 0.97, Bias = 0.3 (central: median)",
                        faceted_note("where the reporting dispersion k is small")),
    deaths_all = paste0(golden_captions[["deaths_all"]],
                        faceted_note("where deaths are sparse or overdispersed")))
  expect_identical(captions_of(res), expected)
  hidden <- suppressWarnings(plot_model_ensemble(ens, output_dir = out, verbose = FALSE,
                                                 show_burn_in = FALSE))
  expect_identical(captions_of(hidden), expected)
})

test_that("show_burn_in and n_cases_warmup_mask are validated", {
  skip_if_not_installed("ggplot2")
  ens <- make_burnin_ensemble()
  td  <- withr::local_tempdir()
  expect_error(plot_model_ensemble(ens, output_dir = td, show_burn_in = NA,
                                   verbose = FALSE), "show_burn_in")
  expect_error(plot_model_ensemble(ens, output_dir = td, show_burn_in = "yes",
                                   verbose = FALSE), "show_burn_in")
  expect_error(plot_model_ensemble(ens, output_dir = td, n_cases_warmup_mask = -1L,
                                   verbose = FALSE), "non-negative integer")
})

test_that("render_MOSAIC_figures draws both prediction figures with the burn-in shown", {
  skip_if_not_installed("ggplot2")
  root <- withr::local_tempdir()
  dirs <- MOSAIC:::.mosaic_ensure_dir_tree(root, clean_output = FALSE)
  ens  <- MOSAIC:::.mosaic_stamp_artifact(make_burnin_ensemble())
  saveRDS(ens, file.path(dirs$calibration, "ensemble_candidate.rds"))
  saveRDS(ens, file.path(dirs$calibration, "medoid_ensemble.rds"))

  calls <- list()
  testthat::local_mocked_bindings(
    plot_model_ensemble = function(...) {
      calls[[length(calls) + 1L]] <<- list(...)
      invisible(NULL)
    }
  )
  render_MOSAIC_figures(root, which = "predictions", verbose = FALSE)
  expect_length(calls, 2L)
  expect_setequal(vapply(calls, `[[`, character(1), "file_prefix"), c("ensemble", "medoid"))
  for (a in calls) expect_true(isTRUE(a$show_burn_in))
})
