# Regression tests for diagnostic-figure fixes from the production-readiness
# review (plotting group).

# --- plotting-05: subset-optimization marker on a flat profile -----------------

.flat_subset_opt <- function() {
  ns <- 30:40
  score <- c(0.500, 0.505, 0.51, 0.512, 0.513, 0.515, 0.51, 0.508, 0.507, 0.506, 0.505)
  structure(list(
    evaluation_table = data.frame(
      n = ns, r2_cases = 0.5, r2_deaths = 0.3, bias_cases = 1, bias_deaths = 1,
      mae_cases = 1, mae_deaths = 1, ess = ns / 2, score = score),
    optimal_n = 40L, optimal_score = 0.505,        # flat profile -> largest N
    diagnostics_n = 40L, diagnostics_score = 0.505, objective = "mae",
    central_method = NULL, stability_flag = TRUE
  ), class = "mosaic_subset_optimization")
}

test_that("flat-profile optimal-N marker sits on the curve and is not called argmax", {
  skip_if_not_installed("ggplot2")
  out <- withr::local_tempdir()
  p <- suppressMessages(plot_model_subset_optimization(.flat_subset_opt(), output_dir = out,
                                                       verbose = FALSE))
  marker <- NULL
  for (ly in p$layers) {
    if (inherits(ly$geom, "GeomPoint") && is.data.frame(ly$data)) marker <- ly$data
  }
  expect_equal(marker$n, 40L)
  expect_equal(marker$value, 0.505)
  expect_false(grepl("argmax", p$labels$subtitle))
  expect_match(p$labels$subtitle, "largest N")
  # NULL central_method is labelled with the package default (mean).
  expect_match(p$labels$subtitle, "central = mean")
})

# --- plotting-06: -Inf likelihoods are failures --------------------------------

test_that("plot_model_likelihood counts -Inf draws as failed", {
  skip_if_not_installed("ggplot2")
  out <- withr::local_tempdir()
  res <- data.frame(sim = 1:6, iter = 1L, likelihood = c(-100, -120, -Inf, -Inf, NA, -90))
  p <- suppressMessages(plot_model_likelihood(res, output_dir = out, verbose = FALSE))
  expect_match(p$labels$subtitle, "N = 3 successful")
  expect_match(p$labels$subtitle, "3 failed")
  expect_true(all(is.finite(p$data$likelihood)))
})

# --- plotting-09: peak smoother matches est_epidemic_peaks ---------------------

test_that("plot_epidemic_peaks smooths with est_epidemic_peaks' window", {
  src <- paste(deparse(body(est_epidemic_peaks)), collapse = "\n")
  est_window <- as.integer(sub(".*window_size <- ([0-9]+).*", "\\1", src))
  expect_identical(MOSAIC:::.EPIDEMIC_PEAKS_SMOOTH_WINDOW, est_window)

  x <- c(0, 5, 3, 8, 10, 2, 1, 0, 4, 6)
  w <- 4L
  ref <- vapply(seq_along(x), function(i)
    mean(x[max(1, i - floor(w / 2)):min(length(x), i + floor(w / 2))]), numeric(1))
  expect_equal(MOSAIC:::.mosaic_peak_running_mean(x, w), ref)
})

# --- plotting-11: TruncNorm label readable at small scale ----------------------

test_that("TruncNorm legend label keeps small-scale values readable", {
  lab <- MOSAIC:::.mosaic_truncnorm_label(3.43e-06, 2.23e-06, 3.43e-07, 3.43e-05)
  expect_identical(lab, "TruncNorm(3.43e-06, 2.23e-06, [3.43e-07, 3.43e-05])")
  expect_identical(MOSAIC:::.mosaic_truncnorm_label(1, 1, 0, Inf), "TruncNorm(1, 1, [0, Inf])")
  expect_identical(MOSAIC:::.mosaic_truncnorm_label(0, 25, -90, 90), "TruncNorm(0, 25, [-90, 90])")
})

# --- plotting-16: data() must not write into the global environment ------------

test_that("plot_model_distributions does not load estimated_parameters into .GlobalEnv", {
  src <- paste(deparse(body(plot_model_distributions)), collapse = "\n")
  expect_match(src, 'data\\("estimated_parameters", package = "MOSAIC", envir = environment\\(\\)\\)')
})

# --- x-artifacts-06 / plotting-12 / plotting-15: PPC ---------------------------

.ppc_csv <- function(dir, loc, n = 12L) {
  d <- data.frame(
    location = loc, date = as.character(seq(as.Date("2024-01-01"), by = "week", length.out = n)),
    metric = rep(c("Suspected Cases", "Deaths"), each = n),
    observed = c(seq(10, by = 3, length.out = n), seq(1, by = 0.5, length.out = n)),
    predicted_central = c(seq(11, by = 3, length.out = n), seq(1.2, by = 0.5, length.out = n)),
    stringsAsFactors = FALSE)
  d$ci_1_lower <- d$predicted_central * 0.5; d$ci_1_upper <- d$predicted_central * 1.5
  d$ci_2_lower <- d$predicted_central * 0.8; d$ci_2_upper <- d$predicted_central * 1.2
  d
}

test_that("PPC auto-discovery ignores the combined predictions_ensemble_all.csv", {
  pdir <- withr::local_tempdir()
  out  <- withr::local_tempdir()
  a <- .ppc_csv(pdir, "AAA"); b <- .ppc_csv(pdir, "BBB")
  utils::write.csv(a, file.path(pdir, "predictions_ensemble_AAA.csv"), row.names = FALSE)
  utils::write.csv(b, file.path(pdir, "predictions_ensemble_BBB.csv"), row.names = FALSE)
  utils::write.csv(rbind(a, b), file.path(pdir, "predictions_ensemble_all.csv"), row.names = FALSE)
  msgs <- character()
  withCallingHandlers(
    tryCatch(plot_model_ppc(predictions_dir = pdir, output_dir = out, verbose = TRUE),
             error = function(e) NULL),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_true(any(grepl("Using ensemble predictions \\(2 files\\)", msgs)))
  expect_true(any(grepl("Total rows: 48", msgs)))
})

test_that("PPC coverage panel does not label the central-exceedance share a Bayesian p", {
  src <- paste(deparse(plot_model_ppc), collapse = "\n")
  expect_false(grepl("Bayesian p =", src, fixed = TRUE))
  expect_match(src, "P(central > obs) = ", fixed = TRUE)
})

test_that("PPC legacy mode reads engine matrices as [locations, days]", {
  out <- withr::local_tempdir()
  obs <- matrix(rpois(2 * 30, 10), nrow = 2)
  model <- list(params = list(reported_cases = obs, reported_deaths = obs / 10,
                              location_name = c("AAA", "BBB")),
                results = list(reported_cases = obs + 1, reported_deaths = obs / 10 + 0.1))
  msgs <- character()
  withCallingHandlers(
    tryCatch(plot_model_ppc(model = model, output_dir = out, verbose = TRUE),
             error = function(e) NULL),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_true(any(grepl("Extracted data for 2 location\\(s\\)", msgs)))
})

# --- plotting-17: a drawing error must not leave a PDF device open -------------

test_that("plot_model_convergence_status closes its PDF device on error", {
  rd <- withr::local_tempdir()
  pd <- withr::local_tempdir()
  before <- grDevices::dev.list()
  bad <- list(metrics_data = data.frame(metric = "ess", value = 1))   # malformed status
  expect_error(suppressWarnings(plot_model_convergence_status(rd, pd, status = bad,
                                                              verbose = FALSE)))
  expect_identical(grDevices::dev.list(), before)
})

# --- plotting-08: suitability figures plot the canonical psi -------------------

test_that("suitability figures plot psi, falling back to pred_smooth", {
  d <- data.frame(psi = c(0.19, 0.2), pred = c(0.05, 0.06), pred_smooth = c(0.07, 0.08))
  expect_equal(MOSAIC:::.mosaic_suitability_series(d), c(0.19, 0.2))
  expect_message(v <- MOSAIC:::.mosaic_suitability_series(d[, c("pred", "pred_smooth")]),
                 "pred_smooth")
  expect_equal(v, c(0.07, 0.08))
  expect_error(MOSAIC:::.mosaic_suitability_series(d[, "pred", drop = FALSE]), "psi")
})

test_that("plot_suitability_and_cases draws psi, not pred", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("glue")
  mi <- withr::local_tempdir(); fig <- withr::local_tempdir()
  dates <- seq(as.Date("2023-01-01"), by = "day", length.out = 40)
  utils::write.csv(data.frame(iso_code = "AGO", date = as.character(dates),
                              cases = c(rep(5, 20), rep(0, 20)),
                              psi = 0.4, pred = 0.05, pred_smooth = 0.06),
                   file.path(mi, "pred_psi_suitability_day.csv"), row.names = FALSE)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  p <- suppressWarnings(suppressMessages(
    plot_suitability_and_cases(list(MODEL_INPUT = mi, DOCS_FIGURES = fig), "AGO")))
  line_panel <- p[[2]]
  ys <- unlist(lapply(line_panel$layers, function(ly) {
    if (inherits(ly$geom, "GeomLine")) ggplot2::layer_data(line_panel, which(vapply(
      line_panel$layers, identical, logical(1), ly)))$y
  }))
  expect_true(length(ys) > 0)
  expect_true(all(abs(ys - 0.4) < 1e-12))
})
