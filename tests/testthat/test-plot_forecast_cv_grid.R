test_that("plot_forecast_cv_grid returns a plot object and saves a file", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  # synthetic predictions: 2 isos x 2 cutoffs x cases, weekly, with segments + CI
  mk_cell <- function(iso, cutoff) {
    dts <- seq(as.Date("2023-01-01"), as.Date("2026-06-01"), by = "week")
    emb <- cutoff + 14
    seg <- ifelse(dts <= cutoff, "IS", ifelse(dts <= emb, "embargo", "OOS"))
    mu  <- 50 + 40 * sin(2 * pi * as.numeric(format(dts, "%j")) / 365)
    data.frame(run_id = "r", model = "ensemble_opt", iso_code = iso,
               cutoff_date = cutoff, date = dts, metric = "cases", segment = seg,
               observed = pmax(0, round(mu + rnorm(length(dts), 0, 8))),
               pred_median = mu, pi95_lo = mu * 0.6, pi95_hi = mu * 1.4,
               pi50_lo = mu * 0.85, pi50_hi = mu * 1.15, stringsAsFactors = FALSE)
  }
  preds <- do.call(rbind, list(
    mk_cell("NGA", as.Date("2024-07-01")), mk_cell("NGA", as.Date("2025-01-01")),
    mk_cell("MOZ", as.Date("2024-07-01")), mk_cell("MOZ", as.Date("2025-01-01"))))

  p <- plot_forecast_cv_grid(preds, metric = "cases", isos = c("NGA", "MOZ"),
                             dir_output = NULL, verbose = FALSE)
  expect_s3_class(p, "ggplot")   # patchwork inherits from ggplot/gg

  dir <- withr::local_tempdir()
  plot_forecast_cv_grid(preds, metric = "cases", isos = c("NGA", "MOZ"),
                        dir_output = dir, save_pdf = FALSE, save_png = TRUE, verbose = FALSE)
  expect_true(file.exists(file.path(dir, "forecast_cv_grid_cases_ensemble_opt.png")))
})

test_that("plot_forecast_cv_grid errors on missing columns and empty selection", {
  bad <- data.frame(iso_code = "MOZ", date = Sys.Date(), metric = "cases")
  expect_error(plot_forecast_cv_grid(bad, metric = "cases"), "missing column")
})
