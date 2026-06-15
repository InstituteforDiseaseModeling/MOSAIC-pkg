# ---- synthetic rolling-CV predictions fixture -------------------------------
# Two cutoffs, four models, cases + deaths; OOS path extends ~13 months so the
# >5-month tail exists and must be truncated by the plotter.
.mk_rcv_predictions <- function() {
     cutoffs <- as.Date(c("2024-03-01", "2025-01-01"))
     models  <- c("ensemble", "ensemble_opt", "best", "medoid")
     metrics <- c("cases", "deaths")
     rows <- list()
     for (cd in cutoffs) {
          is_d  <- seq(cd - 180, cd, by = 7)
          oos_d <- seq(cd + 7, cd + 400, by = 7)   # ~13 months OOS
          for (mt in metrics) {
               base_obs <- if (mt == "cases") 40 else 3
               for (md in models) {
                    is_med  <- base_obs * (1 + 0.2 * sin(seq_along(is_d)))
                    oos_med <- base_obs * (1 + 0.2 * cos(seq_along(oos_d)))
                    mk <- function(dates, seg, med) {
                         data.frame(
                              run_id = paste0("cutoff_", cd), model = md,
                              iso_code = "MOZ", anchor_date = cutoffs[1],
                              cutoff_date = cd, date = dates, metric = mt, segment = seg,
                              observed = pmax(0, med * (1 + 0.1 * sin(seq_along(dates) * 1.7))),
                              observed_source = "surveillance",
                              pred_median = med,
                              pi95_lo = pmax(0, med * 0.5), pi95_hi = med * 1.6,
                              pi50_lo = pmax(0, med * 0.8), pi50_hi = med * 1.2,
                              stringsAsFactors = FALSE)
                    }
                    rows[[length(rows) + 1L]] <- mk(is_d,  "IS",  is_med)
                    rows[[length(rows) + 1L]] <- mk(oos_d, "OOS", oos_med)
               }
          }
     }
     do.call(rbind, rows)
}


test_that("plot_rolling_cv truncates to the assessed horizon and returns plottable objects", {
     skip_if_not_installed("ggplot2")

     pred <- .mk_rcv_predictions()
     res  <- plot_rolling_cv(pred, metric = "cases", annotate = FALSE)

     expect_type(res, "list")
     expect_s3_class(res$overview, "ggplot")
     expect_length(res$per_cutoff, length(unique(pred$cutoff_date)))
     expect_true(all(vapply(res$per_cutoff, inherits, logical(1), "ggplot")))

     # horizon truncation: nothing beyond cutoff + ~5 months, nothing before cutoff - ~2 months
     hi <- ceiling(5 * 30.4375); lo <- ceiling(2 * 30.4375)
     dd <- res$data
     expect_true(all(dd$date <= dd$cutoff_date + hi))
     expect_true(all(dd$date >= dd$cutoff_date - lo))
     # the synthetic OOS path extends ~13 months; confirm the long tail was dropped
     expect_true(max(pred$date) > max(dd$date) + 60)
})

test_that("plot_rolling_cv errors on missing required columns", {
     pred <- .mk_rcv_predictions()
     expect_error(plot_rolling_cv(pred[, setdiff(names(pred), "pi95_lo")]),
                  "missing column")
})

test_that("plot_rolling_cv honours the requested metric only", {
     pred <- .mk_rcv_predictions()
     res  <- plot_rolling_cv(pred, metric = "deaths", annotate = FALSE)
     expect_true(all(res$data$metric == "deaths"))
})

test_that("plot_rolling_cv restricts ribbons/lines to the requested models", {
     pred <- .mk_rcv_predictions()
     res  <- plot_rolling_cv(pred, models_ribbon = "ensemble",
                             models_line = "best", annotate = FALSE)
     expect_setequal(as.character(unique(res$data$model)), c("ensemble", "best"))
})

test_that("plot_rolling_cv writes PNG files when dir_output is supplied", {
     skip_if_not_installed("ggplot2")
     pred <- .mk_rcv_predictions()
     dir  <- withr::local_tempdir()
     res  <- plot_rolling_cv(pred, metric = "cases", annotate = FALSE,
                             dir_output = dir, save_pdf = FALSE, verbose = FALSE)
     expect_true(length(res$files) >= 1L)
     expect_true(all(file.exists(res$files)))
     expect_true(any(grepl("_overview\\.png$", res$files)))
})

test_that("plot_rolling_cv annotation path runs and exposes <=h skill cells", {
     pred <- .mk_rcv_predictions()
     res  <- plot_rolling_cv(pred, metric = "cases", annotate = TRUE)
     expect_s3_class(res$overview, "ggplot")
     expect_true(is.data.frame(res$skill))
     expect_true(all(res$skill$window == "OOS<=5mo"))
     expect_true(all(res$skill$model %in% c("ensemble", "ensemble_opt")))
})
