# CV-07: the RW-CV must retain per-fold held-out predictions.
#
# Before this, `.psi_fit_predict_rw_cv()` discarded all 20+ fold models and
# emitted a single scalar -- round(median(best_epoch)). That is why the pipeline
# has never produced a per-horizon psi skill curve, and why arm selection had no
# per-country evidence to work with.

test_that("rw_diagnostics carries one row per (fold, country, held-out date)", {
     dates <- seq(as.Date("2021-01-04"), by = "week", length.out = 120L)
     isos  <- c("AAA", "BBB")
     grid  <- expand.grid(date = dates, iso_code = isos, stringsAsFactors = FALSE)

     steps <- list(
          list(step = 1L, train_end = as.Date("2022-01-03"),
               test_start = as.Date("2022-01-17"), test_end = as.Date("2022-04-10")),
          list(step = 2L, train_end = as.Date("2022-07-04"),
               test_start = as.Date("2022-07-18"), test_end = as.Date("2022-10-09")))

     bundle <- list(
          rw_steps       = steps,
          cutoff_date    = as.Date("2023-04-23"),
          dates_pred     = grid$date,
          countries_pred = grid$iso_code,
          pool_data = list(X = matrix(0, nrow(grid), 1), intensity = rep(0.1, nrow(grid)),
                           countries = grid$iso_code, dates = grid$date, cw = rep(1, nrow(grid))),
          seq_params = list(timesteps = 13L, max_gap_days = 14L, lead = 0L),
          encoders   = list(country_to_id = list(AAA = 1L, BBB = 2L),
                            region_for_country = list(AAA = 1L, BBB = 1L)),
          use_confidence_weight = FALSE)

     # Stub arch: returns a deterministic prediction over the full pred grid.
     fake <- function(data_bundle, seed, hyperparams) {
          list(pred = seq_len(nrow(grid)) / nrow(grid),
               n_epochs = 7L, val_loss = 0.5, val_metric = 0.4, loss_type = "bce")
     }
     res <- MOSAIC:::.psi_fit_predict_rw_cv(bundle, fake, seed = 11L, verbose = FALSE)
     fp  <- res$rw_diagnostics$fold_predictions

     expect_true(is.data.frame(fp))
     expect_setequal(unique(fp$fold), c(1L, 2L))
     expect_setequal(unique(fp$iso_code), isos)

     # every retained date lies inside its own fold's block -- never outside
     for (k in seq_along(steps)) {
          sub <- fp[fp$fold == steps[[k]]$step, ]
          expect_true(all(sub$date >= steps[[k]]$test_start))
          expect_true(all(sub$date <= steps[[k]]$test_end))
     }
     # both countries present in each fold, weekly resolution
     expect_equal(nrow(fp[fp$fold == 1L & fp$iso_code == "AAA", ]),
                  sum(dates >= steps[[1]]$test_start & dates <= steps[[1]]$test_end))
     expect_true(all(is.finite(fp$pred)))
})

test_that("fold_predictions is NULL rather than an error when no block is scoreable", {
     dates <- seq(as.Date("2021-01-04"), by = "week", length.out = 120L)
     grid  <- data.frame(date = dates, iso_code = "AAA", stringsAsFactors = FALSE)
     steps <- list(list(step = 1L, train_end = as.Date("2022-06-06"),
                        test_start = as.Date("2030-01-01"),   # far outside the grid
                        test_end   = as.Date("2030-03-01")))
     bundle <- list(rw_steps = steps, cutoff_date = as.Date("2023-04-23"),
                    dates_pred = grid$date, countries_pred = grid$iso_code,
                    pool_data = list(X = matrix(0, nrow(grid), 1), intensity = rep(0.1, nrow(grid)),
                                     countries = grid$iso_code, dates = grid$date, cw = rep(1, nrow(grid))),
                    seq_params = list(timesteps = 13L, max_gap_days = 14L, lead = 0L),
                    encoders = list(country_to_id = list(AAA = 1L),
                                    region_for_country = list(AAA = 1L)),
                    use_confidence_weight = FALSE)
     fake <- function(data_bundle, seed, hyperparams)
          list(pred = rep(0.5, nrow(grid)), n_epochs = 5L, val_loss = 1, val_metric = 1)
     res <- MOSAIC:::.psi_fit_predict_rw_cv(bundle, fake, seed = 11L, verbose = FALSE)
     expect_null(res$rw_diagnostics$fold_predictions)
})
