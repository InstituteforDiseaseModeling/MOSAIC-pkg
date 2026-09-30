# Deep review (suitability-01): the lstm_v2 auto fit_date_stop keyed on ENSO
# completeness alone, so on the canonical panel the cutoff landed ~8 months after
# the last surveillance week; .psi_build_data() then zero-filled the missing
# cases BEFORE the "intensity" recipe, so every post-surveillance week trained as
# an observed zero (1,440 fabricated rows on the canonical panel).

mk_panel <- function(isos = c("MOZ", "MWI"), start = "2015-01-01", n_weeks = 300L,
                     last_obs = c(MOZ = "2019-06-30", MWI = "2019-12-31"),
                     seed = 1L) {
     set.seed(seed)
     dts <- seq(as.Date(start) + 3L, by = "week", length.out = n_weeks)
     do.call(rbind, lapply(isos, function(iso) {
          n <- length(dts)
          cases <- stats::rpois(n, 20)
          cases[dts > as.Date(last_obs[[iso]])] <- NA
          data.frame(iso_code = iso, date = dts, cases = cases,
                     region = "SOUTH",
                     f1 = stats::rnorm(n), f2 = stats::rnorm(n), f3 = stats::rnorm(n),
                     f4 = stats::rnorm(n), f5 = stats::rnorm(n),
                     IOD = stats::rnorm(n), ENSO3 = stats::rnorm(n),
                     ENSO34 = stats::rnorm(n), ENSO4 = stats::rnorm(n),
                     stringsAsFactors = FALSE)
     }))
}

build <- function(csv, cutoff, response_var = "transmission_intensity", ...) {
     suppressWarnings(MOSAIC:::.psi_build_data(
          source_csv = csv, cutoff_date = cutoff, fit_date_start = "2015-01-01",
          pred_date_stop = cutoff, country_pool = "regional",
          features = c("f1", "f2", "f3", "f4", "f5"),
          split_params = list(rw_step_months = 6L, rw_test_months = 5L),
          response_var = response_var, verbose = FALSE, ...))
}

test_that("auto fit_date_stop is the last week with cases AND complete ENSO", {
     p <- mk_panel()
     auto <- MOSAIC:::.psi_auto_detect_dates(p)
     expect_equal(auto$fit_date_stop, max(p$date[!is.na(p$cases)]))
     expect_lt(auto$fit_date_stop, max(p$date))
     expect_equal(auto$pred_date_stop, max(p$date))

     # ENSO missing on the last observed week pulls the cutoff back
     last <- max(p$date[!is.na(p$cases)])
     p2 <- p; p2$ENSO4[p2$date == last] <- NA
     expect_lt(MOSAIC:::.psi_auto_detect_dates(p2)$fit_date_stop, last)

     # a trained lead extends the prediction horizon by lead weeks
     expect_equal(MOSAIC:::.psi_auto_detect_dates(p, lead = 12L)$pred_date_stop,
                  max(p$date) + 84L)

     p3 <- p; p3$cases <- NA
     expect_error(MOSAIC:::.psi_auto_detect_dates(p3), "both cholera case data")
})

test_that("intensity target never trains on weeks after a country's last observation", {
     p <- mk_panel()
     csv <- withr::local_tempfile(fileext = ".csv")
     utils::write.csv(p, csv, row.names = FALSE)
     cutoff <- max(p$date)                      # a cutoff past the surveillance end
     b <- build(csv, cutoff)
     pd <- b$pool_data
     last_obs <- tapply(p$date[!is.na(p$cases)], p$iso_code[!is.na(p$cases)], max)
     after <- as.numeric(pd$dates) > as.numeric(last_obs[pd$countries])
     expect_true(any(after))                    # the rows exist in the pool ...
     expect_true(all(is.na(pd$intensity[after])))   # ... but carry no target
     expect_false(any(!is.na(pd$intensity) & after & pd$dates <= cutoff))
     # the observed weeks keep their target
     expect_false(anyNA(pd$intensity[!after]))
     # the train-only anchor ignores the post-surveillance rows
     obs <- p[!is.na(p$cases), ]
     expect_equal(b$cases_99th,
                  stats::quantile(obs$cases, 0.99, names = FALSE))
})

test_that("pre-computed target_* columns are unaffected (already NA-propagating)", {
     p <- mk_panel()
     p$target_D_rate_per_country_floored <- ifelse(is.na(p$cases), NA, 0.3)
     csv <- withr::local_tempfile(fileext = ".csv")
     utils::write.csv(p, csv, row.names = FALSE)
     b <- build(csv, max(p$date[!is.na(p$cases)]),
                response_var = "target_D_rate_per_country_floored")
     expect_equal(sum(!is.na(b$pool_data$intensity)), sum(!is.na(p$cases)))
})
