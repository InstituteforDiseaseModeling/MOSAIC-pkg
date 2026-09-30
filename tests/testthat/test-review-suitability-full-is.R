# Deep review (suitability-05): the final full-IS refit (.psi_slice_full_is and
# the placeholder slice in .psi_build_data) used `date < cutoff_date` while the
# scaler partition, the intensity anchor, the bias-correction fit and the
# "training" label all use `<=`. With the auto cutoff on a panel row date, the
# last observed week was dropped from the deployed model yet used to fit the
# bias correction applied to its output.

mk_is <- function(n_weeks = 120L) {
     d <- seq(as.Date("2020-01-02"), by = "week", length.out = n_weeks)
     p <- do.call(rbind, lapply(c("AAA", "BBB"), function(i)
          data.frame(iso_code = i, date = d, y = seq_along(d) / 1000,
                     stringsAsFactors = FALSE)))
     list(p = p, dates = d)
}

test_that(".psi_slice_full_is includes the target dated exactly at the cutoff", {
     m <- mk_is(); p <- m$p
     cutoff <- m$dates[100]
     bundle <- list(
          pool_data  = list(X = matrix(p$y, ncol = 1), intensity = p$y,
                            countries = p$iso_code, dates = p$date,
                            cw = rep(1, nrow(p))),
          seq_params = list(timesteps = 13L, max_gap_days = 14L, lead = 0L),
          encoders   = list(country_to_id = list(AAA = 0L, BBB = 1L),
                            region_for_country = list(AAA = 0L, BBB = 0L)),
          use_confidence_weight = FALSE,
          cutoff_date = cutoff)
     full <- MOSAIC:::.psi_slice_full_is(bundle)
     # one sequence per country per week from week 13 through the cutoff week
     expect_equal(full$n_train, 2L * (100L - 12L))
     expect_true(p$y[p$date == cutoff][1] %in% full$y_train)
     expect_false(any(full$y_train > p$y[p$date == cutoff][1]))
})

test_that(".psi_build_data's placeholder full-IS slice is inclusive of the cutoff", {
     set.seed(5)
     m <- mk_is(300L)
     n <- length(m$dates)
     p <- do.call(rbind, lapply(c("MOZ", "MWI"), function(iso)
          data.frame(iso_code = iso, date = m$dates, cases = stats::rpois(n, 10),
                     region = "SOUTH",
                     f1 = stats::rnorm(n), f2 = stats::rnorm(n), f3 = stats::rnorm(n),
                     f4 = stats::rnorm(n), f5 = stats::rnorm(n),
                     stringsAsFactors = FALSE)))
     csv <- withr::local_tempfile(fileext = ".csv")
     utils::write.csv(p, csv, row.names = FALSE)
     cutoff <- m$dates[250]
     b <- suppressWarnings(MOSAIC:::.psi_build_data(
          source_csv = csv, cutoff_date = cutoff, fit_date_start = min(m$dates),
          pred_date_stop = max(m$dates), country_pool = "regional",
          features = c("f1", "f2", "f3", "f4", "f5"),
          split_params = list(rw_step_months = 6L, rw_test_months = 5L),
          verbose = FALSE))
     expect_equal(length(b$y_train), 2L * (250L - 12L))
     full <- MOSAIC:::.psi_slice_full_is(b)
     expect_equal(full$n_train, length(b$y_train))
})
