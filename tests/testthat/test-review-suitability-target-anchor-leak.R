# Deep review (suitability-02): with a pre-computed target_* response (the
# est_suitability default is target_D) and fit_date_stop earlier than the end of
# the panel, the targets arrive scaled by full-window anchors -- i.e. by
# post-cutoff outbreaks -- and nothing detected it. The panel now records its
# anchor window end and .psi_build_data() warns when it post-dates the cutoff.

mk_anchor_panel <- function(anchor_col = NULL) {
     set.seed(3)
     dts <- seq(as.Date("2015-01-01"), by = "week", length.out = 400L)
     p <- do.call(rbind, lapply(c("MOZ", "MWI"), function(iso) {
          n <- length(dts)
          data.frame(iso_code = iso, date = dts, cases = stats::rpois(n, 20),
                     region = "SOUTH",
                     f1 = stats::rnorm(n), f2 = stats::rnorm(n), f3 = stats::rnorm(n),
                     f4 = stats::rnorm(n), f5 = stats::rnorm(n),
                     stringsAsFactors = FALSE)
     }))
     p$target_D_rate_per_country_floored <- stats::runif(nrow(p))
     p$target_F_rank_per_country <- stats::runif(nrow(p))
     if (!is.null(anchor_col)) p$target_anchor_stop <- anchor_col
     p
}

build_anchor <- function(p, cutoff, response_var) {
     csv <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
     utils::write.csv(p, csv, row.names = FALSE)
     MOSAIC:::.psi_build_data(
          source_csv = csv, cutoff_date = cutoff, fit_date_start = "2015-01-01",
          pred_date_stop = cutoff, country_pool = "regional",
          features = c("f1", "f2", "f3", "f4", "f5"),
          split_params = list(rw_step_months = 6L, rw_test_months = 5L),
          response_var = response_var, verbose = FALSE)
}

only_leak_warnings <- function(expr) {
     msgs <- character(0)
     val <- withCallingHandlers(expr, warning = function(w) {
          msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning")
     })
     list(value = val, leak = grep("target-side leakage", msgs, value = TRUE))
}

test_that("a full-window panel fit at an earlier cutoff warns about target leakage", {
     p <- mk_anchor_panel()
     r <- only_leak_warnings(build_anchor(p, "2020-01-01", "target_D_rate_per_country_floored"))
     expect_length(r$leak, 1L)
     expect_match(r$leak, "inferred")
     expect_equal(r$value$target_anchor_end, max(p$date))
})

test_that("a recorded anchor window ending at/before the cutoff is silent", {
     p <- mk_anchor_panel(anchor_col = "2019-12-26")
     r <- only_leak_warnings(build_anchor(p, "2020-01-01", "target_D_rate_per_country_floored"))
     expect_length(r$leak, 0L)
     expect_equal(r$value$target_anchor_end, as.Date("2019-12-26"))
})

test_that("a recorded anchor window after the cutoff warns and names the source", {
     p <- mk_anchor_panel(anchor_col = "2022-06-02")
     r <- only_leak_warnings(build_anchor(p, "2020-01-01", "target_D_rate_per_country_floored"))
     expect_length(r$leak, 1L)
     expect_match(r$leak, "recorded")
})

test_that("cutoff at the end of surveillance does not warn (production refresh)", {
     p <- mk_anchor_panel()
     r <- only_leak_warnings(build_anchor(p, max(p$date), "target_D_rate_per_country_floored"))
     expect_length(r$leak, 0L)
})

test_that("target_F is full-window by construction and warns even with a bounded anchor", {
     p <- mk_anchor_panel(anchor_col = "2019-12-26")
     r <- only_leak_warnings(build_anchor(p, "2020-01-01", "target_F_rank_per_country"))
     expect_length(r$leak, 1L)
     expect_match(r$leak, "by construction")
})

test_that("the train-only intensity target never warns", {
     p <- mk_anchor_panel()
     r <- only_leak_warnings(build_anchor(p, "2020-01-01", "transmission_intensity"))
     expect_length(r$leak, 0L)
     expect_true(is.na(r$value$target_anchor_end))
})

test_that("compile's anchor rows are trusted rows bounded by target_anchor_stop", {
     d <- data.frame(date = seq(as.Date("2020-01-02"), by = "week", length.out = 10L),
                     cases = c(1, 2, NA, 4, 5, 6, NA, 8, 9, 10))
     is_ai <- c(rep(FALSE, 4L), TRUE, rep(FALSE, 5L))
     expect_identical(MOSAIC:::.csd_anchor_rows(d, is_ai, NULL), !is_ai)
     rows <- suppressMessages(MOSAIC:::.csd_anchor_rows(d, is_ai, "2020-02-06"))
     expect_identical(rows, !is_ai & d$date <= as.Date("2020-02-06"))
     expect_error(MOSAIC:::.csd_anchor_rows(d, is_ai, "2019-01-01"), "zero trusted rows")
     expect_error(MOSAIC:::.csd_anchor_rows(d, is_ai, "not-a-date"))
})

test_that("the recorded target_anchor_stop is the last trusted, observed anchor date", {
     d <- data.frame(date = seq(as.Date("2020-01-02"), by = "week", length.out = 10L),
                     cases = c(1, 2, NA, 4, 5, 6, NA, 8, 9, 10))
     is_ai <- c(rep(FALSE, 4L), TRUE, TRUE, rep(FALSE, 4L))
     # per-cutoff build at 2020-02-13 (row 7): row 7 is unobserved and rows 5-6
     # are AI, so the effective anchor end is row 4 -- earlier than the bound.
     rows <- suppressMessages(MOSAIC:::.csd_anchor_rows(d, is_ai, "2020-02-13"))
     expect_identical(MOSAIC:::.csd_anchor_stop(d, rows), format(d$date[4]))
     # full-window build: last trusted observed row
     expect_identical(MOSAIC:::.csd_anchor_stop(d, !is_ai), format(d$date[10]))
     # no trusted observed row -> NA
     expect_identical(MOSAIC:::.csd_anchor_stop(d, rep(FALSE, 10L)), NA_character_)
})
