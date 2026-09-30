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

test_that("compile_suitability_data records the anchor window end in the panel", {
     src <- testthat::test_path("..", "..", "R", "compile_suitability_data.R")
     skip_if_not(file.exists(src), "R/ source not available (installed check)")
     txt <- paste(readLines(src, warn = FALSE), collapse = "\n")
     expect_true(grepl("d$target_anchor_stop <-", txt, fixed = TRUE))
})
