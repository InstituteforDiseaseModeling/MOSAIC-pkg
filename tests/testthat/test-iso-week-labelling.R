# DA-01 regression: ISO-8601 week labelling.
#
# `%V` (ISO week) may only be paired with `%G` (ISO week-based year), never with
# `%Y` (calendar year). Pairing %Y with %V collapses the year-boundary week: a
# Monday in late December belongs to ISO week 1 of the NEXT ISO year, so grouping
# by (calendar year, ISO week) merges it with the FOLLOWING January.
#
# Measured on the shipped MOZ climate panel before the fix: 19 (year, week) cells
# per country spanned 365-366 days -- e.g. (2018, week 1) covered
# 2018-01-01..2018-12-31, so that cell's weekly value was the mean of January AND
# December. Downstream, 99 duplicated (iso_code, date) rows appeared in the
# canonical suitability panel and `ENSO34_lag36` (a shipped v7.3 LSTM feature) was
# wrong on 11.7% of rows.

.span_days <- function(dates, year, week) {
     g <- split(dates, paste(year, week, sep = "-"))
     vapply(g, function(x) as.numeric(max(x) - min(x)) + 1, numeric(1))
}

test_that("%G + %V never produces a (year, week) group spanning more than 7 days", {
     # 26 years, deliberately spanning every ISO-year boundary shape including
     # 53-week years (2004, 2009, 2015, 2020) and leap years.
     dates <- seq(as.Date("2000-01-01"), as.Date("2026-12-31"), by = "day")
     spans <- .span_days(dates,
                         as.integer(format(dates, "%G")),
                         as.integer(format(dates, "%V")))
     expect_equal(max(spans), 7)
     expect_false(any(spans > 7))
})

test_that("%Y + %V is broken at the year boundary (the defect this guards)", {
     dates <- seq(as.Date("2000-01-01"), as.Date("2026-12-31"), by = "day")
     spans <- .span_days(dates,
                         as.integer(format(dates, "%Y")),   # WRONG on purpose
                         as.integer(format(dates, "%V")))
     # If this ever stops failing, R's date formatting changed and the guard above
     # is no longer testing what it claims to test.
     expect_true(any(spans > 300),
                 info = "calendar-year + ISO-week must collapse the boundary week")
})

test_that("%G equals the ISO year of the week's Thursday (the EMDAT/IDMC idiom)", {
     dates <- seq(as.Date("2000-01-01"), as.Date("2026-12-31"), by = "day")
     thu   <- dates + (4L - as.integer(format(dates, "%u")))
     expect_identical(format(dates, "%G"), format(thu, "%G"))
     expect_identical(format(dates, "%V"), format(thu, "%V"))
})

test_that("known boundary Mondays take the NEXT ISO year", {
     # Every Monday in the 2000-2026 compile window that is mislabelled by %Y.
     mondays <- as.Date(c("2001-12-31", "2002-12-30", "2003-12-29", "2007-12-31",
                          "2008-12-29", "2012-12-31", "2013-12-30", "2014-12-29",
                          "2018-12-31", "2019-12-30", "2024-12-30", "2025-12-29"))
     expect_true(all(format(mondays, "%V") == "01"))
     expect_equal(as.integer(format(mondays, "%G")),
                  as.integer(format(mondays, "%Y")) + 1L)
})

test_that("the canonical suitability panel has no duplicate (iso_code, date) keys", {
     # End-to-end acceptance for DA-01. Skips until the panel is rebuilt under the
     # fixed labelling; once rebuilt this is the guard that keeps it fixed.
     skip_on_cran()
     # `Config/testthat/parallel: true` reuses workers and does NOT reset options
     # between files, so a bare set_root_directory() here leaks into every later
     # file that touches get_paths(). Scope it.
     skip_if_not_installed("withr")
     root <- Sys.getenv("MOSAIC_ROOT", "~/MOSAIC")
     old <- getOption("root_directory")
     withr::defer(options(root_directory = old))
     PATHS <- tryCatch({
          MOSAIC::set_root_directory(root)
          MOSAIC::get_paths()
     }, error = function(e) NULL)
     skip_if(is.null(PATHS) || is.null(PATHS$DATA_CHOLERA_WEEKLY),
             "get_paths() unavailable (set MOSAIC_ROOT)")
     f <- file.path(PATHS$DATA_CHOLERA_WEEKLY,
                    "cholera_country_weekly_suitability_data.csv")
     skip_if_not(file.exists(f), "canonical suitability panel not present")
     d <- utils::read.csv(f, stringsAsFactors = FALSE)[, c("iso_code", "date")]
     expect_equal(sum(duplicated(d)), 0)
})

test_that("no R/ file pairs a CALENDAR year with an ISO/epi week (guards the class)", {
     # The instance tests above would stay green if any single call site regressed:
     # they test the invariant, not the code. This one parses the package source and
     # guards the whole defect class -- it is what would have caught the
     # process_SUPP_data.R site that the first pass of DA-01 missed.
     skip_on_cran()
     rdir <- testthat::test_path("..", "..", "R")
     skip_if_not(dir.exists(rdir), "R/ not available (installed check)")
     files <- list.files(rdir, pattern = "\\.R$", full.names = TRUE)

     offenders <- list()
     for (f in files) {
          ln <- readLines(f, warn = FALSE)
          code <- sub("#.*$", "", ln)          # strip comments: prose may cite "%Y"
          has_iso_week <- grepl('format\\(.*"%V"', code) |
                          grepl("lubridate::(iso|epi)week\\(", code)
          if (!any(has_iso_week)) next
          # A calendar-year derivation within +/- 3 lines of an ISO-week derivation
          # is the signature of the defect.
          cal_year <- grepl('format\\(.*"%Y"', code) | grepl("lubridate::year\\(", code)
          for (i in which(has_iso_week)) {
               near <- seq(max(1L, i - 3L), min(length(code), i + 3L))
               if (any(cal_year[near])) {
                    j <- near[cal_year[near]][1]
                    offenders[[length(offenders) + 1L]] <-
                         sprintf("%s:%d (ISO week) with calendar year at :%d",
                                 basename(f), i, j)
               }
          }
     }
     expect_equal(offenders, list(),
                  info = paste("calendar year paired with ISO/epi week:",
                               paste(unlist(offenders), collapse = "; ")))
})
