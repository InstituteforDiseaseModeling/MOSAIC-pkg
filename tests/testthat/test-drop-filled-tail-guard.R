# DA-02: `.drop_filled_prediction_tail()` must not fail open.
#
# The original implementation validated `df` but never `genuine_last`, so NULL, a
# zero-row frame, or ISO keys differing only in case each produced an all-NA
# cutoff -> `keep` all TRUE -> nothing dropped, no warning. That is how 98 days x
# 40 countries of carry-forward psi reached a shipped artefact. Each historical
# fail-open mode gets a test here.

mkdf <- function() data.frame(
     iso_code = rep(c("MOZ", "COD"), each = 4),
     date     = rep(as.Date("2026-01-01") + c(0, 7, 14, 21), 2),
     psi      = 0.2, stringsAsFactors = FALSE)

test_that("a valid genuine_last trims exactly the rows past each location's cutoff", {
     gl <- data.frame(iso_code = c("MOZ", "COD"),
                      last_genuine_date = as.Date(c("2026-01-08", "2026-01-15")),
                      stringsAsFactors = FALSE)
     out <- MOSAIC:::.drop_filled_prediction_tail(mkdf(), gl)
     expect_equal(nrow(out), 5L)                       # MOZ 2 + COD 3
     expect_true(all(out$date[out$iso_code == "MOZ"] <= as.Date("2026-01-08")))
     expect_true(all(out$date[out$iso_code == "COD"] <= as.Date("2026-01-15")))
})

test_that("NULL genuine_last errors instead of silently dropping nothing", {
     expect_error(MOSAIC:::.drop_filled_prediction_tail(mkdf(), NULL),
                  "must be a non-empty data.frame")
})

test_that("a zero-row genuine_last errors", {
     gl <- data.frame(iso_code = character(0), last_genuine_date = as.Date(character(0)))
     expect_error(MOSAIC:::.drop_filled_prediction_tail(mkdf(), gl),
                  "must be a non-empty data.frame")
})

test_that("a genuine_last missing its required columns errors", {
     expect_error(
          MOSAIC:::.drop_filled_prediction_tail(mkdf(), data.frame(iso = "MOZ", d = Sys.Date())),
          "must be a non-empty data.frame")
})

test_that("ISO case differences no longer disable the cutoff (was a fail-open mode)", {
     gl <- data.frame(iso_code = c("moz", "cod"),         # lower case on purpose
                      last_genuine_date = as.Date(c("2026-01-08", "2026-01-15")),
                      stringsAsFactors = FALSE)
     out <- MOSAIC:::.drop_filled_prediction_tail(mkdf(), gl)
     expect_equal(nrow(out), 5L)     # trimmed, not passed through
})

test_that("locations absent from genuine_last warn loudly and are passed through", {
     gl <- data.frame(iso_code = "MOZ", last_genuine_date = as.Date("2026-01-08"),
                      stringsAsFactors = FALSE)
     expect_warning(out <- MOSAIC:::.drop_filled_prediction_tail(mkdf(), gl),
                    "passed through UNTRIMMED")
     expect_equal(sum(out$iso_code == "COD"), 4L)   # documented pass-through, now visible
     expect_equal(sum(out$iso_code == "MOZ"), 2L)
})
