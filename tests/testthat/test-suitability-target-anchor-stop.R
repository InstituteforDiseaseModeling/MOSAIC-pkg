# Regression: target_anchor_stop bounds the response-variable normalisation
# anchors, closing the target-side leak in a per-cutoff suitability panel.
#
# THE BUG. The response variables are normalised by per-country and global p99
# anchors computed over every trusted row in the panel. A leak-free per-cutoff
# panel (.rcv_build_leakfree_panel_v74) deliberately keeps `date_stop = NULL`,
# because it must span rows past the cutoff so the model can PREDICT the
# forecast window -- so those post-cutoff rows were in the anchors, and the
# target at time t was scaled by data from after t. `gam_train_stop` had
# already fixed the covariate side; this is its target-side sibling.
#
# Running the whole compiler needs the full MOSAIC data tree, so these tests
# call the two production helpers it uses -- .csd_anchor_rows() (which rows
# define the anchors) and .csd_response_targets() (the anchor arithmetic and
# the target columns) -- on a synthetic panel of the same shape, and assert the
# wiring separately.

.anchor_panel <- function() {
     dates <- seq(as.Date("2020-01-02"), by = "week", length.out = 200L)
     d <- expand.grid(date = dates, iso_code = c("AAA", "BBB"),
                      stringsAsFactors = FALSE)
     d$source <- "WHO"
     # A record spike AFTER 2023-01-01 in AAA only. If the anchor sees it, AAA's
     # p99 jumps and every historical AAA target is scaled down.
     d$cases <- ifelse(d$iso_code == "AAA", 50, 30)
     d$cases[d$iso_code == "AAA" & d$date > as.Date("2023-06-01")] <- 5000
     d$total_population <- 1e7
     d$rate <- d$cases / d$total_population * 1e5
     d
}

# The panel's targets under a given anchor bound, via the production helpers.
.targets_for <- function(d, anchor_stop = NULL) {
     is_ai <- !is.na(d$source) & d$source == "AI"
     is_anchor <- suppressMessages(MOSAIC:::.csd_anchor_rows(d, is_ai, anchor_stop))
     MOSAIC:::.csd_response_targets(d, is_anchor, c("AAA", "BBB"))
}

test_that("a full-window anchor is contaminated by post-cutoff data", {
     d <- .anchor_panel()
     wk <- d$iso_code == "AAA" & d$date == as.Date("2022-01-06")
     full    <- .targets_for(d, NULL)
     bounded <- .targets_for(d, as.Date("2023-01-01"))
     # target D = log1p(rate) / log1p(cp99r): recover the anchor each run used
     cp99r_full    <- expm1(log1p(d$rate[wk]) / full$target_D_rate_per_country_floored[wk])
     cp99r_bounded <- expm1(log1p(d$rate[wk]) / bounded$target_D_rate_per_country_floored[wk])
     expect_gt(cp99r_full, cp99r_bounded)          # the spike raised the full-window anchor
     # and the contamination is large, not a rounding difference
     expect_gt(cp99r_full / cp99r_bounded, 10)
})

test_that("bounding the anchor leaves the historical target unchanged by the future", {
     d <- .anchor_panel()
     wk <- d$iso_code == "AAA" & d$date == as.Date("2022-01-06")
     full    <- .targets_for(d, NULL)
     bounded <- .targets_for(d, as.Date("2023-01-01"))
     for (col in c("target_B_count_per_country", "target_D_rate_per_country_floored",
                   "target_A_count_global", "target_C_rate_global")) {
          expect_false(isTRUE(all.equal(full[[col]][wk], bounded[[col]][wk])), info = col)
          # the bounded target is the LARGER one: it is not being deflated by a
          # record that had not happened yet
          expect_gt(bounded[[col]][wk], full[[col]][wk])
     }
     # a bound AFTER every observation is the full window, bit-identical
     late <- .targets_for(d, as.Date("2030-01-01"))
     expect_identical(late$target_D_rate_per_country_floored,
                      full$target_D_rate_per_country_floored)
})

test_that("a country with no post-cutoff spike is unaffected by the per-country bound", {
     d <- .anchor_panel()
     b <- d$iso_code == "BBB"
     expect_equal(.targets_for(d, NULL)$target_D_rate_per_country_floored[b],
                  .targets_for(d, as.Date("2023-01-01"))$target_D_rate_per_country_floored[b])
     expect_equal(.targets_for(d, NULL)$target_B_count_per_country[b],
                  .targets_for(d, as.Date("2023-01-01"))$target_B_count_per_country[b])
})

test_that("transmission_intensity is NA on unobserved weeks and keeps the legacy anchor", {
     d <- .anchor_panel()
     miss <- d$iso_code == "BBB" & d$date >= as.Date("2021-01-07") &
             d$date <= as.Date("2021-06-24")
     d$cases[miss] <- NA
     d$rate[miss]  <- NA
     out <- .targets_for(d, NULL)
     # unobserved weeks are NA, never a fabricated 0 (review item: no zero-fill)
     expect_true(all(is.na(out$transmission_intensity[miss])))
     expect_false(anyNA(out$transmission_intensity[!miss]))
     # observed weeks match the frozen legacy recipe: NA counted as 0 in the p99
     leg <- d$cases; leg[is.na(leg)] <- 0
     p99 <- stats::quantile(leg, 0.99)
     expect_equal(out$transmission_intensity[!miss],
                  pmin(1, log1p(d$cases[!miss]) / log1p(p99)))
     # the other targets already propagated NA
     expect_true(all(is.na(out$target_D_rate_per_country_floored[miss])))
})

test_that("AI rows receive targets but never move the anchors", {
     d <- .anchor_panel()
     base <- .targets_for(d, NULL)
     d2 <- rbind(d, transform(d[d$iso_code == "AAA", ][1:20, ],
                              date = date + 1, source = "AI", cases = 1e6,
                              rate = 1e6 / 1e7 * 1e5))
     out <- .targets_for(d2, NULL)
     trusted <- seq_len(nrow(d))
     expect_equal(out$target_D_rate_per_country_floored[trusted],
                  base$target_D_rate_per_country_floored)
     expect_false(anyNA(out$target_D_rate_per_country_floored[-trusted]))
})

test_that("compile_suitability_data routes the targets through the tested helpers", {
     src <- gsub("\\s+", " ", paste(deparse(MOSAIC::compile_suitability_data), collapse = " "))
     expect_true(grepl("is_untrusted <- .csd_untrusted_rows(d)", src, fixed = TRUE))
     expect_true(grepl(".csd_anchor_rows(d, is_untrusted, target_anchor_stop)", src, fixed = TRUE))
     expect_true(grepl("pop_rows <- !is_ai & .csd_anchor_window(d, target_anchor_stop)", src, fixed = TRUE))
     expect_true(grepl(".csd_response_targets(d, is_anchor, iso_codes_mosaic, pop_rows = pop_rows)", src, fixed = TRUE))
})

test_that("compile_suitability_data exposes target_anchor_stop", {
     fm <- formals(MOSAIC::compile_suitability_data)
     expect_true("target_anchor_stop" %in% names(fm))
     expect_null(eval(fm$target_anchor_stop))     # back-compatible default
})

test_that("the leak-free v7.4 panel builder passes the cutoff as the anchor bound", {
     # The wiring is what makes the fix real: a parameter no caller sets is the
     # orphaned-function failure this package keeps hitting (lessons 1 and 6).
     src <- deparse(MOSAIC:::.rcv_build_leakfree_panel_v74)
     expect_true(any(grepl("target_anchor_stop\\s*=\\s*cutoff", src)))
})

test_that("target_anchor_stop is folded into the psi cache key", {
     # Otherwise a v7.4 panel cached before the anchors were bounded -- holding
     # a different target -- is silently reused under an unchanged hash.
     src <- deparse(MOSAIC:::.rcv_psi_spec_hash)
     expect_true(any(grepl("target_anchor_stop", src)))
})
