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
# Running the real compiler needs the full MOSAIC data tree, so these tests
# exercise the anchor arithmetic on a synthetic panel of the same shape, and
# assert the wiring separately.

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

# The anchor arithmetic, transcribed from compile_suitability_data() so the
# test states the contract independently of the surrounding 700-line function.
.cp99r_for <- function(d, iso, anchor_stop = NULL) {
     in_window <- if (is.null(anchor_stop)) rep(TRUE, nrow(d)) else d$date <= anchor_stop
     m <- d$iso_code == iso & d$source != "AI" & in_window
     pop <- stats::median(d$total_population[m], na.rm = TRUE)
     max(stats::quantile(d$rate[m], 0.99, na.rm = TRUE), 5 / pop * 1e5, na.rm = TRUE)
}

test_that("a full-window anchor is contaminated by post-cutoff data", {
     d <- .anchor_panel()
     full    <- .cp99r_for(d, "AAA", NULL)
     bounded <- .cp99r_for(d, "AAA", as.Date("2023-01-01"))
     expect_gt(full, bounded)          # the spike raised the full-window anchor
     # and the contamination is large, not a rounding difference
     expect_gt(full / bounded, 10)
})

test_that("bounding the anchor leaves the historical target unchanged by the future", {
     d <- .anchor_panel()
     # target for a pre-cutoff week under each anchor
     wk <- d$iso_code == "AAA" & d$date == as.Date("2022-01-06")
     t_full    <- pmin(1, log1p(d$rate[wk]) / log1p(.cp99r_for(d, "AAA", NULL)))
     t_bounded <- pmin(1, log1p(d$rate[wk]) / log1p(.cp99r_for(d, "AAA", as.Date("2023-01-01"))))
     expect_false(isTRUE(all.equal(t_full, t_bounded)))
     # the bounded target is the LARGER one: it is not being deflated by a
     # record that had not happened yet
     expect_gt(t_bounded, t_full)
})

test_that("a country with no post-cutoff spike is unaffected by the bound", {
     d <- .anchor_panel()
     expect_equal(.cp99r_for(d, "BBB", NULL),
                  .cp99r_for(d, "BBB", as.Date("2023-01-01")))
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
