# Regression (v0.101.0 psi refit): the weeks backfill_weekly_case_gaps() fills
# inside compile_suitability_data() carried source = NA, so the anchor rule
# (non-AI rows) counted them as trusted observations: two weeks interpolated
# between AI-observed BWA weeks (20 and 30 cases) set Botswana's per-country p99
# anchor (x6.5), and est_suitability() weighted them 1.0 (NA confidence = 1).
# A filled week is imputed: labelled backfill_interpolated (trust tier 3), weighted
# at most 0.5, never an anchor row.

# One country-week grid: AI-observed weeks around a two-week reporting gap.
.bf_panel <- function(cw_ai = 0.95) {
     d <- data.frame(iso_code = "BWA",
                     date = seq(as.Date("2009-01-01"), by = "week", length.out = 10L),
                     cases = c(NA, 4, 10, NA, NA, 40, 6, NA, NA, NA),
                     stringsAsFactors = FALSE)
     d$source <- ifelse(is.na(d$cases), NA_character_, "AI")
     d$confidence_weight <- ifelse(is.na(d$cases), NA_real_, cw_ai)
     d$disaggregation_method <- ifelse(is.na(d$cases), NA_character_, "observed")
     d$total_population <- 2e6 + 1e3 * seq_len(nrow(d))
     d
}

.bf_fill <- function(d) {
     d <- backfill_weekly_case_gaps(d, max_interp_weeks = 2L, verbose = FALSE)
     MOSAIC:::.csd_label_backfill(d)
}

test_that("backfilled weeks are labelled as imputed, unsourced and down-weighted", {
     d0 <- .bf_panel()
     d  <- .bf_fill(d0)
     f  <- d$cases_interpolated
     expect_equal(which(f), 4:5)
     expect_equal(d$cases[f], c(20, 30))
     expect_true(all(d$disaggregation_method[f] == "backfill_interpolated"))
     expect_true(all(is.na(d$source[f])))
     expect_equal(d$confidence_weight[f], c(0.5, 0.5))
     expect_equal(MOSAIC:::.surveillance_tier(d$disaggregation_method[f]), c(3L, 3L))
     # every other row is returned unchanged
     keep <- setdiff(names(d0), "cases")
     expect_identical(d[!f, keep], d0[!f, keep])
     expect_identical(d$cases[!f], d0$cases[!f])
})

test_that("a backfilled week is never weighted above the weaker week it is interpolated from", {
     d <- .bf_fill(.bf_panel(cw_ai = 0.475))                     # fourier-like neighbours
     expect_equal(d$confidence_weight[d$cases_interpolated], c(0.475, 0.475))
     # a direct-source neighbour (weight NA in the panel = 1) does not lift the 0.5 ceiling
     d0 <- .bf_panel()
     d0$source[3] <- "WHO"; d0$confidence_weight[3] <- NA; d0$disaggregation_method[3] <- NA
     d0$confidence_weight[6] <- 0.3
     d <- .bf_fill(d0)
     expect_equal(d$confidence_weight[d$cases_interpolated], c(0.3, 0.3))
     # nothing filled: labelling is a no-op
     d0 <- .bf_panel(); d0$cases_interpolated <- FALSE
     expect_identical(MOSAIC:::.csd_label_backfill(d0), d0)
})

test_that("AI rows and imputed weeks of any source are untrusted; observed and reconstructed weeks are not", {
     d <- data.frame(source = c("WHO", "WHO", "JHU", "AI", "AI", NA, NA),
                     disaggregation_method = c(NA, "who_catchup_curated_shaped", NA, "observed",
                                               "fourier_country_k1", "backfill_interpolated", NA),
                     stringsAsFactors = FALSE)
     expect_identical(MOSAIC:::.csd_untrusted_rows(d), c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))
})

test_that("backfilled weeks receive targets but never define the anchors (BWA 2009)", {
     d <- .bf_fill(.bf_panel())
     d$rate <- d$cases / d$total_population * 1e5
     untrusted <- MOSAIC:::.csd_untrusted_rows(d)
     anchor <- MOSAIC:::.csd_anchor_rows(d, untrusted, NULL)
     expect_false(any(anchor & !is.na(d$cases)))                 # no trusted observation at all
     pop_rows <- is.na(d$source) | d$source != "AI"
     out <- MOSAIC:::.csd_response_targets(d, anchor, "BWA", pop_rows = pop_rows)
     # the anchor is the 5-cases/week floor on the non-AI rows' median population
     floor_rate <- 5 / stats::median(d$total_population[pop_rows]) * 1e5
     obs <- !is.na(d$cases)
     expect_equal(out$target_D_rate_per_country_floored[obs], pmin(1, log1p(d$rate[obs]) / log1p(floor_rate)))
     expect_false(anyNA(out$target_D_rate_per_country_floored[d$cases_interpolated]))
     # the pre-fix rule (anchor = every non-AI row) anchored on the interpolated 20 and 30
     old <- MOSAIC:::.csd_response_targets(d, is.na(d$source) | d$source != "AI", "BWA")
     expect_lt(old$target_D_rate_per_country_floored[2], out$target_D_rate_per_country_floored[2])
})

test_that("trusted-row targets are invariant to whether a gap is backfilled", {
     # WHO weeks set the anchor; a gap between large AI weeks is backfilled or not
     d0 <- .bf_panel()
     d0$cases[c(3, 6)] <- c(500, 700)
     d0 <- rbind(d0, transform(d0[1:4, ], date = date + 70, cases = c(5, 12, 20, 8), source = "WHO",
                               confidence_weight = 1, disaggregation_method = NA))
     targets <- function(d) {
          d$rate <- d$cases / d$total_population * 1e5
          is_ai <- !is.na(d$source) & d$source == "AI"
          anchor <- MOSAIC:::.csd_anchor_rows(d, MOSAIC:::.csd_untrusted_rows(d), NULL)
          MOSAIC:::.csd_response_targets(d, anchor, "BWA", pop_rows = !is_ai)
     }
     without <- targets(transform(d0, cases_interpolated = FALSE))
     with    <- targets(.bf_fill(d0))
     who <- which(d0$source %in% "WHO")
     for (col in c("target_A_count_global", "target_B_count_per_country",
                   "target_C_rate_global", "target_D_rate_per_country_floored"))
          expect_identical(with[[col]][who], without[[col]][who], info = col)
})

test_that("compile_suitability_data labels the backfilled weeks right after filling them", {
     src <- paste(deparse(MOSAIC::compile_suitability_data), collapse = "\n")
     expect_true(grepl("d <- .csd_label_backfill(d)", src, fixed = TRUE))
     fill <- regexpr("backfill_weekly_case_gaps(d,", src, fixed = TRUE)
     label <- regexpr(".csd_label_backfill(d)", src, fixed = TRUE)
     anchor <- regexpr(".csd_anchor_rows(d, is_untrusted", src, fixed = TRUE)
     expect_true(fill > 0 && fill < label && label < anchor)
})

test_that("a deaths-only week is not a gap: its fill is undone and its source and deaths kept", {
     # WHO weeks of 10 and 20 cases around a two-week gap; the first gap week is a
     # WHO deaths-only week (3 deaths, no case count), the second an empty week
     d0 <- data.frame(iso_code = "MWI", date = seq(as.Date("2024-01-04"), by = "week", length.out = 4L),
                      cases = c(10, NA, NA, 20), deaths = c(0, 3, NA, 1),
                      source = c("WHO", "WHO", NA, "WHO"), confidence_weight = c(1, 1, NA, 1),
                      disaggregation_method = NA_character_, total_population = 1e7,
                      stringsAsFactors = FALSE)
     filled <- backfill_weekly_case_gaps(d0, max_interp_weeks = 2L, verbose = FALSE)
     expect_equal(which(filled$cases_interpolated), 2:3)              # the generic fill takes both
     d <- MOSAIC:::.csd_label_backfill(filled)
     expect_true(is.na(d$cases[2]) && !d$cases_interpolated[2])       # deaths-only week: not filled
     expect_identical(d[2, c("deaths", "source", "confidence_weight", "disaggregation_method")],
                      d0[2, c("deaths", "source", "confidence_weight", "disaggregation_method")])
     expect_true(d$cases_interpolated[3])                             # the empty week is still filled
     expect_equal(d$disaggregation_method[3], "backfill_interpolated")
     expect_true(is.na(d$source[3]) && is.na(d$deaths[3]))
     expect_equal(d$confidence_weight[3], 0.5)
})

test_that("compile_suitability_data does not backfill by default and keeps the flag column", {
     fm <- formals(MOSAIC::compile_suitability_data)
     expect_false(eval(fm$backfill_case_gaps))
     src <- paste(deparse(MOSAIC::compile_suitability_data), collapse = "\n")
     expect_true(grepl("d$cases_interpolated <- FALSE", src, fixed = TRUE))
})
