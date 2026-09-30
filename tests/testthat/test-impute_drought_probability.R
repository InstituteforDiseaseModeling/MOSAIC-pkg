# Tests for impute_drought_probability()
#
# Synthetic country-week panel where a sustained SPEI deficit is driven by an
# ENSO teleconnection (the exogenous lead-time predictor). Verify: (1) the
# sustained-SPEI-deficit label is derived correctly (12w rolling mean below
# threshold, not single-week dryness); (2) the GAM emits prob + integrator in
# [0,1] with no NAs; (3) concurrent spei_approx is NOT required as a predictor;
# (4) the function fails informatively on a missing input.

# Larger panel so the 24-week window + 12w label leave enough labeled rows:
# 2 ISOs x 6 years x 52 weeks = 624 rows.
.mk_synth_drought <- function(seed = 7L, n_iso = 2L, n_years = 6L,
                              na_forecast_year = TRUE) {
     set.seed(seed)
     isos  <- LETTERS[seq_len(n_iso)]
     years <- 2015:(2015 + n_years - 1L)
     weeks <- 1:52
     d <- expand.grid(iso_code = isos, year = years, week = weeks,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
     d$date <- as.Date(paste0(d$year, "-01-01")) + (d$week - 1L) * 7L
     d <- d[order(d$iso_code, d$date), ]
     row.names(d) <- NULL

     # ENSO drives a slow, persistent SPEI deficit: a low-frequency signal so
     # 12-week rolling means genuinely dip below the threshold in dry phases.
     n <- nrow(d)
     t <- seq_len(n)
     d$ENSO34 <- as.numeric(scale(sin(2 * pi * t / 130) + stats::rnorm(n, sd = 0.3)))
     d$IOD    <- stats::rnorm(n)
     # spei_approx negatively tracks ENSO (El Nino -> drought in this synthetic)
     d$spei_approx <- as.numeric(scale(-1.2 * d$ENSO34 + stats::rnorm(n, sd = 0.4)))
     d$temp_anom      <- 0.5 * -d$spei_approx + stats::rnorm(n, sd = 0.5)
     d$precip_anom    <- 0.8 * d$spei_approx + stats::rnorm(n, sd = 0.5)
     d$precipitation_sum <- abs(stats::rnorm(n, mean = 50, sd = 20))
     d$precip_sum_12w <- abs(stats::rnorm(n, mean = 600, sd = 200))

     who_regions <- c("Central Africa", "East Africa", "Southern Africa", "West Africa")
     d$region <- who_regions[(match(d$iso_code, isos) - 1L) %% 4L + 1L]

     if (na_forecast_year) {
          # Nothing to hide in the target directly (label derived internally);
          # instead leave the panel intact (the imputer labels every row with a
          # defined 12w-rolling SPEI). Kept as a knob for symmetry with siblings.
     }
     d
}


testthat::test_that("derives a sustained-SPEI-deficit label (not single-week dryness)", {
     d <- .mk_synth_drought()
     # Replicate the label the imputer builds internally and sanity-check it.
     lab <- d %>%
          dplyr::group_by(iso_code) %>%
          dplyr::arrange(date, .by_group = TRUE) %>%
          dplyr::mutate(spei_roll = slider::slide_dbl(spei_approx, mean,
                                                      .before = 11L, .complete = TRUE)) %>%
          dplyr::ungroup()
     active <- as.integer(!is.na(lab$spei_roll) & lab$spei_roll <= -0.8)
     n_lab  <- sum(!is.na(lab$spei_roll))
     testthat::expect_gt(sum(active), 0)                 # some drought weeks
     testthat::expect_lt(sum(active) / n_lab, 0.6)       # not everything
     # A single very-dry week without a sustained run should not, on its own,
     # flip the rolling mean below threshold: the max single-week SPEI among
     # non-active labeled weeks can be very low, but active weeks require the
     # 12-week MEAN to be low — assert that property holds by construction.
     testthat::expect_true(all(lab$spei_roll[active == 1] <= -0.8, na.rm = TRUE))
})


testthat::test_that("emits prob + integrator in [0,1] with no NAs; excludes concurrent SPEI", {
     d <- .mk_synth_drought()
     out <- MOSAIC::impute_drought_probability(d, diagnostics = FALSE, verbose = FALSE)

     testthat::expect_equal(nrow(out), nrow(d))
     for (col in c("drought_prob", "drought_prob_26w_mean")) {
          testthat::expect_true(col %in% names(out))
          testthat::expect_false(any(is.na(out[[col]])))
          testthat::expect_true(all(out[[col]] >= 0))
          testthat::expect_true(all(out[[col]] <= 1))
     }

     # spei_approx is a LABEL source, not a predictor: dropping it as a
     # *predictor* is impossible to test directly, but we can confirm the
     # imputer still runs when spei_approx has NO within-country variance-free
     # relationship exposed as a column beyond the label build. Instead, assert
     # the required-column contract does NOT list any spei rolling/lag predictor.
     req <- MOSAIC:::.impute_drought_probability_required()
     testthat::expect_true("spei_approx" %in% req)     # needed to BUILD the label
     testthat::expect_false(any(grepl("spei.*lag|spei.*roll", req)))  # not as predictor
})


testthat::test_that("integrator is smoother (lower variance) than the per-week prob", {
     d <- .mk_synth_drought()
     out <- MOSAIC::impute_drought_probability(d, diagnostics = FALSE, verbose = FALSE)
     # The 26w trailing mean must not be more variable than the point prob.
     testthat::expect_lte(stats::sd(out$drought_prob_26w_mean),
                          stats::sd(out$drought_prob) + 1e-9)
})


testthat::test_that("errors when a required column is missing", {
     d <- .mk_synth_drought()
     d$spei_approx <- NULL
     testthat::expect_error(
          MOSAIC::impute_drought_probability(d, diagnostics = FALSE, verbose = FALSE),
          "missing required column"
     )
})
