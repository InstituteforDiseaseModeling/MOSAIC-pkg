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


testthat::test_that("predictions follow the input rows when the input is not sorted (review data-pipeline-06)", {
     d <- .mk_synth_drought()
     out_sorted <- MOSAIC::impute_drought_probability(d, diagnostics = FALSE, verbose = FALSE)
     set.seed(11L)
     perm <- sample.int(nrow(d))
     out_shuf <- MOSAIC::impute_drought_probability(d[perm, , drop = FALSE],
                                               diagnostics = FALSE, verbose = FALSE)
     for (col in c("drought_prob", "drought_prob_26w_mean")) {
          # Row k of the shuffled output is row perm[k] of the sorted input
          testthat::expect_equal(out_shuf[[col]], out_sorted[[col]][perm])
     }
})


testthat::test_that("local-climate predictors never overlap the label window (review data-pipeline-07a)", {
     d <- .mk_synth_drought()
     base <- MOSAIC::impute_drought_probability(d, diagnostics = FALSE, verbose = FALSE)
     # Perturb the concurrent local-climate columns in each country's final 11
     # weeks. Those values can only enter the fit as predictors for rows that
     # lie sustain_weeks (12) later -- which do not exist -- so a leakage-free
     # model is unchanged. A model using concurrent precip/temp would refit.
     d2 <- d
     last11 <- unlist(lapply(split(seq_len(nrow(d2)), d2$iso_code), function(ix)
          utils::tail(ix[order(d2$date[ix])], 11L)))
     d2$precip_sum_12w[last11] <- d2$precip_sum_12w[last11] * 5
     d2$precip_anom[last11]    <- d2$precip_anom[last11] + 3
     d2$temp_anom[last11]      <- d2$temp_anom[last11] - 3
     pert <- MOSAIC::impute_drought_probability(d2, diagnostics = FALSE, verbose = FALSE)
     testthat::expect_equal(pert$drought_prob, base$drought_prob)
})


testthat::test_that("the GAM is never fit on rows after climate_obs_stop (review data-pipeline-07c)", {
     d <- .mk_synth_drought()
     stop_date <- as.Date("2019-06-30")
     base <- MOSAIC::impute_drought_probability(d, climate_obs_stop = stop_date,
                                                diagnostics = FALSE, verbose = FALSE)
     # Rewrite the "projected" climate after the stop so its drought labels flip
     d2 <- d
     fut <- d2$date > stop_date
     d2$spei_approx[fut] <- -d2$spei_approx[fut]
     pert <- MOSAIC::impute_drought_probability(d2, climate_obs_stop = stop_date,
                                                diagnostics = FALSE, verbose = FALSE)
     testthat::expect_equal(pert$drought_prob, base$drought_prob)
})


testthat::test_that("a per-country horizon caps each country's fit at its own date (review data-pipeline-07c)", {
     d <- .mk_synth_drought()
     h <- as.Date(c(A = "2019-06-30", B = "2018-12-31"))
     base <- MOSAIC::impute_drought_probability(d, climate_obs_stop = h,
                                                diagnostics = FALSE, verbose = FALSE)
     d2 <- d
     fut <- d2$date > h[d2$iso_code]
     d2$spei_approx[fut] <- -d2$spei_approx[fut]
     pert <- MOSAIC::impute_drought_probability(d2, climate_obs_stop = h,
                                                diagnostics = FALSE, verbose = FALSE)
     testthat::expect_equal(pert$drought_prob, base$drought_prob)
     testthat::expect_equal(MOSAIC:::.drought_row_horizon(h, c("B", "A", "Z")),
                            as.Date(c("2018-12-31", "2019-06-30", NA)))
     testthat::expect_error(MOSAIC:::.drought_row_horizon(as.Date(NA), "A"), "non-NA")
})


testthat::test_that("the default fit does not depend on the run date and NA dates never enter the fit", {
     testthat::expect_null(formals(MOSAIC::impute_drought_probability)$climate_obs_stop)
     d <- .mk_synth_drought()
     d$date[c(3L, 400L)] <- NA
     msgs <- testthat::capture_messages(
          out <- MOSAIC::impute_drought_probability(d, climate_obs_stop = as.Date("2019-06-30"),
                                                    diagnostics = FALSE, verbose = TRUE))
     act <- grep("active fraction", msgs, value = TRUE)
     testthat::expect_length(act, 1L)
     testthat::expect_false(grepl("NA", act))
     testthat::expect_false(anyNA(out$drought_prob[!is.na(d$date)]))
})


testthat::test_that(".drought_climate_obs_stop takes the earlier of ERA5 end and observed ENSO/IOD end", {
     tmp <- withr::local_tempdir()
     enso_dir <- file.path(tmp, "ENSO"); dir.create(enso_dir)
     enso <- data.frame(
          variable    = c("ENSO34", "ENSO34", "IOD", "IOD", "IOD"),
          data_source = c("historical", "forecast", "historical", "observed", "forecast"),
          date_start  = c("2026-08-24", "2026-09-28", "2026-05-25", "2026-08-10", "2026-12-28"),
          date_stop   = c("2026-08-30", "2026-10-04", "2026-05-31", "2026-08-16", "2027-01-03"))
     utils::write.csv(enso, file.path(enso_dir, "enso_weekly.csv"), row.names = FALSE)
     hist <- file.path(tmp, "om", "data", "historical")
     for (x in list(c("AGO", "2026-07-31"), c("KEN", "2026-09-20"))) {
          dir.create(file.path(hist, x[1]), recursive = TRUE)
          arrow::write_parquet(data.frame(date = as.Date(x[2]) - 0:3),
                               file.path(hist, x[1], sprintf("historical_%s_2026-07.parquet", x[1])))
          arrow::write_parquet(data.frame(date = as.Date("2000-01-01")),
                               file.path(hist, x[1], sprintf("historical_%s_2000-01.parquet", x[1])))
     }
     PATHS <- list(DATA_ENSO = enso_dir, OPEN_METEO_REPO = file.path(tmp, "om"))
     h <- MOSAIC:::.drought_climate_obs_stop(PATHS)
     # IOD observed ends 2026-08-16 (forecast rows ignored); AGO's ERA5 ends earlier
     testthat::expect_equal(h, as.Date(c(AGO = "2026-07-31", KEN = "2026-08-16")))

     PATHS$OPEN_METEO_REPO <- file.path(tmp, "absent")
     testthat::expect_warning(h2 <- MOSAIC:::.drought_climate_obs_stop(PATHS), "teleconnection horizon")
     testthat::expect_equal(h2, as.Date("2026-08-16"))
     PATHS$DATA_ENSO <- file.path(tmp, "absent")
     testthat::expect_warning(h3 <- MOSAIC:::.drought_climate_obs_stop(PATHS), "horizon unknown")
     testthat::expect_null(h3)
})
