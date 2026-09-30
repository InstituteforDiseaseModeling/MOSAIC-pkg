# Tests for the `gam_train_stop` leakage-hygiene hook on the three hazard
# imputers (impute_flood_probability / impute_cyclone_probability /
# impute_drought_probability) and the `source_csv` override on est_suitability.
#
# The durable Path-A leakage fix: when gam_train_stop is supplied, each hazard
# GAM is FIT only on rows with date <= gam_train_stop, then PREDICTS every row.
# For rolling-origin forecast CV this guarantees a fold's hazard covariates are
# not informed by that fold's OOS future. These tests assert:
#   (a) gam_train_stop changes only the fit-row set, not the predict-row set
#       (same nrow, same output columns, contract still holds);
#   (b) LEAKAGE TEST -- predictions for rows <= stop are independent of the
#       data in rows > stop (perturb/truncate the future, confirm <=stop preds
#       are byte-identical);
#   (c) source_csv override reads an alternate panel.

# ---- Shared synthetic panels (larger than the base fixtures so a mid-panel
# cutoff still leaves >= 100 training rows on each side) --------------------

.mk_panel_flood <- function(seed = 21L, n_iso = 2L, n_years = 8L) {
     set.seed(seed)
     isos  <- LETTERS[seq_len(n_iso)]
     years <- 2014:(2014 + n_years - 1L)
     weeks <- 1:52
     d <- expand.grid(iso_code = isos, year = years, week = weeks,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
     d$date <- as.Date(paste0(d$year, "-01-01")) + (d$week - 1L) * 7L
     d <- d[order(d$iso_code, d$date), ]
     row.names(d) <- NULL
     d$precip_anom               <- stats::rnorm(nrow(d))
     d$precipitation_sum         <- abs(stats::rnorm(nrow(d), 50, 20))
     d$precip_sum_2w             <- abs(stats::rnorm(nrow(d), 100, 40))
     d$precip_sum_4w             <- abs(stats::rnorm(nrow(d), 200, 80))
     d$precip_sum_8w             <- abs(stats::rnorm(nrow(d), 400, 150))
     d$precip_sum_12w            <- abs(stats::rnorm(nrow(d), 600, 200))
     d$precip_extreme_p90_count  <- stats::rbinom(nrow(d), 1, 0.1)
     d$soil_moisture_0_to_10cm_mean <- stats::rnorm(nrow(d), 0.3, 0.05)
     d$soil_moisture_anom        <- stats::rnorm(nrow(d))
     d$spei_approx               <- stats::rnorm(nrow(d))
     d$relative_humidity_2m_mean <- stats::rnorm(nrow(d), 60, 15)
     d$rh_mean_12w               <- stats::rnorm(nrow(d), 60, 10)
     d$wind_speed_10m_max        <- abs(stats::rnorm(nrow(d), 5, 2))
     d$ENSO3                     <- stats::rnorm(nrow(d))
     d$ENSO34                    <- stats::rnorm(nrow(d))
     d$ENSO4                     <- stats::rnorm(nrow(d))
     d$IOD                       <- stats::rnorm(nrow(d))
     who_regions <- c("Central Africa", "East Africa", "Southern Africa", "West Africa")
     d$region <- who_regions[(match(d$iso_code, isos) - 1L) %% 4L + 1L]
     d$emdat_flood_active <- stats::rbinom(nrow(d), 1, stats::plogis(2 * d$precip_anom))
     d
}

.mk_panel_cyclone <- function(seed = 22L, n_iso = 2L, n_years = 8L) {
     set.seed(seed)
     isos  <- LETTERS[seq_len(n_iso)]
     years <- 2014:(2014 + n_years - 1L)
     weeks <- 1:52
     d <- expand.grid(iso_code = isos, year = years, week = weeks,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
     d$date <- as.Date(paste0(d$year, "-01-01")) + (d$week - 1L) * 7L
     d <- d[order(d$iso_code, d$date), ]
     row.names(d) <- NULL
     d$wind_speed_10m_max <- abs(stats::rnorm(nrow(d), 5, 2))
     d$precipitation_sum  <- abs(stats::rnorm(nrow(d), 50, 20))
     d$precip_sum_2w      <- abs(stats::rnorm(nrow(d), 100, 40))
     d$precip_sum_4w      <- abs(stats::rnorm(nrow(d), 200, 80))
     d$ENSO34             <- stats::rnorm(nrow(d))
     d$IOD                <- stats::rnorm(nrow(d))
     who_regions <- c("Central Africa", "East Africa", "Southern Africa", "West Africa")
     d$region <- who_regions[(match(d$iso_code, isos) - 1L) %% 4L + 1L]
     d$emdat_cyclone_active <- stats::rbinom(nrow(d), 1,
                                             stats::plogis(-3 + 1.2 * (d$wind_speed_10m_max - 5)))
     d
}

.mk_panel_drought <- function(seed = 23L, n_iso = 2L, n_years = 8L) {
     set.seed(seed)
     isos  <- LETTERS[seq_len(n_iso)]
     years <- 2014:(2014 + n_years - 1L)
     weeks <- 1:52
     d <- expand.grid(iso_code = isos, year = years, week = weeks,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
     d$date <- as.Date(paste0(d$year, "-01-01")) + (d$week - 1L) * 7L
     d <- d[order(d$iso_code, d$date), ]
     row.names(d) <- NULL
     # spei_approx carries a slow drift so the 12w-rollmean label has both
     # classes on each side of a mid-panel cutoff.
     d$spei_approx      <- as.numeric(scale(stats::filter(stats::rnorm(nrow(d)),
                                                          rep(1, 8), sides = 1)))
     d$spei_approx[is.na(d$spei_approx)] <- stats::rnorm(sum(is.na(d$spei_approx)))
     d$temp_anom        <- stats::rnorm(nrow(d))
     d$precip_anom      <- stats::rnorm(nrow(d))
     d$precip_sum_12w   <- abs(stats::rnorm(nrow(d), 600, 200))
     d$precipitation_sum <- abs(stats::rnorm(nrow(d), 50, 20))
     d$ENSO34           <- stats::rnorm(nrow(d))
     d$IOD              <- stats::rnorm(nrow(d))
     who_regions <- c("Central Africa", "East Africa", "Southern Africa", "West Africa")
     d$region <- who_regions[(match(d$iso_code, isos) - 1L) %% 4L + 1L]
     d
}

.cutoff <- as.Date("2018-06-30")   # mid-panel for the 2014-2021 fixtures


# ===========================================================================
# (a) gam_train_stop changes only the FIT, not the predict-row set
# ===========================================================================

testthat::test_that("gam_train_stop preserves the predict-row set (flood)", {
     d <- .mk_panel_flood()
     full <- MOSAIC::impute_flood_probability(d, diagnostics = FALSE, verbose = FALSE)
     capd <- MOSAIC::impute_flood_probability(d, gam_train_stop = .cutoff,
                                              diagnostics = FALSE, verbose = FALSE)
     testthat::expect_equal(nrow(capd), nrow(d))
     testthat::expect_true("emdat_flood_prob" %in% names(capd))
     testthat::expect_false(any(is.na(capd$emdat_flood_prob)))
     testthat::expect_true(all(capd$emdat_flood_prob >= 0 & capd$emdat_flood_prob <= 1))
     # A cutoff-fit model that drops half the training window should not
     # produce the identical column as the full-data fit (guards against the
     # arg being silently ignored -- cf. CLAUDE.md lesson #13).
     testthat::expect_gt(max(abs(capd$emdat_flood_prob - full$emdat_flood_prob)), 1e-6)
})

testthat::test_that("gam_train_stop preserves the predict-row set (cyclone)", {
     d <- .mk_panel_cyclone()
     full <- MOSAIC::impute_cyclone_probability(d, diagnostics = FALSE, verbose = FALSE)
     capd <- MOSAIC::impute_cyclone_probability(d, gam_train_stop = .cutoff,
                                                diagnostics = FALSE, verbose = FALSE)
     testthat::expect_equal(nrow(capd), nrow(d))
     testthat::expect_false(any(is.na(capd$emdat_cyclone_prob)))
     testthat::expect_true(all(capd$emdat_cyclone_prob >= 0 & capd$emdat_cyclone_prob <= 1))
     testthat::expect_gt(max(abs(capd$emdat_cyclone_prob - full$emdat_cyclone_prob)), 1e-6)
})

testthat::test_that("gam_train_stop preserves the predict-row set + label (drought)", {
     d <- .mk_panel_drought()
     full <- MOSAIC::impute_drought_probability(d, diagnostics = FALSE, verbose = FALSE)
     capd <- MOSAIC::impute_drought_probability(d, gam_train_stop = .cutoff,
                                                diagnostics = FALSE, verbose = FALSE)
     testthat::expect_equal(nrow(capd), nrow(d))
     testthat::expect_true(all(c("drought_prob", "drought_prob_26w_mean") %in% names(capd)))
     testthat::expect_false(any(is.na(capd$drought_prob)))
     testthat::expect_false(any(is.na(capd$drought_prob_26w_mean)))
     testthat::expect_true(all(capd$drought_prob >= 0 & capd$drought_prob <= 1))
     testthat::expect_gt(max(abs(capd$drought_prob - full$drought_prob)), 1e-6)
})


# ===========================================================================
# (b) LEAKAGE TEST: <=stop predictions are independent of >stop data.
#
# Fit with gam_train_stop = cutoff on the full panel, then again on a panel
# whose post-cutoff rows have been (i) perturbed and (ii) truncated. The GAM
# fit uses only <=cutoff rows, and predict() is row-local, so the predictions
# for rows <= cutoff MUST be byte-identical. If any future-data change moved a
# <=cutoff prediction, that would be leakage.
# ===========================================================================

.perturb_future <- function(d, cutoff, cols) {
     fut <- as.Date(d$date) > as.Date(cutoff)
     set.seed(999L)
     for (cc in cols) {
          if (cc %in% names(d) && is.numeric(d[[cc]]))
               d[[cc]][fut] <- d[[cc]][fut] + stats::rnorm(sum(fut), 0, 50)
     }
     d
}

testthat::test_that("LEAKAGE: <=stop flood preds independent of >stop data", {
     d <- .mk_panel_flood()
     base <- MOSAIC::impute_flood_probability(d, gam_train_stop = .cutoff,
                                              diagnostics = FALSE, verbose = FALSE)
     pre <- as.Date(d$date) <= .cutoff

     # (i) perturb future predictor values
     dp <- .perturb_future(d, .cutoff,
                           c("precip_anom", "precipitation_sum", "wind_speed_10m_max",
                             "soil_moisture_anom", "ENSO34", "IOD"))
     # also flip the future outcome label (must not matter: it's not a train row)
     dp$emdat_flood_active[as.Date(dp$date) > .cutoff] <-
          1L - dp$emdat_flood_active[as.Date(dp$date) > .cutoff]
     bp <- MOSAIC::impute_flood_probability(dp, gam_train_stop = .cutoff,
                                            diagnostics = FALSE, verbose = FALSE)
     testthat::expect_identical(base$emdat_flood_prob[pre], bp$emdat_flood_prob[pre])

     # (ii) truncate the panel to <=cutoff entirely: same <=stop preds.
     dt <- d[pre, , drop = FALSE]
     bt <- MOSAIC::impute_flood_probability(dt, gam_train_stop = .cutoff,
                                            diagnostics = FALSE, verbose = FALSE)
     testthat::expect_equal(base$emdat_flood_prob[pre], bt$emdat_flood_prob,
                            tolerance = 1e-10)
})

testthat::test_that("LEAKAGE: <=stop cyclone preds independent of >stop data", {
     d <- .mk_panel_cyclone()
     base <- MOSAIC::impute_cyclone_probability(d, gam_train_stop = .cutoff,
                                                diagnostics = FALSE, verbose = FALSE)
     pre <- as.Date(d$date) <= .cutoff
     dp <- .perturb_future(d, .cutoff,
                           c("wind_speed_10m_max", "precipitation_sum",
                             "precip_sum_2w", "precip_sum_4w", "ENSO34", "IOD"))
     dp$emdat_cyclone_active[as.Date(dp$date) > .cutoff] <-
          1L - dp$emdat_cyclone_active[as.Date(dp$date) > .cutoff]
     bp <- MOSAIC::impute_cyclone_probability(dp, gam_train_stop = .cutoff,
                                              diagnostics = FALSE, verbose = FALSE)
     testthat::expect_identical(base$emdat_cyclone_prob[pre], bp$emdat_cyclone_prob[pre])
})

testthat::test_that("LEAKAGE: <=stop drought preds independent of >stop predictors", {
     d <- .mk_panel_drought()
     base <- MOSAIC::impute_drought_probability(d, gam_train_stop = .cutoff,
                                                diagnostics = FALSE, verbose = FALSE)
     pre <- as.Date(d$date) <= .cutoff
     # Perturb future PREDICTORS only (leaving spei_approx untouched, so the
     # <=cutoff label rows are unchanged; the point of the test is that future
     # predictor rows -- never used in the <=stop fit -- cannot move <=stop
     # preds). The drought point-prob is row-local given the fixed fit.
     dp <- .perturb_future(d, .cutoff,
                           c("temp_anom", "precip_anom", "precip_sum_12w",
                             "precipitation_sum", "ENSO34", "IOD"))
     bp <- MOSAIC::impute_drought_probability(dp, gam_train_stop = .cutoff,
                                              diagnostics = FALSE, verbose = FALSE)
     testthat::expect_identical(base$drought_prob[pre], bp$drought_prob[pre])
})


# ===========================================================================
# (c) source_csv override reads an alternate panel
# ===========================================================================

testthat::test_that("est_suitability rejects source_csv on the frozen legacy path", {
     testthat::expect_error(
          MOSAIC::est_suitability(PATHS = list(MODEL_INPUT = tempdir()),
                                  architecture = "lstm_v1_legacy",
                                  source_csv = tempfile(fileext = ".csv")),
          "source_csv"
     )
})

testthat::test_that("lstm_v2 source_csv override points fitting at an alternate panel", {
     testthat::skip_if_not_installed("keras3")
     testthat::skip_if_not_installed("reticulate")
     # A non-existent override path must surface as the "suitability CSV not
     # found at <that path>" error -- proving the override string is the file
     # actually consulted (not the canonical DATA_CHOLERA_WEEKLY path).
     alt <- file.path(tempdir(), "alternate_v7.4_panel.csv")
     if (file.exists(alt)) file.remove(alt)
     PATHS <- list(MODEL_INPUT = tempdir(),
                   DATA_CHOLERA_WEEKLY = tempdir())
     err <- tryCatch(
          MOSAIC:::.est_suitability_lstm_v2(PATHS = PATHS, source_csv = alt,
                                            verbose = FALSE),
          error = function(e) conditionMessage(e))
     testthat::expect_match(err, alt, fixed = TRUE)
})
