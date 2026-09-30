# Tests for the leak-free v7.4 hazard-panel plumbing wired into the forecast-CV
# psi prefit (R/prefit_rolling_cv_psi.R). These exercise the PLUMBING added in
# v0.63.0 -- request resolution, cache spec-hash sensitivity, the non-destructive
# per-cutoff panel builder, and a panel-level leakage regression -- WITHOUT
# touching the keras3/TensorFlow stack (the psi fit itself is out of scope here;
# the GAM leak-free contract per-imputer lives in test-impute_gam_train_stop.R).
#
# The builder calls compile_suitability_data() unqualified inside the MOSAIC
# namespace, so testthat::local_mocked_bindings() can stand in a synthetic
# compiler that runs the REAL hazard imputers with the gam_train_stop the
# builder passed -- letting us assert leak-freedom of the produced panel and
# that the builder forwarded the aligned window + gam_train_stop = cutoff.

# ===========================================================================
# .rcv_psi_v74_request(): RAW-spec guard + window resolution (lesson #13)
# ===========================================================================

testthat::test_that("v7.4 is active ONLY when feature_set == 'v7.4' is explicit", {
     # default (no feature_set) -> inactive (byte-unchanged legacy path)
     testthat::expect_false(MOSAIC:::.rcv_psi_v74_request(list())$active)
     # other feature sets -> inactive
     testthat::expect_false(MOSAIC:::.rcv_psi_v74_request(list(feature_set = "v7.3"))$active)
     testthat::expect_false(MOSAIC:::.rcv_psi_v74_request(list(feature_set = "v1"))$active)
     # explicit v7.4 -> active
     testthat::expect_true(MOSAIC:::.rcv_psi_v74_request(list(feature_set = "v7.4"))$active)
})

testthat::test_that("compile window is resolved from the LSTM fit_date_start (default 2015)", {
     # default: fixture start
     r0 <- MOSAIC:::.rcv_psi_v74_request(list(feature_set = "v7.4"))
     testthat::expect_identical(r0$compile_date_start, "2015-01-01")
     # overridable via arch_control$fit_date_start (ml's window lever)
     r1 <- MOSAIC:::.rcv_psi_v74_request(
          list(feature_set = "v7.4",
               arch_control = list(fit_date_start = "2016-06-01")))
     testthat::expect_identical(r1$compile_date_start, "2016-06-01")
})


# ===========================================================================
# .rcv_psi_spec_hash(): v7.4 folds panel provenance into the key; back-compat
# ===========================================================================

testthat::test_that("BACK-COMPAT: non-v7.4 spec hash is unchanged by v7.4 plumbing", {
     # The default/v7.3 hash must equal the hash of the SAME key WITHOUT any
     # v74_panel block -- i.e. the legacy content hash. We reconstruct the
     # pre-v7.4 key inline and require byte-identical digests.
     T_k  <- as.Date("2022-06-30")
     spec <- list(feature_set = "v7.3", response_var = "y",
                  arch_control = list(n_seeds = 10L))
     legacy_key <- list(fit_date_stop = as.character(T_k),
                        est_suitability_spec = spec)
     testthat::expect_identical(
          MOSAIC:::.rcv_psi_spec_hash(T_k, spec),
          MOSAIC:::.rcv_obj_hash(legacy_key))
})

testthat::test_that("v7.4 hash differs from v7.3 and is window-sensitive", {
     T_k    <- as.Date("2022-06-30")
     base   <- list(response_var = "y", arch_control = list(n_seeds = 10L))
     h_v73  <- MOSAIC:::.rcv_psi_spec_hash(T_k, c(base, list(feature_set = "v7.3")))
     h_v74  <- MOSAIC:::.rcv_psi_spec_hash(T_k, c(base, list(feature_set = "v7.4")))
     testthat::expect_false(identical(h_v73, h_v74))

     # Same v7.4 spec but a different compile window (via fit_date_start) MUST
     # invalidate the cache -- guards against reusing a wrong-window panel.
     spec_w1 <- list(feature_set = "v7.4",
                     arch_control = list(n_seeds = 10L, fit_date_start = "2015-01-01"))
     spec_w2 <- list(feature_set = "v7.4",
                     arch_control = list(n_seeds = 10L, fit_date_start = "2016-06-01"))
     testthat::expect_false(identical(
          MOSAIC:::.rcv_psi_spec_hash(T_k, spec_w1),
          MOSAIC:::.rcv_psi_spec_hash(T_k, spec_w2)))

     # And the cutoff still moves the hash for v7.4 (gam_train_stop = cutoff).
     testthat::expect_false(identical(
          MOSAIC:::.rcv_psi_spec_hash(as.Date("2022-06-30"), spec_w1),
          MOSAIC:::.rcv_psi_spec_hash(as.Date("2023-06-30"), spec_w1)))
})


# ===========================================================================
# .rcv_build_leakfree_panel_v74(): forwards aligned window + gam_train_stop,
# is non-destructive, and produces a LEAK-FREE panel.
#
# We mock compile_suitability_data() with a synthetic compiler that (a) records
# the args the builder forwarded and (b) writes a panel by running the REAL
# hazard imputers with the passed gam_train_stop -- so the produced panel's
# leak-freedom is a property of the actual production GAM hook.
# ===========================================================================

.mk_raw_cases <- function(tmp_dir, seed = 42L, n_iso = 2L, n_years = 8L) {
     # Minimal cases-input file the builder stages; the mocked compiler ignores
     # its content (it synthesizes its own panel), but the builder asserts the
     # file exists before redirecting PATHS.
     writeLines("iso_code,year,week,cases", file.path(tmp_dir, "cholera_surveillance_weekly_combined.csv"))
}

# Synthetic panel used by the mocked compiler: a hazard panel with a slow-drift
# label so a mid-window GAM fit has both classes on each side of the cutoff.
.mk_hazard_panel <- function(seed = 24L, n_iso = 2L, n_years = 8L) {
     set.seed(seed)
     isos  <- LETTERS[seq_len(n_iso)]
     years <- 2014:(2014 + n_years - 1L)
     weeks <- 1:52
     d <- expand.grid(iso_code = isos, year = years, week = weeks,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
     d$date <- as.Date(paste0(d$year, "-01-01")) + (d$week - 1L) * 7L
     d <- d[order(d$iso_code, d$date), ]; row.names(d) <- NULL
     d$precip_anom       <- stats::rnorm(nrow(d))
     d$precipitation_sum <- abs(stats::rnorm(nrow(d), 50, 20))
     d$precip_sum_2w     <- abs(stats::rnorm(nrow(d), 100, 40))
     d$precip_sum_4w     <- abs(stats::rnorm(nrow(d), 200, 80))
     d$wind_speed_10m_max <- abs(stats::rnorm(nrow(d), 5, 2))
     d$ENSO34            <- stats::rnorm(nrow(d))
     d$IOD               <- stats::rnorm(nrow(d))
     who_regions <- c("Central Africa", "East Africa", "Southern Africa", "West Africa")
     d$region <- who_regions[(match(d$iso_code, isos) - 1L) %% 4L + 1L]
     d$emdat_cyclone_active <- stats::rbinom(nrow(d), 1,
                                             stats::plogis(-3 + 1.2 * (d$wind_speed_10m_max - 5)))
     d
}

# A synthetic compile that honours date_start/gam_train_stop and writes to the
# CANONICAL path inside whatever PATHS$DATA_CHOLERA_WEEKLY it is handed (exactly
# like the real compile_suitability_data). Captures forwarded args to .args_env.
.args_env <- new.env(parent = emptyenv())
.fake_compile <- function(PATHS, cutoff, use_epidemic_peaks, date_start, date_stop,
                          forecast_mode, forecast_horizon, include_lags,
                          include_flood_prob, gam_train_stop, ...) {
     .args_env$date_start     <- date_start
     .args_env$date_stop      <- date_stop
     .args_env$gam_train_stop <- as.Date(gam_train_stop)
     .args_env$write_dir      <- PATHS$DATA_CHOLERA_WEEKLY
     d <- .mk_hazard_panel()
     # Honour the aligned lower bound so the panel's fit rows match [start, T].
     d <- d[d$date >= as.Date(date_start), , drop = FALSE]
     d <- MOSAIC::impute_cyclone_probability(
          d, output_col = "emdat_cyclone_prob",
          gam_train_stop = gam_train_stop, diagnostics = FALSE, verbose = FALSE)
     utils::write.csv(
          d, file.path(PATHS$DATA_CHOLERA_WEEKLY,
                       "cholera_country_weekly_suitability_data.csv"),
          row.names = FALSE)
     invisible(NULL)
}

testthat::test_that("builder forwards aligned window + gam_train_stop = cutoff and is non-destructive", {
     skip_if_not_installed("mgcv")
     root  <- withr::local_tempdir()
     dchw  <- file.path(root, "dchw");  dir.create(dchw)
     cache <- file.path(root, "cache"); dir.create(cache)
     .mk_raw_cases(dchw)
     # A canonical suitability file that MUST survive (non-destructive contract).
     canon <- file.path(dchw, "cholera_country_weekly_suitability_data.csv")
     writeLines("SENTINEL-CANONICAL", canon)
     canon_before <- readLines(canon)

     PATHS <- list(DATA_CHOLERA_WEEKLY = dchw, DATA_CLIMATE = root, DATA_ENSO = root)
     cutoff <- as.Date("2018-06-30")
     out    <- file.path(cache, sprintf("panel_v74_%s.csv", as.character(cutoff)))

     testthat::local_mocked_bindings(
          compile_suitability_data = .fake_compile, .package = "MOSAIC")

     res <- MOSAIC:::.rcv_build_leakfree_panel_v74(
          PATHS = PATHS, cutoff = cutoff,
          compile_date_start = "2015-01-01", out_csv = out, verbose = FALSE)

     testthat::expect_identical(res, out)
     testthat::expect_true(file.exists(out))
     # forwarded the aligned lower bound + leak-free gam cap = cutoff
     testthat::expect_identical(.args_env$date_start, "2015-01-01")
     testthat::expect_equal(.args_env$gam_train_stop, cutoff)
     # NON-DESTRUCTIVE: compile ran in a scratch dir, not the real one -> the
     # canonical file under the true DATA_CHOLERA_WEEKLY is untouched, and the
     # scratch dir was cleaned up on exit.
     testthat::expect_identical(readLines(canon), canon_before)
     # compile wrote into a scratch dir under the cache, NOT the real
     # DATA_CHOLERA_WEEKLY (compare raw strings; the scratch dir is already
     # cleaned up on.exit so it cannot be normalizePath'd).
     testthat::expect_false(identical(.args_env$write_dir, dchw))
     testthat::expect_identical(dirname(.args_env$write_dir), cache)
     testthat::expect_length(list.files(cache, pattern = "^\\.compile_v74", all.files = TRUE), 0L)
})

testthat::test_that("LEAKAGE: produced panel's <=cutoff hazard probs are independent of >cutoff data", {
     skip_if_not_installed("mgcv")
     root  <- withr::local_tempdir()
     dchw  <- file.path(root, "dchw");  dir.create(dchw)
     cache <- file.path(root, "cache"); dir.create(cache)
     .mk_raw_cases(dchw)
     writeLines("x", file.path(dchw, "cholera_country_weekly_suitability_data.csv"))
     PATHS  <- list(DATA_CHOLERA_WEEKLY = dchw, DATA_CLIMATE = root, DATA_ENSO = root)
     cutoff <- as.Date("2018-06-30")

     # Compiler #1: the honest panel.
     testthat::local_mocked_bindings(
          compile_suitability_data = .fake_compile, .package = "MOSAIC")
     out1 <- file.path(cache, "panel_a.csv")
     MOSAIC:::.rcv_build_leakfree_panel_v74(
          PATHS, cutoff, compile_date_start = "2015-01-01",
          out_csv = out1, verbose = FALSE)
     p1 <- utils::read.csv(out1, stringsAsFactors = FALSE); p1$date <- as.Date(p1$date)

     # Compiler #2: identical EXCEPT future (> cutoff) predictor rows + the
     # future outcome label are perturbed before the GAM. Because the GAM is fit
     # on date <= cutoff, the <=cutoff predicted probs MUST be byte-identical.
     fake_compile_perturbed <- function(PATHS, cutoff, use_epidemic_peaks, date_start,
                                        date_stop, forecast_mode, forecast_horizon,
                                        include_lags, include_flood_prob,
                                        gam_train_stop, ...) {
          d <- .mk_hazard_panel()
          d <- d[d$date >= as.Date(date_start), , drop = FALSE]
          fut <- d$date > as.Date(gam_train_stop)
          set.seed(777L)
          for (cc in c("wind_speed_10m_max", "precipitation_sum",
                       "precip_sum_2w", "precip_sum_4w", "ENSO34", "IOD"))
               d[[cc]][fut] <- d[[cc]][fut] + stats::rnorm(sum(fut), 0, 50)
          d$emdat_cyclone_active[fut] <- 1L - d$emdat_cyclone_active[fut]
          d <- MOSAIC::impute_cyclone_probability(
               d, output_col = "emdat_cyclone_prob",
               gam_train_stop = gam_train_stop, diagnostics = FALSE, verbose = FALSE)
          utils::write.csv(
               d, file.path(PATHS$DATA_CHOLERA_WEEKLY,
                            "cholera_country_weekly_suitability_data.csv"),
               row.names = FALSE)
          invisible(NULL)
     }
     testthat::local_mocked_bindings(
          compile_suitability_data = fake_compile_perturbed, .package = "MOSAIC")
     out2 <- file.path(cache, "panel_b.csv")
     MOSAIC:::.rcv_build_leakfree_panel_v74(
          PATHS, cutoff, compile_date_start = "2015-01-01",
          out_csv = out2, verbose = FALSE)
     p2 <- utils::read.csv(out2, stringsAsFactors = FALSE); p2$date <- as.Date(p2$date)

     pre1 <- p1$date <= cutoff
     pre2 <- p2$date <= cutoff
     testthat::expect_identical(sum(pre1), sum(pre2))
     testthat::expect_equal(p1$emdat_cyclone_prob[pre1], p2$emdat_cyclone_prob[pre2],
                            tolerance = 1e-10)
})
