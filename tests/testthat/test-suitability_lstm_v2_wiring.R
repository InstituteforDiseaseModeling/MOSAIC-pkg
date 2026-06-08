# Phase-2 wiring tests for the lstm_v2 path. The keras-free tests (contract,
# arch_control loader, feature resolver) run in CI; the end-to-end orchestrator
# smoke is Tier-3 (skipped unless keras3 + the suitability CSV are available).

ns <- "MOSAIC"
psi_matrix      <- getFromNamespace(".rolling_cv_psi_matrix", ns)
load_arch       <- getFromNamespace(".psi_load_arch_control", ns)
resolve_feats   <- getFromNamespace(".psi_resolve_features", ns)

# ----------------------------------------------------------------------------
# Contract (Tier-2): .rolling_cv_psi_matrix() consumes the canonical `psi`
# column and yields a full, non-flat locations x dates matrix.
# ----------------------------------------------------------------------------
test_that(".rolling_cv_psi_matrix builds a full non-flat matrix from `psi`", {
  dates <- seq(as.Date("2023-01-05"), by = "day", length.out = 120)
  mk <- function(iso, amp) data.frame(
    iso_code = iso, date = dates,
    psi = pmin(0.95, pmax(0.05, amp * (0.5 + 0.4 * sin(seq_along(dates) / 20)))),
    pred_smooth = 0.5, stringsAsFactors = FALSE)   # pred_smooth present but NOT consumed
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(rbind(mk("MOZ", 1.0), mk("KEN", 0.6)), csv, row.names = FALSE)

  m <- psi_matrix(csv, c("MOZ", "KEN"), dates)
  expect_equal(dim(m), c(2L, length(dates)))
  expect_equal(rownames(m), c("MOZ", "KEN"))
  expect_false(anyNA(m))                       # full
  expect_gt(stats::sd(m["MOZ", ]), 0)          # non-flat
  expect_gt(stats::sd(m["KEN", ]), 0)
  # consumes psi, NOT pred_smooth (which is a constant 0.5 here)
  expect_false(isTRUE(all.equal(as.numeric(m["MOZ", ]), rep(0.5, length(dates)))))
})

test_that(".rolling_cv_psi_matrix errors on a stale CSV lacking `psi`", {
  dates <- seq(as.Date("2023-01-05"), by = "day", length.out = 30)
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(data.frame(iso_code = "MOZ", date = dates, pred_smooth = 0.4),
                   csv, row.names = FALSE)
  expect_error(psi_matrix(csv, "MOZ", dates), "canonical `psi` column")
})

# ----------------------------------------------------------------------------
# arch_control loader (Tier-1): fixture + production overrides + int coercion +
# user modifyList.
# ----------------------------------------------------------------------------
test_that(".psi_load_arch_control applies the two production overrides + int coercion", {
  ac <- load_arch(NULL)
  expect_equal(ac$n_seeds, 5L)           # production override (fixture pins 3)
  expect_equal(ac$region_map, "snf_k5")  # production override (fixture pins csv)
  expect_equal(ac$parallel_seeds, 1L)    # execution default
  expect_true(is.integer(ac$timesteps) && ac$timesteps == 13L)
  expect_true(is.integer(ac$epochs) && ac$epochs == 200L)
  expect_equal(ac$loss_kind, "bce")
  expect_equal(ac$rw_subsample, 6L)
})

test_that(".psi_load_arch_control reverts to B4 via user overrides; coerces doubles", {
  ac <- load_arch(list(n_seeds = 3, region_map = "csv"))  # n_seeds=3 is a double here
  expect_equal(ac$n_seeds, 3L)           # B4 reproduction
  expect_true(is.integer(ac$n_seeds))    # coerced from double
  expect_equal(ac$region_map, "csv")
})

# ----------------------------------------------------------------------------
# feature resolver (uses a synthetic CSV header): v7.3 -> 38, schema guard,
# default -> full set + lag columns.
# ----------------------------------------------------------------------------
test_that(".psi_resolve_features resolves v7.3 (38), guards schema, and expands default", {
  v73 <- MOSAIC::MINFEAT_V7_3_FEATURE_SET
  cols <- c("iso_code", "date", "cases", v73,
            "total_population", "GDP", "ENSO4_lag99", "precip_anom_lag99")
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(setNames(as.data.frame(as.list(rep(0, length(cols)))), cols)[0, ],
                   csv, row.names = FALSE)

  f73 <- resolve_feats("v7.3", csv)
  expect_setequal(f73, v73)
  expect_equal(length(f73), 38L)

  # default: curated full candidates present in the CSV + any _lag columns
  fdef <- resolve_feats("default", csv)
  expect_true(all(c("ENSO4_lag99", "precip_anom_lag99") %in% fdef))  # lag cols picked up
  expect_true("total_population" %in% fdef && "GDP" %in% fdef)

  # schema guard: drop one v7.3 feature -> error
  cols2 <- setdiff(cols, "ENSO4")
  csv2 <- tempfile(fileext = ".csv")
  on.exit(unlink(csv2), add = TRUE)
  utils::write.csv(setNames(as.data.frame(as.list(rep(0, length(cols2)))), cols2)[0, ],
                   csv2, row.names = FALSE)
  expect_error(resolve_feats("v7.3", csv2), "absent from the suitability CSV")
})

test_that("get_feature_set rejects unknown names (sole public selector)", {
  expect_error(resolve_feats("not_a_set", tempfile()), "should be one of")
})

# ----------------------------------------------------------------------------
# Tier-3 (local, keras): end-to-end orchestrator schema smoke + bias_correct
# changes psi. Skipped unless keras3 + the production suitability CSV exist.
# ----------------------------------------------------------------------------
test_that("est_suitability(lstm_v2) emits the Option-A schema and bias_correct moves psi", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("keras3")
  testthat::skip_if_not_installed("reticulate")
  if (!nzchar(Sys.getenv("MOSAIC_RUN_KERAS_TESTS")))
    testthat::skip("set MOSAIC_RUN_KERAS_TESTS=1 to run the keras end-to-end smoke")

  suppressMessages(MOSAIC::set_root_directory(Sys.getenv("MOSAIC_ROOT", "~/MOSAIC")))
  PATHS <- MOSAIC::get_paths()
  csv <- file.path(PATHS$DATA_CHOLERA_WEEKLY, "cholera_country_weekly_suitability_data.csv")
  testthat::skip_if(!file.exists(csv), "suitability CSV not available")

  td <- file.path(tempdir(), "psi_smoke"); dir.create(td, showWarnings = FALSE)
  P <- PATHS; P$MODEL_INPUT <- td
  fast <- list(n_seeds = 1L, rw_subsample = 12L, region_map = "csv")
  for (bc in c(TRUE, FALSE)) {
    MOSAIC::est_suitability(P, fit_date_start = "2018-01-01",
                            fit_date_stop = "2023-06-01", pred_date_stop = "2023-11-01",
                            bias_correct = bc, arch_control = fast)
    day <- utils::read.csv(file.path(td, "pred_psi_suitability_day.csv"))
    expect_true(all(c("iso_code","date","psi","pred_raw","pred_smooth",
                      "pred_bias_corrected","q025","q25","q75","q975") %in% names(day)))
    assign(paste0("psi_", bc), day$psi)
  }
  # bias_correct=TRUE changes psi vs FALSE for at least some rows
  expect_false(isTRUE(all.equal(get("psi_TRUE"), get("psi_FALSE"))))
})
