# Tests for process_IDMC_data()
#
# Uses a synthetic IDU event CSV written on the fly (plain CSV, so unlike the
# EM-DAT fixture no xlsx writer is needed). The fixture exercises:
#
#   - split on displacement_type ("Conflict" / "Disaster") into two panels
#   - filter to MOSAIC AFRO ISOs
#   - multi-week event expansion across ISO-8601 weeks
#   - missing / inverted end date -> single-day event
#   - multiple events colliding in the same country-week (new + displaced sum)
#   - log1p transform of the `figure` (persons displaced) column
#   - zero-fill of the panel outside displacement-active cells
#   - de-duplication on event id when per-country mirrors overlap

.mk_idmc_csv <- function(dir) {
     d <- data.frame(
          id = 1:7,
          iso3 = c("KEN", "KEN", "NGA", "NGA", "MOZ", "FRA", "COD"),
          displacement_type = c("Conflict", "Conflict", "Disaster", "Disaster",
                                "Conflict", "Conflict", "Disaster"),
          figure = c(1000, 500, 2000, 3000, 100, 999999, 50),
          # KEN #1 spans 3 ISO weeks; KEN #2 lands inside week 1 of that span
          displacement_start_date = c("2020-01-06", "2020-01-08", "2020-02-03",
                                      "2020-02-03", "2020-03-02", "2020-01-06",
                                      "2020-04-06"),
          displacement_end_date   = c("2020-01-20", "2020-01-08", "2020-02-03",
                                      NA,             "2020-02-01", "2020-01-06",
                                      "2020-04-06"),
          event_name = "synthetic", type = NA, subtype = NA,
          stringsAsFactors = FALSE
     )
     f <- file.path(dir, "synthetic_idu.csv")
     utils::write.csv(d, f, row.names = FALSE)
     f
}

.mk_idmc_paths <- function(envir = parent.frame()) {
     tmp <- withr::local_tempdir(.local_envir = envir)
     PATHS <- list(
          ROOT          = tmp,
          DATA_IDMC_RAW = file.path(tmp, "raw", "IDMC"),
          DATA_IDMC     = file.path(tmp, "processed", "IDMC", "weekly")
     )
     dir.create(PATHS$DATA_IDMC_RAW, recursive = TRUE, showWarnings = FALSE)
     .mk_idmc_csv(PATHS$DATA_IDMC_RAW)
     PATHS
}

test_that("process_IDMC_data writes two aligned country-week panels", {
     PATHS <- .mk_idmc_paths()
     out <- suppressMessages(process_IDMC_data(PATHS, panel_start = "2020-01-06"))

     expect_named(out, c("conflict", "disaster"))
     expect_true(all(file.exists(out)))

     cf <- utils::read.csv(out[["conflict"]], stringsAsFactors = FALSE)
     df <- utils::read.csv(out[["disaster"]], stringsAsFactors = FALSE)

     # identical grids -- required for the compile_suitability_data join
     expect_equal(nrow(cf), nrow(df))
     expect_equal(sort(unique(cf$iso_code)), sort(unique(df$iso_code)))
     expect_identical(cf[, c("iso_code", "year", "week")],
                      df[, c("iso_code", "year", "week")])

     expect_true(all(c("idmc_conflict_active", "idmc_conflict_new",
                       "idmc_conflict_displaced") %in% names(cf)))
     expect_true(all(c("idmc_disaster_active", "idmc_disaster_new",
                       "idmc_disaster_displaced") %in% names(df)))
})

test_that("non-MOSAIC ISOs are dropped", {
     PATHS <- .mk_idmc_paths()
     out <- suppressMessages(process_IDMC_data(PATHS, panel_start = "2020-01-06"))
     cf <- utils::read.csv(out[["conflict"]], stringsAsFactors = FALSE)
     expect_false("FRA" %in% cf$iso_code)   # France is in the fixture, must not survive
})

test_that("multi-week events expand and collisions sum", {
     PATHS <- .mk_idmc_paths()
     out <- suppressMessages(process_IDMC_data(PATHS, panel_start = "2020-01-06"))
     cf <- utils::read.csv(out[["conflict"]], stringsAsFactors = FALSE)

     ken <- cf[cf$iso_code == "KEN" & cf$idmc_conflict_active == 1, ]
     # 2020-01-06 -> 2020-01-20 touches ISO weeks 2, 3 and 4
     expect_equal(nrow(ken), 3L)

     # both KEN events START in ISO week 2 -> new == 2 there, 0 in the later weeks
     wk2 <- ken[ken$week == 2, ]
     expect_equal(wk2$idmc_conflict_new, 2L)
     expect_equal(sum(ken$idmc_conflict_new), 2L)

     # displaced is log1p of the summed figure: week 2 carries both (1000 + 500)
     expect_equal(wk2$idmc_conflict_displaced, log1p(1500), tolerance = 1e-8)
     # a later week of the same event carries only event #1's figure
     expect_equal(ken$idmc_conflict_displaced[ken$week == 4], log1p(1000), tolerance = 1e-8)
})

test_that("missing or inverted end dates collapse to a single-day event", {
     PATHS <- .mk_idmc_paths()
     out <- suppressMessages(process_IDMC_data(PATHS, panel_start = "2020-01-06"))
     df <- utils::read.csv(out[["disaster"]], stringsAsFactors = FALSE)
     cf <- utils::read.csv(out[["conflict"]], stringsAsFactors = FALSE)

     # NGA #4 has a missing end date -> one active week only; both NGA disaster
     # events start the same week, so new == 2 and figures sum
     nga <- df[df$iso_code == "NGA" & df$idmc_disaster_active == 1, ]
     expect_equal(nrow(nga), 1L)
     expect_equal(nga$idmc_disaster_new, 2L)
     expect_equal(nga$idmc_disaster_displaced, log1p(5000), tolerance = 1e-8)

     # MOZ #5 has end_date BEFORE start_date -> reset to start, single week
     moz <- cf[cf$iso_code == "MOZ" & cf$idmc_conflict_active == 1, ]
     expect_equal(nrow(moz), 1L)
})

test_that("panel is zero-filled outside active cells and carries no NAs", {
     PATHS <- .mk_idmc_paths()
     out <- suppressMessages(process_IDMC_data(PATHS, panel_start = "2020-01-06"))
     cf <- utils::read.csv(out[["conflict"]], stringsAsFactors = FALSE)

     expect_false(anyNA(cf$idmc_conflict_active))
     expect_false(anyNA(cf$idmc_conflict_displaced))
     inactive <- cf[cf$idmc_conflict_active == 0, ]
     expect_true(all(inactive$idmc_conflict_new == 0))
     expect_true(all(inactive$idmc_conflict_displaced == 0))
})

test_that("duplicate event ids across overlapping files are dropped once", {
     PATHS <- .mk_idmc_paths()
     # simulate a per-country HDX mirror overlapping the global export
     file.copy(file.path(PATHS$DATA_IDMC_RAW, "synthetic_idu.csv"),
               file.path(PATHS$DATA_IDMC_RAW, "synthetic_idu_copy.csv"))
     out <- suppressMessages(process_IDMC_data(PATHS, panel_start = "2020-01-06"))
     cf <- utils::read.csv(out[["conflict"]], stringsAsFactors = FALSE)

     # KEN week 2 must still show 2 new events, not 4
     wk2 <- cf[cf$iso_code == "KEN" & cf$week == 2 & cf$idmc_conflict_active == 1, ]
     expect_equal(wk2$idmc_conflict_new, 2L)
     expect_equal(wk2$idmc_conflict_displaced, log1p(1500), tolerance = 1e-8)
})

test_that("informative error when the source directory is absent or empty", {
     PATHS <- .mk_idmc_paths()
     expect_error(process_IDMC_data(PATHS, source_dir = file.path(PATHS$ROOT, "nope")),
                  "source directory not found")
     empty <- file.path(PATHS$ROOT, "empty"); dir.create(empty)
     expect_error(suppressMessages(process_IDMC_data(PATHS, source_dir = empty)),
                  "No IDU event CSVs")
})
