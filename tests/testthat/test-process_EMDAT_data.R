# Tests for process_EMDAT_data()
#
# Uses a pre-built synthetic xlsx fixture (built once and committed under
# tests/testthat/fixtures/) so the tests do not depend on an xlsx-writer
# package being installed. The fixture mimics the EM-DAT public-extract
# schema and contains a small set of events that exercise:
#
#   - filter to Disaster Type == "Flood"
#   - filter to MOSAIC AFRO ISOs
#   - multi-week event expansion across ISO-8601 weeks
#   - missing-day-of-month imputation
#   - multiple events colliding in the same country-week
#   - zero-fill of the country-week panel outside flood-active cells

# Fixture file is small enough to read once per test
.emdat_fixture <- testthat::test_path("fixtures", "public_emdat_2099-01-01_synthetic.xlsx")

.mk_emdat_paths <- function(envir = parent.frame()) {
     tmp <- withr::local_tempdir(.local_envir = envir)
     PATHS <- list(
          ROOT            = tmp,
          DATA_EMDAT_RAW  = file.path(tmp, "raw",       "EMDAT"),
          DATA_EMDAT      = file.path(tmp, "processed", "EMDAT", "weekly")
     )
     dir.create(PATHS$DATA_EMDAT_RAW, recursive = TRUE, showWarnings = FALSE)
     file.copy(.emdat_fixture, PATHS$DATA_EMDAT_RAW)
     PATHS
}

# Expected active cells from the fixture
.expected_active <- data.frame(
     iso_code = c("KEN", "NGA", "NGA", "NGA", "UGA", "MOZ"),
     year     = c(2015,  2015,  2015,  2015,  2015,  2010),
     week     = c(19,    19,    20,    21,    20,    11),
     stringsAsFactors = FALSE
)


testthat::test_that("filters to floods and MOSAIC AFRO ISOs", {
     PATHS <- .mk_emdat_paths()
     out_file <- MOSAIC::process_EMDAT_data(PATHS)
     d <- utils::read.csv(out_file, stringsAsFactors = FALSE)

     testthat::expect_true(file.exists(out_file))
     # Panel restricted to MOSAIC AFRO ISOs
     testthat::expect_true(all(d$iso_code %in% MOSAIC::iso_codes_mosaic))
     testthat::expect_false("USA" %in% d$iso_code)
     # KEN epidemic row in June should not produce any active flood-weeks for KEN in June 2015
     ken_june <- d[d$iso_code == "KEN" & d$year == 2015 & d$week %in% 23:27, ]
     testthat::expect_true(all(ken_june$emdat_flood_active == 0))
})


testthat::test_that("multi-week events expand across every spanned ISO-week", {
     PATHS <- .mk_emdat_paths()
     out_file <- MOSAIC::process_EMDAT_data(PATHS)
     d <- utils::read.csv(out_file, stringsAsFactors = FALSE)

     # NGA fixture: 2015-05-04 (Mon W19) through 2015-05-24 (Sun W21)
     nga_active <- d[d$iso_code == "NGA" & d$emdat_flood_active == 1, ]
     testthat::expect_equal(sort(nga_active$week), c(19, 20, 21))
     testthat::expect_true(all(nga_active$year == 2015))

     # Only the starting week (W19) gets the 'new event' count
     testthat::expect_equal(nga_active$emdat_flood_new[nga_active$week == 19], 1)
     testthat::expect_equal(nga_active$emdat_flood_new[nga_active$week == 20], 0)
     testthat::expect_equal(nga_active$emdat_flood_new[nga_active$week == 21], 0)

     # Magnitude is repeated across every spanned week (log1p of single event total)
     testthat::expect_equal(unique(nga_active$emdat_flood_affected),
                            log1p(1000), tolerance = 1e-9)
     testthat::expect_equal(unique(nga_active$emdat_flood_deaths),
                            log1p(5),    tolerance = 1e-9)
})


testthat::test_that("missing Start/End Day are imputed to mid-month", {
     PATHS <- .mk_emdat_paths()
     out_file <- MOSAIC::process_EMDAT_data(PATHS)
     d <- utils::read.csv(out_file, stringsAsFactors = FALSE)

     # UGA fixture: Start/End Day both NA in May 2015 -> imputed to May 15 (Fri W20)
     uga_active <- d[d$iso_code == "UGA" & d$emdat_flood_active == 1, ]
     testthat::expect_equal(nrow(uga_active), 1)
     testthat::expect_equal(uga_active$year, 2015)
     testthat::expect_equal(uga_active$week, 20)
})


testthat::test_that("multiple events in the same country-week are summed", {
     PATHS <- .mk_emdat_paths()
     out_file <- MOSAIC::process_EMDAT_data(PATHS)
     d <- utils::read.csv(out_file, stringsAsFactors = FALSE)

     # KEN fixture: two single-day floods (100, 2) and (300, 8) both in 2015-W19
     cell <- d[d$iso_code == "KEN" & d$year == 2015 & d$week == 19, ]
     testthat::expect_equal(nrow(cell), 1)
     testthat::expect_equal(cell$emdat_flood_active,   1)
     testthat::expect_equal(cell$emdat_flood_new,      2)
     testthat::expect_equal(cell$emdat_flood_affected, log1p(400), tolerance = 1e-9)
     testthat::expect_equal(cell$emdat_flood_deaths,   log1p(10),  tolerance = 1e-9)
})


testthat::test_that("panel is complete and zero-filled outside active cells", {
     PATHS <- .mk_emdat_paths()
     out_file <- MOSAIC::process_EMDAT_data(PATHS)
     d <- utils::read.csv(out_file, stringsAsFactors = FALSE)

     # Panel covers every AFRO ISO
     testthat::expect_setequal(unique(d$iso_code), sort(MOSAIC::iso_codes_mosaic))

     # Output columns match the contract
     testthat::expect_setequal(
          colnames(d),
          c("iso_code", "year", "week", "date_start", "date_stop",
            "emdat_flood_active", "emdat_flood_new",
            "emdat_flood_affected", "emdat_flood_deaths")
     )

     # Active cells exactly match the expected set
     active <- d[d$emdat_flood_active == 1, c("iso_code", "year", "week")]
     active <- active[order(active$iso_code, active$year, active$week), ]
     expected <- .expected_active[order(.expected_active$iso_code,
                                         .expected_active$year,
                                         .expected_active$week), ]
     row.names(active) <- NULL; row.names(expected) <- NULL
     testthat::expect_equal(active, expected)

     # Every non-active cell has 0 across all flood covariates
     inactive <- d[d$emdat_flood_active == 0, ]
     testthat::expect_true(all(inactive$emdat_flood_new      == 0))
     testthat::expect_true(all(inactive$emdat_flood_affected == 0))
     testthat::expect_true(all(inactive$emdat_flood_deaths   == 0))
})


testthat::test_that("errors if no public_emdat_*.xlsx is found", {
     PATHS <- .mk_emdat_paths()
     file.remove(list.files(PATHS$DATA_EMDAT_RAW, full.names = TRUE))
     testthat::expect_error(
          MOSAIC::process_EMDAT_data(PATHS),
          "No 'public_emdat_\\*\\.xlsx' files found"
     )
})
