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
#   - (v7.4) the SEPARATE cyclone label: Storm + Tropical cyclone/Storm surge
#     kept, Storm-General/Severe/Lightning dropped

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


# process_EMDAT_data() now returns c(flood = ..., cyclone = ...); the flood
# tests below read the flood path explicitly.
testthat::test_that("filters to floods and MOSAIC AFRO ISOs", {
     PATHS <- .mk_emdat_paths()
     out_files <- MOSAIC::process_EMDAT_data(PATHS)
     out_file <- out_files[["flood"]]
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
     out_file <- MOSAIC::process_EMDAT_data(PATHS)[["flood"]]
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
     out_file <- MOSAIC::process_EMDAT_data(PATHS)[["flood"]]
     d <- utils::read.csv(out_file, stringsAsFactors = FALSE)

     # UGA fixture: Start/End Day both NA in May 2015 -> imputed to May 15 (Fri W20)
     uga_active <- d[d$iso_code == "UGA" & d$emdat_flood_active == 1, ]
     testthat::expect_equal(nrow(uga_active), 1)
     testthat::expect_equal(uga_active$year, 2015)
     testthat::expect_equal(uga_active$week, 20)
})


testthat::test_that("multiple events in the same country-week are summed", {
     PATHS <- .mk_emdat_paths()
     out_file <- MOSAIC::process_EMDAT_data(PATHS)[["flood"]]
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
     out_file <- MOSAIC::process_EMDAT_data(PATHS)[["flood"]]
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


# ---- Cyclone series (v7.4) --------------------------------------------------
# The fixture adds five Storm rows: MOZ "Tropical cyclone" (KEPT), ZWE
# "Storm surge" (KEPT), and NGA "Storm (General)" / KEN "Severe weather" /
# MWI "Lightning/Thunderstorms" (all DROPPED). Only the first two should
# appear as active cyclone-weeks.

testthat::test_that("cyclone panel keeps only Tropical cyclone + Storm surge", {
     PATHS <- .mk_emdat_paths()
     out_files <- MOSAIC::process_EMDAT_data(PATHS)
     cyc_file <- out_files[["cyclone"]]
     testthat::expect_true(file.exists(cyc_file))
     d <- utils::read.csv(cyc_file, stringsAsFactors = FALSE)

     # Output columns match the cyclone contract
     testthat::expect_setequal(
          colnames(d),
          c("iso_code", "year", "week", "date_start", "date_stop",
            "emdat_cyclone_active", "emdat_cyclone_new",
            "emdat_cyclone_affected", "emdat_cyclone_deaths")
     )

     active <- d[d$emdat_cyclone_active == 1, c("iso_code", "year", "week")]
     active <- active[order(active$iso_code, active$year, active$week), ]
     row.names(active) <- NULL

     # MOZ TC: 2020-01-20 (Mon W04) .. 2020-01-26 (Sun W04) -> single week W04
     # ZWE surge: 2021-02-08 (Mon W06) .. 2021-02-14 (Sun W06) -> single week W06
     expected <- data.frame(
          iso_code = c("MOZ", "ZWE"),
          year     = c(2020,  2021),
          week     = c(4,     6),
          stringsAsFactors = FALSE
     )
     testthat::expect_equal(active, expected)

     # Excluded storm subtypes produce NO active cyclone-weeks
     testthat::expect_equal(sum(d$emdat_cyclone_active[d$iso_code == "NGA"]), 0)  # Storm (General)
     testthat::expect_equal(sum(d$emdat_cyclone_active[d$iso_code == "KEN"]), 0)  # Severe weather
     testthat::expect_equal(sum(d$emdat_cyclone_active[d$iso_code == "MWI"]), 0)  # Lightning
})


testthat::test_that("cyclone panel is complete, zero-filled, and magnitude-correct", {
     PATHS <- .mk_emdat_paths()
     d <- utils::read.csv(MOSAIC::process_EMDAT_data(PATHS)[["cyclone"]],
                          stringsAsFactors = FALSE)

     testthat::expect_setequal(unique(d$iso_code), sort(MOSAIC::iso_codes_mosaic))

     # MOZ TC magnitude: Total Affected 5000, Deaths 50, single starting week
     moz <- d[d$iso_code == "MOZ" & d$emdat_cyclone_active == 1, ]
     testthat::expect_equal(nrow(moz), 1)
     testthat::expect_equal(moz$emdat_cyclone_new, 1)
     testthat::expect_equal(moz$emdat_cyclone_affected, log1p(5000), tolerance = 1e-9)
     testthat::expect_equal(moz$emdat_cyclone_deaths,   log1p(50),   tolerance = 1e-9)

     # Non-active cells zero everywhere
     inactive <- d[d$emdat_cyclone_active == 0, ]
     testthat::expect_true(all(inactive$emdat_cyclone_new      == 0))
     testthat::expect_true(all(inactive$emdat_cyclone_affected == 0))
     testthat::expect_true(all(inactive$emdat_cyclone_deaths   == 0))
})
