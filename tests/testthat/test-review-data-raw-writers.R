# Regression tests for raw-snapshot writer compliance: atomic writes,
# provenance logging, never-overwrite, newest-wins, complete-snapshot
# resolution (deep review data-pipeline-04, -05, -09 and tracker items).

test_that(".write_file_atomic leaves no partial file when the writer fails", {
     tmp <- withr::local_tempdir()
     dest <- file.path(tmp, "a.csv")
     writeLines("old", dest)
     expect_error(MOSAIC:::.write_file_atomic(dest, function(t) {
          writeLines("partial", t); stop("boom")
     }), "boom")
     expect_equal(readLines(dest), "old")
     expect_equal(list.files(tmp, all.files = TRUE, no.. = TRUE), "a.csv")
     MOSAIC:::.write_file_atomic(dest, function(t) writeLines("new", t))
     expect_equal(readLines(dest), "new")
})

test_that(".append_raw_provenance creates then appends one row per call", {
     tmp <- withr::local_tempdir()
     MOSAIC:::.append_raw_provenance(tmp, "x_2026-01-01.csv", 3, 2, "a|b", as.Date("2026-01-01"))
     MOSAIC:::.append_raw_provenance(tmp, "y.csv", NA, NA, "note", as.Date("2026-01-02"))
     l <- readLines(file.path(tmp, "PROVENANCE.md"))
     expect_equal(length(grep("^\\| 2026-", l)), 2L)
     expect_true(any(grepl("| 2026-01-01 | x_2026-01-01.csv | 3 | 2 | a/b |", l, fixed = TRUE)))
})

# ---- WHO annual dashboard snapshots ------------------------------------------

.dash_csv <- function(path, eth_cases = 53, truncate = FALSE) {
     txt <- c('"adm0_name","who_region","iso_3_code","first_epiwk","last_epiwk","case_total","death_total"',
              sprintf('"ETHIOPIA","African Region","ETH",2025-12-29,2026-03-01,%d,1', eth_cases))
     if (truncate) {
          writeBin(charToRaw(substr(paste(txt, collapse = "\n"), 1, 150)), path)
     } else {
          writeLines(txt, path)
     }
     path
}

test_that("dashboard fetch writes a validated, logged snapshot and never overwrites or duplicates", {
     src <- withr::local_tempdir(); dash <- withr::local_tempdir()
     url <- paste0("file://", .dash_csv(file.path(src, "d.csv")))
     now <- as.POSIXct("2026-09-29 10:00:00", tz = "UTC")

     p1 <- suppressMessages(MOSAIC:::.who_dashboard_fetch_snapshot(url, dash, now = now))
     expect_equal(basename(p1), "cholera_adm0_public_snapshot_2026-09-29.csv")
     expect_true(file.exists(file.path(dash, "PROVENANCE.md")))

     # identical content -> no new snapshot
     p2 <- suppressMessages(MOSAIC:::.who_dashboard_fetch_snapshot(url, dash, now = now + 60))
     expect_null(p2)
     expect_equal(length(list.files(dash, pattern = "^cholera")), 1L)

     # revised content same day -> new time-stamped file; the original is untouched
     before <- tools::md5sum(p1)
     .dash_csv(file.path(src, "d.csv"), eth_cases = 50)
     p3 <- suppressMessages(MOSAIC:::.who_dashboard_fetch_snapshot(url, dash, now = now + 3600))
     expect_equal(basename(p3), "cholera_adm0_public_snapshot_2026-09-29_110000.csv")
     expect_equal(tools::md5sum(p1), before)
     expect_equal(length(grep("^\\| 2026-", readLines(file.path(dash, "PROVENANCE.md")))), 2L)

     # truncated fetch -> rejected, nothing written
     .dash_csv(file.path(src, "d.csv"), eth_cases = 49, truncate = TRUE)
     p4 <- suppressMessages(MOSAIC:::.who_dashboard_fetch_snapshot(url, dash, now = now + 7200))
     expect_null(p4)
     expect_equal(length(list.files(dash, pattern = "^cholera")), 2L)
})

test_that("dashboard files rank newest-first by stamp, undated files last", {
     f <- c("cholera_adm0_public_2025.csv",
            "cholera_adm0_public_snapshot_2026-09-18.csv",
            "cholera_adm0_public_snapshot_2026-09-18_101500.csv",
            "cholera_adm0_public_snapshot_2026-06-02.csv")
     expect_equal(order(MOSAIC:::.who_dashboard_file_recency(f), decreasing = TRUE), c(3L, 2L, 4L, 1L))
})

test_that("country names are harmonised to one spelling per ISO code", {
     iso <- c("CIV", "CIV", "CIV", "ETH")
     nm  <- c("Cote d'Ivoire", "Cote d'Ivoire", "Côte D’ivoire", "Ethiopia")
     expect_equal(MOSAIC:::.who_annual_canonical_names(iso, nm),
                  c("Cote d'Ivoire", "Cote d'Ivoire", "Cote d'Ivoire", "Ethiopia"))
})

# ---- IDMC / mobility complete-snapshot resolution ----------------------------

test_that("IDMC resolver skips a newer partial snapshot and honours the manifest", {
     raw <- withr::local_tempdir()
     old <- file.path(raw, "hdx_2026-09-17"); new <- file.path(raw, "hdx_2026-09-18")
     dir.create(old); dir.create(new)
     writeLines("iso3", file.path(old, "ken_idmc_idu_events.csv"))
     writeLines("iso3", file.path(new, "ken_idmc_idu_events.csv"))
     man <- data.frame(iso_code = c("KEN", "NGA", "ERI"), ok = c(TRUE, FALSE, FALSE),
                       n_events = c(1, NA, NA), note = c("", "HTTP 500", "no HDX dataset"))
     utils::write.table(man, file.path(new, "MANIFEST.tsv"), sep = "\t", row.names = FALSE,
                        quote = FALSE)
     expect_false(MOSAIC:::.idmc_snapshot_complete(new))
     expect_warning(got <- suppressMessages(MOSAIC:::.idmc_latest_snapshot(raw)), "partial")
     expect_equal(basename(got), "hdx_2026-09-17")      # legacy (no manifest) accepted

     man$ok[2] <- TRUE
     utils::write.table(man, file.path(new, "MANIFEST.tsv"), sep = "\t", row.names = FALSE,
                        quote = FALSE)
     expect_true(MOSAIC:::.idmc_snapshot_complete(new, c("KEN", "NGA")))
     expect_false(MOSAIC:::.idmc_snapshot_complete(new, c("KEN", "COD")))
     expect_equal(basename(suppressMessages(MOSAIC:::.idmc_latest_snapshot(raw))), "hdx_2026-09-18")
})

test_that("mobility-OD resolver prefers the newest snapshot holding all three sources", {
     raw <- withr::local_tempdir()
     base <- file.path(raw, "mobility_od")
     old <- file.path(base, "snapshot_2026-09-18"); new <- file.path(base, "snapshot_2026-09-29")
     dir.create(old, recursive = TRUE); dir.create(new)
     for (f in MOSAIC:::.MOBILITY_OD_FILES) writeLines("x", file.path(old, f))
     writeLines("x", file.path(new, MOSAIC:::.MOBILITY_OD_FILES[["sci"]]))
     PATHS <- list(DATA_RAW = raw)
     expect_warning(got <- MOSAIC:::.mobility_od_newest_snapshot(PATHS), "partial")
     expect_equal(basename(got), "snapshot_2026-09-18")
     for (f in MOSAIC:::.MOBILITY_OD_FILES) writeLines("x", file.path(new, f))
     expect_equal(basename(MOSAIC:::.mobility_od_newest_snapshot(PATHS)), "snapshot_2026-09-29")
})

# ---- DEM + World Bank processors --------------------------------------------

test_that("download_country_DEM skips an existing raster without re-downloading", {
     tmp <- withr::local_tempdir()
     PATHS <- list(DATA_DEM = file.path(tmp, "DEM"), DATA_SHAPEFILES = file.path(tmp, "shp"))
     dir.create(PATHS$DATA_DEM)
     f <- file.path(PATHS$DATA_DEM, "KEN_1km_DEM.tif")
     writeLines("existing", f)
     # no shapefile exists: a download attempt would be skipped for that reason
     # instead, so assert on the skip message for the existing file
     expect_message(out <- download_country_DEM(PATHS, "KEN"), "already exists")
     expect_equal(readLines(f), "existing")
     expect_equal(out, f)
})

test_that("process_WB_GDP_data writes the name compile reads and creates its directory", {
     tmp <- withr::local_tempdir()
     d <- file.path(tmp, "raw", "world_bank", "GDP"); dir.create(d, recursive = TRUE)
     f <- file.path(d, "API_NY.GDP.MKTP.CD_DS2_en_csv_v2_api_2026-09-18.csv")
     writeLines(c('"Data Source","World Development Indicators",', '',
                  '"Last Updated Date","2026-07-13",', '',
                  '"Country Name","Country Code","Indicator Name","Indicator Code","2020","2021"',
                  '"Kenya","KEN","GDP","NY.GDP.MKTP.CD",1,2'), f)
     PATHS <- list(DATA_RAW = file.path(tmp, "raw"), DATA_PROCESSED = file.path(tmp, "processed"))
     out <- suppressMessages(process_WB_GDP_data(PATHS))
     expect_true(file.exists(file.path(tmp, "processed", "world_bank", "world_bank_GDP_data.csv")))
     expect_equal(out$GDP, c(1, 2))
})
