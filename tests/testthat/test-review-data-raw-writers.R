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

test_that("an HDX transport failure is a failed download, only a not-found is an absence", {
     cls <- MOSAIC:::.idmc_classify_hdx_response
     ok_body <- '{"success":true,"result":{"resources":[{"format":"CSV","url":"https://x/ken.csv"},{"format":"XLSX","url":"https://x/ken.xlsx"}]}}'
     expect_equal(cls(200L, ok_body)$url, "https://x/ken.csv")
     a404 <- cls(404L, "Not found"); af <- cls(200L, '{"success":false}')
     expect_true(a404$absent); expect_null(a404$url); expect_true(af$absent)
     expect_equal(a404$note, MOSAIC:::.IDMC_ABSENT_NOTE)
     for (r in list(cls(NA_integer_, NULL, "Timeout was reached"), cls(503L, ""),
                    cls(200L, "<html>maintenance</html>"),
                    cls(200L, '{"success":true,"result":{"resources":[]}}'),
                    cls(200L, '{"success":true,"result":{"resources":[{"format":"XLSX","url":"u"}]}}'))) {
          expect_false(r$absent); expect_null(r$url)
          expect_false(identical(r$note, MOSAIC:::.IDMC_ABSENT_NOTE))
     }
     expect_match(cls(NA_integer_, NULL, "Timeout was reached")$note, "Timeout")

     # A snapshot whose lookups failed on the network is not complete
     snap <- withr::local_tempdir()
     man <- data.frame(iso_code = c("KEN", "NGA", "ERI"), ok = c(TRUE, FALSE, FALSE),
                       n_events = c(1, NA, NA),
                       note = c("", "HDX lookup failed: Timeout was reached", MOSAIC:::.IDMC_ABSENT_NOTE))
     utils::write.table(man, file.path(snap, "MANIFEST.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
     expect_false(MOSAIC:::.idmc_snapshot_complete(snap, c("KEN", "NGA", "ERI")))
})

test_that("download_IDMC_data records a lookup outage as ok = FALSE and leaves the snapshot partial", {
     PATHS <- list(DATA_IDMC_RAW = withr::local_tempdir())
     local_mocked_bindings(.idmc_hdx_resource_url = function(iso) {
          if (iso == "ERI") list(url = NULL, absent = TRUE, note = MOSAIC:::.IDMC_ABSENT_NOTE)
          else list(url = NULL, absent = FALSE, note = "HDX lookup failed: Could not resolve host")
     })
     out <- download_IDMC_data(PATHS, iso_codes = c("ERI", "KEN"), snapshot_date = as.Date("2026-09-29"),
                               verbose = FALSE)
     expect_equal(out$ok, c(FALSE, FALSE))
     expect_equal(out$note[2], "HDX lookup failed: Could not resolve host")
     snap <- file.path(PATHS$DATA_IDMC_RAW, "hdx_2026-09-29")
     expect_false(MOSAIC:::.idmc_snapshot_complete(snap, c("ERI", "KEN")))
})

test_that("WHO vaccination table is written as a dated, logged snapshot and never overwritten", {
     dir <- withr::local_tempdir()
     PATHS <- list(DATA_SCRAPE_WHO_VACCINATION = dir)
     legacy <- file.path(dir, "who_vaccination_data.csv")
     writeLines("old,content", legacy)
     expect_equal(MOSAIC:::.who_vaccination_latest_file(dir), legacy)
     suppressMessages(capture.output(get_WHO_vaccination_data(PATHS)))
     expect_equal(readLines(legacy), "old,content")        # legacy file untouched
     snaps <- list.files(dir, pattern = "^who_vaccination_data_snapshot_")
     expect_length(snaps, 1L)
     expect_equal(basename(MOSAIC:::.who_vaccination_latest_file(dir)), snaps)
     expect_true(file.exists(file.path(dir, "PROVENANCE.md")))
     # identical rebuild: nothing new written, nothing logged
     prov <- readLines(file.path(dir, "PROVENANCE.md"))
     suppressMessages(capture.output(get_WHO_vaccination_data(PATHS)))
     expect_length(list.files(dir, pattern = "^who_vaccination_data_snapshot_"), 1L)
     expect_equal(readLines(file.path(dir, "PROVENANCE.md")), prov)
     # a same-day path never collides with an existing snapshot
     now <- as.POSIXct("2031-01-01 10:15:00")
     p1 <- MOSAIC:::.who_vaccination_snapshot_path(dir, now)
     writeLines("x", p1)
     p2 <- MOSAIC:::.who_vaccination_snapshot_path(dir, now)
     expect_equal(basename(p2), "who_vaccination_data_snapshot_2031-01-01_101500.csv")
     writeLines("y", p2)
     expect_equal(MOSAIC:::.who_vaccination_latest_file(dir), p2)
})

test_that("download_country_DEM logs each downloaded raster in PROVENANCE.md", {
     skip_if_not_installed("raster")
     tmp <- withr::local_tempdir()
     PATHS <- list(DATA_DEM = file.path(tmp, "DEM"), DATA_SHAPEFILES = file.path(tmp, "shp"))
     dir.create(PATHS$DATA_SHAPEFILES)
     file.create(file.path(PATHS$DATA_SHAPEFILES, "KEN_ADM0.shp"))
     local_mocked_bindings(st_read = function(...) data.frame(id = 1), .package = "sf")
     local_mocked_bindings(get_elev_raster = function(...) raster::raster(matrix(1:4, 2)),
                           .package = "elevatr")
     suppressMessages(download_country_DEM(PATHS, "KEN"))
     expect_true(file.exists(file.path(PATHS$DATA_DEM, "KEN_1km_DEM.tif")))
     prov <- readLines(file.path(PATHS$DATA_DEM, "PROVENANCE.md"))
     expect_true(any(grepl("KEN_1km_DEM.tif", prov, fixed = TRUE)))
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

test_that("process_WB_GDP_data writes world_bank_GDP_data.csv and creates its directory", {
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
