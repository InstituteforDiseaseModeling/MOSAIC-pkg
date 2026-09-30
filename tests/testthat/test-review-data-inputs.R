# Regression tests for vaccination combining, literature-derived inputs,
# the binary suitability label, IDMC role filtering, repo-refresh coverage
# and shapefile rewrites (deep review data-pipeline-08, -11, -12, -13, -14,
# -15 and tracker items).

.vacc_rows <- function(ids, iso, dates, doses) {
     data.frame(year = as.integer(substr(dates, 1, 4)), country = iso,
                request_number = ids, status = "Approved", context = "Outbreak response",
                decision_date = dates, doses_requested = doses, doses_approved = doses,
                doses_shipped = doses, campaign_date = dates, id = ids, iso_code = iso,
                delay = 0, stringsAsFactors = FALSE)
}

test_that("combine_vaccination_data labels every matched campaign and never reuses a GTFCC campaign", {
     tmp <- withr::local_tempdir()
     # GTFCC rows deliberately NOT in date order, so positions change on re-sort
     gtfcc <- .vacc_rows(1:4, c("KEN", "MOZ", "ETH", "NGA"),
                         c("2020-06-01", "2020-01-01", "2020-03-01", "2020-09-01"),
                         c(1e5, 2e5, 3e5, 4e5))
     who <- .vacc_rows(11:15, c("KEN", "MOZ", "ETH", "NGA", "NGA"),
                       c("2020-06-03",  # exact  -> KEN
                         "2020-02-10",  # fuzzy  -> MOZ (40 d, doses equal)
                         "2020-03-10",  # date-only -> ETH (doses differ 3x)
                         "2020-09-05",  # date-only -> NGA (doses differ)
                         "2020-09-20"), # second NGA round within 30 d: must be kept
                       c(1e5, 2e5, 1e5, 1e5, 5e4))
     utils::write.csv(gtfcc, file.path(tmp, "data_vaccinations_GTFCC.csv"), row.names = FALSE)
     utils::write.csv(who,   file.path(tmp, "data_vaccinations_WHO.csv"),   row.names = FALSE)
     utils::capture.output(out <- suppressMessages(combine_vaccination_data(list(MODEL_INPUT = tmp))))

     matched <- out[out$source == "GTFCC_WHO_matched", ]
     expect_false(anyNA(out$match_confidence))
     conf <- stats::setNames(matched$match_confidence, matched$iso_code)
     expect_equal(conf[["KEN"]], "high")
     expect_equal(conf[["MOZ"]], "medium")
     expect_equal(conf[["ETH"]], "low")
     expect_equal(conf[["NGA"]], "low")
     # the second NGA round is retained as WHO_only (its doses are not lost)
     who_only <- out[out$source == "WHO_only", ]
     expect_equal(nrow(who_only), 1L)
     expect_equal(who_only$doses_shipped, 5e4)
     expect_equal(names(out)[ncol(out)], "match_confidence")
})

test_that("get_suspected_cases fits the published 95% interval (2.5%, 50%, 97.5%)", {
     tmp <- withr::local_tempdir()
     utils::capture.output(out <- suppressMessages(get_suspected_cases(list(MODEL_INPUT = tmp))))
     s <- out$parameter_value[out$parameter_name %in% c("shape1", "shape2")]
     ref_all <- propvacc::get_beta_params(quantiles = c(0.025, 0.5, 0.975), probs = c(0.24, 0.52, 0.8))
     ref_out <- propvacc::get_beta_params(quantiles = c(0.025, 0.5, 0.975), probs = c(0.40, 0.78, 0.99))
     expect_equal(s, c(ref_all$shape1, ref_all$shape2, ref_out$shape1, ref_out$shape2))
})

test_that("symptomatic-proportion intervals are internally consistent", {
     tmp <- withr::local_tempdir()
     df <- suppressMessages(get_symptomatic_prop_data(list(DATA_SYMPTOMATIC = tmp)))
     both <- !is.na(df$ci_lo) & !is.na(df$ci_hi)
     expect_true(all(df$ci_lo[both] <= df$ci_hi[both]))
     m <- both & !is.na(df$mean)
     expect_true(all(df$mean[m] >= df$ci_lo[m] & df$mean[m] <= df$ci_hi[m]))
     # Harris et al (2008) Table 1: 127 of 202 culture-confirmed infections
     # symptomatic; exact binomial 95% CI
     h <- df[df$source == "Harris et al (2008)", ]
     ci <- stats::binom.test(127, 202)$conf.int
     expect_equal(c(h$mean, h$ci_lo, h$ci_hi), round(c(127 / 202, ci), 3))
})

test_that("get_cases_binary handles countries with fewer than four weeks", {
     d <- data.frame(iso_code = c("AAA", "BBB", "BBB", "CCC", "CCC", "CCC"),
                     cases = c(5, 0, 3, 1, 2, 3),
                     date_start = as.Date("2024-01-01") + c(0, 0, 7, 0, 7, 14))
     out <- suppressMessages(get_cases_binary(d))
     expect_equal(nrow(out), 6L)
     expect_equal(out$cases_binary[out$iso_code == "AAA"], 1)
     expect_equal(out$cases_binary[out$iso_code == "BBB"], c(1, 1))   # lead-up week marked
     expect_equal(out$cases_binary[out$iso_code == "CCC"], c(1, 1, 1))
})

test_that("process_IDMC_data counts only 'Recommended figure' rows", {
     tmp <- withr::local_tempdir()
     PATHS <- list(DATA_IDMC_RAW = file.path(tmp, "raw"), DATA_IDMC = file.path(tmp, "out"))
     dir.create(PATHS$DATA_IDMC_RAW)
     d <- data.frame(id = 1:3, iso3 = "COD", role = c("Recommended figure", "Triangulation", "Triangulation"),
                     displacement_type = "Conflict", figure = c(100000, 100000, 61),
                     displacement_start_date = "2025-02-18", displacement_end_date = "2025-02-18",
                     event_name = "e", type = NA, subtype = NA)
     utils::write.csv(d, file.path(PATHS$DATA_IDMC_RAW, "cod.csv"), row.names = FALSE)
     out <- suppressMessages(process_IDMC_data(PATHS, panel_start = "2025-01-06"))
     cf <- utils::read.csv(out[["conflict"]])
     wk <- cf[cf$iso_code == "COD" & cf$date_start == "2025-02-17", ]
     expect_equal(wk$idmc_conflict_displaced, log1p(100000))
     expect_equal(wk$idmc_conflict_new, 1)
})

test_that("refresh coverage derives WHO dates from year/week and reports JHU as the static archive", {
     root <- withr::local_tempdir()
     awd <- file.path(root, "ees-cholera-mapping", "data", "cholera", "who", "awd")
     dir.create(awd, recursive = TRUE)
     utils::write.csv(data.frame(country = c("KENYA", "KENYA"), year = c(2025, 2026),
                                 week = c(53, 2), cases_by_week = 1, deaths_by_week = 0),
                      file.path(awd, "cholera_country_weekly.csv"), row.names = FALSE)
     cov <- MOSAIC:::.refresh_data_repos_coverage("ees-cholera-mapping", root)
     expect_equal(cov$date_range, as.Date(c("2025-12-29", "2026-01-12")))

     expect_null(MOSAIC:::.refresh_data_repos_coverage("jhu_cholera_data", root))
     jhu <- file.path(root, "MOSAIC-data", "raw", "JHU", "osfstorage-archive")
     dir.create(jhu, recursive = TRUE)
     saveRDS(data.frame(location_name = "AFR::AGO", epiweek = c("2017-15", "2019-02")),
             file.path(jhu, "Public_surveillance_dataset.rds"))
     cov <- MOSAIC:::.refresh_data_repos_coverage("jhu_cholera_data", root)
     expect_true(cov$static)
     expect_equal(cov$year_range, c(2017L, 2019L))
})

test_that("an unchanged shapefile is not rewritten", {
     tmp <- withr::local_tempdir()
     x <- sf::st_sf(iso = "AAA", geometry = sf::st_sfc(sf::st_point(c(1, 2)), crs = 4326))
     shp <- file.path(tmp, "AAA_ADM0.shp")
     expect_true(MOSAIC:::.write_shapefile_if_changed(x, shp))
     dbf <- file.path(tmp, "AAA_ADM0.dbf")
     old <- readBin(dbf, "raw", file.info(dbf)$size)
     old[2:4] <- as.raw(c(1, 1, 1))            # simulate a file written on another day
     writeBin(old, dbf)
     expect_false(MOSAIC:::.write_shapefile_if_changed(x, shp))
     expect_identical(readBin(dbf, "raw", file.info(dbf)$size), old)
     x$iso <- "BBB"
     expect_true(MOSAIC:::.write_shapefile_if_changed(x, shp))
})

test_that("get_WHO_vaccination_data leaves an identical file untouched", {
     tmp <- withr::local_tempdir()
     PATHS <- list(DATA_SCRAPE_WHO_VACCINATION = tmp)
     suppressMessages(utils::capture.output(get_WHO_vaccination_data(PATHS)))
     f <- MOSAIC:::.who_vaccination_latest_file(tmp)
     Sys.setFileTime(f, as.POSIXct("2020-01-01"))
     msgs <- character(0)
     withCallingHandlers(utils::capture.output(get_WHO_vaccination_data(PATHS)),
                         message = function(m) { msgs <<- c(msgs, conditionMessage(m))
                                                 invokeRestart("muffleMessage") })
     expect_true(any(grepl("unchanged", msgs)))
     expect_equal(as.Date(file.info(f)$mtime), as.Date("2020-01-01"))
})
