# Regression tests for the WHO weekly and JHU surveillance processors
# (deep review data-pipeline-01, -02, -17).

.mk_who_weekly_raw <- function(dir) {
     # Two AFRO countries, 12 weeks each around the 2025/26 boundary, including
     # WHO's genuine 2025-W53 and one row with deaths missing.
     yw <- data.frame(year = c(rep(2025L, 7), rep(2026L, 5)),
                      week = c(47:53, 1:5))
     mk <- function(country, base) {
          data.frame(country = country, year = yw$year, week = yw$week,
                     cases_by_week = base + seq_len(nrow(yw)),
                     deaths_by_week = seq_len(nrow(yw)),
                     stringsAsFactors = FALSE)
     }
     d <- rbind(mk("DEMOCRATIC REPUBLIC OF THE CONGO", 1000), mk("ANGOLA", 50))
     d$cases_by_week[d$country == "DEMOCRATIC REPUBLIC OF THE CONGO" & d$year == 2025 & d$week == 53] <- 1594
     d$cases_by_week[d$country == "DEMOCRATIC REPUBLIC OF THE CONGO" & d$year == 2025 & d$week == 52] <- 1313
     d$deaths_by_week[d$country == "ANGOLA" & d$year == 2025 & d$week == 49] <- NA
     utils::write.csv(d, file.path(dir, "cholera_country_weekly.csv"), row.names = FALSE)
     d
}

test_that("WHO epi weeks follow WHO's calendar, verified against the AWD feature service", {
     # date_wk values read from the WHO cholera_adm0_week service (2026-09-29)
     got <- MOSAIC:::.who_epiweek_start(c(2023, 2023, 2024, 2025, 2025, 2026), c(1, 52, 52, 1, 53, 1))
     expect_equal(got, as.Date(c("2023-01-02", "2023-12-25", "2024-12-23",
                                 "2024-12-30", "2025-12-29", "2026-01-05")))
     # MMWR epi weeks (Sunday start) + 1 day, over 30 years
     mon <- seq(as.Date("2000-01-03"), as.Date("2030-12-30"), by = "week")
     expect_equal(MOSAIC:::.who_epiweek_start(lubridate::epiyear(mon - 1), lubridate::epiweek(mon - 1)), mon)
     expect_error(MOSAIC:::.who_epiweek_start(2024, 53), "does not exist")
})

test_that("process_WHO_weekly_data keeps W53 as its own week and keeps NA-death rows", {
     tmp <- withr::local_tempdir()
     PATHS <- list(DATA_SCRAPE_WHO_WEEKLY = tmp, DATA_WHO_WEEKLY = file.path(tmp, "out"))
     raw <- .mk_who_weekly_raw(tmp)
     suppressMessages(process_WHO_weekly_data(PATHS))
     out <- utils::read.csv(file.path(PATHS$DATA_WHO_WEEKLY, "cholera_country_weekly_processed.csv"),
                            stringsAsFactors = FALSE)

     expect_equal(nrow(out), nrow(raw))
     cod <- out[out$iso_code == "COD", ]
     w52 <- cod[cod$year == 2025 & cod$week == 52, ]
     w53 <- cod[cod$year == 2025 & cod$week == 53, ]
     expect_equal(w52$cases, 1313)            # not 1313 + 1594
     expect_equal(w53$cases, 1594)
     expect_equal(w52$date_start, "2025-12-22")
     expect_equal(w53$date_start, "2025-12-29")
     expect_equal(cod$date_start[cod$year == 2026 & cod$week == 1], "2026-01-05")
     expect_false(any(duplicated(out[, c("iso_code", "date_start")])))
     expect_true(all(as.Date(out$date_stop) - as.Date(out$date_start) == 6))

     ago49 <- out[out$iso_code == "AGO" & out$year == 2025 & out$week == 49, ]
     expect_equal(nrow(ago49), 1L)
     expect_true(is.na(ago49$deaths))
     expect_false(is.na(ago49$cases))
})

test_that("process_WHO_weekly_data refuses duplicated country-weeks instead of summing them", {
     tmp <- withr::local_tempdir()
     PATHS <- list(DATA_SCRAPE_WHO_WEEKLY = tmp, DATA_WHO_WEEKLY = file.path(tmp, "out"))
     raw <- .mk_who_weekly_raw(tmp)
     utils::write.csv(rbind(raw, raw[1, ]), file.path(tmp, "cholera_country_weekly.csv"), row.names = FALSE)
     expect_error(suppressMessages(process_WHO_weekly_data(PATHS)), "duplicated")
})

test_that("process_JHU_weekly_data keeps missing deaths and cases as NA", {
     tmp <- withr::local_tempdir()
     dir.create(file.path(tmp, "JHU", "osfstorage-archive"), recursive = TRUE)
     raw <- data.frame(
          location_name = "AFR::AGO",
          epiweek = c("2017-15", "2017-16", "2017-17", "2017-18"),
          sCh     = c(10, NA, 5, NA),
          cCh     = c(NA, 4, NA, NA),
          deaths  = c(NA, 1, 0, NA),
          spatial_scale = factor("country", levels = c("country", "admin1")),
          stringsAsFactors = FALSE)
     saveRDS(raw, file.path(tmp, "JHU", "osfstorage-archive", "Public_surveillance_dataset.rds"))
     PATHS <- list(DATA_RAW = tmp, DATA_JHU_WEEKLY = file.path(tmp, "out"))
     suppressMessages(process_JHU_weekly_data(PATHS))
     out <- utils::read.csv(file.path(PATHS$DATA_JHU_WEEKLY, "cholera_country_weekly_processed.csv"))

     expect_equal(nrow(out), 3L)                      # all-NA week dropped, not zero-filled
     expect_equal(out$cases, c(10, 4, 5))             # sCh, else cCh
     expect_true(is.na(out$deaths[1]))                # NA deaths is not an observed zero
     expect_equal(out$deaths[2:3], c(1, 0))
})
