test_that("process_enso_data switches between bom and nmme sources", {

     # Build a minimal fake enso-data repo with both sources compiled.
     repo <- withr::local_tempdir()
     out  <- withr::local_tempdir()

     make_source <- function(src_dir, src_tag, latest_rel, with_model) {
          comp <- file.path(repo, "downloads", src_dir, latest_rel, "compiled")
          dir.create(comp, recursive = TRUE, showWarnings = FALSE)
          writeLines(latest_rel, file.path(repo, "downloads", src_dir, "LATEST"))
          for (freq in c("daily", "weekly", "monthly")) {
               d <- data.frame(
                    variable    = c("ENSO34", "ENSO34"),
                    year        = c(2026L, 2026L),
                    month       = c(6L, 7L),
                    month_name  = c("June", "July"),
                    value_org   = c(1.20, 1.35),
                    value_adj   = c(1.10, 1.25),
                    data_source = c("observed", "forecast"),
                    date_start  = c("2026-06-01", "2026-07-01"),
                    date_stop   = c("2026-06-30", "2026-07-31"),
                    stringsAsFactors = FALSE
               )
               if (with_model) d$model <- c("", "ENSMEAN")
               utils::write.csv(
                    d,
                    file.path(comp, paste0("compiled_ENSO_", src_tag, "_", freq, ".csv")),
                    row.names = FALSE
               )
          }
     }

     make_source("bom",  "BOM",  "2026/2026-06-01", with_model = FALSE)
     make_source("nmme", "NMME", "2026/2026-06-08", with_model = TRUE)

     PATHS <- list(ENSO_DATA_REPO = repo, DATA_ENSO = out)

     # Default source is bom (back-compatible).
     process_enso_data(PATHS)
     bom_weekly <- utils::read.csv(file.path(out, "enso_weekly.csv"),
                                   stringsAsFactors = FALSE)
     expect_equal(bom_weekly$value, c(1.10, 1.25))          # value_adj renamed
     expect_false("value_adj" %in% names(bom_weekly))
     expect_false("model" %in% names(bom_weekly))

     # NMME source: reads compiled_ENSO_NMME_* and drops the `model` column so
     # the output schema is identical to BOM.
     process_enso_data(PATHS, source = "nmme")
     nmme_weekly <- utils::read.csv(file.path(out, "enso_weekly.csv"),
                                    stringsAsFactors = FALSE)
     expect_false("model" %in% names(nmme_weekly))
     expect_setequal(names(bom_weekly), names(nmme_weekly))

     # Unknown source is rejected.
     expect_error(process_enso_data(PATHS, source = "iri"))

     # Missing source LATEST gives an informative error.
     PATHS_bad <- list(ENSO_DATA_REPO = withr::local_tempdir(), DATA_ENSO = out)
     expect_error(process_enso_data(PATHS_bad, source = "nmme"),
                  "Cannot find LATEST")
})
