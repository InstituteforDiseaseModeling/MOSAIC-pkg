test_that("process_enso_data switches between bom and nmme sources", {

     # Build a minimal fake enso-data repo with both sources compiled.
     repo <- withr::local_tempdir()
     out  <- withr::local_tempdir()

     make_source <- function(src_dir, src_tag, latest_rel, with_model, vadj) {
          comp <- file.path(repo, "downloads", src_dir, latest_rel, "compiled")
          dir.create(comp, recursive = TRUE, showWarnings = FALSE)
          writeLines(latest_rel, file.path(repo, "downloads", src_dir, "LATEST"))
          for (freq in c("daily", "weekly", "monthly")) {
               d <- data.frame(
                    variable    = c("ENSO34", "ENSO34"),
                    year        = c(2026L, 2026L),
                    month       = c(6L, 7L),
                    month_name  = c("June", "July"),
                    value_org   = vadj + 0.10,
                    value_adj   = vadj,
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

     make_source("bom",  "BOM",  "2026/2026-06-01", with_model = FALSE, vadj = c(1.10, 1.25))
     make_source("nmme", "NMME", "2026/2026-06-08", with_model = TRUE,  vadj = c(2.10, 2.25))

     PATHS <- list(ENSO_DATA_REPO = repo, DATA_ENSO = out)

     # Default source is NMME (v0.55.5): reads compiled_ENSO_NMME_* and drops the
     # extra `model` provenance column.
     process_enso_data(PATHS)
     nmme_weekly <- utils::read.csv(file.path(out, "enso_weekly.csv"),
                                    stringsAsFactors = FALSE)
     expect_equal(nmme_weekly$value, c(2.10, 2.25))         # NMME values, value_adj renamed
     expect_false("value_adj" %in% names(nmme_weekly))
     expect_false("model" %in% names(nmme_weekly))

     # Explicit BOM source (legacy): reads compiled_ENSO_BOM_*; identical schema.
     process_enso_data(PATHS, source = "bom")
     bom_weekly <- utils::read.csv(file.path(out, "enso_weekly.csv"),
                                   stringsAsFactors = FALSE)
     expect_equal(bom_weekly$value, c(1.10, 1.25))          # BOM values
     expect_false("model" %in% names(bom_weekly))
     expect_setequal(names(nmme_weekly), names(bom_weekly))

     # Unknown source is rejected.
     expect_error(process_enso_data(PATHS, source = "iri"))

     # Missing source LATEST gives an informative error.
     PATHS_bad <- list(ENSO_DATA_REPO = withr::local_tempdir(), DATA_ENSO = out)
     expect_error(process_enso_data(PATHS_bad, source = "nmme"),
                  "Cannot find LATEST")
})
