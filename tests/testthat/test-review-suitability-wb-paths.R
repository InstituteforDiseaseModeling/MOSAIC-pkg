# Deep review tracker: compile_suitability_data() read the legacy World Bank
# filenames (GDP_data_world_bank.csv, population_density_data_world_bank.csv)
# while process_WB_GDP_data() / process_WB_population_density_data() write
# world_bank_GDP_data.csv / world_bank_population_density_data.csv, so a World
# Bank refresh never reached the psi panel.

test_that("the current processor output is preferred over the legacy file", {
     root <- withr::local_tempdir()
     dir.create(file.path(root, "world_bank"))
     PATHS <- list(DATA_PROCESSED = root)
     cur <- file.path(root, "world_bank", "world_bank_GDP_data.csv")
     leg <- file.path(root, "world_bank", "GDP_data_world_bank.csv")
     writeLines("x", leg)
     expect_message(
          p <- MOSAIC:::.csd_world_bank_path(PATHS, "world_bank_GDP_data.csv",
                                             "GDP_data_world_bank.csv"),
          "legacy")
     expect_equal(p, leg)
     writeLines("x", cur)
     expect_equal(MOSAIC:::.csd_world_bank_path(PATHS, "world_bank_GDP_data.csv",
                                                "GDP_data_world_bank.csv"), cur)
})

test_that("neither file present returns the current path (caller skips the block)", {
     root <- withr::local_tempdir()
     PATHS <- list(DATA_PROCESSED = root)
     p <- MOSAIC:::.csd_world_bank_path(PATHS, "world_bank_population_density_data.csv",
                                        "population_density_data_world_bank.csv")
     expect_equal(basename(p), "world_bank_population_density_data.csv")
     expect_false(file.exists(p))
})

test_that("compile_suitability_data reads the filenames the WB processors write", {
     src <- testthat::test_path("..", "..", "R")
     skip_if_not(dir.exists(src), "R/ source not available (installed check)")
     csd <- paste(readLines(file.path(src, "compile_suitability_data.R"), warn = FALSE), collapse = "\n")
     for (proc in c("process_WB_GDP_data.R", "process_WB_population_density_data.R")) {
          ptxt <- paste(readLines(file.path(src, proc), warn = FALSE), collapse = "\n")
          out  <- regmatches(ptxt, regexpr("world_bank_[A-Za-z_]+_data\\.csv", ptxt))
          expect_length(out, 1L)
          expect_true(grepl(out, csd, fixed = TRUE), info = out)
     }
})
