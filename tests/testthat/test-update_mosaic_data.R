# Registry + selection logic for the data-update pipeline.
# Pure logic: no network, no files, no MOSAIC-data tree required.

test_that("registry is internally consistent (ids, deps, topological order)", {
     reg <- MOSAIC:::.mosaic_data_steps(as.Date("2026-01-01"))
     ids <- vapply(reg, `[[`, "", "id")

     expect_false(anyDuplicated(ids) > 0)
     # every declared dep names a real step -- intersect() in the driver would
     # otherwise silently drop a typo
     expect_true(all(unlist(lapply(reg, `[[`, "deps")) %in% ids))
     # the driver executes in list order and does not sort
     expect_silent(MOSAIC:::.mosaic_validate_registry(reg))
})

test_that("the registry builds data and fits no models", {
     # v0.91.14 scope boundary: update_mosaic_data() is a DATA builder. The
     # multi-hour est_suitability LSTM fit was removed from the registry (it
     # was group 4B) and is not reachable from here by any argument. Before
     # that it was held back by an include_suitability gate that had already
     # failed once -- it compared s$group to "4" when the ids are "4A"/"4B",
     # so it matched nothing and left the fit in the DEFAULT plan. Deleting
     # the step removes the whole class of failure; this test stops it, or any
     # other model fit, from being reintroduced.
     reg <- MOSAIC:::.mosaic_data_steps(as.Date("2026-01-01"))
     ids <- vapply(reg, `[[`, "", "id")

     expect_false("est_suitability" %in% ids)
     # No est_suitability call anywhere in a step body, however it is reached.
     bodies <- vapply(reg, function(s) paste(deparse(s$run), collapse = " "), "")
     expect_false(any(grepl("est_suitability", bodies, fixed = TRUE)))

     # The suitability DATA compile stays, and stays in the default plan.
     expect_true("compile_suitability_data" %in% ids)
     expect_true("compile_suitability_data" %in%
                 vapply(MOSAIC:::.mosaic_select_steps(reg, NULL, NULL), `[[`, "", "id"))

     # Nothing is filtered out by default: every registry step is a data build.
     expect_length(MOSAIC:::.mosaic_select_steps(reg, NULL, NULL), length(reg))
     expect_equal(nrow(MOSAIC::list_mosaic_data_steps()), length(reg))
})

test_that("step selection resolves ids, groups and group prefixes", {
     reg <- MOSAIC:::.mosaic_data_steps(as.Date("2026-01-01"))
     gid <- function(x) vapply(x, `[[`, "", "group")

     expect_true(all(substr(gid(MOSAIC:::.mosaic_select_steps(reg, "2", NULL)), 1, 1) == "2"))
     expect_true(all(gid(MOSAIC:::.mosaic_select_steps(reg, "1F", NULL)) == "1F"))
     one <- MOSAIC:::.mosaic_select_steps(reg, "est_mobility", NULL)
     expect_length(one, 1L)
     expect_equal(one[[1]]$id, "est_mobility")

     # skip wins over steps
     expect_length(MOSAIC:::.mosaic_select_steps(reg, "1F", "1F"), 0L)
})

test_that("skip= warns when it severs a dependency edge", {
     reg <- MOSAIC:::.mosaic_data_steps(as.Date("2026-01-01"))
     expect_warning(
          MOSAIC:::.mosaic_select_steps(reg, NULL, "process_WHO_weekly_data"),
          "depend on"
     )
})

test_that("the combiner depends on the WHO annual step: a failed annual step blocks it, skipping it warns", {
     # process_cholera_surveillance_data() reconciles imputed rows against the
     # WHO annual file, so it must not run against a stale or missing one.
     reg <- MOSAIC:::.mosaic_data_steps(as.Date("2026-01-01"))
     ids <- vapply(reg, `[[`, "", "id")
     ann  <- reg[[match("process_WHO_annual_data", ids)]]
     comb <- reg[[match("process_cholera_surveillance_data", ids)]]
     expect_true("process_WHO_annual_data" %in% comb$deps)
     expect_warning(MOSAIC:::.mosaic_select_steps(reg, NULL, "process_WHO_annual_data"),
                    "process_WHO_annual_data")

     ran <- FALSE
     fake <- lapply(reg, function(s) { s$run <- function(P) NULL; s })
     fake[[match("process_WHO_annual_data", ids)]]$run <- function(P) stop("WHO annual download failed")
     fake[[match("process_cholera_surveillance_data", ids)]]$run <- function(P) ran <<- TRUE
     withr::local_options(root_directory = getOption("root_directory"))
     local_mocked_bindings(.mosaic_data_steps = function(...) fake,
                           check_mosaic_manual_inputs = function(...) NULL)
     res <- suppressMessages(update_mosaic_data(withr::local_tempdir(), refresh_repos = FALSE, verbose = FALSE,
                                                steps = c("process_WHO_annual_data", "process_cholera_surveillance_data")))
     expect_equal(res$step, c("process_WHO_annual_data", "process_cholera_surveillance_data"))
     expect_equal(res$status, c("failed", "blocked"))
     expect_false(ran)
     expect_match(res$message[2], "process_WHO_annual_data")
})

test_that("registry validation catches typo'd deps and bad ordering", {
     ok <- list(
          list(id = "a", group = "1A", desc = "", deps = character(0), run = function(P) NULL),
          list(id = "b", group = "1B", desc = "", deps = "a",          run = function(P) NULL)
     )
     expect_silent(MOSAIC:::.mosaic_validate_registry(ok))

     typo <- ok; typo[[2]]$deps <- "a_typo"
     expect_error(MOSAIC:::.mosaic_validate_registry(typo), "no such step")

     expect_error(MOSAIC:::.mosaic_validate_registry(rev(ok)), "topological")

     dup <- ok; dup[[2]]$id <- "a"
     expect_error(MOSAIC:::.mosaic_validate_registry(dup), "Duplicate")
})

test_that("raw-file ranking prefers embedded dates over mtime", {
     d <- withr::local_tempdir()
     legacy <- file.path(d, "API_X_DS2_en_csv_v2_132025.csv")   # no date in name
     api    <- file.path(d, "API_X_DS2_en_csv_v2_api_2026-09-17.csv")
     file.create(legacy, api)

     # exact mtime tie: order() is stable and "1" < "a", so mtime ranking
     # deterministically picked the LEGACY file
     tm <- Sys.time(); Sys.setFileTime(legacy, tm); Sys.setFileTime(api, tm)
     expect_equal(basename(MOSAIC:::.rank_raw_candidates(c(legacy, api))[1]), basename(api))

     # legacy touched newer (cp / rsync -a / restore)
     Sys.setFileTime(api, tm - 86400); Sys.setFileTime(legacy, tm)
     expect_equal(basename(MOSAIC:::.rank_raw_candidates(c(legacy, api))[1]), basename(api))

     # a dated file always outranks an undated one, whatever its mtime
     nodate <- file.path(d, "public_emdat_nodate.csv")
     dated  <- file.path(d, "public_emdat_api_2026-09-17.csv")
     file.create(nodate, dated)
     Sys.setFileTime(nodate, Sys.time())
     expect_equal(basename(MOSAIC:::.rank_raw_candidates(c(nodate, dated))[1]), basename(dated))

     # undated only -> still resolves
     expect_equal(basename(MOSAIC:::.rank_raw_candidates(nodate)[1]), basename(nodate))
})

test_that("preflight survives an unreadable file and never blocks", {
     d <- withr::local_tempdir()
     dir.create(file.path(d, "MOSAIC-data", "raw", "EMDAT"), recursive = TRUE)
     file.create(file.path(d, "MOSAIC-data", "raw", "EMDAT", "public_emdat_real.xlsx"))
     # a dangling symlink gives mtime NA, which used to abort the whole run
     # from a preflight documented as non-fatal
     file.symlink("/nonexistent/x",
                  file.path(d, "MOSAIC-data", "raw", "EMDAT", "public_emdat_dangling.xlsx"))

     expect_error(MOSAIC::check_mosaic_manual_inputs(d, verbose = FALSE), NA)
     out <- MOSAIC::check_mosaic_manual_inputs(d, verbose = FALSE)
     expect_s3_class(out, "data.frame")
     expect_false(any(is.na(out$stale)))
})

test_that("EM-DAT preflight finds BOTH .xlsx and .csv extensions", {
     # Regression: a single character-class glob ("*.[xc]s[vx]") matched .csv
     # but not .xlsx, so the preflight reported EM-DAT MISSING while the
     # processor was happily reading an .xlsx. The extensions differ in length,
     # so the manifest must carry a vector of globs.
     for (ext in c("xlsx", "csv")) {
          d <- withr::local_tempdir()
          dir.create(file.path(d, "MOSAIC-data", "raw", "EMDAT"), recursive = TRUE)
          file.create(file.path(d, "MOSAIC-data", "raw", "EMDAT",
                                paste0("public_emdat_2026-09-16.", ext)))
          out <- MOSAIC::check_mosaic_manual_inputs(d, verbose = FALSE)
          emdat <- out[out$source == "EM-DAT disasters", ]
          expect_true(emdat$found, info = ext)
          expect_false(emdat$stale, info = ext)
     }
})
