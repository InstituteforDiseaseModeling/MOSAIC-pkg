# plot_model_calibration_report(): the calibration-diagnostics PDF rendered from
# calibration_diagnostics.json (SCHEMA_v2 section 2). Fixtures (written by the
# package's own writer, claude/redteam/swe_r1/make_fixtures_r1.R):
#   MOZ    a real stubbed run_MOSAIC() (fixed budget, 300 draws, red, surveillance
#          tiers kept, so the fit table has tier 1-2 rows)
#   multi  10 locations, adaptive, red, budget cap reached, ESS growth stalled,
#          k-hat red (indicative projections)
#   edge   flat likelihood (yellow), fixed budget, no deaths, two-point paths,
#          no projection

cd_fixture <- function(id) test_path("fixtures", sprintf("calibration_diagnostics_%s.json", id))
cd_list <- function(id) jsonlite::read_json(cd_fixture(id), simplifyVector = FALSE)

# Page count of a base-pdf() file: one "/Type /Page" object per page.
pdf_pages <- function(f) {
     r <- readBin(f, "raw", file.size(f))
     r[r == as.raw(0)] <- as.raw(32)
     txt <- rawToChar(r)
     length(gregexpr("/Type /Page[^s]", txt, useBytes = TRUE)[[1]])
}
# Four pages plus the appendix, which holds only the metrics the run's report shows.
expected_pages <- function(x) {
     d <- if (is.character(x)) MOSAIC:::.mosaic_calrep_read(x) else MOSAIC:::.mosaic_calrep_normalise(x)
     gl <- MOSAIC:::.mosaic_glossary()
     grDevices::pdf(NULL, width = 11, height = 8.5)
     ov <- length(MOSAIC:::.mosaic_calrep_page1_layout(d, gl)$overflow) > 0L   # reasons continuation page
     grDevices::dev.off()
     4L + ov + length(MOSAIC:::.mosaic_calrep_appendix(gl, d))
}

expect_valid_pdf <- function(f, diagnostics) {
     expect_true(file.exists(f))
     expect_identical(readChar(f, 5L, useBytes = TRUE), "%PDF-")
     expect_gt(file.size(f), 5000)
     expect_identical(pdf_pages(f), expected_pages(diagnostics))
}

# Build every page grob directly, so a panel failure surfaces as an error
# (the report itself replaces a failed page with a note).
expect_pages_build <- function(d) {
     grDevices::pdf(NULL, width = 11, height = 8.5)
     on.exit(grDevices::dev.off(), add = TRUE)
     gl <- MOSAIC:::.mosaic_glossary()
     expect_no_error(suppressWarnings(MOSAIC:::.mosaic_calrep_page_sampling(d, gl, "foot")))
     expect_no_error(suppressWarnings(MOSAIC:::.mosaic_calrep_page_parameters(d, "foot")))
     expect_no_error(suppressWarnings(MOSAIC:::.mosaic_calrep_page_fit(d, "foot")))
     expect_no_error(suppressWarnings(MOSAIC:::.mosaic_calrep_page_scatter(d, "foot")))
}

test_that("the report renders a vector PDF for each fixture variant", {
     skip_on_cran()
     for (id in c("MOZ", "multi", "edge")) {
          dir <- withr::local_tempdir()
          out <- plot_model_calibration_report(cd_fixture(id), dir_output = dir, verbose = FALSE)
          expect_identical(out, file.path(dir, "calibration_report.pdf"))
          expect_valid_pdf(out, cd_fixture(id))
          expect_length(list.files(dir, pattern = "^calibration_report_.*\\.pdf$"), 0L)   # no temp file left
          expect_pages_build(MOSAIC:::.mosaic_calrep_read(cd_fixture(id)))
     }
})

test_that("a parsed list renders like the file and dir_output defaults to the file's directory", {
     skip_on_cran()
     dir <- withr::local_tempdir()
     f <- file.path(dir, "calibration_diagnostics.json")
     file.copy(cd_fixture("edge"), f)
     expect_message(out <- plot_model_calibration_report(f), "Calibration report")
     expect_identical(out, file.path(dir, "calibration_report.pdf"))
     out2 <- plot_model_calibration_report(cd_list("edge"), dir_output = dir, file = "b.pdf", verbose = FALSE)
     expect_valid_pdf(out2, cd_fixture("edge"))
     simplified <- jsonlite::read_json(f, simplifyVector = TRUE)
     expect_equal(MOSAIC:::.mosaic_calrep_normalise(simplified), MOSAIC:::.mosaic_calrep_read(f))
     expect_error(plot_model_calibration_report(cd_list("edge")), "dir_output is required")
})

test_that("degraded inputs never fail a page", {
     skip_on_cran()
     j <- cd_list("MOZ")
     # deaths absent, k-hat null, empty reasons and flags, few path points, null projection
     j$fit$table <- Filter(function(r) r$metric == "cases", j$fit$table)
     j$fit$weekly <- Filter(function(r) r$metric == "cases", j$fit$weekly)
     j$fit$per_location <- Filter(function(r) r$metric == "cases", j$fit$per_location)
     j$sampling["khat_tempered"] <- list(NULL)
     j$sampling["sims_projected_for_target"] <- list(NULL)
     j$sampling["ess_growth_slope"] <- list(NULL)
     j$status$reasons <- list(); j$status$flags <- list()
     j$sampling$ess_path <- j$sampling$ess_path[1]
     j$sampling$loglik_path <- list()
     d <- MOSAIC:::.mosaic_calrep_normalise(j)
     expect_true(is.na(d$sampling$khat_tempered))
     expect_identical(d$status$reasons, character(0))
     expect_pages_build(d)

     # whole blocks missing
     k <- cd_list("MOZ")
     k$parameters <- list(); k$fit <- list(); k$sampling$ess_path <- list(); k$status$checks <- list()
     k$mce_summary <- list(); k$kl_summary <- list(); k$run$date_start <- NULL
     expect_pages_build(MOSAIC:::.mosaic_calrep_normalise(k))
     dir <- withr::local_tempdir()
     expect_valid_pdf(plot_model_calibration_report(k, dir_output = dir, verbose = FALSE), k)
})

test_that("the report refuses a JSON that is not schema 2", {
     dir <- withr::local_tempdir()
     j <- cd_list("edge")
     j$schema_version <- "1"
     expect_error(plot_model_calibration_report(j, dir_output = dir), "not a schema 2")
     j$schema_version <- NULL
     expect_error(plot_model_calibration_report(j, dir_output = dir), "schema_version = missing")
     expect_error(plot_model_calibration_report(file.path(dir, "nope.json"), dir_output = dir), "file not found")
     expect_length(list.files(dir), 0L)
})

test_that("the legacy notice is returned for pre-1.4.0 runs only", {
     run <- withr::local_tempdir()
     dir.create(file.path(run, "3_results"))
     f <- file.path(run, "3_results", "summary.json")
     jsonlite::write_json(list(mode = "fixed", converged = FALSE, posterior_weighting = "coarsened"), f,
                          auto_unbox = TRUE)
     txt <- MOSAIC:::.mosaic_calrep_legacy_text()
     expect_identical(MOSAIC:::.mosaic_calibration_report_legacy_notice(run), txt)
     expect_identical(MOSAIC:::.mosaic_calibration_report_legacy_notice(file.path(run, "3_results")), txt)
     expect_identical(MOSAIC:::.mosaic_calibration_report_legacy_notice(f), txt)
     jsonlite::write_json(list(schema_version = "2", convergence_status = "yellow"), f, auto_unbox = TRUE)
     expect_null(MOSAIC:::.mosaic_calibration_report_legacy_notice(run))
     expect_error(MOSAIC:::.mosaic_calibration_report_legacy_notice(withr::local_tempdir()), "no summary.json")
})

test_that("every metric the report shows has a glossary entry", {
     keys <- MOSAIC:::.mosaic_glossary()$key
     for (id in c("MOZ", "multi", "edge")) {
          used <- MOSAIC:::.mosaic_calrep_metric_keys(MOSAIC:::.mosaic_calrep_read(cd_fixture(id)))
          expect_true(all(used %in% keys), info = paste(id, ":", paste(setdiff(used, keys), collapse = ", ")))
     }
     tabs <- MOSAIC:::.mosaic_calrep_sampling_tables(MOSAIC:::.mosaic_calrep_read(cd_fixture("MOZ")))
     expect_false(anyNA(c(tabs$all$key, tabs$ensemble$key)))
})

test_that("labels, flags and thresholds are read from the JSON, not hard-coded", {
     j <- cd_list("multi")
     d <- MOSAIC:::.mosaic_calrep_normalise(j)
     expect_identical(d$status$label, "Not converged: more simulations needed")
     expect_identical(d$status$flags, c("budget_cap_reached", "ess_growth_stalled", "ensemble_size_bound"))
     ck <- MOSAIC:::.mosaic_calrep_check(d, "ess")
     expect_match(MOSAIC:::.mosaic_calrep_threshold_text(ck), "green >= 200, yellow >= 100", fixed = TRUE)
     j$status$checks[[1]]$threshold_green <- 400
     j$status$checks[[1]]$threshold_yellow <- 250
     j$status$checks[[2]]$threshold_green <- 0.08
     d2 <- MOSAIC:::.mosaic_calrep_normalise(j)
     expect_match(MOSAIC:::.mosaic_calrep_threshold_text(MOSAIC:::.mosaic_calrep_check(d2, "ess")),
                  "green >= 400, yellow >= 250", fixed = TRUE)
     expect_match(MOSAIC:::.mosaic_calrep_threshold_text(MOSAIC:::.mosaic_calrep_check(d2, "mce")),
                  "green <= 8%", fixed = TRUE)
     tabs <- MOSAIC:::.mosaic_calrep_sampling_tables(d2)
     expect_match(tabs$ensemble$Value[tabs$ensemble$Measure == "Monte Carlo error (median/90th/max)"],
                  "90th target <= 8%", fixed = TRUE)
     expect_match(tabs$all$Value[tabs$all$Measure == "Run mode"], "adaptive: stopped at the budget cap")
     expect_match(tabs$all$Value[tabs$all$Measure == "ESS growth (log-log slope)"], "stalled (flag)", fixed = TRUE)

     e <- MOSAIC:::.mosaic_calrep_read(cd_fixture("edge"))
     te <- MOSAIC:::.mosaic_calrep_sampling_tables(e)
     expect_match(te$all$Value[te$all$Measure == "Data informativeness"], "^flat \\(untempered ESS")
     # flat but every sampling check green: no more simulations are projected
     expect_identical(te$all$Value[te$all$Measure == "Simulations projected for green"],
                      "none: the sampling checks are green (more simulations would not change the rest)")
     # RN2: the display rule never rounds a value onto the other side of its threshold
     expect_identical(MOSAIC:::.mosaic_fmt_thr(0.1504, c(0.10, 0.15), 1, scale = 100, suffix = "%"), "15.04%")
     expect_identical(MOSAIC:::.mosaic_fmt_thr(199.6, 200), "199.6")
     expect_identical(MOSAIC:::.mosaic_fmt_thr(0.12, c(0.10, 0.15), 1, scale = 100, suffix = "%"), "12.0%")
     # RN3: a KL band withheld below ensemble ESS 100 is counted as withheld
     expect_true(is.finite(MOSAIC:::.mosaic_calrep_read(cd_fixture("edge"))$kl_summary$withheld))
     expect_match(te$all$Value[te$all$Measure == "Run mode"], "^fixed budget")
     expect_match(te$all$Value[te$all$Measure == "Posterior (all draws)"], "^coarsened \\(alpha = 10")
     # R3/R5/R6: the projection is for green, in simulations, indicative when growth stalled
     tm <- MOSAIC:::.mosaic_calrep_sampling_tables(MOSAIC:::.mosaic_calrep_read(cd_fixture("multi")))
     expect_match(tm$all$Value[tm$all$Measure == "Simulations projected for green"],
                  "^about [0-9,]+ in total \\(incl\\. one confirming batch\\) \\(now [0-9,]+ valid draws\\) \\(indicative: ESS growth stalled\\)")
     # R13: the badge shows the flags in words
     expect_true(all(nzchar(MOSAIC:::.mosaic_calrep_read(cd_fixture("multi"))$status$flags_text)))
     # R11/R17: the appendix shows only the report's metrics, with this run's thresholds
     d_m <- MOSAIC:::.mosaic_calrep_read(cd_fixture("MOZ"))
     gl <- MOSAIC:::.mosaic_glossary()
     expect_lt(length(unlist(MOSAIC:::.mosaic_calrep_appendix(gl, d_m))),
               length(unlist(MOSAIC:::.mosaic_calrep_appendix(gl))) + 1L)
     j2 <- cd_list("MOZ"); j2$status$checks[[1]]$threshold_green <- 40
     d2b <- MOSAIC:::.mosaic_calrep_normalise(j2)
     grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
     txt <- unlist(lapply(MOSAIC:::.mosaic_calrep_appendix(gl, d2b), function(pg)
          vapply(pg("foot")$children, function(g) if (inherits(g, "text")) paste(g$label, collapse = " ") else "", "")))
     expect_true(any(grepl("This run: green >= 40", txt, fixed = TRUE)))
     # round 3: every reason is printed; those that do not fit page 1 continue on a page of their own
     dm <- MOSAIC:::.mosaic_calrep_read(cd_fixture("multi"))
     grDevices::pdf(NULL, width = 11, height = 8.5)
     lay <- MOSAIC:::.mosaic_calrep_page1_layout(dm, gl)
     grDevices::dev.off()
     shown <- length(MOSAIC:::.mosaic_calrep_reason_lines(dm$status$reasons, 76L)) - length(lay$overflow)
     expect_identical(shown + length(lay$overflow), length(MOSAIC:::.mosaic_calrep_reason_lines(dm$status$reasons, 76L)))
     # D8: the MOZ fixture's fit table has the scored and the tier 1-2 rows
     expect_identical(d_m$fit$table$subset, c("scored", "tier12", "scored", "tier12"))
})
