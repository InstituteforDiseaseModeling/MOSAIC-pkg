# plot_model_calibration_report(): the calibration-diagnostics PDF rendered from
# calibration_diagnostics.json (SCHEMA_v2 section 2) and the metric glossary.
# Fixtures (written by the package's own writer,
# claude/v151/scripts/make_fixtures_v151.R):
#   MOZ    a real stubbed run_MOSAIC() (fixed budget, 300 draws, red, surveillance
#          tiers kept, so the fit table has tier 1-2 rows)
#   multi  10 locations, adaptive, red, budget cap reached, ESS growth stalled,
#          k-hat red (indicative projections)
#   edge   flat likelihood (yellow), fixed budget, no deaths, two-point paths,
#          no projection
#   edge_pre151  the edge file as written before 1.5.1 (no display fields,
#          scored weeks only): completed in memory by the report

cd_fixture <- function(id) test_path("fixtures", sprintf("calibration_diagnostics_%s.json", id))
cd_list <- function(id) jsonlite::read_json(cd_fixture(id), simplifyVector = FALSE)
gl_pkg <- function() MOSAIC:::.mosaic_glossary()

# Page count of a base-pdf() file: one "/Type /Page" object per page.
pdf_pages <- function(f) {
     r <- readBin(f, "raw", file.size(f))
     r[r == as.raw(0)] <- as.raw(32)
     txt <- rawToChar(r)
     length(gregexpr("/Type /Page[^s]", txt, useBytes = TRUE)[[1]])
}

# Page 1's left column laid out on a report-sized device: its text labels and
# the reason lines that continue on the next page.
page1_drawn <- function(d, gl = gl_pkg()) {
     grDevices::pdf(NULL, width = 11, height = 8.5, encoding = "WinAnsi.enc")
     on.exit(grDevices::dev.off(), add = TRUE)
     grid::grid.newpage()
     st <- new.env()
     grid::grid.draw(MOSAIC:::.mosaic_calrep_page_sampling(d, gl, "foot", st))
     grid::grid.force()
     labels <- character(0)
     walk <- function(g) {
          if (inherits(g, "text")) {
               l <- g$label
               labels <<- c(labels, if (is.language(l) || is.expression(l)) paste(deparse(l[[1]]), collapse = "")
                                    else as.character(l))
          }
          if (inherits(g, "gTree")) for (k in g$children) walk(k)
     }
     walk(grid::grid.grab(wrap.grobs = FALSE))
     list(labels = labels, overflow = st$overflow %||% character(0))
}

# Four pages plus a reasons continuation page when needed, plus the appendix.
expected_pages <- function(x) {
     d <- if (is.character(x)) MOSAIC:::.mosaic_calrep_read(x) else MOSAIC:::.mosaic_calrep_read(x)
     ov <- length(page1_drawn(d)$overflow) > 0L
     4L + ov + length(MOSAIC:::.mosaic_calrep_appendix(gl_pkg(), d))
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
     grDevices::pdf(NULL, width = 11, height = 8.5, encoding = "WinAnsi.enc")
     on.exit(grDevices::dev.off(), add = TRUE)
     gl <- gl_pkg()
     grid::grid.newpage()
     expect_no_error(suppressWarnings(grid::grid.draw(MOSAIC:::.mosaic_calrep_page_sampling(d, gl, "foot"))))
     expect_no_error(suppressWarnings(MOSAIC:::.mosaic_calrep_page_parameters(d, gl, "foot")))
     expect_no_error(suppressWarnings(MOSAIC:::.mosaic_calrep_page_fit(d, gl, "foot")))
     expect_no_error(suppressWarnings(MOSAIC:::.mosaic_calrep_page_scatter(d, gl, "foot")))
}

test_that("the report renders a vector PDF for each fixture variant", {
     skip_on_cran()
     for (id in c("MOZ", "multi", "edge", "edge_pre151")) {
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
     j$fit$table_display <- Filter(function(r) r$metric == "cases", j$fit$table_display)
     j$fit$weekly <- Filter(function(r) r$metric == "cases", j$fit$weekly)
     j$fit$per_location <- Filter(function(r) r$metric == "cases", j$fit$per_location)
     j$sampling["khat_tempered"] <- list(NULL)
     j$sampling["sims_projected_for_target"] <- list(NULL)
     j$sampling["ess_growth_slope"] <- list(NULL)
     j$status$reasons <- list(); j$status$flags <- list(); j$status$flags_detail <- list()
     j$sampling$ess_path <- j$sampling$ess_path[1]
     j$sampling$loglik_path <- list()
     d <- MOSAIC:::.mosaic_calrep_read(j)
     expect_true(is.na(d$sampling$khat_tempered))
     expect_identical(d$status$reasons, character(0))
     expect_pages_build(d)

     # whole blocks missing, and a file without its display fields
     k <- cd_list("MOZ")
     k$parameters <- list(); k$fit <- list(); k$sampling$ess_path <- list(); k$status$checks <- list()
     k$mce_summary <- list(); k$kl_summary <- list(); k$run$date_start <- NULL; k$info <- NULL
     k$schema_revision <- NULL
     expect_pages_build(MOSAIC:::.mosaic_calrep_read(k))
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

test_that("the legacy notice is returned for pre-1.4.0 runs only, worded by the glossary", {
     run <- withr::local_tempdir()
     dir.create(file.path(run, "3_results"))
     f <- file.path(run, "3_results", "summary.json")
     jsonlite::write_json(list(mode = "fixed", converged = FALSE, posterior_weighting = "coarsened"), f,
                          auto_unbox = TRUE)
     txt <- MOSAIC:::.mosaic_calrep_legacy_text()
     expect_identical(txt, attr(gl_pkg(), "report")$legacy$pre140)
     expect_identical(MOSAIC:::.mosaic_calibration_report_legacy_notice(run), txt)
     expect_identical(MOSAIC:::.mosaic_calibration_report_legacy_notice(file.path(run, "3_results")), txt)
     expect_identical(MOSAIC:::.mosaic_calibration_report_legacy_notice(f), txt)
     jsonlite::write_json(list(schema_version = "2", convergence_status = "yellow"), f, auto_unbox = TRUE)
     expect_null(MOSAIC:::.mosaic_calibration_report_legacy_notice(run))
     expect_error(MOSAIC:::.mosaic_calibration_report_legacy_notice(withr::local_tempdir()), "no summary.json")
})

test_that("every metric the report shows has a glossary entry; the appendix shows those and the retired note", {
     gl <- gl_pkg()
     for (id in c("MOZ", "multi", "edge", "edge_pre151")) {
          d <- MOSAIC:::.mosaic_calrep_read(cd_fixture(id))
          used <- MOSAIC:::.mosaic_calrep_metric_keys(d)
          expect_true(all(used %in% gl$key), info = paste(id, ":", paste(setdiff(used, gl$key), collapse = ", ")))
          expect_true(all(c(d$status$checks$key, vapply(d$info, `[[`, "", "key")) %in% used))
     }
     d <- MOSAIC:::.mosaic_calrep_read(cd_fixture("MOZ"))
     grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
     txt <- unlist(lapply(MOSAIC:::.mosaic_calrep_appendix(gl, d), function(pg)
          vapply(pg("foot")$children, function(g) if (inherits(g, "text")) paste(g$label, collapse = " ") else "", "")))
     shown <- gl$label[gl$key %in% MOSAIC:::.mosaic_calrep_metric_keys(d)]
     hidden <- setdiff(gl$label[!gl$key %in% MOSAIC:::.mosaic_calrep_metric_keys(d)], shown)
     expect_true(all(shown %in% txt))
     expect_false(any(hidden %in% txt))
     ret <- attr(gl, "retired")
     expect_true(ret$heading %in% txt)
     expect_true(all(vapply(ret$items, `[[`, "", "label") %in% txt))
     expect_setequal(vapply(ret$items, `[[`, "", "key"), c("A_best", "CVw_best", "max_best_subset", "ESS_param"))
     # this run's target, from the JSON (tokens as plain ASCII in the appendix)
     expect_true(any(grepl("This run: >= 200 / >= 100", txt, fixed = TRUE)))
})

test_that("checks, targets, info rows and flags are read from the JSON display fields", {
     d <- MOSAIC:::.mosaic_calrep_read(cd_fixture("multi"))
     expect_identical(d$status$label, "Not converged: more simulations needed")
     expect_identical(d$status$flags, c("budget_cap_reached", "ess_growth_stalled", "ensemble_size_bound"))
     m <- MOSAIC:::.mosaic_calrep_page1_model(d, gl_pkg())
     ess <- m$status_rows[[1]]
     expect_identical(c(ess$target$green, ess$target$yellow), c("{ge} 200", "{ge} 100"))
     expect_identical(m$headline, "OVERALL: RED — Not converged: more simulations needed")
     expect_identical(unname(vapply(m$info_rows, `[[`, "", "flag")), c("stalled", "at cap", "at bound"))
     expect_match(m$info_rows[[2]]$target$main, "^green: ~")
     expect_identical(m$info_rows[[2]]$target$note, "(indicative)")
     expect_identical(length(m$flags), 3L)
     # a changed threshold in the file changes the target shown (after completion)
     j <- cd_list("multi")
     j$status$checks[[1]]$threshold_green <- 400
     j$status$checks[[1]]$threshold_yellow <- 250
     j$status$checks[[2]]$threshold_green <- 0.08
     j$schema_revision <- NULL
     d2 <- MOSAIC:::.mosaic_calrep_read(j)
     expect_identical(d2$status$checks$target_display[1:2], c("{ge} 400 / {ge} 250", "{le} 8% / {le} 15%"))
     # a display string in the file is shown as written (the report formats nothing)
     j3 <- cd_list("multi")
     j3$status$checks[[1]]$value_display <- "VALUE-FROM-JSON"
     j3$info$budget$sub_display <- "SUB-FROM-JSON"
     m3 <- MOSAIC:::.mosaic_calrep_page1_model(MOSAIC:::.mosaic_calrep_read(j3), gl_pkg())
     expect_identical(m3$status_rows[[1]]$value$main, "VALUE-FROM-JSON")
     expect_identical(m3$info_rows[[2]]$value$sub, "SUB-FROM-JSON")
     # RN2: the display rule never rounds a value onto the other side of its threshold
     expect_identical(MOSAIC:::.mosaic_fmt_thr(0.1504, c(0.10, 0.15), 1, scale = 100, suffix = "%"), "15.04%")
     expect_identical(MOSAIC:::.mosaic_fmt_thr(199.6, 200), "199.6")
     # flat likelihood, every sampling check green: no projection
     e <- MOSAIC:::.mosaic_calrep_page1_model(MOSAIC:::.mosaic_calrep_read(cd_fixture("edge")), gl_pkg())
     expect_identical(e$info_rows[[2]]$target$main, "n/a")
     expect_identical(e$info_rows[[2]]$target$note, "sampling checks green")
     # D8: the MOZ fixture's fit table has the scored and the tier 1-2 rows, as labelled
     dm <- MOSAIC:::.mosaic_calrep_read(cd_fixture("MOZ"))
     expect_identical(dm$fit$table_display$label, c("Cases", "Cases, tier 1–2", "Deaths", "Deaths, tier 1–2"))
})

test_that("removed wording: no all-green line, no 'Flags: none', no flags block for fixed_budget alone", {
     gl <- gl_pkg()
     rsrc <- if (dir.exists(test_path("..", "..", "R")))
          paste(readLines(test_path("..", "..", "R", "plot_model_calibration_report.R")), collapse = "\n") else ""
     if (nzchar(rsrc)) {
          expect_false(grepl("All checks are green", rsrc, fixed = TRUE))
          expect_false(grepl("Flags: none", rsrc, fixed = TRUE))
     }
     expect_false(any(grepl("All checks are green|Flags: none", unlist(attr(gl, "report")))))
     # a green fixed-budget run whose only flag is fixed_budget
     j <- cd_list("edge")
     j$status$flags <- list("fixed_budget")
     j$status$flags_detail <- list(list(id = "fixed_budget", text = "fixed-budget run: the status is assessed, not used to stop",
                                        display = FALSE))
     j$status$reasons <- list()
     d <- MOSAIC:::.mosaic_calrep_read(j)
     m <- MOSAIC:::.mosaic_calrep_page1_model(d, gl)
     expect_length(m$flags, 0L)
     drawn <- page1_drawn(d, gl)
     expect_false(any(drawn$labels %in% c(attr(gl, "report")$flags$heading, attr(gl, "report")$reasons$heading)))
     expect_false(any(grepl("All checks are green|Flags: none|fixed-budget run", drawn$labels)))
     # a flag other than fixed_budget is shown, under the heading
     drawn2 <- page1_drawn(MOSAIC:::.mosaic_calrep_read(cd_fixture("edge")), gl)
     expect_true(attr(gl, "report")$flags$heading %in% drawn2$labels)
     expect_false(any(grepl("fixed-budget run", drawn2$labels)))
})

test_that("every reason is printed: those that do not fit page 1 continue on a page of their own", {
     skip_on_cran()
     j <- cd_list("multi")
     j$status$reasons <- as.list(rep(j$status$reasons[[1]], 12))
     d <- MOSAIC:::.mosaic_calrep_read(j)
     drawn <- page1_drawn(d)
     expect_gt(length(drawn$overflow), 0L)
     # whole reasons only: the continuation starts at a reason, not mid-sentence
     expect_identical(substr(drawn$overflow[1], 1, 1), "\u2022")
     cont <- attr(gl_pkg(), "report")$reasons$continued
     expect_true(any(grepl(cont, drawn$labels, fixed = TRUE)))
     dir <- withr::local_tempdir()
     out <- plot_model_calibration_report(j, dir_output = dir, verbose = FALSE)
     expect_identical(pdf_pages(out), expected_pages(j))
})


test_that("a multi-location report draws its series: no panel says Unavailable", {
  skip_on_cran()
  gl <- gl_pkg()
  for (id in c("multi", "MOZ")) {
    d <- MOSAIC:::.mosaic_calrep_read(cd_fixture(id))
    grDevices::pdf(NULL, width = 11, height = 8.5, encoding = "WinAnsi.enc")
    grid::grid.newpage()
    for (pg in list(MOSAIC:::.mosaic_calrep_page_fit(d, gl, "foot"), MOSAIC:::.mosaic_calrep_page_scatter(d, gl, "foot"),
                    MOSAIC:::.mosaic_calrep_page_parameters(d, gl, "foot"))) {
      grid::grid.newpage(); grid::grid.draw(pg)
    }
    grid::grid.force()
    labels <- character(0)
    walk <- function(g) {
      if (inherits(g, "text") && is.character(g$label)) labels <<- c(labels, g$label)
      if (inherits(g, "gTree")) for (k in g$children) walk(k)
    }
    walk(grid::grid.grab(wrap.grobs = FALSE))
    grDevices::dev.off()
    expect_false(any(grepl("Unavailable", labels)), info = id)
    for (m in c("cases", "deaths")) {
      p <- MOSAIC:::.mosaic_calrep_ts_plot(d, gl, m)
      expect_identical(p$labels$title, MOSAIC:::.mosaic_fill(attr(gl, "report")$fit_page$series_title, metric = m), info = id)
    }
  }
  # the multi series is the all-locations row, never a sum of per-location rows
  d <- MOSAIC:::.mosaic_calrep_read(cd_fixture("multi"))
  p <- MOSAIC:::.mosaic_calrep_ts_plot(d, gl, "cases")
  allw <- d$fit$weekly[d$fit$weekly$location == "ALL" & d$fit$weekly$metric == "cases", ]
  expect_equal(sort(p$data$predicted[is.finite(p$data$predicted)]), sort(allw$predicted[is.finite(allw$predicted)]))
  # an older multi-location file without the row says so instead of summing
  j <- cd_list("multi")
  j$fit$weekly <- Filter(function(r) r$location != "ALL", j$fit$weekly)
  p2 <- MOSAIC:::.mosaic_calrep_ts_plot(MOSAIC:::.mosaic_calrep_read(j), gl, "cases")
  note <- paste(unlist(lapply(p2$layers, function(l) l$aes_params$label)), collapse = " ")
  expect_match(gsub("\\s+", " ", note), "No all-locations weekly cases", fixed = TRUE)
})
