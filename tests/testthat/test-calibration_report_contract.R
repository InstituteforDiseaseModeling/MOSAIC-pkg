# The 1.5.1 report contract: calibration_diagnostics.json carries every number
# and display string, inst/extdata/metric_glossary.yml every word, formula and
# target template, and the renderer reads nothing else
# (claude/plan_convergence/SCHEMA_v2.md section 2).

cd_fixture <- function(id) test_path("fixtures", sprintf("calibration_diagnostics_%s.json", id))
cd_list <- function(id) jsonlite::read_json(cd_fixture(id), simplifyVector = FALSE)

# The 1.5.1 additions, removed: what a 1.5.0 file holds (verified against a real
# 1.5.0 stub run, whose keys are exactly the 1.5.1 keys minus these).
strip_to_150 <- function(j) {
  for (k in c("schema_revision", "info", "upgrade")) j[[k]] <- NULL
  for (k in c("central_method", "glossary_version", "max_ensemble_members", "max_simulations_total",
              "min_ensemble_members", "n_simulations_requested")) j$run[[k]] <- NULL
  for (k in c("flags_detail", "headline_display", "palette", "thresholds", "thresholds_source")) j$status[[k]] <- NULL
  add <- c("key", "operator", "operator_yellow", "status_display", "target_display", "target_note",
           "threshold_green_display", "threshold_yellow_display", "value_display")
  j$status$checks <- lapply(j$status$checks, function(r) r[setdiff(names(r), add)])
  j$sampling$details_display <- NULL
  j$fit$table_display <- NULL; j$fit$weekly_meta <- NULL
  j$fit$weekly <- lapply(Filter(function(r) isTRUE(r$scored), j$fit$weekly), function(r) { r$scored <- NULL; r })
  j
}
as_json <- function(x) jsonlite::fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA, na = "null",
                                                           null = "null", dataframe = "rows"), simplifyVector = FALSE)
sort_keys <- function(x) if (is.list(x)) { if (!is.null(names(x))) x <- x[order(names(x))]; lapply(x, sort_keys) } else x

test_that("calibration_diagnostics.json carries the 1.5.1 fields with their types", {
  j <- cd_list("MOZ")   # a real stub run_MOSAIC(), fixed budget
  expect_identical(j$schema_version, "2")
  expect_identical(j$schema_revision, "1.5.1")
  expect_identical(j$run$glossary_version, attr(MOSAIC:::.mosaic_glossary(), "version"))
  for (k in c("max_simulations_total", "min_ensemble_members", "max_ensemble_members", "n_simulations_requested"))
    expect_true(is.numeric(j$run[[k]]) && length(j$run[[k]]) == 1L, info = k)
  expect_identical(j$run$central_method, list(cases = "median", deaths = "mean"))
  th <- j$status$thresholds
  expect_setequal(names(th), names(MOSAIC:::.mosaic_convergence_defaults()$thresholds))
  expect_true(is.numeric(th$slope_stall))
  expect_identical(j$status$thresholds_source, "run control")
  for (ck in j$status$checks) {
    for (f in c("key", "operator", "operator_yellow", "value_display", "threshold_green_display",
                "threshold_yellow_display", "target_display", "status_display"))
      expect_true(is.character(ck[[f]]) && length(ck[[f]]) == 1L, info = paste(ck$check, f))
    expect_true(ck$operator %in% c("ge", "le", "lt"))
  }
  expect_true(length(j$status$flags_detail) == length(j$status$flags))
  for (fl in j$status$flags_detail) {
    expect_true(is.character(fl$id) && is.character(fl$text) && is.logical(fl$display))
    expect_identical(fl$display, fl$id != "fixed_budget")
  }
  expect_true(is.character(j$status$headline_display))
  expect_setequal(names(j$status$palette), c("green", "yellow", "red", "grey"))
  expect_setequal(names(j$info), c("ess_growth", "budget", "ensemble_size"))
  for (nm in names(j$info)) {
    r <- j$info[[nm]]
    for (f in c("key", "display", "status", "status_display")) expect_true(is.character(r[[f]]), info = paste(nm, f))
    expect_true("value" %in% names(r) && "target_display" %in% names(r), info = nm)
    expect_true(r$status %in% c("ok", "stalled", "at_cap", "at_bound", "na"))
  }
  expect_identical(j$info$ess_growth$target, j$status$thresholds$slope_stall)
  expect_identical(j$info$budget$cap, j$run$n_simulations_requested)   # fixed budget: the requested draws
  expect_identical(c(j$info$ensemble_size$min, j$info$ensemble_size$max),
                   c(j$run$min_ensemble_members, j$run$max_ensemble_members))
  td <- j$fit$table_display
  expect_identical(vapply(td, `[[`, "", "label"), c("Cases", "Cases, tier 1–2", "Deaths", "Deaths, tier 1–2"))
  for (r in td) expect_true(all(vapply(r, is.character, logical(1))))
  expect_identical(vapply(j$sampling$details_display, `[[`, "", "key"),
                   c("valid_draws", "posterior", "ess_all", "mass", "runs", "scored"))
  # fit.weekly: every complete week, a logical scored flag, observed null when not scored
  wk <- j$fit$weekly
  expect_true(all(vapply(wk, function(r) is.logical(r$scored), logical(1))))
  expect_true(all(vapply(wk, function(r) isTRUE(r$scored) || is.null(r$observed), logical(1))))
  weeks <- as.Date(unique(vapply(wk, `[[`, "", "week")))
  expect_true(all(format(weeks, "%u") == "1"))
  expect_true(all(diff(sort(weeks)) == 7))
  start <- as.Date(j$run$date_start); stop <- as.Date(j$run$date_stop)
  first_monday <- start + ((8 - as.integer(format(start, "%u"))) %% 7)
  expect_identical(min(weeks), first_monday)
  expect_lte(as.numeric(stop - max(weeks)), 13)
  expect_gte(as.numeric(stop - max(weeks)), 6)
  expect_identical(j$fit$weekly_meta$source, "trajectories")
  expect_identical(j$fit$weekly_meta$intervals, list(cases = "observation", deaths = "observation"))
})

test_that("the renderer holds no metric wording, formula or target: they are glossary lookups", {
  gl <- MOSAIC:::.mosaic_glossary()
  f <- test_path("..", "..", "R", "plot_model_calibration_report.R")
  skip_if_not(file.exists(f), "package source not available")
  pd <- utils::getParseData(parse(f, keep.source = TRUE))
  lits <- unique(gsub("^[\"']|[\"']$", "", pd$text[pd$token == "STR_CONST"]))
  lits <- lits[nchar(lits) >= 8]
  # every glossary string the report shows
  rep_strings <- unlist(attr(gl, "report"))
  glossary_txt <- c(gl$short_label, gl$what_it_tells_you, gl$label, gl$formula_plotmath, gl$formula_katex,
                    gl$target_template, gl$target_note, rep_strings,
                    unlist(lapply(attr(gl, "retired")$items, `[[`, "why")), attr(gl, "retired")$note)
  # phrases (16+ characters): single words such as "coverage" also name files and columns
  glossary_txt <- unique(glossary_txt[!is.na(glossary_txt) & nchar(glossary_txt) >= 16])
  hits <- glossary_txt[vapply(glossary_txt, function(g) any(vapply(lits, function(l)
    grepl(g, l, fixed = TRUE) || (nchar(l) >= 20 && grepl(l, g, fixed = TRUE)), logical(1))), logical(1))]
  expect_identical(hits, character(0), info = paste(hits, collapse = " | "))
  # the strings the 1.5.0 renderer hard-coded on pages 1 and 3 are gone
  for (old in c("What it tells you", "not part of the status", "Why not green:", "Sampling details",
                "Posterior ESS as draws accumulate", "Weekly %s: observed and ensemble", "OVERALL:",
                "Fit metrics (central line", "Every complete week"))
    expect_false(any(grepl(old, lits, fixed = TRUE)), info = old)
  expect_false("green / yellow" %in% lits)
})

test_that("page 1 changes when the glossary changes (it reads the glossary, not constants)", {
  skip_on_cran()
  y <- yaml::read_yaml(system.file("extdata", "metric_glossary.yml", package = "MOSAIC"))
  for (i in seq_along(y$metrics)) {
    if (identical(y$metrics[[i]]$key, "ess_best")) {
      y$metrics[[i]]$what_it_tells_you <- "ZZ-WHAT-ESS"
      y$metrics[[i]]$short_label <- "ZZ-LABEL-ESS"
    }
    if (identical(y$metrics[[i]]$key, "budget")) y$metrics[[i]]$what_it_tells_you <- "ZZ-WHAT-BUDGET"
  }
  y$report$status_table$divider <- "ZZ-DIVIDER"
  y$report$fit_table$caption <- "ZZ-FIT-CAPTION"
  y$report$sampling_details$heading <- "ZZ-DETAILS"
  f <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(y, f)
  gl2 <- MOSAIC:::.mosaic_glossary(f)
  d <- MOSAIC:::.mosaic_calrep_read(cd_fixture("MOZ"), gl2)
  grDevices::pdf(NULL, width = 11, height = 8.5, encoding = "WinAnsi.enc")
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::grid.draw(MOSAIC:::.mosaic_calrep_page_sampling(d, gl2, "foot"))
  grid::grid.force()
  labels <- character(0)
  walk <- function(g) {
    if (inherits(g, "text") && is.character(g$label)) labels <<- c(labels, g$label)
    if (inherits(g, "gTree")) for (k in g$children) walk(k)
  }
  walk(grid::grid.grab(wrap.grobs = FALSE))
  for (z in c("ZZ-LABEL-ESS", "ZZ-DIVIDER", "ZZ-FIT-CAPTION", "ZZ-DETAILS"))
    expect_true(z %in% labels, info = z)
  expect_true(any(grepl("ZZ-WHAT", labels)))
  # a missing report string is an error, never a silent fallback
  y$report$status_table$divider <- NULL
  yaml::write_yaml(y, f)
  expect_error(MOSAIC:::.mosaic_gl_text(MOSAIC:::.mosaic_glossary(f), "status_table", "divider"), "no report string")
})

test_that("the glossary: 1.5.1 fields, parseable formulas, defined tokens, readable by a 1.4.0 reader", {
  g <- MOSAIC:::.mosaic_glossary()
  expect_identical(attr(g, "version"), "1.5.1")
  raw <- yaml::read_yaml(system.file("extdata", "metric_glossary.yml", package = "MOSAIC"))
  # the 1.4.0 structure: version + metrics[] with the seven 1.4.0 fields, non-empty
  old <- c("key", "label", "definition", "formula", "how_to_read", "threshold", "not_a_measure_of")
  expect_true(all(vapply(raw$metrics, function(e) all(vapply(old, function(f) is.character(e[[f]]) && nzchar(e[[f]]),
                                                               logical(1))), logical(1))))
  for (k in c("ess_best", "mce", "khat_tempered", "flat_likelihood", "ess_growth_slope", "budget", "ensemble_members")) {
    e <- g[g$key == k, ]
    expect_true(nzchar(e$formula_plotmath) && nzchar(e$formula_katex) && nzchar(e$what_it_tells_you), info = k)
    expect_true(length(str2expression(e$formula_plotmath)) >= 1L)
  }
  expect_false(is.na(g$target_template[g$key == "ess_best"]))
  tk <- attr(g, "tokens")
  expect_true(all(c("ge", "le", "lt", "khat", "alpha", "eta", "f1", "ESSall") %in% names(tk)))
  # the PDF's plain-text fallback is WinAnsi-safe (ASCII)
  expect_true(all(vapply(tk, function(t) all(utf8ToInt(t$ascii) < 128), logical(1))))
  bad <- withr::local_tempfile(fileext = ".yml")
  writeLines(c("version: '1'", "metrics:", "  - key: x", "    label: X"), bad)
  expect_error(MOSAIC:::.mosaic_glossary(bad), "lacks: definition")
  raw2 <- raw; raw2$metrics[[1]]$formula_plotmath <- "a +* ("
  f <- withr::local_tempfile(fileext = ".yml"); yaml::write_yaml(raw2, f)
  expect_error(MOSAIC:::.mosaic_glossary(f), "does not parse")
  raw3 <- raw; raw3$metrics[[1]]$short_label <- "Draws {nope}"
  yaml::write_yaml(raw3, f)
  expect_error(MOSAIC:::.mosaic_glossary(f), "undefined token")
})

test_that("the check operators are the colour rule of calc_convergence_status()", {
  spec <- MOSAIC:::.mosaic_convergence_check_spec()
  th <- MOSAIC:::.mosaic_convergence_defaults()$thresholds
  st <- function(...) {
    a <- list(ess_best = 300, ess_best_target = 200, mce_p90 = 0.05, khat = 0.1, ess_untempered_frac = 1e-5,
              n_draws = 1e5)
    a[names(list(...))] <- list(...)
    ck <- do.call(calc_convergence_status, a)$checks
    stats::setNames(ck$colour, ck$check)
  }
  op <- function(id) spec$operator[spec$check == id]
  opy <- function(id) spec$operator_yellow[spec$check == id]
  # at the threshold itself: "ge"/"le" are inclusive, "lt" exclusive
  expect_identical(unname(st(ess_best = 200)["ess"]), if (op("ess") == "ge") "green" else "yellow")
  expect_identical(unname(st(ess_best = 100)["ess"]), if (opy("ess") == "ge") "yellow" else "red")
  expect_identical(unname(st(mce_p90 = th$mce_green)["mce"]), if (op("mce") == "le") "green" else "yellow")
  expect_identical(unname(st(mce_p90 = th$mce_yellow)["mce"]), if (opy("mce") == "le") "yellow" else "red")
  expect_identical(unname(st(khat = th$khat_green)["khat"]), if (op("khat") == "lt") "yellow" else "green")
  expect_identical(unname(st(khat = th$khat_yellow)["khat"]), if (opy("khat") == "lt") "red" else "yellow")
  expect_identical(unname(st(ess_untempered_frac = th$flat_ess_frac)["informativeness"]),
                   if (op("informativeness") == "lt") "yellow" else "green")
  expect_identical(opy("informativeness"), "ge")
})

test_that("flag texts regenerated from the numbers match the file's own flags_text", {
  for (id in c("MOZ", "multi", "edge", "edge_pre151")) {
    j <- cd_list(id)
    joined <- paste(unlist(j$status$flags_text), collapse = "; ")
    j$status$flags_detail <- NULL; j$status$thresholds <- NULL
    k <- MOSAIC:::.mosaic_calrep_complete(j, MOSAIC:::.mosaic_glossary())
    expect_identical(paste(k$status$flags_detail$text, collapse = "; "), joined, info = id)
    expect_identical(k$status$flags_detail$id, as.character(unlist(j$status$flags)), info = id)
  }
})

test_that("a 1.5.0 run directory is re-rendered: the upgraded file equals what 1.5.1 writes", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  local_test_root()
  d <- withr::local_tempdir()
  .stub_run_mosaic(d, mode = "fixed", keep_tier = TRUE, io_extra = list(persist_ensemble_arrays = TRUE))
  js <- file.path(d, "3_results", "calibration_diagnostics.json")
  written <- jsonlite::read_json(js)
  expect_identical(written$fit$weekly_meta$source, "trajectories")
  # turn it into the file 1.5.0 wrote
  old <- strip_to_150(written)
  jsonlite::write_json(old, js, auto_unbox = TRUE, pretty = TRUE, digits = NA, na = "null", null = "null")
  old_bytes <- readBin(js, "raw", file.size(js))
  fig <- file.path(d, "3_results", "figures", "diagnostics")
  unlink(file.path(fig, "calibration_report.pdf"))
  suppressWarnings(suppressMessages(render_MOSAIC_figures(d, which = "convergence", verbose = FALSE)))
  expect_true(file.exists(file.path(fig, "calibration_report.pdf")))
  bak <- file.path(d, "3_results", "calibration_diagnostics.pre151.json")
  expect_identical(readBin(bak, "raw", file.size(bak)), old_bytes)
  up <- jsonlite::read_json(js)
  expect_identical(up$upgrade$weekly_source, "trajectories")
  expect_true(up$upgrade$control_read)
  up$upgrade <- NULL
  expect_identical(sort_keys(up), sort_keys(written))
  # idempotent: a second render leaves the upgraded file and the backup alone
  m1 <- file.mtime(js)
  suppressWarnings(suppressMessages(render_MOSAIC_figures(d, which = "convergence", verbose = FALSE)))
  expect_identical(file.mtime(js), m1)
  expect_identical(readBin(bak, "raw", file.size(bak)), old_bytes)
})

test_that("without persisted arrays the upgrade falls back to the daily table, and says so", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  local_test_root()
  d <- withr::local_tempdir()
  .stub_run_mosaic(d, mode = "fixed")
  js <- file.path(d, "3_results", "calibration_diagnostics.json")
  written <- jsonlite::read_json(js)
  # in-run the arrays are in memory whatever persist_ensemble_arrays is
  expect_identical(written$fit$weekly_meta$source, "trajectories")
  jsonlite::write_json(strip_to_150(written), js, auto_unbox = TRUE, digits = NA, na = "null", null = "null")
  up <- MOSAIC:::.mosaic_upgrade_calibration_diagnostics(d, write = FALSE)
  expect_identical(up$fit$weekly_meta$source, "daily_sums")
  expect_identical(up$schema_revision, "1.5.1")
  # the display fields do not depend on the weekly source
  expect_identical(sort_keys(as_json(up$status)), sort_keys(written$status))
  expect_identical(sort_keys(as_json(up$info)), sort_keys(written$info))
  expect_false(file.exists(file.path(d, "3_results", "calibration_diagnostics.pre151.json")))
  dd <- MOSAIC:::.mosaic_calrep_read(up)
  expect_identical(dd$fit$weekly_source, "daily_sums")
})
