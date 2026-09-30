# =============================================================================
# Undefined-variable guard for the plotting functions.
#
# v0.80.0 deleted `results_all <- results_full` from
# plot_model_posteriors_detail() as dead code, on the strength of a grep whose
# pattern did not contain "results_all" -- the line matched only because
# `results_full` sits on its right-hand side. A nested function still read it,
# so every posterior-detail page would have died on "object 'results_all' not
# found", swallowed by render_MOSAIC_figures()'s per-figure tryCatch and
# reported only as a skipped figure.
#
# R CMD check caught it and the test suite did not. This closes that gap with
# the same analysis R CMD check runs (codetools), scoped to the functions whose
# failures hide behind a tryCatch.
# =============================================================================

# R/globals.R exists only in a source checkout. Under R CMD check the package is
# installed and the source tree is gone, so the test skips there -- the same way
# test-psock-connection-clamp.R handles it. R CMD check runs its own codetools
# pass anyway, which is what caught the results_all regression this guards; this
# test exists to catch it at devtools::test() time, before a check runs.
globals_src <- function() {
  f <- normalizePath(file.path(testthat::test_path(), "..", "..", "R", "globals.R"),
                     mustWork = FALSE)
  testthat::skip_if_not(file.exists(f),
                        "package source R/ not available (installed check)")
  f
}

# Names the package declares via utils::globalVariables() in R/globals.R.
# R CMD check suppresses these; codetools called directly does not, and without
# them every ggplot2 aes() column (x, y, method, ...) reads as an undefined
# global. Parsing globals.R is deliberate -- hardcoding a list here would drift.
declared_globals <- function() {
  exprs <- parse(file = globals_src())
  out <- character(0)
  for (e in exprs) {
    if (is.call(e)) {
      v <- tryCatch(eval(e[[2L]]), error = function(err) NULL)
      if (is.character(v)) out <- c(out, v)
    }
  }
  unique(out)
}

globals_ok <- function(fn_name, allow) {
  fn <- get(fn_name, envir = asNamespace("MOSAIC"))
  found <- character(0)
  codetools::checkUsage(
    fn, name = fn_name,
    report = function(msg) found <<- c(found, msg),
    all = FALSE
  )
  # Only "no visible binding/global function" findings matter; codetools also
  # reports style issues (unused locals, shadowed args) that are not bugs.
  hits <- grep("no visible (binding|global function)", found, value = TRUE)
  # Drop anything naming a declared global.
  keep <- vapply(hits, function(h) {
    nm <- sub(".*variable [\u2018'\"]([^\u2019'\"]+)[\u2019'\"].*", "\\1", h)
    nm2 <- sub(".*function [\u2018'\"]([^\u2019'\"]+)[\u2019'\"].*", "\\1", h)
    !(nm %in% allow || nm2 %in% allow)
  }, logical(1))
  unname(hits[keep])
}

test_that("plotting functions have no dangling variable references", {
  skip_if_not_installed("codetools")

  # Functions whose errors are swallowed by render_MOSAIC_figures()'s tryCatch,
  # so a dangling reference shows up as a silently missing figure.
  targets <- c(
    "plot_model_posteriors_detail",
    "plot_model_distributions",
    "plot_model_trajectories",
    "render_MOSAIC_figures"
  )

  allow <- declared_globals()
  expect_gt(length(allow), 50L)   # globals.R was found and parsed

  for (fn in targets) {
    bad <- globals_ok(fn, allow)
    expect_identical(bad, character(0),
                     info = paste0(fn, ": ", paste(bad, collapse = " | ")))
  }
})

test_that("the specific v0.80.0 regression stays fixed", {
  # Deparse-based, so this one works installed or from source.
  # `results_all` is the unfiltered sample set, read as the prior series.
  src <- deparse(get("plot_model_posteriors_detail", envir = asNamespace("MOSAIC")))
  reads   <- any(grepl("results_all\\[\\[", src))
  defines <- any(grepl("^\\s*results_all\\s*<-", src))
  expect_true(reads)                  # it is genuinely used
  expect_true(defines)                # and therefore must be defined
})
