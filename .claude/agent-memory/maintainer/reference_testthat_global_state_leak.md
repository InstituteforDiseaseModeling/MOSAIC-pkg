---
name: testthat-global-state-leak
description: Config/testthat/parallel reuses workers across files, so a test that calls set_root_directory() leaks root_directory into every later file on that worker
metadata:
  type: reference
---

`DESCRIPTION` sets `Config/testthat/parallel: true`. testthat's own parallel vignette states:
"state is persisted across test files: options are _not_ reset, loaded packages are _not_ unloaded,
the global environment is _not_ cleared. You are responsible for making sure each file leaves the
world as it finds it." Workers are a **reused pool**, not one process per file.

**Why this bites MOSAIC specifically:** `set_root_directory()` is a single
`options(root_directory = root)` (`R/set_root_directory.R:36`) and `get_paths()` reads that option
(`R/get_paths.R:32`). A test file that calls `set_root_directory()` bare therefore turns
"`get_paths()` errors on a clean process" into "`get_paths()` silently returns the author's live
tree" for every test file that lands on the same worker afterwards — non-deterministically, because
file-to-worker assignment is scheduling-dependent. That converts the known false-pass pattern in
[[sample-parameters-tests-need-root-or-paths]] from machine-dependent to also order-dependent.

**How to apply:** any test that needs a root must use `withr::local_options(root_directory = ...)`
(or `withr::local_envvar`) inside the `test_that()` block, never a bare `set_root_directory()`.
Verified 2026-09-17: with `NOT_CRAN=true`, `getOption("root_directory")` was `NULL` before
`test-iso-week-labelling.R` and `~/MOSAIC` after it.

Related gotcha in the same family: `skip_on_cran()` also skips under a plain `Rscript`
`testthat::test_file()` run (NOT_CRAN unset), so a test guarded that way looks green-and-skipped
locally and only actually executes under `devtools::test()`. Set `NOT_CRAN=true` when you want to
know whether such a test really runs.
