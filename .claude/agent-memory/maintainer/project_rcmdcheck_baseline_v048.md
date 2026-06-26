---
name: rcmdcheck-baseline-v048
description: R CMD check baseline for MOSAIC (v0.48.x and v0.55.7 update) — which ERRORs/WARNINGs/NOTEs are EXPECTED/pre-existing so future audits can tell new from baseline
metadata:
  type: project
---

## UPDATE v0.55.7 (2026-06-26, plain `R CMD check`, --no-manual --no-vignettes)

The two structural ERRORs below are NOW FIXED — plain tarball check is **0 ERRORs**:
- `checking examples ... OK` — the last unguarded runnable `@examples` with `path/to/...`
  placeholders (`process_GTFCC_vaccination_data`, `process_WHO_vaccination_data`) were wrapped in
  `\dontrun{}` (v0.55.7). The combine_vaccination_data one had already been fixed earlier.
- `checking tests ... OK [FAIL 0 | WARN 86 | SKIP 46 | PASS 4127]` — the 10 `source("../../R/...")`
  tests are now all `if(file.exists())`-guarded AND rely on the namespace (exportPattern covers
  them), so the skipped source is harmless. The last unguarded `readLines("../../R/est_initial_R.R")`
  meta-test (test-est_initial_R_parallel.R:45) got a `skip_if_not(file.exists(...))` guard (v0.55.7).
- `test-ic_select_epoch.R` SKIPS cleanly under check because **`data-raw/` is stripped from the
  tarball at build time** ("Removed empty directory data-raw") — its 3-candidate path probe +
  `skip()` fallback is load-bearing, do not "simplify" it to a bare path.

REMAINING baseline at v0.55.7 (plain check): **2 WARNINGs / 4 NOTEs, 0 ERRORs**:
- WARNING non-ASCII in `R/calc_model_ensemble.R` + `R/sample_parameters.R` (needs \uXXXX escapes) —
  swe/author domain; calc_model_ensemble.R is under active edit, don't touch mid-flight.
- WARNING vignettes / NOTE "vignettes but no vignettes" — no VignetteBuilder (pkgdown articles).
- NOTE `:::` self-namespace calls; NOTE `.run_sim_worker` no-visible-global (PSOCK worker pattern).
- `--as-cran` adds back the CRAN-incoming-feasibility ERROR (name clash, LICENSE, Remotes) — expected.

LANDMINE found this pass: **case-only Rd filename drift.** `R/process_ENSO_data.R` defines
`process_enso_data()`; roxygen generates `man/process_enso_data.Rd` (lowercase) but the tracked
file is `man/process_ENSO_data.Rd` (capital). On macOS (case-insensitive FS) `git` sees no change
and `document()` looks clean; on Linux CI (case-sensitive) `document()` would create the lowercase
file and orphan the capital one (possible duplicate-alias / stale Rd). Fix needs a `git mv`
case-rename, not a content edit. NOT yet fixed (left to avoid mid-review churn). Same risk class:
audit `git ls-files man/ | grep -iE '<topic>'` vs what `document()` emits whenever a function's
case changes.

---

## ORIGINAL baseline (~v0.48.x, --as-cran) — much of this now superseded by the v0.55.7 block above

`R CMD check --as-cran` on the built tarball is NOT clean and is not expected to be
(MOSAIC is explicitly non-CRAN). Baseline as of v0.48.x (profiled 2026-06-22). Use this to
separate NEW regressions from the standing baseline.

**Why:** so a future health audit doesn't waste time re-triaging the same standing notes, and
so an agent doesn't claim "R CMD check 0/0/0" (the NEWS for v0.48.1 made that claim — it is
only true under `devtools::check()` source-mode / a filtered view, NOT the tarball check).

**How to apply:** anything below = baseline, ignore. Anything else = investigate as new.

BASELINE Status: ~3 ERRORs / 3 WARNINGs / 4 NOTEs.
- ERROR "CRAN incoming feasibility": name clash with CRAN `mosaic`, non-FOSS LICENSE, `Remotes`
  field, non-mainstream deps (propvacc/mobility). Expected — not going to CRAN.
- ERROR "examples failed": `get_paths()` -> "Cannot find root_directory" in the check sandbox
  (examples need set_root_directory + the multi-repo tree; e.g. combine_vaccination_data not
  wrapped in \dontrun). Structural.
- ERROR "tests failed [FAIL ~10]": 10 test files use `source("../../R/<fn>.R")` which resolves
  under `devtools::test()`/`test_local()` (source tree) but NOT under tarball-installed
  `R CMD check`. This is why devtools::test()=0 fail but check=10 fail. PRE-EXISTING anti-pattern
  (oldest from 2025-09). CI uses `testthat::test_local()` so it's green there. List the 10 via
  `grep -rl 'source("\.\./\.\./R/' tests/testthat/`.
- WARNING non-ASCII (plot_model_subset_optimization.R): FIXED by swe in the v0.48.1 window via
  \uXXXX escapes; should clear next check.
- WARNING Rd cross-refs: `calc_model_ensemble.Rd: i` (roxygen turned `seeds[i]` into \link{i})
  and `get_greek_unicode.Rd: ggplot2` (\link{ggplot2} -> should be \pkg). BOTH FIXED by
  maintainer 2026-06-22 (wrapped index in \code{}; \link{ggplot2}->\pkg{ggplot2}).
- WARNING vignettes: `.Rmd` in vignettes/ but no VignetteBuilder in DESCRIPTION + leftover
  knitr `cache/`. Structural: vignettes are built as pkgdown ARTICLES, not installed R vignettes.
  Added vignettes/cache|figures to .Rbuildignore so stray local renders don't enter tarballs.
- NOTEs: `:::` self-namespace calls (intentional internals), `.run_sim_worker` no-visible /
  assign-to-GlobalEnv (the deliberate PSOCK worker pattern in run_MOSAIC), Rd line widths
  (est_demographic_rates example), "vignettes but no vignettes". All cosmetic/structural baseline.

**Build hygiene fixed this pass:** `__pycache__/`/`*.pyc` were neither gitignored nor
Rbuildignored — `source_python(inst/python/mosaic_dask_worker.py)` (e.g. the new parity test)
compiles a .pyc into inst/python that would enter the tarball. Added patterns to BOTH files.
