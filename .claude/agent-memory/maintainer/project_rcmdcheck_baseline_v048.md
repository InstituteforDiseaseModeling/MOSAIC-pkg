---
name: rcmdcheck-baseline-v048
description: R CMD check baseline for MOSAIC (~v0.48.x) — which ERRORs/WARNINGs/NOTEs are EXPECTED/pre-existing so future audits can tell new from baseline
metadata:
  type: project
---

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
