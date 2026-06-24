---
name: sample-parameters-tests-need-root-or-paths
description: Tests calling sample_parameters() error in get_paths() (no root) unless they pass PATHS=list() or skip-on-no-root; a green run on the author's machine false-passes in CI/clean processes
metadata:
  type: project
---

`sample_parameters()` calls `get_paths()` at the top WHENEVER `PATHS` is NULL
(R/sample_parameters.R ~L238-241). `get_paths()` `stop()`s with "Cannot find
root_directory" if no MOSAIC root is set. So any test that calls
`sample_parameters()` without supplying `PATHS` is root-dependent.

**Why this is a false-pass trap:** the author's interactive session usually has a
root set (a `.Rprofile`/`set_root_directory("~/MOSAIC")` somewhere), so the test
PASSES locally and the commit claims "FAIL 0". In a clean `Rscript`, on CI, or in
a VM without root configured, the SAME test ERRORS in `get_paths()` — *before* it
reaches the logic under test, so a guard the test claims to cover may have ZERO
real coverage (exactly what happened to `test-sample_parameters_B2_mu.R` block 9's
version-skew `stop()` guard before v0.49.4).

**Two correct patterns (pick by the test's intent):**
1. Self-contained math fixtures that supply explicit `priors=`/`config=` (don't
   need real data): pass `PATHS = list()` — any non-NULL value skips `get_paths()`.
   `PATHS` is only carried through to the returned config; sampling does not read
   it. This is the v0.49.4 fix (`.b2_sample()` wrapper injecting `PATHS=list()`).
2. Tests that genuinely load package defaults from a root: follow the sibling
   convention in `test-sample_parameters_zeta.R` / `test-sample_parameters_rho_deaths.R`:
   `tryCatch(set_root_directory("~/MOSAIC"), error=function(e) NULL)` at file top +
   `skip_if(is.null(getOption("root_directory")), "MOSAIC root directory not set")`
   in each block. (`helper-skips.R` `.with_mosaic_root()` does both, preferring
   `/workspace/MOSAIC` then `~/MOSAIC`.)

**Review trigger:** grep a new/changed test for `sample_parameters(`; if the call
has no `PATHS=` arg AND the file has no `set_root`/`skip_if(...root_directory...)`,
it is a root-dependent false-pass — flag it.

See also [[reviewer-checklist]].
