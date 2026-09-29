---
name: rcmdcheck-baseline-v084
description: R CMD check baseline at v0.84/0.85 = 2 ERRORs + 2 NOTEs (tests abort on ../../R/globals.R; 3 of 4 vignettes execute code under check); CI never runs R CMD check at all
metadata:
  type: project
---

Measured 2026-09-16 on a clean `git archive HEAD` export (b3c05788b / v0.85.1),
`R CMD check --no-manual --no-build-vignettes`. Supersedes [[rcmdcheck-baseline-v048]].

**`Status: 2 ERRORs, 0 WARNINGs, 2 NOTEs`** (v0.79.1's four-warning clean-up does hold).

- **ERROR 1 — tests abort.** `tests/testthat/test-no-dangling-globals.R:22` does
  `parse(file = test_path("..","..","R","globals.R"))`. Under `R CMD check` that path is
  `MOSAIC.Rcheck/R`, which does not exist → `file()` throws, killing the whole
  `testthat.R` run (`[ FAIL 1 | WARN 59 | SKIP 43 | PASS 5504 ]`). Passes under
  `devtools::test()` because cwd is the source tree. **Fix exists and is verified:**
  `utils::globalVariables(package = "MOSAIC")` returns all 241 declared names from the
  installed namespace with no source tree.
- **ERROR 2 — "running R code from vignettes", 3 of 4 fail.** All four set
  `knitr::opts_chunk$set(eval = FALSE)` in the setup chunk; check *re-tangles and sources*,
  where that option does not apply. `Deployment.Rmd` runs `run_MOSAIC()` → "root directory not
  set"; `Running-simulations.Rmd` does `png("figures/...")` into a `.Rbuildignore`d dir →
  QuartzBitmap error; `Installation.Rmd` shells out to `R CMD build` on the live tree.
  `eval=FALSE` alone is NOT enough — needs `purl = FALSE` per chunk.
- **2 NOTEs:** (a) `:::` self-calls, incl. needlessly-prefixed
  `MOSAIC:::.impute_{cyclone,drought,flood}_probability_required()` in
  `compile_suitability_data.R:1428,1468`; (b) undefined globals `.dp`, `.run_sim_worker`,
  `.run_sim_worker_chunk` not in `R/globals.R`.

**Why nobody saw it:** `.github/workflows/R-CMD-check.yaml` **never invokes `R CMD check`**.
It runs `R CMD build`, `R CMD INSTALL`, `library(MOSAIC)`, `testthat::test_local()`. So the
enforced gate is not the gate `MOSAIC-pkg/CLAUDE.md` prescribes, and any defect that is
source-tree-vs-installed sensitive merges green. Check this before trusting any "check is clean"
claim.

**Build facts (measured):** tarball 12,653,981 B / 31.7 MB extracted, 44 s from a clean export.
`inst/extdata` 14.4 MB, `tests/` 7.4 MB (5.68 of it `replay_full_length.rds`, which does ship),
`inst/bench` 1.1 MB — **`inst/bench` is not `.Rbuildignore`d so the manual, non-asserting
benchmark harness installs into every user library.**

**`R CMD build .` cannot run in the working tree.** `R CMD build` copies the package dir
*before* applying `.Rbuildignore`, and the tree is ~12 GB (`claude/` 4.6 G, `local/` 5.8 G,
`output/` 728 M, `model/` 316 M — all ignored, all copied first) → `copying to build directory
failed`. Always build from `git archive HEAD | tar -x -C <scratch>`. A stray untracked dir in
the root can also abort the copy mid-flight (observed with `results/`).

**roxygen2 version trap:** `DESCRIPTION` carries `Config/roxygen2/version: 8.0.0` and no
`RoxygenNote`. `devtools::document()` on roxygen2 7.3.3 rewrites 8 `.Rd` files (`\docType{data}`
/`\format{}` blocks, `\link[=x]{x()}` → `\link{x}()`) and re-adds `RoxygenNote: 7.3.3`.
NAMESPACE is byte-identical either way. Document into a *copy* when auditing, or you will
report version churn as drift.

## Static-only baseline re-measured at v0.91.5 (2026-09-18)

Apples-to-apples harness for reviewing an uncommitted batch: `git archive HEAD | tar -x` into
scratch, overlay the dirty files listed by `git status --porcelain -- R man DESCRIPTION NAMESPACE
tests`, then
`R CMD build --no-build-vignettes --no-manual` and
`_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-manual --no-build-vignettes --no-tests
--no-vignettes --no-examples`.

**Pristine HEAD under those flags: 2 WARNINGs + 2 NOTEs.** Both WARNINGs are *harness artifacts* of
`--no-vignettes` (`Files in the 'vignettes' directory but no files in 'inst/doc'` and
`Directory 'inst/doc' does not exist`) — ignore them, they are not the real vignette ERROR recorded
above. The 2 NOTEs are the same two as v0.84: `:::` self-calls (12 names) and undefined globals
(`.dp .run_sim_worker .run_sim_worker_chunk`).

Facts worth keeping:
- **`tools::` does NOT need declaring.** `R CMD check` exempts base/standard packages from
  `'::' or ':::' imports not declared from:`. Verified: `tools::md5sum` is used in the tree and is
  not flagged. Do not report it as a missing dependency.
- **A `requireNamespace()` inside a `for (p in c(...))` loop is invisible to check.** Only literal
  `requireNamespace("pkg")` calls are reported under
  `'loadNamespace' or 'requireNamespace' call not declared from:`.
- **roxygen churn is now tiny, not 8 files.** `roxygen2::roxygenise()` on a scratch copy of
  `R/ man/ NAMESPACE DESCRIPTION data/ inst/extdata` rewrites only the Rd files that genuinely
  drifted, and leaves **NAMESPACE byte-identical** (roxygen2 refuses to overwrite a NAMESPACE it
  did not generate — which is the mechanical proof that MOSAIC's NAMESPACE is hand-maintained).
  It *does* rewrite DESCRIPTION: drops `RoxygenNote:` and bumps `Config/roxygen2/version`.
  At v0.91.5 the tree carries BOTH `Config/roxygen2/version: 8.0.0` and `RoxygenNote: 7.3.3`,
  which is contradictory — settle the pin before documenting.
- Installed size 22.6 Mb (`extdata` 14.1, `help` 3.2, `R` 3.1, **`bench` 1.1 — still shipping**).
