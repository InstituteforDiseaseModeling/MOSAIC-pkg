---
name: inst-dir-and-loadall-traps
description: Verified facts about what R CMD check does/doesn't inspect in inst/, tests/ shipping vs installing, load_all's global system.file shim, and cluster-worker package resolution — the packaging traps behind any shipped-script proposal
metadata:
  type: reference
---

Verified on R 4.5.3 / MOSAIC v0.69.1 (PR #122 checkout) while red-teaming an `inst/bench/`
benchmark-harness proposal. These are the load-bearing packaging facts; re-verify only if
the R release changes.

## R CMD check provably does NOT parse `inst/` R code
- `tools:::.check_package_ASCII_code` reads ONLY `NAMESPACE` + `list_files_with_type(file.path(dir,"R"))`.
- `tools:::.check_packages_used` operates on `file.path(dir, "R")` only.
- `tools:::.check_package_subdirs` only special-cases `R/`, `demo/`, `inst/doc`. No name collision
  risk for arbitrary `inst/<name>/` dirs.
- Empirical corroboration in-repo: `inst/examples/simulate_outbreak_settings.R` has 3 non-ASCII
  lines, yet the baseline check's non-ASCII WARNING lists only `R/plot_Reff.R` + `R/run_rolling_cv.R`.

**Consequence:** any `.R` shipped under `inst/` gets ZERO static analysis — no `:::` NOTE, no
"undefined global" NOTE, no unstated-dependency check, no Rd/NAMESPACE surface. "No R CMD check
surface" is TRUE and is exactly why such code rots undetected. Only a testthat test that sources it
gives coverage.

**Exceptions that DO reach inst/:** executable-bit check ("checking for executable files"), hidden
files/dirs, portable file names, file permissions, installed size. `.Rbuildignore` has no `^\.git$`
rule (hence the standing `.git` hidden-file NOTE) and nothing excluding `inst/`, so stray
`.DS_Store` / default out-dirs under `inst/` join that NOTE and the tarball.

## `tests/` ships in the tarball but is NOT installed
`tar tzf MOSAIC_*.tar.gz` contains `MOSAIC/tests/testthat/fixtures/*`, but the installed tree
(`/Users/johngiles/Library/R/arm64/4.5/library/MOSAIC`) has NO `tests` dir, and
`system.file("tests","testthat","fixtures","<f>.rds", package="MOSAIC")` returns `""`.
R CMD INSTALL needs `--install-tests` (not the default) to change that.
**Consequence:** shipped `inst/` scripts can never reach a `tests/testthat/fixtures/` file.
`replay_full_length.rds` is 5.8 MB — copying it into `inst/` would push installed size 13.2 -> ~19 MB
and create two copies needing lockstep maintenance.

## `pkgload::load_all()` shims `system.file()` GLOBALLY
Under `load_all("/path/to/pkg")`, a plain `system.file("extdata","x", package="MOSAIC")` called from
globalenv resolves to `<src>/inst/extdata/x` — the git WORKING TREE, which is writable.
Without load_all it resolves to the installed lib. So "system.file() is read-only" is FALSE in dev
mode, and the same script silently reads different files depending on invocation. `packageVersion()`
does not disambiguate (load_all reports the source version).

## PSOCK workers load the INSTALLED package, never the load_all'd tree
`make_mosaic_cluster()` workers run `library(MOSAIC)` from `.libPaths()`
(`R/make_mosaic_cluster.R:78`). Under load_all, driver and workers run DIFFERENT builds.
It also hard-stops without `getOption("root_directory")` (`make_mosaic_cluster.R:59-62`) and each
worker calls `set_root_directory()` + `get_paths()`, so ANY cluster workload needs the data tree —
not just calibration.

## git provenance is silently guessed for an installed package
`.mosaic_capture_environment()` (`R/run_MOSAIC_infrastructure.R:266-276`) resolves
`git_dir <- if (file.exists("./.git")) "." else if (file.exists(file.path(pkg_dir,".git"))) pkg_dir else NA`
— i.e. falls back to the CURRENT WORKING DIRECTORY's repo. Installed packages have no `.git`, so a
recorded sha can be from an unrelated repo. Never trust `git_sha` from an installed-package run.
It already captures R/platform/MOSAIC version, hostname, user, os, n_cores_available/requested,
git sha+branch, config/priors versions -> `1_inputs/environment.json`. Extend it; don't build a
second metadata capture. No BLAS/timing field yet; `RhpcBLASctl` is already in Imports (physical
cores) and `extSoftVersion()["BLAS"]` is base, so no new dep is needed.

See [[reviewer-checklist]], [[rcmdcheck-baseline-v048]].
