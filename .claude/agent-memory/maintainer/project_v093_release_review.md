---
name: v093-release-review
description: v0.93.0 production-readiness review (2026-09-29) — dual-default-site drift (sample_kappa), undeclared new deps, vignette purl hazard, NEWS/version gaps; reusable mechanical checks
metadata:
  type: project
---

Full-window (v0.67->v0.93.0) health review, report at `claude/review_v093/maint_report.md`.

**Recurring class found again: two default sites for sampling flags.** `mosaic_control_defaults()$sampling`
(run_MOSAIC.R) and `default_sample_args` (sample_parameters.R) must agree; v0.89.0 flipped
`sample_kappa` in only the control site. run_MOSAIC forwards control$sampling so it is safe; direct
`sample_parameters()` / `calc_model_ensemble(sampling_args=list())` are not.
**How to apply:** on ANY sampling-default change, run the mechanical diff (`claude/review_v093/maint/sampdiff.R`:
eval the `default_sample_args` assignment out of `body(sample_parameters)` and compare to control defaults).

**Mechanical checks that paid off (reuse):**
- `tools:::.check_packages_used(dir=".")` on the source tree = the R CMD check dependency/`:::` findings
  without running check (found gdistance/malariaAtlas/mipfp undeclared).
- `tools::codoc(dir=".")`, `undoc`, `checkDocFiles` work on source; `pkgdown::check_pkgdown()` for index.
- `knitr::purl()` each vignette and grep non-comment lines: global `opts_chunk$set(eval=FALSE)` does NOT
  survive tangling; Installation.Rmd tangles to install_github/install_dependencies/devtools::document.
- Function census: parse top-level `name <- function` at window start vs HEAD (`claude/review_v093/maint/fns.R`).

**Release-plumbing facts:** NEWS skipped ~35 window versions (incl. model-changing v0.89.0); feature-branch
commits carry versions lower than main at merge time (v0.90.6-11 after 0.91.0); fix/reff-postmerge-review
sits at 0.92.2 < main 0.93.0. Downloaders: `.mosaic_download` fetch is atomic but WB/WPP/EMDAT-api final
writes are not. Related: [[rcmdcheck-baseline-v084]], [[relic-audit-v084]], [[downloader-contract-inventory]].
