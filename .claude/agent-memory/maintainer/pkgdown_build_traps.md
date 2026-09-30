---
name: pkgdown-build-traps
description: pkgdown/vignette traps in MOSAIC - root *.md always published, clean:false deploy, eval=FALSE include_graphics, buildignored figures
metadata:
  type: reference
---

- pkgdown 2.1.x `package_mds()` renders EVERY root `*.md` except README/LICENSE/NEWS and has no exclude option or .Rbuildignore awareness. MOSAIC's root CLAUDE.md and plan notes were live on gh-pages; the pkgdown workflow now `rm -f`s them in the CI checkout before building. Any new root .md must be added to that list or live elsewhere.
- The deploy uses JamesIves with `clean: true` (was false, which kept run_LASER/Running-LASER pages alive after removal). docs/ is no longer tracked (gitignored; CI builds it).
- A global `knitr::opts_chunk$set(eval = FALSE)` silently disables `include_graphics()` chunks too; they need `eval = TRUE`. Their PNGs must be tracked AND not .Rbuildignore'd, or R CMD build's vignette step cannot find them. Running-simulations figures live in `vignettes/figures/` (regenerate with `vignettes/articles/generate_running_simulations_figures.R`).
- NEWS headers like `# MOSAIC 0.84.0 - 0.91.x (...)` parse fine in pkgdown (version = first token). Verify with `pkgdown:::data_news(pkgdown::as_pkgdown("."))$version`.
