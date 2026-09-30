---
name: pkgdown-root-md-publish-trap
description: pkgdown renders EVERY root *.md (incl CLAUDE.md, dev plans) to the public site regardless of .Rbuildignore; gh-pages deploy clean:false keeps removed pages forever
metadata:
  type: reference
---

- `pkgdown:::package_mds()` (R/build-home-md.R) globs `*.md` in the pkg root + `.github/`, excluding only README/LICENSE/LICENCE/NEWS/404/issue+PR templates/cran-comments. `.Rbuildignore` has NO effect. Verified 2026-09-29: public gh-pages had CLAUDE.html, migrate-laser-r.html, plan-review.html, pipeline-performance-plan.html (repo is PUBLIC).
- `.github/workflows/pkgdown.yaml` deploys with JamesIves `clean: false`, so pages for removed functions/vignettes (Running-LASER, run_LASER*, Project-setup) stay live indefinitely.
- Tarball check recipe: `git archive origin/main | tar -x -C $T` then `R CMD build --no-build-vignettes --no-manual $T` from a separate out dir (never mkdir inside the export — an empty `src/` would be read as compiled code).

**How to apply:** any new root .md = public web page; put dev plans in MOSAIC-notes. Full 2026-09-29 survey: claude/survey_2026-09-29/misplaced_files.md. See [[project_relic_audit_v084]].
