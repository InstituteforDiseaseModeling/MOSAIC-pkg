---
name: calibration-freeze-verification
description: Mechanical proof that a branch changes no calibration code or data object - hash every namespace function body and lazydata object in two git-archive trees and diff (used for release/v1.0 vs v0.100.1)
metadata:
  type: reference
---

When a release must calibrate exactly like a validated version (the 2026-10-01 release/v1.0
plan: v0.100.1 + docs/plots/tests only, since superseded by the 0.101.0 path), `git diff --stat` is necessary but not sufficient: it
lists files, not what is reachable. The proof that worked (2026-10-01):

1. `git archive <base> | tar -x -C A` and `git archive HEAD | tar -x -C B`.
2. In a fresh `Rscript` per tree: `pkgload::load_all(tree, export_all = FALSE, helpers = FALSE)`,
   then for every function in `asNamespace("MOSAIC")` (`ls(all.names = TRUE)`):
   `rlang::hash(deparse(f, control = c("keepNA","keepInteger","niceNames","showAttributes")))`
   (no `useSource`, so comment-only edits do not count), and for every object in
   `get(".__NAMESPACE__.", ns)$lazydata`: `rlang::hash(obj)`. Also save
   `getNamespaceExports()`.
3. Diff: changed / added / removed function names, changed data objects, export set.

Result to report: "of 773 functions exactly N changed (all plot_*), M dot-prefixed
helpers added, none removed; all 15 lazydata objects (priors_default,
config_default, ...) hash-identical; exports identical". Then grep `R/` for each
changed function to show its only mentions are comments and its callers are the
documentation section of model/LAUNCH.R, never run_MOSAIC()/render paths.

Pair with the full-check recipe in [[rcmdcheck-baseline-v048]]. Related:
[[reviewer-checklist]].
