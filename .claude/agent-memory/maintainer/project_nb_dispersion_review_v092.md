---
name: nb-dispersion-review-v092
description: Review of feature/nb-dispersion-glmnb (v0.92.0) — NA-k escape when <5 fittable locations, splines-only-in-strings NOTE, roxygen 8.1.0 regen collateral, live-editing-during-review
metadata:
  type: project
---

Adversarial review of `feature/nb-dispersion-glmnb` (v0.92.0): `est_nb_dispersion()` (MASS::glm.nb,
weekly, Farrington/Noufaily) replacing `.nb_size_from_obs_weighted()` + the `nb_k_min_*` floor.

**Why:** the maintainer-side findings below are structural traps that will recur in this package,
not one-off bugs in one branch.

**How to apply:** reuse these as standing checks whenever a per-location estimator is added or an
estimated quantity is hoisted out of the likelihood.

## The blocker pattern: an "impossible" NA that is only rescued by a *panel-size-dependent* branch
`.nb_disp_fit_one()` returns `k = NA_real_` for `no_estimate_all_rungs_failed` /
`no_estimate_se_not_finite`. `.nb_disp_shrink()` rescues NA via its `inherit` branch — **but returns
early when `sum(fit_ok) < 5L`**. So the NA escapes for any panel with <5 fittable locations
(every single-country and small multi-country run) and for `shrink = FALSE` on the full 40-location
panel. On shipped `config_default` this bites `BFA`/deaths. NEWS and `@return` both claimed
"never NA".
**Standing check:** when a fallback lives in a *cross-unit* pooling step, always test it at
n_units = 1 and just-below-the-pooling-threshold, not only at full panel scale. MOSAIC's dominant
usage is single-country, so panel-scale fixtures systematically miss this class.

## `Package::fn` inside a *string* is invisible to R CMD check
`splines::ns(...)` only ever appears inside `sprintf()` formula strings, so codetools cannot see it
and check emits `Namespace in Imports field not imported from: 'splines'`. It still *works* at
runtime (`::` loads the namespace when the formula is evaluated). Fix is `importFrom(splines, ns)`
or a literal reference. Same trap applies to any `as.formula()`-built model spec.

## roxygen2 8.1.0 regeneration is destructive and lands silently in unrelated diffs
Running `devtools::document()` under roxygen2 8.1.0 on **untouched v0.91.0** (verified by
documenting a pristine `git archive` of HEAD):
- drops `RoxygenNote:` from DESCRIPTION, bumps `Config/roxygen2/version` 8.0.0 -> 8.1.0;
- eats `>=` at the start of a wrapped `@param` line (markdown blockquote): `(numeric >= 0` becomes
  `(numeric= 0` in `man/make_simulation_config.Rd`) and strips `\code{}` from some inline names;
- drops `\docType{data}` / `\keyword{datasets}` from `man/MINFEAT_V7_4_FEATURE_SET.Rd`;
- deletes the `is_diagnostics` param block from `man/calc_convergence_diagnostics.Rd`, producing a
  new `Rd \usage` WARNING — root cause is a **pre-existing** missing `@param is_diagnostics` in
  `R/calc_convergence_diagnostics.R`; main's committed `.Rd` had hand-maintained text roxygen
  cannot source.
**Standing check:** baseline attribution requires documenting a pristine HEAD archive with the
*same* roxygen, or the churn gets blamed on the feature branch.

## R CMD check baseline at v0.91.0 (macOS, R 4.5.3, roxygen 8.1.0 regen)
`0 ERROR, 3 WARNINGs, 2 NOTEs`, tests OK. The 3 W = `Rd \usage`(is_diagnostics) + the two
vignettes/`inst/doc` ones. The 2 N = `:::` self-calls + `no visible binding` (`.dp`,
`.run_sim_worker*`). Building the tarball from the worktree adds a spurious `hidden files .git`
NOTE — build from a `git archive` copy to avoid it. See [[project_rcmdcheck_baseline_v048]].

## The deprecation shim (lesson #13) was correct here — the proof recipe
`.retired_nbk` in `.mosaic_validate_and_merge_control()` keys off `names(control$likelihood)` (RAW
user input). Proved by scratch script: fires on `defaults() + override` AND on a bare partial list;
does NOT fire on pristine defaults / NULL / `list()` / a second pass over its own output. One nit:
it uses `control$likelihood` (`$` partial-matches), so a key named `likelihoodz` triggers it —
`.mosaic_migrate_renamed` uses `control[[section]]` (exact) and is the better pattern.

## Live-editing during review
The worktree changed under me three times (13:33, 14:04). `rsync` the tree to a scratch snapshot
before running check/tests, hash it, and report against the snapshot — otherwise findings and
evidence disagree.

Related: [[reviewer_checklist]], [[project_config_weight_matrices_carried_not_consumed]] (this change
is the first real consumer of `reported_*_weight`).
