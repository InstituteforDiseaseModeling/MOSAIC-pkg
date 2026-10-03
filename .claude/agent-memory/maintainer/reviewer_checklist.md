---
name: reviewer-checklist
description: Institutional review checklist for MOSAIC data-object/config bumps and engine-semantic changes
metadata:
  type: feedback
---

Durable review checks for MOSAIC, built from caught regressions.

**Data-object bumps (config_default / priors_default):**
- Rebuild the changed array from its committed source and assert bit-exact match
  to the shipped .rda/.json (e.g. psi_jt from pred_psi_suitability_day.csv). See
  [[config-default-psi-provenance]].
- Structural-diff the .json (python json walk) to confirm ONLY the intended keys
  changed -- catches accidental window/prior drift.
- Verify version string bumped in BOTH the data-raw maker AND the metadata$version
  inside the object, and that the drift-guard test (test-cfr-pipeline-consistency.R,
  the Lesson-#12 guard) passes.
- Confirm the regeneration recipe is reproducible from the committed package +
  canonical pipeline (model/LAUNCH.R). If produced by an ad-hoc/compute-VM run with
  overrides, that is a provenance finding -- the artifact is hand-curated, document it.
- **Builder gains a field before the data is rebuilt** (v0.101.0 `reported_tier`): the
  release ships the consumer + builder while the shipped object lacks the field, so the
  feature is inert. Check: NEWS headline qualified; builder `version`/description bumped (a
  rebuild under the old label is indistinguishable to version-keyed preflights);
  `R/config_default.R` field list; a `skip_if(is.null(...))` contract test like
  test-config_default_weights.R (presence-iff-value, domain, JSON round trip).

**Always check for the compute-env parity trap:**
- `git log origin/main..HEAD` -- unpushed config/data commits mean hedgehog/Coiled
  (which install from remote) silently run STALE defaults on large calibrations.
  This is a SHOULD-FIX every time a default data object changes.

**Engine-semantic / field changes:** grep ALL siblings (grep -l old R/), incl.
temporarily-unused fns (Lesson #11). Use reported_deaths/reported_cases not
disease_* (Lesson #12).

**New fns / fixes ship WITHOUT a test = the #1 recurring smell** (Lessons #1/#6).
On any bug-fix commit, grep tests/testthat for the fixed function name; if zero
references, flag missing regression test (e.g. 979f066e CV fix had none).

**Build/runtime separation:** model/ is .Rbuildignore'd -> large CSVs there are
repo-bloat, not build breakage. Don't conflate.

**Removing a parameter from the model** (learned on CFR v2.1, v0.96): the sibling
set is larger than the config/sampler/convert lists. Also check
`data-raw/make_estimated_parameters_inventory.R` + `data/estimated_parameters.rda`
+ `R/estimated_parameters-data.R` row counts + `R/priors_default.R` section counts +
`.claude/skills` lever advice + the MOSAIC-docs spec + country-repo config builders.
Consumers of estimated_parameters all intersect with sample columns, so stale rows
fail SILENTLY (empty plot panels), never loudly.

**Mutation-test the wiring, not just the helpers.** Green unit tests on a helper say
nothing about whether run_MOSAIC calls it: on v0.96.1, replacing the worker's
integrated-deaths call with `if (FALSE)` left the WHOLE suite green (incl. the opt-in
integration test). Harness recipe: `git archive` HEAD to /tmp, apply one fixed-string
mutation per copy, `load_all` + `test_file`/`test_dir` with `TESTTHAT_PARALLEL=false`.
Opt-in tests (`MOSAIC_RUN_INTEGRATION=1`) never run in CI -- a fix guarded only by
one is unguarded.

**Fixture-loosening recurs:** identity tests that set `chi_endemic == chi_epidemic`
collapse the one axis the chi choice acts on (seen in v0.93 roundtrip tests AND v0.96
onset tests). Demand a chi-asymmetric arm (force epidemic PPV with
`epidemic_threshold = 0`, endemic with `= 1`).

**Two cheap check traps:** roxygen markdown turns `[a x b]` into `\link{a x b}` (Rd
cross-ref WARNING) -- escape as `\[a x b\]`; a NEWS.md `##` heading containing
" v2.1" makes R's news parser treat level-2 headings as versions (NOTE, and
`news()` reports the package as version 2.1).
**Never trust "R CMD check is clean" or "tests pass" without checking WHICH runner.**
`.github/workflows/R-CMD-check.yaml` does not invoke `R CMD check` at all (build + INSTALL +
`testthat::test_local()` only). `test_local()` runs from the SOURCE tree; check runs from
`<pkg>.Rcheck`. Any test that reaches for `../../R/...` or `../../<anything>` passes in one and
errors in the other — one such test currently ERRORs the whole check
(see [[rcmdcheck-baseline-v084]]). Grep `tests/` for `\.\./\.\./` on every review.
**Build from a clean export, never in place.** `R CMD build` copies the whole package dir
before applying `.Rbuildignore`; this tree is ~12 GB of ignored dirs, so `R CMD build .` fails.
Use `git archive HEAD | tar -x -C <scratch>` then build there.
**Vignette code runs under check even with `eval = FALSE`.** `R CMD check`'s "running R code
from vignettes" re-tangles and sources; a global `knitr::opts_chunk$set(eval = FALSE)` does not
survive that. Needs `purl = FALSE` per chunk. Three of four MOSAIC vignettes currently fail here.
**Orphan scans need TWO greps, not one.** `name\s*\(` misses functions passed as objects
(`Susceptible = sim_phase_susceptible`, `w <- .mosaic_traj_render_worker`) — that pattern alone
would have falsely condemned all ten engine phase functions. Also grep the bare name. And before
calling a public `process_*`/`plot_*`/`est_*` fn orphaned, remember those are user entry points
with no R/ caller by design (Lesson #8's converse).
**After ANY path/backend removal, re-run the orphan grep** (Lesson #14(iii)) and re-read every
guard that discriminated between the paths (#14(ii)). Live examples: `.mosaic_parse_sim_ids()`
orphaned by its own v0.80.1 fix; `capture_in_gather` permanently FALSE; three
`requireNamespace("reticulate")` guards dead because reticulate never left `Imports:`.
A `requireNamespace("<pkg in Imports>")` guard is the #13 shape — dead on arrival.
**Relic triage rule for `laser*` hits:** `laser-cholera`/`laser-core`/`laser_cholera`/
`laser.cholera` are REAL external package names (Lesson #16(iii)) — most hits are legitimate
provenance. The reportable ones are **present-tense claims about who consumes a value**
("Consumed by laser-cholera v0.13+ at infectious.py:88-92"). Grep for
`Consumed by|Passed to|imported|per worker` near `laser`.
**Version/NEWS parity is a standing check.** `comm -23` the committed version tags
(`git log --oneline <range> | grep -oE '\(v[0-9.]+\)'`) against `grep -oE '^# MOSAIC [0-9.]+'
NEWS.md`. At v0.84, 23 of 25 released versions had no NEWS entry.
**Document into a COPY.** roxygen2 version drift (tree is 8.0.0 via
`Config/roxygen2/version`) makes a local `devtools::document()` rewrite 8 `.Rd` files
cosmetically. Copy `R/ man/ NAMESPACE DESCRIPTION` to scratch, document there, diff back.
**NAMESPACE is HAND-MAINTAINED, not roxygen-generated** (no "Generated by roxygen2" header;
it contains `#` comments roxygen would never emit, and `importFrom("glue","glue")` twice).
So an `@importFrom` tag in a new `R/*.R` file is **inert** — it does not reach NAMESPACE.
Verify a genuinely new dependency by grepping NAMESPACE directly, and treat an `@importFrom`
for a package the file never calls as documentation drift, not a real import.
**Run the new function before reviewing it.** A 2-minute `pkgload::load_all()` + one dry-run
call caught a dead `include_suitability` filter in `update_mosaic_data()` that reads correct
(`Filter(function(s) s$group != "4", ...)`) but never fires because the group ids are
"4A"/"4B" — the Lesson-#13 shape. Static reading had missed it.
See [[update-mosaic-data-registry-review]].
**Adding a VALUE to an enumerated column breaks the readers that enumerate, not the ones
that filter.** Auditing "who filters `split == "selection"`" is not an audit. Also grep
(a) readers that assert a ROW COUNT (`if (length(x) != 9L) stop`), (b) readers that
VALIDATE the legal value set (`!all(g$split %in% c(...))`), (c) readers that filter only
the *other* key (`grid == "prod"`) and then iterate everything. One new EVAL_GRID row
simultaneously disabled every arm fit and every call to the scorer.
See [[psi-evolve-redteam-review]].
**N parallel scorers over the same cells: diff the ESTIMATOR, not just the filters.**
Two tables labelled "burden-weighted MAE" differed by 10.5% with identical cell sets,
purely because one weighted per country-BLOCK and the other averaged blocks within a
country first. Unequal block counts per unit (a country missing from some folds) is what
makes them disagree. Reproduce each estimator from observations alone before believing a
written explanation of the gap.
**Positional indexing across two different time grids is the silent-wrong-number trap.**
`baseline$point[seq_len(nrow(merged))]` is correct only while the baseline is constant.
MOSAIC's psi cache is DAILY and the observed panel is WEEKLY, so that pattern paired 13
weekly observations with 13 consecutive CALENDAR DAYS of climatology. Grep for
`\[seq_len\(nrow\(` near a `merge()`.
**A declared dependency graph must be validated against real file producers.** For any
`deps`-style registry, mechanically (a) assert every dep names an existing id — a driver
using `intersect()` silently ignores typos; (b) assert topological order if the driver does
not sort; (c) for each edge, confirm the named producer actually writes the file the consumer
reads. MOSAIC has processed files with **no producer at all** and consumers guarded by
`if (file.exists())`, so a wrong edge degrades output with zero error.
**After merging parallel fix branches, scan for duplicate top-level definitions.** The
2026-09-30 deep-review integration left `.mosaic_best_subset_weights()` defined in BOTH
`R/grid_search_best_subset.R` and `R/run_MOSAIC_helpers.R` (two branches each added it).
R CMD check and the tests are silent: collation (alphabetical) keeps the last file's copy,
so the other becomes dead code with its own, possibly contradictory, docs. One-liner:
`grep -hoE "^[.A-Za-z_][.A-Za-z0-9_]* *<- *function" R/*.R | sed 's/ *<-.*//' | sort | uniq -d`.
Also after such merges: tests written by one branch can assert the OTHER branch's old
return shape (est_initial_E_I fit names; resim fixture lacking cases_mean once the default
flipped to mean) — run the whole suite on the integration branch, not per branch.
**Changing a helper's input resolution/scale breaks sibling options tuned to the old one.**
v1.0 prep near-miss (my own): to make plot_seasonal_clustering() match
est_seasonal_dynamics() "by construction" I fed it the 365-column daily matrix instead of
52 weekly means. ward.D2/kmeans/knn partitions were unchanged, but the `dbscan` option has a
fixed `eps = 1` (a weekly-scale distance; daily vectors are ~sqrt(7) further apart), so on the
real fits every country became noise. Caught only by running ALL methods on real data before
and after. Rule: when a change alters the dimension or scale of what a multi-method function
consumes, run every method/option on production data both ways and pin any scale-dependent
one with a test whose fixture straddles the threshold.
**NEWS.md `# pkg (development version)` heading:** R's parser
(`tools:::.build_news_db_from_package_NEWS_md`) silently skips a leading non-version heading
(no NOTE); pkgdown lists it as "development version". Safe for release branches that must
not bump DESCRIPTION yet.
**A "legacy mode" knob documented as reproducing old runs: check every OTHER change in the
release is gated by it too.** v0.101.0 `cases_scoring = "daily"` still took the new panel-trend
dispersion (CMR/UGA/ZAF k 0.1 -> 1.0-1.4), so it did not reproduce v0.100.1. Run the resolver
from `git archive <base>` and the branch on the same config and diff.
**A new gate nested inside an old one must also feed the "unscorable -> NA" rule.** v0.101.0's
weekly cases gate (>= 3 complete weeks) sits inside the daily `have_cases` gate, but the NA rule
tests only `have_cases`; 1-2 complete weeks with no deaths scores a constant 0 per draw
(single-location run -> uniform posterior instead of all-NA). Probe the gap between the gates. (Found at
the RC; fixed before release in 1861f0399.)
**Verify every number a NEWS bullet states, including the ones nobody flagged.** The v0.100.1
entry had five measured claims; two were flagged, two more were also off (the ~50-700 E+I
range was ~25-680; "90-100%" 60-day ignition measured 87-100%), and the "~90% failed to
ignite" referred to an unshipped intermediate build, not the shipped priors. Re-measure from
the shipped objects (and `git show <sha>:data/*.rda` for "before" claims).
**A data-object rebuild merged under feature NEWS stales every "on config_default" number.**
v0.101.0: the red-team branch measured on v6.0, the rebuild produced v6.1, and the Rd/code
comment got v6.1 values while NEWS L29 (panel-trend refit-at-30: CMR/UGA/ZAF, SD 1.56) and L30
(CAF phi 1.72; v6.1 gives 1.65) kept v6.0 ones (both corrected before release). Grep NEWS for "on `config_default`" after any
rebuild merge and re-run each one on the shipped rda. Pre-fix code via assignInNamespace in-session.
**A measured figure replacing an estimate: sweep digit AND word forms in every doc surface.**
bdf96a07f swapped "five to seven times" for the measured 4.8x in calc_model_likelihood, NEWS
and CLAUDE.md, but missed `weight_wis` in `mosaic_control_defaults()`'s `@param likelihood`
items (R/run_MOSAIC.R). Those control-default items are a recurring missed sibling. Grep
`"5-7|five to seven|5 to 7"` across R/ man/ vignettes/ inst/ .claude/.
**Hard-coded paths in rerunnable scripts: check the case against `git ls-files`.** The laptop's
APFS is case-insensitive, so `MOSAIC-data/processed/enso` worked there although the tracked
directory is `processed/ENSO`. On dugong/hedgehog (Linux) the same script fails. This hit the
v0.101.0 psi provenance bundle (fixed in MOSAIC-data 2ee3725).
**md5-pinned data objects: never fix a description string in place.** Record the correction in
NEWS and carry it to the next rebuild ([[v0101-pending-rebuild-corrections]]).
