# CLAUDE.md - MOSAIC R Package

## Quick Reference Card

**Check current state (do this FIRST):**

``` bash
git status                      # Any uncommitted changes?
git log --oneline -5           # Recent commits
grep "^Version:" DESCRIPTION   # Current version (you'll bump this)
Rscript -e "devtools::test()" # Baseline: all tests should pass
```

**Development cycle:**

``` bash
# 1. Write production-ready code (no placeholders!)
# 2. Add/update tests
Rscript -e "devtools::test()"              # Must pass

# 3. Update docs if function signatures changed
Rscript -e "devtools::document()"

# 4. Check package builds cleanly
R CMD check .
```

**Commit workflow (ALWAYS):**

``` bash
# 1. Bump version in DESCRIPTION (patch: 0.13.25→0.13.26, minor: 0.13.25→0.14.0)
# 2. Stage relevant files: git add DESCRIPTION R/my_file.R tests/...
# 3. Commit with version: git commit -m "Fix bug (v0.8.8)"
# 4. Push: git push origin main
```

**Essential commands:**

``` bash
Rscript -e "devtools::test()"                                    # Run all tests
Rscript -e "testthat::test_file('tests/testthat/test-foo.R')"  # Single test
Rscript -e "devtools::document()"                                # Update docs
R CMD check .                                                     # Full package check
Rscript -e "MOSAIC::check_dependencies()"                        # Verify Python env
```

**Critical paths (DON’T BREAK THESE):** -
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
— main calibration workflow (`R/run_MOSAIC.R`) -
[`calc_model_likelihood()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_likelihood.md)
— called 1000s of times (`R/calc_model_likelihood.R`) -
[`sample_parameters()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sample_parameters.md)
— 301 parameters (`R/sample_parameters.R`) -
[`calc_model_ensemble()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md)
— posterior-weighted ensemble (`R/calc_model_ensemble.R`)

**File rules:** - Use `./claude/` for ALL temporary/exploratory files -
Use
[`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md)
for ALL file operations (never hardcode paths) - Never modify:
laser-cholera/, ees-cholera-mapping/, jhu_cholera_data/ (read-only) -
Never modify: MOSAIC-data/raw/ (read-only) - Never create files in
package root without necessity

**Ask user first if:** - Adding new R or Python package dependency -
Changing function signature of exported function - Modifying
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
core loop - Unsure about approach (multiple valid solutions)

------------------------------------------------------------------------

## Package Overview

**MOSAIC** (Metapopulation Outbreak Simulation And Interventions for
Cholera) is a production R package for cholera transmission simulation
across Sub-Saharan Africa. It integrates with the Python laser-cholera
engine via reticulate and provides functions for data processing,
parameter estimation, Bayesian calibration, and visualization.

**Key capabilities:** SEIR metapopulation simulation, Bayesian Filtering
with Resampling (BFRS) calibration, environmental suitability modeling,
vaccination/WASH intervention analysis, spatial transmission with human
mobility.

## Architecture

    MOSAIC/                          # Root (set via set_root_directory())
    ├── MOSAIC-pkg/                  # THIS PACKAGE (EDITABLE)
    │   ├── R/                       # Function files
    │   ├── tests/testthat/          # Unit tests
    │   ├── inst/extdata/            # Default parameters (JSON)
    │   ├── inst/py/                 # Python environment.yml
    │   ├── data/                    # R data objects (.rda)
    │   ├── model/                   # LASER model I/O and LAUNCH.R
    │   ├── claude/                  # USE THIS for temporary files
    │   └── DESCRIPTION              # Package metadata
    ├── MOSAIC-data/                 # Data repository (raw/ is READ-ONLY)
    ├── MOSAIC-docs/                 # Documentation website
    ├── laser-cholera/               # Python simulation engine (READ-ONLY)
    ├── ees-cholera-mapping/         # Web scraping tools (READ-ONLY)
    └── jhu_cholera_data/            # JHU scraper (READ-ONLY)

**Function naming conventions:** `process_*()` data cleaning, `est_*()`
parameter estimation, `plot_*()` visualization, `get_*()` data
retrieval, `calc_*()` mathematical calculations, `check_*()` validation,
`sample_*()` parameter sampling.

## The run_MOSAIC Workflow

The
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
workflow is the **centerpiece** of the package — it orchestrates the
complete Bayesian calibration pipeline.

**Key files:** - `R/run_MOSAIC.R` — main workflow and simulation
worker - `R/run_MOSAIC_helpers.R` — convergence detection, weight
calculation - `R/run_MOSAIC_infrastructure.R` — directory setup, I/O,
summary generation

**BFRS calibration (2 phases):** 1. **Adaptive calibration** — batches
of LASER sims until convergence (R² target, ESS thresholds) 2.
**Predictive batches** — model-based batch sizing with ESS re-evaluation
until convergence

**Post-calibration:** - Best model identified, config saved to
`config_best.json` -
[`calc_model_ensemble()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md)
computes posterior-weighted predictions (weighted median/mean across
parameter sets × stochastic reruns) - R² and bias ratio computed from
weighted median vs observed data -
[`plot_model_ensemble()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_ensemble.md)
generates prediction plots (only when `plots=TRUE`)

**Output structure:**

    dir_output/
    ├── 1_inputs/          # config.json, priors.json, control.json, environment.json
    ├── 2_calibration/     # samples.parquet, posterior/, diagnostics/, state/
    └── 3_results/         # summary.json, predictions/, figures/

**Thread safety (CRITICAL for parallel execution):**

``` r

# Built into run_MOSAIC(), but needed for custom parallel code
MOSAIC:::.mosaic_set_blas_threads(1L)
Sys.setenv(OMP_NUM_THREADS="1", MKL_NUM_THREADS="1", OPENBLAS_NUM_THREADS="1",
           NUMEXPR_NUM_THREADS="1", TBB_NUM_THREADS="1", NUMBA_NUM_THREADS="1")
```

## Likelihood Calculation

[`calc_model_likelihood()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_likelihood.md)
computes a multi-component log-likelihood:

**Core:** Negative Binomial time-series likelihood for cases and deaths
(weighted MoM dispersion, k_min floor).

**Shape terms (all T-normalized, weight \> 0 enables):** - Peak timing
(Normal LL on time differences) - Peak magnitude (log-Normal on peak
ratios) - Cumulative progression (NB at fractions 0.25/0.5/0.75/1.0) -
WIS (Weighted Interval Score per Bracher et al. 2021)

**Assembly:**
`LL = w_cases*NB_cases + w_deaths*NB_deaths + (T/N_peaks)*w_pt*peaks + T*w_cum*cumulative + T*w_wis*WIS`

All shape term weights default to 0 (OFF). Non-finite LL returns -Inf.

## Python Integration

**Environment:** `~/.virtualenvs/r-mosaic` (managed via
[`install_dependencies()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/install_dependencies.md))
**Core packages:** laser-cholera, laser-core, numpy, h5py, pyarrow
**Check:**
[`MOSAIC::check_dependencies()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/check_dependencies.md)
**Troubleshoot:** `MOSAIC::remove_MOSAIC_python_env()` then
`MOSAIC::install_dependencies(force = TRUE)`

## Key Files

| File | Purpose |
|----|----|
| `R/run_MOSAIC.R` | Main calibration workflow, simulation worker |
| `R/run_MOSAIC_helpers.R` | Convergence detection, weight calculation |
| `R/run_MOSAIC_infrastructure.R` | Directory setup, I/O, summary generation |
| `R/calc_model_likelihood.R` | Multi-component likelihood |
| `R/calc_model_ensemble.R` | Posterior-weighted ensemble predictions |
| `R/sample_parameters.R` | Sample 301 parameters from priors |
| `R/make_LASER_config.R` | Config validation (60+ parameters) |
| `R/calc_model_R2.R` | R² (corr and SSE methods) + bias ratio |
| `R/get_paths.R` | Directory path management |
| `R/run_LASER.R` | Python laser-cholera wrapper |

## Troubleshooting

**Python environment broken:** `MOSAIC::remove_MOSAIC_python_env()` →
`MOSAIC::install_dependencies(force = TRUE)` → restart R

**Parallel worker deadlock:** BLAS/Numba threading conflict. Ensure all
6 thread env vars set to “1” (built into
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md),
needed for custom parallel code).

**Memory issues:** ~2 GB per worker. 16 cores needs ~32 GB RAM. Use
`results <- vector("list", n)` not `results <- c()`.

**R CMD check errors:** “Undocumented parameters” → add `@param`.
“Undefined global variable” → add to `R/globals.R`. Always run
`devtools::document()` first.

------------------------------------------------------------------------

## Development Standards

Claude Code working on MOSAIC must act as a **production systems
software engineer**. Read the [Verification
Requirements](#verification-requirements) and [Lessons
Learned](#lessons-learned) sections before starting any work.

### Code Quality

**NEVER:** placeholder code, commented-out code, debug statements,
hardcoded paths, superfluous features beyond what was requested.

**ALWAYS:** production-ready from the start, complete implementations
before committing, use
[`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md)
for file operations, be concise — do only what is asked.

**If you have a good idea for improvement:** ASK FIRST before
implementing it.

### Version Management

**Bump version on every commit.** Patch (bugs/docs), Minor (features),
Major (breaking changes). Include version in commit message:
`"Fix bug (v0.13.26)"`.

**Data object versioning:** `config_default` version in
`data-raw/make_config_default.R`, `priors_default` version in
`data-raw/make_priors_default.R`. Bump and rebuild `.rda`/`.json` when
contents change.

### Testing

Run `devtools::test()` before AND after changes. Bug fixes need
regression tests. New features need unit + integration tests. Run
`R CMD check .` before committing.

### Never Break Existing Workflows

Critical paths:
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md),
[`calc_model_likelihood()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_likelihood.md),
[`sample_parameters()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sample_parameters.md),
[`calc_model_ensemble()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md),
weight calculation, parallel execution.

### Performance

Profile before/after on hot paths. Hot paths:
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md),
[`calc_model_likelihood()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_likelihood.md)
(called 1000s of times), `.mosaic_run_simulation_worker()`. Design for
40K sims, test with 10-100. Thread safety: BLAS threads = 1.

### Git, Docs, Dependencies

**Git:** Bump version → test → document → commit (with version) → push.
Atomic commits. **Docs:** One-line `@param`. Update `_pkgdown.yml` when
adding/removing functions. Run `devtools::document()`. **Dependencies:**
NEVER add new R/Python dependencies without user approval.

### Verification Requirements

**After creating any new function:** - Grep the codebase for all
existing inline implementations that the new function should replace -
Replace every call site — creating the function without wiring it in is
incomplete work - List every file modified and every call site updated
in your response - If the function replaces inline code, confirm zero
instances of the old pattern remain

**After modifying or rewriting any existing function:** - State what the
function did BEFORE and what it does AFTER - Confirm the new methodology
matches what was requested — do not substitute a different algorithm
without explicit approval - If the function had callers, verify all
callers still work with the new signature/behavior - If the old
implementation is being replaced, remove it entirely — do not leave two
parallel systems

**After any refactoring (calc/plot splits, function renames, etc.):** -
Confirm the old function is either removed or deprecated with a
wrapper - Verify the new functions are called from the correct places in
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md) -
Check that no code paths are now gated behind flags they shouldn’t be
(e.g., computation gated behind `plots=TRUE`)

**Anti-placeholder rule:** - Never commit functions with
TODO/FIXME/placeholder logic - Every code path must perform a real
calculation, not a stub - If you cannot fully implement something, say
so — do not fake it

**Self-audit before committing:** - Grep for the old pattern (inline
code, old function name) and confirm zero remain - Grep for any new
function you created and confirm it has at least one caller in
production code - Check that utility functions you used actually exist
in the package (don’t invent function names)

### Workflow Checklist

**Before:** Read git log, check if function exists, run tests,
understand workflow fit **During:** Production-ready code, test
incrementally, no hardcoding, stay focused **Before commit:** Bump
version, test, document, R CMD check, verify no debug code **Commit:**
Stage relevant files, clear message with version, push **After:** Verify
push, check CI/CD

### Lessons Learned

Record of specific errors introduced by AI coding assistants. Read these
before starting work — they represent patterns to actively avoid.

1.  [`calc_model_R2()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_R2.md)
    created with proper corr/SSE methods but all 16 call sites kept
    using inline `cor()^2` — function existed for months unused (v0.22.4
    fix)

2.  [`calc_model_ensemble()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md)
    rewritten to do single-config stochastic reruns instead of the
    intended weighted multi-config posterior ensemble — wrong
    methodology under the same name, plus the old
    `plot_model_fit_stochastic_param()` was kept running in parallel
    (v0.22.4-5 fix)

3.  `est_transmission_spatial_structure()` contains
    `df_all <- df_config # placeholder merge` — function appears
    complete but produces wrong results due to missing covariate loading
    (#74)

4.  WIS computation in
    [`calc_model_likelihood()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_likelihood.md)
    was missing the 0.5 MAE coefficient per Bracher et al. 2021 — looked
    correct at a glance but was mathematically wrong (v0.22.0 fix)

5.  Cumulative likelihood had a per-timepoint floor of -1e9 that, after
    T-normalization, produced -115 billion LL — a reasonable-looking
    constant became catastrophic after a scaling change (v0.21.x fix)

6.  ~1,500 lines of orphaned code found across 8+ utility systems that
    were created but never wired into
    [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md):
    convergence (#72), results schema (#73), batch planning (#75), BFRS
    posterior (#76), model loss (#77), adaptive weights (#78)

7.  `get_ENSO_forecast_from_json()` fallback created but only wired in
    for DMI, not ENSO — asymmetric implementation of the same pattern
    (#80)

8.  Agent deleted `calc_model_convergence.R` (6 functions) as “orphaned”
    but 2 of the 6 (`calc_model_agreement_index`, `calc_model_cvw`) had
    active callers in `run_MOSAIC.R` — agent grep’d for the file’s main
    function but not all functions inside it (v0.22.12 fix)

9.  Rewriting
    [`calc_model_ensemble()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md)
    and
    [`plot_model_ensemble()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_ensemble.md)
    silently dropped the best-model prediction plot — the old workflow
    generated it via the old ensemble function, and the rewrite replaced
    that with the posterior ensemble plot without preserving the
    single-best-model plot (v0.22.15 fix)

10. [`plot_model_likelihood()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_likelihood.md)
    call was silently removed from
    [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
    during a refactor — the function still existed but the call site was
    dropped, so the likelihood curve diagnostic plot stopped being
    generated without any error or warning (v0.22.17 fix)

11. v0.14.22 fixed the `expected_cases` → `reported_cases` field rename
    in three sibling plotting functions (`plot_model_ppc.R`,
    `plot_model_fit_stochastic.R`, `plot_model_fit_stochastic_param.R`)
    but missed the fourth (`plot_model_fit.R`). The bug was latent
    because `plot_model_fit()` was temporarily unused by
    [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
    at the time. Nine releases later (v0.22.15) the function was
    re-wired into the best-model block and the stale `expected_cases`
    reference started rendering inflated predicted lines against
    observed surveillance points — best-model plots looked
    catastrophically wrong while the ensemble plot (which used
    `reported_cases`) looked fine. Lesson: when renaming a field or
    changing a semantic convention across similar functions, grep
    exhaustively (e.g. `grep -l "old_name" R/`) and list every file in
    the commit message. Do not skip temporarily-unused functions — the
    bug becomes real the moment they are re-wired. Prefer consolidating
    into a single shared code path over maintaining N parallel functions
    that must be updated in lockstep; this fix retired
    `plot_model_fit()` and routes best/medioid plots through the
    parameterized
    [`plot_model_ensemble()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_ensemble.md)
    for exactly this reason (v0.29.2 fix)

12. The laser-cholera v0.13.0 upgrade (v0.32.0) flipped the
    deaths-likelihood scale: observed surveillance `reported_deaths` was
    previously compared to simulated raw `disease_deaths`, inflating
    simulated deaths by ~1/rho_deaths ≈ 2.4× and forcing calibration to
    absorb the missing factor in `mu_j_baseline`. The fix flips MOSAIC
    to extract `model$results$reported_deaths` everywhere. Multiple
    latent bugs were exposed only by the deep review pass: (a) the Dask
    path’s `.mosaic_inject_likelihood_settings()` overwrote
    [`get_location_config()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_location_config.md)‘s
    filtered epidemic_peaks with the full unfiltered SSA dataset
    (hard-asserts on v0.13); (b) the parity-test fixture didn’t supply
    `loc_idx` to Python’s epidemic_peaks DataFrame, so Python silently
    dropped every peak row and the test false-passed on a path Python
    wasn’t actually scoring; (c) `delta_reporting_deaths` was
    mislabelled as “symptom-onset-to-report” when v0.13’s engine
    implements it as “death-event-to-report”; (d) per-country
    `mu_j_baseline` priors in `make_priors_default.R` encode a pre-v0.13
    derivation that omits the rho_deaths factor — 39 of 40 countries’
    defaults still implicitly absorb 1/rho_deaths. Lessons: when an
    engine upgrade changes the SEMANTICS of a field (not just its name),
    audit every consumer including (i) post-calibration ensemble/plot
    paths, (ii) Dask/remote worker paths separately from local PSOCK
    paths (they often duplicate config injection), (iii) test fixtures
    that mock the result schema (they can silently false-pass when the
    consumer’s contract is loosened), (iv) prior derivations baked from
    observation identities, and (v) prior label descriptions (the
    literature anchor must match the engine implementation, not the
    analyst’s intuition). Run the change through an independent
    reviewer-pass before shipping (v0.32.0 fix)

13. The renamed-control-parameter deprecation shim in
    `.mosaic_validate_and_merge_control()` was dead code for ~15 minor
    versions. It guarded the old→new copy with
    `is.null(def$calibration$<new>)`, but the function deep-merges the
    user’s `control` into
    [`mosaic_control_defaults()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/mosaic_control_defaults.md)
    FIRST, and the defaults always populate the canonical
    `*_adaptive`/`*_total`/`ESS_method` keys with non-NULL values. So
    the guard never fired: every legacy name (`batch_size`,
    `min_batches`, `max_batches`, `target_r2`, `max_simulations`,
    `max_predictive_batch`, plus the case-mismatched `ess_method`) was
    silently dropped and the run reverted to defaults, while the
    intended deprecation
    [`warning()`](https://rdrr.io/r/base/warning.html) never appeared.
    This was invisible because there is no “unknown control key”
    validator, and the published Running-MOSAIC vignette used the old
    names — so a user following the docs would set
    `max_simulations=1e6`/`target_r2=0.95` and silently get the defaults
    (100,000/0.9). Fix (v0.37.1): detect legacy names in the user’s
    ORIGINAL `control` (never the merged `def`), and treat the canonical
    key as “user-set” only when it differs from the pristine default —
    then honour-and-warn, or (if both genuinely set) keep canonical and
    warn it was ignored. Lessons: (i) a deprecation/back-compat shim
    must key off the raw user input, not a structure already merged with
    defaults; (ii) any guard of the form
    `is.null(<thing the defaults always fill>)` is dead on arrival —
    test the shim with the actual `defaults()+override` usage pattern,
    not a bare partial list; (iii) absent an “unknown key” validator,
    silently-ignored config is undetectable at runtime, so add
    regression tests that assert the value actually takes effect
    (v0.37.1 fix)
