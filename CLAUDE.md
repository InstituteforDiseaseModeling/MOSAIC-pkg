# CLAUDE.md - MOSAIC R Package

## Quick Reference Card

**Check current state (do this FIRST):**
```bash
git status                      # Any uncommitted changes?
git log --oneline -5           # Recent commits
grep "^Version:" DESCRIPTION   # Current version (you'll bump this)
Rscript -e "devtools::test()" # Baseline: all tests should pass
```

**Development cycle:**
```bash
# 1. Write production-ready code (no placeholders!)
# 2. Add/update tests
Rscript -e "devtools::test()"              # Must pass

# 3. Update docs if function signatures changed
Rscript -e "devtools::document()"

# 4. Check package builds cleanly
R CMD check .
```

**Commit workflow (ALWAYS):**
```bash
# 1. Bump version in DESCRIPTION (patch: 0.13.25→0.13.26, minor: 0.13.25→0.14.0)
# 2. Stage relevant files: git add DESCRIPTION R/my_file.R tests/...
# 3. Commit with version: git commit -m "Fix bug (v0.8.8)"
# 4. Push: git push origin main
```

**Essential commands:**
```bash
Rscript -e "devtools::test()"                                    # Run all tests
Rscript -e "testthat::test_file('tests/testthat/test-foo.R')"  # Single test
Rscript -e "devtools::document()"                                # Update docs
R CMD check .                                                     # Full package check
Rscript -e "MOSAIC::check_dependencies()"                        # Verify Python env
```

**Critical paths (DON'T BREAK THESE):**
- `run_MOSAIC()` — main calibration workflow (`R/run_MOSAIC.R`)
- `calc_model_likelihood()` — called 1000s of times (`R/calc_model_likelihood.R`)
- `sample_parameters()` — 301 parameters (`R/sample_parameters.R`)
- `calc_model_ensemble()` — posterior-weighted ensemble (`R/calc_model_ensemble.R`)

**File rules:**
- Use `./claude/` for ALL temporary/exploratory files
- Use `get_paths()` for ALL file operations (never hardcode paths)
- Never modify read-only deps: laser-cholera/, ees-cholera-mapping/, jhu_cholera_data/, MOSAIC-data/raw/ (canonical list: root CLAUDE.md "Repository Access Rules")
- Never create files in package root without necessity
- When you reference a file/output/result in a response, state its **full path AND which machine** (e.g. local laptop vs the hedgehog VM) — never just a bare filename. Work in this project spans repos and a remote VM, so an unqualified filename is ambiguous.

**Ask user first if:**
- Adding new R or Python package dependency
- Changing function signature of exported function
- Modifying `run_MOSAIC()` core loop
- Unsure about approach (multiple valid solutions)

**Agent roster & skills:** this package defines a Claude Code subagent roster + the `run-mosaic`,
`est-suitability`, `forecast-cv`, `diagnose-fit`, `context-audit`, `hedgehog-run`, and `dugong-run` skills in **`.claude/`** (tracked in git) — see **`.claude/agents/README.md`**
for the full roster, routing, colors, and aliases. The agents can read/write data and outputs in
sibling repos under `~/MOSAIC` (e.g. country repos, the `output/` tree) via the session's
`additionalDirectories` grant. Shortcuts: `/swe` (engineering), `/stat` (Bayesian/likelihood),
`/dm` (epi/priors), `/ml` (suitability), `/etl` (data ingestion), `/maint` (R-pkg maintenance + review),
`/guide` (run/config how-to → the `run-mosaic` skill; `run-guide` agent retired), `/doctor` + `/diagnose-fit` (calibration diagnosis), `/arch` +
`/context-audit` (AI-context hygiene).

---

## Canonical references — parameter meanings & model spec

**If you are unsure what a parameter means or how a model term is defined, READ THE SPEC — do not
guess from the variable name.** The authoritative definitions live *outside* this package and are
reachable via the session's additional-directory access. Pull the specific section on demand
(don't paste the whole doc into context):

- **`MOSAIC-docs/04-model-description.Rmd`** — THE model specification + full parameter glossary.
  Its **"Table of model parameters"** section (~line 1434) is an inline symbol→meaning table for
  *every* parameter (β_hum/β_env, ψ, κ, ζ₁/ζ₂, θ, ν, φ/ω, μ_jt, σ, ρ, γ₁/γ₂, δ/δ_min/δ_max, ι, ε,
  a₁/b₁/a₂/b₂, α₁/α₂, τ, π, …). **Read the `.Rmd`** (current + inline) — the rendered
  `docs/04-model-description.md` is stale and omits the table. Section index for deep dives:
  - Transmission / force of infection → "## Transmission dynamics"; seasonality → "## Seasonality"
  - Environmental suitability ψ + LSTM → "### Modeling environmental suitability"
  - Infectious dose κ → `{#infectious-dose-kappa}`; shedding ζ → `{#sec:shedding}`; recovery γ → "### Recovery rates"
  - WASH θ → "### WAter, Sanitation, and Hygiene (WASH)"; immunity/vaccination φ/ω/ν → "## Immune dynamics"
  - Spatial τ/π/coupling → "## Spatial dynamics"; observation process σ/ρ + CFR μ → "## The observation process", `{#case-fatality-rate}`
  - R₀ decomposition / Rₜ / generation time → "## The basic reproductive number", "## The effective reproductive number"
  - Initial conditions → "## Initial conditions"; transitions/vaccine terms → "## Table of stochastic transitions", "## Table of vaccination model terms"
- **`MOSAIC-docs/05-model-calibration.Rmd`** — BFRS calibration methodology (weighting, convergence).
  **`MOSAIC-docs/03-data.Rmd`** — data sources & provenance. **`06-scenarios.Rmd`** — scenarios.
- **`R/sim_params.R`** — engine-side authoritative parameter names/types (the contract the
  simulator actually consumes), with `R/sim_results.R` for the 28-channel result contract. The
  read-only `laser-cholera/src/laser/cholera/metapop/params.py` is the **historical** source these
  were ported from — use it to settle *why* the engine behaves as it does, never as a statement of
  what runs today.

---

## Package Overview

**MOSAIC** (Metapopulation Outbreak Simulation And Interventions for Cholera) is a production R package for cholera transmission simulation across Sub-Saharan Africa. The transmission engine is pure R (`run_simulation()`); Python/reticulate is used only by the keras3 environmental-suitability model. The `laser-cholera` dependency went in v0.69.0 and the `LASER` naming in v0.70.0 — `run_LASER()`/`make_LASER_config()`/`get_default_LASER_config()` now raise an error naming their replacement. The package provides functions for data processing, parameter estimation, Bayesian calibration, and visualization.

**Key capabilities:** SEIR metapopulation simulation, Bayesian Filtering with Resampling (BFRS) calibration, environmental suitability modeling, vaccination/WASH intervention analysis, spatial transmission with human mobility.

## Architecture

```
MOSAIC/                          # Root (set via set_root_directory())
├── MOSAIC-pkg/                  # THIS PACKAGE (EDITABLE)
│   ├── R/                       # Function files
│   ├── tests/testthat/          # Unit tests
│   ├── inst/extdata/            # Default parameters (JSON)
│   ├── inst/python/             # Python environment.yml (suitability only)
│   ├── data/                    # R data objects (.rda)
│   ├── model/                   # transmission model I/O and LAUNCH.R
│   ├── claude/                  # USE THIS for temporary files
│   └── DESCRIPTION              # Package metadata
├── MOSAIC-data/                 # Data repository (raw/ is READ-ONLY)
├── MOSAIC-docs/                 # Documentation website
├── laser-cholera/               # Former Python engine — READ-ONLY, historical reference only
├── ees-cholera-mapping/         # Web scraping tools (READ-ONLY)
└── jhu_cholera_data/            # JHU scraper (READ-ONLY)
```

**Function naming conventions:** `process_*()` data cleaning, `est_*()` parameter estimation, `plot_*()` visualization, `get_*()` data retrieval, `calc_*()` mathematical calculations, `check_*()` validation, `sample_*()` parameter sampling.

## The run_MOSAIC Workflow

The `run_MOSAIC()` workflow is the **centerpiece** of the package — it orchestrates the complete Bayesian calibration pipeline.

**Key files:**
- `R/run_MOSAIC.R` — main workflow and simulation worker
- `R/run_MOSAIC_helpers.R` — convergence detection, weight calculation
- `R/run_MOSAIC_infrastructure.R` — directory setup, I/O, summary generation

**BFRS calibration (2 phases):**
1. **Adaptive calibration** — batches of simulations until convergence (R² target, ESS thresholds)
2. **Predictive batches** — model-based batch sizing with ESS re-evaluation until convergence

**Post-calibration:**
- Medoid model identified, config saved to `2_calibration/best_model/config_medoid.json` (no `config_best.json` is produced)
- `calc_model_ensemble()` computes posterior-weighted predictions (weighted median/mean across parameter sets × stochastic reruns)
- R² and bias ratio computed from weighted median vs observed data
- `plot_model_ensemble()` generates prediction plots (only when `plots=TRUE`)

**Output structure:**
```
dir_output/
├── 1_inputs/          # config.json, priors.json, control.json, environment.json
├── 2_calibration/     # samples.parquet, posterior/, diagnostics/, state/
└── 3_results/         # summary.json, predictions/, figures/
```

**Thread safety (CRITICAL for parallel execution):**
```r
# Built into run_MOSAIC(), but needed for custom parallel code
MOSAIC:::.mosaic_set_blas_threads(1L)
Sys.setenv(OMP_NUM_THREADS="1", MKL_NUM_THREADS="1", OPENBLAS_NUM_THREADS="1",
           NUMEXPR_NUM_THREADS="1", TBB_NUM_THREADS="1", NUMBA_NUM_THREADS="1")
```

## Likelihood Calculation

`calc_model_likelihood()` computes a multi-component log-likelihood:

**Core:** Negative Binomial time-series likelihood for cases and deaths (weighted MoM dispersion, k_min floor).

**Shape terms (all T-normalized, weight > 0 enables):**
- Peak timing (Normal LL on time differences)
- Peak magnitude (log-Normal on peak ratios)
- Cumulative progression (NB at fractions 0.25/0.5/0.75/1.0)
- WIS (Weighted Interval Score per Bracher et al. 2021)

**Assembly:** `LL = w_cases*NB_cases + w_deaths*NB_deaths + (T/N_peaks)*w_pt*peaks + T*w_cum*cumulative + T*w_wis*WIS`

All shape term weights default to 0 (OFF). Non-finite LL returns -Inf.

## Python Integration

**Environment:** `~/.virtualenvs/r-mosaic` (managed via `install_dependencies()`)
**Core packages:** numpy, tensorflow/keras3 — the suitability model, and nothing else. Simulation and calibration are pure R and run with no Python at all; `check_dependencies()` validates a TensorFlow environment, so a broken Python env costs you `est_suitability()`, not `run_MOSAIC()`. The `laser-cholera` and `laser-core` wheels, numba, llvmlite and pyarrow were removed from `environment.yml` in v0.69.0.
**Check:** `MOSAIC::check_dependencies()`
**Troubleshoot:** `MOSAIC::remove_python_env()` then `MOSAIC::install_dependencies(force = TRUE)`

## Key Files

| File | Purpose |
|------|---------|
| `R/run_MOSAIC.R` | Main calibration workflow, simulation worker |
| `R/run_MOSAIC_helpers.R` | Convergence detection, weight calculation |
| `R/run_MOSAIC_infrastructure.R` | Directory setup, I/O, summary generation |
| `R/calc_model_likelihood.R` | Multi-component likelihood |
| `R/calc_model_ensemble.R` | Posterior-weighted ensemble predictions |
| `R/sample_parameters.R` | Sample 301 parameters from priors |
| `R/make_simulation_config.R` | Config validation (60+ parameters) |
| `R/calc_model_R2.R` | R² (corr and SSE methods) + bias ratio |
| `R/get_paths.R` | Directory path management |
| `R/sim_engine.R` | `run_simulation()` — the pure-R transmission engine, and the only engine entry point |

## Troubleshooting

**Python environment broken:** `MOSAIC::remove_python_env()` → `MOSAIC::install_dependencies(force = TRUE)` → restart R

**Parallel worker deadlock:** BLAS/Numba threading conflict. Ensure all 6 thread env vars set to "1" (built into `run_MOSAIC()`, needed for custom parallel code).

**Memory issues:** **~1.0 GB per worker** (v0.73.0: VmHWM 992 MB over 5 sequential runs; was 926 MB at v0.70.0 and 945 MB at v0.72.0 — the per-tick row environments of v0.73.0 cost ~47 MB more than the per-channel pointer lists they replaced. Flat from 1 to 19 workers as of A-3a). 16 cores needs ~16 GB. The old ~2 GB/worker figure was the *Python* engine and no longer applies; memory is no longer what caps worker count. Use `results <- vector("list", n)` not `results <- c()`.

**R CMD check errors:** "Undocumented parameters" → add `@param`. "Undefined global variable" → add to `R/globals.R`. Always run `devtools::document()` first.

---

## Development Standards

Claude Code working on MOSAIC must act as a **production systems software engineer**. Read the [Verification Requirements](#verification-requirements) and [Lessons Learned](#lessons-learned) sections before starting any work.

### Code Quality

**NEVER:** placeholder code, commented-out code, debug statements, hardcoded paths, superfluous features beyond what was requested.

**ALWAYS:** production-ready from the start, complete implementations before committing, use `get_paths()` for file operations, be concise — do only what is asked.

**If you have a good idea for improvement:** ASK FIRST before implementing it.

### Version Management

**Bump version on every commit.** Patch (bugs/docs), Minor (features), Major (breaking changes). Include version in commit message: `"Fix bug (v0.13.26)"`.

**Data object versioning:** `config_default` version in `data-raw/make_config_default.R`, `priors_default` version in `data-raw/make_priors_default.R`. Bump and rebuild `.rda`/`.json` when contents change.

### Testing

Run `devtools::test()` before AND after changes. Bug fixes need regression tests. New features need unit + integration tests. Run `R CMD check .` before committing.

### Never Break Existing Workflows

Critical paths: `run_MOSAIC()`, `calc_model_likelihood()`, `sample_parameters()`, `calc_model_ensemble()`, weight calculation, parallel execution.

### Performance

Profile before/after on hot paths. Hot paths: `run_MOSAIC()`, `calc_model_likelihood()` (called 1000s of times), `.mosaic_run_simulation_worker()`. Design for 40K sims, test with 10-100. Thread safety: BLAS threads = 1.

### Git, Docs, Dependencies

**Git:** Bump version → test → document → commit (with version) → push. Atomic commits.
**Docs:** One-line `@param`. Update `_pkgdown.yml` when adding/removing functions. Run `devtools::document()`.
**Dependencies:** NEVER add new R/Python dependencies without user approval.

### Verification Requirements

**After creating any new function:**
- Grep the codebase for all existing inline implementations that the new function should replace
- Replace every call site — creating the function without wiring it in is incomplete work
- List every file modified and every call site updated in your response
- If the function replaces inline code, confirm zero instances of the old pattern remain

**After modifying or rewriting any existing function:**
- State what the function did BEFORE and what it does AFTER
- Confirm the new methodology matches what was requested — do not substitute a different algorithm without explicit approval
- If the function had callers, verify all callers still work with the new signature/behavior
- If the old implementation is being replaced, remove it entirely — do not leave two parallel systems

**After any refactoring (calc/plot splits, function renames, etc.):**
- Confirm the old function is either removed or deprecated with a wrapper
- Verify the new functions are called from the correct places in `run_MOSAIC()`
- Check that no code paths are now gated behind flags they shouldn't be (e.g., computation gated behind `plots=TRUE`)

**Anti-placeholder rule:**
- Never commit functions with TODO/FIXME/placeholder logic
- Every code path must perform a real calculation, not a stub
- If you cannot fully implement something, say so — do not fake it

**Self-audit before committing:**
- Grep for the old pattern (inline code, old function name) and confirm zero remain
- Grep for any new function you created and confirm it has at least one caller in production code
- Check that utility functions you used actually exist in the package (don't invent function names)

### Workflow Checklist

**Before:** Read git log, check if function exists, run tests, understand workflow fit
**During:** Production-ready code, test incrementally, no hardcoding, stay focused
**Before commit:** Bump version, test, document, R CMD check, verify no debug code
**Commit:** Stage relevant files, clear message with version, push
**After:** Verify push, check CI/CD

### Lessons Learned

Record of specific errors introduced by AI coding assistants. Read these before starting work — they represent patterns to actively avoid.

1. `calc_model_R2()` created with proper corr/SSE methods but all 16 call sites kept using inline `cor()^2` — function existed for months unused (v0.22.4 fix)
2. `calc_model_ensemble()` rewritten to do single-config stochastic reruns instead of the intended weighted multi-config posterior ensemble — wrong methodology under the same name, plus the old `plot_model_fit_stochastic_param()` was kept running in parallel (v0.22.4-5 fix)
3. `est_transmission_spatial_structure()` contains `df_all <- df_config  # placeholder merge` — function appears complete but produces wrong results due to missing covariate loading (#74)
4. WIS computation in `calc_model_likelihood()` was missing the 0.5 MAE coefficient per Bracher et al. 2021 — looked correct at a glance but was mathematically wrong (v0.22.0 fix)
5. Cumulative likelihood had a per-timepoint floor of -1e9 that, after T-normalization, produced -115 billion LL — a reasonable-looking constant became catastrophic after a scaling change (v0.21.x fix)
6. ~1,500 lines of orphaned code found across 8+ utility systems that were created but never wired into `run_MOSAIC()`: convergence (#72), results schema (#73), batch planning (#75), BFRS posterior (#76), model loss (#77), adaptive weights (#78)
7. `get_ENSO_forecast_from_json()` fallback created but only wired in for DMI, not ENSO — asymmetric implementation of the same pattern (#80)
8. Agent deleted `calc_model_convergence.R` (6 functions) as "orphaned" but 2 of the 6 (`calc_model_agreement_index`, `calc_model_cvw`) had active callers in `run_MOSAIC.R` — agent grep'd for the file's main function but not all functions inside it (v0.22.12 fix)
9. Rewriting `calc_model_ensemble()` and `plot_model_ensemble()` silently dropped the best-model prediction plot — the old workflow generated it via the old ensemble function, and the rewrite replaced that with the posterior ensemble plot without preserving the single-best-model plot (v0.22.15 fix)
10. `plot_model_likelihood()` call was silently removed from `run_MOSAIC()` during a refactor — the function still existed but the call site was dropped, so the likelihood curve diagnostic plot stopped being generated without any error or warning (v0.22.17 fix)
11. v0.14.22 fixed the `expected_cases` → `reported_cases` field rename in three sibling plotting functions (`plot_model_ppc.R`, `plot_model_fit_stochastic.R`, `plot_model_fit_stochastic_param.R`) but missed the fourth (`plot_model_fit.R`). The bug was latent because `plot_model_fit()` was temporarily unused by `run_MOSAIC()` at the time. Nine releases later (v0.22.15) the function was re-wired into the best-model block and the stale `expected_cases` reference started rendering inflated predicted lines against observed surveillance points — best-model plots looked catastrophically wrong while the ensemble plot (which used `reported_cases`) looked fine. Lesson: when renaming a field or changing a semantic convention across similar functions, grep exhaustively (e.g. `grep -l "old_name" R/`) and list every file in the commit message. Do not skip temporarily-unused functions — the bug becomes real the moment they are re-wired. Prefer consolidating into a single shared code path over maintaining N parallel functions that must be updated in lockstep; this fix retired `plot_model_fit()` and routes best/medoid plots through the parameterized `plot_model_ensemble()` for exactly this reason (v0.29.2 fix)

12. The laser-cholera v0.13.0 upgrade (v0.32.0) flipped the deaths-likelihood scale: observed surveillance `reported_deaths` was previously compared to simulated raw `disease_deaths`, inflating simulated deaths by ~1/rho_deaths ≈ 2.4× and forcing calibration to absorb the missing factor in `mu_j_baseline`. The fix flips MOSAIC to extract `model$results$reported_deaths` everywhere. Multiple latent bugs were exposed only by the deep review pass: (a) the Dask path's `.mosaic_inject_likelihood_settings()` overwrote `get_location_config()`'s filtered epidemic_peaks with the full unfiltered SSA dataset (hard-asserts on v0.13); (b) the parity-test fixture didn't supply `loc_idx` to Python's epidemic_peaks DataFrame, so Python silently dropped every peak row and the test false-passed on a path Python wasn't actually scoring; (c) `delta_reporting_deaths` was mislabelled as "symptom-onset-to-report" when v0.13's engine implements it as "death-event-to-report"; (d) per-country `mu_j_baseline` priors in `make_priors_default.R` encode a pre-v0.13 derivation that omits the rho_deaths factor — 39 of 40 countries' defaults still implicitly absorb 1/rho_deaths. Lessons: when an engine upgrade changes the SEMANTICS of a field (not just its name), audit every consumer including (i) post-calibration ensemble/plot paths, (ii) Dask/remote worker paths separately from local PSOCK paths (they often duplicate config injection), (iii) test fixtures that mock the result schema (they can silently false-pass when the consumer's contract is loosened), (iv) prior derivations baked from observation identities, and (v) prior label descriptions (the literature anchor must match the engine implementation, not the analyst's intuition). Run the change through an independent reviewer-pass before shipping (v0.32.0 fix)

13. The renamed-control-parameter deprecation shim in `.mosaic_validate_and_merge_control()` was dead code for ~15 minor versions. It guarded the old→new copy with `is.null(def$calibration$<new>)`, but the function deep-merges the user's `control` into `mosaic_control_defaults()` FIRST, and the defaults always populate the canonical `*_adaptive`/`*_total`/`ESS_method` keys with non-NULL values. So the guard never fired: every legacy name (`batch_size`, `min_batches`, `max_batches`, `target_r2`, `max_simulations`, `max_predictive_batch`, plus the case-mismatched `ess_method`) was silently dropped and the run reverted to defaults, while the intended deprecation `warning()` never appeared. This was invisible because there is no "unknown control key" validator, and the published Running-MOSAIC vignette used the old names — so a user following the docs would set `max_simulations=1e6`/`target_r2=0.95` and silently get the defaults (100,000/0.9). Fix (v0.37.1): detect legacy names in the user's ORIGINAL `control` (never the merged `def`), and treat the canonical key as "user-set" only when it differs from the pristine default — then honour-and-warn, or (if both genuinely set) keep canonical and warn it was ignored. Lessons: (i) a deprecation/back-compat shim must key off the raw user input, not a structure already merged with defaults; (ii) any guard of the form `is.null(<thing the defaults always fill>)` is dead on arrival — test the shim with the actual `defaults()+override` usage pattern, not a bare partial list; (iii) absent an "unknown key" validator, silently-ignored config is undetectable at runtime, so add regression tests that assert the value actually takes effect (v0.37.1 fix)

14. The Dask/Coiled excision (v0.67.0) removed ~2,200 lines of production code and ~1,900 of tests, and two near-misses inside it are the reusable lessons. (a) The migration plan listed `make_mosaic_cluster.R` for deletion on the strength of its own documentation ("`laser.cholera` Python module imported once per worker"), which reads as pure Dask-era plumbing — but it builds the **local** PSOCK cluster and `run_MOSAIC()` calls it in the heart of the surviving branch. Deleting it broke every local run; it was caught only by reading the call site rather than the docstring. (b) `.mosaic_resume_check_inputs()` carried an allow-list letting a resume proceed across two laser-cholera versions whose on-worker Python likelihood values were verified byte-identical. Once scoring became R-only, its `engine == "python"` condition could never be true — the branch was dead the instant the Dask path went, and it would have sat there looking like protection (the same shape as lesson #13's dead `is.null()` guard). Removing it also retired the now-uncalled `.mosaic_lc_likelihood_compatible()`. Lessons: (i) when deleting a "backend-specific" file, grep its **call sites** and check which branch they sit in — a function's documentation describes what it was built for, not what currently depends on it; (ii) after removing an execution path, re-examine every guard that discriminated *between* paths, because each one is now a constant — delete it rather than leave a condition that cannot fire; (iii) removing a path orphans its helpers transitively, so re-run the orphan grep after the obvious deletions, not just before; (iv) deleting a parity test that only asserted "path A equals path B" is correct, but check first whether it also asserted a property of path A — three of ten Dask test files did, and those assertions were re-homed rather than lost (v0.67.0)

16. The LASER->simulation rename (v0.70.0) was mechanical, and the two things that went wrong were both about blast radius, not about the rename. (a) A script that re-aligned hanging-indent continuation lines after the name-length change matched *every* multi-line `function(` definition in the repo, not just renamed ones — it "fixed" 148 lines of pre-existing indentation in 43 untouched files, burying ~30 real renames in whitespace noise. Caught by `git diff --ignore-all-space` showing files whose entire diff was invisible to it; reverted by restoring the HEAD indentation for every indent-only-changed line whose governing open-paren line did not contain a renamed identifier. (b) A blanket `s/laser_/sim_/g` would have silently rewritten `laser_cholera` and `laser_core`, the names of the real external Python packages that the historical provenance comments legitimately cite — the rename had to protect those two tokens explicitly before the prefix rule ran, and every surviving `laser`/`LASER` mention then had to be triaged by hand into "renamed thing" vs "the Python package that still exists on disk". Lessons: (i) a cosmetic-cleanup script run across a repo needs its match set scoped to the change, not to the pattern — pattern breadth is not correctness; (ii) after any bulk sed, run `git diff --ignore-all-space` and treat a file that vanishes from it as a defect, not a no-op; (iii) when renaming a prefix that also prefixes an external dependency's name, protect the external tokens first and verify with a residual grep, because the failure is silent and reads as a successful rename (v0.70.0)

15. Building the R engine's PRNG draw-site registry (v0.67.0), I produced the table by pairing an ordered list of *conceptual* steps ("sym deaths, disease deaths, sym recovery, asym deaths, ...") against an ordered list of line numbers scraped from `laser-cholera`. The two lists did not correspond: `reported_deaths` (`infectious.py:210`) sits between `disease_deaths` (203) and `sym_recovery` (217), so five `infectious.py` labels were shifted onto the wrong line, one real draw site was omitted, and one phantom site (`infectious/sigma_split`) was invented — it is `np.round(sigma * progressing)`, not a PRNG call. **The total still came to 22**, because the phantom exactly offset the omission, so the A-0 exit check "22/22 draw sites covered" passed while five of the 22 labels were wrong. Lessons: (i) when a lookup table is built by zipping two ordered lists, verify each pairing individually — a matching cardinality is not verification, and here the two errors were self-concealing; (ii) prefer deriving such a table mechanically from the source of truth over transcribing it, and keep the derivation runnable (`claude/oracle/verify_draw_sites.py` re-derives the site list from the oracle and diffs per site; it was negative-tested against the buggy table before being trusted); (iii) a coverage metric over a hand-written label set measures the label set, not the code — assert set *membership* against the source, not the count; (iv) the defect was invisible to the green A-0 replay because `Susceptible`/`Census` happen to use the only two labels that were correct, so passing tests on a subset said nothing about the rest of the table (v0.67.0)

17. The v0.72.0 engine performance work produced correct fixes on top of a wrong measurement, twice, and both times the error was the *timing method* rather than the code. (a) The proposal it implemented attributed the patch-scaling half of the runtime to random variate generation at ~325 ns per variate, inferred from a linear fit plus a C++ analogue benchmark. Measured directly, R's samplers cost **54.5 ns** per variate and 67 ms per run — about 5% of runtime, not 31%. The analogue overstated the cost ~9x because it constructed a `std::binomial_distribution` per call, where `Rf_rbinom` has no setup; and the estimate was never checked against the R samplers it was standing in for. The consequence was a decision rule that subtracted a 0.40 s "irreducible sampler floor" that is really 0.04 s, which would have returned "do not write the C++" on a case worth ~10x. A second, independent consequence: it sent two of the three proposed fixes at targets worth 0.05-0.4% while the two real costs — a per-tick debug assertion defaulting to ON, and the calibration worker's per-simulation `gc()` at 292 ms — went unmentioned. (b) Having caught that, I then reproduced the identical class of error in the ablation that located the real costs: arms were timed **sequentially in one session**, so each later arm inherited the drift and every arm looked better than the last, yielding a clean-looking monotone table (−20.6%, −20.1%, −26.0%, −32.8%) and a headline 1.38x. An interleaved, per-block paired re-run against a git worktree of the baseline commit gave **1.156x** (range 1.07-1.23 over 8 blocks). Two sequential runs of the same two versions had already disagreed by 0.07 s, and one had reported an assertion as *negative* cost — the drift exceeded the effect and I quoted the number anyway before re-running it. Lessons: (i) never quote an unpaired A/B timing on a shared or hybrid-topology machine — interleave the arms, report per-block paired ratios and their spread, and treat a sequential ablation as a *ranking of where to look*, never as effect sizes; (ii) an estimate standing in for a primitive (a sampler, an allocator) must be validated against that primitive before anything is derived from it, because a single wrong constant propagates into every downstream decision rule; (iii) profile-by-function and ablation can disagree legitimately — here the assertion showed 4.35% self time but ~20% ablated, because its real cost was allocation churn charged to `<GC>` — so when they disagree, believe the ablation about *magnitude* and the profiler about *location*; (iv) check whether a per-tick assertion is on in production before optimizing anything else: rewriting `sim_check_invariants()` to drop `Reduce(`+`, lapply(...))`, a per-tick `intersect(..., names(state))`, and a redundant traversal took it from the largest single cost to 1.7% while keeping every assertion, so there was no safety-versus-speed trade to make at all (v0.72.0)

18. The v0.73.0 engine work found a 2.25x win that v0.72.0 had declared absent, and the reason it was missed is that **the mechanism had already been correctly diagnosed and was then reintroduced one level down**. The port's biggest fix replaced per-channel `(nticks+1) x npatches` matrices with per-channel lists of per-tick vectors, on the stated reasoning that "a list element write is a pointer store, so it does not copy" — and `migrate-laser-r.md` records, in the same paragraph, the correct mechanism for why the matrix version was slow: holding the channels in an environment "changed nothing: `env$M[i, ] <- v` still copies, because fetching `M` bumps its reference count before the subassignment." The replacement structure left the lists on the state environment, so `state$S[[i]] <- v` is still a subassignment into an environment-held object, the `*tmp*` fetch still bumps the refcount, and `[[<-` duplicated the whole 1,399-element pointer vector on **every** write — ~60 writes/tick x 1,398 ticks, ~615 MB of garbage per run, about 35% of runtime. The same document then concluded "the profile is flat — the hottest single line is 6.9% — so there is no second structural win of that size." Lessons: (i) when a fix is justified by a mechanism, re-apply the mechanism's test to the structure that replaces it — here one `tracemem()` on the new representation would have shown the copy immediately, and the fix was shipped without it; (ii) "the profile is flat, so there is nothing structural left" does not follow, because a per-function profiler charges allocation churn to whatever line triggered it and to `<GC>` (here 52.65% self time spread across the phase bodies plus 11.16% `<GC>`) — a flat profile is consistent with one diffuse cost, and distinguishing the two needs a structural experiment, not a finer profile; (iii) the structural experiment that worked is worth reusing: **inflate a data structure without changing the work done on it** (padding the channel lists to 4x their length moved a 1.02 s run to 2.08 s, pricing the copies at 0.354 s with no code change), and after the fix the same diagnostic is the acceptance test — the slope fell from 0.354 s to 0.025 s per extra 1,399 rows; (iv) a per-write cost that is **linear in the size of the container** is invisible at fixture scale and worst in production, so micro-benchmarks of a state write must be run at production `nticks` (1.5 microseconds at 200 rows vs 11.6 at 2,800 for the same operation); (v) a bit-identity harness is only as broad as the modes it exercises — the 100-cell golden grid runs `rng` mode only, so it certified a `vapply`-based results assembler that broke eight Tier B `replay` tests, because `vapply` enforces its prototype's storage mode where `do.call(rbind, ...)` promotes, and replayed draws come back from a fixture as doubles in channels allocated integer. The suite caught it; the dedicated gate did not. When adding a fast path, check which of the engine's *modes* the gate actually covers before trusting it (v0.73.0)
