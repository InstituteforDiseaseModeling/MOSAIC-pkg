# Review of migrate-laser-r.md

The plan is strong and implementation-ready after a few important
corrections. The parity design is especially good.

## High-priority corrections

### 1. Phase C-1 currently removes LASER before its replacement exists

The plan says to remove `laser-cholera` from `environment.yml` and
narrow dependency checks in C-1 (lines 163–164), but C-1’s exit
criterion still runs the existing Python engine (line 217). A fresh
installation could no longer run
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md).

Split C into:

- C-1a: remove Dask/Coiled only; retain LASER dependencies and checks.
- A-0 through A-4: build and cut over to R.
- C-1b: remove LASER packages, imports, compatibility code, and
  environment checks.

A safer overall order is
`C-0 -> A-0 -> C-1a -> A-1...A-4 -> C-1b -> A-5`. Running the replay
spike before substantial deletion also tests the central assumption
immediately.

### 2. Make the input and return contracts explicit

The plan promises a list containing `results` (line 100), but at least
[`plot_model_ppc()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_ppc.md)
also consumes `model$params$reported_cases`,
`model$params$reported_deaths`, and `model$params$location_name`
(`R/plot_model_ppc.R:187-201`).

Define the complete top-level return contract, probably:

``` r

list(
  params = normalized_config,
  results = results,
  seed = seed
)
```

Also explicitly decide:

- Supported config paths: JSON, YAML, HDF5, or some subset.
- Matrix orientation and types for every field.
- Whether dimnames are required.
- Behavior of `quiet`, `visualize`, `pdf`, and `outdir` after
  Analyzer/plotting is removed.
- Whether `run_laser` remains an alias.
- Whether `py_module` is accepted-but-deprecated for one release, rather
  than immediately removed.

There is also a wording conflict between “numeric matrices” on line 100
and integer storage on line 132. Use “integer or double matrices,
matching the field contract.”

### 3. Input validation is being underestimated

Saying most of `params.py` is droppable (lines 74 and 88) is safe only
if
[`run_LASER()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/removed_api.md)
accepts exclusively output from
[`make_LASER_config()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/removed_api.md).
Today it also accepts direct lists and paths.

Add a validation checklist covering:

- Required fields and date consistency.
- Scalar broadcasting.
- Patch/time dimensions and orientation.
- Probability and rate bounds.
- Nonnegative compartment counts.
- Missing and non-finite values.
- `nu_jt_sources` names.
- Single-location behavior.
- Integer overflow assumptions.

Parity on valid inputs does not prove compatibility on invalid or
borderline inputs.

### 4. There are 22 active stochastic draw sites, not 35

The grep count of 35 includes 13 commented-out former calls. There are
22 active
`model.prng.binomial()`/[`poisson()`](https://rdrr.io/r/stats/family.html)
sites. Update lines 82, 221, and 266.

Also distinguish static sites from dynamic calls: a 10-tick run records
calls, not “all sites,” and lag or conditional branches may not execute
every site. Give replay records a `tick`, `phase`, and stable `site_id`
in addition to call index. That will make failures much easier to
diagnose than call order alone.

### 5. Define the new RNG contract

The Python engine uses an isolated generator. A naive R implementation
using [`set.seed()`](https://rdrr.io/r/base/Random.html) will mutate the
caller’s global `.Random.seed`, which is an observable API change.

Specify that each simulation:

- Is determined solely by its explicit seed.
- Is independent of PSOCK scheduling.
- Either preserves the caller’s RNG state or documents that it changes
  it.
- Records `R.version`,
  [`RNGkind()`](https://rdrr.io/r/base/Random.html), and engine/package
  version in provenance.
- Has tests for sequential versus PSOCK reproducibility.

## Test and acceptance suggestions

The replay design is excellent, but refine it as follows:

- Compare draw `n` exactly.
- Compare probabilities and rates with combined absolute and relative
  tolerances, especially near zero.
- Assert exhaustion in both directions: no unused records and no extra R
  calls.
- Check invariants every tick: nonnegative compartments, population
  accounting, finite hazards, and valid binomial probabilities.
- Estimate fixture sizes before committing four full 1,398-tick draw
  logs. Keep one full default fixture and use shorter targeted fixtures
  for vaccination and threshold branches if the repository cost is
  large.
- Record the exact oracle commit and wheel SHA256, not just `v0.16.1`; a
  tag and a local checkout are weaker provenance.

Do not delete `test-calc_model_likelihood_python_parity.R` wholesale as
proposed on line 177. Remove its live Python dependency, but preserve
its edge cases as pure-R regression tests or frozen expected-value
tests. Apply the same principle to Dask schema tests that encode useful
local result-shape behavior.

Tier C and A-5 also need prespecified equivalence margins.
“Statistically indistinguishable” (line 258) is not an actionable pass
criterion, and failure to reject a KS test is not evidence of
equivalence. Define tolerances in advance for:

- Likelihood mean and quantiles.
- Total cases and deaths.
- Peak size and timing.
- Posterior median and interval movement.
- R2, bias, and ESS.
- Allowed Monte Carlo error, ideally estimated by Python-versus-Python
  replicate calibrations.

## Scope and inventory gaps

The enumerated cutover list is not yet exhaustive. Relevant references
also exist in:

- `inst/examples/simulate_outbreak_settings.R`
- `inst/examples/forecast_cv_experiment.R`
- `tests/testthat/test-run_MOSAIC_resume.R`
- `tests/testthat/test-check-worker-versions.R`
- `tests/testthat/test-presets.R`
- `.github/workflows/R-CMD-check.yaml`
- `DESCRIPTION`

Use whole-repository `rg` exit checks, with an explicit allowlist for
historical fixtures and the oracle generator. The current Dask exit grep
covers only `R/ inst/`, and the LASER grep omits documentation,
workflows, and `DESCRIPTION`.

The public-API discussion should also cover:

- `mosaic_dask_presets()`
- `run_MOSAIC(dask_spec=...)`
- `run_rolling_cv(dask_spec=...)`
- `py_module`
- `visualize`, `pdf`, and `outdir`

Deprecating only `check_coiled()` and
[`make_mosaic_cluster()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_mosaic_cluster.md)
is incomplete.

## Performance claims

Treat the current benchmark as a promising estimate, not yet proof that
R is five times faster. The representative loop does not include full
state allocation, result assembly, validation, likelihood work,
serialization, or garbage collection.

Add a performance gate after A-2/A-3:

- Actual R engine benchmark on 1, 10, and 40 patches.
- Sequential and 20-worker throughput.
- Peak resident memory for the full calibration worker.
- End-to-end batch timing, including likelihood and I/O.
- A test that worker count is capped by memory, not merely detected
  cores.

Soften “20 workers fit in well under 4 GB” until this is measured.

## Recommendations on the open questions

1.  **Deprecation:** Keep stubs and deprecated arguments for one minor
    release, including `mosaic_dask_presets()` and `dask_spec`.
2.  **VM tooling:** Retain a stripped generic runner until actual
    R-engine throughput and memory are verified. It may remain useful
    for wide sweeps and psi training.
3.  **HDF5:** Keep it during this migration. Removing an exported format
    and helpers conflicts with “config schema unchanged.” Audit and
    remove it separately.
4.  **Plan location:** Move the plan under `claude/`; that follows the
    repository convention and is already covered by `.Rbuildignore`.

With these adjustments, the plan has the right architecture. The most
important change is separating “remove Dask” from “remove LASER
dependencies,” so every phase leaves a runnable package.
