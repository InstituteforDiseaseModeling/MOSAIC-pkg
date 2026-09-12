# Migrating the MOSAIC transmission engine to R

Plan of record for removing the **LASER transmission engine and its associated dependencies** from `MOSAIC-pkg` — the Python engine itself plus the distributed-compute layer (Dask/Coiled) that exists only to make that engine affordable — leaving a package that runs a full calibration in pure R on a laptop. Branch: `migrate-laser-to-r`. Written 2026-09-10, rescoped 2026-09-10.

All paths are on the **local laptop** unless stated otherwise.

## 1. Goal

**After this work, the simulation and calibration path is pure R.** `laser-cholera`, `laser-core`, `numba`, `llvmlite`, `dask`, `coiled` and the `inst/python/mosaic_dask_worker.py` plumbing all leave the package. `run_MOSAIC()` has no `dask_spec` branch and no per-worker Python import. A user installs the R package and runs a calibration with no Python involved.

**Explicitly NOT in scope: the environmental-suitability (ψ) model.** `est_suitability()` and the whole LSTM pipeline keep using `keras3`/TensorFlow via `reticulate`, unchanged by this work. See §5. Consequently `reticulate`, `numpy` and TensorFlow **stay**, as does the Python-environment machinery (`install_dependencies()`, `check_dependencies()`, `check_python_env()`, the `RETICULATE_PYTHON` block in `zzz.R`) — trimmed to describe a TensorFlow-only environment rather than deleted.

Python survives in one further place, and only temporarily: as a **frozen development-time oracle** for validating the engine port (§7). Its outputs are committed as test fixtures, after which `laser-cholera` is never invoked again — not by users, not by CI, not by `devtools::test()`.

**Not in scope either: changing any model equations, parameter names, or the config schema.** Both workstreams are translations and deletions.

**One sequencing constraint dominates the plan:** the Dask layer can go before the engine port, but the `laser-cholera` *dependency* cannot — it is the oracle the port is validated against, and it is the only working engine until cutover. So workstream C splits in two, either side of the port. §2.

## 2. Two workstreams

| | What | Size | Difficulty |
|---|---|---|---|
| **A** | Transmission engine `laser-cholera` → R | ~450 lines of dynamics to port; ~700–900 lines of new R | Moderate, fully de-riskable |
| **C** | Delete Dask/Coiled + the LASER-specific Python plumbing | ~330 `dask`/`coiled` references across `R/`, 2 R files deleted, 1 `inst/python/` file, ~8 test files | Tedious, low risk |

(The workstream letters are kept from the earlier three-workstream version of this plan so that issue references still line up; there is no workstream B — that was the ψ port, now out of scope.)

**C splits in two, and the split is load-bearing.** An earlier draft of this plan had a single workstream C that removed the Dask layer *and* the `laser-cholera` dependency in one pass, while C's own exit criterion still required `run_MOSAIC()` to run end-to-end on the Python engine. Those are contradictory: a fresh checkout after that phase could not run a calibration at all. So:

- **C-1 — remove Dask/Coiled only.** The `laser-cholera` dependency, `environment.yml` entry, and `check_dependencies()` entries all stay untouched. The package still runs the Python engine, just locally.
- **C-2 — remove the LASER dependency.** Runs *after* the R engine is cut over in A-4, never before.

Order: **A-0 → C-1 → A-1…A-4 → C-2 → A-5.** The replay-harness spike (A-0) goes first because it is 1–2 days and tests the single assumption the whole engine port rests on; learning it fails is much cheaper before a week of deletion than after. C-1 then shrinks the surface A has to be wired into (170 dask references in `run_MOSAIC.R` alone), and is pure subtraction with immediate benefit. **Every phase boundary leaves a package that installs and runs a calibration.**

## 3. Is local-only compute actually enough? Yes, comfortably

Measured today, default config (`inst/extdata/config_default.json`, 40 locations × 1,398 daily ticks):

| | wall time |
|---|---:|
| Python `run_model()` (`mo` env at `/software/conda/envs/mo`, direct) | 0.74 s |
| Python `run_model()` via reticulate | 0.76 s |
| `reticulate::import("laser.cholera.metapop.model")`, **per worker process** | **3.28 s** |
| Representative pure-R tick loop (22 draws + π mixing + ~30 vector ops × 1,398 ticks) | **0.144 s** |

Benchmark scripts are in the session scratchpad `/tmp/claude-1000/-home-cliffk-idm-git-MOSAIC-pkg/66d66571-6a36-48b1-9d57-328a66345282/scratchpad/` (`bench_py.py`, `bench_r.R`) — throwaway; re-create under `claude/` if we want them tracked.

At 40 patches the vectors are far too short for NumPy's per-element speed to matter. What dominates is per-tick interpreter overhead, and R's loop is *lighter* than Python's 12-phase dispatch with `perf_counter_ns` on every phase.

**That R number was an estimate and it was wrong — see §11 for the measurement.** It timed a representative arithmetic loop, omitting state allocation, results assembly, config validation, and GC. State storage turned out to be the dominant cost, so the estimate missed the thing that mattered; the engine measures 1.183 s/run against Python's 0.698 s. The surviving claim is the weak one this paragraph already stated: R is in the same ballpark, not 10x worse. The migration does not depend on R winning on speed. What the migration *does* depend on, and what is genuinely measured, is the **3.3 s per-worker import**, which on a 20-core PSOCK fan-out is 66 s of pure startup tax paid on every batch, and which goes to zero.

Arithmetic for the default budget (`calibration$max_simulations_total = 100000`, 20 cores on this laptop), using the measured *Python* per-sim time as a conservative stand-in for the not-yet-existing R engine:

- 100,000 sims × 0.75 s = 75,000 core-seconds ≈ **60 minutes of engine time on 20 cores**. **Measured, not estimated (see §11): the R engine is 1.183 s/run against Python's 0.698 s, so the same 100,000 sims are ~99 minutes on 20 cores, not the ~13 this section originally guessed.** The estimate below it was wrong by about 8x; the import-tax and RAM arguments survive, the speed argument does not.
- Likelihood evaluation and I/O plausibly dominate either way.
- **RAM should collapse.** CLAUDE.md budgets ~2 GB per worker for the current setup, almost all of it Python interpreter + NumPy + the laser-cholera model state (the calibration workers never import TensorFlow — that is only the ψ path). The R engine's live state is 28 matrices of 1,398 × 40 doubles ≈ **12 MB per sim**; peak RSS per worker will be some multiple of that once the results list, config, and R session overhead are counted, but the order of magnitude is tens of MB, not GB.

So "run it locally" is not a compromise even under the pessimistic bound — the current 2 GB/worker, 3.3 s/worker-import profile is what forced the 120-core VMs in the first place. Removing the Python engine is what makes local compute viable, and dropping Dask is therefore a consequence of the migration rather than a sacrifice.

**These numbers are a hypothesis with a gate attached.** Phase A-3a (§8) measures the real engine before we commit to retiring the VM tooling: per-sim wall time at 1/10/40 patches, sequential vs. 20-worker throughput, peak RSS per worker, and end-to-end batch time including likelihood and I/O. If peak RSS per worker lands above ~1 GB, worker count must be capped by measured memory rather than `detectCores()`, and that cap is a code change, not a note.

ψ training is unaffected by this argument either way: it is a single occasional job, not a 100,000-way fan-out, and it keeps its own TensorFlow process.

## 4. Workstream A — transmission engine

Source of truth is the editable v0.16.1 checkout at `/home/cliffk/idm/laser/laser-cholera` (matches the wheel pinned in `inst/python/environment.yml`), importable from the `mo` conda env.

### 4.1 What is actually there

`src/laser/cholera/metapop/` is 4,598 lines, but most of it is docstrings, `matplotlib` `plot()` methods, and `check_*` assertions.

| Python module | LOC | What ports | Real R work |
|---|---:|---|---|
| `susceptible.py` | 175 | non-disease deaths (binom), births (pois) | ~15 lines |
| `exposed.py` | 118 | non-disease deaths | ~10 lines |
| `infectious.py` | 326 | CFR `mu_jt` w/ epidemic flag, disease deaths, γ₁/γ₂ recovery, E→I progression w/ σ split, reported cases (ρ, χ endemic/epidemic), reported deaths (ρ_deaths) | ~70 lines |
| `recovered.py` | 136 | deaths, ε waning → S | ~15 lines |
| `vaccinated.py` | 263 | ω₁/ω₂ waning, dose-2 from V1 (φ₂), dose-1 pro-rata across `nu_jt_sources` (φ₁) | ~60 lines |
| `census.py` | 102 | N = ΣS,E,Isym,Iasym,R,V1,V2 | ~5 lines |
| `humantohuman.py` | 223 | τ/π mixing, α₁/α₂ powers, seasonal β, chain-binomial S→E | ~30 lines |
| `envtohuman.py` | 161 | Ψ = β_env·(1−θ)·W/(κ+W), chain-binomial S→E | ~20 lines |
| `environmental.py` | 226 | W decay (pois, capped), ζ₁/ζ₂ shedding (pois), θ WASH | ~20 lines |
| `derivedvalues.py` | 344 | `spatial_hazard` (last tick), `coupling` = corr matrix | ~40 lines |
| `utils.py` | 422 | `get_daily_seasonality`, `get_pi_from_lat_long` (haversine + gravity) | ~35 lines |
| `params.py` | 1,009 | dtype coercion, scalar broadcasting + validation — **no derivation** | ~120 lines; see §4.2 |
| `model.py` | 592 | frame allocation, phase order, `RInterface` trim/transpose | ~80 lines |

**~450 lines of substantive R** for the dynamics, ~700–900 including results assembly, validation, and roxygen.

Nothing exotic to reproduce:

- **No Numba, no JIT, no C** in `laser-cholera` (`grep -rn "numba|njit|prange" src/` is empty) — which also means `numba`/`llvmlite` can leave `environment.yml` with the engine.
- **22 active stochastic draw sites** — all `prng.binomial(n, p)` / `prng.poisson(λ)` on length-40 vectors. (A naïve grep returns 35; 13 of those are commented-out former calls. Verified: `grep -c` gives 35 raw, 22 after excluding comment lines.) R's `rbinom`/`rpois` vectorise over both `n` and `prob`, so these are 1:1.
- **One scipy call** — `scipy.stats.beta.cdf` in `map_suitability_to_decay` → `pbeta`.
- **One laser-core call** — `laser.core.migration.distance` (haversine) → ~8 lines.

**Zero new R dependencies for workstream A.** Base R only: `rbinom`, `rpois`, `pbeta`, `expm1`, matrix ops.

`params.py` is almost entirely coercion: `R/make_LASER_config.R` already builds and validates the full parameter set and hands the engine dense matrices (`b_jt`, `d_jt`, `nu_1_jt`, `nu_2_jt`, `psi_jt` — all 40 × 1,398 in the default config). Only `nticks = (date_stop - date_start).days + 1` and the float32 casts are genuinely derived there.

**But "coercion" is not the same as "droppable",** and an earlier draft of this plan wrote off ~1,000 of the 4,598 lines on that basis. That is only safe if the engine's sole input is `make_LASER_config()` output — and it is not, since `run_LASER()` also accepts raw lists and file paths. Scalar→matrix broadcasting, dimension/orientation checking, and non-finite rejection all live in `params.py` today and have to live somewhere afterwards. So most of the *volume* does go (dtype tables, float32 casts), but the *behaviour* moves to `R/laser_params.R` as a named deliverable with its own tests. Budget ~120 lines, not ~40. §4.2 has the checklist.

Note that `psi_jt` arrives as a **precomputed matrix in the config**, produced upstream by `est_suitability()`. That boundary is exactly why the ψ model can stay on TensorFlow while the engine moves to R (§5).

### 4.2 Interface

`R/run_LASER.R` is 94 lines and is the primary bridge. Downstream, MOSAIC consumes a narrow slice of the 28 fields `RInterface` exposes: `reported_cases` (10 sites), `reported_deaths` (9), `pi_ij` (5), `spatial_hazard` (4), `coupling` (4), `N` (3), `Isym`/`Iasym`/`E`/`S`, `dose_one_doses`/`dose_two_doses`, `beta_jt_env`/`beta_jt_human`/`delta_jt`, `incidence`. The R engine exposes all 28 (they are cheap views), but only ~18 have consumers.

`run_LASER()` is not, however, the only place the Python module is imported, and an earlier draft of this list **missed the most important site of all** — `run_MOSAIC()` imports and calls the engine directly rather than going through `run_LASER()`. The exhaustive list, re-grepped over `R/ tests/ inst/ DESCRIPTION .github/`:

| Site | What it is | Disposition |
|---|---|---|
| `R/run_MOSAIC.R:452` | **the calibration hot path** — direct `import` + `run_model()`, bypassing `run_LASER()` | **done (A-4)** — now `run_LASER(config = params_sim, seed = seed_ij, quiet = TRUE)` |
| `R/run_MOSAIC.R:1093` | `pkg_laser_cholera` in the environment snapshot | drop from provenance schema (C-2) |
| `R/run_MOSAIC.R:1421` | orchestrator/worker version reconciliation | delete with the Dask path (C-1) |
| `R/run_LASER.R:64` | the public bridge | **done (A-4)** — the file is deleted; `run_LASER()` *is* the engine and lives in `R/laser_engine.R` |
| `R/calc_Reff.R:537` | Rₜ recomputation | **done (A-4)** — and its faithfulness-gate rationale rewritten, because the R engine *is* reproducible across cold processes |
| `R/calc_model_ensemble.R:660`, `:748` | posterior ensemble + medoid reruns | **done (A-4)** — the engine call moved into `.mosaic_ensemble_sim_task()` at C-1, so A-4 swapped it in one place; `:660`'s worker preamble no longer preloads the module. `:748` was the *Dask* medoid dispatch and went with C-1 — the local medoid rerun goes through `calc_model_ensemble()` and so through the same task |
| `R/prefit_rolling_cv_psi.R:275` | version probe | drop (C-2) |
| `R/run_MOSAIC_helpers.R:1394` | `pkg_laser_cholera` in the resume compatibility check | drop with the provenance key (C-2) — **found by the A-4 sweep, not in the original inventory** |
| `R/attach_mosaic_env.R:6` | docstring listing `laser.cholera` as a MOSAIC Python dependency | update (C-2) — **found by the A-4 sweep** |
| `R/plot_model_ppc.R:183` | `inherits(model, "laser.cholera.metapop.model.Model")` class test | **done (A-4)** |
| `R/make_mosaic_cluster.R:92` | per-worker import | **done — pulled forward from C-2 into A-4.** Once nothing resolves worker-side `lc`, leaving the import would make every calibration worker pay the 3.3 s import and hold the Python heap for a module no code calls. `library(reticulate)` and the NumPy warning filter went with it; the function itself stays |
| `R/lock_python_env.R:127-141` | import-check | retarget to TensorFlow (C-2) |
| `R/check_dependencies.R:97,118,134,199` | `core_packages` + messaging | retarget to TensorFlow (C-2) |
| `R/zzz.R:134` | comment only | update (C-2) |
| `inst/python/environment.yml:19` | the pinned wheel | remove (C-2) |
| `tests/testthat/test-check-worker-versions.R` | whole file is worker version skew | delete (C-1) |
| `tests/testthat/test-run_MOSAIC_resume.R:410-527` | `pkg_laser_cholera` in resume-compat fixtures | retarget to a surviving provenance key (C-2) |
| `inst/examples/simulate_outbreak_settings.R`, `inst/examples/forecast_cv_experiment.R` | example scripts | **done (A-4)** — engine version probes and `py_to_r()` unwrapping removed |
| `.github/workflows/R-CMD-check.yaml` | installs the wheel in CI | remove (C-2) |
| `vm/HEDGEHOG.md`, `azure/*.md`, `.claude/skills/{run-mosaic,hedgehog-run,dugong-run,diagnose-fit}/SKILL.md`, `.claude/agents/swe.md`, `.claude/commands/swe.md`, `.claude/agent-memory/**` | docs, skills, agent memory | update or retire (C-2) |

**Every row must be discharged, and each named in the commit message** — this is precisely CLAUDE.md lesson #11's "grep exhaustively, list every file in the commit message". Note that `R/run_MOSAIC.R:452` existing at all means `run_LASER()` is *not* a chokepoint today; part of A-4 is making it one, so there is exactly one engine entry point afterwards.

There is **no `engine=` switch** — the whole point is that there is only one engine. During migration the R engine was reached via an internal `run_LASER_R()` and became `run_LASER()` at Phase A-4 sign-off, so `main` was never broken mid-flight. **A-4 is done:** `run_LASER()` is the R engine, `R/run_LASER.R` is deleted, and the five production call sites (`run_MOSAIC()`'s worker, `.mosaic_ensemble_sim_task()`, `calc_Reff()`, `run_rolling_cv()`, `run_fit_sandbox()`) all reach it and nothing else.

#### Input contract

```r
run_LASER(config, seed = NULL, quiet = FALSE)
```

`config` accepts a config **list** (the `make_LASER_config()` output, and in practice the only thing used) or a **path to a `.json` / `.json.gz` file**. YAML and HDF5 input are dropped: `get_parameters()` in Python never supported YAML despite `run_LASER()`'s docstring claiming it, and nothing in the package passes an HDF5 path to the engine. `seed` resolution is unchanged: explicit arg > `config$seed` > `123L`.

**Four arguments are deleted outright, not deprecated: `py_module`, `visualize`, `pdf`, `outdir`.** A grep of `R/`, `tests/` and `inst/` finds **zero callers** passing any of them — `run_LASER()`'s four live call sites (`R/run_rolling_cv.R:741`, `R/run_fit_sandbox.R` via `.laser_runner`, `inst/examples/simulate_outbreak_settings.R:308`, and the internal path in `run_MOSAIC.R`) pass only `config`, `seed`, and `quiet`. The last three exist solely to drive the Python `Analyzer`/matplotlib visualisation, which is not being ported; carrying them as accepted-and-ignored arguments would be worse than removing them, because it would silently do nothing. A deprecation cycle for arguments with no users is pure ceremony. **However: supplying a removed argument must `stop()` with a message naming it and saying what replaced it — never be silently absorbed by `...`.** Silent absorption is the failure mode CLAUDE.md lesson #13 is about. `run_laser` stays as an alias; it costs one line.

#### Return contract

```r
list(
  params  = <normalized config list>,   # post-validation, post-broadcast
  results = <list of 28 named matrices>,
  seed    = <integer scalar actually used>
)
```

`params` is required, not optional — `R/plot_model_ppc.R:188-202` reads `model$params$reported_cases`, `$reported_deaths` and `$location_name`. All three are echoes of the *input* config rather than simulation output, so `params` is just the normalized config list and costs nothing. `seed` is new and cheap, and makes a result self-describing for provenance.

`results` carries all 28 `RInterface` fields, with its trimming and orientation rules preserved exactly:

| Fields | Python slice | R result |
|---|---|---|
| `S`, `E`, `Isym`, `Iasym`, `R`, `V1`, `V2`, `new_symptomatic`, `incidence`, `incidence_env`, `incidence_human`, `Lambda`, `N`, `Psi`, `spatial_hazard`, `W` | `[1:, :].T` | `[npatches, nticks]` |
| `births`, `disease_deaths`, `non_disease_deaths`, `reported_cases`, `reported_deaths` | `[:-1, :].T` | `[npatches, nticks]` |
| `dose_one_doses`, `dose_two_doses`, `beta_jt_env`, `beta_jt_human`, `delta_jt` | `.T` | `[npatches, nticks]` |
| `pi_ij`, `coupling` | passthrough | `[npatches, npatches]` |

Storage mode is **per field, not uniform** — an earlier draft of this section said "numeric matrices" while §4.5 specified integer compartments, which is a contradiction. The rule: everything counting people or events is `integer` (the 7 compartments, `new_symptomatic`, the three `incidence*`, `births`, `disease_deaths`, `non_disease_deaths`, `reported_cases`, `reported_deaths`, `N`, `dose_one_doses`, `dose_two_doses`); everything that is a rate, hazard, or continuous quantity is `double` (`Lambda`, `Psi`, `W`, `spatial_hazard`, `beta_jt_env`, `beta_jt_human`, `delta_jt`, `pi_ij`, `coupling`). Tier B asserts storage mode per channel, so this table is testable rather than aspirational.

**Dimnames: none.** The Python return has none, no consumer reads any, and adding them would make the structural diff in A-3 fail against the oracle for no gain.

`model$log_likelihood` is **not** reproduced. It only populates when `calc_likelihood=TRUE` in params, MOSAIC never sets that, and `grep` finds no consumer — MOSAIC computes its own likelihood in `calc_model_likelihood()`.

Every existing consumer (`R/run_MOSAIC.R:493-552`, `R/calc_model_ensemble.R:676-695`, `R/run_fit_sandbox.R:99-100`, `R/calc_Reff.R:582-583`, `R/plot_model_ppc.R:189-190`, `R/run_rolling_cv.R:742-743`) should need **zero changes** beyond dropping `reticulate::py_to_r()`. If a consumer needs more than that, we broke the contract.

#### The RNG contract

The Python engine owns an isolated `np.random.Generator`; a naive R port calling `set.seed()` would mutate the caller's global `.Random.seed`, which is a real and observable API change — and inside a PSOCK worker it would make results depend on how many other things in the worker had drawn beforehand. Specify and test:

- A run's output is determined **solely** by `seed` + `config`. Not by worker identity, batch position, or PSOCK scheduling order.
- `run_LASER()` **restores the caller's `.Random.seed` on exit** (`on.exit()` save/restore around the seeded section). Calling the engine must not perturb the caller's stream.
- `RNGkind()` is set explicitly inside the engine rather than inherited, so a caller who has switched to a non-default generator gets the same answer as one who has not.
- Provenance in the returned object's attributes: `R.version.string`, `RNGkind()`, and the MOSAIC package version, so an archived result records what produced it.
- Tests: identical output for (a) the same seed called twice in one session, (b) the same seed with an unrelated `runif()` interleaved, (c) sequential vs. 20-worker PSOCK execution of the same seed set.

#### Input validation

Calling `params.py` "mostly droppable" (§4.1) is only safe if the engine's sole input is `make_LASER_config()` output. It is not — `run_LASER()` also takes raw lists and file paths, and `params.py`'s 1,009 lines are where scalar→matrix broadcasting and dtype coercion currently happen. So `R/laser_params.R` is not a thin shim; it owns:

- **Required-field presence**, and `nticks = as.integer(date_stop - date_start) + 1` consistency against every supplied matrix's time dimension.
- **Scalar broadcasting** to `[npatches]` or `[npatches, nticks]`, matching `params.py`'s rules.
- **Dimension and orientation checks** on every matrix input, with an error naming the offending field and both shapes. A silently transposed 40×1,398 input is the single most likely way to get plausible-looking wrong answers.
- **Bounds**: probabilities in [0,1], rates ≥ 0, compartment counts ≥ 0 and integral, `nu_jt_sources` naming only real compartments.
- **Non-finite rejection** — `NA`/`NaN`/`Inf` in any numeric input, since these propagate into `rbinom(n, p)` as silent `NA` output rather than an error.
- **Single-location behaviour**, where `npatches == 1` collapses R matrices to vectors unless `drop = FALSE` is used everywhere.

Tier B parity on valid inputs proves nothing about invalid ones, so this needs its own unit tests driven from the validation rules, not from the oracle.

### 4.3 New files

```
R/laser_params.R      # config list -> validated numeric params; nticks
R/laser_precompute.R  # pi_ij (haversine+gravity), beta_jt_human, beta_jt_env, delta_jt
R/laser_state.R       # allocate (nticks+1) x npatches matrices
R/laser_rng.R         # draw_binom()/draw_pois() with mode = rng | replay
R/laser_components.R  # the 9 per-tick phase functions, in canonical order
R/laser_derived.R     # spatial_hazard(), coupling()
R/laser_results.R     # RInterface equivalent: trim + transpose
R/laser_engine.R      # run_LASER_R(): assemble, loop, return
```

No `src/`, no compiled code, no new dependencies.

### 4.4 Phase order — preserve exactly

From `model.py:566-579`:

```
Susceptible -> Exposed -> Recovered -> Infectious -> Vaccinated -> Census
  -> HumanToHuman -> EnvToHuman -> Environmental -> DerivedValues
```

(`Analyzer`, `Recorder`, `Parameters` follow but are I/O and diagnostics, not dynamics — and all three are dropped, since HDF5 recording and matplotlib visualisation are not part of the R contract.)

The order is semantically load-bearing. `Infectious` reads `E[tick+1]` *after* `Exposed` has written it; `HumanToHuman` reads `N[tick]` but writes into `S[tick+1]`, which `Census` has already summed. That asymmetry is real. **Port the behaviour, not the intent** — file an issue for anything that looks wrong, do not change it here.

### 4.5 State layout and numerics

Mirror the Python layout: each compartment is a `(nticks + 1) × npatches` matrix, time in rows, patch in columns; transpose only at the end in `laser_results.R`. Do not flip to `[patch, time]` early or every off-by-one has to be re-derived. Integer compartments use `integer` storage to inherit `np.int32`'s rounding discipline; `W`, `Lambda`, `Psi`, `spatial_hazard`, β/δ are `double`.

**float32.** The Python engine stores rates as `float32`; R has no float32 scalar type. Decision: **compute and store in double, and validate the difference rather than assume it away.** float32 carries ~7 significant digits; these rates are O(1e-5)–O(1e-1) feeding a binomial draw, so the difference in `p` is ~1e-7 relative and only changes an individual draw when it lands on an acceptance boundary. Tier B (§7) pins this: integer channels bit-identical, float channels 1e-6 relative. A channel that fails at 1e-6 but passes at 1e-4 is a signal to investigate that expression (usually catastrophic cancellation), not to loosen the tolerance globally.

Two rounding sites need care because a 1-ULP difference there is *not* harmless:

- NumPy's `np.round` and R's `round` both do round-half-to-even, so they agree — but `as.integer()` **truncates**. Every `np.round(x).astype(int32)` must become `as.integer(round(x))`, never `as.integer(x)`. ~10 sites: the σ split and reported-cases χ adjustment in `infectious.py`; dose allocation in `vaccinated.py`; `local = round((1-τ)·S)` in `humantohuman.py` / `envtohuman.py`.
- The χ adjustment itself: `np.round(binomial(...) / chi_eff)`.

### 4.6 Two stale references found while surveying — fix here (**done at A-4, and there were four**)

Pre-existing bugs against the pinned v0.16.1, exactly the class CLAUDE.md lessons #11/#12 warn about:

- **`V1sus` / `V2sus` no longer exist.** v0.16.1 collapsed `V1imm`/`V1sus`/`V1inf` into a single `V1` (φ₁ applied at dose time rather than tracked as a sub-compartment). `tests/testthat/test-lasik_calculations.R:462-463` still reads `model$results$V1sus` / `V2sus`.
- **`expected_cases` no longer exists** in `RInterface`. Referenced at `R/calc_model_ensemble.R:41`, `R/plot_model_trajectories.R:80`, `inst/python/mosaic_dask_worker.py:131` (the last of which is deleted by workstream C anyway).

Port target is v0.16.1 semantics; both get resolved, with every call site listed in the commit message.

**Discharged at A-4, and the survey had undercounted.** `V1sus`/`V2sus`: the spatial-hazard check in `test-lasik_calculations.R` now passes zero matrices, which is not a shortcut — v0.16.1 applies φ₁ at dose time, so an unprotected vaccinee never leaves `S` and there is no waned-vaccinated susceptible sub-compartment left to count; `derivedvalues.py` reads `S` alone. `expected_cases`: removed from `calc_model_ensemble()`'s default trajectory channels and from `plot_model_trajectories()`'s panel spec (`R/calc_model_ensemble.R:41`, `R/plot_model_trajectories.R:80`); the Dask worker copy went with C-1.

Two more surfaced the moment `test-lasik_calculations.R` actually ran, and neither was findable by grepping for a renamed field:

- **The `pi_ij` comparison applied a `t()` that made it wrong.** The transposed comparison disagrees with `calc_diffusion_matrix_pi()` by up to 0.29; the untransposed one agrees to 4e-16. The code even carried a comment doubting itself (*"appears pi_ij in the model may have been transposed although it did not need to be"*). `pi_ij` is `[origin, destination]` in both the engine's return and the helper's output.
- **The population check's 1% tolerance was never achievable by either engine.** The measured max proportional deviation against UN WPP is 2.23% for the R engine and 2.24% for the pinned Python oracle on the same config — engine demography (annual rates applied through per-tick stochastic draws vs. the UN population series), not a port artefact. Tolerance documented and set to 3%.

The lesson is the one this plan keeps relearning: **a test that does not run asserts nothing, and its assertions rot.** This file had been inert on two independent counts (slow-tier gate plus a cwd-relative config path), and three of its thirteen assertions were wrong by the time anyone looked.

## 5. Out of scope — the environmental-suitability (ψ) model stays on keras3

ψ (`psi_jt`) is produced by a **3-layer stacked LSTM with hierarchical FiLM conditioning**, ~4,000 lines of R across `est_suitability.R` (1,368), `build_suitability_sequences.R` (446), `run_rolling_cv_suitability.R` (404), `ensemble_suitability.R` (383), `prefit_rolling_cv_psi.R` (313), `rolling_cv_suitability.R` (300), `loss_suitability.R` (246), `calibrate_psi_predictions.R` (236), `lstm_film_suitability.R` (214), `feature_sets.R` (68) — all calling `keras3`, a thin R wrapper over Python TensorFlow. **None of it changes in this migration.**

The clean separation that makes this safe:

1. **ψ is not on the simulation path.** It enters the engine as a precomputed `psi_jt` matrix in the config (40 × 1,398 in the default). The suitability model is upstream data preparation, run occasionally, not 100,000 times.
2. **It is a separate process.** ψ training/prediction runs in its own single R session with its own TensorFlow import; the calibration workers never touch it. Removing Dask therefore does not touch the ψ pipeline, and porting the engine does not either.

Practical consequences to respect throughout workstream C:

- `reticulate` **stays** in `DESCRIPTION` Imports. `keras3` imports it, and `R/lstm_film_suitability.R:35,49` calls `reticulate::import("tensorflow")` / `("numpy")` directly.
- `tensorflow` and `keras3` stay in `DESCRIPTION` Suggests.
- The Python environment **stays**, slimmed. `inst/python/environment.yml` keeps `python`, `pip`, `numpy` and the pinned `tensorflow`; it loses `laser-core`, the `laser-cholera` wheel, `numba`, `llvmlite`, `pyarrow`, `dask[distributed]` and `coiled`.
- The env-management functions **stay**: `install_dependencies()`, `check_dependencies()`, `check_python_env()`, `get_python_paths()`, `attach_mosaic_env()`, `detach_mosaic_env()`, `use_mosaic_env()`, `lock_python_env()`, `remove_python_env()`, and the `RETICULATE_PYTHON` block in `R/zzz.R`. They are edited, not deleted: `check_dependencies.R:97`'s `core_packages <- c("laser.cholera", "laser.core", "numpy", "h5py", "pyarrow")` becomes `c("numpy", "tensorflow")`, its laser-specific messaging (`:117-141`, `:199-204`, `:273-275`) goes, and `lock_python_env()`'s laser import-check (`:127-141`) is replaced by a TensorFlow check.
- The `est-suitability` and `forecast-cv` skills are **unaffected and unblocked** — `forecast-cv`'s per-cutoff ψ re-fit keeps working exactly as today, so leakage-strict rolling-origin CV is preserved with no modelling decision required.

A future pure-R ψ (R `torch`, or an `mgcv`/`ranger` alternative) remains an option, but it is a separate piece of work with an open modelling question attached, and nothing in this plan depends on it.

## 6. Workstream C — excise Dask/Coiled, then the LASER plumbing

Pure deletion and trimming. No design decisions. Split across two phases so that the package is runnable at every boundary (§2):

- **C-1 (before the port): Dask/Coiled only.** `laser-cholera` stays installed, pinned, and checked.
- **C-2 (after A-4 cutover): the LASER dependency.** `environment.yml`, `check_dependencies()`, `lock_python_env()`, CI, provenance keys, docs.

### 6.1 C-1 — Dask/Coiled

**Delete outright (1 R file):** `check_coiled.R` (34 dask/coiled references).

**`make_mosaic_cluster.R` stays in C-1** — and an earlier draft of this plan was wrong to list it for deletion. Despite its Dask-era framing, it builds the **local** PSOCK cluster that the surviving backend runs on, and `run_MOSAIC()` calls it at the heart of the non-Dask branch. Its per-worker `laser.cholera` import is what goes, and that happens at A-4/C-2 with the engine, not here. Deleting it in C-1 would have broken every local run — exactly the "leaves the package unrunnable" failure the C-1/C-2 split exists to prevent.

**Delete `inst/python/mosaic_dask_worker.py`** (727 lines).

**Delete tests** that exist only to check the Dask path: `test-dask-local-separation.R` (41 lines), `test-dask-psock-orchestrator.R` (56), `test-dask-worker-count.R` (54), `test-dask_worker_score_window_parity.R` (218), `test-spatial_arrays_dask_psock_parity.R` (72), `test-check-worker-versions.R` (worker version skew), `test-epidemic_peaks_dask_inject.R` (32). Drop the dask entries from `Config/testthat/start-first` in `DESCRIPTION`.

**Harvest before deleting — do not delete wholesale.** Three files are *mostly* Dask scaffolding wrapped around assertions that are worth keeping, and deleting them outright throws away real coverage:

- `test-dask_worker_schema_parity.R` (586 lines) encodes the **result-shape contract** — channel names, orientation, dtypes. That contract survives the migration (§4.2) and is exactly what the R engine must satisfy. Rewrite it as a pure-R assertion against `run_LASER()`'s return, and it becomes the return-contract test the plan needs anyway.
- `test-calc_model_likelihood_python_parity.R` (312 lines) tests the R likelihood against a Python *re-implementation* of it inside `laser-cholera`. Once the Python side is gone the comparison has no counterparty — but tests #1–#3 (matrix orientation, 1-based vs. 0-based observation index selection, shape-term `N_obs/N_component` scaling) are properties of the R function **alone** and should be reframed as pure-R invariant tests with frozen expected values. Tests #6–#7 pin known R-vs-Python divergences and do become meaningless; those go.
- `test-burn_in_scoring_parity.R` (294) and `test-tier2_parity.R` (105): audit the same way — keep whatever asserts a property of the local path, drop whatever only asserts "Dask agrees with local".

The rule: **delete the Dask harness, keep the assertion.** A test that only checked path-A-equals-path-B is vacuous once path B is gone; a test that checked path A behaves correctly is not.

**Keep** `setup-python.R` and `helper-skips.R` as they are in C-1 (they still gate on `laser-cholera`); they narrow to TensorFlow in C-2.

### 6.2 C-2 — the LASER dependency

Runs only after A-4. Slim `inst/python/environment.yml` to the TensorFlow-only set in §5; narrow `check_dependencies()` (`:97`'s `core_packages <- c("laser.cholera", "laser.core", "numpy", "h5py", "pyarrow")` → `c("numpy", "tensorflow")`, plus its laser-specific messaging at `:117-141`, `:199-204`, `:273-275`); retarget `lock_python_env()`'s import check (`:127-141`) to TensorFlow; narrow `setup-python.R` / `helper-skips.R` from "is laser-cholera importable" to "is TensorFlow importable"; drop `pkg_laser_cholera` from the environment-snapshot provenance schema (`run_MOSAIC.R:1093`, and the resume-compat fixtures in `test-run_MOSAIC_resume.R:410-527` that read it); remove the wheel install from `.github/workflows/R-CMD-check.yaml`.

**Strip the `dask_spec` code path.** 330 `dask`/`coiled` references live in `R/`: `run_MOSAIC.R` (170), `run_MOSAIC_helpers.R` (81), `check_coiled.R` (34), `presets.R` (31), `calc_model_ensemble.R` (12), `run_rolling_cv.R` (4), and singletons in `zzz.R`, `sample_parameters.R`, `run_MOSAIC_infrastructure.R`, `get_location_config.R`, `data_epidemic_peaks.R`, `check_dependencies.R`. Removing the `dask_spec` argument collapses `use_dask` branching throughout `run_MOSAIC()` — this is the single largest simplification in the whole migration and should visibly shrink the main workflow. Note that `run_MOSAIC_helpers.R`'s worker/orchestrator version-reconciliation helpers (`:1607`, `:3185-3231`) exist only to catch laser-cholera version skew between orchestrator and Dask workers, and go with the path.

**`DESCRIPTION` dependency changes are smaller than you might expect.** `reticulate` stays (§5). `arrow` stays — 13+ R files use `arrow::` for parquet I/O independently of the Dask worker; the earlier draft of this plan was wrong about that. **`hdf5r` stays, and is out of scope.** It backs `read_hdf5_to_list()` / `write_list_to_hdf5()`, and the only caller is `make_LASER_config.R:1025` writing a config to `.h5` — an *output* format for configs, not an engine input path (§4.2 drops HDF5 as an engine input, which is a different thing). `read_hdf5_to_list()` has no caller at all and is dead weight, but removing an exported function and an optional serialization format is unrelated cleanup; file it separately rather than smuggling it in here.

### 6.3 The public API surface being removed

`NAMESPACE` is `exportPattern("^[[:alpha:]]+")` — *everything* is exported, so all of the following are technically public API:

| Removed | Kind |
|---|---|
| `check_coiled_workspace()` | exported function |
| `make_mosaic_cluster()` | exported function |
| `mosaic_dask_presets()` | exported function |
| `run_MOSAIC(dask_spec=)` | argument |
| `run_rolling_cv(dask_spec=)` | argument |
| `run_LASER(py_module=, visualize=, pdf=, outdir=)` | arguments |

An earlier draft covered only the first two, and proposed a `.Deprecated()` stub cycle for them. **Delete them all cold instead** — but loudly. The user base is this team, every one of these has zero callers outside the package (verified by grep), and a deprecation cycle on a branch that is already making the engine non-bit-reproducible is ceremony that buys nothing. What actually matters is the failure mode: a removed argument must **`stop()` with a message naming it**, never be silently swallowed. Keep a one-line `stop()` shim for each removed function and argument for one minor version, then drop the shims. That is the opposite of `.Deprecated()` — it is louder, cheaper, and it is the specific lesson of CLAUDE.md #13, where a back-compat shim silently reverted user settings to defaults for fifteen versions.

`check_dependencies()` and `install_dependencies()` survive but change meaning in C-2 (they validate a TensorFlow environment, not a LASER one), so update their docs, CLAUDE.md's Quick Reference, the `run-mosaic` skill, and the README in the same PR.

**Skills and VM tooling.** The `hedgehog-run` and `dugong-run` skills exist to launch calibrations on remote VMs and are largely about Python-env plumbing (the libexpat/GLIBCXX `LD_PRELOAD` wrappers). If calibration compute is local-only they should be retired or reduced to a generic "run this on a big box" note — bearing in mind ψ training may still occasionally want a large machine.

## 7. Parity strategy — how we prove the engine port is correct

"The distributions look similar" is weak and will not catch an off-by-one in a tick index. Three tiers, sharpest first.

**Tier A — deterministic precomputation, exact.** `pi_ij`, `beta_jt_human`, `beta_jt_env`, `delta_jt` are pure functions of the config with no RNG. They must match Python to float32 representation error (~1e-6 relative). This alone validates the gravity model, the two-harmonic seasonality, the ψ-normalisation (`beta_j0_env * (1 + (psi - psi_bar)/psi_bar)`), and the beta-CDF decay map.

**Tier B — recorded-draw replay, exact, tick-by-tick.** The centrepiece; it removes the PRNG problem entirely.

1. A Python harness replicates the body of `run_model()` but wraps `model.prng` in a **recording shim** that delegates to the real `np.random.Generator` and logs every call. No change to `laser-cholera` is needed — swap the attribute after `Model()` construction, before `model.run()`. (`laser-cholera` is read-only per CLAUDE.md; the harness lives in `claude/`.)
2. Dump the draw log plus the full 28-field `RInterface` output.
3. Run the R engine with `rng = "replay"`, where `draw_binom()`/`draw_pois()` pop the next recorded result instead of drawing, **asserting that the requested call matches the record**.

**Record schema.** A bare `(call_index, kind, n, p, result)` tuple makes a failure say only "call 41,207 disagreed", which is close to useless at 1,398 ticks × 22 sites. Record instead:

```
(call_index, tick, phase, site_id, kind, n, param, result)
```

where `phase` is the component name and `site_id` is a stable label for the draw site (e.g. `infectious/sigma_split`, `vaccinated/dose1_prorata`). Then a failure reads "tick 412, `vaccinated/dose1_prorata`, expected n=[...]" and localises immediately. `site_id` also lets the R engine assert it is at the draw site it thinks it is at, which catches a mis-ordered *phase* rather than just a mis-ordered *call* — the more likely error and the harder one to see.

**A 10-tick run records calls, not sites.** Lagged and conditional branches (the reporting-delay paths, the epidemic-threshold flag, dose allocation when `nu_jt` is zero) may not fire at all early in a run. So the harness must report **site coverage** — which of the 22 sites were exercised — and A-2's exit requires all 22 covered across the fixture set, not merely a green short run. A site with zero coverage is untested code wearing a passing test.

**Tolerances.** Integer channels: bit-identical, no tolerance. Float channels: combined absolute *and* relative, `abs(a-b) <= atol + rtol*abs(b)` with `rtol = 1e-6`, `atol = 1e-9`. Pure relative tolerance is wrong near zero, and `Lambda`/`Psi`/`W` all legitimately hit exactly zero in low-transmission patches, where a relative comparison against 0 either divides by zero or trivially passes. Draw arguments: `n` compared **exactly** (it is an integer count; any difference is a real bug, not float noise), `p`/`λ` with the combined tolerance.

**Exhaustion in both directions.** Assert at the end of a replay run that the record is fully consumed *and* that R never asked for a draw past the end. A port that skips a draw site passes a naive replay test right up to the point the offsets happen to realign.

**Per-tick invariants, checked independently of the oracle.** Compartments ≥ 0 and integral; `N` equals the sum of its seven compartments; `Lambda`/`Psi` finite and ≥ 0; every binomial `p` in [0,1] and every Poisson `λ` ≥ 0 and finite. These catch a class of bug the oracle cannot — one where R and Python agree because both are wrong — and they are the tests that survive after Python is gone.

Then every integer channel must be bit-identical and every float channel must match to the tolerance above. A mismatch localises to a tick, phase, and site. This simultaneously verifies every equation, the phase order, the number and order of PRNG calls per tick, and the trim/transpose contract. **Green Tier B, with all 22 sites covered, is the definition of "the port is correct."**

**This is where Python exits the calibration path.** The draw logs and reference outputs for a small set of configs (default 40-location; single-location; high-vaccination; epidemic-threshold-crossing) are generated **once**, committed under `tests/testthat/fixtures/`, and from then on the replay tests run in pure R against frozen fixtures. CI never installs `laser-cholera` again. (CI still needs TensorFlow for the ψ tests, or keeps skipping them as it does today — unchanged either way.) Keep the fixture-generation script in `claude/` with a header noting it needs the `mo` env — it is a historical artefact, not a build step.

**Size the fixtures before committing them.** A full 1,398-tick × 40-patch draw log is ~22 sites × 1,398 ticks = ~31,000 records, each holding a 40-vector of `n` and of `p` plus a 40-vector result — order 10 MB uncompressed per config, ×4 configs, plus the 28-channel reference output. That is plausibly 50–100 MB of binary fixtures in a git repo forever, which is too much. Measure it at A-0, then: keep **one** full-length default-config fixture (the regression anchor), and make the other three **short and targeted** — 60–120 ticks each, with the config arranged so the branch of interest fires early. Store as compressed `.rds`/`.parquet`, not text. If the full default fixture alone exceeds ~20 MB, truncate it too and rely on Tier C for long-run behaviour.

**Provenance must be exact.** Record the oracle's **git commit SHA** (`06938c1` for the v0.16.1 bump) and the **SHA-256 of the installed wheel**, not just the tag `v0.16.1`. A tag can move and a local editable checkout can drift from the wheel `environment.yml` pins — and the entire correctness argument rests on these fixtures, so "which build produced this" must be answerable in five years. Write it into a `fixtures/ORACLE.md` alongside the data.

**Tier C — free-running distributional parity.** After Tier B is green, run both engines over 200 seeds on those same configs and compare per-patch envelopes of `reported_cases`/`reported_deaths` and — most importantly — the distribution of `calc_model_likelihood()` values, since that is the only thing calibration consumes.

**Prespecify the margins, and note what a KS test does not prove.** Failing to reject a two-sample KS test is not evidence of equivalence — with 200 seeds it is a weak instrument, and "p > 0.05 everywhere" is compatible with a real shift. So KS (per patch, Bonferroni-corrected) is used only as a *screen* for gross shape differences, and the actual pass criterion is prespecified equivalence bands on the quantities calibration consumes:

| Quantity | Band |
|---|---|
| mean `calc_model_likelihood()` | within ±1% of the Python mean, and within ±0.25 σ of the 200-seed spread |
| LL 5th/50th/95th percentiles | each within ±0.25 σ |
| total cases, total deaths (summed over patches and time) | ratio in [0.98, 1.02] |
| per-patch peak magnitude | median ratio in [0.95, 1.05] |
| per-patch peak timing | median absolute difference ≤ 3 days |

**Calibrate those bands against the engine's own noise first.** Run the *Python* engine twice with two disjoint 200-seed sets and measure how far apart the two replicates land on each quantity. That Python-vs-Python gap is the floor; any R-vs-Python band tighter than it is unachievable, and any band much looser than it is not testing anything. Adjust the table above once measured — the numbers there are priors, not results. This is cheap (400 engine runs at 0.75 s) and it is the difference between an acceptance test and a ritual.

Tier C is a regression guard; the proof is Tier B.

**What we deliberately do not attempt.** Bit-exact reproduction of *archived* runs. That would require reimplementing NumPy's `SeedSequence` + PCG64, its BTPE/BINV binomial and PTRS Poisson algorithms, *and* float32 rate arithmetic — because `-expm1(-rate)` in float32 vs. double can straddle a binomial acceptance boundary. Doing that properly means writing the engine in C++, which defeats the migration. **Stated plainly: calibration outputs produced by the Python engine cannot be replayed seed-for-seed by the R engine.** They remain valid artefacts; they are not bit-reproducible. Since there is no `engine="python"` fallback in this plan, anything that must be exactly reproduced should be re-run and re-archived before the Python path is deleted — see Phase C-0.

## 8. Phases

Order: **C-0 → A-0 → C-1 → A-1 → A-2 → A-3 → A-3a → A-4 → C-2 → A-5.** Every boundary leaves a package that installs and runs a calibration.

### Phase C-0 — archive freeze — **SKIPPED (decided 2026-09-10)**

The purpose was to re-run and re-archive anything whose exact reproduction still mattered before the Python engine's stochastic realisations became unreproducible. Confirmed with Cliff: **nothing needs bit-exact preservation.** Archived outputs remain valid artefacts; they are simply not seed-reproducible from here. No action taken, nothing deleted on this account.

### Phase A-0 — replay-harness spike — **DONE**

**Moved ahead of C-1.** This is the single assumption the whole engine port rests on, and it is two days; discovering it fails after a week of deletion would be the worst outcome in the plan.

Delivered in `claude/oracle/` (`record_prng.py`, `dump_fixture.py`, `verify_shim.py`, `truncate_config.py`, `a0_check.R`) and `R/laser_{rng,state,params,components,results,fixture,engine}.R`.

**Results — all exit criteria met, most exceeded:**

| Check | Result |
|---|---|
| Shim is transparent (same seed ± shim) | **PASS** — all 28 channels bit-identical, full pipeline |
| `S`/`N` bit-identical under replay | **PASS** — and `births`/`non_disease_deaths` too, over **60 ticks**, not the 10 required |
| Draw-site coverage | **22/22** sites fire within 60 ticks on the default config — the static count is confirmed dynamically |
| Record exhaustion, both directions | asserted and green |
| float32-vs-double gap | **measured: max 9.9e-8 relative**, against a float32 eps of 5.96e-8. §4.5's "~1e-7" was right; the 1e-6 tolerance has ~10× headroom |
| Full-length fixture size | **9.3 MB** per config (6.7 MB draws + 2.6 MB results), 30,751 PRNG calls, 1.23 M draw elements |

Three design decisions came out of the spike and are now settled:

- **The shim must be purely additive.** The first version wrapped each pipeline phase in a stamper object to supply `tick`/`phase`; that made every entry of `model.phases` report type name `PhaseStamper`, which collapsed the metrics DataFrame to duplicate columns and crashed `model.run()`'s timing summary. `tick`, `phase` and `site_id` are now all recovered from the calling frame (`sys._getframe`), so the harness touches nothing but the `prng` attribute. A shim that perturbs the model at all cannot be trusted to leave the draw sequence alone either.
- **`site_id` is the Python `file:line`**, mapped to the engine's own site labels by `.LASER_ORACLE_SITE_MAP` in `R/laser_fixture.R`. Line numbers move if the oracle is regenerated from another version — which is why the fixture records a source SHA-256: a stale map then raises an unmapped-site error instead of silently mispairing.
- **The dev-time interchange is flat little-endian binary + a JSON manifest**, not HDF5 or parquet. The oracle env has no `pyarrow`, and base R has no HDF5 or parquet reader, so `readBin()` is the only format needing nothing on either side. The committed fixture is a `.rds`, so the test suite loads it in one call with no extra dependency.

**Fixture strategy, now that the size is known.** 9.3 MB for the full default config is acceptable for a single regression anchor. Keep that one full-length, and make the other three short and targeted (60–120 ticks, ~0.4 MB each) — total ~11 MB, well inside budget, and no truncation of the anchor needed.

**An oracle bug found in passing, worth knowing but not fixing here:** a config whose `epidemic_peaks` list is *empty* crashes `params.py:584` (`dict_to_propertysetex` builds a column-less DataFrame and then reads `.iso_code`). It surfaced when truncating the default config to 10 ticks, which filters every peak out of the window. `laser-cholera` is read-only, so the workaround is to pick fixture windows containing at least one peak — but **the R engine must handle zero peaks gracefully**, since nothing stops a user configuring a short window. Add a test for it at A-2.

### Phase C-1 — excise Dask/Coiled — **DONE**

§6.1 in full.

**Measured result: 2,200+ lines of production code deleted, plus ~1,900 lines of tests.**

| File | Before | After |
|---|---:|---:|
| `R/run_MOSAIC.R` | 3,999 | 3,388 |
| `R/run_MOSAIC_helpers.R` | 3,244 | 2,140 |
| `R/presets.R` | 335 | 56 |
| `R/calc_model_ensemble.R` | 1,424 | ~1,330 |
| `R/check_coiled.R` | 181 | deleted |
| `inst/python/mosaic_dask_worker.py` | 727 | deleted |

Nine helper functions went as a family: `.mosaic_count_dask_workers`, `.mosaic_gather_with_heartbeat`, `.extract_base_config`, `.extract_sampled_params`, `.mosaic_sample_and_serialize`, `.mosaic_write_one_shard_dask`, `.mosaic_run_batch_dask`, `.mosaic_postca_dask`, `.mosaic_check_worker_versions` — 1,021 lines. `calc_model_ensemble()` lost its `precomputed_results` argument and all three branches that consumed it.

**Two things this phase got wrong on the first attempt, both worth recording:**

1. **`make_mosaic_cluster.R` was deleted, then restored.** This plan listed it for deletion on the strength of its Dask-era documentation ("`laser.cholera` Python module imported once per worker"). But it builds the **local** PSOCK cluster, and `run_MOSAIC()` calls it in the heart of the surviving branch — deleting it breaks every local run. It stays; only its per-worker Python import goes, at C-2. §6.1 is corrected. This is the exact failure the C-1/C-2 split exists to prevent, and it still nearly happened *inside* C-1.
2. **A dead guard was nearly left behind.** `.mosaic_resume_check_inputs()` had an allow-list permitting a resume across two laser-cholera versions whose on-worker Python likelihood values were verified byte-identical. With scoring now always R-side, its `engine == "python"` condition can never be true — it was dead the moment the Dask path went. Removing it also retired `.mosaic_lc_likelihood_compatible()`. Leaving it would have reproduced CLAUDE.md lesson #13 precisely: a guard that cannot fire, sitting in the codebase looking like protection.

**Tests: harvested, not just deleted.** Seven pure-Dask files were removed outright (~500 lines). Three were harvested per §6.1's "delete the harness, keep the assertion" rule:

- `test-dask_worker_schema_parity.R` (586 lines) → `test-samples_parquet_schema.R`. Its Python round-trip simulation is gone; the **ISO-suffix parquet column contract** it happened to cover survives, asserted directly against `sample_parameters()` + `convert_config_to_matrix()`.
- `test-calc_model_likelihood_python_parity.R` (312 lines) → `test-calc_model_likelihood_regression.R`. The cross-implementation comparison had no counterparty left, but the function's history is a history of scaling bugs (lessons #4, #5), so the R values are now **frozen as regression baselines** (core `-495.0074110579`, cumulative `-497.0989888952`, WIS `-500.1626110579`), plus monotonicity and orientation properties. Note the core value matches the "R -495" recorded in the retired test's own comments.
- `test-presets.R`: nine of its ten tests were Coiled VM-sizing ladders and are not reproduced; `mosaic_io_presets()` remains.

New `test-removed_dask_api.R` asserts the removed surface **errors** rather than being silently absorbed — the "unknown key validator" whose absence caused lesson #13.

**`laser-cholera` was untouched in this phase**, as designed: `environment.yml` still pins the wheel, `check_dependencies()` still checks it, CI still installs it. Only `dask[distributed]` and `coiled` left `environment.yml`.

**Exit:** all `R/*.R` parse; `run_MOSAIC()` retains exactly one execution path (local PSOCK/sequential) on the existing Python engine. Whole-repo `rg -i "dask|coiled"` returns only the allowlist: this plan file, `azure/` historical notes, and intentional "this was removed because…" comments.

### Phase A-1 — deterministic precomputation — **DONE**

`R/laser_precompute.R` (haversine, gravity `pi_ij`, two-harmonic `beta_jt_human`, ψ-normalised `beta_jt_env`, `pbeta` decay `delta_jt`), wired into `laser_params()`, plus `claude/oracle/dump_tier_a.py` and `a1_check.R`.

**Tier A green on both the default 40-location config and a single-location one**, and 59 assertions in `tests/testthat/test-laser_precompute.R` against two committed fixtures (41 KB + 2.7 KB).

| Matrix | worst relative difference vs oracle |
|---|---:|
| `delta_jt` | 1.7e-7 |
| `beta_jt_env` | 5.7e-7 |
| `beta_jt_human` | within combined tolerance (values are O(1e-5), so the absolute term governs) |
| `pi_ij` | **1.17e-6** — needed its own tolerance, see below |

**The `pi_ij` discrepancy is the one substantive finding, and it is not a bug in the port.** It failed the blanket 1e-6 tolerance by a hair, which §4.5 says to investigate rather than paper over. Doing so found that **the oracle's haversine runs in single precision**: `params.py` coerces `latitude`/`longitude` to `float32`, and `np.radians` of a `float32` array stays `float32`, so the entire `a` term (`sin²(dlat/2) + cos·cos·sin²(dlon/2)`) is evaluated in `float32` before being widened for the `arcsin`. `mobility_omega`/`mobility_gamma` are `float32` too.

The diagnosis was confirmed by mimicking each truncation in turn and watching the gap close monotonically — **1.09e-6 → 8.5e-7 (float32 inputs) → 5.2e-7 (float32 `a` term)** — which is the signature of cumulative precision loss, not of a different formula. So:

- **The R implementation is the more accurate of the two.** The oracle's distance matrix carries ~1e-6 relative error; the port's does not.
- `pi_ij` gets a **per-matrix** Tier A tolerance of 1e-5 (~10× headroom over the observed 1.17e-6), documented at the point of use with the evidence. Not a global loosening — every other matrix stays at 1e-6, and three of the four beat 1e-7.
- This is the concrete instance of the float32 decision in §4.5, and it landed exactly where that section predicted: "the difference in `p` is ~1e-7 relative". Recording it here means a future reader does not re-derive it.

**A second oracle bug found and worked around.** `truncate_config.py` now *deletes* an `epidemic_peaks` key that would be empty rather than emitting `[]`, because an empty list crashes `params.py:584`. A zero-peak window is entirely ordinary — a single-location config over a quarter with no peak hits it immediately — and a missing key is handled correctly by the engine, so dropping it is both a valid workaround and evidence of what the intended behaviour was. The R engine must handle zero peaks natively; A-2 gets a test for it.

**Structural invariants asserted independently of the oracle** (these are what survive once Python is gone): `pi_ij` row-stochastic with zero diagonal; `pi_ij == [[0]]` and not `NaN` for one patch; `delta_jt` bounded by `[1/decay_days_long, 1/decay_days_short]` and monotonically decreasing in ψ; `beta_jt_env`'s per-patch time-mean identically `beta_j0_env` (which fails if ψ̄ is taken over patches instead of time); `beta_jt_human` **1-indexed in `t`** (0-indexing would phase-shift the whole seasonal envelope by a day — invisible in a plot, material to a peak-timing likelihood); known great-circle distances; antipodal points not returning `NaN`.

### Phase A-2 — the tick loop (1.5–2 weeks)

`R/laser_state.R`, `R/laser_rng.R`, `R/laser_components.R`, `R/laser_engine.R`. One component per commit, each extending the replay test, in phase order: Susceptible → Exposed → Recovered → Infectious → Vaccinated → Census → HumanToHuman → EnvToHuman → Environmental.

`Infectious` and `Vaccinated` are the two hard ones:

- `Infectious` carries the `delta_reporting_cases`/`delta_reporting_deaths` lags, an epidemic-threshold flag computed against a *lagged* `Isym`, the `mu_jt` time-trend × epidemic-factor product, and the χ endemic/epidemic switch. Note it uses **two different N's deliberately**: the epidemic flag uses `N` recomputed inline from compartments at `tick`, while the χ probe uses `model.patches.N[idx_probe]`. Reproduce both.
- `Vaccinated` allocates first doses pro-rata across `nu_jt_sources` (default `S,E,Isym,Iasym,R`) using `[tick+1]` compartments, with a `maximum(available_pop, 1)` guard and per-compartment rounding *before* the φ₁ multiply. Rounding order is observable in the output; do not simplify it.

Implement the RNG contract from §4.2 here (isolated stream, `on.exit()` restore of the caller's `.Random.seed`, explicit `RNGkind()`), with its three reproducibility tests.

**Exit:** Tier B green on the default config for all 1,398 ticks, all 28 channels; **all 22 draw sites covered** across the fixture set; per-tick invariants green; record exhaustion asserted in both directions.

### Phase A-3 — derived values and results (3–4 days) — **done**

`R/laser_derived.R` (`spatial_hazard`, `coupling` including the constant-prevalence → NaN branch), `R/laser_results.R`.

**Exit:** Tier B green including `spatial_hazard`, `coupling`, `pi_ij`; the harvested schema test from `test-dask_worker_schema_parity.R` (§6.1) passes as the return-contract test — names, dims, per-field storage mode, absence of dimnames, and the `params`/`results`/`seed` top level.

**Met, with one correction to this plan's own bookkeeping.** §6.1 said `test-dask_worker_schema_parity.R` encoded the result-shape contract. It did not: its 586 lines are about **`samples.parquet` column names**, and C-1 already harvested them into `test-samples_parquet_schema.R`. Nothing in the repo asserted the engine's return shape, so the return-contract test is new rather than harvested — `tests/testthat/test-laser_results_contract.R`, running free (not replayed) because the contract has to hold for an ordinary run.

Three things worth recording from the port itself:

- **`DerivedValues` needs `tau_i`, `pi_ij` and `beta_jt_human`, and builds none of them.** The Python component relies on `HumanToHuman` having run first; its `check()` fails outright otherwise. `laser_params()` and `laser_precompute()` now build those three for *either* component, which is the same constraint without the ordering dependency.
- **The two outputs are sliced differently, and the difference is load-bearing.** `spatial_hazard` uses the oracle's `[1:, :]` slices, so column *t* is the state at the *end* of tick *t*; `coupling` is handed the untrimmed arrays and correlates `nticks + 1` observations, seed row included.
- **`spatial_hazard` is not bounded below by zero, in either engine.** The unconstrained two-harmonic seasonal envelope dips below zero in the low season, so `beta_jt_human` goes negative (314 of 2,400 cells on the 60-tick fixture) and the hazard follows it into 20 negative cells — the oracle produces negatives in exactly the same places. `HumanToHuman` clamps its own rate with `pmax(..., 0)`; `derivedvalues.py` does not. Reproduced rather than fixed, per §4.1's "port the behaviour, not the intent"; it is an upstream modelling question, not a port defect.

### Phase A-3a — performance gate (2 days)

**New, and it gates the VM-retirement decision.** §3's numbers are an estimate from a representative loop, not the engine. Measure the real thing before acting on them: per-sim wall time at 1/10/40 patches, sequential vs. 20-worker PSOCK throughput, peak RSS per worker under a full calibration worker payload, and end-to-end batch time including likelihood and I/O.

**Exit:** measured numbers written into §3, replacing the estimates. If peak RSS per worker exceeds ~1 GB, land the change that caps worker count by measured memory rather than `detectCores()`. If per-sim time is worse than the Python engine's 0.75 s, stop and profile before A-4 — the migration still stands on the import-tax and RAM arguments, but we need to know.

**Partly discharged early, and it tripped.** Single-run time and allocation were measured during A-2 because the first working engine was visibly slow; see §11. Per-sim time *is* worse than Python (1.183 s vs 0.698 s) even after a 2.46x fix, so this gate has fired and needs the user's decision before A-4. What A-3a still owes: the 1/10/40-patch scaling curve, sequential vs 20-worker throughput, peak RSS per worker under the real calibration worker, and end-to-end batch timing including likelihood and I/O.

### Phase A-4 — cutover and fixture freeze (1 week)

`run_LASER()` becomes the R engine and, for the first time, the **only** engine entry point — `run_MOSAIC.R:452`'s direct import goes through it. Convert every row of the §4.2 cutover table marked "(A-4)", and fix the §4.6 stale references, with an exhaustive whole-repo grep and every call site named in the commit message. Freeze the Tier B/C fixtures into `tests/testthat/fixtures/` at the sizes A-0 measured, with `fixtures/ORACLE.md` recording the oracle commit SHA and wheel SHA-256; move the generator to `claude/`. Point `test-lasik_calculations.R` at the R engine — it becomes fast enough to un-gate from `skip_if_slow()`.

**Exit:** `run_LASER()` is the single engine entry point; whole-repo `rg -i "laser.cholera|laser_cholera"` returns only C-2's targets and the fixture-oracle allowlist.

**Done.** Exit criteria met: the whole-repo sweep over `R/ tests/ man/ inst/` returns eleven remaining sites and every one is a C-2 row of the §4.2 table (`check_dependencies()`, `lock_python_env()`, `prefit_rolling_cv_psi()`'s version probe, `zzz.R`'s comment, `attach_mosaic_env()`'s docstring, the `pkg_laser_cholera` provenance key in `run_MOSAIC.R` + `run_MOSAIC_helpers.R` + the resume fixtures, and the two Python test helpers). Full suite **4,969 passing, 0 failures, 33 skips**; `R CMD check` on the built tarball at 3 WARNINGs / 3 NOTEs, byte-identical to the `claude/rcmdcheck5.log` baseline except that this run *ran the tests* (the baseline used `--no-tests`) and one fewer undocumented-object NOTE entry, `.mosaic_prepare_config_for_python` having been deleted. Log at `claude/rcmdcheck7.log`.

Three things A-4 changed that the plan did not anticipate:

1. **`run_fit_sandbox()` was already broken, and the test suite structurally could not see it.** It called its runner with `visualize`/`pdf`/`outdir` — which C-1 had made `run_LASER()` reject — but all six tests in the file stub `.laser_runner`, and the stubs accepted those arguments. This is CLAUDE.md lesson #12(iii) exactly: a fixture that mocks the engine false-passes when the real contract tightens. Fixed, plus a test asserting the sandbox only ever passes arguments that are formals of the real `run_LASER()`.
2. **`test-lasik_calculations.R` is un-gated and now actually runs** (~2 s for the full 1,398-tick 40-patch simulation plus thirteen analytic cross-checks, against ~16 s and a Python requirement before). Three of its assertions were wrong — see §4.6.
3. **One test deleted rather than converted.** *"log likelihood calculations match"* compared `calc_model_likelihood()` against the Python engine's own `model$log_likelihood`. That attribute belonged to laser-cholera's engine-side likelihood module, which was never ported — scoring has been R-only since C-1 — so the test had no second implementation left to compare against. Per lesson #14(iv) the question is whether it also asserted a property of the surviving path: it did not, and `calc_model_likelihood()` has eight dedicated test files, so nothing was re-homed.

Three Python-runtime costs were removed alongside the call sites, all on the same argument — after the cutover they initialise Python in every worker to do nothing: `make_mosaic_cluster()`'s per-worker `laser.cholera` import and `library(reticulate)`, the calibration worker's every-100th-sim `reticulate::import("gc")$collect()`, and the same `gc` collect inside `.mosaic_ensemble_sim_task()` and `calc_Reff()`'s re-sim loop.

Not done here, deliberately: the fixture freeze and generator move. The Tier B fixtures were already frozen in `tests/testthat/fixtures/` at A-2/A-3 and `ORACLE.md` already records the oracle commit SHA and wheel hash; the generator already lives in `claude/oracle/`.

### Phase C-2 — remove the LASER dependency (2–3 days)

§6.2 in full. Only now do `environment.yml`, `check_dependencies()`, `lock_python_env()`, the CI wheel install, the `pkg_laser_cholera` provenance key, and the docs/skills/agent-memory references go.

**Exit:** the full test suite runs **with no `laser-cholera` installed**; a clean `install_dependencies()` produces a TensorFlow-only env and `est_suitability()` still trains against it; whole-repo `rg -i "laser.cholera|laser_cholera"` returns only the fixture-oracle allowlist and this plan file.

### Phase A-5 — calibration acceptance (1 week, mostly waiting)

Tier C across 4 configs × 200 seeds, against the prespecified bands in §7 — **with the Python-vs-Python replicate run done first** to establish the noise floor those bands sit on. Then one full local `run_MOSAIC()` calibration compared against the last Python-engine run on R², bias ratio, ESS, and posterior marginals.

**Exit:** every Tier C band met. Posterior marginals for the calibration comparison within their prespecified bands — not "statistically indistinguishable", which is not a pass criterion. Minor version bump.

**Total: roughly 6–7 weeks**, of which workstream A is ~5. C-1 is one week and delivers value immediately.
## 9. Risks

| Risk | Handling |
|---|---|
| Silent semantic drift in one of the 22 draw sites | Tier B asserts `(tick, phase, site_id, kind, n, param)` per call, in order, with exhaustion checked both ways. A mis-ordered, extra, or missing draw fails immediately and localises. |
| A draw site never exercised by the fixtures | A-2 exit requires **site coverage = 22/22**, not merely a green run. Lagged and conditional branches do not fire in a short run. |
| float32 vs. double changing a draw | Tier B pins tolerance at 1e-6; Tier C bounds the aggregate effect. §4.5. |
| Archived runs not bit-reproducible, and no fallback engine | Phase C-0 freeze happens **before** any deletion. Accepted and documented. §7. |
| We "fix" an apparent bug during translation | Explicit rule: port the behaviour, not the intent. The `HumanToHuman`-writes-`S[tick+1]`-after-`Census` asymmetry is the canonical example. |
| Trimming the Python env breaks the ψ pipeline | `environment.yml` and `check_dependencies()` are *edited*, not deleted; `numpy` + `tensorflow` must survive the trim. Gate Phase C-1 exit on `est_suitability()` still training. §5. |
| A missed `laser.cholera` import site outside `run_LASER.R` | §4.2's table enumerates all of them across `R/ tests/ inst/ DESCRIPTION .github/ vm/ azure/ .claude/` — including `run_MOSAIC.R:452`, the calibration hot path, which an earlier draft of this plan **missed entirely**. Exit criteria use whole-repo `rg` with an explicit allowlist, not `grep R/`. CLAUDE.md lesson #11. |
| Deleting exported functions/arguments breaks user code | `exportPattern` means everything is public. Deleted cold but **loudly**: a one-line `stop()` shim per removed name for one minor version. Silence is the hazard, not removal — CLAUDE.md lesson #13. §6.3. |
| A phase boundary leaves the package unrunnable | C splits into C-1 (Dask) before the port and C-2 (LASER dependency) after cutover. §2. |
| §3's performance estimate is wrong | A-3a is a hard gate that measures the real engine before the VM tooling is retired and before A-4. |
| Dropping a Dask test throws away real coverage | §6.1: delete the harness, keep the assertion. Three files are explicitly harvested rather than deleted. |
| Engine mutates the caller's RNG stream | Explicit RNG contract in §4.2 with `on.exit()` restore and three reproducibility tests (repeat-call, interleaved-draw, sequential-vs-PSOCK). |
| `params.py`'s validation/broadcasting quietly lost | `R/laser_params.R` owns it as a named deliverable with its own rule-driven tests; Tier B parity on valid inputs proves nothing here. §4.2. |
| Tier C "passes" without testing anything | Bands prespecified in §7 *and* calibrated against a Python-vs-Python replicate noise floor. A non-rejected KS test is explicitly not the criterion. |
| Fixtures bloat the repo permanently | Size measured at A-0; one full-length anchor plus three short targeted fixtures, compressed. §7. |
| Scope creep into "while we're here, improve the model" | §1: this is translation and deletion only. The ψ model is explicitly untouched (§5). |

## 10. Decisions on the open questions

The earlier draft left four open. All four are now decided, so none of them blocks work.

1. **Deprecation cycle — no.** Delete the removed functions and arguments cold, with a loud one-line `stop()` shim per name for one minor version rather than a `.Deprecated()` warning cycle. Every one has zero callers outside the package; the user base is this team; and the real hazard is silent absorption, not removal. §6.3.
2. **`hedgehog` / `dugong` — reduce, do not retire yet.** Strip both skills to a generic "run this on a big box" note in C-1, but keep that note until **A-3a** has measured real R-engine throughput and per-worker RSS. ψ training may still want a large machine regardless, and retiring the tooling on the strength of an unvalidated estimate is the one irreversible move in the plan.
3. **`hdf5r` — keep, out of scope.** Its only live caller is `make_LASER_config.R:1025` writing a config *out* to `.h5`, which is unrelated to the engine's input path. `read_hdf5_to_list()` is genuinely dead code, but removing an exported function and a serialization format is separate cleanup; file an issue rather than smuggling it in. §6.2.
4. **This file's home — package root, for now.** Add `^migrate-laser-r\.md$` and `^plan-review\.md$` to `.Rbuildignore`, and delete both at merge. It is a transient working document being actively read at the repo root; moving it to `claude/` mid-flight costs more than the one `.Rbuildignore` line it saves.

## 11. Status and next action

| Phase | State |
|---|---|
| C-0 archive freeze | **skipped** — nothing needs bit-exact preservation (§8) |
| A-0 replay-harness spike | **done, with one correction** — shim transparent, fixture size 9.3 MB, float32 gap measured at 9.9e-8. The oracle does exercise all 22 active draw sites within 60 ticks, but A-0's *label table* for them was wrong: five `infectious.py` sites were shifted onto the wrong line, `infectious/reported_deaths` was missing and a phantom `infectious/sigma_split` (actually `np.round`, not a draw) was present. The count matched at 22 because the two errors offset, so "22/22 covered" passed while five labels were wrong. Corrected in both `.LASER_DRAW_SITES` and `.LASER_ORACLE_SITE_MAP`, guarded by `claude/oracle/verify_draw_sites.py` (re-derives the list from the oracle, diffs per site) and by pure-R membership tests. See CLAUDE.md lesson #15 |
| C-1 Dask/Coiled excision | **done and verified** — ~2,200 lines of production code and ~1,900 of tests removed; one execution path remains. Full suite 947 tests / 0 failures / 0 errors; `R CMD check` on the built tarball at 3 WARNINGs + 3 NOTEs, every one of them traced to the base commit `4c1e861` (non-ASCII string literals in `plot_Reff.R`/`run_rolling_cv.R`, six pre-existing `MOSAIC:::` self-calls, `.run_sim_worker`, a `calc_Reff.Rd` xref, and the absent `VignetteBuilder`) |
| A-1 deterministic precomputation | **done** — Tier A green on 40-location and single-location configs; `pi_ij`'s 1.17e-6 gap diagnosed to the oracle's float32 haversine |
| **A-2 the tick loop** | **done** — all seven remaining components ported. Tier B green on three fixtures: 60t x 40p, 60t x 1p, and the full 1398t x 40p anchor. **22/22 draw sites covered, 30,751 draws matched draw-for-draw, and all 19 integer result channels bit-identical over the full 1,398 ticks.** Float channels within a scale-aware 1e-5 |
| **A-3 DerivedValues + results** | **done** — the port is complete: all ten components, all 28 channels. Tier B re-run on all three fixtures with `DerivedValues` in the pipeline, and the draw counts came back identical (1,315 and 30,751), confirming the component consumes no randomness. `spatial_hazard` needs its own 1e-3 tolerance, traced to `beta_jt_human`'s float32 cancellation rather than to anything in `derivedvalues.py`; `coupling` matches to 1.8e-8 and the two engines agree cell-for-cell on which patches are `NaN`. New `test-laser_results_contract.R` asserts the return contract against an ordinary (non-replayed) run |
| **A-3a performance gate** | **pre-measured, and it contradicts this plan — see below.** The user's answer: proceed with the port regardless |
| **A-4 cutover** | **done** — `run_LASER()` is the R engine and the only engine entry point; five production call sites converted, `R/run_LASER.R` deleted, `.mosaic_prepare_config_for_python()` and `.mosaic_strip_laser_file_handler()` removed as orphans. Suite 4,969 / 0 failures; check at baseline. Uncovered a latent `run_fit_sandbox()` breakage and three wrong assertions in a test file that had never run |
| C-2, A-5 | pending |

### A-3a, measured early: the performance premise was wrong

This plan claimed the R engine would be roughly **5x faster** than Python. Measured on the same 1,398-tick 40-patch default config, same nine components, timing the run plus setup in both:

| | time per run |
|---|---|
| Python `model.run()` + `get_parameters()` + `Model()` | **0.698 s** |
| R `run_LASER_R()`, first working version | 2.912 s |
| R `run_LASER_R()`, after the state-storage fix | **1.183 s** |

Re-measured with the complete ten-component pipeline after A-3: **1.21 s** against 1.23 s for the same run without `DerivedValues` — i.e. the two end-of-run diagnostics cost nothing measurable, because they fire once rather than per tick. The ratio below is unchanged by finishing the port.

So R is **1.69x slower**, not 5x faster. The original claim came from a microbenchmark of a representative arithmetic loop, which is exactly the trap §10 warned about and then fell into: it omitted state storage, and state storage turned out to *be* the cost.

The fix was worth 2.46x on its own. Each channel was a `(nticks + 1) x npatches` matrix, and a row write `state$S[row, ] <- v` copies the whole matrix — **15 microseconds** against 0.65 for a row read. Line profiling put roughly **55%** of the run in those writes, at about 40 of them per tick. Channels are now lists of per-tick vectors, where a write is a pointer store, and the `[tick, patch]` matrices the results contract needs are assembled once at the end. Allocation fell from 4.9 GB to 987 MB per run. Note that switching the container from a list to an environment, tried first, changed nothing: `env$M[i, ] <- v` still copies, because fetching `M` bumps its reference count before the subassignment.

After that the profile is flat — the hottest single line is 6.9% — so there is no second structural win of that size. Closing the remaining 1.69x in pure R would mean many small changes for diminishing returns, or a compiled inner loop, which §10 rules out of scope.

**This is a decision for the user, not an implementation detail.** At 1.69x, a 40,000-simulation calibration costs about 13 CPU-hours against 8. Against that, the migration still removes reticulate, the ~2 GB-per-worker Python heap (R peaks near 700 MB), the 3.3 s per-worker import, the whole Python environment as a deployment dependency, and the orchestrator/worker version skew that silently invalidated the Coiled runs (issue #113). Whether 1.69x is an acceptable price for that is a judgement about priorities, and the honest position is that the plan oversold it by roughly a factor of 8.

**One inventory correction found while verifying C-1.** `calc_model_ensemble()` invoked the Python engine through a *closure* defined inside its own body, which imported `laser.cholera.metapop.model` directly rather than going through `run_LASER()` — a third engine call site, and the one the cutover table (§6) reaches via `R/calc_model_ensemble.R:660`. That closure is now the package-level `.mosaic_ensemble_sim_task()` in `R/calc_model_ensemble_task.R`, so A-4 swaps the engine there in one place instead of editing a closure that PSOCK also `clusterExport`s. The hoist was forced by a subtler problem: `precomputed_results=` was the seam four test files used to feed synthetic engine output in, and its only *production* callers were the Dask gather and the Dask medoid dispatch. Deleting the argument silently took 25 assertions with it — on weight/seed alignment, artifact masking and trajectory reduction, all properties of the *surviving* local path. Mocking the hoisted task restores every one of them and exercises strictly more production code than the argument did, because the task list, the dispatch and the gather now run for real. `tests/testthat/helper-ensemble-mock.R` holds the seam; `parity_tier2.rds` still matches bit-for-bit through it, which is the evidence that the hoist changed no arithmetic. Two dead fragments fell out of the same removal, in the shape lesson #14 describes: `.spill_traj()` (orphaned — its only caller was the removed branch) and the record-carried `param_seed` tier of the member-seed fallback, which existed only because a Dask worker held a config the master did not.

**Next action: Phase C-2 — remove the `laser-cholera` dependency.** A-4 is done: the R engine is the engine. What is left is that the package still *declares* a dependency it no longer uses — `environment.yml`'s pinned wheel, `check_dependencies()`'s `core_packages`, `lock_python_env()`'s import check, `zzz.R`'s eager import comment, `attach_mosaic_env()`'s docstring, the `pkg_laser_cholera` provenance key (written by `run_MOSAIC.R`, read by the resume check in `run_MOSAIC_helpers.R:1394` and by the fixtures in `test-run_MOSAIC_resume.R:410-527`), the two Python test helpers, and the wheel install in CI. The provenance key is the only one with a design decision in it: dropping it changes the `environment.json` schema, so old run directories must still resume. Then A-5 is the calibration acceptance run.

The performance gate (A-3a) fired and the user's answer was to proceed with the port regardless; the remaining A-3a measurements — the 1/10/40-patch scaling curve, sequential vs 20-worker throughput, peak RSS per worker, end-to-end batch time — are still owed, and the one that can force a code change is peak RSS, because a worker cap by measured memory rather than `detectCores()` would have to land before A-5.
