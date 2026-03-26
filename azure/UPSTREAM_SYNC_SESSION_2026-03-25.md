# Upstream Sync Session — 2026-03-25

## Context

Syncing changes from `InstituteforDiseaseModeling/MOSAIC-pkg` main into the Dask development branch.

- **Source:** `https://github.com/InstituteforDiseaseModeling/MOSAIC-pkg` main (v0.14.63 → v0.17.33)
- **Target:** Local `validate_dask_local_sim_take2` → new `validate_dask_local_sim_take3`
- **Previous sync:** 2026-03-20 (synced through v0.14.62, documented in `UPSTREAM_SYNC_SESSION_2026-03-20.md`)

## Problem: Upstream history rewrite

### Discovery

Initial merge analysis showed **575 conflicts across 84 files** — far more than expected for ~70 new upstream commits. Investigation revealed the root cause: upstream ran `git filter-repo` to scrub `deprecated/` files from their entire history.

### Evidence

- First 46 commits (through `1c0d789`, 2025-03-23) are hash-identical between fork and upstream
- Starting at commit 47 (2025-03-27), every commit has a **different hash** despite identical messages, authors, and dates
- The tree at each commit differs by progressively more `deprecated/` files: 1 file at commit 47 → 3 at commit 100 → 64 at commit 400
- Git reflog shows `upstream/main` had a **forced-update** at `2026-03-25 09:29:03`:
  ```
  56cbde2 upstream/main@{2026-03-25 09:29:03}: fetch upstream main: forced-update
  eaceda6 upstream/main@{2026-03-20 23:57:13}: fetch upstream main: fast-forward
  ```
- The filter-repo was run between 2026-03-20 23:57 and 2026-03-25 09:29

### Impact

- ~510 commits on local `main` have "twin" commits on upstream with identical content (minus `deprecated/` files) but different hashes
- Git cannot recognize them as the same work — sees two fully diverged histories
- Normal merge produces 575 conflicts across 84 files, making traditional merge impractical

## Strategy: Reset + Cherry-pick (Strategy A)

Evaluated three approaches:

| Strategy | Description | Verdict |
|---|---|---|
| **A: Reset + fresh branch + cherry-pick** | Reset local main to upstream/main, create new branch, cherry-pick Dask commits | **Chosen** — clean history, minimal conflicts, PR-ready |
| B: Reset main, merge into existing branch | Reset main then merge into validate_dask_local_sim_take2 | Same 575 conflicts (old branch still descends from pre-rewrite history) |
| C: Fresh branch, manually copy files | Start fresh, copy Dask files over | Loses per-commit history |

Strategy A was chosen because:
1. Local `main` had no unique R code (only 5 CLAUDE.md edits, superseded by upstream)
2. Preserves individual Dask commit history
3. New branch shares ancestry with upstream — clean PR possible

## Execution

### Step 1: Reset local main

```
git checkout main
git reset --hard upstream/main   # Now at 56cbde2 (v0.17.33)
git push origin main --force
```

### Step 2: Create fresh branch

```
git checkout -b validate_dask_local_sim_take3 main
```

### Step 3: Categorize Dask-branch commits

21 commits from `validate_dask_local_sim_take2` were categorized:

| Type | Count | Description |
|---|---|---|
| **CLEAN** | 17 | Only touch Dask-specific files (`azure/`, `inst/python/`, `R/run_MOSAIC_dask.R`) |
| **SHARED** | 4 | Also touch `R/run_MOSAIC.R`, `R/run_MOSAIC_helpers.R`, or `DESCRIPTION` |

### Step 4: Cherry-pick 17 CLEAN commits

Cherry-picked in order. Three minor conflicts resolved:

1. **`812b5f6`** — `azure/Dockerfile` and `azure/run_mosaic_parallel_country.py` had conflicts from sequential commits modifying the same file; `azure/storage_mount/test_coiled_with_mount.sh` was modify/delete (file existed in old history, removed by filter-repo). Resolution: take incoming version, `git rm` the deleted file.

2. **`45e879d`** — `azure/VALIDATE_DASK_LOCAL_RUN_TAKE2.md` was modify/delete (created by earlier cherry-pick, but the creation path differed from upstream's cleaned history). Resolution: `git add` the modified file.

3. **`28e0107`** — `azure/STORAGE_MOUNTING_INVESTIGATION.md` rename to `*-2026-02-28.md` was rename/delete (original didn't exist in rewritten history). Resolution: `git add` the renamed files.

4. **`f2719fa`** — `azure/mosaic_dask_fixed_test.R` had a 1-line conflict (`n_simulations` value 50000 vs 1000 from different commit ordering). Resolution: take incoming (50000).

### Step 5: Port 4 SHARED commits

The 4 SHARED commits could not be cherry-picked because upstream completely restructured the target files:

| Commit | What it changed | Disposition |
|---|---|---|
| `c631dff` | Added `truncnorm` to DESCRIPTION | **Skipped** — upstream already has it |
| `648f76b` | `load_chunk_size` in `run_MOSAIC.R` + `run_MOSAIC_helpers.R` | **Ported manually** |
| `c1e1bdf` | `simresults` instrumentation in worker + dirs | **Ported manually** |
| `f1b2e79` | `save_simresults` flag gating | **Ported manually** |

These were combined into a single commit adapting the features to upstream's new code structure:

**Changes ported:**

1. **`load_chunk_size`**: Added `chunk_size` parameter to `.mosaic_load_and_combine_results()`. When file count exceeds `chunk_size`, uses batched `data.table::rbindlist` instead of `arrow::open_dataset() %>% collect()` which OOMs with 40K+ small parquets. Added `control$io$load_chunk_size` (default 5000) and passed through both call sites (`run_MOSAIC.R` final load, `run_MOSAIC_helpers.R` ESS check load).

2. **`save_simresults`**: Added `control$io$save_simresults` (default FALSE). When TRUE, creates `dirs$cal_simresults` (`2_calibration/simulation_results/`), passes `dir_cal_simresults` to worker, captures raw per-(sim,iter,j,t) LASER output with psi_jt into `simresults_XXXXXXX.parquet` files.

3. **Adaptations to upstream's new structure:**
   - `dir_bfrs_parameters` → `dir_cal_samples`
   - `dir_bfrs_simresults` → `dir_cal_simresults`
   - Worker uses `param_lookup` / `likelihood_settings` patterns
   - Dir tree uses `2_calibration/` prefix (was `1_bfrs/`)

## Major upstream changes included (v0.14.63 → v0.17.33)

### Architecture changes
- **NPE removed** (v0.16.1): All NPE code moved to `deprecated/npe_removed_v0.15.0/`
- **Output dir restructure** (v0.17.4): `1_bfrs/` → `1_inputs/` + `2_calibration/` + `3_results/`
- **Worker performance overhaul** (v0.14.75–v0.14.85): `param_lookup` for O(1) extraction, `validate=FALSE` in worker, Python GC every 100 sims, `likelihood_settings` as explicit arg
- **Cluster management refactor** (v0.17.21): `make_mosaic_cluster()`, `cluster` param with ownership pattern

### Parameter/prior changes
- Seasonality param rename: `a1`→`a_1_j`, `a2`→`a_2_j`, etc. (v0.14.63–v0.14.65)
- Many prior distribution updates (kappa, zeta, psi_star, decay params, beta_j0_tot, delta_reporting)
- New `update_priors_from_posteriors()` for staged estimation (v0.17.18)
- `nb_k_min` split into `nb_k_min_cases` and `nb_k_min_deaths` (v0.17.28)

### New features
- Color palette system: `R/mosaic_colors.R` (882 lines)
- Parameter sensitivity analysis (PRCC → HSIC)
- Ensemble R² metric
- Prediction CSV combining
- Provenance tracking (`environment.json`, `summary.json`)

### Bug fixes
- 6 likelihood bugs fixed (v0.14.86)
- 20 stale tests fixed (v0.14.87)
- Convergence, ESS, outlier detection fixes (v0.14.38–v0.14.45)

## Result

| Metric | Value |
|---|---|
| **New branch** | `validate_dask_local_sim_take3` |
| **Base** | `upstream/main` at `56cbde2` (v0.17.33) |
| **Commits on branch** | 18 (17 cherry-picked + 1 ported) |
| **Conflicts resolved** | 4 minor (all in `azure/` files during cherry-pick) |
| **Files modified vs upstream** | `R/run_MOSAIC.R`, `R/run_MOSAIC_helpers.R` + all `azure/` and `inst/python/` Dask files |

## TODO / Follow-up

- [ ] Push `validate_dask_local_sim_take3` to origin
- [ ] Update `R/run_MOSAIC_dask.R` to match upstream's new dir structure (`cal_samples`, `cal_simresults`, etc.)
- [ ] Update `R/run_MOSAIC_dask.R` to use upstream's new worker patterns (`param_lookup`, `likelihood_settings`, `make_mosaic_cluster()`)
- [ ] Decide whether to keep NPE references in `run_MOSAIC_dask.R` or remove (upstream dropped embedded NPE)
- [ ] Rebuild Docker image with upstream's updated `environment.yml`
- [ ] Run validation test to confirm Dask vs local equivalence still holds on new codebase
