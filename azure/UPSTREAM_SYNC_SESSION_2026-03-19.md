# Upstream Sync Session — 2026-03-19

## Objective

Sync local fork (`tinghf/MOSAIC-pkg`) with upstream (`InstituteforDiseaseModeling/MOSAIC-pkg`) main branch, bringing 65 upstream commits (v0.13.25 through v0.14.62) into both local `main` and the `validate_dask_local_sim_take2` feature branch. Then port relevant upstream changes to `run_MOSAIC_dask.R`.

## Upstream State Before Sync

- **Local main** was at: v0.13.24 (+ 5 local-only commits: CLAUDE.md edits, quickstart docs, sync check script)
- **Upstream main** was at: v0.14.62 (65 commits ahead)
- **validate_dask_local_sim_take2** had 13 Dask-specific commits on top of old main

## Step 1: Merge upstream/main → local main

```
Commit: bc6ba9a
Message: Merge upstream/main (v0.13.25 through v0.14.62) into local main
```

**Conflict:** 1 file — `CLAUDE.md` (2 regions)
- Resolution: Took upstream's comprehensive 1,613-line version and appended local-only sections (VM Quick Start, External Docs/WebFetch guidance, Maintaining This File)

**Auto-merged cleanly:** 53 files (+3,476 / -2,458 lines)

## Step 2: Merge local main → validate_dask_local_sim_take2

```
Commit: 7d19b1a
Message: Merge local main (upstream v0.13.25–v0.14.62) into validate_dask_local_sim_take2
```

**Conflict:** 1 file — `R/run_MOSAIC.R` (1 region, lines 234–267)
- Resolution: Kept the Dask validation block (`output_matrix` trimming + `simresults_*.parquet` output); upstream side was a blank deletion.

**Auto-merged cleanly:** All other files including `DESCRIPTION`, `R/run_MOSAIC_helpers.R`, `CLAUDE.md`

## Step 3: Port upstream changes to run_MOSAIC_dask.R

```
Commit: afd850e
Message: Port upstream run_MOSAIC.R changes (v0.14.0–v0.14.62) to run_MOSAIC_dask.R
```

Manually ported the following upstream changes from `run_MOSAIC.R` into `run_MOSAIC_dask.R`, keeping all Dask/Coiled cluster logic intact:

| Change | Upstream Version | Description |
|--------|-----------------|-------------|
| Python import path | v0.14.7 | `laser_cholera` → `laser.cholera` (laser-cholera v0.11.1 rename) |
| `.mosaic_prepare_config_for_python()` | v0.14.19 | Wrap length-1 array params as R lists for single-location runs |
| `param_names_sampled` | v0.14.45 | Derive from sampling flags with special-case name mapping (beta_j0_tot→[beta_j0_hum,beta_j0_env], a1→a_1_j, etc.); replaces `param_names_estimated` |
| Outlier detection | v0.14.39 | Lower Tukey fence only — removed upper fence that incorrectly discarded best models |
| ESS check on final batch | v0.14.42 | Always run ESS on final batch — fixes spurious "no convergence" warning |
| Seed cast | v0.14.6 | Explicit `as.integer()` for `seed_iter_1` |
| Remove `resume` param | v0.14.59 | Removed resume parameter and all related code |

## Step 4: Additional Dask-specific fixes

```
Commit: a424463
Message: Fix Dask worker compat with upstream laser 0.11.1 + Docker/Coiled improvements
```

- Updated `inst/python/mosaic_dask_worker.py`: `laser_cholera` → `laser.cholera` import
- Updated `azure/Dockerfile`: install `laser-cholera>=0.11.1`, `laser-core>=1.0.1`
- Updated `azure/mosaic_dask_fixed_test.R`: workspace path fix

```
Commit: d59c149
Message: Fix OOM in FIXED mode + logging + trailing comma bug
```

- Sub-batching in FIXED mode using `control$calibration$batch_size`
- Added logging to `client$gather`
- Fixed trailing comma bug creating empty `stopifnot()` arg

## Step 5: Docker image rebuild and optimization

After the upstream merge, the Docker image needed a full rebuild to pick up Python 3.12, laser-cholera v0.11.1, PyTorch 2.4.0, etc. The rebuild exposed several issues.

### Image size bloat

- **Pre-merge image** (laser-cholera 0.9.1, Python 3.11, PyTorch 2.1.2): ~4.75 GB compressed (see `DOCKER_COILED_SUMMARY.md`)
- **First rebuild**: 33.6 GB — the `install_dependencies()` layer alone was 16.6 GB (conda downloads Python 3.12 + PyTorch 2.4.0 + full transitive dep tree + cache)
- **Root cause**: Docker layers are additive — `pip uninstall tensorflow` in a separate `RUN` layer doesn't reclaim space from the install layer
- **Fix**: Combined install, strip, and cleanup into a single `RUN` layer in the Dockerfile:
  - `MOSAIC::install_dependencies(force = TRUE)` (creates conda env)
  - `pip uninstall -y tensorflow` (~2-3 GB, only needed for suitability modeling, not Dask workers)
  - `conda clean --all -y`
  - `rm -rf /root/.cache/pip /tmp/*`
- **After single-layer fix**: 29.5 GB (still large, but the compressed push size to ACR is much smaller)

### Coiled pull timeout

- With the larger image, Coiled scheduler timed out pulling: "Timed out waiting for process to phone home" (~9 min pull exceeded Coiled's server-side limit)
- First few cluster creations failed; after Coiled cached the image in the region, even D2s_v6 workers could pull successfully
- Workaround during cache warmup: use larger scheduler VM (`Standard_D4s_v6` or `D8s_v6`) for faster disk I/O during pull

### Library version mismatch fixes (added to Dockerfile)

1. **OpenSSL mismatch**: virtualenv bundles OpenSSL 3.3.0 but Ubuntu base ships 3.0.x — missing `OPENSSL_3.3.0` versioned symbol. Fix: copy virtualenv's `libcrypto.so.3`/`libssl.so.3` to system path + `ldconfig`
2. **libexpat mismatch**: virtualenv bundles libexpat 2.7.x but system has 2.6.1 — missing `XML_SetAllocTrackerActivationThreshold`. Fix: copy virtualenv's `libexpat.so.1` to system path + `ldconfig`

Both follow the same pattern: reticulate loads Python through R, LD paths resolve to the system lib instead of the virtualenv's, so the newer virtualenv libs need to be copied to the system path.

### Related sessions

- `DOCKER_COILED_SUMMARY.md` — original Docker image build (4.75 GB)
- `DASK_SCALEOUT_SESSION_2026-03-12.md` — Docker Hub → ACR migration, OOM issues in orchestrator container
- `ACR_SETUP.md` — ACR registry setup and anonymous pull configuration

## Summary of Upstream Changes (v0.14.0–v0.14.62)

### Major categories of upstream work:

**laser-cholera v0.11.1 upgrade (v0.14.7–v0.14.17):**
- Python package rename: `laser_cholera` → `laser.cholera`
- New parameters: `mu_j_baseline`, `mu_j_slope`, `mu_j_epidemic_factor`, `chi_endemic`, `chi_epidemic`, `delta_reporting_*`
- Python 3.12 compatibility in environment.yml
- Seed handling bug fixes

**Prior/parameter overhaul (v0.14.0–v0.14.5, v0.14.27–v0.14.55):**
- Recalibrated epidemic_threshold priors
- New rho care-seeking probability (data-driven estimate)
- chi_endemic/chi_epidemic priors added
- Major prior distribution changes: kappa, zeta_1/2, psi_star_*, decay params
- Truncnorm priors for delta_reporting_* params
- Decoupled posterior distribution family from prior for uniform-prior params

**Calibration engine fixes (v0.14.38–v0.14.62):**
- Fixed cumulative likelihood formula (NA-masked weights)
- Fixed outlier detection (removed upper IQR fence)
- Fixed convergence denominator (ESS-computed param count)
- Require min 5 data points before R² can trigger exit
- Fixed ESS check on final batch
- Removed resume functionality
- Fixed seasonality param name mismatch

**Plotting/visualization (v0.14.21–v0.14.28):**
- Overhauled `plot_model_ppc()` for coverage analysis
- Progress bars in plot functions
- Label/naming fixes across plotting functions

**New functions:**
- `get_rho_care_seeking_params()` + `plot_rho_care_seeking_params()`
- `.mosaic_prepare_config_for_python()` (internal)

**Files changed (62 total across all session commits):**
- +5,284 / -2,882 lines

## Current State

- **Branch:** `validate_dask_local_sim_take2`
- **HEAD:** `a424463` (4 commits ahead of remote)
- **Local main:** synced with upstream through v0.14.62
- **run_MOSAIC_dask.R:** synced with upstream run_MOSAIC.R changes
- **Dask worker Python:** synced with laser-cholera v0.11.1 import rename
- **Docker image:** needs rebuild to pick up new laser-cholera/laser-core versions