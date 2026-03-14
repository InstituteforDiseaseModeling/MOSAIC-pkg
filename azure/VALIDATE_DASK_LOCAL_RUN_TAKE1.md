# Dask ↔ Local Equivalence Validation — Take 1

**Date**: 2026-03-14
**Branch**: `dask_sim_level`
**Goal**: Verify that `run_MOSAIC()` (local R parallel) and `run_MOSAIC_dask()` (Dask/Coiled) produce identical LASER simulation results given the same seeds and parameters.

## Test Setup

| Setting | Value |
|---|---|
| N_SIMS | 50 |
| N_ITER | 1 |
| ISO | ETH |
| Local parallel | 8 PSOCK workers |
| Dask cluster | 5 × Standard_D4s_v6 (Coiled, mosaic-acr-workers) |
| Docker image | `idmmosaicacr.azurecr.io/mosaic-worker:latest` |

**Scripts**:
- `azure/validate_dask_local_equivalence.R` — runs both legs, writes parquets
- `azure/compare_validation_results.R` — standalone comparison of existing parquets

**Docker command**:
```bash
docker run --rm \
  -e LD_PRELOAD="/root/.virtualenvs/r-mosaic/lib/libcrypto.so.3:/root/.virtualenvs/r-mosaic/lib/libssl.so.3" \
  -v ~/MOSAIC/MOSAIC-pkg:/src/MOSAIC-pkg \
  -v ~/MOSAIC/MOSAIC-data:/workspace/MOSAIC/MOSAIC-data \
  -v ~/output:/workspace/output \
  -v ~/.config/dask:/root/.config/dask:ro \
  idmmosaicacr.azurecr.io/mosaic-worker:latest \
  bash -c "\
    /root/.virtualenvs/r-mosaic/bin/pip install --quiet coiled && \
    ln -sfn /src/MOSAIC-pkg /workspace/MOSAIC/MOSAIC-pkg && \
    R CMD INSTALL /src/MOSAIC-pkg && \
    Rscript /src/MOSAIC-pkg/azure/validate_dask_local_equivalence.R"
```

## Issues Encountered & Fixed

### 1. Sequential mode: 0 valid likelihoods

**Symptom**: `Insufficient valid samples: 0 (need at least 50 for ESS calculation)`

**Root cause**: `.mosaic_run_simulation_worker()` in `run_MOSAIC.R` references `control$likelihood$...` as a **free variable** — it's not a function parameter. In parallel mode, `control` is exported to PSOCK workers via `clusterExport()` and found through R's namespace chain (namespace → imports → base → globalenv). In sequential mode (`n_cores=1`), `control` is a local variable of `run_MOSAIC()` and unreachable from the package-level function. Every `calc_model_likelihood()` call fails silently (caught by `tryCatch` → `NA_real_`).

**Fix**: Changed validation script to use `parallel = TRUE, n_cores = 8L`.

### 2. Post-processing crash: `n_best_subset must be non-negative integer`

**Symptom**: `calc_convergence_diagnostics()` fails after simulations complete.

**Root cause**: With only 50 sims, the post-processing subset selection and convergence diagnostics receive edge-case inputs. Not a simulation issue — `simulations.parquet` is already written before this stage.

**Fix**: Wrapped both `run_MOSAIC()` and `run_MOSAIC_dask()` in `tryCatch`. Post-processing errors are logged but don't block the comparison. Parquet paths are constructed from known directory structure rather than relying on the return value.

### 3. `identical()` type mismatch on sim_ids

**Symptom**: `sim_id sets differ!` even though printed values are the same (both `1 2 3 ... 50`).

**Root cause**: Arrow reads integer columns with different precision depending on the writer. One leg writes `int32`, the other `double`. `identical()` is strict about types.

**Fix**: Coerce both `sim` columns to `as.integer()` and compare with `==` instead of `identical()`.

## Comparison Results

**Output dirs**: `~/output/validate_local_20260314_034830/` and `~/output/validate_dask_20260314_034830/`

### Parameters: PASS (exact match)
All 48 parameter columns match exactly across all 50 sims. Seeds match.

### Likelihoods: FAIL (large systematic differences)

```
Exact match (diff == 0):     0 / 50
Close match (|diff| < 1e-6): 0 / 50
Both finite:                 50 / 50

Abs diff — max: 7.95e+04  mean: 4.16e+03  median: 8.84e+02
Rel diff — max: 8.58e-01  mean: 1.08e-01  median: 4.75e-02

  Per-sim detail:
      sim         ll_local          ll_dask      abs_diff      rel_diff
  -------  ---------------  ---------------  ------------  ------------
        1    -16304.293636    -12617.915406      3.69e+03      2.26e-01
        2   -168323.815408   -177406.433636      9.08e+03      5.40e-02
        3    -22926.117886     -9216.933709      1.37e+04      5.98e-01
        4   -105517.915677   -109430.717256      3.91e+03      3.71e-02
        5    -18626.738015    -16583.348732      2.04e+03      1.10e-01
        6    -15379.077662    -15667.220600      2.88e+02      1.87e-02
        7    -33797.433148    -33140.985318      6.56e+02      1.94e-02
        8    -10158.728411    -10640.183170      4.81e+02      4.74e-02
        9   -112126.362723   -116449.721665      4.32e+03      3.86e-02
       10    -33604.582839    -30660.418448      2.94e+03      8.76e-02
       11    -13139.169221    -13428.940192      2.90e+02      2.21e-02
       12    -29104.922226    -31010.091251      1.91e+03      6.55e-02
       13    -92644.264960    -13115.243139      7.95e+04      8.58e-01
       14     -7832.786984     -8015.746987      1.83e+02      2.34e-02
       15    -16426.423603    -15756.531064      6.70e+02      4.08e-02
       16    -23164.000707    -22353.564078      8.10e+02      3.50e-02
       17    -50703.800228    -48051.466721      2.65e+03      5.23e-02
       18    -10531.786152    -10797.087918      2.65e+02      2.52e-02
       19    -34641.621118    -33848.596622      7.93e+02      2.29e-02
       20    -21552.540268    -20214.014241      1.34e+03      6.21e-02
       21    -19007.803605     -9526.603018      9.48e+03      4.99e-01
       22    -14812.634224     -8734.108302      6.08e+03      4.10e-01
       23     -8347.020405     -8320.384467      2.66e+01      3.19e-03
       24    -22326.982658    -20291.945666      2.04e+03      9.11e-02
       25    -11160.405076    -10677.243095      4.83e+02      4.33e-02
       26    -34278.416234    -34427.758529      1.49e+02      4.36e-03
       27    -32866.541931    -35566.602810      2.70e+03      8.22e-02
       28    -13586.444466    -12793.292092      7.93e+02      5.84e-02
       29     -7557.053488     -7592.123248      3.51e+01      4.64e-03
       30    -29392.893488    -23280.658046      6.11e+03      2.08e-01
       31    -70706.126205    -70630.854000      7.53e+01      1.06e-03
       32   -145317.321763   -159849.359182      1.45e+04      1.00e-01
       33    -31385.951650    -30886.457098      4.99e+02      1.59e-02
       34    -41265.447044    -43880.727663      2.62e+03      6.34e-02
       35    -21048.415575    -21335.934709      2.88e+02      1.37e-02
       36    -50693.074312    -55970.201375      5.28e+03      1.04e-01
       37    -56576.720797    -57533.500184      9.57e+02      1.69e-02
       38    -44573.946276    -49910.900306      5.34e+03      1.20e-01
       39    -52346.207552    -52550.392464      2.04e+02      3.90e-03
       40    -26205.249231    -12518.535572      1.37e+04      5.22e-01
       41    -23457.648618    -23721.451848      2.64e+02      1.12e-02
       42    -16576.017989    -16684.778278      1.09e+02      6.56e-03
       43     -8070.433959     -8546.847800      4.76e+02      5.90e-02
       44    -17524.661028    -16982.756436      5.42e+02      3.09e-02
       45     -9040.417916     -8399.518755      6.41e+02      7.09e-02
       46    -34519.473520    -34546.519122      2.70e+01      7.83e-04
       47    -14084.174311    -13584.958301      4.99e+02      3.54e-02
       48    -10239.710471     -8205.611072      2.03e+03      1.99e-01
       49    -11915.375989    -10449.441564      1.47e+03      1.23e-01
       50    -22836.477794    -21748.506472      1.09e+03      4.76e-02
```

Worst case: sim 13, local=-92644, dask=-13115, 86% relative difference.

### Parameters: PASS (exact match)

```
--- Parameter value comparison ---
  Comparing 48 parameter columns

  Top 10 parameter discrepancies (max |diff| across sims):
  parameter                       max_abs_diff
  ------------------------------  ------------
  N_j_initial                         0.00e+00
  S_j_initial_ETH                     0.00e+00
  E_j_initial_ETH                     0.00e+00
  I_j_initial_ETH                     0.00e+00
  R_j_initial_ETH                     0.00e+00
  V1_j_initial_ETH                    0.00e+00
  V2_j_initial_ETH                    0.00e+00
  phi_1                               0.00e+00
  phi_2                               0.00e+00
  omega_1                             0.00e+00

  Parameters with exact match:    48 / 48
  Parameters with |diff| < 1e-10: 48 / 48

--- Seed comparison ---
  seed_sim  match: TRUE
  seed_iter match: TRUE

=============================================================
  FAIL: results differ between local and Dask paths
    - Likelihood mismatch
=============================================================
```

### Verdict

All 48 parameters and seeds match exactly. Likelihoods differ because the LASER model receives different `psi_jt` matrices (see root cause below).

## Root Cause Analysis

**`sample_parameters()` modifies the `psi_jt` matrix — but the Dask path never sends it to workers.**

The call chain in `sample_parameters.R` (line 264):
```r
config_sampled <- apply_psi_star_calibration(config_sampled, sampling_flags, verbose)
```

This recomputes `psi_jt` (environmental suitability matrix, shape `n_locations × n_time_steps`) in-place using the sampled `psi_star_a/b/z/k` parameters via `calc_psi_star()`.

**Local path** (`run_MOSAIC`): passes the full `params_sim` (output of `sample_parameters()`) to LASER. This includes the **updated** `psi_jt`.

**Dask path** (`run_MOSAIC_dask`):
1. `base_config` is extracted from the **original** `config` (pre-sampling) and broadcast once
2. `.extract_sampled_params()` extracts per-sim params but **excludes** `psi_jt` (it's in `base_fields`)
3. The Python worker merges sampled scalars onto the stale base_config
4. LASER runs with the **original, uncalibrated** `psi_jt`

The sampled `psi_star_a/b/z/k` scalars are correctly sent to workers (and match in the parquet), but they are never applied to recompute `psi_jt`. The Dask worker receives stale `psi_jt` for every simulation.

## Required Fix

After `sample_parameters()` in `.mosaic_run_batch_dask()`, the updated `psi_jt` must be sent to the worker. Options:

**Option A**: Include the updated `psi_jt` in the per-sim payload (JSON or separate scatter). Adds ~200KB per sim (for a typical country with ~50 locations × ~200 timesteps × 8 bytes).

**Option B**: Replicate `apply_psi_star_calibration()` in Python on the worker side. The worker already receives `psi_star_a/b/z/k` and the original `psi_jt` — it just needs to apply the same transformation. Avoids sending large matrices per sim.

**Option C**: Move the base_config broadcast to happen per-sim (after sampling). Simplest but defeats the purpose of broadcasting once.

**Recommendation**: Option B — port the `psi_star` calibration logic to the Python worker. It's a simple element-wise transformation and avoids per-sim data transfer overhead.

## Files Created/Modified

| File | Purpose |
|---|---|
| `azure/validate_dask_local_equivalence.R` | End-to-end validation: runs both legs + comparison |
| `azure/compare_validation_results.R` | Standalone parquet comparison (no MOSAIC needed) |
| `azure/VALIDATE_DASK_LOCAL_RUN_TAKE1.md` | This document |