# GPU-Accelerated Batched Likelihood

## Overview

The MOSAIC calibration pipeline spends significant time computing likelihoods serially — one simulation at a time. The GPU path replaces this serial loop with a single batched tensor operation via the R `torch` package, processing all simulations in a batch simultaneously.

This is an **optional, drop-in acceleration** controlled by `ctrl$parallel$use_gpu <- TRUE`. When disabled (default) or when `torch` is not installed, the existing serial CPU path runs unchanged.

## Architecture

```
                        run_MOSAIC_dask()
                              │
                    .mosaic_run_batch_dask()
                              │
              ┌───────────────┴───────────────┐
              │                               │
        use_gpu = TRUE                  use_gpu = FALSE
              │                               │
   Stack gathered results              Serial loop (unchanged)
   into 3D arrays per iter             for each sim:
   [n_sims, n_loc, n_time]              calc_model_likelihood()
              │
   calc_model_likelihood_gpu()
              │
   ┌──────────┴──────────┐
   │                     │
   GPU (torch tensors)   CPU (R helpers)
   ├─ Guardrails         ├─ Peak timing
   ├─ NB core LL         ├─ Peak magnitude
   ├─ Cumulative NB      └─ WIS
   ├─ Max terms
   └─ Assembly
              │
   Return [n_sims] LL vector
              │
   log-mean-exp across iterations
              │
   Write parquet files (serial I/O)
```

### What runs on GPU

| Component | % of compute | Torch operation |
|-----------|-------------|-----------------|
| NB core log-likelihood | ~60% | `lgamma`, `log`, elementwise on `[n_sims, n_loc, n_time]` tensors |
| Guardrail checks | ~5% | Boolean ratio/correlation checks, `any()` reductions |
| Cumulative NB | ~10% | `cumsum` at 4 checkpoints, NB formula at each |
| Max terms | ~3% | `max` reduction + Poisson log-pmf |
| Assembly | ~2% | Weighted sum across locations |

### What stays on CPU

| Component | Why |
|-----------|-----|
| Peak timing/magnitude | Variable-length peak indices per location, windowed `which.max` — irregular, not worth tensorizing for ~10% of compute |
| WIS | Requires `qnbinom()` (NB quantile function) which has no torch equivalent |

### Key design decisions

- **`torch_float64` throughout** — NB `lgamma` on large cumulative values needs double precision
- **Iteration collapsing unchanged** — each iteration is a separate batched GPU call, then `calc_log_mean_exp` collapses on CPU (mathematically identical)
- **Graceful fallback** — if `torch` is not installed or CUDA unavailable, falls back to serial CPU with a message
- **Existing function untouched** — `calc_model_likelihood()` is not modified; `calc_model_likelihood_gpu()` is a parallel path

## Docker Workflows

### Two image tags

| Tag | Base | Extras | Size | Use case |
|-----|------|--------|------|----------|
| `mosaic-worker:latest` | rocker/geospatial:4.4 | MOSAIC, laser-cholera, dask/coiled | ~3.4 GB | Default CPU workers |
| `mosaic-worker:gpu` | `mosaic-worker:latest` | CUDA 12.4, cuDNN, R torch | ~8-10 GB | GPU-accelerated orchestrator |

### Build the GPU image

```bash
# Prerequisite: mosaic-worker:latest must exist
# Build from package root (COPY . installs MOSAIC from local source)
cd MOSAIC-pkg

# Build GPU image
docker build -f azure/Dockerfile.gpu -t mosaic-worker:gpu .

# Verify
docker run --rm mosaic-worker:gpu R -e "library(torch); cat('CUDA:', cuda_is_available(), '\n')"
```

To run with GPU passthrough (requires NVIDIA Container Toolkit):

```bash
docker run --gpus all mosaic-worker:gpu nvidia-smi
```

### Push to ACR (for Coiled workers)

```bash
# Tag for Azure Container Registry
docker tag mosaic-worker:gpu idmmosaicacr.azurecr.io/mosaic-worker:gpu

# Push
docker push idmmosaicacr.azurecr.io/mosaic-worker:gpu
```

### Coiled software environment

If using Coiled with the GPU image, create a separate software environment pointing to the `:gpu` tag. The `dask_spec$software` field in the test script should reference this environment.

## Running a Fixed-Mode GPU Test

### 1. Local Docker test (no Dask/Coiled)

Validate the GPU likelihood function matches serial output:

```bash
# Install torch at runtime and run validation (CPU-only, no GPU needed)
docker run --rm \
  -v $(pwd):/workspace/MOSAIC-pkg \
  mosaic-worker:latest bash -c '
    R -e "install.packages(\"torch\", repos=\"https://cloud.r-project.org\", quiet=TRUE)"
    R -e "torch::install_torch(type=\"cpu\")"
    Rscript /workspace/MOSAIC-pkg/azure/test_gpu_cpu_validation.R
  '
```

With the GPU image (includes torch pre-installed):

```bash
docker run --rm --gpus all \
  -v $(pwd):/workspace/MOSAIC-pkg \
  mosaic-worker:gpu \
  Rscript /workspace/MOSAIC-pkg/azure/test_gpu_cpu_validation.R
```

### 2. Dask/Coiled calibration run

The test script `azure/mosaic_dask_gpu_test.R` runs a small fixed-mode calibration (500 sims, 3 iterations, single country ETH):

```bash
# From the orchestrator VM (with Coiled credentials)
docker run --rm --gpus all \
  -v $(pwd):/workspace/MOSAIC-pkg \
  -v /path/to/output:/workspace/output \
  -e COILED_TOKEN=$COILED_TOKEN \
  mosaic-worker:gpu \
  Rscript /workspace/MOSAIC-pkg/azure/mosaic_dask_gpu_test.R
```

The key control settings in the test script:

```r
ctrl <- mosaic_control_defaults()
ctrl$calibration$n_simulations <- 500
ctrl$calibration$batch_size    <- 500
ctrl$calibration$n_iterations  <- 3
ctrl$parallel$use_gpu          <- TRUE   # enable GPU batched likelihood
```

Note: the Dask workers still run LASER simulations on CPU (the GPU acceleration is for the likelihood computation on the orchestrator side, not for the LASER model itself).

### 3. Adding GPU to an existing script

To enable GPU in any existing `run_MOSAIC_dask()` call, add one line:

```r
ctrl$parallel$use_gpu <- TRUE
```

No other changes needed. If `torch` is not available, it prints a message and falls back to the serial path automatically.

## Validating GPU vs CPU Results

### Automated validation suite

`azure/test_gpu_cpu_validation.R` runs 6 tests comparing `calc_model_likelihood_gpu()` (torch path) against `calc_model_likelihood()` (serial R path) on identical synthetic data:

| Test | Components enabled | Expected tolerance |
|------|-------------------|-------------------|
| 1. NB core only | NB likelihood | < 1e-9 |
| 2. NB + max terms | NB + Poisson max | < 1e-9 |
| 3. NB + cumulative | NB + cumulative NB at 4 checkpoints | < 1e-6 |
| 4. NB + guardrails | NB + ratio/correlation checks | < 1e-9 |
| 5. All GPU features | NB + max + cumulative + guardrails | < 1e-6 |
| 6. Performance (500 sims) | NB + max + cumulative | Reports speedup + diff |

Run it:

```bash
docker run --rm -v $(pwd):/workspace/MOSAIC-pkg mosaic-worker:gpu \
  Rscript /workspace/MOSAIC-pkg/azure/test_gpu_cpu_validation.R
```

Expected output:

```
=== Test 1: NB core only ===
[NB_core] max_diff=2.248726e-10  PASS
=== Test 2: NB + max_terms ===
[NB+max] max_diff=2.249863e-10  PASS
=== Test 3: NB + cumulative ===
[NB+cum] max_diff=3.103216e-07  PASS
=== Test 4: NB + guardrails ===
[NB+guard] max_diff=2.247589e-10  PASS
=== Test 5: All GPU features ===
[all_gpu] max_diff=2.025089e-07  PASS
=== Test 6: Performance (500 sims) ===
[perf] GPU: 0.03s, Serial: 0.21s, Speedup: 7.1x, max_diff=2.73e-07
=== ALL VALIDATION TESTS PASSED ===
```

The ~1e-7 tolerance on cumulative tests comes from floating-point differences between torch's `lgamma` implementation and R's `dnbinom` C implementation on large cumulative sums (~2600). This is negligible relative to likelihood values (typically -500 to -50000).

### Manual comparison on real calibration output

To compare a full calibration run with and without GPU:

```r
# Run 1: GPU path
ctrl$parallel$use_gpu <- TRUE
result_gpu <- run_MOSAIC_dask(config, priors, dir_output = "output/gpu_run", control = ctrl, dask_spec = dask_spec)

# Run 2: CPU path (same control, same seeds)
ctrl$parallel$use_gpu <- FALSE
result_cpu <- run_MOSAIC_dask(config, priors, dir_output = "output/cpu_run", control = ctrl, dask_spec = dask_spec)

# Compare likelihood values
gpu_samples <- arrow::read_parquet("output/gpu_run/2_calibration/samples.parquet")
cpu_samples <- arrow::read_parquet("output/cpu_run/2_calibration/samples.parquet")

# Match by sim ID and compare
merged <- merge(gpu_samples[, c("sim", "likelihood")],
                cpu_samples[, c("sim", "likelihood")],
                by = "sim", suffixes = c("_gpu", "_cpu"))
cat("Max likelihood difference:", max(abs(merged$likelihood_gpu - merged$likelihood_cpu), na.rm = TRUE), "\n")
```

Both runs use the same seeds (`seed_sim = sim_id`) so parameter samples and LASER outputs are identical — only the likelihood computation path differs.
