# MOSAIC Dask Calibration Guide

**Function**: `run_MOSAIC_dask()` in `R/run_MOSAIC_dask.R`
**Worker module**: `inst/python/mosaic_dask_worker.py`
**Last Updated**: 2026-03-02

---

## Why Dask instead of R `parallel`?

MOSAIC's existing `run_MOSAIC()` uses R's built-in `parallel::makeCluster()`, which is limited to one machine. For multi-country runs this is the **only option** — country coupling (mobility, shared environmental suitability) happens *inside* each LASER simulation, so you cannot parallelize at the country level. Each simulation must see all countries simultaneously.

`run_MOSAIC_dask()` replaces the R cluster with a Dask cluster spanning multiple cloud VMs (via Coiled.io on Azure), dispatching each LASER simulation as a pure-Python Dask task. LASER is already Python (`laser_cholera`); workers call it directly without any R subprocess.

---

## Architecture

```
R orchestrator (local / VM)
├── sample_parameters()  ×  batch_size    [R, serial, fast]
├── serialize sampled params → JSON       [one per sim, ~few KB]
├── client.scatter(base_config)           [matrices broadcast ONCE to all workers]
└── client.map(run_laser_sim, sim_ids)    [all sims dispatched simultaneously]

     ┌─────────── Coiled worker ─────────────┐
     │  mosaic_dask_worker.run_laser_sim()   │
     │  ├── merge base_config + sampled JSON │
     │  ├── laser_cholera.run_model() × n_iter│
     │  └── return {expected_cases, deaths}  │
     └───────────────────────────────────────┘

R orchestrator (continued)
├── client.gather()                        [blocks until all futures done]
├── calc_model_likelihood()  ×  batch_size [R, serial, fast]
├── write sim_XXXXXXX.parquet              [one file per sim]
└── [all post-processing identical to run_MOSAIC()]
    ESS check → next batch decision → repeat until converged
```

**Key design points:**

| Concern | Approach |
|---------|----------|
| Large matrix fields (`psi_jt`, `b_jt`, etc.) | Broadcast via `client.scatter()` once; reused by all workers |
| Per-sim sampled params (scalars/vectors) | JSON string per future; ~few KB |
| Threading safety | `OMP_NUM_THREADS=1` etc. set inside worker function (not module level) |
| Python GC | `gc.collect()` after each simulation on worker |
| Worker code updates | `client.upload_file()` — no Docker rebuild needed |

---

## Prerequisites

### 1. Docker image with MOSAIC

The worker image `idmmosaicacr.azurecr.io/mosaic-worker:latest` is hosted on Azure Container Registry (ACR) to avoid Docker Hub pull rate limits. It includes:
- MOSAIC R package (v0.13.24)
- `laser_cholera==0.9.1`
- PyTorch, SBI, Zuko (for NPE)
- Dask / distributed
- MOSAIC-data repo (`/workspace/MOSAIC/MOSAIC-data`)

See [ACR_SETUP.md](ACR_SETUP.md) for full ACR configuration details.

**Verify the image exists:**
```bash
docker pull idmmosaicacr.azurecr.io/mosaic-worker:latest
docker run --rm idmmosaicacr.azurecr.io/mosaic-worker:latest R -e "library(MOSAIC); packageVersion('MOSAIC')"
```

**Rebuild if MOSAIC-pkg has changed:**
```bash
cd ~/MOSAIC/MOSAIC-pkg
docker build -f azure/Dockerfile -t mosaic-worker:latest .
docker tag mosaic-worker:latest idmmosaicacr.azurecr.io/mosaic-worker:latest
az acr login --name idmmosaicacr
docker push idmmosaicacr.azurecr.io/mosaic-worker:latest
```

### 2. Coiled software environment

`mosaic-acr-workers` is a **Coiled software environment** — a cloud-side definition stored in your Coiled account that tells Coiled workers which Docker image to use (from ACR). It is not a conda environment.

**Check it exists** (`mosaic-coiled` conda env is just where the `coiled` CLI lives):
```bash
conda activate mosaic-coiled   # activates the local env that has the coiled CLI
coiled env list                # lists your Coiled-side software environments
```

You should see `mosaic-acr-workers` in the output.

**Recreate if needed** (e.g. after pushing a new Docker image):
```python
import coiled
coiled.create_software_environment(
    name='mosaic-acr-workers',
    container='idmmosaicacr.azurecr.io/mosaic-worker:latest',
    force_rebuild=True
)
```

### 3. Local R environment

The R session that calls `run_MOSAIC_dask()` needs:
- MOSAIC package installed
- `reticulate` with access to a Python environment that has `coiled` and `dask.distributed`
- `coiled login` authenticated (run once in terminal)

The local Python environment (`mosaic-coiled` conda env) already has `coiled` and `dask`. Point reticulate at it:

```r
# In R (or add to ~/.Rprofile)
Sys.setenv(RETICULATE_PYTHON = "~/.conda/envs/mosaic-coiled/bin/python")
```

Or activate first in the shell before starting R:
```bash
conda activate mosaic-coiled
R
```

---

## Quick Sample Run

Runs 50 ETH simulations across 2 Coiled workers — verifies the full pipeline end-to-end in ~10 minutes.

**Two options depending on your setup:**

| | Option A: Docker (recommended) | Option B: Local R |
|---|---|---|
| R packages needed locally | None | Full MOSAIC + deps |
| Coiled credentials | Mount `~/.config/coiled` | `coiled login` before starting R |
| Gets `run_MOSAIC_dask()` | Installs from local source in container | Must be installed locally |

---

### Option A: Run R orchestrator inside Docker (no local R setup needed)

The Docker image already has all R packages and Python deps. You run R *inside* the container, which connects outward to Coiled to spin up worker VMs.

**Prerequisite:** `coiled login` must have been run at least once locally. Credentials are saved to `~/.config/dask/coiled.yaml` (not `~/.config/coiled/`).

#### Step 1: Write the R test script

```bash
cat > /tmp/mosaic_dask_test.R << 'EOF'
library(MOSAIC)

# Auto-detect Docker vs local, create a timestamped run directory
base_output_dir <- if (dir.exists("/workspace/output")) {
  "/workspace/output"             # Docker container
} else {
  file.path(getwd(), "output")   # Local run (current working directory)
}
run_stamp  <- format(Sys.time(), "%Y%m%d_%H%M%S")
dir_output <- file.path(base_output_dir, paste0("ETH_", run_stamp))
cat("Output directory:", dir_output, "\n")

set_root_directory("/workspace/MOSAIC")

config <- get_location_config(iso = "ETH")
priors <- get_location_priors(iso = "ETH")

ctrl <- mosaic_control_defaults(
  calibration = list(n_simulations = 50, n_iterations = 1),
  paths       = list(plots = FALSE)
)

dask_spec <- list(
  type         = "coiled",
  n_workers    = 2,
  software     = "mosaic-docker-workers",
  vm_types     = c("Standard_D8s_v6"),
  region       = "westus2",
  idle_timeout = "30 minutes"
)

result <- run_MOSAIC_dask(
  config     = config,
  priors     = priors,
  dir_output = dir_output,
  control    = ctrl,
  dask_spec  = dask_spec
)

cat("Done. Simulations:", result$summary$sims_total, "\n")
cat("Results saved to:", dir_output, "\n")
EOF
```

#### Step 2: Launch the container

```bash
mkdir -p ~/output

docker run --rm \
  -v ~/MOSAIC/MOSAIC-pkg:/src/MOSAIC-pkg \
  -v ~/MOSAIC/MOSAIC-data:/workspace/MOSAIC/MOSAIC-data \
  -v ~/output:/workspace/output \
  -v ~/.config/dask:/root/.config/dask:ro \
  idmmosaicacr.azurecr.io/mosaic-worker:latest \
  bash -c "R CMD INSTALL /src/MOSAIC-pkg && Rscript /src/MOSAIC-pkg/azure/mosaic_dask_test.R"
```

What happens:
- `R CMD INSTALL /src/MOSAIC-pkg` — installs `run_MOSAIC_dask()` from your local source into the container's R library (~60 s; all deps already present)
- `Rscript /src/MOSAIC-pkg/azure/mosaic_dask_test.R` — runs the calibration; connects to Coiled using the mounted credentials

#### Step 3: Verify outputs

```bash
ls ~/output/ETH_dask_test/1_bfrs/outputs/
# → simulations.parquet
```

---

### Option B: Run from local R session

Install any missing R packages first, then run directly in R:

```r
install.packages("ggplot2")          # or: devtools::install_deps()
library(MOSAIC)
set_root_directory("~/MOSAIC")

# Verify Python env has coiled + dask (activate mosaic-coiled conda env before starting R)
reticulate::import("coiled")
reticulate::import("dask.distributed")
```

Then continue with the same config/control/dask_spec/run steps below:

```r
config <- get_location_config(iso = "ETH")
priors <- get_location_priors(iso = "ETH")

ctrl <- mosaic_control_defaults(
  calibration = list(n_simulations = 50, n_iterations = 1),
  paths       = list(plots = FALSE)
)

dask_spec <- list(
  type         = "coiled",
  n_workers    = 2,
  software     = "mosaic-docker-workers",
  vm_types     = c("Standard_D8s_v6"),
  region       = "westus2",
  idle_timeout = "30 minutes"
)

result <- run_MOSAIC_dask(
  config     = config,
  priors     = priors,
  dir_output = "./output/ETH_dask_test",
  control    = ctrl,
  dask_spec  = dask_spec
)
```

---

### Expected output (both options)

```
[...] Starting MOSAIC Dask calibration
[...] Creating Coiled cluster: 2 workers (Standard_D8s_v6, westus2)
[...] Dask dashboard: https://cloud.coiled.io/...
[...] Uploaded mosaic_dask_worker.py to all workers
[...] Broadcasting base config to workers...
[...] [FIXED MODE] Running exactly 50 simulations
[...]   Sampling 50 parameter sets in R...
[...]   Submitting 50 futures to Dask cluster...
[...]   Waiting for 50 Dask futures...
[...] Fixed batch complete: 50/50 successful (100.0%) in 8.3 minutes
[...] Dask calibration complete: 1 batches, 50 simulations, 12.4 min
```

### Verify results

```r
df <- arrow::read_parquet("~/output/ETH_dask_test/1_bfrs/outputs/simulations.parquet")
cat("Simulations:", nrow(df), "\n")
cat("Valid likelihoods:", sum(is.finite(df$likelihood)), "\n")
cat("Likelihood range:", range(df$likelihood[is.finite(df$likelihood)]), "\n")
```

**Expected:** 50 rows, all likelihoods finite and negative, columns `sim, iter, seed_sim, seed_iter, likelihood, gamma_1, ...`

---

## Full Production Run

For a real calibration with auto-convergence mode:

```r
library(MOSAIC)
set_root_directory("~/MOSAIC")

# Multi-country (coupled) run
config <- get_location_config(iso = c("ETH", "KEN", "TZA"))
priors <- get_location_priors(iso = c("ETH", "KEN", "TZA"))

ctrl <- mosaic_control_defaults(
  calibration = list(
    n_simulations = NULL,    # NULL = auto mode (adaptive batching until convergence)
    n_iterations  = 3,
    batch_size    = 500
  ),
  parallel = list(
    n_cores = 10             # used as default n_workers if dask_spec$n_workers not set
  )
)

dask_spec <- list(
  type        = "coiled",
  n_workers   = 10,
  software    = "mosaic-docker-workers",
  vm_types    = c("Standard_D8s_v6"),
  region      = "westus2",
  idle_timeout = "2 hours"
)

result <- run_MOSAIC_dask(
  config     = config,
  priors     = priors,
  dir_output = "./output/ETH_KEN_TZA_production",
  control    = ctrl,
  dask_spec  = dask_spec
)
```

---

## Connecting to an Existing Scheduler

If you have a Dask scheduler already running (e.g. inside a VM or HPC cluster):

```r
dask_spec <- list(
  type    = "scheduler",
  address = "tcp://10.0.0.5:8786"   # scheduler address
)

result <- run_MOSAIC_dask(
  config     = config,
  priors     = priors,
  dir_output = "./output/my_run",
  dask_spec  = dask_spec
)
```

For a **local test** without Coiled (uses all cores on the current machine):

```python
# In terminal, start a local Dask scheduler first:
python -c "
from dask.distributed import LocalCluster
cluster = LocalCluster(n_workers=4)
print('Scheduler address:', cluster.scheduler_address)
cluster.get_client().sync(cluster.get_client().scheduler.run_forever)
"
```

Then in R:
```r
dask_spec <- list(type = "scheduler", address = "tcp://127.0.0.1:XXXX")
```

---

## Resuming an Interrupted Run

```r
result <- run_MOSAIC_dask(
  config     = config,
  priors     = priors,
  dir_output = "./output/ETH_dask_test",   # same dir as interrupted run
  control    = ctrl,
  dask_spec  = dask_spec,
  resume     = TRUE                         # skips already-completed sim IDs
)
```

The run state is checkpointed to `<dir_output>/1_bfrs/diagnostics/run_state.rds` after every batch.

---

## Output Structure

Output structure is identical to `run_MOSAIC()`:

```
<dir_output>/
├── 0_setup/
│   ├── simulation_params.json    # full run config (includes dask_spec)
│   ├── priors.json
│   └── config_base.json
├── 1_bfrs/
│   ├── outputs/
│   │   └── simulations.parquet   # all sims combined (schema below)
│   ├── diagnostics/
│   │   ├── run_state.rds         # checkpoint for resume
│   │   ├── parameter_ess.csv
│   │   ├── convergence_results.parquet
│   │   └── convergence_diagnostics.json
│   ├── posterior/
│   │   ├── posterior_quantiles.csv
│   │   └── posteriors.json
│   └── plots/                    # (if control$paths$plots = TRUE)
└── 2_npe/                        # (if control$npe$enable = TRUE)
```

**`simulations.parquet` schema:**

| Column | Type | Description |
|--------|------|-------------|
| `sim` | int | Simulation ID (1-based) |
| `iter` | int | Always 1 (iterations already collapsed) |
| `seed_sim` | int | Equals `sim` — parameter sampling seed |
| `seed_iter` | int | First iteration seed: `(sim-1)*n_iter + 1` |
| `likelihood` | float | Log-likelihood (or -999999999 if guardrail floored) |
| `gamma_1`, `gamma_2`, ... | float | All sampled parameter values |
| `is_finite`, `is_valid`, ... | bool | Post-processing flags |
| `weight_all`, `weight_retained`, `weight_best` | float | Akaike/Gibbs weights |

---

## Troubleshooting

### `ImportError: No module named 'coiled'`

The local R session cannot find the `coiled` Python package. Fix:
```bash
conda activate mosaic-coiled
R  # start R from this activated environment
```
Or in R before calling the function:
```r
Sys.setenv(RETICULATE_PYTHON = path.expand("~/.conda/envs/mosaic-coiled/bin/python"))
reticulate::use_python(Sys.getenv("RETICULATE_PYTHON"), required = TRUE)
```

### `Cannot find inst/python/mosaic_dask_worker.py`

The MOSAIC package needs to be reinstalled from source (the new Python file was added):
```r
devtools::install()   # from MOSAIC-pkg directory
# or
R CMD INSTALL .
```

### Worker returns `success=False`

Check the error message in the R log — it prints `res$error` and `res$traceback` for failed futures. Common causes:
- `laser_cholera` not importable on worker → verify Docker image has it: `docker run --rm idmmosaicacr.azurecr.io/mosaic-worker:latest python -c "import laser_cholera"`
- `TypeError: 'int' object is not iterable` → a scalar field in `sampled_params` is being treated as a vector by LASER; check `_VECTOR_FIELDS` list in `mosaic_dask_worker.py`
- Memory error on worker → reduce `n_iterations` or increase VM size

### Azure quota exceeded

```bash
# Check and clean up stopped clusters
coiled cluster list
# Delete via Coiled dashboard or:
python -c "import coiled; coiled.delete_cluster('cluster-name')"
```

### Slow batch (workers idle)

Check the Dask dashboard link printed at startup. Workers may be waiting for `base_config_future` to broadcast. For very large configs (many locations × many time steps), the scatter can take 30-60 seconds on first batch.

---

## Cost Estimate (Coiled.io on Azure, westus2)

| Run type | Workers | VM | Sims | ~Time | ~Cost |
|----------|---------|-----|------|-------|-------|
| Quick test | 2 | D8s_v6 | 50 | 10 min | $0.50 |
| Small run | 5 | D8s_v6 | 500 | 25 min | $3 |
| Production (auto) | 10 | D8s_v6 | ~5000 | 2–3 hr | $40 |
| Large multi-country | 20 | D16s_v6 | ~10000 | 3–4 hr | $150 |

Coiled workers are billed per-second and shut down automatically (`shutdown_on_close=True`, `idle_timeout` setting).

---

## Scaling Up (>1000 sims per batch)

Currently uses **Option A** output: workers return `expected_cases`/`disease_deaths` arrays in-memory via `client.gather()`. The Dask scheduler holds all gathered results in RAM. For batches >1000 sims this can exhaust scheduler memory.

**TODO — Option B** (see `@section Output storage` in `run_MOSAIC_dask` roxygen docs):
Workers write parquet files directly to Azure Blob Storage using the `azure-storage-blob` Python SDK. R orchestrator reads back via `AzureStor`. This removes the scheduler memory bottleneck and enables 10K+ sim batches. Requires `AZURE_STORAGE_CONNECTION_STRING` on workers (pass via `coiled.Cluster(environ={"AZURE_STORAGE_CONNECTION_STRING": ...})`).

---

## Files

| File | Purpose |
|------|---------|
| `R/run_MOSAIC_dask.R` | R function `run_MOSAIC_dask()` — orchestrator |
| `inst/python/mosaic_dask_worker.py` | Python worker function — runs on Dask nodes |
| `azure/Dockerfile` | Worker Docker image (MOSAIC + laser_cholera + dask) |
| `azure/run_mosaic_dask_bfrs.py` | Original partial Python implementation (reference) |
| `azure/STATUS_AND_NEXT_STEPS.md` | Prior session context and known issues |
| `azure/STORAGE_MOUNTING_INVESTIGATION.md` | Why filesystem mounting (CIFS/BlobFuse) was ruled out |
