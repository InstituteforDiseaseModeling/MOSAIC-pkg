# MOSAIC Cloud Parallel Calibration — Primer

**What this document covers**: A conceptual introduction to the tools and architecture
introduced in the `tt/azure-coiled` work, how they change the MOSAIC package, and why
each piece is necessary. Assumes familiarity with R and MOSAIC but no prior knowledge of
Docker, Dask, or cloud computing.

---

## 1. The Problem Being Solved

MOSAIC's Bayesian calibration (`run_MOSAIC()`) works by running thousands of LASER
simulations with different parameter draws and evaluating how well each fits the
observed data. On a single machine with 16–32 CPU cores, this takes hours to days for
a full multi-country calibration.

**Why not just use more cores on one machine?**

The existing `run_MOSAIC()` already uses R's `parallel::makeCluster()` to spread
simulations across all cores of a single machine. The ceiling is the number of cores
you have locally (typically 8–32). Beyond that, you need multiple machines — which R's
built-in parallel machinery cannot manage.

**Why not parallelize at the country level?**

Countries are coupled in MOSAIC: mobility parameters (`tau_i`) move people across
borders, so Ethiopia, Kenya, and Tanzania all interact within a single LASER simulation.
Every simulation must see all countries at once. You cannot split countries across
machines — but you *can* split *simulations* across machines.

**The solution: simulation-level distributed parallelism.**

Run each LASER simulation as an independent task dispatched to a worker machine in the
cloud. R remains the orchestrator (handling parameter sampling, likelihood computation,
and results aggregation); Python workers run the computationally expensive LASER
simulations. This is what `run_MOSAIC()` implements.

---

## 2. The Tool Stack

### 2.1 Dask — Python's Distributed Task Scheduler

**What it is**: Dask is a Python library for parallel and distributed computing. Its
`distributed` component lets you run Python functions across many machines (or many
processes on one machine) as a task graph, with automatic scheduling, failure recovery,
and result transport.

**The three components** (all relevant to MOSAIC):

```
┌─────────────────────────────────────────────────────────┐
│                    YOUR CODE (R/Python)                  │
│         Connects to Dask via the "Client"                │
└───────────────────┬─────────────────────────────────────┘
                    │
              [Dask Client]
              (Python object in your R session via reticulate)
                    │
              [Dask Scheduler]
              (a dedicated process on one machine)
              Receives tasks, assigns them to workers,
              tracks which workers are idle/busy
                    │
        ┌───────────┴───────────┐
   [Worker 1]            [Worker N]
   (cloud VM)            (cloud VM)
   runs Python           runs Python
   functions             functions
```

**Key API methods** (used in `run_MOSAIC.R` via reticulate):

| Method | What it does in MOSAIC |
|--------|------------------------|
| `client$scatter(obj)` | Sends the large base config (matrices) to all workers *once*. Workers cache it — avoids re-sending 10+ MB for every simulation. Returns a "future" pointing to the cached data. |
| `client$map(fn, args)` | Dispatches `fn(args[[1]])`, `fn(args[[2]])`, ... as parallel tasks across all workers. Returns a list of futures immediately (doesn't wait). Used to send a whole batch of simulations at once. |
| `client$gather(futures)` | Waits for all futures to complete and collects their results back to the orchestrator. This is the "blocking" step — R waits here until all simulations in the batch are done. |
| `client$upload_file(path)` | Copies a Python file (`mosaic_dask_worker.py`) to every worker so they can import it. |

**Futures**: A future is a handle to a computation that hasn't necessarily finished yet.
`client$map()` returns futures immediately; `client$gather()` waits for them to resolve.
This is analogous to how `parallel::parLapply()` works in R but explicit about the
asynchrony.

**Comparison with R's `parallel`**:

| | R `parallel::makeCluster()` | Dask distributed |
|---|---|---|
| Scale | One machine, N cores | Multiple machines, M×N cores |
| Workers | R processes | Python processes |
| Task unit | R function call | Python function call |
| Data transfer | Shared memory (FORK) or socket (PSOCK) | Serialized over network |
| Setup | Minimal | Requires Python + Dask install |

---

### 2.2 Coiled.io — Managed Dask Clusters in the Cloud

**What it is**: Coiled is a commercial service (free tier available) that provisions
Dask clusters on cloud infrastructure (AWS, Azure, GCP) on demand. You call
`coiled.Cluster(...)` in Python and within 1–2 minutes you have N cloud VMs running
Dask workers, all connected to a scheduler. When the cluster shuts down, the VMs are
terminated and billing stops.

**Why it matters for MOSAIC**:
- You pay only for the time simulations are actually running (per-second billing).
- Workers start in ~1–2 minutes (vs. hours to provision a dedicated VM cluster).
- You can scale to 100 workers for a large calibration run and shut down afterward.
- No cluster administration: no Kubernetes, no HPC job scheduler, no SSH key management.

**How it works**:

1. You have a Coiled account linked to an Azure subscription.
2. You define a *software environment* in Coiled — this tells Coiled what Docker image
   (or conda environment) to run on each worker VM.
3. When you call `coiled.Cluster(n_workers=10, software="mosaic-acr-workers")`, Coiled
   spins up 10 Azure VMs, each running the specified Docker image, each starting a Dask
   worker process inside that image.
4. Coiled returns the scheduler address. Your local Dask Client connects to it.
5. From that point on, from R's perspective, it's just a regular Dask cluster.
6. `shutdown_on_close=True` (default) ensures VMs are terminated when the Client closes.

**Coiled software environments** are cloud-side configurations stored in your Coiled
account. They are *not* local conda environments — they are metadata entries saying
"when a worker spins up, use this Docker image." The `coiled env list` command lists them.

**Pricing** (Azure westus2, approximate):
- Standard_D4s_v6 (4 vCPU, 16 GB RAM): ~$0.192/hour per VM
- Standard_D8s_v6 (8 vCPU, 32 GB RAM): ~$0.384/hour per VM
- Coiled adds a small management fee on top (typically ~20% of compute cost).
- 100 workers × D4s_v6 × 1 hour ≈ $19 in compute + ~$4 Coiled fee = ~$23/hour.
- Actual calibration runs typically take 1–3 hours; workers are idle between batches.

**Coiled authentication** is done once via `coiled login` in a terminal. Credentials
are saved to `~/.config/dask/coiled.yaml`. These can be mounted into Docker containers
to authenticate headlessly.

---

### 2.3 Docker — Reproducible Worker Environments

**The problem Docker solves**: Coiled worker VMs are blank cloud machines. Before they
can run LASER simulations, they need R, MOSAIC, laser-cholera, numpy, and dozens of
other dependencies installed. Installing all of this at runtime would take 15–30 minutes
per VM and frequently fails due to network issues or version conflicts.

**What Docker is**: Docker is a containerization system. A *container* is a lightweight
isolated process that runs inside a pre-built, self-contained filesystem image. The
image contains the OS, all installed software, and configuration. Containers start in
seconds because everything is already installed.

**A Dockerfile** is a script that defines how to build an image, layer by layer:

```dockerfile
FROM rocker/geospatial:4.4          # Start from this base image (has R 4.4 + geospatial libs)
RUN apt-get install -y libhdf5-dev  # Add system libraries
RUN R -e "install.packages('arrow')"# Install R packages
RUN pip install dask coiled          # Install Python packages
```

Each `RUN` instruction adds a layer. Layers are cached: if you change only the last
line, Docker only re-runs that layer. The MOSAIC Dockerfile (`azure/Dockerfile`) builds
a ~8–12 GB image containing:
- Ubuntu base (via `rocker/geospatial:4.4`)
- R 4.4 with all MOSAIC dependencies
- MOSAIC R package (installed from GitHub main branch)
- Python virtualenv with laser-cholera, numpy, Dask, Coiled
- MOSAIC-data repository (cloned into the image)
- Thread safety env vars baked in (`OMP_NUM_THREADS=1`, etc.)

**The Docker workflow**:

```bash
# Build the image (takes ~30–60 minutes; done rarely)
docker build -f azure/Dockerfile -t mosaic-worker:latest .

# Tag for a registry
docker tag mosaic-worker:latest REGISTRY/mosaic-worker:latest

# Push to registry (so workers can pull it)
docker push REGISTRY/mosaic-worker:latest
```

**Why not Docker Hub?** Docker Hub rate-limits unauthenticated pulls to 100 per 6 hours
per IP. When 100 Coiled workers all pull simultaneously from the same Azure region,
they share IP addresses and exhaust this limit instantly. Azure Container Registry (ACR)
has no pull rate limits and keeps traffic within Azure.

---

### 2.4 Azure Container Registry (ACR)

**What it is**: ACR is Azure's private Docker registry service. It stores Docker images
and serves them to other Azure services (like Coiled workers) with no rate limits and
near-zero latency because the traffic stays within Azure's network.

**The MOSAIC ACR**:
- Registry: `idmmosaicacr` (managed by Tony Ting / IDM Research 2 subscription)
- Login server: `idmmosaicacr.azurecr.io`
- Image: `idmmosaicacr.azurecr.io/mosaic-worker:latest`
- Anonymous pull: enabled (workers don't need auth credentials to pull)
- Cost: ~$0.67/day (~$20/month) for Standard tier

**To push a new image** (when MOSAIC-pkg changes):
```bash
az acr login --name idmmosaicacr        # Authenticates your local Docker to the registry
docker push idmmosaicacr.azurecr.io/mosaic-worker:latest
```

---

### 2.5 reticulate — R's Bridge to Python

**What it is**: `reticulate` is an R package that embeds a Python interpreter inside
your R session. It lets you call Python functions, pass R objects to Python, and receive
Python objects back — all within a single R script.

**How MOSAIC uses it**:

```r
# Import Python modules as R objects
coiled  <- reticulate::import("coiled")
dd      <- reticulate::import("dask.distributed")

# Call Python functions with R arguments
cluster <- coiled$Cluster(
  n_workers = 10L,
  software  = "mosaic-acr-workers"
)
client  <- dd$Client(cluster)

# Use Python objects as if they were R objects
base_config_future <- client$scatter(base_config_py, broadcast = TRUE)
futures <- client$map(mosaic_worker$run_laser_sim, sim_ids, ...)
results <- client$gather(futures)
```

**Data type translation**: reticulate automatically converts between R and Python types:
- R list → Python dict
- R numeric vector → Python numpy array or list
- R matrix → Python numpy 2D array
- R character → Python str
- R integer → Python int

This translation is used in `run_MOSAIC.R` to convert the R config list into a
Python dict that `client$scatter()` can broadcast to workers.

**Python environment**: `coiled` and `dask.distributed` are included in MOSAIC's
standard Python environment (`~/.virtualenvs/r-mosaic`, defined in
`inst/py/environment.yml`). No separate environment is needed. `run_MOSAIC()`
calls `check_python_env()` at startup to confirm the correct environment is active —
the same environment that all other MOSAIC functions use for LASER simulations.

---

## 3. How the Pieces Fit Together in MOSAIC

### 3.1 The Two Execution Paths

MOSAIC now has two parallel execution strategies:

| | `run_MOSAIC()` | `run_MOSAIC()` |
|---|---|---|
| **Workers** | R processes (via `parallel::makeCluster`) | Python Dask workers (local or Coiled cloud) |
| **Scale** | 1 machine, up to ~32 cores | Many machines, up to 1000s of cores |
| **LASER called by** | R on each worker (via reticulate) | Python directly (no R on workers) |
| **Likelihood computed** | On each R worker | On the R orchestrator after gather |
| **Orchestrator** | R process (same machine as workers) | R process (local or Azure VM) |
| **Best for** | Development, small runs, no cloud setup | Large production calibrations |
| **Output format** | Identical | Identical |

Both functions produce the same output directory structure (`1_inputs/`, `2_calibration/`,
`3_results/`) and the same `samples.parquet` file. Post-processing code is unchanged.

### 3.2 What Runs Where

```
YOUR LOCAL MACHINE (or Azure orchestrator VM)
┌────────────────────────────────────────────────────────────┐
│  R session                                                  │
│  ├── MOSAIC::run_MOSAIC()                             │
│  ├── sample_parameters()    ← parameter draws, all in R    │
│  ├── client$scatter()       ← sends base_config to cloud   │
│  ├── client$map()           ← dispatches simulations        │
│  ├── client$gather()        ← waits for results             │
│  ├── calc_model_likelihood()← evaluates fit, all in R       │
│  └── writes .parquet files  ← saves results, all in R       │
│                                                             │
│  Python (via reticulate)                                    │
│  └── coiled, dask.distributed Client                        │
└────────────────────────────────────────────────────────────┘
                         │
                         │ Internet (encrypted)
                         │
           ┌─────────────┴──────────────┐
           │     COILED SCHEDULER VM     │
           │  (Azure Standard_D4s_v6)    │
           │  Dask scheduler process     │
           │  routes tasks to workers    │
           └─────────────┬──────────────┘
                         │
        ┌────────────────┼────────────────┐
        │                │                │
  [Worker VM 1]   [Worker VM 2]  ...  [Worker VM N]
  Docker container running:
  ├── Python interpreter
  ├── laser_cholera (LASER simulation engine)
  ├── mosaic_dask_worker.run_laser_sim()
  └── Returns {expected_cases, disease_deaths} to scheduler
```

### 3.3 What `mosaic_dask_worker.py` Does

This file (`inst/python/mosaic_dask_worker.py`) is the Python code that runs on each
Dask worker. Its single public function `run_laser_sim(sim_id, n_iterations,
sampled_params_json, base_config)` does exactly what the R worker function
`.mosaic_run_simulation_worker()` does in `run_MOSAIC.R`, but entirely in Python:

1. **Receives** the simulation ID, number of iterations, per-sim parameters (as JSON
   string), and the broadcast base config (as Python dict with numpy arrays).
2. **Merges** the per-sim parameters into a deep copy of the base config.
3. **Runs** `laser_cholera.run_model()` N times with different seeds.
4. **Returns** the resulting case and death arrays to the scheduler (which forwards them
   to R's `client$gather()`).
5. **Never** computes a likelihood — that stays in R.

**Why JSON for parameters?** The per-sim parameters (scalars and 1D vectors, ~few KB)
are serialized to a JSON string for transport. The large matrix fields (2D arrays like
`b_jt`, `psi_jt`) are handled differently: they live in the `base_config` that was
broadcast once via `client$scatter()`. An important exception: `psi_jt` is included
in the per-sim JSON because `apply_psi_star_calibration()` recalculates it per-simulation
using the sampled `psi_star_*` parameters — the broadcast copy would be stale.

### 3.4 The New R Functions

**`run_MOSAIC(config, priors, dir_output, control, dask_spec)`**
The main entry point. Takes the same `config` and `priors` as `run_MOSAIC()`, plus a
`dask_spec` list describing the cluster:

```r
dask_spec <- list(
  type     = "coiled",              # "coiled" or "scheduler" (existing Dask scheduler)
  n_workers = 10,                   # number of cloud VMs to spin up
  software  = "mosaic-acr-workers", # Coiled software environment name
  vm_types  = "Standard_D4s_v6",   # Azure VM type for workers
  region    = "westus2",            # Azure region
  idle_timeout = "2 hours"          # auto-shutdown after idle
)
```

**Two new upstream additions** (also backported to `run_MOSAIC()`):
- `control$io$save_simresults = TRUE`: Saves raw per-(sim, iteration, location, timestep)
  simulation output to `2_calibration/simulation_results/`. Used for validation — confirms
  that local and Dask paths produce numerically identical simulations.
- Chunked parquet loading in `.mosaic_load_and_combine_results()`: When runs produce
  40K+ individual sim parquet files, loading them all at once caused OOM errors. Files
  are now loaded in batches of 5,000 (configurable via `control$io$load_chunk_size`).

---

## 4. What Has Been Validated

Before merging, Tony ran three rounds of equivalence validation:

**Take 3 (March 26, 2026 — current, PASS)**:
- 50 simulations, ETH, 1 iteration
- Local: 8 PSOCK workers with `run_MOSAIC()`
- Dask: 5 × Standard_D4s_v6 with `run_MOSAIC()`
- **Parameter match: 56/56 exact** (all sampled parameters identical)
- **Simulation results**: Expected stochastic divergence only (~1–2 ULP floating-point
  difference in `psi_jt` from JSON serialization propagates through the stochastic
  model). This is correct and acceptable — BFRS calibration works on distributions of
  likelihoods, not individual trajectories.

---

## 5. Current Limitations and Future Work

**Memory bottleneck for large batches**: Workers currently return `expected_cases` and
`disease_deaths` arrays in-memory via `client$gather()`. For batches of >1,000
simulations, the scheduler holds all these arrays in RAM simultaneously. The code comments
(see `R/run_MOSAIC.R` roxygen) document a planned "Option B": workers write directly
to Azure Blob Storage and R reads back via `AzureStor`. This would remove the memory
ceiling entirely.

**ACR is managed by Tony's IDM subscription**: The Docker image at
`idmmosaicacr.azurecr.io/mosaic-worker:latest` and the ACR registry itself are
provisioned under the `IDM Research 2` Azure subscription in Tony's resource group
`rg-coiled-tting`. If you need to rebuild the image or provision your own infrastructure,
see the setup guide.

**The Coiled workspace** `idm-coiled-idmad-r2` and software environment
`mosaic-acr-workers` are also under Tony's account. New users need their own Coiled
account and software environment, or access to Tony's workspace.

---

## 6. File Map

| File | Role | Where it runs |
|------|------|----------------|
| `R/run_MOSAIC.R` | R orchestrator function | Local R / orchestrator VM |
| `inst/python/mosaic_dask_worker.py` | Python worker function | Coiled worker VMs |
| `azure/Dockerfile` | Worker image definition | Built locally, runs on worker VMs |
| `azure/environment.yml` | Local conda env (coiled+dask only) | Your local machine |
| `azure/mosaic_dask_fixed_test.R` | Production test script (50K sims, 100 workers) | Orchestrator VM |
| `azure/mosaic_dask_adaptive_test.R` | Adaptive test script | Orchestrator VM |
| `azure/validate_dask_local_equivalence.R` | Runs both local+Dask for comparison | Orchestrator VM |
| `azure/compare_validation_results.R` | Compares outputs from both paths | Anywhere |
| `azure/ACR_SETUP.md` | ACR creation and image update instructions | Reference |
| `azure/PROVISION_ORCHESTRATOR_AZURE.md` | Azure orchestrator VM setup | Reference |
| `azure/MOSAIC_DASK_GUIDE.md` | Quick-start usage guide | Reference |
