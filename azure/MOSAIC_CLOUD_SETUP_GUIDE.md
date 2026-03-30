# MOSAIC Cloud Parallel Calibration — Setup Guide

**What this document covers**: Step-by-step instructions for setting up and running
`run_MOSAIC_dask()` from scratch. Each step explains what is happening and where.

**Before starting**: Read `MOSAIC_CLOUD_PRIMER.md` for conceptual background on all tools.

**Estimated setup time**: 2–3 hours (most of which is a one-time Docker build).

---

## Overview: The Three Environments

The system involves three distinct computing environments:

```
A. YOUR LOCAL MACHINE
   Has: R, MOSAIC, conda
   Does: R development, triggering runs
   Optionally: runs R orchestrator for small tests

B. COILED WORKER VMs (cloud, Azure westus2)
   Has: Docker image with MOSAIC + Python + LASER
   Does: Runs LASER simulations
   Provisioned automatically by Coiled when you call run_MOSAIC_dask()
   Billed per-second, terminated when run completes

C. AZURE CONTAINER REGISTRY (ACR)
   Has: The Docker image (stored artifact)
   Does: Serves the image to worker VMs when they start up
   Persistent, low cost (~$20/month)
```

For large production runs, a fourth environment is recommended:

```
D. AZURE ORCHESTRATOR VM (optional but recommended for production)
   Has: Docker, Coiled CLI, access to ACR
   Does: Runs the R orchestrator process inside Docker
   Use: Avoids keeping your laptop running for multi-hour calibrations
   Cost: ~$0.15/hour (Standard_L4s_v4), stop when not in use
```

---

## Part 1: Local Machine Setup

This sets up your local machine to develop with MOSAIC and optionally run small
calibration tests.

### Step 1.1 — Install conda (if not already installed)

Conda manages Python environments on your local machine. We need it to create an
isolated environment with `coiled` and `dask`.

```bash
# Check if you have conda
conda --version

# If not, install Miniforge (recommended — smaller, conda-forge by default)
# On macOS (Apple Silicon):
curl -L -O "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-MacOSX-arm64.sh"
bash Miniforge3-MacOSX-arm64.sh

# On macOS (Intel):
curl -L -O "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-MacOSX-x86_64.sh"
bash Miniforge3-MacOSX-x86_64.sh

# On Linux:
curl -L -O "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-Linux-x86_64.sh"
bash Miniforge3-Linux-x86_64.sh
```

Follow the prompts and allow conda to initialize your shell. Restart your terminal.

---

### Step 1.2 — Create the local orchestration conda environment

This environment is tiny — it only contains `coiled`, `dask`, and `pyarrow`. All
MOSAIC dependencies live inside the Docker image on workers, not here.

```bash
# Navigate to MOSAIC-pkg
cd ~/MOSAIC/MOSAIC-pkg

# Create the environment from the provided spec
conda env create -f azure/environment.yml

# This creates an environment named "mosaic-local-orchestrate"
# Verify it was created:
conda env list
# You should see "mosaic-local-orchestrate" in the list
```

What `azure/environment.yml` installs: Python 3.11, coiled, dask, distributed,
numpy, pandas, pyarrow. That's it — a minimal environment for orchestration.

> **If you already have a conda environment with coiled and dask** (e.g. `mosaic-coiled`
> from previous work), you can reuse it instead of creating a new one. The environment
> name just needs to have `coiled` and `dask.distributed` importable.

---

### Step 1.3 — Create a Coiled account and log in

Coiled provisions your cloud workers. You need a free account.

**Step 1.3a: Create account**

Go to https://coiled.io and sign up. You can authenticate via GitHub or Google.
You will be prompted to connect a cloud provider — choose **Azure** and follow the
steps to link your Azure subscription. This grants Coiled permission to create VMs in
your subscription.

> **IDM users**: Tony Ting has an existing workspace `idm-coiled-idmad-r2` under the
> IDM Research 2 Azure subscription. Ask Tony to add you to this workspace instead of
> creating your own, to share the existing Docker image and infrastructure.

**Step 1.3b: Install the Coiled CLI and log in**

```bash
# Activate your orchestration environment first
conda activate mosaic-local-orchestrate

# Log in to Coiled (opens a browser window for OAuth)
coiled login

# Credentials are saved to: ~/.config/dask/coiled.yaml
# Verify you're logged in:
coiled whoami
```

**What `coiled login` does**: Opens a browser where you authenticate with your Coiled
account. After approval, a token is saved to `~/.config/dask/coiled.yaml`. This file
is used by both the `coiled` CLI and by Python/R code that imports `coiled`.

---

### Step 1.4 — Point reticulate at the orchestration Python environment

When you run `run_MOSAIC_dask()` from R, it uses reticulate to import Python modules
(`coiled`, `dask.distributed`). Reticulate needs to find the right Python environment.

**The simplest approach**: always activate the conda environment before starting R:

```bash
conda activate mosaic-local-orchestrate
R
```

**To make this permanent** (add to `~/.Rprofile`):

```r
# ~/.Rprofile
local({
  coiled_python <- path.expand("~/.conda/envs/mosaic-local-orchestrate/bin/python")
  if (file.exists(coiled_python)) {
    Sys.setenv(RETICULATE_PYTHON = coiled_python)
  }
})
```

**Verify the setup in R**:

```r
library(reticulate)
reticulate::import("coiled")          # Should print: Module(coiled)
reticulate::import("dask.distributed") # Should print: Module(dask.distributed)
```

If you see `ImportError: No module named 'coiled'`, the wrong Python is active.
Check `reticulate::py_config()` to see which Python reticulate is using.

---

## Part 2: Docker Image (one-time build, rebuild when MOSAIC changes)

The Docker image is the pre-built, self-contained environment that runs on every
Coiled worker VM. Building it is the most time-consuming step but only needs to be
redone when the MOSAIC package or its Python dependencies change.

> **If the image already exists in ACR** (`idmmosaicacr.azurecr.io/mosaic-worker:latest`)
> and MOSAIC-pkg has not changed since it was built, **skip directly to Part 3**.

### Step 2.1 — Install Docker Desktop

Docker Desktop runs the Docker daemon on your local machine. You need it to build images.

- macOS/Windows: Download from https://www.docker.com/products/docker-desktop/
- Linux: Follow https://docs.docker.com/engine/install/ for your distro

Verify after installation:
```bash
docker --version  # should print something like "Docker version 26.x.x"
docker run hello-world  # should pull and run a test container
```

---

### Step 2.2 — Install the Azure CLI

The Azure CLI (`az`) lets you interact with Azure services, including logging into ACR.

```bash
# macOS (via Homebrew)
brew install azure-cli

# Linux (Ubuntu/Debian)
curl -sL https://aka.ms/InstallAzureCLIDeb | sudo bash

# Verify
az --version
```

---

### Step 2.3 — Build the Docker image

This step downloads the base image (rocker/geospatial, ~1.5 GB) and installs everything
on top. It runs entirely on your local machine.

```bash
cd ~/MOSAIC/MOSAIC-pkg

# Build the image. This takes 30–60 minutes.
# -f: path to the Dockerfile
# -t: name and tag for the resulting image
# .: the "build context" (files Docker can access; MOSAIC-pkg root)
docker build -f azure/Dockerfile -t mosaic-worker:latest .
```

**What happens during the build** (each `RUN` in the Dockerfile is one layer):
1. Starts from `rocker/geospatial:4.4` — Ubuntu with R 4.4 and geospatial libraries
2. Installs system libraries (libhdf5, python3, fonts, git)
3. Ensures Python >= 3.9
4. Installs critical R packages (arrow, hdf5r, exactextractr, etc.)
5. Installs MOSAIC R package from GitHub (`main` branch)
6. Creates the MOSAIC Python virtualenv (`MOSAIC::install_dependencies()`)
7. Strips TensorFlow (not needed for workers; saves ~3 GB)
8. Installs dask, distributed, coiled in the virtualenv
9. Fixes OpenSSL and libexpat version mismatches (library compatibility)
10. Verifies MOSAIC installation (`MOSAIC::check_dependencies()`)
11. Clones MOSAIC-data repository into `/workspace/MOSAIC/MOSAIC-data`
12. Sets up directory structure and env vars

**If the build fails**: Each layer is cached. Re-running `docker build` picks up where
it left off (at the first changed/failed layer). Common failure points:
- GitHub rate limit on `remotes::install_github()` — wait 10 minutes and retry
- R package compilation errors — check system dependency versions in the error

---

### Step 2.4 — Verify the image locally

Before pushing, confirm the image actually works:

```bash
# Test R can load MOSAIC
docker run --rm mosaic-worker:latest \
  R -e "library(MOSAIC); packageVersion('MOSAIC')"
# Expected: prints version like [1] '0.17.34'

# Test laser.cholera is importable
docker run --rm mosaic-worker:latest \
  python3 -c "import laser.cholera; print('laser.cholera OK')"
# Expected: prints "laser.cholera OK"

# Test dask and coiled are importable
docker run --rm mosaic-worker:latest \
  python3 -c "import coiled, dask.distributed; print('coiled + dask OK')"
# Expected: prints "coiled + dask OK"
```

---

### Step 2.5 — Push to Azure Container Registry

Workers pull the image from ACR when they start. You must push before workers can use it.

```bash
# Log in to ACR (requires Azure CLI and Azure account with AcrPush role on idmmosaicacr)
az login                          # if not already logged in to Azure
az acr login --name idmmosaicacr  # authenticates Docker to the registry

# Tag the image for the ACR repository
docker tag mosaic-worker:latest idmmosaicacr.azurecr.io/mosaic-worker:latest

# Push (uploads all changed layers; ~10–20 minutes on first push, faster on updates)
docker push idmmosaicacr.azurecr.io/mosaic-worker:latest

# Verify the push succeeded and note the timestamp
az acr repository show-tags \
  --name idmmosaicacr \
  --repository mosaic-worker \
  --detail \
  --orderby time_desc
```

> **Access**: You need `AcrPush` role on `idmmosaicacr`. Ask Tony to grant this if
> needed (see `PROVISION_ORCHESTRATOR_AZURE.md` for the `az role assignment` command).

---

### Step 2.6 — Update the Coiled software environment

A Coiled software environment is a cloud-side record saying "workers should use this
Docker image." After pushing a new image, you must update this record so new workers
pick up the latest image digest.

```bash
# Activate your local conda env (needs the coiled Python package)
conda activate mosaic-local-orchestrate

# Update the Coiled software environment to the new image
python3 -c "
import coiled
coiled.create_software_environment(
    name='mosaic-acr-workers',
    container='idmmosaicacr.azurecr.io/mosaic-worker:latest',
    force_rebuild=True,
    workspace='idm-coiled-idmad-r2'   # omit if using your own workspace
)
print('Coiled environment updated.')
"
```

**What this does**: Tells Coiled's servers to pull the new image digest and register
it under the name `mosaic-acr-workers`. Workers started with `software="mosaic-acr-workers"`
will now use the updated image.

**Verify**:
```bash
coiled env list
# Should show "mosaic-acr-workers" with a recent timestamp
```

---

## Part 3: Running Your First Dask Calibration

At this point:
- Your local R session has reticulate pointing at Python with coiled+dask
- A Docker image is in ACR and registered as a Coiled software environment
- You are authenticated to Coiled

### Step 3.1 — Minimal test run (from local R)

This runs 50 simulations for Ethiopia across 2 cloud workers. Takes ~10–15 minutes
and costs roughly $0.50.

```r
library(MOSAIC)
set_root_directory("~/MOSAIC")

# Load config and priors for Ethiopia
config <- get_location_config(iso = "ETH")
priors <- get_location_priors(iso = "ETH")

# Control: fixed mode, small run
ctrl <- mosaic_control_defaults(
  calibration = list(
    n_simulations = 50,   # exact number of simulations (fixed mode)
    n_iterations  = 1,    # 1 LASER run per simulation (fast for testing)
    batch_size    = 50    # all in one batch
  ),
  paths = list(plots = FALSE)
)

# Dask cluster specification
dask_spec <- list(
  type         = "coiled",
  n_workers    = 2,
  software     = "mosaic-acr-workers",
  vm_types     = "Standard_D4s_v6",   # 4 vCPU, 16 GB per worker
  region       = "westus2",
  idle_timeout = "30 minutes"
)

# Run!
result <- run_MOSAIC_dask(
  config     = config,
  priors     = priors,
  dir_output = "./output/ETH_dask_test",
  control    = ctrl,
  dask_spec  = dask_spec
)
```

**What you will see in the R console**:

```
[2026-03-29 10:00:01] Starting MOSAIC Dask calibration
[2026-03-29 10:00:02] Creating Coiled cluster: 2 workers (Standard_D4s_v6, westus2)
[2026-03-29 10:01:30] Dask dashboard: https://cloud.coiled.io/clusters/xxxxx/status
[2026-03-29 10:01:31] Uploading mosaic_dask_worker.py to all workers...
[2026-03-29 10:01:35] Broadcasting base config to workers (1 location, 1155 time steps)...
[2026-03-29 10:01:36] [FIXED MODE] Running exactly 50 simulations
[2026-03-29 10:01:36]   Sampling 50 parameter sets in R...
[2026-03-29 10:01:37]   Serializing 50 parameter sets to JSON...
[2026-03-29 10:01:37]   Submitting 50 futures to Dask cluster...
[2026-03-29 10:01:38]   Waiting for 50 Dask futures...
[2026-03-29 10:09:45]   Gathering results: 50/50 successful (100.0%) in 8.1 min
[2026-03-29 10:09:46]   Computing likelihoods for 50 simulations...
[2026-03-29 10:09:47]   Writing 50 parquet files...
[2026-03-29 10:09:48] Fixed batch complete: 50 simulations in 8.2 min
[2026-03-29 10:09:48] Dask calibration complete. Shutting down cluster.
```

**What happens step by step**:
1. R creates a Coiled cluster — Coiled spins up 2 Azure VMs running the Docker image.
   The VMs start a Dask worker process each and connect to a Dask scheduler.
   *This takes 1–2 minutes.*
2. R uploads `mosaic_dask_worker.py` to all workers via `client$upload_file()`.
3. R serializes the base config (the large matrices) and broadcasts to all workers
   via `client$scatter()`. Workers cache this in memory.
4. R calls `sample_parameters()` 50 times to draw parameter sets.
5. R serializes each parameter set to JSON and calls `client$map()` to dispatch
   50 `run_laser_sim()` tasks across the 2 workers.
   *Workers immediately begin running LASER simulations.*
6. R calls `client$gather()` and waits. Workers run LASER, each returning
   `{expected_cases, disease_deaths}` arrays.
7. R receives all 50 results, computes likelihoods with `calc_model_likelihood()`,
   writes one parquet per simulation, then combines and runs all standard
   post-processing (posterior estimation, diagnostics, etc.).
8. Cluster shuts down automatically.

**Verify results**:

```r
# Read the combined samples file
df <- arrow::read_parquet("./output/ETH_dask_test/2_calibration/samples.parquet")
cat("Simulations:", nrow(df), "\n")
cat("Finite likelihoods:", sum(is.finite(df$likelihood)), "\n")
cat("Likelihood range:", paste(round(range(df$likelihood[is.finite(df$likelihood)]), 1), collapse=" to "), "\n")
```

**Expected**: 50 rows, most likelihoods finite and negative (e.g. -500 to -200).

---

### Step 3.2 — Monitoring the run

**Dask dashboard**: The log prints a URL like `https://cloud.coiled.io/clusters/xxxxx/status`.
Open it in a browser to see:
- Worker status (active, idle)
- Task progress (queued, running, completed)
- Per-worker CPU and memory usage
- Task timing histograms

**Coiled dashboard** at https://cloud.coiled.io shows all your clusters (running and
historical) with cost estimates.

---

### Step 3.3 — Production calibration run

For a real calibration with adaptive convergence:

```r
library(MOSAIC)
set_root_directory("~/MOSAIC")

config <- get_location_config(iso = "ETH")
priors <- get_location_priors(iso = "ETH")

ctrl <- mosaic_control_defaults(
  calibration = list(
    n_simulations = NULL,   # NULL = adaptive mode: runs until convergence
    n_iterations  = 3,      # 3 stochastic LASER runs per simulation (more stable likelihood)
    batch_size    = 500     # 500 simulations per batch
  ),
  targets = list(
    target_r2 = 0.5         # stop when R² reaches 0.5
  )
)

dask_spec <- list(
  type         = "coiled",
  n_workers    = 20,
  software     = "mosaic-acr-workers",
  vm_types     = "Standard_D4s_v6",
  region       = "westus2",
  idle_timeout = "2 hours"
)

result <- run_MOSAIC_dask(
  config     = config,
  priors     = priors,
  dir_output = "./output/ETH_production",
  control    = ctrl,
  dask_spec  = dask_spec
)
```

**Choosing the right number of workers and VM size**:

| Scenario | n_workers | vm_types | batch_size | Notes |
|----------|-----------|----------|------------|-------|
| Test (< 100 sims) | 2 | D4s_v6 | same as n_sims | Minimal cost |
| Small run (500–2K sims) | 5–10 | D4s_v6 | 500 | ~$3–15 |
| Production (5K–50K sims) | 20–100 | D4s_v6 | 500–1000 | ~$20–150 |
| Large multi-country | 50–100 | D8s_v6 | 1000 | More RAM for larger configs |

**Benchmark results** (from `PROVISION_ORCHESTRATOR_AZURE.md`):
- Standard_D4s_v6: mean 4.0 sec/sim, D8s_v6: mean 3.9 sec/sim (nearly identical)
- D4s_v6 is the recommended choice: best cost-performance ratio

---

## Part 4: Running from an Azure Orchestrator VM (Production Recommended)

For runs that take 2+ hours, it's better to run the R orchestrator on an Azure VM
rather than your laptop. This frees your local machine and avoids network interruptions.

### Step 4.1 — Provision the VM

This creates an Azure VM with local NVMe SSD for fast parquet I/O:

```bash
# Log in to Azure
az login

# Create the VM (takes ~3 minutes)
az vm create \
  --resource-group rg-coiled-tting \
  --name mosaic-orchestrator \
  --image Canonical:ubuntu-24_04-lts:server:latest \
  --size Standard_L4s_v4 \
  --admin-username YOUR_USERNAME \
  --generate-ssh-keys \
  --os-disk-size-gb 128 \
  --location westus2 \
  --public-ip-sku Standard \
  --nsg-rule SSH

# Note the public IP address from the output (or find it with:)
az vm show --resource-group rg-coiled-tting --name mosaic-orchestrator \
  --show-details --query publicIps -o tsv
```

---

### Step 4.2 — Install Docker on the VM

```bash
# SSH into the VM
ssh YOUR_USERNAME@VM_IP_ADDRESS

# Install Docker
curl -fsSL https://get.docker.com | sudo sh
sudo usermod -aG docker $USER

# Log out and back in so the docker group takes effect
exit
ssh YOUR_USERNAME@VM_IP_ADDRESS

# Verify
docker run hello-world
```

---

### Step 4.3 — Install Azure CLI and authenticate to ACR

```bash
# Install Azure CLI
curl -sL https://aka.ms/InstallAzureCLIDeb | sudo bash

# Log in to Azure (opens a browser auth link — paste the code in your browser)
az login

# Authenticate Docker to the ACR registry
az acr login --name idmmosaicacr

# Pull the MOSAIC worker image to verify access
docker pull idmmosaicacr.azurecr.io/mosaic-worker:latest
```

---

### Step 4.4 — Set up Coiled authentication on the VM

```bash
# Install coiled (system Python is fine for the CLI)
sudo apt install -y python3-pip
pip install coiled --break-system-packages
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc

# Log in to Coiled
coiled login
# Follow the browser link printed in the terminal
```

---

### Step 4.5 — Mount the NVMe SSD for fast I/O

The Standard_L4s_v4 VM has local NVMe SSDs that are much faster than the OS disk
for writing thousands of parquet files. **Important: this data is wiped when the VM
is deallocated — save results elsewhere before stopping the VM.**

```bash
# See available disks
lsblk
# nvme0n1 = OS disk (DO NOT format)
# nvme1n1 = local NVMe SSD (447 GB) — format this one

# Format and mount
sudo mkfs.ext4 /dev/nvme1n1
sudo mkdir -p /mnt/nvme
sudo mount /dev/nvme1n1 /mnt/nvme
sudo chown $USER:$USER /mnt/nvme
mkdir -p /mnt/nvme/output
```

---

### Step 4.6 — Clone repositories

```bash
git clone https://github.com/InstituteforDiseaseModeling/MOSAIC-pkg ~/MOSAIC/MOSAIC-pkg
git clone https://github.com/InstituteforDiseaseModeling/MOSAIC-data ~/MOSAIC/MOSAIC-data
```

---

### Step 4.7 — Run the calibration via Docker

This runs the R orchestrator *inside* the Docker container, which has all R dependencies
pre-installed. The Coiled credentials are mounted read-only so the orchestrator can
authenticate headlessly:

```bash
docker run --rm \
  -v ~/MOSAIC/MOSAIC-pkg:/src/MOSAIC-pkg \
  -v ~/MOSAIC/MOSAIC-data:/workspace/MOSAIC/MOSAIC-data \
  -v /mnt/nvme/output:/workspace/output \
  -v ~/.config/dask:/root/.config/dask:ro \
  idmmosaicacr.azurecr.io/mosaic-worker:latest \
  bash -c "R CMD INSTALL /src/MOSAIC-pkg && Rscript /src/MOSAIC-pkg/azure/mosaic_dask_fixed_test.R"
```

**Breaking down this command**:
- `-v ~/MOSAIC/MOSAIC-pkg:/src/MOSAIC-pkg` — mounts your local MOSAIC-pkg source into
  the container. The `R CMD INSTALL /src/MOSAIC-pkg` at the end installs the *current*
  version of MOSAIC into the container's R library (~60 seconds). This means the
  container always runs the latest code without requiring a Docker rebuild.
- `-v ~/MOSAIC/MOSAIC-data:/workspace/MOSAIC/MOSAIC-data` — mounts the data repo
  (overrides the version baked into the image, ensures you have the latest data).
- `-v /mnt/nvme/output:/workspace/output` — results are written to the fast NVMe SSD.
- `-v ~/.config/dask:/root/.config/dask:ro` — mounts Coiled credentials read-only.

**To use a custom script** instead of the test script:

```bash
docker run --rm \
  -v ~/MOSAIC/MOSAIC-pkg:/src/MOSAIC-pkg \
  -v ~/MOSAIC/MOSAIC-data:/workspace/MOSAIC/MOSAIC-data \
  -v /mnt/nvme/output:/workspace/output \
  -v ~/.config/dask:/root/.config/dask:ro \
  idmmosaicacr.azurecr.io/mosaic-worker:latest \
  bash -c "R CMD INSTALL /src/MOSAIC-pkg && Rscript /src/MOSAIC-pkg/my_run_script.R"
```

---

### Step 4.8 — Copy results off the VM before stopping

The NVMe SSD is wiped when the VM is deallocated:

```bash
# From your LOCAL machine (not the VM):
scp -r YOUR_USERNAME@VM_IP_ADDRESS:/mnt/nvme/output ./local_results/

# Or use rsync for large outputs:
rsync -avz --progress YOUR_USERNAME@VM_IP_ADDRESS:/mnt/nvme/output/ ./local_results/
```

---

### Step 4.9 — Stop the VM to avoid charges

```bash
# Stop the VM (VM is deallocated — no compute charges)
az vm deallocate --resource-group rg-coiled-tting --name mosaic-orchestrator

# Start again later:
az vm start --resource-group rg-coiled-tting --name mosaic-orchestrator
# Note: re-mount NVMe after each restart (see Step 4.5)
```

---

## Part 5: Rebuilding the Docker Image (When MOSAIC Changes)

When you make significant changes to MOSAIC-pkg (new functions, changed LASER interface,
updated Python dependencies), rebuild and push the image.

```bash
cd ~/MOSAIC/MOSAIC-pkg

# 1. Build new image
docker build -f azure/Dockerfile -t mosaic-worker:latest .

# 2. Verify it works
docker run --rm mosaic-worker:latest \
  R -e "library(MOSAIC); packageVersion('MOSAIC')"

# 3. Tag for ACR
docker tag mosaic-worker:latest idmmosaicacr.azurecr.io/mosaic-worker:latest

# 4. Push
az acr login --name idmmosaicacr
docker push idmmosaicacr.azurecr.io/mosaic-worker:latest

# 5. Update the Coiled software environment
conda activate mosaic-local-orchestrate
python3 -c "
import coiled
coiled.create_software_environment(
    name='mosaic-acr-workers',
    container='idmmosaicacr.azurecr.io/mosaic-worker:latest',
    force_rebuild=True,
    workspace='idm-coiled-idmad-r2'
)
print('Done.')
"
```

> **Tip**: For rapid iteration during development, you don't need to rebuild the Docker
> image. The `R CMD INSTALL /src/MOSAIC-pkg` in the docker run command reinstalls
> the latest R code at runtime. Only rebuild the image when Python dependencies change
> (new packages in `inst/py/environment.yml`) or when LASER itself is updated.

---

## Part 6: Troubleshooting

### "ImportError: No module named 'coiled'" in R

Reticulate is using the wrong Python. Fix:

```bash
conda activate mosaic-local-orchestrate
R   # start R from this activated environment
```

Or in R before calling the function:
```r
Sys.setenv(RETICULATE_PYTHON = path.expand("~/.conda/envs/mosaic-local-orchestrate/bin/python"))
library(reticulate)
reticulate::import("coiled")  # should now work
```

---

### "Cannot find inst/python/mosaic_dask_worker.py"

The MOSAIC package was installed before `mosaic_dask_worker.py` was added. Reinstall:

```r
devtools::install()  # from the MOSAIC-pkg directory
```

---

### Workers return `success=False` with an import error

The `laser.cholera` module isn't importable on workers. Verify the Docker image:

```bash
docker run --rm idmmosaicacr.azurecr.io/mosaic-worker:latest \
  python3 -c "import laser.cholera; print('OK')"
```

If this fails, the image is stale or broken — rebuild it (Part 5).

---

### "You have reached your pull rate limit" (Docker Hub error)

This happens if using a Docker Hub image instead of ACR. The default `software` in
`run_MOSAIC_dask()` is `"mosaic-acr-workers"` which uses ACR. If you explicitly
specified a Docker Hub image, switch to ACR.

---

### Cluster starts but workers are idle (all sims stuck)

Check the Dask dashboard URL printed in the log. The most common cause is `client$scatter()`
taking longer than expected for large configs. For a 40-country run with 5+ years of
daily data, `base_config` is ~100 MB and broadcast takes 30–90 seconds on the first
batch. This is normal.

---

### Azure quota exceeded

Coiled workers need Azure VM quota. If you get quota errors:

```bash
# List your current quota in westus2
az vm list-usage --location westus2 --output table | grep -i "Standard DSv6"

# Request a quota increase in the Azure Portal:
# Portal > Subscriptions > [your sub] > Usage + quotas > Request increase
```

---

### Run interrupted — how to check costs incurred

```bash
# List Coiled clusters (running and recently stopped)
coiled cluster list

# Delete any lingering running clusters
python3 -c "import coiled; coiled.delete_cluster('CLUSTER_NAME')"
```

Costs for interrupted runs are visible in the Coiled dashboard at https://cloud.coiled.io
and in your Azure Cost Management portal.

---

## Quick Reference

```bash
# Activate local Python env
conda activate mosaic-local-orchestrate

# Verify Coiled auth
coiled whoami

# List Coiled software environments
coiled env list

# Build and push Docker image
docker build -f azure/Dockerfile -t mosaic-worker:latest .
az acr login --name idmmosaicacr
docker tag mosaic-worker:latest idmmosaicacr.azurecr.io/mosaic-worker:latest
docker push idmmosaicacr.azurecr.io/mosaic-worker:latest

# Update Coiled env after new image
python3 -c "import coiled; coiled.create_software_environment(name='mosaic-acr-workers', container='idmmosaicacr.azurecr.io/mosaic-worker:latest', force_rebuild=True)"

# Start/stop orchestrator VM
az vm start   --resource-group rg-coiled-tting --name mosaic-orchestrator
az vm deallocate --resource-group rg-coiled-tting --name mosaic-orchestrator

# Run on orchestrator VM (after SSH in and re-mounting NVMe)
docker run --rm \
  -v ~/MOSAIC/MOSAIC-pkg:/src/MOSAIC-pkg \
  -v ~/MOSAIC/MOSAIC-data:/workspace/MOSAIC/MOSAIC-data \
  -v /mnt/nvme/output:/workspace/output \
  -v ~/.config/dask:/root/.config/dask:ro \
  idmmosaicacr.azurecr.io/mosaic-worker:latest \
  bash -c "R CMD INSTALL /src/MOSAIC-pkg && Rscript /src/MOSAIC-pkg/azure/mosaic_dask_fixed_test.R"
```
