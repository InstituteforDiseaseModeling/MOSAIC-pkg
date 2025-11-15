# Deployment

## Overview

This guide shows how to deploy MOSAIC on remote virtual machines or
compute clusters for production use and large-scale simulations. MOSAIC
provides ready-to-use bash scripts that handle all installation steps
automatically.

The deployment scripts are **non-interactive** and execute remotely via
`ssh user@host 'bash -s' < script.sh`. All scripts are available in the
`vm/` directory of the MOSAIC-pkg repository.

------------------------------------------------------------------------

## Complete VM Setup

The `vm/setup_mosaic.sh` script installs R, system dependencies, MOSAIC
package, and Python components from scratch on Ubuntu/Debian systems.

### Execution Options

**Option 1: Direct from GitHub (recommended)**

No local files needed - downloads and executes the latest script
directly:

``` sh
curl -fsSL https://raw.githubusercontent.com/InstituteforDiseaseModeling/MOSAIC-pkg/main/vm/setup_mosaic.sh | ssh user@host 'bash -s'
```

**Option 2: From local MOSAIC repository**

If you have MOSAIC-pkg cloned locally (from Standard Setup in
Installation vignette):

``` sh
ssh user@host 'bash -s' < ~/MOSAIC/MOSAIC-pkg/vm/setup_mosaic.sh
```

### Script Contents

``` bash
#!/bin/bash
set -e  # Exit on any error

echo "======================================"
echo "MOSAIC VM Setup Script"
echo "======================================"

# Update system
echo "[1/7] Updating system packages..."
sudo apt-get update
sudo apt-get upgrade -y

# Install essential build tools
echo "[2/7] Installing essential build tools..."
sudo apt-get install -y \
  git \
  ca-certificates \
  build-essential \
  gfortran

# Install R and dependencies
echo "[3/7] Installing R (>= 4.1.1)..."
sudo apt-get install -y software-properties-common dirmngr
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
sudo apt-get update
sudo apt-get install -y r-base r-base-dev

# Install system dependencies for geospatial operations and R packages
echo "[4/7] Installing system libraries (GDAL, PROJ, GEOS, Arrow, HDF5)..."
sudo apt-get install -y \
  gdal-bin libgdal-dev \
  libproj-dev proj-bin \
  libgeos-dev libgeos++-dev \
  libudunits2-dev \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libfontconfig1-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  libfreetype6-dev \
  libpng-dev \
  libtiff5-dev \
  libjpeg-dev \
  libarrow-dev \
  libhdf5-dev \
  zlib1g-dev

# Install Python 3.9+
echo "[5/7] Installing Python 3.9+..."
sudo apt-get install -y \
  python3 \
  python3-pip \
  python3-venv \
  python3-dev

# Verify Python version
PYTHON_VERSION=$(python3 --version 2>&1 | grep -oP '\d+\.\d+' | head -1)
PYTHON_MAJOR=$(echo $PYTHON_VERSION | cut -d. -f1)
PYTHON_MINOR=$(echo $PYTHON_VERSION | cut -d. -f2)

if [ "$PYTHON_MAJOR" -lt 3 ] || ([ "$PYTHON_MAJOR" -eq 3 ] && [ "$PYTHON_MINOR" -lt 9 ]); then
  echo "WARNING: Python $PYTHON_VERSION detected, but >= 3.9 required"
  echo "Attempting to install Python 3.9..."
  sudo add-apt-repository -y ppa:deadsnakes/ppa
  sudo apt-get update
  sudo apt-get install -y python3.9 python3.9-venv python3.9-dev
  sudo update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.9 1
  echo "Python 3.9 installed and set as default"
else
  echo "Python $PYTHON_VERSION detected (OK)"
fi

# Install R packages system-wide (non-interactive)
echo "[6/7] Installing MOSAIC R package..."
sudo Rscript -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); \
  if (!requireNamespace('remotes', quietly = TRUE)) install.packages('remotes'); \
  remotes::install_github('InstituteforDiseaseModeling/MOSAIC-pkg', dependencies = TRUE, upgrade = 'always')"

# Install Python dependencies
echo "[7/7] Installing Python dependencies..."
sudo Rscript -e "MOSAIC::install_dependencies()"

# Verify installation
echo ""
echo "Verifying installation..."
Rscript -e "
  library(MOSAIC)
  result <- tryCatch({
    MOSAIC::check_dependencies()
    TRUE
  }, error = function(e) {
    cat('ERROR:', e\$message, '\n')
    FALSE
  })
  if (!result) quit(status = 1)
"

if [ $? -eq 0 ]; then
  echo ""
  echo "======================================"
  echo "Installation complete and verified!"
  echo "======================================"
else
  echo ""
  echo "======================================"
  echo "Installation completed with errors"
  echo "Please check the output above"
  echo "======================================"
  exit 1
fi
```

------------------------------------------------------------------------

## Minimal VM Setup

If R \>= 4.1.1 is already installed on your VM, use
`vm/setup_mosaic_minimal.sh`.

### Execution Options

**Option 1: Direct from GitHub (recommended)**

``` sh
curl -fsSL https://raw.githubusercontent.com/InstituteforDiseaseModeling/MOSAIC-pkg/main/vm/setup_mosaic_minimal.sh | ssh user@host 'bash -s'
```

**Option 2: From local MOSAIC repository**

``` sh
ssh user@host 'bash -s' < ~/MOSAIC/MOSAIC-pkg/vm/setup_mosaic_minimal.sh
```

### What it does

This script is identical to the complete setup but skips R installation
(step 2/6). It installs:

- System libraries (GDAL, PROJ, GEOS, UDUNITS, Python)
- MOSAIC R package from GitHub
- Python dependencies (laser-cholera)
- Verifies installation with
  [`check_dependencies()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/check_dependencies.md)

------------------------------------------------------------------------

## Parallel Execution

For running multiple simulations in parallel on a cluster:

``` r
# Set number of cores
library(MOSAIC)
options(mc.cores = parallel::detectCores() - 1)

# Run parallel simulations
ctrl <- mosaic_control_defaults(
  parallel = TRUE,
  n_cores = parallel::detectCores() - 1
)

results <- run_LASER(
  config = config_default,
  control = ctrl,
  n_sim = 100,
  seed = 123
)
```

------------------------------------------------------------------------

## Troubleshooting

**Python issues:**

``` r
# Check Python configuration
reticulate::py_config()

# Reset if needed
remove_MOSAIC_python_env()
install_dependencies()
```

------------------------------------------------------------------------

## Next Steps

After deployment, see the **“Running MOSAIC”** vignette to learn model
execution.
