# Installation

## Overview

MOSAIC has two installation levels depending on your needs:

| Setup | Purpose | Requirements |
|:---|:---|:---|
| **Basic** | Run models with pre-configured data | R + internet |
| **Developer** | Full access: data collection, prior estimation, development | R + git + system libraries |

Most users should start with **Basic**.

**Python is optional.** As of v0.68.0 the transmission engine is pure R,
so simulation and calibration -
[`run_simulation()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_simulation.md),
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md),
and everything downstream of them - need no Python at all. A Python
environment is required only to *re-fit* the environmental-suitability
(psi) model with
[`est_suitability()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md),
which uses TensorFlow/Keras. Pre-computed suitability values ship with
the package, so you can run and calibrate models without ever installing
it. See [Optional: Python for environmental
suitability](#optional-python-for-environmental-suitability).

------------------------------------------------------------------------

## Basic Setup

Install the R package from GitHub. This is all you need to run models
with pre-configured parameters and data - there is no Python step.

``` r

# Install remotes if not already installed
if (!require("remotes")) install.packages("remotes")

# Install MOSAIC R package
remotes::install_github("InstituteforDiseaseModeling/MOSAIC-pkg", upgrade = "never", force = TRUE)

# Load the package (prints the banner below)
library(MOSAIC)
```

**Successful installation output:**

When MOSAIC loads successfully, you should see:

     __  __   ___   ____     _     ___  ____       __      ___    _____  _____   _____ ___
    |  \/  | / _ \ / ___|   / \   |_ _|/ ___|   __/ /_    / /    /   |  / ___/ / ____// __ \
    | |\/| || | | |\___ \  / _ \   | || |      /_  __/   / /    / /| |  \__ \ / __/  / /_/ /
    | |  | || |_| | ___) |/ ___ \  | || |___    /_/     / /___ / ___ | ___/ // /___ / _, _/
    |_|  |_| \___/ |____//_/   \_\|___|\____|          /_____//_/  |_|/____//_____//_/ |_|

    Welcome to the Metapopulation Outbreak Simulation with Agent-based Implementation
    for Cholera (MOSAIC)!

    Version: <the version you installed>

------------------------------------------------------------------------

## Optional: Python for environmental suitability

Skip this section unless you intend to re-fit the
environmental-suitability (psi) model.
[`est_suitability()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
trains an LSTM in TensorFlow/Keras and is the only function in the
package that touches Python. Everything else - including
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md) -
reads psi from pre-computed values and never initialises a Python
interpreter.

``` r

library(MOSAIC)

# Build the conda environment (installs Miniconda if you do not have conda)
install_dependencies()

# Verify it
check_python_env()      # interpreter, paths, reticulate binding
check_dependencies()    # package versions + a capabilities summary
```

[`check_dependencies()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/check_dependencies.md)
ends with a capabilities summary. Simulation and calibration are always
reported as available because they are pure R; only the suitability line
depends on what it finds. If this environment is missing or broken, the
cost is
[`est_suitability()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
and nothing else.

------------------------------------------------------------------------

## Developer Setup

Clone the full repository structure for data processing, parameter
estimation, and code development. Requires system dependencies (GDAL,
PROJ, GEOS, UDUNITS).

**Note**: We recommend creating a `~/MOSAIC` directory to organize all
MOSAIC repositories in one location.

### Install System Dependencies

**macOS:**

``` sh
brew install gdal proj geos udunits
```

**Ubuntu/Debian:**

``` sh
sudo apt-get install -y gdal-bin libgdal-dev libproj-dev libgeos-dev libudunits2-dev
```

### Clone Repositories

``` sh
mkdir -p ~/MOSAIC && cd ~/MOSAIC
git clone git@github.com:InstituteforDiseaseModeling/MOSAIC-pkg.git
git clone git@github.com:InstituteforDiseaseModeling/MOSAIC-data.git
git clone git@github.com:InstituteforDiseaseModeling/MOSAIC-docs.git
git clone git@github.com:InstituteforDiseaseModeling/ees-cholera-mapping.git
```

### Install and Configure

``` r

# Install R package from source (upgrade = "never" avoids prompts)
devtools::install("~/MOSAIC/MOSAIC-pkg", upgrade = "never")

# Set paths
library(MOSAIC)
set_root_directory("~/MOSAIC")

# Only if you will re-fit the suitability model -- see the optional section above
# install_dependencies()
# check_dependencies()
```

### Development Workflow

``` r

# Load for development
devtools::load_all("~/MOSAIC/MOSAIC-pkg")

# Run tests
devtools::test()

# Build documentation
devtools::document()
pkgdown::build_site()
```

### Optional: Mobility Package

The `mobility` package is only needed if you want to regenerate mobility
estimates from scratch. Most users can skip this - pre-computed mobility
files are included in `model/input/`.

``` r

# Only install if you need to regenerate mobility data
# Requires JAGS system library

# macOS:
# brew install jags

# Ubuntu/Debian:
# sudo apt-get install -y jags libjags-dev

# Then install R packages:
install.packages("rjags")
remotes::install_github("COVID-19-Mobility-Data-Network/mobility")
```

**When is this needed?** Only if you’re calling
[`MOSAIC::est_mobility()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_mobility.md)
to regenerate mobility matrices. Otherwise, use the pre-computed files
included with the package.

------------------------------------------------------------------------

## Troubleshooting

**Python issues** (only affects
[`est_suitability()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md);
simulation and calibration are pure R):

``` r

# Check Python environment and dependencies
check_python_env()
check_dependencies()

# Check detailed Python configuration
reticulate::py_config()

# Reset if needed
remove_python_env()
install_dependencies(force = TRUE)
```

**Path issues (Developer only):**

``` r

# Check and reset root directory
getOption("root_directory")
set_root_directory("~/MOSAIC")
```

------------------------------------------------------------------------

## Next Steps

- **Running MOSAIC**: Learn how to execute models and work with outputs
- **Deployment**: Set up MOSAIC on remote VMs or compute clusters
