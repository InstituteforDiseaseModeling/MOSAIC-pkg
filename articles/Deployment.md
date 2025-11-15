# Deployment

## Overview

Deploy MOSAIC on remote virtual machines or compute clusters using
transportable bash scripts. All scripts are non-interactive and can be
executed via SSH or cloud-init.

Setup scripts are available in the `vm/` directory of the MOSAIC-pkg
repository.

------------------------------------------------------------------------

## Complete VM Setup

The `vm/setup_mosaic.sh` script installs R, system dependencies, MOSAIC
package, and Python components from scratch on Ubuntu/Debian systems.

**Execute via SSH:**

``` sh
ssh user@host 'bash -s' < vm/setup_mosaic.sh
```

**Or run directly on VM:**

``` sh
bash vm/setup_mosaic.sh
```

**Script contents:**

------------------------------------------------------------------------

## Minimal VM Setup

If R \>= 4.1.1 is already installed on your VM, use
`vm/setup_mosaic_minimal.sh`:

**Execute via SSH:**

``` sh
ssh user@host 'bash -s' < vm/setup_mosaic_minimal.sh
```

**Script contents:**

------------------------------------------------------------------------

## Parallel Execution

For running multiple simulations in parallel on a cluster:

``` r
# Set number of cores
library(MOSAIC)
options(mc.cores = parallel::detectCores() - 1)

# Run parallel simulations
ctrl <- mosaic_control(
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
