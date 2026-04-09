# Create a Reusable MOSAIC Parallel Cluster

Creates a properly configured parallel cluster for use with
`run_MOSAIC`. The cluster handles all one-time setup: library loading,
thread safety, Python/LASER import, and root directory propagation. It
can be passed to multiple `run_MOSAIC` calls (e.g. across staged
estimation) to avoid the overhead of repeated cluster creation.

## Usage

``` r
make_mosaic_cluster(n_cores = parallel::detectCores() - 1L, type = "PSOCK")
```

## Arguments

- n_cores:

  Integer. Number of worker processes (default:
  `parallel::detectCores() - 1`).

- type:

  Character. Cluster type: `"PSOCK"` (default, all platforms) or
  `"FORK"` (Linux/Mac only, faster startup).

## Value

A `cluster` object (from
[`parallel::makeCluster`](https://rdrr.io/r/parallel/makeCluster.html))
ready to pass to `run_MOSAIC(cluster = cl)`.

## Details

The cluster setup includes:

1.  Thread environment variables (`OMP_NUM_THREADS`, `MKL_NUM_THREADS`,
    `TBB_NUM_THREADS`, `NUMBA_NUM_THREADS`, `OPENBLAS_NUM_THREADS`) set
    to 1 in both the main process and each worker to prevent
    oversubscription.

2.  BLAS threads limited to 1 per worker via
    `.mosaic_set_blas_threads(1L)`.

3.  Libraries loaded on each worker: `MOSAIC`, `reticulate`, `arrow`.

4.  `laser.cholera` Python module imported once per worker.

5.  Root directory propagated from the main process via
    [`set_root_directory()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/set_root_directory.md).

The caller is responsible for stopping the cluster when done:
`parallel::stopCluster(cl)`.

## See also

[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
for the calibration workflow that accepts this cluster.

## Examples

``` r
if (FALSE) { # \dontrun{
library(MOSAIC)
set_root_directory("~/MOSAIC")

# Create cluster once
cl <- make_mosaic_cluster(n_cores = 8)

# Use across multiple calibration stages
result_s1 <- run_MOSAIC(config, priors, "./stage_1", control, cluster = cl)
result_s2 <- run_MOSAIC(config, priors_s2, "./stage_2", control, cluster = cl)

# Clean up
parallel::stopCluster(cl)
} # }
```
