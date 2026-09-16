# Create a Reusable MOSAIC Parallel Cluster

Creates a properly configured parallel cluster for use with
`run_MOSAIC`. The cluster handles all one-time setup: library loading,
thread safety, and root directory propagation. It can be passed to
multiple `run_MOSAIC` calls (e.g. across staged estimation) to avoid the
overhead of repeated cluster creation.

## Usage

``` r
make_mosaic_cluster(n_cores = parallel::detectCores() - 1L, type = "PSOCK")
```

## Arguments

- n_cores:

  Integer. Number of worker processes (default:
  `parallel::detectCores() - 1`). Clamped down to the number of free R
  connections if that is smaller, with a message – see Details.

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

3.  Libraries loaded on each worker: `MOSAIC`, `arrow`.

4.  Root directory propagated from the main process via
    [`set_root_directory()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/set_root_directory.md).

**Connection cap.** Each worker holds one R connection. A default R
build permits 128 connections in total, three of which are already taken
by stdin, stdout and stderr, so requesting more than ~126 workers fails
inside
[`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html).
`n_cores` is therefore clamped to `parallelly::freeConnections() - 2`
(two held back for worker parquet I/O) and a message reports the clamp.
R 4.4.0 and later accept `--max-connections=N` up to 4096 to raise the
ceiling.

The caller is responsible for stopping the cluster when done:
`parallel::stopCluster(cl)`. The returned object also carries a
`"mosaic_worker_pids"` attribute recorded at creation, which
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
uses to reap any worker that survives `stopCluster()` – see
[`?.mosaic_stop_cluster`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/dot-mosaic_stop_cluster.md).

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
