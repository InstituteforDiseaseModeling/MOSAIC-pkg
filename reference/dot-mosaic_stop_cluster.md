# Stop a PSOCK cluster and reap workers that outlive the request

[`parallel::stopCluster()`](https://rdrr.io/r/parallel/makeCluster.html)
shuts a worker down by writing a shutdown message to its socket. A
worker that is not currently *reading* that socket never receives it, so
`stopCluster()` returns cleanly while the worker survives its own
cluster. That happens whenever a run is interrupted mid-task, or a task
stalls inside
[`run_simulation()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_simulation.md)
and the gather gives up on it.

## Usage

``` r
.mosaic_stop_cluster(cl, pids = NULL)

.mosaic_cluster_worker_pids(cl)

.mosaic_proc_cmdline(pid)

.mosaic_psock_alive(pids)
```

## Arguments

- cl:

  A cluster object from
  [`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html)
  or
  [`make_mosaic_cluster()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_mosaic_cluster.md).

- pids:

  Integer worker PIDs. Defaults to the `"mosaic_worker_pids"` attribute
  of `cl`, else a live query.

## Value

`NULL`, invisibly.

## Details

A leaked worker is not merely idle: it inherited the parent's stdout, so
the pipe never reaches EOF and a *finished* run looks like it is hanging
(no output from `tail`, no R master in the process table). It also holds
~1 GB of RSS and one of R's 128 connection slots, which is enough to
make a later cluster creation in the same session fail.

`.mosaic_stop_cluster()` therefore calls `stopCluster()` and then
SIGKILLs any recorded worker PID that is still a live PSOCK worker.

## Why the PIDs are collected up front

The PIDs cannot be asked for at teardown time in the case that matters –
`clusterEvalQ(cl, Sys.getpid())` would queue behind the very task that
is stuck.
[`make_mosaic_cluster()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_mosaic_cluster.md)
therefore records them on the cluster object as the
`"mosaic_worker_pids"` attribute at creation, while every worker is
known to be idle, and `.mosaic_stop_cluster()` reads that attribute. For
a cluster built elsewhere (no attribute) it falls back to querying,
which works unless a worker is already wedged – the same behaviour as
before this function existed, never worse.

## Scope

The kill is Linux-only (it reads `/proc/<pid>/cmdline`) and fires only
for processes whose command line still contains `RSOCK`. On any other
platform, and for `FORK` workers (whose command line is the parent's),
this degrades to plain `stopCluster()`.
