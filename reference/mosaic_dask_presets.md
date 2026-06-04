# Get an Optimal Dask/Coiled Cluster Spec for a Given Core Budget

Returns a `dask_spec` list for
[`run_MOSAIC`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
sized to any total core budget between 1 and 1,000 (the workspace cap on
`idm-coiled-idmad-r2`). Routes to the fastest worker/scheduler
configuration for LASER's single-threaded sim workload: every core
becomes a concurrent LASER worker, capped at the workspace limit.

## Usage

``` r
mosaic_dask_presets(cores)
```

## Arguments

- cores:

  Integer between 1 and 1,000 inclusive. The total core budget for the
  cluster. Non-integer values are rounded.

## Value

Named list ready to pass as the `dask_spec` argument of
[`run_MOSAIC`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md).

## Details

**Routing rules:**

- **Worker count** — `floor(cores / 2)`, with a minimum of 1 worker. So
  `cores = 100` → 50 workers (100 cores), `cores = 99` → 49 workers (98
  cores). A request of `cores = 1` rounds up to 1 worker = 2 cores (the
  smallest viable cluster).

- **Worker VM family** — three equivalent 2-vCPU x86 variants are passed
  to Coiled so it can grab whichever has capacity in westus2:
  `Standard_D2s_v6` (Intel Emerald Rapids), `Standard_D2ds_v6` (same
  Intel with local SSD), `Standard_D2ads_v6` (AMD Genoa with local SSD).
  All 2 vCPU / 8 GiB. Smallest VM = max workers per core = max
  concurrent sims at `nthreads = 1`.

- **Scheduler VM** — 5-tier graduation by worker count:

  |                    |                                        |
  |--------------------|----------------------------------------|
  | `n_workers <= 50`  | `Standard_D2s_v6` (2 vCPU / 8 GiB)     |
  | `n_workers <= 100` | `Standard_D4s_v6` (4 vCPU / 16 GiB)    |
  | `n_workers <= 200` | `Standard_D8s_v6` (8 vCPU / 32 GiB)    |
  | `n_workers <= 350` | `Standard_D16s_v6` (16 vCPU / 64 GiB)  |
  | `n_workers > 350`  | `Standard_D32s_v6` (32 vCPU / 128 GiB) |

  Scheduler is mostly connection / task-graph state, not CPU-bound.
  Finer tiers avoid over-provisioning at low worker counts while keeping
  RAM headroom at high counts.

- **wait_for_workers** — absolute number of workers that must be up
  before tasks dispatch. Equivalent to 80\\ pool at `n_workers <= 250`,
  90\\ Passed as an integer (not a fraction) so Coiled has no ambiguity
  to resolve at small `n_workers` (a fractional `0.8 * 1 worker = 0.8`
  risks truncation to 0). Coiled's fraction default of 0.3 is too
  aggressive for short-task calibrations: first sims dispatch onto a
  partial pool, then stall as remaining workers join.

- **timeout** — cluster-creation timeout in seconds. Scales as
  `max(1800, 600 + 4 * n_workers)`. At `cores = 1000` this is ~33
  minutes, large enough for the long tail of Azure VM provisioning in
  westus2 (per-VM p99 ~7 min, parallelized but not free) plus the 90\\

- **workspace** — defaults to `"idm-coiled-idmad-r2"`. Override globally
  without forking the helper by setting
  `options(mosaic.coiled_workspace = "your-workspace")`.

**Rough sizing guide** (~0.7 s per LASER iteration measured on ETH
single-location):

- `cores = 50-100`: smoke tests, 1-5K sims, day-to-day.

- `cores = 250`: typical 10K-sim fixed-budget calibrations.

- `cores = 500`: 24K-sim predictive batches.

- `cores = 1000`: run_10-class jobs (60K+ sims). At the cap.

**Notes:**

- 8 GiB RAM per worker is fine for ETH single-location and similar small
  configs. For multi-country MOSAIC (\>10 locations) you may need more
  headroom — override `vm_types` to `"Standard_D4s_v6"` (16 GiB),
  accepting that the worker count halves for the same core budget.

- Defaults to on-demand pricing. For spot/preemptible VMs (~70\\
  `spot_policy = "spot_with_fallback"` (NOT `"spot"`) so the cluster
  falls back to on-demand if Azure spot capacity in westus2 runs out.
  Most useful at `cores >= 500` where sim wall-time absorbs the
  occasional eviction retry.

## See also

[`mosaic_io_presets`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/mosaic_io_presets.md)
for I/O presets,
[`run_MOSAIC`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
for the calibration workflow.

## Examples

``` r
if (FALSE) { # \dontrun{
# 10K-sim calibration on a 250-core cluster
result <- run_MOSAIC(
  config     = get_location_config(iso = "ETH"),
  priors     = get_location_priors(iso = "ETH"),
  dir_output = "output_eth_10k",
  control    = mosaic_control_defaults(
    calibration = list(n_simulations = 10000, n_iterations = 3)
  ),
  dask_spec  = mosaic_dask_presets(250)
)

# Max cluster with spot VMs for a long adaptive run
spec <- mosaic_dask_presets(1000)
spec$spot_policy <- "spot_with_fallback"

# Multi-country: override to D4s_v6 for RAM headroom
spec <- mosaic_dask_presets(500)
spec$vm_types <- c("Standard_D4s_v6")  # halves to 125 workers, 16 GiB each
} # }
```
