# Get an Optimal Dask/Coiled Cluster Spec for a Given Worker Count

Returns a `dask_spec` list for
[`run_MOSAIC`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
sized for a given number of LASER workers (1 to 500). Each worker is one
D2s_v6 VM running one cholera sim at a time, so `n_workers` is also the
number of **concurrent simulations** and the number of subnet IPs the
cluster will consume. The workspace cap (1,000 total vCPUs at 2 per
worker = 500 workers) sets the upper bound.

## Usage

``` r
mosaic_dask_presets(n_workers)
```

## Arguments

- n_workers:

  Integer between 1 and 500 inclusive. The number of LASER worker VMs to
  provision. Each runs one cholera sim concurrently. Non-integer values
  are rounded.

## Value

Named list ready to pass as the `dask_spec` argument of
[`run_MOSAIC`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md).

## Details

**Routing rules:**

- **Worker VM family** — always 2-vCPU x86 v6. Three equivalent variants
  are passed to Coiled so it can grab whichever has capacity in westus2:
  `Standard_D2s_v6` (Intel Emerald Rapids), `Standard_D2ds_v6` (same
  Intel with local SSD), `Standard_D2ads_v6` (AMD Genoa with local SSD).
  All 2 vCPU / 8 GiB. The cholera sim is single-threaded GIL-bound
  Python, so larger VMs would idle cores; the second vCPU absorbs Dask
  plumbing (network I/O, nanny, heartbeats).

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
  to resolve at small `n_workers`. Coiled's fraction default of 0.3 is
  too aggressive for short-task calibrations: first sims dispatch onto a
  partial pool, then stall as remaining workers join.

- **timeout** — cluster-creation timeout in seconds. Scales as
  `max(1800, 600 + 4 * n_workers)`. At `n_workers = 500` this is ~43
  minutes, large enough for the long tail of Azure VM provisioning in
  westus2 (per-VM p99 ~7 min, parallelized but not free) plus the 90\\

- **workspace** — defaults to `"idm-coiled-idmad-r2"`. Override globally
  without forking the helper by setting
  `options(mosaic.coiled_workspace = "your-workspace")`.

**Sizing guide** (~0.7 s per LASER iteration measured on ETH
single-location):

- `n_workers = 25-50`: smoke tests, 1-5K sims, day-to-day.

- `n_workers = 125`: typical 10K-sim fixed-budget calibrations.

- `n_workers = 250`: 24K-sim predictive batches.

- `n_workers = 500`: run_10-class jobs (60K+ sims). At the workspace
  1,000-core cap.

**Notes:**

- Each worker consumes one Azure subnet IP. The `idm-coiled-idmad-r2`
  workspace is on a `/23` subnet (~507 usable IPs); under contention
  from other tenants, large clusters can fail at provisioning with
  `SubnetIsFull`. If that happens, drop `n_workers` or coordinate with
  the workspace admin to expand the subnet.

- 8 GiB RAM per worker is fine for ETH single-location and similar small
  configs. For multi-country MOSAIC (\>10 locations) you may need more
  headroom — override `vm_types` to `"Standard_D4s_v6"` (16 GiB),
  accepting that the cluster's cost-per-sim doubles (idle cores) and the
  IP footprint is unchanged.

- Defaults to on-demand pricing. For spot/preemptible VMs (~70\\
  `spot_policy = "spot_with_fallback"` (NOT `"spot"`) so the cluster
  falls back to on-demand if Azure spot capacity in westus2 runs out.

## See also

[`mosaic_io_presets`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/mosaic_io_presets.md)
for I/O presets,
[`run_MOSAIC`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
for the calibration workflow.

## Examples

``` r
if (FALSE) { # \dontrun{
# 10K-sim calibration on 125 workers
result <- run_MOSAIC(
  config     = get_location_config(iso = "ETH"),
  priors     = get_location_priors(iso = "ETH"),
  dir_output = "output_eth_10k",
  control    = mosaic_control_defaults(
    calibration = list(n_simulations = 10000, n_iterations = 3)
  ),
  dask_spec  = mosaic_dask_presets(n_workers = 125)
)

# Max cluster with spot VMs for a long adaptive run
spec <- mosaic_dask_presets(500)
spec$spot_policy <- "spot_with_fallback"

# Multi-country: override to D4s_v6 for RAM headroom
spec <- mosaic_dask_presets(250)
spec$vm_types <- c("Standard_D4s_v6")  # 16 GiB / worker, ~2x cost-per-sim
} # }
```
