# Check Coiled Workspace Capacity and Activity

Prints a snapshot of a Coiled workspace's current resource usage: core
limit, cores in use (account-wide and by you), free cores, and any
active clusters. Useful before launching a large
[`mosaic_dask_presets()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/mosaic_dask_presets.md)
cluster to confirm there's headroom.

## Usage

``` r
check_coiled_workspace(workspace = NULL, subnet_ip_estimate = 507L)
```

## Arguments

- workspace:

  Character. The Coiled workspace slug. Defaults to
  `getOption("mosaic.coiled_workspace", "idm-coiled-idmad-r2")`.

- subnet_ip_estimate:

  Integer. Estimated usable IP count for the workspace's Azure subnet.
  Defaults to 507, the usable capacity of a `/23` subnet (the layout of
  `idm-coiled-idmad-r2`'s `snet-coiled`). Used only to compute a
  heuristic SubnetIsFull warning when active clusters' IP footprint
  exceeds 50\\ Pass a different value for workspaces backed by a
  different subnet size.

## Value

Invisibly, a list with `workspace`, `core_limit`, `cores_used_account`,
`cores_used_user`, `free_cores`, `active_clusters` (a list with `id`,
`name`, `state`, `n_workers` per cluster), `mosaic_ips_used` (IPs
consumed by mosaic-dask clusters), `total_ips_used` (IPs consumed by all
visible clusters), and `subnet_ip_estimate`.

## Details

This shows the **Coiled-enforced** core limit and the clusters Coiled
can see in your workspace. The Azure subnet IP heuristic is a
best-effort warning: Coiled's API doesn't expose subnet usage from OTHER
tenants on the same VNet, so the IP count is a lower bound on actual
consumption. A clean Coiled view here does NOT guarantee that the next
large cluster will provision – a noisy tenant in the same subnet could
still cause SubnetIsFull. For the authoritative view, query Azure
directly (requires Azure CLI + resource-group read access):


    az network vnet subnet show \
      --resource-group rg-coiled-tting \
      --vnet-name vnet-coiled-tting \
      --name snet-coiled

## See also

[`mosaic_dask_presets`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/mosaic_dask_presets.md)
for cluster spec presets.

## Examples

``` r
if (FALSE) { # \dontrun{
check_coiled_workspace()
# === Coiled workspace: idm-coiled-idmad-r2 ===
#   Core limit:        1000
#   Cores in use:      0 (0%) account-wide, 0 yours
#   Free cores:        1000
#   Active clusters:   1
#     - portfolio-workers     state=ready      workers=0

# Override workspace globally:
options(mosaic.coiled_workspace = "ws-idm-coiled-azure")
check_coiled_workspace()

# Or per-call:
check_coiled_workspace("ws-idm-coiled-azure")
} # }
```
