# Check Coiled Workspace Capacity and Activity

Prints a snapshot of a Coiled workspace's current resource usage: core
limit, cores in use (account-wide and by you), free cores, and any
active clusters. Useful before launching a large
[`mosaic_dask_presets()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/mosaic_dask_presets.md)
cluster to confirm there's headroom.

## Usage

``` r
check_coiled_workspace(workspace = NULL)
```

## Arguments

- workspace:

  Character. The Coiled workspace slug. Defaults to
  `getOption("mosaic.coiled_workspace", "idm-coiled-idmad-r2")`.

## Value

Invisibly, a list with `workspace`, `core_limit`, `cores_used_account`,
`cores_used_user`, `free_cores`, and `active_clusters` (a list with
`id`, `name`, `state`, `n_workers` per cluster).

## Details

This shows the **Coiled-enforced** core limit and the clusters Coiled
can see in your workspace. It does NOT show the underlying Azure subnet
IP availability, which is the constraint that bites large clusters under
shared-tenancy. For that, query Azure directly (requires Azure CLI +
resource-group read access):


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
