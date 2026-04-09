# Canonical distribution parameter fields

Returns the canonical parameter field names for each supported
distribution family. Used by posterior fitting and prior updating to
strip diagnostic metadata and keep only the fields required by
[`sample_from_prior`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sample_from_prior.md).

## Usage

``` r
.mosaic_dist_core_fields()
```

## Value

Named list where each element is a character vector of canonical
parameter names for that distribution family.
