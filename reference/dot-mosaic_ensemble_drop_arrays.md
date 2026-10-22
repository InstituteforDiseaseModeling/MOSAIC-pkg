# Drop the heavy 4-D arrays from a mosaic_ensemble (for lightweight persistence)

Returns a copy of a `mosaic_ensemble` object with the dense 4-D arrays –
the observation-level `cases_array`/`deaths_array` and the engine-level
`cases_engine_array`/`deaths_engine_array` – set to `NULL`, preserving
the S3 class and every light field (central tendencies, envelopes,
weights, seeds, obs, metadata). The captured R_eff inputs
(`reff_capture`, three arrays of the same size) go with them: the R_eff
CI that reads them needs the arrays too. Idempotent: safe to call when
the arrays are already `NULL`. Used at
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
save time so persisted ensemble RDS files are small; the in-memory
object is never mutated.

## Usage

``` r
.mosaic_ensemble_drop_arrays(ens)
```

## Arguments

- ens:

  A `mosaic_ensemble` object.

## Value

The same object with the four arrays nulled.
