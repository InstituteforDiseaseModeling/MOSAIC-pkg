# Rake the fused OD structure to per-country outbound departure margins

Iterative proportional fitting
([`mipfp::Ipfp`](https://rdrr.io/pkg/mipfp/man/Ipfp.html)) of the
unit-free fused structure onto real daily person-flow margins, giving an
OD matrix that carries both the fused structure AND a defensible
amplitude.

## Usage

``` r
rake_mobility_od_to_tau(
  PATHS,
  iso_codes = NULL,
  tau_daily = NULL,
  N = NULL,
  pop_year = 2017L,
  verbose = TRUE
)
```

## Arguments

- PATHS:

  List of paths from
  [`get_paths`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md).

- iso_codes:

  ISO3 codes. Defaults to
  [`MOSAIC::iso_codes_mosaic`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/iso_codes_mosaic.md).

- tau_daily:

  Named daily departure probabilities. `NULL` (default) takes them from
  [`est_overland_tau_prior`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_overland_tau_prior.md).

- N:

  Named population vector. `NULL` (default) reads the demographics used
  elsewhere in
  [`est_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_mobility.md).

- pop_year:

  Population vintage for the row margin. Default `2017`, which is what
  [`est_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_mobility.md)
  divides by (“to match OAG data”). **These must agree**: raking against
  a different year makes the recovered `tau_i` equal the target times
  `N(pop_year)/N(2017)`.

- verbose:

  Print progress.

## Value

Invisibly, the raked OD matrix (daily person-flows, origin x
destination). Written to `processed/mobility/M_fused_raked.csv`.

## Row margins only

Raking is to the OUTBOUND (row) margin `tau_daily * N` only. E3 tested
raking to both a row and a population column margin and rejected it: on
a 10x10 it over-constrains and distorts the row shape (Nigeria's
structure flattened). Row-only raking hits the tau target to machine
precision and leaves the fused row structure untouched.

## See also

[`est_overland_tau_prior`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_overland_tau_prior.md),
[`est_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_mobility.md)
