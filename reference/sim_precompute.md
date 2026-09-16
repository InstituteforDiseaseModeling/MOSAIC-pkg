# Deterministic precomputation for the R transmission engine

These four matrices are pure functions of the config with no randomness
involved, which makes them the sharpest part of the parity story: they
can be checked against the Python oracle exactly, with no PRNG to align
first (Tier A, `migrate-laser-r.md` section 7). Between them they
validate the gravity model, the two-harmonic seasonality, the
psi-normalisation and the beta-CDF decay map.

## Usage

``` r
sim_precompute(par)
```

## Arguments

- par:

  Parameters from
  [`sim_params()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sim_params.md).

## Value

`par` with `pi_ij`, `beta_jt_human`, `beta_jt_env` and `delta_jt`
attached.

## Details

In the Python engine each is built once in a component constructor and
then held constant for the whole run: `pi_ij` and `beta_jt_human` in
`HumanToHuman.__init__`, `beta_jt_env` in `EnvToHuman.__init__`,
`delta_jt` in `Environmental.__init__`.
