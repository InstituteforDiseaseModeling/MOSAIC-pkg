# Dispatch post-calibration LASER simulations via an existing Dask client

Uses a reconnected Dask client to dispatch ensemble and/or stochastic
parameter uncertainty simulations. Returns pre-computed results that can
be passed to calc_model_ensemble() and plot_model_ensemble() via their
precomputed_results argument.

## Usage

``` r
.mosaic_postca_dask(
  client,
  mosaic_worker,
  param_configs = NULL,
  n_stochastic_per = 10L,
  log_msg = message
)
```

## Arguments

- client:

  dask.distributed.Client (already connected).

- mosaic_worker:

  Python module (mosaic_dask_worker, already imported).

- param_configs:

  List of config lists for posterior parameter sets (for stochastic
  sims). NULL to skip stochastic dispatch.

- n_stochastic_per:

  Integer. Number of stochastic sims per param config.

- log_msg:

  Function. Logging function.

## Value

Named list with \$stochastic_results (or NULL if skipped).
