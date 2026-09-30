# Parameters removed from the model in v0.96.0

The pre-v0.96.0 mortality model's parameters. A priors object older than
priors_default v16.0 still carries priors for them; the sampler skips
them (with a one-time warning) instead of writing them into the config,
where they would mark it as a pre-v0.96.0 config.

## Usage

``` r
.MOSAIC_REMOVED_MORTALITY_PARAMS
```
