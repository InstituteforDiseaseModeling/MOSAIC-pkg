# Store Sampling Metadata in Config

Adds sampling flag metadata to a config object for later retrieval
during matrix reconstruction. This enables preservation of sampling
intent throughout the parameter handling pipeline.

## Usage

``` r
store_sampling_metadata(config, ...)
```

## Arguments

- config:

  Config object to annotate

- ...:

  Sampling flag arguments (e.g., sample_alpha_1 = FALSE)

## Value

Config object with sampling metadata stored in \\**sampling_metadata**

## Details

This function stores the sampling flags used during parameter generation
so they can be retrieved later when reconstructing configs from
parameter matrices. The metadata is stored in a special field to avoid
conflicts with model parameters.

## Examples

``` r
if (FALSE) { # \dontrun{
config <- sample_parameters(PATHS, seed = 123, sample_alpha_1 = FALSE)
config <- store_sampling_metadata(config, sample_alpha_1 = FALSE, sample_phi_1 = TRUE)

# Later retrieve the metadata
sampling_flags <- extract_sampling_metadata(config)
} # }
```
