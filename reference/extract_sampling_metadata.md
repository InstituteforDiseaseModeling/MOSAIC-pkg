# Extract Sampling Metadata from Config

Retrieves sampling flag metadata from a config object that was created
with
[`store_sampling_metadata()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/store_sampling_metadata.md).

## Usage

``` r
extract_sampling_metadata(config)
```

## Arguments

- config:

  Config object containing sampling metadata

## Value

Named list of sampling flags, or NULL if no metadata found

## Examples

``` r
if (FALSE) { # \dontrun{
config <- sample_parameters(PATHS, seed = 123)
config <- store_sampling_metadata(config, sample_alpha_1 = FALSE)

sampling_flags <- extract_sampling_metadata(config)
print(sampling_flags$sample_alpha_1)  # FALSE
} # }
```
