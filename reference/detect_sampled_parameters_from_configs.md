# Detect Parameter Sampling Flags from Config

Attempts to infer which parameters were originally sampled vs fixed by
analyzing parameter variance across multiple configs. This is a fallback
when original sampling flags are not available.

## Usage

``` r
detect_sampled_parameters_from_configs(configs, variance_threshold = 1e-10)
```

## Arguments

- configs:

  List of config objects from parameter sampling

- variance_threshold:

  Parameters with variance below this threshold are considered "fixed".
  Default 1e-10.

## Value

Named list of logical values indicating which parameters appear to be
sampled

## Details

This function compares parameter values across multiple config objects
to identify which parameters show variation (likely sampled) vs those
that remain constant (likely fixed). This is a heuristic approach when
original sampling metadata is lost.

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate multiple configs with some fixed parameters
configs <- lapply(1:10, function(i) {
  sample_parameters(PATHS, seed = i, sample_alpha_1 = FALSE, sample_phi_1 = TRUE)
})
sampling_flags <- detect_sampled_parameters_from_configs(configs)
} # }
```
