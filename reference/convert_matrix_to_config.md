# Convert Matrix Row to Config Object

Converts a numeric vector (typically from a calibration results matrix
row) back into a MOSAIC config list structure. This is the inverse
operation of
[`convert_config_to_matrix()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/convert_config_to_matrix.md).

## Usage

``` r
convert_matrix_to_config(
  param_vector,
  config_base,
  col_params = NULL,
  sampling_flags = NULL
)
```

## Arguments

- param_vector:

  Named numeric vector with parameter values, as returned by
  [`convert_config_to_matrix()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/convert_config_to_matrix.md).

- config_base:

  Base config object to use as template. Parameters not found in
  param_vector will keep their base values.

- col_params:

  Optional integer vector specifying which columns correspond to
  parameters. If NULL, assumes all elements of param_vector are
  parameters.

- sampling_flags:

  Optional named list specifying which parameters were originally
  sampled (TRUE) vs fixed (FALSE). Parameters with sampling_flags =
  FALSE will NOT be updated from param_vector, preserving original fixed
  values. If NULL, all parameters in param_vector will be updated
  (legacy behavior).

## Value

A MOSAIC config list object with parameter values updated from
param_vector, respecting original sampling intentions.

## Details

This function reconstructs a config object by:

1.  Starting with config_base as template

2.  Parsing parameter names to identify structure (e.g.,
    "beta_j0_hum_ETH")

3.  Checking sampling_flags to determine which parameters should be
    updated

4.  Updating only parameters that were originally sampled (if
    sampling_flags provided)

5.  Handling both scalar and vector parameters appropriately

**IMPORTANT**: The sampling_flags parameter addresses a critical bug
where parameters intended to be fixed (sample\_\* = FALSE) were being
overwritten during matrix reconstruction, making them appear stochastic
in downstream analyses.

Parameter naming conventions expected:

- Scalar parameters: "param_name" (e.g., "phi_1", "gamma_2")

- Location-specific: "param_name_LOCATION" (e.g., "beta_j0_hum_ETH")

- Indexed parameters: "param_name_1", "param_name_2", etc.

## See also

[`convert_config_to_matrix()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/convert_config_to_matrix.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Legacy usage (updates all parameters)
config_orig <- sample_parameters(PATHS, seed = 123)
param_vec <- convert_config_to_matrix(config_orig)
config_reconstructed <- convert_matrix_to_config(param_vec, config_orig)

# Preserving sampling intent (recommended)
sampling_flags <- list(alpha_1 = FALSE, phi_1 = TRUE, beta_j0_hum = TRUE)
config_best <- convert_matrix_to_config(param_row, config_base, 
                                       sampling_flags = sampling_flags)

# Use with calibration results
param_row <- results[best_idx, col_params]
names(param_row) <- param_names
config_best <- convert_matrix_to_config(param_row, config_base,
                                       sampling_flags = original_sampling_flags)
} # }
```
