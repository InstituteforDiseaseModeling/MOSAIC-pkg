# Convert Config to Numeric Vector for Matrix Storage

Converts a complex MOSAIC config list object directly into a numeric
vector suitable for storage in a matrix. This function is optimized for
calibration workflows where parameters need to be stored efficiently in
pre-allocated matrices.

## Usage

``` r
convert_config_to_matrix(config)
```

## Arguments

- config:

  A configuration list object in the format of MOSAIC::config_default.
  The config object should contain both global parameters (scalars) and
  location-specific parameters (vectors/lists).

## Value

A named numeric vector where:

- All parameter values are converted to numeric representation

- Logical values become 0 (FALSE) or 1 (TRUE)

- Character values are converted to NA with a warning

- Vector parameters are expanded with appropriate suffixes

- Names preserve the parameter structure for later reconstruction

## Details

This function is more efficient than
[`convert_config_to_dataframe()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/convert_config_to_dataframe.md)
when the goal is to store parameters in a numeric matrix, as it avoids
the overhead of creating an intermediate data.frame. It processes
parameters directly into a numeric vector while maintaining the same
naming convention.

The function handles:

- Scalar parameters: Direct numeric conversion

- Location-specific vectors: Expanded with location suffixes

- Node-level parameters: Expanded with numeric indices

- Logical values: Converted to 0/1

- NULL values: Skipped

- Non-numeric values: Converted to NA with warning

## See also

[`convert_config_to_dataframe`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/convert_config_to_dataframe.md)
for data.frame output

## Examples

``` r
if (FALSE) { # \dontrun{
# Convert sampled config for matrix storage
config_sampled <- sample_parameters(PATHS, seed = 123)
vec_params <- convert_config_to_matrix(config_sampled)

# Use in calibration workflow
n_sim <- 100
n_params <- length(vec_params)
results_matrix <- matrix(NA_real_, nrow = n_sim, ncol = 4 + n_params)
results_matrix[1, 5:ncol(results_matrix)] <- vec_params

# Compare with dataframe approach (slower)
df_params <- convert_config_to_dataframe(config_sampled)
vec_params_slow <- as.numeric(df_params)
all.equal(vec_params, vec_params_slow)  # Should be TRUE
} # }
```
