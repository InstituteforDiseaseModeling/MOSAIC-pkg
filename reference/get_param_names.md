# Get Parameter Names from MOSAIC Objects

Extracts parameter names from MOSAIC model configuration objects or
results matrices, organizing them into global parameters and
location-specific parameters by ISO code. This function is useful for
organizing and categorizing parameters for analysis and visualization
workflows.

## Usage

``` r
get_param_names(object)
```

## Arguments

- object:

  A MOSAIC configuration list object (like `config_default`) or a named
  numeric vector/matrix created by
  [`convert_config_to_matrix()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/convert_config_to_matrix.md).

## Value

A list with three elements:

- all:

  Character vector of all parameter names found in the object

- global:

  Character vector of global (scalar) parameter names

- location:

  Named list where each element is a character vector of
  location-specific parameter names for that ISO code

## Details

The function distinguishes between:

- **Global parameters**: Scalar parameters that apply to all locations
  (e.g., "phi_1", "omega_2", "epsilon")

- **Location-specific parameters**: Parameters that vary by location,
  either stored as vectors (in config objects) or with ISO suffixes (in
  results matrices)

For config objects, location-specific parameters are identified by their
base names and the presence of location names. For results matrices,
location-specific parameters are identified by their "\_ISO" suffix
pattern (e.g., "S_j_initial_ETH").

## See also

[`convert_config_to_matrix`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/convert_config_to_matrix.md),
[`convert_config_to_dataframe`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/convert_config_to_dataframe.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# With a config object
config <- MOSAIC::config_default
param_names <- get_param_names(config)

# View all parameter names
param_names$all

# View global parameters only
param_names$global

# View parameters for specific location
param_names$location$ETH

# With a results matrix
config_sampled <- sample_parameters(seed = 123)
results_vec <- convert_config_to_matrix(config_sampled)
param_names <- get_param_names(results_vec)
} # }
```
