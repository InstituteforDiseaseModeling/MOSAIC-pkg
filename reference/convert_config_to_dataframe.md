# Convert Config to DataFrame

Converts a complex MOSAIC config list object into a single-row dataframe
with organized named columns. This function handles both scalar
parameters and location-specific parameters, creating appropriately
named columns for each.

## Usage

``` r
convert_config_to_dataframe(config)
```

## Arguments

- config:

  A configuration list object in the format of MOSAIC::config_default.
  The config object should contain both global parameters (scalars) and
  location-specific parameters (vectors/lists).

## Value

A single-row data.frame where:

- Scalar parameters become single columns with their original names

- Vector parameters for multiple locations become columns named as
  "parameter_location" (e.g., "beta_j0_env_ETH", "beta_j0_env_KEN")

- Node-level parameters (matching population nodes) are expanded with
  node indices (e.g., "N_1", "N_2", etc.)

## Details

The function intelligently handles different parameter types:

- Character/numeric scalars: Direct conversion to columns

- Named vectors/lists: Expanded to location-specific columns

- Unnamed vectors matching node count: Expanded to node-indexed columns

- Date fields: Converted to character representation

- NULL values: Skipped

Location-specific parameters are identified by having names that match
ISO3 country codes. Node-level parameters are identified by having
length matching the number of population nodes (N parameter).

## Examples

``` r
if (FALSE) { # \dontrun{
# Convert default config
df <- convert_config_to_dataframe(MOSAIC::config_default)

# Convert sampled config
config_sampled <- sample_parameters(PATHS, seed = 123)
df_sampled <- convert_config_to_dataframe(config_sampled)

# Convert location-specific config
config_eth <- get_location_config(iso = "ETH")
df_eth <- convert_config_to_dataframe(config_eth)

# View structure
str(df)
names(df)
} # }
```
