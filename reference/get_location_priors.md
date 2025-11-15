# Get Location-Specific Priors

Extracts priors for specific location(s) from a priors object while
maintaining the exact structure of MOSAIC::priors_default. Keeps all
global parameters and filters location-specific parameters to only
include the requested location(s).

## Usage

``` r
get_location_priors(priors = NULL, iso)
```

## Arguments

- priors:

  A priors list object in the format of MOSAIC::priors_default. If NULL,
  will use MOSAIC::priors_default.

- iso:

  Character vector of ISO3 country codes to extract (e.g., "ETH" or
  c("ETH", "KEN")).

## Value

A priors list object with the same structure as the input, containing:

- All metadata (unchanged)

- All global parameters (unchanged)

- Location-specific parameters filtered to only the requested
  location(s)

## Details

This function preserves the exact structure of the priors object:

- `metadata`: Copied unchanged

- `parameters_global`: All global parameters preserved

- `parameters_location`: Filtered to include only the specified ISO
  codes

The function validates that all requested locations exist in the priors
before extracting. If any location is missing, an error is thrown
listing the missing locations and available locations.

## Examples

``` r
if (FALSE) { # \dontrun{
# Extract priors for Ethiopia
eth_priors <- get_location_priors(iso = "ETH")

# Extract priors for multiple countries
east_africa_priors <- get_location_priors(
  iso = c("ETH", "KEN", "UGA", "TZA")
)

# Use with custom priors object
my_priors <- get_location_priors(
  priors = my_custom_priors,
  iso = "SEN"
)

# Use extracted priors with sample_parameters
config_sampled <- sample_parameters(
  priors = eth_priors,
  config = my_config,
  seed = 123
)
} # }
```
