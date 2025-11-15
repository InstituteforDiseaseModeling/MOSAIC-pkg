# Check Sampled Parameter Against Prior

Helper function to compare sampled parameter values against their prior
distributions to verify sampling is working correctly.

## Usage

``` r
check_sampled_parameter(config_sampled, priors, param_name, location = NULL)
```

## Arguments

- config_sampled:

  The sampled config

- priors:

  The priors list

- param_name:

  Name of parameter to check

- location:

  Optional location code for location-specific parameters

## Value

Data frame with parameter info and sampled value
