# Create Sampling Arguments for Common Patterns

Helper function to create argument lists for common sampling scenarios,
making it easier to work with the many parameters.

## Usage

``` r
create_sampling_args(
  pattern = "all",
  seed,
  custom = list(),
  PATHS = NULL,
  priors = NULL,
  config = NULL
)
```

## Arguments

- pattern:

  Character string specifying the pattern. Options:

  - "all": Sample all parameters (default)

  - "none": Don't sample any parameters

  - "disease_only": Sample only disease progression and immunity
    parameters

  - "transmission_only": Sample only transmission parameters

  - "mobility_only": Sample only mobility parameters

  - "spatial_only": Sample only spatial parameters

  - "initial_conditions_only": Sample only initial condition proportions

- seed:

  Random seed for sampling

- custom:

  Named list of custom overrides for specific parameters

- PATHS:

  Optional PATHS object

- priors:

  Optional priors object

- config:

  Optional config object

## Value

Named list of arguments suitable for do.call(sample_parameters, ...)

## Examples

``` r
if (FALSE) { # \dontrun{
# Sample only disease parameters
args <- create_sampling_args("disease_only", seed = 123)
config <- do.call(sample_parameters, args)

# Sample all except mobility
args <- create_sampling_args("all", seed = 123,
  custom = list(sample_mobility_omega = FALSE,
                sample_mobility_gamma = FALSE))
config <- do.call(sample_parameters, args)
} # }
```
