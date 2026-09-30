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

  - "all": The package default flags (every parameter except alpha_1,
    alpha_2, kappa and rho_deaths, which stay pinned)

  - "none": Don't sample any parameters

  - "disease_only": Sample only disease progression, immunity and
    reporting parameters

  - "transmission_only": Sample only the transmission rates
    (beta_j0_tot, p_beta)

  - "mobility_only": Sample only the mobility and diffusion parameters
    (mobility_omega, mobility_gamma, tau_i)

  - "spatial_only": Same flags as "mobility_only"; the spatial coupling
    of the model is its mobility network

  - "environmental_only": Sample only shedding and environmental decay
    (zeta_1, zeta_ratio, decay\_\*)

  - "initial_conditions_only": Sample only initial condition proportions

- seed:

  Random seed for sampling

- custom:

  Named list of flag overrides (e.g. `list(sample_kappa = TRUE)`)
  applied after the pattern

- PATHS:

  Optional PATHS object

- priors:

  Optional priors object

- config:

  Optional config object

## Value

Named list of arguments suitable for `do.call(sample_parameters, ...)`:
`sample_args` (a complete named list of logical flags), `seed`, and
`PATHS`, `priors` and `config` when supplied.

## Details

Every pattern other than "all" sets each `sample_*` flag to FALSE except
the ones the pattern names. No pattern turns on a parameter the package
pins by default (`alpha_1`, `alpha_2`, `kappa`, `rho_deaths`); re-enable
one explicitly through `custom`. `ic_moment_match` keeps its default
(FALSE) unless set in `custom`. With every psi_star flag FALSE, the
config's psi_star values are still applied to `psi_jt` when they differ
from the identity transform (see
[`sample_parameters`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sample_parameters.md)).

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
