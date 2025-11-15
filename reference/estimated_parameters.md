# Estimated Parameters Inventory

A comprehensive inventory of all stochastic parameters that are
estimated through Bayesian sampling in the MOSAIC cholera transmission
modeling framework. This dataset contains metadata, categorization, and
ordering information for parameters that have prior distributions and
are part of the parameter estimation process.

## Usage

``` r
estimated_parameters
```

## Format

A data frame with 43 rows and 11 columns:

- parameter_name:

  Character. Parameter names as used in configuration files and sampling
  functions

- display_name:

  Character. Human-readable display names for plotting and reporting

- description:

  Character. Detailed descriptions of what each parameter represents

- units:

  Character. Units of measurement for each parameter

- distribution:

  Character. Prior distribution type (beta, gamma, lognormal, uniform,
  normal, gompertz, truncnorm, derived)

- scale:

  Character. Parameter scale: "global" (same value across all locations)
  or "location" (varies by location)

- category:

  Character. Biological/functional category: transmission,
  environmental, disease, immunity, surveillance, mobility, spatial,
  initial_conditions, seasonality

- order:

  Integer. Overall ordering for systematic presentation (1-43)

- order_scale:

  Character. Scale-based ordering (01 = global, 02 = location)

- order_category:

  Character. Category-based ordering within each scale

- order_parameter:

  Character. Parameter-based ordering within each category

## Source

Created by `data-raw/make_parameters_inventory.R`

## Details

This inventory focuses exclusively on **estimated parameters** - those
with prior distributions that are sampled during Bayesian parameter
estimation. It does not include:

- Fixed model constants

- Non-stochastic derived quantities

- Intermediate calculations

- Data inputs (observed cases, demographics, etc.)

The inventory is organized hierarchically:

- **Global parameters** (22): Same value across all locations

- **Location-specific parameters** (21): Vary by geographic location

Categories reflect biological processes in cholera transmission:

- **transmission**: Population mixing, frequency dependence

- **environmental**: V. cholerae survival, shedding, dose-response

- **disease**: Recovery rates, incubation, symptom proportions

- **immunity**: Natural and vaccine-induced protection

- **surveillance**: Reporting rates and delays

- **mobility**: Human movement parameters

- **spatial**: Geographic covariates (WASH coverage)

- **initial_conditions**: Starting compartment proportions

- **seasonality**: Temporal transmission patterns

## Usage

This dataset is used by:

- [`sample_parameters`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sample_parameters.md)
  for Bayesian parameter sampling

- Plotting functions for consistent parameter ordering

- Model validation and convergence diagnostics

- Documentation and reporting functions

## Data Sources

Parameter metadata compiled from:

- [`priors_default`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/priors_default.md) -
  Prior distributions

- [`config_default`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/config_default.md) -
  Configuration templates

- Literature on cholera transmission modeling

- Expert knowledge of epidemic processes

## See also

[`sample_parameters`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sample_parameters.md),
[`priors_default`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/priors_default.md),
[`config_default`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/config_default.md)

## Author

John Giles

## Examples

``` r
# Load the estimated parameters inventory
data(estimated_parameters)

# View parameter structure
str(estimated_parameters)
#> 'data.frame':    43 obs. of  11 variables:
#>  $ parameter_name : chr  "alpha_1" "alpha_2" "decay_days_short" "decay_days_long" ...
#>  $ display_name   : chr  "Population Mixing" "Frequency-Driven Transmission" "Minimum V. cholerae Survival" "Maximum V. cholerae Survival" ...
#>  $ description    : chr  "Population mixing within metapops (0-1, 1 = well-mixed)" "Degree of frequency driven transmission (0-1)" "Minimum V. cholerae survival time in environment" "Maximum V. cholerae survival time in environment" ...
#>  $ units          : chr  "proportion" "proportion" "days" "days" ...
#>  $ distribution   : chr  "beta" "beta" "uniform" "uniform" ...
#>  $ scale          : chr  "global" "global" "global" "global" ...
#>  $ category       : chr  "transmission" "transmission" "environmental" "environmental" ...
#>  $ order          : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ order_scale    : chr  "01" "01" "01" "01" ...
#>  $ order_category : chr  "01" "01" "02" "02" ...
#>  $ order_parameter: chr  "01" "02" "01" "02" ...
#>  - attr(*, "creation_date")= Date[1:1], format: "2025-10-08"
#>  - attr(*, "version")= chr "1.1.0"
#>  - attr(*, "description")= chr "Comprehensive parameter inventory for MOSAIC cholera transmission model. Includes metadata, categorization, and"| __truncated__

# Global vs location-specific parameters
table(estimated_parameters$scale)
#> 
#>   global location 
#>       22       21 

# Parameters by biological category
table(estimated_parameters$category)
#> 
#>            disease      environmental           immunity initial_conditions 
#>                  5                 11                  5                  6 
#>           mobility        seasonality            spatial       surveillance 
#>                  3                  4                  1                  2 
#>       transmission 
#>                  6 

# Parameters by distribution type
table(estimated_parameters$distribution)
#> 
#>      beta   derived     gamma  gompertz lognormal    normal truncnorm   uniform 
#>        16         2         5         1         6         5         1         7 

# Transmission parameters
subset(estimated_parameters, category == "transmission")
#>    parameter_name                  display_name
#> 1         alpha_1             Population Mixing
#> 2         alpha_2 Frequency-Driven Transmission
#> 29    beta_j0_tot  Total Base Transmission Rate
#> 30         p_beta     Human-to-Human Proportion
#> 31    beta_j0_hum   Human-to-Human Transmission
#> 32    beta_j0_env    Environmental Transmission
#>                                                                        description
#> 1                          Population mixing within metapops (0-1, 1 = well-mixed)
#> 2                                    Degree of frequency driven transmission (0-1)
#> 29                            Total base transmission rate (human + environmental)
#> 30                         Proportion of total transmission that is human-to-human
#> 31    Human-to-human component of transmission (derived from beta_j0_tot * p_beta)
#> 32 Environmental component of transmission (derived from beta_j0_tot * (1-p_beta))
#>         units distribution    scale     category order order_scale
#> 1  proportion         beta   global transmission     1          01
#> 2  proportion         beta   global transmission     2          01
#> 29    per day     gompertz location transmission    29          02
#> 30 proportion         beta location transmission    30          02
#> 31    per day      derived location transmission    31          02
#> 32    per day      derived location transmission    32          02
#>    order_category order_parameter
#> 1              01              01
#> 2              01              02
#> 29             02              01
#> 30             02              02
#> 31             02              03
#> 32             02              04

# Location-specific initial conditions
subset(estimated_parameters,
       scale == "location" & category == "initial_conditions")
#>     parameter_name                           display_name
#> 23  prop_S_initial         Initial Susceptible Proportion
#> 24  prop_E_initial             Initial Exposed Proportion
#> 25  prop_I_initial            Initial Infected Proportion
#> 26  prop_R_initial           Initial Recovered Proportion
#> 27 prop_V1_initial Initial One-Dose Vaccinated Proportion
#> 28 prop_V2_initial Initial Two-Dose Vaccinated Proportion
#>                                           description      units distribution
#> 23      Proportion of population susceptible at start proportion         beta
#> 24          Proportion of population exposed at start proportion         beta
#> 25         Proportion of population infected at start proportion         beta
#> 26 Proportion of population recovered/immune at start proportion         beta
#> 27          Proportion with one vaccine dose at start proportion         beta
#> 28         Proportion with two vaccine doses at start proportion         beta
#>       scale           category order order_scale order_category order_parameter
#> 23 location initial_conditions    23          02             01              01
#> 24 location initial_conditions    24          02             01              02
#> 25 location initial_conditions    25          02             01              03
#> 26 location initial_conditions    26          02             01              04
#> 27 location initial_conditions    27          02             01              05
#> 28 location initial_conditions    28          02             01              06
```
