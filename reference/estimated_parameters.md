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

A data frame with 46 rows and 14 columns:

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

  Integer. Overall ordering for systematic presentation

- order_scale:

  Character. Scale-based ordering (01 = global, 02 = location)

- order_category:

  Character. Category-based ordering within each scale

- order_parameter:

  Character. Parameter-based ordering within each category

- posterior_distribution:

  Character. Fitted posterior distribution type for each parameter (NA
  when not yet estimated)

- posterior_lower:

  Numeric. Lower bound of the fitted posterior support (NA when not yet
  estimated)

- posterior_upper:

  Numeric. Upper bound of the fitted posterior support (NA when not yet
  estimated)

## Source

Created by `data-raw/make_estimated_parameters_inventory.R`

## Details

This inventory focuses exclusively on **estimated parameters** - those
with prior distributions that are sampled during Bayesian parameter
estimation. It does not include:

- Fixed model constants

- Non-stochastic derived quantities

- Intermediate calculations

- Data inputs (observed cases, demographics, etc.)

The inventory is organized hierarchically:

- **Global parameters** (26): Same value across all locations

- **Location-specific parameters** (20): Vary by geographic location
  (including `alpha_1`, per-location since priors_default v15.16)

The reported case fatality ratio `mu_jt` is not listed: it is not
sampled but integrated out of the deaths likelihood (see
[`calc_log_likelihood_deaths_integrated`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_log_likelihood_deaths_integrated.md)),
and its calibrated value is `3_results/posterior/cfr_posterior.csv`.

Categories reflect biological processes in cholera transmission:

- **transmission**: Population mixing, frequency dependence

- **environmental**: V. cholerae survival, shedding, dose-response

- **disease**: Recovery rates, incubation, symptom proportions

- **immunity**: Natural and vaccine-induced protection

- **surveillance**: Reporting rates, delays and the case-reporting PPV
  switch

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
#> 'data.frame':    46 obs. of  14 variables:
#>  $ parameter_name        : chr  "alpha_2" "decay_days_short" "decay_days_spread" "decay_days_long" ...
#>  $ display_name          : chr  "Frequency-Driven Transmission" "Minimum V. cholerae Survival" "V. cholerae Survival Spread" "Maximum V. cholerae Survival (derived)" ...
#>  $ description           : chr  "Degree of frequency driven transmission (0-1)" "Minimum V. cholerae survival time in environment" "Spread between min and max V. cholerae survival time (decay_days_long - decay_days_short)" "Maximum V. cholerae survival time in environment (DERIVED = decay_days_short + decay_days_spread)" ...
#>  $ units                 : chr  "proportion" "days" "days" "days" ...
#>  $ distribution          : chr  "beta" "truncnorm" "truncnorm" "truncnorm" ...
#>  $ scale                 : chr  "global" "global" "global" "global" ...
#>  $ category              : chr  "transmission" "environmental" "environmental" "environmental" ...
#>  $ order                 : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ order_scale           : chr  "01" "01" "01" "01" ...
#>  $ order_category        : chr  "01" "02" "02" "02" ...
#>  $ order_parameter       : chr  "01" "01" "02" "03" ...
#>  $ posterior_distribution: chr  "beta" "truncnorm" "truncnorm" "truncnorm" ...
#>  $ posterior_lower       : num  NA NA NA 1.01 NA NA NA NA NA NA ...
#>  $ posterior_upper       : num  NA NA NA 425 NA NA NA NA NA NA ...
#>  - attr(*, "creation_date")= Date[1:1], format: "2026-09-30"
#>  - attr(*, "version")= chr "1.3.0"
#>  - attr(*, "description")= chr "Comprehensive parameter inventory for MOSAIC cholera transmission model. Includes metadata, categorization, and"| __truncated__

# Global vs location-specific parameters
table(estimated_parameters$scale)
#> 
#>   global location 
#>       26       20 

# Parameters by biological category
table(estimated_parameters$category)
#> 
#>            disease      environmental           immunity initial_conditions 
#>                  4                 13                  5                  6 
#>           mobility        seasonality            spatial       surveillance 
#>                  3                  4                  1                  6 
#>       transmission 
#>                  4 

# Parameters by distribution type
table(estimated_parameters$distribution)
#> 
#>      beta     gamma lognormal    normal truncnorm 
#>        18         4        10         5         9 

# Transmission parameters
subset(estimated_parameters, category == "transmission")
#>    parameter_name                  display_name
#> 1         alpha_2 Frequency-Driven Transmission
#> 33        alpha_1             Population Mixing
#> 34    beta_j0_tot  Total Base Transmission Rate
#> 35         p_beta     Human-to-Human Proportion
#>                                                description      units
#> 1            Degree of frequency driven transmission (0-1) proportion
#> 33 Population mixing within metapops (0-1, 1 = well-mixed) proportion
#> 34    Total base transmission rate (human + environmental)    per day
#> 35 Proportion of total transmission that is human-to-human proportion
#>    distribution    scale     category order order_scale order_category
#> 1          beta   global transmission     1          01             01
#> 33         beta location transmission    33          02             02
#> 34    lognormal location transmission    34          02             02
#> 35         beta location transmission    35          02             02
#>    order_parameter posterior_distribution posterior_lower posterior_upper
#> 1               01                   beta              NA              NA
#> 33              01                   beta              NA              NA
#> 34              02              lognormal              NA              NA
#> 35              03                   beta              NA              NA

# Location-specific initial conditions
subset(estimated_parameters,
       scale == "location" & category == "initial_conditions")
#>     parameter_name                           display_name
#> 27  prop_S_initial         Initial Susceptible Proportion
#> 28  prop_E_initial             Initial Exposed Proportion
#> 29  prop_I_initial            Initial Infected Proportion
#> 30  prop_R_initial           Initial Recovered Proportion
#> 31 prop_V1_initial Initial One-Dose Vaccinated Proportion
#> 32 prop_V2_initial Initial Two-Dose Vaccinated Proportion
#>                                                                                                                                                                         description
#> 27 Proportion of population susceptible at start (diagnostic reference: sample_parameters() sets S to the simplex residual of the sampled V1/V2/E/I/R, it does not draw this prior)
#> 28                                                                                                                                        Proportion of population exposed at start
#> 29                                                                                                                                       Proportion of population infected at start
#> 30                                                                                                                               Proportion of population recovered/immune at start
#> 31                                                                                                                                        Proportion with one vaccine dose at start
#> 32                                                                                                                                       Proportion with two vaccine doses at start
#>         units distribution    scale           category order order_scale
#> 27 proportion         beta location initial_conditions    27          02
#> 28 proportion         beta location initial_conditions    28          02
#> 29 proportion         beta location initial_conditions    29          02
#> 30 proportion         beta location initial_conditions    30          02
#> 31 proportion         beta location initial_conditions    31          02
#> 32 proportion         beta location initial_conditions    32          02
#>    order_category order_parameter posterior_distribution posterior_lower
#> 27             01              01                   beta              NA
#> 28             01              02                   beta              NA
#> 29             01              03                   beta              NA
#> 30             01              04                   beta              NA
#> 31             01              05                   beta              NA
#> 32             01              06                   beta              NA
#>    posterior_upper
#> 27              NA
#> 28              NA
#> 29              NA
#> 30              NA
#> 31              NA
#> 32              NA
```
