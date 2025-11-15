# Default prior distributions for MOSAIC model parameters

A comprehensive list containing default informative prior distributions
for all MOSAIC model parameters, organized into global parameters and
location-specific parameters for 40 African countries (301 parameters
total).

## Usage

``` r
priors_default
```

## Format

A list with 3 main components:

- metadata:

  List containing version, date, and description of the priors

- parameters_global:

  List of 21 global model parameters with their prior distributions:

  - `alpha_1`: Transmission parameter for mixing (beta distribution,
    range 0-1)

  - `alpha_2`: Transmission parameter for density dependence (beta
    distribution, range 0-1)

  - `decay_days_long`, `decay_days_short`: Natural immunity decay
    duration parameters (uniform distribution)

  - `decay_shape_1`, `decay_shape_2`: Natural immunity decay shape
    parameters (uniform distribution)

  - `epsilon`: Importation rate of infected individuals (lognormal
    distribution)

  - `gamma_1`: Recovery rate for symptomatic infections (uniform
    distribution, 1/7 to 1/3 day⁻¹)

  - `gamma_2`: Recovery rate for asymptomatic infections (uniform
    distribution, 1/14 to 1/7 day⁻¹)

  - `iota`: Incubation rate (lognormal distribution)

  - `kappa`: Environmental carrying capacity for V. cholerae (uniform
    distribution)

  - `mobility_gamma`, `mobility_omega`: Human mobility model parameters
    (gamma distribution)

  - `omega_1`: Vaccine-derived immunity waning rate for one dose OCV
    (gamma distribution)

  - `omega_2`: Vaccine-derived immunity waning rate for two doses OCV
    (gamma distribution)

  - `phi_1`: Vaccine effectiveness of one dose OCV (beta distribution)

  - `phi_2`: Vaccine effectiveness of two doses OCV (beta distribution)

  - `rho`: Reporting fraction/detection rate of symptomatic cases (beta
    distribution)

  - `sigma`: Proportion of infections that are symptomatic (beta
    distribution)

  - `zeta_1`: Bacterial shedding rate for symptomatic individuals
    (uniform distribution)

  - `zeta_2`: Bacterial shedding rate for asymptomatic individuals
    (uniform distribution)

- parameters_location:

  List of 7 location-specific parameter types for 40 African countries
  (ISO3 codes):

  - `beta_j0_env`: Environmental base transmission rate (gamma
    distribution by country)

  - `beta_j0_hum`: Human-to-human base transmission rate (gamma
    distribution by country)

  - `tau_i`: Country-level travel probabilities (beta distribution with
    uncertainty adjustment)

  - `a1`, `a2`: Seasonality Fourier coefficients for cases (normal
    distribution)

  - `b1`, `b2`: Seasonality Fourier coefficients for deaths (normal
    distribution)

## Details

Each parameter entry contains:

- `parameter_name`: Name of the parameter

- `distribution`: Type of prior distribution (beta, gamma, normal,
  uniform, lognormal)

- `parameters`: Distribution-specific parameters (e.g., shape1/shape2
  for beta, mean/sd for normal)

Location-specific parameters include data for the following 40 African
countries: AGO, BDI, BEN, BFA, BWA, CAF, CIV, CMR, COD, COG, ERI, ETH,
GAB, GHA, GIN, GMB, GNB, GNQ, KEN, LBR, MLI, MOZ, MRT, MWI, NAM, NER,
NGA, RWA, SEN, SLE, SOM, SSD, SWZ, TCD, TGO, TZA, UGA, ZAF, ZMB, ZWE

## See also

- [config_default](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/config_default.md)
  – Default MOSAIC configuration that can use these priors

- [`sample_parameters()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sample_parameters.md)
  – Function that uses these priors for parameter sampling

- [config_simulation_epidemic](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/config_simulation_epidemic.md)
  – One-year outbreak simulation configuration

- [config_simulation_endemic](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/config_simulation_endemic.md)
  – 5-year endemic simulation configuration
