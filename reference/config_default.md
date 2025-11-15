# Default LASER Configuration

The **canonical** LASER parameter object shipped with MOSAIC. It
contains default values for *all* model parameters, initial state
vectors, and file-path references required to run the full cholera
metapopulation transmission model across any number of locations.

## Usage

``` r
config_default
```

## Format

A named **list** created by `make_default_LASER_config()`, whose
elements include:

- **Scalars** – biological constants (`phi_1`, `gamma_1`, `epsilon`, …);

- **Vectors** – initial populations (`S_j_initial`, `I_j_initial`, …),
  initial proportions (optional: `prop_S_initial`, `prop_I_initial`, …),
  coordinates (`longitude`, `latitude`), mobility coefficients, etc.;

- **Matrices** – day-by-day series such as human-to-human transmission
  (`b_jt`), environmental suitability (`psi_jt`), birth/death rates;

- **Character paths** – locations of required input data (see below).

## Details

Unlike the lightweight *simulation* configs exported in
`make_simulation_epidemic_LASER_config_files.R` and
`make_simulation_endemic_LASER_config_files.R`, the **default**
configuration assumes the presence of external data files arranged in
canonical MOSAIC directories:

- MODEL_INPUT:

  Directory containing baseline CSV inputs (population, rainfall,
  vaccination, WASH, etc.).

- DATA_WHO_DAILY:

  Directory with processed WHO daily cholera line-list or aggregated
  case/death counts.

- ENV_RASTERS:

  (Optional) Path to geotiff rasters used to build spatial priors for
  environmental suitability.

The object is intended for full-scale analyses and is therefore
**large** (tens of megabytes) and **not self-contained** – it will error
if the file paths it references do not exist on the user's machine. For
tutorials or automated tests, consider using
[`config_simulation_epidemic`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/config_simulation_epidemic.md)
or
[`config_simulation_endemic`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/config_simulation_endemic.md),
which run without any external dependencies.

**Note on Initial Condition Formats**: This configuration includes both
count (`*_j_initial`) and proportion (`prop_*_initial`) representations
of initial conditions. The count fields are required by the LASER model
for simulation, while the proportion fields are optional and provided
for statistical analysis convenience. Both formats are automatically
maintained in sync during parameter sampling operations.

## See also

- make_defaut_LASER_config() – function that creates this object.

- [config_simulation_epidemic](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/config_simulation_epidemic.md)
  – one-year outbreak toy data set.

- [config_simulation_endemic](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/config_simulation_endemic.md)
  – 5-year endemic toy data set.
