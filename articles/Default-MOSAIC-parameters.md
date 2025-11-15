# Default parameters for running MOSIAC

To run a MOSAIC model simulation, you must first define model parameters
via a YAML file. One YAML file provides point estimates for all
parameters. The simulation has stochastic transmission processes so one
YAML file can be used to run a specified number of stochastic replicates
for the given set of parameter values. Parameter uncertainty is
incorporated by running many instances of MOSIAC with a large number of
YAML files.

The MOSAIC R package contains an example YAML file to run a default
simulation. The following code documents how this default parameter
object is created. It can also be accessed with
`MOSAIC::default_parameters`.

``` r

path_to_yaml_file <- file.path(tempdir(), 'parameters.yaml')

make_param_yaml(
          output_file_path = path_to_yaml_file,
          date_start = "2023-03-01",
          date_stop = "2024-12-31",
          location_id = seq_along(MOSAIC::iso_codes_mosaic),
          location_name = c("Location A", "Location B"),
          S_j_initial = as.integer(c(999, 999)),
          I_j_initial = as.integer(c(1, 1)),
          R_j_initial = as.integer(c(0, 0)),
          b_j = c(0.01, 0.02),
          d_j = c(0.01, 0.02),
          nu_jt = matrix(data = 0, nrow = 2, ncol = 31),
          phi = 0.8,
          omega = 0.1,
          epsilon = 0.05,
          gamma = 0.2,
          mu = 0.01,
          rho = 0.9,
          sigma = 0.5,
          beta_j0_hum = c(0.05, 0.03),
          tau_i = c(0.1, 0.2),
          pi_ij = matrix(c(0.8, 0.2, 0.2, 0.8), nrow = 2),
          alpha = 0.95,
          beta_j0_env = c(0.02, 0.04),
          theta_j = c(0.6, 0.7),
          psi_jt = matrix(data = 0, nrow = 2, ncol = 31),
          zeta = 0.5,
          kappa = 10^5,
          delta_min = 0.001,
          delta_max = 0.01
     )

yaml_contents <- yaml::read_yaml(path_to_yaml_file)
print(yaml_contents)
```
