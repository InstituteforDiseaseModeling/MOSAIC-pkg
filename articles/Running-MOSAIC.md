# Running MOSAIC

## Overview

MOSAIC (Metapopulation Outbreak Simulation with Agent-based
Implementation for Cholera) simulates cholera transmission dynamics
across Sub-Saharan Africa using the LASER (Light-agent Spatial model for
ERadication) engine. This guide shows how to run models, customize
parameters, and work with outputs.

**Before you begin**: Complete the Installation vignette to set up
MOSAIC and its dependencies.

------------------------------------------------------------------------

## Quick Start

Load MOSAIC and run a simple simulation using default parameters:

``` r
library(MOSAIC)

# Run a 2-location simulation with default parameters
results <- run_LASER(
  config = config_default,
  n_sim = 2,
  seed = 123
)

# View results structure
str(results, max.level = 1)
```

------------------------------------------------------------------------

## Pre-configured Models

MOSAIC includes three pre-configured parameter sets for different
scenarios:

``` r
# 1. Default configuration (minimal 2-location example)
config_default

# 2. Endemic scenario (stable transmission)
config_simulation_endemic

# 3. Epidemic scenario (outbreak dynamics)
config_simulation_epidemic
```

Run any pre-configured model:

``` r
# Run endemic scenario
results_endemic <- run_LASER(
  config = config_simulation_endemic,
  n_sim = 10,
  seed = 456
)
```

------------------------------------------------------------------------

## Single-Location Simulations

Use
[`run_mosaic_iso()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_mosaic_iso.md)
for country-level simulations with ISO codes:

``` r
# Democratic Republic of Congo (DRC)
results_cod <- run_mosaic_iso(
  iso_code = "COD",
  date_start = "2024-01-01",
  date_stop = "2024-12-31",
  n_sim = 10,
  seed = 123
)

# Multiple countries (sequential)
countries <- c("COD", "AGO", "TZA")  # DRC, Angola, Tanzania
results_list <- lapply(countries, function(iso) {
  run_mosaic_iso(
    iso_code = iso,
    date_start = "2024-01-01",
    date_stop = "2024-12-31",
    n_sim = 5,
    seed = 123
  )
})
names(results_list) <- countries
```

**Available ISO codes**: See `MOSAIC::iso_codes` for all supported
countries.

------------------------------------------------------------------------

## Multi-Location Simulations

Use
[`run_LASER()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_LASER.md)
with custom configurations for multi-location models:

``` r
# Create configuration for three countries
config_multi <- make_LASER_config(
  location_name = c("COD", "AGO", "TZA"),
  date_start = "2023-01-01",
  date_stop = "2025-12-31",
  n_patch = 3
)

# Run multi-location simulation
results_multi <- run_LASER(
  config = config_multi,
  n_sim = 20,
  seed = 789
)
```

------------------------------------------------------------------------

## Customizing Model Parameters

The
[`make_LASER_config()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_LASER_config.md)
function creates configurations with custom parameters:

### Basic Customization

``` r
# Modify transmission parameters
custom_config <- make_LASER_config(
  location_name = "COD",
  date_start = "2024-01-01",
  date_stop = "2024-12-31",

  # Transmission rates
  beta_j0_hum = 0.15,     # Human-to-human transmission
  beta_j0_env = 0.25,     # Environment-to-human transmission

  # Population
  N_j_initial = 1000000,

  # Initial conditions
  I_j_initial = 10,       # Initial infected

  verbose = TRUE
)

results_custom <- run_LASER(config = custom_config, n_sim = 10, seed = 100)
```

### Advanced Customization

``` r
# Complex multi-patch configuration
advanced_config <- make_LASER_config(
  location_name = c("COD", "AGO", "TZA", "ZMB"),
  date_start = "2023-01-01",
  date_stop = "2026-12-31",
  n_patch = 4,

  # Vaccination parameters
  vaccination_rate = 0.05,
  vaccine_efficacy = 0.65,

  # Mobility between locations (if available)
  # mobility_matrix = custom_mobility,

  # Reporting parameters
  reporting_rate = 0.3,

  verbose = TRUE
)
```

**See function documentation**:
[`?make_LASER_config`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_LASER_config.md)
for all available parameters.

------------------------------------------------------------------------

## Understanding Model Outputs

### Output Structure

``` r
# Run model
results <- run_LASER(config = config_default, n_sim = 5, seed = 123)

# Output is a list with:
names(results)
# - patches: Time series for each location
# - params: Model parameters used
# - summary: Summary statistics

# Access time series for first location
ts_data <- results$patches[[1]]
head(ts_data)

# Columns include:
# - time: Simulation time step
# - S, E, I, R: Compartment sizes
# - cases: Daily incident cases
# - deaths: Daily deaths
# - cumulative_cases: Cumulative cases
```

### Extracting Key Metrics

``` r
# Extract total cases across all simulations
total_cases <- sapply(results$patches, function(patch) {
  sum(patch$cases, na.rm = TRUE)
})

# Peak timing
peak_time <- sapply(results$patches, function(patch) {
  which.max(patch$cases)
})

# Attack rate
attack_rate <- sapply(results$patches, function(patch) {
  max(patch$cumulative_cases) / patch$N[1]
})

cat("Mean total cases:", mean(total_cases), "\n")
cat("Mean attack rate:", mean(attack_rate), "\n")
```

------------------------------------------------------------------------

## Parallel Execution

For large-scale simulations, use
[`mosaic_control_defaults()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/mosaic_control_defaults.md)
to enable parallel processing:

``` r
# Configure parallel execution
ctrl <- mosaic_control_defaults(
  parallel = TRUE,
  n_cores = parallel::detectCores() - 1,
  verbose = TRUE
)

# Run 100 simulations in parallel
results_parallel <- run_LASER(
  config = config_simulation_endemic,
  control = ctrl,
  n_sim = 100,
  seed = 999
)
```

**Performance tip**: Parallel execution is most beneficial for
`n_sim >= 10`.

------------------------------------------------------------------------

## Stochastic vs Deterministic

MOSAIC runs stochastic simulations by default. Control randomness with
the `seed` parameter:

``` r
# Same seed = reproducible results
run1 <- run_LASER(config = config_default, n_sim = 1, seed = 123)
run2 <- run_LASER(config = config_default, n_sim = 1, seed = 123)
identical(run1, run2)  # TRUE

# Different seeds = different outcomes
run3 <- run_LASER(config = config_default, n_sim = 1, seed = 456)
identical(run1, run3)  # FALSE

# Multiple simulations capture stochastic variation
runs_many <- run_LASER(config = config_default, n_sim = 50, seed = 789)
```

------------------------------------------------------------------------

## Working with Model Configurations

### Viewing Configuration Details

``` r
# Examine a configuration
config <- config_default

# Key parameters
config$date_start
config$date_stop
config$location_name
config$N_j_initial      # Population size
config$beta_j0_hum      # Transmission parameters

# See all parameters
str(config, max.level = 1)
```

### Modifying Existing Configurations

``` r
# Start with a pre-configured model
config_modified <- config_simulation_endemic

# Modify specific parameters
config_modified$date_start <- "2025-01-01"
config_modified$date_stop <- "2026-12-31"
config_modified$vaccination_rate <- 0.1

# Run with modifications
results_modified <- run_LASER(
  config = config_modified,
  n_sim = 10,
  seed = 111
)
```

------------------------------------------------------------------------

## Direct Python Interface (Advanced)

For advanced users, access the LASER Python engine directly:

``` r
# Import laser-cholera Python module
lc <- reticulate::import("laser_cholera.metapop.model")

# Run model directly
model <- lc$run_model(
  paramfile = config_default,
  seed = 123,
  quiet = FALSE
)

# Access raw Python outputs
names(model$params)     # Parameters
names(model$patches)    # Patch data
model$metadata          # Run metadata

# Extract arrays
cases_array <- model$patches$expected_cases
deaths_array <- model$patches$disease_deaths
```

**When to use**: Direct Python interface is useful for debugging or
accessing outputs not returned by R wrapper functions.

------------------------------------------------------------------------

## Common Workflows

### Scenario Comparison

``` r
# Baseline scenario
baseline <- run_LASER(
  config = config_simulation_endemic,
  n_sim = 20,
  seed = 100
)

# Intervention scenario (increased vaccination)
config_intervention <- config_simulation_endemic
config_intervention$vaccination_rate <- 0.15

intervention <- run_LASER(
  config = config_intervention,
  n_sim = 20,
  seed = 100  # Same seed for paired comparison
)

# Compare total cases
baseline_cases <- mean(sapply(baseline$patches, function(p) sum(p$cases)))
intervention_cases <- mean(sapply(intervention$patches, function(p) sum(p$cases)))

reduction <- (baseline_cases - intervention_cases) / baseline_cases * 100
cat("Cases prevented:", reduction, "%\n")
```

### Sensitivity Analysis

``` r
# Test range of transmission rates
beta_values <- seq(0.1, 0.3, by = 0.05)

sensitivity_results <- lapply(beta_values, function(beta) {
  config <- config_default
  config$beta_j0_hum <- beta

  run_LASER(config = config, n_sim = 10, seed = 123)
})

# Extract total cases for each beta
total_cases_by_beta <- sapply(sensitivity_results, function(r) {
  mean(sapply(r$patches, function(p) sum(p$cases)))
})

# Visualize (if desired)
plot(beta_values, total_cases_by_beta,
     type = "b",
     xlab = "Transmission rate (beta)",
     ylab = "Mean total cases")
```

------------------------------------------------------------------------

## Troubleshooting

### Check Python Environment

``` r
# Verify laser-cholera is installed
check_dependencies()

# View Python configuration
reticulate::py_config()

# Reinstall if needed
# install_dependencies()
```

### Common Issues

**Issue**: `Error: Python module laser_cholera not found` **Solution**:
Run
[`install_dependencies()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/install_dependencies.md)

**Issue**: Model runs slowly **Solution**: Enable parallel execution
with `mosaic_control_defaults(parallel = TRUE)`

**Issue**: Results vary between runs **Solution**: Set a fixed seed:
`run_LASER(..., seed = 123)`

------------------------------------------------------------------------

## Next Steps

- **Installation**: Set up MOSAIC on your system (Installation vignette)
- **Deployment**: Deploy on VMs or clusters (Deployment vignette)
- **Parameter Estimation**: Use `est_*` functions for data-driven
  parameters
- **Calibration**: Bayesian parameter fitting with NPE workflows
- **Data Processing**: Process surveillance data with `process_*`
  functions

------------------------------------------------------------------------

## Getting Help

- **Package documentation**:
  [`help(package = "MOSAIC")`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference)
- **Function help**:
  [`?run_LASER`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_LASER.md),
  [`?make_LASER_config`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_LASER_config.md),
  [`?run_mosaic_iso`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_mosaic_iso.md)
- **Report issues**:
  <https://github.com/InstituteforDiseaseModeling/MOSAIC-pkg/issues>
- **Example scripts**: See `MOSAIC-pkg/model/` directory for complete
  workflows
